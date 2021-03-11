/********************************************/
/** NIST Speech Header Resources (SPHERE)  **/
/** Release 1.5 (beta)                     **/
/** Stan Janet (stan@jaguar.ncsl.nist.gov) **/
/** October 1990                           **/
/********************************************/

/* File: h_read.c */

/****************************************************************************/
/* By default, this program reads headers from the files specified on the   */
/* command line and prints the values of all fields in the headers.         */
/* Various options can alter the output.                                    */
/* See the manual page h_read.1 for a complete description of the command.  */
/****************************************************************************/

#include <stdio.h>
#include <ctype.h>
#include <strings.h>
#include <sys/types.h>
#include <sys/stat.h>

#include "header.h"
#include "sp.h"
#include "version.h"

#define SELECT_ALL		1
#define SELECT_STD		2
#define SELECT_LISTED		3
#define DEF_SELECT		SELECT_ALL

char *prog, *current_file;
int errors = 0;

int fieldno, total_fields;

int selection = DEF_SELECT;
int complement_selection = FALSE;

int pr_str=TRUE, pr_int=TRUE, pr_real=TRUE;

int verbose = FALSE;
int nfields_expected = -1;
int pr_quote_flag = FALSE;
int pr_data_flag = TRUE;
int pr_type_flag = FALSE;
int pr_fieldname_flag = TRUE;
int check_alphanum = FALSE;
int check_std_fields = FALSE;
int verify_byte_count = FALSE;
int check_byte_order = FALSE;		/* not implemented */
int pr_filename_first = FALSE;
int pr_filename_with_fields = FALSE;
int pr_field_numbers = FALSE;
int pr_total_field_numbers = FALSE;
int number_from_zero = FALSE;
char *odd = CNULL;
char *delim = " ";
char *prf[MAXFIELDS];
int nprf = 0;


/**********************************************************************/

main(argc,argv)
int argc;
char *argv[];
{
int i, j, files, blanklines=0, exit_status;
extern char *optarg, *rindex();
extern int optind, getopt(), atoi();


prog = rindex(argv[0],'/');
prog = (prog == CNULL) ? argv[0] : (prog + 1);

for (;;) {

  int c;
  char *p;

  optarg = CNULL;
  c = getopt(argc,argv,"aB:cde:F:f:N:no:qSs:T:tVv0");
  if (c == EOF)
	break;

  if (verbose)
	if (optarg == CNULL)
		(void) printf("Option: -%c\n",c);
	else
		(void) printf("Option: -%c %s\n",c,optarg);

  switch (c) {
	case 'a':	check_alphanum = TRUE;			break;
	case 'B':	blanklines = atoi(optarg);		break;
	case 'b':	check_byte_order = TRUE;		break;
	case 'c':	complement_selection = TRUE;		break;
	case 'd':	pr_data_flag = FALSE;			break;

	case 'f':	p = optarg;
			while (*p)
			  switch (*p++) {
				case 'h':
					pr_filename_first = TRUE;
					break;
				case 'a':
					pr_filename_with_fields = TRUE;
					break;
				default:
					usage();
			  } /* switch */
			break;

	case 'N':	p = optarg;
			while (*p)
			  switch (*p++) {
				case 'f':
					pr_field_numbers = TRUE;
					break;
				case 't':
					pr_total_field_numbers = TRUE;
					break;
				default:
					usage();
			  } /* switch */
			break;

	case 'o':	if (odd != CNULL) {
				(void) fprintf(stderr,
					"Only one -o option is allowed\n");
				usage();
			}
			odd = optarg;				break;

	case 'T':	pr_str = pr_int = pr_real = FALSE;
			p = optarg;
			while (*p)
			  switch (*p++) {
				case 's':
					pr_str = TRUE;
					break;
				case 'i':
					pr_int = TRUE;
					break;
				case 'r':
					pr_real = TRUE;
					break;
				default:
					usage();
			  } /* switch */
			break;

	case 'q':	pr_quote_flag = TRUE;			break;
	case 'n':	pr_fieldname_flag = FALSE;		break;
	case 't':	pr_type_flag = TRUE;			break;
	case 'F':	prf[nprf++] = optarg;			break;
	case 'e':	nfields_expected = atoi(optarg);	break;
	case 'S':	selection = SELECT_STD;			break;
	case 's':	delim = optarg;				break;
	case 'V':	verify_byte_count = TRUE;		break;
	case 'v':	verbose = TRUE;				break;
	case '0':	number_from_zero = TRUE;		break;
	default:	usage();
  } /* switch */

} /* for (;;) */


		/*
		If any fields to be printed were named for on the command
		line, change selection mode, except if standard fields
		were also selected (error)
		*/
if (nprf)
  switch (selection) {
	case SELECT_ALL:
		selection = SELECT_LISTED;
		break;
	case SELECT_STD:
		(void) fprintf(stderr,"Cannot specify both -F and -S\n");
		usage();
  }


if (pr_total_field_numbers)
	total_fields = number_from_zero ? 0 : 1;
files = argc - optind;
for (i = optind; i < argc; i++) {
	current_file = argv[i];
	if (pr_filename_first)
		(void) printf("::::: %s :::::\n",current_file);
	read_file(current_file);
	if (i != argc - 1)
		for (j = 0; j < blanklines; j++)
			(void) putchar('\n');
}


exit_status = errors ? ERROR_EXIT_STATUS : 0;
if (verbose) {
	(void) printf("Files: %d\n",files);
	(void) printf("Errors: %d\n",errors);
	(void) printf("Exit Status: %d\n",exit_status);
}
exit(exit_status);
}


/********************************************************************/


read_file(file)
char *file;
{
register FILE *fp;
register struct header_t *h;
int nfields;
char *errmsg;
extern char *malloc(), *index();

fp = fopen(file,"r");
if (fp == FPNULL) {
	pr_error("Cannot open");
	return;
}

h = sp_open_header(fp,TRUE,&errmsg);
if (h == HDRNULL) {
	pr_error("Cannot read header");
	(void) fclose(fp);
	return;
}

if (verify_byte_count) {
	struct stat s;
	long samples, sample_bytes, actual, expected;
	int type, size;

	if (stat(file,&s) < 0) {
		pr_error("Stat failed");
		goto end;
	}

	if (sp_get_field(h,"sample_count",&type,&size) < 0) {
		pr_error("No field \"sample_count\"");
		goto end;
	}

	if (type != T_INTEGER) {
		pr_error("Field sample_count is not integer");
		goto end;
	}

	if (sp_get_data(h,"sample_count",(char *) &samples,&size) < 0) {
		pr_error("Lookup on field sample_count failed");
		goto end;
	}

	if (sp_get_field(h,"sample_n_bytes",&type,&size) < 0) {
		pr_error("No field \"sample_n_bytes\"");
		goto end;
	}

	if (type != T_INTEGER) {
		pr_error("Field sample_n_bytes is not integer");
		goto end;
	}

	if (sp_get_data(h,"sample_n_bytes",(char *)&sample_bytes,&size) < 0) {
		pr_error("Lookup on field sample_n_bytes failed");
		goto end;
	}

	actual = (long) s.st_size - (long) ftell(fp);
	expected = sample_bytes * samples;
	if (actual != expected) {
		(void) fprintf(stderr,
			"%s: ERROR: %s: %ld bytes of data (expected %ld)\n",
			prog,file,actual,expected);
		errors++;
	} else {
		if (verbose)
			(void) printf("%s: Byte count ok\n",file);
	}

	end:
		;
}

nfields = sp_get_nfields(h);
if (verbose)
	(void) printf("Fields: %d\n",nfields);

if (check_std_fields) {
	int type, size;
	register char **f = &std_fields[0];
	char buffer[1024];

	while (*f != CNULL) {
	  if (sp_get_field(h,*f,&type,&size) < 0) {
		(void) sprintf(buffer,"Missing standard field %s",*f);
		pr_error(buffer);
	  }
	  f++;
	}
}


if (check_alphanum) {
	register struct field_t **fv;
	int k, i, j;
	char *p;

	fv = h->fv;
	for (k=0; k < nfields; k++) {

		if (fv[k]->type != T_STRING)
			continue;

		p = fv[k]->data;
		j = fv[k]->datalen;
		for (i=0; i < j; i++, p++) {
		  if (isprint(*p))
			continue;
		  if ((odd != CNULL) && (index(odd,*p) != CNULL))
			continue;
		  (void) fprintf(stderr,
		    "%s: ERROR: %s: Odd character #%d (\\0%o) in field %s\n",
		    prog,file,i+1,*p,fv[k]->name);
		  errors++;
		}
	}
}

if ((nfields_expected >= 0) && (nfields != nfields_expected)) {
	(void) printf("%s: %s: %d fields (expected %d)\n",
		prog,file,nfields,nfields_expected);
	errors++;
}

if (nfields > 0) {
	int i;
	char **fv;

	fieldno = number_from_zero ? 0 : 1;
	fv = (char **) malloc((u_int) (nfields * sizeof(char *)));
	if (fv == (char **) NULL)
		pr_error("Out of memory");
	else {
		if (sp_get_fieldnames(h,nfields,fv) != nfields)
			pr_error("Cannot get fieldnames");
		else
			for (i=0; i < nfields; i++)
				field(h,fv[i]);
		free((char *) fv);
	}
}

(void) fclose(fp);

if (sp_close_header(h) < 0)
	pr_error("Cannot close header");
}


/******************************************************************/

pr_error(s)
char *s;
{
errors++;
(void) fflush(stdout);
(void) fprintf(stderr,"%s: ERROR: %s: %s\n",prog,current_file,s);
}

/********************************************************************/

usage()
{
static char usage_msg[] =
    "Usage:  %s [options] [file ...]\n\
	[-acdnqStVv0]\n\
	[-F field]\n\
	[-T typespecs]\n\
	[-f (a|h)]\n\
	[-N (f|t)]\n\
	[-s separator]\n\
	[-B #]\n\
	[-o string]\n\
	[-e #]\n";

(void) fflush(stdout);
(void) fprintf(stderr,usage_msg,prog);
exit(ERROR_EXIT_STATUS);
}

/*******************************************************************/

field(h,name)
register struct header_t *h;
char *name;
{
char *p, *q, buffer[1024];
int n, print_it, type, size, size2, nprinted;

switch (selection) {
	case SELECT_ALL:
		print_it = TRUE;
		break;
	case SELECT_STD:
		print_it = sp_is_std(name);
		break;
	case SELECT_LISTED:
		print_it = in_list(name);
		break;
}

if (complement_selection)
	print_it = ! print_it;

if (! print_it) {
  if (verbose)
	(void) printf("Field %s ignored (based on selection qualifier)\n",name);
  return;
}

n = sp_get_field(h,name,&type,&size);
if (n < 0) {
	(void) sprintf(buffer,"No field \"%s\"",name);
	pr_error(buffer);
	return;
}

switch (type) {
	case T_STRING:
		print_it = pr_str;
		break;
	case T_INTEGER:
		print_it = pr_int;
		break;
	case T_REAL:
		print_it = pr_real;
		break;
}

if (! print_it) {
  if (verbose)
	(void) printf("Field %s ignored (based on type)\n",name);
  return;
}

p = q = malloc((u_int) size);
if (p == CNULL) {
	pr_error("Out of memory");
	return;
}
size2 = size;
n = sp_get_data(h,name,p,&size2);
if (n < 0) {
	(void) sprintf(buffer,"Lookup on field %s failed");
	pr_error(buffer);
	return;
}
if (size != size2) {
  (void) sprintf(buffer,"Lookup on field %s returned %d bytes (expected %d)",
	size2,size);
  pr_error(buffer);
  return;
}


nprinted = 0;

if (pr_total_field_numbers) {
	(void) printf("%d",total_fields++);
	nprinted++;
}

if (pr_filename_with_fields) {
	if (nprinted++)
		(void) fputs(delim,stdout);
	(void) printf("%s",current_file);
}

if (pr_field_numbers) {
	if (nprinted++)
		(void) fputs(delim,stdout);
	(void) printf("%d",fieldno++);
}

if (pr_fieldname_flag) {
	if (nprinted++)
		(void) fputs(delim,stdout);
	(void) printf("%s",name);
}

if (pr_type_flag) {
	if (nprinted++)
		(void) fputs(delim,stdout);
	(void) printf("%c",spx_tp(type));
}

if (pr_data_flag) {
	if (nprinted++)
		(void) fputs(delim,stdout);
	if (pr_quote_flag)
		(void) putchar('"');
	switch (type) {
		case T_STRING:
			while (size--)
				(void) putchar(*p++);
			break;
		case T_INTEGER:
			(void) printf("%ld",*(long *)p);
			break;
		case T_REAL:
			(void) printf("%f",*(double *)p);
			break;
	}
	if (pr_quote_flag)
		(void) putchar('"');
}

if (nprinted)
	(void) putchar('\n');

free(q);
}

/*****************************************************/

int in_list(name)
register char *name;
{
register int i;

if (name == CNULL)
	return FALSE;

for (i=0; i < nprf; i++)
	if (strcmp(name,prf[i]) == 0)
		return TRUE;
return FALSE;
}
