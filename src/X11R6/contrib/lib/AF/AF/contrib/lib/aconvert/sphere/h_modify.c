/********************************************/
/** NIST Speech Header Resources (SPHERE)  **/
/** Release 1.5 (beta)                     **/
/** Stan Janet (stan@jaguar.ncsl.nist.gov) **/
/** October 1990                           **/
/********************************************/

/* File: h_modify.c */

#include <stdio.h>
#include <strings.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/param.h>

#include "header.h"
#include "sp.h"
#include "version.h"

			/* for temporary files */
#define EXTENSION	".BAK"

int ignore_failure = 0;
char *prog;

main(argc,argv)
int argc;
char *argv[];
{
int exit_status = 0;

char *field[MAXFIELDS];
char op[MAXFIELDS];
int nfields = 0;

int i, c, uflag = 0;
char *outfile = CNULL, *dir = CNULL;
extern int optind;
extern char *optarg, *index(), *rindex();
extern char opspec[], ops[];

static char use1[] = "Usage: %s [-uf] [-%s fieldname ...] [-D dir] file ...\n";
static char use2[] = "   or: %s [-uf] [-%s fieldname ...] [-o outfile] file\n";



prog = rindex(argv[0],'/');
prog = (prog == CNULL) ? argv[0] : (prog + 1);

while ((c = getopt(argc,argv,opspec)) != EOF)

  switch (c) {
	case 'D':
		if (optarg == CNULL)
			goto usage;
		dir = optarg;
		break;

	case 'f':
		ignore_failure = 1;
		break;

	case 'o':
		if (optarg == CNULL)
			goto usage;
		outfile = optarg;
		break;

	case 'u':
		uflag = 1;
		break;

	default:
		if (optarg == CNULL)
			goto usage;
		if (index(ops,c) == CNULL)
			goto usage;
		field[nfields] = optarg;
		op[nfields] = (char) c;
		nfields++;
		break;
  }

if (nfields == 0) {
	(void) fprintf(stderr,"%s: Warning -- no fields specified\n",prog);
	exit(0);
}

if (outfile != CNULL) {
	if (dir != CNULL)
		goto usage;
	if (optind + 1 != argc)
		goto usage;
}

if (optind >= argc) {
	(void) fprintf(stderr,"%s: Warning -- no files specified\n",prog);
	exit(0);
}

for (i=optind; i < argc; i++) {

	static char ofile[MAXPATHLEN];

	if (outfile != CNULL)
		(void) strcpy(ofile,outfile);
	else {
		if (dir == CNULL) {
			(void) strcpy(ofile,argv[i]);
			(void) strcat(ofile,EXTENSION);
		} else {
			char *base;

			(void) strcpy(ofile,dir);
			(void) strcat(ofile,"/");
			base = rindex(argv[i],'/');
			base = (base == CNULL) ? argv[i] : (base + 1);
			(void) strcat(ofile,base);
		}
	}

	if (wav_edit(argv[i],ofile,nfields,field,op) < 0) {
		exit_status = ERROR_EXIT_STATUS;
	} else if ((outfile == CNULL) && (dir == CNULL)) {
		if (rename(ofile,argv[i]) < 0) {
			perror(argv[i]);
			exit_status = ERROR_EXIT_STATUS;
		}
	} else {
		if (uflag && (unlink(argv[i]) < 0)) {
			perror(argv[i]);
			exit_status = ERROR_EXIT_STATUS;
		}
	}
}

exit(exit_status);

/*****************/

usage:
{
int len;

len = strlen(ops);
(void) fprintf(stderr,use1,prog,len<=1?ops:"opchar");
(void) fprintf(stderr,use2,prog,len<=1?ops:"opchar");
if (len > 1)
	(void) fprintf(stderr,"Opchar is any of %s\n",ops);
	exit(ERROR_EXIT_STATUS);
}
}
