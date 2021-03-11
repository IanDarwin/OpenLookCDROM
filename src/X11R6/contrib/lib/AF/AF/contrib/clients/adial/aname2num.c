/*              Copyright 1990, 1991, 1992, 1993 Digital Equipment Corporation
 *		Maynard, Massachusetts
 * 
 * Permission to use, copy, modify, distribute, and sell this software and its 
 * documentation for any purpose is hereby granted without fee, provided that 
 * the above copyright notice appear in all copies and that both that 
 * copyright notice and this permission notice appear in supporting 
 * documentation, and that the name of Digital not be used in advertising or 
 * publicity pertaining to distribution of the software without specific, 
 * written prior permission.  Digital makes no representations about the 
 * suitability of this software for any purpose.  It is provided "as is" 
 * without express or implied warranty.
 * 
 * DIGITAL DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING 
 * ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL 
 * DIGITAL BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY 
 * DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN 
 * AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF 
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE. *
 */
#include <stdio.h>
#include <ctype.h>
#include <string.h>

/***************** Usage message ***************************/
static usage(msg)
char*	msg;
{
    fprintf(stderr,"\n");
    fprintf(stderr,"Error:   %s\n",msg);
    fprintf(stderr,"Usage:   aname2num [options] [args]\n");
    fprintf(stderr,"    Finds the first name in your phone book that\n");
    fprintf(stderr,"    matches the `args' anywhere on word boundary lines\n");
    fprintf(stderr,"    in your .phonelist file.\n");
    fprintf(stderr,"  Options:\n");
    fprintf(stderr,"    -n       don't apply number rewrite rules\n");
    fprintf(stderr,"    -c       case is significant [ignore case]\n");
    fprintf(stderr,"    -f nam   filename to use [$HOME/.phonelist]\n");
    fprintf(stderr,"  Args:\n");
    fprintf(stderr,"    Name to look up in phone list (all args will be\n");
    fprintf(stderr,"    concatentated with a space between each arg).\n");
    fprintf(stderr,"    If no arguments... then read a line from standard\n");
    fprintf(stderr,"    input.\n");
    fprintf(stderr,"\n");
    exit(-1);
}

/***************** Lookup the string in the phonelist file ****************/
static char* find(fileName,searchStr,doCase)
char*	fileName;
char*	searchStr;
int	doCase;
{
    int			len = strlen(searchStr);
    int			cmp;
    static char		cur[256];
    char*		str;
    char*		end;

    FILE*	fp = fopen(fileName,"r");
    if (!fp) usage("Can't open phonelist file");

    /* Search the phone list file */
    while (fgets(cur,sizeof(cur),fp)) {

	/* Skip lines starting with whitespace */
	if (isspace(cur[0])) continue;

	/* find out where to stop looking */
	end = strchr(cur,':');
	if (!end) end = cur + strlen(cur);

	/* find sub strings to match */
	str = cur;
	while (str && str < end) {
	    if (doCase) cmp = strncmp(str,searchStr,len);
	    else	cmp = strncasecmp(str,searchStr,len);

	    /* Did we get a match? */
	    if (cmp == 0) {
		fclose(fp);
		return cur;
	    }

	    /* Find the next sub string */
	    str += strcspn(str," \t");
	    str += strspn(str," \t");
	}
    }
    fclose(fp);
    return NULL;
}

/************** Clean up the line (return between : and ;) *************/
static char* clean(numStr)
char*	numStr;
{
    char*	start;
    char*	end;

    start = strchr(numStr,':');
    if (!start) start = numStr;
    else	start++;
    end   = strchr(start,';');
    if (end)	*end = '\0';
    else	start[strlen(start)-1] = '\0';
    return start;
}

/***************** Rewrite the number into a dialable string ***************/
static char* rewrite(numStr)
char*	numStr;
{
    int		i,bad;
    char*	str;
    static char	out[256];

    /* First, remove all non-digits from the string */
    str = numStr;
    while (str) {
	str += strspn(str,"0123456789");
	bad  = strcspn(str,"0123456789");
	if (!bad) break;
	for (i=bad; str[i]; i++) str[i-bad] = str[i];
	str[i-bad] = '\0';
    }

    /* Rewrite depending on length */
    switch (strlen(numStr)) {
	case 7:		/* local call */
	sprintf(out,"9,%s",numStr);
	break;

	case 10:	/* long distance */
	sprintf(out,"8,%s",numStr);
	break;

	default:	/* leave all else alone */
	strcpy(out,numStr);
	break;
    }
    return out;
}

/***************** Top level program ***********************/
main(argc,argv)
int	argc;
char	**argv;
{
    int			doCase = 0;		/* is case significant? */
    int			noRewrite = 0;		/* don't apply rewrite rules */
    char		fileName[256];		/* name of phonelist file */
    char		searchStr[256];		/* string to search for */
    char*		numStr;			/* returned number string */
    int			c,idx;

    extern int		getopt();
    extern char*	optarg;
    extern int		optind,opterr;
    extern char*	getenv();

    /* Setup a default file name */
    sprintf(fileName,"%s/.phonelist",getenv("HOME"));

    /* parse command line arguments */
    while ((c = getopt(argc,argv,"cnf:")) != EOF) {
	switch (c) {
	    case 'n':
	    	noRewrite = 1;
		break;
	    case 'c':
	    	doCase	= 1;
		break;
	    case 'f':
		strcpy(fileName,optarg);
		break;
	    default:
		usage("Unknown option");
		break;
	}
    }

    /* Handle the "no arguments" case */
    if (optind == argc) gets(searchStr);
    else		searchStr[0] = '\0';

    /* Accumulate the name to search for */
    for (; optind < argc; optind++) {
	if (strlen(searchStr)+strlen(argv[optind])+1 >= sizeof(searchStr))
	    usage("Arguments too long");
	if (searchStr[0]) strcat(searchStr," ");
	strcat(searchStr,argv[optind]);
    }
    if (strlen(searchStr) == 0) usage("No name to search for");

    /* If we were handed a number... don't search for a name */
    if (isdigit(searchStr[0])) numStr = searchStr;
    else {
	/* See if we can find the string */
	numStr = find(fileName,searchStr,doCase);
	if (!numStr) {
	    puts("");
	    fflush(stdout);
	    exit(1);
	}

	/* Clean up the phone book entry */
	numStr = clean(numStr);
    }

    /* Rewrite the number and output it */
    if (noRewrite) puts(numStr);
    else puts(rewrite(numStr));
    exit(0);
}

