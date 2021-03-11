/*
 * main.c
 *
 * (c) Copyright 1988-1994 Adobe Systems Incorporated.
 * All rights reserved.
 * 
 * Permission to use, copy, modify, distribute, and sublicense this software
 * and its documentation for any purpose and without fee is hereby granted,
 * provided that the above copyright notices appear in all copies and that
 * both those copyright notices and this permission notice appear in
 * supporting documentation and that the name of Adobe Systems Incorporated
 * not be used in advertising or publicity pertaining to distribution of the
 * software without specific, written prior permission.  No trademark license
 * to use the Adobe trademarks is hereby granted.  If the Adobe trademark
 * "Display PostScript"(tm) is used to describe this software, its
 * functionality or for any other purpose, such use shall be limited to a
 * statement that this software works in conjunction with the Display
 * PostScript system.  Proper trademark attribution to reflect Adobe's
 * ownership of the trademark shall be given whenever any such reference to
 * the Display PostScript system is made.
 * 
 * ADOBE MAKES NO REPRESENTATIONS ABOUT THE SUITABILITY OF THE SOFTWARE FOR
 * ANY PURPOSE.  IT IS PROVIDED "AS IS" WITHOUT EXPRESS OR IMPLIED WARRANTY.
 * ADOBE DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING ALL
 * IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 * NON- INFRINGEMENT OF THIRD PARTY RIGHTS.  IN NO EVENT SHALL ADOBE BE LIABLE
 * TO YOU OR ANY OTHER PARTY FOR ANY SPECIAL, INDIRECT, OR CONSEQUENTIAL
 * DAMAGES OR ANY DAMAGES WHATSOEVER WHETHER IN AN ACTION OF CONTRACT,
 * NEGLIGENCE, STRICT LIABILITY OR ANY OTHER ACTION ARISING OUT OF OR IN
 * CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.  ADOBE WILL NOT
 * PROVIDE ANY TRAINING OR OTHER SUPPORT FOR THE SOFTWARE.
 * 
 * Adobe, PostScript, and Display PostScript are trademarks of Adobe Systems
 * Incorporated which may be registered in certain jurisdictions
 * 
 * Author:  Adobe Systems Incorporated
 */

#include <stdio.h>

#ifdef XENVIRONMENT
#include <X11/Xos.h>
#else
#include <string.h>
#endif

#define SLASH '/'
#include <ctype.h>
#include <stdlib.h>
#include "pswpriv.h"

#define MIN_MAXSTRING 80	/* min allowable value of maxstring */

/* global data */
char	*prog;			/* program name */
char	*hfile = NULL;		/* name of -h file */
char	*ofile = NULL;		/* name of -o file */
char	*ifile = NULL;		/* name of input file */
int	gotInFile = 0;		/* got an explicit input file name */
int	doANSI = 0;		/* got -a (ansi) flag for -h file */
int 	pad = 0;		/* got -p (padding) flag */
boolean	noUserNames = false;	/* got -n (don't use usernames) flag */
int	reentrant = 0;		/* automatic vars for generated BOS */
int	bigFile = 0;		/* got -b flag => call free */
FILE	*header;		/* stream for -h file output */
char	headid[200];		/* id for header file #include */
int	maxstring = 2000;	/* -s max string length to scan */
char	*string_temp;		/* string buffer of above size */
int	outlineno = 1;		/* output line number */
int	nWraps = 0;		/* total number of wraps */
#ifdef __MACH__
char	*shlibInclude = NULL;	/* special file to be #included at top of */
                  		/* file.  Used only when building shlibs */
#endif /* __MACH__ */

extern char *psw_malloc();
extern void InitWellKnownPSNames();

static void ScanArgs();
static void Usage();
static void FatalError();

main(argc,argv)
int argc; char *argv[];
{
    extern int	errorCount;	/* non-fatal errs */
    int		retval;		/* return from yyparse */
	
    ScanArgs(argc, argv);

    if (ifile == NULL)
	ifile = "stdin";
    else {
	gotInFile = 1;
	if (freopen(ifile,"r",stdin) == NULL)
	    FatalError("can't open %s for input",ifile);
    }
    if ((string_temp = (char *) malloc((unsigned) (maxstring+1))) == 0)
	FatalError("can't allocate %d char string; try a smaller -s value",maxstring);
    if (ofile == NULL)
		ofile = "stdout";
    else {
#ifdef __MACH__
 		(void)unlink(ofile);
#endif /* __MACH__ */
    	if (freopen(ofile,"w",stdout) == NULL)
			FatalError("can't open %s for output",ofile);
	}
    InitOFile();

    if (hfile != NULL) {
#ifdef __MACH__
 		(void)unlink(hfile);
#endif /* __MACH__ */
 		if ((header = fopen(hfile,"w")) == NULL)
 	    	FatalError("can't open %s for output",hfile);
    }
    if (header != NULL)	InitHFile();

    InitWellKnownPSNames();

    if ((retval = yyparse()) != 0)
	fprintf(stderr,"%s: error in parsing %s\n",prog,ifile);
    else if (errorCount != 0) {
	fprintf(stderr,"%s: errors were encountered\n",prog);
	retval = errorCount;
    }

    if (hfile != NULL) FinishHFile();

    exit (retval);
}

static void ScanArgs(argc, argv)
    int argc;
    char *argv[];
{
    extern int	lexdebug;	/* debug flag for lexer */
    char	*slash;		/* index of last / in hfile */
    char	*c;		/* pointer into headid for conversion */
    int 	i = 0;

    prog = argv[i++];
    slash = rindex(prog,SLASH);
    if (slash)
	prog = slash + 1;
    while (i < argc) {
	if (*argv[i] != '-') {
	    if (ifile != NULL) Usage("Only one input file can be specified.");
	    else ifile = argv[i];
	} else {
	    switch (*(argv[i]+1)) {
	    case 'a':
		doANSI++;
		reentrant++;
		break;
	    case 'b':
		bigFile++;
		break;
#if PSWDEBUG
	    case 'd':
	    	lexdebug++;
		break;
#endif /* PSWDEBUG */
	    case 'h':
		hfile = argv[++i];
		slash = rindex(hfile,SLASH);
		strcpy(headid, slash ? slash+1 : hfile);
		for (c = headid; *c != '\0'; c++) {
		    if (*c == '.') *c = '_';
		    else isascii(*c) && islower(*c) && (*c = toupper(*c));
		}
		break;
	    case 'o':
		ofile = argv[++i];
		break;
	    case 'r':
		reentrant++;
		break;
	    case 's':
		if ((maxstring = atoi(argv[++i])) < MIN_MAXSTRING) {
		    fprintf(stderr,"%s: -s %d is the minimum\n", prog, MIN_MAXSTRING);
		    maxstring = MIN_MAXSTRING;
		}
		break;
	    case 'w':
	    	break;
#ifdef __MACH__
 	    case 'S':
 		shlibInclude = argv[++i];
 		break;
#endif /* __MACH__ */
	     case 'n':
	     	noUserNames = true;
		break;
	    case 'p':
		pad++;
		break;
	    default:
		Usage("bad option '-%c'", *(argv[i]+1));
		break;
	    } /* switch */
	} /* else */
	i++;
    } /* while */
} /* ScanArgs */

static void Usage(msg, arg1, arg2, arg3, arg4) 
	char *msg;
{
    fprintf(stderr,"%s:  ", prog);
    fprintf(stderr, msg, arg1, arg2, arg3, arg4);
    fprintf(stderr,"\nUsage:  pswrap [options] [input-file]\n");
    fprintf(stderr,"    -a              produce ANSI C procedure prototypes\n");
    fprintf(stderr,"    -b              process a big file\n");
    fprintf(stderr,"    -h filename     specify header filename\n");
    fprintf(stderr,"    -o filename     specify output C filename\n");
    fprintf(stderr,"    -r              make wraps re-entrant\n");
    fprintf(stderr,"    -s length       set maximum string length\n");
    exit(1);
}

/* a - d are optional args for fprintf */
static void FatalError(msg, a, b, c, d)
    char *msg;
{
    fprintf(stderr,"%s:  ", prog);
    fprintf(stderr, msg, a, b, c, d);
    fprintf(stderr, "\n");
    exit(1);
} /* FatalError */
