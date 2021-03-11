/* ********************************************************************** *\
 *         Copyright IBM Corporation 1988,1991 - All Rights Reserved      *
 *        For full copyright information see:'andrew/config/COPYRITE'     *
\* ********************************************************************** */

/*
	$Disclaimer: 
 * Permission to use, copy, modify, and distribute this software and its 
 * documentation for any purpose is hereby granted without fee, 
 * provided that the above copyright notice appear in all copies and that 
 * both that copyright notice, this permission notice, and the following 
 * disclaimer appear in supporting documentation, and that the names of 
 * IBM, Carnegie Mellon University, and other copyright holders, not be 
 * used in advertising or publicity pertaining to distribution of the software 
 * without specific, written prior permission.
 * 
 * IBM, CARNEGIE MELLON UNIVERSITY, AND THE OTHER COPYRIGHT HOLDERS 
 * DISCLAIM ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING 
 * ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS.  IN NO EVENT 
 * SHALL IBM, CARNEGIE MELLON UNIVERSITY, OR ANY OTHER COPYRIGHT HOLDER 
 * BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY 
 * DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, 
 * WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS 
 * ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE 
 * OF THIS SOFTWARE.
 *  $
*/

#ifndef NORCSID
#define NORCSID
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/overhead/wputil/RCS/nickgen.c,v 5.7 1992/12/15 21:12:21 rr2b R6tape $";
#endif

#include <stdio.h>
#include <ctype.h>
#include <andrewos.h>
#include <wp.h>

#include <btwp.h>

static int Debugging = 0;
static int NickExceptions = 0;

main(argc, argv)
int argc; char *argv[];
{
    int arg;
    char *ifile = NULL;
    FILE *IF;
    static char Usage[] = " [-d(ebug)|-n(icknames)|-s(urnames)]";
    static char InBuf[1000];
    char *CPtr, *NextPtr, *Canon1, *Canon2;
    int DoSurns;

    DoSurns = 0;
    for (arg = 1; arg < argc; ++arg) {
	if (argv[arg][0] == '-') {
	    if (strcmp(argv[arg], "-d") == 0) {
		extern int bwDebugging;
		bwDebugging = Debugging = 1;
	    } else if (strcmp(argv[arg], "-s") == 0) {
		DoSurns = 1;
	    } else if (strcmp(argv[arg], "-n") == 0) {
		NickExceptions = 1;
	    } else {
		fprintf(stderr, "Bad switch %s\nusage: %s%s\n",
			argv[arg], argv[0], Usage);
		exit(1);
	    }
	} else {
	    if (ifile == NULL) {
		ifile = argv[arg];
	    } else {
		fprintf(stderr, "Extra argument %s\nusage: %s%s\n",
			argv[arg], argv[0], Usage);
		exit(1);
	    }
	}
    }
    if (ifile != NULL) {
	IF = fopen(ifile, "r");
	if (IF == NULL) {
	    fprintf(stderr, "File %s not readable\n", ifile);
	    exit(1);
	}
	if (NickExceptions) {	/* Producing dictionary of nickname mappings */
	    for (;;) {
		if (fgets(InBuf, sizeof(InBuf), IF) == NULL) exit(0);
		CPtr = index(InBuf, '\n');
		if (CPtr != NULL) *CPtr = '\0';
		else {
		    fprintf(stderr, "InBuf too small; need a larger buffer; change and recompile.\n");
		    exit(1);
		}
		Canon1 = Canon2 = NULL;
		for (CPtr = InBuf; CPtr != NULL; CPtr = NextPtr) {
		    NextPtr = index(CPtr, ' ');
		    if (NextPtr != NULL) *NextPtr++ = '\0';
		    if (Canon1 == NULL) {
			Canon1 = CanonGiven(CPtr);
			if (Canon1 == NULL) {
			    fprintf(stderr, "Out of memory on %s\n", CPtr);
			    exit(1);
			}
		    } else {
			Canon2 = CanonNick(CPtr);
			if (Canon2 == NULL) {
			    fprintf(stderr, "Out of memory on %s\n", CPtr);
			    exit(1);
			}
			if (strlen(Canon2) > strlen(Canon1)
			    || ULstlmatch(Canon1, Canon2) == 0) {
			    printf("%s\t%s\t%s\t%s\n",
				   Canon2, Canon1, CPtr, InBuf);
			}
			free(Canon2);
			Canon2 = NULL;
		    }
		}
		if (Canon1 != NULL) free(Canon1);
		if (Canon2 != NULL) free(Canon2);
	    }
	} else if (DoSurns) {
	    for (;;) {
		if (fgets(InBuf, sizeof(InBuf), IF) == NULL) exit(0);
		CPtr = index(InBuf, '\n');
		if (CPtr != NULL) *CPtr = '\0';
		Canon1 = Canon2 = NULL;
		for (CPtr = InBuf; CPtr != NULL; CPtr = NextPtr) {
		    NextPtr = index(CPtr, ' ');
		    if (NextPtr != NULL) *NextPtr++ = '\0';
		    Canon2 = CanonSurn(CPtr);
		    if (Canon2 == NULL) {
			printf("Out of memory on %s\n", CPtr);
			continue;
		    }
		    if (Canon1 != NULL) {
			if (strcmp(Canon1, Canon2) != 0) {
			    printf("%s/%s   %s/%s\n", InBuf, Canon1,
				   CPtr, Canon2);
			}
			free(Canon2);
			Canon2 = NULL;
		    } else {
			Canon1 = Canon2;
			Canon2 = NULL;
		    }
		}
		if (Canon1 != NULL) free(Canon1);
		if (Canon2 != NULL) free(Canon2);
	    }
	} else {	/* doing given/nicknames */
	    for (;;) {
		if (fgets(InBuf, sizeof(InBuf), IF) == NULL) exit(0);
		CPtr = index(InBuf, '\n');
		if (CPtr != NULL) *CPtr = '\0';
		Canon1 = Canon2 = NULL;
		for (CPtr = InBuf; CPtr != NULL; CPtr = NextPtr) {
		    NextPtr = index(CPtr, ' ');
		    if (NextPtr != NULL) *NextPtr++ = '\0';
		    if (Canon1 == NULL) {
			Canon1 = CanonGiven(CPtr);
			if (Canon1 == NULL) {
			    printf("Out of memory on %s\n", CPtr);
			    continue;
			}
		    } else {
			Canon2 = CanonNick(CPtr);
			if (Canon2 == NULL) {
			    printf("Out of memory on %s\n", CPtr);
			    continue;
			}
			if (ULstlmatch(Canon1, Canon2) == 0) {
			    printf("%s  %s	(%s/%s)\n",
				   Canon1, Canon2, InBuf, CPtr);
			}
			free(Canon2);
			Canon2 = NULL;
		    }
		}
		if (Canon1 != NULL) free(Canon1);
		if (Canon2 != NULL) free(Canon2);
	    }
	}
    }

    for (;;) {
	if (fgets(InBuf, sizeof(InBuf), stdin) == NULL) exit(0);
	CPtr = index(InBuf, '\n');
	if (CPtr != NULL) *CPtr = '\0';
	Canon1 = (DoSurns ? CanonSurn(InBuf) : CanonNick(InBuf));
	if (Canon1 == NULL) {
	    printf("out of memory\n");
	} else {
	    printf("%s    %s\n", Canon1, InBuf);
	    free(Canon1);
	}
    }
}
