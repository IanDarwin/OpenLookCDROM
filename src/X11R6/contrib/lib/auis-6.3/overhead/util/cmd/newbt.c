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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/overhead/util/cmd/RCS/newbt.c,v 2.14 1993/09/23 20:05:34 gk5g Exp $";
#endif

/* ************************************************************ *\
	newbt.c
	Create a new B-tree of specified desired-node size.
\* ************************************************************ */

#include <andrewos.h> /* strings.h */
#include <sys/param.h>
#include <stdio.h>
#ifdef WHITEPAGES_ENV  /* avoid makedepend "errors" */
#include "bt.h"
#endif /* WHITEPAGES_ENV   */

extern int errno;

static int Debugging = 0;

static char *NewFile;
static int EntrySize;

static void ParseArguments(argc,argv)
int argc;
char *argv[];
{
    int thisarg;
    char *Swch;
    static char *UsageArgs = " [-dDrRwW] FileToCreate EntrySize\n";

    for (thisarg = 1; thisarg < argc && argv[thisarg][0] == '-'; ++thisarg) {
	Swch = &argv[thisarg][1];
	if (strcmp(Swch, "d") == 0) Debugging = 1;
	else if (strcmp(Swch, "D") == 0) Debugging = 2;
	else if (strcmp(Swch, "r") == 0) (void) btr_SetDebugging(1);
	else if (strcmp(Swch, "R") == 0) (void) btr_SetDebugging(2);
	else if (strcmp(Swch, "w") == 0) (void) btw_SetDebugging(1);
	else if (strcmp(Swch, "W") == 0) (void) btw_SetDebugging(2);
	else {
	    fprintf(stdout, "Unrecognized option: ``%s''\nusage: %s%s",
		    argv[thisarg], argv[0], UsageArgs);
	    exit(1);
	}
    }

    if (thisarg < argc) {
	NewFile = argv[thisarg++];
    } else {
	fprintf(stdout, "Missing NewFile argument.\nusage: %s%s", argv[0], UsageArgs);
	exit(2);
    }

    if (thisarg < argc) {
	EntrySize = atoi(argv[thisarg++]);
    } else {
	fprintf(stdout, "Missing NewFile argument.\nusage: %s%s", argv[0], UsageArgs);
	exit(2);
    }

    if (thisarg < argc) {
	fprintf(stdout, "Extra arguments beginning with ``%s'';\nusage: %s%s",
		argv[thisarg], argv[0], UsageArgs);
	exit(3);
    }
}

main(argc, argv)
int argc;
char *argv[];
{
    bt_ErrorCode ErrVal;

    ParseArguments(argc,argv);

    ErrVal = bt_Create(NewFile, EntrySize, btlock_UseFileLock, NULL);
    if (ErrVal == bterr_NoError) {
	fprintf(stdout, "NewBT: File ``%s'' created.\n", NewFile);
	exit(0);
    } else {
	fprintf(stderr, "Error in bt_Create(%s): %s.\n", NewFile,
		bt_ErrorString(ErrVal));
	exit(1);
    }
}

