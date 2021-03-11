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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/overhead/class/cmd/RCS/dotest.c,v 2.7 1992/12/15 20:58:14 rr2b R6tape $";
#endif

/* 
	dotest.c - test loadability of .do file

	Author:  John H Howard - June 11, 1987
 */

#include <stdio.h>
#include <andrewos.h> /* sys/file.h */

char *doload();

extern int doload_trace;				/* nonzero for debugging */

/* globals.o calls for some wmclient routines, and wmclient */
/* calls FlagRedraw.  Therefore we must provide a dummy here. */

FlagRedraw()
{
    fprintf(stderr, "dolist:  FlagRedraw called ... what a blunder!\n");
    exit(69);
}

/* main program for testing loadability */

main(argc, argp)
int argc;
char **argp;
{
    int fd;
    int gotcha = 0;
    char *result;

    while (--argc > 0) {
	if (**++argp == '-') {
	    switch (*++*argp) {
	    case 'd':
		doload_trace++;
		break;
	    default:
		fprintf(stderr, "dotest:  Unknown switch -%c ignored\n", **argp);
	    }
	}
	else {
	    gotcha++;
	    fd = open(*argp, O_RDONLY, 0);
	    if (fd < 0)
		fprintf(stderr, "dotest:  File %s not found\n", *argp);
	    else {
		result = doload(fd, *argp, NULL, NULL);
                close(fd);
                if (result == NULL) {
                    printf("dotest:  error loading %s\n", *argp);
                    exit(2);
                }
	    }
	}
    }
    if (gotcha == 0) {
	printf("dotest:  no files specified\n");
	exit(1);
    }
    exit(0);	/* if everything goes well, return 0 */
}
