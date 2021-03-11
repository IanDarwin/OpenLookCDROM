/* ********************************************************************** *\
 *	   Copyright IBM Corporation 1988,1991 - All Rights Reserved	  *
 *	  For full copyright information see:'andrew/config/COPYRITE'     *
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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/overhead/class/machdep/sco_i386/RCS/dolist.c,v 1.4 1992/12/15 20:59:52 rr2b R6tape $";
#endif

/*
	dolist.c - list contents of a.out or .o file

	Author:  John H Howard - April 9, 1987
 */


#include <stdio.h>
#include <a.out.h>
#include <setjmp.h>
#include <andrewos.h>
#include <doload.h>

/* read and list module */

void ListIt(inFD)		/* return pointer to data segment, */
				/* or NULL if error */
int inFD;			/* open fd for package file */
{
    struct doload_environment E;
    register struct doload_environment *e;
    RELOC *rp;
    int s;

    /* set up environment */

    doload_setup(e = &E, inFD, List);
    if (setjmp(e->errorJump)) {
	printf("\nerror exit taken\n"); fflush(stdout); fflush(stderr);
	doload_cleanup(e);
	return;
    }

    /* read module into memory */

    doload_read(e);

    /* list symbol table */

    doload_preset(e);

    /* list contents */

    doload_fixup(e);

    (void) puts("\n");
    doload_cleanup(e);
    return ;
}

/* main program for testing doload */

main(argc, argp)
int argc;
char **argp;
{
    int fd;
    int gotcha = 0;

    while (--argc > 0) {
	if (**++argp == '-') {
	    switch (*++*argp) {
	    case 'd':
		doload_trace += 1;
		break;
	    default:
		fprintf( stderr,
			 "dolist:  Unknown switch -%c ignored\n", **argp);
	    }
	}
	else {
	    gotcha += 1;
	    fd = open(*argp, O_RDONLY, 0);
	    if (fd < 0)
		fprintf(stderr, "dolist:  File %s not found\n", *argp);
	    else {
		(void) printf("Listing of program file %s\n", *argp);
		ListIt(fd);
		close(fd);
	    }
	}
    }
    if (gotcha == 0) {
	(void) puts("Listing of program file from standard input");
	ListIt(0);
    }
return 0 ;
}
