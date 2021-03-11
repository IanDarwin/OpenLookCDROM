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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/overhead/util/hdrs/RCS/genhdr.c,v 1.6 1992/12/15 21:07:26 rr2b R6tape $";
#endif

#include <stdio.h>

#define	OFILENAME   "andrdir.h"

/**
 **
 **  Create the file andrdir.h which gives the defined 
 **  QUOTED_DEFAULT_ANDREWDIR_ENV and possibly the 
 **  QUOTED_DEFAULT_LOCALDIR_ENV.
 **  Also, define QUOTED_DEFAULT_ANDREWDIR_ANDREWSETUP.
 **
 **/
int
main(argc, argv)

int argc;
char *argv[];

{
    /*
     * Check command line.
     */
    if ((argc != 2) && (argc != 3) && (argc!=4)) {
	(void) fprintf(stderr, "\ngenhdr:  genhdr [,XBASEDIR] ANDREWDIR [LOCALDIR] \n");
	exit(1);
    }

    if(argv[1][0]==',') {
	printf("\n");
	printf("#define QUOTED_DEFAULT_XBASEDIR_ENV \"%s\"\n", argv[1]+1);
	argv++;
	argc--;
    }

    /*
     * Write out the #defines.
     */
    (void) printf(" \n");
    (void) printf("#define QUOTED_DEFAULT_ANDREWDIR_ENV \"%s\"\n", argv[1]);
    if (argc == 3) {
	(void) printf("#define QUOTED_DEFAULT_LOCALDIR_ENV \"%s\"\n", argv[2]);
    }
    (void) printf("#define QUOTED_DEFAULT_ANDREWDIR_ANDREWSETUP \"%s/etc/AndrewSetup\"\n", argv[1]);
    (void) printf(" \n");


    /*
     * We must check flush because flushs can fail with remote filesystems.
     */
    if (0 != fflush(stdout)) {
	(void) fprintf(stderr, "\ngenhdr:  error fflush'ing stdout\n");
	exit(1);
    }

    /*
     * All done!
     */
    exit(0);

}
