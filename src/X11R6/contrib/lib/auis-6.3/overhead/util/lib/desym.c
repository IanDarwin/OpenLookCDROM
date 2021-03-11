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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/overhead/util/lib/RCS/desym.c,v 1.9 1993/05/04 00:53:32 susan Exp $";
#endif


#include <andyenv.h>
#include <sys/param.h>
#include <andrewos.h>	/* file, time, strings */
#include <errno.h>
#include <truth.h> /* itc.h -> truth.h DMT */

extern int errno;

#ifndef _IBMR2
extern char *malloc();
extern char *realloc();
#endif /* _IBMR2 */

int DeSymLink(inp, outp, newRoots)
char *inp, *outp; int newRoots;
{/* Rewrite the file path ``inp'' in the buffer pointed to by ``outp'' by following any symbolic links in ``inp''.  If newRoots is false, don't follow symbolic links whose values start with ``/''; if newRoots is true, go ahead and follow them.  Return 0 for all-OK, non-0 for failure.  If the error code is -1, the value of errno will be relevant. */
    char Buf1[MAXPATHLEN+1], Buf2[MAXPATHLEN+1];
    char *This, *Next;
    int RC;
    
    strcpy(outp, inp);
    This = outp; while (*This == '/') ++This;
    for (Next = index(This, '/'); TRUE; Next = index(This, '/')) {
	if (Next != NULL) *Next = '\0';
	RC = osi_readlink(outp, Buf1, sizeof(Buf1)-1);
	if (Next != NULL) *Next = '/';
	if (RC < 0) {
	    if (errno == ENXIO || errno == EINVAL || errno == EACCES) {	/* not a sym link */
		if (Next == NULL) break;
		This = Next; while (*This == '/') ++This;
		if (*This == '\0') break;
		continue;
	    }
	    return -1;
	}
	Buf1[RC] = '\0';
#if SY_AIX11 || SY_AIX12
	if (!strncmp(Buf1, "<LOCAL>", 7)) {
	    Buf2[0] = '\0';
	    getlocal(Buf2, sizeof(Buf2)-1);
	    strcat(Buf2, Buf1+7);
	    strcpy(Buf1, Buf2);
	}
#endif /* SY_AIX11 || SY_AIX12 */
	if (Buf1[0] == '/') {
	    if (!newRoots) return 0;
	    if (Next != NULL) strcat(Buf1, Next);
	    strcpy(outp, Buf1);
	    Next = outp;
	} else {
	    Buf2[0] = '\0';
	    if (Next != NULL) strcpy(Buf2, Next);
	    strcpy(This, Buf1);
	    strcat(This, Buf2);
	    Next = index(This, '/');
	}
	if (Next == NULL) break;
	This = Next; while (*This == '/') ++This;
	if (*This == '\0') break;
    }
    return 0;
}

#ifdef TESTINGONLYTESTING
#include <stdio.h>
main(argc, argv) int argc; char **argv; {
    char Out[1000];
    int Full, AC, RC;
    static char Usage[] = "[-f] file file ...";

    Full = 0;
    for (AC = 1; AC < argc; ++AC) {
	if (argv[AC][0] == '-') {
	    if (strcmp(argv[AC], "-f") == 0) Full = 1;
	    else {
		fprintf(stderr, "Bad option ``%s''\nusage: %s %s\n", argv[AC], argv[0], Usage);
		exit(1);
	    }
	} else {
	    RC = DeSymLink(argv[AC], Out, Full);
	    if (RC != 0) {
		fprintf(stderr, "DeSymLink(``%s'', %d): error %d (%d)\n", argv[AC], Full, RC, errno);
	    } else {
		fprintf(stderr, "DeSymLink(``%s'', %d): ``%s''\n", argv[AC], Full, Out);
	    }
	}
    }
}
#endif /* TESTINGONLYTESTING */
