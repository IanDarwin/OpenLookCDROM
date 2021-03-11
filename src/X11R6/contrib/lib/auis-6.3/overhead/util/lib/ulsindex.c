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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/overhead/util/lib/RCS/ulsindex.c,v 2.7 1992/12/15 21:10:49 rr2b R6tape $";
#endif

/*
	ulsindex.c--find index of one string within another, ignoring alphabetic case.
*/


 

char *ULsindex(big, small)
char *big, *small;
/*  ULsindex  --  find index of one string within another, ignoring alphabetic case.
 *
 *  Usage:  p = ULsindex (big,small)
 *	char *p,*big,*small;
 *
 *  ULsindex searches for a substring of big which matches small,
 *  and returns a pointer to this substr.  If no matching
 *  substring is found, 0 is returned.
 *
 */
{
    register char *bp, *bp1, *sp, bc, sc;
    register char c = *small;

    if (c==0) return(0);
    if (c <= 'Z') if (c >= 'A') c += ('a' - 'A');
    for (bp=big;  *bp;  bp++) {
	bc = *bp; if (bc <= 'Z') if (bc >= 'A') bc += ('a' - 'A');
	if (bc == c) {
	    sp = small;
	    bp1 = bp;
	    do {
		sc = *++sp; if (sc == '\0') {
		    return(bp);
		}
		if (sc <= 'Z') if (sc >= 'A') sc += ('a' - 'A');
		bc = *++bp1; if (bc <= 'Z') if (bc >= 'A') bc += ('a' - 'A');
	    } while (sc == bc);
	}
    }
    return 0;
}
