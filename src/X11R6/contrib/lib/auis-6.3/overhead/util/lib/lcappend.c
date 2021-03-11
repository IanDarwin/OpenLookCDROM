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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/overhead/util/lib/RCS/lcappend.c,v 2.6 1992/12/15 21:10:10 rr2b R6tape $";
#endif


 

/*
  lcappend.c

  LCappend(s1, s2): append a lower-alpha-case copy of s2 to s1.
      */

#include <ctype.h>

LCappend(s1, s2)
char *s1, *s2;
{
    char *e1;

    for (e1 = &s1[strlen(s1)]; *s2 != '\0'; ++e1) {
	*e1 = *s2++;
	if (isupper(*e1)) *e1 = tolower(*e1);
    }
    *e1 = '\0';
}
#ifdef TESTINGONLYTESTING
#include <stdio.h>
main()
{
    char a[100];
    strcpy(a, "This is nice: ``");
    LCappend(a, "Andrew.CMU.EDU");
    strcat(a, "''.\n");
    fputs(a, stdout);
}
#endif /* TESTINGONLYTESTING */
