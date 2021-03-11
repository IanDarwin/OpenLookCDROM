

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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/contrib/mit/fxlib/lib/RCS/strcasecmp.c,v 1.3 1992/12/15 21:53:21 rr2b R6tape $";
#endif

/*
 * This file is stolen from the Athena OLH system
 * It contains the definition of strcasecmp, which isn't present on the
 * *^&)(&* broken PS/2's.
 *
 * $Source: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/contrib/mit/fxlib/lib/RCS/strcasecmp.c,v $
 * $Id: strcasecmp.c,v 1.3 1992/12/15 21:53:21 rr2b R6tape $
 * $Author: rr2b $
 *
 */

#if defined(_I386) && defined(_BSD)

#include <stdio.h>

int strcasecmp(str1,str2)
     char *str1,*str2;
{
  int a;

  if (str1 == NULL) {
    if (str2 == NULL)
      return(0);
    else
      return(-1);
  }
  if (str2 == NULL)
    return(1);
  while ((*str1 != '\0') && (*str2 != '\0')) {
    a = (*str1++ - *str2++);
    if ((a != 0) && (a != 'A' - 'a') && (a != 'a' -'A')) return(a);
  }
  if (*str1 == *str2) return(0);
  if (*str1 ==  '\0') return(-1); else return(1);
}

#endif 
