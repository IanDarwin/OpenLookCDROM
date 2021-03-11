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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/overhead/util/lib/RCS/ucstring.c,v 1.4 1992/12/15 21:12:21 rr2b R6tape $";
#endif

/*
	ucstring.c-- copies a source string to a dest buff, converting all lowercase to upper.
*/


 

#include <ctype.h>

#define UPCASE(x) (isascii(x) && isalpha(x) && isupper(x) ? (x) : (toupper(x)))

/* 
 * Just like strncpy but shift-case in transit and forces null termination.  
 * Copied 8/24/89 from afs/util/casestrcpy.c to allow omission of 
 * lib/afs/util.a; NOTE: on the sun4c version of AFS we need 
 * lib/afs/util.a so this function is going into its very own module.
 * (4/4/91)
*/

char *ucstring (d, s, n)
  char *d;				/* dest string */
  char *s;				/* source string */
  int   n;				/* max transfer size */
{   char *original_d = d;
    char  c;

    if ((s == 0) || (d == 0)) return 0;	/* just to be safe */
    while (n) {
	c = *s++;
	c = UPCASE(c);
	*d++ = c;
	if (c == 0) break;		/* quit after transferring null */
	if (--n == 0) *(d-1) = 0;	/* make sure null terminated */
    }
    return original_d;
}

#undef UPCASE

#ifdef TESTINGONLYTESTING
#include <stdio.h>
main(argc,argv)
int argc;
char *argv[];
{
char *str1, str2;
int len;

  switch(argc) {
  case 3:
    len = argv[3];
    str1 = strcpy(str1,(char*)malloc(strlen(argv[1]) + len + 1));
    str2 = strcpy(str2,(char*)malloc(strlen(argv[2]) + 1));
    printf("str1 %s\nstr2 %s\n", str1, str2);
    result = ucstring(str1, str2, len);
    break;
    fprintf(stderr, "usage: ucstring s1 s2 [n].\n");
    exit(1);
  }
  printf("new str1: %s", str1);
}
#endif /* TESTINGONLYTESTING */
