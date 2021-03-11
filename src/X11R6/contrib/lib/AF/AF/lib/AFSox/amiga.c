#ifndef LINT
#ifdef RCS_ID
static char *rcsid = "@(#)$Header: /home/wesw/nwd/terminator/src/av/crl/audio/AF/lib/AFSox/RCS/amiga.c,v 1.4 1994/01/12 21:07:53 marvinw Exp $";
#endif /* RCS_ID */
#endif /* LINT */
/***********************************************************
$Copyright$,1994 by Tektronix, Inc., Wilsonville, Oregon.

                        All Rights Reserved

Permission to use, copy, modify, and distribute this software and its 
documentation for any purpose and without fee is hereby granted, 
provided that the above copyright notice appear in all copies and that
both that copyright notice and this permission notice appear in 
supporting documentation, and that the names of Tektronix or Tek not be
used in advertising or publicity pertaining to distribution of the
software without specific, written prior permission.  

TEKTRONIX DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING
ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL
TEKTRONIX BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR
ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,
WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION,
ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS
SOFTWARE.

******************************************************************/
/* tmpfile() function for AmigaDOS.
 * Not needed with SAS/C 6.0 and higher, but included for
 * the benefit of people with pther compilers who might
 * want it.
 * All rights to this code are relinquished to the public domain.
 * --D. Champion, 13 Jan 1993
 */

#include <exec/types.h>
#include <exec/tasks.h>
#include <stdio.h>

#ifndef P_tmpdir
#define P_tmpdir	"t:"
#define B_tmpnam	5
#undef L_tmpnam
#define L_tmpnam	sizeof(P_tmpdir)+B_tmpnam+21
	/* sizeof(P_tmpdir) includes terminating null; B_tmpnam is how much   *
	 * of myname to include; 8x2 for hex longs + 3 dots + 2 sequence nums */
#endif

char tmpnam_count=0;

char *tmpnam(char *s)
{
extern char *myname;	/* myname MUST exist! */
extern char tmpnam_count;
char *uniqsuff=".00000000.00000000.00";
int i;

strcpy (s,P_tmpdir);
strncat(s,myname,B_tmpnam);	/* start tmpfilename with 1st B_tmpnam chars of myname */
sprintf(uniqsuff,".%8x.%8x.%2x\0",(ULONG)FindTask(NULL),(ULONG)time(),(char)tmpnam_count);
for(i=0;i<L_tmpnam+B_tmpnam;i++) {
	if (uniqsuff[i]==' ') uniqsuff[i]='0';
}
strcat (s,uniqsuff);
tmpnam_count++;

return(s);
}

/* Note that tmpfile() does not yet know how to delete files! Sorry, *
 * but I haven't yet found a way to make it track the files it opens *
 * (then close them, if necessary) and delete them.  Maybe soon....  */
FILE *tmpfile(void)
{
FILE *file;
char fnam[L_tmpnam];

return(file=fopen(tmpnam(fnam),"wb+"));
}
