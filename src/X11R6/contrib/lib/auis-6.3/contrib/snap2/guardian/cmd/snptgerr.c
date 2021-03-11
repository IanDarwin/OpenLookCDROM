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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/contrib/snap2/guardian/cmd/RCS/snptgerr.c,v 1.8 1992/12/15 21:05:43 rr2b R6tape $";
#endif


#include <pfiou.h>
#include <stdio.h>
#include <snap.h>
#include <gasp.h>
#include <snptguaru.h>

#ifndef TRUE
#define TRUE (0==0)
#define FALSE (0==1)
#endif

extern jmp_buf guarcatch;

void rl_error(fmt,
	 a1,a2,a3,a4,a6,a7,a8,a9,a10,
	 b1,b2,b3,b4,b6,b7,b8,b9,b10,
	 c1,c2,c3,c4,c6,c7,c8,c9,c10)
char *fmt;
long a1,a2,a3,a4,a6,a7,a8,a9,a10;
long b1,b2,b3,b4,b6,b7,b8,b9,b10;
long c1,c2,c3,c4,c6,c7,c8,c9,c10;
{
  fflush(stdout);
  fprintf(stderr,"snptguar io error:");

  fprintf(stderr,fmt,
	 a1,a2,a3,a4,a6,a7,a8,a9,a10,
	 b1,b2,b3,b4,b6,b7,b8,b9,b10,
	 c1,c2,c3,c4,c6,c7,c8,c9,c10);
  if(errno!=0)
     fprintf(stderr,"\nlast errno=%d '%s'\n",errno,errno_to_text(errno));
  fflush(stderr);
  set_gerror(GASP_AUTH_FORMAT,"snptguar");
  longjmp(SETJM guarcatch,1);
}

void rl_parerr(parblk,
	       fmt,
	 a1,a2,a3,a4,a6,a7,a8,a9,a10,
	 b1,b2,b3,b4,b6,b7,b8,b9,b10,
	 c1,c2,c3,c4,c6,c7,c8,c9,c10)
char *fmt;
PFM_pt parblk;
long a1,a2,a3,a4,a6,a7,a8,a9,a10;
long b1,b2,b3,b4,b6,b7,b8,b9,b10;
long c1,c2,c3,c4,c6,c7,c8,c9,c10;
{
  rl_error(fmt,
	 a1,a2,a3,a4,a6,a7,a8,a9,a10,
	 b1,b2,b3,b4,b6,b7,b8,b9,b10,
	 c1,c2,c3,c4,c6,c7,c8,c9,c10);
}
