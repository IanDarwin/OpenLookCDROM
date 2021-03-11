static char rcsid[] = "$RCSfile: main.c,v $, $Revision: 1.22 $, $Date: 1994/06/03 17:22:47 $";

/************************************************************************
 *       Copyright 1990, 1991, 1992, 1993 Digital Equipment Corporation
 *		Maynard, Massachusetts
 * 
 * Permission to use, copy, modify, distribute, and sell this software and its 
 * documentation for any purpose is hereby granted without fee, provided that 
 * the above copyright notice appear in all copies and that both that 
 * copyright notice and this permission notice appear in supporting 
 * documentation, and that the name of Digital not be used in advertising or 
 * publicity pertaining to distribution of the software without specific, 
 * written prior permission.  Digital makes no representations about the 
 * suitability of this software for any purpose.  It is provided "as is" 
 * without express or implied warranty.
 * 
 * DIGITAL DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING 
 * ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL 
 * DIGITAL BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY 
 * DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN 
 * AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF 
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 ************************************************************************/

/************************************************************************
 *
 * FILE NAME		: $RCSfile: main.c,v $
 * AUTHOR		: $Author: jg $
 * DATE			: $Date: 1994/06/03 17:22:47 $
 * REVISION		: $Revision: 1.22 $
 * DESCRIPTION		: Generate 16 bit 16khz audio from 8/8 ulaw
 *
 * REVISION HISTORY
 *
 * $Log: main.c,v $
 * Revision 1.22  1994/06/03  17:22:47  jg
 * *** empty log message ***
 *
 * Revision 1.21  1993/10/26  21:43:28  tml
 * endif fix
 *
 * Revision 1.20  1993/10/25  20:35:12  wecker
 * test
 *
 * Revision 1.19  1993/10/20  18:09:38  wecker
 * Moving to crl/audio tree
 *
 * Revision 1.18  1993/10/20  17:34:00  wecker
 * Added copyrights
 *
 * Revision 1.17  1993/08/26  20:44:46  wecker
 * First real version with multi-channel support
 *
 * Revision 1.16  1993/08/21  20:07:50  wecker
 * Added SPHERE file format
 *
 * Revision 1.15  1993/08/20  15:27:33  wecker
 * New callable interface
 *
 * Revision 1.14  1993/08/10  22:45:56  wecker
 * Full callable version
 *
 * Revision 1.13  1993/08/10  17:42:14  wecker
 * *** empty log message ***
 *
 * Revision 1.12  1993/08/09  22:29:53  wecker
 * *** empty log message ***
 *
 * Revision 1.11  1993/06/25  21:24:19  wecker
 * Added STEREO on INPUT
 *
 * Revision 1.10  1993/06/25  20:44:43  wecker
 * Added STEREO, LEFT and RIGHT support
 *
 * Revision 1.9  1993/06/25  19:15:54  wecker
 * New filter code
 *
 * Revision 1.8  1993/06/25  17:26:00  wecker
 * ADPCM code now works
 *
 * Revision 1.7  1993/06/24  20:53:14  wecker
 * Complete re-write with variable rate changing
 *
 * Revision 1.6  1993/06/19  16:56:30  wecker
 * patched some problems.
 *
 * Revision 1.5  1993/06/18  21:43:06  wecker
 * add -h and -l switches
 *
 * Revision 1.4  1993/06/18  20:50:52  wecker
 * Full Sinc/Kaiser filtering
 *
 * Revision 1.3  1993/06/17  20:25:05  wecker
 * *** empty log message ***
 *
 * Revision 1.2  1993/06/17  15:55:44  wecker
 * Added -m and -d
 *
 * Revision 1.1  1993/06/17  15:34:19  wecker
 * Initial revision
 *
 * Revision 1.1  1993/06/16  19:15:14  wecker
 * Initial revision
 *
 ************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include "aconvert.h"

static void Leave()
{
  fprintf(stderr,"\n****ERROR: %s\n",AConvertError());
  exit(1);
}

#ifdef TEST1

main(int argc,char** argv) {
  int		iLen;
  u_char	ucaBuf[1024];
  short*	spBuf;
  AConvertCtxt*	tpACC = AConvertAllocInit(MU255,8000,1,LIN16,16000,1);

  if (!tpACC) Leave();

  /* Read in buffers from standard input */
  while ((iLen = fread(ucaBuf,sizeof(u_char),1024,stdin)) > 0) {
    if (AConvert(tpACC,iLen,ucaBuf,&spBuf) != iLen * 2)	Leave();
    fwrite(spBuf,sizeof(short),iLen * 2,stdout);
  }
  AConvertClose(tpACC);
}

#else /*TEST1*/
#ifdef TEST2

main(int argc,char** argv) {
  int		iLen;
  u_char*	ucaBuf;
  short		spBuf[1024];
  AConvertCtxt*	tpACC = AConvertAllocInit(LIN16,16000,1,MU255,8000,1);

  if (!tpACC) Leave();

  /* Read in buffers from standard input */
  while ((iLen = fread(spBuf,sizeof(short),1024,stdin)) > 0) {
    if (AConvert(tpACC,iLen,spBuf,&ucaBuf) != iLen / 2)	Leave();
    fwrite(ucaBuf,sizeof(u_char),iLen / 2,stdout);
  }
  AConvertClose(tpACC);
}

#else /*TEST2*/

main(int argc,char** argv) {
  if (AConvertFiles(argc,argv)) Leave();
  exit(0);
}

#endif
#endif
