/*static char rcsid[] = "$RCSfile: AFadpcm.h,v $, $Revision: 1.2 $, $Date: 1994/02/22 18:29:50 $";*/

/*
 * Copyright 1993 by Digital Equipment Corporation, Maynard, Massachusetts.
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
 */

/*
 * adpcm.c
 * Written by tml.
 * Initial creation date 5-Jul-91.
 * Revision history:
 *	 5-Jul-91 tml	initial verion, supports 24Kbps.
 *	 7-Jul-91 tml	add 32Kbps and 16Kbps to 24Kbps implementation.
 *	 8-Jul-91 tml	add this header.  Clean-up cruft.
 *	 9-Jul-91 tml/lcs fix several bugs.  
 *	25-Jun-93 dbw   rewrite for inclusion in aconvert
 *
 *  The original version of this program is modeled after,
 *	"Adaptive Differential Pulse Code Modulation Coding",
 *	JR Boddie, JD Johnston, RE Crochiere, JL Flanagan, CA McGonegal,
 *	JW Upton, and DA Berkley, BSTJ, Vol 60, No 7, Sept. 1981.
 */

void AConvertADPCMInit(AFConvert_Data *acd, AFConvertCmd cmd);

void  AConvertADPCMcompress(	
  AFConvert_Data *acd,
  void* 	ucpInpBuf,
  void* 	ucpOutBuf,
  int 		iUnits
);

void  AConvertADPCMuncompress(	
  AFConvert_Data *acd,
  void* 	ucpInpBuf,
  void* 	ucpOutBuf,
  int 		iUnits
);
