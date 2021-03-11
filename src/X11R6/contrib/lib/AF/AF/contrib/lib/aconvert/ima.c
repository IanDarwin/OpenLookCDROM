static char     rcsid[] = "$RCSfile: ima.c,v $, $Revision: 1.8 $, $Date: 1994/06/03 17:13:28 $";

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
 * FILE NAME		: $RCSfile: ima.c,v $
 * AUTHOR		: $Author: jg $
 * DATE			: $Date: 1994/06/03 17:13:28 $
 * REVISION		: $Revision: 1.8 $
 * DESCRIPTION		: Generate 16 bit 16khz audio from 8/8 ulaw
 *
 * REVISION HISTORY
 *
 * $Log: ima.c,v $
 * Revision 1.8  1994/06/03  17:13:28  jg
 * clear copyrights out.
 *
 * Revision 1.7  1994/06/01  19:30:00  jg
 * change header file name to meet PC standards
 *
 * Revision 1.6  1993/10/25  20:33:53  wecker
 * *** empty log message ***
 *
 * Revision 1.5  1993/10/20  18:09:30  wecker
 * Moving to crl/audio tree
 *
 * Revision 1.4  1993/10/20  17:33:52  wecker
 * Added copyrights
 *
 * Revision 1.3  1993/10/19  23:17:29  wecker
 * Changed to Imakefile build
 *
 * Revision 1.2  1993/08/31  20:44:50  wecker
 * Added IMA code
 *
 *
 ************************************************************************/

#include "aconvP.h"

static int      iaAdj[8] = {-1, -1, -1, -1, 2, 4, 6, 8};

static int      iaQuantStep[89] = {
  7, 8, 9, 10, 11, 12, 13, 14,
  16, 17, 19, 21, 23, 25, 28, 31,
  34, 37, 41, 45, 50, 55, 60, 66,
  73, 80, 88, 97, 107, 118, 130, 143,
  157, 173, 190, 209, 230, 253, 279, 307,
  337, 371, 408, 449, 494, 544, 598, 658,
  724, 796, 876, 963, 1060, 1166, 1282, 1411,
  1552, 1707, 1878, 2066, 2272, 2499, 2749, 3024,
  3327, 3660, 4026, 4428, 4871, 5358, 5894, 6484,
  7132, 7845, 8630, 9493, 10442, 11487, 12635, 13899,
  15289, 16818, 18500, 20350, 22385, 24623, 27086, 29794,
  32767
};

/* check value 12635, 22385 against tables */
static Quant taQuant[89];

void AConvertInitIMA(Data* tpD)
{
  int             i, q;

  tpD->iOut	= 0;
  tpD->iLevel	= 0;
  tpD->iLast	= 0;

  for (q = 0; q < 89; q++) {
    for (i = 0; i < 8; i++) {
      taQuant[q].iaOut[i] = iaQuantStep[q] >> 3;
      if (i & 4) taQuant[q].iaOut[i] += iaQuantStep[q];
      if (i & 2) taQuant[q].iaOut[i] += (iaQuantStep[q] >> 1);
      if (i & 1) taQuant[q].iaOut[i] += (iaQuantStep[q] >> 2);
    }
  }
}

int AConvertCompressIMA(		/* Return number of units (error: -1)*/
  Data* 	tpD,			/* Current context */
  int		iCnt,			/* Number of input samples */
  short*	spInp,			/* Input buffer */
  u_char*	ucpOut)			/* Output buffer */
{
  int           i;

  INITUSAGE(-1);

  for (i=0; i<iCnt; i++) {
    int		iDiff = *spInp++ - tpD->iOut;
    int 	iStep = iaQuantStep[tpD->iLevel];
    int 	iCode = 0;
    Quant*	tpQ   = &taQuant[tpD->iLevel];

    if (iDiff < 0) 		{ iCode  = 8; iDiff = -iDiff; }
    if (iDiff >= iStep) 	{ iCode += 4; iDiff -= iStep; }
    if (iDiff >= (iStep >> 1))	{ iCode += 2; iDiff -= iStep >> 1; }
    if (iDiff >= (iStep >> 2))	  iCode += 1;
    if (i & 1)	*ucpOut++   = tpD->iLast + iCode;
    else	tpD->iLast  = iCode << 4;
    if (iCode & 8) tpD->iOut -= tpQ->iaOut[iCode & 7];
    else	   tpD->iOut += tpQ->iaOut[iCode & 7];
    tpD->iLevel += iaAdj[iCode & 7];
    if (tpD->iLevel < 0) 		tpD->iLevel = 0;
    else if (tpD->iLevel > 88)		tpD->iLevel = 88;
    if (tpD->iOut > 32767)		tpD->iOut = 32767;
    else if (tpD->iOut < -32767)	tpD->iOut = -32767;
  }
  iCnt >>= 1;
  RETURN(iCnt);
}

int AConvertUncompressIMA(		/* Return number of units (error: -1)*/
  Data* 	tpD,			/* Current context */
  int		iCnt,			/* Number of input units */
  u_char*	ucpInp,			/* Input buffer */
  short*	spOut)			/* Output buffer */
{
  int           i;

  INITUSAGE(-1);

  iCnt <<= 1;
  for (i=0; i<iCnt; i++) {
    int		iPacked;
    int		iCode;
    Quant*	tpQ	= &taQuant[tpD->iLevel];

    if (!(i & 1)) {
      iPacked	= *ucpInp++;
      iCode	= iPacked >> 4;
    } else
      iCode	= iPacked;
    if (iCode & 8) tpD->iOut -= tpQ->iaOut[iCode & 7];
    else	   tpD->iOut += tpQ->iaOut[iCode & 7];
    tpD->iLevel += iaAdj[iCode & 7];
    if (tpD->iLevel < 0) 		tpD->iLevel = 0;
    else if (tpD->iLevel > 88)		tpD->iLevel = 88;
    if (tpD->iOut > 32767) 		tpD->iOut   = 32767;
    else if (tpD->iOut < -32767)	tpD->iOut   = -32767;
    *spOut++ = tpD->iOut;
  }
  RETURN(iCnt);
}
