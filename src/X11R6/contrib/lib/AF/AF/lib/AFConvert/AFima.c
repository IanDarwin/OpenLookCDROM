static char     rcsid[] = "$RCSfile: AFima.c,v $, $Revision: 1.2 $, $Date: 1994/02/15 13:56:51 $";

/************************************************************
Copyright 1993 by Digital Equipment Corporation, Maynard, Massachusetts.

                        All Rights Reserved

Permission to use, copy, modify, and distribute this software and its 
documentation for any purpose and without fee is hereby granted, provided 
that the above copyright notice appear in all copies and that both that 
copyright notice and this permission notice appear in supporting 
documentation, and that the names of Digital not be used in 
advertising or publicity pertaining to distribution of the software without 
specific, written prior permission.

DIGITAL DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING 
ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL 
DIGITAL BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY 
DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN 
AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF 
OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

********************************************************/

/************************************************************************
 *
 * FILE NAME		: $RCSfile: AFima.c,v $
 * AUTHOR		: $Author: tml $
 * DATE			: $Date: 1994/02/15 13:56:51 $
 * REVISION		: $Revision: 1.2 $
 * DESCRIPTION		: Generate 16 bit 16khz audio from 8/8 ulaw
 *
 * REVISION HISTORY
 *
 * $Log: AFima.c,v $
 * Revision 1.2  1994/02/15  13:56:51  tml
 * eliminate Amd dependency.
 *
 * Revision 1.1  1994/02/01  16:46:28  stewart
 * Initial revision
 *
 * Revision 1.1  1993/12/10  13:29:54  stewart
 * Initial revision
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

#include "AFima.h"
#include "AFClientTypes.h"
#include "audio.h"
#include <server/include/misc.h>
#include <server/include/ac.h>
#include "AFUtils.h"
		
typedef struct {
  int iaOut[8];
} Quant;

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
static int imaInit = 0;

void AFConvert_InitIMA(AFConvert_Data *cd, AFConvertCmd cmd)
{
  int i, q;
  IMAState *tpD = (IMAState *) cd->state;
  if (cmd == ALLOC) {
    tpD = (IMAState *) malloc(sizeof(IMAState) * cd->nChannels);
    AoD(tpD != NULL, "Can't alloc IMA Conversion structure\n");
    cd->state = tpD;
  }
  for (i = 0; i < cd->nChannels; i += 1) {
    tpD = &((IMAState *) cd->state)[i];
    tpD->iOut	= 0;
    tpD->iLevel	= 0;
    tpD->iLast	= 0;
  }
  if (imaInit == 0) {
    imaInit = 1;
    for (q = 0; q < 89; q++) {
      for (i = 0; i < 8; i++) {
	taQuant[q].iaOut[i] = iaQuantStep[q] >> 3;
	if (i & 4) taQuant[q].iaOut[i] += iaQuantStep[q];
	if (i & 2) taQuant[q].iaOut[i] += (iaQuantStep[q] >> 1);
	if (i & 1) taQuant[q].iaOut[i] += (iaQuantStep[q] >> 2);
      }
    }
  }
  if (cmd == DEALLOC) {
    if (cd->state != NULL) free(cd->state);
    cd->state = NULL;
  }
}

void AFConvert_CompressIMA(
			   AFConvert_Data *cd,
			   void	*ucpIn,		/* Input buffer */
			   void	*ucpOut,	/* Output buffer */
			   int		iCnt		/* Number of units */
			   )
{
  int i;
  int channel;
  for (channel = 0; channel < cd->nChannels; channel += 1) {
    IMAState *tpD = &(((IMAState *) cd->state)[channel]);
    int iOut = tpD->iOut;
    int iLevel = tpD->iLevel;
    int iLast = tpD->iLast;
    AF_CARD8 *ucpLocalIn = ((AF_CARD8 *) ucpIn) + channel;
    AF_INT16 *spIn = ((AF_INT16 *) ucpIn) + channel;
    AF_CARD8 *ucpLocalOut = ((AF_CARD8 *) ucpOut) + channel;

    for (i=0; i<iCnt; i++) {
      int 	iStep = iaQuantStep[iLevel];
      int 	iCode = 0;
      Quant*	tpQ   = &taQuant[iLevel];
      int		iDiff;
      
      switch (cd->fromType) {
      case MU255: 
	iDiff = AF_cvt_u2s[*ucpLocalIn] - iOut; 
	ucpLocalIn += cd->nChannels;
	break;
      case ALAW: 
	iDiff = AF_cvt_a2s[*ucpLocalIn] - iOut; 
	ucpLocalIn += cd->nChannels;
	break;
      case LIN16: 
	iDiff = *spIn - iOut; 
	spIn += cd->nChannels; 
	break;
      default: AoD(0, "Bad fromType in IMA Compression\n");
      }
      
      if (iDiff < 0) 		{ iCode  = 8; iDiff = -iDiff; }
      if (iDiff >= iStep) 	{ iCode += 4; iDiff -= iStep; }
      if (iDiff >= (iStep >> 1))	{ iCode += 2; iDiff -= iStep >> 1; }
      if (iDiff >= (iStep >> 2))	  iCode += 1;
      if (i & 1) {
	*ucpLocalOut = iLast + iCode;
	ucpLocalOut += cd->nChannels;
      }
      else	iLast  = iCode << 4;
      if (iCode & 8) iOut -= tpQ->iaOut[iCode & 7];
      else	   iOut += tpQ->iaOut[iCode & 7];
      iLevel += iaAdj[iCode & 7];
      if (iLevel < 0) 		iLevel = 0;
      else if (iLevel > 88)	iLevel = 88;
      if (iOut > 32767)		iOut = 32767;
      else if (iOut < -32767)	iOut = -32767;
    }
    tpD->iLevel = iLevel;
    tpD->iLast = iLast;
    tpD->iOut = iOut;
  }
}

void AFConvert_UncompressIMA(
			     AFConvert_Data *cd,	/* Current context */
			     void	*ucpIn, 	/* Input buffer */
			     void	*ucpOut,	/* Output buffer */
			     int	iCnt		/* Number of units */
			     )
{
  int i, channel;
  iCnt <<= 1;
  for (channel = 0; channel < cd->nChannels; channel += 1) {
    AF_INT16 *spOut = ((AF_INT16 *) ucpOut) + channel;
    IMAState *tpD = &(((IMAState *) cd->state)[channel]);
    AF_CARD8 *ucpLocalIn = ((AF_CARD8 *) ucpIn) + channel;
    AF_CARD8 *ucpLocalOut = ((AF_CARD8 *) ucpOut) + channel;

    int iOut = tpD->iOut;
    int iLevel = tpD->iLevel;
    int iLast = tpD->iLast;

    for (i=0; i<iCnt; i++) {
      int		iPacked;
      int		iCode;
      Quant*	tpQ	= &taQuant[iLevel];
      
      if (!(i & 1)) {
	iPacked	= *ucpLocalIn;
	ucpLocalIn += cd->nChannels;
	iCode	= iPacked >> 4;
      } else
	iCode	= iPacked;
      if (iCode & 8) iOut -= tpQ->iaOut[iCode & 7];
      else	   iOut += tpQ->iaOut[iCode & 7];
      iLevel += iaAdj[iCode & 7];
      if (iLevel < 0) 		iLevel = 0;
      else if (iLevel > 88)	iLevel = 88;
      if (iOut > 32767) 		iOut   = 32767;
      else if (iOut < -32767)	iOut   = -32767;
      switch (cd->toType) {
      case MU255: 
	*ucpLocalOut = AF_comp_u[(iOut >> 2) & 0x3fff]; 
	ucpLocalOut += cd->nChannels;
	break;
      case ALAW: 
	*ucpLocalOut = AF_comp_a[(iOut >> 2) & 0x3fff]; 
	ucpLocalOut += cd->nChannels;
	break;
      case LIN16: 
	*spOut = iOut; 
	spOut += cd->nChannels;
	break;
      default: AoD(0, "Bad fromType in IMA decompression\n");
      }
    }
    tpD->iOut = iOut;
    tpD->iLevel = iLevel;
    tpD->iLast = iLast;
  }
}


