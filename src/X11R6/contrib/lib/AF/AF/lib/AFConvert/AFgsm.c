static char     rcsid[] = "$RCSfile: AFgsm.c,v $, $Revision: 1.2 $, $Date: 1994/02/15 13:56:51 $";

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
 * FILE NAME		: $RCSfile: AFgsm.c,v $
 * AUTHOR		: $Author: tml $
 * DATE			: $Date: 1994/02/15 13:56:51 $
 * REVISION		: $Revision: 1.2 $
 * DESCRIPTION		: Generate 16 bit 16khz audio from 8/8 ulaw
 *
 * REVISION HISTORY
 *
 * $Log: AFgsm.c,v $
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
 * Changed to Gsmkefile build
 *
 * Revision 1.2  1993/08/31  20:44:50  wecker
 * Added GSM code
 *
 ************************************************************************/

#include "AFgsm.h"
#include "AFClientTypes.h"
#include "audio.h"
#include <server/include/misc.h>
#include <server/include/ac.h>
#include "AFUtils.h"
#include "gsm.h"
#include "private.h"
#include "malloc.h"
		
void AFConvert_InitGSM(AFConvert_Data *cd, AFConvertCmd cmd)
{
  gsm handle = (gsm) cd->state;
  int i;
  if (cmd == ALLOC) {
    handle = (gsm) malloc(sizeof(struct gsm_state) * cd->nChannels);
    AoD(handle != NULL, "Can't alloc GSM Conversion structure\n");
    cd->state = handle;
  }
  if ((cmd == ALLOC) || (cmd == RESET)) {
    for (i = 0; i < cd->nChannels; i += 1)
      {
	handle = &((gsm) cd->state)[i];
	memset ((char *) handle, 0, sizeof(struct gsm_state));
	handle->nrp = 40;
      }
  }
  if (cmd == DEALLOC) {
    if (cd->state != NULL) free(cd->state);
    cd->state = NULL;
  }
}

void AFConvert_CompressGSM(
			   AFConvert_Data *cd,
			   void	*ucpIn,	         	/* Input buffer */
			   void	*ucpOut,	        /* Output buffer */
			   int	iCnt           		/* Number of units */
			   )
{
  int channel, frame, samp;
  iCnt /= 33;

  if ((cd->nChannels == 1) && (cd->fromType == LIN16)) {
    gsm handle = (gsm) cd->state;
    gsm_signal *src = (gsm_signal *) ucpIn;
    gsm_byte *dst = (gsm_byte *) ucpOut;
    for (frame = 0; frame < iCnt; frame += 1) {  /* gsm block */
      gsm_encode(handle, src, dst);
      src += 160;
      dst += 33;
    }
  }
  else {
    for (channel = 0; channel < cd->nChannels; channel += 1) {
      gsm handle = &(((gsm) cd->state)[channel]);
      gsm_signal src[160];
      AF_CARD8 *ucpLocalIn = ((AF_CARD8 *) ucpIn) + channel;
      gsm_signal *spIn = ((AF_INT16 *) ucpIn) + channel;
      gsm_byte *dst = (gsm_byte *) ucpOut + (channel * 33);
      for (frame = 0; frame < iCnt; frame += 1) {  /* gsm block */
	switch (cd->fromType) {
	case MU255: 
	  for (samp = 0; samp < 160; samp += 1) {
	    src[samp] = AF_cvt_u2s[*ucpLocalIn]; 
	    ucpLocalIn += cd->nChannels;
	  }
	  break;
	case ALAW: 
	  for (samp = 0; samp < 160; samp += 1) {
	    src[samp] = AF_cvt_a2s[*ucpLocalIn]; 
	    ucpLocalIn += cd->nChannels;
	  }
	  break;
	case LIN16: 
	  for (samp = 0; samp < 160; samp += 1) {
	    src[samp] = *spIn; 
	    spIn += cd->nChannels; 
	  }
	  break;
	default: AoD(0, "Bad fromType in GSM Compression\n");
	}
	gsm_encode(handle, src, dst);
	dst += (cd->nChannels * 33);
      } 
    }
  }
}

void AFConvert_UncompressGSM(
			     AFConvert_Data *cd,	/* Current context */
			     void	*ucpIn, 	/* Input buffer */
			     void	*ucpOut,	/* Output buffer */
			     int	iCnt		/* Number of units */
			     )
{
  int frame, channel, samp;
  if ((cd->nChannels == 1) && (cd->toType == LIN16)) {
    gsm handle = (gsm) cd->state;
    gsm_byte *src = (gsm_byte *) ucpIn;
    gsm_signal *dst = (gsm_signal *) ucpOut;
    for (frame = 0; frame < iCnt; frame += 1) {
      gsm_decode(handle, src, dst);
      src += 33;
      dst += 160;
    }
  }
  else {
    for (channel = 0; channel < cd->nChannels; channel += 1) {
      gsm handle = &(((gsm) cd->state)[channel]);
      gsm_byte *src = (gsm_byte *) ucpIn + (channel * 33);
      gsm_signal dst[160];
      AF_CARD8 *ucpLocalOut = ((AF_CARD8 *) ucpOut) + channel;
      gsm_signal *spOut = ((AF_INT16 *) ucpOut) + channel;
      for (frame = 0; frame < iCnt; frame += 1) {  /* gsm block */
	gsm_decode(handle, src, dst);
	src += (33 * cd->nChannels);
	switch (cd->toType) {
	case MU255: 
	  for (samp = 0; samp < 160; samp += 1) {
	    *ucpLocalOut = AF_comp_u[(dst[samp] >> 2) & 0x3fff]; 
	    ucpLocalOut += cd->nChannels;
	  }
	  break;
	case ALAW: 
	  for (samp = 0; samp < 160; samp += 1) {
	    *ucpLocalOut = AF_comp_a[(dst[samp] >> 2) & 0x3fff]; 
	    ucpLocalOut += cd->nChannels;
	  }
	  break;
	case LIN16: 
	  for (samp = 0; samp < 160; samp += 1) {
	    *spOut = dst[samp]; 
	    spOut += cd->nChannels; 
	  }
	  break;
	default: AoD(0, "Bad fromType in GSM Decompression\n");
	}
      } 
    }
  }
}


