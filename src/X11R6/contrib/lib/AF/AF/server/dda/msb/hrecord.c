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
#if !defined(lint) && !defined(SABER)
static char read_c_rcsid[]="$Header: /crl/audio/AF/server/dda/msb/RCS/hrecord.c,v 1.4 1993/11/29 22:37:00 stewart Exp $";
#endif

#include <stdio.h>
#include <string.h>
#include <include/audio.h>
#include <server/include/audiodev.h>
#include <server/include/acstruct.h>
#include <server/include/ac.h>
#include "msbdda.h"



/* This procedure is invoked by a protocol dispatching routine
 * in the device independent server (in the case of record request with
 * upside down mu-law bytes as the data type) or through the conversion
 * routine (in the case of other than upside down mu-law bytes.)
 *
 * Input:
 *  ptime	audio device time at which to begin recording first sample.
 *		time can be anywhere in range [-3.1, 3.1) days (at 8KHz.)
 *  plen	nsamples to attempt to record, starting at ptime.  Since
 *		it is assumed the record stream is chunking, plen had 
 *		better not be larger than the server's buffer.
 *  p		pointer to buffer to hold record data.
 *  aDev	audio device struct.
 *  ac		Audio context pointer.
 *
 * The caller's buffer is aligned in "time" and compared to the current
 * state of the audio device's record buffer.  Silence is returned in 
 * the past.  Data that aligns in time with the server buffer
 * will be written to the buffer.  
 * Attempt to record in the future will return nrecorded of 0.
 *
 * The time line can be viewed as:
 *                             Valid record data          t0
 * -----------------[----------------------------------]--|-----------
 *                  |                    t(last update)|
 *
 */
int hifiRecord(ATime rtime, HSAMP *dp, int rlen, ACPtr ac)
{
  AudioDevicePtr aDev=ac->aDev;
  msbPhysDevice *pDev = (msbPhysDevice *) aDev->devPtr;
  HiFiPrivate	*hPtr = (HiFiPrivate *) aDev->privPtr;
  int len;				/* Temporary length.	        */
  int remlen = rlen;			/* Local remaining length.	*/
  int lastUpdate = pDev->hR.baseTime + pDev->hR.size;

  if (rlen <= 0) { ErrorF("codecRecord: called with len <= 0\n"); return 0; }
  
  /* set record hints */
  if(!ac->recRef) {
    ac->recRef = TRUE;
    pDev->recRefCount++;
    if (!pDev->recDMARunning) msb_record_start(pDev);
  }
  
  HIFI_UPDATE_TIME(pDev);
  aDev->time0 = pDev->time0;

  if (pDev->eventLog) printf("Record %d %d %d\n", pDev->time0, rtime, rlen);  

  /* If the start of the request is in the future, return nothing */
  if(AFTER(rtime, pDev->time0)) return 0;
  
  /* If the end of the request is beyond the time last updated, call
   * hifiRecUpdate() to bring in the latest samples
   */ 
  if(AFTER(rtime + rlen, lastUpdate)) hifiRecUpdate(pDev);
  /* begTime is the ATime of the oldest sample in the server buffer */
  lastUpdate = pDev->hR.baseTime + pDev->hR.size;
  
  if(BEFORE(rtime, pDev->timeEarliestValid)){	/* Start time in past. */
    /* Fill buffer with silence for part before begTime. */
    len = min(remlen, DIFF(pDev->timeEarliestValid, rtime));
    memset(dp, SILENCE, len * aDev->recNchannels * sizeof(HSAMP));
    dp += len * aDev->recNchannels; 
    rtime += len; 
    remlen -= len;
    if (remlen <= 0) return rlen;
  }
  
  /* If all goes well, the start time will be in the valid record
   * time range.
   */
  len = min(remlen, pDev->sR.size);
  len = min(len, DIFF(lastUpdate, rtime));
  if (len > 0) {
    if (pDev->eventLog) printf("RecCopy %d %d\n", rtime, len);
    if (aDev->recNchannels == 2) { /* stereo device */
      ReadStereo(&pDev->sR, rtime, (CARD32 *) dp, len, ac->recGainMul);
    }
    else {  /* mono copy */
      ReadMono(&pDev->sR, rtime, dp, len, hPtr->channel, ac->recGainMul);
    }
  }
  remlen -= len;
  
  if (remlen <=0) 
    return rlen;
  
  return rlen-remlen;
}
