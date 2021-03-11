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
static char write_c_rcsid[]="$Header: /crl/audio/AF/server/dda/msb/RCS/hplay.c,v 1.4 1993/11/29 22:37:00 stewart Exp $";
#endif

#include <string.h>
#include <include/audio.h>
#include <server/include/audiodev.h>
#include <server/include/misc.h>
#include "msbdda.h"
#include <server/include/ac.h>
#include <server/include/acstruct.h>
#include <stdio.h>

/* This procedure is invoked by a protocol dispatching routine
 * in the device independent server or through the conversion
 * routine (in the case of a non-native datatype from the client)
 *
 * Input:
 *  ptime	audio device time at which to begin playing first sample.
 *		time can be anywhere in range [-3.1, 3.1) days (at 8KHz.)
 *  plen	nsamples to attempt to play, starting at ptime.  Since
 *		it is assumed the play stream is chunking, plen had 
 *		better not be larger than the server's buffer.
 *  p		pointer to beginning of array of samples to play.
 *  aDev	audio device struct.
 *  ac		Audio context pointer.
 *
 * The caller's buffer is aligned in "time" and compared to the current
 * state of the audio device's play buffer.  Data in the past is 
 * thrown away (as if played.)  Data aligned with the server buffer
 * will be written.  Data beyond the end of the playable server buffer
 * is "returned" by returning the number played (nleft = plen - nplayed).
 */
int hifiPlay(ATime ptime, HSAMP *p, int plen, ACPtr ac)
{
  AudioDevicePtr aDev=ac->aDev;
  msbPhysDevice *pDev = (msbPhysDevice *) aDev->devPtr;
  HiFiPrivate *hPtr = (HiFiPrivate *) aDev->privPtr;
  int length;				/* Temporary length.	        */
  int rlen = plen;			/* Local remaining length.	*/
  int preempt;                          /* preempt or mix?              */
  int init;                             /* is sw buffer initialized ?   */
  
  if (plen <= 0) {
    ErrorF("hifiPlay: called with len <= 0\n");
    return (plen);
  }

  if (!pDev->playDMARunning) {
    HIFI_UPDATE_TIME(pDev);
    msb_playback_start(pDev);
  }

  if (pDev->eventLog) printf("PlayIn %d %d %d %d\n", 
			     pDev->time0, ptime, plen, 
			     pDev->timeLastValid);
  
  /* The hardware buffer starts at pDev->hP.baseTime for pDev->hP.size
   * The server buffer starts at pDev->hP.baseTime + pDev->hP.size
   * for aDev->sP.size.  hP.baseTime is the really fundamental
   * value, others are derived.
   */
  pDev->sP.baseTime = pDev->hP.baseTime + pDev->hP.size;
  
  while(rlen > 0) {
    preempt = ac->preempt;  /* default, client requested */
    init = 0;
    if(BEFORE(ptime, pDev->hP.baseTime)) {
      /*
       * The play request starts in the past.  Gobble up the
       * part of the request before the hardware buffer starts.
       */
      length = min(rlen, DIFF(pDev->hP.baseTime, ptime));
    }
    else if (BEFORE(ptime, pDev->sP.baseTime)) {
      /*
       * The play reqeust starts in the hardware buffer.  Copy
       * part or all of it.
       */
      length = min(rlen, DIFF(pDev->sP.baseTime, ptime));

      if(aDev->playNchannels == 2) {
	if (pDev->eventLog) printf("WriteStereo %d %d %d %d\n", 
				   ptime, length, preempt, init);
	WriteStereo(&pDev->hP, ptime, (CARD32 *) p, length, 
		    preempt | init, ac->playGainMul);
      } else {
	if (pDev->eventLog) printf("WriteMono %d %d %d %d\n", 
				   ptime, length, preempt, init);
	WriteMono(&pDev->hP, ptime, p, length, hPtr->channel, 
		  preempt, init, ac->playGainMul);
      }
    }
    else if (BEFORE(ptime, pDev->sP.baseTime + pDev->sP.size)) {
      /*
       * The play request starts in the server buffer.  If the client
       * request starts AFTER timeLastValid, then fill the intervening
       * space with Silence.  Then handle the play request
       */
      length = min(rlen, DIFF(pDev->sP.baseTime + pDev->sP.size, ptime));
      if (!AFTER(pDev->timeLastValid, ptime)) {
	KEEPUP(pDev->timeLastValid, pDev->sP.baseTime);
	preempt = Preempt;
	init = 1;
	/* Fill silence in the server buffer between timeLastValid and
         * the start of the client request
         */
	WrapZero(&pDev->sP, pDev->timeLastValid, 
		 DIFF(ptime,  pDev->timeLastValid));
	pDev->timeLastValid = ptime;
      }
      /* Now we know where the data goes, and so far we won't cross
       * a buffer boundary.  Now we make sure not to cross timeLastValid
       */
      if (AFTER(pDev->timeLastValid, ptime)) 
	length = min(length, DIFF(pDev->timeLastValid, ptime));
      if(aDev->playNchannels == 2) {
	if (pDev->eventLog) printf("WriteStereo %d %d %d %d\n", 
				   ptime, length, preempt, init);
	WriteStereo(&pDev->sP, ptime, (CARD32 *) p, length, 
		    preempt | init, ac->playGainMul);
      } else {
	if (pDev->eventLog) printf("WriteMono %d %d %d %d\n", 
				   ptime, length, preempt, init);
	WriteMono(&pDev->sP, ptime, p, length, hPtr->channel, 
		  preempt, init, ac->playGainMul);
      }
    }
    else break; /* client request is after all buffers */

    p += length * aDev->playNchannels; /* Advance data pointer.    */
    ptime += length;
    rlen -= length;
    /* timeLastValid will at least be the end of this part of the
     * client request
     */
    KEEPUP(pDev->timeLastValid, ptime);
  }
  
  if(rlen <= 0) return plen;
  else return plen-rlen;
}

