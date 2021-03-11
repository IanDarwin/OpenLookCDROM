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
static char write_c_rcsid[]="$Header: /crl/audio/AF/server/dda/conf/RCS/hplay.c,v 1.2 1994/02/01 17:52:30 stewart Exp $";
#endif

#include <string.h>
#include <include/audio.h>
#include <server/include/audiodev.h>
#include <server/include/misc.h>
#include "confdda.h"
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
  confPhysDevice *pDev = (confPhysDevice *) aDev->devPtr;
  int i, used;
  int minused = plen + 1;
  /* play into each of the OTHER ac's */
  for (i = 0; i < MAXCONF; i += 1) {
    if (ac == pDev->ac[i]) continue;
    if (pDev->ac[i] != NULL) {
      used = hifiEachPlay(ptime, p, plen, pDev->ac[i]);
      if (used < minused) minused = used;
    }
  }
  /* if there has been no change to minused, then there are no 
   * other AC's so just discard.
   */
  if (minused == plen + 1) minused = plen;
  return(minused);
}

int hifiEachPlay(ATime ptime, HSAMP *p, int plen, ACPtr ac)
{
  AudioDevicePtr aDev=ac->aDev;
  confPhysDevice *pDev = (confPhysDevice *) aDev->devPtr;
  HiFiPrivate *hPtr = (HiFiPrivate *) aDev->privPtr;
  ACPrivate *acp = (ACPrivate *) ac->privPtr;
  int length;				/* Temporary length.	        */
  int remlen = plen;			/* Local remaining length.	*/
  int preempt;                          /* preempt or mix?              */
  int init;                             /* is sw buffer initialized ?   */
  
  if (plen <= 0) {
    ErrorF("hifiPlay: called with len <= 0\n");
    return (plen);
  }
  if (pDev->eventLog) printf("PlayIn %d %d %d %d\n", 
			     pDev->time0, ptime, plen, 
			     acp->timeLastValid);
  
  while(remlen > 0) {
    preempt = ac->preempt;  /* default, client requested */
    init = 0;
    if(BEFORE(ptime, acp->ring.baseTime)) {
      /*
       * The play request starts in the past.  Gobble up the
       * part of the request before the hardware buffer starts.
       */
      length = min(remlen, DIFF(acp->ring.baseTime, ptime));
    }
    else if (BEFORE(ptime, acp->ring.baseTime + acp->ring.size)) {
      /*
       * The play request starts in the server buffer.  Limit the
       * length to the space remaining in the buffer.
       */
      length = min(remlen, DIFF(acp->ring.baseTime + acp->ring.size, ptime));
      /*
       * If this play request predates any other, then limit the
       * length to the space before timeEarliestValid and switch to
       * preempt mode
       */
      if (BEFORE(ptime, acp->timeEarliestValid)) {
	length = min(length, DIFF(acp->timeEarliestValid, ptime));
	preempt = Preempt;
	init = 1;
        /*
	 * if the END of the play is STILL earlier than timeEarliestValid
         * then fill the intervening space with silence
         */
	if (BEFORE(ptime + length, acp->timeEarliestValid)) {
	  if (pDev->eventLog) printf("PlayPreValid %d %d\n", ptime, length);
	  WrapZero(&acp->ring, ptime + length, 
		   DIFF(acp->timeEarliestValid, ptime + length));
	}
	/*
	 * regardless, timeEarliestValid is now the beginning of this req
         */
	acp->timeEarliestValid = ptime;
      }
      /*
       * If the client request starts AFTER timeLastValid, then fill 
       * the intervening space with Silence and switch to prempt mode.
       */
      if (!AFTER(acp->timeLastValid, ptime)) {
	preempt = Preempt;
	init = 1;
	if (pDev->eventLog) printf("PlayPostValid %d %d\n", 
				   acp->timeLastValid, 
				   DIFF(ptime,  acp->timeLastValid));
	WrapZero(&acp->ring, acp->timeLastValid, 
		 DIFF(ptime,  acp->timeLastValid));
	acp->timeLastValid = ptime;
      }
      /* Now we know where the data goes, and so far we won't cross
       * a buffer boundary.  Now we make sure not to cross timeLastValid
       */
      if (AFTER(acp->timeLastValid, ptime))
	length = min(length, DIFF(acp->timeLastValid, ptime));
      if (length > 0) {
	if(aDev->playNchannels == 2) {
	  if (pDev->eventLog) printf("WriteStereo %d %d %d %d\n", 
				     ptime, length, preempt, init);
	  WriteStereo(&acp->ring, ptime, (CARD32 *) p, length, 
		      preempt | init, ac->playGainMul);
	} else {
	  if (pDev->eventLog) printf("WriteMono %d %d %d %d\n", 
				     ptime, length, preempt, init);
	  WriteMono(&acp->ring, ptime, p, length, hPtr->channel, 
		    preempt, init, ac->playGainMul);
	}
      }
    }
    else break; /* client request is after all buffers */

    p += length * aDev->playNchannels; /* Advance data pointer.    */
    ptime += length;
    remlen -= length;
    /* timeLastValid will at least be the end of this part of the
     * client request
     */
    KEEPUP(acp->timeLastValid, ptime);
  }
  
  if(remlen <= 0) return plen;
  else return plen-remlen;
}

