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

#include <include/audio.h>
#include <server/include/audiodev.h>
#include <server/include/task.h>
#include <server/include/acstruct.h>	/* picks up ac too. */
#include "write.h"
#include "lofi_io.h"
#include "physdevice.h"
#include "hifi.h"
#include "devtime.h"

/*
  Copies samples to the hardware buffer.  The source buffer is stereo (i.e.
  alternating Left and Right samples).
*/
void hifi_copy (ATime time, HSAMP *source, int nsamples, 
		volatile CARD32 *lbuf, volatile CARD32 *rbuf)
{
	register volatile CARD32 *lp; 
	register volatile CARD32 *rp; 
	register int *src = (int *) source;
	unsigned int	tmp;

	if (nsamples > HIFI_HWBUF){
		ErrorF("hifi_copy: nsamples too big, %d.\n",nsamples);
		return;
	}

	time = time % HIFI_HWBUF;
	lp = lbuf + time;
	rp = rbuf + time;
	while(nsamples--) {
#ifdef __alpha
	  if( (((long) lp) & 0x7) == 0)	/* don't lock out the lofi */
		MB();
#endif
	  tmp = *src++;
	  *lp++ = tmp << 16;
	  *rp++ = tmp & 0xffff0000;
	  if(lp >= &lbuf[HIFI_HWBUF]) {
	       	lp = lbuf;
	       	rp = rbuf;
	  }
	}
	MB();
}

/*
  Mix a source buffer (stereo samples) into a destination buffer.  'len' is
  the number of stereo samples.
*/
static void hifi_write_mix(HSAMP *dp, HSAMP *sp, int len)
{
	while(len--) {
		*dp++ += *sp++;
		*dp++ += *sp++;
	}
}

/*
  Write stereo samples into a stereo buffer. 'len' is the number of 
  stereo samples.
*/
static void hifi_write_stereo(HSAMP *dest, HSAMP *src, int len, int preempt)
{
	if(preempt == Preempt) {
		memcpy(dest, src, len * 2 * HSIZE);
	} else {
		while(len--) {
			*dest++ += *src++;
			*dest++ += *src++;
		}
	}
}

/*
  Write mono samples into a stereo buffer. 'len' is the number of 
  mono samples.

  If init is TRUE, we need to initialize the other channel.
*/
static void hifi_write_mono(HSAMP *dest, HSAMP *src, int len,
		int channel, int preempt, int init)
{
	if(!init) {
		dest += channel;
		if(preempt == Preempt) {
			while(len--) {
				*dest = *src++;
				dest += 2;
			}
		} else {
			while(len--) {
				*dest += *src++;
				dest += 2;
			}
		}
	} else {
		if(channel == 0) {
			while(len--) {
				*dest++ = *src++;
				*dest++ = 0;
			}
		} else {
			while(len--) {
				*dest++ = 0;
				*dest++ = *src++;
			}
		}
	}
}

/*
 * Copy "len" samples from the server's buffer at "time" down to the DSP's
 * hardware buffer at "time".  Only valid for the primary stereo device.
 */
void hifi_write_back(ATime time, int len, AudioDevicePtr aDev)
{
	HSAMP *sbuf= (HSAMP *)aDev->playBuf;
	HiFiPrivate *hPtr = (HiFiPrivate *) aDev->privPtr;
	int ssize=aDev->playNSamplesBuf;
	HSAMP *p;
	int slen;

	if (len > HIFI_HWBUF) {
		ErrorF("hifi_write_back: nsamples too big, %d.\n", len);
		return;
	}

	slen = ssize - (time % ssize);	/* room to end of buffer in memory. */
	slen = MIN(len, slen);
	p = sbuf + (time % ssize) * 2;
	hifi_copy(time, p, slen, hPtr->leftPlayBuf, hPtr->rightPlayBuf);
	len -= slen;
	if(len > 0) { 
		hifi_copy(time+slen, sbuf, len, hPtr->leftPlayBuf, 
				hPtr->rightPlayBuf);
	}
}

/*
 * Write samples into the server's buffer.  The source samples may be mono
 * or stereo (type specified by aDev).
 *
 * If the write request falls into the "update region", then that part of
 * the request is written through to the server.
 */
void hifi_write_server(ATime time, HSAMP *p, int len, AudioDevicePtr aDev, 
				int gain, int preempt, int init)
{
	HSAMP *sbuf = (HSAMP *)aDev->playBuf; /* Base of server buffer. */
	HiFiPrivate *hPtr = (HiFiPrivate *) aDev->privPtr;
	int ssize=aDev->playNSamplesBuf; /* Size of server buffer. 	*/
	HSAMP *d;			/* Destination in buffer. 	*/
	int slen;			/* Block size to write.	  	*/
	AudioDevicePtr prim = hPtr->prim;
	int diff;

	/* may need to copy in two steps if source straddles end of buffer. */
	slen = ssize - (time % ssize);	/* room to end of buffer in memory. */
	slen = MIN(len, slen);
	d = sbuf + (time % ssize) * 2;

/* ...When supported, boost client by gain goes here... */

     /*
      * First chunk...
      */
	if(aDev->playNchannels == 2) {			/* stereo copy */
		hifi_write_stereo(d, p, slen, preempt);
		p += slen * 2;
	} else {					/* mono copy */
		hifi_write_mono(d, p, slen, hPtr->channel, preempt, init);
		p += slen;
	}
	
     /*
      * Second chunk...
      */
	if ((len - slen) > 0) {
		if((len-slen)>ssize) 
		  ErrorF("Write server remants larger than server buf %d.\n",
				len-slen);
		else {
			if(aDev->playNchannels == 2)
				hifi_write_stereo(sbuf, p, len-slen, preempt);
			else
				hifi_write_mono(sbuf, p, len-slen, 
					hPtr->channel, preempt, init);
		}
	}

     /*
      * If any of the playback fell into the region *before* 
      * timeNextUpdate, then that region has to be written through to the
      * DSP ring buffers.
      */
	diff = prim->timeNextUpdate - time;
	len = MIN(diff, len);
	if(len > 0)
		hifi_write_back(time, len, prim);
}

/*
  Fill the server buffer with silence.  This routine is just a special case of
  write_server().   This is called from the regular update task for the 
  primary stereo hifi device.
*/
void hifi_silence_fill(ATime time, int len, AudioDevicePtr aDev)
{
	HSAMP *sbuf = (HSAMP *)aDev->playBuf; /* Base of server buffer. */
	int ssize=aDev->playNSamplesBuf; /* Size of server buffer. 	*/
	HSAMP *d;			/* Destination in buffer. 	*/
	int slen;			/* Block size to write.	  	*/

	/* may need to copy in two steps if source straddles end of buffer. */
	slen = ssize - (time % ssize);	/* room to end of buffer in memory. */
	slen = MIN(len, slen);
	d = sbuf + (time % ssize) * 2;

	memset(d, SILENCE, HBYTES(slen)*2);
	
	/* remnants */
	if ((len - slen) > 0){
		if((len-slen)>ssize)
		  ErrorF("Silence fill remants larger than server buf %d of %d.\n",
				len-slen,len);
		else
			memset(sbuf, SILENCE, HBYTES(len-slen)*2);

	}
}

#define	EPSILON	20		/* 0.5 ms */

/* This procedure is invoked by a protocol dispatching routine
 * in the device independent server (in the case of play request with
 * upside down mu-law bytes as the data type) or through the conversion
 * routine (in the case of other than upside down mu-law bytes.)
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
 * thrown away (as if played.)  Data that aligns in time with the server buffer
 * will be written.  Data beyond the end of the playable server buffer
 * is "returned" by returning the number played (nleft = plen - nplayed).
 */
int hifiPlay(ATime ptime, HSAMP *p, int plen, ACPtr ac)
{
    AudioDevicePtr aDev=ac->aDev;
    HiFiPrivate	*hPtr = (HiFiPrivate *) aDev->privPtr;
    AudioDevicePtr	prim = hPtr->prim;
    int future;				/* Flag indicating relative time. */
    int delta;
    int len;				/* Temporary length.	*/
    ATime endBufferTime;		/* ATime at end of playable buffer. */
    ATime time=ptime;			/* Local temporary time.	*/
    int rlen = plen;			/* Local remaining length.	*/
    int preempt;			/* preempt or mix? */

    if (plen <= 0) { ErrorF("hifiPlay: called with len <= 0\n"); return; }

    /*
     * Make sure that update is keeping up.   Play relies on
     * consistent update parameters.
     */
    HIFI_UPDATE_TIME(prim);		/* Snapshot current device time. */
    aDev->time0 = prim->time0;		/* Maintain mono time too. */
    if(FUTURE(prim->time0, prim->timeLastValid)) {
	hifiUpdate(prim);	/* Ensure updated through guardband. */
	HIFI_UPDATE_TIME(prim);	/* Snapshot current device time, again. */
        aDev->time0 = prim->time0;	/* Maintain mono time too. */
    }

/*
  If part of the play request starts in the past, gobble it up.
*/
    future = FUTURE(time, prim->time0); 
    delta = DELTA(future, time, prim->time0);

    if(!future){
	delta += EPSILON;
	time += delta;				/* Move time to time0+e.	*/
	p += delta * aDev->playNchannels;	/* Move data pointer to time0+e base. */
	rlen -= delta;				/* Remaining data after throw away. */
     }else {
     	/* Else make sure time is > current time plus epsilon */
     	if (delta < EPSILON){
		time += EPSILON-delta;
		p += (EPSILON-delta) * aDev->playNchannels;
		rlen -= EPSILON-delta;
        }	
     }
    if (rlen <=0)
	goto done;

/*
  This play routine is responsible for backfilling silence.  timeLastValid
  is the time of the last valid play data.

  If timeLastValid is before the start of the play request, silence fill from 
  now to the start of the play request, set preemptive mode.
*/
	preempt = ac->preempt;
	if(FUTURE(time, prim->timeLastValid)) {
		delta = time - prim->timeLastValid;
		if(delta > 0) {
			hifi_silence_fill(prim->timeLastValid, delta, prim);
		}
		preempt = Preempt;
		prim->timeLastValid = time;
	}

/*
  Copy the part of the play request (if any) that lands in the server
  buffer into the server buffer.  We also have to handle the case where
  timeLastValid is in the middle of the play request:  the first part 
  gets mixed in normally.  The rest must be played Preempt.
*/
    endBufferTime = prim->time0 + prim->playNSamplesBuf;
    if(FUTURE(endBufferTime, time)) {
	delta = endBufferTime - time;
	len = MIN(delta, rlen);
	delta = prim->timeLastValid - time;
	if(delta > 0) {	
		delta = MIN(delta, len);
		hifi_write_server(time, p, delta, aDev, ac->playGain, 
			preempt, 0);
		p += delta * aDev->playNchannels;
		time += delta;
		rlen -= delta;
		len -= delta;
	}
	if(len) {
		hifi_write_server(time, p, len, aDev, ac->playGain, 
			Preempt, 1);
		p += len * aDev->playNchannels;
		time += len;
		rlen -= len;
	}
    }

done:
    if(FUTURE(time, prim->timeLastValid))
	prim->timeLastValid = time;

     if(rlen <= 0)
	return plen;
     else
	return plen-rlen;
}
