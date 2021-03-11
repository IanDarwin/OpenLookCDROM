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
static char read_c_rcsid[]="$Header: /crl/audio/AF/server/dda/jv/RCS/hrecord.c,v 1.8 1993/11/15 21:20:39 tml Exp $";
#endif

#include <include/audio.h>
#include <server/include/acstruct.h>
#include "jv_io.h"
#include "physdevice.h"
#include <server/include/task.h>
#include "devtime.h"
#include "hifi.h"


/*
  Sets the specified number of samples to silence.
*/
void hifi_silence(HSAMP *dest, int len)
{
	while(len) {
		*dest++ = 0;
		len--;
	}
}

/*
  Reads the specified number of samples from the Hifi's hardware buffer.
  The source buffers are single channel buffers, the destination buffer
  is stereo (alternating left/right samples).
*/
void hifi_read(ATime time, HSAMP *dest,  int nsamples, 
		volatile CARD32 *lbuf, volatile CARD32 *rbuf)
{
	register volatile CARD32 *lp; 
	register volatile CARD32 *rp; 

	if (nsamples > HIFI_HWBUF){
		 ErrorF("hifi_read: nsamples too big, %d.\n",nsamples);
		return;
	}

	time = time % HIFI_HWBUF;
	lp = lbuf + time;
	rp = rbuf + time;

	while(nsamples--) {
		*dest++ = *lp++ >> 16;
		*dest++ = *rp++ >> 16;
		if(lp >= &lbuf[HIFI_HWBUF]) {
			lp = lbuf;
			rp = rbuf;
		}
	}
}

/*
  Read specified number of samples from the hifi record buffer (NOT the
  hardware buffer).  'len' is in samples.  The destination may be mono or 
  stereo (specified by aDev).
*/
void hifi_read_server(ATime time, HSAMP *dp, int len, AudioDevicePtr aDev, 
			ACPtr ac)
{
	HSAMP *sbuf = (HSAMP *)aDev->recBuf; /* Base of server buffer. 	*/
	HiFiPrivate *hPtr = (HiFiPrivate *) aDev->privPtr;
	int ssize=aDev->recNSamplesBuf; /* Size of server buffer. 	*/
	HSAMP *sp;			/* Location in buffer. 		*/
	int slen;			/* Block size to read.	  	*/
	int	i;

	/* may need to copy in two steps if straddles end of buffer. */
	slen = ssize - (time % ssize);	/* room to end of buffer in memory. */
	slen = MIN(len, slen);
	sp = sbuf + (time % ssize) * 2;

	if(aDev->recNchannels == 2) {
		memcpy(dp, sp, HBYTES(slen) * 2);
		dp += slen * 2;
	} else {
		sp += hPtr->channel;
		i = slen;
		while(i--) {			/* pick up a single channel */
			*dp++ = *sp;
			sp += 2;
		}
	}

	/* remnants */
	if ((len - slen) > 0){
		if((len-slen)>ssize) 
		  ErrorF("Read server remants larger than server buf, %d of %d.\n",
				len-slen, len);
		else {
			if(aDev->recNchannels == 2)
				memcpy(dp, sbuf, HBYTES(len-slen)*2);
			else {
				sp = sbuf + hPtr->channel;
				i = len - slen;
				while(i--) {
					*dp++ = *sp;
					sp += 2;
				}
			}
		}
	}
}

/*
 * Copy "len" sample data from the hardware buffer to the
 * server's buffer at "time".  Only valid for the primary stereo device.
 */ 
void hifi_read_back(ATime time, int len, AudioDevicePtr aDev)
{
	HSAMP *sbuf= (HSAMP *)aDev->recBuf; /* Base of server buffer. 	*/
	int ssize=aDev->recNSamplesBuf; /* Size of server buffer. 	*/
	HSAMP *p;			/* Destination in buffer. 	*/
	int slen;			/* Block size to read.	  	*/

	/* may need to copy in two steps if  straddles end of buffer. */
	slen = ssize - (time % ssize);	/* room to end of buffer in memory. */
	slen = MIN(len, slen);
	p = sbuf + (time % ssize) * 2;

	hifi_read(time, p, slen, ((HiFiPrivate *)aDev->privPtr)->leftRecBuf,
			((HiFiPrivate *)aDev->privPtr)->rightRecBuf);
	
	/* remnants */
	if ((len-slen) > 0){
		if((len-slen)>ssize) 
		  ErrorF("Read server remants larger than server buf, %d of %d.\n",
				len-slen, len);
		else
			hifi_read(time+slen, sbuf, len-slen, 
				((HiFiPrivate *)aDev->privPtr)->leftRecBuf,
				((HiFiPrivate *)aDev->privPtr)->rightRecBuf);
	}
}


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
    HiFiPrivate	*hPtr = (HiFiPrivate *) aDev->privPtr;
    AudioDevicePtr prim=hPtr->prim;
    int future;				/* Flag indicating relative time. */
    int delta;
    int len;				/* Temporary length.	*/
    ATime begTime;			/* Beginning time of rec buf.	*/
    ATime time=rtime;			/* Local temporary time.	*/
    int remlen = rlen;			/* Local remaining length.	*/

    if (rlen <= 0) { ErrorF("codecRecord: called with len <= 0\n"); return 0; }

/* set record hints */
    if(!ac->recRef) {
	ac->recRef = TRUE;
	prim->recRefCount++;
    }

/* If the start of the request is in the future, return nothing */
    HIFI_UPDATE_TIME(prim);
    aDev->time0 = prim->time0;
    if(FUTURE(time, prim->time0))
	return 0;

/* If the end of the request is beyond the time last updated, call
   hifiRecUpdate() to bring in the latest samples
*/ 
    if(FUTURE(time + rlen, prim->timeRecLastUpdated))
    	hifiRecUpdate(prim);

    begTime = prim->timeRecLastUpdated - prim->recNSamplesBuf;
    future = FUTURE(time, begTime); 
    delta = DELTA(future, time, begTime);

    if(!future){			/* Start time in past. */
       	/* Fill buffer with silence for part before begTime. */
	len = MIN(remlen, delta);
	hifi_silence(dp, len * aDev->recNchannels);
	dp += len * aDev->recNchannels; 
	time += len; 
	remlen -= len;
	if (remlen <= 0) 
		return rlen;
    }

    /* If all goes well, the start time will be in the valid record
     * time range.
     */
    delta = DELTA(1, time, begTime);
    len = MIN(remlen, prim->recNSamplesBuf - delta);
    hifi_read_server(time, dp, len, aDev, ac);
    dp += len * aDev->recNchannels; 
    time += len; 
    remlen -= len;

    if (remlen <=0) 
	return rlen;

    return rlen-remlen;
}
