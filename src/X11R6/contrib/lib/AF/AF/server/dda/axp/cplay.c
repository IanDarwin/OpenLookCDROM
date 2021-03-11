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
static char cplay_c_rcsid[]="$Header: /crl/audio/AF/server/dda/axp/RCS/cplay.c,v 1.8 1993/11/15 21:19:36 tml Exp $";
#endif

#include <include/audio.h>
#include <server/include/acstruct.h>
#include <server/include/task.h>
#include <io/dec/tc/amd79c30.h>
#include "ringbuffers.h"
#include "physdevice.h"
#include "max_io.h"
#include "write.h"
#include "pscodec.h"
#include "devtime.h"

#define	EPSILON	BBA_DMASIZE


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
int
codecPlay(ptime, p, plen, ac)
ATime ptime; 
unsigned char *p; 
int plen; 
ACPtr ac;
{
    AudioDevicePtr aDev=ac->aDev;
    int future;				/* Flag indicating relative time. */
    int delta,udelta,edelta;		/* */
    int len;				/* Temporary length.	*/
    ATime endBufferTime;			/* ATime at end of playable buffer. */
    ATime time=ptime;			/* Local temporary time.	*/
    int rlen = plen;			/* Local remaining length.	*/

    if (plen <= 0) { ErrorF("codecPlay: called with len <= 0\n"); return; }
/*
printf("cp %x %d\n", ptime, plen);
*/
    /*
     * Make sure that update is keeping up.   Play relies on
     * consistent update parameters.
     */
    UPDATE_TIME(aDev);		/* Snapshot current device time. */
    /* Update the view of current time
     * and determine whether request is in the future or past,
     * then compute an absolute delta in time of the requested time
     * to current time.
     */
    future = FUTURE(time, aDev->time0); 
    delta = DELTA(future, time, aDev->time0);

    if(!future){
       	/* Consume all or part of the play data until
       	 * end reached or at current time.
       	 */
	delta += EPSILON;
	time += delta;			/* Move time to time0+e.	*/
	p += delta;			/* Move data pointer to time0+e base. */
	rlen -= delta;			/* Remaining data after throw away. */
     }else {
     	/* Else make sure time is > current time plus epsilon */
     	if (delta < EPSILON){
		time += EPSILON-delta;
		p += EPSILON-delta;
		rlen -= EPSILON-delta;
        }	
     }
    if (rlen <=0) goto done;

    /* If all goes well, the task will only be playing beyond 
     * time0+epsilon here.
     */

    /*
     * First check to see if play request is earlier than the 
     * time at which the buffer has already been copied to the hardware.
     * If so, we must write through the server's buffer into the 
     * hardware.
     * Note:
     *  What if the server update is not keeping up and codecPlay()
     *  samples timeNextUpdate before "reset" by update proc?
     * This routine will "assist" the update proc.
     */
    udelta = DELTA(1, aDev->timeNextUpdate, aDev->time0);
    delta = DELTA(1, time, aDev->time0);
    if (delta < udelta){
	/* Begin time is in update interval. Only copy up to time HW 
	 * consistent.
	 */
	len = MIN(rlen, udelta-delta);
	write_through(time, p, len, aDev, ac->playGain, ac->preempt);
	p += len; 	time += len;	rlen -= len;
    }
    if (rlen <=0) goto done;

    /* Compute useful indicators. 
     * Determine how much more to copy at this time.
     */
    delta = DELTA(1, time, aDev->time0);
    endBufferTime = aDev->timeLastUpdated + aDev->playNSamplesBuf;
    edelta = DELTA(1, endBufferTime, aDev->time0);
    if (delta < edelta){
    	/* Start time is beyond update time, within server's buffer. */
	len = MIN(rlen, edelta-delta);
	write_server(time, p, len, aDev, ac->playGain, ac->preempt);
	p += len;	time += len;	rlen -= len;
    }
    /* Common exit point.
     * At this point play data is written until a reasonable time in
     * the future and/or the play data has been completely consumed.
     */
done:
     if(rlen <= 0) 
	return plen;
     else 
	return plen-rlen;
}

