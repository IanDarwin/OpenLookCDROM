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
#include "write.h"
#include "lofi_io.h"
#include "physdevice.h"
#include "pscodec.h"
#include <AF/AFUtils.h>
#include "uatables.h"


void lofi_copy (ATime time, unsigned char *source,  int nsamples, volatile CARD32 *hwbuf);
void write_mix(unsigned char *dp, unsigned char *sp, int len);

void 
write_server(ATime time, unsigned char *p, int len, AudioDevicePtr aDev,
	int gain, int preempt)
{
	unsigned char *sbuf = aDev->playBuf;	/* Base of server buffer. */
	int ssize=aDev->playNSamplesBuf; /* Size of server buffer. 	*/
	unsigned char *d;		/* Destination in buffer. 	*/
	int slen;			/* Block size to write.	  	*/
	unsigned char *gp;		/* gain table pointer.		*/
	int i;

	/* may need to copy in two steps if source straddles end of buffer. */
	slen = ssize - (time % ssize);	/* room to end of buffer in memory. */
	slen = MIN(len, slen);
	d = &sbuf[time % ssize];

	/* Boost client data by gain. */
	if(gain != 0) {
		gp = AF_gain_table_u[gain - GAIN_MIN ];
		for(i=0;i<len;++i)
			p[i] = gp[p[i]];
	}

	/* Fill server with client data, mix or preempt as appropriate. */
	if (preempt==Preempt)
		memcpy(d,p,slen);
	else
		write_mix(d,p,slen);
	
	/* remnants */
	if ((len - slen) > 0){
		if((len-slen)>ssize) 
		  ErrorF("Write server remants larger than server buf %d.\n",
				len-slen);
		else {
			if (preempt==Preempt)
				memcpy(sbuf, p+slen, len-slen);
			else
				write_mix(sbuf, p+slen,len-slen);
		}
	}
}

/*
 * Assumes arguments are valid.
 */
void
write_mix(unsigned char *dp, unsigned char *sp, int len)
{
	int i;
	for(i=0;i<len;++i) { *dp = MIX(*sp++, *dp); ++dp; }
}

void write_through(ATime time, unsigned char *p, int len, AudioDevicePtr aDev,
	int gain, int preempt)
{

	/* Mix into server buffers, */
	write_server(time, p, len, aDev, gain, preempt);

	/* and write through to hardware. */
	write_back(time, len, aDev);
}

/*
 * Copy "len" sample data from the server's buffer at "time" to the 
 * hardware buffer at "time".
 */ 
void
write_back(ATime time, int len, AudioDevicePtr aDev)
{
	unsigned char *sbuf=aDev->playBuf; /* Base of server buffer. 	*/
	int ssize=aDev->playNSamplesBuf; /* Size of server buffer. 	*/
	unsigned char *p;		/* Destination in buffer. 	*/
	int slen;			/* Block size to write.	  	*/

	/* may need to copy in two steps if source straddles end of buffer. */
	slen = ssize - (time % ssize);	/* room to end of buffer in memory. */
	slen = MIN(len, slen);
	p = &sbuf[time % ssize];

	lofi_copy(time, p, slen, ((CodecPrivate *)aDev->privPtr)->playBuf);
	
	/* remnants */
	if ((len-slen) > 0)
		lofi_copy(time+slen, sbuf, len-slen, 
			((CodecPrivate *)aDev->privPtr)->playBuf);
}

/* This is just a special case of write_server() */
void
silence_fill(ATime time, int len, AudioDevicePtr aDev)
{
	unsigned char *sbuf = aDev->playBuf;	/* Base of server buffer. */
	int ssize=aDev->playNSamplesBuf; /* Size of server buffer. 	*/
	unsigned char *d;		/* Destination in buffer. 	*/
	int slen;			/* Block size to write.	  	*/

	/* may need to copy in two steps if source straddles end of buffer. */
	slen = ssize - (time % ssize);	/* room to end of buffer in memory. */
	slen = MIN(len, slen);
	d = &sbuf[time % ssize];

	memset(d,SILENCE,slen);
	
	/* remnants */
	if ((len - slen) > 0){
		if((len-slen)>ssize)
		  ErrorF("Silence fill remants larger than server buf %d of %d.\n",
				len-slen,len);
		else
			memset(sbuf,SILENCE,len-slen);

	}
}

/*
 * will have to change when the ring goes to three entries/word 
 * Something is wrong if nsamples is > LENHWBUF.
 */
void
lofi_copy (ATime time, unsigned char *source,  int nsamples, 
		volatile CARD32 *hwbuf)
{
	register int i;
	register volatile CARD32 *dp = &hwbuf[time%LENHWBUF]; 

	if (nsamples > LENHWBUF){
		 ErrorF("lofi_copy: nsamples too big, %d.\n",nsamples);
		return;
	}

	for (i = 0; i < nsamples; i++) {
#ifdef __alpha
		if((((long)dp) & 0x7) == 0)	/* don't lock out the lofi */
			MB();
#endif
		*dp++ = TO_RAM(source[i]);
		if (dp >= &hwbuf[LENHWBUF])
			dp = hwbuf;
	}
	MB();
}

