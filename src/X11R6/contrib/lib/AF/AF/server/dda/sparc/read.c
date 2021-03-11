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
#include <server/include/acstruct.h>
#include "read.h"
#include "max_io.h"
#include "physdevice.h"
#include "pscodec.h"
#include <AF/AFUtils.h>
#include "uatables.h"

void lofi_read();

char localbuf[16*1024];

void 
read_server(time, dp, len, aDev, ac)
ATime time; unsigned char *dp; int len; AudioDevicePtr aDev; ACPtr ac;
{
	unsigned char *sbuf = aDev->recBuf;	/* Base of server buffer. */
	int ssize=aDev->recNSamplesBuf; /* Size of server buffer. 	*/
	unsigned char *sp;		/* Location in buffer. 		*/
	int slen;			/* Block size to read.	  	*/
	extern int start_time;


	/* may need to copy in two steps if straddles end of buffer. */
	slen = ssize - (time % ssize);	/* room to end of buffer in memory. */
	slen = MIN(len, slen);
	sp = &sbuf[time % ssize];

	memcpy(dp,sp,slen);
	
	/* remnants */
	if ((len - slen) > 0){
		if((len-slen)>ssize) 
		  ErrorF("Read server remants larger than server buf, %d of %d.\n",
				len-slen, len);
		else
			memcpy(&dp[slen],sbuf,len-slen);
	}
}


/*
 * Copy "len" sample data from the hardware buffer to the
 * server's buffer at "time".
 */ 
void
read_back(time, len, aDev)
ATime time; int len; AudioDevicePtr aDev;
{
	unsigned char *sbuf=aDev->recBuf; /* Base of server buffer. 	*/
	int ssize=aDev->recNSamplesBuf; /* Size of server buffer. 	*/
	unsigned char *p;		/* Destination in buffer. 	*/
	int slen;			/* Block size to read.	  	*/

	/* may need to copy in two steps if  straddles end of buffer. */
	slen = ssize - (time % ssize);	/* room to end of buffer in memory. */
	slen = MIN(len, slen);
	p = &sbuf[time % ssize];

/*
printf("rb %x %x %d\n", time, p, slen);
*/

	lofi_read(time, p, slen, ((CodecPrivate *)aDev->privPtr)->recBuf);
	
	/* remnants */
	if ((len-slen) > 0){
		if((len-slen)>ssize) 
		  ErrorF("Read server remants larger than server buf, %d of %d.\n",
				len-slen, len);
		else
			lofi_read(time+slen, sbuf, len-slen, 
			((CodecPrivate *)aDev->privPtr)->recBuf);
	}
}
/*
 * will have to change when the ring goes to three entries/word 
 * Something is wrong if nsamples is > LENHWBUF.
 */
#include <sys/ioctl.h>
#include <sys/filio.h>
#include <sys/stropts.h>

char jbuf[8192]; /* junk buffer for a full second */

void
lofi_read (time, dest, nsamples, hwbuf)
ATime time; unsigned char *dest;  int nsamples; 
		unsigned long *hwbuf;
{
	extern int sunfd1;
	int rdata;
	extern int start_time;

	ioctl(sunfd1, FIONREAD, &rdata);
	if(rdata < nsamples) {
/*  printf("missed samples %d %d\n", nsamples, rdata); */
#ifdef PUS
	if(rdata< 240)
		return;
#endif
	nsamples = rdata;
	}
	if(rdata > LENHWBUF) { /* Something wrong - dump a bunch */
	if(rdata > sizeof(jbuf)) {
		ioctl(sunfd1, I_FLUSH, FLUSHR); 
printf("dumped data %d\n", rdata);
		return;
	} else {
	(void)read(sunfd1, jbuf, rdata-nsamples-240); /* sizeof standard buffer */
	}
	}
	(void)read(sunfd1, dest, nsamples);
/* 	printf("read size %d\n", nsamples); */
}
