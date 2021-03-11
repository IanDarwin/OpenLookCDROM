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
static char read_c_rcsid[]="$Header: /crl/audio/AF/server/RCS/read.c,v 1.11 1993/11/17 20:09:37 tml Exp $";
#endif

#include <sys/types.h>
#include <sys/ioctl.h>
#include <sys/signal.h>
#include <stropts.h>
#include <stdio.h>
#include <errno.h>

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

void
lofi_read (time, dest, nsamples, hwbuf)
ATime time; unsigned char *dest;  int nsamples; 
		unsigned long *hwbuf;
{
	int result;
	extern int axpfd;
	int rdata=0;
	int n= 0;
	static int last_ttime = 0;
	extern int errno;

	n = ioctl(axpfd, I_NREAD, &rdata);
	/*
	 * This ioctl returns the number of streams messages
	 * in the Q (not the total length). A little unfortunate
	 * as FIONREAD seems not to work at all. So for the time
	 * being we take a stab at how much audio info is in each
	 * message and try not to let us come up short or allow too
	 * much to pile up.
	 */
	if(n < 2) {
		return;
	}
	if(n > 15) {
		ioctl(axpfd, I_FLUSH, FLUSHR);
		return;
	}
	result = read(axpfd, dest, nsamples);
	last_ttime = start_time;
}
