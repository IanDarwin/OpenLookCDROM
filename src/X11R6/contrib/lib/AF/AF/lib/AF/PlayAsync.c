/*
 * $RCS$
 */

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

#include <stdio.h>
#include "Alibint.h"


static int
#if NeedFunctionPrototypes
PlayChunkAsync(	   AC ac,
                   ATime startTime,
                   int nbytes,
                   unsigned char *buf)
#else
PlayChunkAsync(ac, startTime, nbytes, buf)
AC ac;
ATime startTime;
int nbytes;
unsigned char *buf;
#endif
{
	register AFAudioConn *aud = ac->connection;
	aGetTimeReply reply;
	register aPlaySamplesReq *req;
	long nwritten;

	LockConnection(aud);
	GetReq(PlaySamples, req);
	req->ac = ac->acontext;
	req->startTime = startTime;
	req->nbytes = nbytes;
	req->sampleType = ac->attributes.type;
	req->nchannels = ac->attributes.channels;
	req->length += (req->nbytes+3)>>2;
	req->mask = 0;
        if(ac->attributes.endian == ABigEndian)
                req->mask |= ABigEndianMask;

	req->mask |= ANoTimeReplyMask;
	nwritten = _ASendAsync(aud, buf, (long) req->nbytes);
	UnlockConnection(aud);
	return(nwritten);
}

int
#if NeedFunctionPrototypes
AFPlaySamplesAsync(
		   AC ac,
                   ATime startTime,
                   int nbytes,
                   unsigned char *buf)
#else
AFPlaySamplesAsync(ac, startTime, nbytes, buf)
AC ac;
ATime startTime;
int nbytes;
unsigned char *buf;
#endif
{
    register int  len;
    register int chunk_size;
    register int nsamples;
    register int ssize;
    int nwritten;
    int totalwritten;
/*
  A hack: figure the sample size given the type and number of channels
*/
    switch(ac->attributes.type) {
        case MU255:
        case ALAW:
                ssize = ac->attributes.channels;
                break;
        case LIN16:
                ssize = 2 * ac->attributes.channels;
                break;
        case LIN32:
                ssize = 4 * ac->attributes.channels;
                break;
        default:
		fprintf(stderr,"AIO:  unknown sample type in AFPlaySamples()\n");
                _ADefaultIOError(ac->connection);
    }
    nsamples = nbytes / ssize;
    chunk_size = (MIN(CHUNKSIZE, ac->connection->max_request_size)
			- sz_aPlaySamplesReq) / ssize;

    totalwritten = 0;

    while (nsamples > 0) {
	    len = min(chunk_size, nsamples);
	    nsamples -= len;
	    nwritten = PlayChunkAsync(ac, startTime, len*ssize, buf);
	    if (nwritten >= 0) {
		totalwritten += nwritten;
	    } else {
		/* must be -1 therefore EWOULDBLOCK */
		if (totalwritten == 0) {
		    totalwritten = -1;
		}
		break;
	    }
	    startTime += len;		/* we played that many samples. */
	    buf += len * ssize;
    }
    return (totalwritten);
}
