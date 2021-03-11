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


static ATime
#if NeedFunctionPrototypes
RecordChunkAsync(
		   AC ac,
		   ATime startTime,
		   int nbytes,
	           unsigned char *buf)
#else
RecordChunkAsync(ac, startTime, nbytes, buf)
AC ac;
ATime startTime;
int nbytes;
unsigned char *buf;
#endif
{
	register AFAudioConn *aud = ac->connection;
	aRecordSamplesReply reply;
	register aRecordSamplesReq *req;
	int n_read;

	LockConnection(aud);
	GetReq(RecordSamples, req);
	req->ac = ac->acontext;
	req->startTime = startTime;
	req->nbytes = nbytes;
	req->sampleType = ac->attributes.type;
	req->nchannels = ac->attributes.channels;
	req->mask = 0;
	if(ac->attributes.endian == ABigEndian)
		req->mask |= ABigEndianMask;

 	if (_AReplyAsync(aud, (aRecordSamplesReply *) &reply, 0, aFalse) == -1) {
	    n_read = -1;
	    goto skip_read;
	}
	nbytes = MIN(nbytes, reply.length * 4);
	n_read = _AReadPadAsync(aud, buf, nbytes);
skip_read:
	UnlockConnection(aud);
	return(n_read);
}

ATime
#if NeedFunctionPrototypes
AFRecordSamplesAsync(
		   AC ac,
		   ATime startTime,
		   int nbytes,
	           unsigned char *buf)
#else
AFRecordSamplesAsync(ac, startTime, nbytes, buf)
AC ac;
ATime startTime;
int nbytes;
unsigned char *buf;
#endif
{
    register int  len;
    register ATime dtime;
    register int chunk_size;
    register int nsamples;
    register int ssize;
    register int delta;
    int total_read = 0;
    int n_read;

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
		fprintf(stderr,"AIO:  unknown sample type in AFRecordSamples()\n");
		_ADefaultIOError(ac->connection);
    }
    nsamples = nbytes / ssize;
    chunk_size = (MIN(CHUNKSIZE, ac->connection->max_request_size)
			 - sz_aRecordSamplesReq) / ssize;

    while (nsamples > 0) {
	    len = min(chunk_size / ssize, nsamples);
	    n_read = RecordChunkAsync(ac, startTime, len*ssize, buf);

	    if (n_read > 0) {
		total_read += n_read;
	    } else if (n_read == -1) {
		/* assume we encountered EWOULDBLOCK */
		if (total_read <= 0) {
		    total_read = -1;
		}
		break;
	    }

	    nsamples -= len;
	    buf += len * ssize;
	    startTime += len;	/* we played that many samples. */
    }
    return (total_read);
}
