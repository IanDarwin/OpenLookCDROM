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
#include "AFUtils.h"

static ATime
RecordChunk(
		   AC ac,
		   ATime startTime,
		   int nbytes,
	           unsigned char *buf,
		   ABool block)
{
	register AFAudioConn *aud = ac->connection;
	aRecordSamplesReply reply;
	register aRecordSamplesReq *req;

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

	if(block == ABlock)
		req->mask |= ABlockMask;

 	_AReply(aud, (aReply *) &reply, 0, aFalse);
	nbytes = MIN(nbytes, reply.length * 4);
	_AReadPad(aud, (char *)buf, nbytes);

	UnlockConnection(aud);
	return(reply.currentTime);
}

ATime
AFRecordSamples(
		   AC ac,
		   ATime startTime,
		   int nbytes,
	           unsigned char *buf,
		   ABool block)
{
    register int  len;
    register ATime dtime;
    register int chunk_size;
    register int nsamples;
    register int ssize;
    register int delta;

    int nUnits;
    int unitSize;
    int unitsPerSecond;

    unitSize = BytesPerUnit(ac->attributes.type) * ac->attributes.channels;
    unitsPerSecond = ac->device->recSampleFreq / 
      SampsPerUnit(ac->attributes.type);
    nUnits = nbytes / unitSize;
    if (nbytes != (nUnits * unitSize)) {
      fprintf(stderr, 
	      "AIO: AFRecordSamples block not a multiple of encoding size\n");
      _ADefaultIOError(ac->connection);
    }

    chunk_size = (MIN(CHUNKSIZE, ac->connection->max_request_size)
			 - sz_aRecordSamplesReq) / unitSize;
    chunk_size = MIN(chunk_size, unitsPerSecond);

    while (nUnits > 0) {
	    len = min(chunk_size, nUnits);
	    dtime = RecordChunk(ac, startTime, len*unitSize, buf, block);

/*
  For the non-blocking case, if the request was only partially satisfied
  (i.e. straddles or is beyond "now"), then we can return immediately.
*/
	    if(block == ANoBlock) {
		delta = dtime - startTime;
		if(delta < 
		   (len * SampsPerUnit(ac->attributes.type)))
		    break;
	    }

	    nUnits -= len;
	    buf += len * unitSize;
	    startTime += len * SampsPerUnit(ac->attributes.type);
    }
    return dtime;
}
