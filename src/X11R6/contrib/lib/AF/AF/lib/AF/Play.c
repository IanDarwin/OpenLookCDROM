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
PlayChunk(	   AC ac,
                   ATime startTime,
                   int nbytes,
                   unsigned char *buf,
	  	   ABool gettime)
{
	register AFAudioConn *aud = ac->connection;
	aGetTimeReply reply;
	register aPlaySamplesReq *req;

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

	if (!gettime) req->mask |= ANoTimeReplyMask;
	_ASend(aud, (char *)buf, (long) req->nbytes);
 	if (gettime) _AReply(aud, (aReply *) &reply, 0, aTrue);
	UnlockConnection(aud);
	return(reply.time);
}

ATime
AFPlaySamples(
		   AC ac,
                   ATime startTime,
                   int nbytes,
                   unsigned char *buf)
{
    ATime dtime;
    int len;
    int chunk_size;
    int nUnits;
    int unitSize;
    int unitsPerSecond;
    unitSize = BytesPerUnit(ac->attributes.type) * ac->attributes.channels;
    unitsPerSecond = ac->device->playSampleFreq / 
      SampsPerUnit(ac->attributes.type);
    nUnits = nbytes / unitSize;
    if (nbytes != (nUnits * unitSize)) {
      fprintf(stderr, 
	      "AIO: AFPlaySamples block not a multiple of encoding size\n");
      _ADefaultIOError(ac->connection);
    }
    /* Protocol and socket limitation on chunk size. */
    chunk_size = (MIN(CHUNKSIZE, ac->connection->max_request_size)
			- sz_aPlaySamplesReq) / unitSize;
    /* Now reduce if necessary to at most 1 second of audio. */
    chunk_size = MIN(chunk_size, unitsPerSecond);
    /* Because of server CPU computation load on slow systems, 
     * limit GSM compressed length to 1/8 sec. 
     */
    if (ac->attributes.type == GSM) 
       chunk_size = MIN(chunk_size, unitsPerSecond>>3);

    while (nUnits > 0) {
	    len = MIN(chunk_size, nUnits);
	    nUnits -= len;
	    dtime = PlayChunk(ac, startTime, len*unitSize, buf, 
			      (nUnits <= 0));
	    startTime += len * SampsPerUnit(ac->attributes.type);
	    /* we played that many samples. */
	    buf += len * unitSize;
    }
    return dtime;
}
