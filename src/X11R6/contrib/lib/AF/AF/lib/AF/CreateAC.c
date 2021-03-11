/* Copyright    Massachusetts Institute of Technology    1987	*/
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

#include "Alibint.h"

#define AllMaskBits (ACRecordGain | ACPlayGain | ACStartTimeout | \
    ACEndSilence | ACPreemption | ACEncodingType | ACChannels | ACEndian)

#define ServerMaskBits (ACRecordGain | ACPlayGain | ACStartTimeout | \
    ACEndSilence | ACPreemption | ACEncodingType)

static AFSetACAttributes initial_AC = {
    0,
    0,
    InfiniteTimeout,
    InfiniteTimeout,
    Mix,
    MU255,				/* gets written over at create */
    1,
    ALittleEndian
};

AC AFCreateAC (
    register AFAudioConn *aud,
    register ADevice device,
    register unsigned long valuemask,
    register AFSetACAttributes *attributes
	)
{
    register AC ac;
    register aCreateACReq *req;
    int indian = 1;

    LockConnection(aud);
    GetReq(CreateAC, req);

    if ((ac = (AC)Xmalloc (sizeof(struct _AC))) == NULL) {
	UnlockConnection(aud);
	SyncHandle();
	return (NULL);
    }
    aud = aud->connection;
    ac->connection = aud;
    ac->device = &aud->devices[device];
    ac->ext_data = NULL;
    ac->acontext = req->ac = AAllocID(aud);
    ac->attributes = initial_AC;

/*
  Initialize to what's supported by the device
*/
    ac->attributes.type = ac->device->playBufType;
    ac->attributes.channels = ac->device->playNchannels;
    if (*(char *) &indian)
	ac->attributes.endian = ALittleEndian;
    else
	ac->attributes.endian = ABigEndian;
    
    req->mask = valuemask;
    req->device = device;
    valuemask &= AllMaskBits;
    if (req->mask = valuemask)
        _AFProcessACAttributes (aud, ac, (aChangeACAttributesReq *)req,
                        valuemask, attributes);

    req->mask &= ServerMaskBits;
    UnlockConnection(aud);
    SyncHandle();
    return (ac);
}

void
_AFProcessACAttributes (
    register AFAudioConn *aud,
    AC ac,
    aChangeACAttributesReq *req,
    register unsigned long valuemask,
    register AFSetACAttributes *attributes
	)
    {

    /* Warning!  This code assumes that "unsigned long" is 32-bits wide */

    CARD32 values[32];
    register CARD32 *value = values;
    unsigned int nvalues;

    if (valuemask & ACRecordGain)
        *value++ = ac->attributes.rec_gain = attributes->rec_gain;

    if (valuemask & ACPlayGain)
        *value++ = ac->attributes.play_gain = attributes->play_gain;

    if (valuemask & ACStartTimeout)
        *value++ = ac->attributes.start_timeout = attributes->start_timeout;

    if (valuemask & ACEndSilence)
        *value++ = ac->attributes.end_silence = attributes->end_silence;

    if (valuemask & ACPreemption)
        *value++ = ac->attributes.preempt = attributes->preempt;

    if (valuemask & ACEncodingType)
        *value++ = ac->attributes.type = attributes->type;

/* the following attributes are visible only on the client side */
    if (valuemask & ACChannels)
        ac->attributes.channels = attributes->channels;

    if (valuemask & ACEndian)
        ac->attributes.endian = attributes->endian;

    req->length += (nvalues = value - values);

    nvalues <<= 2;                          /* watch out for macros... */
    Data32 (aud, (INT32 *) values, (long)nvalues);

}
