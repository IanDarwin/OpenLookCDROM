/*
 * Copyright 1993 Network Computing Devices, Inc.
 * 
 * Permission to use, copy, modify, distribute, and sell this software and its
 * documentation for any purpose is hereby granted without fee, provided that
 * the above copyright notice appear in all copies and that both that
 * copyright notice and this permission notice appear in supporting
 * documentation, and that the name Network Computing Devices, Inc. not be
 * used in advertising or publicity pertaining to distribution of this
 * software without specific, written prior permission.
 * 
 * THIS SOFTWARE IS PROVIDED 'AS-IS'.  NETWORK COMPUTING DEVICES, INC.,
 * DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING WITHOUT
 * LIMITATION ALL IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS FOR A
 * PARTICULAR PURPOSE, OR NONINFRINGEMENT.  IN NO EVENT SHALL NETWORK
 * COMPUTING DEVICES, INC., BE LIABLE FOR ANY DAMAGES WHATSOEVER, INCLUDING
 * SPECIAL, INCIDENTAL OR CONSEQUENTIAL DAMAGES, INCLUDING LOSS OF USE, DATA,
 * OR PROFITS, EVEN IF ADVISED OF THE POSSIBILITY THEREOF, AND REGARDLESS OF
 * WHETHER IN AN ACTION IN CONTRACT, TORT OR NEGLIGENCE, ARISING OUT OF OR IN
 * CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 * 
 * $NCDId: @(#)soundlib.c,v 1.38 1994/06/01 03:01:00 greg Exp $
 */

#define _SOUNDLIB_C_

#include <stdio.h>
#include <audio/Aos.h>
#include "Alibint.h"
#include "soundlib.h"

unsigned int    AuSoundFileChunkSize = 10 * 1024,	/* in bytes */
                AuSoundPortDuration = 5,	/* in seconds */
                AuSoundPortLowWaterMark = 25,	/* in percent */
                AuSoundPortHighWaterMark = 75;	/* in percent */
AuBool          AuSoundRestartHardwarePauses = AuTrue;

extern int      AuMonitorRate,
                AuMonitorFormat;

AuBucketID
AuSoundCreateBucketFromFile(aud, filename, access, ret_attr, ret_status)
AuServer       *aud;
_SoundConst char *filename;
AuUint32   access;
AuBucketAttributes **ret_attr;			/* RETURN */
AuStatus       *ret_status;
{
    Sound           s;
    AuBucketID      bucket;
    AuFlowID        flow;
    int             import;
    char           *buf;
    unsigned int    count;
    AuString        desc;
    AuBool          done = AuFalse;

    if (!(s = SoundOpenFileForReading(filename)))
	return AuNone;

    if (!(buf = (char *) Aumalloc(AuSoundFileChunkSize)))
    {
	SoundCloseFile(s);
	return AuNone;
    }

    desc.type = AuStringLatin1;
    desc.len = strlen(SoundComment(s));
    desc.data = SoundComment(s);

    bucket = AuCreateBucket(aud, SoundDataFormat(s), SoundNumTracks(s), access,
			    SoundSampleRate(s), SoundNumSamples(s), &desc,
			    ret_status);

    if (bucket)
    {
	/*
	 * Get an unclocked flow into a bucket.  We can do this operation
	 * synchronously, so we don't have to horse around with events.
	 */
	flow = AuGetScratchFlowToBucket(aud, bucket, &import, ret_status);

	if (flow)
	{
	    int             numBytes = SoundNumBytes(s);

	    while (!done)
	    {
		count = aumin(AuSoundFileChunkSize, numBytes);
		count = SoundReadFile(buf, count, s);
		numBytes -= count;
		done = !(count && numBytes);

		(void) AuWriteElement(aud, flow, import, count, buf,
				      done, ret_status);
	    }

	    AuReleaseScratchFlow(aud, flow, ret_status);
	}

	if (ret_attr)
	    *ret_attr = AuGetBucketAttributes(aud, bucket, ret_status);
    }

    Aufree(buf);
    SoundCloseFile(s);

    return bucket;
}

AuBool
AuSoundCreateFileFromBucket(aud, filename, fileFormat, bucket, ret_status)
AuServer       *aud;
_AuConst char  *filename;
int             fileFormat;
AuBucketID      bucket;
AuStatus       *ret_status;
{
    AuBucketAttributes *ba;
    AuFlowID        flow;
    int             export;
    AuBool          result = AuTrue;
    char           *p;
    AuUint32   size;
    Sound           s;

    if (!(ba = AuGetBucketAttributes(aud, bucket, ret_status)))
	return AuFalse;

    size = AuBucketNumSamples(ba) * AuSizeofFormat(AuBucketFormat(ba)) *
	AuBucketNumTracks(ba);

    if (!(p = (char *) Aumalloc(AuSoundFileChunkSize)))
    {
	AuFreeBucketAttributes(aud, 1, ba);
	return AuFalse;
    }

    s = SoundCreate(fileFormat, AuBucketFormat(ba), AuBucketNumTracks(ba),
		    AuBucketSampleRate(ba), AuBucketNumSamples(ba),
		    AuBucketDescription(ba)->data);

    /* For now, assume that description is ISOLatin1 */

    if (!s || !SoundOpenFileForWriting(filename, s))
    {
	if (s)
	    SoundDestroy(s);

	Aufree(p);
	AuFreeBucketAttributes(aud, 1, ba);
	return AuFalse;
    }

    /*
     * Get an unclocked flow from a bucket.  We can do this operation
     * synchronously, so we don't have to horse around with events.
     */
    flow = AuGetScratchFlowFromBucket(aud, bucket, &export, ret_status);

    if (flow)
    {
	AuUint32   wanted = size,
	                got = 0;

	while (wanted &&
	       (got = AuReadElement(aud, flow, export,
				    aumin(wanted, AuSoundFileChunkSize), p,
				    ret_status)))
	{
	    if (SoundWriteFile(p, got, s) != got)
	    {
		result = AuFalse;
		break;
	    }

	    wanted -= got;
	}

	AuReleaseScratchFlow(aud, flow, ret_status);
    }

    free(p);
    AuFreeBucketAttributes(aud, 1, ba);
    SoundCloseFile(s);

    return result;
}

AuBucketID
AuSoundCreateBucketFromData(aud, s, data, access, ret_attr, ret_status)
AuServer       *aud;
Sound           s;
AuPointer       data;
AuUint32   access;
AuBucketAttributes **ret_attr;			/* RETURN */
AuStatus       *ret_status;
{
    AuBucketID      bucket;
    AuFlowID        flow;
    int             import;
    AuString        desc;

    if (SoundNumSamples(s) == SoundUnknownNumSamples)
	return AuNone;

    desc.type = AuStringLatin1;
    desc.len = strlen(SoundComment(s));
    desc.data = (char *) SoundComment(s);

    bucket = AuCreateBucket(aud, SoundDataFormat(s), SoundNumTracks(s), access,
			    SoundSampleRate(s), SoundNumSamples(s), &desc,
			    ret_status);

    if (bucket)
    {
	/*
	 * Get an unclocked flow into a bucket.  We can do this operation
	 * synchronously, so we don't have to horse around with events.
	 */
	flow = AuGetScratchFlowToBucket(aud, bucket, &import, ret_status);

	if (flow)
	{
	    (void) AuWriteElement(aud, flow, import, SoundNumBytes(s), data,
				  AuTrue, ret_status);
	    AuReleaseScratchFlow(aud, flow, ret_status);
	}

	if (ret_attr)
	    *ret_attr = AuGetBucketAttributes(aud, bucket, ret_status);

    }

    return bucket;
}

AuPointer
AuSoundCreateDataFromBucket(aud, bucket, ps, ret_status)
AuServer       *aud;
AuBucketID      bucket;
Sound          *ps;
AuStatus       *ret_status;
{
    AuBucketAttributes *ba;
    AuFlowID        flow;
    int             export;
    char           *p;
    AuInt32            nbytes;

    ba = AuGetBucketAttributes(aud, bucket, ret_status);

    if (!ba)
	return NULL;

    nbytes = AuBucketNumSamples(ba) * AuSizeofFormat(AuBucketFormat(ba)) *
	AuBucketNumTracks(ba);

    *ps = SoundCreate(SoundFileFormatNone, AuBucketFormat(ba),
		      AuBucketNumTracks(ba), AuBucketSampleRate(ba),
		      AuBucketNumSamples(ba), AuBucketDescription(ba)->data);

    if (!*ps)
    {
	AuFreeBucketAttributes(aud, 1, ba);
	return NULL;
    }

    p = (char *) Aumalloc(nbytes);

    if (!p)
    {
	AuFreeBucketAttributes(aud, 1, ba);
	SoundDestroy(*ps);
	return NULL;
    }

    /*
     * Get an unclocked flow from a bucket.  We can do this operation
     * synchronously, so we don't have to horse around with events.
     */
    flow = AuGetScratchFlowFromBucket(aud, bucket, &export, ret_status);

    if (flow)
    {
	AuReadElement(aud, flow, export, nbytes, p, ret_status);
	AuReleaseScratchFlow(aud, flow, ret_status);
    }

    AuFreeBucketAttributes(aud, 1, ba);
    return (AuPointer) p;
}

typedef struct
{
    Sound           s;
    AuFlowID        flow;
    char           *buf;
    AuPointer       callback_data;
    int             loopCount,
                    numBytes;
    void            (*callback) (),
                    (*dataHandler) (),
                    (*dataHandlerStop) ();
}               PrivRec, *PrivPtr;

static void
sendData(aud, priv, numBytes)
AuServer       *aud;
PrivPtr         priv;
AuUint32   numBytes;
{
    int             n = aumin(numBytes, priv->numBytes);

    if (n)
    {
	AuWriteElement(aud, priv->flow, 0, n, priv->buf, n != numBytes, NULL);
	priv->numBytes -= n;
	priv->buf += n;
    }
}

static void
receiveData(aud, priv, numBytes)
AuServer       *aud;
PrivPtr         priv;
AuUint32   numBytes;
{
    AuUint32   n;

    n = AuReadElement(aud, priv->flow, 1, numBytes, priv->buf, NULL);
    priv->buf += n;
}

static void
sendFile(aud, priv, numBytes)
AuServer       *aud;
PrivPtr         priv;
AuUint32   numBytes;
{
    int             n;

    if ((n = SoundReadFile(priv->buf, aumin(numBytes, priv->numBytes),
			   priv->s)) > 0)
    {
	AuWriteElement(aud, priv->flow, 0, n, priv->buf, n != numBytes, NULL);
	priv->numBytes -= n;
    }
}

static void
receiveFile(aud, priv, numBytes)
AuServer       *aud;
PrivPtr         priv;
AuUint32   numBytes;
{
    AuUint32   n;

    n = AuReadElement(aud, priv->flow, 1, numBytes, priv->buf, NULL);
    SoundWriteFile(priv->buf, n, priv->s);
}

static          AuBool
EventHandler(aud, ev, handler)
AuServer       *aud;
AuEvent        *ev;
AuEventHandlerRec *handler;
{
    PrivPtr         priv = (PrivPtr) handler->data;

    switch (ev->type)
    {
	case AuEventTypeMonitorNotify:
	    if (priv->callback)
		(*priv->callback) (aud, handler, ev, priv->callback_data);
	    break;
	case AuEventTypeElementNotify:
	    {
		AuElementNotifyEvent *event = (AuElementNotifyEvent *) ev;

		switch (event->kind)
		{
		    case AuElementNotifyKindHighWater:
		    case AuElementNotifyKindLowWater:
			(*priv->dataHandler) (aud, priv, event->num_bytes);
			break;
		    case AuElementNotifyKindState:
			switch (event->cur_state)
			{
			    case AuStateStop:
				if (priv->dataHandlerStop)
				    (*priv->dataHandlerStop) (aud, priv,
							  event->num_bytes);

				if (priv->s)
				    SoundCloseFile(priv->s);

				if (priv->callback)
				    (*priv->callback) (aud, handler, ev,
						       priv->callback_data);

				if (priv->loopCount &&
				    event->reason == AuReasonEOF)
				{
				    if (!--priv->loopCount)
					AuStopFlow(aud, priv->flow, NULL);
				    break;
				}

				AuUnregisterEventHandler(aud, handler);
				AuReleaseScratchFlow(aud, priv->flow, NULL);
				Aufree(priv);
				break;
			    case AuStatePause:
				if (event->reason == AuReasonHardware)
				    if (AuSoundRestartHardwarePauses)
					AuStartFlow(aud, priv->flow, NULL);
				    else
					AuStopFlow(aud, priv->flow, NULL);
				else if (event->reason != AuReasonUser)
				    (*priv->dataHandler) (aud, priv,
							  event->num_bytes);
				break;
			}
			break;
		}
	    }
    }

    return AuTrue;
}

#define ABORT()								       \
{									       \
    SoundCloseFile(priv->s);						       \
    Aufree(priv);							       \
									       \
    return NULL;							       \
}

/* play a Sound file */
AuEventHandlerRec *
AuSoundPlayFromFile(aud, filename, device, volume, callback, callback_data,
		    ret_flow, ret_mult_elem, ret_mon_elem, ret_status)
AuServer       *aud;
_AuConst char  *filename;
AuDeviceID      device;
AuFixedPoint    volume;
void            (*callback) ();
AuPointer       callback_data;
AuFlowID       *ret_flow;
int            *ret_mult_elem,
               *ret_mon_elem;
AuStatus       *ret_status;
{
    AuElement       elements[4];
    PrivPtr         priv;
    unsigned int    importSize,
                    bufSize,
                    i;
    Sound           s;
    AuEventHandlerRec *handler;

    /* open the file and get the header */
    if (!(s = SoundOpenFileForReading(filename)))
	return NULL;

    /* calc the size of the import */
    importSize = SoundSampleRate(s) * AuSoundPortDuration;
    bufSize = importSize * SoundNumTracks(s) * SoundBytesPerSample(s);

    /* alloc private data */
    if (!(priv = (PrivPtr) Aumalloc(PAD4(sizeof(PrivRec)) + bufSize)))
    {
	SoundCloseFile(s);
	return NULL;
    }

    priv->loopCount = 0;
    priv->callback = callback;
    priv->callback_data = callback_data;
    priv->dataHandler = sendFile;
    priv->dataHandlerStop = NULL;
    priv->buf = ((char *) priv) + PAD4(sizeof(PrivRec));
    priv->s = s;
    priv->numBytes = SoundNumBytes(s);

    /* if no ouput device was specified, look for an appropriate one */
    if (device == AuNone)
    {
	for (i = 0; i < AuServerNumDevices(aud); i++)
	    if ((AuDeviceKind(AuServerDevice(aud, i)) ==
		 AuComponentKindPhysicalOutput) &&
	     AuDeviceNumTracks(AuServerDevice(aud, i)) == SoundNumTracks(s))
	    {
		device = AuDeviceIdentifier(AuServerDevice(aud, i));
		break;
	    }
	/* abort if we didn't find an appropriate device */
	if (device == AuNone)
	    ABORT();
    }
    /* create the flow */
    if ((priv->flow = AuGetScratchFlow(aud, NULL)) == AuNone)
	ABORT();

    AuMakeElementImportClient(&elements[0], SoundSampleRate(s),
			      SoundDataFormat(s),
			      SoundNumTracks(s),
			      AuTrue,
			      importSize,
			      importSize * AuSoundPortLowWaterMark / 100,
			      0, NULL);
    AuMakeElementMultiplyConstant(&elements[1], 0, volume);
    AuMakeElementExportDevice(&elements[2], 1, device, SoundSampleRate(s),
			      AuUnlimitedSamples, 0, NULL);

    if (ret_mon_elem)
    {
	i = 4;
	*ret_mon_elem = 3;
	AuMakeElementExportMonitor(&elements[3], 0, AuMonitorRate,
				   AuMonitorFormat, SoundNumTracks(s));
    }
    else
	i = 3;

    /* set up the flow */
    AuSetElements(aud, priv->flow, AuTrue, i, elements, ret_status);

    /* set up the event handler */
    if (!(handler = AuRegisterEventHandler(aud, AuEventHandlerIDMask,
					   0, priv->flow, EventHandler,
					   (AuPointer) priv)))
    {
	AuReleaseScratchFlow(aud, priv->flow, ret_status);
	ABORT();
    }

    /* start up the components */
    AuStartFlow(aud, priv->flow, ret_status);

    if (ret_flow)
	*ret_flow = priv->flow;

    if (ret_mult_elem)
	*ret_mult_elem = 1;

    return handler;
}

/* ARGSUSED */
/* record a Sound file */
AuEventHandlerRec *
AuSoundRecordToFile(aud, filename, device, gain, callback, callback_data,
		    mode, fileFormat, comment, rate, dataFormat, ret_flow,
		    ret_mult_elem, ret_status)
AuServer       *aud;
_AuConst char  *filename;
AuDeviceID      device;
AuFixedPoint    gain;
void            (*callback) ();
AuPointer       callback_data;
int             mode,
                dataFormat,
                fileFormat;
AuUint32   rate;
char           *comment;
AuFlowID       *ret_flow;
int            *ret_mult_elem;
AuStatus       *ret_status;
{
    return AuSoundRecordToFileN(aud, filename, device, gain,
				AuUnlimitedSamples, callback,
			     callback_data, mode, fileFormat, comment, rate,
				dataFormat, ret_flow, ret_mult_elem,
				ret_status);
}

/* ARGSUSED */
/* record a Sound file */
AuEventHandlerRec *
AuSoundRecordToFileN(aud, filename, device, gain, numSamples, callback,
		     callback_data, mode, fileFormat, comment, rate,
		     dataFormat, ret_flow, ret_mult_elem, ret_status)
AuServer       *aud;
_AuConst char  *filename;
AuDeviceID      device;
AuFixedPoint    gain;
void            (*callback) ();
AuPointer       callback_data;
int             mode,
                dataFormat,
                fileFormat;
AuUint32   rate,
                numSamples;
char           *comment;
AuFlowID       *ret_flow;
int            *ret_mult_elem;
AuStatus       *ret_status;
{
    AuElementAction actions[1];
    AuElement       elements[2];
    PrivPtr         priv;
    unsigned int    exportSize,
                    bufSize,
                    i,
     		    mask = 0;
    Sound           s;
    AuEventHandlerRec *handler;
    AuDeviceAttributes *d = NULL,
                    da;

    /* get the device attributes */
    for (i = 0; i < AuServerNumDevices(aud); i++)
	if (AuDeviceIdentifier(AuServerDevice(aud, i)) == device)
	{
	    d = AuServerDevice(aud, i);
	    break;
	}

    if (!d)
	return NULL;

    s = SoundCreate(fileFormat, dataFormat, AuDeviceNumTracks(d), rate,
		    SoundUnknownNumSamples, comment);

    if (!s)
	return NULL;

    /* calc the size of the export */
    exportSize = SoundSampleRate(s) * AuSoundPortDuration;
    bufSize = exportSize * SoundNumTracks(s) * SoundBytesPerSample(s);

    /* alloc private data */
    if (!(priv = (PrivPtr) Aumalloc(PAD4(sizeof(PrivRec)) + bufSize)))
    {
	SoundDestroy(s);
	return NULL;
    }

    /* create the flow */
    if ((priv->flow = AuGetScratchFlow(aud, NULL)) == AuNone)
    {
	Aufree(priv);
	SoundDestroy(s);
	return NULL;
    }

    AuMakeChangeStateAction(&actions[0], AuStateStop, AuStateAny, AuReasonEOF,
			    priv->flow, 1, AuStateStop);

    AuMakeElementImportDevice(&elements[0], SoundSampleRate(s), device,
			      numSamples, 1, actions);
    AuMakeElementExportClient(&elements[1], 0, SoundSampleRate(s),
			      SoundDataFormat(s),
			      SoundNumTracks(s),
			      AuTrue,
			      exportSize,
			      exportSize * AuSoundPortHighWaterMark / 100,
			      0, NULL);

    /* set up the flow */
    AuSetElements(aud, priv->flow, AuTrue, 2, elements, ret_status);

    /* set up the event handler */
    if (!(handler = AuRegisterEventHandler(aud, AuEventHandlerTypeMask |
					   AuEventHandlerIDMask,
					   AuEventTypeElementNotify,
					   priv->flow, EventHandler, priv)))
    {
	AuReleaseScratchFlow(aud, priv->flow, ret_status);
	Aufree(priv);
	SoundDestroy(s);
	return NULL;
    }

    /* create the file */
    if (!SoundOpenFileForWriting(filename, s))
    {
	AuUnregisterEventHandler(aud, handler);
	AuReleaseScratchFlow(aud, priv->flow, ret_status);
	Aufree(priv);
	SoundDestroy(s);
	return NULL;
    }

    priv->loopCount = 0;
    priv->s = s;
    priv->buf = ((char *) priv) + PAD4(sizeof(PrivRec));
    priv->callback = callback;
    priv->callback_data = callback_data;
    priv->dataHandler = priv->dataHandlerStop = receiveFile;

    if (AuDeviceChangableMask(d) & AuCompDeviceLineModeMask)
    {
	AuDeviceLineMode(&da) = mode;
	mask |= AuCompDeviceLineModeMask;
    }

    if (AuDeviceChangableMask(d) & AuCompDeviceGainMask)
    {
	AuDeviceGain(&da) = gain;
	mask |= AuCompDeviceGainMask;
    }

    AuSetDeviceAttributes(aud, device, mask, &da, NULL);

    /* start up the components */
    AuStartFlow(aud, priv->flow, ret_status);

    if (ret_flow)
	*ret_flow = priv->flow;

    if (ret_mult_elem)
	*ret_mult_elem = -1;	/* XXX - not implemented yet */

    return handler;
}

#define	VOL(volume)		((1 << 16) * (volume) / 100)

/* ARGSUSED */
static void
sync_play_cb(aud, handler, ev, data)
AuServer       *aud;
AuEventHandlerRec *handler;
AuEvent        *ev;
AuPointer       data;
{
    int            *d = (int *) data;

    *d = 1;
}

AuBool
AuSoundPlaySynchronousFromFile(aud, fname, volume)
AuServer       *aud;
_AuConst char  *fname;
int             volume;
{
    int             d = 0;
    AuStatus        ret;
    AuEvent         ev;

    if (!AuSoundPlayFromFile(aud, fname, AuNone, VOL(volume),
			     sync_play_cb, (AuPointer) &d, (AuFlowID *) NULL,
			     (int *) NULL, (int *) NULL, &ret))
	return AuFalse;		/* XXX do something with ret? */

    while (1)
    {
	AuNextEvent(aud, AuTrue, &ev);
	AuDispatchEvent(aud, &ev);

	if (d)
	    break;
    }

    return AuTrue;
}

/* ARGSUSED */
/* record into a bucket */
AuEventHandlerRec *
AuSoundRecordToBucket(aud, bucket, device, gain, callback, callback_data,
		      mode, ret_flow, ret_mult_elem, ret_status)
AuServer       *aud;
AuBucketID      bucket;
AuDeviceID      device;
AuFixedPoint    gain;
void            (*callback) ();
AuPointer       callback_data;
int             mode;
AuFlowID       *ret_flow;
int            *ret_mult_elem;
AuStatus       *ret_status;
{
    AuElement       elements[2];
    PrivPtr         priv;
    AuBucketAttributes *ba;
    AuEventHandlerRec *handler;
    AuDeviceAttributes da, *d;
    unsigned int    i,
    		    mask = 0;

    /* get the device attributes */
    for (i = 0; i < AuServerNumDevices(aud); i++)
	if (AuDeviceIdentifier(AuServerDevice(aud, i)) == device)
	{
	    d = AuServerDevice(aud, i);
	    break;
	}

    if (!d || !(ba = AuGetBucketAttributes(aud, bucket, ret_status)))
	return NULL;

    /* alloc private data */
    if (!(priv = (PrivPtr) Aumalloc(sizeof(PrivRec))))
    {
	AuFreeBucketAttributes(aud, 1, ba);
	return NULL;
    }

    priv->loopCount = 0;
    priv->callback = callback;
    priv->callback_data = callback_data;
    priv->dataHandlerStop = NULL;
    priv->s = NULL;

    /* create the flow */
    if ((priv->flow = AuGetScratchFlow(aud, NULL)) == AuNone)
    {
	AuFreeBucketAttributes(aud, 1, ba);
	Aufree(priv);
	return NULL;
    }

    AuMakeElementImportDevice(&elements[0], AuBucketSampleRate(ba), device,
			      AuUnlimitedSamples, 0, NULL);
    AuMakeElementExportBucket(&elements[1], 0, bucket, AuUnlimitedSamples,
			      0, 0, NULL);

    /* set up the flow */
    AuSetElements(aud, priv->flow, AuTrue, 2, elements, ret_status);

    /* set up the event handler */
    if (!(handler = AuRegisterEventHandler(aud, AuEventHandlerTypeMask |
					   AuEventHandlerIDMask,
					   AuEventTypeElementNotify,
					   priv->flow, EventHandler, priv)))
    {
	AuReleaseScratchFlow(aud, priv->flow, ret_status);
	AuFreeBucketAttributes(aud, 1, ba);
	Aufree(priv);
	return NULL;
    }

    if (AuDeviceChangableMask(d) & AuCompDeviceLineModeMask)
    {
	AuDeviceLineMode(&da) = mode;
	mask |= AuCompDeviceLineModeMask;
    }

    if (AuDeviceChangableMask(d) & AuCompDeviceGainMask)
    {
	AuDeviceGain(&da) = gain;
	mask |= AuCompDeviceGainMask;
    }

    AuSetDeviceAttributes(aud, device, mask, &da, NULL);

    /* start up the components */
    AuStartFlow(aud, priv->flow, ret_status);

    if (ret_flow)
	*ret_flow = priv->flow;

    if (ret_mult_elem)
	*ret_mult_elem = -1;	/* XXX - not implemented yet */

    AuFreeBucketAttributes(aud, 1, ba);
    return handler;
}

/* play from a bucket */
AuEventHandlerRec *
AuSoundPlayFromBucket(aud, bucket, device, volume, callback, callback_data,
		   count, ret_flow, ret_mult_elem, ret_mon_elem, ret_status)
AuServer       *aud;
AuBucketID      bucket;
AuDeviceID      device;
AuFixedPoint    volume;
void            (*callback) ();
AuPointer       callback_data;
int             count;
AuFlowID       *ret_flow;
int            *ret_mult_elem,
               *ret_mon_elem;
AuStatus       *ret_status;
{
    AuElement       elements[4];
    AuElementAction actions[4];
    PrivPtr         priv;
    unsigned int    i,
                    numActions = 0;
    AuBucketAttributes *ba;
    AuEventHandlerRec *handler;

    if (!(ba = AuGetBucketAttributes(aud, bucket, ret_status)))
	return NULL;

    /* alloc private data */
    if (!(priv = (PrivPtr) Aumalloc(sizeof(PrivRec))))
    {
	AuFreeBucketAttributes(aud, 1, ba);
	return NULL;
    }

    priv->loopCount = 0;
    priv->callback = callback;
    priv->callback_data = callback_data;
    priv->dataHandlerStop = NULL;
    priv->s = NULL;

    /* if no ouput device was specified, look for an appropriate one */
    if (device == AuNone)
    {
	for (i = 0; i < AuServerNumDevices(aud); i++)
	    if ((AuDeviceKind(AuServerDevice(aud, i)) ==
		 AuComponentKindPhysicalOutput) &&
		AuDeviceNumTracks(AuServerDevice(aud, i)) ==
		AuBucketNumTracks(ba))
	    {
		device = AuDeviceIdentifier(AuServerDevice(aud, i));
		break;
	    }

	/* abort if we didn't find an appropriate device */
	if (device == AuNone)
	{
	    AuFreeBucketAttributes(aud, 1, ba);
	    Aufree(priv);
	    return NULL;
	}
    }

    /* create the flow */
    if ((priv->flow = AuGetScratchFlow(aud, NULL)) == AuNone)
    {
	AuFreeBucketAttributes(aud, 1, ba);
	Aufree(priv);
	return NULL;
    }

    if (count > 1)
    {
	AuMakeSendNotifyAction(&actions[0], AuStatePause, AuStateAny,
			       AuReasonAny);
	AuMakeSendNotifyAction(&actions[1], AuStateStop, AuStateAny,
			       AuReasonAny);
	AuMakeChangeStateAction(&actions[2], AuStateStop, AuStateAny,
				AuReasonUser, priv->flow, AuElementAll,
				AuStateStop);
	AuMakeChangeStateAction(&actions[3], AuStateStop, AuStateAny,
				AuReasonEOF, priv->flow, 0, AuStateStart);

	priv->loopCount = count;
	numActions = 4;
    }

    AuMakeElementImportBucket(&elements[0], AuBucketSampleRate(ba), bucket,
			      AuUnlimitedSamples, 0, numActions, actions);
    AuMakeElementMultiplyConstant(&elements[1], 0, volume);
    AuMakeElementExportDevice(&elements[2], 1, device, AuBucketSampleRate(ba),
			      AuUnlimitedSamples, 0, NULL);

    if (ret_mon_elem)
    {
	i = 4;
	*ret_mon_elem = 3;
	AuMakeElementExportMonitor(&elements[3], 0, AuMonitorRate,
				   AuMonitorFormat, AuBucketNumTracks(ba));
    }
    else
	i = 3;

    /* set up the flow */
    AuSetElements(aud, priv->flow, AuTrue, i, elements, ret_status);

    /* set up the event handler */
    if (!(handler = AuRegisterEventHandler(aud, AuEventHandlerIDMask,
					   0, priv->flow, EventHandler,
					   (AuPointer) priv)))
    {
	AuReleaseScratchFlow(aud, priv->flow, ret_status);
	AuFreeBucketAttributes(aud, 1, ba);
	Aufree(priv);
	return NULL;
    }

    /* start up the components */
    AuStartFlow(aud, priv->flow, ret_status);

    if (ret_flow)
	*ret_flow = priv->flow;

    if (ret_mult_elem)
	*ret_mult_elem = 1;

    AuFreeBucketAttributes(aud, 1, ba);
    return handler;
}

#undef ABORT
#define ABORT()								       \
{									       \
    Aufree(priv);							       \
    return NULL;							       \
}

/* play from data in memory */
AuEventHandlerRec *
AuSoundPlayFromData(aud, s, data, device, volume, callback, callback_data,
		    ret_flow, ret_mult_elem, ret_mon_elem, ret_status)
AuServer       *aud;
Sound           s;
AuPointer       data;
AuDeviceID      device;
AuFixedPoint    volume;
void            (*callback) ();
AuPointer       callback_data;
AuFlowID       *ret_flow;
int            *ret_mult_elem,
               *ret_mon_elem;
AuStatus       *ret_status;
{
    AuElement       elements[4];
    PrivPtr         priv;
    unsigned int    importSize,
                    i;
    AuEventHandlerRec *handler;

    /* calc the size of the import */
    importSize = SoundSampleRate(s) * AuSoundPortDuration;

    /* alloc private data */
    if (!(priv = (PrivPtr) Aumalloc(sizeof(PrivRec))))
	return NULL;

    priv->loopCount = 0;
    priv->callback = callback;
    priv->callback_data = callback_data;
    priv->dataHandler = sendData;
    priv->dataHandlerStop = NULL;
    priv->buf = (char *) data;
    priv->s = NULL;
    priv->numBytes = SoundNumBytes(s);

    /* if no ouput device was specified, look for an appropriate one */
    if (device == AuNone)
    {
	for (i = 0; i < AuServerNumDevices(aud); i++)
	    if ((AuDeviceKind(AuServerDevice(aud, i)) ==
		 AuComponentKindPhysicalOutput) &&
	     AuDeviceNumTracks(AuServerDevice(aud, i)) == SoundNumTracks(s))
	    {
		device = AuDeviceIdentifier(AuServerDevice(aud, i));
		break;
	    }

	/* abort if we didn't find an appropriate device */
	if (device == AuNone)
	    ABORT();
    }
    /* create the flow */
    if ((priv->flow = AuGetScratchFlow(aud, NULL)) == AuNone)
	ABORT();

    AuMakeElementImportClient(&elements[0], SoundSampleRate(s),
			      SoundDataFormat(s),
			      SoundNumTracks(s),
			      AuTrue,
			      importSize,
			      importSize * AuSoundPortLowWaterMark / 100,
			      0, NULL);
    AuMakeElementMultiplyConstant(&elements[1], 0, volume);
    AuMakeElementExportDevice(&elements[2], 1, device, SoundSampleRate(s),
			      AuUnlimitedSamples, 0, NULL);

    if (ret_mon_elem)
    {
	i = 4;
	*ret_mon_elem = 3;
	AuMakeElementExportMonitor(&elements[3], 0, AuMonitorRate,
				   AuMonitorFormat, SoundNumTracks(s));
    }
    else
	i = 3;

    /* set up the flow */
    AuSetElements(aud, priv->flow, AuTrue, i, elements, ret_status);

    /* set up the event handler */
    if (!(handler = AuRegisterEventHandler(aud, AuEventHandlerIDMask,
					   0, priv->flow, EventHandler,
					   (AuPointer) priv)))
    {
	AuReleaseScratchFlow(aud, priv->flow, ret_status);
	ABORT();
    }

    /* start up the components */
    AuStartFlow(aud, priv->flow, ret_status);

    if (ret_flow)
	*ret_flow = priv->flow;

    if (ret_mult_elem)
	*ret_mult_elem = 1;

    return handler;
}

/* record into data in memory */
AuEventHandlerRec *
AuSoundRecordToData(aud, s, data, device, gain, callback, callback_data,
		    mode, ret_flow, ret_mult_elem, ret_status)
AuServer       *aud;
Sound           s;
AuPointer       data;
AuDeviceID      device;
AuFixedPoint    gain;
void            (*callback) ();
AuPointer       callback_data;
int             mode;
AuFlowID       *ret_flow;
int            *ret_mult_elem;
AuStatus       *ret_status;
{
    AuElementAction actions[1];
    AuElement       elements[2];
    PrivPtr         priv;
    AuEventHandlerRec *handler;
    AuDeviceAttributes da,
                   *d;
    unsigned int    i,
                    exportSize,
                    mask = 0;

    /* calc the size of the export */
    exportSize = SoundSampleRate(s) * AuSoundPortDuration;

    /* get the device attributes */
    for (i = 0; i < AuServerNumDevices(aud); i++)
	if (AuDeviceIdentifier(AuServerDevice(aud, i)) == device)
	{
	    d = AuServerDevice(aud, i);
	    break;
	}

    if (!d || !(priv = (PrivPtr) Aumalloc(sizeof(PrivRec))))
	return NULL;

    /* create the flow */
    if ((priv->flow = AuGetScratchFlow(aud, NULL)) == AuNone)
    {
	Aufree(priv);
	return NULL;
    }

    AuMakeChangeStateAction(&actions[0], AuStateStop, AuStateAny, AuReasonEOF,
			    priv->flow, 1, AuStateStop);

    AuMakeElementImportDevice(&elements[0], SoundSampleRate(s), device,
			      SoundNumSamples(s), 1, actions);
    AuMakeElementExportClient(&elements[1], 0, SoundSampleRate(s),
			      SoundDataFormat(s), SoundNumTracks(s),
			      AuTrue, exportSize,
			      exportSize * AuSoundPortHighWaterMark / 100,
			      0, NULL);

    /* set up the flow */
    AuSetElements(aud, priv->flow, AuTrue, 2, elements, ret_status);

    /* set up the event handler */
    if (!(handler = AuRegisterEventHandler(aud, AuEventHandlerTypeMask |
					   AuEventHandlerIDMask,
					   AuEventTypeElementNotify,
					   priv->flow, EventHandler, priv)))
    {
	AuReleaseScratchFlow(aud, priv->flow, ret_status);
	Aufree(priv);
	return NULL;
    }

    priv->loopCount = 0;
    priv->s = NULL;
    priv->buf = (char *) data;
    priv->callback = callback;
    priv->callback_data = callback_data;
    priv->dataHandler = priv->dataHandlerStop = receiveData;

    if (AuDeviceChangableMask(d) & AuCompDeviceLineModeMask)
    {
	AuDeviceLineMode(&da) = mode;
	mask |= AuCompDeviceLineModeMask;
    }

    if (AuDeviceChangableMask(d) & AuCompDeviceGainMask)
    {
	AuDeviceGain(&da) = gain;
	mask |= AuCompDeviceGainMask;
    }

    AuSetDeviceAttributes(aud, device, mask, &da, NULL);

    /* start up the components */
    AuStartFlow(aud, priv->flow, ret_status);

    if (ret_flow)
	*ret_flow = priv->flow;

    if (ret_mult_elem)
	*ret_mult_elem = -1;		       /* XXX - not implemented yet */

    return handler;
}
