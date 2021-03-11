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
 * THIS SOFTWARE IS PROVIDED `AS-IS'.  NETWORK COMPUTING DEVICES, INC.,
 * DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING WITHOUT
 * LIMITATION ALL IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS FOR A
 * PARTICULAR PURPOSE, OR NONINFRINGEMENT.  IN NO EVENT SHALL NETWORK
 * COMPUTING DEVICES, INC., BE LIABLE FOR ANY DAMAGES WHATSOEVER, INCLUDING
 * SPECIAL, INCIDENTAL OR CONSEQUENTIAL DAMAGES, INCLUDING LOSS OF USE, DATA,
 * OR PROFITS, EVEN IF ADVISED OF THE POSSIBILITY THEREOF, AND REGARDLESS OF
 * WHETHER IN AN ACTION IN CONTRACT, TORT OR NEGLIGENCE, ARISING OUT OF OR IN
 * CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 * 
 * $NCDId: @(#)auprocess.c,v 1.24 1994/06/01 18:30:07 greg Exp $
 */

#define  _AUPROCESS_C_

#include <audio/audio.h>
#include <audio/Aproto.h>
#include "au.h"

AuInt32         auMinibufSamples;
AuUint32        auPhysicalOutputBuffersSize;
AuUint8        *auPhysicalOutputBuffers;

extern AuBool   AuChangeElementState();
extern void     AuRequestElementNotifyEvent(), AuRequestMonitorNotifyEvent();

static AuUint32 rc_1(), rc_2(), rc_4(), rc_n(), rcNull(),
                rcm_1(), rcm_2(), rcm_n();
static void     (*writePhysicalOutputs) ();
static CompiledFlowPtr auFlow;
static AuUint32 currentSampleRate,
                readTag;
static AuFixedPoint maxOutputGain = AuFixedPointFromSum(100, 0),
                desiredOutputGain;
static AuBool   processFlowEnabled;

void            AuULAW8ToNative(), AuNativeToULAW8(),
                AuUnsigned8ToNative(), AuNativeToUnsigned8(),
                AuSigned8ToNative(), AuNativeToSigned8(),
                changeSign(), byteSwap(),
                AuNeg16LSBTo16MSB(), AuNeg16MSBto16LSB();

static struct
{
    void            (*toNativeFormat) (),
                    (*fromNativeFormat) ();
}               converters[] =

{
    (void (*) ()) 0, (void (*) ()) 0,		/* unused */
    AuULAW8ToNative, AuNativeToULAW8,		/* ULAW8 */
    AuUnsigned8ToNative, AuNativeToUnsigned8,	/* LinUnsigned8 */
    AuSigned8ToNative, AuNativeToSigned8,	/* LinSigned8 */
    AuSigned16MSBToNative, AuNativeToSigned16MSB,	/* LinSigned16MSB */
    AuUnsigned16MSBToNative, AuNativeToUnsigned16MSB,	/* LinUnsigned16MSB */
    AuSigned16LSBToNative, AuNativeToSigned16LSB,	/* LinSigned16LSB */
    AuUnsigned16LSBToNative, AuNativeToUnsigned16LSB,	/* LinUnsigned16LSB */
};

#define ulawToLinear(_x) ulawToLinearTable[_x]

static AuUint16 ulawToLinearTable[] =
{
    0x8284, 0x8684, 0x8a84, 0x8e84, 0x9284, 0x9684, 0x9a84, 0x9e84,
    0xa284, 0xa684, 0xaa84, 0xae84, 0xb284, 0xb684, 0xba84, 0xbe84,
    0xc184, 0xc384, 0xc584, 0xc784, 0xc984, 0xcb84, 0xcd84, 0xcf84,
    0xd184, 0xd384, 0xd584, 0xd784, 0xd984, 0xdb84, 0xdd84, 0xdf84,
    0xe104, 0xe204, 0xe304, 0xe404, 0xe504, 0xe604, 0xe704, 0xe804,
    0xe904, 0xea04, 0xeb04, 0xec04, 0xed04, 0xee04, 0xef04, 0xf004,
    0xf0c4, 0xf144, 0xf1c4, 0xf244, 0xf2c4, 0xf344, 0xf3c4, 0xf444,
    0xf4c4, 0xf544, 0xf5c4, 0xf644, 0xf6c4, 0xf744, 0xf7c4, 0xf844,
    0xf8a4, 0xf8e4, 0xf924, 0xf964, 0xf9a4, 0xf9e4, 0xfa24, 0xfa64,
    0xfaa4, 0xfae4, 0xfb24, 0xfb64, 0xfba4, 0xfbe4, 0xfc24, 0xfc64,
    0xfc94, 0xfcb4, 0xfcd4, 0xfcf4, 0xfd14, 0xfd34, 0xfd54, 0xfd74,
    0xfd94, 0xfdb4, 0xfdd4, 0xfdf4, 0xfe14, 0xfe34, 0xfe54, 0xfe74,
    0xfe8c, 0xfe9c, 0xfeac, 0xfebc, 0xfecc, 0xfedc, 0xfeec, 0xfefc,
    0xff0c, 0xff1c, 0xff2c, 0xff3c, 0xff4c, 0xff5c, 0xff6c, 0xff7c,
    0xff88, 0xff90, 0xff98, 0xffa0, 0xffa8, 0xffb0, 0xffb8, 0xffc0,
    0xffc8, 0xffd0, 0xffd8, 0xffe0, 0xffe8, 0xfff0, 0xfff8, 0x0000,
    0x7d7c, 0x797c, 0x757c, 0x717c, 0x6d7c, 0x697c, 0x657c, 0x617c,
    0x5d7c, 0x597c, 0x557c, 0x517c, 0x4d7c, 0x497c, 0x457c, 0x417c,
    0x3e7c, 0x3c7c, 0x3a7c, 0x387c, 0x367c, 0x347c, 0x327c, 0x307c,
    0x2e7c, 0x2c7c, 0x2a7c, 0x287c, 0x267c, 0x247c, 0x227c, 0x207c,
    0x1efc, 0x1dfc, 0x1cfc, 0x1bfc, 0x1afc, 0x19fc, 0x18fc, 0x17fc,
    0x16fc, 0x15fc, 0x14fc, 0x13fc, 0x12fc, 0x11fc, 0x10fc, 0x0ffc,
    0x0f3c, 0x0ebc, 0x0e3c, 0x0dbc, 0x0d3c, 0x0cbc, 0x0c3c, 0x0bbc,
    0x0b3c, 0x0abc, 0x0a3c, 0x09bc, 0x093c, 0x08bc, 0x083c, 0x07bc,
    0x075c, 0x071c, 0x06dc, 0x069c, 0x065c, 0x061c, 0x05dc, 0x059c,
    0x055c, 0x051c, 0x04dc, 0x049c, 0x045c, 0x041c, 0x03dc, 0x039c,
    0x036c, 0x034c, 0x032c, 0x030c, 0x02ec, 0x02cc, 0x02ac, 0x028c,
    0x026c, 0x024c, 0x022c, 0x020c, 0x01ec, 0x01cc, 0x01ac, 0x018c,
    0x0174, 0x0164, 0x0154, 0x0144, 0x0134, 0x0124, 0x0114, 0x0104,
    0x00f4, 0x00e4, 0x00d4, 0x00c4, 0x00b4, 0x00a4, 0x0094, 0x0084,
    0x0078, 0x0070, 0x0068, 0x0060, 0x0058, 0x0050, 0x0048, 0x0040,
    0x0038, 0x0030, 0x0028, 0x0020, 0x0018, 0x0010, 0x0008, 0x0000,
};

#ifndef AU_OPTIMIZE_SINGLE_SAMPLE
#define _numSamples numSamples
#else						/* AU_OPTIMIZE_SINGLE_SAMPLE */
#define _numSamples 1
#endif						/* AU_OPTIMIZE_SINGLE_SAMPLE */

void
AuULAW8ToNative(p, tracks, numSamples)
AuUint8        *p;
AuInt32         tracks,
                numSamples;
{
    AuUint8        *s;
    AuInt16         i,
                   *d;

    s = p + _numSamples * tracks - 1;
    d = (AuInt16 *) (p + (_numSamples * tracks - 1) * 2);

    for (i = 0; i < _numSamples * tracks; i++, s--, d--)
	*d = ulawToLinear(*s);
}

/**
 * This routine converts from linear to ulaw.
 *
 * Craig Reese: IDA/Supercomputing Research Center
 * Joe Campbell: Department of Defense
 * 29 September 1989
 *
 * References:
 * 1) CCITT Recommendation G.711  (very difficult to follow)
 * 2) "A New Digital Technique for Implementation of Any
 *     Continuous PCM Companding Law," Villeret, Michel,
 *     et al. 1973 IEEE Int. Conf. on Communications, Vol 1,
 *     1973, pg. 11.12-11.17
 * 3) MIL-STD-188-113,"Interoperability and Performance Standards
 *     for Analog-to_Digital Conversion Techniques,"
 *     17 February 1987
 *
 * Input: Signed 16 bit linear sample
 * Output: 8 bit ulaw sample
 */

#if 0
#define ZEROTRAP				/* turn on the trap as per
						 * the MIL-STD */
#define CLIP 32635
#endif

#define BIAS 0x84				/* define the add-in bias for
						 * 16 bit samples */

void
AuNativeToULAW8(s, tracks, numSamples)
AuInt16        *s;
AuInt32         tracks,
                numSamples;
{
    static AuInt32  exp_lut[256] =
    {
	0, 0, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3, 3, 3, 3, 3,
	4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4,
	5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5,
	5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5,
	6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6,
	6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6,
	6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6,
	6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6,
	7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7,
	7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7,
	7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7,
	7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7,
	7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7,
	7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7,
	7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7,
	7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7
    };
    AuInt16         sign,
                    exponent,
                    mantissa,
                    sample,
                    i;
    AuUint8         ulawbyte,
                   *d;

    d = (unsigned char *) s;

    for (i = 0; i < _numSamples * tracks; i++, s++, d++)
    {
	sample = *s;

	/* Get the sample into sign-magnitude. */
	sign = (sample >> 8) & 0x80;	       /* set aside the sign */

	if (sign)
	    sample = -sample;		       /* get magnitude */
#ifdef CLIP
	if (sample > CLIP)
	    sample = CLIP;		       /* clip the magnitude */
#endif
	/* Convert from 16 bit linear to ulaw. */
	/* sample = sample + BIAS; */
	exponent = exp_lut[(sample >> 7) & 0xff];
	mantissa = (sample >> (exponent + 3)) & 0xf;
	ulawbyte = ~(sign | (exponent << 4) | mantissa);
#ifdef ZEROTRAP
	if (ulawbyte == 0)
	    ulawbyte = 0x02;		       /* optional CCITT trap */
#endif
	*d = ulawbyte;
    }
}

void
AuSigned8ToNative(p, tracks, numSamples)
AuUint8        *p;
AuInt32         tracks,
                numSamples;
{
    AuUint8        *s;
    AuInt16        *d;
    AuInt32         i;

    s = p + _numSamples * tracks - 1;
    d = (AuInt16 *) (p + (_numSamples * tracks - 1) * 2);

    for (i = 0; i < _numSamples * tracks; i++, s--, d--)
	*d = *s << 8;
}

void
AuNativeToSigned8(s, tracks, numSamples)
AuUint16       *s;
AuInt32         tracks,
                numSamples;
{
    AuUint8        *d = (AuUint8 *) s;
    AuInt32         i;

    for (i = 0; i < _numSamples * tracks; i++, s++, d++)
	*d = *s >> 8;
}

void
AuUnsigned8ToNative(p, tracks, numSamples)
AuUint8        *p;
AuInt32         tracks,
                numSamples;
{
    AuUint8        *s;
    AuInt16        *d;
    AuInt32         i;

    s = p + _numSamples * tracks - 1;
    d = (AuInt16 *) (p + (_numSamples * tracks - 1) * 2);

    for (i = 0; i < _numSamples * tracks; i++, s--, d--)
	*d = (*s << 8) ^ 0x8000;
}

void
AuNativeToUnsigned8(s, tracks, numSamples)
AuInt16        *s;
AuInt32         tracks,
                numSamples;
{
    AuInt32         i;
    AuUint8        *d = (AuUint8 *) s;

    for (i = 0; i < _numSamples * tracks; i++, s++, d++)
	*d = (*s ^ 0x8000) >> 8;
}

void
changeSign(p, tracks, numSamples)
AuUint16       *p;
AuInt32         tracks,
                numSamples;
{
    AuInt32         i;

    for (i = 0; i < _numSamples * tracks; i++)
	*p++ ^= 0x8000;
}

void
AuNeg16LSBTo16MSB(p, tracks, numSamples)
AuUint16       *p;
AuInt32         tracks,
                numSamples;
{
    AuInt32         i;
    AuUint8         x;

    for (i = 0; i < _numSamples * tracks; i++, p++)
    {
	x = ((AuUint8 *) p)[0];
	((AuUint8 *) p)[0] = ((AuUint8 *) p)[1] ^ 0x80;
	((AuUint8 *) p)[1] = x;
    }
}

void
AuNeg16MSBto16LSB(p, tracks, numSamples)
AuInt16        *p;
AuInt32         tracks,
                numSamples;
{
    AuInt32         i;
    AuUint8         x;

    for (i = 0; i < _numSamples * tracks; i++, p++)
    {
	x = ((AuUint8 *) p)[0];
	((AuUint8 *) p)[0] = ((AuUint8 *) p)[1];
	((AuUint8 *) p)[1] = x ^ 0x80;
    }
}

void
byteSwap(p, tracks, numSamples)
AuUint16       *p;
AuInt32         tracks,
                numSamples;
{
    AuInt32         i;
    AuUint8         x;

    for (i = 0; i < _numSamples * tracks; i++, p++)
    {
	x = ((AuUint8 *) p)[0];
	((AuUint8 *) p)[0] = ((AuUint8 *) p)[1];
	((AuUint8 *) p)[1] = x;
    }
}

/* returns AuTrue if a flow was started */
AuBool
AuStartFlow(newFlow, pOldFlow)
CompiledFlowPtr newFlow,
               *pOldFlow;
{
    AuBool          status;
    AuBlock         l;

    if (newFlow)
	AuCallback(AuSetWritePhysicalOutputFunctionCB,
		   (newFlow, &writePhysicalOutputs));

    l = AuBlockAudio();

    *pOldFlow = auFlow;

    if (newFlow && newFlow->numOutputs)
    {
	AuInt32         i,
	                j;
	CompiledFlowOutputPtr output;

	auFlow = newFlow;
	status = AuTrue;
	readTag = 0;

	for (i = 0; i < auFlow->numOutputs; i++)
	{
	    output = &auFlow->outputs[i];

	    for (j = 0; j < output->numInputs; j++)
		*output->inputs[j].preadTag = 0;
	}
    }
    else
    {
	auFlow = (CompiledFlowPtr) 0;
	status = AuFalse;
    }

    AuUnBlockAudio(l);

    if (status)
    {
	if (!processFlowEnabled)
	{
	    AuCallback(AuEnableProcessFlowCB, ());
	    processFlowEnabled = AuTrue;
	}
    }
    else if (processFlowEnabled)
    {
	AuCallback(AuDisableProcessFlowCB, ());
	processFlowEnabled = AuFalse;
    }

    return status;
}

static AuBool
readWaveForm(input)
CompiledFlowInputPtr input;
{
    ComponentPtr    c = input->component;
    FlowElementPtr  flowEl = input->flowEl;
    AuUint8        *s,
                   *d;
    AuUint32        size,
                    bytesOut,
                    bytesIn,
                    available;

    if (flowEl->parmsChanged)
    {
	flowEl->countSamples = flowEl->raw->importwaveform.num_samples !=
	    AuUnlimitedSamples;

	if (flowEl->countSamples)
	    flowEl->numBytes = flowEl->raw->importwaveform.num_samples *
		auNativeBytesPerSample;

	c->frequency = flowEl->raw->importwaveform.frequency * c->waveSamples;

	flowEl->parmsChanged = AuFalse;
    }

    d = flowEl->minibuf;
    s = flowEl->read;

    /* how much do we want */
    size = flowEl->minibufChunk;

    if (flowEl->countSamples && flowEl->numBytes)
	while (size)
	{
	    available = aumin(c->dataEnd - s, flowEl->numBytes);

	    /* do the rate conversion */
	    bytesOut = (*input->rateConvert)
		(flowEl, s, d, size, available, &bytesIn);
	    size -= bytesOut;
	    d += bytesOut;
	    s += bytesIn;
	    flowEl->numBytes -= bytesIn;

	    if (s == c->dataEnd)
		s = c->data;
	}
    else
	while (size)
	{
	    available = c->dataEnd - s;

	    /* do the rate conversion */
	    bytesOut = (*input->rateConvert)
		(flowEl, s, d, size, available, &bytesIn);
	    size -= bytesOut;
	    d += bytesOut;
	    s += bytesIn;

	    if (s == c->dataEnd)
		s = c->data;
	}

    flowEl->read = s;

    /* did we run out of data? */
    if (flowEl->countSamples && size)
    {
	flowEl->minibufSamples = bytesOut / c->bytesPerSample;
	return AuChangeElementState(flowEl->flow, flowEl->elementNum,
				    AuStateStop, AuFalse, AuReasonEOF);
    }

    return AuFalse;
}

static AuBool
readBucket(input)
CompiledFlowInputPtr input;
{
    ComponentPtr    c = input->component;
    FlowElementPtr  flowEl = input->flowEl;
    AuUint8        *s,
                   *d;
    AuUint32        size,
                    bytesOut,
                    bytesIn,
                    available;

    if (flowEl->parmsChanged)
    {
	flowEl->read = c->data + flowEl->raw->importbucket.offset *
	    c->bytesPerSample;

	if (flowEl->countSamples)
	    flowEl->numBytes = flowEl->raw->importbucket.num_samples *
		c->bytesPerSample;

	flowEl->parmsChanged = AuFalse;
    }

    s = flowEl->read;
    d = flowEl->minibuf;

    /* how much do we want */
    size = flowEl->minibufChunk;

    /* how much can we get from the bucket */
    available = c->dataEnd - s;

    if (flowEl->countSamples)
	available = aumin(available, flowEl->numBytes);

    /* do the rate conversion */
    bytesOut = (*input->rateConvert) (flowEl, s, d, size, available, &bytesIn);

    flowEl->read = s + bytesIn;
    flowEl->numBytes -= bytesIn;

    /* did we run out of data? */
    if (flowEl->read == c->dataEnd)
    {
	flowEl->minibufSamples = bytesOut / c->bytesPerSample;

	return AuChangeElementState(flowEl->flow, flowEl->elementNum,
				    AuStateStop, AuFalse, AuReasonEOF);
    }

    return AuFalse;
}

static AuBool
readClient(input)
CompiledFlowInputPtr input;
{
    ComponentPtr    c = input->component;
    FlowElementPtr  flowEl = input->flowEl;
    AuUint8        *s,
                   *d;
    AuUint32        pass1,
                    size,
                    totalBytesIn,
                    available,
                    bytesOut,
                    prevSize,
                    newSize;

    s = c->read;
    d = flowEl->minibuf;

    /* how much do we want */
    size = flowEl->minibufChunk;

    /* how much can we get from the port */
    available = aumin(c->currentSize, size);

    /* how much can we get before we need to wrap */
    pass1 = aumin(c->dataEnd - s, c->currentSize);

    /* do the rate conversion */
    bytesOut = (*input->rateConvert) (flowEl, s, d, size, pass1, &totalBytesIn);

    /* wrap if necessary */
    if ((s += totalBytesIn) == c->dataEnd)
	s = c->data;

    /* have we read all we can? */
    if (bytesOut < available && pass1 < c->currentSize)
    {
	AuUint32        pass2 = aumin(c->dataEnd - s,
				      c->currentSize - totalBytesIn),
	                bytesIn;

	d += bytesOut;
	size -= bytesOut;

	bytesOut +=
	    (*input->rateConvert) (flowEl, s, d, size, pass2, &bytesIn);
	s += bytesIn;
	totalBytesIn += bytesIn;
    }

    c->read = s;
    prevSize = c->currentSize;
    newSize = c->currentSize -= totalBytesIn;

    /* did we underrun? */
    if (!c->currentSize)
    {
	AuInt32         newState,
	                reason;

	flowEl->minibufSamples = bytesOut / c->bytesPerSample;

	/* if we've received an EOF then stop */
	if (c->eof)
	{
	    newState = AuStateStop;
	    reason = AuReasonEOF;
	}
	else
	{
	    /* otherwise pause */
	    newState = AuStatePause;
	    reason = AuReasonUnderrun;
	}

	return AuChangeElementState(flowEl->flow, flowEl->elementNum,
				    newState, AuFalse, reason);
    }

    /*
     * see if we hit the low water mark but don't ask for more if we've
     * already received an EOF
     */
    if ((prevSize > c->lowWaterMark || !c->incoming) &&
	newSize <= c->lowWaterMark && !c->eof)
	AuRequestElementNotifyEvent(AuElementNotifyKindLowWater,
				    AuReasonWatermark, flowEl);

    return AuFalse;
}

static AuBool
readDevice(input)
CompiledFlowInputPtr input;
{
    FlowElementPtr  flowEl = input->flowEl;

    if (flowEl->countSamples)
	if (flowEl->numBytes < flowEl->minibufChunk)
	    return AuChangeElementState(flowEl->flow, flowEl->elementNum,
					AuStateStop, AuFalse, AuReasonEOF);
	else
	    flowEl->numBytes -= flowEl->minibufChunk;

    return AuFalse;
}

static AuBool
readInput(input)
CompiledFlowInputPtr input;
{
    AuBool          flowStateChanged;
    FlowElementPtr  flowEl = input->flowEl;

    flowEl->minibufSamples = auMinibufSamples;
    flowStateChanged = (*input->readInput) (input);

    if (flowEl->toNativeFormat)
	(*flowEl->toNativeFormat) (flowEl->minibuf, input->component->numTracks,
				   input->flowEl->minibufSamples);

    *input->preadTag = readTag;
    return flowStateChanged;
}

static AuBool
writeClient(output)
CompiledFlowOutputPtr output;
{
    ComponentPtr    c = output->component;
    FlowElementPtr  flowEl = output->flowEl;
    AuUint8        *s,
                   *d;
    AuUint32        pass1,
                    size,
                    bytesIn,
                    bytesOut,
                    prevSize,
                    newSize;
    AuBool          overrun;

    s = flowEl->minibuf;
    d = c->write;

    /* how much do we want */
    size = flowEl->minibufBytes;

    overrun = size > (c->dataSize - c->currentSize);

    /* how much can we send before we need to wrap */
    pass1 = aumin(c->dataEnd - d, c->dataSize - c->currentSize);

    /* do the rate conversion */
    bytesOut = (*output->rateConvert) (flowEl, s, d, pass1, size, &bytesIn);

    /* wrap if necessary */
    if ((d += bytesOut) == c->dataEnd)
	d = c->data;

    /* have we written all we can? */
    if (pass1 < size)
    {
	AuUint32        pass2 = aumin(size - pass1, c->dataEnd - d),
	                n;

	s += bytesIn;
	size -= bytesIn;

	n = (*output->rateConvert) (flowEl, s, d, pass2, size, &bytesIn);

	d += n;
	bytesOut += n;
    }

    c->write = d;
    prevSize = c->currentSize;
    newSize = c->currentSize += bytesOut;

    /* did we overrun? */
    if (overrun)
	return AuChangeElementState(flowEl->flow, flowEl->elementNum,
				    AuStatePause, AuFalse,
				    AuReasonOverrun);

    /* see if we hit the high water mark */
    if ((prevSize < c->highWaterMark || !c->outgoing) &&
	newSize >= c->highWaterMark)
	AuRequestElementNotifyEvent(AuElementNotifyKindHighWater,
				    AuReasonWatermark, flowEl);

    return AuFalse;
}

static AuBool
writeBucket(output)
CompiledFlowOutputPtr output;
{
    ComponentPtr    c = output->component;
    FlowElementPtr  flowEl = output->flowEl;
    AuUint8        *s,
                   *d;
    AuUint32        size,
                    bytesIn,
                    bytesOut;
    AuBool          overrun;

    if (flowEl->parmsChanged)
    {
	flowEl->write = c->data + flowEl->raw->exportbucket.offset *
	    c->bytesPerSample;

	if (flowEl->countSamples)
	    flowEl->numBytes = flowEl->raw->exportbucket.num_samples *
		c->bytesPerSample;

	flowEl->parmsChanged = AuFalse;
    }

    s = flowEl->minibuf;
    d = flowEl->write;

    /* how much do we want */
    size = flowEl->minibufBytes;

    overrun = size > (c->dataEnd - d);

    if (flowEl->countSamples)
	size = aumin(size, flowEl->numBytes);

    /* do the rate conversion */
    bytesOut =
	(*output->rateConvert) (flowEl, s, d, c->dataEnd - d, size, &bytesIn);

    flowEl->write = d + bytesOut;
    flowEl->numBytes -= bytesOut;

    /* did we overrun? */
    if (overrun)
	return AuChangeElementState(flowEl->flow, flowEl->elementNum,
				    AuStateStop, AuFalse, AuReasonEOF);

    return AuFalse;
}


static AuBool
writeMonitor(output)
CompiledFlowOutputPtr output;
{
    AuInt32         n;

    if ((n = (*output->rateConvert) (output->flowEl)))
    {
	ComponentPtr    c = output->component;
	AuInt16        *s = (AuInt16 *) c->read;
	AuUint32       *p = (AuUint32 *) c->read;
	int             i;

	/*
	 * convert from native format - note that for a monitor the
	 * toNativeFormat pointer actually points to the fromNative format
	 * converter
	 */
	if (output->flowEl->toNativeFormat)
	    (*output->flowEl->toNativeFormat) (s, c->numTracks, n << 1);

	while (n--)
	{
	    AuRequestMonitorNotifyEvent(output->flowEl, s);
	    s += c->numTracks;
	}

	for (i = 0; i < (int) c->numTracks * auMinibufSamples; i++)
	    *p++ = 0x7fff8001;

	c->write = c->read;
	return AuTrue;
    }

    return AuFalse;
}

static AuBool
writeDevice(output)
CompiledFlowOutputPtr output;
{
    FlowElementPtr  flowEl = output->flowEl;
    ComponentPtr    c = output->component;

    c->minibufSamples = flowEl->minibufSamples;

    if (flowEl->countSamples)
    {
	if (flowEl->numBytes < flowEl->minibufBytes)
	    return AuChangeElementState(flowEl->flow, flowEl->elementNum,
					AuStateStop, AuFalse, AuReasonEOF);
	else
	    flowEl->numBytes -= flowEl->minibufBytes;
    }

    return AuFalse;
}

static AuBool
writeOutput(output)
CompiledFlowOutputPtr output;
{
    FlowElementPtr  flowEl = output->flowEl;

    if (flowEl->fromNativeFormat)
	(*flowEl->fromNativeFormat)
	    (flowEl->minibuf, output->component->numTracks,
	     flowEl->minibufSamples);

    return (*output->writeOutput) (output);
}

static void
accumulateOutput(input, output)
CompiledFlowInputPtr input;
CompiledFlowOutputPtr output;
{
    AuInt16        *in,
                   *out;
    AuUint32        i,
                    j;
    AuInt32         multiplyConstant,
                    addConstant;

    in = (AuInt16 *) input->flowEl->minibuf;
    out = (AuInt16 *) output->flowEl->minibuf;
    multiplyConstant = input->multiplyConstant;
    addConstant = input->addConstant;

    /*
     * input->numTracks == 0 means all input tracks map directly to the
     * output tracks
     */
    if (input->numTracks)
	for (i = 0; i < input->flowEl->minibufSamples; i++,
	     out += output->component->numTracks,
	     in += input->component->numTracks)
	    for (j = 0; j < input->numTracks; j++)
		out[input->outTrack[j]] += (in[input->inTrack[j]] *
				      multiplyConstant + addConstant) >> 16;
    else
	for (i = 0; i < input->flowEl->minibufSamples; i++)
	    for (j = 0; j < output->component->numTracks; j++)
		*out++ += (*in++ * multiplyConstant + addConstant) >> 16;
}

static void
accumulateOutputSimple(input, output)
CompiledFlowInputPtr input;
CompiledFlowOutputPtr output;
{
    AuInt16        *in,
                   *out;
    AuUint32        i;
    AuInt32         multiplyConstant;

    in = (AuInt16 *) input->flowEl->minibuf;
    out = (AuInt16 *) output->flowEl->minibuf;
    multiplyConstant = input->multiplyConstant;

    for (i = 0; i < input->flowEl->minibufSamples; i++)
	*out++ += (*in++ * multiplyConstant) >> 16;
}

/* process an audio flow */
void
AuProcessFlow(fl, clocked)
CompiledFlowPtr fl;
AuBool          clocked;
{
    AuInt32         i,
                    j;
    CompiledFlowOutputPtr output;
    CompiledFlowInputPtr input;
    AuBool          flowStateChanged = AuFalse;

    if (fl->physicalDeviceMask & AllPhysicalInputs)
	AuCallback(AuReadPhysicalInputsCB, ());

    readTag++;

    /* clear the physical output buffers */
    if (clocked)
	auclr(auPhysicalOutputBuffers, auPhysicalOutputBuffersSize);

    for (i = 0; i < fl->numOutputs; i++)
    {
	AuUint32        outputState;
	ComponentPtr    oc;
	FlowElementPtr  oel;

	output = &fl->outputs[i];
	oc = output->component;
	oel = output->flowEl;
	outputState = oel->state;

	/* clear the output minibuffer */
	if (oc->kind != AuComponentKindPhysicalOutput)
	    bzero(oel->minibuf, oc->minibufSize);

	oel->minibufSamples = 0;

	for (j = 0; j < output->numInputs; j++)
	{
	    input = &output->inputs[j];

	    if (input->flowEl->state == AuStateStart)
	    {
		if (*input->preadTag != readTag)
		    flowStateChanged |= readInput(input);

		if (outputState == AuStateStart)
		{
		    (*fl->accumulateOutput) (input, output);
		    oel->minibufSamples = aumax(oel->minibufSamples,
					     input->flowEl->minibufSamples);
		}
	    }
	}

	if (outputState == AuStateStart)
	{
	    oel->minibufBytes = oel->minibufSamples * oc->bytesPerSample;
	    flowStateChanged |= writeOutput(output);
	}
    }

    if (clocked)
    {
	(*writePhysicalOutputs) (fl);

	if (flowStateChanged)
	    AuRequestElementNotifyEvent(AuElementNotifyKindSpecial, 0,
					(FlowElementPtr) 0);
    }
}

void
AuProcessData()
{
    if (auFlow)
	AuProcessFlow(auFlow, AuTrue);
    else if (processFlowEnabled)
    {
	AuCallback(AuDisableProcessFlowCB, ());
	processFlowEnabled = AuFalse;
    }
}

void
AuSetInputGainAndLineMode(gain, lineMode)
AuFixedPoint    gain;
AuUint8         lineMode;
{
    AuCallback(AuSetPhysicalInputGainAndLineModeCB, (gain, lineMode));
}

void
AuSetOutputGain(gain)
AuFixedPoint    gain;
{
    desiredOutputGain = gain;

    if (gain > maxOutputGain)
	gain = maxOutputGain;

    AuCallback(AuSetPhysicalOutputGainCB, (gain));
}

AuFixedPoint
AuGetOutputGain()
{
    return (AuFixedPoint) AuCallback(AuGetPhysicalOutputGainCB, ());
}

void
AuSetMaxOutputGain(gain)
AuFixedPoint    gain;
{
    maxOutputGain = gain;
    AuSetOutputGain(desiredOutputGain);
}

AuUint32
AuGetMaxOutputGain()
{
    return maxOutputGain;
}

static void
doSetup(flowEl, rateConverter, globalRate, isInput)
FlowElementPtr  flowEl;
AuUint32        (**rateConverter) (),
                globalRate;
AuBool          isInput;
{
    AuUint32        elementRate;
    ComponentPtr    c = flowEl->component;

    if (!flowEl->setup)
    {
	AuInt32         format = c->format;

	flowEl->minibufChunk = c->minibufSize;
	flowEl->toNativeFormat = converters[format].toNativeFormat;
	flowEl->fromNativeFormat = converters[format].fromNativeFormat;

	if (flowEl->raw->type == AuElementTypeExportMonitor)
	{
	    flowEl->toNativeFormat = flowEl->fromNativeFormat;
	    flowEl->fromNativeFormat = (void (*) ()) 0;
	}
	else if (sizeofFormat(format) == 1)
	    flowEl->minibufChunk >>= 1;

	flowEl->setup = AuTrue;
    }

    if (flowEl->raw->type == AuElementTypeImportBucket)
	elementRate = flowEl->sampleRate;
    else if (flowEl->raw->type == AuElementTypeImportWaveForm)
    {
	AuUint32        integ,
	                fract;

	elementRate = c->frequency;
	integ = elementRate / globalRate;
	fract = ((elementRate - (integ * globalRate)) << 16) / globalRate;
	flowEl->nextSample = (integ << 16) | fract;
    }
    else
	elementRate = c->sampleRate;

    if (flowEl->raw->type != AuElementTypeImportWaveForm)
	flowEl->nextSample = isInput ? (elementRate << 16) / globalRate :
	    (globalRate << 16) / elementRate;

    if (flowEl->raw->type == AuElementTypeExportMonitor)
    {
	if (c->numTracks == 1)
	    *rateConverter = rcm_1;
	else if (c->numTracks == 2)
	    *rateConverter = rcm_2;
	else
	    *rateConverter = rcm_n;
    }
    else if (elementRate == globalRate)
	*rateConverter = rcNull;
    else if (c->bytesPerSample == 1)
	*rateConverter = rc_1;
    else if (c->bytesPerSample == 2)
	*rateConverter = rc_2;
    else if (c->bytesPerSample == 4)
	*rateConverter = rc_4;
    else
	*rateConverter = rc_n;
}

void
AuSetupCompiledFlow(fl, rate)
CompiledFlowPtr fl;
AuUint32        rate;
{
    CompiledFlowOutputPtr output;
    CompiledFlowInputPtr input;
    AuInt32         i,
                    j;
    AuBool          simple = AuTrue;
    extern auConnSetup auSetup;
    static AuBool   (*readInputTable[]) () =
    {
	readClient, readDevice, readBucket, readWaveForm
    };
    static AuBool   (*writeOutputTable[]) () =
    {
	writeClient, writeDevice, writeBucket, (AuBool (*) ()) 0,
	writeMonitor
    };

    if (rate)
    {
	if (rate > auSetup.maxSampleRate)
	    rate = auSetup.maxSampleRate;

	if (rate < auSetup.minSampleRate)
	    rate = auSetup.minSampleRate;

	if (CallbackExists(AuSetSampleRateCB))
	{
	    currentSampleRate = rate =
		(AuUint32) AuCallback(AuSetSampleRateCB, (rate));
	}
	else
	    currentSampleRate = rate;
    }

    for (i = 0; i < fl->numOutputs; i++)
    {
	output = &fl->outputs[i];

	if (output->component->numTracks > 1)
	    simple = AuFalse;

	for (j = 0; j < output->numInputs; j++)
	{
	    input = &output->inputs[j];

	    if (input->addConstant || input->numTracks)
		simple = AuFalse;

	    input->readInput = readInputTable[input->flowEl->raw->type];
	    doSetup(input->flowEl, &input->rateConvert, rate, AuTrue);
	}

	output->writeOutput = writeOutputTable[output->flowEl->raw->type -
					       AuElementTypeExportClient];
	doSetup(output->flowEl, &output->rateConvert, rate, AuFalse);
    }

    fl->accumulateOutput = simple ? accumulateOutputSimple : accumulateOutput;

    return;
}

#define rcm_x(_name, _tracks, _minmax)					      \
static AuUint32								      \
_name(el)								      \
FlowElementPtr  el;							      \
{									      \
    AuInt16        *s = (AuInt16 *) el->minibuf,			      \
                   *d = (AuInt16 *) el->component->write;		      \
    AuUint32        inc,						      \
                    c = el->currentSample,				      \
                    n = el->nextSample;					      \
    AuInt32         i,							      \
                    avail = el->minibufBytes / el->component->bytesPerSample, \
                    count = 0;						      \
									      \
    if (c & 0xffff0000)							      \
	goto NEXT;							      \
									      \
    while (avail)							      \
    {									      \
	c += n;								      \
NEXT:									      \
	if ((inc = c >> 16))						      \
	{								      \
	    AuInt16 *p = d;						      \
									      \
	    c &= 0xffff;						      \
									      \
	    if (inc > avail)						      \
	    {								      \
		c |= (inc - avail) << 16;				      \
		inc = avail;						      \
	    }								      \
	    else							      \
	    {								      \
		d += _tracks * 2;					      \
		count++;						      \
	    }								      \
									      \
	    avail -= inc;						      \
									      \
	    while (inc--)						      \
	    {								      \
		AuInt16 v;						      \
									      \
		_minmax;						      \
	    }								      \
	}								      \
    }									      \
									      \
    el->currentSample = c;						      \
    el->component->write = (AuUint8 *) d;				      \
    return count;							      \
}

#define ONE_TRACK							      \
{									      \
    v = *s++;								      \
									      \
    if (v < p[0])							      \
	p[0] = v;							      \
									      \
    if (v > p[1])							      \
	p[1] = v;							      \
}

#define TWO_TRACKS							      \
{									      \
    AuInt16 *p1 = p;							      \
									      \
    ONE_TRACK;								      \
    p += 2;								      \
    ONE_TRACK;								      \
    p = p1;								      \
}

#define N_TRACKS							      \
{									      \
    AuInt16 *p1 = p;							      \
									      \
    for (i = 0; i < (int) el->component->numTracks; i++, p += 2)	      \
	ONE_TRACK;							      \
									      \
    p = p1;								      \
}

rcm_x(rcm_1, 1, ONE_TRACK)
rcm_x(rcm_2, 2, TWO_TRACKS)
rcm_x(rcm_n, el->component->numTracks, N_TRACKS)

#define rc_x(_name, _bps, _copy)					      \
static AuUint32								      \
_name(el, src, dst, want, avail, in)					      \
FlowElementPtr  el;							      \
AuUint8        *src,							      \
               *dst;							      \
AuInt32         want,							      \
                avail;							      \
AuUint32       *in;							      \
{									      \
    AuUint8        *s = src,						      \
                   *d = dst;						      \
    AuUint32        inc,						      \
                    bps = el->component->bytesPerSample,		      \
                    c = el->currentSample,				      \
                    n = el->nextSample;					      \
									      \
    if (c & 0xffff0000)							      \
	goto NEXT;							      \
									      \
    while (want && avail)						      \
    {									      \
	_copy;								      \
        d += _bps;							      \
	want -= _bps;							      \
									      \
	c += n;								      \
NEXT:									      \
	if ((inc = c >> 16))						      \
	{								      \
	    inc *= _bps;						      \
	    c &= 0xffff;						      \
									      \
	    if (inc > avail)						      \
	    {								      \
		c |= ((inc - avail) / _bps) << 16;			      \
		inc = avail;						      \
	    }								      \
									      \
	    s += inc;							      \
	    avail -= inc;						      \
	}								      \
    }									      \
									      \
    el->currentSample = c;						      \
    *in = s - src;							      \
    return d - dst;							      \
}

rc_x(rc_1, 1, *((AuInt8 *) d) = *(AuInt8 *) s)
rc_x(rc_2, 2, *((AuInt16 *) d) = *(AuInt16 *) s)
rc_x(rc_4, 4, *((AuInt32 *) d) = *(AuInt32 *) s)
rc_x(rc_n, bps, aucopy(s, d, bps))

static AuUint32
rcNull(el, src, dst, want, avail, in)
FlowElementPtr  el;
AuUint8        *src,
               *dst;
AuInt32         want,
                avail;
AuUint32       *in;
{
    AuUint32        size = aumin(want, avail);

    aucopy(src, dst, size);
    return *in = size;
}
