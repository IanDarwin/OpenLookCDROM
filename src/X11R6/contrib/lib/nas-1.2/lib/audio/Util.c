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
 * $NCDId: @(#)Util.c,v 1.19 1994/02/02 18:37:03 greg Exp $
 */

#include "Alibint.h"

AuFlowID
AuGetScratchFlow(aud, ret_status)
AuServer       *aud;
AuStatus       *ret_status;
{
    AuFlowID        flow;
    int             i;

    if (aud->scratch_flows.num_inuse == AU_MAX_SCRATCH_FLOWS)
	return AuCreateFlow(aud, ret_status);

    for (i = 0; i < aud->scratch_flows.total; i++)
	if (!aud->scratch_flows.flows[i].inuse)
	{
	    aud->scratch_flows.flows[i].inuse = AuTrue;
	    aud->scratch_flows.num_inuse++;
	    return aud->scratch_flows.flows[i].flow;
	}

    if ((flow = AuCreateFlow(aud, ret_status)) != AuNone)
    {
	aud->scratch_flows.flows[aud->scratch_flows.total].flow = flow;
	aud->scratch_flows.flows[aud->scratch_flows.total].inuse = AuTrue;
	aud->scratch_flows.total++;
	aud->scratch_flows.num_inuse++;
    }

    return flow;
}

AuFlowID
AuGetScratchFlowToBucket(aud, bucket, import, ret_status)
AuServer       *aud;
AuBucketID      bucket;
int            *import;
AuStatus       *ret_status;
{
    AuElement       elements[2];
    AuFlowID        flow;
    AuBucketAttributes *ba;

    if (!(flow = AuGetScratchFlow(aud, ret_status)))
	return NULL;

    if (!(ba = AuGetBucketAttributes(aud, bucket, ret_status)))
    {
	AuReleaseScratchFlow(aud, flow, ret_status);
	return NULL;
    }

    AuMakeElementImportClient(&elements[0], AuBucketSampleRate(ba),
			      AuBucketFormat(ba), AuBucketNumTracks(ba),
			      AuTrue, 0, 0, 0, NULL);
    AuMakeElementExportBucket(&elements[1], 0, bucket,
			      AuBucketNumSamples(ba), 0, 0, NULL);

    /* set up the flow */
    AuSetElements(aud, flow, AuFalse, 2, elements, ret_status);

    *import = 0;
    AuFreeBucketAttributes(aud, 1, ba);
    return flow;
}

AuFlowID
AuGetScratchFlowFromBucket(aud, bucket, export, ret_status)
AuServer       *aud;
AuBucketID      bucket;
int            *export;
AuStatus       *ret_status;
{
    AuElement       elements[2];
    AuFlowID        flow;
    AuBucketAttributes *ba;

    if (!(flow = AuGetScratchFlow(aud, ret_status)))
	return NULL;

    if (!(ba = AuGetBucketAttributes(aud, bucket, ret_status)))
    {
	AuReleaseScratchFlow(aud, flow, ret_status);
	return NULL;
    }

    AuMakeElementImportBucket(&elements[0], AuBucketSampleRate(ba), bucket,
			      AuBucketNumSamples(ba), 0, 0, NULL);
    AuMakeElementExportClient(&elements[1], 0, AuBucketSampleRate(ba),
			      AuBucketFormat(ba), AuBucketNumTracks(ba),
			      AuTrue, 0, 0, 0, NULL);

    /* set up the flow */
    AuSetElements(aud, flow, AuFalse, 2, elements, ret_status);

    *export = 1;
    AuFreeBucketAttributes(aud, 1, ba);
    return flow;
}

void
AuReleaseScratchFlow(aud, flow, ret_status)
AuServer       *aud;
AuFlowID        flow;
AuStatus       *ret_status;
{
    int             i;

    for (i = 0; i < aud->scratch_flows.total; i++)
	if (aud->scratch_flows.flows[i].flow == flow)
	{
	    aud->scratch_flows.flows[i].inuse = AuFalse;
	    aud->scratch_flows.num_inuse--;
	    return;
	}

    AuDestroyFlow(aud, flow, ret_status);
}

void
AuStartFlow(aud, flow, ret_status)
AuServer       *aud;
AuFlowID        flow;
AuStatus       *ret_status;
{
    AuElementState  states[1];

    AuMakeElementState(&states[0], flow, AuElementAll, AuStateStart);
    AuSetElementStates(aud, 1, states, ret_status);
}

void
AuStopFlow(aud, flow, ret_status)
AuServer       *aud;
AuFlowID        flow;
AuStatus       *ret_status;
{
    AuElementState  states[1];

    AuMakeElementState(&states[0], flow, AuElementAll, AuStateStop);
    AuSetElementStates(aud, 1, states, ret_status);
}

void
AuPauseFlow(aud, flow, ret_status)
AuServer       *aud;
AuFlowID        flow;
AuStatus       *ret_status;
{
    AuElementState  states[1];

    AuMakeElementState(&states[0], flow, AuElementAll, AuStatePause);
    AuSetElementStates(aud, 1, states, ret_status);
}

static struct
{
    int             format;
    char           *string,
                   *define;
}               formats[] =
{
    AuFormatULAW8, "8-bit uLAW", "AuFormatULAW8",
    AuFormatLinearUnsigned8, "8-bit unsigned linear", "AuFormatLinearUnsigned8",
    AuFormatLinearSigned8, "8-bit signed linear", "AuFormatLinearSigned8",
    AuFormatLinearSigned16MSB, "16-bit signed linear (big endian)",
    "AuFormatLinearSigned16MSB",
    AuFormatLinearUnsigned16MSB, "16-bit unsigned linear (big endian)",
    "AuFormatLinearUnsigned16MSB",
    AuFormatLinearSigned16LSB, "16-bit signed linear (little endian)",
    "AuFormatLinearSigned16LSB",
    AuFormatLinearUnsigned16LSB, "16-bit unsigned linear (little endian)",
    "AuFormatLinearUnsigned16LSB",
};

_AuConst char  *
AuFormatToString(format)
unsigned int    format;
{
    int             i;

    for (i = 0; i < sizeof(formats) / sizeof(formats[0]); i++)
	if (formats[i].format == format)
	    return formats[i].string;

    return "Unknown";
}

int
AuStringToFormat(s)
_AuConst char  *s;
{
    int             i;

    for (i = 0; i < sizeof(formats) / sizeof(formats[0]); i++)
	if (!strcasecmp(s, formats[i].string))
	    return formats[i].format;

    return -1;
}

_AuConst char  *
AuFormatToDefine(format)
unsigned int    format;
{
    int             i;

    for (i = 0; i < sizeof(formats) / sizeof(formats[0]); i++)
	if (formats[i].format == format)
	    return formats[i].define;

    return "Unknown";
}

int
AuDefineToFormat(s)
_AuConst char  *s;
{
    int             i;

    for (i = 0; i < sizeof(formats) / sizeof(formats[0]); i++)
	if (!strcasecmp(s, formats[i].define))
	    return formats[i].format;

    return -1;
}

static struct
{
    int             waveform;
    char           *string;
}               waveforms[] =
{
    AuWaveFormSquare, "Square",
    AuWaveFormSine, "Sine",
    AuWaveFormSaw, "Saw",
    AuWaveFormConstant, "Constant",
};

_AuConst char  *
AuWaveFormToString(waveform)
unsigned int    waveform;
{
    int             i;

    for (i = 0; i < sizeof(waveforms) / sizeof(waveforms[0]); i++)
	if (waveforms[i].waveform == waveform)
	    return waveforms[i].string;

    return "Unknown";
}

int
AuStringToWaveForm(s)
_AuConst char  *s;
{
    int             i;

    for (i = 0; i < sizeof(waveforms) / sizeof(waveforms[0]); i++)
	if (!strcasecmp(s, waveforms[i].string))
	    return waveforms[i].waveform;

    return -1;
}
