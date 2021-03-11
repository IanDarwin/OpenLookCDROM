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
 * $NCDId: @(#)SetElement.c,v 1.15 1994/03/22 22:50:27 greg Exp $
 */

#include "Alibint.h"

static void
writeActions(aud, list)
AuServer       *aud;
AuElementActionList *list;
{
    int             i;
    AuElementAction *action;
    auElementAction a;

    for (i = 0; i < list->num_actions; i++)
    {
	action = &list->actions[i];

#undef xfer
#define xfer(x) a.x = action->x
	xfer(flow);
	xfer(element_num);
	xfer(trigger_state);
	xfer(trigger_prev_state);
	xfer(trigger_reason);
	xfer(action);
	xfer(new_state);

	_AuData32(aud, &a, SIZEOF(auElementAction));
    }
}

void
AuSetElements(aud, flow, clocked, num_elements, elements, ret_status)
AuServer       *aud;
AuFlowID        flow;
AuBool          clocked;
int             num_elements;
AuElement      *elements;
AuStatus       *ret_status;
{
    register auSetElementsReq *req;
    AuElement      *el;
    auElement       e;
    int             varLen,
                    i;
    AuBool          bundle,
                    sum;

    if (ret_status)
	*ret_status = AuSuccess;

    /* calc the amount of variable length data */
    for (el = &elements[0], i = varLen = 0; i < num_elements; i++, el++)
	switch (el->type)
	{
	    case AuElementTypeImportClient:
		varLen += el->importclient.actions.num_actions *
		    SIZEOF(auElementAction);
		break;
	    case AuElementTypeImportDevice:
		varLen += el->importdevice.actions.num_actions *
		    SIZEOF(auElementAction);
		break;
	    case AuElementTypeImportBucket:
		varLen += el->importbucket.actions.num_actions *
		    SIZEOF(auElementAction);
		break;
	    case AuElementTypeImportWaveForm:
		varLen += el->importwaveform.actions.num_actions *
		    SIZEOF(auElementAction);
		break;
	    case AuElementTypeBundle:
		varLen += el->bundle.num_inputs * SIZEOF(auInputTrack);
		break;
	    case AuElementTypeMultiplyConstant:
		break;
	    case AuElementTypeAddConstant:
		break;
	    case AuElementTypeSum:
		varLen += PAD4(el->sum.num_inputs * sizeof(CARD16));
		break;
	    case AuElementTypeExportClient:
		varLen += el->exportclient.actions.num_actions *
		    SIZEOF(auElementAction);
		break;
	    case AuElementTypeExportDevice:
		varLen += el->exportdevice.actions.num_actions *
		    SIZEOF(auElementAction);
		break;
	    case AuElementTypeExportBucket:
		varLen += el->exportbucket.actions.num_actions *
		    SIZEOF(auElementAction);
		break;
	    case AuElementTypeExportMonitor:
		break;
	    default:
		if (ret_status)
		    *ret_status = AuBadElement;
		return;
	}

    _AuLockServer(aud);
    _AuGetReq(SetElements, req, aud);

    req->flow = flow;
    req->clocked = clocked;
    req->numElements = num_elements;
    req->length += (num_elements * SIZEOF(auElement) + varLen) >> 2;

    for (el = &elements[0], i = 0; i < num_elements; i++, el++)
    {
	AuElementActionList *actions;

	actions = NULL;
	bundle = sum = AuFalse;

	switch (e.type = el->type)
	{
	    case AuElementTypeImportClient:
#undef xfer
#define xfer(x)	e.importclient.x = el->importclient.x
		xfer(sample_rate);
		xfer(format);
		xfer(num_tracks);
		xfer(discard);
		xfer(max_samples);
		xfer(low_water_mark);
		xfer(actions.num_actions);
		actions = &el->importclient.actions;
		break;
	    case AuElementTypeImportDevice:
#undef xfer
#define xfer(x)	e.importdevice.x = el->importdevice.x
		xfer(sample_rate);
		xfer(num_samples);
		xfer(device);
		xfer(actions.num_actions);
		actions = &el->importdevice.actions;
		break;
	    case AuElementTypeImportBucket:
#undef xfer
#define xfer(x)	e.importbucket.x = el->importbucket.x
		xfer(sample_rate);
		xfer(num_samples);
		xfer(bucket);
		e.importbucket.offset =
		    el->importbucket.parms[AuParmsImportBucketOffset];
		xfer(actions.num_actions);
		actions = &el->importbucket.actions;
		break;
	    case AuElementTypeImportWaveForm:
#undef xfer
#define xfer(x)	e.importwaveform.x = el->importwaveform.x
		xfer(sample_rate);
		xfer(wave_form);
		e.importwaveform.frequency =
		    el->importwaveform.parms[AuParmsImportWaveFormFrequency];
		e.importwaveform.num_samples =
		    el->importwaveform.parms[AuParmsImportWaveFormNumSamples];
		xfer(actions.num_actions);
		actions = &el->importwaveform.actions;
		break;
	    case AuElementTypeBundle:
#undef xfer
#define xfer(x)	e.bundle.x = el->bundle.x
		xfer(num_inputs);
		bundle = AuTrue;
		break;
	    case AuElementTypeMultiplyConstant:
#undef xfer
#define xfer(x)	e.multiplyconstant.x = el->multiplyconstant.x
		xfer(input);
		e.multiplyconstant.constant =
		    el->multiplyconstant.parms[AuParmsMultiplyConstantConstant];
		break;
	    case AuElementTypeAddConstant:
#undef xfer
#define xfer(x)	e.addconstant.x = el->addconstant.x
		xfer(input);
		e.addconstant.constant =
		    el->addconstant.parms[AuParmsAddConstantConstant];
		break;
	    case AuElementTypeSum:
#undef xfer
#define xfer(x)	e.sum.x = el->sum.x
		xfer(num_inputs);
		sum = AuTrue;
		break;
	    case AuElementTypeExportClient:
#undef xfer
#define xfer(x)	e.exportclient.x = el->exportclient.x
		xfer(sample_rate);
		xfer(input);
		xfer(format);
		xfer(num_tracks);
		xfer(discard);
		xfer(max_samples);
		xfer(high_water_mark);
		xfer(actions.num_actions);
		actions = &el->exportclient.actions;
		break;
	    case AuElementTypeExportDevice:
#undef xfer
#define xfer(x)	e.exportdevice.x = el->exportdevice.x
		xfer(sample_rate);
		xfer(num_samples);
		xfer(input);
		xfer(device);
		xfer(actions.num_actions);
		actions = &el->exportdevice.actions;
		break;
	    case AuElementTypeExportBucket:
#undef xfer
#define xfer(x)	e.exportbucket.x = el->exportbucket.x
		xfer(input);
		xfer(num_samples);
		xfer(bucket);
		e.exportbucket.offset =
		    el->exportbucket.parms[AuParmsExportBucketOffset];
		xfer(actions.num_actions);
		actions = &el->exportbucket.actions;
		break;
	    case AuElementTypeExportMonitor:
#undef xfer
#define xfer(x)	e.exportmonitor.x = el->exportmonitor.x
		xfer(input);
		xfer(event_rate);
		xfer(format);
		xfer(num_tracks);
		break;
	}

	_AuData32(aud, &e, SIZEOF(auElement));

	if (actions)
	    writeActions(aud, actions);
	else if (bundle)
	{
	    auInputTrack    t;
	    AuInputTrack   *tr = el->bundle.inputs;
	    int             k;

#undef xfer
#define xfer(x) t.x = tr->x

	    for (k = 0; k < (int) el->bundle.num_inputs; k++, tr++)
	    {
		xfer(element_num);
		xfer(track);
		_AuData32(aud, &t, SIZEOF(auInputTrack));
	    }
	}
	else if (sum)
	{
	    int             n = el->sum.num_inputs * sizeof(CARD16);

	    _AuData16(aud, el->sum.inputs, n);
	}
    }

    (void) _AuIfRoundTrip(aud, ret_status);
    _AuUnlockServer(aud);
    _AuSyncHandle(aud);
}
