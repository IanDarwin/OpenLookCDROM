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
 * $NCDId: @(#)GetElement.c,v 1.4 1994/03/22 22:50:00 greg Exp $
 */

#include "Alibint.h"

AuElement      *
AuGetElements(aud, flow, pclocked, pnum_elements, ret_status)
AuServer       *aud;
AuFlowID        flow;
AuBool         *pclocked;
int            *pnum_elements;
AuStatus       *ret_status;
{
    register auResourceReq *req;
    auGetElementsReply rep;
    AuElement      *elements;
    int             i;

    _AuLockServer(aud);
    _AuGetResReq(GetElements, flow, req, aud);

    (void) _AuReply(aud, (auReply *) & rep, 0, auFalse, ret_status);

    *pclocked = rep.clocked;
    *pnum_elements = rep.num_elements;

    if (!(elements =
	  (AuElement *) Aucalloc(rep.num_elements, sizeof(AuElement))))
    {
	_AuUnlockServer(aud);
	_AuSyncHandle(aud);
	return NULL;
    }

    for (i = 0; i < rep.num_elements; i++)
    {
	AuElement      *el;
	auElement       e;
	AuElementActionList *actions;
	AuBool          bundle,
	                sum;

	actions = NULL;
	bundle = sum = AuFalse;
	el = &elements[i];

	_AuReadPad(aud, (char *) &e, SIZEOF(auElement));

	el->type = e.type;

	switch (el->type)
	{
	    case AuElementTypeImportClient:
#undef xfer
#define xfer(x)	el->importclient.x = e.importclient.x
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
#define xfer(x)	el->importdevice.x = e.importdevice.x
		xfer(sample_rate);
		xfer(num_samples);
		xfer(device);
		xfer(actions.num_actions);
		actions = &el->importdevice.actions;
		break;
	    case AuElementTypeImportBucket:
#undef xfer
#define xfer(x)	el->importbucket.x = e.importbucket.x
		xfer(sample_rate);
		xfer(num_samples);
		xfer(bucket);
		el->importbucket.parms[AuParmsImportBucketOffset] =
		    e.importbucket.offset;
		xfer(actions.num_actions);
		actions = &el->importbucket.actions;
		break;
	    case AuElementTypeImportWaveForm:
#undef xfer
#define xfer(x)	el->importwaveform.x = e.importwaveform.x
		xfer(sample_rate);
		xfer(wave_form);
		el->importwaveform.parms[AuParmsImportWaveFormFrequency] =
		    e.importwaveform.frequency;
		el->importwaveform.parms[AuParmsImportWaveFormNumSamples] =
		    e.importwaveform.num_samples;
		xfer(actions.num_actions);
		actions = &el->importwaveform.actions;
		break;
	    case AuElementTypeBundle:
#undef xfer
#define xfer(x)	el->bundle.x = e.bundle.x
		xfer(num_inputs);
		bundle = AuTrue;
		break;
	    case AuElementTypeMultiplyConstant:
#undef xfer
#define xfer(x)	el->multiplyconstant.x = e.multiplyconstant.x
		xfer(input);
		el->multiplyconstant.parms[AuParmsMultiplyConstantConstant] =
		    e.multiplyconstant.constant;
		break;
	    case AuElementTypeAddConstant:
#undef xfer
#define xfer(x)	el->addconstant.x = e.addconstant.x
		xfer(input);
		el->addconstant.parms[AuParmsAddConstantConstant] =
		    e.addconstant.constant;
		break;
	    case AuElementTypeSum:
#undef xfer
#define xfer(x)	el->sum.x = e.sum.x
		xfer(num_inputs);
		sum = AuTrue;
		break;
	    case AuElementTypeExportClient:
#undef xfer
#define xfer(x)	el->exportclient.x = e.exportclient.x
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
#define xfer(x)	el->exportdevice.x = e.exportdevice.x
		xfer(sample_rate);
		xfer(num_samples);
		xfer(input);
		xfer(device);
		xfer(actions.num_actions);
		actions = &el->exportdevice.actions;
		break;
	    case AuElementTypeExportBucket:
#undef xfer
#define xfer(x)	el->exportbucket.x = e.exportbucket.x
		xfer(input);
		xfer(num_samples);
		xfer(bucket);
		el->exportbucket.parms[AuParmsExportBucketOffset] =
		    e.exportbucket.offset;
		xfer(actions.num_actions);
		actions = &el->exportbucket.actions;
		break;
	    case AuElementTypeExportMonitor:
#undef xfer
#define xfer(x)	el->exportmonitor.x = e.exportmonitor.x
		xfer(input);
		xfer(event_rate);
		xfer(format);
		xfer(num_tracks);
		break;
	}

	if (actions)
	{
	    auElementAction a;
	    AuElementAction *act;
	    int             j;

	    if (!(actions->actions = act =
		  (AuElementAction *) Aumalloc(actions->num_actions *
					       sizeof(AuElementAction))))
	    {
		AuFreeElements(aud, rep.num_elements, elements);
		_AuUnlockServer(aud);
		_AuSyncHandle(aud);
		return NULL;
	    }

	    for (j = 0; j < actions->num_actions; j++, act++)
	    {
		_AuReadPad(aud, (char *) &a, SIZEOF(auElementAction));
#undef xfer
#define xfer(x) act->x = a.x
		xfer(flow);
		xfer(element_num);
		xfer(trigger_state);
		xfer(trigger_prev_state);
		xfer(trigger_reason);
		xfer(action);
		xfer(new_state);
	    }
	}
	else if (bundle)
	{
	    auInputTrack    t;
	    AuInputTrack   *tr;
	    int             j;

	    if (!(el->bundle.inputs = tr = (AuInputTrack *)
		  Aumalloc(el->bundle.num_inputs * sizeof(AuInputTrack))))
	    {
		AuFreeElements(aud, rep.num_elements, elements);
		_AuUnlockServer(aud);
		_AuSyncHandle(aud);
		return NULL;
	    }

	    for (j = 0; j < (int) el->bundle.num_inputs; j++, tr++)
	    {
		_AuReadPad(aud, (char *) &t, SIZEOF(auInputTrack));
#undef xfer
#define xfer(x) tr->x = t.x
		xfer(element_num);
		xfer(track);
	    }
	}
	else if (sum)
	{
	    int             n = el->sum.num_inputs * sizeof(unsigned short);

	    if (!(el->sum.inputs = (unsigned short *) Aumalloc(n)))
	    {
		AuFreeElements(aud, rep.num_elements, elements);
		_AuUnlockServer(aud);
		_AuSyncHandle(aud);
		return NULL;
	    }

	    _AuReadPad(aud, (char *) el->sum.inputs, n);
	}
    }

    _AuUnlockServer(aud);
    _AuSyncHandle(aud);

    return elements;
}

void
AuFreeElements(aud, num_elements, elements)
AuServer       *aud;
int             num_elements;
AuElement      *elements;
{
    int             i;

    for (i = 0; i < num_elements; i++)
    {
	AuElement      *el;
	AuElementActionList *actions;
	AuBool          bundle,
	                sum;

	actions = NULL;
	bundle = sum = AuFalse;
	el = &elements[i];

	switch (el->type)
	{
	    case AuElementTypeImportClient:
		actions = &el->importclient.actions;
		break;
	    case AuElementTypeImportDevice:
		actions = &el->importdevice.actions;
		break;
	    case AuElementTypeImportBucket:
		actions = &el->importbucket.actions;
		break;
	    case AuElementTypeImportWaveForm:
		actions = &el->importwaveform.actions;
		break;
	    case AuElementTypeBundle:
		bundle = AuTrue;
		break;
	    case AuElementTypeMultiplyConstant:
		break;
	    case AuElementTypeAddConstant:
		break;
	    case AuElementTypeSum:
		sum = AuTrue;
		break;
	    case AuElementTypeExportClient:
		actions = &el->exportclient.actions;
		break;
	    case AuElementTypeExportDevice:
		actions = &el->exportdevice.actions;
		break;
	    case AuElementTypeExportBucket:
		actions = &el->exportbucket.actions;
		break;
	    case AuElementTypeExportMonitor:
		break;
	}

	if (actions)
	    Aufree(actions->actions);
	else if (bundle)
	    Aufree(el->bundle.inputs);
	else if (sum)
	    Aufree(el->sum.inputs);
    }

    Aufree(elements);
}
