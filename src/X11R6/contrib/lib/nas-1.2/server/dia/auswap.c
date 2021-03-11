/*
 * Copyright 1993 Network Computing Devices, Inc.
 *
 * Permission to use, copy, modify, distribute, and sell this software and
 * its documentation for any purpose is hereby granted without fee, provided
 * that the above copyright notice appear in all copies and that both that
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
 * $NCDId: @(#)auswap.c,v 1.7 1994/04/20 22:35:21 greg Exp $
 */

#include	<audio/audio.h>
#include	<audio/Aproto.h>
#include	"misc.h"
#include	"dixstruct.h"
#include	"au.h"

extern void WriteToClient();

extern int      (*AuProcVector[256]) ();

static void SwapDeviceAttributes();
static void SwapBucketAttributes();


/* audio request/reply swappers */

int
SProcAuSimpleReq(client)
ClientPtr       client;
{
    char            n;

    REQUEST(auReq);
    swaps(&stuff->length, n);
    return (*AuProcVector[stuff->reqType]) (client);
}

int
SProcAuResourceReq(client)
ClientPtr       client;
{
    char            n;

    REQUEST(auResourceReq);
    swaps(&stuff->length, n);
    REQUEST_AT_LEAST_SIZE(auResourceReq);	/* not EXACT */
    swapl(&stuff->id, n);
    return (*AuProcVector[stuff->reqType]) (client);
}

int
SProcAuSetDeviceAttributes(client)
ClientPtr       client;
{
    char            n;

    REQUEST(auResourceReq);
    swaps(&stuff->length, n);
    REQUEST_AT_LEAST_SIZE(auResourceReq);	/* not EXACT */
    swapl(&stuff->id, n);
    SwapDeviceAttributes(&stuff[1]);
    return (*AuProcVector[stuff->reqType]) (client);
}

int
SProcAuCreateBucket(client)
ClientPtr       client;
{
    char            n;

    REQUEST(auResourceReq);
    swaps(&stuff->length, n);
    REQUEST_AT_LEAST_SIZE(auResourceReq);	/* not EXACT */
    swapl(&stuff->id, n);
    SwapBucketAttributes(&stuff[1]);
    return (*AuProcVector[stuff->reqType]) (client);
}

int
SProcAuListBuckets(client)
ClientPtr       client;
{
    char            n;

    REQUEST(auReq);
    swaps(&stuff->length, n);
    REQUEST_AT_LEAST_SIZE(auReq);	/* not EXACT */
    SwapBucketAttributes(&stuff[1]);
    return (*AuProcVector[stuff->reqType]) (client);
}

int
SProcAuListDevices(client)
ClientPtr       client;
{
    char            n;

    REQUEST(auReq);
    swaps(&stuff->length, n);
    REQUEST_AT_LEAST_SIZE(auReq);	/* not EXACT */
    SwapDeviceAttributes(&stuff[1]);
    return (*AuProcVector[stuff->reqType]) (client);
}

static auElement *
SwapElement(e, incoming)
auElement      *e;
AuBool          incoming;
{
    char            n;
    int             na = 0;
    AuUint8          *varData = (AuUint8 *) &e[1];

    if (incoming) {
	swaps(&e->type, n);
    }

    switch (e->type)
    {
	case AuElementTypeImportClient:
	    swaps(&e->importclient.sample_rate, n);
	    swapl(&e->importclient.max_samples, n);
	    swapl(&e->importclient.low_water_mark, n);
	    if (!incoming)
		na = e->importclient.actions.num_actions;
	    swapl(&e->importclient.actions.num_actions, n);
	    if (incoming)
		na = e->importclient.actions.num_actions;
	    break;
	case AuElementTypeImportDevice:
	    swaps(&e->importdevice.sample_rate, n);
	    swapl(&e->importdevice.num_samples, n);
	    swapl(&e->importdevice.device, n);
	    if (!incoming)
		na = e->importdevice.actions.num_actions;
	    swapl(&e->importdevice.actions.num_actions, n);
	    if (incoming)
		na = e->importdevice.actions.num_actions;
	    break;
	case AuElementTypeImportBucket:
	    swaps(&e->importbucket.sample_rate, n);
	    swapl(&e->importbucket.num_samples, n);
	    swapl(&e->importbucket.bucket, n);
	    swapl(&e->importbucket.offset, n);
	    if (!incoming)
		na = e->importbucket.actions.num_actions;
	    swapl(&e->importbucket.actions.num_actions, n);
	    if (incoming)
		na = e->importbucket.actions.num_actions;
	    break;
	case AuElementTypeImportWaveForm:
	    swaps(&e->importwaveform.sample_rate, n);
	    swapl(&e->importwaveform.num_samples, n);
	    swapl(&e->importwaveform.frequency, n);
	    if (!incoming)
		na = e->importwaveform.actions.num_actions;
	    swapl(&e->importwaveform.actions.num_actions, n);
	    if (incoming)
		na = e->importwaveform.actions.num_actions;
	    break;
	case AuElementTypeBundle:
	    if (incoming) {
		swaps(&e->bundle.num_inputs, n);
	    }
	    /* nothing to swap in the track list */
	    varData += e->bundle.num_inputs * sizeof(auInputTrack);
	    if (!incoming) {
		swaps(&e->bundle.num_inputs, n);
	    }
	    break;
	case AuElementTypeMultiplyConstant:
	    swaps(&e->multiplyconstant.input, n);
	    swapl(&e->multiplyconstant.constant, n);
	    break;
	case AuElementTypeAddConstant:
	    swaps(&e->addconstant.input, n);
	    swapl(&e->addconstant.constant, n);
	    break;
	case AuElementTypeSum:
	    if (incoming) {
		swaps(&e->sum.num_inputs, n);
	    }
	    SwapShorts(&e[1], e->sum.num_inputs);
	    varData += PAD4(e->sum.num_inputs * sizeof(CARD16));
	    if (!incoming) {
		swaps(&e->sum.num_inputs, n);
	    }
	    break;
	case AuElementTypeExportClient:
	    swaps(&e->exportclient.sample_rate, n);
	    swaps(&e->exportclient.input, n);
	    swapl(&e->exportclient.max_samples, n);
	    swapl(&e->exportclient.high_water_mark, n);
	    if (!incoming)
		na = e->exportclient.actions.num_actions;
	    swapl(&e->exportclient.actions.num_actions, n);
	    if (incoming)
		na = e->exportclient.actions.num_actions;
	    break;
	case AuElementTypeExportDevice:
	    swaps(&e->exportdevice.sample_rate, n);
	    swaps(&e->exportdevice.input, n);
	    swapl(&e->exportdevice.num_samples, n);
	    swapl(&e->exportdevice.device, n);
	    if (!incoming)
		na = e->exportdevice.actions.num_actions;
	    swapl(&e->exportdevice.actions.num_actions, n);
	    if (incoming)
		na = e->exportdevice.actions.num_actions;
	    break;
	case AuElementTypeExportBucket:
	    swaps(&e->exportbucket.input, n);
	    swapl(&e->exportbucket.num_samples, n);
	    swapl(&e->exportbucket.bucket, n);
	    swapl(&e->exportbucket.offset, n);
	    if (!incoming)
		na = e->exportbucket.actions.num_actions;
	    swapl(&e->exportbucket.actions.num_actions, n);
	    if (incoming)
		na = e->exportbucket.actions.num_actions;
	    break;
	case AuElementTypeExportMonitor:
	    swaps(&e->exportmonitor.event_rate, n);
	    swaps(&e->exportmonitor.input, n);
	    break;
	default:
	    return AuFalse;
    }

    if (na)
    {
	int             i;
	auElementAction *a = (auElementAction *) varData;

	for (i = 0; i < na; i++, a++) {
	    swapl(&a->flow, n);	/* swap action */
	}
	varData += na * sizeof(auElementAction);
    }

    if (!incoming) {
	swaps(&e->type, n);
    }

    return (auElement *) varData;
}

int
SProcAuSetElements(client)
ClientPtr       client;
{
    char            n;
    int             i;
    auElement      *e;

    REQUEST(auSetElementsReq);
    swaps(&stuff->length, n);
    swapl(&stuff->flow, n);
    swapl(&stuff->numElements, n);
    REQUEST_AT_LEAST_SIZE(auSetElementsReq);	/* not EXACT */

    e = (auElement *) & stuff[1];

    for (i = 0; i < stuff->numElements; i++)
	if (!(e = SwapElement(e, AuTrue)))
	    AU_ERROR(AuBadElement, e->type);

    return (*AuProcVector[stuff->reqType]) (client);
}

/* NOTE: this is also used for GetElementStates */
int
SProcAuSetElementStates(client)
ClientPtr       client;
{
    char            n;
    int             i;
    auElementState *states;

    REQUEST(auSetElementStatesReq);
    swaps(&stuff->length, n);
    swapl(&stuff->numStates, n);
    REQUEST_AT_LEAST_SIZE(auSetElementStatesReq);	/* not EXACT */
    states = (auElementState *) & stuff[1];

    for (i = 0; i < stuff->numStates; i++, states++)
    {
	swapl(&states->flow, n);
    }

    return (*AuProcVector[stuff->reqType]) (client);
}

int
SProcAuSetElementParameters(client)
ClientPtr       client;
{
    char            n;
    int             i;
    auElementParameters *parms;
    CARD32         *p;
    AuUint8	   *tmp;

    REQUEST(auSetElementParametersReq);
    swaps(&stuff->length, n);
    swapl(&stuff->numParameters, n);
    REQUEST_AT_LEAST_SIZE(auSetElementParametersReq);	/* not EXACT */
    parms = (auElementParameters *) & stuff[1];
    for (i = 0; i < stuff->numParameters; i++)
    {
	swapl(&parms->flow, n);
	p = (CARD32 *) & parms[1];
	SwapLongs(p, parms->num_parameters);
	tmp = (AuUint8 *) parms;
	tmp += parms->num_parameters * sizeof(CARD32);
	parms = (auElementParameters *) tmp;
    }

    return (*AuProcVector[stuff->reqType]) (client);
}

int
SProcAuWriteElement(client)
ClientPtr       client;
{
    char            n;

    REQUEST(auWriteElementReq);
    swaps(&stuff->length, n);
    swapl(&stuff->flow, n);
    swapl(&stuff->num_bytes, n);
    REQUEST_AT_LEAST_SIZE(auWriteElementReq);	/* not EXACT */
    return (*AuProcVector[stuff->reqType]) (client);
}

int
SProcAuReadElement(client)
ClientPtr       client;
{
    char            n;

    REQUEST(auReadElementReq);
    swaps(&stuff->length, n);
    swapl(&stuff->flow, n);
    swapl(&stuff->num_bytes, n);
    REQUEST_AT_LEAST_SIZE(auReadElementReq);	/* not EXACT */
    return (*AuProcVector[stuff->reqType]) (client);
}

/* replies */

void
SAuGetDeviceAttributesReply(client, size, pRep)
ClientPtr       client;
int             size;
auGetDeviceAttributesReply *pRep;
{
    char            n;

    swaps(&pRep->sequenceNumber, n);
    swapl(&pRep->length, n);

    (void) WriteToClient(client, size, (char *) pRep);
}

void
SAuListBucketsReply(client, size, pRep)
ClientPtr       client;
int             size;
auListBucketsReply *pRep;
{
    char            n;

    swaps(&pRep->sequenceNumber, n);
    swapl(&pRep->length, n);
    swapl(&pRep->num_buckets, n);

    (void) WriteToClient(client, size, (char *) pRep);
}

void
SAuListDevicesReply(client, size, pRep)
ClientPtr       client;
int             size;
auListDevicesReply *pRep;
{
    char            n;

    swaps(&pRep->sequenceNumber, n);
    swapl(&pRep->length, n);
    swapl(&pRep->num_devices, n);

    (void) WriteToClient(client, size, (char *) pRep);
}

void
SAuGetBucketAttributesReply(client, size, pRep)
ClientPtr       client;
int             size;
auGetBucketAttributesReply *pRep;
{
    char            n;

    swaps(&pRep->sequenceNumber, n);
    swapl(&pRep->length, n);

    (void) WriteToClient(client, size, (char *) pRep);
}

void
SAuReadElementReply(client, size, pRep)
ClientPtr       client;
int             size;
auReadElementReply *pRep;
{
    char            n;

    swaps(&pRep->sequenceNumber, n);
    swapl(&pRep->length, n);
    swapl(&pRep->num_bytes, n);

    (void) WriteToClient(client, size, (char *) pRep);
}

void
SAuGetElementsReply(client, size, pRep)
ClientPtr       client;
int             size;
auGetElementsReply *pRep;
{
    char            n;

    swaps(&pRep->sequenceNumber, n);
    swapl(&pRep->length, n);
    swapl(&pRep->flow, n);
    swapl(&pRep->num_elements, n);

    (void) WriteToClient(client, size, (char *) pRep);
}

void
SAuGetElementStatesReply(client, size, pRep)
ClientPtr       client;
int             size;
auGetElementStatesReply *pRep;
{
    char            n;

    swaps(&pRep->sequenceNumber, n);
    swapl(&pRep->length, n);
    swapl(&pRep->numStates, n);

    (void) WriteToClient(client, size, (char *) pRep);
}

void
SAuGetCloseDownModeReply(client, size, pRep)
ClientPtr       client;
int             size;
auGetCloseDownModeReply *pRep;
{
    char            n;

    swaps(&pRep->sequenceNumber, n);
    swapl(&pRep->length, n);

    (void) WriteToClient(client, size, (char *) pRep);
}

void
SAuGetServerTimeReply(client, size, pRep)
ClientPtr       client;
int             size;
auGetServerTimeReply *pRep;
{
    char            n;

    swaps(&pRep->sequenceNumber, n);
    swapl(&pRep->length, n);
    swapl(&pRep->time, n);

    (void) WriteToClient(client, size, (char *) pRep);
}

static void
SwapCommonAttributes(p)
auCommonPart   *p;
{
    char            n;

    swapl(&p->value_mask, n);
    swapl(&p->changable_mask, n);
    swapl(&p->id, n);
    swapl(&p->access, n);
    swapl(&p->description.len, n);
}

static void
SwapDeviceAttributes(p)
auDeviceAttributes *p;
{
    char            n;

    SwapCommonAttributes(&p->common);

    swapl(&p->device.location, n);
    swapl(&p->device.gain, n);
    swaps(&p->device.min_sample_rate, n);
    swaps(&p->device.max_sample_rate, n);
}

void
AuSwapDeviceAttributesWrite(client, size, p)
ClientPtr       client;
int             size;
auDeviceAttributes *p;
{
    SwapDeviceAttributes(p);
    (void) WriteToClient(client, size, (char *) p);
}

static void
SwapBucketAttributes(p)
auBucketAttributes *p;
{
    char            n;

    SwapCommonAttributes(&p->common);

    swaps(&p->bucket.sample_rate, n);
    swapl(&p->bucket.num_samples, n);
}

void
AuSwapBucketAttributesWrite(client, size, p)
ClientPtr       client;
int             size;
auBucketAttributes *p;
{
    SwapBucketAttributes(p);
    (void) WriteToClient(client, size, (char *) p);
}

void
AuSwapSetupPrefix(client, size, p)
ClientPtr       client;
int             size;
auConnSetupPrefix *p;
{
    auConnSetupPrefix sp;

    sp.success = p->success;
    sp.lengthReason = p->lengthReason;
    cpswaps(p->majorVersion, sp.majorVersion);
    cpswaps(p->minorVersion, sp.minorVersion);
    cpswaps(p->length, sp.length);

    (void) WriteToClient(client, size, (char *) &sp);
}

void
AuSwapSetup(client, size, p)
ClientPtr       client;
int             size;
auConnSetup    *p;
{
    auConnSetup     sp;

    sp = *p;
    cpswapl(p->release, sp.release);
    cpswapl(p->ridBase, sp.ridBase);
    cpswapl(p->ridMask, sp.ridMask);
    cpswaps(p->minSampleRate, sp.minSampleRate);
    cpswaps(p->maxSampleRate, sp.maxSampleRate);
    cpswaps(p->nbytesVendor, sp.nbytesVendor);
    cpswaps(p->maxRequestSize, sp.maxRequestSize);

    (void) WriteToClient(client, size, (char *) &sp);
}

void
AuCopySwapElementWrite(client, size, p)
ClientPtr       client;
int             size;
auElement      *p;
{
    auElement      *e;

    if (!(e = (auElement *) aualloc(size)))
	return;			/* XXX - what should we really do? */

    bcopy(p, e, size);
    SwapElement(e, AuFalse);
    (void) WriteToClient(client, size, (char *) e);

    aufree(e);
}

void
AuSwapElementStateWrite(client, size, p)
ClientPtr       client;
int             size;
auElementState *p;
{
    char            n;

    swapl(&p->flow, n);
    (void) WriteToClient(client, size, (char *) p);
}

/* events */

void
SAuElementNotifyEvent(from, to)
auEvent        *from,
               *to;
{
    to->u.u.type = from->u.u.type;
    to->u.u.detail = from->u.u.detail;
    cpswaps(from->u.u.sequenceNumber, to->u.u.sequenceNumber);
    cpswapl(from->u.u.time, to->u.u.time);

    cpswapl(from->u.elementNotify.flow, to->u.elementNotify.flow);
    cpswaps(from->u.elementNotify.element_num, to->u.elementNotify.element_num);
    cpswaps(from->u.elementNotify.kind, to->u.elementNotify.kind);
    cpswaps(from->u.elementNotify.prev_state, to->u.elementNotify.prev_state);
    cpswaps(from->u.elementNotify.cur_state, to->u.elementNotify.cur_state);
    cpswaps(from->u.elementNotify.reason, to->u.elementNotify.reason);
    cpswapl(from->u.elementNotify.num_bytes, to->u.elementNotify.num_bytes);
}

void
SAuMonitorNotifyEvent(from, to)
auEvent        *from,
               *to;
{
    to->u.u.type = from->u.u.type;
    to->u.u.detail = from->u.u.detail;
    cpswaps(from->u.u.sequenceNumber, to->u.u.sequenceNumber);
    cpswapl(from->u.u.time, to->u.u.time);

    cpswapl(from->u.monitorNotify.flow, to->u.monitorNotify.flow);
    cpswaps(from->u.monitorNotify.element_num, to->u.monitorNotify.element_num);
    to->u.monitorNotify.format = from->u.monitorNotify.format;
    to->u.monitorNotify.num_tracks = from->u.monitorNotify.num_tracks;
    cpswaps(from->u.monitorNotify.count, to->u.monitorNotify.count);
    cpswaps(from->u.monitorNotify.num_fields, to->u.monitorNotify.num_fields);
    to->u.monitorNotify.data = from->u.monitorNotify.data;
    to->u.monitorNotify.data1 = from->u.monitorNotify.data1;
    to->u.monitorNotify.data2 = from->u.monitorNotify.data2;
}

void
SAuErrorEvent(from, to)
auError        *from,
               *to;
{
    to->type = Au_Error;
    to->errorCode = from->errorCode;
    cpswaps(from->sequenceNumber, to->sequenceNumber);
    cpswapl(from->time, to->time);
    cpswapl(from->resourceID, to->resourceID);
    cpswaps(from->minorCode, to->minorCode);
    to->majorCode = from->majorCode;
}
