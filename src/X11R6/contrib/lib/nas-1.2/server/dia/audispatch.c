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
 * $NCDId: @(#)audispatch.c,v 1.11 1994/04/27 17:45:06 greg Exp $
 */

#include	"release.h"
#include	"misc.h"
#include	"dixstruct.h"
#include	"servermd.h"			/* VENDOR release & strings */
#include	<audio/audio.h>
#include	<audio/Aproto.h>
#include	"au.h"

extern char     isItTimeToYield;
extern int      (*InitialVector[3]) ();
extern int      (*AuReplySwapVector[256]) ();
extern void     Swap32Write(), AuSwapDeviceAttributesWrite(),
                AuSwapBucketAttributesWrite(), AuSwapSetupPrefix(),
                AuSwapSetup(), AuCopySwapElementWrite(),
                AuSwapElementStateWrite(), AuFreeComponent(),
                AuFreeFlowElements(), AuProcessClockedFlows(),
                AuProcessStateChanges(), AuProcessUnclockedFlows(),
                AuSetInputGainAndLineMode(), AuSetOutputGain(),
                CloseDownRetainedResources(), WriteEventsToClient(),
                WriteToClient();
extern AuBool   AuChangeElementState(), AuInitDevice(), AuMatchAttributes();
extern int      AuCompileFlow(), AuSetComponentAttributes();
extern AuFixedPoint AuGetOutputGain();

extern RESTYPE  auFlowType,
                auComponentType;
extern FlowPtr  auClockedFlows,
                auUnclockedFlows;
extern ComponentPtr auBuckets,
                auDevices;

static auConnSetupPrefix auSetupPrefix;
auConnSetup auSetup;

static char    *auVendorString;

#define WriteAuSwappedDataToClient(client, size, pbuf, swapper)		       \
{									       \
    (client)->pSwapReplyFunc = (swapper);				       \
    WriteSwappedDataToClient(client, size, pbuf);			       \
}

#define xferCommonAttributes(c, a)					       \
{									       \
    (a).common.value_mask = (c)->valueMask;				       \
    (a).common.changable_mask = (c)->changableMask;			       \
    (a).common.id = (c)->id;						       \
    (a).common.kind = (c)->kind;					       \
    (a).common.use = (c)->use;						       \
    (a).common.format = (c)->format;					       \
    (a).common.num_tracks = (c)->numTracks;				       \
    (a).common.access = (c)->access;					       \
    (a).common.description.type = (c)->description.type;		       \
    (a).common.description.len = (c)->description.len;			       \
}


#define xferDeviceAttributes(d, da)					       \
{									       \
    xferCommonAttributes(d, da);					       \
    (da).device.min_sample_rate = (d)->minSampleRate;			       \
    (da).device.max_sample_rate = (d)->maxSampleRate;			       \
    (da).device.location = (d)->location;				       \
    (da).device.gain = (d)->gain;					       \
    (da).device.line_mode = (d)->lineMode;				       \
    (da).device.num_children = (d)->numChildren;			       \
}

#define xferBucketAttributes(b, ba)					       \
{									       \
    xferCommonAttributes(b, ba);					       \
    (ba).bucket.sample_rate = (b)->sampleRate;				       \
    (ba).bucket.num_samples = (b)->numSamples;				       \
}

/* REQUESTS */

int
ProcAuNotImplemented(client)
ClientPtr       client;
{
    AU_ERROR(AuBadImplementation, 0);
}

int
ProcAuListDevices(client)
ClientPtr       client;
{
    REQUEST(auReq);
    auListDevicesReply rep;
    auDeviceAttributes a,
                   *match;
    ComponentPtr    c;
    int             ndevices,
                    len;

    REQUEST_AT_LEAST_SIZE(auReq);
    len = (stuff->length << 2) - sizeof(auReq) - sizeof(auDeviceAttributes);

    if (len < 0)
	AU_ERROR(AuBadLength, 0);

    match = (auDeviceAttributes *) & stuff[1];

    if (match->common.value_mask & AuCompCommonDescriptionMask)
	if (len != PAD4(match->common.description.len))
	    AU_ERROR(AuBadLength, 0);

    c = auDevices;
    ndevices = len = 0;

    /*
     * first get the number of devices and the length of the variable length
     * data
     */
    while (c)
    {
	c->list = AuMatchAttributes(match, c, &match[1]);

	if (c->list)
	{
	    ndevices++;
	    len += PAD4(c->description.len);

	    if (c->numChildren)
		len += c->numChildren * sizeof(AuDeviceID);
	}

	c = c->next;
    }

    /* write out the reply */
    rep.type = Au_Reply;
    rep.sequenceNumber = client->sequence;
    rep.length = (sizeof(auDeviceAttributes) * ndevices + len) >> 2;
    rep.num_devices = ndevices;
    WriteAuReplyToClient(client, sizeof(rep), &rep);

    c = auDevices;

    while (c)
    {
	if (c->list)
	{
	    if (c->kind == AuComponentKindPhysicalOutput)
		c->gain = AuGetOutputGain();

	    xferDeviceAttributes(c, a);

	    WriteAuSwappedDataToClient(client, sizeof(a), &a,
				       AuSwapDeviceAttributesWrite);
	    (void) WriteToClient(client, c->description.len,
				 c->description.string);

	    if (c->numChildren)
	    {
		bcopy(c->children, c->childSwap,
		      c->numChildren * sizeof(AuDeviceID));
		WriteAuSwappedDataToClient(client,
					c->numChildren * sizeof(AuDeviceID),
					   c->childSwap, Swap32Write);
	    }

	}

	c = c->next;
    }

    return client->noClientException;
}

int
ProcAuGetDeviceAttributes(client)
ClientPtr       client;
{
    REQUEST(auResourceReq);
    auGetDeviceAttributesReply rep;
    auDeviceAttributes a;
    ComponentPtr    c;

    REQUEST_SIZE_MATCH(auResourceReq);

    if (!(c = (ComponentPtr) LookupIDByType(stuff->id, auComponentType)))
	AU_ERROR(AuBadDevice, stuff->id);

    /* write out the reply */
    rep.type = Au_Reply;
    rep.sequenceNumber = client->sequence;
    rep.length = (sizeof(auDeviceAttributes) + PAD4(c->description.len) +
		  PAD4(c->numChildren * sizeof(AuDeviceID))) >> 2;
    WriteAuReplyToClient(client, sizeof(rep), &rep);

    if (c->kind == AuComponentKindPhysicalOutput)
	c->gain = AuGetOutputGain();

    xferDeviceAttributes(c, a);

    WriteAuSwappedDataToClient(client, sizeof(a), &a,
			       AuSwapDeviceAttributesWrite);
    (void) WriteToClient(client, c->description.len, c->description.string);

    if (c->numChildren)
    {
	bcopy(c->children, c->childSwap, c->numChildren * sizeof(AuDeviceID));
	WriteAuSwappedDataToClient(client, c->numChildren * sizeof(AuDeviceID),
				   c->childSwap, Swap32Write);
    }

    return client->noClientException;
}

int
ProcAuSetDeviceAttributes(client)
ClientPtr       client;
{
    REQUEST(auResourceReq);
    ComponentPtr    c;
    auDeviceAttributes *a;
    int             len,
                    status;

    REQUEST_AT_LEAST_SIZE(auResourceReq);

    if (!(c = (ComponentPtr) LookupIDByType(stuff->id, auComponentType)))
	AU_ERROR(AuBadDevice, stuff->id);

    len = (stuff->length << 2) - sizeof(auResourceReq) -
	sizeof(auDeviceAttributes);

    if (len < 0)
	AU_ERROR(AuBadLength, 0);

    a = (auDeviceAttributes *) & stuff[1];

    /* see if any illegal changes are being attempted */
    if (a->common.value_mask & ~c->changableMask)
	AU_ERROR(AuBadValue, a->common.value_mask);

    if (a->common.value_mask & AuCompCommonDescriptionMask)
	if (len != PAD4(a->common.description.len))
	    AU_ERROR(AuBadLength, 0);

    if ((status = AuSetComponentAttributes(a, c, NULL)) != AuSuccess)
	return status;

    if (c->kind == AuComponentKindPhysicalInput)
	AuSetInputGainAndLineMode(c->gain, c->lineMode);
    else if (c->kind == AuComponentKindPhysicalOutput)
	AuSetOutputGain(c->gain);

    return AuSuccess;
}

int
ProcAuCreateBucket(client)
ClientPtr       client;
{
    REQUEST(auResourceReq);
    ComponentPtr    c;
    auBucketAttributes *a;
    int             len,
                    status;
    AuUint32           dataSize;
    extern int      auMinibufSamples;

    REQUEST_AT_LEAST_SIZE(auResourceReq);
    len = (stuff->length << 2) - sizeof(auResourceReq) -
	sizeof(auBucketAttributes);

    if (len < 0)
	AU_ERROR(AuBadLength, 0);

    a = (auBucketAttributes *) & stuff[1];

    /* see if we've got all the values we want */
    if (a->common.value_mask !=
     (AuCompBucketAllMasks & ~(AuCompCommonKindMask | AuCompCommonUseMask)))
	AU_ERROR(AuBadValue, a->common.value_mask);

    if (a->common.value_mask & AuCompCommonDescriptionMask)
	if (len != PAD4(a->common.description.len))
	    AU_ERROR(AuBadLength, 0);

    dataSize = a->bucket.num_samples * sizeofFormat(a->common.format) *
	a->common.num_tracks;

    if (!(c = (ComponentPtr) aucalloc(1, PAD4(sizeof(ComponentRec)) +
				      PAD4(dataSize) +
				      PAD4(a->common.description.len))))
	AU_ERROR(AuBadAlloc, 0);

    c->kind = AuComponentKindBucket;
    c->use = AuComponentUseAllMasks;
    c->valueMask = AuCompBucketAllMasks;
    c->data = (AuUint8 *) c + PAD4(sizeof(ComponentRec));
    c->dataSize = dataSize;
    c->dataEnd = c->data + c->dataSize;
    c->read = c->write = c->data;
    c->physicalDeviceMask = NotAPhysicalDevice;
    c->description.string = (char *) c->data + PAD4(dataSize);
    c->minibufSize = auNativeBytesPerSample * auMinibufSamples *
	a->common.num_tracks;
    c->destroyed = AuFalse;
    c->bytesPerSample = a->common.num_tracks * sizeofFormat(a->common.format);

    AddToLinkedList(auBuckets, c);

    if ((status = AuSetComponentAttributes(a, c, &a[1])) != AuSuccess)
    {
	AuFreeComponent(c);
	return status;
    }

    if (!AddResource(stuff->id, auComponentType, c))
	AU_ERROR(AuBadAlloc, 0);

    return AuSuccess;
}

int
ProcAuDestroyBucket(client)
ClientPtr       client;
{
    REQUEST(auResourceReq);
    ComponentPtr    c;

    REQUEST_SIZE_MATCH(auResourceReq);

    if (!(c = (ComponentPtr) LookupIDByType(stuff->id, auComponentType)))
	AU_ERROR(AuBadBucket, stuff->id);

    if (!(c->access & AuAccessDestroyMask) &&
	CLIENT_ID(stuff->id) != client->index)
	AU_ERROR(AuBadAccess, stuff->id);

    c->destroyed = AuTrue;
    FreeResource(stuff->id, AuFalse);

    return AuSuccess;
}

int
ProcAuListBuckets(client)
ClientPtr       client;
{
    REQUEST(auReq);
    auListBucketsReply rep;
    auBucketAttributes a,
                   *match;
    ComponentPtr    c;
    int             nbuckets,
                    len;

    REQUEST_AT_LEAST_SIZE(auReq);
    len = (stuff->length << 2) - sizeof(auReq) - sizeof(auBucketAttributes);

    if (len < 0)
	AU_ERROR(AuBadLength, 0);

    match = (auBucketAttributes *) & stuff[1];

    if (match->common.value_mask & AuCompCommonDescriptionMask)
	if (len != PAD4(match->common.description.len))
	    AU_ERROR(AuBadLength, 0);

    c = auBuckets;
    nbuckets = len = 0;

    /*
     * first get the number of buckets and the length of the variable length
     * data
     */
    while (c)
    {
	c->list = !c->destroyed &&
	    (CLIENT_ID(c->id) == client->index ||
	     c->access & AuAccessListMask) &&
	    AuMatchAttributes(match, c, &match[1]);

	if (c->list)
	{
	    nbuckets++;
	    len += PAD4(c->description.len);
	}

	c = c->next;
    }

    /* write out the reply */
    rep.type = Au_Reply;
    rep.sequenceNumber = client->sequence;
    rep.length = (sizeof(auBucketAttributes) * nbuckets + len) >> 2;
    rep.num_buckets = nbuckets;
    WriteAuReplyToClient(client, sizeof(rep), &rep);

    c = auBuckets;

    while (c)
    {
	if (c->list)
	{
	    xferBucketAttributes(c, a);

	    WriteAuSwappedDataToClient(client, sizeof(a), &a,
				       AuSwapBucketAttributesWrite);
	    (void) WriteToClient(client, c->description.len,
				 c->description.string);
	}

	c = c->next;
    }

    return client->noClientException;
}

int
ProcAuGetBucketAttributes(client)
ClientPtr       client;
{
    REQUEST(auResourceReq);
    auGetBucketAttributesReply rep;
    auBucketAttributes a;
    ComponentPtr    c;

    REQUEST_SIZE_MATCH(auResourceReq);

    if (!(c = (ComponentPtr) LookupIDByType(stuff->id, auComponentType)))
	AU_ERROR(AuBadBucket, stuff->id);

    /* write out the reply */
    rep.type = Au_Reply;
    rep.sequenceNumber = client->sequence;
    rep.length = (sizeof(auBucketAttributes) + PAD4(c->description.len)) >> 2;
    WriteAuReplyToClient(client, sizeof(rep), &rep);

    xferBucketAttributes(c, a);

    WriteAuSwappedDataToClient(client, sizeof(a), &a, AuSwapBucketAttributesWrite);
    (void) WriteToClient(client, c->description.len, c->description.string);

    return client->noClientException;
}

int
ProcAuCreateFlow(client)
ClientPtr       client;
{
    REQUEST(auResourceReq);
    FlowPtr         flow;

    REQUEST_SIZE_MATCH(auResourceReq);

    if (!(flow = (FlowPtr) aucalloc(1, sizeof(FlowRec))))
	AU_ERROR(AuBadAlloc, 0);

    flow->flowId = stuff->id;
    flow->state = flow->pendingState = AuStateStop;
    flow->compiled.freeComponents = AuTrue;

    if (!AddResource(stuff->id, auFlowType, flow))
	AU_ERROR(AuBadAlloc, 0);

    return AuSuccess;
}

int
ProcAuDestroyFlow(client)
ClientPtr       client;
{
    REQUEST(auResourceReq);
    FlowPtr         flow;

    REQUEST_SIZE_MATCH(auResourceReq);

    if (!(flow = (FlowPtr) LookupIDByType(stuff->id, auFlowType)))
	AU_ERROR(AuBadFlow, stuff->id);

    FreeResource(stuff->id, AuFalse);

    return AuSuccess;
}

int
ProcAuGetElements(client)
ClientPtr       client;
{
    REQUEST(auResourceReq);
    auGetElementsReply rep;
    FlowPtr         flow;
    int             i;

    REQUEST_SIZE_MATCH(auResourceReq);

    if (!(flow = (FlowPtr) LookupIDByType(stuff->id, auFlowType)))
	AU_ERROR(AuBadFlow, stuff->id);

    /* write out the reply */
    rep.type = Au_Reply;
    rep.sequenceNumber = client->sequence;
    rep.length = (sizeof(auElement) * flow->numElements + flow->varLen) >> 2;
    rep.flow = flow->flowId;
    rep.clocked = flow->clocked;
    rep.num_elements = flow->numElements;
    WriteAuReplyToClient(client, sizeof(rep), &rep);

    for (i = 0; i < flow->numElements; i++)
	WriteAuSwappedDataToClient(client,
			       flow->elements[i].varLen + sizeof(auElement),
				   flow->elements[i].raw,
				   AuCopySwapElementWrite);

    return AuSuccess;
}

static int      numDefaultActions[] =
{
    5,						/* AuElementTypeImportClient */
    0,						/* AuElementTypeImportDevice */
    3,						/* AuElementTypeImportBucket */
    3,						/* AuElementTypeImportWaveForm
						 * */
    0,						/* AuElementTypeImportRadio */
    0,						/* AuElementTypeBundle */
    0,						/* AuElementTypeMultiplyConsta
						 * nt */
    0,						/* AuElementTypeAddConstant */
    0,						/* AuElementTypeSum */
    5,						/* AuElementTypeExportClient */
    0,						/* AuElementTypeExportDevice */
    3,						/* AuElementTypeExportBucket */
    0,						/* AuElementTypeExportRadio */
    0,						/* AuElementTypeExportMonitor */
};

#define ADD_VAR(n)							      \
{									      \
    AuUint8 *_t = (AuUint8 *) el;					      \
									      \
    varLen += (n);							      \
    _t += (n);								      \
    el = (auElement *) _t;						      \
}

#define COMP_ACTIONS(num)						       \
{									       \
    numActions += (num) ? (num) : numDefaultActions[el->type];		       \
    ADD_VAR((num) * sizeof(auElementAction));				       \
}

#define FREE_FLOW_ERROR(e, v)						       \
{									       \
    AuFreeFlowElements(flow);						       \
    AU_ERROR(e, v);							       \
}

int
ProcAuSetElements(client)
ClientPtr       client;
{
    REQUEST(auSetElementsReq);
    FlowPtr         flow;
    int             len,
                    i,
                    varLen,
                    numActions,
                    status;
    auElement      *el;
    FlowElementPtr  e;
    AuUint8          *p;
    ActionPtr       a;

    REQUEST_AT_LEAST_SIZE(auSetElementsReq);

    if (!(flow = (FlowPtr) LookupIDByType(stuff->flow, auFlowType)))
	AU_ERROR(AuBadFlow, stuff->flow);

    el = (auElement *) & stuff[1];

    /* compute length of variable data and do some error checking */
    for (i = varLen = numActions = 0; i < stuff->numElements; i++, el++)
	switch (el->type)
	{
	    case AuElementTypeImportClient:
		COMP_ACTIONS(el->importclient.actions.num_actions);
		break;
	    case AuElementTypeImportDevice:
		COMP_ACTIONS(el->importdevice.actions.num_actions);
		break;
	    case AuElementTypeImportBucket:
		COMP_ACTIONS(el->importbucket.actions.num_actions);
		break;
	    case AuElementTypeImportWaveForm:
		COMP_ACTIONS(el->importwaveform.actions.num_actions);
		break;
	    case AuElementTypeBundle:
		ADD_VAR(el->bundle.num_inputs * sizeof(auInputTrack));
		break;
	    case AuElementTypeMultiplyConstant:
		break;
	    case AuElementTypeAddConstant:
		break;
	    case AuElementTypeSum:
		ADD_VAR(PAD4(el->sum.num_inputs * sizeof(CARD16)));
		break;
	    case AuElementTypeExportClient:
		COMP_ACTIONS(el->exportclient.actions.num_actions);
		break;
	    case AuElementTypeExportDevice:
		COMP_ACTIONS(el->exportdevice.actions.num_actions);
		break;
	    case AuElementTypeExportBucket:
		COMP_ACTIONS(el->exportbucket.actions.num_actions);
		break;
	    case AuElementTypeExportMonitor:
		break;
	    default:
		AU_ERROR(AuBadElement, el->type);
	}

    /* size of element list */
    len = (stuff->length << 2) - sizeof(auSetElementsReq);

    /* do a length check */
    if (varLen != len - stuff->numElements * sizeof(auElement))
	AU_ERROR(AuBadLength, 0);

    /* get rid of the existing elements */
    if (flow->elements)
	AuFreeFlowElements(flow);

    /* alloc some space */
    if (!(p = (AuUint8 *)
	  aualloc(PAD4(stuff->numElements * sizeof(FlowElementRec)) +
		  PAD4(len) +
		  PAD4(numActions * sizeof(ActionRec)))))
	AU_ERROR(AuBadAlloc, 0);

    flow->elements = (FlowElementPtr) p;
    p += PAD4(stuff->numElements * sizeof(FlowElementRec));

    flow->raw = (auElement *) p;
    p += PAD4(len);

    a = (ActionPtr) p;

    /* copy the raw data */
    bcopy(&stuff[1], flow->raw, len);

    flow->clocked = stuff->clocked;
    flow->numElements = stuff->numElements;
    flow->state = AuStateStop;
    flow->varLen = varLen;

    el = flow->raw;
    e = flow->elements;

    for (i = 0; i < stuff->numElements; i++, e++, a += numActions)
    {
	int             copyActions;
	AuUint8 	*tmp;

	varLen = numActions = copyActions = 0;

	e->client = (AuPointer) client;
	e->raw = el;
	e->elementNum = i;
	e->compiled = AuFalse;
	e->setup = AuFalse;
	e->flow = flow;
	e->isComponent = AuTrue;
	e->stateChange = AuFalse;
	e->state = e->prevState = AuStateStop;
	e->refCnt = 0;
	e->parmsChanged = AuFalse;

	switch (el->type)
	{
	    case AuElementTypeImportClient:
		if (!(copyActions = el->importclient.actions.num_actions))
		{
		    numActions = numDefaultActions[el->type];

		    a[0].triggerState = AuStatePause;
		    a[0].triggerPrevState = AuStateAny;
		    a[0].triggerReason = AuReasonAny;
		    a[0].action = AuElementActionSendNotify;

		    a[1].triggerState = AuStateStop;
		    a[1].triggerPrevState = AuStateAny;
		    a[1].triggerReason = AuReasonAny;
		    a[1].action = AuElementActionSendNotify;

		    a[2].triggerState = AuStatePause;
		    a[2].triggerPrevState = AuStateAny;
		    a[2].triggerReason = AuReasonAny;
		    a[2].action = AuElementActionChangeState;
		    a[2].flow = flow;
		    a[2].elementNum = AuElementAll;
		    a[2].newState = AuStatePause;

		    a[3].triggerState = AuStateStart;
		    a[3].triggerPrevState = AuStatePause;
		    a[3].triggerReason = AuReasonAny;
		    a[3].action = AuElementActionChangeState;
		    a[3].flow = flow;
		    a[3].elementNum = AuElementAll;
		    a[3].newState = AuStateStart;

		    a[4].triggerState = AuStateStop;
		    a[4].triggerPrevState = AuStateAny;
		    a[4].triggerReason = AuReasonAny;
		    a[4].action = AuElementActionChangeState;
		    a[4].flow = flow;
		    a[4].elementNum = AuElementAll;
		    a[4].newState = AuStateStop;
		}
		break;
	    case AuElementTypeImportDevice:
		copyActions = el->importdevice.actions.num_actions;
		break;
	    case AuElementTypeImportBucket:
		if (!(copyActions = el->importbucket.actions.num_actions))
		{
		    numActions = numDefaultActions[el->type];

		    a[0].triggerState = AuStatePause;
		    a[0].triggerPrevState = AuStateAny;
		    a[0].triggerReason = AuReasonAny;
		    a[0].action = AuElementActionSendNotify;

		    a[1].triggerState = AuStateStop;
		    a[1].triggerPrevState = AuStateAny;
		    a[1].triggerReason = AuReasonAny;
		    a[1].action = AuElementActionSendNotify;

		    a[2].triggerState = AuStateStop;
		    a[2].triggerPrevState = AuStateAny;
		    a[2].triggerReason = AuReasonAny;
		    a[2].action = AuElementActionChangeState;
		    a[2].flow = flow;
		    a[2].elementNum = AuElementAll;
		    a[2].newState = AuStateStop;
		}
		break;
	    case AuElementTypeImportWaveForm:
		if (!(copyActions = el->importwaveform.actions.num_actions))
		{
		    numActions = numDefaultActions[el->type];

		    a[0].triggerState = AuStatePause;
		    a[0].triggerPrevState = AuStateAny;
		    a[0].triggerReason = AuReasonAny;
		    a[0].action = AuElementActionSendNotify;

		    a[1].triggerState = AuStateStop;
		    a[1].triggerPrevState = AuStateAny;
		    a[1].triggerReason = AuReasonAny;
		    a[1].action = AuElementActionSendNotify;

		    a[2].triggerState = AuStateStop;
		    a[2].triggerPrevState = AuStateAny;
		    a[2].triggerReason = AuReasonAny;
		    a[2].action = AuElementActionChangeState;
		    a[2].flow = flow;
		    a[2].elementNum = AuElementAll;
		    a[2].newState = AuStateStop;
		}
		break;
	    case AuElementTypeBundle:
		varLen = el->bundle.num_inputs * sizeof(auInputTrack);
		e->isComponent = AuFalse;
		break;
	    case AuElementTypeMultiplyConstant:
		e->isComponent = AuFalse;
		break;
	    case AuElementTypeAddConstant:
		e->isComponent = AuFalse;
		break;
	    case AuElementTypeSum:
		varLen = PAD4(el->sum.num_inputs * sizeof(CARD16));
		e->isComponent = AuFalse;
		break;
	    case AuElementTypeExportClient:
		if (!(copyActions = el->exportclient.actions.num_actions))
		{
		    numActions = numDefaultActions[el->type];

		    a[0].triggerState = AuStatePause;
		    a[0].triggerPrevState = AuStateAny;
		    a[0].triggerReason = AuReasonAny;
		    a[0].action = AuElementActionSendNotify;

		    a[1].triggerState = AuStateStop;
		    a[1].triggerPrevState = AuStateAny;
		    a[1].triggerReason = AuReasonAny;
		    a[1].action = AuElementActionSendNotify;

		    a[2].triggerState = AuStatePause;
		    a[2].triggerPrevState = AuStateAny;
		    a[2].triggerReason = AuReasonAny;
		    a[2].action = AuElementActionChangeState;
		    a[2].flow = flow;
		    a[2].elementNum = AuElementAll;
		    a[2].newState = AuStatePause;

		    a[3].triggerState = AuStateStart;
		    a[3].triggerPrevState = AuStatePause;
		    a[3].triggerReason = AuReasonAny;
		    a[3].action = AuElementActionChangeState;
		    a[3].flow = flow;
		    a[3].elementNum = AuElementAll;
		    a[3].newState = AuStateStart;

		    a[4].triggerState = AuStateStop;
		    a[4].triggerPrevState = AuStateAny;
		    a[4].triggerReason = AuReasonAny;
		    a[4].action = AuElementActionChangeState;
		    a[4].flow = flow;
		    a[4].elementNum = AuElementAll;
		    a[4].newState = AuStateStop;
		}
		break;
	    case AuElementTypeExportDevice:
		copyActions = el->exportdevice.actions.num_actions;
		break;
	    case AuElementTypeExportBucket:
		if (!(copyActions = el->exportbucket.actions.num_actions))
		{
		    numActions = numDefaultActions[el->type];

		    a[0].triggerState = AuStatePause;
		    a[0].triggerPrevState = AuStateAny;
		    a[0].triggerReason = AuReasonAny;
		    a[0].action = AuElementActionSendNotify;

		    a[1].triggerState = AuStateStop;
		    a[1].triggerPrevState = AuStateAny;
		    a[1].triggerReason = AuReasonAny;
		    a[1].action = AuElementActionSendNotify;

		    a[2].triggerState = AuStateStop;
		    a[2].triggerPrevState = AuStateAny;
		    a[2].triggerReason = AuReasonAny;
		    a[2].action = AuElementActionChangeState;
		    a[2].flow = flow;
		    a[2].elementNum = AuElementAll;
		    a[2].newState = AuStateStop;
		}
		break;
	    case AuElementTypeExportMonitor:
		break;
	}

	if (copyActions)
	{
	    auElementAction *raw = (auElementAction *) (el + 1);
	    ActionPtr       act = a;
	    FlowPtr         fl;

	    numActions = copyActions;
	    varLen = numActions * sizeof(auElementAction);

	    while (copyActions--)
	    {
		if (!AuValidTriggerState(raw->trigger_state))
		    FREE_FLOW_ERROR(AuBadValue, raw->trigger_state);

		if (!AuValidTriggerState(raw->trigger_prev_state))
		    FREE_FLOW_ERROR(AuBadValue, raw->trigger_prev_state);

		if (!AuValidTriggerReason(raw->trigger_reason))
		    FREE_FLOW_ERROR(AuBadValue, raw->trigger_reason);

		act->triggerState = raw->trigger_state;
		act->triggerPrevState = raw->trigger_prev_state;
		act->triggerReason = raw->trigger_reason;
		act->action = raw->action;

		if (raw->action == AuElementActionChangeState)
		{
		    if (!(fl = (FlowPtr) LookupIDByType(raw->flow, auFlowType)))
			FREE_FLOW_ERROR(AuBadFlow, raw->flow);

		    if (raw->element_num != AuElementAll &&
			raw->element_num >= flow->numElements)
			FREE_FLOW_ERROR(AuBadElement, raw->element_num);

		    if (!AuValidState(raw->new_state))
			FREE_FLOW_ERROR(AuBadValue, raw->new_state);

		    act->flow = fl;
		    act->elementNum = raw->element_num;
		    act->newState = raw->new_state;
		}
		else if (raw->action != AuElementActionSendNotify &&
			 raw->action != AuElementActionNoop)
		    FREE_FLOW_ERROR(AuBadValue, raw->action);

		raw++;
		act++;
	    }
	}

	e->numActions = numActions;
	e->actions = a;
	e->varLen = varLen;

	tmp = (AuUint8 *) el;
	tmp += sizeof(auElement) + varLen;
	el = (auElement *) tmp;
    }

    if (flow->clocked)
    {
	AddToLinkedList(auClockedFlows, flow);
    }
    else
    {
	AddToLinkedList(auUnclockedFlows, flow);
    }

    if ((status = AuCompileFlow(client, flow)) == AuSuccess)
    {
	flow->trivial = flow->numElements == 2 &&
	    (((flow->elements[0].raw->type == AuElementTypeImportClient &&
	       flow->elements[1].raw->type == AuElementTypeExportBucket) ||
	      (flow->elements[1].raw->type == AuElementTypeImportClient &&
	       flow->elements[0].raw->type == AuElementTypeExportBucket)) ||
	     (flow->elements[0].raw->type == AuElementTypeImportBucket &&
	      flow->elements[1].raw->type == AuElementTypeExportClient) ||
	     (flow->elements[1].raw->type == AuElementTypeImportBucket &&
	      flow->elements[0].raw->type == AuElementTypeExportClient));

	flow->count = 0;
    }
    else
	AuFreeFlowElements(flow);

    return status;
}

int
ProcAuSetElementStates(client)
ClientPtr       client;
{
    REQUEST(auSetElementStatesReq);
    int             i;
    auElementState *st;
    FlowPtr        *flows;

    REQUEST_AT_LEAST_SIZE(auSetElementStatesReq);

    /* check the length */
    if (stuff->length != (sizeof(auSetElementStatesReq) +
			  stuff->numStates * sizeof(auElementState)) >> 2)
	AU_ERROR(AuBadLength, 0);

    if (!(flows = (FlowPtr *) aualloc(sizeof(FlowPtr) * stuff->numStates)))
	AU_ERROR(AuBadAlloc, 0);

    st = (auElementState *) & stuff[1];

    /* do some error checking */
    for (i = 0; i < stuff->numStates; i++, st++)
    {
	FlowPtr         flow;

	/* look up the flow */
	if (!(flow = (FlowPtr) LookupIDByType(st->flow, auFlowType)))
	{
	    aufree(flows);
	    AU_ERROR(AuBadFlow, st->flow);
	}

	/*
	 * make sure the element_num is in range and that the element is a
	 * component
	 */
	if (st->element_num != AuElementAll &&
	    (st->element_num >= flow->numElements ||
	     !flow->elements[st->element_num].isComponent))
	{
	    aufree(flows);
	    AU_ERROR(AuBadElement, st->element_num);
	}

	flows[i] = flow;
    }

    AuProcessStateChanges(stuff->numStates, &stuff[1], flows);

    aufree(flows);
    return AuSuccess;
}

int
ProcAuGetElementStates(client)
ClientPtr       client;
{
    REQUEST(auGetElementStatesReq);
    auGetElementStatesReply rep;
    int             i,
                    totalElements = 0;
    auElementState *st;
    FlowPtr        *flows;

    REQUEST_AT_LEAST_SIZE(auGetElementStatesReq);

    /* check the length */
    if (stuff->length != (sizeof(auSetElementStatesReq) +
			  stuff->numStates * sizeof(auElementState)) >> 2)
	AU_ERROR(AuBadLength, 0);

    if (!(flows = (FlowPtr *) aualloc(sizeof(FlowPtr) * stuff->numStates)))
	AU_ERROR(AuBadAlloc, 0);

    st = (auElementState *) & stuff[1];

    /* do some error checking */
    for (i = 0; i < stuff->numStates; i++, st++)
    {
	FlowPtr         flow;

	/* look up the flow */
	if (!(flow = (FlowPtr) LookupIDByType(st->flow, auFlowType)))
	{
	    aufree(flows);
	    AU_ERROR(AuBadFlow, st->flow);
	}

	/*
	 * make sure the element_num is in range and that the element is a
	 * component
	 */
	if (st->element_num != AuElementAll &&
	    (st->element_num >= flow->numElements ||
	     !flow->elements[st->element_num].isComponent))
	{
	    aufree(flows);
	    AU_ERROR(AuBadElement, st->element_num);
	}

	if (st->element_num == AuElementAll)
	    totalElements += flow->numElements;
	else
	    totalElements++;

	flows[i] = flow;
    }

    /* write out the reply */
    rep.type = Au_Reply;
    rep.sequenceNumber = client->sequence;
    rep.length = (sizeof(auElementState) * totalElements) >> 2;
    rep.numStates = totalElements;
    WriteAuReplyToClient(client, sizeof(rep), &rep);

    st = (auElementState *) & stuff[1];

    /* write out the states */
    for (i = 0; i < stuff->numStates; i++, st++)
    {
	auElementState  state;
	int             j,
	                last;

	if (st->element_num == AuElementAll)
	{
	    j = 0;
	    last = flows[i]->numElements - 1;
	}
	else
	    j = last = st->element_num;

	state.flow = st->flow;

	for (; j <= last; j++)
	{
	    state.element_num = j;
	    state.state = flows[i]->elements[j].state;
	    WriteAuSwappedDataToClient(client, sizeof(state), &state,
				       AuSwapElementStateWrite);
	}
    }

    aufree(flows);
    return AuSuccess;
}

int
ProcAuSetElementParameters(client)
ClientPtr       client;
{
    REQUEST(auSetElementParametersReq);
    FlowPtr         flow;
    auElement      *el;
    auElementParameters *parms;
    CARD32         *p;
    AuUint8	   *tmp;
    int             status,
                    len,
                    i;
    AuBool *changed;

    REQUEST_AT_LEAST_SIZE(auSetElementParametersReq);

    parms = (auElementParameters *) & stuff[1];

    len = (stuff->length << 2) - sizeof(auSetElementParametersReq);

    for (i = 0; len && i < stuff->numParameters; i++, parms++)
    {
	if (!(flow = (FlowPtr) LookupIDByType(parms->flow, auFlowType)))
	    AU_ERROR(AuBadFlow, parms->flow);

	if (parms->element_num >= flow->numElements)
	    AU_ERROR(AuBadElement, parms->element_num);

	len -= sizeof(auElementParameters) -
	    parms->num_parameters * sizeof(CARD32);

	if (len < 0)
	    break;

	el = flow->elements[parms->element_num].raw;
	changed = &flow->elements[parms->element_num].parmsChanged;
	p = (CARD32 *) & parms[1];

	tmp = (AuUint8 *) parms;
	tmp += parms->num_parameters * sizeof(CARD32);
	parms = (auElementParameters *) tmp;

	switch (el->type)
	{
	    case AuElementTypeImportClient:
		break;
	    case AuElementTypeImportDevice:
		break;
	    case AuElementTypeImportBucket:
		el->importbucket.offset = p[AuParmsImportBucketOffset];
		flow->count = 0;
		break;
	    case AuElementTypeImportWaveForm:
		el->importwaveform.frequency =
		    p[AuParmsImportWaveFormFrequency];
		el->importwaveform.num_samples =
		    p[AuParmsImportWaveFormNumSamples];
		break;
	    case AuElementTypeBundle:
		break;
	    case AuElementTypeMultiplyConstant:
		el->multiplyconstant.constant =
		    p[AuParmsMultiplyConstantConstant];
		break;
	    case AuElementTypeAddConstant:
		el->addconstant.constant = p[AuParmsAddConstantConstant];
		break;
	    case AuElementTypeSum:
		break;
	    case AuElementTypeExportClient:
		break;
	    case AuElementTypeExportDevice:
		break;
	    case AuElementTypeExportBucket:
		el->exportbucket.offset = p[AuParmsExportBucketOffset];
		flow->count = 0;
		break;
	}

	flow->needsRecompile = AuTrue;
	*changed = AuTrue;
    }

    status = AuSuccess;
    flow = auUnclockedFlows;

    while (flow && (status == AuSuccess))
    {
	if (flow->needsRecompile)
	{
	    status = AuCompileFlow(client, flow);
	    flow->needsRecompile = AuFalse;
	}

	flow = flow->next;
    }

    if (status == AuSuccess)
	AuProcessUnclockedFlows();

    flow = auClockedFlows;

    while (flow && (status == AuSuccess))
    {
	if (flow->needsRecompile)
	{
	    status = AuCompileFlow(client, flow);
	    flow->needsRecompile = AuFalse;
	}

	flow = flow->next;
    }

    if (status == AuSuccess)
	AuProcessClockedFlows();

    return status;
}

#define AuValidDataTransferState(s)					       \
    ((s) == AuTransferStateReady || (s) == AuTransferStatePending ||	       \
     (s) == AuTransferStateEnd)

int
ProcAuWriteElement(client)
ClientPtr       client;
{
    REQUEST(auWriteElementReq);
    FlowPtr         flow;
    AuUint32           currentSize,
                    n;
    AuBlock           l;
    ComponentPtr    c;
    AuUint8          *s,
                   *d;

    REQUEST_AT_LEAST_SIZE(auWriteElementReq);

    if (!(flow = (FlowPtr) LookupIDByType(stuff->flow, auFlowType)))
	AU_ERROR(AuBadFlow, stuff->flow);

    if (stuff->element_num >= flow->numElements ||
	flow->elements[stuff->element_num].raw->type !=
	AuElementTypeImportClient)
	AU_ERROR(AuBadElement, stuff->element_num);

    if (!AuValidDataTransferState(stuff->state))
	AU_ERROR(AuBadValue, stuff->state);

    if (stuff->length - (sizeof(auWriteElementReq) >> 2) !=
	PAD4(stuff->num_bytes) >> 2)
	AU_ERROR(AuBadLength, 0);

    /*
     * if this is a trivial flow (an import to a bucket) then just bcopy the
     * data directly to the bucket
     */
    if (flow->trivial)
    {
	AuUint8          *d;
	FlowElementPtr  el;

	el = &flow->elements[stuff->element_num ? 0 : 1];
	c = el->component;
	d = c->data + el->raw->exportbucket.offset * c->bytesPerSample +
	    flow->count;

	if (d + stuff->num_bytes > c->dataEnd)
	    AU_ERROR(AuBadValue, stuff->num_bytes);

	bcopy((AuUint8 *) &stuff[1], d, stuff->num_bytes);

	flow->count = stuff->state == AuTransferStateEnd ? 0 :
	    flow->count + stuff->num_bytes;

	return AuSuccess;
    }

    c = flow->elements[stuff->element_num].component;

    l = AuBlockAudio();
    currentSize = c->currentSize;
    AuUnBlockAudio(l);

    if (stuff->num_bytes > c->dataSize - currentSize)
	AU_ERROR(AuBadValue, stuff->num_bytes);

    if (stuff->num_bytes)
    {
	s = (AuUint8 *) &stuff[1];
	d = c->write;
	n = aumin(stuff->num_bytes, c->dataEnd - d);
	bcopy(s, d, n);

	/* wrap if necessary */
	if (n != stuff->num_bytes)
	    bcopy(s + n, c->data, stuff->num_bytes - n);

	if ((d += stuff->num_bytes) >= c->dataEnd)
	    d -= c->dataSize;

	l = AuBlockAudio();
	c->write = d;
	c->currentSize += stuff->num_bytes;
	c->incoming -= stuff->num_bytes;
	AuUnBlockAudio(l);
    }

    if (stuff->state != AuTransferStatePending)
    {
	if (stuff->state == AuTransferStateEnd)
	{
	    c->eof = AuTrue;
	    l = AuBlockAudio();
	    c->incoming = 0;
	    AuUnBlockAudio(l);
	}

	/*
	 * if the port is paused and we got some data then start the port
	 * back up
	 */
	if (stuff->num_bytes &&
	    flow->elements[stuff->element_num].state == AuStatePause)
	    AuChangeElementState(flow, stuff->element_num, AuStateStart,
				 AuTrue, AuReasonUser);
    }

    return AuSuccess;
}

int
ProcAuReadElement(client)
ClientPtr       client;
{
    REQUEST(auReadElementReq);
    FlowPtr         flow;
    AuUint32           numBytes,
                    totalBytes,
                    size;
    AuBlock           l;
    ComponentPtr    c;
    AuUint8          *p;
    auReadElementReply rep;

    REQUEST_SIZE_MATCH(auReadElementReq);

    if (!(flow = (FlowPtr) LookupIDByType(stuff->flow, auFlowType)))
	AU_ERROR(AuBadFlow, stuff->flow);

    if (stuff->element_num >= flow->numElements ||
	flow->elements[stuff->element_num].raw->type !=
	AuElementTypeExportClient)
	AU_ERROR(AuBadElement, stuff->element_num);

    /* write out the reply */
    rep.type = Au_Reply;
    rep.sequenceNumber = client->sequence;

    /*
     * if this is a trivial flow (a bucket to an export) then just copy the
     * data directly from the bucket
     */
    if (flow->trivial)
    {
	AuUint8          *s;
	FlowElementPtr  el;

	el = &flow->elements[stuff->element_num ? 0 : 1];
	c = el->component;
	s = c->data + el->raw->importbucket.offset * c->bytesPerSample +
	    flow->count;

	numBytes = aumin(stuff->num_bytes, c->dataEnd - s);

	rep.length = (numBytes + 3) >> 2;
	rep.num_bytes = numBytes;
	WriteAuReplyToClient(client, sizeof(rep), &rep);

	(void) WriteToClient(client, numBytes, s);
	flow->count += numBytes;

	return AuSuccess;
    }

    c = flow->elements[stuff->element_num].component;

    l = AuBlockAudio();
    size = c->currentSize;
    AuUnBlockAudio(l);

    numBytes = totalBytes = aumin(stuff->num_bytes, size);

    rep.length = (numBytes + 3) >> 2;
    rep.num_bytes = numBytes;
    WriteAuReplyToClient(client, sizeof(rep), &rep);

    p = c->read;

    /* see if the read needs to wrap */
    if (p + numBytes > c->dataEnd)
    {
	unsigned int    bytesLeft = c->dataEnd - p;

	/* don't let (void) WriteToClient put in any padding */
	if (bytesLeft & 3)
	{
	    unsigned int    n = bytesLeft & ~3;
	    unsigned char   buf[4];

	    (void) WriteToClient(client, n, p);
	    numBytes -= n;

	    /* copy from end of port to buffer */
	    bytesLeft &= 3;
	    bcopy(p + n, buf, bytesLeft);

	    /* copy from beginning of port to buffer */
	    n = numBytes > 4 - bytesLeft ? 4 - bytesLeft : numBytes;
	    bcopy(c->data, buf + bytesLeft, n);

	    (void) WriteToClient(client, bytesLeft + n, buf);
	    numBytes -= bytesLeft + n;
	    p = c->data + n;
	}
	else
	{
	    (void) WriteToClient(client, bytesLeft, p);
	    p = c->data;
	    numBytes -= bytesLeft;
	}
    }

    if (numBytes)
	(void) WriteToClient(client, numBytes, p);

    p += numBytes;

    if (p >= c->dataEnd)
	p -= c->dataSize;

    l = AuBlockAudio();
    c->outgoing -= totalBytes;
    c->currentSize -= totalBytes;
    c->read = p;
    AuUnBlockAudio(l);

    /* if the port was paused and we read some data, start it back up */
    if (flow->elements[stuff->element_num].state == AuStatePause)
	AuChangeElementState(flow, stuff->element_num, AuStateStart,
			     AuTrue, AuReasonUser);

    return client->noClientException;
}

int
ProcAuGetServerTime(client)
ClientPtr       client;
{
    REQUEST(auReq);
    auGetServerTimeReply rep;

    REQUEST_SIZE_MATCH(auReq);

    UpdateCurrentTimeIf();

    rep.type = Au_Reply;
    rep.sequenceNumber = client->sequence;
    rep.time = currentTime.milliseconds;
    rep.length = 0;

    WriteAuReplyToClient(client, sizeof(auGetCloseDownModeReply), &rep);
    return client->noClientException;
}

int
ProcAuGetCloseDownMode(client)
ClientPtr       client;
{
    REQUEST(auReq);
    auGetCloseDownModeReply rep;

    REQUEST_SIZE_MATCH(auReq);

    rep.type = Au_Reply;
    rep.sequenceNumber = client->sequence;
    rep.closeDownMode = client->closeDownMode;
    rep.length = 0;

    WriteAuReplyToClient(client, sizeof(auGetCloseDownModeReply), &rep);
    return client->noClientException;
}

int
ProcAuSetCloseDownMode(client)
ClientPtr       client;
{
    REQUEST(auSetCloseDownModeReq);

    REQUEST_SIZE_MATCH(auSetCloseDownModeReq);

    if ((stuff->mode == AuCloseDownDestroy) ||
	(stuff->mode == AuCloseDownRetainPermanent) ||
	(stuff->mode == AuCloseDownRetainTemporary))
    {
	client->closeDownMode = stuff->mode;
	return client->noClientException;
    }
    else
    {
	client->errorValue = stuff->mode;
	return AuBadValue;
    }
}

/*
 * XXX maybe just call to ProcKillClient()?
 */

int
ProcAuKillClient(client)
ClientPtr       client;
{
    REQUEST(auResourceReq);

    pointer        *pResource;
    int             clientIndex,
                    myIndex;

    REQUEST_SIZE_MATCH(auResourceReq);

    if (stuff->id == AuCloseDownDestroy)
    {
	CloseDownRetainedResources();
	return (client->noClientException);
    }
    pResource = (pointer *) LookupIDByClass(stuff->id, RC_ANY);
    clientIndex = CLIENT_ID(stuff->id);

    if (clientIndex && pResource && clients[clientIndex] &&
	!(stuff->id & SERVER_BIT) &&
	(clients[clientIndex]->requestVector != InitialVector))
    {
	myIndex = client->index;
	CloseDownClient(clients[clientIndex]);
	if (myIndex == clientIndex)
	{
	    /*
	     * force yield and return Success, so that Dispatch() doesn't try
	     * to touch client
	     */
	    isItTimeToYield = AuTrue;
	    return (AuSuccess);
	}
	return (client->noClientException);
    }
    else
    {
	client->errorValue = stuff->id;
	return (AuBadValue);
    }
}

int
ProcAuNoOperation(client)
ClientPtr       client;
{
    return AuSuccess;
}

int
AuInitSetupReply()
{
    int             len;
    char           *rel = release + 4;
    static char    *string;

    if (!AuInitDevice(&auSetup, &len))
	return 0;

    if (string)
    {
	aufree(string);
	string = (char *) 0;
    }

    if (!(string = (char *) aualloc(strlen(rel) +
				    3 + strlen(VENDOR_STRING) + 1)))
	return 0;

    strcpy(string, rel);
    strcat(string, " - ");
    strcat(string, VENDOR_STRING);

    auVendorString = string;
    auSetupPrefix.success = AuTrue;
    auSetupPrefix.majorVersion = AuProtocolMajorVersion;
    auSetupPrefix.minorVersion = AuProtocolMinorVersion;

    auSetup.release = VENDOR_RELEASE;
    auSetup.nbytesVendor = strlen(auVendorString);
    auSetup.maxRequestSize = MAX_REQUEST_SIZE;

    auSetupPrefix.length = (sizeof(auConnSetup) + len +
			    PAD4(auSetup.nbytesVendor)) >> 2;

    return 1;
}

/* sends return init packet */
int
AuSendInitResponse(client)
ClientPtr       client;
{
    extern ComponentPtr *auServerDevices,	/* array of devices */
                   *auServerBuckets,		/* array of server owned
						 * buckets */
                   *auServerRadios;		/* array of server owned
						 * radios */
    extern AuUint32    auNumServerDevices,		/* number of devices */
                    auNumServerBuckets,		/* number of server owned
						 * buckets */
                    auNumServerRadios;		/* number of server owned
						 * radios */
    extern unsigned char auFormats[],
                    auElementTypes[],
                    auWaveForms[],
                    auActions[];
    int             i;

    auSetup.ridBase = client->clientAsMask;
    auSetup.ridMask = RESOURCE_ID_MASK;

    WriteAuSwappedDataToClient(client, sizeof(auSetupPrefix),
			       &auSetupPrefix, AuSwapSetupPrefix);
    WriteAuSwappedDataToClient(client, sizeof(auSetup), &auSetup,
			       AuSwapSetup);
    (void) WriteToClient(client, auSetup.nbytesVendor, auVendorString);
    (void) WriteToClient(client, auSetup.numFormats, auFormats);
    (void) WriteToClient(client, auSetup.numElementTypes, auElementTypes);
    (void) WriteToClient(client, auSetup.numWaveForms, auWaveForms);
    (void) WriteToClient(client, auSetup.numActions, auActions);

    for (i = 0; i < auNumServerDevices; i++)
    {
	auDeviceAttributes da;
	ComponentPtr    d;

	d = auServerDevices[i];

	if (d->kind == AuComponentKindPhysicalOutput)
	    d->gain = AuGetOutputGain();

	xferDeviceAttributes(d, da);

	WriteAuSwappedDataToClient(client, sizeof(da), &da,
				   AuSwapDeviceAttributesWrite);
	(void) WriteToClient(client, d->description.len, d->description.string);

	if (d->numChildren)
	{
	    bcopy(d->children, d->childSwap,
		  d->numChildren * sizeof(AuDeviceID));
	    WriteAuSwappedDataToClient(client,
				       d->numChildren * sizeof(AuDeviceID),
				       d->childSwap, Swap32Write);
	}
    }

    for (i = 0; i < auNumServerBuckets; i++)
    {
	auBucketAttributes ba;
	ComponentPtr    c;

	c = auServerBuckets[i];

	xferBucketAttributes(c, ba);

	WriteAuSwappedDataToClient(client, sizeof(ba), &ba,
				   AuSwapBucketAttributesWrite);
	(void) WriteToClient(client, c->description.len, c->description.string);
    }

    return (client->noClientException);
}

/* XXX - could probably toss this and just use SendErrorToClient */
void
SendAuErrorToClient(client, majorCode, minorCode, resId, errorCode)
ClientPtr       client;
unsigned        majorCode;
unsigned short  minorCode;
AuID             resId;
int             errorCode;
{
    auError         rep;

    rep.type = Au_Error;
    rep.sequenceNumber = client->sequence;
    rep.errorCode = errorCode;
    rep.majorCode = majorCode;
    rep.minorCode = minorCode;
    rep.resourceID = resId;

    WriteEventsToClient(client, 1, (auEvent *) & rep);
}
