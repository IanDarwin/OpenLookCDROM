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
 * $NCDId: @(#)auutil.c,v 1.15 1994/06/01 01:20:09 greg Exp $
 */

#define		_AUUTIL_C_

#include	"misc.h"
#include	"dixstruct.h"
#include	<audio/audio.h>
#include	<audio/Aproto.h>
#include	"au.h"

extern void     AuDequeueEvents(), AuSetupCompiledFlow(),
                AuRequestElementNotifyEvent();
extern AuBool   AuStartFlow();
extern AuFixedPoint AuGetOutputGain();

extern int      auMinibufSamples;
extern ComponentPtr auBuckets,
                auDevices;

AuBool          AuChangeElementState();

RESTYPE         auComponentType,
                auFlowType;
FlowPtr         auClockedFlows,
                auUnclockedFlows;
AuBool          auAverageFlows = AuFalse;

unsigned int    auBytesPerSample[] =
{
    0,						/* unused */
    1,						/* ULAW8 */
    1,						/* LinearUnsigned8 */
    1,						/* LinearSigned8 */
    2,						/* LinearSigned16MSB */
    2,						/* LinearUnsigned16MSB */
    2,						/* LinearSigned16LSB */
    2,						/* LinearUnsigned16LSB */
};

void
AuFreeComponent(c)
ComponentPtr    c;
{
    if (!c->refCnt--)
    {
	if (c->kind == AuComponentKindBucket)
	{
	    RemoveFromLinkedList(auBuckets, c);
	}
	else if (c->kind == AuComponentKindPhysicalInput ||
		 c->kind == AuComponentKindPhysicalOutput)
	{
	    RemoveFromLinkedList(auDevices, c);
	}

	aufree(c);
    }
}

static void
freeCompiledFlow(cmp)
CompiledFlowPtr cmp;
{
    int             i;

    for (i = 0; i < cmp->numOutputs; i++)
    {
	if (cmp->outputs[i].inputs)
	{
	    if (cmp->freeComponents)
	    {
		int             j;

		for (j = 0; j < cmp->outputs[i].numInputs; j++)
		{
		    CompiledFlowInputPtr in = &cmp->outputs[i].inputs[j];

		    if (!in->flowEl->refCnt-- &&
			in->flowEl->raw->type == AuElementTypeImportBucket)
			aufree(in->flowEl->minibuf);

		    if (in->freeDevPrivate)
			(*in->freeDevPrivate) (in->devPrivate);

		    AuFreeComponent(in->component);
		}
	    }

	    aufree(cmp->outputs[i].inputs);
	}

	if (cmp->freeComponents)
	{
	    if (cmp->outputs[i].flowEl->raw->type == AuElementTypeExportBucket)
		aufree(cmp->outputs[i].flowEl->minibuf);

	    AuFreeComponent(cmp->outputs[i].component);
	}

	if (cmp->outputs[i].freeDevPrivate)
	    (*cmp->outputs[i].freeDevPrivate) (cmp->outputs[i].devPrivate);
    }

    aufree(cmp->outputs);

    if (cmp->freeDevPrivate)
	(*cmp->freeDevPrivate) (cmp->devPrivate);

    cmp->numOutputs = 0;
    cmp->outputs = NULL;
}

void
AuFreeFlowElements(flow)
FlowPtr         flow;
{
    /* stop the flow if it's running */
    if (flow->state != AuStateStop)
	AuChangeElementState(flow, AuElementAll, AuStateStop, AuTrue,
			     AuReasonUser);

    if (flow->clocked)
    {
	RemoveFromLinkedList(auClockedFlows, flow);
    }
    else
    {
	RemoveFromLinkedList(auUnclockedFlows, flow);
    }

    freeCompiledFlow(&flow->compiled);
    aufree(flow->elements);
    flow->elements = NULL;
    flow->numElements = 0;
}

static void
freeFlow(flow)
FlowPtr         flow;
{
    if (flow->elements)
	AuFreeFlowElements(flow);

    AuDequeueEvents(flow);
    aufree(flow);
}

void
AuCreateResourceTypes()
{
    void            AuFreeComponent();
    static void     freeFlow();

    auComponentType = CreateNewResourceType(AuFreeComponent);
    auFlowType = CreateNewResourceType(freeFlow);
}

AuPointer
aucalloc(nelem, elsize)
AuUint32        nelem,
                elsize;
{
    AuPointer       p;

    if ((p = aualloc(nelem * elsize)))
	bzero((char *)p, nelem * elsize);

    return p;
}

AuPointer
auProtectedAlloc(size)
AuUint32        size;
{
    AuBlock         l = AuBlockAudio();
    AuPointer       p;

    p = (AuPointer) xalloc(size);
    AuUnBlockAudio(l);
    return p;
}

AuPointer
auProtectedRealloc(p, size)
AuPointer       p;
AuUint32        size;
{
    AuBlock         l = AuBlockAudio();

    p = (AuPointer) xrealloc(p, size);
    AuUnBlockAudio(l);
    return p;
}

void
auProtectedFree(p)
AuPointer       p;
{
    AuBlock         l = AuBlockAudio();

    xfree(p);
    AuUnBlockAudio(l);
}

/* create an import or export */
static          ComponentPtr
createPort(format, numTracks, numSamples, lowWater, highWater, discard)
AuUint32        format,
                numTracks,
                numSamples,
                lowWater,
                highWater;
AuBool          discard;
{
    ComponentPtr    port;
    AuUint32        dataSize,
                    minibufSize;

    dataSize = numSamples * sizeofFormat(format) * numTracks;
    minibufSize = auNativeBytesPerSample * auMinibufSamples * numTracks;

    if (!(port = (ComponentPtr) aualloc(PAD4(sizeof(ComponentRec)) +
					PAD4(dataSize) +
					PAD4(minibufSize))))
	return NULL;

    port->numSamples = numSamples;
    port->format = format;
    port->numTracks = numTracks;
    port->lowWaterMark = lowWater * sizeofFormat(format) * numTracks;
    port->highWaterMark = highWater * sizeofFormat(format) * numTracks;
    port->dataSize = dataSize;
    port->data = (AuUint8 *) port + PAD4(sizeof(ComponentRec));
    port->dataEnd = port->data + port->dataSize;
    port->refCnt = 0;
    port->read = port->write = port->data;
    port->currentSize = 0;
    port->discard = discard;
    port->minibuf = port->data + PAD4(dataSize);
    port->minibufSize = minibufSize;
    port->kind = AuComponentKindOther;
    port->bytesPerSample = port->numTracks * sizeofFormat(port->format);
    port->physicalDeviceMask = NotAPhysicalDevice;

    return port;
}

/* create a monitor export */
static          ComponentPtr
createMonitor(format, numTracks)
AuUint32        format,
                numTracks;
{
    ComponentPtr    c;
    AuUint32        minibufSize;

    minibufSize = auNativeBytesPerSample * auMinibufSamples * numTracks;

    if (!(c = (ComponentPtr) aualloc(PAD4(sizeof(ComponentRec)) +
				     PAD4(minibufSize) * 3)))
	return NULL;

    c->format = format;
    c->numTracks = numTracks;
    c->refCnt = 0;
    c->minibuf = (AuUint8 *) c + PAD4(sizeof(ComponentRec));
    c->read = c->minibuf + PAD4(minibufSize);
    c->minibufSize = minibufSize;
    c->physicalDeviceMask = NotAPhysicalDevice;
    c->kind = AuComponentKindOther;
    c->bytesPerSample = c->numTracks * sizeofFormat(c->format);

    return c;
}

/* create a wave form import */
static          ComponentPtr
createWaveForm(form)
int             form;
{
    ComponentPtr    c;
    AuUint32        minibufSize, n;
    /* these arrays must have a power of 2 number of elements */
    static AuUint16 squareWave[] =
    {
	0x7fff, 0x8001
    },
                    sineWave[] =
    {
	0x0000, 0x0324, 0x0647, 0x096a, 0x0c8b, 0x0fab, 0x12c7, 0x15e1,
	0x18f8, 0x1c0b, 0x1f19, 0x2223, 0x2527, 0x2826, 0x2b1e, 0x2e10,
	0x30fb, 0x33de, 0x36b9, 0x398c, 0x3c56, 0x3f16, 0x41cd, 0x447a,
	0x471c, 0x49b3, 0x4c3f, 0x4ebf, 0x5133, 0x539a, 0x55f4, 0x5842,
	0x5a81, 0x5cb3, 0x5ed6, 0x60eb, 0x62f1, 0x64e7, 0x66ce, 0x68a5,
	0x6a6c, 0x6c23, 0x6dc9, 0x6f5e, 0x70e1, 0x7254, 0x73b5, 0x7503,
	0x7640, 0x776b, 0x7883, 0x7989, 0x7a7c, 0x7b5c, 0x7c29, 0x7ce2,
	0x7d89, 0x7e1c, 0x7e9c, 0x7f08, 0x7f61, 0x7fa6, 0x7fd7, 0x7ff5,
	0x7fff, 0x7ff5, 0x7fd7, 0x7fa6, 0x7f61, 0x7f08, 0x7e9c, 0x7e1c,
	0x7d89, 0x7ce2, 0x7c29, 0x7b5c, 0x7a7c, 0x7989, 0x7883, 0x776b,
	0x7640, 0x7503, 0x73b5, 0x7254, 0x70e1, 0x6f5e, 0x6dc9, 0x6c23,
	0x6a6c, 0x68a5, 0x66ce, 0x64e7, 0x62f1, 0x60eb, 0x5ed6, 0x5cb3,
	0x5a81, 0x5842, 0x55f4, 0x539a, 0x5133, 0x4ebf, 0x4c3f, 0x49b3,
	0x471c, 0x447a, 0x41cd, 0x3f16, 0x3c56, 0x398c, 0x36b9, 0x33de,
	0x30fb, 0x2e10, 0x2b1e, 0x2826, 0x2527, 0x2223, 0x1f19, 0x1c0b,
	0x18f8, 0x15e1, 0x12c7, 0x0fab, 0x0c8b, 0x096a, 0x0647, 0x0324,
	0x0000, 0xfcdc, 0xf9b9, 0xf696, 0xf375, 0xf055, 0xed39, 0xea1f,
	0xe708, 0xe3f5, 0xe0e7, 0xdddd, 0xdad9, 0xd7da, 0xd4e2, 0xd1f0,
	0xcf05, 0xcc22, 0xc947, 0xc674, 0xc3aa, 0xc0ea, 0xbe33, 0xbb86,
	0xb8e4, 0xb64d, 0xb3c1, 0xb141, 0xaecd, 0xac66, 0xaa0c, 0xa7be,
	0xa57f, 0xa34d, 0xa12a, 0x9f15, 0x9d0f, 0x9b19, 0x9932, 0x975b,
	0x9594, 0x93dd, 0x9237, 0x90a2, 0x8f1f, 0x8dac, 0x8c4b, 0x8afd,
	0x89c0, 0x8895, 0x877d, 0x8677, 0x8584, 0x84a4, 0x83d7, 0x831e,
	0x8277, 0x81e4, 0x8164, 0x80f8, 0x809f, 0x805a, 0x8029, 0x800b,
	0x8001, 0x800b, 0x8029, 0x805a, 0x809f, 0x80f8, 0x8164, 0x81e4,
	0x8277, 0x831e, 0x83d7, 0x84a4, 0x8584, 0x8677, 0x877d, 0x8895,
	0x89c0, 0x8afd, 0x8c4b, 0x8dac, 0x8f1f, 0x90a2, 0x9237, 0x93dd,
	0x9594, 0x975b, 0x9932, 0x9b19, 0x9d0f, 0x9f15, 0xa12a, 0xa34d,
	0xa57f, 0xa7be, 0xaa0c, 0xac66, 0xaecd, 0xb141, 0xb3c1, 0xb64d,
	0xb8e4, 0xbb86, 0xbe33, 0xc0ea, 0xc3aa, 0xc674, 0xc947, 0xcc22,
	0xcf05, 0xd1f0, 0xd4e2, 0xd7da, 0xdad9, 0xdddd, 0xe0e7, 0xe3f5,
	0xe708, 0xea1f, 0xed39, 0xf055, 0xf375, 0xf696, 0xf9b9, 0xfcdc,
    };

    minibufSize = auNativeBytesPerSample * auMinibufSamples;

    if (!(c = (ComponentPtr) aualloc(PAD4(sizeof(ComponentRec)) +
				     PAD4(minibufSize))))
	return NULL;

    c->format = auNativeFormat;
    c->numTracks = 1;
    c->refCnt = 0;
    c->minibuf = (AuUint8 *) c + PAD4(sizeof(ComponentRec));
    c->minibufSize = minibufSize;
    c->physicalDeviceMask = NotAPhysicalDevice;
    c->kind = AuComponentKindOther;
    c->bytesPerSample = sizeofFormat(c->format);

    switch (form)
    {
	case AuWaveFormSquare:
	    c->data = (AuUint8 *) squareWave;
	    n = sizeof(squareWave);
	    break;
	case AuWaveFormSine:
	    c->data = (AuUint8 *) sineWave;
	    n = sizeof(sineWave);
	    break;
    }

    c->dataEnd = c->data + n;
    c->waveSamples = n / auNativeBytesPerSample;
    return c;
}

static int
compileInputs(client, elements, output, inputNum, multiplyConstant, addConstant,
	      numTracks, inTracks, firstOutTrack, recompile, inputCnt)
ClientPtr       client;
FlowElementPtr  elements;
CompiledFlowOutputPtr output;
AuUint32        inputNum,
               *inputCnt;
AuFixedPoint    multiplyConstant,
                addConstant;
AuBool          recompile;
AuUint8         numTracks,
               *inTracks,
                firstOutTrack;

{
    auElement      *el = elements[inputNum].raw;
    int             status = AuSuccess;
    AuBool          compiled = elements[inputNum].compiled,
                    compileInput = AuFalse;
    AuUint32        nextInput;
    AuUint8         inputTracks[auMaxTracks];

    /* XXX - need to check for loops */

    /* indicate that we've compiled this element */
    elements[inputNum].compiled = AuTrue;

    switch (el->type)
    {
	case AuElementTypeImportBucket:
	case AuElementTypeImportDevice:
	case AuElementTypeImportClient:
	case AuElementTypeImportWaveForm:
	    {
		CompiledFlowInputPtr input;

		if (!recompile)
		{
		    if (!(input = (CompiledFlowInputPtr)
			  aurealloc(output->inputs,
				    sizeof(CompiledFlowInputRec) *
				    (output->numInputs + 1))))
			AU_ERROR(AuBadAlloc, 0);

		    output->inputs = input;
		}

		input = &output->inputs[(*inputCnt)++];
		input->multiplyConstant = multiplyConstant;
		input->addConstant = addConstant;
		input->devPrivate = NULL;
		input->freeDevPrivate = (void (*) ()) NULL;

		if (recompile)
		    break;

		input->flowEl = &elements[inputNum];
		input->numTracks = numTracks;

		if (numTracks)
		{
		    int             i;

		    for (i = 0; i < (int) numTracks; i++)
			input->outTrack[i] = firstOutTrack++;

		    bcopy(inTracks, input->inTrack, numTracks);
		}

		if (compiled)
		{
		    input->component = input->flowEl->component;
		    input->flowEl->refCnt++;
		    input->component->refCnt++;
		    output->numInputs++;
		    break;
		}

		switch (el->type)
		{
		    case AuElementTypeImportClient:
			{
			    auElementImportClient *p = &el->importclient;

			    if (!(input->component =
				  createPort(p->format, p->num_tracks,
					  p->max_samples, p->low_water_mark,
					     0, p->discard)))
				AU_ERROR(AuBadAlloc, 0);

			    input->component->sampleRate = p->sample_rate;
			    break;
			}
		    case AuElementTypeImportWaveForm:
			{
			    auElementImportWaveForm *p = &el->importwaveform;

			    if (!(input->component =
				  createWaveForm(p->wave_form)))
				AU_ERROR(AuBadAlloc, 0);

			    input->component->sampleRate = p->sample_rate;
			    break;
			}
		    case AuElementTypeImportDevice:
			{
			    auElementImportDevice *p = &el->importdevice;

			    if (!(input->component = (ComponentPtr)
				LookupIDByType(p->device, auComponentType)))
				AU_ERROR(AuBadDevice, p->device);

			    input->component->refCnt++;
			    input->component->sampleRate = p->sample_rate;
			    break;
			}
		    case AuElementTypeImportBucket:
			{
			    auElementImportBucket *p = &el->importbucket;

			    if (!(input->component = (ComponentPtr)
				LookupIDByType(p->bucket, auComponentType)))
				AU_ERROR(AuBadBucket, p->bucket);

			    input->flowEl->sampleRate = p->sample_rate;
			    break;
			}
		}

		if (el->type == AuElementTypeImportBucket)
		{
		    if (!(input->flowEl->minibuf = (AuUint8 *)
			  aualloc(input->component->minibufSize)))
			AU_ERROR(AuBadAlloc, 0);

		    input->component->refCnt++;
		}
		else
		    input->flowEl->minibuf = input->component->minibuf;

		input->flowEl->component = input->component;
		output->numInputs++;
	    }
	    break;
	case AuElementTypeBundle:
	    {
		int             i;
		auInputTrack   *tracks = (auInputTrack *) & el[1];

		numTracks = firstOutTrack = 0;
		inTracks = inputTracks;
		nextInput = -1;

		for (i = 0; i < (int) el->bundle.num_inputs; i++, tracks++)
		{
		    if (numTracks && tracks->element_num != nextInput)
		    {
			status = compileInputs(client, elements, output,
					       nextInput, multiplyConstant,
					       addConstant, numTracks,
					       inTracks, firstOutTrack,
					       recompile, inputCnt);

			if (status != AuSuccess)
			    return status;

			firstOutTrack += numTracks;
			numTracks = 0;
		    }

		    nextInput = tracks->element_num;
		    inTracks[numTracks++] = tracks->track;
		}

		compileInput = AuTrue;
		break;
	    }
	case AuElementTypeMultiplyConstant:
	    multiplyConstant =
		AuFixedPointMultiply(multiplyConstant,
				     el->multiplyconstant.constant);
	    compileInput = AuTrue;
	    nextInput = el->multiplyconstant.input;
	    break;
	case AuElementTypeAddConstant:
	    addConstant += el->addconstant.constant;
	    compileInput = AuTrue;
	    nextInput = el->addconstant.input;
	    break;
	case AuElementTypeSum:
	    {
		AuUint16       *inputs = (AuUint16 *) &el[1];
		int             i;

		for (i = 0;
		     i < (int) el->sum.num_inputs && status == AuSuccess; i++)
		    status = compileInputs(client, elements, output, *inputs++,
				   multiplyConstant, addConstant, numTracks,
					 inTracks, firstOutTrack, recompile,
					   inputCnt);
		break;
	    }
	default:
	    AU_ERROR(AuBadElement, inputNum);
    }

    if (compileInput)
	status = compileInputs(client, elements, output, nextInput,
			       multiplyConstant, addConstant, numTracks,
			       inTracks, firstOutTrack, recompile, inputCnt);

    return status;
}

int
AuCompileFlow(client, flow)
ClientPtr       client;
FlowPtr         flow;
{
    AuUint32        i,
                    outputCnt = 0,
                    inputCnt,
                    status;
    auElement      *el;
    CompiledFlowPtr cmp = &flow->compiled;
    CompiledFlowOutputPtr output;
    AuBool          recompile;

    recompile = cmp->numOutputs ? AuTrue : AuFalse;

    for (i = 0; i < flow->numElements; i++)
    {
	el = flow->elements[i].raw;

	/* look for all of the exports */
	switch (el->type)
	{
	    case AuElementTypeExportBucket:
	    case AuElementTypeExportClient:
	    case AuElementTypeExportDevice:
	    case AuElementTypeExportMonitor:
		if (!recompile)
		{
		    /* alloc space for the new output */
		    if (!(output = (CompiledFlowOutputPtr)
			  aurealloc(cmp->outputs,
				    sizeof(CompiledFlowOutputRec) *
				    (cmp->numOutputs + 1))))
			AU_ERROR(AuBadAlloc, 0);

		    cmp->outputs = output;
		}

		output = &cmp->outputs[outputCnt++];

		if (!recompile)
		{
		    output->flowEl = &flow->elements[i];

		    switch (el->type)
		    {
			case AuElementTypeExportClient:
			    {
				auElementExportClient *p = &el->exportclient;

				if (!(output->component =
				      createPort(p->format, p->num_tracks,
						 p->max_samples, 0,
					   p->high_water_mark, p->discard)))
				    AU_ERROR(AuBadAlloc, 0);

				output->component->sampleRate = p->sample_rate;
				output->firstInput = el->exportclient.input;
				break;
			    }
			case AuElementTypeExportDevice:
			    {
				auElementExportDevice *p = &el->exportdevice;

				if (!(output->component = (ComponentPtr)
				LookupIDByType(p->device, auComponentType)))
				    AU_ERROR(AuBadDevice, p->device);

				output->component->refCnt++;
				output->component->sampleRate = p->sample_rate;
				output->firstInput = el->exportdevice.input;
				break;
			    }
			case AuElementTypeExportBucket:
			    {
				auElementExportBucket *p = &el->exportbucket;

				if (!(output->component = (ComponentPtr)
				LookupIDByType(p->bucket, auComponentType)))
				    AU_ERROR(AuBadBucket, p->bucket);
				output->firstInput = el->exportbucket.input;
				break;
			    }
			case AuElementTypeExportMonitor:
			    {
				auElementExportMonitor *p = &el->exportmonitor;

				if (!(output->component =
				   createMonitor(p->format, p->num_tracks)))
				    AU_ERROR(AuBadAlloc, 0);

				output->component->sampleRate = p->event_rate;
				output->firstInput = el->exportmonitor.input;
				break;
			    }
		    }

		    if (el->type == AuElementTypeExportBucket)
		    {
			if (!(output->flowEl->minibuf = (AuUint8 *)
			      aualloc(output->component->minibufSize)))
			    AU_ERROR(AuBadAlloc, 0);

			output->component->refCnt++;
		    }
		    else
			output->flowEl->minibuf = output->component->minibuf;

		    /* initialize compiled output */
		    output->numInputs = 0;
		    output->inputs = NULL;
		    output->devPrivate = NULL;
		    output->freeDevPrivate = (void (*) ()) NULL;
		    output->flowEl->component = output->component;
		    output->flowEl->compiled = AuTrue;
		    cmp->numOutputs++;
		}

		/* compile the inputs for this output */
		inputCnt = 0;
		status = compileInputs(client, flow->elements, output,
				       output->firstInput,
				       AuFixedPointFromSum(1, 0),
				       AuFixedPointFromSum(0, 0),
				       0, NULL, 0, recompile, &inputCnt);

		if (status != AuSuccess)
		    return status;

		break;
	}
    }

    return AuSuccess;
}

static          CompiledFlowPtr
AuProcessFlows(fl)
FlowPtr         fl;
{
    FlowPtr         flow,
                    f;
    CompiledFlowPtr new;
    CompiledFlowOutputPtr output,
                    newOutput;
    int             i,
                    j,
                    numGroups,
                    totalInputs;
    AuUint32        maxRate = 0;
    CompiledFlowOutputPtr *groups;

    /* allocate and clear the compiled flow */
    if (!(new = (CompiledFlowPtr) aucalloc(1, sizeof(CompiledFlowRec))))
	return NULL;		/* XXX: this means we'll keep trying */

    /* clear all the processed flags */
    flow = fl;

    while (flow)
    {
	output = flow->compiled.outputs;

	for (i = 0; i < flow->compiled.numOutputs; i++, output++)
	    output->component->processed = AuFalse;

	flow = flow->next;
    }

    flow = fl;

    while (flow)
    {
	output = flow->compiled.outputs;

	/* nothing to do if we're not starting the flow */
	if (flow->pendingState != AuStateStart)
	{
	    flow = flow->next;
	    continue;
	}

	/* process all the outputs in the flow */
	for (i = 0; i < flow->compiled.numOutputs; i++, output++)
	{
	    if (output->component->processed)
		continue;

	    if (!(newOutput = (CompiledFlowOutputPtr)
		  aurealloc(new->outputs, sizeof(CompiledFlowOutputRec) *
			    (new->numOutputs + 1))))
	    {
		freeCompiledFlow(new);
		aufree(new);
		return NULL;	/* XXX: this means we'll keep trying */
	    }

	    new->outputs = newOutput;
	    newOutput = &new->outputs[new->numOutputs];
	    new->numOutputs++;

	    newOutput->component = output->component;
	    newOutput->numInputs = 0;
	    newOutput->inputs = NULL;
	    newOutput->flowEl = output->flowEl;
	    newOutput->devPrivate = NULL;
	    newOutput->freeDevPrivate = (void (*) ()) NULL;
	    new->physicalDeviceMask |= output->component->physicalDeviceMask;

	    /* accumulate all the input groups for this output */
	    f = fl;
	    numGroups = totalInputs = 0;
	    groups = NULL;

	    while (f)
	    {
		int             k;
		CompiledFlowOutputPtr *group,
		                out;
		CompiledFlowPtr cmp;

		if (f->pendingState != AuStateStart)
		{
		    f = f->next;
		    continue;
		}

		cmp = &f->compiled;

		for (k = 0; k < cmp->numOutputs; k++)
		{
		    out = &cmp->outputs[k];

		    /* look for the current output */
		    if (out->component != output->component)
			continue;

		    if (!(group = (CompiledFlowOutputPtr *)
			  aurealloc(groups, sizeof(CompiledFlowOutputPtr) *
				    (numGroups + 1))))
		    {
			freeCompiledFlow(new);
			aufree(groups);
			aufree(new);
			return NULL;	/* XXX: this means we'll keep trying */
		    }

		    groups = group;
		    groups[numGroups++] = out;
		    totalInputs += out->numInputs;
		    out->component->processed = AuTrue;
		}

		f = f->next;
	    }

	    if (!(newOutput->inputs = (CompiledFlowInputPtr)
		  aualloc(sizeof(CompiledFlowInputRec) * totalInputs)))
	    {
		freeCompiledFlow(new);
		aufree(groups);
		aufree(new);
		return NULL;	/* XXX: this means we'll keep trying */
	    }

	    /* special case for only one group of inputs */
	    if (numGroups == 1)
	    {
		/* copy the inputs */
		bcopy(output->inputs, newOutput->inputs,
		      sizeof(CompiledFlowInputRec) * output->numInputs);
	    }
	    else
	    {
		/*
		 * adjust the multiply constants to average all the input
		 * groups
		 */
		CompiledFlowOutputPtr *group = groups;
		CompiledFlowInputPtr input = newOutput->inputs;
		AuFixedPoint    m = (1 << 16) / numGroups;

		while (numGroups--)
		{
		    int             k;

		    /* copy the group */
		    bcopy((*group)->inputs, input,
			sizeof(CompiledFlowInputRec) * (*group)->numInputs);

		    if (auAverageFlows)
			/* average the multiply constants */
			for (k = 0; k < (*group)->numInputs; k++, input++)
			    input->multiplyConstant =
				AuFixedPointMultiply(input->multiplyConstant,
						     m);
		    else
			input += (*group)->numInputs;

		    group++;
		}
	    }

	    newOutput->numInputs = totalInputs;
	    aufree(groups);

	    /*
	     * see if any of the inputs have a higher sampling rate and
	     * initialize some stuff
	     */
	    for (j = 0; j < newOutput->numInputs; j++)
	    {
		CompiledFlowInputPtr input = &newOutput->inputs[j];

		new->physicalDeviceMask |= input->component->physicalDeviceMask;

		if (input->flowEl->raw->type == AuElementTypeImportBucket)
		{
		    input->preadTag = &input->flowEl->readTag;

		    if (input->flowEl->sampleRate > maxRate)
			maxRate = input->flowEl->sampleRate;
		}
		else
		{
		    input->preadTag = &input->component->readTag;

		    if (input->component->sampleRate > maxRate)
			maxRate = input->component->sampleRate;
		}
	    }

	    if (newOutput->component->sampleRate > maxRate)
		maxRate = newOutput->component->sampleRate;

	    newOutput->component->processed = AuTrue;
	}

	flow = flow->next;
    }

    flow = fl;

    /* update all of the states */
    while (flow)
    {
	flow->state = flow->pendingState;
	flow = flow->next;
    }

    AuSetupCompiledFlow(new, maxRate);
    return new;
}

void
AuProcessUnclockedFlows()
{
    /* XXX: yet to be implemented */
}

void
AuProcessClockedFlows()
{
    CompiledFlowPtr new,
                    old;

    if ((new = AuProcessFlows(auClockedFlows)))
    {
	if (!AuStartFlow(new, &old))
	{
	    freeCompiledFlow(new);
	    aufree(new);
	}

	if (old)
	{
	    freeCompiledFlow(old);
	    aufree(old);
	}
    }
}

static void
setFlowState(flow)
FlowPtr         flow;
{
    int             i,
                    stop,
                    start,
                    pause;

    start = stop = pause = 0;

    for (i = 0; i < flow->numElements; i++)
	if (flow->elements[i].isComponent)
	    switch (flow->elements[i].state)
	    {
		case AuStateStop:
		    stop++;
		    break;
		case AuStateStart:
		    start++;
		    break;
		default:	/* AuStatePause */
		    pause++;
	    }

    /**
     * Flow state is:
     *
     *	AuStateStart - if any element is started
     *  AuStatePause - if no elements are started and any element is paused
     *  AuStateStop  - if all elements are stopped
     */
    flow->pendingState = start ? AuStateStart :
	(pause ? AuStatePause : AuStateStop);
}

#define CHECK_STATE(t, s) ((t) == AuStateAny || (t) == (s))
#define CHECK_REASON(t, r) ((t) == AuReasonAny || (t) == (r))

static void
checkForTriggeredActions(el, processFlow, reason)
FlowElementPtr  el;
AuBool          processFlow;
int             reason;
{
    int             i,
                    state = el->state,		/* so these don't get changed */
                    prevState = el->prevState;	/* out from under us */
    ActionPtr       a;

    for (i = 0; i < el->numActions; i++)
    {
	a = &el->actions[i];

	if (CHECK_STATE(a->triggerState, state) &&
	    CHECK_STATE(a->triggerPrevState, prevState) &&
	    CHECK_REASON(a->triggerReason, reason))
	    switch (a->action)
	    {
		case AuElementActionChangeState:
		    AuChangeElementState(a->flow, a->elementNum, a->newState,
					 processFlow, reason);
		    break;
		case AuElementActionSendNotify:
		    AuRequestElementNotifyEvent(AuElementNotifyKindState,
						reason, el);
		    break;
		case AuElementActionNoop:
		    break;
	    }
    }
}

/* could be called at audio interrupt level */
AuBool
AuChangeElementState(flow, elementNum, newState, processFlow, reason)
FlowPtr         flow;
AuUint32        elementNum,
                newState;
AuBool          processFlow;
int             reason;
{
    AuBlock         l;
    FlowElementPtr  el;
    int             first,
                    last,
                    i;
    AuBool          stateChange = AuFalse;

    if (elementNum == AuElementAll)
    {
	first = 0;
	last = flow->numElements - 1;
    }
    else
	first = last = elementNum;

    l = AuBlockAudio();
    for (i = first; i <= last; i++)
    {
	el = &flow->elements[i];

	/* see if there's a state change */
	if (newState == el->state)
	    continue;

	/* changing from stopped to paused is not allowed */
	if (newState == AuStatePause && el->state == AuStateStop)
	    continue;

	/*
	 * if we're starting up an element from a stopped state, we may need
	 * to do some stuff
	 */
	if (el->compiled &&
	    newState == AuStateStart && el->state == AuStateStop)
	{
	    el->currentSample = 0;

	    switch (el->raw->type)
	    {
		case AuElementTypeImportWaveForm:
		    {
			el->countSamples =
			    el->raw->importwaveform.num_samples !=
			    AuUnlimitedSamples;

			if (el->countSamples)
			    el->numBytes = el->raw->importwaveform.num_samples *
				auNativeBytesPerSample;

			el->component->frequency =
			    el->raw->importwaveform.frequency *
			    el->component->waveSamples;

			el->read = el->component->data;
			break;
		    }
		case AuElementTypeImportDevice:
		    el->countSamples = el->raw->importdevice.num_samples !=
			AuUnlimitedSamples;

		    if (el->countSamples)
			el->numBytes = el->raw->importdevice.num_samples *
			    el->component->bytesPerSample;
		    break;
		case AuElementTypeExportDevice:
		    el->countSamples = el->raw->exportdevice.num_samples !=
			AuUnlimitedSamples;

		    if (el->countSamples)
			el->numBytes = el->raw->exportdevice.num_samples *
			    el->component->bytesPerSample;
		    break;
		case AuElementTypeImportClient:
		    if (el->component->discard)
		    {
			el->component->read = el->component->write =
			    el->component->data;
			el->component->incoming = 0;
		    }

		    el->component->eof = AuFalse;
		    break;
		case AuElementTypeExportClient:
		    if (el->component->discard)
		    {
			el->component->read = el->component->write =
			    el->component->data;
			el->component->outgoing = 0;
		    }
		    break;
		case AuElementTypeImportBucket:
		    {
			AuUint32        n;

			n = el->component->bytesPerSample;
			el->read = el->component->data +
			    el->raw->importbucket.offset * n;
			el->countSamples = el->raw->importbucket.num_samples !=
			    AuUnlimitedSamples;

			if (el->countSamples)
			    el->numBytes =
				el->raw->importbucket.num_samples * n;
			break;
		    }
		case AuElementTypeExportBucket:
		    {
			AuUint32        n;

			n = el->component->bytesPerSample;
			el->write = el->component->data +
			    el->raw->exportbucket.offset * n;
			el->countSamples = el->raw->exportbucket.num_samples !=
			    AuUnlimitedSamples;

			if (el->countSamples)
			    el->numBytes =
				el->raw->exportbucket.num_samples * n;
			break;
		    }
		case AuElementTypeExportMonitor:
 		    {
 			int             i;
 			AuUint32       *p = (AuUint32 *) el->component->read;
 
 			for (i = 0; i < (int) el->component->numTracks *
 			     auMinibufSamples; i++)
 			    *p++ = 0x7fff8001;

			el->component->write = el->component->read;
  			break;
		    }
	    }
	}

	el->prevState = el->state;
	el->state = newState;
	stateChange = el->stateChange = AuTrue;
    }

    for (i = first; i <= last; i++)
    {
	el = &flow->elements[i];

	if (el->stateChange)
	{
	    el->stateChange = AuFalse;

	    if (el->raw->type == AuElementTypeImportClient &&
		el->component->discard && el->prevState == AuStateStop &&
		el->state == AuStateStart)
	    {
		/*
		 * this forces an empty import to be paused before anything
		 * gets going
		 */
		el->state = AuStatePause;
		el->prevState = AuStateStart;
		checkForTriggeredActions(el, processFlow, AuReasonUnderrun);
	    }
	    else
		checkForTriggeredActions(el, processFlow, reason);
	}
    }

    if (stateChange)
	setFlowState(flow);

    AuUnBlockAudio(l);

    if (processFlow && flow->state != flow->pendingState)
	if (flow->clocked)
	    AuProcessClockedFlows();
	else
	    AuProcessUnclockedFlows();

    return flow->state != flow->pendingState;
}

void
AuProcessStateChanges(numStates, states, flows)
AuUint32        numStates;
auElementState *states;
FlowPtr        *flows;
{
    while (numStates--)
    {
	AuChangeElementState(*flows, states->element_num, states->state,
			     AuFalse, AuReasonUser);
	states++;
	flows++;
    }

    AuProcessUnclockedFlows();
    AuProcessClockedFlows();
}

#define COMMON(x)	((auCommonPart *) a)->x
#define DEVICE(x)	((auDeviceAttributes *) a)->device.x
#define BUCKET(x)	((auBucketAttributes *) a)->bucket.x

int
AuSetComponentAttributes(a, c, varData)
AuUint8        *a,
               *varData;
ComponentPtr    c;
{
    AuUint32        mask,
                    i;

    /* XXX: need to add error checking */

    mask = COMMON(value_mask) & AuCompCommonMasks;
    i = 1;

    while (mask)
    {
	if (mask & 1)
	    switch (i)
	    {
		case AuCompCommonIDMask:
		    c->id = COMMON(id);
		    break;
		case AuCompCommonKindMask:
		    c->kind = COMMON(kind);
		    break;
		case AuCompCommonUseMask:
		    c->use = COMMON(use);
		    break;
		case AuCompCommonFormatMask:
		    c->format = COMMON(format);
		    break;
		case AuCompCommonNumTracksMask:
		    c->numTracks = COMMON(num_tracks);
		    break;
		case AuCompCommonAccessMask:
		    c->access = COMMON(access);
		    break;
		case AuCompCommonDescriptionMask:
		    c->description.type = COMMON(description.type);
		    c->description.len = COMMON(description.len);
		    bcopy(varData, c->description.string, c->description.len);
		    varData += PAD4(c->description.len);
		    break;
	    }

	mask >>= 1;
	i <<= 1;
    }

    /* this assumes that the common masks occupy the lower 16 bits */
    i = (1L << 16);
    mask = COMMON(value_mask);

    switch (c->kind)
    {
	case AuComponentKindPhysicalInput:
	case AuComponentKindPhysicalOutput:
	    mask = (mask & AuCompDeviceMasks) >> 16;

	    while (mask)
	    {
		if (mask & 1)
		    switch (i)
		    {
#if 0
			case AuCompDeviceMinSampleRateMask:
			    c->minSampleRate = DEVICE(min_sample_rate);
			    break;
			case AuCompDeviceMaxSampleRateMask:
			    c->maxSampleRate = DEVICE(max_sample_rate);
			    break;
			case AuCompDeviceLocationMask:
			    c->location = DEVICE(location);
			    break;
#endif
			case AuCompDeviceGainMask:
			    c->gain = DEVICE(gain);
			    break;
			case AuCompDeviceLineModeMask:
			    c->lineMode = DEVICE(line_mode);
			    break;
#if 0
			case AuCompDeviceChildrenMask:
			    break;
#endif
		    }

		mask >>= 1;
		i <<= 1;
	    }
	    break;
	case AuComponentKindBucket:
	    mask = (mask & AuCompBucketMasks) >> 16;

	    while (mask)
	    {
		if (mask & 1)
		    switch (i)
		    {
			case AuCompBucketSampleRateMask:
			    c->sampleRate = BUCKET(sample_rate);
			    break;
			case AuCompBucketNumSamplesMask:
			    c->numSamples = BUCKET(num_samples);
			    break;
		    }

		mask >>= 1;
		i <<= 1;
	    }
	    break;
#ifdef NOTYET
	case AuComponentKindRadio:
	    mask &= AuCompRadioMasks;

	    while (mask)
	    {
		switch (mask & 1)
		{
		}

		mask >>= 1;
		i <<= 1;
	    }
	    break;
#endif				/* NOTYET */
    }

    return AuSuccess;
}

#define COMPARE_COMMON(x, y) 	     if (c->x != COMMON(y)) 	return AuFalse
#define COMPARE_COMMON_MASK(x, y)    if (!(c->x & COMMON(y))) 	return AuFalse
#define COMPARE_BUCKET(x, y) 	     if (c->x != BUCKET(y)) 	return AuFalse
#define COMPARE_DEVICE(x, y) 	     if (c->x != DEVICE(y)) 	return AuFalse
#define COMPARE_DEVICE_MASK(x, y)    if (!(c->x & DEVICE(y))) 	return AuFalse

AuBool
AuMatchAttributes(a, c, varData)
AuUint8        *a,
               *varData;
ComponentPtr    c;
{
    AuUint32        mask,
                    i;

    mask = COMMON(value_mask) & AuCompCommonMasks;
    i = 1;

    while (mask)
    {
	if (mask & 1)
	    switch (i)
	    {
		case AuCompCommonIDMask:
		    COMPARE_COMMON(id, id);
		    break;
		case AuCompCommonKindMask:
		    COMPARE_COMMON(kind, kind);
		    break;
		case AuCompCommonUseMask:
		    COMPARE_COMMON_MASK(use, use);
		    break;
		case AuCompCommonFormatMask:
		    COMPARE_COMMON(format, format);
		    break;
		case AuCompCommonNumTracksMask:
		    COMPARE_COMMON(numTracks, num_tracks);
		    break;
		case AuCompCommonAccessMask:
		    COMPARE_COMMON_MASK(access, access);
		    break;
		case AuCompCommonDescriptionMask:
		    COMPARE_COMMON(description.type, description.type);
		    COMPARE_COMMON(description.len, description.len);

		    if (strncmp(c->description.string, (char *)varData,
				COMMON(description.len)))
			return AuFalse;

		    varData += PAD4(COMMON(description.len));
		    break;
	    }

	mask >>= 1;
	i <<= 1;
    }

    /* this assumes that the common masks occupy the lower 16 bits */
    i = (1L << 16);
    mask = COMMON(value_mask);

    switch (c->kind)
    {
	case AuComponentKindPhysicalInput:
	case AuComponentKindPhysicalOutput:
	    mask = (mask & AuCompDeviceMasks) >> 16;

	    while (mask)
	    {
		if (mask & 1)
		    switch (i)
		    {
			case AuCompDeviceMinSampleRateMask:
			    COMPARE_DEVICE(minSampleRate, min_sample_rate);
			    break;
			case AuCompDeviceMaxSampleRateMask:
			    COMPARE_DEVICE(maxSampleRate, max_sample_rate);
			    break;
			case AuCompDeviceLocationMask:
			    COMPARE_DEVICE_MASK(location, location);
			    break;
			case AuCompDeviceGainMask:
			    if (c->kind == AuComponentKindPhysicalOutput)
				c->gain = AuGetOutputGain();
			    COMPARE_DEVICE(gain, gain);
			    break;
			case AuCompDeviceLineModeMask:
			    COMPARE_DEVICE(lineMode, line_mode);
			    break;
			case AuCompDeviceChildrenMask:
			    COMPARE_DEVICE(numChildren, num_children);
			    if (memcmp(c->children, varData,
				       DEVICE(num_children) *
				       sizeof(AuDeviceID)))
				return AuFalse;

			    varData += DEVICE(num_children) *
				sizeof(AuDeviceID);
			    break;
		    }

		mask >>= 1;
		i <<= 1;
	    }
	    break;
	case AuComponentKindBucket:
	    mask = (mask & AuCompBucketMasks) >> 16;

	    while (mask)
	    {
		if (mask & 1)
		    switch (i)
		    {
			case AuCompBucketSampleRateMask:
			    COMPARE_BUCKET(sampleRate, sample_rate);
			    break;
			case AuCompBucketNumSamplesMask:
			    COMPARE_BUCKET(numSamples, num_samples);
			    break;
		    }

		mask >>= 1;
		i <<= 1;
	    }
	    break;
#ifdef NOTYET
	case AuComponentKindRadio:
	    mask &= AuCompRadioMasks;

	    while (mask)
	    {
		switch (mask & 1)
		{
		}

		mask >>= 1;
		i <<= 1;
	    }
	    break;
#endif				/* NOTYET */
    }

    return AuTrue;
}

AuFixedPoint
AuFixedPointMultiply(m1, m2)
AuFixedPoint    m1,
                m2;
{
    AuBool          negative = AuFalse;
    AuFixedPoint    result;

    if (m1 < 0)
    {
	negative = !negative;
	m1 = -m1;
    }

    if (m2 < 0)
    {
	negative = !negative;
	m2 = -m2;
    }

    /**
     *		 A.B
     *	       x C.D
     *         -----
     *	        D * A
     *	        xx.xx
     * +	   D * B
     *		   xx.xx
     * +      C * AB
     *		xx.xx
     * -----------------
     *		xx.xx
     */

#define A 	AuFixedPointIntegralAddend(m1)
#define B 	AuFixedPointFractionalAddend(m1)
#define C 	AuFixedPointIntegralAddend(m2)
#define D 	AuFixedPointFractionalAddend(m2)
#define	AB	m1

    result = D * A + (D * B >> 16) + C * AB;

#undef A
#undef B
#undef C
#undef D
#undef AB

    if (negative)
	result = -result;

    return result;
}
