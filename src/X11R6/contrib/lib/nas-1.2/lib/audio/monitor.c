/**
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
 * $NCDId: @(#)monitor.c,v 1.1 1994/02/02 18:37:48 greg Exp $
 */

/* monitor a device */

#include "Alibint.h"

typedef struct
{
    AuFlowID        flow;
    AuPointer       callback_data;
    void            (*callback) (
#if NeedNestedPrototypes
				 AuServer *, AuEventHandlerRec *,
				 AuEvent *, AuPointer
#endif
    );
}               MonitorDeviceRec, *MonitorDevicePtr;

static AuBool
EventHandler(aud, ev, handler)
AuServer       *aud;
AuEvent        *ev;
AuEventHandlerRec *handler;
{
    MonitorDevicePtr priv = (MonitorDevicePtr) handler->data;

    switch (ev->type)
    {
      case AuEventTypeMonitorNotify:
	if (priv->callback)
	    (*priv->callback) (aud, handler, ev, priv->callback_data);
	break;
      case AuEventTypeElementNotify:
	{
	    AuElementNotifyEvent *event = (AuElementNotifyEvent *) ev;

	    if (event->kind == AuElementNotifyKindState &&
		event->cur_state == AuStateStop)
	    {
		if (priv->callback)
		    (*priv->callback) (aud, handler, ev,
				       priv->callback_data);

		AuUnregisterEventHandler(aud, handler);
		AuReleaseScratchFlow(aud, priv->flow, NULL);
		Aufree(priv);
	    }
	    break;
	}
    }

    return AuTrue;
}

/* sampling rate of the monitor (in Hz) */
int             AuMonitorRate = 10;
/* format of the monitor data */
int             AuMonitorFormat = AuFormatLinearSigned16MSB;

AuEventHandlerRec *
AuMonitorDevice(aud, rate, in_device, out_device, volume, callback,
		callback_data, pflow, pmultelem, pmonelem, ret_status)
AuServer       *aud;
int             rate;
AuDeviceID      in_device,
                out_device;
AuFixedPoint    volume;
void            (*callback) (
#if NeedFunctionPrototypes
			                     AuServer *, AuEventHandlerRec *,
			                     AuEvent *, AuPointer
#endif
);
AuPointer       callback_data;
AuFlowID       *pflow;
int            *pmultelem,
               *pmonelem;
AuStatus       *ret_status;
{
    AuElement       elements[4];
    AuElementState  states[1];
    AuElementAction actions[1];
    AuEventHandlerRec *handler;
    MonitorDevicePtr priv;
    int             numTracks,
                    i;

    if (!(priv = (MonitorDevicePtr) Aumalloc(sizeof(MonitorDeviceRec))))
	return NULL;

    priv->callback = callback;
    priv->callback_data = callback_data;

    if ((priv->flow = AuGetScratchFlow(aud, NULL)) == AuNone)
    {
	Aufree(priv);
	return NULL;
    }

    if (out_device == AuNone)
    {
	AuDeviceAttributes *d = NULL;

	/* look up the input device attributes */
	for (i = 0; i < AuServerNumDevices(aud); i++)
	{
	    if (AuDeviceIdentifier(AuServerDevice(aud, i)) == in_device)
	    {
		d = AuServerDevice(aud, i);
		break;
	    }
	}

	if (!d)
	{
	    Aufree(priv);
	    return NULL;
	}

	numTracks = AuDeviceNumTracks(d);

	/* look for a matching output device */
	for (i = 0; i < AuServerNumDevices(aud); i++)
	    if ((AuDeviceKind(AuServerDevice(aud, i)) ==
		 AuComponentKindPhysicalOutput) &&
		AuDeviceNumTracks(AuServerDevice(aud, i)) == numTracks)
	    {
		out_device = AuDeviceIdentifier(AuServerDevice(aud, i));
		break;
	    }

	if (out_device == AuNone)
	{
	    Aufree(priv);
	    return NULL;
	}
    }
    else
    {
	AuDeviceAttributes *d;

	if (!(d = AuGetDeviceAttributes(aud, out_device, ret_status)))
	{
	    Aufree(priv);
	    return NULL;
	}

	numTracks = AuDeviceNumTracks(d);
	AuFreeDeviceAttributes(aud, 1, d);
    }

    AuMakeSendNotifyAction(&actions[0], AuStateStop, AuStateAny, AuReasonAny);

    AuMakeElementImportDevice(&elements[0], rate, in_device, AuUnlimitedSamples,
			      1, actions);
    AuMakeElementMultiplyConstant(&elements[1], 0, volume);
    AuMakeElementExportDevice(&elements[2], 1, out_device, rate,
			      AuUnlimitedSamples, 0, NULL);

    if (pmonelem)
    {
	i = 4;
	*pmonelem = 3;
	AuMakeElementExportMonitor(&elements[3], 0, AuMonitorRate,
				   AuMonitorFormat, numTracks);
    }
    else
	i = 3;

    /* set up the flow */
    AuSetElements(aud, priv->flow, AuTrue, i, elements, ret_status);

    if (!(handler = AuRegisterEventHandler(aud, AuEventHandlerIDMask,
					0, priv->flow, EventHandler, priv)))
    {
	AuReleaseScratchFlow(aud, priv->flow, ret_status);
	Aufree(priv);
	return NULL;
    }

    /* start up the components */
    AuMakeElementState(&states[0], priv->flow, AuElementAll, AuStateStart);
    AuSetElementStates(aud, 1, states, ret_status);

    if (pflow)
	*pflow = priv->flow;

    if (pmultelem)
	*pmultelem = 1;

    return handler;
}
