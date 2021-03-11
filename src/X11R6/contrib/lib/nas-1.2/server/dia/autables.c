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
 * $NCDId: @(#)autables.c,v 1.2 1993/08/28 00:35:21 lemke Exp $
 */

#define ProcAuSetBucketAttributes	ProcAuNotImplemented
#define ProcAuGetFlowAttributes		ProcAuNotImplemented
#define ProcAuSetFlowAttributes		ProcAuNotImplemented
#define ProcAuGetElementParameters	ProcAuNotImplemented
#define ProcAuCreateRadio		ProcAuNotImplemented
#define ProcAuDestroyRadio		ProcAuNotImplemented
#define ProcAuListRadios		ProcAuNotImplemented
#define ProcAuGetRadioAttributes	ProcAuNotImplemented
#define ProcAuSetRadioAttributes	ProcAuNotImplemented
#define ProcAuGrabComponent		ProcAuNotImplemented
#define ProcAuUngrabComponent		ProcAuNotImplemented
#define ProcAuSendEvent			ProcAuNotImplemented
#define ProcAuGetAllowedUsers		ProcAuNotImplemented
#define ProcAuSetAllowedUsers		ProcAuNotImplemented
#define ProcAuListExtensions		ProcAuNotImplemented
#define ProcAuQueryExtension		ProcAuNotImplemented

/* audio dispatch, reply, event vectors */

extern int      ProcAuNotImplemented(),		/* 0 */
                ProcAuListDevices(),		/* 1 */
                ProcAuGetDeviceAttributes(),	/* 2 */
                ProcAuSetDeviceAttributes(),	/* 3 */
                ProcAuCreateBucket(),		/* 4 */
                ProcAuDestroyBucket(),		/* 5 */
                ProcAuListBuckets(),		/* 6 */
                ProcAuGetBucketAttributes(),	/* 7 */
                ProcAuSetBucketAttributes(),	/* 8 */
                ProcAuCreateRadio(),		/* 9 */
                ProcAuDestroyRadio(),		/* 10 */
                ProcAuListRadios(),		/* 11 */
                ProcAuGetRadioAttributes(),	/* 12 */
                ProcAuSetRadioAttributes(),	/* 13 */
                ProcAuCreateFlow(),		/* 14 */
                ProcAuDestroyFlow(),		/* 15 */
                ProcAuGetFlowAttributes(),	/* 16 */
                ProcAuSetFlowAttributes(),	/* 17 */
                ProcAuGetElements(),		/* 18 */
                ProcAuSetElements(),		/* 19 */
                ProcAuGetElementStates(),	/* 20 */
                ProcAuSetElementStates(),	/* 21 */
                ProcAuGetElementParameters(),	/* 22 */
                ProcAuSetElementParameters(),	/* 23 */
                ProcAuWriteElement(),		/* 24 */
                ProcAuReadElement(),		/* 25 */
                ProcAuGrabComponent(),		/* 26 */
                ProcAuUngrabComponent(),	/* 27 */
                ProcAuSendEvent(),		/* 28 */
                ProcAuGetAllowedUsers(),	/* 29 */
                ProcAuSetAllowedUsers(),	/* 30 */
                ProcAuListExtensions(),		/* 31 */
                ProcAuQueryExtension(),		/* 32 */
                ProcAuGetCloseDownMode(),	/* 33 */
                ProcAuSetCloseDownMode(),	/* 34 */
                ProcAuKillClient(),		/* 35 */
                ProcAuGetServerTime(),		/* 36 */
                ProcAuNoOperation();		/* 37 */

extern int      SProcAuResourceReq(),
                SProcAuSimpleReq(),
                SProcAuSetDeviceAttributes(),
                SProcAuCreateBucket(),
                SProcAuListDevices(),
                SProcAuListBuckets(),
                SProcAuSetElements(),
                SProcAuSetElementStates(),
                SProcAuSetElementParameters(),
                SProcAuWriteElement(),
                SProcAuReadElement();

extern void	NotImplemented(),
		SAuGetDeviceAttributesReply(),
		SAuListDevicesReply(),
		SAuGetElementsReply(),
		SAuGetElementStatesReply(),
		SAuListBucketsReply(),
		SAuGetBucketAttributesReply(),
		SAuReadElementReply(),
		SAuGetServerTimeReply(),
		SAuGetCloseDownModeReply();

extern void	SAuErrorEvent(),
		SAuElementNotifyEvent(),
		SAuMonitorNotifyEvent();

int             (*AuProcVector[256]) () =
{
    ProcAuNotImplemented,			/* 0 */
    ProcAuListDevices,				/* 1 */
    ProcAuGetDeviceAttributes,			/* 2 */
    ProcAuSetDeviceAttributes,			/* 3 */
    ProcAuCreateBucket,				/* 4 */
    ProcAuDestroyBucket,			/* 5 */
    ProcAuListBuckets,				/* 6 */
    ProcAuGetBucketAttributes,			/* 7 */
    ProcAuSetBucketAttributes,			/* 8 */
    ProcAuCreateRadio,				/* 9 */
    ProcAuDestroyRadio,				/* 10 */
    ProcAuListRadios,				/* 11 */
    ProcAuGetRadioAttributes,			/* 12 */
    ProcAuSetRadioAttributes,			/* 13 */
    ProcAuCreateFlow,				/* 14 */
    ProcAuDestroyFlow,				/* 15 */
    ProcAuGetFlowAttributes,			/* 16 */
    ProcAuSetFlowAttributes,			/* 17 */
    ProcAuGetElements,				/* 18 */
    ProcAuSetElements,				/* 19 */
    ProcAuGetElementStates,			/* 20 */
    ProcAuSetElementStates,			/* 21 */
    ProcAuGetElementParameters,			/* 22 */
    ProcAuSetElementParameters,			/* 23 */
    ProcAuWriteElement,				/* 24 */
    ProcAuReadElement,				/* 25 */
    ProcAuGrabComponent,			/* 26 */
    ProcAuUngrabComponent,			/* 27 */
    ProcAuSendEvent,				/* 28 */
    ProcAuGetAllowedUsers,			/* 29 */
    ProcAuSetAllowedUsers,			/* 30 */
    ProcAuListExtensions,			/* 31 */
    ProcAuQueryExtension,			/* 32 */
    ProcAuGetCloseDownMode,			/* 33 */
    ProcAuSetCloseDownMode,			/* 34 */
    ProcAuKillClient,				/* 35 */
    ProcAuGetServerTime,			/* 36 */
    ProcAuNoOperation				/* 37 */
};

int             (*AuSwappedProcVector[256]) () =
{
    ProcAuNotImplemented,			/* 0 */
    SProcAuListDevices,				/* 1 */
    SProcAuResourceReq,				/* 2 */
    SProcAuSetDeviceAttributes,			/* 3 */
    SProcAuCreateBucket,			/* 4 */
    SProcAuResourceReq,				/* 5 */
    SProcAuListBuckets,				/* 6 */
    SProcAuResourceReq,				/* 7 */
    ProcAuSetBucketAttributes,			/* 8 */
    ProcAuCreateRadio,				/* 9 */
    ProcAuDestroyRadio,				/* 10 */
    ProcAuListRadios,				/* 11 */
    ProcAuGetRadioAttributes,			/* 12 */
    ProcAuSetRadioAttributes,			/* 13 */
    SProcAuResourceReq,				/* 14 */
    SProcAuResourceReq,				/* 15 */
    ProcAuGetFlowAttributes,			/* 16 */
    ProcAuSetFlowAttributes,			/* 17 */
    SProcAuResourceReq,				/* 18 */
    SProcAuSetElements,				/* 19 */
    SProcAuSetElementStates,			/* 20 */
    SProcAuSetElementStates,			/* 21 */
    ProcAuGetElementParameters,			/* 22 */
    SProcAuSetElementParameters,		/* 23 */
    SProcAuWriteElement,			/* 24 */
    SProcAuReadElement,				/* 25 */
    ProcAuGrabComponent,			/* 26 */
    ProcAuUngrabComponent,			/* 27 */
    ProcAuSendEvent,				/* 28 */
    ProcAuGetAllowedUsers,			/* 29 */
    ProcAuSetAllowedUsers,			/* 30 */
    ProcAuListExtensions,			/* 31 */
    ProcAuQueryExtension,			/* 32 */
    SProcAuSimpleReq,				/* 33 */
    SProcAuSimpleReq,				/* 34 */
    SProcAuResourceReq,				/* 35 */
    ProcAuGetServerTime,			/* 36 */
    ProcAuNoOperation				/* 37 */
};

void            (*AuEventSwapVector[256]) () =
{
    SAuErrorEvent,		/* XXX */
    NotImplemented,
    SAuElementNotifyEvent,
    NotImplemented,
    SAuMonitorNotifyEvent,
};

void             (*AuReplySwapVector[256]) () =
{
    NotImplemented,				/* 0 */
    SAuListDevicesReply,			/* 1 */
    SAuGetDeviceAttributesReply,		/* 2 */
    NotImplemented,				/* 3 */
    NotImplemented,				/* 4 */
    NotImplemented,				/* 5 */
    SAuListBucketsReply,			/* 6 */
    SAuGetBucketAttributesReply,		/* 7 */
    NotImplemented,				/* 8 */
    NotImplemented,				/* 9 */
    NotImplemented,				/* 10 */
    NotImplemented,				/* 11 */
    NotImplemented,				/* 12 */
    NotImplemented,				/* 13 */
    NotImplemented,				/* 14 */
    NotImplemented,				/* 15 */
    NotImplemented,				/* 16 */
    NotImplemented,				/* 17 */
    SAuGetElementsReply,			/* 18 */
    NotImplemented,				/* 19 */
    SAuGetElementStatesReply,			/* 20 */
    NotImplemented,				/* 21 */
    NotImplemented,				/* 22 */
    NotImplemented,				/* 23 */
    NotImplemented,				/* 24 */
    SAuReadElementReply,			/* 25 */
    NotImplemented,				/* 26 */
    NotImplemented,				/* 27 */
    NotImplemented,				/* 28 */
    NotImplemented,				/* 29 */
    NotImplemented,				/* 30 */
    NotImplemented,				/* 31 */
    NotImplemented,				/* 32 */
    SAuGetCloseDownModeReply,			/* 33 */
    NotImplemented,				/* 34 */
    NotImplemented,				/* 35 */
    SAuGetServerTimeReply,			/* 36 */
    NotImplemented				/* 37 */
};
