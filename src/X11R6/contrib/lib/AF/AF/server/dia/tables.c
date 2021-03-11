/***********************************************************
Copyright 1987 by Digital Equipment Corporation, Maynard, Massachusetts,
and the Massachusetts Institute of Technology, Cambridge, Massachusetts.

                        All Rights Reserved


DIGITAL DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING
ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL
DIGITAL BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR
ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,
WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION,
ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS
SOFTWARE.

******************************************************************/
/*
 * Copyright 1993 by Digital Equipment Corporation, Maynard, Massachusetts.
 * 
 * Permission to use, copy, modify, distribute, and sell this software and its 
 * documentation for any purpose is hereby granted without fee, provided that 
 * the above copyright notice appear in all copies and that both that 
 * copyright notice and this permission notice appear in supporting 
 * documentation, and that the name of Digital not be used in advertising or 
 * publicity pertaining to distribution of the software without specific, 
 * written prior permission.  Digital makes no representations about the 
 * suitability of this software for any purpose.  It is provided "as is" 
 * without express or implied warranty.
 * 
 * DIGITAL DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING 
 * ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL 
 * DIGITAL BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY 
 * DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN 
 * AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF 
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 */

#include "audio.h"
#include "audioproto.h"
#include "misc.h"
#include "diastruct.h"
#include "audiodev.h"

extern int    ProcInitialConnection(), ProcEstablishConnection();

extern int    ProcBadRequest(), 
    ProcQueryExtension(), ProcListExtensions(),
    ProcChangeHosts(), ProcListHosts(), ProcChangeAccessControl(),
    ProcKillClient(), ProcNoOperation(), ProcDialPhone(), ProcHookSwitch(),
    ProcFlashHook(),
    ProcSelectEvents(), ProcCreateAC(), ProcChangeACAttributes(),
    ProcFreeAC(), ProcPlaySamples(), ProcRecord(), ProcGetTime(),
    ProcQueryExtension(),
    ProcListExtensions(), ProcKillClient(), ProcListHosts(), ProcChangeHosts(),
    ProcChangeAccessControl(), ProcSetInputGain(), ProcSetOutputGain(),
    ProcQueryInputGain(), ProcQueryOutputGain(),
    ProcEnableInput(), ProcEnableOutput(), 
    ProcDisableInput(), ProcDisableOutput(), 
    ProcEnablePassThrough(), ProcDisablePassThrough(),
    ProcSyncConnection(),
    ProcQueryPhone(),
    ProcChangeProperty(), ProcDeleteProperty(),
    ProcGetProperty(), ProcListProperties(), 
    ProcInternAtom(), ProcGetAtomName(),
    ProcEnableGainControl(), ProcDisableGainControl();


extern void NotImplemented();
/* Replies */
extern void SRecordSamplesReply(), SGenericReply(),
  SGetTimeReply(), SQueryPhoneReply(), SQueryGainReply(), SControlIOReply(),
  SListExtensionsReply(), SListHostsReply(), SInternAtomReply(),
  SGetAtomNameReply(), SGetPropertyReply(), SListPropertiesReply();

/* Events */
extern void SErrorEvent(), SPhoneDTMFEvent(), SPhoneEvent(), SDSPEvent(),
  SPropertyEvent();

/* Requests */
extern int SProcSimpleReq(), SProcResourceReq(),
 SProcHookSwitchReq(), SProcFlashHookReq(), SProcDialPhoneReq(),
 SProcSelectEventsReq(), SProcControlIOReq(), SProcChangeACAttributesReq(),
 SProcCreateACReq(), SProcPlaySamplesReq(), SProcRecordSamplesReq(),
 SProcQueryExtensionReq(), SProcChangeHostsReq(),
 SProcChangePropertyReq(), SProcDeletePropertyReq(), SProcGetPropertyReq(),
 SProcInternAtomReq(), SProcSetGainReq();
/*ARGSUSED*/
extern int SProcNoOperationReq();



int (* InitialVector[3]) () =
{
    0,
    ProcInitialConnection,
    ProcEstablishConnection
};

int (* ProcVector[256]) () =

{
    ProcBadRequest,			/* 0 */
    ProcBadRequest,
    ProcSelectEvents,
    ProcDialPhone,			/* 3 */
    ProcCreateAC,			/* 4 */
    ProcChangeACAttributes,		/* 5 */
    ProcFreeAC,
    ProcPlaySamples,
    ProcRecord,
    ProcGetTime,
    ProcBadRequest,			/* 10 */
    ProcBadRequest,
    ProcQueryExtension,
    ProcListExtensions,
    ProcKillClient,
    ProcHookSwitch,			/* 15 */
    ProcChangeAccessControl,
    ProcChangeHosts,
    ProcListHosts,
    ProcSetInputGain,
    ProcSetOutputGain,			/* 20 */
    ProcQueryInputGain,
    ProcQueryOutputGain,
    ProcEnableInput,
    ProcEnableOutput,
    ProcDisableInput,			/* 25 */
    ProcDisableOutput,
    ProcSyncConnection,
    ProcQueryPhone,
    ProcBadRequest,			/* was record recent */
    ProcEnablePassThrough,		/* 30 */
    ProcDisablePassThrough,
    ProcFlashHook,
    ProcChangeProperty,
    ProcDeleteProperty,
    ProcGetProperty,			/* 35 */
    ProcListProperties,
    ProcInternAtom,
    ProcGetAtomName,
    ProcEnableGainControl,
    ProcDisableGainControl,		/* 40 */
    ProcBadRequest,
    ProcBadRequest,
    ProcBadRequest,
    ProcBadRequest,
    ProcBadRequest,			/* 45 */
    ProcBadRequest,
    ProcBadRequest,
    ProcBadRequest,
    ProcBadRequest,
    ProcBadRequest,			/* 50 */
    ProcBadRequest,
    ProcBadRequest,
    ProcBadRequest,
    ProcBadRequest,
    ProcBadRequest,			/* 55 */
    ProcBadRequest,
    ProcBadRequest,
    ProcBadRequest,
    ProcBadRequest,
    ProcBadRequest,			/* 60 */
    ProcBadRequest,
    ProcBadRequest,
    ProcBadRequest,
    ProcBadRequest,
    ProcBadRequest,			/* 65 */
    ProcBadRequest,
    ProcBadRequest,
    ProcBadRequest,
    ProcBadRequest,
    ProcBadRequest,			/* 70 */
    ProcBadRequest,
    ProcBadRequest,
    ProcBadRequest,
    ProcBadRequest,
    ProcBadRequest,			/* 75 */
    ProcBadRequest,
    ProcBadRequest,
    ProcBadRequest,
    ProcBadRequest,
    ProcBadRequest,			/* 80 */
    ProcBadRequest,
    ProcBadRequest,
    ProcBadRequest,
    ProcBadRequest,
    ProcBadRequest,			/* 85 */
    ProcBadRequest,
    ProcBadRequest,
    ProcBadRequest,
    ProcBadRequest,
    ProcBadRequest,			/* 90 */
    ProcBadRequest,
    ProcBadRequest,
    ProcBadRequest,
    ProcBadRequest,
    ProcBadRequest,			/* 95 */
    ProcBadRequest,
    ProcBadRequest,
    ProcBadRequest,
    ProcBadRequest,
    ProcBadRequest,			/* 100 */
    ProcBadRequest,
    ProcBadRequest,
    ProcBadRequest,
    ProcBadRequest,
    ProcBadRequest,			/* 105 */
    ProcBadRequest,
    ProcBadRequest,
    ProcBadRequest,
    ProcBadRequest,
    ProcBadRequest,			/* 110 */
    ProcBadRequest,
    ProcBadRequest,
    ProcBadRequest,
    ProcBadRequest,
    ProcBadRequest,			/* 115 */
    ProcBadRequest,
    ProcBadRequest,
    ProcBadRequest,
    ProcBadRequest,
    0,					/* 120 */
    0,
    0,
    0,
    0,
    0,					/* 125 */
    0,
    ProcNoOperation    			/* 127 */
};

int (* SwappedProcVector[256]) () =
{
    ProcBadRequest,
    ProcBadRequest,
    SProcSelectEventsReq,		/* 2 */
    SProcDialPhoneReq,
    SProcCreateACReq,			
    SProcChangeACAttributesReq,		/* 5 */
    SProcResourceReq,			
    SProcPlaySamplesReq,
    SProcRecordSamplesReq,			
    SProcResourceReq,			
    ProcBadRequest,			/* 10 */			
    ProcBadRequest,
    SProcQueryExtensionReq,
    SProcSimpleReq,
    SProcResourceReq,			/* 14 KillClient */
    SProcHookSwitchReq,			/* 15 */
    SProcSimpleReq,			/* 16 ChangeAccessControl */
    SProcChangeHostsReq,		/* 17 ChangeHosts */
    SProcSimpleReq,			/* 18 ListHosts */
    SProcSetGainReq,
    SProcSetGainReq,			/* 20 */
    SProcResourceReq,			/* 21 QueryInputGain */
    SProcResourceReq,			/* 22 QueryOutputGainReq */
    SProcControlIOReq,
    SProcControlIOReq,
    SProcControlIOReq,			/* 25 */
    SProcControlIOReq,
    SProcSimpleReq,
    SProcResourceReq,
    ProcBadRequest,			/* 29 doesn't exist */
    SProcControlIOReq,			/* 30 EnablePassThrough */
    SProcControlIOReq,			/* 31 DisablePassThrough */
    SProcFlashHookReq,
    SProcChangePropertyReq,
    SProcDeletePropertyReq,
    SProcGetPropertyReq,		/* 35 */
    SProcResourceReq,
    SProcInternAtomReq,
    SProcResourceReq,
    SProcControlIOReq,			/* 39 EnableGainControl */
    SProcControlIOReq,			/* 40 DisableGainControl */
    ProcBadRequest,
    ProcBadRequest,
    ProcBadRequest,			
    ProcBadRequest,			
    ProcBadRequest,			/* 45 */
    ProcBadRequest,			
    ProcBadRequest, 			
    ProcBadRequest,			
    ProcBadRequest,
    ProcBadRequest,			/* 50 */
    ProcBadRequest,
    ProcBadRequest,			
    ProcBadRequest,
    ProcBadRequest,			
    ProcBadRequest,			/* 55 */
    ProcBadRequest,
    ProcBadRequest,
    ProcBadRequest,
    ProcBadRequest,
    ProcBadRequest,			/* 60 */
    ProcBadRequest,
    ProcBadRequest,
    ProcBadRequest,
    ProcBadRequest,			
    ProcBadRequest,			/* 65 */
    ProcBadRequest,			
    ProcBadRequest,			
    ProcBadRequest,			
    ProcBadRequest,
    ProcBadRequest,			/* 70 */
    ProcBadRequest,			
    ProcBadRequest,
    ProcBadRequest,
    ProcBadRequest,
    ProcBadRequest,			/* 75 */
    ProcBadRequest,
    ProcBadRequest,
    ProcBadRequest,
    ProcBadRequest,			
    ProcBadRequest,			/* 80 */
    ProcBadRequest,			
    ProcBadRequest,			
    ProcBadRequest, 			
    ProcBadRequest,
    ProcBadRequest,			/* 85 */
    ProcBadRequest,
    ProcBadRequest,
    ProcBadRequest,
    ProcBadRequest,
    ProcBadRequest,			/* 90 */
    ProcBadRequest,
    ProcBadRequest,
    ProcBadRequest,
    ProcBadRequest,
    ProcBadRequest,			/* 95 */
    ProcBadRequest,
    ProcBadRequest,
    ProcBadRequest,
    ProcBadRequest,			
    ProcBadRequest,			/* 100 */
    ProcBadRequest,			
    ProcBadRequest,
    ProcBadRequest,			
    ProcBadRequest,			
    ProcBadRequest,			/* 105 */
    ProcBadRequest,			
    ProcBadRequest,
    ProcBadRequest,			
    ProcBadRequest,
    ProcBadRequest,			/* 110 */
    ProcBadRequest,			
    ProcBadRequest,			
    ProcBadRequest,			
    ProcBadRequest,
    ProcBadRequest,			/* 115 */
    ProcBadRequest,			
    ProcBadRequest,			
    ProcBadRequest,			
    ProcBadRequest,			
    0,					/* 120 */
    0,
    0,
    0,
    0,
    0,					/* 125 */
    0,
    SProcNoOperationReq
};

void (* EventSwapVector[128]) () =
{
    SErrorEvent,
    NotImplemented,
    SPhoneEvent,
    SPhoneDTMFEvent,
    SPhoneEvent,
    SDSPEvent,				/* 5 */
    SPhoneEvent,
    SPropertyEvent,
    NotImplemented,
    NotImplemented,
    NotImplemented,			/* 10 */
    NotImplemented,
    NotImplemented,
    NotImplemented,
    NotImplemented,
    NotImplemented,			/* 15 */
    NotImplemented,
    NotImplemented,
    NotImplemented,
    NotImplemented,
    NotImplemented,			/* 20 */
    NotImplemented,
    NotImplemented,
    NotImplemented,
    NotImplemented,
    NotImplemented,			/* 25 */
    NotImplemented,
    NotImplemented,
    NotImplemented,
    NotImplemented,
    NotImplemented,			/* 30 */
    NotImplemented,
    NotImplemented,
    NotImplemented,
    NotImplemented,
};


void (* ReplySwapVector[256]) () =
{
    NotImplemented,
    NotImplemented,
    NotImplemented,
    NotImplemented,
    NotImplemented,
    NotImplemented,			/* 5 */
    NotImplemented,
    SGetTimeReply,
    SRecordSamplesReply,
    SGetTimeReply,
    NotImplemented,			/* 10 */
    NotImplemented,
    NotImplemented,
    NotImplemented,
    NotImplemented,
    NotImplemented,			/* 15 */
    NotImplemented,
    NotImplemented,
    SListHostsReply,
    NotImplemented,
    NotImplemented,			/* 20 */
    SQueryGainReply,
    SQueryGainReply,
    SControlIOReply,
    SControlIOReply,
    SControlIOReply,			/* 25 */
    SControlIOReply,
    SGenericReply,
    SQueryPhoneReply,
    NotImplemented,
    SControlIOReply,			/* 30 */
    SControlIOReply,
    NotImplemented,
    NotImplemented,
    NotImplemented,
    SGetPropertyReply,			/* 35 */
    SListPropertiesReply,
    SInternAtomReply,
    SGetAtomNameReply,
    NotImplemented,
    NotImplemented,			/* 40 */
    NotImplemented,
    NotImplemented,
    NotImplemented,
    NotImplemented,
    NotImplemented,			/* 45 */
    NotImplemented,
    NotImplemented,
    NotImplemented,
    NotImplemented,
    NotImplemented,			/* 50 */
    NotImplemented,
    NotImplemented,
    NotImplemented,
    NotImplemented,
    NotImplemented,			/* 55 */
    NotImplemented,
    NotImplemented,
    NotImplemented,
    NotImplemented,
    NotImplemented,			/* 60 */
    NotImplemented,
    NotImplemented,
    NotImplemented,
    NotImplemented,
    NotImplemented,			/* 65 */
    NotImplemented,
    NotImplemented,
    NotImplemented,
    NotImplemented,
    NotImplemented,			/* 70 */
    NotImplemented,
    NotImplemented,
    NotImplemented,
    NotImplemented,
    NotImplemented,			/* 75 */
    NotImplemented,
    NotImplemented,
    NotImplemented,
    NotImplemented,
    NotImplemented,			/* 80 */
    NotImplemented,
    NotImplemented,
    NotImplemented,
    NotImplemented,
    NotImplemented,			/* 85 */
    NotImplemented,
    NotImplemented,
    NotImplemented,
    NotImplemented,
    NotImplemented,			/* 90 */
    NotImplemented,
    NotImplemented,
    NotImplemented,
    NotImplemented,
    NotImplemented,			/* 95 */
    NotImplemented,
    NotImplemented,
    NotImplemented,
    NotImplemented,
    NotImplemented,			/* 100 */
    NotImplemented,
    NotImplemented,
    NotImplemented,
    NotImplemented,
    NotImplemented,			/* 105 */
    NotImplemented,
    NotImplemented,
    NotImplemented,
    NotImplemented,
    NotImplemented,			/* 110 */
    NotImplemented,
    NotImplemented,
    NotImplemented,
    NotImplemented,
    NotImplemented,			/* 115 */
    NotImplemented,
    NotImplemented,
    NotImplemented,
    NotImplemented,			/* 119 */
    NotImplemented,			/* 120 */
    NotImplemented,			/* 121 */
    NotImplemented,			/* 122 */
    NotImplemented,			/* 123 */
    NotImplemented,			/* 124 */
    NotImplemented,			/* 125 */
    NotImplemented,			/* 126 */
    NotImplemented,			/* NoOperation */
    NotImplemented
};
