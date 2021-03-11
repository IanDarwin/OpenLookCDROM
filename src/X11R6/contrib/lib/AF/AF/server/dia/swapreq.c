/************************************************************
Copyright 1990 by Digital Equipment Corporation, Maynard, Massachusetts,
and the Massachusetts Institute of Technology, Cambridge, Massachusetts.

                        All Rights Reserved

********************************************************/
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
#define NEED_EVENTS
#include "audioproto.h"
#include "misc.h"
#include "diastruct.h"

extern int (* ProcVector[256]) ();
extern void (* EventSwapVector[128]) ();  /* for SendEvent */

/* Byte swap a list of longs */

void
SwapLongs (CARD32 *list, unsigned long count)
{
	register int n;

	while (count >= 8) {
	    swapl(list+0, n);
	    swapl(list+1, n);
	    swapl(list+2, n);
	    swapl(list+3, n);
	    swapl(list+4, n);
	    swapl(list+5, n);
	    swapl(list+6, n);
	    swapl(list+7, n);
	    list += 8;
	    count -= 8;
	}
	if (count != 0) {
	    do {
		swapl(list, n);
		list++;
	    } while (--count != 0);
	}
}

/* Byte swap a list of shorts */

void
SwapShorts (CARD16 *list, unsigned long count)
{
	register int n;

	while (count >= 16) {
	    swaps(list+0, n);
	    swaps(list+1, n);
	    swaps(list+2, n);
	    swaps(list+3, n);
	    swaps(list+4, n);
	    swaps(list+5, n);
	    swaps(list+6, n);
	    swaps(list+7, n);
	    swaps(list+8, n);
	    swaps(list+9, n);
	    swaps(list+10, n);
	    swaps(list+11, n);
	    swaps(list+12, n);
	    swaps(list+13, n);
	    swaps(list+14, n);
	    swaps(list+15, n);
	    list += 16;
	    count -= 16;
	}
	if (count != 0) {
	    do {
		swaps(list, n);
		list++;
	    } while (--count != 0);
	}
}

/* The following is used for all requests that have
   no fields to be swapped (except "length") */
int
SProcSimpleReq(client)
    register ClientPtr client;
{
    register char n;

    REQUEST(aReq);
    swaps(&stuff->length, n);
    return(*ProcVector[stuff->reqType])(client);
}

/* The following is used for all requests that have
   only a single 32-bit field to be swapped, coming
   right after the "length" field */
int
SProcResourceReq(client)
    register ClientPtr client;
{
    register char n;

    REQUEST(aResourceReq);
    swaps(&stuff->length, n);
    swapl(&stuff->id, n);
    return(*ProcVector[stuff->reqType])(client);
}


int
SProcHookSwitchReq(client)
    register ClientPtr client;
{
    register char n;

    REQUEST(aHookSwitchReq);
    swaps(&stuff->length, n);
    swapl(&stuff->ac, n);
    swapl(&stuff->onoff, n);
    return(*ProcVector[stuff->reqType])(client);
}

int
SProcSetGainReq(client)
    register ClientPtr client;
{
    register char n;

    REQUEST(aSetGainReq);
    swaps(&stuff->length, n);
    swapl(&stuff->ac, n);
    swapl(&stuff->gain, n);
    return(*ProcVector[stuff->reqType])(client);
}

int
SProcFlashHookReq(client)
    register ClientPtr client;
{
    register char n;

    REQUEST(aFlashHookReq);
    swaps(&stuff->length, n);
    swapl(&stuff->ac, n);
    swapl(&stuff->duration, n);
    return(*ProcVector[stuff->reqType])(client);
}

int
SProcDialPhoneReq(client)
    register ClientPtr client;
{
    register char n;

    REQUEST(aDialPhoneReq);
    swaps(&stuff->length, n);
    swapl(&stuff->ac, n);
    swaps(&stuff->nbytes, n);
    return(*ProcVector[stuff->reqType])(client);
}

int
SProcSelectEventsReq(client)
    register ClientPtr client;
{
    register char n;

    REQUEST(aSelectEventsReq);
    swaps(&stuff->length, n);
    swapl(&stuff->ac, n);
    swapl(&stuff->mask, n);
    return(*ProcVector[stuff->reqType])(client);
}

int
SProcControlIOReq(client)
    register ClientPtr client;
{
    register char n;

    REQUEST(aControlIOReq);
    swaps(&stuff->length, n);
    swapl(&stuff->ac, n);
    swapl(&stuff->mask, n);
    return(*ProcVector[stuff->reqType])(client);
}

int
SProcChangeACAttributesReq(client)
    register ClientPtr client;
{
    register char n;

    REQUEST(aChangeACAttributesReq);
    swaps(&stuff->length, n);
    REQUEST_AT_LEAST_SIZE(aChangeACAttributesReq);
    swapl(&stuff->ac, n);
    swapl(&stuff->mask, n);
    SwapRestL(stuff);
    return(*ProcVector[stuff->reqType])(client);
}

int
SProcCreateACReq(client)
    register ClientPtr client;
{
    register char n;

    REQUEST(aCreateACReq);
    swaps(&stuff->length, n);
    REQUEST_AT_LEAST_SIZE(aCreateACReq);
    swapl(&stuff->ac, n);
    swapl(&stuff->mask, n);
    swapl(&stuff->device, n);
    SwapRestL(stuff);
    return(*ProcVector[stuff->reqType])(client);
}

int
SProcPlaySamplesReq(client)
    register ClientPtr client;
{
    register char n;

    REQUEST(aPlaySamplesReq);
    swaps(&stuff->length, n);
    REQUEST_AT_LEAST_SIZE(aPlaySamplesReq);
    swapl(&stuff->ac, n);
    swapl(&stuff->startTime, n);
    swapl(&stuff->nbytes, n);
    swaps(&stuff->nchannels, n);
    swaps(&stuff->sampleType, n);
    return(*ProcVector[stuff->reqType])(client);
}

int
SProcRecordSamplesReq(client)
    register ClientPtr client;
{
    register char n;

    REQUEST(aRecordSamplesReq);
    swaps(&stuff->length, n);
    REQUEST_AT_LEAST_SIZE(aRecordSamplesReq);
    swapl(&stuff->ac, n);
    swapl(&stuff->startTime, n);
    swapl(&stuff->nbytes, n);
    swaps(&stuff->nchannels, n);
    swaps(&stuff->sampleType, n);
    return(*ProcVector[stuff->reqType])(client);
}


int
SProcQueryExtensionReq(client)
    register ClientPtr client;
{
    register char n;
    REQUEST(aQueryExtensionReq);
    swaps(&stuff->length, n);
    REQUEST_AT_LEAST_SIZE(aQueryExtensionReq);
    swaps(&stuff->nbytes, n);
    return((*ProcVector[stuff->reqType])(client));
}

int
SProcChangeHostsReq(client)
    ClientPtr client;
{
    register char n;

    REQUEST(aChangeHostsReq);
    swaps(&stuff->length, n);
    REQUEST_AT_LEAST_SIZE(aChangeHostsReq);
    swaps(&stuff->hostLength, n);
    return((*ProcVector[stuff->reqType])(client));
}

SProcChangePropertyReq(client)
    register ClientPtr client;
{
    register char n;
    REQUEST(aChangePropertyReq);
    swaps(&stuff->length, n);
    REQUEST_AT_LEAST_SIZE(aChangePropertyReq);
    swapl(&stuff->ac, n);
    swapl(&stuff->property, n);
    swapl(&stuff->type, n);
    swapl(&stuff->nUnits, n);
    switch ( stuff->format ) {
        case 8 :
            break;
        case 16:
            SwapRestS(stuff);
            break;
        case 32:
            SwapRestL(stuff);
            break;
        }
    return((* ProcVector[stuff->reqType])(client));
}


int
SProcDeletePropertyReq(client)
    ClientPtr client;
{
    register char n;
    REQUEST(aDeletePropertyReq);
    swaps(&stuff->length, n);
    REQUEST_SIZE_MATCH(aDeletePropertyReq);
    swapl(&stuff->ac, n);
    swapl(&stuff->property, n);
    return((* ProcVector[stuff->reqType])(client));

}

int
SProcGetPropertyReq(client)
    ClientPtr client;
{
    register char n;
    REQUEST(aGetPropertyReq);
    swaps(&stuff->length, n);
    REQUEST_SIZE_MATCH(aGetPropertyReq);
    swapl(&stuff->ac, n);
    swapl(&stuff->property, n);
    swapl(&stuff->type, n);
    swapl(&stuff->longOffset, n);
    swapl(&stuff->longLength, n);
    return((* ProcVector[stuff->reqType])(client));
}

int
SProcInternAtomReq(client)
    register ClientPtr client;
{
    register char n;
    REQUEST(aInternAtomReq);
    swaps(&stuff->length, n);
    REQUEST_AT_LEAST_SIZE(aInternAtomReq);
    swaps(&stuff->nbytes, n);
    return((* ProcVector[stuff->reqType])(client));
}

/*ARGSUSED*/
int
SProcNoOperationReq(client)
    ClientPtr client;
{
    /* noop -- don't do anything */
    return(ASuccess);
}


SwapConnClientPrefix(pCCP)
    aConnClientPrefix *pCCP;
{
    register char n;

    swaps(&pCCP->majorVersion, n);
    swaps(&pCCP->minorVersion, n);
    swaps(&pCCP->nbytesAuthProto, n);
    swaps(&pCCP->nbytesAuthString, n);
}

