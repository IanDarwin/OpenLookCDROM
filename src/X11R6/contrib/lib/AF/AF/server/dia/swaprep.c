/*
 * Copyright 1987, 1990 by Digital Equipment Corporation, Maynard, 
 * Massachusetts,
 * and the Massachusetts Institute of Technology, Cambridge, Massachusetts.
 *
 *                       All Rights Reserved
 */
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

void SwapConnSetup();
void SwapDeviceInfo();


void
Swap32Write(pClient, size /*in bytes*/, pbuf)
     ClientPtr pClient;
     int size /*in bytes*/;
     register CARD32 *pbuf;
{
    int		n, i;

    size >>= 2;
    for(i = 0; i < size; i++)
    /* brackets are mandatory here, because "swapl" macro expands
       to several statements */
    {   
	swapl(&pbuf[i], n);
    }
    (void)WriteToClient(pClient, size << 2, (char *) pbuf);
}

void
CopySwap32Write(pClient, size, pbuf)
    ClientPtr pClient;
    int size;
    CARD32 *pbuf;
{
    int bufsize = size;
    CARD32 *pbufT;
    register CARD32 *from, *to, *fromLast, *toLast;
    CARD32 tmpbuf[1];
    
    /* Allocate as big a buffer as we can... */
    while (!(pbufT = (CARD32 *) ALLOCATE_LOCAL(bufsize)))
    {
        bufsize >>= 1;
	if (bufsize == 4)
	{
	    pbufT = tmpbuf;
	    break;
	}
    }
    
    /* convert lengths from # of bytes to # of CARD32s */
    size >>= 2;
    bufsize >>= 2;

    from = pbuf;
    fromLast = from + size;
    while (from < fromLast) {
	int nbytes;
        to = pbufT;
        toLast = to + min (bufsize, fromLast - from);
        nbytes = (toLast - to) << 2;
        while (to < toLast) {
            /* can't write "cpswapl(*from++, *to++)" because cpswapl is a macro
	       that evaulates its args more than once */
	    cpswapl(*from, *to);
            from++;
            to++;
	    }
	(void)WriteToClient (pClient, nbytes, (char *) pbufT);
	}

    if (pbufT != tmpbuf)
	DEALLOCATE_LOCAL ((char *) pbufT);
}

void
CopySwap16Write(pClient, size, pbuf)
    ClientPtr pClient;
    int size;
    CARD16 *pbuf;
{
    int bufsize = size;
    CARD16 *pbufT;
    register CARD16 *from, *to, *fromLast, *toLast;
    CARD16 tmpbuf[2];
    
    /* Allocate as big a buffer as we can... */
    while (!(pbufT = (CARD16 *) ALLOCATE_LOCAL(bufsize)))
    {
        bufsize >>= 1;
	if (bufsize == 4)
	{
	    pbufT = tmpbuf;
	    break;
	}
    }
    
    /* convert lengths from # of bytes to # of CARD16s */
    size >>= 1;
    bufsize >>= 1;

    from = pbuf;
    fromLast = from + size;
    while (from < fromLast) {
	int nbytes;
        to = pbufT;
        toLast = to + min (bufsize, fromLast - from);
        nbytes = (toLast - to) << 1;
        while (to < toLast) {
            /* can't write "cpswaps(*from++, *to++)" because cpswaps is a macro
	       that evaulates its args more than once */
	    cpswaps(*from, *to);
            from++;
            to++;
	    }
	(void)WriteToClient (pClient, nbytes, (char *) pbufT);
	}

    if (pbufT != tmpbuf)
	DEALLOCATE_LOCAL ((char *) pbufT);
}


/* Extra-small reply */
void
SGenericReply(pClient, size, pRep)
    ClientPtr pClient;
    int size;
    aGenericReply *pRep;
{
    int n;

    swaps(&pRep->sequenceNumber, n);
    (void)WriteToClient(pClient, size, (char *) pRep);
}

void
SRecordSamplesReply(pClient, size, pRep)
    ClientPtr pClient;
    int size;
    aRecordSamplesReply *pRep;
{
    register char n;
    swaps(&pRep->sequenceNumber, n);
    swapl(&pRep->length, n);
    swapl(&pRep->currentTime, n);
    swapl(&pRep->nbytes, n);
    (void)WriteToClient(pClient, size, (char *) pRep);
}

void
SGetTimeReply(pClient, size, pRep)
    ClientPtr pClient;
    int size;
    aGetTimeReply *pRep;
{
    int n;

    swaps(&pRep->sequenceNumber, n);
    swapl(&pRep->length, n);
    swapl(&pRep->time, n);
    (void)WriteToClient(pClient, size, (char *) pRep);
}

void
SQueryPhoneReply(pClient, size, pRep)
    ClientPtr pClient;
    int size;
    aQueryPhoneReply *pRep;
{
    int n;

    swaps(&pRep->sequenceNumber, n);
    swapl(&pRep->length, n);
    (void)WriteToClient(pClient, size, (char *) pRep);
}

void
SQueryGainReply(pClient, size, pRep)
    ClientPtr pClient;
    int size;
    aQueryGainReply *pRep;
{
    int n;

    swaps(&pRep->sequenceNumber, n);
    swapl(&pRep->length, n);
    swapl(&pRep->gain, n);
    swapl(&pRep->minGain, n);
    swapl(&pRep->maxGain, n);
    (void)WriteToClient(pClient, size, (char *) pRep);
}

void
SControlIOReply(pClient, size, pRep)
    ClientPtr pClient;
    int size;
    aControlIOReply *pRep;
{
    int n;

    swaps(&pRep->sequenceNumber, n);
    swapl(&pRep->length, n);
    swapl(&pRep->oldState, n);
    swapl(&pRep->newState, n);
    (void)WriteToClient(pClient, size, (char *) pRep);
}

void
SListExtensionsReply(pClient, size, pRep)
    ClientPtr pClient;
    int size;
    aListExtensionsReply *pRep;
{
    int n;

    swaps(&pRep->sequenceNumber, n);
    swapl(&pRep->length, n);
    (void)WriteToClient(pClient, size, (char *) pRep);
}

void
SListHostsReply(pClient, size, pRep)
    ClientPtr pClient;
    int size;
    aListHostsReply *pRep;
{
    int n;

    swaps(&pRep->sequenceNumber, n);
    swapl(&pRep->length, n);
    swaps(&pRep->nHosts, n);
    (void)WriteToClient(pClient, size, (char *) pRep);
}

void
SInternAtomReply(pClient, size, pRep)
    ClientPtr pClient;
    int size;
    aInternAtomReply *pRep;
{
    int n;

    swaps(&pRep->sequenceNumber, n);
    swapl(&pRep->length, n);
    swapl(&pRep->atom, n);
    (void)WriteToClient(pClient, size, (char *) pRep);
}

void
SGetAtomNameReply(pClient, size, pRep)
    ClientPtr pClient;
    int size;
    aGetAtomNameReply *pRep;
{
    int n;

    swaps(&pRep->sequenceNumber, n);
    swapl(&pRep->length, n);
    swaps(&pRep->nameLength, n);
    (void)WriteToClient(pClient, size, (char *) pRep);
}

void
SGetPropertyReply(pClient, size, pRep)
    ClientPtr pClient;
    int size;
    aGetPropertyReply *pRep;
{
    int n;

    swaps(&pRep->sequenceNumber, n);
    swapl(&pRep->length, n);
    swapl(&pRep->propertyType, n);
    swapl(&pRep->bytesAfter, n);
    swapl(&pRep->nItems, n);
    (void)WriteToClient(pClient, size, (char *) pRep);
}

void
SListPropertiesReply(pClient, size, pRep)
    ClientPtr pClient;
    int size;
    aListPropertiesReply *pRep;
{
    int n;

    swaps(&pRep->sequenceNumber, n);
    swapl(&pRep->length, n);
    swaps(&pRep->nProperties, n);
    (void)WriteToClient(pClient, size, (char *) pRep);
}

void
SErrorEvent(from, to)
    aError *from, *to;
{
    to->type = A_Error;
    to->errorCode = from->errorCode;
    cpswaps(from->sequenceNumber, to->sequenceNumber);
    cpswapl(from->resourceID, to->resourceID);
    cpswaps(from->minorCode, to->minorCode);
    to->majorCode = from->majorCode;
}

void
SPhoneDTMFEvent(from, to)
    aEvent *from, *to;
{
    to->u.u.type = from->u.u.type;
    to->u.u.detail = from->u.u.detail;
    cpswaps(from->u.u.sequenceNumber, to->u.u.sequenceNumber);
    cpswapl(from->u.PhoneDTMF.sec, to->u.PhoneDTMF.sec);
    cpswapl(from->u.PhoneDTMF.usec, to->u.PhoneDTMF.usec);
    cpswapl(from->u.PhoneDTMF.time, to->u.PhoneDTMF.time);
    cpswapl(from->u.PhoneDTMF.device, to->u.PhoneDTMF.device);
    to->u.PhoneDTMF.state = from->u.PhoneDTMF.state;
    to->u.PhoneDTMF.digit = from->u.PhoneDTMF.digit;
}

void
SPhoneEvent(from, to)
    aEvent *from, *to;
{
    to->u.u.type = from->u.u.type;
    to->u.u.detail = from->u.u.detail;
    cpswaps(from->u.u.sequenceNumber, to->u.u.sequenceNumber);
    cpswapl(from->u.PhoneRing.sec, to->u.PhoneRing.sec);
    cpswapl(from->u.PhoneRing.usec, to->u.PhoneRing.usec);
    cpswapl(from->u.PhoneRing.time, to->u.PhoneRing.time);
    cpswapl(from->u.PhoneRing.device, to->u.PhoneRing.device);
    to->u.PhoneRing.state = from->u.PhoneRing.state;
}

void
SDSPEvent(from, to)
    aEvent *from, *to;
{
    to->u.u.type = from->u.u.type;
    to->u.u.detail = from->u.u.detail;
    cpswaps(from->u.u.sequenceNumber, to->u.u.sequenceNumber);
    cpswapl(from->u.DSP.sec, to->u.DSP.sec);
    cpswapl(from->u.DSP.usec, to->u.DSP.usec);
    cpswapl(from->u.DSP.time, to->u.DSP.time);
    cpswapl(from->u.DSP.device, to->u.DSP.device);
    to->u.DSP.hd0 = from->u.DSP.hd0;
    to->u.DSP.hd1 = from->u.DSP.hd1;
    to->u.DSP.hd2 = from->u.DSP.hd2;
    to->u.DSP.hd3 = from->u.DSP.hd3;
    to->u.DSP.hd4 = from->u.DSP.hd4;
    to->u.DSP.hd5 = from->u.DSP.hd5;
    to->u.DSP.hd6 = from->u.DSP.hd6;
    to->u.DSP.hd7 = from->u.DSP.hd7;
}

void
SPropertyEvent(from, to)
    aEvent *from, *to;
{
    to->u.u.type = from->u.u.type;
    to->u.u.detail = from->u.u.detail;
    cpswaps(from->u.u.sequenceNumber, to->u.u.sequenceNumber);
    cpswapl(from->u.property.device, to->u.property.device);
    cpswapl(from->u.property.atom, to->u.property.atom);
    cpswapl(from->u.property.time, to->u.property.time);
    to->u.property.state = from->u.property.state;
}

void
WriteSConnectionInfo(pClient, size, pInfo)
    ClientPtr pClient;
    unsigned long size;
    char *pInfo;
{
    int		i;
    char	*pInfoT, *pInfoTBase;
    aConnSetup	*pConnSetup = (aConnSetup *)pInfo;

    pInfoT = pInfoTBase = (char *) ALLOCATE_LOCAL(size);
    if (!pInfoTBase)
    {
	pClient->noClientException = -1;
	return;
    }
    SwapConnSetup(pConnSetup, (aConnSetup *)pInfoT);
    pInfo += sizeof(aConnSetup);
    pInfoT += sizeof(aConnSetup);

    /* Copy the vendor string */
    i = (pConnSetup->nbytesVendor + 3) & ~3;
    bcopy(pInfo, pInfoT, i);
    pInfo += i;
    pInfoT += i;

    for(i = 0; i < audioDeviceInfo.numDevices; i++)
    {
	SwapDeviceInfo((aDevice *)pInfo, (aDevice *) pInfoT);
	pInfo += sizeof(aDevice);
	pInfoT += sizeof(aDevice);
    }
    (void)WriteToClient(pClient, (int)size, (char *) pInfoTBase);
    DEALLOCATE_LOCAL(pInfoTBase);
}

void
SwapConnSetup(pConnSetup, pConnSetupT)
    aConnSetup *pConnSetup;
    aConnSetup *pConnSetupT;
{
    cpswapl(pConnSetup->release, pConnSetupT->release);
    cpswapl(pConnSetup->ridBase, pConnSetupT->ridBase);
    cpswapl(pConnSetup->ridMask, pConnSetupT->ridMask);
    cpswaps(pConnSetup->nbytesVendor, pConnSetupT->nbytesVendor);
    cpswaps(pConnSetup->maxRequestSize, pConnSetupT->maxRequestSize);
    pConnSetupT->numDevices = pConnSetup->numDevices;
}


void
WriteSConnSetupPrefix(pClient, pcsp)
    ClientPtr pClient;
    aConnSetupPrefix *pcsp;
{
    aConnSetupPrefix	cspT;

    cspT.success = pcsp->success;
    cspT.lengthReason = pcsp->lengthReason;
    cpswaps(pcsp->majorVersion, cspT.majorVersion);
    cpswaps(pcsp->minorVersion, cspT.minorVersion);
    cpswaps(pcsp->length, cspT.length);
    (void)WriteToClient(pClient, sizeof(cspT), (char *) &cspT);
}

void
SLHostsExtend(pClient, size, buf)
    ClientPtr pClient;
    int size;
    char *buf;
{
    char *bufT = buf;
    char *endbuf = buf + size;
    while (bufT < endbuf) {
	aHostEntry *host = (aHostEntry *) bufT;
	int len = host->length;
        int n;
	swaps (&host->length, n);
	bufT += sizeof (aHostEntry) + (((len + 3) >> 2) << 2);
	}
    (void)WriteToClient (pClient, size, buf);
}


void
SwapDeviceInfo(pInfo, pInfoT)
    aDevice *pInfo;
    aDevice *pInfoT;
{
    pInfoT->type = pInfo->type;
    pInfoT->playNchannels = pInfo->playNchannels;
    pInfoT->recordNchannels = pInfo->recordNchannels;
    pInfoT->pad = pInfo->pad;
    cpswapl(pInfo->numberOfInputs, pInfoT->numberOfInputs);
    cpswapl(pInfo->numberOfOutputs,pInfoT->numberOfOutputs);
    cpswapl(pInfo->inputsFromPhone,pInfoT->inputsFromPhone);
    cpswapl(pInfo->outputsToPhone,pInfoT->outputsToPhone);

    cpswapl(pInfo->playSampleFreq,pInfoT->playSampleFreq);
    cpswapl(pInfo->playBufType,pInfoT->playBufType);
    cpswapl(pInfo->playNSamplesBuf,pInfoT->playNSamplesBuf);

    cpswapl(pInfo->recordSampleFreq,pInfoT->recordSampleFreq);
    cpswapl(pInfo->recordBufType,pInfoT->recordBufType);
    cpswapl(pInfo->recordNSamplesBuf,pInfoT->recordNSamplesBuf);
}
