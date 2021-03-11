/***********************************************************
Copyright 1987, 1990 by Digital Equipment Corporation, Maynard, Massachusetts,
and the Massachusetts Institute of Technology, Cambridge, Massachusetts.

                        All Rights Reserved

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
#include "Amd.h"
#include "audioproto.h"
#include "property.h"
#include "propstr.h"
#include "misc.h"
#include "resource.h"
#include "acstruct.h"
#include "audiodev.h"
#include "dia.h"
#include "diastruct.h"
#include "acfuncs.h"
#include "AFUtils.h"

#ifdef DEBUG
extern void NotImplemented();
#endif


/* DoChangeAC(pAC, mask, pval)
   mask is a set of bits indicating which values to change.
   pval contains an appropriate value for each mask.
   fPointer is true if the values for embedded resource fields
   are pointers instead of IDs.  
   if there is an error, the value is marked as changed 
   anyway, which is probably wrong, but infrequent.

NOTE:
	all values sent over the protocol for ChangeAC requests are
32 bits long
*/

int
DoChangeAC(
    register ClientPtr  client,
    register AC 	*pAC,
    register BITS32	mask,
    AID			*pval
	)
{
    register BITS32 	index;
    register int 	error = 0;
    BITS32		maskQ;

    pAC->serialNumber |= AC_CHANGE_SERIAL_BIT;

    maskQ = mask;	/* save these for when we walk the ACque */
    while (mask && !error) 
    {
	index = (BITS32) lowbit (mask);
	mask &= ~index;
	pAC->stateChanges |= index;
	switch (index)
	{
	    case ACRecordGain:
		pAC->recordGain = *pval++;
		if (pAC->recordGain < -100) pAC->recordGain = -100;
		else if (pAC->recordGain > 100) pAC->recordGain = 100;
	        pAC->recGainMul = AFdBtoLin((double) pAC->recordGain);
		break;
	    case ACPlayGain:
		pAC->playGain = *pval++;
		if (pAC->playGain < -100) pAC->playGain = -100;
		else if (pAC->playGain > 100) pAC->playGain = 100;
	        pAC->playGainMul = AFdBtoLin((double) pAC->playGain);
		break;
	    case ACStartTimeout:
		pAC->startTimeout = *pval++;
		break;
	    case ACEndSilence:
		pAC->endSilence = *pval++;
		break;
	    case ACPreemption:
		if (((CARD8)*pval == Mix) || ((CARD8)*pval == Preempt))
			pAC->preempt = (CARD8)*pval;
		else
		{
			client->errorValue = (CARD8)*pval;
			error = ABadValue;
		}
		pval++;
		break;
	    case ACEncodingType:
		pAC->recType = pAC->playType = *pval++;
		pAC->compressState.fromType = pAC->aDev->recBufType;
		pAC->uncompressState.toType = pAC->aDev->playBufType;
		pAC->compressState.toType = pAC->recType;
		pAC->uncompressState.fromType = pAC->playType;
		if (!AFConvert_InitConvert(&pAC->compressState))
		  error = ABadValue;
		else
		  (pAC->compressState.convertInitProc)(&pAC->compressState, 
						       ALLOC);
		if (!AFConvert_InitConvert(&pAC->uncompressState))
		  error = ABadValue;
		else
		  (pAC->uncompressState.convertInitProc)(&pAC->uncompressState,
							 ALLOC);
		break;
	    default:
		client->errorValue = maskQ;
		error = ABadValue;
		pval++;
		break;
	}
    }
    (*pAC->funcs->ChangeAC)(client, pAC, maskQ);
    return error;
}

/* Publically defined entry to ChangeAC.  Just calls DoChangeAC and tells
 * it that all of the entries are constants or IDs */
int
ChangeAC(
    register ClientPtr  client,
    register AC 	*pAC,
    register BITS32	mask,
    AID			*pval
	)
{
    return (DoChangeAC(client, pAC, mask, pval));
}

/* CreateAC(client, aDev, mask, pval, pStatus)
   creates a default AC for the given AC, using mask to fill
   in any non-default values.
   Returns a pointer to the new AC on success, NULL otherwise.
   returns status of non-default fields in pStatus
*/

static ACPtr
AllocateAC(void)
{
    ACPtr pAC;
    pAC = (ACPtr)xalloc(sizeof (struct _AC));
    return pAC;
}

ACPtr
CreateAC(
    register ClientPtr client,
    AudioDevicePtr	aDev,
    BITS32	mask,
    AID		*pval,
    int		*pStatus
	)
{
    register ACPtr pAC;
    pAC = AllocateAC();
    if (!pAC)
    {
	*pStatus = ABadAlloc;
	return (ACPtr)NULL;
    }

    pAC->aDev = aDev;
    pAC->serialNumber = AC_CHANGE_SERIAL_BIT;
    /* init values in AC */
    pAC->playGain = 0;
    pAC->playGainMul = 1.0;
    pAC->recordGain = 0;
    pAC->recGainMul = 1.0;
    pAC->startTimeout = InfiniteTimeout;
    pAC->endSilence = InfiniteTimeout;
    pAC->preempt = Mix;
    pAC->playType = aDev->playBufType;
    pAC->recType = aDev->recBufType;

    pAC->compressState.fromType = aDev->recBufType;
    pAC->compressState.toType = aDev->recBufType;
    pAC->compressState.next = 0;
    pAC->compressState.buffer = (unsigned char *)
      xalloc(aDev->playSampleFreq * 
	      AF_sample_sizes[aDev->recBufType].bytes_per_unit *
	      aDev->recNchannels);
    pAC->compressState.convertInitProc = NULL;
    pAC->compressState.convertProc = NULL;
    pAC->compressState.state = NULL;
    pAC->compressState.nChannels = aDev->recNchannels;

    pAC->uncompressState.fromType = aDev->playBufType;
    pAC->uncompressState.toType = aDev->playBufType;
    pAC->uncompressState.next = 0;
    pAC->uncompressState.buffer = (unsigned char *)
      xalloc(aDev->playSampleFreq * 
	      AF_sample_sizes[aDev->playBufType].bytes_per_unit *
	      aDev->playNchannels);
    pAC->uncompressState.convertInitProc = NULL;
    pAC->uncompressState.convertProc = NULL;
    pAC->uncompressState.state = NULL;
    pAC->uncompressState.nChannels = aDev->playNchannels;

    if (!(*pAC->aDev->CreateAC)(pAC))
	*pStatus = ABadAlloc;
    else if (mask)
        *pStatus = ChangeAC(client, pAC, mask, pval);
    else
	*pStatus = ASuccess;
    if (*pStatus != ASuccess)
    {
	FreeAC(pAC, (AContext)0);
	pAC = (ACPtr)NULL;
    }

    return (pAC);
}



/*
 * FreeAC 
 *   does the diA part of freeing the characteristics in the AC 
 */

/*ARGSUSED*/
int
FreeAC(ACPtr pAC, AContext gid)
{
    (*pAC->funcs->DestroyAC) (pAC);
    if (pAC->compressState.buffer != NULL) 
      xfree(pAC->compressState.buffer);
    if (pAC->compressState.state != NULL) 
      (pAC->compressState.convertInitProc)(&pAC->compressState, DEALLOC);
    if (pAC->uncompressState.buffer != NULL) 
      xfree(pAC->uncompressState.buffer);
    if (pAC->uncompressState.state != NULL) 
      (pAC->uncompressState.convertInitProc)(&pAC->uncompressState, DEALLOC);
    xfree(pAC);
    return(ASuccess);
}

void
SetACMask(ACPtr pAC, AMask selectMask, AMask newDataMask)
{
    pAC->stateChanges = (~selectMask & pAC->stateChanges) |
		        (selectMask & newDataMask);
    if (selectMask & newDataMask)
        pAC->serialNumber |= AC_CHANGE_SERIAL_BIT;        
}
