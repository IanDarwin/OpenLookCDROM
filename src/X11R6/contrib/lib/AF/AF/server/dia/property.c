/***********************************************************
Copyright 1987 by Digital Equipment Corporation, Maynard, Massachusetts,
and the Massachusetts Institute of Technology, Cambridge, Massachusetts.

                        All Rights Reserved

Permission to use, copy, modify, and distribute this software and its 
documentation for any purpose and without fee is hereby granted, 
provided that the above copyright notice appear in all copies and that
both that copyright notice and this permission notice appear in 
supporting documentation, and that the names of Digital or MIT not be
used in advertising or publicity pertaining to distribution of the
software without specific, written prior permission.  
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
#define NEED_REPLIES
#define NEED_EVENTS
#include "audioproto.h"
#include "audiodev.h"
#include "acstruct.h"
#include "propstr.h"
#include "diastruct.h"

extern void (*ReplySwapVector[]) ();
extern void CopySwap16Write(), CopySwap32Write(), Swap32Write();
extern int WriteToClient();

/*****************************************************************
 * Property Stuff
 *
 *    ChangeProperty, DeleteProperty, GetProperties,
 *    ListProperties
 *
 *   Properties below to devices.  A allocate slots each time
 *   a property is added.  No fancy searching done.
 *
 *****************************************************************/


int 
ProcChangeProperty(client)
    ClientPtr client;
{	      
    AC *pAC;
    AudioDevicePtr pDev;
    char format, mode;
    unsigned long len;
    int sizeInBytes;
    int totalSize;
    int err;
    REQUEST(aChangePropertyReq);

    REQUEST_AT_LEAST_SIZE(aChangePropertyReq);
    UpdateCurrentTime();
    format = stuff->format;
    mode = stuff->mode;
    if ((mode != APropModeReplace) && (mode != APropModeAppend) &&
	(mode != APropModePrepend))
    {
	client->errorValue = mode;
	return ABadValue;
    }
    if ((format != 8) && (format != 16) && (format != 32))
    {
	client->errorValue = format;
        return ABadValue;
    }
    len = stuff->nUnits;
    sizeInBytes = format>>3;
    totalSize = len * sizeInBytes;
    REQUEST_FIXED_SIZE(aChangePropertyReq, totalSize);

    VERIFY_AC(pAC, stuff->ac, client);
    pDev = pAC->aDev;

    if (!ValidAtom(stuff->property))
    {
	client->errorValue = stuff->property;
	return(ABadAtom);
    }
    if (!ValidAtom(stuff->type))
    {
	client->errorValue = stuff->type;
	return(ABadAtom);
    }

    err = ChangeDeviceProperty(pDev, stuff->property, stuff->type, (int)format,
			       (int)mode, len, (pointer)&stuff[1], TRUE);
    if (err != ASuccess)
	return err;
    else
	return client->noClientException;
}

int
ChangeDeviceProperty(pDev, property, type, format, mode, len, value, sendevent)
    AudioDevicePtr	pDev;
    AAtom	property, type;
    int		format, mode;
    unsigned long len;
    pointer	value;
    ABool	sendevent;
{
    PropertyPtr pProp;
    aEvent event;
    int sizeInBytes;
    int totalSize;
    pointer data;

    sizeInBytes = format>>3;
    totalSize = len * sizeInBytes;

    /* first see if property already exists */

    pProp = pDev->userProps;
    while (pProp)
    {
	if (pProp->propertyName == property)
	    break;
	pProp = pProp->next;
    }
    if (!pProp)   /* just add to list */
    {
        pProp = (PropertyPtr)xalloc(sizeof(PropertyRec));
	if (!pProp)
	    return(ABadAlloc);
        data = (pointer)xalloc(totalSize);
	if (!data && len)
	{
	    xfree(pProp);
	    return(ABadAlloc);
	}
        pProp->propertyName = property;
        pProp->type = type;
        pProp->format = format;
        pProp->data = data;
	if (len)
	    bcopy((char *)value, (char *)data, totalSize);
	pProp->size = len;
        pProp->next = pDev->userProps;
        pDev->userProps = pProp;
    }
    else
    {
	/* To append or prepend to a property the request format and type
		must match those of the already defined property.  The
		existing format and type are irrelevant when using the mode
		"APropModeReplace" since they will be written over. */

        if ((format != pProp->format) && (mode != APropModeReplace))
	    return(ABadMatch);
        if ((pProp->type != type) && (mode != APropModeReplace))
            return(ABadMatch);
        if (mode == APropModeReplace) 
        {
	    if (totalSize != pProp->size * (pProp->format >> 3))
	    {
	    	data = (pointer)xrealloc(pProp->data, totalSize);
	    	if (!data && len)
		    return(ABadAlloc);
            	pProp->data = data;
	    }
	    if (len)
		bcopy((char *)value, (char *)pProp->data, totalSize);
	    pProp->size = len;
    	    pProp->type = type;
	    pProp->format = format;
	}
	else if (len == 0)
	{
	    /* do nothing */
	}
        else if (mode == APropModeAppend)
        {
	    data = (pointer)xrealloc(pProp->data,
				     sizeInBytes * (len + pProp->size));
	    if (!data)
		return(ABadAlloc);
            pProp->data = data;
	    bcopy((char *)value,
		  &((char *)data)[pProp->size * sizeInBytes], 
		  totalSize);
            pProp->size += len;
	}
        else if (mode == APropModePrepend)
        {
            data = (pointer)xalloc(sizeInBytes * (len + pProp->size));
	    if (!data)
		return(ABadAlloc);
	    bcopy((char *)pProp->data, &((char *)data)[totalSize], 
		  (int)(pProp->size * sizeInBytes));
            bcopy((char *)value, (char *)data, totalSize);
	    xfree(pProp->data);
            pProp->data = data;
            pProp->size += len;
	}
    }
    event.u.u.type = APropertyEvent;
    event.u.property.device = pDev->index;
    event.u.property.state = APropertyNewValue;
    event.u.property.atom = pProp->propertyName;
    event.u.property.time = (*(pDev->GetTime))(pDev);
    FilterEvents(&event, pDev->index);

    return(ASuccess);
}

DeleteProperty(pDev, propName)
    AudioDevicePtr pDev;
    AAtom propName;
{
    PropertyPtr pProp, prevProp;
    aEvent event;

    if (!(pProp = pDev->userProps))
	return(ASuccess);
    prevProp = (PropertyPtr)NULL;
    while (pProp)
    {
	if (pProp->propertyName == propName)
	    break;
        prevProp = pProp;
	pProp = pProp->next;
    }
    if (pProp) 
    {		    
        if (prevProp == (PropertyPtr)NULL)      /* takes care of head */
        {
            pDev->userProps = pProp->next;
        }
	else
        {
            prevProp->next = pProp->next;
        }
	event.u.u.type = APropertyEvent;
	event.u.property.device = pDev->index;
	event.u.property.state = APropertyDelete;
        event.u.property.atom = pProp->propertyName;
	event.u.property.time = (*(pDev->GetTime))(pDev);
	FilterEvents(&event, pDev->index);
	xfree(pProp->data);
        xfree(pProp);
    }
    return(ASuccess);
}

DeleteAllDeviceProperties(pDev)
    AudioDevicePtr pDev;
{
    PropertyPtr pProp, pNextProp;
    aEvent event;
    pProp = pDev->userProps;
    while (pProp)
    {
	event.u.u.type = APropertyEvent;
	event.u.property.device = pDev->index;
	event.u.property.state = APropertyDelete;
	event.u.property.atom = pProp->propertyName;
	event.u.property.time = currentTime.milliseconds;
	FilterEvents(&event, pDev->index);
	pNextProp = pProp->next;
        xfree(pProp->data);
        xfree(pProp);
	pProp = pNextProp;
    }
}

/*****************
 * GetProperty
 *    If type Any is specified, returns the property from the specified
 *    window regardless of its type.  If a type is specified, returns the
 *    property only if its type equals the specified type.
 *    If delete is True and a property is returned, the property is also
 *    deleted from the device and a PropertyEvent event is generated on the
 *    window.
 *****************/

int
ProcGetProperty(client)
    ClientPtr client;
{
    AC *pAC;
    PropertyPtr pProp, prevProp;
    unsigned long n, len, ind;
    AudioDevicePtr pDev;
    aGetPropertyReply reply;
    REQUEST(aGetPropertyReq);

    REQUEST_SIZE_MATCH(aGetPropertyReq);
    if (stuff->c_delete)
	UpdateCurrentTime();
 
    VERIFY_AC(pAC, stuff->ac, client);
    pDev = pAC->aDev;

    if (pDev)
    {
	if (!ValidAtom(stuff->property))
	{
	    client->errorValue = stuff->property;
	    return(ABadAtom);
	}
	if ((stuff->c_delete != aTrue) && (stuff->c_delete != aFalse))
	{
	    client->errorValue = stuff->c_delete;
	    return(ABadValue);
	}
	if ((stuff->type == AAnyPropertyType) || ValidAtom(stuff->type))
	{
	    pProp = pDev->userProps;
            prevProp = (PropertyPtr)NULL;
            while (pProp)
            {
	        if (pProp->propertyName == stuff->property) 
	            break;
		prevProp = pProp;
		pProp = pProp->next;
            }
	    reply.type = A_Reply;
	    reply.sequenceNumber = client->sequence;
            if (pProp) 
            {

		/* If the request type and actual type don't match. Return the
		property information, but not the data. */

                if ((stuff->type != pProp->type) &&
		    (stuff->type != AAnyPropertyType))
		{
		    reply.bytesAfter = pProp->size;
		    reply.format = pProp->format;
		    reply.length = 0;
		    reply.nItems = 0;
		    reply.propertyType = pProp->type;
		    WriteReplyToClient(client, sizeof(aGenericReply), &reply);
		    return(ASuccess);
		}

	    /*
             *  Return type, format, value to client
             */
		n = (pProp->format/8) * pProp->size; /* size (bytes) of prop */
		ind = stuff->longOffset << 2;        

               /* If longOffset is invalid such that it causes "len" to
                        be negative, it's a value error. */

		if (n < ind)
		{
		    client->errorValue = stuff->longOffset;
		    return ABadValue;
		}

		len = min(n - ind, 4 * stuff->longLength);

		reply.bytesAfter = n - (ind + len);
		reply.format = pProp->format;
		reply.length = (len + 3) >> 2;
		reply.nItems = len / (pProp->format / 8 );
		reply.propertyType = pProp->type;

                if (stuff->c_delete && (reply.bytesAfter == 0))
                { /* send the event */
		    aEvent event;
		
		    event.u.u.type = APropertyEvent;
		    event.u.property.device = pDev->index;
		    event.u.property.state = APropertyDelete;
		    event.u.property.atom = pProp->propertyName;
		    event.u.property.time = (*(pDev->GetTime))(pDev);
		    FilterEvents(&event, pDev->index);
		}

		WriteReplyToClient(client, sizeof(aGenericReply), &reply);
		if (len)
		{
		    switch (reply.format) {
		    case 32: client->pSwapReplyFunc = CopySwap32Write; break;
		    case 16: client->pSwapReplyFunc = CopySwap16Write; break;
		    default: client->pSwapReplyFunc = (void (*) ())WriteToClient; break;
		    }
		    WriteSwappedDataToClient(client, len, pProp->data + ind);
		}

                if (stuff->c_delete && (reply.bytesAfter == 0))
                { /* delete the Property */
                    if (prevProp == (PropertyPtr)NULL) /* takes care of head */
		    {
                        pDev->userProps = pProp->next;
		    }
	            else
                        prevProp->next = pProp->next;
		    xfree(pProp->data);
                    xfree(pProp);
		}
	    }
            else 
	    {   
                reply.nItems = 0;
		reply.length = 0;
		reply.bytesAfter = 0;
		reply.propertyType = ANone;
		reply.format = 0;
		WriteReplyToClient(client, sizeof(aGenericReply), &reply);
	    }
            return(client->noClientException);

	}
        else
	{
	    client->errorValue = stuff->type;
            return(ABadAtom);
	}
    }
    else            
        return (ABadDevice); 
}

int
ProcListProperties(client)
    ClientPtr client;
{
    AC *pAC;
    AAtom *pAtoms, *temppAtoms;
    aListPropertiesReply xlpr;
    int	numProps = 0;
    AudioDevicePtr pDev;
    PropertyPtr pProp;
    REQUEST(aResourceReq);

    REQUEST_SIZE_MATCH(aResourceReq);

    VERIFY_AC(pAC, stuff->id, client);
    pDev = pAC->aDev;

    if (!pDev)
        return(ABadDevice);

    pProp = pDev->userProps;
    while (pProp)
    {        
        pProp = pProp->next;
	numProps++;
    }
    if (numProps)
        if(!(pAtoms = (AAtom *)ALLOCATE_LOCAL(numProps * sizeof(AAtom))))
            return(ABadAlloc);

    xlpr.type = A_Reply;
    xlpr.nProperties = numProps;
    xlpr.length = (numProps * sizeof(AAtom)) >> 2;
    xlpr.sequenceNumber = client->sequence;
    pProp = pDev->userProps;
    temppAtoms = pAtoms;
    while (pProp)
    {
	*temppAtoms++ = pProp->propertyName;
	pProp = pProp->next;
    }
    WriteReplyToClient(client, sizeof(aGenericReply), &xlpr);
    if (numProps)
    {
        client->pSwapReplyFunc = Swap32Write;
        WriteSwappedDataToClient(client, numProps * sizeof(AAtom), pAtoms);
        DEALLOCATE_LOCAL(pAtoms);
    }
    return(client->noClientException);
}
