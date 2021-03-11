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
 * $NCDId: @(#)dispatch.c,v 1.6 1994/04/07 20:53:27 greg Exp $
 */
/************************************************************
Some portions derived from: 

Copyright 1987, 1989 by Digital Equipment Corporation, Maynard, Massachusetts,
and the Massachusetts Institute of Technology, Cambridge, Massachusetts.

                        All Rights Reserved

Permission to use, copy, modify, and distribute this software and its 
documentation for any purpose and without fee is hereby granted, 
provided that the above copyright notice appear in all copies and that
both that copyright notice and this permission notice appear in 
supporting documentation, and that the names of Digital or MIT not be
used in advertising or publicity pertaining to distribution of the
software without specific, written prior permission.  

DIGITAL DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING
ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL
DIGITAL BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR
ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,
WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION,
ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS
SOFTWARE.

********************************************************/

#include        <audio/audio.h>
#include        <audio/Aproto.h>
#include "dixstruct.h"
#include "osstruct.h"
#include "opaque.h"
#include "servermd.h"

extern void     ProcessAudioEvents(), SendAuErrorToClient(),
                SwapConnClientPrefix(), ResetCurrentRequest(), WriteToClient();
extern int      CompareTimeStamps(), WaitForSomething(), AuSendInitResponse();
extern Bool     ClientIsAsleep(), ClientSignal();

#define mskcnt ((MAXCLIENTS + 31) / 32)
#define BITMASK(i) (1 << ((i) & 31))
#define MASKIDX(i) ((i) >> 5)
#define MASKWORD(buf, i) buf[MASKIDX(i)]
#define BITSET(buf, i) MASKWORD(buf, i) |= BITMASK(i)
#define BITCLEAR(buf, i) MASKWORD(buf, i) &= ~BITMASK(i)
#define GETBIT(buf, i) (MASKWORD(buf, i) & BITMASK(i))

extern void NotImplemented();
extern Bool InitClientResources();

extern int (* InitialVector[3]) ();
extern void Swap32Write(), WriteSConnectionInfo();
extern void WriteSConnSetupPrefix();
extern char *ClientAuthorized();
extern Bool InsertFakeRequest();
static void KillAllClients();
extern void ProcessWorkQueue();

extern int (* AuProcVector[256]) ();
extern int (* AuSwappedProcVector[256]) ();
extern int (* AuEventSwapVector[256]) ();
extern int (* AuReplySwapVector[256]) ();

static int nextFreeClientID; /* always MIN free client ID */

static int	nClients;	/* number active clients */

char dispatchException = 0;
char isItTimeToYield;

/* Various of the DIX function interfaces were not designed to allow
 * the client->errorValue to be set on BadValue and other errors.
 * Rather than changing interfaces and breaking untold code we introduce
 * a new global that dispatch can use.
 */
AuID clientErrorValue;   /* XXX this is a kludge */

void
UpdateCurrentTime()
{
    TimeStamp systime;

    /* To avoid time running backwards, we must call GetTimeInMillis before
     * calling ProcessInputEvents.
     */
    systime.months = currentTime.months;
    systime.milliseconds = GetTimeInMillis();
    if (systime.milliseconds < currentTime.milliseconds)
	systime.months++;
    ProcessAudioEvents();
    if (CompareTimeStamps(systime, currentTime) == LATER)
	currentTime = systime;
}

/* Like UpdateCurrentTime, but can't call ProcessInputEvents */
void
UpdateCurrentTimeIf()
{
    TimeStamp systime;

    systime.months = currentTime.months;
    systime.milliseconds = GetTimeInMillis();
    if (systime.milliseconds < currentTime.milliseconds)
	systime.months++;

    currentTime = systime;
}

#define MAJOROP ((auReq *)client->requestBuffer)->reqType

void
Dispatch()
{
    register int        *clientReady;     /* array of request ready clients */
    register int	result;
    register ClientPtr	client;
    register int	nready;

    nextFreeClientID = 1;
    nClients = 0;

    clientReady = (int *) ALLOCATE_LOCAL(sizeof(int) * MaxClients);
    if (!clientReady)
	return;

    while (!dispatchException)
    {
	FlushIfCriticalOutputPending();

	ProcessAudioEvents();

	nready = WaitForSomething(clientReady);

       /***************** 
	*  Handle events in round robin fashion, doing input between 
	*  each round 
	*****************/

	while (!dispatchException && (--nready >= 0))
	{
	    client = clients[clientReady[nready]];
	    if (! client)
	    {
		/* KillClient can cause this to happen */
		continue;
	    }
	    isItTimeToYield = FALSE;
 
	    while (!isItTimeToYield)
	    {
		FlushIfCriticalOutputPending();

		ProcessAudioEvents();
	   
		/* now, finally, deal with client requests */

	        result = ReadRequestFromClient(client);
	        if (result <= 0) 
	        {
		    if (result < 0)
			CloseDownClient(client);
		    break;
	        }

		client->sequence++;
#ifdef DEBUG
		if (client->requestLogIndex == MAX_REQUEST_LOG)
		    client->requestLogIndex = 0;
		client->requestLog[client->requestLogIndex] = MAJOROP;
		client->requestLogIndex++;
#endif
		if (result > (MAX_BIG_REQUEST_SIZE << 2))
		    result = AuBadLength;
		else
		    result = (* client->requestVector[MAJOROP])(client);
	    
		if (result != AuSuccess) 
		{
		    if (client->noClientException != AuSuccess)
                        CloseDownClient(client);
                    else
		        SendAuErrorToClient(client, MAJOROP,
					  0, client->errorValue, result);
		    break;
	        }
	    }
	    FlushAllOutput();
	}
    }
    KillAllClients();
    DEALLOCATE_LOCAL(clientReady);
    dispatchException &= ~DE_RESET;
}

#undef MAJOROP

/*ARGSUSED*/
int
ProcBadRequest(client)
    ClientPtr client;
{
    return (AuBadRequest);
}

extern int Ones();

/**********************
 * CloseDownClient
 *
 *  Client can either mark his resources destroy or retain.  If retained and
 *  then killed again, the client is really destroyed.
 *********************/

Bool terminateAtReset = FALSE;

void
CloseDownClient(client)
    register ClientPtr client;
{
    if (!client->clientGone)
    {
	if (client->closeDownMode == AuCloseDownDestroy)
	{
	    client->clientGone = TRUE;  /* so events aren't sent to client */
	    CloseDownConnection(client);
	    FreeClientResources(client);
	    if (ClientIsAsleep (client))
		ClientSignal (client);
	    if (client->index < nextFreeClientID)
		nextFreeClientID = client->index;
	    clients[client->index] = NullClient;
	    if ((client->requestVector != InitialVector) &&
		(--nClients == 0))
	    {
		if (terminateAtReset)
		    dispatchException |= DE_TERMINATE;
		else
		    dispatchException |= DE_RESET;
	    }
	    xfree(client);
	}
	else
	{
	    client->clientGone = TRUE;
	    CloseDownConnection(client);
	    --nClients;
	}
    }
    else
    {
	/* really kill resources this time */
        FreeClientResources(client);
	if (ClientIsAsleep (client))
	    ClientSignal (client);
	if (client->index < nextFreeClientID)
	    nextFreeClientID = client->index;
	clients[client->index] = NullClient;
        xfree(client);
    }

    while (!clients[currentMaxClients-1])
      currentMaxClients--;
}

static void
KillAllClients()
{
    int i;
    for (i=1; i<currentMaxClients; i++)
        if (clients[i])
            CloseDownClient(clients[i]);     
}

/*********************
 * CloseDownRetainedResources
 *
 *    Find all clients that are gone and have terminated in RetainTemporary 
 *    and  destroy their resources.
 *********************/
void
CloseDownRetainedResources()
{
    register int i;
    register ClientPtr client;

    for (i=1; i<currentMaxClients; i++)
    {
        client = clients[i];
        if (client && (client->closeDownMode == AuCloseDownRetainTemporary)
	    && (client->clientGone))
	    CloseDownClient(client);
    }
}

void InitClient(client, i, ospriv)
    ClientPtr client;
    int i;
    pointer ospriv;
{
    client->index = i;
    client->sequence = 0; 
    client->clientAsMask = ((AuMask)i) << CLIENTOFFSET;
    client->clientGone = FALSE;
    client->noClientException = AuSuccess;
#ifdef DEBUG
    client->requestLogIndex = 0;
#endif
    client->requestVector = InitialVector;
    client->osPrivate = ospriv;
    client->swapped = FALSE;
    client->big_requests = FALSE;
    client->closeDownMode = AuCloseDownDestroy;
}

/************************
 * int NextAvailableClient(ospriv)
 *
 * OS dependent portion can't assign client id's because of CloseDownModes.
 * Returns NULL if there are no free clients.
 *************************/

ClientPtr
NextAvailableClient(ospriv)
    pointer ospriv;
{
    register int i;
    register ClientPtr client;
    auReq data;

    i = nextFreeClientID;
    if (i == MAXCLIENTS)
	return (ClientPtr)NULL;
    clients[i] = client = (ClientPtr)xalloc(sizeof(ClientRec));
    if (!client)
	return (ClientPtr)NULL;
    InitClient(client, i, ospriv);
    if (!InitClientResources(client))
    {
	xfree(client);
	return (ClientPtr)NULL;
    }
    data.reqType = 1;
    data.length = (sz_auReq + sz_auConnClientPrefix) >> 2;
    if (!InsertFakeRequest(client, (char *)&data, sz_auReq))
    {
	FreeClientResources(client);
	xfree(client);
	return (ClientPtr)NULL;
    }
    if (i == currentMaxClients)
	currentMaxClients++;
    while ((nextFreeClientID < MAXCLIENTS) && clients[nextFreeClientID])
	nextFreeClientID++;
    return(client);
}

int
ProcInitialConnection(client)
    register ClientPtr client;
{
    REQUEST(auReq);
    register auConnClientPrefix *prefix;
    int whichbyte = 1;

    prefix = (auConnClientPrefix *)((char *)stuff + sz_auReq);
    if ((prefix->byteOrder != 'l') && (prefix->byteOrder != 'B'))
	return (client->noClientException = -1);
    if (((*(char *) &whichbyte) && (prefix->byteOrder == 'B')) ||
	(!(*(char *) &whichbyte) && (prefix->byteOrder == 'l')))
    {
	client->swapped = TRUE;
	SwapConnClientPrefix(prefix);
    }
    stuff->reqType = 2;
    stuff->length += (((int) prefix->nbytesAuthProto + 3) >> 2) +
		     (((int) prefix->nbytesAuthString + 3) >> 2);
    if (client->swapped)
    {
	swaps(&stuff->length, whichbyte);
    }
    ResetCurrentRequest(client);
    return (client->noClientException);
}

int
ProcEstablishConnection(client)
    register ClientPtr client;
{
    char *reason, *auth_proto, *auth_string;
    register auConnClientPrefix *prefix;
    REQUEST(auReq);

    prefix = (auConnClientPrefix *)((char *)stuff + sz_auReq);
    auth_proto = (char *)prefix + sz_auConnClientPrefix;
    auth_string = auth_proto + ((prefix->nbytesAuthProto + 3) & ~3);
    if (prefix->majorVersion != AuProtocolMajorVersion)
	reason = "Protocol version mismatch";
    else
	reason = ClientAuthorized(client,
				  (unsigned short)prefix->nbytesAuthProto,
				  auth_proto,
				  (unsigned short)prefix->nbytesAuthString,
				  auth_string);
    if (reason)
    {
	auConnSetupPrefix csp;
	char pad[3];

	csp.success = auFalse;
	csp.lengthReason = strlen(reason);
	csp.length = ((int) csp.lengthReason + 3) >> 2;
	csp.majorVersion = AuProtocolMajorVersion;
	csp.minorVersion = AuProtocolMinorVersion;
	if (client->swapped)
	    WriteSConnSetupPrefix(client, &csp);
	else
	    (void)WriteToClient(client, sz_auConnSetupPrefix, (char *) &csp);
        (void)WriteToClient(client, (int)csp.lengthReason, reason);
	if (csp.lengthReason & 3)
	    (void)WriteToClient(client, (int)(4 - (csp.lengthReason & 3)),
				pad);
	return (client->noClientException = -1);
    }

    nClients++;
    client->sequence = 0;
    client->requestVector = 
	client->swapped ? AuSwappedProcVector : AuProcVector;
    return AuSendInitResponse(client);
}

void
MarkClientException(client)
    ClientPtr client;
{
    client->noClientException = -1;
}
