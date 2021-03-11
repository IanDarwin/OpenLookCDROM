/************************************************************
Copyright 1987, 1989 by Digital Equipment Corporation, 
Maynard, Massachusetts, and the Massachusetts Institute of Technology, 
Cambridge, Massachusetts.

                        All Rights Reserved

DIGITAL DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING
ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL
DIGITAL BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR
ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,
WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION,
ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS
SOFTWARE.

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

#include <strings.h>
#include "audio.h"
#define NEED_REPLIES
#define NEED_EVENTS
#include "audioproto.h"
#include "acstruct.h"
#include "opaque.h"
#include "input.h"
#include "servermd.h"
#include "diastruct.h"
#include "audiodev.h"
#include "task.h"
#include "extnsionst.h"
#include "acfuncs.h"
#include "ddafuncs.h"
#include "AFUtils.h"

extern AAtom MakeAtom();
extern char *NameForAtom();
extern aConnSetupPrefix connSetupPrefix;
extern char *ConnectionInfo;
extern void NotImplemented();
extern ABool InitClientResources();

static ClientPtr onlyClient;
static ABool grabbingClient = FALSE;
static long *checkForInput[2];
extern int connBlockScreenStart;

extern int (* InitialVector[3]) ();
extern int (* ProcVector[256]) ();
extern int (* SwappedProcVector[256]) ();
extern void (* EventSwapVector[128]) ();
extern void (* ReplySwapVector[256]) ();
extern void SLHostsExtend();
extern void WriteSConnSetupPrefix();
extern char *ClientAuthorized();
extern ABool InsertFakeRequest();
static void KillAllClients();
extern void WriteSConnectionInfo();
extern int Ones();

extern int CompareTimeStamps(TimeStamp, TimeStamp);
extern int WaitForSomething(int *);

void SendErrorToClient(ClientPtr, unsigned, unsigned short, AID, int);

static int nextFreeClientID; /* always MIN free client ID */

static int	nClients;	/* number active clients */

char dispatchException = 0;
char isItTimeToYield;

#define	RECLOWBOUND	4


void
SetInputCheck(c0, c1)
    long *c0, *c1;
{
    checkForInput[0] = c0;
    checkForInput[1] = c1;
}

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
    if (*checkForInput[0] != *checkForInput[1])
	ProcessInputEvents();
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
    if (*checkForInput[0] == *checkForInput[1])
	currentTime = systime;
}

void 
FlushClientCaches(id)
    AID id;
{
    int i;
    register ClientPtr client;

    client = clients[CLIENT_ID(id)];
    if (client == NullClient)
        return ;
    for (i=0; i<currentMaxClients; i++)
    {
	client = clients[i];
        if (client != NullClient)
	{
            if (client->lastACID == id)
	    {
                client->lastACID = INVALID;
		client->lastAC = (ACPtr)NULL;
	    }
            else if (client->lastACID == id)
	    {
                client->lastACID = INVALID;
		client->lastAC = (ACPtr)NULL;
	    }
	}
    }
}

#define MAJOROP ((aReq *)client->requestBuffer)->reqType

void
Dispatch()
{
    register int        *clientReady;     /* array of request ready clients */
    register int	result;
    register ClientPtr	client;
    register int	nready;
    register long	**icheck = checkForInput;

    nextFreeClientID = 1;
    nClients = 0;

    clientReady = (int *) ALLOCATE_LOCAL(sizeof(int) * MaxClients);
    if (!clientReady)
	return;

    while (!dispatchException)
    {
        if (*icheck[0] != *icheck[1])
	{
	    ProcessInputEvents();
	    FlushIfCriticalOutputPending();
	}

	nready = WaitForSomething(clientReady);
       /***************** 
	*  Handle events in round robin fashion, doing input between 
	*  each round 
	*****************/

	/* Process pending tasks. */

	RunPendingTasks();


	while (!dispatchException && (--nready >= 0))
	{
	    client = clients[clientReady[nready]];
	    if (! client)
	    {
		/* KillClient can cause this to happen */
		continue;
	    }
	    /* GrabServer activation can cause this to be true */
	    if (grabbingClient && (client != onlyClient))
		break;
	    isItTimeToYield = FALSE;
 
	    while (!isItTimeToYield)
	    {
	        if (*icheck[0] != *icheck[1])
		{
		    ProcessInputEvents();
		    FlushIfCriticalOutputPending();
		}
	   
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
		if (result > (MAX_REQUEST_SIZE << 2))
		    result = ABadLength;
		else
		    result = (* client->requestVector[MAJOROP])(client);
	    
		if (result != ASuccess) 
		{
		    if (client->noClientException != ASuccess)
                        CloseDownClient(client);
                    else
		        SendErrorToClient(client, MAJOROP,
					  MinorOpcodeOfRequest(client),
					  client->errorValue, result);
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
ProcBadRequest(ClientPtr client)
{
    return (ABadRequest);
}

extern int Ones();

int ProcNoOperation(client)
    register ClientPtr client;
{
    REQUEST(aReq);

    REQUEST_AT_LEAST_SIZE(aReq);
    
    /* noop -- don't do anything */
    return(client->noClientException);
}

int ProcSelectEvents(client)
    register ClientPtr client;
{
    AC *pAC;
    AMask temp;
    REQUEST(aSelectEventsReq);

    REQUEST_AT_LEAST_SIZE(aSelectEventsReq);
    VERIFY_AC(pAC, stuff->ac, client);

    temp = (1 << ALASTEvent) - 1;
    temp &= ~3;		/* get rid of low order two bits */

    if (stuff->mask & (~temp))	return ABadValue;

    client->selectMask[pAC->aDev->index] = stuff->mask;
    return (client->noClientException);
}

int ProcDialPhone(client)
     register ClientPtr client;
{
    AC *pAC;
    char *tchar;
    int status;

    REQUEST(aDialPhoneReq);
    REQUEST_FIXED_SIZE(aDialPhoneReq, stuff->nbytes);
    VERIFY_AC(pAC, stuff->ac, client);

    if(pAC->aDev->Dial == (IntProc)NoopDDA) 
	return ABadDevice;

    tchar = (char *) &stuff[1];
    status = (pAC->aDev->Dial)(pAC->aDev,tchar);
    return ASuccess;
}

int ProcCreateAC(client)
    register ClientPtr client;
{
    int error;
    AC *pAC;
    unsigned len;
    AudioDevicePtr aDev;
    REQUEST(aCreateACReq);

    REQUEST_AT_LEAST_SIZE(aCreateACReq);
    client->errorValue = stuff->ac;
    LEGAL_NEW_RESOURCE(stuff->ac, client);
    if ((stuff->device < 0)||(stuff->device >= audioDeviceInfo.numDevices)) 
	return ABadDevice;	/* XXX this should be better */
    len = stuff->length -  (sizeof(aCreateACReq) >> 2);
    if (len != Ones(stuff->mask))
        return ABadLength;
    aDev = audioDeviceInfo.devices[stuff->device];
    pAC = (AC *)CreateAC(client, aDev, stuff->mask,
                         (AID *) &stuff[1], &error);
    if (error != ASuccess)
        return error;
    if (!AddResource(stuff->ac, RT_AC, (pointer)pAC))
        return (ABadAlloc);
    return(client->noClientException);
}

int
ProcChangeACAttributes(client)
    register ClientPtr client;
{
    AC *pAC;
    REQUEST(aChangeACAttributesReq);
    int result;
    unsigned len;
		
    REQUEST_AT_LEAST_SIZE(aChangeACAttributesReq);
    VERIFY_AC(pAC, stuff->ac, client);
    len = stuff->length -  (sizeof(aChangeACAttributesReq) >> 2);
    if (len != Ones(stuff->mask))
        return ABadLength;
    result = DoChangeAC(client, pAC, stuff->mask, (AID *) &stuff[1]);
    if (client->noClientException != ASuccess)
        return(client->noClientException);
    else
    {
        return(result);
    }
}

int ProcFreeAC(client)
    register ClientPtr client;
{
    register AC *pAC;
    REQUEST(aResourceReq);

    REQUEST_SIZE_MATCH(aResourceReq);
    VERIFY_AC(pAC,stuff->id,client);
    FreeResource(stuff->id, RT_NONE);
    return(client->noClientException);
}


static int
FinishProcPlaySamples(TaskPtr t)
{
  int nplayed;
  aGetTimeReply reply;
  ACPtr pAC = t->ac;

  nplayed = (pAC->ops->ConvertPlay)(t->time, t->p, t->len, pAC);
/*
  printf("finish play(%d) ac %x time %d len %d ptr 0x%x\n",
     t->ssize, pAC, t->time, t->len, t->p); 
*/
  if(nplayed != t->len) {
    int sampleSize = BytesPerUnit(pAC->aDev->playBufType) *
      pAC->aDev->playNchannels;
    TaskPtr task=NewTask();
    ATime endTime = t->time + t->len;
    /* lastBufTime should be set to the last sample buffered by the server.
       We want to task until the remnant of the client request will all fit,
       There is really no need to call GetTime (which forces an immediate
       update.  However, there is no convenient interface to get the time
       as of the last update. */
    ATime lastBufTime = (*(pAC->aDev->GetTime))(pAC->aDev) 
      + pAC->aDev->playNSamplesBuf;
    int t0;


    task->client = t->client;
    task->time = t->time + nplayed;
    task->len = t->len - nplayed;
    task->p = ((u_char *) t->p) + (nplayed * sampleSize);
    task->ac = pAC;
    task->mask = t->mask;
    if (t->ssize == 0) IgnoreClient(t->client);
    task->ssize = 1;

    /* If we are here with a fake task, and are not decompressing,
     * then we need to copy the data from the "stuff" buffer since
     * it will not survive until the task runs.
     * Look at the ssize flag in the original task struct.
     */
    if ((t->ssize == 0) && (pAC->playType == pAC->aDev->playBufType)) {
       bcopy((const char *)task->p, (char *)task->client->crdata, 
          task->len * sampleSize);
       task->p = task->client->crdata;
    }

/*
    printf("Finish play time %d len %d ptr 0x%x\n", 
    task->time, task->len, t->p);   
*/
    /* wait until the end time is 300 milliseconds inside the
       buffers */
    t0 = (int) endTime - (int) lastBufTime;
    if (t0 < 0) t0 = 0;
    t0 = (t0 * 1000) / pAC->aDev->playSampleFreq;
/* printf("FinishProcPlay task %d %d\n", task->time,
   ((t0 + 300) * pAC->aDev->playSampleFreq) / 1000); */
    AddTask((VoidProc) FinishProcPlaySamples, task, t0 + 300);
    return (t->client->noClientException);
  }
  
  if (t->ssize) AttendClient(t->client);

  if(!(t->mask & ANoTimeReplyMask)) {
    reply.type = A_Reply;
    reply.sequenceNumber = t->client->sequence;
    reply.length = 0;
    reply.time = (*(pAC->aDev->GetTime))(pAC->aDev);
    WriteReplyToClient(t->client, sizeof(aGetTimeReply), &reply);
  }
  return (t->client->noClientException);
}

#include "../os/4.2bsd/osdep.h"

int ProcPlaySamples(client)
    register ClientPtr client;
{
  AC *pAC;
  REQUEST(aPlaySamplesReq);
  int samples, units, unitSize;
  int endian = 1;
  ATime endTime, lastBufTime;
  Task taskStruct;
  TaskPtr task = &taskStruct;
  int ms300;
  
  REQUEST_AT_LEAST_SIZE(aPlaySamplesReq);
  VERIFY_AC(pAC, stuff->ac, client);

/*
printf("ProcPlay client %x oc %x oc->input %x ac %x bufptr %x\n",
        client,
        client->osPrivate,
        ((OsCommPtr)(client->osPrivate))->input, 
        pAC,&stuff[1]);
*/
  if (stuff->sampleType != pAC->playType) {
    ErrorF("play: sample type %d different from ac %d\n",
	   stuff->sampleType, pAC->playType);
    return(ABadMatch);
  }
  if (stuff->nchannels != pAC->aDev->playNchannels) {
    ErrorF("play: num channels: %d different from device %d\n", 
	   stuff->nchannels, pAC->aDev->playNchannels);
    return(ABadMatch);
  }

  /*
   * XXX this should be in device dependent code, but for now, we
   * don't have any byte swapping code in the Lofi firmware, so we'll do
   * it here.  Lets see how good a C boolean hacker you are!
   */
  switch(pAC->playType) {
  case LIN16:
    if (((stuff->mask & ABigEndianMask) == ABigEndianMask)
	^ (!(*(char *) & endian))) {
      SwapShorts((CARD16 *) &stuff[1], 
		 stuff->nbytes / sizeof(CARD16));
    }
    break;
  case LIN32:
    if (((stuff->mask & ABigEndianMask) == ABigEndianMask)
	^ (!(*(char *) & endian))) {
      SwapLongs((CARD32 *) &stuff[1], 
		stuff->nbytes / sizeof(CARD32));
    }
    break;
  }
  unitSize = BytesPerUnit(pAC->playType) * stuff->nchannels;
  units = stuff->nbytes / unitSize;

  if ((units * unitSize) != stuff->nbytes) {
    ErrorF("play: client request %d not a multiple of sample unit %d\n", 
	   stuff->nbytes, unitSize);
    return(ABadImplementation);
  }
  samples = units * SampsPerUnit(pAC->playType);
  endTime = stuff->startTime + samples;
  /* We need lastBufTime to tell if the entire client request will fit
     or whether we need to task until it will fit.  We'd rather have
     a "recent" time, as of the last periodic update, but there is no way
     to get that from the DDA.  Instead, we have to call the GetTime
     procedure.  This will have the side effect of going all the way to
     the hardware for the truth, which we don't need. aDev->time0 would
     work, except that not all DDA's keep it up to date. */
  lastBufTime = (*(pAC->aDev->GetTime))(pAC->aDev) 
    + pAC->aDev->playNSamplesBuf;

  /* decompress if necessary */
  if (pAC->playType == pAC->aDev->playBufType) {
    task->p = (pointer) &stuff[1];
  }
  else {
/* 
 printf("uncomp.next %d, starttime %d\n", pAC->uncompressState.next, stuff->startTime);
 */
    if (stuff->startTime != pAC->uncompressState.next)
      (pAC->uncompressState.convertInitProc)(&pAC->uncompressState, RESET);
    pAC->uncompressState.next = endTime;
/*
    printf("convert from 0x%x to 0x%x, units %d\n",
	   (unsigned char *)&stuff[1], 
	   pAC->uncompressState.buffer,
	   units);
 */
    (pAC->uncompressState.convertProc)(&pAC->uncompressState,
				       (unsigned char *)&stuff[1], 
				       pAC->uncompressState.buffer,
				       units);
    task->p = pAC->uncompressState.buffer;
  }

  task->client = client;
  task->time = stuff->startTime;
  task->len = samples;
  task->ac = pAC;
  task->mask = stuff->mask;
  task->ssize = 0; /* not yet ignoring client */

  ms300 = (pAC->aDev->playSampleFreq / 10) * 3;
  if (((int) lastBufTime - (int) endTime) < ms300) {
    TaskPtr realTask=NewTask();
    ATime t0;

    realTask->client = task->client;
    realTask->time = task->time;
    realTask->len = task->len;
    realTask->ac = task->ac;
    realTask->mask = task->mask;
    /* Since the request buffer is not preserved across multiple 
     * read request from client calls, we will need to copy the data
     * in the non-decompressing case when the data does not fit entirely. 
     * 
     * It would be better to have an interface that would
     * lock the request buffer down in the os layer such that the dia
     * did not have to perform extra copies.  Don't blame us, that code
     * came from X11.
     */
    if (pAC->playType != pAC->aDev->playBufType) 
      realTask->p = task->p;
    else {
      /* Perform an extra copy since we will not complete now. */
      bcopy((const char *)&stuff[1], (char *)task->client->crdata,
             units * unitSize);
      realTask->p = task->client->crdata;
    }
    realTask->ssize = 1; /* ssize == 1 means Finish must AttendClient */
    IgnoreClient(client);
    t0 = (((endTime - lastBufTime) + ms300) * 1000) / 
      pAC->aDev->playSampleFreq;
    /* wait until the end time is 300 milliseconds inside the
       buffers */
/* if (eventlog) printf("ProcPlay task %d %d\n", task->time, 
   (t0 * pAC->aDev->playSampleFreq) / 1000); */
    AddTask((VoidProc) FinishProcPlaySamples, realTask, t0);
    return (client->noClientException);
  }
  FinishProcPlaySamples(task);
  return(client->noClientException);
}

static void
FinishProcRecordSamples(TaskPtr t)
{
  aRecordSamplesReply reply;
  int endian = 1;
  ACPtr pAC = t->ac;
  int nrecorded;
  int sampleSize;
  /* XXX t->len is actually device native units, not samples */
  sampleSize = BytesPerUnit(pAC->aDev->recBufType) * pAC->aDev->recNchannels;

  nrecorded = (pAC->ops->ConvertRec)(t->time, t->p, t->len, t->ac);
    
  if((t->mask & ABlockMask) && (nrecorded < t->len)) {
    TaskPtr task=NewTask();
    ATime t0;
    if (t->ssize == 0) IgnoreClient(t->client);
    task->ssize = 1; /* now ignoring client */
    task->client = t->client;
    task->time = t->time + nrecorded;
    task->len = t->len - nrecorded;
    task->ac = t->ac;
    task->p = t->p + (nrecorded * sampleSize);
    task->mask = t->mask;
    
    t0 = (task->len * 1000) / t->ac->aDev->recSampleFreq;
    t0 += RECLOWBOUND;
    AddTask((VoidProc) FinishProcRecordSamples, task, t0);
  }
  else {
    int unitSize, units, nBytes;
    unitSize = BytesPerUnit(pAC->recType) * pAC->aDev->recNchannels;
    if (pAC->recType == pAC->aDev->recBufType) {
      nrecorded += ((t->p - t->client->crdata) / sampleSize);
    }
    else {
      nrecorded += ((t->p - ((CARD8 *) pAC->compressState.buffer)) / 
		     sampleSize);
    }
    /* XXX nrecorded is in device Units, not samples */
    units = nrecorded / SampsPerUnit(pAC->recType);
    nBytes = units * unitSize;
    
    if (t->ssize) AttendClient(t->client);
    
    reply.type = A_Reply;
    reply.sequenceNumber = t->client->sequence;
    reply.length = (nBytes + 3) >> 2;
    reply.nbytes = nBytes;
    
    if (pAC->recType != pAC->aDev->recBufType) {
      (pAC->compressState.convertProc)(&pAC->compressState,
				       pAC->compressState.buffer,
				       t->client->crdata,
				       nrecorded);
    }
    /*
     * XXX this should be in device dependent code, but for now, we
     * don't have any byte swapping code in the Lofi firmware, so we'll do
     * it here.  Lets see how good a C boolean hacker you are!
     */
    if ((((t->mask & ABigEndianMask) == ABigEndianMask)
	 ^ (!(*(char *) & endian)))) {
      switch(t->ac->recType) {
      LIN16:
	SwapShorts((CARD16 *)t->client->crdata, 
		   nBytes / sizeof(CARD16));
	break;
      LIN32:
	SwapLongs((CARD32 *) t->client->crdata, 
		  nBytes / sizeof(CARD32));
	break;
      default:
	break;
      }
    }
    if (t->mask & ABlockMask)
      {
	reply.currentTime = (*(pAC->aDev->GetTime))(t->ac->aDev);
      }
    else
      {
	/* 
	  We can't return the most current time for non-blocking 
	  records because time may have advanced since we read data 
	  from the record buffers.  So tell the client the approximate
	  time that can be used for seamless non-blocking record
	  operations.
	  */
	/* t->time == stuff->startTime, because this is the first
	   pass through FinishProcRecordSamples
	   */
	reply.currentTime = t->time + nrecorded;
      }
    WriteReplyToClient(t->client, sizeof(aRecordSamplesReply), &reply);
    (void) WriteToClient(t->client, nBytes, t->client->crdata);
  }
}

int ProcRecord(client)
    register ClientPtr client;
{
  AC *pAC;
  REQUEST(aRecordSamplesReq);
  int unitSize;
  int units;
  int samples;
  Task taskStruct;
  TaskPtr task = &taskStruct;

  REQUEST_AT_LEAST_SIZE(aRecordSamplesReq);
  VERIFY_AC(pAC, stuff->ac, client);

  if (stuff->sampleType != pAC->recType) {
    ErrorF("record: sample type: %d different from ac %d\n", 
	   stuff->sampleType, pAC->recType);
    return(ABadMatch);
  }
  if (stuff->nchannels != pAC->aDev->recNchannels) {
    ErrorF("record: num channels: %d different from device %d\n", 
	   stuff->nchannels, pAC->aDev->recNchannels);
    return(ABadMatch);
  }
  unitSize = BytesPerUnit(pAC->recType) *
    stuff->nchannels;
  units = stuff->nbytes / unitSize;
  samples = units * SampsPerUnit(pAC->recType);
  if ((units * unitSize) != stuff->nbytes) {
    ErrorF("Client request %d not a multiple of sample unit %d\n", 
	   stuff->nbytes, unitSize);
    return(ABadImplementation);
  }

  /* If the current request doesn't follow the last one, reset the
   * compression state.
   * Always set the state time to the end of this request
   */
  if (pAC->recType != pAC->aDev->recBufType) {
    if (stuff->startTime != pAC->compressState.next)
      (pAC->compressState.convertInitProc)(&pAC->compressState, RESET);
    pAC->compressState.next = stuff->startTime + samples;
  }

  /* if we are not compressing, then record to crdata, if we are
   * compressing, then record to the compressState.buffer
   */
  if (pAC->recType == pAC->aDev->recBufType) {
    task->p = client->crdata;
  }
  else {
    task->p = pAC->compressState.buffer;
  }

  task->client = client;
  task->time = stuff->startTime;
  task->len = samples;
  task->ac = pAC;
  task->mask = stuff->mask;
  task->ssize = 0; /* 0 means attending client, 1 means ignoring client */
  FinishProcRecordSamples(task);
  return(client->noClientException);
}

int ProcGetTime(client)
    register ClientPtr client;
{
        AC *pAC;
	REQUEST(aGetTimeReq);
	aGetTimeReply reply;
	AudioDevicePtr aDev;

        VERIFY_AC(pAC, stuff->ac, client);
        aDev = pAC->aDev;

	reply.type = A_Reply;
	reply.sequenceNumber = client->sequence;
	reply.length = 0;
	if (aDev->GetTime == (TimeProc) NoopDDA)
	  return ABadDevice;

	reply.time = (*(aDev->GetTime))(aDev);
	WriteReplyToClient(client, sizeof(aGetTimeReply), &reply);
	return(client->noClientException);
}

int ProcSyncConnection(client)
    register ClientPtr client;
{
	REQUEST(aSyncConnectionReq);
	aGenericReply reply;
	reply.type = A_Reply;
	reply.sequenceNumber = client->sequence;
	reply.length = 0;
	WriteReplyToClient(client, sizeof(aGenericReply), &reply);
	return(client->noClientException);
}

int
ProcSetInputGain(client)
    register ClientPtr client;
{
    AC *pAC;
    REQUEST(aSetInputGainReq);
    int result;
    AudioDevicePtr aDev;
		
    REQUEST_SIZE_MATCH(aSetInputGainReq);
    VERIFY_AC(pAC, stuff->ac, client);

    aDev = pAC->aDev;
    result =  (*aDev->SelectInputGain)(aDev, stuff->gain);
    if (result != ASuccess)
	return ABadValue;
    return ASuccess;
}

int
ProcSetOutputGain(client)
    register ClientPtr client;
{
    AC *pAC;
    REQUEST(aSetOutputGainReq);
    int result;
    AudioDevicePtr aDev;
		
    REQUEST_SIZE_MATCH(aSetOutputGainReq);
    VERIFY_AC(pAC, stuff->ac, client);

    aDev = pAC->aDev;
    result = (*aDev->SelectOutputGain)(aDev, stuff->gain);
    if (result != ASuccess)
	return ABadValue;
    return ASuccess;
}

int
ProcEnableInput(client)
    register ClientPtr client;
{
    AC *pAC;
    REQUEST(aEnableInputReq);
    aControlIOReply reply;
    AudioDevicePtr aDev;

    REQUEST_SIZE_MATCH(aEnableInputReq);
    VERIFY_AC(pAC, stuff->ac, client);
    aDev = pAC->aDev;
    if ( stuff->mask & ~((1 << aDev->numberOfInputs) - 1)) 
    	return ABadValue;
    (void) (*aDev->ChangeInput)
	(aDev, 1, stuff->mask, (int *) &reply.oldState, (int *) &reply.newState);
    reply.type = A_Reply;
    reply.sequenceNumber = client->sequence;
    reply.length = 0;
    WriteReplyToClient(client, sizeof(aControlIOReply), &reply);
    return(client->noClientException);

}

int
ProcEnableOutput(client)
    register ClientPtr client;
{
    AC *pAC;
    REQUEST(aEnableOutputReq);
    aControlIOReply reply;
    AudioDevicePtr aDev;
    REQUEST_SIZE_MATCH(aEnableOutputReq);
    VERIFY_AC(pAC, stuff->ac, client);

    aDev = pAC->aDev;
    if ( stuff->mask & ~((1 << aDev->numberOfOutputs) - 1)) 
    	return ABadValue;
    (void) (*aDev->ChangeOutput)
	(aDev, 1, stuff->mask, (int *) &reply.oldState, (int *)&reply.newState);
    reply.type = A_Reply;
    reply.sequenceNumber = client->sequence;
    reply.length = 0;
    WriteReplyToClient(client, sizeof(aControlIOReply), &reply);
    return(client->noClientException);

}

int
ProcEnableGainControl(client)
    register ClientPtr client;
{
    AC *pAC;
    REQUEST(aEnableGainControlReq);
    aControlIOReply reply;
    AudioDevicePtr aDev;
    REQUEST_SIZE_MATCH(aEnableGainControlReq);
    VERIFY_AC(pAC, stuff->ac, client);

    aDev = pAC->aDev;
    if ( stuff->mask & ~((1 << aDev->numberOfOutputs) - 1)) 
    	return ABadValue;
    (void) (*aDev->ChangeGainCtl)
	(aDev, 1, stuff->mask, (int *) &reply.oldState, (int *) &reply.newState);
    reply.type = A_Reply;
    reply.sequenceNumber = client->sequence;
    reply.length = 0;
    WriteReplyToClient(client, sizeof(aControlIOReply), &reply);
    return(client->noClientException);

}

int
ProcDisableInput(client)
    register ClientPtr client;
{
    AC *pAC;
    REQUEST(aDisableInputReq);
    aControlIOReply reply;
    AudioDevicePtr aDev;
    REQUEST_SIZE_MATCH(aDisableInputReq);
    VERIFY_AC(pAC, stuff->ac, client);

    aDev = pAC->aDev;
    if ( stuff->mask & ~((1 << aDev->numberOfInputs) - 1)) 
    	return ABadValue;
    (void) (*aDev->ChangeInput)
	(aDev, 0, stuff->mask, (int *) &reply.oldState, (int *) &reply.newState);
    reply.type = A_Reply;
    reply.sequenceNumber = client->sequence;
    reply.length = 0;
    WriteReplyToClient(client, sizeof(aControlIOReply), &reply);
    return(client->noClientException);

}

int
ProcDisableOutput(client)
    register ClientPtr client;
{
    AC *pAC;
    REQUEST(aDisableOutputReq);
    aControlIOReply reply;
    AudioDevicePtr aDev;
    REQUEST_SIZE_MATCH(aDisableOutputReq);
    VERIFY_AC(pAC, stuff->ac, client);

    aDev = pAC->aDev;
    if ( stuff->mask & ~((1 << aDev->numberOfOutputs) - 1)) 
    	return ABadValue;
    (void) (*aDev->ChangeOutput)
	(aDev, 0, stuff->mask, (int *) &reply.oldState, (int *) &reply.newState);
    reply.type = A_Reply;
    reply.sequenceNumber = client->sequence;
    reply.length = 0;
    WriteReplyToClient(client, sizeof(aControlIOReply), &reply);
    return(client->noClientException);

}

int
ProcDisableGainControl(client)
    register ClientPtr client;
{
    AC *pAC;
    REQUEST(aDisableGainControlReq);
    aControlIOReply reply;
    AudioDevicePtr aDev;
    REQUEST_SIZE_MATCH(aDisableGainControlReq);
    VERIFY_AC(pAC, stuff->ac, client);

    aDev = pAC->aDev;
    if ( stuff->mask & ~((1 << aDev->numberOfOutputs) - 1)) 
    	return ABadValue;
    (void) (*aDev->ChangeGainCtl)
	(aDev, 0, stuff->mask, (int *) &reply.oldState, (int * ) &reply.newState);
    reply.type = A_Reply;
    reply.sequenceNumber = client->sequence;
    reply.length = 0;
    WriteReplyToClient(client, sizeof(aControlIOReply), &reply);
    return(client->noClientException);

}

int
ProcQueryInputGain(client)
    register ClientPtr client;
{
    AC *pAC;
    REQUEST(aQueryInputGainReq);
    aQueryGainReply reply;
    AudioDevicePtr aDev;

    REQUEST_SIZE_MATCH(aQueryInputGainReq);
    VERIFY_AC(pAC, stuff->ac, client);
    aDev = pAC->aDev;
    reply.gain = (*aDev->QueryInputGain)
			(aDev, (int *) &reply.minGain, (int *) &reply.maxGain);
    reply.type = A_Reply;
    reply.sequenceNumber = client->sequence;
    reply.length = 0;
    WriteReplyToClient(client, sizeof(aQueryGainReply), &reply);
    return(client->noClientException);

}

int
ProcQueryOutputGain(client)
    register ClientPtr client;
{
    AC *pAC;
    REQUEST(aQueryOutputGainReq);
    aQueryGainReply reply;
    AudioDevicePtr aDev;
    REQUEST_SIZE_MATCH(aQueryOutputGainReq);
    VERIFY_AC(pAC, stuff->ac, client);

    aDev = pAC->aDev;
    reply.gain = (*aDev->QueryOutputGain)
			(aDev, (int *) &reply.minGain, (int *) &reply.maxGain);
    reply.type = A_Reply;
    reply.sequenceNumber = client->sequence;
    reply.length = 0;
    WriteReplyToClient(client, sizeof(aQueryGainReply), &reply);
    return(client->noClientException);

}

int
ProcEnablePassThrough(client)
    register ClientPtr client;
{
    AC *pAC;
    REQUEST(aEnablePassThroughReq);
    aControlIOReply reply;
    AudioDevicePtr aDev;

    REQUEST_SIZE_MATCH(aEnablePassThroughReq);
    VERIFY_AC(pAC, stuff->ac, client);
    aDev = pAC->aDev;
    if ( stuff->mask > 1)
    	return ABadValue;
    (void) (*aDev->ChangePassThrough)
	(aDev, 1, stuff->mask, (int *) &reply.oldState, (int *) &reply.newState);
    reply.type = A_Reply;
    reply.sequenceNumber = client->sequence;
    reply.length = 0;
    WriteReplyToClient(client, sizeof(aControlIOReply), &reply);
    return(client->noClientException);

}

int
ProcDisablePassThrough(client)
    register ClientPtr client;
{
    AC *pAC;
    REQUEST(aDisablePassThroughReq);
    aControlIOReply reply;
    AudioDevicePtr aDev;

    REQUEST_SIZE_MATCH(aDisablePassThroughReq);
    VERIFY_AC(pAC, stuff->ac, client);

    aDev = pAC->aDev;
    if ( stuff->mask > 1) 
    	return ABadValue;
    (void) (*aDev->ChangePassThrough)
	(aDev, 0, stuff->mask, (int *) &reply.oldState, (int *) &reply.newState);
    reply.type = A_Reply;
    reply.sequenceNumber = client->sequence;
    reply.length = 0;
    WriteReplyToClient(client, sizeof(aControlIOReply), &reply);
    return(client->noClientException);

}

int ProcKillClient(client)
    register ClientPtr client;
{
    ErrorF("Kill Client not implemented...\n");
    return(ABadImplementation);
}

int
ProcQueryPhone(client)
    register ClientPtr client;
{
    AC *pAC;
    REQUEST(aQueryPhoneReq);
    aQueryPhoneReply reply;
    AudioDevicePtr aDev;

    REQUEST_SIZE_MATCH(aQueryPhoneReq);
    VERIFY_AC(pAC, stuff->ac, client);
    aDev = pAC->aDev;


    reply.hs_state = (aDev->HookSwitchState)(aDev);
    reply.loop_state = (aDev->LoopCurrentState)(aDev);
    reply.type = A_Reply;
    reply.sequenceNumber = client->sequence;
    reply.length = 0;

    WriteReplyToClient(client, sizeof(aQueryPhoneReply), &reply);
    return(client->noClientException);
}

int ProcHookSwitch(client)
     register ClientPtr client;
{
    AC *pAC;
    int status;
    aEvent ae;
    int device;
    AudioDevicePtr aDev;

    REQUEST(aHookSwitchReq);

    REQUEST_SIZE_MATCH(aHookSwitchReq);
    VERIFY_AC(pAC, stuff->ac, client);
    aDev = pAC->aDev;
    if(aDev->HookSwitch == (IntProc)NoopDDA) 
	return ABadDevice;

    status = (aDev->HookSwitch)(aDev, stuff->onoff);
    device = (aDev->TLICraftHookSwitchEvent)(aDev, &ae);
    FilterEvents(&ae, device);
    return ASuccess;
}

int ProcFlashHook(client)
     register ClientPtr client;
{
    AC *pAC;
    int status;
    AudioDevicePtr aDev;

    REQUEST(aFlashHookReq);

    REQUEST_SIZE_MATCH(aFlashHookReq);
    VERIFY_AC(pAC, stuff->ac, client);
    aDev = pAC->aDev;
    if(aDev->FlashHook == (IntProc)NoopDDA) 
	return ABadDevice;

    status = (aDev->FlashHook)(aDev, stuff->duration);

    return ASuccess;
}

int
ProcListHosts(client)
    register ClientPtr client;
{
    extern int GetHosts();
    aListHostsReply reply;
    int	len, nHosts, result;
    pointer	pdata;
    REQUEST(aListHostsReq);

    REQUEST_SIZE_MATCH(aListHostsReq);
    result = GetHosts(&pdata, &nHosts, &len, &reply.enabled);
    if (result != ASuccess)
	return(result);
    reply.type = A_Reply;
    reply.sequenceNumber = client->sequence;
    reply.nHosts = nHosts;
    reply.length = len >> 2;
    WriteReplyToClient(client, sizeof(aListHostsReply), &reply);
    if (nHosts)
    {
	client->pSwapReplyFunc = SLHostsExtend;
	WriteSwappedDataToClient(client, len, pdata);
    }
    xfree(pdata);
    return (client->noClientException);
}

int
ProcChangeHosts(client)
    register ClientPtr client;
{
    REQUEST(aChangeHostsReq);
    int result;

    REQUEST_FIXED_SIZE(aChangeHostsReq, stuff->hostLength);

    if(stuff->mode == AHostInsert)
	result = AddHost(client, (int)stuff->hostFamily,
			 stuff->hostLength, (pointer)&stuff[1]);
    else if (stuff->mode == AHostDelete)
	result = RemoveHost(client, (int)stuff->hostFamily, 
			    stuff->hostLength, (pointer)&stuff[1]);  
    else
    {
	client->errorValue = stuff->mode;
        return ABadValue;
    }
    if (!result)
	result = client->noClientException;
    return (result);
}

int
ProcChangeAccessControl(client)
    register ClientPtr client;
{
    int result;
    REQUEST(aSetAccessControlReq);

    REQUEST_SIZE_MATCH(aSetAccessControlReq);
    if ((stuff->mode != AEnableAccess) && (stuff->mode != ADisableAccess))
    {
	client->errorValue = stuff->mode;
        return ABadValue;
    }
    result = ChangeAccessControl(client, stuff->mode == AEnableAccess);
    if (!result)
	result = client->noClientException;
    return (result);
}

int
ProcInternAtom(client)
    register ClientPtr client;
{
    AAtom atom;
    char *tchar;
    REQUEST(aInternAtomReq);

    REQUEST_FIXED_SIZE(aInternAtomReq, stuff->nbytes);
    if ((stuff->onlyIfExists != aTrue) && (stuff->onlyIfExists != aFalse))
    {
	client->errorValue = stuff->onlyIfExists;
        return(ABadValue);
    }
    tchar = (char *) &stuff[1];
    atom = MakeAtom(tchar, stuff->nbytes, !stuff->onlyIfExists);
    if (atom != BAD_RESOURCE)
    {
	aInternAtomReply reply;
	reply.type = A_Reply;
	reply.length = 0;
	reply.sequenceNumber = client->sequence;
	reply.atom = atom;
	WriteReplyToClient(client, sizeof(aInternAtomReply), &reply);
	return(client->noClientException);
    }
    else
	return (ABadAlloc);
}

int
ProcGetAtomName(client)
    register ClientPtr client;
{
    char *str;
    aGetAtomNameReply reply;
    int len;
    REQUEST(aResourceReq);

    REQUEST_SIZE_MATCH(aResourceReq);
    if (str = NameForAtom(stuff->id)) 
    {
	len = strlen(str);
	reply.type = A_Reply;
	reply.length = (len + 3) >> 2;
	reply.sequenceNumber = client->sequence;
	reply.nameLength = len;
	WriteReplyToClient(client, sizeof(aGetAtomNameReply), &reply);
	(void)WriteToClient(client, len, str);
	return(client->noClientException);
    }
    else 
    { 
	client->errorValue = stuff->id;
	return (ABadAtom);
    }
}

int 
ProcDeleteProperty(client)
    register ClientPtr client;
{
    AC *pAC;
    AudioDevicePtr pDev;
    REQUEST(aDeletePropertyReq);
    int result;
              
    REQUEST_SIZE_MATCH(aDeletePropertyReq);
    UpdateCurrentTime();

    VERIFY_AC(pAC, stuff->ac, client);
    pDev = pAC->aDev;

    if (!pDev)
        return(ABadDevice);
    if (ValidAtom(stuff->property))
    {
	result = DeleteProperty(pDev, stuff->property);
        if (client->noClientException != ASuccess)
            return(client->noClientException);
	else
	    return(result);
    }
    else 
    {
	client->errorValue = stuff->property;
	return (ABadAtom);
    }
}


void
InitProcVectors()
{
    int i;
    for (i = 0; i<256; i++)
    {
	if(!ProcVector[i])
	{
            ProcVector[i] = SwappedProcVector[i] = ProcBadRequest;
	    ReplySwapVector[i] = NotImplemented;
	}
    }
    for(i = ALASTEvent; i < 128; i++)
    {
	EventSwapVector[i] = NotImplemented;
    }
    
}

/**********************
 * CloseDownClient
 *
 *  Client can either mark his resources destroy or retain.  If retained and
 *  then killed again, the client is really destroyed.
 *********************/

void
CloseDownClient(client)
    register ClientPtr client;
{
    if (!client->clientGone)
    {
	/* ungrab server if grabbing client dies */
	if (grabbingClient &&  (onlyClient == client))
	{
	    grabbingClient = FALSE;
	    ListenToAllClients();
	}
    
	if (client->closeDownMode == ADestroyAll)
	{
	    client->clientGone = TRUE;  /* so events aren't sent to client */
	    CloseDownConnection(client);
	    FreeClientResources(client);
	    if (client->index < nextFreeClientID)
		nextFreeClientID = client->index;
	    clients[client->index] = NullClient;
	    if ((client->requestVector != InitialVector) &&
		(--nClients == 0))
#ifdef GPROF
		dispatchException |= DE_TERMINATE;
#else
		dispatchException |= DE_RESET;
#endif
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
 *    Find all clients that are gone and have terminated in ARetainTemporary 
 *    and  destroy their resources.
 *********************/

CloseDownRetainedResources()
{
    register int i;
    register ClientPtr client;

    for (i=1; i<currentMaxClients; i++)
    {
        client = clients[i];
        if (client && (client->closeDownMode == ARetainTemporary)
	    && (client->clientGone))
	    CloseDownClient(client);
    }
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
    aReq data;

    i = nextFreeClientID;
    if (i == MAXCLIENTS)
	return (ClientPtr)NULL;
    clients[i] = client = (ClientPtr)xalloc(sizeof(ClientRec));
    if (!client)
	return (ClientPtr)NULL;
    client->index = i;
    client->sequence = 0; 
    client->clientAsMask = ((AMask)i) << CLIENTOFFSET;
    client->closeDownMode = ADestroyAll;
    client->clientGone = FALSE;
    client->noClientException = ASuccess;
    client->lastACID = INVALID;
#ifdef DEBUG
    client->requestLogIndex = 0;
#endif
    client->requestVector = InitialVector;
    client->osPrivate = ospriv;   ++nClients; /* lcs, tml maybe this is the
						right place. */
    client->swapped = FALSE;
    if (!InitClientResources(client))
    {
	xfree(client);
	return (ClientPtr)NULL;
    }
    data.reqType = 1;
    data.length = (sz_aReq + sz_aConnClientPrefix) >> 2;
    if (!InsertFakeRequest(client, (char *)&data, sz_aReq))
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
    REQUEST(aReq);
    register aConnClientPrefix *prefix;
    int whichbyte = 1;

    prefix = (aConnClientPrefix *)((char *)stuff + sz_aReq);
    if ((prefix->byteOrder != 'l') && (prefix->byteOrder != 'B'))
	return (client->noClientException = -1);
    if (((*(char *) &whichbyte) && (prefix->byteOrder == 'B')) ||
	(!(*(char *) &whichbyte) && (prefix->byteOrder == 'l')))
    {
	client->swapped = TRUE;
	SwapConnClientPrefix(prefix);
    }
    stuff->reqType = 2;
    stuff->length += ((prefix->nbytesAuthProto + 3) >> 2) +
		     ((prefix->nbytesAuthString + 3) >> 2);
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
    register aConnClientPrefix *prefix;
    REQUEST(aReq);

    prefix = (aConnClientPrefix *)((char *)stuff + sz_aReq);
    auth_proto = (char *)prefix + sz_aConnClientPrefix;
    auth_string = auth_proto + ((prefix->nbytesAuthProto + 3) & ~3);
    if ((prefix->majorVersion != A_PROTOCOL) ||
	(prefix->minorVersion != A_PROTOCOL_REVISION))
	reason = "Protocol version mismatch";
    else
	reason = ClientAuthorized(client,
				  (unsigned short)prefix->nbytesAuthProto,
				  auth_proto,
				  (unsigned short)prefix->nbytesAuthString,
				  auth_string);
    if (reason)
    {
	aConnSetupPrefix csp;
	char pad[3];

	csp.success = aFalse;
	csp.lengthReason = strlen(reason);
	csp.length = (csp.lengthReason + 3) >> 2;
	csp.majorVersion = A_PROTOCOL;
	csp.minorVersion = A_PROTOCOL_REVISION;
	if (client->swapped)
	    WriteSConnSetupPrefix(client, &csp);
	else
	    (void)WriteToClient(client, sz_aConnSetupPrefix, (char *) &csp);
        (void)WriteToClient(client, (int)csp.lengthReason, reason);
	if (csp.lengthReason & 3)
	    (void)WriteToClient(client, (int)(4 - (csp.lengthReason & 3)),
				pad);
	return (client->noClientException = -1);
    }

/*    nClients++; */ /* lcs,tml moved to NextAvailableClient. */
    client->requestVector = client->swapped ? SwappedProcVector : ProcVector;
    client->sequence = 0;
    ((aConnSetup *)ConnectionInfo)->ridBase = client->clientAsMask;
    ((aConnSetup *)ConnectionInfo)->ridMask = 0xfffff;
    if (client->swapped)
    {
	WriteSConnSetupPrefix(client, &connSetupPrefix);
	WriteSConnectionInfo(client,
			     (unsigned long)(connSetupPrefix.length << 2),
			     ConnectionInfo);
    }
    else
    {
	(void)WriteToClient(client, sizeof(aConnSetupPrefix),
			    (char *) &connSetupPrefix);
	(void)WriteToClient(client, (int)(connSetupPrefix.length << 2),
			    ConnectionInfo);
    }
    return (client->noClientException);
}

void
SendErrorToClient(
    ClientPtr client,
    unsigned majorCode,
    unsigned short minorCode,
    AID resId,
    int errorCode
	)
{
    aError rep;

    rep.type = A_Error;
    rep.sequenceNumber = client->sequence;
    rep.errorCode = errorCode;
    rep.majorCode = majorCode;
    rep.minorCode = minorCode;
    rep.resourceID = resId;

    WriteEventsToClient (client, 1, (aEvent *)&rep);
}

void
MarkClientException(client)
    ClientPtr client;
{
    client->noClientException = -1;
}


