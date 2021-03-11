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

#include <server/include/dia.h>
#include <server/include/misc.h>
#include <server/include/acstruct.h>
#include <server/include/task.h>
#include "pscodec.h"
#include "physdevice.h" 
#include "audiomaster.h"
#include "audioproto.h"
#include "codec.h"
#include "devtime.h"
#include "buffer.h"

extern char *getenv();

/*
  Returns TRUE if socket is has packets waiting.
*/
static int socket_ready(int s)
{
	int	 mask;
	struct timeval	t;

	mask = 1 << s;
	t.tv_sec = t.tv_usec = 0;

	return select(32, &mask, 0, 0, &t) != 0;
}
		
/*
  Given a just-received packet, save the LineServer's time stamp and a 
  workstation time stamp.  These timestamps are used for estimating the
  LineServer time (see codecGetTime()).
*/
static void UpdateTime(AudioDevicePtr aDev, struct audio_command *c)
{
	lsPhysDevice *pDev = (lsPhysDevice *)aDev->devPtr;
	int	delta;

	gettimeofday(&pDev->wstime, 0);
	aDev->time0 = pDev->lstime = ntohl(c->time);
}

/*
  Processes a valid reply packet (e.g. an incoming packet that has been 
  matched with a request in the exchange hash table):

	- for record requests, copy bytes into buffer and update 
	  TimeRecLastUpdated

	- for play requests, backfill with silence and update 
	  TimeLastUpdated
*/
void ProcessReply(AudioDevicePtr aDev, int len, struct audio_command *rep,
	struct audio_command *req)
{
	int	delta;

	switch(ntohl(rep->opcode)) {
	case AUDIO_RECORD:
		if(ntohl(req->time) == aDev->timeRecLastUpdated) {
			buffer_move(0, aDev->timeRecLastUpdated,
				aDev->recBuf, aDev->recNSamplesBuf,
				rep->data, len, FALSE, 0);
			aDev->timeRecLastUpdated += len;
		}
		break;
	case AUDIO_PLAY:
		delta = ntohl(req->time) - ntohl(rep->time);
#ifdef DEBUG
		if(delta< 0)
			printf("============ Late play request: %d\n", delta);
#endif
		if(ntohl(req->time) == aDev->timeLastUpdated) {
			UpdateTime(aDev, rep);
			aDev->timeLastUpdated += ntohl(rep->param);
		}
#ifdef DEBUG
		 else
			printf("ProcessReply:  out of order play reply\n");
#endif
		break;
	case AUDIO_READ_CODEC:
	case AUDIO_WRITE_CODEC:
		break;
	default:
		printf("ProcessIncoming:  unknown upcode reply %d\n", ntohl(rep->opcode));
		break;
	}
}
			
#define BUFSIZE	2000
/*
  Process any packets on the incoming connection, updating the exchange
  array.
*/
void ProcessIncoming(AudioDevicePtr aDev)
{
	int	s;
	char	buf[BUFSIZE];
	lsPhysDevice *pDev = (lsPhysDevice *)aDev->devPtr;
	pExchangePtr	e;
	struct audio_command *c = (struct audio_command *)buf;
	int	len;

	s = pDev->slave->s;
	while(socket_ready(s)) {
		if((len = recv(s, buf, BUFSIZE, 0)) < sizeof(struct audio_command)) {
			printf("ProcessIncoming:  short packet!\n");
			continue;
		}
		len -= sizeof(struct audio_command);
		e = pDev->elist + c->sequence % EXCHANGESIZE;
		if(e->packet != NULL && e->packet->sequence == c->sequence) {
			ProcessReply(aDev, len, c, e->packet);
			free(e->packet);
			e->packet = NULL;
			e->retry = -1;
		}	
	}
}

/*
  Send a request to a LineServer, registering the request in the exchange 
  array.  'len' is the length of the audio command's data segment.  If retry
  is true, a retry task will be queued for this packet.

  NOTE:  the sender of a packet must leave enough space in the allocated
  packet for the reply.
*/
void SendRequest(AudioDevicePtr aDev, struct audio_command *c, int len,
	int retry)
{
	TaskPtr newTask;
	void TimeOutTask(TaskPtr oldTask);
	lsPhysDevice *pDev = (lsPhysDevice *)aDev->devPtr;
	pExchangePtr	e;

	e = pDev->elist + c->sequence % EXCHANGESIZE;
	if(e->packet != NULL) {
		free(e->packet);
#ifdef DEBUG
		printf("SendRequest:  had to free someone.\n");
#endif
	}
	e->packet = c;
	e->len = len;
	e->retry = 0;

	if(retry) {				/* queue a timeout task */
		newTask = NewTask();
		newTask->aDev = aDev;
		newTask->time = 0;
		newTask->p = (pointer) e;
		AddTask(TimeOutTask, newTask, 100);
	}
        if(send(pDev->slave->s, c, sizeof(struct audio_command) + len, 0) !=
		sizeof(struct audio_command) + len)
		FatalError("SendRequest:  send to socket failed!\n");
}

/*
  Called to handle packet timeouts and retries.  If no reply has been 
  received, resend packet.
*/
static void TimeOutTask(TaskPtr oldTask)
{
	pExchangePtr e=(pExchangePtr)oldTask->p;
	AudioDevicePtr	aDev = oldTask->aDev;
	lsPhysDevice *pDev = (lsPhysDevice *)aDev->devPtr;
	TaskPtr	newTask;

	ProcessIncoming(aDev);

	if(e->retry != -1) {		/* not acked */
		if(e->retry++ > 3)
			FatalError("TimeOutTask:  retry count exceeded.  Dead LineServer?\n");

		if(e->packet == NULL)	/* ? */
			return;

		if(send(pDev->slave->s, e->packet, sizeof(struct audio_command) + e->len, 0) !=
			sizeof(struct audio_command) + e->len)
				FatalError("SendRequest:  send to socket failed!\n");

		newTask = NewTask();		/* resubmit */
		*newTask = *oldTask;
		AddTask(TimeOutTask, newTask, 100);
	}
}
		
/* ------------------------------------------------------------------------- */
			
#define	LIMIT(low, val, high) \
	  if (val < (low)) val = (low); \
	  else \
	  if ((val) > (high)) val = (high);

/*
  Get the LineServer's local time.  Use the workstation's current time,
  the workstation's timestamp of the last packet received, and the LineServer
  time of that last packet to make an estimate of the LineServer's time.
*/
static ATime
codecGetTime(AudioDevicePtr aDev)
{
	int	t;
	lsPhysDevice *pDev = (lsPhysDevice *)aDev->devPtr;
	struct timeval	ctime;
	ATime	dif;

	ProcessIncoming(aDev);

/* compute time difference, in samples */
	if(gettimeofday(&ctime, 0) == -1)
		FatalError("codecGetTime:  gettimeofday() returned error.\n");

	ctime.tv_sec -= pDev->wstime.tv_sec;
	ctime.tv_usec -= pDev->wstime.tv_usec;
	dif = ctime.tv_sec * 8000 + (ctime.tv_usec / 125);
	return (aDev->time0 = pDev->lstime + dif);
}
	
/*
  Play a sample on the LineServer.  Returns the number of bytes played.
*/
int codecPlay(ATime ptime, unsigned char *p, int plen, ACPtr ac)
{
	AudioDevicePtr aDev=ac->aDev;
	lsPhysDevice *pDev = (lsPhysDevice *)aDev->devPtr;
	int rlen = plen;		/* local:  bytes left to play */
	int len = 0;			/* local:  bytes played */
	ATime time = ptime;
	ATime endBufferTime;
	int	future;
	int	delta;
	int	edelta;
	struct audio_command *c;

	codecGetTime(aDev);		/* update the time */

	future = FUTURE(time, aDev->time0);
	delta = DELTA(future, time, aDev->time0);

	if(!future) {			/* truncate too far in past */
#ifdef DEBUG
		printf("+++++ Too far in past!\n");
#endif
		time += delta;
		p += delta;
		rlen -= delta;
		len += delta;
	} else {			/* truncate too far in future */
		endBufferTime = pDev->endPlayTime + aDev->playNSamplesBuf;
		edelta = endBufferTime - time;
		rlen = min(edelta, rlen);
	}
	if(rlen <= 0)
		return len;

/* copy rest of request into outgoing play buffer */
	buffer_move(0, time, aDev->playBuf, aDev->playNSamplesBuf, p, rlen,
			!ac->preempt, ac->playGain);
	len += rlen;

/* move update point back if necessary */
	delta = pDev->lastPotentialUpdate - time;
	if(delta > 0) {
		delta = min(delta, rlen);
#ifdef ACP
		printf("Resending play request (delta = %d, before = %d).\n", 
			delta, time - aDev->time0);
#endif
		if(delta > MAX_SIZE) {
			aDev->timeLastUpdated = time;
			delta = MAX_SIZE;
			pDev->lastPotentialUpdate = aDev->timeLastUpdated + delta;
		}
		c = AudioPlayReq(pDev->slave, time, delta);
		memcpy(c->data, p, delta);
		SendRequest(aDev, c, delta, FALSE);
	}
#ifdef ACP
	printf("codecPlay:  %d at %d (ls time %d, timeLastUpdated %d).\n", 
		len, time, aDev->time0, aDev->timeLastUpdated);
#endif
	return len;
}

/*
  Retrieves record bytes from the AudioFile's local buffer.  Returns 
  number of bytes recorded.
*/
int codecRecord(ATime rtime, unsigned char *dp, int rlen, ACPtr ac)
{
	int	t, i;
	AudioDevicePtr aDev=ac->aDev;
	lsPhysDevice *pDev = (lsPhysDevice *)aDev->devPtr;
	ATime	begTime;
	ATime	time = rtime;
	int	delta;
	int	future;
	int	remlen = rlen;
	int	len;

	codecGetTime(aDev);		/* update */

	if(FUTURE(time, aDev->timeRecLastUpdated))	/* start in future */
		return 0;

	begTime = aDev->timeRecLastUpdated - aDev->recNSamplesBuf;
	future = FUTURE(time, begTime);
	delta = DELTA(future, time, begTime);

/* part of request too far in the past:  return silence */
	if(!future) {
		len = MIN(remlen, delta);
		memset(dp, SILENCE, len);
		dp += len;
		time += len;
		remlen -= len;
		if(remlen <= 0)
			return rlen;
	}

/* copy bytes from the AudioFile's local buffer */
	delta = DELTA(1, time, begTime);
	len = MIN(remlen, aDev->recNSamplesBuf - delta);

	buffer_move(1, time, aDev->recBuf, aDev->recNSamplesBuf, dp, len,
			FALSE, 0);
	remlen -= len;
#ifdef ACP
	printf("codecRecord:  %d at %d (ls time %d)\n", 
		rlen - remlen, rtime, aDev->time0);
#endif
	return rlen - remlen;
}

/*
 * Codec Device Control, Input/Output Selection and Gain.
 */

#define	MAXINPUTS	2
#define	MAXOUTPUTS	2
#define	TOMASK(x)	((1<<(x))-1)
#define	OUTMASK		TOMASK(MAXOUTPUTS)
#define	INMASK		TOMASK(MAXINPUTS)

/*
  Write a one byte codec register.
*/
static void codec_write_1(AudioDevicePtr aDev, int reg, char data1)
{
	lsPhysDevice *pDev=(lsPhysDevice *)aDev->devPtr;
	struct audio_command	*c;

	c = AudioWriteCodecReq(pDev->slave, reg, 1, &data1);
	SendRequest(aDev, c, 1, TRUE);
}

/*
  Write a two byte codec register.
*/
static void codec_write_2(AudioDevicePtr aDev, int reg, char data1, char data2)
{
	lsPhysDevice *pDev=(lsPhysDevice *)aDev->devPtr;
	struct audio_command	*c;
	char	data[2];

	data[0] = data1;
	data[1] = data2;
	c = AudioWriteCodecReq(pDev->slave, reg, 2, data);
	SendRequest(aDev, c, 2, TRUE);
}

/*
 * Set output according to nmask.
 * Return a bit mask describe which outputs are actually enabled and
 * the previous mask.
 *
 *  Output #0: Loud Speaker
 *  Output #1: Earphone
 */ 
void
codecChangeOutput(AudioDevicePtr aDev, int onoff, 
	int nmask, int *omaskp, int *amaskp)
{
	lsPhysDevice *pDev=(lsPhysDevice *)aDev->devPtr;

/* compute old mask */
	if(pDev->map_mmr2 & MAP_MMR2_BITS_LS)
		*omaskp = 1;
	else
		*omaskp = 2;
	if(pDev->map_mmr3 & MAP_MMR3_BITS_BOTH)
		*omaskp = 3;

/* compute new mask */
	if(onoff)
		*amaskp = *omaskp | (nmask & TOMASK(aDev->numberOfOutputs));
	else
		*amaskp = *omaskp & ~(nmask & TOMASK(aDev->numberOfOutputs));


/* update CODEC MMR registers from new mask */
	if(*amaskp == 3)		/* both on */
		pDev->map_mmr3 |= MAP_MMR3_BITS_BOTH;
	else {
		pDev->map_mmr3 &= ~MAP_MMR3_BITS_BOTH;
		if(*amaskp & 1)
			pDev->map_mmr2 |= MAP_MMR2_BITS_LS;
		else
			pDev->map_mmr2 &= ~MAP_MMR2_BITS_LS;
	}
	codec_write_1(aDev, MAP_MMR2, pDev->map_mmr2);
	codec_write_1(aDev, MAP_MMR3, pDev->map_mmr3);
#ifdef DEBUG
	printf("codecChangeOutput:  new mask %d, onoff %d, nmask %d\n", 
		*amaskp, onoff, nmask);
#endif
}

/*
  Set inputs according to the mask
*/ 
void
codecChangeInput(AudioDevicePtr aDev, int onoff, 
	int nmask, int *omaskp, int *amaskp)
{
	lsPhysDevice *pDev=(lsPhysDevice *)aDev->devPtr;

/* compute old mask */
	if(pDev->map_mmr3 & MAP_MMR3_BITS_MUTE)
		*omaskp = 0;
	else if(pDev->map_mmr2 & MAP_MMR2_BITS_AINB)
		*omaskp = 2;
	else
		*omaskp = 1;

/* compute new mask */
	if(onoff)
		*amaskp = *omaskp | (nmask & TOMASK(aDev->numberOfInputs));
	else
		*amaskp = *omaskp & ~(nmask & TOMASK(aDev->numberOfInputs));

	if(*amaskp == 3)		/* both can't be on */
		*amaskp = nmask;

/* update CODEC MMR registers from new mask */
	if(*amaskp == 0)		/* everything muted */
		pDev->map_mmr3 |= MAP_MMR3_BITS_MUTE;
	else {
		pDev->map_mmr3 &= ~MAP_MMR3_BITS_MUTE;
		if(*amaskp & 1)
			pDev->map_mmr2 &= ~MAP_MMR2_BITS_AINB;
		else
			pDev->map_mmr2 |= MAP_MMR2_BITS_AINB;
	}
	codec_write_1(aDev, MAP_MMR2, pDev->map_mmr2);
	codec_write_1(aDev, MAP_MMR3, pDev->map_mmr3);

#ifdef DEBUG
	printf("codecChangeInput: new mask %d\n", *amaskp);
#endif
}

#define	MAXOUTPUTGAIN	18
#define	MINOUTPUTGAIN	-10

int
codecQueryOutputGain(AudioDevicePtr aDev, int *minp, int *maxp)
{
	lsPhysDevice *pDev=(lsPhysDevice *)aDev->devPtr;

	if(minp != NULL)
		*minp = MINOUTPUTGAIN;

	if(maxp != NULL)
		*maxp = MAXOUTPUTGAIN;

	return (int) pDev->ogain;
}

int
codecSelectOutputGain(AudioDevicePtr aDev, int gdB)
{
	lsPhysDevice *pDev=(lsPhysDevice *)aDev->devPtr;
	static int ger[] = {	/* -10 dB to +18 dB */
		0xaaaa, 0x79ac, 0x4199, 0x9cde, 0x749c, 0x6aae, 0xabdf,
		0x64ab, 0x2abd, 0x5cce, 0x0099, 0x43dd, 0x52ef, 0x5542,
		0x31dd, 0x431f, 0x40dd, 0x440f, 0x311f, 0x10dd, 0x410f,
		0x600b, 0x4210, 0x110f, 0x7200, 0x2110, 0x2200
	};
	int	r;

	if(gdB < MINOUTPUTGAIN)
		gdB = MINOUTPUTGAIN;
	if(gdB > MAXOUTPUTGAIN)
		gdB = MAXOUTPUTGAIN;

	r = ger[gdB - MINOUTPUTGAIN];
	codec_write_2(aDev, MAP_GER, r >> 8, r & 0xff);
	pDev->ogain = gdB;
	pDev->map_mmr1 |= MAP_MMR1_BITS_GER;
	codec_write_1(aDev, MAP_MMR1, pDev->map_mmr1);
#ifdef ACP
	printf("codecSelectOutputGain:\n");
#endif
	return 0;
}


#define	MAXINPUTGAIN	24
#define	MININPUTGAIN	0

/*
  Retrieve the current setting of the CODEC input gain.
*/
int
codecQueryInputGain(AudioDevicePtr aDev, int *minp, int *maxp)
{
	int	gain;

	lsPhysDevice *pDev=(lsPhysDevice *)aDev->devPtr;

	if(minp != NULL)
		*minp = MININPUTGAIN;

	if(maxp != NULL)
		*maxp = MAXINPUTGAIN;

	switch(pDev->map_mmr3 & MAP_MMR3_BITS_GA) {
		case MAP_MMR3_BITS_GA0:
			return 0;
		case MAP_MMR3_BITS_GA6:
			return 6;
		case MAP_MMR3_BITS_GA12:
			return 12;
		case MAP_MMR3_BITS_GA18:
			return 18;
		case MAP_MMR3_BITS_GA24:
			return 24;
		default:
			return 0;
	}
}

/*
  Set the CODEC input gain to the specified value.
*/
int
codecSelectInputGain(AudioDevicePtr aDev, int gdB)
{
	float	aga;

	lsPhysDevice *pDev=(lsPhysDevice *)aDev->devPtr;

	pDev->map_mmr3 &= ~MAP_MMR3_BITS_GA;
	if(gdB < 6)
		pDev->map_mmr3 |= MAP_MMR3_BITS_GA0;
	else if(gdB < 12)
		pDev->map_mmr3 |= MAP_MMR3_BITS_GA6;
	else if(gdB < 18)
		pDev->map_mmr3 |= MAP_MMR3_BITS_GA12;
	else if(gdB < 24)
		pDev->map_mmr3 |= MAP_MMR3_BITS_GA18;
	else
		pDev->map_mmr3 |= MAP_MMR3_BITS_GA24;

	codec_write_1(aDev, MAP_MMR3, pDev->map_mmr3);
	return 0;
}


/* -------------------------------------------------------------------- */

ABool
codecCreateAC(ACPtr pAC)
{
	if((pAC->funcs = (ACFuncs *)AFalloc(sizeof(ACFuncs)))==NULL){
		return FALSE;
	}
	if((pAC->ops = (ACOps *)AFalloc(sizeof(ACOps)))==NULL){
		xfree(pAC->funcs);
		return FALSE;
	}
	pAC->funcs->ChangeAC = codecChangeAC;
	pAC->funcs->DestroyAC = codecDestroyAC;

	pAC->ops->ConvertPlay = codecPlay;
	pAC->ops->ConvertRec = codecRecord;
	return TRUE;
}

void
codecDestroyAC(ACPtr pAC)
{
	xfree(pAC->funcs);
	xfree(pAC->ops);
}

void
codecChangeAC(void *ptr, ACPtr pAC, int mask)
{
/*
	LIMIT(GAIN_MIN, pAC->playGain, GAIN_MAX);	
*/
}

/*
  Initialize the codec structure & device.
*/
ABool
codecInit(AudioDevicePtr aDev) 
{
	lsPhysDevice *pDev=(lsPhysDevice *)aDev->devPtr;
	TaskPtr		t;
	extern void UpdateTask(TaskPtr oldTask);
	int	i;

	/*
	 * Fill in AudioDevice Structure.
	 */

	aDev->time0 = 0;		/* The beginning of time, 	*/
	aDev->oldDevTime = 0;		/* the first time.		*/
	aDev->dsptime = 0;

	aDev->ChangeOutput = codecChangeOutput;
	aDev->ChangeInput = codecChangeInput;
	aDev->QueryOutputGain = codecQueryOutputGain;
	aDev->QueryInputGain = codecQueryInputGain;
	aDev->SelectOutputGain = codecSelectOutputGain;
	aDev->SelectInputGain = codecSelectInputGain;

	aDev->CreateAC = codecCreateAC;		/* 			*/
	aDev->GetTime = codecGetTime;		/* Gets device time.	*/

	aDev->playSampleFreq = SAMPLEFREQ;	/* Describes the play 	*/
	aDev->playBufType = MU255; 		/* hardware.		*/
	aDev->playNchannels = 1; 
	aDev->playNSamplesBuf = TIMEPLAYBUF; 

	aDev->recSampleFreq = SAMPLEFREQ; 	/* Describe the record 	*/
	aDev->recNchannels = 1; 		/* hardware.		*/
	aDev->recBufType = MU255; 
	aDev->recNSamplesBuf = TIMERECBUF; 

	aDev->numberOfInputs = 1;		/* Mic */
	aDev->numberOfOutputs = 2;		/* Ear and monitor. */
	aDev->inputsFromPhone = 0x0000;		/* none */
	aDev->outputsToPhone = 0x0000;		/* none */

/* allocate play and record buffers */
	if((aDev->recBuf = (pointer)AFalloc(aDev->recNSamplesBuf)) == NULL)
		return FALSE;

	if((aDev->playBuf = (pointer)AFalloc(aDev->playNSamplesBuf)) == NULL)
		return FALSE;
	memset(aDev->playBuf, SILENCE, aDev->playNSamplesBuf);

/* allocate packet exchange structure */
	if((pDev->elist = (pExchangePtr)AFalloc(EXCHANGESIZE * sizeof(pExchange))) == NULL)
		return FALSE;
	for(i=0; i<EXCHANGESIZE; i++)
		pDev->elist[i].packet = NULL;

/* physical device */
	AudioReadCodec(pDev->slave, MAP_MMR1, 1, &pDev->map_mmr1);
	AudioReadCodec(pDev->slave, MAP_MMR2, 1, &pDev->map_mmr2);
	AudioReadCodec(pDev->slave, MAP_MMR3, 1, &pDev->map_mmr3);

	AudioTime(pDev->slave, &i);
	aDev->time0 = i;

/* set up the update task */
	t = NewTask();
	t->aDev = aDev;
	t->time = 0;
	aDev->timeLastUpdated = aDev->timeRecLastUpdated = aDev->time0;
	pDev->endPlayTime = pDev->lastPotentialUpdate = aDev->time0;
	AddTask(UpdateTask, t, 0);
	
	codecChangeOutput(aDev, 1, 3, &i, &i);	/* all outputs */
	codecChangeInput(aDev, 1, 1, &i, &i);	/* input #0 */
	codecSelectOutputGain(aDev, 0);		/* 0 dB */

	return TRUE;
}

/*
  The update task:  called at regular intervals to copy bytes to/from the 
  AudioFile's buffers and the LineServer's buffers.
*/
static void UpdateTask(TaskPtr oldTask)
{
	TaskPtr	newTask = NewTask();
	AudioDevicePtr aDev=oldTask->aDev;
	lsPhysDevice *pDev=(lsPhysDevice *)aDev->devPtr;
	struct audio_command *c;
	int	delta;

	codecGetTime(aDev);			/* update */

/* backfill silence into play buffer */
	delta = aDev->time0 - pDev->endPlayTime;
	if(delta > 0) {
		delta = min(delta, aDev->playNSamplesBuf);
		buffer_set(pDev->endPlayTime, aDev->playBuf, aDev->playNSamplesBuf,
			delta, SILENCE);
	}
	pDev->endPlayTime = aDev->time0;

/* send out a record request */
	c = AudioRecReq(pDev->slave, aDev->timeRecLastUpdated, MAX_SIZE);
	SendRequest(aDev, c, 0, FALSE);

/* send out a play request */
#ifdef ACP
	delta = aDev->time0 - aDev->timeLastUpdated;
	if(delta > 0) {
		printf("death:  time0 past last update!\n");
		printf("        time0 = %d, last update = %d, delta = %d\n",
			aDev->time0, aDev->timeLastUpdated, delta);
	}
#endif
	c = AudioPlayReq(pDev->slave, aDev->timeLastUpdated, UPDATE_SIZE);
	buffer_move(1, aDev->timeLastUpdated, aDev->playBuf,
		aDev->playNSamplesBuf, c->data, UPDATE_SIZE,
		FALSE, 0);
	SendRequest(aDev, c, UPDATE_SIZE, FALSE);
	pDev->lastPotentialUpdate = aDev->timeLastUpdated + UPDATE_SIZE;

/* resubmit a new update task */
	*newTask = *oldTask;
	AddTask(UpdateTask, newTask, UPDATE_INTERVAL);
}
	
