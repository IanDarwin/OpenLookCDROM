/***********************************************************
Copyright 1993 by Stichting Mathematisch Centrum, Amsterdam, The Netherlands.

                        All Rights Reserved

Permission to use, copy, modify, and distribute this software and its 
documentation for any purpose and without fee is hereby granted, 
provided that the above copyright notice appear in all copies and that
both that copyright notice and this permission notice appear in 
supporting documentation, and that the names of Stichting Mathematisch
Centrum or CWI not be used in advertising or publicity pertaining to
distribution of the software without specific, written prior permission.

STICHTING MATHEMATISCH CENTRUM DISCLAIMS ALL WARRANTIES WITH REGARD TO
THIS SOFTWARE, INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND
FITNESS, IN NO EVENT SHALL STICHTING MATHEMATISCH CENTRUM BE LIABLE
FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT
OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
******************************************************************/

/* SGI Indigo interface, called from dda.c (or through pointers) */


#include <server/include/os.h>
#include <server/include/dia.h>
#include <server/include/misc.h>
#include <server/include/task.h>
#include <server/include/audiodev.h>
#include <server/include/acstruct.h>
#include <AFUtils.h>

#undef fabs /* Disable definition of fabs from <server/misc.h> */
#include <math.h>

/* SGI's Audio Library interface (avoid name conflict with AF's <audio.h>) */
#include "/usr/include/audio.h"

/* Tunable parameters */
#define MSQUEUE 100		/* Amount of hardware queue to allocate */
#define MSDELAY 20		/* Delay between two updates */
#define MSTOLERANCE 25		/* Maximum drift between record/play */
#define PLAYLENGTH (1024*256)	/* Play buffer length */
#define RECLENGTH PLAYLENGTH	/* Record buffer length */


/* Sampling rate, set by -rate option */
int sgi_rate = 8000;


/* Supported sampling rates, used by -rate option parsing */
int sgi_validrates[] = {
	8000, 11025, 16000, 22050, 32000, 44100, 48000,
};
int sgi_n_validrates = sizeof sgi_validrates / sizeof sgi_validrates[0];


/* dB <--> multiplication factor conversions */
#define DB2FACT(dB) (exp((dB)*(M_LN10/20.0)))
#define FACT2DB(fact) (log(value)*(20.0/M_LN10))


/* Clip a value within bounds (don't use in if or else branch!) */
#define	LIMIT(low, val, high) \
	  if (val < (low)) val = (low); \
	  else \
	  if ((val) > (high)) val = (high);


/* Forward declared internal functions */
static void sgiChangeAC();
static ABool sgiCreateAC();
static void sgiDestroyAC();
static pointer sgiInitPrivate();
static void sgiSetRate();
static void sgiUpdateInit();
static void sgiUpdateTask();
static void sgiUpdate();
static void sgiChangeOutput();
static void sgiChangeInput();
static void sgiChangePassThrough();
static int sgiQueryOutputGain();
static int  sgiSelectOutputGain();
static int sgiQueryInputGain();
static int  sgiSelectInputGain();
static int sgiRecord();
static int sgiPlay();
static ATime sgiGetTime();


/* Initialize a device */
int
sgiDevInit(aDev)
	AudioDevicePtr aDev;
{
	static pointer private = NULL;

	if (private == NULL) {
		private = sgiInitPrivate(sgi_rate);
		if (private == NULL)
			return FALSE;
	}


	aDev->devPtr = NULL;
	aDev->privPtr = private;

	aDev->time0 = 0;		/* The beginning of time, 	*/
	aDev->oldDevTime = 0;		/* the first time.		*/
	aDev->dsptime = 0;

	aDev->ChangeOutput = sgiChangeOutput;
	aDev->ChangeInput = sgiChangeInput;
	aDev->ChangePassThrough = sgiChangePassThrough;
	aDev->QueryOutputGain = sgiQueryOutputGain;
	aDev->QueryInputGain = sgiQueryInputGain;
	aDev->SelectOutputGain = sgiSelectOutputGain;
	aDev->SelectInputGain = sgiSelectInputGain;

	aDev->CreateAC = sgiCreateAC;		/* 			*/
	aDev->GetTime = sgiGetTime;		/* Gets device time.	*/

	/* Describe the play hardware */
	aDev->numberOfOutputs = 1;
	aDev->playSampleFreq = sgi_rate;
	aDev->playBufType = LIN16;
	aDev->playNchannels = 2;
	aDev->playNSamplesBuf = PLAYLENGTH;

	/* Describe the record hardware */
	aDev->numberOfInputs = 3;
	aDev->recSampleFreq = sgi_rate;
	aDev->recBufType = LIN16;
	aDev->recNchannels = 2;
	aDev->recNSamplesBuf = 0;

	aDev->playBuf = NULL;
	aDev->recBuf = NULL;

	return TRUE;
}


/* Here's how it works (for playing, anyway).

   The model that the server presents to the client is an infinite
   buffer of samples, indexed by clock value.  On each clock tick the
   clock is incremented by one and the corresponding sample is sent to
   the output device.  Initially, the entire buffer is filled with
   silence.  The Play command, given a clock value and a number of
   samples, simply mixes the samples into the buffer, starting at the
   sample indexed by the given clock value.  (Obviously, if this
   affects samples that have already been sent to the output device
   this has no audible effect.)

   Every implementation has to cope with a buffer of finite size.  This
   is done by using the buffer as a "window" on time which moves as
   the clocks advances.  Attempting to play samples far in the future
   will hold the samples in another buffer until the window has
   reached the desired time.  Attempting to play samples in the past
   results in throwing the samples away.  (Clock values are also
   represented as 32-bit numbers, making it impossible to distinguish
   clock values that are multiple of 2**32 ticks apart; this is solved
   by interpreting clock values as closest to the current time.)

   The SGI audio hardware interface has a configurable queue.  Once
   sample values have been placed into this queue, they cannot be
   modified.  The SGI also supports multiple queues which allow
   hardware mixing, but there is no way to control the relative times
   at which values are mixed, so we don't use this facility (anyway
   there is a too restrictive limit to the number of channels that
   can be mixed in this way).  Instead, we use a single hardware queue
   which we configure to be as short as we dare.  An "update task"
   is executed at frequent intervals and fills the hardware queue with
   the next set of samples from the server buffer (which is used as
   a circular queue).  The space in the server buffer thus freed is
   filled with silence so it is ready for mixing in new samples.

   The implementation doesn't use any of the buffers, timers,
   pointers and counters in the AudioDevice structure, instead all
   administration is kept in its own private structure.  The function
   sgiUpdateTime(aDev) must be called to update the time0 field from
   the private structure (for sgiGetTime).

   To complicate matters, the current time as presented to the client
   includes the samples that have already been transferred to the
   hardware queue (to approximate "real time" as closely as possible).
   This means that samples played in the very near future cannot be
   mixed in (since the corresponding slots from the idealized buffer
   have already been transferred to the hardware buffer).  For most
   clients this will mean at most that a tiny fraction of sound at the
   beginning will not be made heard -- this is a possibility anyway
   because of network delays.

   For recording things go rather similar, except the data movement
   direction is reversed, and the server's buffer holds the last so
   many samples *preceding* the current time.  Requesting recorded
   samples from far in the past will return silence because the
   server's circular buffer has lost the data.  (While this may seem
   asymmetric, it is not: both recording and playback must be done in
   a timely fashion.)

   There is a complication in the interface: the model of a device
   has a single clock for recording and playback, while our
   implementation in fact has different clocks.  Under normal
   circumstances these will run at the same speed, but once queues
   start under- or overflowing they may drift apart.  We compensate
   for this drift by comparing the two clocks and dropping or
   inserting samples when the difference becomes too large.
*/

/* Number of channels (2 == stereo == left+right) */
#define NCH 2			/* Don't change! Hardcoded at many places */

/* Private structure */
struct private {
	long dev;		/* For ALgetparams/ALsetparams */
	int playQsize;		/* AL queue sizes */
	int recQsize;
	int playRate;		/* Sampling rates */
	int recRate;
	ALport playPort;	/* Audio Library ports */
	ALport recPort;
	short *playBuf;		/* Buffers */
	short *recBuf;
	int playLength;		/* Length buffers, in samples */
	int recLength;
	int playIndex;		/* Current index into buffers */
	int recIndex;
	int playCycles;		/* Incremented each time index wraps */
	int recCycles;
};

/* Get private struct from aDev struct */
#define PRIV(aDev) ((struct private *)(aDev->privPtr))

/* Get device number from aDev struct */
#define DEV(aDev) (PRIV(aDev)->dev)

/* Get the play and record times from private struct */
#define PLAYTIME(p) (p->playIndex - ALgetfilled(p->playPort) + \
		     p->playCycles * p->playLength)
#define RECTIME(p) (p->recIndex + ALgetfilled(p->recPort) + \
		    p->recCycles * p->recLength)


/* Initialize private structure (call only once!) */

static pointer
sgiInitPrivate(rate)
{
	struct private *p;
	ALconfig c;
	long qsize;

	/* XXX This function doesn't clean up if it fails */

	if ((p = (struct private *) AFalloc(sizeof(struct private))) == NULL)
		return NULL;

	p->dev = AL_DEFAULT_DEVICE;
	p->playRate = rate;
	p->recRate = rate;

	p->playLength = PLAYLENGTH;
	p->recLength = RECLENGTH;
	p->playBuf = (short *) AFalloc(sizeof(short) * NCH * p->playLength);
	p->recBuf = (short *) AFalloc(sizeof(short) * NCH * p->recLength);
	if (p->playBuf == NULL || p->recBuf == NULL)
		return NULL;
	p->playIndex = 0;
	p->recIndex = 0;
	p->playCycles = 0;
	p->recCycles = 0;
	memset(p->playBuf, 0, p->playLength*NCH*sizeof(short));
	memset(p->recBuf, 0, p->recLength*NCH*sizeof(short));

	sgiSetRate(p);

	if ((c = ALnewconfig()) == NULL)
		return NULL;
	ALsetwidth(c, AL_SAMPLE_16);
	ALsetchannels(c, AL_STEREO);
	qsize = rate*NCH*MSQUEUE/1000;
	if (qsize < 1024)
		qsize = 1024; /* Minimal queue size */
	ALsetqueuesize(c, qsize);
	p->playQsize = p->recQsize = qsize;
	p->playPort = ALopenport("AudioFile play", "w", c);
	p->recPort = ALopenport("AudioFile record", "r", c);
	ALfreeconfig(c);
	if (p->playPort == NULL || p->recPort == NULL)
		return NULL;

	sgiUpdateInit(p); /* Start the update task */

	return (pointer)p;
}

static void
sgiSetRate(p)
	struct private *p;
{

	long params[4];
	params[0] = AL_OUTPUT_RATE;
	params[1] = p->playRate;
	params[2] = AL_INPUT_RATE;
	params[3] = p->recRate;
	ALsetparams(p->dev, params, 4);
}


/* Update task management */

static void
sgiUpdateInit(p)
	struct private *p;
{
	TaskPtr	newTask;
	sgiUpdate(p);
	newTask = NewTask();
	newTask->p = (pointer)p;
	AddTask(sgiUpdateTask, newTask, MSDELAY);
}

static void
sgiUpdateTask(oldTask)
	TaskPtr oldTask;
{
	TaskPtr	newTask;
	sgiUpdate((struct private *)oldTask->p);
	newTask = NewTask();
	newTask->p = oldTask->p;
	AddTask(sgiUpdateTask, newTask, MSDELAY);
}

static void
sgiUpdate(p)
	struct private *p;
{
	int avail;
	ATime playtime = PLAYTIME(p);
	ATime rectime = RECTIME(p);
	int diff = playtime - rectime;

	/* Need to correct clock differences? */
	if (diff >= MSTOLERANCE * sgi_rate / 1000 ||
	    -diff >= MSTOLERANCE * sgi_rate / 1000) {
		if (p->playCycles)
			printf("sgiUpdate: resynchronize: "
			       "playtime %u, rectime %u, diff %d, tol. %d\n",
			       playtime, rectime, diff,
			       MSTOLERANCE * sgi_rate / 1000);

		sgiSetRate(p);

		if (diff > 0) {
			/* Player has run ahead of recorder, insert some
			   silence in record buffer */
			if (p->playCycles)
				printf("sgiUpdate: "
				       "silence fill %d rec samples\n",
				       diff);
			while (--diff >= 0) {
				p->recBuf[p->recIndex*NCH] = 0;
				p->recBuf[p->recIndex*NCH+1] = 0;
				if (++p->recIndex == p->recLength) {
					p->recIndex = 0;
					p->recCycles++;
				}
			}
		}
		else {
			diff = -diff;
			/* Recorder has run ahead of player, insert some
			   silence in play buffer */
			if (p->playCycles)
				printf("sgiUpdate: "
				       "silence fill %d play samples\n",
				       diff);
			while (--diff >= 0) {
				p->playBuf[p->playIndex*NCH] = 0;
				p->playBuf[p->playIndex*NCH+1] = 0;
				if (++p->playIndex == p->playLength) {
					p->playIndex = 0;
					p->playCycles++;
				}
			}
		}
	}

	/* Update play port */
	avail = ALgetfillable(p->playPort) / NCH;
	while (avail > 0) {
		if (p->playCycles > 0 && ALgetfilled(p->playPort) == 0)
			printf("sgiUpdate: play late at %d:%d\n",
			       p->playCycles, p->playIndex);
		if (p->playIndex+avail > p->playLength)
			avail = p->playLength - p->playIndex;
		ALwritesamps(p->playPort,
			     p->playBuf+p->playIndex*NCH, avail*NCH);
		memset(p->playBuf+p->playIndex*NCH, 0,avail*sizeof(short)*NCH);
		p->playIndex += avail;
		avail = 0;
		if (p->playIndex == p->playLength) {
			p->playCycles++;
			p->playIndex = 0;
			avail = ALgetfillable(p->playPort) / NCH;
		}
	}

	/* Update record port */
	avail = ALgetfilled(p->recPort) / NCH;
	while (avail > 0) {
		if (p->recCycles > 0 && ALgetfillable(p->recPort) == 0)
			printf("sgiUpdate: rec late at %d:%d\n",
			       p->recCycles, p->recIndex);
		if (p->recIndex+avail > p->recLength)
			avail = p->recLength - p->recIndex;
		ALreadsamps(p->recPort,
			    p->recBuf+p->recIndex*NCH, avail*NCH);
		p->recIndex += avail;
		avail = 0;
		if (p->recIndex == p->recLength) {
			p->recCycles++;
			p->recIndex = 0;
			avail = ALgetfilled(p->recPort) / NCH;
		}
	}
}


/* Copy the private time to the pseudo device */

static void
sgiUpdateTime(aDev)
	AudioDevicePtr aDev;
{
	struct private *p = (struct private *)aDev->privPtr;
	ATime playtime = PLAYTIME(p);
	ATime rectime = RECTIME(p);
	int diff = playtime - rectime;
	if (diff < -2500 || diff > 2500) {
		printf("sgiUpdateTime: " /* string literal concatenation */
		       "big time diff: playtime %u, rectime %u, diff %d\n",
		       playtime, rectime, diff);
	}
	if (playtime >= rectime)
		aDev->time0 = playtime;
	else
		aDev->time0 = rectime;
}


/* Connection management */

static void
sgiChangeInput(aDev, onoff, nmask, omaskp, amaskp)
	AudioDevicePtr aDev;
	int onoff;
	int nmask, *omaskp, *amaskp;
{
	int cmask;
	int i;
	long params[2];
	params[0] = AL_INPUT_SOURCE;
	ALgetparams(DEV(aDev), params, 2);
	cmask = 1<<params[1];
	*omaskp = cmask;
	/* Can only select exactly one input device */
	if (onoff) {
		for (i = 0; i < 3; i++) {
			if (nmask & (1<<i))
				params[1] = i;
		}
		ALsetparams(DEV(aDev), params, 2);
		cmask = 1<<params[1];
	}
	*amaskp = cmask;
}

static void
sgiChangeOutput(aDev, onoff, nmask, omaskp, amaskp)
	AudioDevicePtr aDev;
	int onoff;
	int nmask, *omaskp, *amaskp;
{
	/* Only one output */
	*omaskp = *amaskp = 1;
}

static void
sgiChangePassThrough(aDev, onoff, nmask, omaskp, amaskp)
	AudioDevicePtr aDev;
	int onoff;
	int nmask, *omaskp, *amaskp;
{
	/* Not supported */
	*omaskp = *amaskp = 0;
}


/* Input gain management */

#define	MININPUTGAIN	(-70)
#define	MAXINPUTGAIN	15

static int
sgiQueryInputGain(aDev, minp, maxp)
	AudioDevicePtr aDev;
	int *minp, *maxp;
{
	int value, gdB;
	long params[4];
	params[0] = AL_LEFT_INPUT_ATTEN;
	params[2] = AL_RIGHT_INPUT_ATTEN;
	ALgetparams(DEV(aDev), params, 4);
	value = (params[1] + params[3]) / 2; /* average of two channels */
	gdB = 15 - value*3/8;
	LIMIT(MININPUTGAIN, gdB, MAXINPUTGAIN);
	if(minp != NULL)
		*minp = MININPUTGAIN;
	if(maxp != NULL)
		*maxp = MAXINPUTGAIN;
	return gdB;
}

static int
sgiSelectInputGain(aDev, gdB)
	AudioDevicePtr aDev;
	int gdB;
{
	int value;
	long params[4];

	if ((gdB < MININPUTGAIN) || (gdB > MAXINPUTGAIN))
		return ABadValue;

	if (gdB <= MININPUTGAIN)
		value = 255;
	else
		value = (15 - gdB)*8/3;
	LIMIT(0, value, 255);
	params[0] = AL_LEFT_INPUT_ATTEN;
	params[1] = value;
	params[2] = AL_RIGHT_INPUT_ATTEN;
	params[3] = value;
	ALsetparams(DEV(aDev), params, 4);
	return ASuccess;
}


/* Output gain management */

#define	MINOUTPUTGAIN	(-24)
#define	MAXOUTPUTGAIN	24

static int
sgiQueryOutputGain(aDev, minp, maxp)
	AudioDevicePtr aDev;
	int *minp, *maxp;
{
	int value, gdB;
	long params[4];
	params[0] = AL_LEFT_SPEAKER_GAIN;
	params[2] = AL_RIGHT_SPEAKER_GAIN;
	ALgetparams(DEV(aDev), params, 4);
	value = (params[1] + params[3])/2;
	if (value == 0)
		gdB = MINOUTPUTGAIN;
	else
		gdB = (int)(FACT2DB(value) + 0.5) + MINOUTPUTGAIN;
	LIMIT(MINOUTPUTGAIN, gdB, MAXOUTPUTGAIN);
	if(minp != NULL)
		*minp = MINOUTPUTGAIN;
	if(maxp != NULL)
		*maxp = MAXOUTPUTGAIN;
	return gdB;
}

static int
sgiSelectOutputGain(aDev, gdB)
	AudioDevicePtr aDev;
	int gdB;
{
	int value;
	long params[4];

	if ((gdB < MINOUTPUTGAIN) || (gdB > MAXOUTPUTGAIN))
		return ABadValue;
	value = (int)(DB2FACT(gdB - MINOUTPUTGAIN) + 0.5);
	LIMIT(0, value, 255);
	params[0] = AL_LEFT_SPEAKER_GAIN;
	params[1] = value;
	params[2] = AL_RIGHT_SPEAKER_GAIN;
	params[3] = value;
	ALsetparams(DEV(aDev), params, 4);
	return ASuccess;
}


/* Audio Context management */

static ABool
sgiCreateAC(pAC)
	ACPtr pAC;
{
	if((pAC->funcs = (ACFuncs *)AFalloc(sizeof(ACFuncs)))==NULL){
		return FALSE;
	}
	if((pAC->ops = (ACOps *)AFalloc(sizeof(ACOps)))==NULL){
		xfree(pAC->funcs);
		return FALSE;
	}

	pAC->funcs->ChangeAC = sgiChangeAC;
	pAC->funcs->DestroyAC = sgiDestroyAC;

	pAC->ops->ConvertPlay = sgiPlay;
	pAC->ops->ConvertRec = sgiRecord;

	return TRUE;
}

static void
sgiChangeAC(ptr, pAC, mask)
void *ptr;
ACPtr pAC;
int mask;
{
}

static void
sgiDestroyAC(pAC)
ACPtr pAC;
{
	xfree(pAC->funcs);
	xfree(pAC->ops);
}


/* Real time management */

static ATime
sgiGetTime(aDev)
	AudioDevicePtr aDev;
{
	sgiUpdateTime(aDev);
	return aDev->time0;
}


/* Playing samples */

/* Forward */
static void mix(short *, int, int, short *, int, int, float);
static void mixulaw(short *, int, int, unsigned char *, int, float);

static int
sgiPlay(ptime, pbuf, plen, ac)
	ATime ptime;
	unsigned char *pbuf;
	int plen;
	ACPtr ac;
{
	AudioDevicePtr aDev = ac->aDev;
	int nch = aDev->playNchannels;
	int ssize = (aDev->playBufType == MU255) ? nch : 2*nch;
	struct private *p = PRIV(aDev);
	ATime time0 = p->playIndex + p->playLength * p->playCycles;
	int future = ptime - time0;
	int throw;
	float gainf;
	if (future < 0) {
		if (future <= -plen)
			return plen; /* Throw away all samples */
		/* Throw away initial samples */
		throw = -future;
		future = 0;
		plen -= throw;
		pbuf += throw*ssize;
	}
	else
		throw = 0;
	if (future >= p->playLength)
		return 0; /* Can't play any samples now */
	if (future + plen > p->playLength)
		plen = p->playLength - future; /* Hold some samples */
	if (ac->playGain != 0)
		gainf = DB2FACT(ac->playGain);
	else
		gainf = 1.0;
	switch (aDev->playBufType) {
	case LIN16:
		mix(p->playBuf, p->playIndex+future, p->playLength,
		    (short *)pbuf, plen, nch, gainf);
		break;
	case MU255:
		if (nch != 1)
			printf("sgiPlay: bad U-LAW Nchannels\n");
		else
			mixulaw(p->playBuf, p->playIndex+future, p->playLength,
				pbuf, plen, gainf);
		break;
	default:
		printf("sgiPlay: bad data format\n");
		break;
	}
	return plen + throw;
}

/* XXX These are too slow -- apass at 48000 stereo takes 50% of R4000 CPU! */

static void
mix(register short *dst,
    int at,
    int size,
    register short *src,
    register int count,
    int nch,
    register float gainf)
{
	at %= size;
	if (count > size-at) {
		mix(dst, at, size, src, size-at, nch, gainf);
		src += (size-at)*nch;
		count -= size-at;
		at = 0;
	}
	dst += at*NCH;
	switch (nch) {
	case 1:
		while (--count >= 0) {
			*dst++ += *src * gainf;
			*dst++ += *src * gainf;
			src++;
		}
		break;
	case 2:
		while (--count >= 0) {
			*dst++ += *src++ * gainf;
			*dst++ += *src++ * gainf;
		}
		break;
	default:
		printf("mix: bad nch (%d)\n", nch);
		break;
	}
}

static void
mixulaw(register short *dst,
	int at,
	int size,
	register unsigned char *src,
	register int count,
	register float gainf)
{
	at %= size;
	if (count > size-at) {
		mixulaw(dst, at, size, src, size-at, gainf);
		src += (size-at);
		count -= size-at;
		at = 0;
	}
	dst += at*NCH;
	while (--count >= 0) {
		register value = AF_cvt_u2s[*src++];
		*dst++ += value * gainf;
		*dst++ += value * gainf;
	}
}


/* Recording samples */

/* Forward */
static void get(short *, int, int, short *, int, int, float);
static void getulaw(short *, int, int, unsigned char *, int, float);

static int
sgiRecord(rtime, rbuf, rlen, ac)
	ATime rtime;
	unsigned char *rbuf;
	int rlen;
	ACPtr ac;
{
	AudioDevicePtr aDev = ac->aDev;
	int nch = aDev->recNchannels;
	int ssize = (aDev->recBufType == MU255) ? nch : 2*nch;
	struct private *p = PRIV(aDev);
	ATime time0 = p->recIndex + p->recLength * p->recCycles;
	int past = rtime - (time0 - p->recLength);
	int lost;
	float gainf;
	if (rtime >= time0)
		return 0; /* All in the future */
	if (past < 0) { /* Lost some samples */
		lost = -past;
		if (lost > rlen)
			lost = rlen;
		switch (aDev->recBufType) {
		default:
			printf("Don't know how to silence fill, assume 0\n");
			/* Fall through */
		case LIN16:
			memset(rbuf, 0, lost*ssize);
			break;
		case MU255:
			memset(rbuf, 0xff, lost*ssize); /* U-LAW silence */
			break;
		}
		if (lost >= rlen)
			return rlen;
		rbuf += lost*ssize;
		rlen -= lost;
	}
	else
		lost = 0;
	if (past + rlen > p->recLength)
		rlen = p->recLength - past;
	if (ac->recordGain != 0)
		gainf = DB2FACT(ac->recordGain);
	else
		gainf = 1.0;
	switch (aDev->recBufType) {
	case LIN16:
		get(p->recBuf, p->recIndex + past, p->recLength,
			(short *)rbuf, rlen, nch, gainf);
		break;
	case MU255:
		getulaw(p->recBuf, p->recIndex + past, p->recLength,
			rbuf, rlen, gainf);
		break;
	default:
		printf("sgiRecord: bad data format\n");
		break;
	}
	return rlen + lost;
}

static void
get(short *src,
    int at,
    int size,
    short *dst,
    int count,
    int nch,
    float gainf)
{
	at %= size;
	if (count > size-at) {
		get(src, at, size, dst, size-at, nch, gainf);
		dst += (size-at)*nch;
		count -= size-at;
		at = 0;
	}
	src += at*NCH;
	switch (nch) {
	case 1:
		gainf *= 0.5;
		while (--count >= 0) {
			register int value;
			value = *src++;
			*dst++ = (value + *src++) * gainf;
		}
		break;
	case 2:
		while (--count >= 0) {
			*dst++ = *src++ * gainf;
			*dst++ = *src++ * gainf;
		}
		break;
	default:
		printf("get: bad nch (%d)\n", nch);
		break;
	}
}

static void
getulaw(short *src,
	int at,
	int size,
	unsigned char *dst,
	int count,
	float gainf)
{
	at %= size;
	if (count > size-at) {
		getulaw(src, at, size, dst, size-at, gainf);
		dst += size-at;
		count -= size-at;
		at = 0;
	}
	src += at*NCH;
	gainf /= 8; /* Average 2ch and scale down to 14 bits */
	while (--count >= 0) {
		register int value;
		value = *src++;
		value += *src++;
		value *= gainf;
		LIMIT(-8192, value, 8191);
		*dst++ = AF_comp_u[value & 0x3fff];
	}
}
