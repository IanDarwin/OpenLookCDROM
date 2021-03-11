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

#include <stdio.h>
#include <server/include/dia.h>
#include <server/include/misc.h>
#include <server/include/acstruct.h>
#include <server/include/task.h>
#include "pscodec.h"
#include "codec.h"
#include "lofi_io.h"
#include "physdevice.h"
#include "cplay.h"
#include "crecord.h"
#include "devtime.h"
#include "write.h"
#include "read.h"
#include "dsp.h"
#include "dsp_func.h"
#include <AF/AFUtils.h>
#include "uatables.h"

extern char *coeffname;

extern char *getenv();

#define	LIMIT(low, val, high) \
	  if (val < (low)) val = (low); \
	  else \
	  if ((val) > (high)) val = (high);

static ATime
codecGetTime(AudioDevicePtr aDev)
{
	CODEC_UPDATE_TIME(aDev);
	return aDev->time0;
}

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
	LIMIT(GAIN_MIN, pAC->playGain, GAIN_MAX);	
}

/*ARGUSED*/
ABool
codecInitCommon( AudioDevicePtr aDev) 
{
	CodecPrivate	*cPtr;
	lofiPhysDevice *pDev=(lofiPhysDevice *)aDev->devPtr;
	static int BeenHere = FALSE;

	/*
	 * Fill in AudioDevice Structure.
	 */
	aDev->time0 = 0;		/* The beginning of time, 	*/
	aDev->oldDevTime = 0;		/* the first time.		*/
	aDev->dsptime = 0;

	aDev->ChangeOutput = codecChangeOutput;
	aDev->ChangeInput = codecChangeInput;
	aDev->ChangePassThrough = codecChangePassThrough;
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

	/* Initialize player.						*/
	if((aDev->playBuf = (pointer)AFalloc(aDev->playNSamplesBuf))
		==(pointer)NULL)	return FALSE;
	memset(aDev->playBuf, (int)SILENCE, aDev->playNSamplesBuf);

	/* Initialize recorder.						*/
	if((aDev->recBuf = (pointer)AFalloc(aDev->recNSamplesBuf))
		==(pointer)NULL)	return FALSE;
	memset(aDev->recBuf, (int)SILENCE, aDev->recNSamplesBuf);

	/* Initialize update. 						*/
	codecUpdateInit(aDev);

	/*
	 * Hang a special structure off the AudioDev structure
	 * that descibes the physical device state of the codec
	 * in question.
	 */
	if ((aDev->privPtr = (pointer)AFalloc(sizeof(CodecPrivate)))
		== (pointer)NULL)	return FALSE;
	cPtr = (CodecPrivate *) aDev->privPtr;

	/* XXX Actually init the codec device HW. */

	if(!BeenHere){
	  codec_init(pDev->lofi, CODEC_SECONDARY);
	  codec_init(pDev->lofi, CODEC_PRIMARY);
	  BeenHere = TRUE;
	}

	/* XX call "initcodecall(many args below);*/

	cPtr->playGain = 0;		/* XXX */
	cPtr->recGain = 0;		/* XXX */
/*	codecChangeOutput(aDev, TRUE, 0, &temp, &cPtr->outSel); */
/*	codecChangeInput(aDev, TRUE, 0, &temp, &cPtr->inSel); */
	cPtr->x = 0;			/* XXX */
	cPtr->gx = 0;			/* XXX */
	cPtr->stg = 0;			/* XXX */
	cPtr->gr = 0;			/* XXX */
	cPtr->r = 0;			/* XXX */
	cPtr->atg[0] = 0;		/* XXX */
	cPtr->atg[1] = 0;		/* XXX */
	cPtr->ftg[0] = 0;		/* XXX */
	cPtr->ftg[1] = 0;		/* XXX */
	cPtr->Mute = 0;			/* XXX */

	return TRUE;
}

/*
 * Compensates for 120 Hz peak introduced by DAA.
 */
unsigned char coeff[16]={
  0x94, 0x0F,
  0xc2, 0xAC,
  0x94, 0xff,
  0x22, 0xAB,
  0x36, 0xEa,
  0x4a, 0xB2,
  0xa4, 0xe2,
  0xbc, 0xBA
};

ABool 
codecPrimaryInit(AudioDevicePtr aDev)
{
	lofiPhysDevice *pDev = (lofiPhysDevice*) aDev->devPtr;
	CodecPrivate *cPtr;

	if (!codecInitCommon(aDev))
		return FALSE;

	aDev->numberOfInputs = 1;		/* Phone */
	aDev->numberOfOutputs = 2;		/* Phone and monitor. */
	aDev->inputsFromPhone = 0x0001;		/* Input 0. */
	aDev->outputsToPhone = 0x0001;		/* Input 0. */

	cPtr = (CodecPrivate*) aDev->privPtr;
	cPtr->psflag = CODEC_PRIMARY;
	cPtr->playBuf = dspGetCodecPlayBuf(pDev, CODEC_PRIMARY);
	cPtr->recBuf = dspGetCodecRecBuf(pDev, CODEC_PRIMARY);
	cPtr->devTimePtr = (DSPTime *)dspGetCodecTimePtr(pDev);
	cPtr->ga = codecQueryInputGain(aDev, NULL, NULL);
	cPtr->ger = codecQueryOutputGain(aDev, NULL, NULL);

	codec_set_output(pDev->lofi, CODEC_PRIMARY, TRUE, 0x3);
	codec_set_input(pDev->lofi, CODEC_PRIMARY, TRUE, 1<<0);

	if(coeffname != (char *)NULL) {
		int i;
		FILE *fp;
		unsigned char lb,mb;
		if ((fp=fopen(coeffname,"r"))!=NULL) {
			for(i=0;i<16;i+=2) {
				fscanf(fp,"%x %x\n",&lb,&mb);
				coeff[i] = lb;
				coeff[i+1] = mb;
			}
			fclose (fp);
		} else
			ErrorF("cannot read coefficient file, %s\n",coeffname);
	}
	codec_xfilter_coeff(pDev->lofi, CODEC_PRIMARY, coeff);
	codec_xfilter(pDev->lofi, CODEC_PRIMARY, TRUE);

	/* 
	 * TLI.
	 */
	if(!tliInit(aDev))
		FatalError("Could not initialize phone.\n");

	return TRUE;
}

extern double strtod();

ABool 
codecSecondaryInit(AudioDevicePtr aDev)
{
	lofiPhysDevice *pDev = (lofiPhysDevice*) aDev->devPtr;
	CodecPrivate *cPtr;

	if (!codecInitCommon(aDev))
		return FALSE;

	aDev->numberOfInputs = 2;		/* Mic and Line. */
	aDev->numberOfOutputs = 2;		/* Ear and speaker. */
	aDev->inputsFromPhone = 0x0000;
	aDev->outputsToPhone = 0x0000;

	cPtr = (CodecPrivate*) aDev->privPtr;
	cPtr->psflag = CODEC_SECONDARY;
	cPtr->playBuf = dspGetCodecPlayBuf(pDev, CODEC_SECONDARY);
	cPtr->recBuf = dspGetCodecRecBuf(pDev, CODEC_SECONDARY);
	cPtr->devTimePtr = (DSPTime *) dspGetCodecTimePtr(pDev);
	cPtr->ga = codecQueryInputGain(aDev, NULL, NULL);
	cPtr->ger = codecQueryOutputGain(aDev, NULL, NULL);

	codec_set_output(pDev->lofi, CODEC_SECONDARY, TRUE, 0x3);
	codec_set_input(pDev->lofi, CODEC_SECONDARY, TRUE, 1<<0);

	return TRUE;
}

void
codecUpdateInit(AudioDevicePtr aDev)
{
	TaskPtr 	task;

	aDev->timeLastUpdated = 0;
	aDev->timeNextUpdate = MSTOTICKS(MSGUARDBAND);
	aDev->timeRecLastUpdated = 0;

	task = NewTask();
	task->aDev = aDev;
	task->time = 0;
	AddTask((VoidProc) codecUpdateTask, task, MSUPDATE);
}

/* Wrapper for the update procedure.
 * The update procedure must perform several tasks:
 * 1) update the hardware play buffer (region C)
 * 2) perform silence backfill (region A)
 * 3) move record data in the server buffer.
 *
 * The time line can be viewed as:
 *                  t0                                 
 * -----------------[----------------------------------]-------------
 *             | A  | B   | C  | D                 |  E
 * Where:
 * t0		Current device time when sampled in codecUpdate.
 * A		Region between time at previous update (aDev->timeLastUpdated)
 *		and current time.  This region is backfilled with silence.
 * B		Current time to aDev->timeLastUpdate+MSTOTICKS(MSUPDATE)
 *		which is the band that is consistent between server buffer
 *		and hardware buffer.
 * C		Region made consistent at this invocation of update which
 *		is from last time consistent through to 
 *		current time + MSTOTICKS(MSUPDATE).
 * D		Region that the player can play into which begins 
 *		at timeLastUpdated and continues for size of buffer samples.
 */
void
codecUpdateTask(TaskPtr oldTask)
{	

	TaskPtr	newTask=NewTask();
	AudioDevicePtr aDev=oldTask->aDev;

	*newTask = *oldTask;		/* Task for next time.		*/

	/* Get the current device time and update audio device time. */
	CODEC_UPDATE_TIME(aDev);
	
	/* Perform the write-back update with silence fill. */
	codecUpdate(aDev);

	newTask->time = aDev->time0;	/* Mark new task with old time. */
	AddTask((VoidProc) codecUpdateTask, newTask, MSUPDATE);
}

#define EPSILON 4	/* 0.5 ms at 8KHz  */

/*
 * Assumes time has been updated recently...
 * If losing track of real-time, it may update time and reset.
 * Invariant on exit:
 *	hardware updated (consistent) from time0 to time0 + MSGUARDBAND
 */
static int	nUpdatesMissed=0;
static int	lenUpdatesMissed=0;

void
codecUpdate(AudioDevicePtr aDev)
{	
	ATime	tnupdate;
	int diff;

	diff = DELTA(1, aDev->time0, aDev->timeLastUpdated);
	if( diff > MSTOTICKS(MSGUARDBAND)){ 
		++nUpdatesMissed;
		lenUpdatesMissed += diff - MSTOTICKS(MSGUARDBAND);

		/* "Schedule" next update for now + epsilon. */
		CODEC_UPDATE_TIME(aDev);
		aDev->timeNextUpdate = aDev->time0+(EPSILON);
		aDev->timeLastUpdated = aDev->time0 - 
			MSTOTICKS(MSGUARDBAND)+(EPSILON);
	}

	/* Write back data to hardware buffer. */
	tnupdate = aDev->time0 + MSTOTICKS(MSGUARDBAND);
	diff = 	tnupdate - aDev->timeNextUpdate;
	if (diff > 0) write_back(aDev->timeNextUpdate, diff, aDev);

	/* Silence backfill. */
	diff = 	aDev->time0 - aDev->timeLastUpdated;
	if (diff > 0) silence_fill(aDev->timeLastUpdated, diff, aDev);

	/* Move Record Data. */
	codecRecUpdate(aDev);

	/* Modify the audio device state and leave. */
	aDev->timeLastUpdated = aDev->time0;
	aDev->timeNextUpdate = aDev->time0 + MSTOTICKS(MSGUARDBAND);
}


/*
  Copy bytes from the DSP buffers into the server buffer.   On return,
  samples are valid through time0
*/
void
codecRecUpdate(AudioDevicePtr aDev)
{	
	int diff;

	diff = DELTA(1, aDev->time0, aDev->timeRecLastUpdated);
	if(diff > LENHWBUF){
		aDev->timeRecLastUpdated = aDev->time0-LENHWBUF;
		diff = LENHWBUF;
	}
	if (diff > 0) read_back(aDev->timeRecLastUpdated, diff, aDev);
	aDev->timeRecLastUpdated = aDev->time0;
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
 * Set output according to nmask.
 * Return a bit mask describe which outputs are actually enabled and
 * the previous mask.
 */ 
void
codecChangeOutput(AudioDevicePtr aDev, int onoff, 
	int nmask, int *omaskp, int *amaskp)
{
	lofiPhysDevice *pDev=(lofiPhysDevice *)aDev->devPtr;
	CodecPrivate	*cPtr= (CodecPrivate *) aDev->privPtr;

	*omaskp = codec_get_output(pDev->lofi, cPtr->psflag)
		& TOMASK(aDev->numberOfOutputs);
	codec_set_output(pDev->lofi, cPtr->psflag, onoff, nmask&OUTMASK);
	*amaskp = codec_get_output(pDev->lofi, cPtr->psflag)
		& TOMASK(aDev->numberOfOutputs);
}

void
codecChangeInput(AudioDevicePtr aDev, int onoff, 
	int nmask, int *omaskp, int *amaskp)
{
	lofiPhysDevice *pDev=(lofiPhysDevice *)aDev->devPtr;
	CodecPrivate	*cPtr= (CodecPrivate *) aDev->privPtr;

	*omaskp = codec_get_input(pDev->lofi, cPtr->psflag)
		& TOMASK(aDev->numberOfInputs);
	codec_set_input(pDev->lofi, cPtr->psflag, onoff, nmask&INMASK);
	*amaskp = codec_get_input(pDev->lofi, cPtr->psflag)
		& TOMASK(aDev->numberOfInputs);
}

void
codecChangePassThrough(AudioDevicePtr aDev, int onoff, 
	int nmask, int *omaskp, int *amaskp)
{
	lofiPhysDevice *pDev=(lofiPhysDevice *)aDev->devPtr;

	*omaskp = codec_get_connect();
	codec_connect(pDev->lofi, onoff, nmask&INMASK);
	*amaskp = codec_get_connect();
}

#define	MAXOUTPUTGAIN	18
#define	MINOUTPUTGAIN	-70

int
codecQueryOutputGain(AudioDevicePtr aDev, int *minp, int *maxp)
{
	float	aga;

	lofiPhysDevice *pDev=(lofiPhysDevice *)aDev->devPtr;
	CodecPrivate	*cPtr= (CodecPrivate *) aDev->privPtr;

	aga = codec_get_ger(pDev->lofi, cPtr->psflag);

	if(minp!=NULL) *minp = MINOUTPUTGAIN;
	if(maxp!=NULL) *maxp = MAXOUTPUTGAIN;
	return (int)aga;
}

int
codecSelectOutputGain(AudioDevicePtr aDev, int gdB)
{
	int	aga;

	lofiPhysDevice *pDev=(lofiPhysDevice *)aDev->devPtr;
	CodecPrivate	*cPtr= (CodecPrivate *) aDev->privPtr;

	if ((gdB < MINOUTPUTGAIN) || (gdB > MAXOUTPUTGAIN))
		return ABadValue;

	(void)codec_set_ger(pDev->lofi, cPtr->psflag, (float)gdB);

	aga = codecQueryOutputGain(aDev, NULL, NULL);
	return ASuccess;
}

#define	MAXINPUTGAIN	24
#define	MININPUTGAIN	0

int
codecQueryInputGain(AudioDevicePtr aDev, int *minp, int *maxp)
{
	float	aga;

	lofiPhysDevice *pDev=(lofiPhysDevice *)aDev->devPtr;
	CodecPrivate	*cPtr= (CodecPrivate *) aDev->privPtr;

	aga = codec_get_preamp(pDev->lofi, cPtr->psflag);

	if(minp!=NULL) *minp = MININPUTGAIN;
	if(maxp!=NULL) *maxp = MAXINPUTGAIN;
	return (int)aga;
}

int
codecSelectInputGain(AudioDevicePtr aDev, int gdB)
{
	float	aga;

	lofiPhysDevice *pDev=(lofiPhysDevice *)aDev->devPtr;
	CodecPrivate	*cPtr= (CodecPrivate *) aDev->privPtr;

	if ((gdB < MININPUTGAIN) || (gdB > MAXINPUTGAIN))
		return ABadValue;

	aga = codec_set_preamp(pDev->lofi, cPtr->psflag, (float)gdB);

	return ASuccess;
}
