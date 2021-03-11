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
#if !defined(lint) && !defined(SABER)
static char pscodec_c_rcsid[] = "$Header: /crl/audio/AF/server/dda/axp/RCS/pscodec.c,v 1.23 1993/12/08 17:52:09 tml Exp $";
#endif

#include <stdio.h>
#include <server/include/dia.h>
#include <server/include/misc.h>
#include <server/include/acstruct.h>
#include <server/include/task.h>
#include <io/dec/tc/amd79c30.h>
#include "ringbuffers.h"
#include "pscodec.h"
#include "max_io.h"
#include "physdevice.h"
#include "cplay.h"
#include "crecord.h"
#include "devtime.h"
#include "write.h"
#include "read.h"
#include <AF/AFUtils.h>
#include "uatables.h"

extern int axpfd;

extern char *getenv();

#define	LIMIT(low, val, high) \
	  if (val < (low)) val = (low); \
	  else \
	  if ((val) > (high)) val = (high);

struct timeval tp, lstart;
unsigned int start_time = 0;

unsigned int
getctime()
{
/*	if(start_time == 0) { */
        gettimeofday(&tp, 0);
	if(lstart.tv_sec == 0) {
		lstart.tv_sec = tp.tv_sec;
		lstart.tv_usec = tp.tv_usec;
	} else {
		tp.tv_sec -= lstart.tv_sec;
		tp.tv_usec -= lstart.tv_usec;
	}
	start_time = (((unsigned int)(tp.tv_sec*1000 + tp.tv_usec/1000))*8);
/*	}  */
	return(start_time);
}


ATime bba_get_time ()
{
	return((ATime) getctime());
}

static ATime
codecGetTime(AudioDevicePtr aDev)
{
	UPDATE_TIME(aDev);
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
codecInitCommon(AudioDevicePtr aDev)
{
	CodecPrivate	*cPtr;

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
	aDev->playNSamplesBuf = BCHANBUFFSIZE; 

	aDev->recSampleFreq = SAMPLEFREQ; 	/* Describe the record 	*/
	aDev->recNchannels = 1; 		/* hardware.		*/
	aDev->recBufType = MU255; 
	aDev->recNSamplesBuf = BCHANBUFFSIZE; 
	aDev->playBuf = (pointer) malloc(BCHANBUFFSIZE);
	memset(aDev->playBuf, (int)SILENCE, aDev->recNSamplesBuf);
	aDev->recBuf = (pointer) malloc(BCHANBUFFSIZE);
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

	/* XX call "initcodecall(many args below);*/

	cPtr->playGain = 0;		/* XXX */
	cPtr->recGain = 0;		/* XXX */
	cPtr->ga = codecQueryInputGain(aDev, NULL, NULL);
	cPtr->ger = codecQueryOutputGain(aDev, NULL, NULL);
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


ABool 
codecPrimaryInit(AudioDevicePtr aDev)
{
	CodecPrivate *cPtr;

	if (!codecInitCommon(aDev))
		return FALSE;

	aDev->numberOfInputs = 2;
	aDev->numberOfOutputs = 1;
	aDev->inputsFromPhone = 0x0000;		/* Input 0. */
	aDev->outputsToPhone = 0x0000;		/* Input 0. */

	cPtr = (CodecPrivate*) aDev->privPtr;
	cPtr->psflag = 0;
	cPtr->playBuf = NULL;
	cPtr->recBuf = NULL;

	return TRUE;
}

extern double strtod();

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
	UPDATE_TIME(aDev);
	
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
		UPDATE_TIME(aDev);
		aDev->timeNextUpdate = aDev->time0-(EPSILON);
		aDev->timeLastUpdated = aDev->time0 - (EPSILON);
	}
	/* Write back data to hardware buffer. */
	tnupdate = aDev->time0 /* + MSTOTICKS(MSGUARDBAND) */; 
	diff = 	tnupdate - aDev->timeNextUpdate;
	if (diff > 0) { 
		write_back(aDev->timeNextUpdate, diff, aDev);
	}
/*	Silence is zeroed out as written (its buffered in the kernel anyway */

	/* Move Record Data. */
	codecRecUpdate(aDev);

	/* Modify the audio device state and leave. */
	aDev->timeLastUpdated = aDev->time0;
	aDev->timeNextUpdate = aDev->time0 /* + MSTOTICKS(MSGUARDBAND) */;
}

void
codecRecUpdate(AudioDevicePtr aDev)
{	
	int diff;

	diff = DELTA(1, aDev->time0, aDev->timeRecLastUpdated);
	if(diff > LENHWBUF){
		aDev->timeRecLastUpdated = aDev->time0-LENHWBUF;
		diff = LENHWBUF;
	}
	if (diff > 0 && diff < 2048) read_back(aDev->timeRecLastUpdated, diff, aDev);
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

	if(omaskp) *omaskp = 1;
	if(amaskp) *amaskp = 1;
}

void
codecChangeInput(AudioDevicePtr aDev, int onoff,
	int nmask, int *omaskp, int *amaskp)
{
	maxPhysDevice *pDev=(maxPhysDevice *)aDev->devPtr;

	*omaskp = axp_get_input(pDev->fd)
		& TOMASK(aDev->numberOfInputs);
	axp_set_input(pDev->fd, onoff, nmask&INMASK);
	*amaskp = axp_get_input(pDev->fd)
		& TOMASK(aDev->numberOfInputs);
}

int	odb = 0;
int	idb = 0;

#define	MAXOUTPUTGAIN	18
#define	MINOUTPUTGAIN	-10

int
codecQueryOutputGain(AudioDevicePtr aDev, int *minp, int *maxp)
{
	float	aga;

	 aga = (float) odb;

	if(minp!=NULL) *minp = MINOUTPUTGAIN;
	if(maxp!=NULL) *maxp = MAXOUTPUTGAIN;
	return (int)aga;
}

int
codecSelectOutputGain(AudioDevicePtr aDev, int gdB)
{
	int	aga;

	maxPhysDevice *pDev=(maxPhysDevice *)aDev->devPtr;

	{
	int data[2];
	int rv;

	if ((gdB < MINOUTPUTGAIN) || (gdB > MAXOUTPUTGAIN))
		return ABadValue;

	data[0] = BBA_GAIN_DB;
	data[1] = gdB * 10;
	rv = axpIoctl(pDev->fd, BBA_SET_H_I_GAIN, data, sizeof(int)*2);
	odb = gdB;
	}	

	aga = codecQueryOutputGain(aDev, NULL, NULL);
	return ASuccess;
}

#define	MAXINPUTGAIN	24
#define	MININPUTGAIN	0

int
codecQueryInputGain(AudioDevicePtr aDev, int *minp, int *maxp)
{
	float	aga;

	aga = (float)idb;

	if(minp!=NULL) *minp = MININPUTGAIN;
	if(maxp!=NULL) *maxp = MAXINPUTGAIN;
	return (int)aga;
}

int
codecSelectInputGain(AudioDevicePtr aDev, int gdB)
{
	float	aga;

	maxPhysDevice *pDev=(maxPhysDevice *)aDev->devPtr;

	{
	int data[2];
	int rv;

	if ((gdB < MININPUTGAIN) || (gdB > MAXINPUTGAIN))
		return ABadValue;

	data[0] = BBA_GAIN_DB;
	data[1] = gdB * 10;
	rv = axpIoctl(pDev->fd, BBA_SET_PREAMP_GAIN, data, sizeof(int)*2);
	idb = gdB;
	aga = (float) idb;
	}	

	return ASuccess;
}
