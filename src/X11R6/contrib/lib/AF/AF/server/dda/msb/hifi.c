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
static char pscodec_c_rcsid[] = "$Header: /crl/audio/AF/server/dda/msb/RCS/hifi.c,v 1.12 1994/04/21 16:14:51 stewart Exp $";
#endif

#include <stdio.h>
#include <string.h>
#include <server/include/dia.h>
#include <server/include/misc.h>
#include <server/include/audiodev.h>
#include <server/include/ac.h>
#include <server/include/acstruct.h>
#include <server/include/task.h>
#include "msbdda.h"
#include <AF/AFUtils.h>
#include <assert.h>
#include <c_asm.h>
/* forward */
extern void hifiUpdateTask(TaskPtr oldTask);


/*
 * There are three hifi devices:  a stereo device, a left channel device, and
 * a right channel device.  All three devices share the same server buffers,
 * which are maintained in stereo (alternating left/right channel samples).
 *
 * The AudioDevice fields 'playNchannels' and 'recNchannels' differentiate
 * between the stereo device and the two mono devices.  For the mono devices,
 * the HiFiPrivate field 'channel' differentiates (valid 0 or 1) specifies the
 * left or right channel.
 *
 */


/*
 *  Handle GetTime request:  read and return the time for a Hifi channel.
 */
static ATime hifiGetTime(AudioDevicePtr aDev)
{
	msbPhysDevice *pDev = (msbPhysDevice *)aDev->devPtr;

	HIFI_UPDATE_TIME(pDev);
	return pDev->time0;
}

/*
 * Destroy an audio context.  If a record was done on this context, then 
 * decrement the reference count for the hardware device.
 */
void hifiDestroyAC(ACPtr pAC)
{
	msbPhysDevice *pDev = (msbPhysDevice *)pAC->aDev->devPtr;
	if(pAC->recRef)	{
	  pDev->recRefCount--;
	  if (pDev->recRefCount == 0) msb_record_stop(pDev);
	}
	xfree(pAC->funcs);
	xfree(pAC->ops);
}

void hifiChangeAC(void *ptr, ACPtr pAC, int mask)
{
}

/*  Create an audio context */
ABool hifiCreateAC(ACPtr pAC)
{
  if((pAC->funcs = (ACFuncs *)AFalloc(sizeof(ACFuncs)))==NULL){
    return FALSE;
  }
  if((pAC->ops = (ACOps *)AFalloc(sizeof(ACOps)))==NULL){
    xfree(pAC->funcs);
    return FALSE;
  }
  pAC->recRef = FALSE;
  pAC->funcs->ChangeAC = hifiChangeAC;
  pAC->funcs->DestroyAC = hifiDestroyAC;
  pAC->ops->ConvertPlay = 
    (int (*)(ATime, unsigned char *, int , struct _AC *)) hifiPlay;
  pAC->ops->ConvertRec = 
    (int (*)(ATime, unsigned char *, int , struct _AC *))  hifiRecord;
  return TRUE;
}

/* On entry, timeRecLastUpdated is the time of the last valid
 * sample in the server buffer.
 * we copy the samples from timeRecLastUpdated to time0 into
 * the server buffer
 */
void hifiRecUpdate(msbPhysDevice *pDev)
{
  int advance, lastUpdate;

  if (pDev->recDMARunning) {
    lastUpdate = pDev->hR.baseTime + pDev->hR.size;
    advance = DIFF(pDev->time0, lastUpdate);

    if (pDev->eventLog) printf("RecUpdate %d %d\n", pDev->time0, lastUpdate);

    if(!BETWEEN(0, advance, pDev->hR.size)) {
      printf("RecUpdate overrun: time %d adv %d\n", pDev->time0, advance);
      /* Two classes of error:
       * all data lost -> reinitialize state
       * some data lost -> substitute silence
       */
      if (BEFORE(pDev->time0, lastUpdate) ||
	  AFTER(pDev->time0, lastUpdate + pDev->sR.size)) {
	/* major error, entire sR is invalid */
	pDev->hR.baseTime = pDev->time0 - pDev->hR.size;
	pDev->hR.zeroTime = pDev->hR.baseTime - pDev->lastRecOffset;
	pDev->sR.baseTime = pDev->time0 - pDev->sR.size;
	pDev->sR.zeroTime = pDev->sR.baseTime;
	pDev->timeEarliestValid = pDev->hR.baseTime;
	lastUpdate = pDev->hR.baseTime;
	advance = pDev->hR.size;
      } else {
	/* minor error, gap between lastUpdate and time0-hR.size
	 * must be filled with silence
	 */
	WrapZero(&pDev->sR, lastUpdate, 
		 DIFF(pDev->time0 - pDev->hR.size, lastUpdate));
	pDev->hR.baseTime = pDev->time0 - pDev->hR.size;
	pDev->hR.zeroTime = pDev->hR.baseTime - pDev->lastRecOffset;
	/* sR.baseTime is still valid
         * sR.zeroTime is still valid
	 * timeEarliestValid is still valid
	 */
	lastUpdate = pDev->hR.baseTime;
	advance = pDev->hR.size;
      }
    }
    WrapCopy(&pDev->hR, &pDev->sR, lastUpdate, lastUpdate, advance);
    pDev->hR.baseTime = pDev->time0 - pDev->hR.size;
    pDev->sR.baseTime = pDev->time0 - pDev->sR.size;
    KEEPUP(pDev->timeEarliestValid, pDev->sR.baseTime);
    
    UpdateZeroTime(&pDev->hR);
    UpdateZeroTime(&pDev->sR);
  }
}


/*
 *  Periodic update.  Copies play and record data between the hardware
 *  buffers and the server's buffers.
 *
 * on entry:
 *   time0                     current time, from hardware
 *   hP.baseTime               oldest sample in hw buffer
 *   hP.zeroTime               ATime of hP.buf[0]
 *   sP.zeroTime               ATime of sP.buf[0]
 *
 * The play hardware has consumed the data between
 * hP.baseTime and time0, so consequently, the hw buffer is empty
 * between hP.baseTime+hP.size and time0+hP.size.
 *
 * If BEFORE(timeLastValid, hP.baseTime, we need do nothing, 
 * because it is already silent.
 *
 * otherwise, copy from sP to hP at time sP.baseTime
 * up until timeLastValid, copy data, then copy silence
 */
void hifiUpdate(msbPhysDevice *pDev)
{	
  int advance, length;

  if (pDev->playDMARunning) {
    if (pDev->eventLog) printf("Update %d %d %d\n", pDev->time0, 
			       pDev->hP.baseTime, pDev->timeLastValid);

    /* time has moved this much */
    advance = DIFF(pDev->time0, pDev->hP.baseTime); 
    /* derived variables */
    pDev->sP.baseTime = pDev->hP.baseTime + pDev->hP.size;
    
    /* advance must not be negative or larger than hP.size.
     * There are two strategies for dealing with errors:
     * Major errors - advance is negative or later than timeLastValid
     *     do a serious reset
     * Minor errors - try to salvage what we can
     */
    if (!BETWEEN(0, advance, pDev->hP.size)) {
      printf("Update overrun: time %d adv %d\n", pDev->time0, advance);
      if (BEFORE(pDev->time0, pDev->hP.baseTime) ||
	  AFTER(pDev->time0, pDev->timeLastValid)) {
	/* major error */
	pDev->hP.baseTime = pDev->time0;
	pDev->hP.zeroTime = pDev->time0 - pDev->lastPlayOffset;
	pDev->sP.zeroTime = pDev->time0 + pDev->hP.size;
	pDev->sP.baseTime = pDev->time0 + pDev->hP.size;
	pDev->timeLastValid = pDev->time0;
	advance = pDev->hP.size;
	WrapZero(&pDev->hP, pDev->hP.zeroTime, pDev->hP.size);
      }
      else {
	/* minor error, there is some data to salvage 
	 * hP is completely empty, but sP has some data in it
	 */
	pDev->hP.baseTime = pDev->time0 - pDev->hP.size;
	pDev->hP.zeroTime = pDev->time0 - pDev->lastPlayOffset;
	pDev->sP.baseTime = pDev->time0;
	/* sP.zeroTime is still valid */
	/* timeLastValid is still valid */
	/* copy data from sP to hP at sP.baseTime */
	advance = pDev->hP.size;
      }
    }
    if (AFTER(pDev->timeLastValid, pDev->hP.baseTime)) {
      /* otherwise, the hP buffer is already all silent */
      if (AFTER(pDev->timeLastValid, pDev->sP.baseTime)) {
	length = min(advance, DIFF(pDev->timeLastValid, pDev->sP.baseTime));
	if (pDev->eventLog) 
	  printf("WriteBack %d %d\n", 
		 pDev->hP.baseTime + pDev->hP.size, length);
	WrapCopy(&pDev->sP, &pDev->hP, 
		 pDev->sP.baseTime, pDev->sP.baseTime,
		 length);
	advance -= length;
	pDev->hP.baseTime += length;
	pDev->sP.baseTime += length;
      }
      if (advance > 0) {
	advance = min(advance, pDev->timeLastValid - pDev->hP.baseTime);
	if (pDev->eventLog) 
	  printf("WriteSilence %d %d\n", pDev->sP.baseTime, advance);
	WrapZero(&pDev->hP, pDev->sP.baseTime, advance);
	pDev->hP.baseTime += advance;
      }
    } else {
      msb_playback_stop(pDev);
    }
    
    pDev->hP.baseTime = pDev->time0;
    
    KEEPUP(pDev->timeLastValid, pDev->hP.baseTime);
    UpdateZeroTime(&pDev->hP);
    UpdateZeroTime(&pDev->sP);
  }
  /* move record data */
  hifiRecUpdate(pDev);
}

/*
 * Periodic update task:  performs periodic operation and 
 * resubmits itself for next time.
 */
void hifiUpdateTask(TaskPtr oldTask)
{	
  TaskPtr		newTask=NewTask();
  AudioDevicePtr 	aDev=oldTask->aDev;
  msbPhysDevice *pDev = (msbPhysDevice *) aDev->devPtr;
  
  *newTask = *oldTask;		/* Task for next time.		*/
  
  /* Get the current device time and update audio device time. */
  HIFI_UPDATE_TIME(pDev);
  aDev->time0 = pDev->time0;
  
  /* Perform the write-back update with silence fill. */
  hifiUpdate(pDev);
  
  newTask->time = pDev->time0;	/* Mark new task with old time. */
  AddTask(hifiUpdateTask, newTask, pDev->updateBandMs);
}


/*
 *  Initialize a HiFi device.  'rate' specifies the sample rate for the device.
 *  'type' specifies the type of the device (stereo, left, or right channel).
 */
ABool hifiInit(AudioDevicePtr aDev, int rate, int type)
{
  msbPhysDevice *pDev = (msbPhysDevice *)aDev->devPtr;
  HiFiPrivate	*hPtr;
  
  pDev->aDevs[type] = aDev;            /* initialize back pointer */  
  aDev->ChangeOutput = (VoidProc) msbChangeOutput;
  aDev->ChangeInput = (VoidProc) msbChangeInput;
  aDev->ChangePassThrough = (VoidProc)NoopDDA;
  aDev->QueryOutputGain = msbQueryOutputGain;
  aDev->QueryInputGain = msbQueryInputGain;
  aDev->SelectOutputGain = (IntProc) msbSelectOutputGain;
  aDev->SelectInputGain = (IntProc) msbSelectInputGain;
  
  aDev->CreateAC = hifiCreateAC;
  aDev->GetTime = hifiGetTime;	          /* Gets device time.	*/
  
  aDev->playSampleFreq = rate;	    /* Describes the play HW */
  aDev->playBufType = LIN16;
  aDev->playNchannels = (type == HIFI_STEREO) ? 2 : 1; 
  aDev->playNSamplesBuf = pDev->hP.size + pDev->sP.size; 
  
  aDev->recSampleFreq = rate;	    /* Describe the record HW */
  aDev->recBufType = LIN16; 
  aDev->recNchannels = (type == HIFI_STEREO) ? 2 : 1;
  aDev->recNSamplesBuf = pDev->sR.size;
  
  aDev->numberOfInputs = (type == HIFI_STEREO) ? 6 : 3;
  aDev->numberOfOutputs = 1;
  aDev->inputsFromPhone = 0;
  aDev->outputsToPhone = 0;
  
  /* Create the hifi private structure */
  if((aDev->privPtr = (pointer) AFalloc(sizeof(HiFiPrivate))) == NULL)
    return FALSE;
  
  hPtr = (HiFiPrivate *) aDev->privPtr;
  hPtr->channel = (type == HIFI_RIGHT) ? 1 : 0;
  hPtr->channel_type = type;

  /*
   * Intitialize the Hifi periodic update procedure
   */
  if (type == HIFI_STEREO) {
    TaskPtr task;
    task = NewTask();
    task->aDev = aDev;
    task->time = 0;
    AddTask(hifiUpdateTask, task, pDev->updateBandMs);
  }

  return TRUE;
}

