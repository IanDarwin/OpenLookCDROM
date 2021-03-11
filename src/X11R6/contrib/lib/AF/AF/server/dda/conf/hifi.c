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
static char pscodec_c_rcsid[] = "$Header: /crl/audio/AF/server/dda/conf/RCS/hifi.c,v 1.4 1994/02/01 18:56:59 tml Exp $";
#endif

#include <stdio.h>
#include <string.h>
#include <server/include/dia.h>
#include <server/include/misc.h>
#include <server/include/audiodev.h>
#include <server/include/ac.h>
#include <server/include/acstruct.h>
#include <server/include/task.h>
#include "confdda.h"
#include <AF/AFUtils.h>
#include <assert.h>

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
	confPhysDevice *pDev = (confPhysDevice *)aDev->devPtr;

	HIFI_UPDATE_TIME(pDev);
	aDev->time0 = pDev->time0;
	return pDev->time0;
}

/*
 * Destroy an audio context.  If a record was done on this context, then 
 * decrement the reference count for the hardware device.
 */
void hifiDestroyAC(ACPtr pAC)
{
  int i;
  confPhysDevice *pDev = (confPhysDevice *)pAC->aDev->devPtr;
  ACPrivate *acp = (ACPrivate *) pAC->privPtr;
  for (i = 0; i < MAXCONF; i += 1) {
    if (pAC == pDev->ac[i]) pDev->ac[i] = NULL;
  }
  xfree(acp->ring.buf);
  xfree(pAC->funcs);
  xfree(pAC->ops);
}

void hifiChangeAC(void *ptr, ACPtr pAC, int mask)
{
  if (mask & ACEncodingType) {
    /* */
  }
  if (pAC->aDev->playBufType == MU255) {
    LIMIT(AF_gain_min_u, pAC->playGain, AF_gain_max_u);
    pAC->recGainMul = 1.0;
    pAC->playGainMul = 1.0;
  } else	{
    LIMIT(-100, pAC->playGain, 100);	
    pAC->recGainMul = AFdBtoLin((double) pAC->recordGain);
    pAC->playGainMul = AFdBtoLin((double) pAC->playGain);
  }
}

/*  Create an audio context */
ABool hifiCreateAC(ACPtr pAC)
{
  confPhysDevice *pDev = (confPhysDevice *)pAC->aDev->devPtr;
  ACPrivate *acp;
  int i, done;
  /* see if there is enough room */
  done = 0;
  for (i = 0; i < MAXCONF; i += 1) {
    if (pDev->ac[i] == NULL) {
      done = 1;
      pDev->ac[i] = pAC;
      break;
      }
  }
  if (done == 0) return (FALSE);
  /* now allocate necessary structures */
  if ((acp  = (ACPrivate *) AFalloc(sizeof(ACPrivate))) == NULL) {
    return (FALSE);
  }
  if((pAC->funcs = (ACFuncs *)AFalloc(sizeof(ACFuncs)))==NULL){
    return (FALSE);
  }
  if((pAC->ops = (ACOps *)AFalloc(sizeof(ACOps)))==NULL){
    xfree(pAC->funcs);
    return (FALSE);
  }
  pAC->recRef = FALSE;
  pAC->funcs->ChangeAC = hifiChangeAC;
  pAC->funcs->DestroyAC = hifiDestroyAC;
  pAC->privPtr = (pointer) acp;
  pAC->playType = pAC->aDev->playBufType;
  pAC->recType = pAC->aDev->recBufType;
  pAC->ops->ConvertPlay = 
    (int (*)(ATime, unsigned char *, int , ACPtr)) hifiPlay;
  pAC->ops->ConvertRec = 
    (int (*)(ATime, unsigned char *, int , ACPtr))  hifiRecord;
  /* allocate and initialize ringbuffer */
  acp->ring.size = (CONFBUFMS * pDev->sampleRate) / 1000;
  acp->histSize = (CONFHISTMS * pDev->sampleRate) / 1000;
  acp->ring.buf = (CARD32 *) AFalloc(acp->ring.size * sizeof(CARD32));
  if (acp->ring.buf == NULL) return(FALSE);
  acp->ring.zeroTime = pDev->time0 - acp->histSize;
  acp->ring.baseTime = pDev->time0 - acp->histSize;
  acp->lastPlay = acp->ring.baseTime;
  acp->lastRecord = acp->ring.baseTime;
  acp->timeEarliestValid = acp->ring.baseTime;
  acp->timeLastValid = acp->ring.baseTime;
  return TRUE;
}

void hifiUpdate(confPhysDevice *pDev)
{
  int i;
  ACPtr ac;
  ACPrivate *acp;
  if (pDev->eventLog) printf("Update %d\n", pDev->time0);
  for (i = 0; i < MAXCONF; i += 1) {
    ac = pDev->ac[i];
    if (ac != NULL) {
      acp = (ACPrivate *) ac->privPtr;
      if (acp != NULL) {
	acp->ring.baseTime = pDev->time0 - acp->histSize;
	KEEPUP(acp->timeEarliestValid, acp->ring.baseTime);
	KEEPUP(acp->timeLastValid, acp->ring.baseTime);
	UpdateZeroTime(&acp->ring);
      }
    }
  }
}

/*
 * Periodic update task:  performs periodic operation and 
 * resubmits itself for next time.
 */
void hifiUpdateTask(TaskPtr oldTask)
{	
  TaskPtr		newTask=NewTask();
  AudioDevicePtr 	aDev=oldTask->aDev;
  confPhysDevice *pDev = (confPhysDevice *) aDev->devPtr;
  
  *newTask = *oldTask;		/* Task for next time.		*/
  
  /* Get the current device time and update audio device time. */
  HIFI_UPDATE_TIME(pDev);
  aDev->time0 = pDev->time0;

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
  confPhysDevice *pDev = (confPhysDevice *)aDev->devPtr;
  HiFiPrivate	*hPtr;
  
  aDev->ChangeOutput = (VoidProc) confChangeOutput;
  aDev->ChangeInput = (VoidProc) confChangeInput;
  aDev->ChangePassThrough = (VoidProc)NoopDDA;
  aDev->QueryOutputGain = confQueryOutputGain;
  aDev->QueryInputGain = confQueryInputGain;
  aDev->SelectOutputGain = (IntProc) confSelectOutputGain;
  aDev->SelectInputGain = (IntProc) confSelectInputGain;
  
  aDev->CreateAC = hifiCreateAC;
  aDev->GetTime = hifiGetTime;	          /* Gets device time.	*/
  
  aDev->playSampleFreq = rate;	    /* Describes the play HW */
  aDev->playBufType = LIN16;
  aDev->playNchannels = (type == HIFI_STEREO) ? 2 : 1; 
  aDev->playNSamplesBuf = (CONFFUTMS * pDev->sampleRate) / 1000; 
  
  aDev->recSampleFreq = rate;	    /* Describe the record HW */
  aDev->recBufType = LIN16; 
  aDev->recNchannels = (type == HIFI_STEREO) ? 2 : 1;
  aDev->recNSamplesBuf = (CONFHISTMS * pDev->sampleRate) / 1000; 
  
  aDev->numberOfInputs = 1;
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

/*
  This is the Hifi sample rate, as set by the -hrate command line option.
*/
int hifi_rate = 8000;
int eventLog = 0;       /* print log events */

/* local procedures */

void conf_update_times(confPhysDevice *pDev)
{
  int milliClock, advance;
  milliClock = (int) GetTimeInMillis();
  advance = ((milliClock - pDev->lastMilliClock) * pDev->sampleRate)
    / 1000;
  pDev->lastMilliClock = milliClock;
  pDev->time0 += advance;
}


int conf_set_output_gain(confPhysDevice *pDev, int channel, int gdB)
{
  return (ASuccess);
}

int
confInit(char *devName, confPhysDevice *pDev)
{
  pDev->eventLog = eventLog;
  pDev->sampleRate = hifi_rate;
  pDev->updateBandMs = 250;
  pDev->time0 = 0;
  pDev->lastMilliClock = GetTimeInMillis();
  return(0);
}

void confClose(confPhysDevice *pDev)
{
}

/* work procedures */

int confQueryInputGain(AudioDevicePtr aDev, int *minp, int *maxp)
{
  if(minp != NULL) *minp = 0;
  if(maxp != NULL) *maxp = 0;
  return(0);
}

int confQueryOutputGain(AudioDevicePtr aDev, int *minp, int *maxp)
{
  if(minp != NULL) *minp = 0;
  if(maxp != NULL) *maxp = 0;
  return(0);
}

int confSelectInputGain(AudioDevicePtr aDev, int gdB )
{
  return (ASuccess);
}

int confSelectOutputGain(AudioDevicePtr aDev, int gdB)
{
  return (ASuccess);
}

void confChangeInput(AudioDevicePtr aDev, int onoff,
		    int nmask, int *omaskp, int *amaskp)
{
  if (omaskp != NULL) *omaskp = 1;
  if (amaskp != NULL) *amaskp = 1;
}

void confChangeOutput(AudioDevicePtr aDev, int onoff,
		     int nmask, int *omaskp, int *amaskp)
{
  if (omaskp != NULL) *omaskp = 1;
  if (amaskp != NULL) *amaskp = 1;
}



