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

#include	<sys/time.h>
#include	<server/include/misc.h>
#include	<server/include/audiodev.h>
#include	<server/include/dia.h>
#include <server/include/task.h>
#include <audioproto.h>
#include <audio.h>
#include	"dda.h"
#include	"teleport.h"
#include	"physdevice.h"
#include	"devtime.h"



int
teleHookSwitch(AudioDevicePtr aDev, int func)
{
	HiFiPrivate *hPtr = (HiFiPrivate *) aDev->privPtr;
	AudioDevicePtr prim = hPtr->prim;
	int mask;
        HiFiPrivate *primHPtr = (HiFiPrivate *) prim->privPtr;

	if (hPtr->channel == 0) mask = TELELEFTHOOK;
	else if (hPtr->channel == 1) mask = TELERIGHTHOOK;
	else return ABadValue;

	if(func == OnHook) primHPtr->offHook |= mask;
	else if(func == OffHook) primHPtr->offHook &= (~mask);
	else return ABadValue;

        dspWriteRAM(prim->devPtr, DSP_LCTL, primHPtr->offHook << 8);
	return ASuccess;
}

int
teleHookSwitchState(AudioDevicePtr aDev)
{
	HiFiPrivate *hPtr = (HiFiPrivate *) aDev->privPtr;
	AudioDevicePtr prim = hPtr->prim;
	int mask;
        HiFiPrivate *primHPtr = (HiFiPrivate *) prim->privPtr;

	if (hPtr->channel == 0) mask = TELELEFTHOOK;
	else if (hPtr->channel == 1) mask = TELERIGHTHOOK;
	else mask = 0;

        return ((primHPtr->offHook & mask) == 0);
}

int
teleLoopState(AudioDevicePtr aDev)
{
	HiFiPrivate *hPtr = (HiFiPrivate *) aDev->privPtr;
	AudioDevicePtr prim = hPtr->prim;
        HiFiPrivate *primHPtr = (HiFiPrivate *) prim->privPtr;


	if (hPtr->channel == 0)
          return ((primHPtr->leftInput) & TELELOOPMASK == 0);
	else if (hPtr->channel == 1)
          return (0); /* RIGHT channel has no loop current detect */

	return 0; /* can't happen */
}

void
TeleRingTask(TaskPtr oldTask)
{	

	TaskPtr	newTask;

	AudioDevicePtr aDev=oldTask->aDev;
	HiFiPrivate *hPtr = (HiFiPrivate *) aDev->privPtr;
	int channel = hPtr->channel;

	AudioDevicePtr prim = hPtr->prim;
	HiFiPrivate *primhPtr = (HiFiPrivate *) prim->privPtr;

	HIFI_UPDATE_TIME(prim);
	hifiUpdate(prim);

	if (!primhPtr->ringTimerActive[channel]) return;
	if ((prim->time0 - primhPtr->ringETime[channel]) > 600) {
	  primhPtr->ringTimerActive[channel] = 0;
	  TeleEvent(aDev, primhPtr->ringETime[channel], channel, 
		    TELE_EVENT_REAL_RING, 0);
	}
	else {
	  newTask=NewTask();
	  newTask->aDev = aDev;
	  newTask->time = prim->time0;	/* Mark new task with old time. */
	  AddTask((VoidProc) TeleRingTask, newTask, 100);
	}
}

void TeleEvent(AudioDevicePtr aDev, ATime etime, 
               int channel, int etype, int new)
{
    aEvent cEvent;
    struct timeval systime;
    struct timezone tz;
    AudioDevicePtr dev;
    HiFiPrivate	*hPtr = 
          (HiFiPrivate *) ((HiFiPrivate *)aDev->privPtr)->prim->privPtr;
    aDev = hPtr->chADev[channel];
    /* Teleport reports every cycle of the ring current, so filter them */
    if (etype == TELE_EVENT_RING) {
      if (new == 0) return;   /* ignore down events */
      hPtr->ringETime[channel] = etime;
      if (hPtr->ringTimerActive[channel]) return;
      else {
	/* start task to create down event later */
	TaskPtr task = NewTask();
	hPtr->ringTimerActive[channel] = 1;
	task->aDev = aDev;
	task->time = etime;
	AddTask((VoidProc) TeleRingTask, task, 100);
      }
    }

    if(gettimeofday(&systime, &tz) != 0){
	ErrorF("dda: gettimeofday failed.\n");
    }
    dev = hPtr->chADev[channel];
    switch(etype){
             case TELE_EVENT_RING:
             case TELE_EVENT_REAL_RING:
		cEvent.u.u.type = APhoneRingEvent; 
		cEvent.u.PhoneRing.sec = systime.tv_sec;
		cEvent.u.PhoneRing.usec = systime.tv_usec;
		cEvent.u.PhoneRing.time = etime;
		cEvent.u.PhoneRing.device = dev->index;
		cEvent.u.PhoneRing.state = new;
		break;
	      case TELE_EVENT_LOOP:
		cEvent.u.u.type = APhoneLoopEvent; 
		cEvent.u.PhoneLoop.sec = systime.tv_sec;
		cEvent.u.PhoneLoop.usec = systime.tv_usec;
		cEvent.u.PhoneLoop.time = etime;
		cEvent.u.PhoneLoop.device = dev->index;
		cEvent.u.PhoneLoop.state = new;
		break;
 	      default:
		ErrorF("Unknown event type in TeleEvent %d\n",etype);
	}
    FilterEvents(&cEvent,dev->index);
}

/*
 * Set output according to nmask.
 * Return a bit mask describe which outputs are actually enabled and
 * the previous mask.
 */ 

/* if nmask == 0, report only 
 *  nmask     onoff      effect
 *   00         X        report only
 *   01         1        turn on
 *   01         0        turn off
 *   10         1        turn off
 *   10         0        turn on
 *   11         X        report only
 */

void
teleChangeBit(AudioDevicePtr aDev, int onoff, 
	int nmask, int *omaskp, int *amaskp, int bit)
{
	HiFiPrivate *hPtr = (HiFiPrivate *) aDev->privPtr;
	AudioDevicePtr prim = hPtr->prim;
        HiFiPrivate *primHPtr = (HiFiPrivate *) prim->privPtr;

	if (hPtr->channel != 0) return;
        *omaskp = (primHPtr->localAndLine & bit) ? 1 : 2;
	if ((nmask == 1) || (nmask == 2)) {
	  onoff = (onoff ^ (nmask >> 1)) & 1;
	  if(onoff) primHPtr->localAndLine |= bit;
	  else primHPtr->localAndLine &= ~bit;
	  dspWriteRAM(prim->devPtr, DSP_RCTL, primHPtr->localAndLine << 8);
	}
        *amaskp = (primHPtr->localAndLine & bit) ? 1 : 2;
}

/* for the Teleport, ChangeOutput actually changes the LINE relay
 * position.  0 = LINE and 1 = LOCAL.  Default = LINE
 */
void
teleChangeOutput(AudioDevicePtr aDev, int onoff, 
	int nmask, int *omaskp, int *amaskp)
{
  teleChangeBit(aDev, onoff, nmask, omaskp, amaskp, TELELINEMASK);
}

/* for the Teleport, ChangePassThrough actually changes the LOCAL relay
 * position.  0 = LINE and 1 = LOCAL.  Default = LINE
 */
void
teleChangePassThrough(AudioDevicePtr aDev, int onoff, 
	int nmask, int *omaskp, int *amaskp)
{	
  teleChangeBit(aDev, onoff, nmask, omaskp, amaskp, TELELOCALMASK);
  if (*omaskp == 2) *omaskp = 0;
  if (*amaskp == 2) *amaskp = 0;
}
