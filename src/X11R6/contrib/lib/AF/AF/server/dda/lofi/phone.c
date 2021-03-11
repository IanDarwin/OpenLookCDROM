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
#include	"codec.h"
#include	"lofi_io.h"
#include	"dda.h"
#include	"phone.h"
#include	"lofi_tli.h"
#include	"physdevice.h"

#define       TOMASK(x)       ((1<<(x))-1)

extern int daa_gain_control;

int changeDAAGainControl(AudioDevicePtr aDev, int onoff, 
	int nmask, int *omaskp, int *amaskp)
{

	*omaskp = daa_gain_control
		& TOMASK(aDev->numberOfInputs);

	daa_gain_control = !onoff;
	*amaskp = daa_gain_control
		& TOMASK(aDev->numberOfInputs);

	return 0;
}

void phoneCheck(TaskPtr oldTask)
{
	TaskPtr	newTask=NewTask();

	*newTask = *oldTask;		/* Task for next time.		*/
	newTask->time = 0;
	AddTask((VoidProc) phoneCheck, newTask, 3000);

	/* Pulse the DAA Gain control bit from 1-0-1 
         * causing the gain limiting function to reset.
         * Only take action if the dda.c enabling
         * variable is not set.
         */
	if (daa_gain_control == 0) {
		lofiPhysDevice *pDev = (lofiPhysDevice *)newTask->aDev->devPtr;

		LoFiSetCSR(pDev->lofi->us_reg, FGC, 1);
		LoFiSetCSR(pDev->lofi->us_reg, FGC, 0);
	}
}

ABool
tliInit(AudioDevicePtr aDev)
{
	lofiPhysDevice *pDev;
	TaskPtr 	task;

	aDev->Dial = tliDial;
	aDev->FlashHook = tliFlashHook;
	aDev->HookSwitch = tliHookswitch;
	aDev->HookSwitchState = tliHookSwitchState; 
	aDev->TLICraftHookSwitchEvent = tliCraftHookSwitchEvent;
	aDev->LoopCurrentState = tliLoopState;
	aDev->ChangeGainCtl = changeDAAGainControl;

	pDev = (lofiPhysDevice *)aDev->devPtr;

	pDev->TimeLastPinged = 0;

	task = NewTask();
	task->aDev = aDev;
	task->time = 0;
	AddTask((VoidProc) phoneCheck, task, 3000);

	return TRUE;
}

int
tliHookswitch(AudioDevicePtr aDev, int func)
{
	lofiPhysDevice *pDev=(lofiPhysDevice *)(aDev->devPtr);

	if(func == OnHook) lofi_tli_onhook(pDev->lofi);
	else if(func == OffHook) lofi_tli_offhook(pDev->lofi);
	else return ABadValue;

	return ASuccess;
}

int
tliFlashHook(AudioDevicePtr aDev, int duration)
{
	lofiPhysDevice *pDev=(lofiPhysDevice *)(aDev->devPtr);

	if(!tliHookSwitchState(aDev)) return ABadValue;
	if((duration > 3000) || (duration < 0)) return ABadValue;

	lofi_tli_onhook(pDev->lofi);
	msleep(pDev->lofi->us_reg, duration);
	lofi_tli_offhook(pDev->lofi);

	return ASuccess;
}

int
tliHookSwitchState(AudioDevicePtr aDev)
{
	CARD32 image;
	struct lofi_status_reg *csrp=(struct lofi_status_reg *)&image;
	image = ((lofiPhysDevice *)(aDev->devPtr))->lofi->us_reg->rd_csr;
	return csrp->s_hs;
}

int
tliLoopState(AudioDevicePtr aDev)
{
	CARD32 image;
	struct lofi_status_reg *csrp=(struct lofi_status_reg *)&image;
	image = ((lofiPhysDevice *)(aDev->devPtr))->lofi->us_reg->rd_csr;
	return csrp->s_lcdstat;
}

int
tliDial(AudioDevicePtr aDev, char *number)
{
	int status;
	lofiPhysDevice *pDev=(lofiPhysDevice *)(aDev->devPtr);

	status = lofi_dial(pDev->lofi, number);

	return status;
}

void
tliEnable(AudioDevicePtr aDev)
{

	lofiTliEnable((lofiPhysDevice *)aDev->devPtr);
}

void
tliDisable(AudioDevicePtr aDev)
{

	lofiTliDisable((lofiPhysDevice *)aDev->devPtr);
}

/*
 * Mitel 8870 DTMF receiver code to key-pad key character table.
 */
char dtmf_map[16]={
'D','1','2','3','4','5','6','7','8','9','0','*','#','A','B','C'
};

ABool
tliCraftEvent(ATime etime, int devNum, struct interrupt_event *ep, aEvent *aep)
{
	switch(ep->type){
	case TLI_DTMF:
#if	0
		if(!(ep->status && DTMF_VALID_DEF)) return -1;
#endif
		aep->u.u.type = APhoneDTMFEvent; 
		aep->u.PhoneDTMF.sec = ep->time.tv_sec;
		aep->u.PhoneDTMF.usec = ep->time.tv_usec;
		aep->u.PhoneDTMF.time = etime;
		aep->u.PhoneDTMF.device = 0; /* XXX ? */
		aep->u.PhoneDTMF.state = (DTMF_INTR(ep->status) != 0 ? 1 : 0);
		aep->u.PhoneDTMF.digit = dtmf_map[DTMF_KEY(ep->status)];
		break;
	case TLI_RING:
		aep->u.u.type = APhoneRingEvent; 
		aep->u.PhoneRing.sec = ep->time.tv_sec;
		aep->u.PhoneRing.usec = ep->time.tv_usec;
		aep->u.PhoneRing.time = etime;
		aep->u.PhoneRing.device = 0; /* XXX ? */
		aep->u.PhoneRing.state = (RING_INTR(ep->status) != 0 ? 1 : 0);
		break;
	case TLI_LOOP:
		aep->u.u.type = APhoneLoopEvent; 
		aep->u.PhoneLoop.sec = ep->time.tv_sec;
		aep->u.PhoneLoop.usec = ep->time.tv_usec;
		aep->u.PhoneLoop.time = etime;
		aep->u.PhoneLoop.device = 0; /* XXX ? */
		aep->u.PhoneLoop.state = (CURRENT_LOOP_INTR(ep->status) != 0 ? 1 : 0);
		break;
	default:
		ErrorF("Unknown event type in phoneCraftEvent %d\n",ep->type);
		return -1;
	}
	return 0;	/* Primary, telephone device. XXX */
}

/* LCS 10/6/93  I think tliCraftHookSwitchEvent is now machine independent */
int
tliCraftHookSwitchEvent(AudioDevicePtr aDev, aEvent *aep)
{
	struct timeval systime;
	struct timezone tz;

	if(gettimeofday(&systime, &tz) != 0){
		ErrorF("dda: gettimeofday failed.\n");
	}
	aep->u.u.type = APhoneHookSwitchEvent; 
	aep->u.PhoneHookSwitch.sec = systime.tv_sec;
	aep->u.PhoneHookSwitch.usec = systime.tv_usec;
	aep->u.PhoneHookSwitch.time = (*(aDev->GetTime))(aDev);
	aep->u.PhoneHookSwitch.device = aDev->index;
	aep->u.PhoneHookSwitch.state = (*(aDev->HookSwitchState))(aDev);

	return 0;	/* XXX should look up which device. */
}
