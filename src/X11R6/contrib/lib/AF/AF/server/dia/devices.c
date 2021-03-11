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
/* ADevice-independent device abstraction procedures. */


#if !defined(lint) && !defined(SABER)
static char devices_rcsid[] = "$Header";
#endif

#include "audio.h"
#include "audioproto.h"
#include "acstruct.h"
#include "os.h"
#include "resource.h"
#include "opaque.h"
#include "servermd.h"
#include "diastruct.h"
#include "inputstr.h"

typedef ATime (*ATimeProc)();

AudioDeviceRec audioDevices[MAXDEVICES];
static int audioDevIndex = 0;


AudioDevicePtr
MakeDevice(void)
{

	if(audioDeviceInfo.numDevices == MAXDEVICES)
		FatalError("MakeDevice: Exceeded Max Devices\n");
	audioDeviceInfo.devices[audioDevIndex] = &audioDevices[audioDevIndex];
	audioDeviceInfo.numDevices += 1;
	audioDevices[audioDevIndex].index = audioDevIndex;
	/* */
	audioDevices[audioDevIndex].CreateAC = (BoolProc) NoopDDA;
	audioDevices[audioDevIndex].BlockHandler = (IntProc) NoopDDA;
	audioDevices[audioDevIndex].WakeupHandler = (IntProc) NoopDDA;
	audioDevices[audioDevIndex].GetTime = (ATimeProc)  NoopDDA;
	audioDevices[audioDevIndex].Dial = (IntProc) NoopDDA;
	audioDevices[audioDevIndex].HookSwitch = (IntProc) NoopDDA;
	audioDevices[audioDevIndex].FlashHook = (IntProc) NoopDDA;
	audioDevices[audioDevIndex].HookSwitchState = (IntProc) NoopDDA;
	audioDevices[audioDevIndex].LoopCurrentState = (IntProc) NoopDDA;
	audioDevices[audioDevIndex].PokeTLI = (IntProc) NoopDDA;
	audioDevices[audioDevIndex].TelephoneCtl = (IntProc) NoopDDA;
	audioDevices[audioDevIndex].TLICraftHookSwitchEvent = (IntProc) NoopDDA;
	audioDevices[audioDevIndex].ChangeOutput = (VoidProc) NoopDDA;
	audioDevices[audioDevIndex].ChangeInput = (VoidProc) NoopDDA;
	audioDevices[audioDevIndex].ChangePassThrough = (VoidProc) NoopDDA;
	audioDevices[audioDevIndex].QueryOutputGain = (IntProc) NoopDDA;
	audioDevices[audioDevIndex].QueryInputGain = (IntProc) NoopDDA;
	audioDevices[audioDevIndex].SelectOutputGain = (IntProc) NoopDDA;
	audioDevices[audioDevIndex].SelectInputGain = (IntProc) NoopDDA;
	audioDevices[audioDevIndex].ChangeGainCtl = (IntProc) NoopDDA;
	/* */
	return(&audioDevices[audioDevIndex++]);
}

void
CloseDownDevices(void)
{
/* XXX */
}
