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
#ifndef	TELEPORT_H
#define	TELEPORT_H

#include <server/include/misc.h>

#define TELE_EVENT_RING 1
#define TELE_EVENT_LOOP 2
#define TELE_EVENT_REAL_RING 3


extern int teleHookSwitch(AudioDevicePtr aDev, int func);
extern int teleLoopState(AudioDevicePtr aDev);
extern int teleHookSwitchState(AudioDevicePtr aDev);

extern void TeleEvent(AudioDevicePtr aDev, ATime etime, 
               int channel, int etype, int new);

extern void teleChangeOutput(AudioDevicePtr aDev, int onoff, 
	int nmask, int *omaskp, int *amaskp);
extern void teleChangePassThrough(AudioDevicePtr aDev, int onoff, 
	int nmask, int *omaskp, int *amaskp);

#endif	/* TELEPORT_H */
