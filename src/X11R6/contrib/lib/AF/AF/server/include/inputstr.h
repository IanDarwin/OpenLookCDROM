/************************************************************
Copyright 1987, 1990 by Digital Equipment Corporation, Maynard, Massachusetts,
and the Massachusetts Institute of Technology, Cambridge, Massachusetts.

                        All Rights Reserved

********************************************************/
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

#ifndef INPUTSTRUCT_H
#define INPUTSTRUCT_H

#include <input.h>
#include <diastruct.h>
#include <audiodev.h>

#define BitIsOn(ptr, bit) (((BYTE *) (ptr))[(bit)>>3] & (1 << ((bit) & 7)))

#define SameClient(obj,client) \
	(CLIENT_BITS((obj)->resource) == (client)->clientAsMask)

#define MAX_DEVICES	7

#define EMASKSIZE	MAX_DEVICES

/* Kludge: OtherClients and InputClients must be compatible, see code */

typedef struct _OtherClients {
    OtherClientsPtr	next;
    AID			resource; /* id for putting into resource manager */
    AMask		mask;
} OtherClients;

typedef struct _InputClients {
    InputClientsPtr	next;
    AID			resource; /* id for putting into resource manager */
    AMask		mask[EMASKSIZE];
} InputClients;

typedef struct _OtherInputMasks {
    AMask		deliverableEvents[EMASKSIZE];
    AMask		inputEvents[EMASKSIZE];
    AMask		dontPropagateMask[EMASKSIZE];
    InputClientsPtr	inputClients;
} OtherInputMasks;

typedef struct _DeviceIntRec *DeviceIntPtr;

typedef struct _DeviceIntRec {
    DeviceRec	public;
    DeviceIntPtr next;
    ABool	startup;		/* true if needs to be turned on at
				          server intialization time */
    DeviceProc	deviceProc;		/* proc(DevicePtr, DEVICE_xx). It is
					  used to initialize, turn on, or
					  turn off the device */
    ABool	inited;			/* TRUE if INIT returns ASuccess */
    AAtom		type;
    char		*name;
    CARD8		id;
} DeviceIntRec;

typedef struct {
    int			numDevices;	/* total number of devices */
    DeviceIntPtr	devices;	/* all devices turned on */
    DeviceIntPtr	off_devices;	/* all devices turned off */
} InputInfo;

#endif
