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
/* $Header: /crl/audio/AF/server/dia/RCS/main.c,v 1.21 1993/12/08 17:50:22 tml Exp $ */

#include <strings.h>
#include <stdio.h>
#include <sys/signal.h>

#include "audio.h"
#include "audioproto.h"
#include "audiodev.h"
#include "os.h"
#include "resource.h"
#include "opaque.h"
#include "servermd.h"
#include "diastruct.h"
#include "task.h"
#include "ddafuncs.h"

extern char *deviceName;
char *ConnectionInfo;
aConnSetupPrefix connSetupPrefix;

extern void SetInputCheck();
extern void InitProcVectors();
extern void InitEvents();
extern void InitExtensions();
extern ABool InitClientResources();
static ABool CreateConnectionBlock();
extern void ResetWellKnownSockets();
extern void CloseDownDevices();

int connBlockScreenStart;

static int restart = 0;

void
NotImplemented()
{
    FatalError("Not implemented");
}

main(argc, argv)
    int		argc;
    char	*argv[];
{
    int		i;
    long	alwaysCheckForInput[2];

    /* Notice if we're restart.  Probably this is because we jumped through
     * uninitialized pointer */
    if (restart)
	FatalError("server restarted. Jumped through uninitialized pointer?\n");
    else
	restart = 1;

    /* These are needed by some routines which are called from interrupt
     * handlers, thus have no direct calling path back to main and thus
     * can't be passed argc, argv as parameters */
    argcGlobal = argc;
    argvGlobal = argv;
    ProcessCommandLine(argc, argv);

    alwaysCheckForInput[0] = 0;
    alwaysCheckForInput[1] = 1;
    while(1)
    {
	serverGeneration++;
	InitBlockAndWakeupHandlers();
	/* Perform any operating system dependent initializations you'd like */
	OsInit();		
	if(serverGeneration == 1)
	{
	    CreateWellKnownSockets();
	    InitProcVectors();
	    clients = (ClientPtr *)xalloc(MAXCLIENTS * sizeof(ClientPtr));
	    if (!clients)
		FatalError("couldn't create client array");
	    for (i=1; i<MAXCLIENTS; i++) 
		clients[i] = NullClient;
	    serverClient = (ClientPtr)xalloc(sizeof(ClientRec));
	    if (!serverClient)
		FatalError("couldn't create server client");
            serverClient->sequence = 0;
            serverClient->closeDownMode = ARetainPermanent;
            serverClient->clientGone = FALSE;
	    serverClient->index = 0;
	    serverClient->clientAsMask = (AMask)0;
	    InitTaskQueue();
	}
	else
	    ResetWellKnownSockets ();
        clients[0] = serverClient;
        currentMaxClients = 1;

	if (!InitClientResources(serverClient))      /* for root resources */
	    FatalError("couldn't init server resources");

	if(serverGeneration == 1)
	{
	    SetInputCheck(&alwaysCheckForInput[0], &alwaysCheckForInput[1]);
	    audioDeviceInfo.arraySize = MAXDEVICES;
	    audioDeviceInfo.numDevices = 0;

	    InitAtoms();    
	    InitEvents();
	    InitDevices(&audioDeviceInfo, argc, argv); 
	}

	InitExtensions(argc, argv);

	if (!CreateConnectionBlock())
	    FatalError("could not create connection block info");

	Dispatch();

	/* Now free up whatever must be freed */

	CloseDownExtensions();
	FreeAllResources();
	CloseDownDevices();
	if (dispatchException & DE_TERMINATE)
	{
	    ddaGiveUp();
	    break;
	}

	xfree(ConnectionInfo);
    }
    exit(0);
}

static int padlength[4] = {0, 3, 2, 1};

static ABool
CreateConnectionBlock()
{
    aConnSetup setup;
    int i,
        lenofblock,
        sizesofar = 0;
    aDevice device;
    char *pBuf;
    AudioDevicePtr aDev;
    
    /* Leave off the ridBase and ridMask, these must be sent with 
       connection */

    setup.release = VENDOR_RELEASE;
    /*
     * per-server parameters are defined in Amd.h
     */

    setup.nbytesVendor = strlen(VENDOR_STRING); 
    setup.maxRequestSize = MAX_REQUEST_SIZE;
    setup.numDevices = audioDeviceInfo.numDevices;

    lenofblock = sizeof(aConnSetup) + 
            ((setup.nbytesVendor + 3) & ~3) +
	    ((setup.numDevices * sizeof(aDevice)));
    ConnectionInfo = (char *) xalloc(lenofblock);
    if (!ConnectionInfo)
	return FALSE;

    bcopy((char *)&setup, ConnectionInfo, sizeof(aConnSetup));
    sizesofar = sizeof(aConnSetup);
    pBuf = ConnectionInfo + sizeof(aConnSetup);

    bcopy(VENDOR_STRING, pBuf, (int)setup.nbytesVendor);
    sizesofar += setup.nbytesVendor;
    pBuf += setup.nbytesVendor;
    i = padlength[setup.nbytesVendor & 3];
    sizesofar += i;
    while (--i >= 0)
        *pBuf++ = 0;

    for (i= 0; i < setup.numDevices; i++) {
	aDev = audioDeviceInfo.devices[i];
	device.playNchannels = aDev->playNchannels;
	device.recordNchannels = aDev->recNchannels;
	device.numberOfInputs = aDev->numberOfInputs;
	device.numberOfOutputs = aDev->numberOfOutputs;
	device.inputsFromPhone = aDev->inputsFromPhone;
	device.outputsToPhone = aDev->outputsToPhone;
	device.playSampleFreq = aDev->playSampleFreq;
	device.playBufType = aDev->playBufType;
	device.playNSamplesBuf = aDev->playNSamplesBuf;
	device.recordSampleFreq = aDev->recSampleFreq;
	device.recordBufType = aDev->recBufType;
	device.recordNSamplesBuf = aDev->recNSamplesBuf;
	bcopy ((char *) &device, pBuf, sizeof(aDevice));
	pBuf += sizeof(aDevice);
	sizesofar += sizeof(aDevice);
    }
    connBlockScreenStart = sizesofar;
    connSetupPrefix.success = aTrue;
    connSetupPrefix.length = lenofblock/4;
    connSetupPrefix.majorVersion = A_PROTOCOL;
    connSetupPrefix.minorVersion = A_PROTOCOL_REVISION;
    return TRUE;
}

