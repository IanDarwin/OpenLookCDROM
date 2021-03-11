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
#include <include/audio.h>
#include <include/audioproto.h>
#include <server/include/dia.h>
#include <server/include/audiodev.h>
#include <server/include/misc.h>
#include <server/include/input.h>
#include <server/include/task.h>
#include "dda.h"
#include "physdevice.h"
#include "event.h"
#include "max_io.h"
#include "ddafuncs.h"

static char		*argDevPtr=(char *)NULL;

maxPhysDevice		physDevices[MAXPHYSDEVICES];	
physToPseudoMap		physmap[MAXPHYSDEVICES];

/*
 * Externs
 */
extern int  errno;
extern char *getenv();

/*
 * Forwards and extern.
 */
char *getNextDevName();

/*
 * It all begins here...
 * This dda initialization entry point is invoked by the dia at
 * server start-up time.
 */
static int BeenHere = FALSE;
/*ARGSUSED*/
void
InitDevices(AudioDeviceInfo *junk, int argc, char **argv)
{
    char *devName;
    AudioDevicePtr aDev;

    if (BeenHere)
      return;

    BeenHere = TRUE;

    if( (devName=getNextDevName()) == (char *)NULL) return;

    /*
     * Initialize the physical device.
     */
    initMAX(devName, &physDevices[0]);

    /* 
     * Codec initialization. 
     */
    if ((aDev=MakeDevice()) == ((AudioDevicePtr) NULL)) 
	    FatalError("Could not allocate an audio device in dda\n");
    physmap[0].primDev = aDev;
    aDev->devPtr = (pointer) &physDevices[0];
    if (!codecPrimaryInit(aDev))
	    FatalError("Could not initialize primary codec AudioDev.\n");
    
    /* There is no secondary audio device or hifi device. */
    physmap[0].secDev = (AudioDevicePtr) NULL;
    physmap[0].hifiDev = (AudioDevicePtr) NULL;
}

char	*
getNextDevName()
{
    char    *ep;

    /* Return command line list of arguments. */
    if (argDevPtr != (char *)NULL){
	ep = argDevPtr;
    }else{
	if( (ep=getenv("AUDIO_DEVICE")) == (char *)NULL)
		ep = DEFAULT_AUDIO_DEVICE;
    }
    return ep;
}



/*
 * DDA - specific abort routine.  Called by AbortServer().
 */
void
ddashutdown()
{
    if (physDevices[0].state != STATE_CLOSED)
	closeMAX(&physDevices[0]);
}

void
AbortDDA()
{
    ddashutdown();
}

/* Called by GiveUp(). */
void
ddaGiveUp()
{
    ddashutdown();
}


int
ddaProcessArgument (argc, argv, i)
    int argc;
    char *argv[];
    int i;
{
    int			argind=i;
    int			skip;
    void		ddaUseMsg();

    skip = 0;
    if (strcmp( argv[argind], "-device") == 0)
    {
	if (++argind < argc)
	{
	    argDevPtr = argv[argind];
	    skip = 2;
	}
	else
	    return 0;	/* failed to parse */
    }
    return skip;
}

void
ddaUseMsg()
{
    ErrorF ("\n");
    ErrorF ("\n");
    ErrorF ("ADevice Dependent Usage\n");
    ErrorF ("\n");
    ErrorF ("-device dev_name   Name of audio device to use.\n");
}


