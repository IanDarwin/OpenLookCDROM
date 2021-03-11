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
static char dda_c_rcsid[]="$Header: /crl/audio/AF/server/dda/conf/RCS/dda.c,v 1.3 1994/02/01 18:46:38 tml Exp $";
#endif

#include <stdlib.h>
#include <string.h>
#include <include/audio.h>
#include <include/audioproto.h>
#include <server/include/dia.h>
#include <server/include/audiodev.h>
#include <server/include/misc.h>
#include <server/include/input.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/ioctl.h>

#include "confdda.h"

static int		nArgDevs=0;
static char		*argDevList[MAXPHYSDEVICES];

int 			nDev=0;
confPhysDevice		physDevices[MAXPHYSDEVICES];	

/*
 * Externs
 */
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

void MakeADev(int device)
{
  AudioDevicePtr aDev;
  if ((aDev=MakeDevice()) == ((AudioDevicePtr) NULL)) {
    ErrorF("Could not allocate an audio device in dda\n");
    exit(1);
  }
  aDev->devPtr = (pointer) &physDevices[nDev];
  if (!hifiInit(aDev, hifi_rate, device)) {
    ErrorF("Could not initialize hifi AudioDev.\n");
    exit(1);
  }
}

static int BeenHere = FALSE;

void
InitDevices(int argc, char **argv)
{
  char *devName;
  int status;

  if (BeenHere) return;

  BeenHere = TRUE;
  while(((devName=getNextDevName()) != (char *)NULL)&&(nDev<MAXPHYSDEVICES)){

    status = confInit(devName, &physDevices[nDev]); /* init physical device */
    if (status < 0)  FatalError("Cant open device\n");

    /* 
     *  init logical devices
     */
    MakeADev(HIFI_STEREO);
    MakeADev(HIFI_LEFT);
    MakeADev(HIFI_RIGHT);
 
    ++nDev;
  }
}

static int dindex=0;
char	*
getNextDevName()
{
    char    *ep;

    /* Return command line list of arguments. */
    if (nArgDevs > 0){
	if(dindex<nArgDevs)
		ep = argDevList[dindex++];
	else
		ep = (char *)NULL;
    }else if(dindex==0){
	if( (ep=getenv("AUDIO_DEVICE")) == (char *)NULL)
		ep = DEFAULT_AUDIO_DEVICE;
	++dindex;
    }else
	ep = (char *)NULL;
    return ep;
}


/*
 * DDA - specific abort routine.  Called by AbortServer().
 */
void
ddashutdown()
{
    int	i;

    for(i=0;i<nDev;++i){
	if (physDevices[i].state != STATE_CLOSED)
	    confClose(&physDevices[i]);
    }
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
ddaProcessArgument (int argc, char *argv[], int i)
{
    int			argind=i;
    int			skip;
    void		ddaUseMsg();

    skip = 0;
    if (strcmp( argv[argind], "-device") == 0)
    {
	if (++argind < argc)
	{
	  if(nArgDevs < MAXPHYSDEVICES) argDevList[nArgDevs++] = argv[argind];
	  skip = 2;
	}
	else
	  return 0;	/* failed to parse */
    } else if (!strcmp(argv[argind], "-eventlog")) {
	    eventLog = 1;	
            return 1;
    } else if (!strcmp(argv[argind], "-hrate")) {
	if(++argind < argc) {
	    hifi_rate = atoi(argv[argind]);	
            return 2;
	} else
	    ErrorF("-hrate expects a parameter\n");
	return 0;
    }
    return skip;
}

void
ddaUseMsg()
{
    ErrorF ("\n");
    ErrorF ("ADevice Dependent Usage\n");
    ErrorF ("\n");
    ErrorF ("-device dev_name  Name of audio device to use.\n");
    ErrorF ("-hrate <#>        Sets the ADC/DAC sampling rate\n");
    ErrorF("                   use {5512, 6615, 8000, 9600, 11025, 16000,\n");
    ErrorF("                       18900, 22050, 27428, 32000, 33075,\n");
    ErrorF("                       37800, 44100, or 48000}\n");
}

void
ProcessInputEvents()
{
}

