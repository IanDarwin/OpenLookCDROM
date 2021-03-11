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
#include "jv_io.h"
#include "dspio.h"
#include "vector.h"
#include "ddafuncs.h"

static char 		kernel[256]="\0";
static int		nArgDevs=0;
static char		*argDevList[MAXPHYSDEVICES];

int 			nDev=0;
jvPhysDevice		physDevices[MAXPHYSDEVICES];	
physToPseudoMap		physmap[MAXPHYSDEVICES];

int			hifi_enable = TRUE;	/* set FALSE to disable the
						   Hifi part of the driver */

/*
  These are the DSP Port C bits, which (for many external devices) sets the
  sample rate.
*/
int dsp_portc_bits = 0;

/*
  This is the Hifi sample rate, as set by the -hrate command line option.
*/
int hifi_rate = 8000;
int hifi_rate_cr = DSP_8KHZ_RATE;

/*
  These are values that get written to the DSP's CRA and CRB registers
  (that configure the DSP port).

  These defaults are for the JVideo built-in stereo ADC/DAC.
*/

int dsp_cra = 0x4100;		/* 16-bit words, 2-frame */
int dsp_crb = 0x7208;		/* tx int, network mode, sync */


/*
 * Externs
 */
extern int  errno;
extern char *getenv();

/*
 * Forwards and extern.
 */
char *getNextDevName(void);

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
    AudioDevicePtr prim;

    if (BeenHere)
      return;

    BeenHere = TRUE;
    while(((devName=getNextDevName()) != (char *)NULL)&&(nDev<MAXPHYSDEVICES)){
	char *ep;
	char *up = "Ajvmain.lod";

	/* First see if explicit file is named */
	if ( (strlen(kernel) == 0) && ((ep = getenv("AF_UKERNEL")) != NULL)) {
 	  strcpy(kernel,ep); 
	} 
	/* Now check for a local path env variable. */
	if ( (strlen(kernel) == 0) && ((ep = getenv("LODPATH")) != NULL)) {
 	  strcpy(kernel,ep); 
	  strcat(kernel,"/");
	  strcat(kernel,up);
	}
	/* Now use the default, if still nothing. */
	if ( strlen(kernel) == 0 ) {
	  strcpy(kernel, LODPATH);
	  strcat(kernel,"/");
	  strcat(kernel,up);
	}
	/*
 	 * Initialize the physical device.
	 */
	initJv(devName, kernel, &physDevices[nDev]);

	/*
 	 * Initialize each audio device.
	 */

	/* 
	 * Input initialization
	 */
	jvInputInit(&physDevices[nDev], nDev);

	/* 
	 *  Hifi device initialization.  Currently, both Hifi channels are
	 *  treated as different devices.
	 */
	if(hifi_enable) {
	    if ((aDev=MakeDevice()) == ((AudioDevicePtr) NULL)) {
		ErrorF("Could not allocate an audio device in dda\n");
		exit(1);
	    }
	    prim = physmap[nDev].hifiDev[0] = aDev;
	    aDev->devPtr = (pointer) &physDevices[nDev];

	    if (!hifiInit(aDev, hifi_rate, HIFI_STEREO, prim)) {
		ErrorF("Could not initialize hifi AudioDev.\n");
		exit(1);
	    }
	    if ((aDev=MakeDevice()) == ((AudioDevicePtr) NULL)) {
		ErrorF("Could not allocate an audio device in dda\n");
		exit(1);
	    }
	    if (!hifiInit(aDev, hifi_rate, HIFI_LEFT, prim)) {
		ErrorF("Could not initialize hifi AudioDev.\n");
		exit(1);
	    }
	    if ((aDev=MakeDevice()) == ((AudioDevicePtr) NULL)) {
		ErrorF("Could not allocate an audio device in dda\n");
		exit(1);
	    }
	    if (!hifiInit(aDev, hifi_rate, HIFI_RIGHT, prim)) {
		ErrorF("Could not initialize hifi AudioDev.\n");
		exit(1);
	    }
	}
	++nDev;
    }
}

static int dindex=0;
char	*
getNextDevName(void)
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
ddashutdown(void)
{
    int	i;

    for(i=0;i<nDev;++i){
	if (physDevices[i].state != STATE_CLOSED)
	    closeJv(&physDevices[i]);
    }
}

void
AbortDDA(void)
{
    ddashutdown();
}

/* Called by GiveUp(). */
void
ddaGiveUp(void)
{
    ddashutdown();
}

int
assertRate(int rate)
{
	switch(rate){
	case  8000:
		hifi_rate_cr = DSP_8KHZ_RATE;
		break;
	case 16000:
		hifi_rate_cr = DSP_16KHZ_RATE;
		break;
	case 22000:
		hifi_rate_cr = DSP_22KHZ_RATE;
		break;
	case 32000:
		hifi_rate_cr = DSP_32KHZ_RATE;
		break;
	case 44100:
		hifi_rate_cr = DSP_44KHZ_RATE;
		break;
	case 48000:
		hifi_rate_cr = DSP_48KHZ_RATE;
		break;
	default:
		return 1;
	}
	return 0;
}

int
ddaProcessArgument (int argc, char **argv, int i)
{
    int			argind=i;
    int			skip;

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
    } else if (strcmp( argv[argind], "-ukernel") == 0) {
	if (++argind < argc)
	{
	    strcpy(kernel, argv[argind]);
	    skip = 2;
	}
	else
	    return 0;
    } else if(!strcmp(argv[argind], "-hrate")) {
	if(++argind < argc) {
	    hifi_rate = atoi(argv[argind]);
	    if((hifi_rate <= 0) || (assertRate(hifi_rate))) {
		ErrorF("-hrate must be {8000,16000,22000,32000,44100,or 48000}\n");
		return 0;
	    }
            return 2;
	} else
	    ErrorF("-hrate expects a parameter\n");
	return 0;
    }
    return skip;
}


void
ddaUseMsg(void)
{
    ErrorF ("\n");
    ErrorF ("\n");
    ErrorF ("ADevice Dependent Usage\n");
    ErrorF ("\n");
    ErrorF ("-device dev_name   Name of audio device to use.\n");
    ErrorF ("-ukernel lod_file  Name of DSP lodfile.\n");
    ErrorF ("-hrate <#>         Sets the ADC/DAC sampling rate\n");
    ErrorF ("                   Use one of {8000,16000,22000,32000,44100,48000}\n");
}


