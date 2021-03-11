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
#include "lofi_io.h"
#include "phone.h"
#include "dspio.h"
#include "ddafuncs.h"

static char 		kernel[256] = "\0";
char			*coeffname=NULL;
static int		nArgDevs=0;
static char		*argDevList[MAXPHYSDEVICES];

int 			nDev=0;
lofiPhysDevice		physDevices[MAXPHYSDEVICES];	
physToPseudoMap		physmap[MAXPHYSDEVICES];

int			hifi_enable = TRUE;	/* set FALSE to disable the
						   Hifi part of the driver */

/* general notes:
   Probably none of the following global variables should exist.  They
   shoulc all be arrays indexed by physdevice or fields in the
   dda data structure

   A way is needed to communicate command line args between processargs
   and InitDevices, without using globals.
*/

/* This parameter, if TRUE, says we have plugged in an Ariel Teleport
   dsp port device
*/
int teleport = FALSE;

/*  
  This parameter sets the state of the DAA's gain control bit.  A 1 value
  (default) DISABLES the AGC in the product DECaudio's DAA and 0 value 
  enables the DAA.   On pre-product DECaudios (lofis), this bit has
  no effect.
*/
int daa_gain_control = 1;

/*
  These are the DSP Port C bits, which (for many external devices) sets the
  sample rate.
*/
int dsp_portc_bits = 4;

/*
  This is the Hifi sample rate, as set by the -hrate command line option.
*/
int hifi_rate = 44100;

/*
  These are values that get written to the DSP's CRA and CRB registers
  (that configure the DSP port).

  These defaults are for the DECaudio's built-in stereo DAC.
*/
int dsp_cra = 0x4100;		/* 16-bit words, 2-frame */
int dsp_crb = 0x7b1f;		/* tx int, network mode, sync */

#ifdef OMIT
int dsp_cra = 0x4100;		/* 16-bit words, 2-frame network mode */
int dsp_crb = 0x7a00;		/* rx int, network mode, synchronous */
#endif

/*
 * Externs
 */
extern int  errno;
extern char *getenv();

/*
 * Forwards and extern.
 */
char *getNextDevName(void);
ABool hifiInit(AudioDevicePtr aDev, int rate, int type, AudioDevicePtr prim,
               int teleport);

/*
 * It all begins here...
 * This dda initialization entry point is invoked by the dia at
 * server start-up time.
 */
static int BeenHere = FALSE;
/*ARGSUSED*/
void
InitDevices(AudioDeviceInfo *audioDeviceInfo, int argc, char **argv)
{
    char *devName;
    AudioDevicePtr aDev;
    AudioDevicePtr prim;

    if (BeenHere)
      return;

    BeenHere = TRUE;
    while(((devName=getNextDevName()) != (char *)NULL)&&(nDev<MAXPHYSDEVICES)){
	char *ep;
	char *up = "main.lod";

	if (teleport) up = "mainteleport.lod" ;
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
	initLoFi(devName, kernel, &physDevices[nDev]);

	/*
 	 * Initialize each audio device, Primary, Secondary, Hi-Fi.
	 */

	/* 
	 * Input initialization (turns on DSP and TLI interrupts.)
	 */
	lofiInputInit(&physDevices[nDev], nDev);

	/* 
	 * Primary Codec initialization. 
	 */
	if ((aDev=MakeDevice()) == ((AudioDevicePtr) NULL)) {
		ErrorF("Could not allocate an audio device in dda\n");
		exit(1);
	}
	physmap[nDev].primDev = aDev;
	aDev->devPtr = (pointer) &physDevices[nDev];
	if (!codecPrimaryInit(aDev)) {
		ErrorF("Could not initialize primary codec AudioDev.\n");
		exit(1);
	}
	
	/* 
	 * Secondary Codec initialization (before primary!) 
	 */
	if ((aDev=MakeDevice()) == ((AudioDevicePtr) NULL))  {
		ErrorF("Could not allocate an audio device in dda\n");
		exit(1);
	}
	physmap[nDev].secDev = aDev;
	aDev->devPtr = (pointer) &physDevices[nDev];
	if (!codecSecondaryInit(aDev)){
		ErrorF("Could not initialize secondary codec AudioDev.\n");
		exit(1);
	}
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
            /* The first hifi device is the primary.  In teleport
             * mode, there are two: LEFT and RIGHT.  In normal
             * mode there are three: STEREO, LEFT, and RIGHT.
             */
            if (!teleport) {
	      if (!hifiInit(aDev, hifi_rate, HIFI_STEREO, prim, teleport)) {
		ErrorF("Could not initialize hifi AudioDev.\n");
		exit(1);
	      }
              if ((aDev=MakeDevice()) == ((AudioDevicePtr) NULL)) {
                ErrorF("Could not allocate an audio device in dda\n");
                exit(1);
              }
            }
	    if (!hifiInit(aDev, hifi_rate, HIFI_LEFT, prim, teleport)) {
		ErrorF("Could not initialize hifi AudioDev.\n");
		exit(1);
	    }
	    if ((aDev=MakeDevice()) == ((AudioDevicePtr) NULL)) {
		ErrorF("Could not allocate an audio device in dda\n");
		exit(1);
	    }
	    if (!hifiInit(aDev, hifi_rate, HIFI_RIGHT, prim, teleport)) {
		ErrorF("Could not initialize hifi AudioDev.\n");
		exit(1);
	    }
	}
	nDev++;
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
	    closeLoFi(&physDevices[i]);
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
ddaProcessArgument (int argc, char **argv, int i)
{
    int			argind=i;
    int			skip;
    char		*mode;

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
    }
    else if (strcmp( argv[argind], "-ukernel") == 0)
    {
	if (++argind < argc)
	{
	    strcpy(kernel, argv[argind]);
	    skip = 2;
	}
	else
	    return 0;
    }
    else if (strcmp( argv[argind], "-pcoeff") == 0)
    {
	if (++argind < argc)
	{
	    coeffname = argv[argind];
	    skip = 2;
	}
	else
	    return 0;
    } else if(!strcmp(argv[argind], "-nohifi")) {
	hifi_enable = FALSE;
	skip = 1;
    } else if(!strcmp(argv[argind], "-teleport")) {
	teleport = TRUE;
	skip = 1;
    } else if(!strcmp(argv[argind], "-daa_gain")) {
	if(++argind < argc) {
	    daa_gain_control = (atoi(argv[argind]) != 0);
	    skip = 2;
        } else
	    return 0;
    } else if(!strcmp(argv[argind], "-dspc")) {
	if(++argind < argc) {
	    dsp_portc_bits = atoi(argv[argind]);
	    if(dsp_portc_bits >= 0 && dsp_portc_bits <= 7)
		return 2;
	    ErrorF("-dspc paramter out of range (must be between 0 and 7)\n\n");
	} else
	    ErrorF("-dspc expects a parameter\n\n");
	return 0;
    } else if(!strcmp(argv[argind], "-hrate")) {
	if(++argind < argc) {
	    hifi_rate = atoi(argv[argind]);
	    if(hifi_rate > 0)
		return 2;
	    ErrorF("-hrate must be positive\n");
	} else
	    ErrorF("-hrate expects a parameter\n");
	return 0;
    } else if(!strcmp(argv[argind], "-mode")) {
	if(++argind < argc) {
	    mode = argv[argind];
	    switch(mode[0]) {
		case 'i':		/* internal DAC */
			dsp_cra = 0x4100;	/* 16-bit words, 2-frame */
			dsp_crb = 0x7b1f;	/* tx int, network mode, sync */
			hifi_rate = 44100;	/* no choice */
			break;
		case 'e':		/* external DSP port */
			dsp_cra = 0x4100;	/* 16-bit words, 2-frame network mode */
			dsp_crb = 0x7a00;	/* rx int, network mode, synchronous */
			break;
		default:
			ErrorF("\n\n-mode must be one of:\n");
			ErrorF("     internal\n");
			ErrorF("     external\n");
			return 0;
	    }
	    return 2;
	} else
	    ErrorF("-mode expects a parameter\n");
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
    ErrorF ("-device dev_name           Name of audio device to use.\n");
    ErrorF ("-ukernel kernel_lod_file   Name of DSP lodfile.\n");
    ErrorF ("-pcoeff coeff_file         Name of CODEC X filter coefficient file.\n");
    ErrorF ("-nohifi                    Disable Hifi support\n");
    ErrorF ("-daa_gain {0,1}            Set state of DAA's gain control\n");
    ErrorF ("                           (default is 1)\n");
    ErrorF ("-dspc <#>                  Sets the DSP Port C bits\n");
    ErrorF ("-hrate <#>                 Sets the Hi-fi sampling rate\n");
    ErrorF ("-mode {ext,int}            Sets operating mode\n");
}


