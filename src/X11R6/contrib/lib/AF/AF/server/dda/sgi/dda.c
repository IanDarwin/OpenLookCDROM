/***********************************************************
Copyright 1993 by Stichting Mathematisch Centrum, Amsterdam, The Netherlands.

                        All Rights Reserved

Permission to use, copy, modify, and distribute this software and its 
documentation for any purpose and without fee is hereby granted, 
provided that the above copyright notice appear in all copies and that
both that copyright notice and this permission notice appear in 
supporting documentation, and that the names of Stichting Mathematisch
Centrum or CWI not be used in advertising or publicity pertaining to
distribution of the software without specific, written prior permission.

STICHTING MATHEMATISCH CENTRUM DISCLAIMS ALL WARRANTIES WITH REGARD TO
THIS SOFTWARE, INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND
FITNESS, IN NO EVENT SHALL STICHTING MATHEMATISCH CENTRUM BE LIABLE
FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT
OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
******************************************************************/

/* SGI Indigo device dependent audio server */

/* All public entry points in this file are referenced from the dia code */

/* Initial design:
   - use the same sampling rate for play and record
   - fix the sampling rate with a command line switch
   - use different device numbers for different sample formats
*/

#include <stdlib.h>
#include <string.h>

#include <server/include/os.h>
#include <server/include/audiodev.h>
#include "ddafuncs.h"
#include "sgi.h"

/*ARGSUSED*/
void
InitDevices(AudioDeviceInfo *junk, int argc, char **argv)
{
	AudioDevicePtr aDev, aDev0;

	/* Device 0 is the U-LAW variant */
	if ((aDev = MakeDevice()) == NULL)
		FatalError("InitDevices: MakeDevice failed\n");
	if (!sgiDevInit(aDev))
		FatalError("InitDevices: sgiDevInit failed\n");
	aDev->playBufType = MU255;
	aDev->playNchannels = 1;
	aDev->recBufType = MU255;
	aDev->recNchannels = 1;

	aDev0 = aDev; /* Save device 0 to clone it */

	/* Device 1 is the mono LIN16 variant */
	if ((aDev = MakeDevice()) == NULL)
		FatalError("InitDevices: MakeDevice failed\n");
	*aDev = *aDev0;
	aDev->playBufType = LIN16;
	aDev->playNchannels = 1;
	aDev->recBufType = LIN16;
	aDev->recNchannels = 1;

	/* Device 2 is the stereo LIN16 variant */
	if ((aDev = MakeDevice()) == NULL)
		FatalError("InitDevices: MakeDevice failed\n");
	*aDev = *aDev0;
	aDev->playBufType = LIN16;
	aDev->playNchannels = 2;
	aDev->recBufType = LIN16;
	aDev->recNchannels = 2;

	/* XXX Should have two more for separate left and right as well */
}

void
AbortDDA()
{
}

void
ddaGiveUp()
{
}

int
ddaProcessArgument (argc, argv, iarg)
	int argc;
	char *argv[];
	int iarg;
{
	if (iarg+1 < argc && strcmp(argv[iarg], "-rate") == 0) {
		int rate = atoi(argv[iarg+1]);
		int i;
		/* Check whether it's a valid sampling rate */
		for (i = sgi_n_validrates; --i >= 0; ) {
			if (rate == sgi_validrates[i])
				break;
		}
		if (i < 0) {
			UseMsg();
			exit(1);
		}
		sgi_rate = rate;
		return 2; /* Have processed this argument and the next */
	}
	else {
		return 0; /* Haven't processed this argument */
	}
}

void
ddaUseMsg()
{
	int i;
	ErrorF("Asgi Dependent Usage:\n");
	ErrorF("-rate number           input sampling rate; ");
	ErrorF("valid sampling rates are:\n");
	ErrorF("                      ");
	for (i = 0; i < sgi_n_validrates; i++)
		ErrorF(" %d", sgi_validrates[i]);
	ErrorF(";\n");
	ErrorF("                       default: 8000\n");
}
