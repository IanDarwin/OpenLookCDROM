/* X display of audio power.
 *
 *	Tom Levergood
 *	Cambridge Research Lab
 *	Digital Equipment Corp.
 *	tml@crl.dec.com
 *
 *	Created: 22 March 1991
 *
 *	$Header: /crl/audio/AF/contrib/clients/afxpow/RCS/xpow.c,v 1.16 1994/06/03 17:33:39 jg Exp $
 *	$CRL$
 *
 */
/*              Copyright 1990, 1991, 1992, 1993 Digital Equipment Corporation
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
static char rcsid[]="$Header: /crl/audio/AF/contrib/clients/afxpow/RCS/xpow.c,v 1.16 1994/06/03 17:33:39 jg Exp $";
#endif

#include <stdio.h>
#include <math.h>
#include <sys/types.h>
#include <sys/dir.h>
#include <AF/AFlib.h>
#include <AF/AFUtils.h>
#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
#include <X11/Shell.h>
#include <X11/Xaw/Cardinals.h>
#include <X11/Xaw/Paned.h>
#include "LineChart.h"

#define	LIMIT(low, val, high) \
	  if (val < (low)) val = (low); \
	  else \
	  if ((val) > (high)) val = (high);

#define BUFSIZE  (250<<3)
#define	MSTOTICKS(x)	((x)<<3)

typedef struct {
  Boolean log;
  int audioDevice;
} OptionsRec;

OptionsRec options;
#define Offset(f) XtOffsetOf(OptionsRec,f)
static Boolean defaultFalse = False;

static XtResource resources[] = {
  {"audioDevice", "AudioDevice", XtRInt, sizeof(int), Offset(audioDevice),
   XtRImmediate, (XtPointer) NULL},
  {"log", "Log", XtRBoolean, sizeof(Boolean), Offset(log),
   XtRBoolean, (XtPointer) &defaultFalse},
};
#undef Offset
static XrmOptionDescRec optionList[] = {
  {"-audioDevice", "audioDevice", XrmoptionSepArg, (XtPointer) NULL},
  {"-d", "audioDevice", XrmoptionSepArg, (XtPointer) NULL},
  {"-dev", "audioDevice", XrmoptionSepArg, (XtPointer) NULL},
  {"-log", "log", XrmoptionNoArg, (XtPointer)"True"}
} ;

Widget TopWidget;
XtAppContext appContext;
AFAudioConn *aud;

/*
 * Find a suitable default device (the first device not connected to the phone),
 *  returns device number.
 *
 * Returns -1 if no suitable device can be found.
 */
int FindDefaultDevice(AFAudioConn *aud)
{
        AFDeviceDescriptor *aDev;
        int     i;

        for(i=0; i<ANumberOfAudioDevices(aud); i++) {
                aDev = AAudioDeviceDescriptor(aud, i);
                if(aDev->inputsFromPhone == 0 && aDev->outputsToPhone == 0)
                        return i;
        }
        return -1;
}

void
Usage(progname)
char *progname;
{
    fprintf(stderr, "\nUsage: %s\n", progname);
    fprintf(stderr, "options: [-dev {0,1}] \n");
    exit(1);
}

/*ARGSUSED*/
static void
AudioPower(w, client_data, ret_val)
Widget w;
caddr_t client_data;
caddr_t ret_val;
{
	*(int *) ret_val = record_power();
}

#define REFPOW  ((0.707 * 8031.0) * (0.707 * 8031.0)) 
/* REFPOW is the power of a sine wave at digital clipping, which is
   +3.16 dbm */

static unsigned char buf[BUFSIZE];
record_power()
{
	ATime t,rt;
	AFDeviceDescriptor *aDev = AAudioDeviceDescriptor (aud,options.audioDevice);
	int i;
	int unitSize;
	int nb;
	double pow = 0.0;
	static ATime lastTime=0;
	AC ac;
	AFSetACAttributes attributes;
	/*
	 * Create the audio context for the recording
	 */
	ac = AFCreateAC(aud, options.audioDevice, 0, NULL);
	if (ac == NULL) {
		fprintf(stderr, "Could not create AC.\n");
		exit(1);
	}
	unitSize = AF_sample_sizes[MU255].bytes_per_unit*ac->device->recNchannels;
	/*
	 * Make sure that the recorded data is mulaw
	 */
	if (aDev->recBufType != MU255) {
	  attributes.type = MU255;
	  AFChangeACAttributes (ac, ACEncodingType, &attributes);
	  AFSync (aud,0);
	}
	/*
	 * Now get the audio time and figure out how many samples to read
	 */
	t = AFGetTime(ac);
	nb = ((t-lastTime) < BUFSIZE ? (t-lastTime) : BUFSIZE);
	/*
	 * Make sure that the number of samples is a multiple of the 
	 * sample unit size.
	 */
	if (nb%unitSize)
	  nb -= nb%unitSize;

	if(nb>0){
		AFRecordSamples(ac,t,nb,buf,ABlock);
		for(i=0;i<nb;++i)
			pow += (double)(AF_exp_u[buf[i]]  * AF_exp_u[buf[i]] );
		pow /= (double)nb;
	
		if(pow == 0.0) pow = -80.0;
		else	pow = 10.0 * log10(pow/REFPOW);
		pow += 3.16;
		LIMIT(-80.0,pow,3.0);
	}
	if (options.log) fprintf(stdout, "%6.2f dBm\n", pow);
	lastTime = t;
	AFFreeAC(ac);
if(((int)pow) == 0x7fff) exit(0);
	return((int)pow);
}

void
main(argc, argv)
int argc;
char *argv[];
{
	Widget pane, chart;
	Arg args[10];
	Cardinal arg_cnt;
	int	i;
	char title[32];

	TopWidget = XtAppInitialize(&appContext, "Xpow", optionList, 
				    XtNumber(optionList),
				    &argc, argv, NULL, NULL, ZERO);
	XtGetApplicationResources (TopWidget, (XtPointer) &options,
				   resources, XtNumber(resources), (Arg *) NULL, 0);

	if ( (aud = AFOpenAudioConn("")) == NULL) {
		fprintf(stderr, "arecord: can't open connection.\n");
		exit(1);
	}
	if(options.audioDevice < 0)
		options.audioDevice = FindDefaultDevice(aud);

	arg_cnt = 0;
  	pane = XtCreateManagedWidget("pane", panedWidgetClass, TopWidget,
				      args, arg_cnt);

	arg_cnt = 0;
	XtSetArg(args[arg_cnt], XtNlabel, "Ref 0 dBm"); arg_cnt++;
	XtSetArg(args[arg_cnt], XtNmaxValue, 3); arg_cnt++;
	XtSetArg(args[arg_cnt], XtNminValue, -80); arg_cnt++;
	XtSetArg(args[arg_cnt], XtNupdate, 250); arg_cnt++;
	chart = XtCreateManagedWidget("linechart", lineChartWidgetClass,
					     pane, args, arg_cnt);
	XtAddCallback(chart, XtNgetValue, (XtCallbackProc)AudioPower, 
		(XtPointer)NULL);

	XtRealizeWidget(TopWidget);

	sprintf (title,"xpow -d %d",options.audioDevice);
	XStoreName (XtDisplay(TopWidget),XtWindow(TopWidget),title);

	XtAppMainLoop(appContext);

}
