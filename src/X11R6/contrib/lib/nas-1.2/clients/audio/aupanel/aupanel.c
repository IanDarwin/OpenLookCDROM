/**
 * Copyright 1993 Network Computing Devices, Inc.
 *
 * Permission to use, copy, modify, distribute, and sell this software and
 * its documentation for any purpose is hereby granted without fee, provided
 * that the above copyright notice appear in all copies and that both that
 * copyright notice and this permission notice appear in supporting
 * documentation, and that the name Network Computing Devices, Inc. not be
 * used in advertising or publicity pertaining to distribution of this
 * software without specific, written prior permission.
 *
 * THIS SOFTWARE IS PROVIDED `AS-IS'.  NETWORK COMPUTING DEVICES, INC.,
 * DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING WITHOUT
 * LIMITATION ALL IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS FOR A
 * PARTICULAR PURPOSE, OR NONINFRINGEMENT.  IN NO EVENT SHALL NETWORK
 * COMPUTING DEVICES, INC., BE LIABLE FOR ANY DAMAGES WHATSOEVER, INCLUDING
 * SPECIAL, INCIDENTAL OR CONSEQUENTIAL DAMAGES, INCLUDING LOSS OF USE, DATA,
 * OR PROFITS, EVEN IF ADVISED OF THE POSSIBILITY THEREOF, AND REGARDLESS OF
 * WHETHER IN AN ACTION IN CONTRACT, TORT OR NEGLIGENCE, ARISING OUT OF OR IN
 * CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *
 * Author:	Greg Renda <greg@ncd.com>
 * 		Network Computing Devices, Inc.
 * 		350 North Bernardo Ave.
 * 		Mountain View, CA  94043
 *
 * $NCDId: @(#)aupanel.c,v 1.11 1994/04/20 22:32:31 greg Exp $
 */

#include <stdio.h>
#ifndef SYSV
#include <audio/Aos.h>		/* for string and other os stuff */
#endif
#include <audio/Afuncs.h> 	/* for bcopy et. al. */
#include <audio/audiolib.h>
#include <audio/soundlib.h>

#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
#include <X11/Shell.h>
#include <X11/Xaw/Cardinals.h>
#include <audio/Xtutil.h>

/* widgets */
#include <X11/Xaw/Form.h>
#include <X11/Xaw/Command.h>
#include <X11/Xaw/Label.h>
#include <X11/Xaw/Toggle.h>
#include <X11/Xaw/SimpleMenu.h>
#include <X11/Xaw/MenuButton.h>
#include <X11/Xaw/SmeBSB.h>

#include <Slider.h>

#define	APP_CLASS		"Aupanel"

#define USAGE "\
usage: aupanel [-a audioserver]\
"

#define MakeCommandButton(w, parent, label, callback)			       \
{									       \
    (w) = XtCreateManagedWidget(label, commandWidgetClass, parent, NULL, 0);   \
									       \
    if ((void *) (callback) != NULL)					       \
	XtAddCallback(w, XtNcallback, callback, g);			       \
}

#define MakeLabel(w, parent, label)					       \
{									       \
    (w) = XtCreateManagedWidget(label, labelWidgetClass, parent, NULL, 0);     \
}

#define MakeWidget(w, parent, type, name)				       \
{									       \
    (w) = XtCreateManagedWidget(name, type, parent, NULL, 0);		       \
}

typedef struct
{
    Widget          top,
                    form,
                    quit,
                    query,
                    menu,
                    menuButton,
                    device,
                    gainSlider,
                    modeLabel,
                    mode;
    AuServer       *aud;
    int             numDevices,
                    deviceNum;
    AuDeviceAttributes *da;
}               GlobalDataRec, *GlobalDataPtr;

static String   defaultResources[] =
{
    "*font:                           *courier-medium-r-normal*140*",
    "*query.label:                    Query",
    "*devices.label:                  Devices",
    "*devices.fromHoriz:              query",
    "*quit.label:                     Quit",
    "*quit.fromHoriz:                 devices",
    "*deviceLabel.label:              Stereo Channel Output",
    "*deviceLabel.fromVert:           query",
    "*deviceLabel.font:               *courier-bold-r-normal*140*",
    "*deviceLabel.borderWidth:        0",
    "*gainSlider.label:               Gain:  %3d%%",
    "*gainSlider.fromVert:            deviceLabel",
    "*gainSlider.min:                 1",
    "*gainSlider.max:                 100",
    "*gainSlider.resizable:           true",
    "*modeLabel.label:                Input mode:",
    "*modeLabel.fromVert:             gainSlider",
    "*modeLabel.borderWidth:          0",
    "*mode.label:                     Microphone",
    "*mode.fromHoriz:                 modeLabel",
    "*mode.fromVert:                  gainSlider",
    NULL
};

static void
fatalError(message, arg)
char           *message,
               *arg;
{
    fprintf(stderr, message, arg);
    fprintf(stderr, "\n");
    exit(1);
}

static void
quitCB(w, gp, call_data)
Widget          w;
XtPointer       gp;
XtPointer       call_data;
{
    exit(0);
}

static void
setDevice(g)
GlobalDataPtr   g;
{
    AuDeviceAttributes *da = &g->da[g->deviceNum];

    AuSetDeviceAttributes(g->aud, AuDeviceIdentifier(da),
			  AuDeviceChangableMask(da) &
			  (AuCompDeviceGainMask | AuCompDeviceLineModeMask),
			  da, NULL);
}

static void
showDevice(g)
GlobalDataPtr   g;
{
    Boolean         modeEnable,
                    gainEnable;
    AuDeviceAttributes *da = &g->da[g->deviceNum];

    XtVaSetValues(g->device, XtNlabel, AuDeviceDescription(da)->data, NULL);

    XtVaSetValues(g->gainSlider, XtNvalue,
		  AuFixedPointRoundUp(AuDeviceGain(da)), NULL);

    modeEnable = AuDeviceChangableMask(da) & AuCompDeviceLineModeMask ?
	True : False;

    XtVaSetValues(g->modeLabel, XtNsensitive, modeEnable, NULL);
    XtVaSetValues(g->mode, XtNsensitive, modeEnable, NULL);

    gainEnable = AuDeviceChangableMask(da) & AuCompDeviceGainMask ?
	True : False;

    XtVaSetValues(g->gainSlider, XtNsensitive, gainEnable, NULL);

    if (modeEnable)
	if (AuDeviceLineMode(da) == AuDeviceLineModeHigh)
	    XtVaSetValues(g->mode, XtNlabel, "Microphone", NULL);
	else
	    XtVaSetValues(g->mode, XtNlabel, "Line", NULL);
}

static void
modeCB(w, gp, call_data)
Widget          w;
XtPointer       gp;
XtPointer       call_data;
{
    GlobalDataPtr   g = (GlobalDataPtr) gp;
    AuDeviceAttributes *da = &g->da[g->deviceNum];

    XtCallActionProc(w, "reset", NULL, NULL, 0);

    if (AuDeviceLineMode(da) == AuDeviceLineModeLow)
    {
	XtVaSetValues(g->mode, XtNlabel, "Microphone", NULL);
	AuDeviceLineMode(da) = AuDeviceLineModeHigh;
    }
    else
    {
	XtVaSetValues(g->mode, XtNlabel, "Line", NULL);
	AuDeviceLineMode(da) = AuDeviceLineModeLow;
    }

    setDevice(g);
}

static void
queryCB(w, gp, call_data)
Widget          w;
XtPointer       gp;
XtPointer       call_data;
{
    GlobalDataPtr   g = (GlobalDataPtr) gp;

    AuFreeDeviceAttributes(g->aud, g->numDevices, g->da);
    g->da = AuListDevices(g->aud, 0, NULL, &g->numDevices, NULL);
    showDevice(g);
}

static void
menuCB(w, gp, call_data)
Widget          w;
XtPointer       gp;
XtPointer       call_data;
{
    GlobalDataPtr   g = (GlobalDataPtr) gp;
    int             i;
    String          string;

    XtVaGetValues(w, XtNlabel, &string, NULL);
    XtVaSetValues(g->device, XtNlabel, string, NULL);

    for (i = 0; i < g->numDevices; i++)
	if (!strcmp(string, AuDeviceDescription(&g->da[i])->data))
	    break;

    g->deviceNum = i;
    showDevice(g);
}

static void
setGain(w, gp, valuep)
Widget          w;
XtPointer       gp;
XtPointer       valuep;
{
    GlobalDataPtr   g = (GlobalDataPtr) gp;
    int             value = (int) valuep;

    AuDeviceGain(&g->da[g->deviceNum]) = AuFixedPointFromSum(value, 0);
    setDevice(g);
}

static void
createWidgets(g)
GlobalDataPtr   g;
{
    int             i;
    Widget          w;

    MakeWidget(g->form, g->top, formWidgetClass, "form");

    MakeCommandButton(g->query, g->form, "query", queryCB);

    g->menu = XtCreatePopupShell("menu", simpleMenuWidgetClass, g->form,
				 NULL, 0);

    g->da = AuListDevices(g->aud, 0, NULL, &g->numDevices, NULL);

    if (!g->numDevices)
	fatalError("no devices");

    for (i = 0; i < g->numDevices; i++)
    {
	MakeWidget(w, g->menu, smeBSBObjectClass,
		   AuDeviceDescription(&g->da[i])->data);
	XtAddCallback(w, XtNcallback, menuCB, g);
    }

    MakeWidget(g->menuButton, g->form, menuButtonWidgetClass, "devices");

    MakeCommandButton(g->quit, g->form, "quit", quitCB);

    MakeLabel(g->device, g->form, "deviceLabel");

    MakeWidget(g->gainSlider, g->form, sliderWidgetClass, "gainSlider");
    XtAddCallback(g->gainSlider, XtNcallback, setGain, g);

    MakeLabel(g->modeLabel, g->form, "modeLabel");
    MakeWidget(g->mode, g->form, toggleWidgetClass, "mode");
    XtAddCallback(g->mode, XtNcallback, modeCB, g);
}

static void
alignWidgets(g)
GlobalDataPtr   g;
{
    Dimension       w,
                    w1;
    Position        x,
                    x1;
    int             d;

    XtVaGetValues(g->mode, XtNwidth, &w, XtNx, &x, NULL);
    XtVaGetValues(g->gainSlider, XtNwidth, &w1, XtNhorizDistance, &d, NULL);
    XtVaSetValues(g->gainSlider, XtNwidth, w1 + (w + x) - w1 - d, NULL);

    XtVaGetValues(g->quit, XtNx, &x1, XtNhorizDistance, &d, XtNwidth, &w1,
		  NULL);
    XtVaSetValues(g->quit, XtNhorizDistance, w - w1 - (x1 - d - x), NULL);
}

int
main(argc, argv)
int             argc;
char          **argv;
{
    GlobalDataRec   globals;
    GlobalDataPtr   g = &globals;
    XtAppContext    appContext;
    char           *audioServer = NULL;

    g->top = XtVaAppInitialize(&appContext, APP_CLASS, NULL, ZERO,
			       &argc, argv, defaultResources, NULL, 0);

    if (argc == 3)
	if (!strncmp(argv[1], "-a", 2))
	    audioServer = argv[2];
	else
	    fatalError(USAGE);
    else if (argc != 1)
	fatalError(USAGE);

    if (!(g->aud = AuOpenServer(audioServer, 0, NULL, 0, NULL, NULL)))
	fatalError("Can't connect to audio server");

    createWidgets(g);
    XtRealizeWidget(g->top);
    alignWidgets(g);

    g->deviceNum = 0;
    showDevice(g);

    AuXtAppAddAudioHandler(appContext, g->aud);
    XtAppMainLoop(appContext);
    return 0;
}
