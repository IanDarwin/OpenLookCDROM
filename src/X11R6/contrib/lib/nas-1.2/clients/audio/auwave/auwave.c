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
 * $NCDId: @(#)auwave.c,v 1.6 1994/04/20 22:30:57 greg Exp $
 */

#include <stdio.h>
#ifndef SYSV
#include <audio/Aos.h> 		/* for string and other os stuff */
#endif
#include <audio/Afuncs.h>	/* for bcopy et. al. */
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
#include <X11/Xaw/AsciiText.h>
#include <X11/Xaw/SimpleMenu.h>
#include <X11/Xaw/MenuButton.h>
#include <X11/Xaw/SmeBSB.h>

#include <Slider.h>

#define	APP_CLASS		"Auwave"
#define	SAMPLE_RATE		8000

#define USAGE "\
usage: auwave [-a audioserver]\
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

#define Invert(w)							      \
{									      \
    Pixel fg, bg;							      \
									      \
    XtVaGetValues(w, XtNforeground, &fg, XtNbackground, &bg, NULL);	      \
    XtVaSetValues(w, XtNforeground, bg, XtNbackground, fg, NULL);	      \
}

typedef struct
{
    Widget          top,
                    form,
                    quit,
                    start,
                    menu,
                    menuButton,
                    freqText,
                    freqSlider,
                    gainSlider,
                    volSlider;
    Display        *dpy;
    AuServer       *aud;
    AuDeviceID      device;
    AuFlowID        flow;
    Atom            wm_delete_window;
    AuBool          gainAdjustable;
}               GlobalDataRec, *GlobalDataPtr;

static GlobalDataPtr globals;			/* for actions */

static String   defaultResources[] =
{
    "*auwave.translations:            #override\\n\
	<Message>WM_PROTOCOLS: quit()",
    "*font:                           *courier-medium-r-normal*140*",
    "*start.label:                    Start",
    "*quit.label:                     Quit",
    "*quit.fromHoriz:                 start",
    "*waveLabel.label:                \\ WaveForm:",
    "*waveLabel.fromVert:             start",
    "*waveLabel.borderWidth:          0",
    "*menuButton.fromHoriz:           waveLabel",
    "*menuButton.fromVert:            start",
    "*menuButton.resizable:           true",
    "*freqLabel.label:                Frequency:",
    "*freqLabel.fromVert:             waveLabel",
    "*freqLabel.borderWidth:          0",
    "*freqText.fromHoriz:             freqLabel",
    "*freqText.fromVert:              waveLabel",
    "*freqText*editType:              edit",
    "*freqText*string:                440",
    "*freqText.translations:          #override\\n\
	<Key>Return: updateFrequency()\\n\
	<LeaveNotify>: updateFrequency()",
    "*freqSlider.fromHoriz:           freqText",
    "*freqSlider.fromVert:            waveLabel",
    "*freqSlider.min:                 60",
    "*freqSlider.max:                 3999",
    "*freqSlider.value:               440",
    "*freqSlider.resizable:           true",
    "*volSlider.fromVert:             freqText",
    "*volSlider.label:                \\   Volume: %3d%%",
    "*volSlider.min:                  1",
    "*volSlider.max:                  100",
    "*volSlider.value:                100",
    "*volSlider.resizable:            true",
    "*gainSlider.fromVert:            volSlider",
    "*gainSlider.label:               \\     Gain: %3d%%",
    "*gainSlider.min:                 1",
    "*gainSlider.max:                 100",
    "*gainSlider.resizable:           true",
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
XtPointer       gp,
                call_data;
{
    exit(0);
}

static void
startCB(w, gp, call_data)
Widget          w;
XtPointer       gp,
                call_data;
{
    GlobalDataPtr   g = (GlobalDataPtr) gp;
    AuElement       elements[3];
    String          wave;
    int             freq,
                    vol;

    if (!g->flow)
    {
	XtVaGetValues(g->menuButton, XtNlabel, &wave, NULL);
	XtVaGetValues(g->freqSlider, XtNvalue, &freq, NULL);
	XtVaGetValues(g->volSlider, XtNvalue, &vol, NULL);

	AuMakeElementImportWaveForm(&elements[0], SAMPLE_RATE,
				    AuStringToWaveForm(wave),
				    AuUnlimitedSamples, freq, 0, NULL);
	AuMakeElementMultiplyConstant(&elements[1], 0,
				      AuFixedPointFromFraction(vol, 100));
	AuMakeElementExportDevice(&elements[2], 1, g->device, SAMPLE_RATE,
				  AuUnlimitedSamples, 0, NULL);

	g->flow = AuCreateFlow(g->aud, NULL);
	AuSetElements(g->aud, g->flow, AuTrue, 3, elements, NULL);

	AuStartFlow(g->aud, g->flow, NULL);
	Invert(w);
	XtSetSensitive(g->menuButton, False);
    }
    else if (w)
	AuStopFlow(g->aud, g->flow, NULL);
    else
    {
	AuDestroyFlow(g->aud, g->flow, NULL);
	g->flow = 0;
	Invert(g->start);
	XtSetSensitive(g->menuButton, True);
    }
}

static void
menuCB(w, gp, call_data)
Widget          w;
XtPointer       gp,
                call_data;
{
    GlobalDataPtr   g = (GlobalDataPtr) gp;
    String          string;

    XtVaGetValues(w, XtNlabel, &string, NULL);
    XtVaSetValues(g->menuButton, XtNlabel, string, NULL);
}

static void
setVol(w, gp, valuep)
Widget          w;
XtPointer       gp,
                valuep;
{
    GlobalDataPtr   g = (GlobalDataPtr) gp;
    int             value = (int) valuep;

    AuElementParameters parms;

    if (g->flow)
    {
	parms.flow = g->flow;
	parms.element_num = 1;
	parms.num_parameters = AuParmsMultiplyConstant;
	parms.parameters[AuParmsMultiplyConstantConstant] =
	    AuFixedPointFromFraction(value, 100);
	AuSetElementParameters(g->aud, 1, &parms, NULL);
    }
}

static void
setFreq(w, gp, valuep)
Widget          w;
XtPointer       gp,
                valuep;
{
    GlobalDataPtr   g = (GlobalDataPtr) gp;
    int             value = (int) valuep;
    AuElementParameters parms;
    char            buf[10];

    sprintf(buf, "%d", value);
    XtVaSetValues(g->freqText, XtNstring, buf, NULL);

    if (g->flow)
    {
	parms.flow = g->flow;
	parms.element_num = 0;
	parms.num_parameters = AuParmsImportWaveForm;
	parms.parameters[AuParmsImportWaveFormFrequency] = value;
	parms.parameters[AuParmsImportWaveFormNumSamples] = AuUnlimitedSamples;
	AuSetElementParameters(g->aud, 1, &parms, NULL);
    }
}

static void
setGain(w, gp, valuep)
Widget          w;
XtPointer       gp,
                valuep;
{
    GlobalDataPtr   g = (GlobalDataPtr) gp;
    int             value = (int) valuep;
    AuDeviceAttributes da;

    AuDeviceGain(&da) = AuFixedPointFromSum(value, 0);
    AuSetDeviceAttributes(g->aud, g->device, AuCompDeviceGainMask, &da, NULL);
}

static void
createWidgets(g)
GlobalDataPtr   g;
{
    int             i;
    Widget          w;
    _AuConst char  *s = NULL;

    MakeWidget(g->form, g->top, formWidgetClass, "form");

    MakeCommandButton(g->start, g->form, "start", startCB);

    g->menu = XtCreatePopupShell("menu", simpleMenuWidgetClass, g->form,
				 NULL, 0);

    for (i = 0; i < AuServerNumWaveForms(g->aud); i++)
    {
	_AuConst char  *p;

	p = AuWaveFormToString(AuServerWaveForm(g->aud, i));
	MakeWidget(w, g->menu, smeBSBObjectClass, p);
	XtAddCallback(w, XtNcallback, menuCB, g);

	if (!s)
	    s = p;
    }

    MakeLabel(w, g->form, "waveLabel");
    MakeWidget(g->menuButton, g->form, menuButtonWidgetClass, "menuButton");
    XtVaSetValues(g->menuButton, XtNlabel, s, NULL);

    MakeCommandButton(g->quit, g->form, "quit", quitCB);

    MakeLabel(w, g->form, "freqLabel");
    MakeWidget(g->freqText, g->form, asciiTextWidgetClass, "freqText");
    MakeWidget(g->freqSlider, g->form, sliderWidgetClass, "freqSlider");
    XtAddCallback(g->freqSlider, XtNcallback, setFreq, g);
    MakeWidget(g->volSlider, g->form, sliderWidgetClass, "volSlider");
    XtAddCallback(g->volSlider, XtNcallback, setVol, g);

    if (g->gainAdjustable)
    {
	MakeWidget(g->gainSlider, g->form, sliderWidgetClass, "gainSlider");
	XtAddCallback(g->gainSlider, XtNcallback, setGain, g);
    }
}

static void
alignWidgets(g)
GlobalDataPtr   g;
{
    Dimension       w;
    Position        x,
                    x1;
    Widget          widget;

    XtVaGetValues(g->freqSlider, XtNx, &x, NULL);

    widget = XtNameToWidget(g->form, "volSlider.scrollbar");
    XtVaGetValues(widget, XtNx, &x1, NULL);
    XtVaGetValues(g->volSlider, XtNwidth, &w, NULL);
    XtVaSetValues(widget, XtNleft, XtChainRight, NULL);
    XtVaSetValues(g->volSlider, XtNwidth, w + (x - x1), NULL);
    XtVaSetValues(widget, XtNleft, XtChainLeft, NULL);

    if (g->gainAdjustable)
    {
	widget = XtNameToWidget(g->form, "gainSlider.scrollbar");
	XtVaGetValues(widget, XtNx, &x1, NULL);
	XtVaGetValues(g->gainSlider, XtNwidth, &w, NULL);
	XtVaSetValues(widget, XtNleft, XtChainRight, NULL);
	XtVaSetValues(g->gainSlider, XtNwidth, w + (x - x1), NULL);
	XtVaSetValues(widget, XtNleft, XtChainLeft, NULL);
    }
}

/* Actions */

static void
updateFrequency(w, event, params, num_params)
Widget          w;
XEvent         *event;
String         *params;
Cardinal       *num_params;
{
    GlobalDataPtr   g = globals;
    String          s;

    XtVaGetValues(w, XtNstring, &s, NULL);
    XtVaSetValues(g->freqSlider, XtNvalue, atoi(s), NULL);
}


static AuBool
EventHandler(aud, ev, handler)
AuServer       *aud;
AuEvent        *ev;
AuEventHandlerRec *handler;
{
    AuElementNotifyEvent *event = (AuElementNotifyEvent *) ev;

    if (ev->type == AuEventTypeElementNotify &&
	event->kind == AuElementNotifyKindState &&
	event->cur_state == AuStateStop)
    {
	startCB((Widget) 0, (GlobalDataPtr) handler->data, (XtPointer) 0);
	return AuTrue;
    }

    return AuFalse;
}

static void
quit(w, event, params, num_params)
Widget          w;
XEvent         *event;
String         *params;
Cardinal       *num_params;
{
    GlobalDataPtr   g = globals;

    if (event->type == ClientMessage &&
	event->xclient.data.l[0] != g->wm_delete_window)
	XBell(g->dpy, 0);
    else
	quitCB(g->quit, g, 0);
}

int
main(argc, argv)
int             argc;
char          **argv;
{
    GlobalDataRec   globalData;
    GlobalDataPtr   g = &globalData;
    XtAppContext    appContext;
    char           *audioServer = NULL;
    int             i;
    static XtActionsRec actions[] =
    {
	{"quit", quit},
	{"updateFrequency", updateFrequency}
    };

    globals = g;
    g->top = XtVaAppInitialize(&appContext, APP_CLASS, NULL, ZERO,
			       &argc, argv, defaultResources, NULL, 0);
    XtAppAddActions(appContext, actions, XtNumber(actions));

    if (argc == 3)
	if (!strncmp(argv[1], "-a", 2))
	    audioServer = argv[2];
	else
	    fatalError(USAGE);
    else if (argc != 1)
	fatalError(USAGE);

    if (!(g->aud = AuOpenServer(audioServer, 0, NULL, 0, NULL, NULL)))
	fatalError("Can't connect to audio server");

    g->dpy = XtDisplay(g->top);
    g->flow = 0;

    for (i = 0; i < AuServerNumDevices(g->aud); i++)
	if ((AuDeviceKind(AuServerDevice(g->aud, i)) ==
	     AuComponentKindPhysicalOutput) &&
	    AuDeviceNumTracks(AuServerDevice(g->aud, i)) == 1)
	{
	    g->device = AuDeviceIdentifier(AuServerDevice(g->aud, i));
	    g->gainAdjustable =
		AuDeviceChangableMask(AuServerDevice(g->aud, i)) &
		AuCompDeviceGainMask ? AuTrue : AuFalse;
	    break;
	}

    createWidgets(g);
    XtRealizeWidget(g->top);
    alignWidgets(g);

    if (g->gainAdjustable)
	XtVaSetValues(g->gainSlider, XtNvalue,
	       AuFixedPointRoundUp(AuDeviceGain(AuServerDevice(g->aud, i))),
		      NULL);

    if (!AuRegisterEventHandler(g->aud, 0, 0, 0, EventHandler, (AuPointer) g))
	fatalError("Can't register event handler");

    /* handle delete window message */
    g->wm_delete_window = XInternAtom(g->dpy, "WM_DELETE_WINDOW", FALSE);
    XSetWMProtocols(g->dpy, XtWindow(g->top), &g->wm_delete_window, 1);

    AuXtAppAddAudioHandler(appContext, g->aud);
    XtAppMainLoop(appContext);
    return 0;
}
