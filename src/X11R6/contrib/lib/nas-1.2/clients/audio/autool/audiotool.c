/*
 * Copyright 1993 Network Computing Devices, Inc.
 * 
 * Permission to use, copy, modify, distribute, and sell this software and its
 * documentation for any purpose is hereby granted without fee, provided that
 * the above copyright notice appear in all copies and that both that
 * copyright notice and this permission notice appear in supporting
 * documentation, and that the name Network Computing Devices, Inc. not be
 * used in advertising or publicity pertaining to distribution of this
 * software without specific, written prior permission.
 * 
 * THIS SOFTWARE IS PROVIDED 'AS-IS'.  NETWORK COMPUTING DEVICES, INC.,
 * DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING WITHOUT
 * LIMITATION ALL IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS FOR A
 * PARTICULAR PURPOSE, OR NONINFRINGEMENT.  IN NO EVENT SHALL NETWORK
 * COMPUTING DEVICES, INC., BE LIABLE FOR ANY DAMAGES WHATSOEVER, INCLUDING
 * SPECIAL, INCIDENTAL OR CONSEQUENTIAL DAMAGES, INCLUDING LOSS OF USE, DATA,
 * OR PROFITS, EVEN IF ADVISED OF THE POSSIBILITY THEREOF, AND REGARDLESS OF
 * WHETHER IN AN ACTION IN CONTRACT, TORT OR NEGLIGENCE, ARISING OUT OF OR IN
 * CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 * 
 * $NCDId: @(#)audiotool.c,v 1.26 1994/06/01 17:58:58 greg Exp $
 */

/*
 * auplay -- a simple clone of Sun's audiotool
 */

#include	<stdio.h>
#include	<malloc.h>
#include	<audio/audiolib.h>
#include	<audio/soundlib.h>

#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
#include <X11/Shell.h>
#include <X11/Xaw/Cardinals.h>
#include <audio/Xtutil.h>

/* widgets */
#include <X11/Xaw/Command.h>
#include <X11/Xaw/Form.h>
#include <X11/Xaw/List.h>
#include <X11/Xaw/Viewport.h>
#include <X11/Xaw/AsciiText.h>
#include <X11/Xaw/Scrollbar.h>
#include <X11/Xaw/Label.h>
#include <X11/Xaw/SimpleMenu.h>
#include <X11/Xaw/MenuButton.h>
#include <X11/Xaw/SmeBSB.h>
#include <X11/Xaw/Dialog.h>
#include <X11/Xaw/Toggle.h>

#if XtSpecificationRelease < 5
#define XawChainTop XtChainTop
#define XawChainLeft XtChainLeft
#endif

#define APP_INSTANCE		"autool"
#define	APP_CLASS		"Autool"
#define SPACING_X		5
#define SPACING_Y		5
#define BORDER_X		5
#define BORDER_Y		5
#define INFO_COLS		50
#define INFO_ROWS		10
#define SAMPLES_COLS		50
#define SAMPLES_ROWS		10
#define BUCKETS_COLS		strlen(BUCKET_HEADER)
#define BUCKETS_ROWS		5
#define	VOLUME_FORMAT		"Volume: %3d%%"
#define	DEFAULT_VOLUME		10
#define BUF_SIZE 		200
#define	MAX_VOLUME		200
#define	MIN_VOLUME		1
#define	DOUBLE_CLICK_TIME	500000		/* in microseconds */
#define PORTAL_SIZE		100000
#define VOL			AuFixedPointFromFraction(globals->volume, 100)


#define AddToLinkedList(head, item)                                            \
{                                                                              \
    (item)->prev = NULL;                                                       \
    (item)->next = head;						       \
    if (head)								       \
	(head)->prev = item;						       \
    head = item;							       \
}

#define RemoveFromLinkedList(head, item)				       \
{									       \
    if ((item)->next)							       \
	(item)->next->prev = (item)->prev;				       \
									   \
   if ((item)->prev)							      \
       (item)->prev->next = (item)->next;				      \
   else 								      \
       head = (item)->next;						      \
}


static String   defaultResources[] =
{
    "*font:		*courier-medium-r-normal*140*",
    NULL
};

static void     fatalError();

#define Invert(w)							       \
{									       \
    Pixel fg, bg;							       \
									       \
    XtVaGetValues(w, XtNforeground, &fg, XtNbackground, &bg, NULL);	       \
    XtVaSetValues(w, XtNforeground, bg, XtNbackground, fg, NULL);	       \
}

#define makeArg(resource, value)					       \
    XtSetArg(args[i], resource, (XtArgVal) (value)); i++

#define Anchor()							       \
    makeArg(XtNtop, XawChainTop);					       \
    makeArg(XtNbottom, XawChainTop);					       \
    makeArg(XtNleft, XawChainLeft);					       \
    makeArg(XtNright, XawChainLeft);					       \

#define MakeCommandButton(w, parent, label, horiz, vert, callback)	       \
{									       \
    Arg args[6];							       \
    int i = 0;								       \
									       \
    Anchor();								       \
    makeArg(XtNfromHoriz, horiz);					       \
    makeArg(XtNfromVert, vert);						       \
									       \
    (w) = XtCreateManagedWidget(label, commandWidgetClass, parent, args, i);   \
									       \
    /* SUPPRESS 558 */							       \
    if ((void *) (callback) != NULL)					       \
	XtAddCallback(w, XtNcallback, callback, (XtPointer) globals);	       \
}

#define MakeLabel(w, parent, label, horiz, vert)			       \
{									       \
    Arg args[7];							       \
    int i = 0;								       \
									       \
    Anchor();								       \
    makeArg(XtNfromHoriz, horiz);					       \
    makeArg(XtNfromVert, vert);						       \
    makeArg(XtNborderWidth, 0);						       \
									       \
    (w) = XtCreateManagedWidget(label, labelWidgetClass, parent, args, i);     \
}

typedef struct _VolumeList
{
    AuFlowID        flow;
    unsigned int    elementNumber;
    struct _VolumeList *prev,
                   *next;
}               VolumeListRec, *VolumeListPtr;

typedef VolumeListPtr VolumeListId;

typedef struct
{
    Widget          topLevel,
                    quit,
                    form,
                    play,
                    stop,
                    samples,
                    info,
                    view,
                    volumeBar,
                    volumeLabel;
    int             volume;
    AuServer       *aud;
    Display        *dpy;
    AuDeviceID      inputDeviceId;
    char           *filename;

    Bool            playing;
    AuFlowID        flow;
    int             multiplier;
    VolumeListId    vol;
}               GlobalDataRec, *GlobalDataPtr;

static VolumeListPtr VolumeList;
static int      VolumeListCount;

#define RemoveFromVolumeList(id)					       \
{									       \
    RemoveFromLinkedList(VolumeList, (VolumeListPtr) id);		       \
    VolumeListCount--;							       \
}

static          VolumeListId
AddToVolumeList(flow, elementNumber)
AuFlowID        flow;
unsigned int    elementNumber;
{
    VolumeListPtr   p;

    if (!(p = (VolumeListPtr) malloc(sizeof(VolumeListRec))))
	fatalError("malloc error in AddToVolumeList");

    p->flow = flow;
    p->elementNumber = elementNumber;

    AddToLinkedList(VolumeList, p);
    VolumeListCount++;

    return (VolumeListId) p;
}

typedef struct
{
    GlobalDataPtr   globals;
    void            (*callback) ();
}               DonePrivRec, *DonePrivPtr;

/* ARGSUSED */
static void
doneCB(aud, handler, ev, d)
AuServer       *aud;
AuEventHandlerRec *handler;
AuEvent        *ev;
AuPointer       d;
{
    DonePrivPtr     data = (DonePrivPtr) d;

    (*data->callback) (NULL, data->globals, data);
}

/* ARGSUSED */
static void
stopCB(w, g, data)
Widget          w;
XtPointer       g;
XtPointer       data;
{
    GlobalDataPtr   globals = (GlobalDataPtr) g;

    if (globals->playing)
    {
	if (w)
	    /* user requested stop */
	    AuStopFlow(globals->aud, globals->flow, NULL);
	else
	{
	    /* got a done callback */
	    RemoveFromVolumeList(globals->vol);
	    globals->playing = False;
	    Invert(globals->play);
	}

	return;
    }
}

/* ARGSUSED */
static void
playCB(w, g, data)
Widget          w;
XtPointer       g;
XtPointer       data;
{
    static DonePrivRec priv;
    GlobalDataPtr   globals = (GlobalDataPtr) g;

    priv.globals = globals;
    priv.callback = stopCB;

    if (globals->playing)
	return;
    if (AuSoundPlayFromFile(globals->aud, globals->filename, AuNone,
	     VOL, doneCB, &priv, &globals->flow, &globals->multiplier, NULL,
			    NULL))
    {
	globals->vol = AddToVolumeList(globals->flow, globals->multiplier);
	globals->playing = True;
	Invert(globals->play);
    }
}

static          Bool
showInfo(globals)
GlobalDataPtr   globals;
{
    Sound           s;

    if ((s = SoundOpenFileForReading(globals->filename)))
    {
	char           *buf,
	               *p;

#define PRINT(p, f, a)							       \
{									       \
    sprintf(p, f, a);							       \
    p += strlen(p);							       \
}

	if (!(p = buf = (char *) malloc(2000 + strlen(SoundComment(s)))))
	    fatalError("Can't malloc text in showInfo");

	PRINT(p, "   Filename: %s\n", globals->filename);
	PRINT(p, "File Format: %s\n", SoundFileFormatString(s));
	PRINT(p, "Data Format: %s\n", SoundDataFormatString(s));
	PRINT(p, "     Tracks: %d\n", SoundNumTracks(s));
	PRINT(p, "  Frequency: %d Hz\n", SoundSampleRate(s));
	PRINT(p, "   Duration: %.2f seconds\n",
	      (float) SoundNumSamples(s) / SoundSampleRate(s));

	PRINT(p, "\n%s", SoundComment(s));

	XtVaSetValues(globals->info, XtNstring, buf, NULL);

	free(buf);
	SoundCloseFile(s);
	return AuTrue;
    }
    else
    {
	return AuFalse;
    }
}

/* ARGSUSED */
static void
quitCB(w, data, call_data)
Widget          w;
XtPointer       data;
XtPointer       call_data;
{
    exit(0);
}

static void
adjustVolume(globals)
GlobalDataPtr   globals;
{
    AuElementParameters *parms;
    VolumeListPtr   p = VolumeList;
    int             i = 0;

    if (!VolumeListCount)
	return;

    if (!(parms = (AuElementParameters *)
	  malloc(sizeof(AuElementParameters) * VolumeListCount)))
	fatalError("malloc error in adjustVolume");

    while (p)
    {
	parms[i].flow = p->flow;
	parms[i].element_num = p->elementNumber;
	parms[i].num_parameters = AuParmsMultiplyConstant;
	parms[i].parameters[AuParmsMultiplyConstantConstant] = VOL;

	p = p->next;
	i++;
    }

    AuSetElementParameters(globals->aud, VolumeListCount, parms, NULL);
    free(parms);
}

/* ARGSUSED */
static void
scrollProcCB(w, data, cd)
Widget          w;
XtPointer       data;
XtPointer       cd;
{
    GlobalDataPtr   globals = (GlobalDataPtr) data;
    int             position = (int) cd;
    int             newVolume;
    char            buf[50];

    newVolume = globals->volume + (position > 0 ? -1 : 1);

    if (newVolume < MIN_VOLUME)
	newVolume = MIN_VOLUME;
    else if (newVolume > MAX_VOLUME)
	newVolume = MAX_VOLUME;

    if (newVolume != globals->volume)
    {
	globals->volume = newVolume;
	sprintf(buf, VOLUME_FORMAT, globals->volume);
	XtVaSetValues(globals->volumeLabel, XtNlabel, buf, NULL);

	XawScrollbarSetThumb(globals->volumeBar,
			     (float) globals->volume / MAX_VOLUME, -1.0);
	adjustVolume(globals);
    }
}

/* ARGSUSED */
static void
jumpProcCB(w, data, cd)
Widget          w;
XtPointer       data;
XtPointer       cd;
{
    int             newVolume;
    char            buf[50];
    GlobalDataPtr   globals = (GlobalDataPtr) data;
    float          *percent = (float *) cd;;

    newVolume = *percent * MAX_VOLUME;

    if (newVolume < MIN_VOLUME)
	newVolume = MIN_VOLUME;

    if (newVolume != globals->volume)
    {
	globals->volume = newVolume;
	sprintf(buf, VOLUME_FORMAT, globals->volume);
	XtVaSetValues(globals->volumeLabel, XtNlabel, buf, NULL);
	adjustVolume(globals);
    }
}

static          Dimension
getFontCharWidth(w)
Widget          w;
{
    XFontStruct    *font;

    XtVaGetValues(w, XtNfont, &font, NULL);
    return font->max_bounds.rbearing - font->min_bounds.lbearing;
}

static          Dimension
getFontCharHeight(w)
Widget          w;
{
    XFontStruct    *font;

    XtVaGetValues(w, XtNfont, &font, NULL);
    return font->max_bounds.ascent + font->max_bounds.descent;
}

static char    *progname;

static void
usage()
{
    fprintf(stderr,
	    "Usage: %s [-audio servername] [-volume percentage] [-toolkitoption ...] file\n", APP_INSTANCE);
    exit(-1);
}

int
main(argc, argv)
int             argc;
char          **argv;
{
    int             i;
    Dimension       infoWidth,
                    labelHeight;
    Arg             args[10];
    char            buf[BUF_SIZE],
                   *audioServerString = NULL;
    GlobalDataRec   globalData,
                   *globals;
    Position        infoX,
                    barX;
    XtAppContext    appContext;
    Widget          w;

    /* static          XtActionsRec Actions[] = {"Ok", okAction}; */

    progname = argv[0];

    globals = &globalData;

    globals->topLevel = XtVaAppInitialize(&appContext, APP_CLASS, NULL, ZERO,
					  &argc, argv, defaultResources,
					  NULL, 0);

    globals->volume = DEFAULT_VOLUME;
    globals->playing = False;
    globals->dpy = XtDisplay(globals->topLevel);
    globals->filename = NULL;

    while (argc > 1)
    {
	argv++;
	argc--;

	if (!strcmp("-a", *argv) || !strcmp("-audio", *argv))
	{
	    if (argv[1])
		audioServerString = argv[1];
	    else
		usage();
	    argv++;
	    argc--;
	}
	else if (!strcmp("-v", *argv))
	{
	    if (argv[1])
		globals->volume = atoi(argv[1]);
	    else
		usage();
	    argv++;
	    argc--;
	}
	else if (**argv == '-')
	{
	    usage();
	}
	else
	    globals->filename = *argv;
    }
    if (!globals->filename)
	fatalError("No sound file to play");


    if (!(globals->aud = AuOpenServer(audioServerString, 0, NULL, 0, NULL,
				      NULL)))
	fatalError("Can't connect to audio server");

    for (i = 0; i < AuServerNumDevices(globals->aud); i++)
	if ((AuDeviceKind(AuServerDevice(globals->aud, i)) ==
	     AuComponentKindPhysicalInput))
	{
	    globals->inputDeviceId =
		AuDeviceIdentifier(AuServerDevice(globals->aud, i));
	    break;
	}
    /* XtAppAddActions(appContext, Actions, XtNumber(Actions)); */

    globals->form = XtCreateManagedWidget("form", formWidgetClass,
					  globals->topLevel, NULL, 0);

    MakeCommandButton(globals->play, globals->form, "Play", NULL, NULL, playCB);
    MakeCommandButton(globals->stop, globals->form, "Stop", globals->play,
		      NULL, stopCB);
    MakeCommandButton(globals->quit, globals->form, "Quit", globals->stop,
		      NULL, quitCB);

    sprintf(buf, "NAS %d.%d", AuServerProtocolMajorVersion(globals->aud),
	    AuServerProtocolMinorVersion(globals->aud));
    MakeLabel(w, globals->form, buf, globals->quit, NULL);

    i = 0;
    Anchor();
    makeArg(XtNfromVert, globals->play);
    sprintf(buf, VOLUME_FORMAT, globals->volume);
    makeArg(XtNlabel, buf);
    globals->volumeLabel = XtCreateManagedWidget("VolumeLabel",
						 labelWidgetClass,
						 globals->form, args, i);

    i = 0;
    makeArg(XtNtop, XawChainTop);
    makeArg(XtNbottom, XawChainTop);
    makeArg(XtNleft, XawChainLeft);
    makeArg(XtNfromHoriz, globals->volumeLabel);
    makeArg(XtNfromVert, globals->play);
    makeArg(XtNorientation, XtorientHorizontal);
    makeArg(XtNresizable, True);
    globals->volumeBar = XtCreateManagedWidget("VolumeBar",
					       scrollbarWidgetClass,
					       globals->form, args, i);
    XawScrollbarSetThumb(globals->volumeBar,
			 ((float) globals->volume) / MAX_VOLUME, -1.0);
    XtAddCallback(globals->volumeBar, XtNscrollProc, scrollProcCB, globals);
    XtAddCallback(globals->volumeBar, XtNjumpProc, jumpProcCB, globals);

    i = 0;
    makeArg(XtNtop, XawChainTop);
    makeArg(XtNbottom, XawChainTop);
    makeArg(XtNscrollVertical, XawtextScrollWhenNeeded);
    makeArg(XtNwrap, XawtextWrapWord);
    makeArg(XtNdisplayCaret, False);
    makeArg(XtNfromVert, globals->volumeLabel);
    globals->info = XtCreateManagedWidget("info", asciiTextWidgetClass,
					  globals->form, args, i);

    i = 0;
    infoWidth = getFontCharWidth(globals->info) * INFO_COLS;
    makeArg(XtNwidth, infoWidth);
    makeArg(XtNheight, getFontCharHeight(globals->info) * INFO_ROWS);
    XtSetValues(globals->info, args, i);

    if (!showInfo(globals))
    {
	FILE           *fp;

	/* figure out why it failed */
	fp = fopen(globals->filename, "r");
	if (fp)
	{
	    fprintf(stderr,
		    "Cannot parse sound file \"%s\".\n", globals->filename);
	}
	else
	{
	    perror(globals->filename);
	}
	fclose(fp);
	exit(-1);
    }
    XtRealizeWidget(globals->topLevel);

    XtVaGetValues(globals->info, XtNx, &infoX, NULL);
    XtVaGetValues(globals->volumeBar, XtNx, &barX, NULL);
    XtVaGetValues(globals->volumeLabel, XtNheight, &labelHeight, NULL);
    XtVaSetValues(globals->volumeBar, XtNwidth, infoX + infoWidth - barX,
		  XtNheight, labelHeight, NULL);

    AuXtAppAddAudioHandler(appContext, globals->aud);
    playCB(globals->play, (XtPointer) globals, (XtPointer) 0);
    XtAppMainLoop(appContext);
    return 0;
}

static void
fatalError(message, arg)
char           *message,
               *arg;
{
    fprintf(stderr, message, arg);
    fprintf(stderr, ".\n");
    exit(1);
}
