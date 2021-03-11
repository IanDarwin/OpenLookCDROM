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
 * $NCDId: @(#)auedit.c,v 1.34 1994/06/01 22:12:25 greg Exp $
 */

#include <stdio.h>
#ifdef __STDC__
#include <stdlib.h>
#endif
#include <unistd.h>
#include <malloc.h>
#include <limits.h>				/* for SHRT_MIN and SHRT_MAX */
#ifndef SYSV
#include <audio/Aos.h>				/* for string and other os
						 * stuff */
#endif
#include <audio/Afuncs.h>			/* for bcopy et. al. */
#include <audio/audiolib.h>
#include <audio/soundlib.h>
#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
#include <X11/Shell.h>
#include <X11/Xaw/Cardinals.h>
#include <audio/Xtutil.h>

/* widgets */
#include <X11/Xaw/AsciiText.h>
#include <X11/Xaw/Form.h>
#include <X11/Xaw/Label.h>
#include <X11/Xaw/Command.h>
#include <X11/Xaw/SimpleMenu.h>
#include <X11/Xaw/MenuButton.h>
#include <X11/Xaw/SmeBSB.h>
#include <X11/Xaw/SmeLine.h>
#include <X11/Xaw/Dialog.h>
#include <X11/Xaw/Toggle.h>

#include <Slider.h>

#if XlibSpecificationRelease < 5
typedef char   *XPointer;
#endif

#include "Graph.h"

#include "play.xbm"
#include "stop.xbm"
#include "pause.xbm"

#define	APP_CLASS		"Auedit"
#define LITTLE_ENDIAN 		(*(char *) &g->endian == 1)
#define BIG_ENDIAN 		(!LITTLE_ENDIAN)
#define SELECTION_HEADER_SIZE	4
#define DEFAULT_FREQUENCY	8000
#define ZOOM_SCALE		2
#define MALLOC_FAIL		"Malloc failed"

#define USAGE "\
usage: auedit [-audio audioserver] [filename]"

#define min(a, b) ((a) < (b) ? (a) : (b))
#define max(a, b) ((a) > (b) ? (a) : (b))

#define MakeWidget(_w, _parent, _type, _name)				      \
    (_w) = XtCreateManagedWidget(_name, _type, _parent, NULL, 0)	      \

#define MakeWidgetCB(_w, _parent, _type, _callback, _name)		      \
{									      \
    (_w) = XtCreateManagedWidget(_name, _type, _parent, NULL, 0);	      \
									      \
    if ((void *) (_callback) != NULL)					      \
	XtAddCallback(_w, XtNcallback, _callback, g);			      \
}

#define MakePopup(_w, _parent, _type, _name)				      \
    (_w) = XtCreatePopupShell(_name, _type, _parent, NULL, 0)		      \

#define MakeMenuPopup(_w, _parent, _name)				      \
    MakePopup(_w, _parent, simpleMenuWidgetClass, _name)

#define MakeMenuItemW(_w, _parent, _callback, _label)			      \
{									      \
    _w = XtCreateManagedWidget(_label, smeBSBObjectClass, _parent, NULL, 0);  \
									      \
    if ((void *) (_callback) != NULL)					      \
	XtAddCallback(_w, XtNcallback, _callback, g);			      \
}

#define MakeMenuItem(_parent, _callback, _label)			      \
{									      \
    Widget _ww;								      \
    MakeMenuItemW(_ww, _parent, _callback, _label);			      \
}

#define MakeMenuLine(_parent)						      \
    XtCreateManagedWidget("line", smeLineObjectClass, _parent, NULL, 0)

#define MakeMenuButton(_w, _parent, _name)				      \
    MakeWidget(_w, _parent, menuButtonWidgetClass, _name)

#define MakeButton(_w, _parent, _callback, _name)			      \
    MakeWidgetCB(_w, _parent, commandWidgetClass, _callback, _name)

#define MakeLabel(_w, _parent, _name)					      \
    MakeWidget(_w, _parent, labelWidgetClass, _name)

#define Invert(w)							      \
{									      \
    Pixel fg, bg;							      \
									      \
    XtVaGetValues(w, XtNforeground, &fg, XtNbackground, &bg, NULL);	      \
    XtVaSetValues(w, XtNforeground, bg, XtNbackground, fg, NULL);	      \
}

#define ReplaceCallback(w, cb)						      \
{									      \
    XtRemoveAllCallbacks(w, XtNcallback);				      \
    XtAddCallback(w, XtNcallback, cb, g);				      \
}

#define AddToLinkedList(head, item)					      \
{									      \
    (item)->prev = NULL;						      \
    (item)->next = head;						      \
    if (head)								      \
	(head)->prev = item;						      \
    head = item;							      \
}

#define RemoveFromLinkedList(head, item)				      \
{									      \
    if ((item)->next)							      \
	(item)->next->prev = (item)->prev;				      \
									      \
    if ((item)->prev)							      \
	(item)->prev->next = (item)->next;				      \
    else								      \
	head = (item)->next;						      \
}

#define DIALOG(_callback, _title, _label)				      \
{									      \
    if (w)								      \
    {									      \
        XtVaSetValues(g->dialog, XtNvalue, "", NULL);			      \
	popupDialog(g, g->dialog, _callback, _title, _label);		      \
	return;								      \
    }									      \
}

#define WARNING(_callback, _label)					      \
{									      \
    if (w)								      \
    {									      \
	popupDialog(g, g->warning, _callback, "Warning", _label);	      \
	return;								      \
    }									      \
}

#define ERROR(_msg)							      \
    popupDialog(g, g->error, NULL, "Error", _msg)

#define ERRORf(_msg)							      \
{									      \
    ERROR(_msg);							      \
    return FALSE;							      \
}

#define ERRORv(_msg)							      \
{									      \
    ERROR(_msg);							      \
    return;								      \
}

typedef struct
{
    short          *data;
    Atom            atom;
    XContext        context;
    int             len;
    void            (*callback) ();
}               SelectionRec, *SelectionPtr;

#define UNDO_DEL	0
#define UNDO_INS	1
#define UNDO_REP	2

typedef struct
{
    GraphDataType  *data,
                   *repData;
    int             insStart,
                    insLen,
                    repStart,
                    repLen,
                    delStart,
                    delLen;
}               UndoRec, *UndoPtr;

typedef struct
{
    Widget          top,
                    form,
                    gainSlider,
                    duration,
                    mode,
                    freq;
    int             inputDeviceNum,
                    currentMode;
    AuDeviceID      inputDevice;
}               RecordDialogRec, *RecordDialogPtr;

typedef struct
{
    Widget          top,
                    form,
                    play,
                    stop,
                    pause,
                    record,
                    fileMenuButton,
                    editMenuButton,
                    editPasteInsert,
                    editPasteReplace,
                    editPasteMix,
                    editUndo,
                    zoomMenuButton,
                    effectsMenuButton,
                    volumeSlider,
                    leftTime,
                    positionTime,
                    rightTime,
                    durationTime,
                    fileFormatMenuButton,
                    dataFormatMenuButton,
                    frequency,
                    comment,
                    filename,
                    dialogShell,
                    dialog,
                    errorShell,
                    error,
                    warningShell,
                    warning,
                    graph,
                    popdownCause;
    GraphDataType  *data;
    AuServer       *aud;
    int             monitorInc,
                    numSamples,
                    numTracks,
                    sampleRate,
                    endian;
    SelectionRec    selection;
    Display        *dpy;
    Boolean         modified;
    void            (*dialogCallback) (),
                    (*modifiedCallback) ();
    Atom            wm_delete_window;
    UndoRec         undo;
    RecordDialogRec recordDialog;
}               GlobalDataRec, *GlobalDataPtr;

static String   defaultResources[] =
{
#include "resources.h"
    NULL
};

typedef struct
{
    GlobalDataPtr   g;
    void            (*callback) ();
    int             data;
}               DonePrivRec, *DonePrivPtr;

typedef struct _ElementList
{
    AuFlowID        flow;
    int             volumeElement;
    struct _ElementList *prev,
                   *next;
}               ElementListRec, *ElementListPtr;

typedef ElementListPtr ElementListId;

static ElementListPtr ElementList;
static int      ElementCount;
static GlobalDataPtr globals;			/* for actions */
static Boolean  loadFile(), saveFile();
static void     popupDialog(), clearUndo(), setUndo();

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
RemoveFromElementList(p)
ElementListPtr  p;
{
    RemoveFromLinkedList(ElementList, p);
    ElementCount--;
    free(p);
}

static          ElementListId
AddToElementList(flow, volumeElement)
AuFlowID        flow;
int             volumeElement;
{
    ElementListPtr  p;

    if (!(p = (ElementListPtr) malloc(sizeof(ElementListRec))))
	fatalError("malloc error in AddToElementList");

    p->flow = flow;
    p->volumeElement = volumeElement;

    AddToLinkedList(ElementList, p);
    ElementCount++;

    return (ElementListId) p;
}

static void
setTime(g, w, p)
GlobalDataPtr   g;
Widget          w;
int             p;
{
    char            buf[20];
    float           t;
    int             m;

    t = (float) p / g->sampleRate;
    m = t / 60;
    t -= m * 60;
    sprintf(buf, "%02d:%05.2f", m, t);
    XtVaSetValues(w, XtNlabel, buf, NULL);
}

static void
leftMarker(w, gp, pp)
Widget          w;
XtPointer       gp;
XtPointer       pp;
{
    GlobalDataPtr   g = (GlobalDataPtr) gp;
    int             p = (int) pp;
    int             n;

    XtVaGetValues(g->graph, XtNrightMarker, &n, NULL);

    setTime(g, g->leftTime, p);
    setTime(g, g->durationTime, n - p);
}

static void
rightMarker(w, gp, pp)
Widget          w;
XtPointer       gp;
XtPointer       pp;
{
    GlobalDataPtr   g = (GlobalDataPtr) gp;
    int             p = (int) pp;
    int             n;

    XtVaGetValues(g->graph, XtNleftMarker, &n, NULL);

    setTime(g, g->rightTime, p);
    setTime(g, g->durationTime, p - n);
}

static void
handleMonitorEvent(aud, handler, ev, g)
AuServer       *aud;
AuEventHandlerRec *handler;
AuEvent        *ev;
GlobalDataPtr   g;
{
    int             current,
                    end;

    XtVaGetValues(g->graph, XtNposition, &current, XtNrightMarker, &end, NULL);
    current += g->monitorInc;

    if (current >= end)
	current = end - 1;

    GraphSetPosition(g->graph, current);
    setTime(g, g->positionTime, current);
}

static void
done(aud, handler, ev, datap)
AuServer       *aud;
AuEvent        *ev;
AuEventHandlerRec *handler;
XtPointer       datap;
{
    DonePrivPtr     data = (DonePrivPtr) datap;

    if (ev->type == AuEventTypeMonitorNotify)
	handleMonitorEvent(aud, handler, ev, data->g);
    else
	(*data->callback) (NULL, data->g, data->data);
}

static void
adjustGain(w, gp, gainp)
Widget          w;
XtPointer       gp;
XtPointer       gainp;
{
    GlobalDataPtr   g = (GlobalDataPtr) gp;
    int             gain = (int) gainp;
    AuDeviceAttributes da;

    if (!AuDeviceChangableMask(AuServerDevice(g->aud,
					  g->recordDialog.inputDeviceNum)) &
	AuCompDeviceGainMask)
	return;

    AuDeviceGain(&da) = AuFixedPointFromSum(gain, 0);
    AuSetDeviceAttributes(g->aud, g->recordDialog.inputDevice,
			  AuCompDeviceGainMask, &da, NULL);
}

static void
recordStart(w, gp, call_data)
Widget          w;
XtPointer       gp;
XtPointer       call_data;
{
    GlobalDataPtr   g = (GlobalDataPtr) gp;
    static DonePrivRec priv;
    static AuFlowID flow;
    RecordDialogPtr r = &g->recordDialog;
    String          st;
    Sound           s;
    int             duration,
                    freq,
                    gain;
    char            buf[20];
    static AuBool   recording;

    if (!recording)
    {
	if (w != XtNameToWidget(g->recordDialog.form, "recordStart"))
	    return;

	XtVaGetValues(r->duration, XtNstring, &st, NULL);
	duration = atoi(st);
	XtVaGetValues(r->freq, XtNstring, &st, NULL);
	freq = atoi(st);

	if (!duration || !freq)
	    return;

	if (g->data)
	    free(g->data);

	g->numSamples = duration * freq;
	g->numTracks =
	    AuDeviceNumTracks(AuServerDevice(g->aud, r->inputDeviceNum));

	if (!(g->data = (GraphDataType *)
	   calloc(1, sizeof(GraphDataType) * g->numTracks * g->numSamples)))
	    return;

	g->sampleRate = freq;
	sprintf(buf, "%d", g->sampleRate);
	XtVaSetValues(g->frequency, XtNstring, buf, NULL);

	s = SoundCreate(SoundFileFormatNone,
			LITTLE_ENDIAN ? AuFormatLinearSigned16LSB :
			AuFormatLinearSigned16MSB, g->numTracks,
			freq, g->numSamples, NULL);

	priv.g = g;
	priv.callback = recordStart;

	XtVaGetValues(r->gainSlider, XtNvalue, &gain, NULL);

	AuSoundRecordToData(g->aud, s, g->data, r->inputDevice,
			    AuFixedPointFromSum(gain, 0),
			    done, &priv, r->currentMode, &flow, NULL, NULL);
	SoundDestroy(s);
	Invert(w);
	recording = AuTrue;
    }
    else if (w)
	AuStopFlow(g->aud, flow, NULL);
    else
    {
	XtVaSetValues(g->graph, XtNdata, g->data,
		      XtNnumSamples, g->numSamples,
		      XtNnumTracks, g->numTracks,
		      XtNstart, 0, XtNend, g->numSamples,
		      XtNleftMarker, 0, XtNrightMarker, g->numSamples,
		      NULL);
	GraphRedraw(g->graph);
	g->modified = TRUE;

	XtVaSetValues(g->filename, XtNlabel, "new", NULL);
	XtVaSetValues(g->comment, XtNstring, "", NULL);
	clearUndo(g);
	Invert(XtNameToWidget(g->recordDialog.form, "recordStart"));
	recording = AuFalse;
    }
}

static void
recordMonitor(w, gp, call_data)
Widget          w;
XtPointer       gp;
XtPointer       call_data;
{
    GlobalDataPtr   g = (GlobalDataPtr) gp;
    RecordDialogPtr r = &g->recordDialog;
    static Boolean  monitoring;
    static AuFlowID flow;
    static DonePrivRec priv;
    String          st;
    int             freq;

    if (monitoring)
    {
	if (w)
	    /* user requested stop */
	    AuStopFlow(g->aud, flow, NULL);
	else
	{
	    /* got a done callback */
	    monitoring = False;
	    Invert(XtNameToWidget(r->form, "monitor"));
	}

	return;
    }

    if (w != XtNameToWidget(r->form, "monitor"))
	return;

    XtVaGetValues(r->freq, XtNstring, &st, NULL);
    freq = atoi(st);

    priv.g = g;
    priv.callback = recordMonitor;

    if (AuMonitorDevice(g->aud, freq, r->inputDevice, AuNone,
			AuFixedPointFromSum(1, 0),
			done, &priv, &flow, NULL, NULL, NULL))
    {
	monitoring = True;
	Invert(w);
    }
}

static void
recordDismiss(w, gp, call_data)
Widget          w;
XtPointer       gp;
XtPointer       call_data;
{
    GlobalDataPtr   g = (GlobalDataPtr) gp;

    recordMonitor((Widget) 1, g, 0);
    recordStart((Widget) 1, g, 0);

    XtPopdown(g->recordDialog.top);
    XtSetSensitive(g->record, True);
}

static void
recordMode(w, gp, call_data)
Widget          w;
XtPointer       gp;
XtPointer       call_data;
{
    GlobalDataPtr   g = (GlobalDataPtr) gp;
    AuDeviceAttributes da;
    RecordDialogPtr r = &g->recordDialog;

    XtCallActionProc(w, "reset", NULL, NULL, 0);

    if (r->currentMode == AuDeviceLineModeLow)
    {
	XtVaSetValues(r->mode, XtNlabel, "Mic", NULL);
	r->currentMode = AuDeviceLineModeHigh;
    }
    else
    {
	XtVaSetValues(r->mode, XtNlabel, "Line", NULL);
	r->currentMode = AuDeviceLineModeLow;
    }

    AuDeviceLineMode(&da) = r->currentMode;
    AuSetDeviceAttributes(g->aud, r->inputDevice, AuCompDeviceLineModeMask,
			  &da, NULL);
}

#define PLAY_PLAY	0
#define PLAY_STOP	1
#define PLAY_PAUSE	2

static void
setPlaySensitive(g, v)
GlobalDataPtr   g;
Boolean         v;
{
    XtSetSensitive(g->frequency, v);
    XtSetSensitive(g->graph, v);
    XtSetSensitive(g->play, v);
    XtSetSensitive(g->fileMenuButton, v);
    XtSetSensitive(g->editMenuButton, v);
    XtSetSensitive(g->zoomMenuButton, v);
    XtSetSensitive(g->effectsMenuButton, v);

    if (g->recordDialog.inputDevice)
	XtSetSensitive(g->record, v);
}

static void
doPlay(w, g, mode)
Widget          w;
GlobalDataPtr   g;
int             mode;
{
    static AuFlowID flow;
    static DonePrivRec priv;
    static AuBool   paused,
                    playing,
                    userStop;
    static ElementListId listid;
    extern int      AuMonitorRate;

    switch (mode)
    {
	case PLAY_PLAY:
	    {
		int             start,
		                end,
		                multiplier,
		                monitor,
		                vol;
		Sound           s;

		priv.g = g;
		priv.callback = doPlay;
		priv.data = PLAY_STOP;
		userStop = AuFalse;

		XtVaGetValues(g->graph, XtNleftMarker, &start,
			      XtNrightMarker, &end, NULL);

		GraphSetPosition(g->graph, start);
		setTime(g, g->positionTime, start);

		s = SoundCreate(SoundFileFormatNone,
				LITTLE_ENDIAN ? AuFormatLinearSigned16LSB :
				AuFormatLinearSigned16MSB,
			    g->numTracks, g->sampleRate, end - start, NULL);

		g->monitorInc = g->sampleRate / AuMonitorRate;

		XtVaGetValues(g->volumeSlider, XtNvalue, &vol, NULL);

		if (AuSoundPlayFromData(g->aud, s,
					g->data + start * g->numTracks,
					AuNone,
					AuFixedPointFromFraction(vol, 100),
					done, &priv, &flow,
					&multiplier, &monitor, NULL))
		{
		    listid = AddToElementList(flow, multiplier);
		    setPlaySensitive(g, FALSE);
		    Invert(g->play);
		    playing = AuTrue;
		}

		SoundDestroy(s);
		break;
	    }
	case PLAY_STOP:
	    if (!playing)
		break;

	    if (w)
	    {
		userStop = AuTrue;
		AuStopFlow(g->aud, flow, NULL);/* user requested stop */
	    }
	    else
	    {
		if (!userStop)
		    /* fake a monitor event to bump the position to the end */
		    handleMonitorEvent(g->aud, (AuEventHandlerRec *) 0,
				       (AuEvent *) 0, g);

		/* got a done callback */
		RemoveFromElementList(listid);
		setPlaySensitive(g, TRUE);
		Invert(g->play);

		if (paused)
		    Invert(g->pause);

		playing = paused = AuFalse;
	    }
	    break;
	case PLAY_PAUSE:
	    if (!playing)
		break;

	    if (paused)
		AuStartFlow(g->aud, flow, NULL);
	    else
		AuPauseFlow(g->aud, flow, NULL);

	    Invert(g->pause);
	    paused = !paused;
	    break;
    }
}

static void
stop(w, gp, call_data)
Widget          w;
XtPointer       gp;
XtPointer       call_data;
{
    GlobalDataPtr   g = (GlobalDataPtr) gp;

    doPlay(w, g, PLAY_STOP);
}

static void
Pause(w, gp, call_data)
Widget          w;
XtPointer       gp;
XtPointer       call_data;
{
    GlobalDataPtr   g = (GlobalDataPtr) gp;

    doPlay(w, g, PLAY_PAUSE);
}

static void
play(w, gp, call_data)
Widget          w;
XtPointer       gp;
XtPointer       call_data;
{
    GlobalDataPtr   g = (GlobalDataPtr) gp;

    doPlay(w, g, PLAY_PLAY);
}

#define MODIFIED(_x)							      \
{									      \
    if (g->modified)							      \
    {									      \
	g->modifiedCallback = _x;					      \
	checkModified(w, g);						      \
	return;								      \
    }									      \
}

static void
checkModified(w, gp, call_data)
Widget          w;
XtPointer       gp;
XtPointer       call_data;
{
    GlobalDataPtr   g = (GlobalDataPtr) gp;

    WARNING(checkModified, "File has been modified - Are you sure?");

    g->modified = FALSE;
    (*g->modifiedCallback) ((Widget) 1, g);
}

static void
record(w, gp, call_data)
Widget          w;
XtPointer       gp;
XtPointer       call_data;
{
    GlobalDataPtr   g = (GlobalDataPtr) gp;
    MODIFIED(record);

    XtSetSensitive(g->record, False);
    XtPopup(g->recordDialog.top, XtGrabExclusive);
}

static void
fileExit(w, gp, call_data)
Widget          w;
XtPointer       gp;
XtPointer       call_data;
{
    GlobalDataPtr   g = (GlobalDataPtr) gp;
    MODIFIED(fileExit);
    exit(0);
}

static void
fileRevert(w, gp, call_data)
Widget          w;
XtPointer       gp;
XtPointer       call_data;
{
    GlobalDataPtr   g = (GlobalDataPtr) gp;
    String          name;

    MODIFIED(fileRevert);
    XtVaGetValues(g->filename, XtNlabel, &name, NULL);
    loadFile(g, name);
}

static void
newFile(g, numTracks)
GlobalDataPtr   g;
int             numTracks;
{
    g->numSamples = 1;
    g->numTracks = numTracks;

    if (!(g->data =
	  (GraphDataType *) malloc(sizeof(GraphDataType) * numTracks)))
	fatalError("malloc error in newFile");

    *g->data = 0;

    XtVaSetValues(g->graph, XtNdata, g->data,
		  XtNnumSamples, g->numSamples,
		  XtNnumTracks, g->numTracks,
		  XtNstart, 0, XtNend, g->numSamples,
		  XtNleftMarker, 0, XtNrightMarker, g->numSamples,
		  NULL);

    XtVaSetValues(g->filename, XtNlabel, "new", NULL);
    XtVaSetValues(g->comment, XtNstring, "", NULL);
    clearUndo(g);
}

static void
fileNew(w, gp, call_data)
Widget          w;
XtPointer       gp;
XtPointer       call_data;
{
    GlobalDataPtr   g = (GlobalDataPtr) gp;
    int             numTracks;

    MODIFIED(fileNew);
    DIALOG(fileNew, "New File", "Number of tracks:");

    numTracks = atoi(XawDialogGetValueString(g->dialog));

    if (numTracks < 1 || numTracks > AuServerMaxTracks(g->aud))
    {
	ERROR("Illegal number of tracks");
	numTracks = 1;
    }

    newFile(g, numTracks);
}

static Boolean
noFile(g)
GlobalDataPtr   g;
{
    char            buf[20];

    g->sampleRate = DEFAULT_FREQUENCY;
    sprintf(buf, "%d", g->sampleRate);
    XtVaSetValues(g->frequency, XtNstring, buf, NULL);
    newFile(g, 1);
    return FALSE;
}

static void
checkSelection(w, gp, call_data)
Widget          w;
XtPointer       gp;
XtPointer       call_data;
{
    GlobalDataPtr   g = (GlobalDataPtr) gp;
    Boolean         canPaste;

    canPaste = XGetSelectionOwner(g->dpy, g->selection.atom) != None;

    XtSetSensitive(g->editPasteInsert, canPaste);
    XtSetSensitive(g->editPasteReplace, canPaste);
    XtSetSensitive(g->editPasteMix, canPaste);
}

static void
placePopup(w, gp, call_data)
Widget          w;
XtPointer       gp;
XtPointer       call_data;
{
    GlobalDataPtr   g = (GlobalDataPtr) gp;
    Dimension       width,
                    height;
    Position        x,
                    y,
                    oldX,
                    oldY;

    XtVaGetValues(g->form, XtNwidth, &width, XtNheight, &height, NULL);
    XtTranslateCoords(g->form, width / 2, height / 2, &x, &y);
    XtVaGetValues(w, XtNwidth, &width, XtNheight, &height, NULL);
    x -= width / 2;
    y -= height / 2;
    XtVaGetValues(w, XtNx, &oldX, XtNy, &oldY, NULL);

    if (x != oldX || y != oldY)
	XtVaSetValues(w, XtNx, x, XtNy, y, NULL);
}

typedef struct
{
    unsigned char  *p;
    int             len;
}               SelRec, *SelPtr;

static Boolean
selectionSend(w, selection, target, type, value, length, format, maxLength,
	      gp, id)
Widget          w;
Atom           *selection;
Atom           *target;
Atom           *type;
XtPointer      *value;
AuUint32       *length;
int            *format;
AuUint32       *maxLength;
XtPointer       gp;
XtRequestId    *id;
{
    GlobalDataPtr   g = (GlobalDataPtr) gp;
    SelPtr          con;
    int             len;
    XID             xid = (XID) * id;

    if (*selection != g->selection.atom || *target != g->selection.atom)
	return FALSE;

    if (XFindContext(g->dpy, xid, g->selection.context, (XPointer *) & con))
    {					       /* first call */
	if (!(con = (SelPtr) malloc(sizeof(SelRec))))
	    return FALSE;

	con->len = g->selection.len;
	con->p = (unsigned char *) g->selection.data;
    }

    len = min(*maxLength, con->len);

    if (!len)				       /* out of data */
    {
	free(con);
	XDeleteContext(g->dpy, xid, g->selection.context);

	*value = (XtPointer) XtMalloc(1);
	*length = 0;
	return TRUE;
    }

    if (!(*value = (XtPointer) XtMalloc(len)))
	return FALSE;

    bcopy(con->p, *value, len);

    con->len -= len;
    con->p += len;
    XSaveContext(g->dpy, xid, g->selection.context, (XPointer) con);

    *type = g->selection.atom;
    *format = 16;
    *length = len >> 1;
    return TRUE;
}

static short   *
convertRate(data, fromRate, toRate, numSamples, numTracks)
short          *data;
int             fromRate,
                toRate,
               *numSamples,
                numTracks;
{
    int             n,
                    len,
                    phase;
    short          *new,
                   *last;
    unsigned char  *s,
                   *d;

    n = (float) toRate / fromRate * *numSamples;

    if (!(new = (short *) XtMalloc(n * sizeof(short) * numTracks)))
	return NULL;

    len = sizeof(short) * numTracks;

    if (!(last = (short *) XtMalloc(len)))
    {
	XtFree((char *) new);
	return NULL;
    }

    *numSamples = n;
    phase = 0;
    s = (unsigned char *) data;
    d = (unsigned char *) new;

    while (n--)
    {
	while (phase >= 0)
	{
	    phase -= toRate;
	    bcopy(s, last, len);
	    s += len;
	}

	phase += fromRate;
	bcopy(last, d, len);
	d += len;
    }

    XtFree((char *) last);
    XtFree((char *) data);
    return new;
}

static void
selectionReceive(w, gp, selection, type, value, length, format)
Widget          w;
XtPointer       gp;
Atom           *selection;
Atom           *type;
XtPointer       value;
AuUint32       *length;
int            *format;
{
    GlobalDataPtr   g = (GlobalDataPtr) gp;
    unsigned short *p;
    short          *data;

    p = (unsigned short *) value;
    data = (short *) p + SELECTION_HEADER_SIZE;

    if (g->selection.callback)
	if (p[0] != g->numTracks)
	    ERROR("Selection has a different number of tracks");
	else
	{
	    int             numSamples;

	    /*
	     * due to a bug in Xt we can't believe the length parameter.  So
	     * the actual data length is included in the data.
	     */
	    numSamples = (((p[2] << 16) + p[3]) / sizeof(short) -
			  SELECTION_HEADER_SIZE) / g->numTracks;

	    if (p[1] != g->sampleRate)
		if (!(data = convertRate(data, p[1], g->sampleRate,
					 &numSamples, g->numTracks)))
		    ERROR(MALLOC_FAIL);

	    if (data)
	    {
		Boolean         zoomOut = g->numSamples == 1;

		(*g->selection.callback) (g, data, numSamples);
		g->modified = TRUE;

		if (zoomOut)
		    XtVaSetValues(g->graph, XtNstart, 0, XtNleftMarker, 0,
				  XtNend, g->numSamples, XtNrightMarker,
				  g->numSamples, NULL);
	    }
	}

    XtSetSensitive(g->form, TRUE);
    XtFree(value);
}

static void
pasteInsert(g, data, numSamples)
GlobalDataPtr   g;
GraphDataType  *data;
int             numSamples;
{
    GraphDataType  *new;
    unsigned char  *p;
    int             left,
                    numBytes,
                    bytesPerSample = sizeof(GraphDataType) * g->numTracks;

    if (!(new = (GraphDataType *)
	  realloc(g->data, (g->numSamples + numSamples) * bytesPerSample)))
	ERRORv(MALLOC_FAIL);

    XtVaGetValues(g->graph, XtNleftMarker, &left, NULL);

    p = (unsigned char *) (new + left * g->numTracks);
    numBytes = numSamples * bytesPerSample;
    bcopy(p, p + numBytes, (g->numSamples - left) * bytesPerSample);
    bcopy(data, p, numBytes);

    g->data = new;
    g->numSamples += numSamples;

    XtVaSetValues(g->graph, XtNdata, g->data, XtNnumSamples, g->numSamples,
		  NULL);
    clearUndo(g);
    setUndo(g, left, left + numSamples, UNDO_INS);
}

static void
pasteReplace(g, data, numSamples)
GlobalDataPtr   g;
GraphDataType  *data;
int             numSamples;
{
    int             left,
                    n,
                    bytesPerSample = sizeof(GraphDataType) * g->numTracks;
    Boolean         newValues = FALSE;

    XtVaGetValues(g->graph, XtNleftMarker, &left, NULL);
    clearUndo(g);

    /* see if we need to insert any */
    n = g->numSamples - left;

    if (n < numSamples)
    {
	GraphDataType  *new;
	int             needed = numSamples - n;

	if (!(new = (GraphDataType *)
	      realloc(g->data, (g->numSamples + needed) * bytesPerSample)))
	    ERRORv(MALLOC_FAIL);

	setUndo(g, g->numSamples, g->numSamples + needed, UNDO_INS);

	g->data = new;
	g->numSamples += needed;
	newValues = TRUE;
    }

    setUndo(g, left, left + numSamples, UNDO_REP);
    bcopy(data, g->data + left * g->numTracks, numSamples * bytesPerSample);

    if (newValues)
	XtVaSetValues(g->graph, XtNdata, g->data, XtNnumSamples, g->numSamples,
		      NULL);
    else
	GraphRedraw(g->graph);
}

static void
pasteMix(g, data, numSamples)
GlobalDataPtr   g;
GraphDataType  *data;
int             numSamples;
{
    GraphDataType  *p;
    int             left,
                    n,
                    bytesPerSample = sizeof(GraphDataType) * g->numTracks;
    Boolean         newValues = FALSE;

    XtVaGetValues(g->graph, XtNleftMarker, &left, NULL);
    clearUndo(g);

    /* see if we need to insert any */
    n = g->numSamples - left;

    if (n < numSamples)
    {
	GraphDataType  *new;
	int             needed = numSamples - n;

	if (!(new = (GraphDataType *)
	      realloc(g->data, (g->numSamples + needed) * bytesPerSample)))
	    ERRORv(MALLOC_FAIL);

	setUndo(g, g->numSamples, g->numSamples + needed, UNDO_INS);
	bzero(new + g->numSamples * g->numTracks, needed * bytesPerSample);

	g->data = new;
	g->numSamples += needed;
	newValues = TRUE;
    }

    setUndo(g, left, left + numSamples, UNDO_REP);
    p = g->data + left * g->numTracks;

    while (numSamples--)
	*p++ = (*p + *data++) / 2;

    if (newValues)
	XtVaSetValues(g->graph, XtNdata, g->data, XtNnumSamples, g->numSamples,
		      NULL);
    else
	GraphRedraw(g->graph);
}

static void
editPaste(w, gp, call_data)
Widget          w;
XtPointer       gp;
XtPointer       call_data;
{
    GlobalDataPtr   g = (GlobalDataPtr) gp;

    if (w == g->editPasteInsert)
	g->selection.callback = pasteInsert;
    else if (w == g->editPasteReplace)
	g->selection.callback = pasteReplace;
    else if (w == g->editPasteMix)
	g->selection.callback = pasteMix;
    else
	g->selection.callback = NULL;

    XtSetSensitive(g->form, FALSE);
    XtGetSelectionValue(g->top, g->selection.atom, g->selection.atom,
		     selectionReceive, g, XtLastTimestampProcessed(g->dpy));
}

static Boolean
copyToSelection(g)
GlobalDataPtr   g;
{
    int             start,
                    end;

    if (g->selection.data)
    {
	free(g->selection.data);
	g->selection.data = NULL;
    }

    XtVaGetValues(g->graph, XtNleftMarker, &start, XtNrightMarker, &end, NULL);
    g->selection.len = (end - start) * sizeof(GraphDataType) * g->numTracks +
	SELECTION_HEADER_SIZE * sizeof(short);

    if (!(g->selection.data = (short *) malloc(g->selection.len)))
	ERRORf(MALLOC_FAIL);

    g->selection.data[0] = g->numTracks;
    g->selection.data[1] = g->sampleRate;
    g->selection.data[2] = g->selection.len >> 16;
    g->selection.data[3] = g->selection.len & 0xffff;

    bcopy(g->data + start * g->numTracks,
	  g->selection.data + SELECTION_HEADER_SIZE, g->selection.len -
	  SELECTION_HEADER_SIZE * sizeof(short));

    if (!XtOwnSelectionIncremental(g->top, g->selection.atom,
				   XtLastTimestampProcessed(g->dpy),
				   selectionSend, NULL, NULL, NULL, g))
    {
	free(g->selection.data);
	g->selection.data = NULL;
	ERRORf("Couldn't get selection");
    }

    return TRUE;
}

static void
clearUndo(g)
GlobalDataPtr   g;
{
    if (g->undo.data)
	free(g->undo.data);

    if (g->undo.repData)
	free(g->undo.repData);

    bzero(&g->undo, sizeof(UndoRec));
    XtSetSensitive(g->editUndo, FALSE);
}

static void
setUndo(g, start, end, type)
GlobalDataPtr   g;
int             start,
                end,
                type;
{
    int             len;

    len = (end - start) * sizeof(GraphDataType) * g->numTracks;

    switch (type)
    {
	case UNDO_DEL:
	    g->undo.delLen = len;

	    if (!(g->undo.data = (GraphDataType *) malloc(g->undo.delLen)))
		ERRORv(MALLOC_FAIL);

	    bcopy(g->data + start * g->numTracks, g->undo.data,
		  g->undo.delLen);

	    g->undo.delStart = start;
	    break;
	case UNDO_INS:
	    g->undo.insLen = len;
	    g->undo.insStart = start;
	    break;
	case UNDO_REP:
	    g->undo.repLen = len;

	    if (!(g->undo.repData = (GraphDataType *) malloc(g->undo.repLen)))
		ERRORv(MALLOC_FAIL);

	    bcopy(g->data + start * g->numTracks, g->undo.repData,
		  g->undo.repLen);

	    g->undo.repStart = start;
	    break;
    }

    XtSetSensitive(g->editUndo, TRUE);
}

static void
editCut(w, gp, call_data)
Widget          w;
XtPointer       gp;
XtPointer       call_data;
{
    GlobalDataPtr   g = (GlobalDataPtr) gp;
    int             start,
                    end,
                    n;

    if (!copyToSelection(g))
	return;

    XtVaGetValues(g->graph, XtNleftMarker, &start, XtNrightMarker, &end, NULL);
    clearUndo(g);
    setUndo(g, start, end, UNDO_DEL);

    n = (g->numSamples - end) * sizeof(GraphDataType) * g->numTracks;

    if (n)
	bcopy(g->data + end * g->numTracks,
	      g->data + start * g->numTracks, n);

    g->numSamples -= end - start;

    if (!g->numSamples)
	g->numSamples = 1;

    g->data = (GraphDataType *)
	realloc(g->data, g->numSamples * sizeof(GraphDataType) *
		g->numTracks);

    XtVaSetValues(g->graph, XtNdata, g->data, XtNnumSamples, g->numSamples,
		  NULL);
    g->modified = TRUE;
}

static void
editCopy(w, gp, call_data)
Widget          w;
XtPointer       gp;
XtPointer       call_data;
{
    GlobalDataPtr   g = (GlobalDataPtr) gp;

    copyToSelection(g);
}

static void
editUndo(w, gp, call_data)
Widget          w;
XtPointer       gp;
XtPointer       call_data;
{
    GlobalDataPtr   g = (GlobalDataPtr) gp;
    UndoRec         u;
    int             numSamples,
                    n;
    GraphDataType  *p;

    bzero(&u, sizeof(u));

    if (g->undo.delLen)
    {
	Boolean         zoomOut = g->numSamples == 1;
	GraphDataType  *new;

	numSamples = g->undo.delLen / sizeof(GraphDataType) / g->numTracks;

	if (!(new = (GraphDataType *)
	      realloc(g->data, (g->numSamples + numSamples) *
		      sizeof(GraphDataType) * g->numTracks)))
	    ERRORv(MALLOC_FAIL);

	p = new + g->undo.delStart * g->numTracks;
	n = (g->numSamples - g->undo.delStart) *
	    sizeof(GraphDataType) * g->numTracks;

	if (n)
	    bcopy(p, p + numSamples * g->numTracks, n);

	bcopy(g->undo.data, p, g->undo.delLen);

	free(g->undo.data);

	g->data = new;
	g->numSamples += numSamples;

	XtVaSetValues(g->graph, XtNdata, g->data, XtNnumSamples, g->numSamples,
		      NULL);

	if (zoomOut)
	    XtVaSetValues(g->graph, XtNstart, 0, XtNleftMarker, 0,
			  XtNend, g->numSamples, XtNrightMarker,
			  g->numSamples, NULL);

	u.insLen = g->undo.delLen;
	u.insStart = g->undo.delStart;
    }

    if (g->undo.repLen)
    {
	GraphDataType  *a,
	               *b,
	                t;
	int             i;

	a = g->data + g->undo.repStart * g->numTracks;
	b = g->undo.repData;
	i = g->undo.repLen / sizeof(GraphDataType);

	while (i--)
	{
	    t = *a;
	    *a++ = *b;
	    *b++ = t;
	}

	u.repStart = g->undo.repStart;
	u.repLen = g->undo.repLen;
	u.repData = g->undo.repData;

	GraphRedraw(g->graph);
    }

    if (g->undo.insLen)
    {
	if (!(u.data = (GraphDataType *) malloc(g->undo.insLen)))
	    ERRORv(MALLOC_FAIL);

	p = g->data + g->undo.insStart * g->numTracks;

	bcopy(p, u.data, g->undo.insLen);

	u.delLen = g->undo.insLen;
	u.delStart = g->undo.insStart;

	numSamples = g->undo.insLen / sizeof(GraphDataType) / g->numTracks;

	n = (g->numSamples - (g->undo.insStart + numSamples)) *
	    sizeof(GraphDataType) * g->numTracks;

	if (n)
	    bcopy(p + numSamples * g->numTracks, p, n);

	if (!(g->numSamples -= numSamples))
	    g->numSamples = 1;

	g->data = (GraphDataType *)
	    realloc(g->data, g->numSamples * sizeof(GraphDataType) *
		    g->numTracks);

	XtVaSetValues(g->graph, XtNdata, g->data, XtNnumSamples, g->numSamples,
		      NULL);
    }

    g->undo = u;
}

static void
fileLoad(w, gp, call_data)
Widget          w;
XtPointer       gp;
XtPointer       call_data;
{
    GlobalDataPtr   g = (GlobalDataPtr) gp;

    MODIFIED(fileLoad);
    DIALOG(fileLoad, "Load File", "Filename:");
    loadFile(g, XawDialogGetValueString(g->dialog));
}

static void
fileSaveInterval(w, gp, call_data)
Widget          w;
XtPointer       gp;
XtPointer       call_data;
{
    GlobalDataPtr   g = (GlobalDataPtr) gp;
    int             start,
                    end;

    DIALOG(fileSaveInterval, "Save Interval", "Filename:");
    XtVaGetValues(g->graph, XtNleftMarker, &start, XtNrightMarker, &end, NULL);
    saveFile(g, XawDialogGetValueString(g->dialog), start, end);
}

static void
fileSaveAs(w, gp, call_data)
Widget          w;
XtPointer       gp;
XtPointer       call_data;
{
    GlobalDataPtr   g = (GlobalDataPtr) gp;
    int             numSamples;
    String          string;

    DIALOG(fileSaveAs, "Save As", "Filename:");

    string = XawDialogGetValueString(g->dialog);
    XtVaGetValues(g->graph, XtNnumSamples, &numSamples, NULL);
    XtVaSetValues(g->filename, XtNlabel, string, NULL);
    saveFile(g, string, 0, numSamples);
    g->modified = FALSE;
}

static void
fileSave(w, gp, call_data)
Widget          w;
XtPointer       gp;
XtPointer       call_data;
{
    GlobalDataPtr   g = (GlobalDataPtr) gp;
    int             numSamples;
    String          name;

    XtVaGetValues(g->graph, XtNnumSamples, &numSamples, NULL);
    XtVaGetValues(g->filename, XtNlabel, &name, NULL);
    saveFile(g, name, 0, numSamples);
    g->modified = FALSE;
}

static void
zoomIn(w, gp, call_data)
Widget          w;
XtPointer       gp;
XtPointer       call_data;
{
    GlobalDataPtr   g = (GlobalDataPtr) gp;
    int             start,
                    end,
                    n;

    XtVaGetValues(g->graph, XtNstart, &start, XtNend, &end, NULL);
    n = (end - start) / ZOOM_SCALE / 2;
    start += n;
    end -= n;
    XtVaSetValues(g->graph, XtNstart, start, XtNend, end, NULL);
}

static void
zoomOut(w, gp, call_data)
Widget          w;
XtPointer       gp;
XtPointer       call_data;
{
    GlobalDataPtr   g = (GlobalDataPtr) gp;
    int             start,
                    end,
                    n;

    XtVaGetValues(g->graph, XtNstart, &start, XtNend, &end, NULL);
    n = (end - start) * ZOOM_SCALE / 2;
    start -= n;
    end += n;
    XtVaSetValues(g->graph, XtNstart, start, XtNend, end, NULL);
}

static void
zoomMarkers(w, gp, call_data)
Widget          w;
XtPointer       gp;
XtPointer       call_data;
{
    GlobalDataPtr   g = (GlobalDataPtr) gp;
    int             start,
                    end;

    XtVaGetValues(g->graph, XtNleftMarker, &start, XtNrightMarker, &end, NULL);
    XtVaSetValues(g->graph, XtNstart, start, XtNend, end, NULL);
}

static void
zoomFull(w, gp, call_data)
Widget          w;
XtPointer       gp;
XtPointer       call_data;
{
    GlobalDataPtr   g = (GlobalDataPtr) gp;
    int             numSamples;

    XtVaGetValues(g->graph, XtNnumSamples, &numSamples, NULL);
    XtVaSetValues(g->graph, XtNstart, 0, XtNend, numSamples - 1, NULL);
}

static void
effectsReverse(w, gp, call_data)
Widget          w;
XtPointer       gp;
XtPointer       call_data;
{
    GlobalDataPtr   g = (GlobalDataPtr) gp;
    int             start,
                    end;
    GraphDataType  *a,
                   *b,
                    t;

    XtVaGetValues(g->graph, XtNleftMarker, &start, XtNrightMarker, &end, NULL);
    clearUndo(g);
    setUndo(g, start, end, UNDO_REP);
    a = g->data + start * g->numTracks;
    b = g->data + (end - 1) * g->numTracks;

    while (a < b)
    {
	t = *a;
	*a++ = *b;
	*b-- = t;
    }

    GraphRedraw(g->graph);
    g->modified = TRUE;
}

static void
effectsFadeIn(w, gp, call_data)
Widget          w;
XtPointer       gp;
XtPointer       call_data;
{
    GlobalDataPtr   g = (GlobalDataPtr) gp;
    int             start,
                    end,
                    n;
    GraphDataType  *p;
    float           scale,
                    delta;

    XtVaGetValues(g->graph, XtNleftMarker, &start, XtNrightMarker, &end, NULL);
    clearUndo(g);
    setUndo(g, start, end, UNDO_REP);

    p = g->data + start * g->numTracks;
    n = (end - start) * g->numTracks;
    delta = 1.0 / n;
    scale = 0;

    while (n--)
    {
	*p++ *= scale;
	scale += delta;
    }

    GraphRedraw(g->graph);
    g->modified = TRUE;
}

static void
effectsFadeOut(w, gp, call_data)
Widget          w;
XtPointer       gp;
XtPointer       call_data;
{
    GlobalDataPtr   g = (GlobalDataPtr) gp;
    int             start,
                    end,
                    n;
    GraphDataType  *p;
    float           scale,
                    delta;

    XtVaGetValues(g->graph, XtNleftMarker, &start, XtNrightMarker, &end, NULL);
    clearUndo(g);
    setUndo(g, start, end, UNDO_REP);

    p = g->data + start * g->numTracks;
    n = (end - start) * g->numTracks;
    delta = 1.0 / n;
    scale = 1;

    while (n--)
    {
	*p++ *= scale;
	scale -= delta;
    }

    GraphRedraw(g->graph);
    g->modified = TRUE;
}

static void
effectsMaxAmplitude(w, gp, call_data)
Widget          w;
XtPointer       gp;
XtPointer       call_data;
{
    GlobalDataPtr   g = (GlobalDataPtr) gp;
    int             start,
                    end,
                    n;
    GraphDataType  *p,
                    m;
    float           scale;

    XtVaGetValues(g->graph, XtNleftMarker, &start, XtNrightMarker, &end, NULL);
    clearUndo(g);
    setUndo(g, start, end, UNDO_REP);

    p = g->data + start * g->numTracks;
    n = (end - start) * g->numTracks;
    m = 0;

    while (n--)
    {
	m = max(m, abs(*p));
	*p++;
    }

    scale = 32767.0 / m;

    p = g->data + start * g->numTracks;
    n = (end - start) * g->numTracks;

    while (n--)
    {
	int x = *p * scale;
	*p++ = x < SHRT_MIN ? SHRT_MIN : x > SHRT_MAX ? SHRT_MAX : x;
    }

    GraphRedraw(g->graph);
    g->modified = TRUE;
}

static void
effectsAmplitude(w, gp, call_data)
Widget          w;
XtPointer       gp;
XtPointer       call_data;
{
    GlobalDataPtr   g = (GlobalDataPtr) gp;
    int             start,
                    end,
                    n;
    GraphDataType  *p;
    float           scale;
    String          s;

    DIALOG(effectsAmplitude, "Change Amplitude", "Amplitude scale:");
    s = XawDialogGetValueString(g->dialog);
    sscanf(s, "%f", &scale);

    XtVaGetValues(g->graph, XtNleftMarker, &start, XtNrightMarker, &end, NULL);
    clearUndo(g);
    setUndo(g, start, end, UNDO_REP);
    p = g->data + start * g->numTracks;
    n = (end - start) * g->numTracks;

    while (n--)
	*p++ *= scale;

    GraphRedraw(g->graph);
    g->modified = TRUE;
}

static void
adjustVolume(w, gp, valuep)
Widget          w;
XtPointer       gp;
XtPointer       valuep;
{
    GlobalDataPtr   g = (GlobalDataPtr) gp;
    int             value = (int) valuep;
    AuElementParameters *parms;
    ElementListPtr  p = ElementList;
    int             i = 0;

    if (!ElementCount)
	return;

    if (!(parms = (AuElementParameters *)
	  malloc(sizeof(AuElementParameters) * ElementCount)))
	fatalError("malloc error in adjustVolume");

    while (p)
    {
	parms[i].flow = p->flow;
	parms[i].element_num = p->volumeElement;
	parms[i].num_parameters = AuParmsMultiplyConstant;
	parms[i].parameters[AuParmsMultiplyConstantConstant] =
	    AuFixedPointFromFraction(value, 100);

	p = p->next;
	i++;
    }

    AuSetElementParameters(g->aud, ElementCount, parms, NULL);
    free(parms);
}

static void
setFileFormatMenuButton(w, gp, call_data)
Widget          w;
XtPointer       gp;
XtPointer       call_data;
{
    GlobalDataPtr   g = (GlobalDataPtr) gp;
    String          string;

    XtVaGetValues(w, XtNlabel, &string, NULL);
    XtVaSetValues(g->fileFormatMenuButton, XtNlabel, string, NULL);
}

static void
setDataFormatMenuButton(w, gp, call_data)
Widget          w;
XtPointer       gp;
XtPointer       call_data;
{
    GlobalDataPtr   g = (GlobalDataPtr) gp;
    String          string;

    XtVaGetValues(w, XtNlabel, &string, NULL);
    XtVaSetValues(g->dataFormatMenuButton, XtNlabel, string, NULL);
}

static void
popupDialog(g, dialog, callback, title, label)
GlobalDataPtr   g;
Widget          dialog;
void            (*callback) ();
char           *title,
               *label;
{
    g->dialogCallback = callback;
    XtVaSetValues(XtParent(dialog), XtNtitle, title, NULL);
    XtVaSetValues(dialog, XtNlabel, label, NULL);
    XtPopup(XtParent(dialog), XtGrabExclusive);
}

static void
shellCancel(w, gp, call_data)
Widget          w;
XtPointer       gp;
XtPointer       call_data;
{
    GlobalDataPtr   g = (GlobalDataPtr) gp;

    g->popdownCause = w;
    XtPopdown(XtParent(XtParent(w)));
}

static void
shellOk(w, gp, call_data)
Widget          w;
XtPointer       gp;
XtPointer       call_data;
{
    GlobalDataPtr   g = (GlobalDataPtr) gp;

    shellCancel(w, g);

    if (g->dialogCallback)
	(*g->dialogCallback) ((Widget) 0, g);
}

static void
createWidgets(g)
GlobalDataPtr   g;
{
    Widget          w;
    int             i;
    Pixmap          bitmap;
    RecordDialogPtr r;

    MakeWidget(g->form, g->top, formWidgetClass, "form");

    /* file menu */
    MakeMenuPopup(w, g->form, "fileMenu");
    MakeMenuItem(w, fileNew, "fileNew");
    MakeMenuItem(w, fileLoad, "fileLoad");
    MakeMenuItem(w, fileSave, "fileSave");
    MakeMenuItem(w, fileSaveAs, "fileSaveAs");
    MakeMenuItem(w, fileSaveInterval, "fileSaveInterval");
    MakeMenuItem(w, fileRevert, "fileRevert");
    MakeMenuLine(w);
    MakeMenuItem(w, fileExit, "fileExit");
    MakeMenuButton(g->fileMenuButton, g->form, "fileMenuButton");

    /* edit menu */
    MakeMenuPopup(w, g->form, "editMenu");
    XtAddCallback(w, XtNpopupCallback, checkSelection, g);
    MakeMenuItem(w, editCut, "editCut");
    MakeMenuItem(w, editCopy, "editCopy");
    MakeMenuItemW(g->editPasteInsert, w, editPaste, "editPasteInsert");
    MakeMenuItemW(g->editPasteReplace, w, editPaste, "editPasteReplace");
    MakeMenuItemW(g->editPasteMix, w, editPaste, "editPasteMix");
    MakeMenuLine(w);
    MakeMenuItemW(g->editUndo, w, editUndo, "editUndo");
    MakeMenuButton(g->editMenuButton, g->form, "editMenuButton");

    /* zoom menu */
    MakeMenuPopup(w, g->form, "zoomMenu");
    MakeMenuItem(w, zoomIn, "zoomIn");
    MakeMenuItem(w, zoomOut, "zoomOut");
    MakeMenuItem(w, zoomMarkers, "zoomMarkers");
    MakeMenuItem(w, zoomFull, "zoomFull");
    MakeMenuButton(g->zoomMenuButton, g->form, "zoomMenuButton");

    /* effects menu */
    MakeMenuPopup(w, g->form, "effectsMenu");
    MakeMenuItem(w, effectsAmplitude, "effectsAmplitude");
    MakeMenuItem(w, effectsMaxAmplitude, "effectsMaxAmplitude");
    MakeMenuItem(w, effectsReverse, "effectsReverse");
    MakeMenuItem(w, effectsFadeIn, "effectsFadeIn");
    MakeMenuItem(w, effectsFadeOut, "effectsFadeOut");
    MakeMenuButton(g->effectsMenuButton, g->form, "effectsMenuButton");

    /* record button */
    MakeButton(g->record, g->form, record, "record");

    /* filename */
    MakeLabel(g->filename, g->form, "filename");

    /* volume slider */
    MakeWidget(g->volumeSlider, g->form, sliderWidgetClass, "volumeSlider");
    XtAddCallback(g->volumeSlider, XtNcallback, adjustVolume, g);

    MakeWidget(g->graph, g->form, graphWidgetClass, "graph");
    XtAddCallback(g->graph, XtNleftProc, leftMarker, g);
    XtAddCallback(g->graph, XtNrightProc, rightMarker, g);

    MakeLabel(w, g->form, "leftLabel");
    MakeLabel(g->leftTime, g->form, "leftTime");

    MakeLabel(w, g->form, "durationLabel");
    MakeLabel(g->durationTime, g->form, "durationTime");

    MakeLabel(w, g->form, "rightLabel");
    MakeLabel(g->rightTime, g->form, "rightTime");

    MakeLabel(w, g->form, "positionLabel");
    MakeLabel(g->positionTime, g->form, "positionTime");

    /* control buttons */
    MakeButton(g->play, g->form, play, "play");
    bitmap = XCreateBitmapFromData(g->dpy,
				   RootWindow(g->dpy, DefaultScreen(g->dpy)),
				   (char *) play_bits,
				   play_width, play_height);
    XtVaSetValues(g->play, XtNbitmap, bitmap, NULL);

    MakeButton(g->stop, g->form, stop, "stop");
    bitmap = XCreateBitmapFromData(g->dpy,
				   RootWindow(g->dpy, DefaultScreen(g->dpy)),
				   (char *) stop_bits,
				   stop_width, stop_height);
    XtVaSetValues(g->stop, XtNbitmap, bitmap, NULL);

    MakeButton(g->pause, g->form, Pause, "pause");
    bitmap = XCreateBitmapFromData(g->dpy,
				   RootWindow(g->dpy, DefaultScreen(g->dpy)),
				   (char *) pause_bits,
				   pause_width, pause_height);
    XtVaSetValues(g->pause, XtNbitmap, bitmap, NULL);

    /* file format menu */
    MakeLabel(w, g->form, "fileFormatLabel");
    MakeMenuPopup(w, g->form, "fileFormatMenu");

    for (i = 0; i < SoundNumFileFormats; i++)
	MakeMenuItem(w, setFileFormatMenuButton, SoundFileFormatToString(i));

    MakeMenuButton(g->fileFormatMenuButton, g->form, "fileFormatMenuButton");
    XtVaSetValues(g->fileFormatMenuButton, XtNlabel, SoundFileFormatToString(0),
		  NULL);

    /* data format menu */
    MakeLabel(w, g->form, "dataFormatLabel");
    MakeMenuPopup(w, g->form, "dataFormatMenu");

    for (i = 0; i < AuServerNumFormats(g->aud); i++)
	MakeMenuItem(w, setDataFormatMenuButton,
		     AuFormatToString(AuServerFormat(g->aud, i)));

    MakeMenuButton(g->dataFormatMenuButton, g->form, "dataFormatMenuButton");
    XtVaSetValues(g->dataFormatMenuButton, XtNlabel,
		  AuFormatToString(AuServerFormat(g->aud, 0)), NULL);

    /* frequency */
    MakeLabel(w, g->form, "frequencyLabel");
    MakeWidget(g->frequency, g->form, asciiTextWidgetClass, "frequency");

    /* comment */
    MakeLabel(w, g->form, "commentLabel");
    MakeWidget(g->comment, g->form, asciiTextWidgetClass, "comment");

    /* dialog box */
    MakePopup(g->dialogShell, g->top, transientShellWidgetClass, "dialogShell");
    XtAddCallback(g->dialogShell, XtNpopupCallback, placePopup, g);
    MakeWidget(g->dialog, g->dialogShell, dialogWidgetClass, "dialog");
    XawDialogAddButton(g->dialog, "okButton", shellOk, g);
    XawDialogAddButton(g->dialog, "cancelButton", shellCancel, g);

    /* warning box */
    MakePopup(g->warningShell, g->top, transientShellWidgetClass,
	      "warningShell");
    XtAddCallback(g->warningShell, XtNpopupCallback, placePopup, g);
    MakeWidget(g->warning, g->warningShell, dialogWidgetClass, "warning");
    XawDialogAddButton(g->warning, "okButton", shellOk, g);
    XawDialogAddButton(g->warning, "cancelButton", shellCancel, g);

    /* error box */
    MakePopup(g->errorShell, g->top, transientShellWidgetClass, "errorShell");
    XtAddCallback(g->errorShell, XtNpopupCallback, placePopup, g);
    MakeWidget(g->error, g->errorShell, dialogWidgetClass, "error");
    XawDialogAddButton(g->error, "okButton", shellCancel, g);

    /* record box */
    r = &g->recordDialog;
    MakePopup(r->top, g->top, transientShellWidgetClass, "recordShell");
    XtAddCallback(r->top, XtNpopupCallback, placePopup, g);
    MakeWidget(r->form, r->top, formWidgetClass, "form");

    MakeLabel(w, r->form, "recordDurationLabel");
    MakeWidget(r->duration, r->form, asciiTextWidgetClass, "duration");

    MakeLabel(w, r->form, "recordFreqLabel");
    MakeWidget(r->freq, r->form, asciiTextWidgetClass, "recordFreq");

    MakeLabel(w, r->form, "modeLabel");
    MakeWidget(r->mode, r->form, toggleWidgetClass, "mode");
    XtAddCallback(r->mode, XtNcallback, recordMode, g);

    /* gain slider */
    MakeWidget(r->gainSlider, r->form, sliderWidgetClass, "gainSlider");
    XtAddCallback(r->gainSlider, XtNcallback, adjustGain, g);

    MakeButton(w, r->form, recordStart, "recordStart");
    MakeButton(w, r->form, recordMonitor, "monitor");
    MakeButton(w, r->form, recordDismiss, "dismiss");
}

static Boolean
loadFile(g, name)
GlobalDataPtr   g;
char           *name;
{
    Sound           s;
    char            buf[20];

    if (g->data)
    {
	free(g->data);
	g->data = NULL;
    }

    if (!(s = SoundOpenFileForReading(name)))
    {
	ERROR("Can't open file");
	return noFile(g);
    }

    g->numSamples = SoundNumSamples(s);
    g->numTracks = SoundNumTracks(s);
    g->sampleRate = SoundSampleRate(s);

    if (g->numSamples != SoundUnknownNumSamples)
    {
	if (!(g->data = (GraphDataType *)
	      malloc(sizeof(GraphDataType) * SoundNumSamples(s) *
		     SoundNumTracks(s))))
	{
	    SoundCloseFile(s);
	    return noFile(g);
	}

	if (SoundReadFile((char *) g->data, SoundNumBytes(s), s) !=
	    SoundNumBytes(s) ||
	    AuConvertDataToShort(SoundDataFormat(s), SoundNumBytes(s),
				 g->data) == -1)
	{
	    free(g->data);
	    SoundCloseFile(s);
	    g->data = NULL;
	    return noFile(g);
	}
    }
    else
#define CHUNK		(10 * 1024)
#define DATA_BYTES(_n)	((_n) * sizeof(GraphDataType) * SoundNumTracks(s))
#define FILE_BYTES(_n)	((_n) * SoundBytesPerSample(s) * SoundNumTracks(s))
    {
	int             n,
	                offset = 0;
	AuBool          done = AuFalse;
	GraphDataType  *p;

	g->data = (GraphDataType *) malloc(1);
	g->numSamples = 0;

	while (!done)
	{
	    if (!(p = (GraphDataType *)
		  realloc(g->data, DATA_BYTES(g->numSamples + CHUNK))))
	    {
		free(g->data);
		SoundCloseFile(s);
		g->data = NULL;
		return noFile(g);
	    }

	    g->data = p;
	    n = SoundReadFile(((char *) g->data) + offset,
			      FILE_BYTES(CHUNK), s);

	    offset += n;
	    g->numSamples += n / SoundBytesPerSample(s) / SoundNumTracks(s);
	    done = n < FILE_BYTES(CHUNK);
	}

	if (AuConvertDataToShort(SoundDataFormat(s), FILE_BYTES(g->numSamples),
				 g->data) == -1)
	{
	    free(g->data);
	    SoundCloseFile(s);
	    g->data = NULL;
	    return noFile(g);
	}
    }

    sprintf(buf, "%d", g->sampleRate);

    XtVaSetValues(g->filename, XtNlabel, name, NULL);
    XtVaSetValues(g->comment, XtNstring, SoundComment(s), NULL);
    XtVaSetValues(g->frequency, XtNstring, buf, NULL);
    XtVaSetValues(g->dataFormatMenuButton, XtNlabel, SoundDataFormatString(s),
		  NULL);
    XtVaSetValues(g->fileFormatMenuButton, XtNlabel, SoundFileFormatString(s),
		  NULL);

    XtVaSetValues(g->graph, XtNdata, g->data,
		  XtNnumSamples, g->numSamples,
		  XtNnumTracks, g->numTracks,
		  XtNstart, 0, XtNend, g->numSamples,
		  XtNleftMarker, 0, XtNrightMarker, g->numSamples,
		  NULL);

    SoundCloseFile(s);
    clearUndo(g);

    return TRUE;
}

static Boolean
saveFile(g, name, start, end)
String          name;
GlobalDataPtr   g;
int             start,
                end;
{
    Sound           s;
    char           *tmpName,
                   *mktemp();
    short          *p;
    String          st;
    int             n,
                    fileFormat,
                    dataFormat,
                    status;

    XtVaGetValues(g->fileFormatMenuButton, XtNlabel, &st, NULL);
    fileFormat = SoundStringToFileFormat(st);

    XtVaGetValues(g->dataFormatMenuButton, XtNlabel, &st, NULL);
    dataFormat = AuStringToFormat(st);
    XtVaGetValues(g->comment, XtNstring, &st, NULL);

    s = SoundCreate(fileFormat, dataFormat, g->numTracks, g->sampleRate,
		    end - start, st);

    if (!s)
	ERRORf("File/data format mismatch");

    if (!(tmpName = (char *) malloc(strlen(name) + 7)))
    {
	SoundDestroy(s);
	ERRORf(MALLOC_FAIL);
    }

    sprintf(tmpName, "%sXXXXXX", name);
    tmpName = mktemp(tmpName);

    if (!SoundOpenFileForWriting(tmpName, s))
    {
	free(tmpName);
	SoundDestroy(s);
	ERRORf("Can't create output file");
    }

    p = g->data + start * g->numTracks;
    n = (end - start) * g->numTracks;

    AuConvertShortToData(SoundDataFormat(s), n * sizeof(short), p);
    n *= SoundBytesPerSample(s);
    status = SoundWriteFile((char *) p, n, s) != n;
    AuConvertDataToShort(SoundDataFormat(s), n, p);
    GraphRedraw(g->graph);

    if (status || SoundCloseFile(s))
    {
	free(tmpName);
	ERRORf("Error writing output file");
    }

    /* if the file exists then back it up */
    if (!access(name, R_OK))
    {
	char           *backup;

	if (!(backup = (char *) malloc(strlen(name) + 2)))
	{
	    free(tmpName);
	    ERRORf(MALLOC_FAIL);
	}

	sprintf(backup, "%s~", name);

	if (rename(name, backup))
	{
	    free(backup);
	    free(tmpName);
	    ERRORf("Can't create backup file");
	}
    }

    if (rename(tmpName, name))
    {
	free(tmpName);
	ERRORf("Can't rename temp file");
    }

    free(tmpName);
    return TRUE;
}

#define SET_WIDTH(_w)							      \
{									      \
    XtVaGetValues(_w, XtNx, &x, NULL);					      \
    XtVaSetValues(_w, XtNwidth, maxX - x, NULL);			      \
}

static void
alignWidgets(g)
GlobalDataPtr   g;
{
    Position        maxX,
                    x;
    Dimension       width;

    XtRealizeWidget(g->top);
    XtRealizeWidget(g->dialogShell);
    XtRealizeWidget(g->errorShell);
    XtRealizeWidget(g->warningShell);
    XtRealizeWidget(g->recordDialog.top);

    XtVaGetValues(g->graph, XtNx, &x, XtNwidth, &width, NULL);
    maxX = x + width;

    SET_WIDTH(g->filename);
    SET_WIDTH(g->volumeSlider);
    SET_WIDTH(g->comment);

    XtVaSetValues(g->filename, XtNresizable, FALSE, NULL);
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
    int             f;

    XtVaGetValues(w, XtNstring, &s, NULL);
    f = atoi(s);

    if (f != g->sampleRate)
    {
	int             l,
	                r,
	                p;

	if (f < 1 || f > 100000)
	{
	    char            buf[20];

	    sprintf(buf, "%d", g->sampleRate);
	    XtVaSetValues(g->frequency, XtNstring, buf, NULL);
	    ERROR("Illegal frequency");
	}
	else
	{
	    g->sampleRate = f;

	    XtVaGetValues(g->graph, XtNleftMarker, &l, XtNrightMarker, &r,
			  XtNposition, &p, NULL);
	    setTime(g, g->leftTime, l);
	    setTime(g, g->rightTime, r);
	    setTime(g, g->durationTime, r - l);
	    setTime(g, g->positionTime, p);
	}
    }
}

static void
popdownCallback(w, gp, call_data)
Widget          w;
XtPointer       gp;
XtPointer       call_data;
{
    GlobalDataPtr   g = (GlobalDataPtr) gp;

    if (g->popdownCause == XtNameToWidget(g->warning, "cancelButton"))
	fileExit(w, g);
}

static void
offerToSave(w, g)
Widget          w;
GlobalDataPtr   g;
{
    g->modified = FALSE;
    XtAddCallback(g->warningShell, XtNpopdownCallback, popdownCallback, g);
    WARNING(offerToSave, "File has been modified - Save it?");

    g->modified = TRUE;
    fileSave(w, g);
    fileExit(w, g);
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
    {
	if (w == g->top)
	{
	    if (g->modified)
		offerToSave(w, g);
	    else
		fileExit(w, g);
	}
	else if (w == g->dialogShell)
	    shellCancel(XtNameToWidget(g->dialog, "cancelButton"), g);
	else if (w == g->errorShell)
	    shellCancel(XtNameToWidget(g->error, "okButton"), g);
	else if (w == g->recordDialog.top)
	    recordDismiss(XtNameToWidget(g->recordDialog.top, "dismiss"), g, 0);
    }
}

static void
ok(w, event, params, num_params)
Widget          w;
XEvent         *event;
String         *params;
Cardinal       *num_params;
{
    GlobalDataPtr   g = globals;

    XtCallCallbacks(XtNameToWidget(g->dialog, "okButton"), XtNcallback, g);
}

int
main(argc, argv)
int             argc;
char          **argv;
{
    XtAppContext    appContext;
    GlobalDataRec   globalData,
                   *g;
    char           *filename = NULL,
                   *arg,
                   *audioServer = NULL;
    int             i;
    static XtActionsRec actions[] =
    {
	{"quit", quit},
	{"updateFrequency", updateFrequency},
	{"ok", ok},
    };
    RecordDialogPtr r;

    g = globals = &globalData;
    r = &g->recordDialog;
    g->top = XtVaAppInitialize(&appContext, APP_CLASS, NULL, ZERO,
			       &argc, argv, defaultResources, NULL, 0);
    XtAppAddActions(appContext, actions, XtNumber(actions));

    while (--argc)
    {
	arg = *++argv;

	if (!strncmp(arg, "-a", 2))
	{
	    if (--argc)
		audioServer = *++argv;
	    else
		fatalError(USAGE);
	}
	else if (!strncmp(arg, "-h", 2) || !strncmp(arg, "-?", 2))
	    fatalError(USAGE);
	else
	    filename = arg;
    }

    if (!(g->aud = AuOpenServer(audioServer, 0, NULL, 0, NULL, NULL)))
	fatalError("Can't connect to audio server");

    g->selection.data = NULL;
    g->selection.callback = NULL;
    g->dpy = XtDisplay(g->top);
    g->data = NULL;
    g->modified = FALSE;
    g->endian = 1;
    g->undo.data = NULL;
    g->undo.repData = NULL;

    createWidgets(g);

    if (filename)
	loadFile(g, filename);
    else
	noFile(g);

    alignWidgets(g);

    /* handle delete window message */
    g->wm_delete_window = XInternAtom(g->dpy, "WM_DELETE_WINDOW", FALSE);
    XSetWMProtocols(g->dpy, XtWindow(g->top), &g->wm_delete_window, 1);
    XSetWMProtocols(g->dpy, XtWindow(g->dialogShell), &g->wm_delete_window, 1);
    XSetWMProtocols(g->dpy, XtWindow(g->errorShell), &g->wm_delete_window, 1);
    XSetWMProtocols(g->dpy, XtWindow(g->warningShell), &g->wm_delete_window, 1);
    XSetWMProtocols(g->dpy, XtWindow(r->top), &g->wm_delete_window, 1);

    /* get atoms for selections */
    g->selection.atom = XInternAtom(g->dpy, APP_CLASS, FALSE);

    /* get contexts for selections */
    g->selection.context = XUniqueContext();

    r->inputDevice = (AuDeviceID) 0;

    for (i = 0; i < AuServerNumDevices(g->aud); i++)
	if ((AuDeviceKind(AuServerDevice(g->aud, i)) ==
	     AuComponentKindPhysicalInput))
	{
	    r->inputDevice =
		AuDeviceIdentifier(AuServerDevice(globals->aud, i));
	    r->inputDeviceNum = i;
	    break;
	}

    if (r->inputDevice)
    {
	if (AuDeviceChangableMask(AuServerDevice(g->aud, r->inputDeviceNum)) &
	    AuCompDeviceGainMask)
	    XtVaSetValues(r->gainSlider, XtNvalue,
			  AuFixedPointRoundUp(AuDeviceGain(AuServerDevice(g->aud, r->inputDeviceNum))),
			  NULL);
	else
	    XtSetSensitive(r->gainSlider, False);

	if (AuDeviceChangableMask(AuServerDevice(g->aud, r->inputDeviceNum)) &
	    AuCompDeviceLineModeMask)
	{
	    r->currentMode =
		AuDeviceLineMode(AuServerDevice(g->aud, r->inputDeviceNum));
	    XtVaSetValues(r->mode, XtNlabel, r->currentMode ==
			  AuDeviceLineModeLow ? "Line" : "Mic", NULL);
	}
	else
	{
	    XtSetSensitive(r->mode, False);
	    XtSetSensitive(XtNameToWidget(r->form, "modeLabel"), False);
	}
    }
    else
	XtSetSensitive(g->record, False);

    clearUndo(g);

    AuXtAppAddAudioHandler(appContext, g->aud);
    XtAppMainLoop(appContext);
    return 0;
}
