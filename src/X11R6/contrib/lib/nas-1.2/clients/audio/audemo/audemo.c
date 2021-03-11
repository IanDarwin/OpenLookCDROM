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
 * $NCDId: @(#)audemo.c,v 1.58 1994/06/01 17:57:38 greg Exp $
 */

#include <stdio.h>
#ifdef __STDC__
#include <stdlib.h>
#endif
#include <signal.h>
#include <malloc.h>

#ifndef SYSV
#include <audio/Aos.h>		/* for string and other os stuff */
#endif
#include <audio/Afuncs.h>	/* for bcopy et. al. */
#include <audio/audiolib.h>
#include <audio/soundlib.h>

#ifdef SYSV  
#include <sys/time.h>		/* for timeval */

/* not defined in <stdio.h> when -Xc used */
extern FILE    *
popen(
#if NeedFunctionPrototypes
      const char *, const char *
#endif
);
#endif						/* SVR4 */

#define AddToLinkedList(head, item)					       \
{									       \
    (item)->prev = NULL;						       \
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
    if ((item)->prev)							       \
	(item)->prev->next = (item)->next;				       \
    else								       \
	head = (item)->next;						       \
}

#define XT

#ifdef XT
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
#include <X11/Xaw/Toggle.h>
#endif						/* XT */

#define APP_INSTANCE		"audemo"
#define	APP_CLASS		"Audemo"
#define	VOLUME_FORMAT		"Volume: %3d%%"
#define	GAIN_FORMAT		"Gain: %3d%%"
#define	DEFAULT_VOLUME		100
#define BUF_SIZE 		200
#define	MAX_VOLUME		200
#define	MIN_VOLUME		1
#define	MAX_GAIN		100
#define	MIN_GAIN		1
#define	DOUBLE_CLICK_TIME	500000		/* in microseconds */

#define VOL			AuFixedPointFromFraction(globals->volume, 100)
#define GAIN			AuFixedPointFromSum(globals->rec.gain, 0)

#define BUCKET_HEADER_FORMAT \
"%-20s  %5.2f     %2d     %5d Hz     %c      %s"

#define MakePopup(_w, _parent, _type, _name)				       \
{									       \
    (_w) =								       \
	XtCreatePopupShell(_name, _type, _parent, NULL, 0);		       \
}

#define MakeWidget(_w, _parent, _type, _callback, _name)		       \
{									       \
    (_w) = XtCreateManagedWidget(_name, _type, _parent, NULL, 0);	       \
									       \
    if ((void *) (_callback) != NULL)					       \
	XtAddCallback(_w, XtNcallback, _callback, g);			       \
}

#define MakeButton(_w, _parent, _callback, _name)			       \
    MakeWidget(_w, _parent, commandWidgetClass, _callback, _name)

#define MakeLabel(_w, _parent, _name)					       \
    MakeWidget(_w, _parent, labelWidgetClass, NULL, _name)

#define MakeMenuItem(_parent, _label)					       \
    XtCreateManagedWidget(_label, smeBSBObjectClass, _parent, NULL, 0)

static String   defaultResources[] =
{
#include "resources.h"
    NULL
};

static char   **makeFileList();
static int      getFileNames();
static void     fatalError(), bucketQueryCB(), bucketRecordDismissCB(),
                meterCB();

#ifdef XT
#define Invert(w)							       \
{									       \
    Pixel fg, bg;							       \
									       \
    XtVaGetValues(w, XtNforeground, &fg, XtNbackground, &bg, NULL);	       \
    XtVaSetValues(w, XtNforeground, bg, XtNbackground, fg, NULL);	       \
}

typedef struct
{
    Widget          popShell,
                    file,
                    fileFormatMenuButton,
                    form;
}               SaveDialogDataRec, *SaveDialogDataPtr;

typedef struct
{
    Widget          popShell,
                    form,
                    record,
                    dismiss,
                    file,
                    fileFormatMenuButton,
                    dataFormatMenuButton,
                    rate,
                    monitor,
                    comment,
                    duration,
                    new,
                    readOnly,
                    mode,
                    gainLabel,
                    gainBar;
    unsigned int    gain;
}               RecordDialogDataRec, *RecordDialogDataPtr;

typedef struct
{
    Widget          popShell,
                    form,
                    dismiss,
                    viewport,
                    bucketList,
                    query,
                    play,
                    record,
                    load,
                    delete,
                    save,
                    formatMenu,
                    formatMenuButton,
                    accessMenu,
                    accessMenuButton;
    int             numBuckets;
    AuBucketAttributes *list;
    char          **bucketText;
}               BucketDialogDataRec, *BucketDialogDataPtr;

typedef struct
{
    Widget          topLevel,
                    meter,
                    quit,
                    form,
                    play,
                    samples,
                    info,
                    viewport,
                    volumeBar,
                    volumeLabel,
                    leftMeter,
                    rightMeter,
                    record,
                    buckets,
                    directory,
                    template,
                    rescan;
    int             volume,
                    numFiles,
                    inputDeviceNum,
                    sourceIndex;
    AuServer       *aud;
    char          **fileNames;
    Display        *dpy;
    RecordDialogDataRec rec;
    BucketDialogDataRec buf;
    SaveDialogDataRec save;
    AuDeviceID      inputDeviceId;
}               GlobalDataRec, *GlobalDataPtr;

typedef struct _ElementList
{
    AuFlowID        flow;
    int             volumeElement,
                    monitorElement;
    struct _ElementList *prev,
                   *next;
}               ElementListRec, *ElementListPtr;

typedef ElementListPtr ElementListId;

static ElementListPtr ElementList;
static int      ElementCount,
                MonitorCount;

#ifndef sun					/* who else doesn't have
						 * this? */
#define strdup ncd_strdup			/* To avoid conflicting with
						 * headers on hosts that *do*
						 * have strdup... */
char           *
strdup(s)
char           *s;
{
    char           *t;

    if (!s)
	return s;
    t = (char *) malloc(strlen(s) + 1);
    strcpy(t, s);
    return t;
}
#endif

static void
RemoveFromElementList(globals, p)
GlobalDataPtr   globals;
ElementListPtr  p;
{
    RemoveFromLinkedList(ElementList, p);
    ElementCount--;

    if (p->monitorElement != -1)
    {
	MonitorCount--;

	if (!MonitorCount)
	    meterCB(globals->aud, NULL, NULL, globals);
    }

    free(p);
}

static          ElementListId
AddToElementList(flow, volumeElement, monitorElement)
AuFlowID        flow;
int             volumeElement,
                monitorElement;
{
    ElementListPtr  p;

    if (!(p = (ElementListPtr) malloc(sizeof(ElementListRec))))
	fatalError("malloc error in AddToElementList");

    p->flow = flow;
    p->volumeElement = volumeElement;
    p->monitorElement = monitorElement;

    AddToLinkedList(ElementList, p);
    ElementCount++;

    if (monitorElement != -1)
	MonitorCount++;

    return (ElementListId) p;
}

static void
queryInputAttributes(globals)
GlobalDataPtr   globals;
{
    Boolean         mode;
    AuDeviceAttributes *attr;
    char            buf[50];

    attr = AuGetDeviceAttributes(globals->aud, globals->inputDeviceId, NULL);
    globals->rec.gain = AuFixedPointRoundUp(AuDeviceGain(attr));
    sprintf(buf, GAIN_FORMAT, globals->rec.gain);
    XtVaSetValues(globals->rec.gainLabel, XtNlabel, buf, NULL);
    XawScrollbarSetThumb(globals->rec.gainBar,
			 (float) globals->rec.gain / MAX_GAIN, -1.0);

    mode = AuDeviceLineMode(attr) == AuDeviceLineModeHigh;
    XtVaSetValues(globals->rec.mode, XtNstate, mode, NULL);

    AuFreeDeviceAttributes(globals->aud, 1, attr);
}

static void
meterToggleCB(w, globalsp, call_data)
Widget          w;
XtPointer       globalsp;
XtPointer       call_data;
{
    GlobalDataPtr   globals = (GlobalDataPtr) globalsp;
    Boolean         meter;
    AuElementState *states,
                   *s;
    ElementListPtr  p;
    int             state;

    XtVaGetValues(globals->meter, XtNstate, &meter, NULL);

    state = meter ? AuStateStart : AuStateStop;

    if (MonitorCount)
    {
	if (!(states = (AuElementState *) malloc(sizeof(AuElementState) *
						 MonitorCount)))
	    return;

	p = ElementList;
	s = states;

	while (p)
	{
	    if (p->monitorElement != -1)
	    {
		AuMakeElementState(s, p->flow, p->monitorElement, state);
		s++;
	    }

	    p = p->next;
	}

	AuSetElementStates(globals->aud, MonitorCount, states, NULL);
	free(states);
	meterCB(globals->aud, NULL, NULL, globals);
    }
}

#define MonitorNotifyEventMin(e, i) ((short *) &(e)->data)[i * (e)->num_fields]
#define MonitorNotifyEventMax(e, i)					       \
    ((short *) &(e)->data)[i * (e)->num_fields + 1]

static void
meterCB(aud, handler, ev, globals)
AuServer       *aud;
AuEventHandlerRec *handler;
AuEvent        *ev;
GlobalDataPtr   globals;
{
    AuMonitorNotifyEvent *e = (AuMonitorNotifyEvent *) ev;
    static int      count;
    static float    left,
                    right;
    float           l;
    Boolean         meter;


    if (!ev)
    {
	XawScrollbarSetThumb(globals->leftMeter, -1.0, 0);
	XawScrollbarSetThumb(globals->rightMeter, -1.0, 0);
	count = left = right = 0;
	return;
    }

    XtVaGetValues(globals->meter, XtNstate, &meter, NULL);

    if (!meter)
	return;

    l = (float) (MonitorNotifyEventMax(e, 0) -
		 MonitorNotifyEventMin(e, 0)) / 65535;

    left += l;

    if (e->num_tracks == 1)
	right += l;
    else
	right += (float) (MonitorNotifyEventMax(e, 1) -
			  MonitorNotifyEventMin(e, 1)) / 65535;

    count++;

    if (count >= MonitorCount)
    {
	left /= MonitorCount;
	right /= MonitorCount;

	XawScrollbarSetThumb(globals->leftMeter, -1.0, left);
	XawScrollbarSetThumb(globals->rightMeter, -1.0, right);
	count = left = right = 0;
    }
}

typedef struct
{
    GlobalDataPtr   globals;
    void            (*callback) ();
}               DonePrivRec, *DonePrivPtr;

static void
doneCB(aud, handler, ev, datap)
AuServer       *aud;
AuEvent        *ev;
AuEventHandlerRec *handler;
AuPointer       datap;
{
    DonePrivPtr     data = (DonePrivPtr) datap;

    if (ev->type == AuEventTypeMonitorNotify)
	meterCB(aud, handler, ev, data->globals);
    else
	(*data->callback) (NULL, data->globals, data);
}

static void
modeCB(w, globalsp, call_data)
Widget          w;
XtPointer       globalsp;
XtPointer       call_data;
{
    GlobalDataPtr   globals = (GlobalDataPtr) globalsp;
    Boolean         mode;
    AuDeviceAttributes a;

    XtVaGetValues(globals->rec.mode, XtNstate, &mode, NULL);
    AuDeviceValueMask(&a) = AuCompDeviceLineModeMask;
    AuDeviceLineMode(&a) = mode ? AuDeviceLineModeHigh : AuDeviceLineModeLow;
    AuSetDeviceAttributes(globals->aud, globals->inputDeviceId,
			  AuCompDeviceLineModeMask, &a, NULL);
}

static void
newBucketCB(w, globalsp, call_data)
Widget          w;
XtPointer       globalsp;
XtPointer       call_data;
{
    GlobalDataPtr   globals = (GlobalDataPtr) globalsp;
    String          s;
    unsigned int    rate,
                    format;
    Boolean         readOnly;
    AuString        desc;
    float           maxDuration;

    XtVaGetValues(globals->rec.rate, XtNstring, &s, NULL);
    rate = atoi(s);
    XtVaGetValues(globals->rec.duration, XtNstring, &s, NULL);
    sscanf(s, "%f", &maxDuration);
    XtVaGetValues(globals->rec.readOnly, XtNstate, &readOnly, NULL);
    XtVaGetValues(globals->rec.dataFormatMenuButton, XtNlabel, &s, NULL);
    format = AuStringToFormat(s);
    XtVaGetValues(globals->rec.comment, XtNstring, &s, NULL);

    desc.type = AuStringLatin1;
    desc.len = strlen(s);
    desc.data = s;

    AuCreateBucket(globals->aud, format, 1,
		   readOnly ? AuAccessImportMask | AuAccessListMask :
		   AuAccessAllMasks, rate,
		   (AuUint32) (maxDuration * rate), &desc, NULL);

    bucketQueryCB(globals->buf.query, globals, NULL);
}

static void
rescanCB(w, globalsp, call_data)
Widget          w;
XtPointer       globalsp;
XtPointer       call_data;
{
    GlobalDataPtr   globals = (GlobalDataPtr) globalsp;
    static char    *noFilesString = "No files found";
    char           *dir,
                   *template;
    int             i;

    if (globals->numFiles)
    {
	for (i = 0; i < globals->numFiles; i++)
	    free(globals->fileNames[i]);

	free(globals->fileNames);
    }

    XtVaGetValues(globals->directory, XtNstring, &dir, NULL);
    XtVaGetValues(globals->template, XtNstring, &template, NULL);
    globals->numFiles = getFileNames(dir, &globals->fileNames, template);

    if (globals->numFiles)
    {
	XawListChange(globals->samples,
		      makeFileList(globals->fileNames, globals->numFiles),
		      globals->numFiles, -1, True);
	XtSetSensitive(globals->samples, True);
    }
    else
    {
	XawListChange(globals->samples, &noFilesString, 1, -1, True);
	XtSetSensitive(globals->samples, False);
    }
}

static void
bucketPlayCB(w, globalsp, call_data)
Widget          w;
XtPointer       globalsp;
XtPointer       call_data;
{
    GlobalDataPtr   globals = (GlobalDataPtr) globalsp;
    static Boolean  playing;
    static AuFlowID flow;
    static int      multiplier;
    static DonePrivRec priv;
    static ElementListId listid;
    XawListReturnStruct *sel;
    BucketDialogDataPtr buf = &globals->buf;
    Boolean         meter;
    int             ind,
                    monitor = -1;

    if (playing)
    {
	if (w)
	    /* user requested stop */
	    AuStopFlow(globals->aud, flow, NULL);
	else
	{
	    /* got a done callback */
	    RemoveFromElementList(globals, listid);
	    playing = False;
	    Invert(globals->buf.play);
	}

	return;
    }
    else if (w != globals->buf.play)
	return;

    sel = XawListShowCurrent(buf->bucketList);

    if ((ind = sel->list_index) == -1)
	return;

    priv.globals = globals;
    priv.callback = bucketPlayCB;

    XtVaGetValues(globals->meter, XtNstate, &meter, NULL);

    if (AuSoundPlayFromBucket(globals->aud,
			      AuBucketIdentifier(&buf->list[ind]), AuNone,
			      VOL, doneCB, &priv, 1, &flow, &multiplier,
			      meter ? &monitor : NULL, NULL))
    {
	listid = AddToElementList(flow, multiplier, monitor);
	playing = True;
	Invert(globals->buf.play);
    }
}

#define COMMENT_LEN 20

static void
bucketQueryCB(w, globalsp, call_data)
Widget          w;
XtPointer       globalsp;
XtPointer       call_data;
{
    GlobalDataPtr   globals = (GlobalDataPtr) globalsp;
    BucketDialogDataPtr buf = &globals->buf;
    char            tmp[200],
                    access[4],
                    desc[COMMENT_LEN];
    int             i;
    static char    *noBucketString = "No buckets";

    if (buf->numBuckets)
    {
	AuFreeBucketAttributes(globals->aud, buf->numBuckets, buf->list);

	for (i = 0; i < buf->numBuckets; i++)
	    free(buf->bucketText[i]);

	free(buf->bucketText);
    }

    buf->list = AuListBuckets(globals->aud, 0, NULL, &buf->numBuckets, NULL);

    if (buf->numBuckets &&
    !(buf->bucketText = (char **) malloc(sizeof(char *) * buf->numBuckets)))
	fatalError("malloc error in bucketQueryCB");

    for (i = 0; i < buf->numBuckets; i++)
    {
	AuBucketAttributes *p;
	int             j;
	char            format;

	p = &buf->list[i];
	*desc = 0;
	*access = 0;

	if (AuBucketValueMask(p) & AuCompCommonDescriptionMask)
	{
	    strncpy(desc, AuBucketDescription(p)->data, COMMENT_LEN - 1);
	    desc[COMMENT_LEN - 1] = 0;
	}

	if (AuBucketAccess(p) & AuAccessImportMask)
	    strcat(access, "I");
	if (AuBucketAccess(p) & AuAccessExportMask)
	    strcat(access, "E");
	if (AuBucketAccess(p) & AuAccessDestroyMask)
	    strcat(access, "D");

	for (j = 0, format = 'A'; j < AuServerNumFormats(globals->aud);
	     j++, format++)
	    if (AuBucketFormat(p) == AuServerFormat(globals->aud, j))
		break;

	sprintf(tmp, BUCKET_HEADER_FORMAT,
		desc, (float) AuBucketNumSamples(p) / AuBucketSampleRate(p),
		AuBucketNumTracks(p), AuBucketSampleRate(p), format, access);

	buf->bucketText[i] = strdup(tmp);
    }

    if (buf->numBuckets)
    {
	XawListChange(buf->bucketList, buf->bucketText, buf->numBuckets, -1,
		      True);
	XtSetSensitive(buf->bucketList, True);
    }
    else
    {
	XawListChange(buf->bucketList, &noBucketString, 1, -1, True);
	XtSetSensitive(buf->bucketList, False);
    }
}

static void
bucketDeleteCB(w, globalsp, call_data)
Widget          w;
XtPointer       globalsp;
XtPointer       call_data;
{
    GlobalDataPtr   globals = (GlobalDataPtr) globalsp;
    XawListReturnStruct *sel;
    BucketDialogDataPtr buf = &globals->buf;
    int             ind;

    sel = XawListShowCurrent(buf->bucketList);

    if ((ind = sel->list_index) == -1)
	return;

    AuDestroyBucket(globals->aud, AuBucketIdentifier(&buf->list[ind]), NULL);
    bucketQueryCB(buf->query, globals, NULL);
}

static void
bucketLoadCB(w, globalsp, call_data)
Widget          w;
XtPointer       globalsp;
XtPointer       call_data;
{
    GlobalDataPtr   globals = (GlobalDataPtr) globalsp;
    XawListReturnStruct *sel;
    int             ind;

    sel = XawListShowCurrent(globals->samples);

    if ((ind = sel->list_index) == -1)
	return;

    AuSoundCreateBucketFromFile(globals->aud, globals->fileNames[ind],
				AuAccessAllMasks, NULL, NULL);
    bucketQueryCB(globals->buf.query, globals, NULL);
}

static void
bucketsCB(w, globalsp, call_data)
Widget          w;
XtPointer       globalsp;
XtPointer       call_data;
{
    GlobalDataPtr   globals = (GlobalDataPtr) globalsp;

    bucketQueryCB(globals->buf.query, globals, 0);
    XtSetSensitive(w, False);
    XtPopup(globals->buf.popShell, XtGrabNone);
}

static void
bucketRecordStartCB(w, globalsp, call_data)
Widget          w;
XtPointer       globalsp;
XtPointer       call_data;
{
    GlobalDataPtr   globals = (GlobalDataPtr) globalsp;
    static Boolean  recording;
    static DonePrivRec priv;
    static AuFlowID flow;
    Boolean         mode;
    XawListReturnStruct *sel;
    BucketDialogDataPtr buf = &globals->buf;
    int             ind;

    if (recording)
    {
	if (w)
	    /* user requested stop */
	    AuStopFlow(globals->aud, flow, NULL);
	else
	{
	    /* got a done callback */
	    recording = False;
	    Invert(globals->rec.record);
	}

	return;
    }
    else if (w != globals->rec.record)
	return;

    sel = XawListShowCurrent(buf->bucketList);

    if ((ind = sel->list_index) == -1)
	return;

    priv.globals = globals;
    priv.callback = bucketRecordStartCB;

    XtVaGetValues(globals->rec.mode, XtNstate, &mode, NULL);

    if (AuSoundRecordToBucket(globals->aud,
			      AuBucketIdentifier(&buf->list[ind]),
			      globals->inputDeviceId, GAIN, doneCB, &priv,
			  mode ? AuDeviceLineModeHigh : AuDeviceLineModeLow,
			      &flow, NULL, NULL))
    {
	Invert(globals->rec.record);
	recording = True;
    }
}

static void
recordStartCB(w, globalsp, call_data)
Widget          w;
XtPointer       globalsp;
XtPointer       call_data;
{
    GlobalDataPtr   globals = (GlobalDataPtr) globalsp;
    static Boolean  recording;
    static DonePrivRec priv;
    static AuFlowID flow;
    String          s,
                    comment;
    AuUint32        rate,
                    dataFormat,
                    fileFormat;
    Boolean         mode;

    if (recording)
    {
	if (w)
	    /* user requested stop */
	    AuStopFlow(globals->aud, flow, NULL);
	else
	{
	    /* got a done callback */
	    recording = False;
	    Invert(globals->rec.record);
	    rescanCB(globals->rescan, globals, 0);
	}

	return;
    }
    else if (w != globals->rec.record)
	return;

    priv.globals = globals;
    priv.callback = recordStartCB;

    XtVaGetValues(globals->rec.fileFormatMenuButton, XtNlabel, &s, NULL);
    fileFormat = SoundStringToFileFormat(s);

    XtVaGetValues(globals->rec.dataFormatMenuButton, XtNlabel, &s, NULL);
    dataFormat = AuStringToFormat(s);

    XtVaGetValues(globals->rec.rate, XtNstring, &s, NULL);
    rate = atoi(s);

    XtVaGetValues(globals->rec.mode, XtNstate, &mode, NULL);
    XtVaGetValues(globals->rec.comment, XtNstring, &comment, NULL);
    XtVaGetValues(globals->rec.file, XtNstring, &s, NULL);

    if (strlen(s) &&
	AuSoundRecordToFile(globals->aud, s, globals->inputDeviceId, GAIN,
			    doneCB, &priv,
			    mode ? AuDeviceLineModeHigh :
			    AuDeviceLineModeLow,
			    fileFormat, comment, rate, dataFormat,
			    &flow, NULL, NULL))
    {
	Invert(globals->rec.record);
	recording = True;
    }
}

static void
monitorCB(w, globalsp, call_data)
Widget          w;
XtPointer       globalsp;
XtPointer       call_data;
{
    GlobalDataPtr   globals = (GlobalDataPtr) globalsp;
    static Boolean  monitoring;
    static AuFlowID flow;
    static int      multiplier;
    static DonePrivRec priv;
    static ElementListId listid;
    String          s;
    Boolean         meter;
    int             rate,
                    monitor = -1;

    if (monitoring)
    {
	if (w)
	    /* user requested stop */
	    AuStopFlow(globals->aud, flow, NULL);
	else
	{
	    /* got a done callback */
	    RemoveFromElementList(globals, listid);
	    monitoring = False;
	    Invert(globals->rec.monitor);
	}

	return;
    }
    else if (w != globals->rec.monitor)
	return;

    XtVaGetValues(globals->rec.rate, XtNstring, &s, NULL);
    rate = atoi(s);

    priv.globals = globals;
    priv.callback = monitorCB;

    XtVaGetValues(globals->meter, XtNstate, &meter, NULL);

    if (AuMonitorDevice(globals->aud, rate, globals->inputDeviceId, AuNone, VOL,
			doneCB, &priv, &flow, &multiplier,
			meter ? &monitor : NULL, NULL))
    {
	listid = AddToElementList(flow, multiplier, monitor);
	monitoring = True;
	Invert(globals->rec.monitor);
    }
}

static void
saveCancel(w, globalsp, call_data)
Widget          w;
XtPointer       globalsp;
XtPointer       call_data;
{
    GlobalDataPtr   globals = (GlobalDataPtr) globalsp;

    XtPopdown(globals->save.popShell);
    XtSetSensitive(globals->buf.save, True);
}

static void
saveOk(w, globalsp, call_data)
Widget          w;
XtPointer       globalsp;
XtPointer       call_data;
{
    GlobalDataPtr   globals = (GlobalDataPtr) globalsp;
    String          s;
    XawListReturnStruct *sel;
    BucketDialogDataPtr buf = &globals->buf;
    int             ind,
                    fileFormat;

    sel = XawListShowCurrent(buf->bucketList);

    if ((ind = sel->list_index) == -1)
	return;

    XtVaGetValues(globals->save.fileFormatMenuButton, XtNlabel, &s, NULL);
    fileFormat = SoundStringToFileFormat(s);

    XtVaGetValues(globals->save.file, XtNstring, &s, NULL);

    if (strlen(s) &&
	AuSoundCreateFileFromBucket(globals->aud, s, fileFormat,
				    AuBucketIdentifier(&buf->list[ind]),
				    NULL))
    {
	saveCancel(w, globals, call_data);
	rescanCB(globals->rescan, globals, 0);
    }
}

static void
okAction(w, event, params, num_params)
Widget          w;
XEvent         *event;
String         *params;
Cardinal       *num_params;
{
    GlobalDataPtr   globals;

    /* retrieve the address of the globals from the first parameter */
    globals = (GlobalDataPtr) atoi(params[0]);
    saveOk(w, globals, 0);
}

static void
bucketSaveCB(w, globalsp, call_data)
Widget          w;
XtPointer       globalsp;
XtPointer       call_data;
{
    GlobalDataPtr   globals = (GlobalDataPtr) globalsp;
    XawListReturnStruct *sel;

    sel = XawListShowCurrent(globals->buf.bucketList);

    if (sel->list_index == -1)
	return;

    XtSetSensitive(w, False);
    XtPopup(globals->save.popShell, XtGrabNonexclusive);
}

static void
bucketDismissCB(w, globalsp, call_data)
Widget          w;
XtPointer       globalsp;
XtPointer       call_data;
{
    GlobalDataPtr   globals = (GlobalDataPtr) globalsp;

    if (globals->inputDeviceId)
	bucketRecordDismissCB(globals->rec.dismiss, globals, NULL);

    bucketPlayCB(NULL, globals, NULL);

    XtPopdown(globals->buf.popShell);
    XtSetSensitive(globals->buckets, True);
}

static void
recordDismissCB(w, globalsp, call_data)
Widget          w;
XtPointer       globalsp;
XtPointer       call_data;
{
    GlobalDataPtr   globals = (GlobalDataPtr) globalsp;

    monitorCB(w, globals, NULL);
    recordStartCB(w, globals, NULL);

    XtPopdown(globals->rec.popShell);
    XtSetSensitive(globals->record, True);
    XtSetSensitive(globals->buf.record, True);
}

static void
bucketRecordDismissCB(w, globalsp, call_data)
Widget          w;
XtPointer       globalsp;
XtPointer       call_data;
{
    GlobalDataPtr   globals = (GlobalDataPtr) globalsp;

    monitorCB(w, globals, NULL);
    bucketRecordStartCB(w, globals, NULL);

    if (globals->inputDeviceId)
    {
	XtPopdown(globals->rec.popShell);
	XtSetSensitive(globals->record, True);
	XtSetSensitive(globals->buf.record, True);
    }
}

#define ReplaceCallback(w, cb)						       \
{									       \
    XtRemoveAllCallbacks(w, XtNcallback);				       \
    XtAddCallback(w, XtNcallback, cb, globals);				       \
}

static void
recordSensitives(globals, state)
GlobalDataPtr   globals;
Boolean         state;
{
    XtSetSensitive(globals->rec.file, state);
}

static void
bucketRecordSensitives(globals, state)
GlobalDataPtr   globals;
Boolean         state;
{
    XtSetSensitive(globals->rec.duration, state);
    XtSetSensitive(globals->rec.readOnly, state);
    XtSetSensitive(globals->rec.new, state);
}

static void
recordCB(w, globalsp, call_data)
Widget          w;
XtPointer       globalsp;
XtPointer       call_data;
{
    GlobalDataPtr   globals = (GlobalDataPtr) globalsp;

    ReplaceCallback(globals->rec.record, recordStartCB);
    ReplaceCallback(globals->rec.dismiss, recordDismissCB);

    recordSensitives(globals, True);
    bucketRecordSensitives(globals, False);
    XtSetSensitive(globals->buf.record, False);

    queryInputAttributes(globals);

    XtSetSensitive(w, False);
    XtPopup(globals->rec.popShell, XtGrabNone);
}

static void
bucketRecordCB(w, globalsp, call_data)
Widget          w;
XtPointer       globalsp;
XtPointer       call_data;
{
    GlobalDataPtr   globals = (GlobalDataPtr) globalsp;

    ReplaceCallback(globals->rec.record, bucketRecordStartCB);
    ReplaceCallback(globals->rec.dismiss, bucketRecordDismissCB);

    recordSensitives(globals, False);
    bucketRecordSensitives(globals, True);
    XtSetSensitive(globals->record, False);

    queryInputAttributes(globals);

    XtSetSensitive(w, False);
    XtPopup(globals->rec.popShell, XtGrabNone);
}

static void
playCB(w, globalsp, datap)
Widget          w;
XtPointer       globalsp;
XtPointer       datap;
{
    GlobalDataPtr   globals = (GlobalDataPtr) globalsp;
    static Boolean  playing;
    static AuFlowID flow;
    static int      multiplier;
    static DonePrivRec priv;
    static ElementListId listid;
    XawListReturnStruct *sel;
    Boolean         meter;
    int             ind,
                    monitor = -1;

    if (playing)
    {
	if (w)
	    /* user requested stop */
	    AuStopFlow(globals->aud, flow, NULL);
	else
	{
	    /* got a done callback */
	    RemoveFromElementList(globals, listid);
	    playing = False;
	    Invert(globals->play);
	}

	return;
    }

    sel = XawListShowCurrent(globals->samples);

    if ((ind = sel->list_index) == -1)
	return;

    priv.globals = globals;
    priv.callback = playCB;

    XtVaGetValues(globals->meter, XtNstate, &meter, NULL);

    if (AuSoundPlayFromFile(globals->aud, globals->fileNames[ind], AuNone,
			    VOL, doneCB, &priv, &flow, &multiplier,
			    meter ? &monitor : NULL, NULL))
    {
	listid = AddToElementList(flow, multiplier, monitor);
	playing = True;
	Invert(globals->play);
    }
}

static void
bucketListCB(w, globalsp, listInfop)
Widget          w;
XtPointer       globalsp;
XtPointer       listInfop;
{
    GlobalDataPtr   globals = (GlobalDataPtr) globalsp;
    XawListReturnStruct *listInfo = (XawListReturnStruct *) listInfop;
    struct timeval  tp;
    AuUint32        currentTime;
    static AuInt32  lastTime;
    static int      lastSelection;

    gettimeofday(&tp, NULL);
    currentTime = tp.tv_sec * 1000000 + tp.tv_usec;

    if (listInfo->list_index == lastSelection &&
	currentTime - lastTime < DOUBLE_CLICK_TIME)
	bucketPlayCB(globals->buf.play, globals, NULL);

    lastTime = currentTime;
    lastSelection = listInfo->list_index;
}

static void
samplesCB(w, globalsp, listInfop)
Widget          w;
XtPointer       globalsp;
XtPointer       listInfop;
{
    GlobalDataPtr   globals = (GlobalDataPtr) globalsp;
    XawListReturnStruct *listInfo = (XawListReturnStruct *) listInfop;
    struct timeval  tp;
    AuInt32         currentTime;
    static AuInt32  lastTime;
    static int      lastSelection = -1;

    gettimeofday(&tp, NULL);
    currentTime = tp.tv_sec * 1000000 + tp.tv_usec;

    if (listInfo->list_index == lastSelection &&
	currentTime - lastTime < DOUBLE_CLICK_TIME)
	playCB(globals->play, globals, (XtPointer) 0);
    else if (lastSelection != listInfo->list_index)
    {
	Sound           s;

	lastSelection = listInfo->list_index;

	if ((s = SoundOpenFileForReading(globals->fileNames[lastSelection])))
	{
	    char           *buf,
	                   *p;

#define PRINT(p, f, a)							       \
{									       \
    sprintf(p, f, a);							       \
    p += strlen(p);							       \
}

	    if (!(p = buf = (char *) malloc(2000 + strlen(SoundComment(s)))))
		fatalError("Can't malloc text in samplesCB");

	    PRINT(p, "   Filename: %s\n", globals->fileNames[lastSelection]);
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
	}
    }

    gettimeofday(&tp, NULL);
    lastTime = tp.tv_sec * 1000000 + tp.tv_usec;
}

static void
quitCB(w, globalsp, call_data)
Widget          w;
XtPointer       globalsp;
XtPointer       call_data;
{
    exit(0);
}

static void
adjustVolume(globals)
GlobalDataPtr   globals;
{
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
	parms[i].parameters[AuParmsMultiplyConstantConstant] = VOL;

	p = p->next;
	i++;
    }

    AuSetElementParameters(globals->aud, ElementCount, parms, NULL);
    free(parms);
}

static void
scrollProcCB(w, globalsp, positionp)
Widget          w;
XtPointer       globalsp;
XtPointer       positionp;
{
    GlobalDataPtr   globals = (GlobalDataPtr) globalsp;
    int             position = (int) positionp;
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

static void
jumpProcCB(w, globalsp, percentp)
Widget          w;
XtPointer       globalsp;
XtPointer       percentp;
{
    GlobalDataPtr   globals = (GlobalDataPtr) globalsp;
    float          *percent = (float *) percentp;
    int             newVolume;
    char            buf[50];

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

static void
adjustGain(globals)
GlobalDataPtr   globals;
{
    AuDeviceAttributes a;

    AuDeviceValueMask(&a) = AuCompDeviceGainMask;
    AuDeviceGain(&a) = GAIN;
    AuSetDeviceAttributes(globals->aud, globals->inputDeviceId,
			  AuCompDeviceGainMask, &a, NULL);
}

static void
gainScrollCB(w, globalsp, positionp)
Widget          w;
XtPointer       globalsp;
XtPointer       positionp;
{
    GlobalDataPtr   globals = (GlobalDataPtr) globalsp;
    int             position = (int) positionp;
    int             newGain;
    char            buf[50];

    newGain = globals->rec.gain + (position > 0 ? -1 : 1);

    if (newGain < MIN_GAIN)
	newGain = MIN_GAIN;
    else if (newGain > MAX_GAIN)
	newGain = MAX_GAIN;

    if (newGain != globals->rec.gain)
    {
	globals->rec.gain = newGain;
	sprintf(buf, GAIN_FORMAT, globals->rec.gain);
	XtVaSetValues(globals->rec.gainLabel, XtNlabel, buf, NULL);

	XawScrollbarSetThumb(globals->rec.gainBar,
			     (float) globals->rec.gain / MAX_GAIN, -1.0);
	adjustGain(globals);
    }
}

static void
gainJumpCB(w, globalsp, percentp)
Widget          w;
XtPointer       globalsp;
XtPointer       percentp;
{
    GlobalDataPtr   globals = (GlobalDataPtr) globalsp;
    float          *percent = (float *) percentp;
    int             newGain;
    char            buf[50];

    newGain = *percent * MAX_GAIN;

    if (newGain < MIN_GAIN)
	newGain = MIN_GAIN;

    if (newGain != globals->rec.gain)
    {
	globals->rec.gain = newGain;
	sprintf(buf, GAIN_FORMAT, globals->rec.gain);
	XtVaSetValues(globals->rec.gainLabel, XtNlabel, buf, NULL);
	adjustGain(globals);
    }
}

static void
setFileFormatMenuButton(w, globalsp, call_data)
Widget          w;
XtPointer       globalsp;
XtPointer       call_data;
{
    GlobalDataPtr   g = (GlobalDataPtr) globalsp;
    String          string;

    XtVaGetValues(w, XtNlabel, &string, NULL);
    XtVaSetValues(g->save.fileFormatMenuButton, XtNlabel, string, NULL);

    if (g->inputDeviceId)
	XtVaSetValues(g->rec.fileFormatMenuButton, XtNlabel, string, NULL);
}

static void
setDataFormatMenuButton(w, globalsp, call_data)
Widget          w;
XtPointer       globalsp;
XtPointer       call_data;
{
    GlobalDataPtr   g = (GlobalDataPtr) globalsp;
    String          string;

    XtVaGetValues(w, XtNlabel, &string, NULL);
    XtVaSetValues(g->rec.dataFormatMenuButton, XtNlabel, string, NULL);
}

static void
makeSaveDialog(g)
GlobalDataPtr   g;
{
    SaveDialogDataPtr s = &g->save;
    Widget          w;
    char            tmp[50];

    MakePopup(s->popShell, g->topLevel, transientShellWidgetClass,
	      "savePop");
    MakeWidget(s->form, s->popShell, formWidgetClass, NULL, "saveWin");

    /* filename */
    MakeLabel(w, s->form, "filenameLabel");
    MakeWidget(s->file, s->form, asciiTextWidgetClass, NULL, "filename");

    /* pass the address of the globals as an argument to the action */
    sprintf(tmp, "<Key>Return: ok(%u)", (unsigned int) g);
    XtOverrideTranslations(s->file, XtParseTranslationTable(tmp));

    /* file format */
    MakeLabel(w, s->form, "fileFormatLabel");
    MakeWidget(s->fileFormatMenuButton, s->form, menuButtonWidgetClass, NULL,
	       "fileFormatMenuButton");
    XtVaSetValues(s->fileFormatMenuButton, XtNlabel, SoundFileFormatToString(0),
		  NULL);

    MakeButton(w, s->form, saveOk, "ok");
    MakeButton(w, s->form, saveCancel, "cancel");
}

static void
makeBucketDialog(g)
GlobalDataPtr   g;
{
    Widget          w;
    int             i;
    BucketDialogDataPtr b = &g->buf;
    char            tmp[100];

    makeSaveDialog(g);

    b->numBuckets = 0;
    MakePopup(b->popShell, g->topLevel, transientShellWidgetClass,
	      "bucketPop");
    MakeWidget(b->form, b->popShell, formWidgetClass, NULL, "bucketWin");

    /* command buttons */
    MakeButton(b->query, b->form, bucketQueryCB, "query");
    MakeButton(b->play, b->form, bucketPlayCB, "play");
    MakeButton(b->record, b->form, bucketRecordCB, "record");
    MakeButton(b->load, b->form, bucketLoadCB, "load");
    MakeButton(b->save, b->form, bucketSaveCB, "save");
    MakeButton(b->delete, b->form, bucketDeleteCB, "delete");
    MakeButton(b->dismiss, b->form, bucketDismissCB, "dismiss");

    /* format menu */
    MakePopup(b->formatMenu, b->form, simpleMenuWidgetClass, "formatMenu");
    for (i = 0; i < AuServerNumFormats(g->aud); i++)
    {
	sprintf(tmp, "%c - %s", 'A' + i,
		AuFormatToString(AuServerFormat(g->aud, i)));
	MakeMenuItem(b->formatMenu, tmp);
    }
    MakeWidget(b->formatMenuButton, b->form, menuButtonWidgetClass, NULL,
	       "format");

    /* access menu */
    MakePopup(b->accessMenu, b->form, simpleMenuWidgetClass, "accessMenu");
    MakeMenuItem(b->accessMenu, "I - Importable");
    MakeMenuItem(b->accessMenu, "E - Exportable");
    MakeMenuItem(b->accessMenu, "D - Destroyable");
    MakeWidget(b->accessMenuButton, b->form, menuButtonWidgetClass, NULL,
	       "access");

    MakeLabel(w, b->form, "header");
    MakeWidget(b->viewport, b->form, viewportWidgetClass, NULL, "viewport");

    MakeWidget(b->bucketList, b->viewport, listWidgetClass, bucketListCB,
	       "list");
}

static void
makeRecordDialog(g)
GlobalDataPtr   g;
{
    Widget          w;
    RecordDialogDataPtr r = &g->rec;

    MakePopup(r->popShell, g->topLevel, transientShellWidgetClass, "recordPop");
    MakeWidget(r->form, r->popShell, formWidgetClass, NULL, "recordWin");

    /* filename */
    MakeLabel(w, r->form, "filenameLabel");
    MakeWidget(r->file, r->form, asciiTextWidgetClass, NULL, "filename");

    /* file format */
    MakeLabel(w, r->form, "fileFormatLabel");
    MakeWidget(r->fileFormatMenuButton, r->form, menuButtonWidgetClass, NULL,
	       "fileFormatMenuButton");
    XtVaSetValues(r->fileFormatMenuButton, XtNlabel, SoundFileFormatToString(0),
		  NULL);

    /* data format */
    MakeLabel(w, r->form, "dataFormatLabel");
    MakeWidget(r->dataFormatMenuButton, r->form, menuButtonWidgetClass, NULL,
	       "dataFormatMenuButton");
    XtVaSetValues(r->dataFormatMenuButton, XtNlabel,
		  AuFormatToString(AuServerFormat(g->aud, 0)), NULL);

    /* max duration */
    MakeLabel(w, r->form, "durationLabel");
    MakeWidget(r->duration, r->form, asciiTextWidgetClass, NULL, "duration");

    /* read only toggle */
    MakeWidget(r->readOnly, r->form, toggleWidgetClass, NULL, "readOnly");

    /* frequency */
    MakeLabel(w, r->form, "frequencyLabel");
    MakeWidget(r->rate, r->form, asciiTextWidgetClass, NULL, "frequency");

    /* mode toggle */
    MakeWidget(r->mode, r->form, toggleWidgetClass, modeCB, "lineMode");

    /* comment */
    MakeLabel(w, r->form, "commentLabel");
    MakeWidget(r->comment, r->form, asciiTextWidgetClass, NULL, "comment");

    /* gain */
    MakeLabel(r->gainLabel, r->form, "gainLabel");
    MakeWidget(r->gainBar, r->form, scrollbarWidgetClass, NULL, "gain");
    XtAddCallback(r->gainBar, XtNscrollProc, gainScrollCB, g);
    XtAddCallback(r->gainBar, XtNjumpProc, gainJumpCB, g);

    /* command buttons */
    MakeButton(r->record, r->form, recordStartCB, "record");
    MakeButton(r->monitor, r->form, monitorCB, "monitor");
    MakeButton(r->new, r->form, newBucketCB, "new");
    MakeButton(r->dismiss, r->form, NULL, "dismiss");
}

static void
createWidgets(g, dir)
GlobalDataPtr   g;
char           *dir;
{
    Widget          w,
                    ww;
    int             i;
    char            buf[20];

    MakeWidget(g->form, g->topLevel, formWidgetClass, NULL, "mainWin");

    /* command buttons */
    MakeButton(g->play, g->form, playCB, "play");
    MakeButton(g->record, g->form, recordCB, "record");
    MakeButton(g->buckets, g->form, bucketsCB, "buckets");
    MakeWidget(g->meter, g->form, toggleWidgetClass, meterToggleCB,
	       "meterToggle");
    MakeButton(g->rescan, g->form, rescanCB, "rescan");
    MakeButton(g->quit, g->form, quitCB, "quit");

    /* version label */
    MakeLabel(w, g->form, "version");
    sprintf(buf, "NAS %d.%d", AuServerProtocolMajorVersion(g->aud),
	    AuServerProtocolMinorVersion(g->aud));
    XtVaSetValues(w, XtNlabel, buf, NULL);

    /* left meter */
    MakeLabel(w, g->form, "leftMeterLabel");
    MakeWidget(g->leftMeter, g->form, scrollbarWidgetClass, NULL, "leftMeter");

    /* right meter */
    MakeLabel(w, g->form, "rightMeterLabel");
    MakeWidget(g->rightMeter, g->form, scrollbarWidgetClass, NULL,
	       "rightMeter");

    /* volume label */
    MakeLabel(g->volumeLabel, g->form, "volumeLabel");
    sprintf(buf, VOLUME_FORMAT, g->volume);
    XtVaSetValues(g->volumeLabel, XtNlabel, buf, NULL);

    /* volume slider */
    MakeWidget(g->volumeBar, g->form, scrollbarWidgetClass, NULL, "volume");
    XawScrollbarSetThumb(g->volumeBar,
			 ((float) DEFAULT_VOLUME) / MAX_VOLUME, -1.0);
    XtAddCallback(g->volumeBar, XtNscrollProc, scrollProcCB, g);
    XtAddCallback(g->volumeBar, XtNjumpProc, jumpProcCB, g);

    /* info window */
    MakeWidget(g->info, g->form, asciiTextWidgetClass, NULL, "info");

    /* directory */
    MakeLabel(w, g->form, "directoryLabel");
    MakeWidget(g->directory, g->form, asciiTextWidgetClass, NULL, "directory");
    XtVaSetValues(g->directory, XtNstring, dir, NULL);

    /* template */
    MakeLabel(w, g->form, "templateLabel");
    MakeWidget(g->template, g->form, asciiTextWidgetClass, NULL, "template");

    /* samples window */
    MakeWidget(g->viewport, g->form, viewportWidgetClass, NULL, "viewport");
    MakeWidget(g->samples, g->viewport, listWidgetClass, samplesCB, "list");

    /* file format menu */
    MakePopup(w, g->topLevel, simpleMenuWidgetClass, "fileFormatMenu");

    for (i = 0; i < SoundNumFileFormats; i++)
	MakeWidget(ww, w, smeBSBObjectClass, setFileFormatMenuButton,
		   SoundFileFormatToString(i));

    /* data format menu */
    MakePopup(w, g->topLevel, simpleMenuWidgetClass, "dataFormatMenu");

    for (i = 0; i < AuServerNumFormats(g->aud); i++)
	MakeWidget(ww, w, smeBSBObjectClass, setDataFormatMenuButton,
		   AuFormatToString(AuServerFormat(g->aud, i)));

    makeBucketDialog(g);

    if (g->inputDeviceId)
    {
	makeRecordDialog(g);

	if (!(AuDeviceChangableMask(AuServerDevice(g->aud, g->inputDeviceNum)) &
	      AuCompDeviceLineModeMask))
	    XtVaSetValues(g->rec.mode, XtNsensitive, False, NULL);
    }
    else
    {
	g->rec.popShell = (Widget) 0;
	XtVaSetValues(g->record, XtNsensitive, False, NULL);
	XtVaSetValues(g->buf.record, XtNsensitive, False, NULL);
    }

}

#define SET_WIDTH(_w)							       \
{									       \
    XtVaGetValues(_w, XtNx, &x, NULL);					       \
    XtVaSetValues(_w, XtNwidth, maxX - x, NULL);			       \
}

static void
alignWidgets(g)
GlobalDataPtr   g;
{
    Position        maxX,
                    x;
    Dimension       width;
    BucketDialogDataPtr b = &g->buf;
    RecordDialogDataPtr r = &g->rec;
    SaveDialogDataPtr s = &g->save;

    /* main window */
    XtRealizeWidget(g->topLevel);
    XtVaGetValues(XtNameToWidget(g->form, "version"),
		  XtNx, &x, XtNwidth, &width, NULL);
    maxX = x + width - 2;		       /* the -2 is a hack since
					        * "version" has no border */

    SET_WIDTH(g->leftMeter);
    SET_WIDTH(g->rightMeter);
    SET_WIDTH(g->volumeBar);
    SET_WIDTH(g->info);
    SET_WIDTH(g->directory);
    SET_WIDTH(g->template);
    SET_WIDTH(g->viewport);

    /* buckets window */
    XtRealizeWidget(b->popShell);
    XtVaGetValues(b->accessMenuButton, XtNx, &x, XtNwidth, &width, NULL);
    maxX = x + width;

    SET_WIDTH(b->viewport);

    if (r->popShell)
    {
	/* record window */
	XtRealizeWidget(r->popShell);
	XtVaGetValues(r->readOnly, XtNx, &x, XtNwidth, &width, NULL);
	maxX = x + width;

	SET_WIDTH(r->file);
	SET_WIDTH(r->comment);
	SET_WIDTH(r->fileFormatMenuButton);
	SET_WIDTH(r->dataFormatMenuButton);
	SET_WIDTH(r->gainBar);

	XtVaSetValues(r->fileFormatMenuButton, XtNresizable, False, NULL);
	XtVaSetValues(r->dataFormatMenuButton, XtNresizable, False, NULL);
    }

    XtRealizeWidget(s->popShell);
    XtVaGetValues(s->file, XtNx, &x, XtNwidth, &width, NULL);
    maxX = x + width;
    SET_WIDTH(s->fileFormatMenuButton);
    XtVaSetValues(s->fileFormatMenuButton, XtNresizable, False, NULL);

    XtVaSetValues(g->viewport, XtNresizable, False, NULL);
    XtVaSetValues(b->viewport, XtNresizable, False, NULL);
}

int
main(argc, argv)
int             argc;
char          **argv;
{
    int             i,
                    endian = 1;
    char           *audioServerString = NULL,
                   *dir = NULL;
    GlobalDataRec   globalData,
                   *globals;
    XtAppContext    appContext;
    extern int      AuMonitorFormat;
    static XtActionsRec Actions[] = {{"ok", okAction}};

    globals = &globalData;

    globals->topLevel = XtVaAppInitialize(&appContext, APP_CLASS, NULL, ZERO,
					  &argc, argv, defaultResources,
					  NULL, 0);

    globals->volume = DEFAULT_VOLUME;
    globals->numFiles = 0;
    globals->dpy = XtDisplay(globals->topLevel);

    while (argc > 1)
    {
	argv++;
	argc--;

	if (!strcmp("-a", *argv) || !strcmp("-audio", *argv))
	{
	    audioServerString = argv[1];
	    argv++;
	    argc--;
	    if (!argc)
	    {
		printf("usage: audemo [ -toolkitoption ...] [-audio AUDIOSERVER] [directory]\n");
		exit(1);
	    }
	}
	else
	    dir = *argv;
    }

    if (!dir)
	dir = (char *) getcwd(NULL, 256);

    if (!(globals->aud =
	  AuOpenServer(audioServerString, 0, NULL, 0, NULL, NULL)))
	fatalError("Can't connect to audio server");

    globals->inputDeviceId = (AuDeviceID) 0;

    for (i = 0; i < AuServerNumDevices(globals->aud); i++)
	if ((AuDeviceKind(AuServerDevice(globals->aud, i)) ==
	     AuComponentKindPhysicalInput))
	{
	    globals->inputDeviceId =
		AuDeviceIdentifier(AuServerDevice(globals->aud, i));
	    globals->inputDeviceNum = i;
	    break;
	}

    XtAppAddActions(appContext, Actions, XtNumber(Actions));

    createWidgets(globals, dir);
    rescanCB(NULL, globals, NULL);
    alignWidgets(globals);

    AuMonitorFormat = *(char *) &endian ? AuFormatLinearSigned16LSB :
	AuFormatLinearSigned16MSB;

    AuXtAppAddAudioHandler(appContext, globals->aud);
    XtAppMainLoop(appContext);
    return 0;
}

#endif						/* XT */

static void
fatalError(message, arg)
char           *message,
               *arg;
{
    fprintf(stderr, message, arg);
    fprintf(stderr, "\n");
    exit(1);
}

static int
sortRoutine(ap, bp)
_AuConst void          *ap,
                       *bp;
{
    _AuConst char **a = (_AuConst char **)ap;
    _AuConst char **b = (_AuConst char **)bp;

    return strcmp(strrchr(*a, '/') + 1, strrchr(*b, '/') + 1);
}

static char   **
makeFileList(fileNames, nfiles)
char          **fileNames;
int             nfiles;
{
    char          **fileList,
                   *p;
    int             i;

    qsort(fileNames, nfiles, sizeof(char *), sortRoutine);

    if (!(fileList = (char **) malloc(sizeof(char *) * nfiles)))
	fatalError("Can't malloc file list in makeFileList");

    for (i = 0; i < nfiles; i++)
    {
	p = strrchr(fileNames[i], '/') + 1;
	fileList[i] = (char *) strdup(p);

	if ((p = strchr(fileList[i], '.')))
	    *p = 0;

	fileList[i] = (char *) realloc(fileList[i], strlen(fileList[i]) + 1);

	if (!fileList[i])
	    fatalError("Can't realloc file list in makeFileList");
    }

    return fileList;
}

static FILE    *
startFind(dir, template)
char           *dir,
               *template;
{
    char           *cmd,
                   *p;
    FILE           *fp;
    int             first = 1;

    cmd = (char *) malloc(2000);

    sprintf(cmd, "find %s -type f -a \\( ", dir);

    p = strtok(template, " ");

    while (p)
    {
	if (!first)
	    strcat(cmd, " -o -name '");
	else
	{
	    strcat(cmd, " -name '");
	    first = 0;
	}

	strcat(cmd, p);
	strcat(cmd, "'");

	p = strtok(NULL, " ");
    }

    strcat(cmd, " \\) -print");

    fp = popen(cmd, "r");
    free(cmd);
    return fp;
}

static int
getFileNames(dir, fileNames, template)
char           *dir,
             ***fileNames,
               *template;
{
    int             files;
    FILE           *fp;
    char            line[BUF_SIZE];

    /* prime the realloc pump */
    *fileNames = (char **) malloc(1);

    if (!fileNames)
	fatalError("Can't malloc file names in getFileNames");

    if (!(fp = startFind(dir, template)))
    {
	free(*fileNames);
	return 0;
    }

    files = 0;

    while (fgets(line, BUF_SIZE, fp))
    {
	*fileNames =
	    (char **) realloc(*fileNames, sizeof(char *) * (files + 1));

	if (!*fileNames)
	    fatalError("Can't realloc file names in getFileNames");

	line[strlen(line) - 1] = 0;	       /* zap the trailing newline */
	(*fileNames)[files++] = (char *) strdup(line);
    }

    pclose(fp);

    if (!files)
	free(*fileNames);

    return files;
}
