/**
 * Copyright 1994 Network Computing Devices, Inc.
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
 * $NCDId: @(#)auphone.c,v 1.8 1994/04/20 22:29:22 greg Exp $
 */

#include <stdio.h>
#include <malloc.h>

#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
#include <X11/Shell.h>
#include <X11/Xaw/Cardinals.h>

#ifndef SYSV
#include <audio/Aos.h>			/* for string and other os stuff */
#endif
#include <audio/Afuncs.h> 		/* for bcopy et. al. */
#include <audio/audiolib.h>
#include <audio/soundlib.h>
#include <audio/Xtutil.h>

/* widgets */
#include <X11/Xaw/AsciiText.h>
#include <X11/Xaw/Form.h>
#include <X11/Xaw/Label.h>
#include <X11/Xaw/Command.h>
#include <X11/Xaw/Toggle.h>

#include <Slider.h>
#include "phone.xbm"

#ifdef SVR4
/* with "cc -Xc", this isn't defined in <strings.h> */
extern char *strdup(const char *);
#endif

#define USAGE "\
usage: auphone [options]\n\
where options include:\n\
\n\
    -a server    audio server\n\
    -v volume    incoming ring volume in percent (default: 15)\n\
    -l latency   latency in seconds (default: .25)\n\
    -r file      personalized incoming ring sound\n\
    -? or -h     help\n"

#define	APP_CLASS		"Auphone"
#define VERSION			"0.1"

#define	RINGIN			"auphone-ringin"
#define	RINGOUT			"auphone-ringout"
#define BUSY			"auphone-busy"
#define BUSYSIGNAL		"auphone-bsignal"

#define DEFAULT_RING_VOLUME	15
#define DEFAULT_OUTPUT_VOLUME	100
#define DEFAULT_LATENCY		.25
#define SAMPLE_RATE		8000
#define	DATA_FORMAT		AuFormatULAW8
#define	MAX_SAMPLES		((int) (SAMPLE_RATE * g->latency))
#define HIGH_WATER		(MAX_SAMPLES * 3 / 4)
#define LOW_WATER		(MAX_SAMPLES * 1 / 4)
#define MAX_LATENCY_BYTES	(MAX_SAMPLES * g->local.tracks)

#define DEV_ID(_aud, _num)						      \
    AuDeviceIdentifier(AuServerDevice(_aud, _num))

#define min(_a, _b) ((_a) < (_b) ? (_a) : (_b))

#define MakeWidget(_w, _parent, _type, _name)				      \
    (_w) = XtCreateManagedWidget(_name, _type, _parent, NULL, 0)	      \

#define MakeWidgetCB(_w, _parent, _type, _callback, _name)		      \
{									      \
    (_w) = XtCreateManagedWidget(_name, _type, _parent, NULL, 0);	      \
									      \
    if ((void *) (_callback) != NULL)					      \
	XtAddCallback(_w, XtNcallback, _callback, g);			      \
}

#define MakeButton(_w, _parent, _callback, _name)			      \
    MakeWidgetCB(_w, _parent, commandWidgetClass, _callback, _name)

#define MakeLabel(_w, _parent, _name)					      \
    MakeWidget(_w, _parent, labelWidgetClass, _name)

typedef struct
{
    AuServer       *aud;
    AuFlowID        flow,
                    volFlow;
    AuBucketID      ringoutBucket,
                    busyBucket;
    AuEventHandlerRec *handler;
    int             inputDeviceNum,
                    currentMode,
                    tracks,
                    volEl;
    void            (*callback) ();
    XtInputId       id;
}               ServerRec, *ServerPtr;

typedef struct
{
    Display        *dpy;
    XtAppContext    appContext;
    Widget          top,
                    form,
                    call,
                    answer,
                    callerId,
                    hangup,
                    quit,
                    who,
                    volSlider,
                    inputGainSlider,
                    mode,
                    status;
    Atom            wm_delete_window;
    ServerRec       local,
                    remote;
    char           *buf;
    int             in,
                    out,
                    inBytes,
                    outBytes,
                    bufSize;
    float           latency;
}               GlobalData, *GlobalDataPtr;

static GlobalData globals;			/* for actions */

static String   defaultResources[] =
{
    "*auphone.translations:           #override\\n\
	<Message>WM_PROTOCOLS: quit()",
    "*font:                           *courier-medium-r-normal*140*",
    "*call.label:                     Call",
    "*answer.label:                   Answer",
    "*answer.fromHoriz:               call",
    "*callerId.label:                 Caller ID",
    "*callerId.fromHoriz:             answer",
    "*hangup.label:                   Hang up",
    "*hangup.fromHoriz:               callerId",
    "*quit.label:                     Quit",
    "*quit.fromHoriz:                 hangup",
    "*status.borderWidth:             0",
    "*status.fromVert:                call",
    "*status.resizable:               true",
    "*whoLabel.label:                 \\        Who:     ",
    "*whoLabel.borderWidth:           0",
    "*whoLabel.fromVert:              status",
    "*who.fromHoriz:                  whoLabel",
    "*who.fromVert:                   status",
    "*who*editType:                   edit",
    "*who.resizable:                  true",
    "*who.translations:               #override\\n\
	<Key>Return: call()",
    "*volSlider.fromVert:             who",
    "*volSlider.label:                \\     Volume: %3d%%",
    "*volSlider.min:                  1",
    "*volSlider.max:                  200",
    "*volSlider.value:                100",
    "*volSlider.resizable:            true",
    "*inputGainSlider.fromVert:       volSlider",
    "*inputGainSlider.label:          \\ Input Gain: %3d%%",
    "*inputGainSlider.min:            1",
    "*inputGainSlider.max:            100",
    "*inputGainSlider.resizable:      true",
    "*modeLabel.label:                \\ Input Mode:     ",
    "*modeLabel.fromVert:             inputGainSlider",
    "*modeLabel.borderWidth:          0",
    "*mode.label:                     Line",
    "*mode.fromVert:                  inputGainSlider",
    "*mode.fromHoriz:                 modeLabel",
    NULL
};

extern int      ringinFormat,
                ringinRate,
                ringinTracks,
                ringinSize,
                ringoutFormat,
                ringoutRate,
                ringoutTracks,
                ringoutSize,
                busyFormat,
                busyRate,
                busyTracks,
                busySize;
extern char    *ringinData,
               *ringoutData,
               *busyData;

static int
fatalError(message, arg)
char           *message,
               *arg;
{
    fprintf(stderr, message, arg);
    fprintf(stderr, "\n");
    exit(1);
    return 0;
}

static void
readData(g, e)
GlobalDataPtr   g;
AuElementNotifyEvent *e;
{
    int             n;

    if (!g->buf)
    {
	g->bufSize = e->num_bytes;

	if (!(g->buf = (char *) malloc(g->bufSize)))
	    fatalError("malloc failed");
    }
    else
    {
	bcopy(g->buf + g->out, g->buf, g->bufSize - g->out);
	g->in -= g->out;
	g->bufSize -= g->out;
	g->out = 0;

	g->bufSize += e->num_bytes;

	if (!(g->buf = (char *) realloc(g->buf, g->bufSize)))
	    fatalError("malloc failed");
    }

    n = AuReadElement(g->local.aud, e->flow, e->element_num, e->num_bytes,
		      g->buf + g->in, NULL);
    g->in += n;
    g->inBytes += n;
}

static void
writeData(g)
GlobalDataPtr   g;
{
    int             n;

    if (g->inBytes > MAX_LATENCY_BYTES)
    {
	n = g->inBytes - MAX_LATENCY_BYTES;
	g->out += n;
	g->inBytes -= n;
    }

    n = min(g->inBytes, g->outBytes);

    if (!n)
	return;

    AuWriteElement(g->remote.aud, g->remote.flow, 0, n, g->buf + g->out,
		   AuFalse, NULL);

    g->out += n;
    g->outBytes -= n;
    g->inBytes -= n;

    if (!g->inBytes)
    {
	free(g->buf);
	g->buf = NULL;
	g->in = g->out = g->bufSize = 0;
    }
}

static AuBool
EventHandler(aud, ev, handler)
AuServer       *aud;
AuEvent        *ev;
AuEventHandlerRec *handler;
{
    GlobalDataPtr   g = (GlobalDataPtr) handler->data;
    AuElementNotifyEvent *event = (AuElementNotifyEvent *) ev;

    if (ev->type == AuEventTypeElementNotify)
	switch (event->kind)
	{
	    case AuElementNotifyKindHighWater:
		readData(g, event);
		writeData(g);
		break;
	    case AuElementNotifyKindLowWater:
		g->outBytes += event->num_bytes;
		writeData(g);
		break;
	    case AuElementNotifyKindState:
		switch (event->cur_state)
		{
		    case AuStateStop:
			if (handler == g->remote.handler)
			{
			    if (g->remote.callback)
				(*g->remote.callback) (g);
			}
			else
			{
			    if (g->local.callback)
				(*g->local.callback) (g);
			}
			break;
		    case AuStatePause:
			if (aud == g->local.aud)
			{
			    readData(g, event);
			    writeData(g);
			}
			else
			{
			    g->outBytes += event->num_bytes;
			    writeData(g);
			}
			break;
		}
		break;
	}

    return AuTrue;
}

static void
createEmptyBucket(g, aud, server, flow)
GlobalDataPtr   g;
AuServer       *aud;
char           *server;
AuFlowID        flow;
{
    char            buf[100];
    AuString        desc;
    int             i;
    ServerPtr       r = &g->remote;

    for (i = 0; i < AuServerNumDevices(r->aud); i++)
	if (AuDeviceKind(AuServerDevice(r->aud, i)) ==
	    AuComponentKindPhysicalOutput &&
	    AuDeviceNumTracks(AuServerDevice(r->aud, i)) == g->local.tracks)
	    break;

    /* format is: id, version, user, server, flow, deviceNum, multEl */
    sprintf(buf, "%s %s %s %s %d %d 1", BUSY, VERSION, (char *) getenv("USER"),
	    server, flow, i);
    AuSetString(&desc, AuStringLatin1, strlen(buf), buf);
    AuCreateBucket(aud, AuFormatULAW8, 1, AuAccessListMask, 0, 0, &desc, NULL);
}

static void
showStatus(g, format, arg)
GlobalDataPtr   g;
char           *format,
               *arg;
{
    char            buf[50];

    sprintf(buf, format, arg);
    XtVaSetValues(g->status, XtNlabel, buf, NULL);
}

static void
makePhone(g)
GlobalDataPtr   g;
{
    AuElement       elements[3];
    ServerPtr       l = &g->local,
                    r = &g->remote;
    AuDeviceID      d;
    int             i;

    AuMakeElementImportDevice(&elements[0], SAMPLE_RATE,
			      DEV_ID(l->aud, l->inputDeviceNum),
			      AuUnlimitedSamples, 0, NULL);
    AuMakeElementExportClient(&elements[1], 0, SAMPLE_RATE, DATA_FORMAT,
			      l->tracks, AuTrue, MAX_SAMPLES, HIGH_WATER,
			      0, NULL);

    AuSetElements(l->aud, l->flow, AuTrue, 2, elements, NULL);

    /* search for an appropriate output device */
    for (i = 0; i < AuServerNumDevices(r->aud); i++)
	if (AuDeviceKind(AuServerDevice(r->aud, i)) ==
	    AuComponentKindPhysicalOutput &&
	    AuDeviceNumTracks(AuServerDevice(r->aud, i)) == l->tracks)
	{
	    d = AuDeviceIdentifier(AuServerDevice(r->aud, i));
	    break;
	}

    if (i == AuServerNumDevices(r->aud))
	fatalError("Can't find output device");

    AuMakeElementImportClient(&elements[0], SAMPLE_RATE, DATA_FORMAT,
			      l->tracks, AuTrue, MAX_SAMPLES, LOW_WATER,
			      0, NULL);
    AuMakeElementMultiplyConstant(&elements[1], 0,
		      AuFixedPointFromFraction(DEFAULT_OUTPUT_VOLUME, 100));
    AuMakeElementExportDevice(&elements[2], 1, d, SAMPLE_RATE,
			      AuUnlimitedSamples, 0, NULL);

    AuSetElements(r->aud, r->flow, AuTrue, 3, elements, NULL);

    g->inBytes = g->outBytes = g->in = g->out = g->bufSize = 0;
    g->buf = NULL;
}

static int
getCallerInfo(g, user, server, flow, device, mult)
GlobalDataPtr   g;
char          **user,
              **server;
AuFlowID       *flow;
int            *device,
               *mult;
{
    int             n,
                    i;
    AuBucketAttributes *ba;

    ba = AuListBuckets(g->local.aud, 0, NULL, &n, NULL);

    for (i = 0; i < n; i++)
    {
	if (!strncmp(BUSY, AuBucketDescription(&ba[i])->data, strlen(BUSY)))
	{
	    strtok(AuBucketDescription(&ba[i])->data, " ");
	    strtok(NULL, " ");		       /* skip version number */
	    *user = strdup(strtok(NULL, " "));
	    *server = strdup(strtok(NULL, " "));
	    *flow = atoi(strtok(NULL, " "));
	    *device = atoi(strtok(NULL, " "));
	    *mult = atoi(strtok(NULL, " "));
	    AuFreeBucketAttributes(g->local.aud, n, ba);
	    return 1;
	}
    }

    AuFreeBucketAttributes(g->local.aud, n, ba);
    return 0;
}

static void
callerId(w, gp, call_data)
Widget          w;
XtPointer       gp,
                call_data;
{
    GlobalDataPtr   g = (GlobalDataPtr) gp;
    AuFlowID        flow;
    char           *server,
                   *user,
                    buf[50];
    int             dev,
                    mult;

    if (getCallerInfo(g, &user, &server, &flow, &dev, &mult))
    {
	sprintf(buf, "%s@%s", user, server);
	showStatus(g, buf);
	free(user);
	free(server);
    }
    else
	showStatus(g, "No callers");
}

static Boolean  doHangup();

static void
remoteHangup(g)
GlobalDataPtr   g;
{
    g->local.volFlow = 0;
    XtAppAddWorkProc(g->appContext, doHangup, g);
}

static void
remoteAnswered(g)
GlobalDataPtr   g;
{
    ServerPtr       l = &g->local,
                    r = &g->remote;
    char           *user,
                   *server,
                    buf[50];
    int             dev;

    r->callback = remoteHangup;

    while (!getCallerInfo(g, &user, &server, &l->volFlow, &dev, &l->volEl));
    sprintf(buf, "%s@%s", user, server);
    showStatus(g, buf);
    free(user);
    free(server);
    XtVaSetValues(g->volSlider, XtNvalue, DEFAULT_OUTPUT_VOLUME, NULL);
    XtSetSensitive(g->volSlider, True);

    AuStopFlow(l->aud, l->flow, NULL);	       /* stop local ringing */

    makePhone(g);

    AuStartFlow(l->aud, l->flow, NULL);
    AuStartFlow(r->aud, r->flow, NULL);
}

static Bool
ringRemote(g)
GlobalDataPtr   g;
{
    AuBucketID      b;
    AuDeviceID      d;
    AuElement       elements[3];
    AuElementAction actions[2];
    ServerPtr       r = &g->remote;
    AuBucketAttributes *ba;
    int             i,
                    n,
                    volume,
                    rate,
                    tracks;
    char           *p;

    g->local.volFlow = 0;

    ba = AuListBuckets(r->aud, 0, NULL, &n, NULL);

    for (i = 0; i < n; i++)
	if (!strncmp(RINGIN, AuBucketDescription(&ba[i])->data, strlen(RINGIN)))
	    break;

    if (i == n)
    {
	AuFreeBucketAttributes(r->aud, n, ba);
	return False;
    }

    b = AuBucketIdentifier(&ba[i]);
    rate = AuBucketSampleRate(&ba[i]);
    tracks = AuBucketNumTracks(&ba[i]);
    strtok(AuBucketDescription(&ba[i])->data, " ");
    p = strtok(NULL, " ");
    volume = p ? atoi(p) : DEFAULT_RING_VOLUME;

    /* search for an appropriate output device */
    for (i = 0; i < AuServerNumDevices(r->aud); i++)
	if (AuDeviceKind(AuServerDevice(r->aud, i)) ==
	    AuComponentKindPhysicalOutput &&
	    AuDeviceNumTracks(AuServerDevice(r->aud, i)) == tracks)
	{
	    d = AuDeviceIdentifier(AuServerDevice(r->aud, i));
	    break;
	}

    if (i == AuServerNumDevices(r->aud))
	fatalError("Can't find output device");

    AuFreeBucketAttributes(r->aud, n, ba);

    AuMakeSendNotifyAction(&actions[0], AuStateStop, AuStateStart,
			   AuReasonUser);
    AuMakeChangeStateAction(&actions[1], AuStateStop, AuStateStart,
			  AuReasonEOF, r->flow, AuElementAll, AuStateStart);

    AuMakeElementImportBucket(&elements[0], rate, b, AuUnlimitedSamples,
			      0, 2, actions);
    AuMakeElementMultiplyConstant(&elements[1], 0,
				  AuFixedPointFromFraction(volume, 100));
    AuMakeElementExportDevice(&elements[2], 1, d, rate, AuUnlimitedSamples,
			      0, NULL);

    AuSetElements(r->aud, r->flow, AuTrue, 3, elements, NULL);

    r->callback = remoteAnswered;

    if (!(r->handler = AuRegisterEventHandler(r->aud, 0, 0, 0, EventHandler,
					      (AuPointer) g)))
	fatalError("AuRegisterEventHandler failed");

    createEmptyBucket(g, r->aud, AuServerString(g->local.aud), r->flow);

    AuStartFlow(r->aud, r->flow, NULL);
    AuFlush(r->aud);
    return True;
}

static void
ringLocal(g)
GlobalDataPtr   g;
{
    AuElement       elements[3];
    AuElementAction actions[2];
    AuDeviceID      d;
    ServerPtr       l = &g->local;
    int             i;

    AuMakeSendNotifyAction(&actions[0], AuStateStop, AuStateStart,
			   AuReasonUser);
    AuMakeChangeStateAction(&actions[1], AuStateStop, AuStateStart,
			  AuReasonEOF, l->flow, AuElementAll, AuStateStart);

    /* search for an appropriate output device */
    for (i = 0; i < AuServerNumDevices(l->aud); i++)
	if (AuDeviceKind(AuServerDevice(l->aud, i)) ==
	    AuComponentKindPhysicalOutput &&
	    AuDeviceNumTracks(AuServerDevice(l->aud, i)) == ringoutTracks)
	{
	    d = AuDeviceIdentifier(AuServerDevice(l->aud, i));
	    break;
	}

    if (i == AuServerNumDevices(l->aud))
	fatalError("Can't find output device");

    AuMakeElementImportBucket(&elements[0], ringoutRate, l->ringoutBucket,
			      AuUnlimitedSamples, 0, 2, actions);
    AuMakeElementMultiplyConstant(&elements[1], 0,
			       AuFixedPointFromFraction(DEFAULT_RING_VOLUME,
							100));
    AuMakeElementExportDevice(&elements[2], 1, d, ringoutRate,
			      AuUnlimitedSamples, 0, NULL);

    AuSetElements(l->aud, l->flow, AuTrue, 3, elements, NULL);
    AuStartFlow(l->aud, l->flow, NULL);
    showStatus(g, "Ringing...");
}

static void
busySignal(g)
GlobalDataPtr   g;
{
    AuElement       elements[3];
    AuElementAction actions[2];
    AuDeviceID      d;
    ServerPtr       l = &g->local;
    int             i;

    AuMakeSendNotifyAction(&actions[0], AuStateStop, AuStateStart,
			   AuReasonUser);
    AuMakeChangeStateAction(&actions[1], AuStateStop, AuStateStart,
			  AuReasonEOF, l->flow, AuElementAll, AuStateStart);

    /* search for an appropriate output device */
    for (i = 0; i < AuServerNumDevices(l->aud); i++)
	if (AuDeviceKind(AuServerDevice(l->aud, i)) ==
	    AuComponentKindPhysicalOutput &&
	    AuDeviceNumTracks(AuServerDevice(l->aud, i)) == busyTracks)
	{
	    d = AuDeviceIdentifier(AuServerDevice(l->aud, i));
	    break;
	}

    if (i == AuServerNumDevices(l->aud))
	fatalError("Can't find output device");

    AuMakeElementImportBucket(&elements[0], busyRate, l->busyBucket,
			      AuUnlimitedSamples, 0, 2, actions);
    AuMakeElementMultiplyConstant(&elements[1], 0,
			       AuFixedPointFromFraction(DEFAULT_RING_VOLUME,
							100));
    AuMakeElementExportDevice(&elements[2], 1, d, busyRate,
			      AuUnlimitedSamples, 0, NULL);

    AuSetElements(l->aud, l->flow, AuTrue, 3, elements, NULL);
    AuStartFlow(l->aud, l->flow, NULL);
    showStatus(g, "Busy");
}

static void
call(w, gp, call_data)
Widget          w;
XtPointer       gp,
                call_data;
{
    GlobalDataPtr   g = (GlobalDataPtr) gp;
    int             i,
                    n;
    String          server;
    ServerPtr       r = &g->remote;
    AuBucketAttributes *ba;

    XtVaGetValues(g->who, XtNstring, &server, NULL);

    if (!strlen(server))
	return;

    if (!(r->aud = AuOpenServer(server, 0, NULL, 0, NULL, NULL)))
    {
	showStatus(g, "Can't connect to %s", server);
	return;
    }

    if (!(r->flow = AuCreateFlow(r->aud, NULL)))
	fatalError("Couldn't create remote flow");

    /* look for an input device */
    for (i = 0; i < AuServerNumDevices(r->aud); i++)
	if (AuDeviceKind(AuServerDevice(r->aud, i)) ==
	    AuComponentKindPhysicalInput)
	    break;

    if (i == AuServerNumDevices(r->aud))
    {
	showStatus(g, "%s has no input devices", server);
	AuCloseServer(r->aud);
	r->aud = (AuServer *) 0;
	return;
    }

    ba = AuListBuckets(r->aud, 0, NULL, &n, NULL);

    for (i = 0; i < n; i++)
	if (!strncmp(BUSY, AuBucketDescription(&ba[i])->data, strlen(BUSY)))
	    break;

    AuFreeBucketAttributes(r->aud, n, ba);

    if (i == n)
    {
	if (!ringRemote(g))
	{
	    showStatus(g, "%s is not available", server);
	    AuCloseServer(r->aud);
	    r->aud = (AuServer *) 0;
	    return;
	}

	ringLocal(g);

	r->id = AuXtAppAddAudioHandler(g->appContext, r->aud);
    }
    else
    {
	AuCloseServer(r->aud);
	r->aud = (AuServer *) 0;
	busySignal(g);
    }

    XtSetSensitive(g->call, False);
    XtSetSensitive(g->answer, False);
    XtSetSensitive(g->hangup, True);
}

static void
answer(w, gp, call_data)
Widget          w;
XtPointer       gp,
                call_data;
{
    GlobalDataPtr   g = (GlobalDataPtr) gp;
    AuFlowID        flow;
    char           *server,
                   *user,
                    buf[50];
    ServerPtr       l = &g->local,
                    r = &g->remote;
    int             dev;

    l->volFlow = 0;

    if (!getCallerInfo(g, &user, &server, &flow, &dev, &l->volEl))
    {
	showStatus(g, "No callers");
	return;
    }

    sprintf(buf, "%s@%s", user, server);
    showStatus(g, buf);

    if (!(r->aud = AuOpenServer(server, 0, NULL, 0, NULL, NULL)))
	fatalError("Can't open remote server");

    if (!(r->flow = AuCreateFlow(r->aud, NULL)))
	fatalError("Couldn't create remote flow");

    if (!(r->handler = AuRegisterEventHandler(r->aud, 0, 0, 0, EventHandler,
					      (AuPointer) g)))
	fatalError("AuRegisterEventHandler failed");

    makePhone(g);

    createEmptyBucket(g, r->aud, AuServerString(g->local.aud), r->flow);

    r->id = AuXtAppAddAudioHandler(g->appContext, r->aud);

    AuStartFlow(l->aud, l->flow, NULL);
    AuStartFlow(r->aud, r->flow, NULL);
    AuFlush(r->aud);

    XtVaSetValues(g->volSlider, XtNvalue, DEFAULT_OUTPUT_VOLUME, NULL);
    l->volFlow = flow;

    r->callback = remoteHangup;

    AuStopFlow(l->aud, flow, NULL);
    free(user);
    free(server);
    XtSetSensitive(g->call, False);
    XtSetSensitive(g->answer, False);
    XtSetSensitive(g->hangup, True);
    XtSetSensitive(g->volSlider, True);
}

static Boolean
doHangup(g)
GlobalDataPtr   g;
{
    ServerPtr       l = &g->local,
                    r = &g->remote;

    if (r->aud)
    {
	AuUnregisterEventHandler(r->aud, r->handler);
	AuXtAppRemoveAudioHandler(r->aud, r->id);
	AuCloseServer(r->aud);
	r->aud = (AuServer *) 0;
    }

    AuStopFlow(l->aud, l->flow, NULL);

    if (l->volFlow)
	AuStopFlow(l->aud, l->volFlow, NULL);

    XtSetSensitive(g->call, True);
    XtSetSensitive(g->answer, True);
    XtSetSensitive(g->hangup, False);
    XtSetSensitive(g->volSlider, False);

    showStatus(g, "Disconnected");
    return True;
}

static void
hangup(w, gp, call_data)
Widget          w;
XtPointer       gp,
                call_data;
{
    GlobalDataPtr   g = (GlobalDataPtr) gp;
    doHangup(g);
}

static void
quit(w, gp, call_data)
Widget          w;
XtPointer       gp,
                call_data;
{
    GlobalDataPtr   g = (GlobalDataPtr) gp;
    if (g->remote.aud)
    {
	hangup(w, g, call_data);
	AuFlush(g->local.aud);
    }

    exit(0);
}

static void
mode(w, gp, call_data)
Widget          w;
XtPointer       gp,
                call_data;
{
    GlobalDataPtr   g = (GlobalDataPtr) gp;
    AuDeviceAttributes da;

    XtCallActionProc(w, "reset", NULL, NULL, 0);

    if (g->local.currentMode == AuDeviceLineModeLow)
    {
	XtVaSetValues(g->mode, XtNlabel, "Mic", NULL);
	g->local.currentMode = AuDeviceLineModeHigh;
    }
    else
    {
	XtVaSetValues(g->mode, XtNlabel, "Line", NULL);
	g->local.currentMode = AuDeviceLineModeLow;
    }

    AuDeviceLineMode(&da) = g->local.currentMode;
    AuSetDeviceAttributes(g->local.aud,
			  DEV_ID(g->local.aud, g->local.inputDeviceNum),
			  AuCompDeviceLineModeMask, &da, NULL);
}

static void
adjustVolume(w, gp, volp)
Widget          w;
XtPointer	gp,
		volp;
{
    GlobalDataPtr   g = (GlobalDataPtr)gp;
    int             vol = (int)volp;
    AuElementParameters parms;

    if (!g->local.volFlow)
	return;

    parms.flow = g->local.volFlow;
    parms.element_num = g->local.volEl;
    parms.num_parameters = AuParmsMultiplyConstant;
    parms.parameters[AuParmsMultiplyConstantConstant] =
	AuFixedPointFromFraction(vol, 100);

    AuSetElementParameters(g->local.aud, 1, &parms, NULL);
}

static void
adjustInputGain(w, gp, gainp)
Widget          w;
XtPointer	gp,
		gainp;
{
    GlobalDataPtr   g = (GlobalDataPtr)gp;
    int             gain = (int)gainp;
    ServerPtr       l = &g->local;
    AuDeviceAttributes da;

    if (!AuDeviceChangableMask(AuServerDevice(l->aud, l->inputDeviceNum)) &
	AuCompDeviceGainMask)
	return;

    AuDeviceGain(&da) = AuFixedPointFromSum(gain, 0);
    AuSetDeviceAttributes(g->local.aud,
			  DEV_ID(g->local.aud, g->local.inputDeviceNum),
			  AuCompDeviceGainMask, &da, NULL);
}

static void
createWidgets(g)
GlobalDataPtr   g;
{
    Widget          w;

    MakeWidget(g->form, g->top, formWidgetClass, "form");
    MakeButton(g->call, g->form, call, "call");
    MakeButton(g->answer, g->form, answer, "answer");
    MakeButton(g->callerId, g->form, callerId, "callerId");
    MakeButton(g->hangup, g->form, hangup, "hangup");
    MakeButton(g->quit, g->form, quit, "quit");

    MakeLabel(g->status, g->form, "status");

    MakeLabel(w, g->form, "whoLabel");
    MakeWidget(g->who, g->form, asciiTextWidgetClass, "who");

    /* output volume */
    MakeWidget(g->volSlider, g->form, sliderWidgetClass, "volSlider");
    XtAddCallback(g->volSlider, XtNcallback, adjustVolume, g);

    /* input gain slider */
    MakeWidget(g->inputGainSlider, g->form, sliderWidgetClass,
	       "inputGainSlider");
    XtAddCallback(g->inputGainSlider, XtNcallback, adjustInputGain, g);

    MakeLabel(w, g->form, "modeLabel");
    MakeWidget(g->mode, g->form, toggleWidgetClass, "mode");
    XtAddCallback(g->mode, XtNcallback, mode, g);
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
    Position        x,
                    maxX;
    Dimension       width;

    XtRealizeWidget(g->top);

    XtVaGetValues(g->quit, XtNx, &x, XtNwidth, &width, NULL);
    maxX = x + width;

    SET_WIDTH(g->status);
    SET_WIDTH(g->who);

    XtVaGetValues(g->status, XtNwidth, &width, NULL);
    XtVaSetValues(g->volSlider, XtNwidth, width, NULL);
    XtVaSetValues(g->inputGainSlider, XtNwidth, width, NULL);

    XtVaSetValues(g->status, XtNresizable, FALSE, NULL);
    XtVaSetValues(g->who, XtNresizable, FALSE, NULL);
}

static void
callAction(w, event, params, num_params)
Widget          w;
XEvent         *event;
String         *params;
Cardinal       *num_params;
{
    GlobalDataPtr   g = &globals;

    call(g->call, g, 0);
}

static void
quitAction(w, event, params, num_params)
Widget          w;
XEvent         *event;
String         *params;
Cardinal       *num_params;
{
    GlobalDataPtr   g = &globals;

    if (event->type == ClientMessage &&
	event->xclient.data.l[0] != g->wm_delete_window)
	XBell(g->dpy, 0);
    else
	quit(g->quit, g, 0);
}

static void
initLocalAudioServer(g, audioServer, ring, volume)
GlobalDataPtr   g;
char           *audioServer,
               *ring;
int             volume;
{
    ServerPtr       l = &g->local;
    AuDeviceAttributes *da;
    AuBucketAttributes *ba;
    int             i,
                    n;
    Sound           s;
    char            buf[50];

    if (!(l->aud = AuOpenServer(audioServer, 0, NULL, 0, NULL, NULL)))
	fatalError("Can't connect to audio server");

    if (!(l->flow = AuCreateFlow(l->aud, NULL)))
	fatalError("Couldn't create flow");

    if (!(l->handler = AuRegisterEventHandler(l->aud, 0, 0, 0, EventHandler,
					      (AuPointer) g)))
	fatalError("AuRegisterEventHandler failed");

    /* look for an input device */
    for (i = 0; i < AuServerNumDevices(l->aud); i++)
	if (AuDeviceKind(AuServerDevice(l->aud, i)) ==
	    AuComponentKindPhysicalInput)
	{
	    l->inputDeviceNum = i;
	    break;
	}

    if (i == AuServerNumDevices(l->aud))
	fatalError("Audio server has no input devices");

    ba = AuListBuckets(l->aud, 0, NULL, &n, NULL);

    for (i = 0; i < n; i++)
	if (!strncmp(RINGOUT, AuBucketDescription(&ba[i])->data,
		     strlen(RINGOUT)))
	    fatalError("auphone is already running on this server");

    da = AuServerDevice(l->aud, l->inputDeviceNum);

    if (AuDeviceChangableMask(da) & AuCompDeviceGainMask)
	XtVaSetValues(g->inputGainSlider, XtNvalue,
		      AuFixedPointRoundUp(AuDeviceGain(da)), NULL);
    else
	XtSetSensitive(g->inputGainSlider, False);

    if (AuDeviceChangableMask(da) & AuCompDeviceLineModeMask)
    {
	l->currentMode = AuDeviceLineMode(da);
	XtVaSetValues(g->mode, XtNlabel, l->currentMode == AuDeviceLineModeLow ?
		      "Line" : "Mic", NULL);
    }
    else
    {
	XtSetSensitive(g->mode, False);
	XtSetSensitive(XtNameToWidget(g->form, "modeLabel"), False);
    }

    l->tracks = AuDeviceNumTracks(da);
    l->callback = NULL;

    if (ring)
    {
	AuString        desc;
	AuBucketID      bucket;
	AuFlowID        flow;
	int             import;
	unsigned int    numBytes,
	                count;
	char           *chunk;
	AuBool          done = AuFalse;
#define CHUNK_SIZE (10 * 1024)

	if (!(s = SoundOpenFileForReading(ring)))
	    fatalError("Can't open ring %s", ring);

	if (!(chunk = (char *) malloc(CHUNK_SIZE)))
	    fatalError("malloc failed");

	sprintf(buf, "%s %d", RINGIN, volume);
	AuSetString(&desc, AuStringLatin1, strlen(buf), buf);

	if (!(bucket = AuCreateBucket(l->aud, SoundDataFormat(s),
				      SoundNumTracks(s), AuAccessImportMask |
				      AuAccessListMask, SoundSampleRate(s),
				      SoundNumSamples(s), &desc, NULL)))
	    fatalError("Can't create bucket");

	if (!(flow = AuGetScratchFlowToBucket(l->aud, bucket, &import,
					      NULL)))
	    fatalError("Error creating flow");

	numBytes = SoundNumBytes(s);

	while (!done)
	{
	    count = SoundReadFile(chunk, min(CHUNK_SIZE, numBytes), s);
	    numBytes -= count;
	    done = !(count && numBytes);
	    AuWriteElement(l->aud, flow, import, count, chunk, done, NULL);
	}

	AuReleaseScratchFlow(l->aud, flow, NULL);
	free(chunk);
	SoundDestroy(s);
    }
    else
    {
	for (i = 0; i < n; i++)
	    if (!strncmp(RINGIN, AuBucketDescription(&ba[i])->data,
			 strlen(RINGIN)))
		break;

	if (i == n)
	{
	    sprintf(buf, "%s %d", RINGIN, volume);
	    s = SoundCreate(SoundFileFormatNone, ringinFormat, 1, ringinRate,
			    ringinSize, buf);
	    if (!AuSoundCreateBucketFromData(l->aud, s, ringinData,
					     AuAccessImportMask |
					     AuAccessListMask, NULL, NULL))
		fatalError("Can't create bucket");

	    SoundDestroy(s);
	}

    }

    AuFreeBucketAttributes(l->aud, n, ba);

    s = SoundCreate(SoundFileFormatNone, ringoutFormat, ringoutTracks,
		    ringoutRate, ringoutSize, RINGOUT);
    l->ringoutBucket =
	AuSoundCreateBucketFromData(l->aud, s, ringoutData, AuAccessListMask,
				    NULL, NULL);
    SoundDestroy(s);

    s = SoundCreate(SoundFileFormatNone, busyFormat, busyTracks,
		    busyRate, busySize, BUSYSIGNAL);
    l->busyBucket =
	AuSoundCreateBucketFromData(l->aud, s, busyData, AuAccessListMask,
				    NULL, NULL);
    SoundDestroy(s);
}

int
main(argc, argv)
int             argc;
char          **argv;
{
    GlobalDataPtr   g = &globals;
    char           *audioServer = NULL,
                   *ring = NULL,
                   *arg;
    static XtActionsRec actions[] =
    {
	{"call", callAction},
	{"quit", quitAction}
    };
    Pixmap          icon;
    int             volume = DEFAULT_RING_VOLUME;
    double          atof();

    g->top = XtVaAppInitialize(&g->appContext, APP_CLASS, NULL, ZERO,
			       &argc, argv, defaultResources, NULL, 0);
    XtAppAddActions(g->appContext, actions, XtNumber(actions));

    g->dpy = XtDisplay(g->top);
    g->latency = DEFAULT_LATENCY;

    icon = XCreateBitmapFromData(g->dpy, RootWindowOfScreen(XtScreen(g->top)),
				 (char *) phone_bits, phone_width,
				 phone_height);

    XtVaSetValues(g->top, XtNiconPixmap, icon, NULL);

#define ARG() (--argc ? *++argv : (char *) fatalError(USAGE))

    while (--argc)
    {
	arg = *++argv;

	if (arg[0] == '-')
	    switch (arg[1])
	    {
		case 'a':
		    audioServer = ARG();
		    break;
		case 'v':
		    volume = atoi(ARG());
		    break;
		case 'r':
		    ring = ARG();
		    break;
		case 'l':
		    g->latency = (float) atof(ARG());
		    break;
		default:
		    fatalError(USAGE);
	    }
    }

    createWidgets(g);
    alignWidgets(g);

    initLocalAudioServer(g, audioServer, ring, volume);

    /* handle delete window message */
    g->wm_delete_window = XInternAtom(g->dpy, "WM_DELETE_WINDOW", FALSE);
    XSetWMProtocols(g->dpy, XtWindow(g->top), &g->wm_delete_window, 1);

    XtSetSensitive(g->hangup, False);
    XtSetSensitive(g->volSlider, False);

    showStatus(g, "Version %s", VERSION);
    AuXtAppAddAudioHandler(g->appContext, g->local.aud);
    XtAppMainLoop(g->appContext);
    return 0;
}
