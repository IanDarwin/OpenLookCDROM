/*
 * Copyright 1993 Greg Renda and Network Computing Devices, Inc.
 * 
 * Permission to use, copy, modify, distribute, and sell this software and its
 * documentation for any purpose is hereby granted without fee, provided that
 * the above copyright notice appear in all copies and that both that
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
 * $NCDId: @(#)audial.c,v 1.13 1994/05/31 22:38:40 greg Exp $
 * 
 * Author:	Greg Renda <greg@ncd.com>
 *		recognition by Kevin Martin
 */

/* audial.c - touch tone dialer and recognizer */

#include <stdio.h>
#include <sys/types.h>
#ifndef ISC40
#include <sys/file.h>
#endif /* !ISC40 */
#include <malloc.h>

#include <audio/Aos.h>			/* for string and other os stuff */
#include <audio/Afuncs.h> 		/* for bcopy et. al. */
#include <audio/audiolib.h>

#if defined(SYSV) || defined(SVR4)
#define signal sigset
#endif

#define USAGE "\
usage: audial [-options] [dialing_string]\n\
general options:\n\
    -a audiosvr    audio server\n\
dialing options:\n\
    -s spacing	   spacing between digits in milliseconds (default 100)\n\
    -p pause	   duration of pause \",\" in milliseconds (default 400)\n\
    -d duration	   duration of digit in milliseconds (default 100)\n\
    -v volume      output volume in percent (default 100)\n\
    legal digits in the dialing_string are: 0123456789abcd*#,\n\
    all others are ignored\n\
recognition options:\n\
    -r             enable recognition mode\n\
    -m             use microphone line level\n\
    -g gain        input gain in percent (default 95)\n\
    -t time        how long to listen in seconds (default forever)\n\
"

#define SAMPLE_RATE		4000
#define WAVE_FORM		AuWaveFormSine
#define DEFAULT_SPACE 		100
#define DEFAULT_PAUSE 		400
#define DEFAULT_DURATION 	100
#define DEFAULT_VOLUME 		100
#define	DEFAULT_GAIN		95
#define HIGHWATER_MARK		75

#define EXPORT_SIZE		(SAMPLE_RATE * 1)

#define PAUSE_DIGIT		16
#define	DEVICE_ELEMENT		17

#ifdef NEEDUSLEEP
#ifdef VMS
#define MAXINT 0xFFFFFFFF
/*
 * VMS uses a quadword to hold the binary time in nsecs; in addition, the
 * time that gets passed to SYS$SCHDWK (Scheduled Wakeup) must be negative to
 * indicate a relative time. OBH-4/16/91
 */
static void
usleep(usecs)
unsigned int    usecs;
{
    double          s1;
    AuUint32   quad[2];

    s1 = (double) usecs *-100.0 * 100.0;

    quad[1] = (AuUint32) MAXINT;
    s1 += ((double) quad[1]);
    quad[0] = (AuUint32) s1 + 1;
    sys$schdwk(0, 0, &quad[0], 0);
    sys$hiber();
}

#else

#ifdef SYSV
#include <poll.h>

int
usleep(usec)
unsigned int    usec;
{
    struct pollfd   f;
    return poll(&f, (unsigned long) 0, usec / 1000);
}

#else


#include <signal.h>
#include <sys/time.h>

static void
stopme(sig)
int sig;
{
    signal(SIGALRM, SIG_DFL);
}

static void
usleep(usecs)
unsigned int    usecs;
{
    void            stopme();
    struct itimerval ntval,
                    otval;

    ntval.it_interval.tv_sec = 0;
    ntval.it_interval.tv_usec = 0;
    ntval.it_value.tv_sec = 0;
    ntval.it_value.tv_usec = usecs;
    signal(SIGALRM, stopme);
    setitimer(ITIMER_REAL, &ntval, &otval);
    pause();
}

#endif						/* SYSV else not */
#endif						/* VMS else not */
#endif						/* NEEDUSLEEP */

static void
fatalError(message, arg1)
char           *message,
               *arg1;
{
    fprintf(stderr, message, arg1);
    fprintf(stderr, "\n");
    exit(1);
}

#define xlate(_n) ((_n) >= '0' && (_n) <= '9' ? (_n) - '0' :		      \
		   ((_n) >= 'A' && (_n) <= 'D' ? (_n) - 'A' + 10 :	      \
		    ((_n) >= 'a' && (_n) <= 'd' ? (_n) - 'a' + 10 :	      \
		     ((_n) == '*' ? (_n) - '*' + 10 + 4 :		      \
		      ((_n) == '#' ? (_n) - '#' + 10 + 4 + 1:		      \
		       ((_n) == ',' ? (_n) - ',' + 10 + 4 + 1 + 1:	      \
			-1))))))

static struct
{
    int             tone1,
                    tone2;
}               map[] =
{
    6, 10,					/* zero */
    0, 8,					/* one */
    0, 10,					/* two */
    0, 12,					/* three */
    2, 8,					/* four */
    2, 10,					/* five */
    2, 12,					/* six */
    4, 8,					/* seven */
    4, 10,					/* eight */
    4, 12,					/* nine */
    0, 14,					/* A */
    2, 14,					/* B */
    4, 14,					/* C */
    6, 14,					/* D */
    6, 8,					/* star */
    6, 12,					/* pound */
};

#define TONE(_e, _v, _f)						       \
{									       \
    AuMakeElementImportWaveForm(&elements[_e], SAMPLE_RATE, WAVE_FORM,	       \
				AuUnlimitedSamples, _f, 1, tone_act);	       \
    AuMakeElementMultiplyConstant(&elements[(_e) + 1], _e, _v);		       \
    (_e) += 2;								       \
}

static          AuFlowID
createDTMFflow(aud, outputDevice, volume, duration)
AuServer       *aud;
AuDeviceID      outputDevice;
int             volume,
                duration;
{
    AuFlowID        flow;
    AuElement       elements[18];
    AuElementAction dev_act[2],
                    tone_act[1];
    unsigned short  inputs[8];
    int             elNum = 0,
                    i;
    AuFixedPoint    v = AuFixedPointFromFraction(1, 200 / volume);

    flow = AuCreateFlow(aud, NULL);

    AuMakeNoopAction(&tone_act[0], AuStateAny, AuStateAny, AuReasonAny);

    TONE(elNum, v, 697);
    TONE(elNum, v, 770);
    TONE(elNum, v, 852);
    TONE(elNum, v, 941);
    TONE(elNum, v, 1209);
    TONE(elNum, v, 1336);
    TONE(elNum, v, 1477);
    TONE(elNum, v, 1633);

    for (i = 0; i < 8; i++)
	inputs[i] = i * 2 + 1;

    AuMakeElementSum(&elements[elNum], 8, inputs);

    AuMakeChangeStateAction(&dev_act[0], AuStateStop, AuStateAny, AuReasonAny,
			    flow, AuElementAll, AuStateStop);
    AuMakeSendNotifyAction(&dev_act[1], AuStateStop, AuStateAny, AuReasonAny);

    AuMakeElementExportDevice(&elements[elNum + 1], elNum,
			      outputDevice, SAMPLE_RATE,
			      SAMPLE_RATE / 1000 * duration, 2, dev_act);

    /* set up the flow */
    AuSetElements(aud, flow, AuTrue, 18, elements, NULL);

    return flow;
}

static void
dial(aud, flow, dialString, pause, spacing)
AuServer       *aud;
AuFlowID        flow;
char           *dialString;
int             pause,
                spacing;
{
    AuEvent         event;
    AuElementState  states[3];
    int             digit,
                    pauseTime;
    AuBool          done;

    while ((digit = *dialString++))
	if ((digit = xlate(digit)) != -1)
	{
	    if (digit != PAUSE_DIGIT)
	    {
		/* start up the components */
		AuMakeElementState(&states[0], flow, map[digit].tone1,
				   AuStateStart);
		AuMakeElementState(&states[1], flow, map[digit].tone2,
				   AuStateStart);
		AuMakeElementState(&states[2], flow, DEVICE_ELEMENT,
				   AuStateStart);
		AuSetElementStates(aud, 3, states, NULL);

		done = AuFalse;

		while (!done)
		{
		    AuNextEvent(aud, AuTrue, &event);	/* dequeue the event */

		    done = ((event.type == AuEventTypeElementNotify) &&
			    (event.auelementnotify.kind ==
			     AuElementNotifyKindState) &&
			 (event.auelementnotify.cur_state == AuStateStop) &&
		     (event.auelementnotify.element_num == DEVICE_ELEMENT));
		}

		pauseTime = spacing;
	    }
	    else
		pauseTime = pause;

	    usleep(pauseTime * 1000);
	}
}

static void
doDial(aud, dialString, volume, pause, spacing, duration)
AuServer       *aud;
char           *dialString;
int             volume,
                pause,
                spacing,
                duration;
{
    AuDeviceID      outputDevice = AuNone;
    AuFlowID        flow;
    int             i;

    /* make sure the server supports wave form elements */
    for (i = 0; i < AuServerNumElementTypes(aud); i++)
	if (AuServerElementType(aud, i) == AuElementTypeImportWaveForm)
	    break;

    if (i == AuServerNumElementTypes(aud))
	fatalError("audio server does not support the wave form element type");

    /* make sure the server supports sine waves */
    for (i = 0; i < AuServerNumWaveForms(aud); i++)
	if (AuServerWaveForm(aud, i) == AuWaveFormSine)
	    break;

    if (i == AuServerNumWaveForms(aud))
	fatalError("audio server does not support sine waves");

    /* look for an appropriate output device */
    for (i = 0; i < AuServerNumDevices(aud); i++)
	if ((AuDeviceKind(AuServerDevice(aud, i)) ==
	     AuComponentKindPhysicalOutput) &&
	    AuDeviceNumTracks(AuServerDevice(aud, i)) == 1)
	{
	    outputDevice = AuDeviceIdentifier(AuServerDevice(aud, i));
	    break;
	}

    if (outputDevice == AuNone)
	fatalError("Couldn't find an appropriate output device");

    flow = createDTMFflow(aud, outputDevice, volume, duration);

    dial(aud, flow, dialString, pause, spacing);
}

#define NBINS 		4
#define SLICE_MSECS 	50
#define THRESHHOLD 	3

static void
recognize(p, n)
unsigned char  *p;
int             n;
{
    static int      freqs[8] = {697, 770, 852, 941, 1209, 1336, 1477, 1633},
                    sums[8][NBINS],
                    binphases[8],
                    phases[8],
                    hits[8];
    static char     tone,
                    last_tone = ' ';
    int             i,
                    j,
                    count,
                    avg_sum,
                    threshhold;
    unsigned int    c;

    if (!p)			/* initialize */
    {
	for (i = 0; i < 8; i++)
	{
	    freqs[i] *= NBINS;
	    phases[i] = -freqs[i];
	    binphases[i] = 0;
	}

	return;
    }

    do
    {
	for (i = 0; i < 8; i++)
	    for (j = 0; j < NBINS; j++)
		sums[i][j] = 0;

	count = (SAMPLE_RATE * SLICE_MSECS) / 1000;
	while (count > 0 && n--)
	{
	    c = *p++;
	    count--;
	    for (i = 0; i < 8; i++)
	    {
		while (phases[i] < 0)
		{
		    phases[i] += 2 * SAMPLE_RATE;
		    sums[i][binphases[i]] += ((int) (c & 0xff)) - 0x80;
		    binphases[i]++;
		    if (binphases[i] == NBINS)
			binphases[i] = 0;
		}
		phases[i] -= 2 * freqs[i];
	    }
	}

	avg_sum = 0;
	for (i = 0; i < 8; i++)
	    for (j = 0; j < NBINS; j++)
		avg_sum += abs(sums[i][j]);
	avg_sum /= 8 * NBINS;
	threshhold = THRESHHOLD * avg_sum;

	for (i = 0; i < 8; i++)
	{
	    hits[i] = 0;
	    for (j = 0; j < NBINS; j++)
	    {
		if (abs(sums[i][j]) > threshhold)
		    hits[i] = 1;
	    }
	}

	if (hits[0] && hits[4])
	    tone = '1';
	else if (hits[0] && hits[5])
	    tone = '2';
	else if (hits[0] && hits[6])
	    tone = '3';
	else if (hits[0] && hits[7])
	    tone = 'a';
	else if (hits[1] && hits[4])
	    tone = '4';
	else if (hits[1] && hits[5])
	    tone = '5';
	else if (hits[1] && hits[6])
	    tone = '6';
	else if (hits[1] && hits[7])
	    tone = 'b';
	else if (hits[2] && hits[4])
	    tone = '7';
	else if (hits[2] && hits[5])
	    tone = '8';
	else if (hits[2] && hits[6])
	    tone = '9';
	else if (hits[2] && hits[7])
	    tone = 'c';
	else if (hits[3] && hits[4])
	    tone = '*';
	else if (hits[3] && hits[5])
	    tone = '0';
	else if (hits[3] && hits[6])
	    tone = '#';
	else if (hits[3] && hits[7])
	    tone = 'd';
	else
	    tone = ' ';

	if (tone != ' ' && tone != last_tone)
	{
	    putchar(tone);
	    fflush(stdout);
	}

	last_tone = tone;
    } while (n > 0);
}

static void
doRecognize(aud, mic, gain, time)
AuServer       *aud;
AuBool          mic;
int             gain,
                time;
{
    AuElementAction actions[1];
    AuEvent         event;
    AuDeviceID      inputDevice = AuNone;
    AuDeviceAttributes *da;
    AuFixedPoint    oldGain;
    AuFlowID        flow;
    AuElement       elements[2];
    AuElementState  states[1];
    AuInt32         oldMode;
    int             i,
                    mask = 0;
    char           *buf;

    recognize(NULL, 0);		/* initialize */

    /* look for a one track input device */
    for (i = 0; i < AuServerNumDevices(aud); i++)
    {
	da = AuServerDevice(aud, i);

	if (AuDeviceKind(da) == AuComponentKindPhysicalInput &&
	    AuDeviceNumTracks(da) == 1)
	{
	    inputDevice = AuDeviceIdentifier(da);
	    break;
	}
    }

    if (inputDevice == AuNone)
	fatalError("Can't find an appropriate input device");

    if (!(buf = (char *) malloc(EXPORT_SIZE *
				AuSizeofFormat(AuFormatLinearUnsigned8))))
	fatalError("malloc error in doRecognize()");

    /* save old gain and line mode values */
    da = AuGetDeviceAttributes(aud, inputDevice, NULL);
    oldGain = AuDeviceGain(da);
    oldMode = AuDeviceLineMode(da);

    /* set new gain and line mode values */
    AuDeviceGain(da) = AuFixedPointFromSum(gain, 0);
    AuDeviceLineMode(da) = mic ? AuDeviceLineModeHigh : AuDeviceLineModeLow;

    if (AuDeviceChangableMask(da) & AuCompDeviceLineModeMask)
	mask |= AuCompDeviceLineModeMask;

    if (AuDeviceChangableMask(da) & AuCompDeviceGainMask)
	mask |= AuCompDeviceGainMask;

    if (mask)
	AuSetDeviceAttributes(aud, inputDevice, mask, da, NULL);

    flow = AuCreateFlow(aud, NULL);

    AuMakeSendNotifyAction(&actions[0], AuStateStop, AuStateAny, AuReasonAny);

    AuMakeElementImportDevice(&elements[0], SAMPLE_RATE, inputDevice,
			      time ? SAMPLE_RATE * time : AuUnlimitedSamples,
			      1, actions);
    AuMakeElementExportClient(&elements[1], 0, SAMPLE_RATE,
			    AuFormatLinearUnsigned8, 1, AuTrue, EXPORT_SIZE,
			      EXPORT_SIZE * HIGHWATER_MARK / 100, 0, NULL);

    /* set up the flow */
    AuSetElements(aud, flow, AuTrue, 2, elements, NULL);

    /* start up the components */
    AuMakeElementState(&states[0], flow, AuElementAll, AuStateStart);
    AuSetElementStates(aud, 1, states, NULL);

    while (1)
    {
	AuNextEvent(aud, AuTrue, &event);	/* dequeue the event */

	if (event.type == AuEventTypeElementNotify)
	    if (event.auelementnotify.kind == AuElementNotifyKindHighWater ||
		(event.auelementnotify.kind == AuElementNotifyKindState &&
		 event.auelementnotify.cur_state == AuStatePause))
	    {
		AuUint32   n;

		n = AuReadElement(aud, flow, 1, event.auelementnotify.num_bytes,
				  buf, NULL);
		recognize(buf, n);
	    }
	    else if (event.auelementnotify.kind == AuElementNotifyKindState &&
		     event.auelementnotify.cur_state == AuStateStop)
		break;
    }

    free(buf);

    /* restore gain and line mode values */
    AuDeviceGain(da) = oldGain;
    AuDeviceLineMode(da) = oldMode;

    if (mask)
	AuSetDeviceAttributes(aud, inputDevice, mask, da, NULL);
    AuFlush(aud);
    AuFreeDeviceAttributes(aud, 1, da);
}

int
main(argc, argv)
int             argc;
char          **argv;
{
    int             i,
                    time = 0,
                    gain = DEFAULT_GAIN,
                    pause = DEFAULT_PAUSE,
                    spacing = DEFAULT_SPACE,
                    duration = DEFAULT_DURATION,
                    volume = DEFAULT_VOLUME;
    char           *serverName = NULL,
                   *dialString = NULL;
    AuBool          usage = AuFalse,
                    recognize = AuFalse,
                    mic = AuFalse;
    AuServer       *aud;

    for (i = 1; i < argc && !usage; i++)
    {
	char           *arg = argv[i];

	if (arg[0] == '-')
	{
	    switch (arg[1])
	    {
		case 'a':
		    if (++i >= argc)
			usage = AuTrue;
		    else
			serverName = argv[i];
		    continue;
		case 'p':
		    if (++i >= argc)
			usage = AuTrue;
		    else
			pause = atoi(argv[i]);
		    continue;
		case 's':
		    if (++i >= argc)
			usage = AuTrue;
		    else
			spacing = atoi(argv[i]);
		    continue;
		case 'v':
		    if (++i >= argc)
			usage = AuTrue;
		    else
			volume = atoi(argv[i]);
		    continue;
		case 'd':
		    if (++i >= argc)
			usage = AuTrue;
		    else
			duration = atoi(argv[i]);
		    continue;
		case 'g':
		    if (++i >= argc)
			usage = AuTrue;
		    else
			gain = atoi(argv[i]);
		    continue;
		case 't':
		    if (++i >= argc)
			usage = AuTrue;
		    else
			time = atoi(argv[i]);
		    continue;
		case 'r':
		    recognize = AuTrue;
		    continue;
		case 'm':
		    mic = AuTrue;
		    continue;
		case 'h':
		    usage = AuTrue;
	    }
	}

	if (!dialString)
	{
	    dialString = arg;
	    continue;
	}
    }

    if (usage || (!dialString && !recognize))
	fatalError(USAGE);

    /* open the audio server */
    if (!(aud = AuOpenServer(serverName, 0, NULL, 0, NULL, NULL)))
	fatalError("Can't connect to audio server");

    if (recognize)
	doRecognize(aud, mic, gain, time);
    else
	doDial(aud, dialString, volume, pause, spacing, duration);

    return 0;
}
