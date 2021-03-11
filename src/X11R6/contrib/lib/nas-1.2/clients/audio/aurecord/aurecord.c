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
 * $NCDId: @(#)aurecord.c,v 1.3 1994/04/07 18:25:57 greg Exp $
 */

#include <stdio.h>
#include <signal.h>
#include <audio/Aos.h>			/* for string and other os stuff */
#include <audio/Afuncs.h> 		/* for bcopy et. al. */
#include <audio/audiolib.h>
#include <audio/soundlib.h>

#if defined(SYSV) || defined(SVR4)
#define signal sigset
#ifdef SVR4
#if NeedFunctionPrototypes
extern void (*signal(int, void (*)(int)))(int);
#else
extern void (*signal())();
#endif
#endif
#endif

#define DEFAULT_MODE		AuDeviceLineModeHigh
#define DEFAULT_FILE_FORMAT	SoundFileFormatSnd
#define DEFAULT_DATA_FORMAT	AuFormatULAW8
#define DEFAULT_SAMPLE_RATE	8000
#define DEFAULT_GAIN		AuFixedPointFromSum(75, 0)

#define USAGE "\
usage: aurecord [options] filename\n\
where options include:\n\n\
    -audio server       audio server\n\
    -mode mode          mic or line (default: mic)\n\
    -file format        file format (default: snd)\n\
    -data format        data format (default: AuFormatULAW8)\n\
    -rate sample rate   sampling rate (default: 8000)\n\
    -gain percent       input gain in percent (default: 75)\n\
    -time seconds       record time in seconds (default: unlimited)\n\
    -comment string     comment (default: \"\")\n\
"

static AuServer *aud;
static AuFlowID flow;

static void
usage()
{
    int             i;

    fprintf(stderr, USAGE);

    fprintf(stderr, "\nFile Formats:\n\n");

    for (i = 0; i < SoundNumFileFormats; i++)
	fprintf(stderr, "%30s - %s\n", SoundFileFormatToAbbrev(i),
		SoundFileFormatToString(i));

    if (!aud)
	exit(1);

    fprintf(stderr, "\nData Formats:\n\n");

    for (i = 0; i < AuServerNumFormats(aud); i++)
	fprintf(stderr, "%30s - %s\n", AuFormatToDefine(AuServerFormat(aud, i)),
		AuFormatToString(AuServerFormat(aud, i)));

    fprintf(stderr, "\nNote that each file format may only support selected data formats\n");

    exit(1);
}

static void
fatalError(message, arg)
char           *message,
               *arg;
{
    fprintf(stderr, message, arg);
    fprintf(stderr, "\n");
    exit(1);
}

#define OPTION(_o) (!strncmp(arg, _o, strlen(_o)))
#define GETARG(_d, _s)							      \
do									      \
{									      \
    if (--argc)								      \
    {									      \
	arg = *(++argv);						      \
	_d = _s;							      \
    }									      \
    else								      \
	usage();						      \
} while(0)

static void
#if !(defined(SYSV) || defined(SVR4))
stop()
#else /* defined(SYSV) || defined(SVR4) */
stop(sig)
int sig;
#endif /* defined(SYSV) || defined(SVR4) */
{
    AuStopFlow(aud, flow, NULL);
    AuFlush(aud);
}

static void
finished(aud, handler, ev, data)
AuServer       *aud;
AuEventHandlerRec *handler;
AuEvent        *ev;
AuPointer       data;
{
    *(AuBool *) data = AuTrue;
}

static int
convertMode(s)
char           *s;
{
    if (!strcasecmp(s, "mic"))
	return AuDeviceLineModeHigh;
    else if (!strcasecmp(s, "line"))
	return AuDeviceLineModeLow;
    else
	usage();

    return 0;
}

static int
convertDataFormat(s)
char           *s;
{
    int             f;

    f = AuDefineToFormat(s);

    if (f == -1)
	usage();

    return f;
}

static int
convertFileFormat(s)
char           *s;
{
    int             f;

    f = SoundAbbrevToFileFormat(s);

    if (f == -1)
	usage();

    return f;
}

int
main(argc, argv)
int             argc;
char          **argv;
{
    char           *arg,
                   *audioServer = NULL,
                   *filename = NULL;
    AuDeviceID      inputDeviceId = AuNone;
    AuEvent         ev;
    int             i,
                    time = 0,
                    mode = DEFAULT_MODE,
                    dataFormat = DEFAULT_DATA_FORMAT,
                    fileFormat = DEFAULT_FILE_FORMAT;
    AuUint32        rate = DEFAULT_SAMPLE_RATE,
                    numSamples;
    char           *comment = "";
    AuFixedPoint    gain = DEFAULT_GAIN;
    AuBool          done = AuFalse;

    while (--argc)
    {
	arg = *(++argv);

	if (OPTION("-a"))
	    GETARG(audioServer, arg);
	else if (OPTION("-m"))
	    GETARG(mode, convertMode(arg));
	else if (OPTION("-f"))
	    GETARG(fileFormat, convertFileFormat(arg));
	else if (OPTION("-d"))
	    GETARG(dataFormat, convertDataFormat(arg));
	else if (OPTION("-r"))
	    GETARG(rate, atoi(arg));
	else if (OPTION("-g"))
	    GETARG(gain, AuFixedPointFromSum(atoi(arg), 0));
	else if (OPTION("-t"))
	    GETARG(time, atoi(arg));
	else if (OPTION("-c"))
	    GETARG(comment, arg);
	else if (OPTION("-?"))
	    usage();
	else
	    filename = arg;
    }

    if (!(aud = AuOpenServer(audioServer, 0, NULL, 0, NULL, NULL)))
	fatalError("Can't connect to audio server");

    if (!filename)
	usage();

    if (!SoundValidDataFormat(fileFormat, dataFormat))
    {
	fprintf(stderr, "%s file format does not support %s data.\n",
		SoundFileFormatToString(fileFormat),
		AuFormatToString(dataFormat));
	exit(1);
    }

    numSamples = time ? rate * time : AuUnlimitedSamples;

    /* look for an input device */
    for (i = 0; i < AuServerNumDevices(aud); i++)
	if ((AuDeviceKind(AuServerDevice(aud, i)) ==
	     AuComponentKindPhysicalInput))
	{
	    inputDeviceId = AuDeviceIdentifier(AuServerDevice(aud, i));
	    break;
	}

    if (inputDeviceId == AuNone)
	fatalError("Audio server has no input devices");

    signal(SIGTERM, stop);

    if (signal(SIGINT, SIG_IGN) == SIG_DFL)
    	signal(SIGINT, stop);

    if (!AuSoundRecordToFileN(aud, filename, inputDeviceId, gain, numSamples,
			      finished, (AuPointer) &done, mode, fileFormat,
			      comment, rate, dataFormat, &flow,
			      NULL, NULL))
	fatalError("Can't record to file");

    while (!done)
    {
	AuNextEvent(aud, AuTrue, &ev);
	AuDispatchEvent(aud, &ev);
    }

    return 0;
}
