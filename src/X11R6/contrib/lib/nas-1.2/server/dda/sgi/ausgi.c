/*
 * Copyright 1993 Network Computing Devices, Inc. Copyright (C) Siemens
 * Nixdorf Informationssysteme AG 1993
 * 
 * Permission to use, copy, modify, distribute, and sell this software and its
 * documentation for any purpose is hereby granted without fee, provided that
 * the above copyright notice appear in all copies and that both that
 * copyright notice and this permission notice appear in supporting
 * documentation, and that the name Network Computing Devices, Inc.  or
 * Siemens Nixdorf Informationssysteme AG not be used in advertising or
 * publicity pertaining to distribution of this software without specific,
 * written prior permission.
 * 
 * THIS SOFTWARE IS PROVIDED `AS-IS'.  NETWORK COMPUTING DEVICES, INC. AND
 * SIEMENS NIXDORF INFORMATIONSSYSTEME AG DISCLAIMS ALL WARRANTIES WITH
 * REGARD TO THIS SOFTWARE, INCLUDING WITHOUT LIMITATION ALL IMPLIED
 * WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE, OR
 * NONINFRINGEMENT.  IN NO EVENT SHALL NETWORK COMPUTING DEVICES, INC. NOR
 * SIEMENS NIXDORF INFORMATIONSSYSTEME AG BE LIABLE FOR ANY DAMAGES
 * WHATSOEVER, INCLUDING SPECIAL, INCIDENTAL OR CONSEQUENTIAL DAMAGES,
 * INCLUDING LOSS OF USE, DATA, OR PROFITS, EVEN IF ADVISED OF THE
 * POSSIBILITY THEREOF, AND REGARDLESS OF WHETHER IN AN ACTION IN CONTRACT,
 * TORT OR NEGLIGENCE, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
 * PERFORMANCE OF THIS SOFTWARE.
 * 
 * $NCDId: @(#)ausgi.c,v 1.4 1994/04/27 21:20:32 greg Exp $
 * 
 * Copyright (C) Siemens Nixdorf Informationssysteme AG 1993 All rights reserved
 */

#ifdef DEBUG
#include <stdio.h>
#define PRMSG(x,a,b)    fprintf(stderr, x,a,b); fflush(stdout)
#else
#define PRMSG(x,a,b)
#endif

#define _BSD_SIGNALS

#include "dixstruct.h"				/* for RESTYPE */
#include "os.h"					/* for xalloc/xfree and NULL */
#include <fcntl.h>
#include <stropts.h>
#include <sys/time.h>
#include <sys/param.h>

/* SGI's AudioLibrary interface (avoid name conflict with NetAudio's audio.h */
#include </usr/include/audio.h>

#include <audio/audio.h>
#include <audio/Aproto.h>
#include "au.h"

static ALconfig in_conf,
                out_mono_conf,
                out_stereo_conf;
static ALport   in_port,
                out_mono_port,
                out_stereo_port;
static AuUint8 *auOutputMono,
               *auOutputStereo,
               *auInputMono;
static AuBool   processFlowEnabled;
static AuUint32 *monoSamples,
               *stereoSamples;

extern AuInt32  auMinibufSamples;

#define	SERVER_CLIENT		0
#define MINIBUF_SIZE		800

#define auMinSampleRate		AL_RATE_8000
#define auMaxSampleRate		AL_RATE_48000
#define auDefaultInputGain	AuFixedPointFromSum(50, 0)
#define auDefaultOutputGain	AuFixedPointFromSum(50, 0)
#define minALqueueSize		1024

#define auPhysicalOutputChangableMask AuCompDeviceGainMask

#define auPhysicalOutputValueMask					       \
    (AuCompCommonAllMasks |						       \
     AuCompDeviceMinSampleRateMask |					       \
     AuCompDeviceMaxSampleRateMask |					       \
     AuCompDeviceMaxSampleRateMask |					       \
     AuCompDeviceGainMask |						       \
     AuCompDeviceLocationMask |						       \
     AuCompDeviceChildrenMask)

#define auPhysicalInputChangableMask AuCompDeviceGainMask

#define auPhysicalInputValueMask					       \
    (AuCompCommonAllMasks |						       \
     AuCompDeviceMinSampleRateMask |					       \
     AuCompDeviceMaxSampleRateMask |					       \
     AuCompDeviceLocationMask |						       \
     AuCompDeviceGainMask)						       \

static void            setPhysicalOutputGain();
static void            setPhysicalInputGainAndLineMode();

static int
createServerComponents(auServerDeviceListSize, auServerBucketListSize,
		       auServerRadioListSize, auServerMinRate,
		       auServerMaxRate)
AuUint32       *auServerDeviceListSize,
               *auServerBucketListSize,
               *auServerRadioListSize,
               *auServerMinRate,
               *auServerMaxRate;
{
    AuDeviceID      stereo,
                    mono;
    ComponentPtr    d,
                   *p;
    int             i;
    static AuBool   initialized = AuFalse;
    extern RESTYPE  auComponentType;
    extern ComponentPtr *auServerDevices,	/* array of devices */
                   *auServerBuckets,		/* array of server owned
						 * buckets */
                   *auServerRadios,		/* array of server owned
						 * radios */
                    auDevices,			/* list of all devices */
                    auBuckets,			/* list of all buckets */
                    auRadios;			/* list of all radios */
    extern AuUint32 auNumServerDevices,		/* number of devices */
                    auNumActions,		/* number of defined actions */
                    auNumServerBuckets,		/* number of server owned
						 * buckets */
                    auNumServerRadios;		/* number of server owned
						 * radios */

    PRMSG("ausgi: AuCreateServerComponents()\n", 0, 0);

    *auServerMinRate = auMinSampleRate;
    *auServerMaxRate = auMaxSampleRate;

    auNumServerDevices = *auServerDeviceListSize = *auServerBucketListSize =
	*auServerRadioListSize = 0;

    stereo = FakeClientID(SERVER_CLIENT);
    mono = FakeClientID(SERVER_CLIENT);

    AU_ALLOC_DEVICE(d, 1, 0);
    d->id = mono;
    d->changableMask = auPhysicalOutputChangableMask;
    d->valueMask = auPhysicalOutputValueMask;
    d->kind = AuComponentKindPhysicalOutput;
    d->use = AuComponentUseExportMask;
    d->access = AuAccessExportMask | AuAccessListMask;
    d->format = auNativeFormat;
    d->numTracks = 1;
    d->description.type = AuStringLatin1;
    d->description.string = "Mono Channel Output";
    d->description.len = strlen(d->description.string);
    d->minSampleRate = auMinSampleRate;
    d->maxSampleRate = auMaxSampleRate;
    d->location = AuDeviceLocationCenterMask | AuDeviceLocationInternalMask;
    d->numChildren = 0;
    d->minibuf = auOutputMono;
    d->minibufSize = auMinibufSamples * auNativeBytesPerSample *
	d->numTracks;
    d->physicalDeviceMask = PhysicalOutputMono;
    monoSamples = &d->minibufSamples;
    AU_ADD_DEVICE(d);

    AU_ALLOC_DEVICE(d, 2, 1);
    d->id = stereo;
    d->changableMask = auPhysicalOutputChangableMask;
    d->valueMask = auPhysicalOutputValueMask;
    d->kind = AuComponentKindPhysicalOutput;
    d->use = AuComponentUseExportMask;
    d->access = AuAccessExportMask | AuAccessListMask;
    d->format = auNativeFormat;
    d->numTracks = 2;
    d->description.type = AuStringLatin1;
    d->description.string = "Stereo Channel Output";
    d->description.len = strlen(d->description.string);
    d->minSampleRate = auMinSampleRate;
    d->maxSampleRate = auMaxSampleRate;
    d->location = AuDeviceLocationCenterMask | AuDeviceLocationInternalMask;
    d->numChildren = 1;
    d->children = (AuID *) ((AuUint8 *) d + PAD4(sizeof(ComponentRec)));
    d->childSwap = (char *) (d->children + d->numChildren);
    d->children[0] = mono;
    d->minibuf = auOutputStereo;
    d->minibufSize = auMinibufSamples * auNativeBytesPerSample *
	d->numTracks;
    d->physicalDeviceMask = PhysicalOutputStereo;
    stereoSamples = &d->minibufSamples;
    AU_ADD_DEVICE(d);

    AU_ALLOC_DEVICE(d, 1, 0);
    d->id = FakeClientID(SERVER_CLIENT);
    d->changableMask = auPhysicalInputChangableMask;
    d->valueMask = auPhysicalInputValueMask;
    d->kind = AuComponentKindPhysicalInput;
    d->use = AuComponentUseImportMask;
    d->access = AuAccessImportMask | AuAccessListMask;
    d->format = auNativeFormat;
    d->numTracks = 1;
    d->description.type = AuStringLatin1;
    d->description.string = "Mono Channel Input";
    d->description.len = strlen(d->description.string);
    d->minSampleRate = auMinSampleRate;
    d->maxSampleRate = auMaxSampleRate;
    d->location = AuDeviceLocationRightMask | AuDeviceLocationLeftMask |
	AuDeviceLocationExternalMask;
    d->numChildren = 0;
    d->gain = auDefaultInputGain;
    d->minibuf = auInputMono;
    d->minibufSize = auMinibufSamples * auNativeBytesPerSample *
	d->numTracks;
    d->physicalDeviceMask = PhysicalInputMono;
    AU_ADD_DEVICE(d);

    /* set the array of server devices */
    if (!(auServerDevices =
       (ComponentPtr *) aualloc(sizeof(ComponentPtr) * auNumServerDevices)))
	return AuBadAlloc;

    p = auServerDevices;
    d = auDevices;

    while (d)
    {
	*p++ = d;
	d = d->next;
    }

    if (!initialized)
    {
	initialized = AuTrue;
	setPhysicalOutputGain(auDefaultOutputGain);
	setPhysicalInputGainAndLineMode(auDefaultInputGain, 0);
    }

    return AuSuccess;
}

static AuUint32
setSampleRate(rate)
AuUint32        rate;
{
#undef NUM_PARAMS
#define NUM_PARAMS 4

    AuInt32         params[NUM_PARAMS];
    struct itimerval ntval,
                    otval;
    AuInt16         timer_ms;
    AuInt32         queue_size;

    /* should I also change the device->minibufSize ? */

    /* change timer according to new sample rate */
    timer_ms = (auMinibufSamples * 1000) / rate;

    PRMSG("ausgi: setSampleRate(%d) setitimer to %ld ms \n", rate, timer_ms);

    ntval.it_interval.tv_sec = 0;
    ntval.it_interval.tv_usec = timer_ms;
    ntval.it_value.tv_sec = 0;
    ntval.it_value.tv_usec = timer_ms;
    setitimer(ITIMER_REAL, &ntval, &otval);

    /* set hw sample rate */
    params[0] = AL_OUTPUT_RATE;
    params[1] = rate;
    params[2] = AL_INPUT_RATE;
    params[3] = rate;

    ALsetparams(AL_DEFAULT_DEVICE, params, NUM_PARAMS);

    /**
     * set queue size according to sampling rate.
     * queue_size = MAX((rate/10), minALqueueSize);
     * PRMSG ("	ALsetqueuesize() to %ld\n", queue_size, 0);
     * ALsetqueuesize (in_conf, queue_size);
     * ALsetqueuesize (out_mono_conf, queue_size);
     * ALsetqueuesize (out_stereo_conf, (2 * queue_size));
     * ALsetconfig (in_port, in_conf);
     * ALsetconfig (out_mono_port, out_mono_conf);
     * ALsetconfig (out_stereo_port, out_stereo_conf);
     *
     * Unfortunately the current ALsetconfig(3A) does not affect the queuesize
     * of a port (see BUGS of manpage for ALsetconfig).
     */

    return rate;
}

#ifdef DEBUG
static void
eventPosted()
{
    PRMSG("ausgi: eventPosted()\n", 0, 0);
}
#endif

static void
serverReset()
{
    signal(SIGALRM, SIG_IGN);
    PRMSG("ausgi: serverReset()\n", 0, 0);
}

#ifdef DEBUG
static void
errorHandler(errnum, fmt)
long            errnum;
const char     *fmt;
{
    PRMSG("ausgi: errorHandler()\n", 0, 0);
}
#endif

static void
intervalProc()
{
    extern void     AuProcessData();

    /* don't interrupt blocking ALwritesamps or ALreadsamps */
    signal(SIGALRM, SIG_IGN);

    if (processFlowEnabled)
    {
	AuProcessData();

	signal(SIGALRM, intervalProc);
    }
}

/**
  * Gains are mapped thusly:
  *
  *   Software   s	0 - 49     50 - 100
  *   Hardware   h	0 - 49     50 - 255
  *   ==>
  *		if ( [s|h] >=50 ) {
  *
  *				(100-50)   (255-50)
  *				-------- = --------
  *				( s -50)   ( h -50)
  *
  *					   205 * (s-50)
  *				    h    = ------------ + 50
  *					       50
  *
  *				           50 * (h-50)
  *				    s    = ----------- + 50
  *				              205
  *		}
  */
static void
setPhysicalOutputGain(gain)
AuFixedPoint    gain;
{
    AuInt16         g = AuFixedPointIntegralAddend(gain);
    AuInt16         outputGain;
    AuInt32         params[4];

    if (g < 50)
	outputGain = g;
    else
	/* (gain - 50) * (205 / 50) + 50 */
	outputGain = ((0x41999 * (g - 50)) >> 16) + 50;

    PRMSG("ausgi: setPhysicalOutputGain(%d) -> %d\n", g, outputGain);

    params[0] = AL_LEFT_SPEAKER_GAIN;
    params[1] = (AuInt32) outputGain;
    params[2] = AL_RIGHT_SPEAKER_GAIN;
    params[3] = (AuInt32) outputGain;
    ALsetparams(AL_DEFAULT_DEVICE, params, 4);
}

static          AuFixedPoint
getPhysicalOutputGain()
{
    AuInt16         outputGain;
    AuInt32         params[4];

    params[0] = AL_LEFT_SPEAKER_GAIN;
    params[2] = AL_RIGHT_SPEAKER_GAIN;
    ALgetparams(AL_DEFAULT_DEVICE, params, 4);

    outputGain = (params[1] + params[3]) / 2;

    if (outputGain < 50)
	return AuFixedPointFromSum(outputGain, 0);

    /* (gain - 50) * (50 / 205) + 50 */
    return (outputGain - 50) * 0x3e70 + 0x320000;
}

/**
  * Gain is mapped to attenuation thusly
  *
  *   Software    gain	0   - 50   50 - 100
  *   Hardware	  atten 255 - 50   50 - 0
  */

static void
setPhysicalInputGainAndLineMode(gain, lineMode)
AuFixedPoint    gain;
AuUint8         lineMode;
{
    AuInt16         g = AuFixedPointIntegralAddend(gain);
    AuInt16         inputAttenuation;
    AuInt32         params[4];

    PRMSG("ausgi: setPhysicalInputGainAndLineMode(%d,%d)\n", g, lineMode);

    if (g < 50)
	/* gain  * (-205 / 50) + 255 */
	inputAttenuation = 255 - ((0x41999 * g) >> 16);
    else
	/* -gain + 100 */
	inputAttenuation = 100 - g;

    PRMSG("	mapped to %d\n", inputAttenuation, 0);

    params[0] = AL_LEFT_INPUT_ATTEN;
    params[1] = (AuInt32) inputAttenuation;
    params[2] = AL_RIGHT_INPUT_ATTEN;
    params[3] = (AuInt32) inputAttenuation;
    ALsetparams(AL_DEFAULT_DEVICE, params, 4);
}

static void
enableProcessFlow()
{
    AuUint8        *p;

    PRMSG("ausgi: AuEnableProcessFlow()\n", 0, 0);

    processFlowEnabled = AuTrue;

    signal(SIGALRM, intervalProc);
}

static void
disableProcessFlow()
{
    signal(SIGALRM, SIG_IGN);

    PRMSG("ausgi: AuDisableProcessFlow()\n", 0, 0);

    processFlowEnabled = AuFalse;
}

static void
writePhysicalOutputsMono()
{
    PRMSG("wm=%ld", ALgetfilled(out_mono_port), 0);

    ALwritesamps(out_mono_port, auOutputMono, *monoSamples);
}

static void
writePhysicalOutputsStereo()
{
    PRMSG("ws=%ld", ALgetfilled(out_stereo_port), 0);

    ALwritesamps(out_stereo_port, auOutputStereo, 2 * *stereoSamples);
}

static void
writePhysicalOutputsBoth()
{
    PRMSG("wb=%ld=%ld", ALgetfilled(out_mono_port), ALgetfilled(out_stereo_port));

    ALwritesamps(out_stereo_port, auOutputStereo, 2 * *stereoSamples);
    ALwritesamps(out_mono_port, auOutputMono, *monoSamples);
}

static void
readPhysicalInputs()
{
    int             n;

    PRMSG("r %ld", ALgetfillable(in_port), 0);

    ALreadsamps(in_port, auInputMono, auMinibufSamples);
}

static void
noop()
{
}

static void
setWritePhysicalOutputFunction(flow, funct)
CompiledFlowPtr flow;
void            (**funct) ();
{
    AuUint32        mask = flow->physicalDeviceMask;

    if ((mask & (PhysicalOutputMono | PhysicalOutputStereo)) ==
	(PhysicalOutputMono | PhysicalOutputStereo))
	*funct = writePhysicalOutputsBoth;
    else if (mask & PhysicalOutputMono)
	*funct = writePhysicalOutputsMono;
    else if (mask & PhysicalOutputStereo)
	*funct = writePhysicalOutputsStereo;
    else
	*funct = noop;
}

#define	PhysicalOneTrackBufferSize					       \
    PAD4(auMinibufSamples * auNativeBytesPerSample * 1)
#define	PhysicalTwoTrackBufferSize					       \
    PAD4(auMinibufSamples * auNativeBytesPerSample * 2)

AuBool
AuInitPhysicalDevices()
{
    static AuUint8 *physicalBuffers;
    AuUint32        physicalBuffersSize;
    static AuBool   AL_initialized = AuFalse;
    struct itimerval ntval,
                    otval;
    AuInt16         timer_ms;
    AuInt32         queue_size;
    extern AuUint32 auPhysicalOutputBuffersSize;
    extern AuUint8 *auPhysicalOutputBuffers;
    extern void     AuProcessData();

#undef NUM_PARAMS
#define NUM_PARAMS 6

    long            buf[NUM_PARAMS] = {
	AL_INPUT_RATE, AL_RATE_8000,
	AL_OUTPUT_RATE, AL_RATE_8000,
	AL_INPUT_SOURCE, AL_INPUT_MIC
    };

    PRMSG("ausgi: AuInitPhysicalDevices()\n", 0, 0);

    if (physicalBuffers)
	aufree(physicalBuffers);

    auMinibufSamples = MINIBUF_SIZE;

    physicalBuffersSize =
	2 * PhysicalOneTrackBufferSize + PhysicalTwoTrackBufferSize;

    if (!(physicalBuffers = (AuUint8 *) aualloc(physicalBuffersSize)))
	return AuFalse;

    auInputMono = physicalBuffers + 0 * PhysicalOneTrackBufferSize;
    auOutputMono = physicalBuffers + 1 * PhysicalOneTrackBufferSize;
    auOutputStereo = physicalBuffers + 2 * PhysicalOneTrackBufferSize;

    auPhysicalOutputBuffers = auOutputMono;
    auPhysicalOutputBuffersSize = physicalBuffersSize -
	PhysicalOneTrackBufferSize;

    /*
     * create the input and output ports
     */
    if (!AL_initialized)
    {
	AL_initialized = AuTrue;

	/*
	 * Unfortunately the queue size of a port cannot be changed
	 * dynamically (see comment in setSampleRate()). We therefore set it
	 * to a queue size according to the max sampling rate, which is too
	 * big for slower sampling rates :-(
	 */
	queue_size = MAX((auMaxSampleRate / 10), minALqueueSize);
	PRMSG("ausgi: ALsetqueuesize to %ld\n", queue_size, 0);

	in_conf = ALnewconfig();
	ALsetwidth(in_conf, AL_SAMPLE_16);
	ALsetchannels(in_conf, AL_MONO);
	ALsetqueuesize(in_conf, queue_size);
	out_mono_conf = ALnewconfig();
	ALsetwidth(out_mono_conf, AL_SAMPLE_16);
	ALsetchannels(out_mono_conf, AL_MONO);
	ALsetqueuesize(out_mono_conf, queue_size);
	out_stereo_conf = ALnewconfig();
	ALsetwidth(out_stereo_conf, AL_SAMPLE_16);
	ALsetchannels(out_stereo_conf, AL_STEREO);
	ALsetqueuesize(out_stereo_conf, (2 * queue_size));

	in_port = ALopenport("NetAudio in", "r", in_conf);
	out_mono_port = ALopenport("NetAudio mono out", "w", out_mono_conf);
	out_stereo_port = ALopenport("NetAudio stereo out", "w", out_stereo_conf);

	if (in_port == NULL || out_mono_port == NULL || out_stereo_port == NULL)
	    return AuFalse;

	ALsetparams(AL_DEFAULT_DEVICE, buf, NUM_PARAMS);

#ifdef DEBUG
	ALseterrorhandler(errorHandler);
#endif

	/*
	 * Call AuProcessData() in signal handler often enough to drain the
	 * input devices and keep the output devices full at the current
	 * sample rate.
	 */
	timer_ms = (auMinibufSamples * 1000) / AL_RATE_8000;
	PRMSG("ausgi: setitimer to %ld ms\n", timer_ms, 0);
	ntval.it_interval.tv_sec = 0;
	ntval.it_interval.tv_usec = timer_ms;
	ntval.it_value.tv_sec = 0;
	ntval.it_value.tv_usec = timer_ms;
	signal(SIGALRM, SIG_IGN);
	setitimer(ITIMER_REAL, &ntval, &otval);
    }

    processFlowEnabled = AuFalse;

    AuRegisterCallback(AuCreateServerComponentsCB, createServerComponents);
    AuRegisterCallback(AuSetPhysicalOutputGainCB, setPhysicalOutputGain);
    AuRegisterCallback(AuGetPhysicalOutputGainCB, getPhysicalOutputGain);
    AuRegisterCallback(AuSetPhysicalInputGainAndLineModeCB,
		       setPhysicalInputGainAndLineMode);
    AuRegisterCallback(AuEnableProcessFlowCB, enableProcessFlow);
    AuRegisterCallback(AuDisableProcessFlowCB, disableProcessFlow);
    AuRegisterCallback(AuReadPhysicalInputsCB, readPhysicalInputs);
    AuRegisterCallback(AuSetWritePhysicalOutputFunctionCB,
		       setWritePhysicalOutputFunction);

    AuRegisterCallback(AuSetSampleRateCB, setSampleRate);

#ifdef DEBUG
    AuRegisterCallback(AuEventPostedCB, eventPosted);
#endif

    /* bogus resource so we can have a cleanup function at server reset */
    AddResource(FakeClientID(SERVER_CLIENT),
		CreateNewResourceType(serverReset), 0);

    return AuTrue;
}
