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
 * STEPHEN HOCKING DISCLAIMS ALL WARRANTIES WITH
 * REGARD TO THIS SOFTWARE, INCLUDING WITHOUT LIMITATION ALL IMPLIED
 * WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE, OR
 * NONINFRINGEMENT.  IN NO EVENT SHALL NETWORK COMPUTING DEVICES, INC. NOR
 * STEPHEN HOCKING BE LIABLE FOR ANY DAMAGES
 * WHATSOEVER, INCLUDING SPECIAL, INCIDENTAL OR CONSEQUENTIAL DAMAGES,
 * INCLUDING LOSS OF USE, DATA, OR PROFITS, EVEN IF ADVISED OF THE
 * POSSIBILITY THEREOF, AND REGARDLESS OF WHETHER IN AN ACTION IN CONTRACT,
 * TORT OR NEGLIGENCE, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
 * PERFORMANCE OF THIS SOFTWARE.
 * 
 * $NCDId: @(#)auvoxsvr4.c,v 1.6 1994/05/27 17:07:11 greg Exp $
 * 
 * Copyright? What's that?
 */

#ifdef DEBUG
#include <stdio.h>
#define PRMSG(x,a,b)    fprintf(stderr, x,a,b); fflush(stdout)
#else
#define PRMSG(x,a,b)
#endif

#include "dixstruct.h"				/* for RESTYPE */
#include "os.h"					/* for xalloc/xfree and NULL */
#include <fcntl.h>
#include <sys/time.h>
#include <sys/param.h>

#include <sys/soundcard.h>

#include <audio/audio.h>
#include <audio/Aproto.h>
#include "au.h"

static int	devAudioOut = -1;
static int	devAudioIn = -1;
static int      devMixer = -1;
static AuBool	isStereoOut = FALSE;
static AuBool	isStereoIn = FALSE;
static int	auMaxSampleRateOut = 0;
static int	auMaxSampleRateIn = 0;
static int	auMinSampleRate = 4000;
static int	auWordSizeOut = 1;			/* 1 byte */
static int	auWordSizeIn = 1;			/* 1 byte */
static int      Letsplay;
static int      level[100];
static int      mixerfd;             /* The mixer device */
static int      devmask = 0;         /* Bitmask for supported mixer devices */
static int      recsrc = 0;          /* Currently selected recording sources */
static int      recmask = 0;         /* Supported recording sources */
static int      stereodevs = 0;      /* Channels supporting stereo */
static void	(*NativeToCard)();	/* Conversion to card */
static void	(*CardToNative)();		/* Conversion from card */
static time_t	last_read_time, last_write_time;
static AuUint32	oldRate = 0;
static AuUint32 *monoSamples,
               *stereoSamples;
 

static AuUint8 *auOutputMono,
               *auOutputStereo,
               *auInputMono;
static AuBool   processFlowEnabled;

extern AuInt32  auMinibufSamples;

#define	SERVER_CLIENT		0
#define MINIBUF_MAX_SIZE		256

#define auDefaultInputGain	AuFixedPointFromSum(50, 0)
#define auDefaultOutputGain	AuFixedPointFromSum(50, 0)
#define auDefaultSampleRate	8000

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

    PRMSG("auvoxware: AuCreateServerComponents()\n", 0, 0);

    *auServerMinRate = auMinSampleRate;
    *auServerMaxRate = auMaxSampleRateOut;

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
    d->maxSampleRate = auMaxSampleRateOut;
    d->location = AuDeviceLocationCenterMask | AuDeviceLocationInternalMask;
    d->gain = auDefaultOutputGain;
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
    d->maxSampleRate = auMaxSampleRateOut;
    d->location = AuDeviceLocationCenterMask | AuDeviceLocationInternalMask;
    d->gain = auDefaultOutputGain;
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
    d->maxSampleRate = auMaxSampleRateIn;
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

static int setTimer (rate)
AuInt32		rate;
{
	struct itimerval	n, o;
	AuInt32			timer_ms;

	if (rate == 0)		/* Disable timer case */
	{
            n.it_value.tv_sec = n.it_value.tv_usec = 0;
            n.it_interval.tv_sec = n.it_interval.tv_usec = 0;
            timer_ms = 0x7ffffff;
	}
	else
	{
            timer_ms = (auMinibufSamples * 750) / rate; /* 75% to allow for slop */
            n.it_interval.tv_sec = 0;
            n.it_interval.tv_usec = timer_ms;
            n.it_value.tv_sec = 0;
            n.it_value.tv_usec = (timer_ms / 2) + 1;
	}
        setitimer(ITIMER_REAL, &n, &o);
	return timer_ms;
}

static AuUint32
setSampleRate(rate)
AuUint32        rate;
{
    struct itimerval ntval,
                    otval;
    AuInt16         timer_ms;

    PRMSG("auvoxware: setSampleRate(%d), oldRate(%d)\n", rate, oldRate);

    if (oldRate != rate)
    {
        AuBlock     l;
	oldRate = rate;
        /* should I also change the device->minibufSize ? */

        PRMSG("auvoxware: setSampleRate(%d) setitimer to %ld ms \n", rate, timer_ms);

        /* set hw sample rate */
        l = AuBlockAudio();
        ioctl(devAudioOut, SNDCTL_DSP_SYNC, 1);
        ioctl(devAudioOut, SNDCTL_DSP_SPEED, &rate);
        if (devAudioIn != devAudioOut)
            ioctl(devAudioIn, SNDCTL_DSP_SPEED, &rate);
        /* change timer according to new sample rate */

        timer_ms = setTimer(rate);
        AuUnBlockAudio(l);
    }

    return oldRate;	/* cuz rate is modified by the call */
}

#ifdef DEBUG
static void
eventPosted()
{
    PRMSG("auvoxware: eventPosted()\n", 0, 0);
}
#endif

static void
serverReset()
{
    sigset(SIGALRM, SIG_IGN);
    ioctl(devAudioOut, SNDCTL_DSP_SYNC, 1);
    close(devAudioOut);
    close(devAudioIn);
    close(devMixer);
    oldRate = 0;
    setTimer(0);
    PRMSG("auvoxware: serverReset()\n", 0, 0);
}

#ifdef DEBUG
static void
errorHandler(errnum, fmt)
long            errnum;
const char     *fmt;
{
    PRMSG("auvoxware: errorHandler()\n", 0, 0);
}
#endif

static void
intervalProc()
{
    extern void     AuProcessData();

    AuProcessData();
}

/* Virtual Gains .... */

/**
  * Gains are mapped thusly:
  *
  *   Software   s	0 - 100
  *   Hardware   h	0 - 100
**/

static void
setPhysicalOutputGain(gain)
AuFixedPoint    gain;
{
    AuInt32         g = AuFixedPointIntegralAddend(gain);
    AuInt32         i, gusvolume;


    if (g > 100 )
	g = 100;
    Letsplay = g;
    g /= 2;
    gusvolume =   g;
    if (devMixer >= 0)
        i = ioctl(mixerfd, MIXER_WRITE(SOUND_MIXER_SYNTH), &gusvolume);
}

static          AuFixedPoint
getPhysicalOutputGain()
{
    AuInt16         outputGain;

    outputGain = Letsplay;

    return AuFixedPointFromSum(outputGain, 0);
}

static void
setPhysicalInputGainAndLineMode(gain, lineMode)
AuFixedPoint    gain;
AuUint8         lineMode;
{
    AuInt16         g = AuFixedPointIntegralAddend(gain);
    AuInt16         inputAttenuation;
    AuInt16         zero = 0;
    AuInt32         params[4];

    if (g < 100)
	inputAttenuation = g;
    else 
        inputAttenuation = 100;

    inputAttenuation = inputAttenuation << 8 | inputAttenuation;

    if (devMixer >= 0)
    {
        if (lineMode == AuDeviceLineModeHigh )
        {
    
            if (ioctl(mixerfd,MIXER_WRITE(SOUND_MIXER_MIC), &inputAttenuation) == -1)
            {
                PRMSG("auvoxware: mic request failed\n", 0, 0);
            }
    
            if (ioctl(mixerfd, MIXER_WRITE(SOUND_MIXER_LINE), &zero) == -1)
            {
                 PRMSG("auvoxware: line  request failed\n", 0, 0);
            }
        }
        if (lineMode == AuDeviceLineModeLow )
        {
            if (ioctl(mixerfd,MIXER_WRITE(SOUND_MIXER_LINE), &inputAttenuation) == -1)
            {
                PRMSG("auvoxware: line  request failed\n", 0, 0);
            }
    
            if (ioctl(mixerfd, MIXER_WRITE(SOUND_MIXER_MIC), &zero) == -1)
            {
                PRMSG("auvoxware: line  request failed\n", 0, 0);
            }
        }
    }
}


static void
enableProcessFlow()
{
    AuUint8        *p;

    PRMSG("auvoxware: AuEnableProcessFlow() oldRate %d\n", oldRate, 0);
    processFlowEnabled = AuTrue;
    setTimer(oldRate);
}

static void
disableProcessFlow()
{
    setTimer(0); /* was SIG_IGN */
    PRMSG("auvoxware: AuDisableProcessFlow()\n", 0, 0);

    processFlowEnabled = AuFalse;
    ioctl(devAudioOut, SNDCTL_DSP_SYNC, 1);
    ioctl(devAudioOut, SNDCTL_DSP_SPEED, &oldRate);
    if (devAudioIn != devAudioOut)
        ioctl(devAudioIn, SNDCTL_DSP_SPEED, &oldRate);
}

static void
writeOutput(p, n)
AuInt16        *p;
AuUint32 n;
{
    time_t	cur_time = time((time_t *)0);
    AuBlock	l;

    PRMSG("auvoxware: writeOutput\n", 0, 0);
    NativeToCard(p, isStereoOut + 1, n);
    l = AuBlockAudio();
    write(devAudioOut, p, n * (isStereoOut + 1) * auWordSizeOut);
    AuUnBlockAudio(l);
    last_write_time = cur_time;
}

static void
writePhysicalOutputsMono()
{
    AuInt32         i;
    AuInt16        *s,
                   *d;

    s = (AuInt16 *) auOutputStereo;
    d = (AuInt16 *) auOutputMono;

    PRMSG("auvoxware: writeOutputMono\n", 0, 0);

    if (isStereoOut)
    {
	for (i = 0; i < *monoSamples; i++, s += 2)
	    s[0] = s[1] = *d++;
        writeOutput(auOutputStereo, *monoSamples);
    }
    else
        writeOutput(auOutputMono, *monoSamples);
}

static void
writePhysicalOutputsStereo()
{
    AuInt32         i;
    AuInt16        *s,
                   *d;

    PRMSG("auvoxware: writeOutputStereo\n", 0, 0);
    s = d = (AuInt16 *) auOutputStereo;

    if (!isStereoOut)
        for (i = 0; i < *stereoSamples; i++, s += 2)
   	    *d++ = (s[0] + s[1]) >> 1;

    writeOutput(auOutputStereo, *stereoSamples);
}

static void
writePhysicalOutputsBoth()
{
    AuInt32         i;
    AuInt16        *s,
                   *m;
    AuUint32 n = aumax(*monoSamples, *stereoSamples);

    PRMSG("auvoxware: writeOutputBoth\n", 0, 0);
    s = (AuInt16 *) auOutputStereo;
    m = (AuInt16 *) auOutputMono;

    if (isStereoOut)
    {
        for (i = 0; i < n; i++, s += 2)
        {
            s[0] = (s[0] + *m) / 2;
            s[1] = (s[1] + *m) / 2;
        }
        writeOutput(auOutputStereo, n);
    }
    else
    {
        for (i = 0; i < n; i++, s += 2)
	    *m++ = (s[0] + s[1] + *m) / 3;
        writeOutput(auOutputMono, n);
    }
}

static void
readPhysicalInputs()
{
    time_t	cur_time = time((time_t *)0);
    AuBlock     l;
    PRMSG("auvoxware: readPhysicalInputs\n", 0, 0);
    l = AuBlockAudio();
    read(devAudioIn, auInputMono, auMinibufSamples * auWordSizeIn * (isStereoIn + 1));
    AuUnBlockAudio(l);
    CardToNative(auInputMono, (isStereoOut + 1), auMinibufSamples);
    last_read_time = cur_time;
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
    PAD4(MINIBUF_MAX_SIZE * auNativeBytesPerSample * 1)
#define	PhysicalTwoTrackBufferSize					       \
    PAD4(MINIBUF_MAX_SIZE * auNativeBytesPerSample * 2)

AuBool
AuInitPhysicalDevices()
{
    static AuUint8 *physicalBuffers;
    AuUint32        physicalBuffersSize;
    struct itimerval ntval,
                    otval;
    AuInt16         timer_ms;
    AuInt32         queue_size;
    int             i;
    extern AuUint32 auPhysicalOutputBuffersSize;
    extern AuUint8 *auPhysicalOutputBuffers;
    extern void     AuProcessData();

    PRMSG("auvoxware: AuInitPhysicalDevices()\n", 0, 0);

    /*
     * create the input and output ports
     */
    devAudioOut = open("/dev/dsp", O_RDWR);

    if (devAudioOut == -1)
	return AuFalse;

    devAudioIn = open("/dev/dsp1", O_RDONLY);
        if (devAudioIn == -1)
            devAudioIn = devAudioOut;

    isStereoOut = 1;

    if (ioctl (devAudioOut, SNDCTL_DSP_STEREO, &isStereoOut) == -1)
	isStereoOut = 0;

    if (ioctl (devAudioIn, SNDCTL_DSP_STEREO, &isStereoIn) == -1)
	isStereoIn = 0;

    if (ioctl (devAudioOut, SNDCTL_DSP_SAMPLESIZE, &auWordSizeOut) == -1)
	auWordSizeOut = 1;
    else
        auWordSizeOut /= 8;		/* reported in bits */

    if (ioctl (devAudioIn, SNDCTL_DSP_SAMPLESIZE, &auWordSizeIn) == -1)
	auWordSizeIn = 1;
    else
        auWordSizeIn /= 8;		/* reported in bits */

    if (auWordSizeOut > 1)
	NativeToCard = AuSigned16LSBToNative;
    else
        NativeToCard = AuNativeToUnsigned8;

    if (auWordSizeIn > 1)
        CardToNative = AuSigned16LSBToNative;
    else
        CardToNative = AuUnsigned8ToNative;

    auMaxSampleRateOut = 44100;
    if (ioctl(devAudioOut, SNDCTL_DSP_SPEED, &auMaxSampleRateOut) == -1)
    {
        auMaxSampleRateOut = 22050;
        if (ioctl(devAudioOut, SNDCTL_DSP_SPEED, &auMaxSampleRateOut) == -1)
            return AuFalse;		/* Cant cope with silly cards */
    }

    auMaxSampleRateIn = 44100;
    if (ioctl(devAudioIn, SNDCTL_DSP_SPEED, &auMaxSampleRateIn) == -1)
    {
        auMaxSampleRateIn = 22050;
        if (ioctl(devAudioIn, SNDCTL_DSP_SPEED, &auMaxSampleRateIn) == -1)
            return AuFalse;		/* Cant cope with silly cards */
    }

    if ((devMixer = open ("/dev/mixer", O_RDONLY)) == -1)
        return AuFalse;
   
    auMinibufSamples = MINIBUF_MAX_SIZE;

    queue_size = 4096;
    i = ioctl(devAudioOut, SNDCTL_DSP_GETBLKSIZE, &queue_size);

    PRMSG("auvoxware: Buffer size %d\n", queue_size, 0);

    queue_size = 4096;
    i = ioctl(devAudioIn, SNDCTL_DSP_GETBLKSIZE, &queue_size);
        
    if (ioctl(devMixer, SOUND_MIXER_READ_DEVMASK, &devmask) == -1)
    {
        close(devMixer);		/* It's not valid.... */
        devMixer = -1; 
    }
    
    if (devMixer >= 0)
    {
        if (ioctl(devMixer, SOUND_MIXER_READ_RECMASK, &recmask) == -1)
        {
            return AuFalse;
        }
        
        if (ioctl(devMixer, SOUND_MIXER_READ_RECSRC, &recsrc) == -1)
        {
            return AuFalse;
        }
        
        if (ioctl(devMixer, SOUND_MIXER_READ_STEREODEVS, &stereodevs) == -1)
        {
            return AuFalse;
        }

        /* get all sound levels */
        for(i=0; i<SOUND_MIXER_NRDEVICES; i++)
        {
            if ((1 << i) & devmask)
            {
        
                if (ioctl(devMixer, MIXER_READ(i), &level[i]) == -1)
                {
        	        return AuFalse;
                }
            }
        }

        /* set mic input to full volume */
        i = 100;
        if (ioctl(devMixer,MIXER_WRITE(SOUND_MIXER_MIC), &i) == -1)
        {
            printf ("mic request failed\n");
        }
    }


    if (physicalBuffers)
	aufree(physicalBuffers);

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

    sigset(SIGALRM, intervalProc);
    oldRate = 0;

    /*
     * Call AuProcessData() in signal handler often enough to drain the
     * input devices and keep the output devices full at the current
     * sample rate.
     */
    processFlowEnabled = AuTrue;

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
