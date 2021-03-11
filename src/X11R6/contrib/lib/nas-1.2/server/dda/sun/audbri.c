/**
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
 * $NCDId: @(#)audbri.c,v 1.5 1994/04/27 17:49:24 greg Exp $
 */

#include "dixstruct.h"				/* for RESTYPE */
#include "os.h"					/* for xalloc/xfree and NULL */
#include <fcntl.h>
#include <stropts.h>
#ifndef SVR4
#include <sun/audioio.h>
#else						/* SVR4 */
#include <sys/audioio.h>
#endif						/* SVR4 */
#include <audio/audio.h>
#include <audio/Aproto.h>
#include "au.h"

#if defined(SYSV) || defined(SVR4)
#define signal sigset
#endif

#ifndef AUDIO_GETDEV
AuBool
AuInitPhysicalDevices_dbri()
{
    return AuFalse;
}
#else						/* AUDIO_GETDEV */

#ifndef SVR4
typedef int     audio_device_t;
#define IS_DBRI(_t)							      \
    ((_t) == AUDIO_DEV_SPEAKERBOX || (_t) == AUDIO_DEV_CODEC)
#else						/* SVR4 */
#define IS_DBRI(_t)	(!strcmp((_t).name, "SUNW,dbri"))
#endif						/* SVR4 */

static int      devAudio,
                devAudioCtl,
                bufSize;
static AuUint8 *auOutputMono,
               *auOutputStereo,
               *auOutputLeft,
               *auOutputRight,
               *auInputStereo,
               *emptyOutput;
static AuInt16  outputGain,
                inputGain;
static AuBool   updateGains,
                updateSampleRate;
static AuUint32 leftAverage,
                rightAverage,
                sampleRate,
               *leftSamples,
               *rightSamples,
               *monoSamples,
               *stereoSamples,
                inputLineMode;

extern AuInt32  auMinibufSamples;

#define	SUN_DBRI_VENDOR		"Sun dbri"
#define	SERVER_CLIENT		0
#define MINIBUF_SAMPLES		800

#define auMinSampleRate		8000
#define auMaxSampleRate		48000

#define auPhysicalOutputChangableMask AuCompDeviceGainMask

#define auPhysicalOutputValueMask					      \
    (AuCompCommonAllMasks |						      \
     AuCompDeviceMinSampleRateMask |					      \
     AuCompDeviceMaxSampleRateMask |					      \
     AuCompDeviceGainMask |						      \
     AuCompDeviceLocationMask |						      \
     AuCompDeviceChildrenMask)

#define auPhysicalInputChangableMask					      \
    (AuCompDeviceGainMask | AuCompDeviceLineModeMask)

#define auPhysicalInputValueMask					      \
    (AuCompCommonAllMasks |						      \
     AuCompDeviceMinSampleRateMask |					      \
     AuCompDeviceMaxSampleRateMask |					      \
     AuCompDeviceLocationMask |						      \
     AuCompDeviceLineModeMask |						      \
     AuCompDeviceGainMask)						      \

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
                    mono,
                    left,
                    right;
    ComponentPtr    d,
                   *p;
    int             i;
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

    *auServerMinRate = auMinSampleRate;
    *auServerMaxRate = auMaxSampleRate;

    auNumServerDevices = *auServerDeviceListSize = *auServerBucketListSize =
	*auServerRadioListSize = 0;

    stereo = FakeClientID(SERVER_CLIENT);
    mono = FakeClientID(SERVER_CLIENT);
    left = FakeClientID(SERVER_CLIENT);
    right = FakeClientID(SERVER_CLIENT);

    AU_ALLOC_DEVICE(d, 1, 0);
    d->id = left;
    d->changableMask = auPhysicalOutputChangableMask;
    d->valueMask = auPhysicalOutputValueMask;
    d->kind = AuComponentKindPhysicalOutput;
    d->use = AuComponentUseExportMask;
    d->access = AuAccessExportMask | AuAccessListMask;
    d->format = auNativeFormat;
    d->numTracks = 1;
    d->description.type = AuStringLatin1;
    d->description.string = "Left Channel Output";
    d->description.len = strlen(d->description.string);
    d->minSampleRate = auMinSampleRate;
    d->maxSampleRate = auMaxSampleRate;
    d->location = AuDeviceLocationLeftMask;
    d->numChildren = 0;
    d->minibuf = auOutputLeft;
    d->minibufSize = auMinibufSamples * auNativeBytesPerSample *
	d->numTracks;
    d->physicalDeviceMask = PhysicalOutputLeft;
    leftSamples = &d->minibufSamples;
    AU_ADD_DEVICE(d);

    AU_ALLOC_DEVICE(d, 1, 0);
    d->id = right;
    d->changableMask = auPhysicalOutputChangableMask;
    d->valueMask = auPhysicalOutputValueMask;
    d->kind = AuComponentKindPhysicalOutput;
    d->use = AuComponentUseExportMask;
    d->access = AuAccessExportMask | AuAccessListMask;
    d->format = auNativeFormat;
    d->numTracks = 1;
    d->description.type = AuStringLatin1;
    d->description.string = "Right Channel Output";
    d->description.len = strlen(d->description.string);
    d->minSampleRate = auMinSampleRate;
    d->maxSampleRate = auMaxSampleRate;
    d->location = AuDeviceLocationRightMask;
    d->numChildren = 0;
    d->minibuf = auOutputRight;
    d->minibufSize = auMinibufSamples * auNativeBytesPerSample *
	d->numTracks;
    d->physicalDeviceMask = PhysicalOutputRight;
    rightSamples = &d->minibufSamples;
    AU_ADD_DEVICE(d);

    AU_ALLOC_DEVICE(d, 1, 2);
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
    d->location = AuDeviceLocationLeftMask | AuDeviceLocationRightMask;
    d->numChildren = 2;
    d->children = (AuID *) ((AuUint8 *) d + PAD4(sizeof(ComponentRec)));
    d->childSwap = (char *) (d->children + d->numChildren);
    d->children[0] = left;
    d->children[1] = right;
    d->minibuf = auOutputMono;
    d->minibufSize = auMinibufSamples * auNativeBytesPerSample *
	d->numTracks;
    d->physicalDeviceMask = PhysicalOutputMono;
    monoSamples = &d->minibufSamples;
    AU_ADD_DEVICE(d);

    AU_ALLOC_DEVICE(d, 2, 2);
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
    d->location = AuDeviceLocationLeftMask | AuDeviceLocationRightMask;
    d->numChildren = 2;
    d->children = (AuID *) ((AuUint8 *) d + PAD4(sizeof(ComponentRec)));
    d->childSwap = (char *) (d->children + d->numChildren);
    d->children[0] = left;
    d->children[1] = right;
    d->minibuf = auOutputStereo;
    d->minibufSize = auMinibufSamples * auNativeBytesPerSample *
	d->numTracks;
    d->physicalDeviceMask = PhysicalOutputStereo;
    stereoSamples = &d->minibufSamples;
    AU_ADD_DEVICE(d);

    AU_ALLOC_DEVICE(d, 2, 0);
    d->id = FakeClientID(SERVER_CLIENT);
    d->changableMask = auPhysicalInputChangableMask;
    d->valueMask = auPhysicalInputValueMask;
    d->kind = AuComponentKindPhysicalInput;
    d->use = AuComponentUseImportMask;
    d->access = AuAccessImportMask | AuAccessListMask;
    d->format = auNativeFormat;
    d->numTracks = 2;
    d->description.type = AuStringLatin1;
    d->description.string = "Stereo Channel Input";
    d->description.len = strlen(d->description.string);
    d->minSampleRate = auMinSampleRate;
    d->maxSampleRate = auMaxSampleRate;
    d->location = AuDeviceLocationRightMask | AuDeviceLocationLeftMask;
    d->numChildren = 0;
    d->gain = AuFixedPointFromFraction(inputGain * 100, AUDIO_MAX_GAIN);
    d->lineMode = inputLineMode == AUDIO_MICROPHONE ? AuDeviceLineModeHigh :
	AuDeviceLineModeLow;
    d->minibuf = auInputStereo;
    d->minibufSize = auMinibufSamples * auNativeBytesPerSample *
	d->numTracks;
    d->physicalDeviceMask = PhysicalInputStereo;
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

    return AuSuccess;
}

static void
serverReset()
{
    signal(SIGPOLL, SIG_IGN);
    ioctl(devAudio, AUDIO_DRAIN, 0);	       /* drain everything out */
}

static void
updateHardware()
{
    if (updateGains || updateSampleRate)
    {
	audio_info_t    info;

	ioctl(devAudioCtl, I_SETSIG, 0);       /* disable signal */

	if (updateGains)
	{
	    AUDIO_INITINFO(&info);
	    info.play.gain = outputGain;
	    info.record.gain = inputGain;
	    info.record.port = inputLineMode;
	    updateGains = AuFalse;
	    ioctl(devAudioCtl, AUDIO_SETINFO, &info);
	}

	if (updateSampleRate)
	{
	    AUDIO_INITINFO(&info);
	    info.play.sample_rate = info.record.sample_rate = sampleRate;
	    updateSampleRate = AuFalse;
	    ioctl(devAudio, AUDIO_SETINFO, &info);
	}

	ioctl(devAudioCtl, I_SETSIG, S_MSG);   /* re-enable signal */
    }
}

/**
  * Gains are mapped thusly:
  *
  *   Software   0 - 49     50 - 100
  *   Hardware   0 - 49     50 - 255
  */
static void
setPhysicalOutputGain(gain)
AuFixedPoint    gain;
{
    AuInt16         g = AuFixedPointIntegralAddend(gain);

    if (g < 50)
	outputGain = g;
    else
	/* (gain - 50) * (205 / 50) + 50 */
	outputGain = ((0x41999 * (g - 50)) >> 16) + 50;

    updateGains = AuTrue;
}

static          AuFixedPoint
getPhysicalOutputGain()
{
    if (outputGain < 50)
	return AuFixedPointFromSum(outputGain, 0);

    /* (gain - 50) * (50 / 205) + 50 */
    return (outputGain - 50) * 0x3e70 + 0x320000;
}

static void
setPhysicalInputGainAndLineMode(gain, lineMode)
AuFixedPoint    gain;
AuUint8         lineMode;
{
    AuInt16         g = AuFixedPointIntegralAddend(gain);

    if (g < 50)
	inputGain = g;
    else
	/* (gain - 50) * (205 / 50) + 50 */
	inputGain = ((0x41999 * (g - 50)) >> 16) + 50;

    inputLineMode = lineMode == AuDeviceLineModeHigh ? AUDIO_MICROPHONE :
	AUDIO_LINE_IN;

    updateGains = AuTrue;
}

static void
writeEmptyOutput()
{
    AuBlock         l = AuBlockAudio();

    write(devAudio, emptyOutput, 0);
    write(devAudio, emptyOutput, bufSize);
    AuUnBlockAudio(l);
}

static void
enableProcessFlow()
{
    ioctl(devAudio, I_FLUSH, FLUSHRW);		/* flush pending io */
    ioctl(devAudioCtl, I_SETSIG, S_MSG);       /* enable signal */
    updateHardware();
    writeEmptyOutput();
}

static void
disableProcessFlow()
{
    ioctl(devAudioCtl, I_SETSIG, 0);		/* disable signal */
    ioctl(devAudio, AUDIO_DRAIN, 0);	       /* drain everything out */
}

static void
writeOutput(p, n)
AuInt16        *p;
unsigned int    n;
{
    AuBlock         l;

    l = AuBlockAudio();
    write(devAudio, p, 0);
    write(devAudio, p, n << 2);
    AuUnBlockAudio(l);
}

static void
writeStereoOutput()
{
    writeOutput(auOutputStereo, *stereoSamples);
}

static void
writeMonoOutput()
{
    AuInt16        *m,
                   *p;
    int             i;

    m = (AuInt16 *) auOutputMono;
    p = (AuInt16 *) auOutputStereo;

    for (i = 0; i < *monoSamples; i++)
    {
	*p++ = *m;
	*p++ = *m++;
    }

    writeOutput(auOutputStereo, *monoSamples);
}

static void
writeAllOutputs()
{
    AuInt16        *l,
                   *r,
                   *m,
                   *s,
                   *p;
    int             i;
    unsigned int    n;

    l = (AuInt16 *) auOutputLeft;
    r = (AuInt16 *) auOutputRight;
    m = (AuInt16 *) auOutputMono;
    s = p = (AuInt16 *) auOutputStereo;
    n = aumax(aumax(*monoSamples, *stereoSamples),
	      aumax(*leftSamples, *rightSamples));

    for (i = 0; i < n; i++)
    {
	*p++ = ((*l++ + *m + *s++) * leftAverage) >> 16;
	*p++ = ((*r++ + *m++ + *s++) * rightAverage) >> 16;
    }

    writeOutput(auOutputStereo, n);
}

static void
readPhysicalInputs()
{
    read(devAudio, auInputStereo, bufSize);
}

static void
setWritePhysicalOutputFunction(flow, funct)
CompiledFlowPtr flow;
void            (**funct) ();
{
    AuUint32        mask = flow->physicalDeviceMask & AllPhysicalOutputs;

    if (mask)
	if (mask == PhysicalOutputMono)
	    *funct = writeMonoOutput;
	else if (mask == PhysicalOutputStereo)
	    *funct = writeStereoOutput;
	else
	{
	    int             left,
	                    right;

	    leftAverage = rightAverage = 0x10000;

	    left = mask & PhysicalOutputLeft ? 1 : 0;
	    right = mask & PhysicalOutputRight ? 1 : 0;

	    if (mask & PhysicalOutputMono)
	    {
		left++;
		right++;
	    }

	    if (mask & PhysicalOutputStereo)
	    {
		left++;
		right++;
	    }

	    if (left > 1)
		leftAverage /= left;

	    if (right > 1)
		rightAverage /= right;

	    *funct = writeAllOutputs;
	}
    else
	*funct = writeEmptyOutput;
}

static void
processAudioSignal(sig)
int             sig;
{
    updateHardware();
    AuProcessData();
}

static AuUint32
setSampleRate(rate)
AuUint32        rate;
{
    int             i;
    AuUint32        closestRate;
    static AuUint32 rates[] =
    {
	8000, 9600, 11025, 16000, 18900, 22050, 32000, 37800, 44100, 48000
    };

    closestRate = 48000;

    for (i = 0; i < sizeof(rates) / sizeof(rates[0]); i++)
	if (rates[i] >= rate &&
	    ((rates[i] - rate) < (closestRate - rate)))
	    closestRate = rates[i];

    if (closestRate != sampleRate)
    {
	sampleRate = closestRate;
	updateSampleRate = AuTrue;
    }

    return closestRate;
}

#define	PhysicalOneTrackBufferSize					      \
    PAD4(auMinibufSamples * auNativeBytesPerSample * 1)
#define	PhysicalTwoTrackBufferSize					      \
    PAD4(auMinibufSamples * auNativeBytesPerSample * 2)

AuBool
AuInitPhysicalDevices_dbri()
{
    static AuUint8 *physicalBuffers;
    AuUint32        physicalBuffersSize;
    audio_info_t    info;
    extern AuUint32 auPhysicalOutputBuffersSize;
    extern AuUint8 *auPhysicalOutputBuffers;

    if (VENDOR_STRING)
    {
	aufree(VENDOR_STRING);
	VENDOR_STRING = (char *) 0;
    }

    if (!devAudio)
    {
	audio_device_t  type;

	if ((devAudio = open("/dev/audio", O_RDWR)) == -1 ||
	    (devAudioCtl = open("/dev/audioctl", O_RDWR)) == -1 ||
	    ioctl(devAudio, AUDIO_GETDEV, &type) == -1 ||
	    !IS_DBRI(type))
	{
	    close(devAudio);
	    close(devAudioCtl);
	    devAudio = 0;
	    return AuFalse;
	}
    }

    if (!(VENDOR_STRING = (char *) aualloc(strlen(SUN_DBRI_VENDOR) + 1)))
	return AuFalse;

    strcpy(VENDOR_STRING, SUN_DBRI_VENDOR);

    if (physicalBuffers)
	aufree(physicalBuffers);

    if (emptyOutput)
	aufree(emptyOutput);

    auMinibufSamples = MINIBUF_SAMPLES;
    bufSize = MINIBUF_SAMPLES * 2 * 2;

    if (!(emptyOutput = (AuUint8 *) aualloc(bufSize)))
	return AuFalse;

    auset(emptyOutput, 0, bufSize);

    physicalBuffersSize =
	3 * PhysicalOneTrackBufferSize + 2 * PhysicalTwoTrackBufferSize;

    if (!(physicalBuffers = (AuUint8 *) aualloc(physicalBuffersSize)))
	return AuFalse;

    auInputStereo = physicalBuffers;

    auOutputMono = physicalBuffers + 2 * PhysicalOneTrackBufferSize;
    auOutputLeft = physicalBuffers + 3 * PhysicalOneTrackBufferSize;
    auOutputRight = physicalBuffers + 4 * PhysicalOneTrackBufferSize;
    auOutputStereo = physicalBuffers + 5 * PhysicalOneTrackBufferSize;

    auPhysicalOutputBuffers = auOutputMono;
    auPhysicalOutputBuffersSize = physicalBuffersSize -
	PhysicalTwoTrackBufferSize;

    signal(SIGPOLL, processAudioSignal);

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

    ioctl(devAudioCtl, AUDIO_GETINFO, &info);
    outputGain = info.play.gain;
    inputGain = info.record.gain;
    inputLineMode = info.record.port;

    AUDIO_INITINFO(&info);
    info.play.encoding = info.record.encoding = AUDIO_ENCODING_LINEAR;
    info.play.precision = info.record.precision = 16;
    info.play.channels = info.record.channels = 2;
    ioctl(devAudio, AUDIO_SETINFO, &info);

    /* bogus resource so we can have a cleanup function at server reset */
    AddResource(FakeClientID(SERVER_CLIENT),
		CreateNewResourceType(serverReset), 0);

    return AuTrue;
}
#endif						/* AUDIO_GETDEV */
