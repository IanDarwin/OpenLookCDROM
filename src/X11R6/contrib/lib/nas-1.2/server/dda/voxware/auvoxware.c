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
 * $NCDId: @(#)auvoxware.c,v 1.2 1994/05/09 21:05:12 greg Exp $
 * 
 * Copyright (C) Siemens Nixdorf Informationssysteme AG 1993 All rights reserved
 */
/* Modified by Amancio Hasty */

#include <stdio.h>
#ifdef DEBUG
#include <stdio.h>

#else
#define PRMSG(x,a,b)
#endif

#define _BSD_SIGNALS
#include <errno.h> 
#include "dixstruct.h"				/* for RESTYPE */
#include "os.h"					/* for xalloc/xfree and NULL */
#include <fcntl.h>
#include <sys/time.h>
#include <sys/param.h>

/* SGI's AudioLibrary interface (avoid name conflict with NetAudio's audio.h */
#ifdef __FreeBSD__
#include </usr/include/machine/soundcard.h>
#else
#include </usr/include/sys/soundcard.h>
#endif

#include <audio/audio.h>
#include <audio/Aproto.h>
#include "au.h"

static int tri_state = 0;
/* linux sound driver mixer control variables */

static int Letsplay;
static int level[100];
static int mixerfd;                  /* The mixer device */
static int devmask = 0;              /* Bitmask for supported mixer devices */
static int recsrc = 0;               /* Currently selected recording sources */
static int recmask = 0;              /* Supported recording sources */
static int stereodevs = 0;           /* Channels supporting stereo */
static int audio_rate = 0;
static int we_are_16 = 0;

static char *labels[SOUND_MIXER_NRDEVICES] = SOUND_DEVICE_LABELS;

/* end of linux driver mixer control variables */

static int debugfd;
static AuUint8 *auOutputMono,
               *auOutputStereo,
               *auInputMono;
static AuUint32 *monoSamples,
               *stereoSamples;

static AuBool   processFlowEnabled;

static audio_state;

static int audiofd, inputfd;
extern AuInt32  auMinibufSamples;

#define MODE_MONO 0
#define MODE_STEREO 1
#define	SERVER_CLIENT		0
#define MINIBUF_SIZE		1024
#define AL_RATE_8000 8000
#define AL_RATE_44000 44000

#define auMinSampleRate		AL_RATE_8000
#define auMaxSampleRate		AL_RATE_44000
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

#define auPhysicalInputChangableMask                                           \
     (AuCompDeviceGainMask |                                                   \
      AuCompDeviceLineModeMask)                                                \


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
    struct itimerval ntval,
                    otval;
    AuInt16         timer_ms;
    AuInt32         queue_size;
    AuInt32         foo;
    int foo1;

    if (audio_rate == rate) return rate;
    audio_rate = rate;
    /* should I also change the device->minibufSize ? */

    /* change timer according to new sample rate */
    timer_ms = (auMinibufSamples * 1000) / rate;

    ntval.it_interval.tv_sec = 0;
    ntval.it_interval.tv_usec = timer_ms;
    ntval.it_value.tv_sec = 0;
    ntval.it_value.tv_usec = timer_ms;
    foo = setitimer(ITIMER_REAL, &ntval, &otval);
    ioctl (audiofd, SNDCTL_DSP_SYNC, NULL); 

    foo = ioctl(audiofd, SNDCTL_DSP_SPEED, &rate);

    return rate;
}


static void
serverReset()
{
    signal(SIGALRM, SIG_IGN);
}


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

    if (lineMode == AuDeviceLineModeHigh ) {

      if (ioctl(mixerfd,MIXER_WRITE(SOUND_MIXER_MIC), &inputAttenuation) == -1)
	  {
	    fprintf (stderr, "mic request failed\n");
	  }

      if (ioctl(mixerfd, MIXER_WRITE(SOUND_MIXER_LINE), &zero) == -1)
	  {
	    fprintf (stderr, "line  request failed\n");
	  }
    }
    if (lineMode == AuDeviceLineModeLow ) {
      if (ioctl(mixerfd,MIXER_WRITE(SOUND_MIXER_LINE), &inputAttenuation) == -1)
	  {
	    fprintf (stderr, "line  request failed\n");
	  }

      if (ioctl(mixerfd, MIXER_WRITE(SOUND_MIXER_MIC), &zero) == -1)
	  {
	    fprintf (stderr, "line  request failed\n");

	  }
    }
}

static void
enableProcessFlow()
{
    AuUint8        *p;

    processFlowEnabled = AuTrue;

    signal(SIGALRM, intervalProc);
}

static void
disableProcessFlow()
{
    signal(SIGALRM, SIG_IGN);

    processFlowEnabled = AuFalse;
/* if we ever get the sound driver not to click we need this call
   to have flow control: Amancio

   ioctl (audiofd, SNDCTL_DSP_SYNC, NULL);  
 */
   ioctl (audiofd, SNDCTL_DSP_SYNC, NULL);  
}

static void
writePhysicalOutputsMono()
{
    AuBlock         l;
    int dsp_stereo;
    int i;
    int trimbuffer;
    int go = 1;

    if (audio_state == 1) {
      dsp_stereo = MODE_MONO;
      ioctl (audiofd, SNDCTL_DSP_SYNC, NULL);

      ioctl(audiofd, SNDCTL_DSP_STEREO, &dsp_stereo);
      audio_state = 0;
    }

    if (we_are_16 ) {
      AuNativeToUnsigned16LSB(auOutputMono, 1, *monoSamples);    
    } else {
      AuNativeToUnsigned8(auOutputMono, 1, *monoSamples);    
    }

    write(audiofd, auOutputMono, *monoSamples);
  }

static void
writePhysicalOutputsStereo()
{    
  AuBlock         l;
  int dsp_stereo;

    if (we_are_16 ) {
      AuNativeToUnsigned16LSB(auOutputStereo, 2, *stereoSamples);    
    } else {
      AuNativeToUnsigned8(auOutputStereo, 2, *stereoSamples);    
    }

    if (audio_state == 0) {
      dsp_stereo = MODE_STEREO;
      ioctl (audiofd, SNDCTL_DSP_SYNC, NULL);
      ioctl(audiofd, SNDCTL_DSP_STEREO, &dsp_stereo);
      audio_state = 1;
    }


    write(audiofd, auOutputStereo, 2 * *stereoSamples);
}

static void
writePhysicalOutputsBoth()
{
    AuUint32 n = aumax(*monoSamples, *stereoSamples);

    if (we_are_16 ) {
      AuNativeToUnsigned16LSB(auOutputStereo, 1, n);    
    } else {
      AuNativeToUnsigned8(auOutputStereo, 1, n);    
    }

    write(audiofd, auOutputStereo, 2 * n);
}

static void
readPhysicalInputs()
{
    int             n;

    read(inputfd, auInputMono, auMinibufSamples);

    if (we_are_16) {
      AuUnsigned16LSBToNative(auInputMono, 1, auMinibufSamples); 
    } else {
      AuUnsigned8ToNative(auInputMono, 1, auMinibufSamples); 
    }
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
    AuInt32         i;
    AuInt16         timer_ms;
    AuInt32         queue_size;
    AuInt32         subdivide;
    AuInt32         samplesize;
    extern AuUint32 auPhysicalOutputBuffersSize;
    extern AuUint8 *auPhysicalOutputBuffers;
    extern void     AuProcessData();

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

/*      debug stuff to capture what netaudio is sending us ..
 *      debugfd=open("/tmp/sound.dmp", O_WRONLY); 
*/

	if ((audiofd = open("/dev/dsp",  O_WRONLY, 0)) == -1) 
	  return AuFalse;

	if ((inputfd = open("/dev/dsp1",  O_RDONLY, 0)) == -1)  {
	  /* we don't have bi-directional dma support */
	  /* so lets use the same fd for input and input */
	  inputfd = audiofd;
	}

	/*    set block size */
	samplesize = MINIBUF_SIZE;
	i = ioctl(audiofd, SNDCTL_DSP_GETBLKSIZE, &samplesize);
	
	ioctl (audiofd, SNDCTL_DSP_SYNC, NULL);
	/* to do 8 bit sampling or not to do 8 bit sampling */
	samplesize = 16;
	/* global flag to determine if we are operating in 16 mode */
	we_are_16 = 1;
	if ( ~ (ioctl(audiofd, SNDCTL_DSP_SAMPLESIZE, &samplesize) &&
	      (ioctl(inputfd, SNDCTL_DSP_SAMPLESIZE, &samplesize)))) {

	/*     okay, 16 bit is not allowed so lets do 8 bit sampling  */

	  we_are_16 = 0;
	  samplesize = 8;
	  ioctl(audiofd, SNDCTL_DSP_SAMPLESIZE, &samplesize);
	  ioctl(inputfd, SNDCTL_DSP_SAMPLESIZE, &samplesize);
	}

	ioctl (audiofd, SNDCTL_DSP_SYNC, NULL);

	if ((mixerfd = open("/dev/mixer",  O_RDONLY, 0)) == -1) 
	  return AuFalse;
        
	if (ioctl(mixerfd, SOUND_MIXER_READ_DEVMASK, &devmask) == -1)
          {
	    return AuFalse;
          }
        
	if (ioctl(mixerfd, SOUND_MIXER_READ_RECMASK, &recmask) == -1)
          {
	    return AuFalse;
          }
        
	if (ioctl(mixerfd, SOUND_MIXER_READ_RECSRC, &recsrc) == -1)
          {
	    return AuFalse;
          }
        
	if (ioctl(mixerfd, SOUND_MIXER_READ_STEREODEVS, &stereodevs) == -1)
          {
	    return AuFalse;
          }

	/* get all sound levels */
	for(i=0; i<SOUND_MIXER_NRDEVICES; i++)  {
	  if ((1 << i) & devmask) {
        
                if (ioctl(mixerfd, MIXER_READ(i), &level[i]) == -1)
                {
		  return AuFalse;
                }
	      }
	}

	/* lets assume we are in stereo */

	audio_state = 1;

	/* set mic input to full volume */
	i = 100;
	if (ioctl(mixerfd,MIXER_WRITE(SOUND_MIXER_MIC), &i) == -1)
	  {
	    fprintf (stderr, "mic request failed\n");
	  }

	/*
	 * Call AuProcessData() in signal handler often enough to drain the
	 * input devices and keep the output devices full at the current
	 * sample rate.
	 */
	
	timer_ms = (auMinibufSamples * 1000) / 8000;
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


    /* bogus resource so we can have a cleanup function at server reset */
    AddResource(FakeClientID(SERVER_CLIENT),
		CreateNewResourceType(serverReset), 0);

    return AuTrue;
}


