/*
 * Copyright 1993 by Digital Equipment Corporation, Maynard, Massachusetts.
 * 
 * Permission to use, copy, modify, distribute, and sell this software and its 
 * documentation for any purpose is hereby granted without fee, provided that 
 * the above copyright notice appear in all copies and that both that 
 * copyright notice and this permission notice appear in supporting 
 * documentation, and that the name of Digital not be used in advertising or 
 * publicity pertaining to distribution of the software without specific, 
 * written prior permission.  Digital makes no representations about the 
 * suitability of this software for any purpose.  It is provided "as is" 
 * without express or implied warranty.
 * 
 * DIGITAL DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING 
 * ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL 
 * DIGITAL BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY 
 * DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN 
 * AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF 
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 */
/* $header$ */

#ifndef	MSBDDA_H
#define	MSBDDA_H

#include <include/audio.h>
#include <include/audioproto.h>
#include <server/include/audiodev.h>
#include "sampletypes.h"
#include "ringbuffer.h"
#include "msb.h"

#define	MAXPHYSDEVICES	3
#define	DEFAULT_AUDIO_DEVICE	"/dev/msb0"
/* Give a hifi audio device, update current time in the audio device. */
#define HIFI_UPDATE_TIME(pDev)	(msb_update_times(pDev))

/*
 * sample data types and sizes
 */

#ifndef LIMIT
#define	LIMIT(low, val, high) \
	  if ((val) < (low)) (val) = (low); \
	  else \
	  if ((val) > (high)) (val) = (high);
#endif

/* global variables */
extern int hifi_rate;
extern int hifi_rate_cr;
extern int eventLog;

/*
  Hifi device types.  Used in calls to hifiInit()
*/
#define HIFI_STEREO	0		/* hifi stereo channel */
#define HIFI_LEFT	1		/* hifi left channel */
#define HIFI_RIGHT	2		/* hifi right channel */
#define HIFI_NUM_DEV    3		/* how many logical devices? */
/*
 * MSB physical device data
 */
typedef struct {
  int           state;                 /* open or closed */
  int           fd;                    /* file descriptor */
  int           eventLog;              /* if != 0 print events */
  AudioDevicePtr aDevs[HIFI_NUM_DEV];  /* back table of aDevs */

  ring_buffer  hP;                     /* hardware play buffer */
  ring_buffer  hR;                     /* hardware record buffer */
  ring_buffer  sP;                     /* server play buffer */
  ring_buffer  sR;                     /* server record buffer */

  ATime        lastPlayOffset;         /* recent play offset */
  ATime        lastRecOffset;          /* recent record offset */
  ATime        time0;                  /* this is the master ATime */
  ATime        timeLastValid;          /* time of last valid data
					  in server play buffer */
  ATime        timeEarliestValid;      /* time of earliest valid data
					  in server record buffer */

/* record reference count, for stopping record dma when not needed */
  int          recRefCount;
  int          lastMilliClock;          /* for use when not doing DMA */
  int          recDMARunning;
  int          playDMARunning;
  /* for stereo devices, pointers to the DSP ring buffers */
  int	       sampleRate;		/* in samples per second */
  int	       updateBandMs;		/* size of udpate band, in ms */

  msb_input_t selectedInput;            /* input source */
  int gainInL[4];                       /* left input gain */
  int gainInR[4];                       /* right input gain */
  int gainOutL;                         /* left output gain */
  int gainOutR;                         /* left input gain */
  int codecType;			/* msb codec type */
}  msbPhysDevice;


/*
 * Hifi private data (for both stereo devices and mono devices)
 */
typedef struct {
/* specifies stereo, left, or right channel */
    int		channel;
    int         channel_type;  /* HIFI_STEREO, HIFI_LEFT or HIFI_RIGHT */
} HiFiPrivate;

extern int nDev;
extern msbPhysDevice physDevices[];

/* Procedure forward definitions */
extern ABool hifiInit(AudioDevicePtr aDev, int rate, int type);

extern void hifiUpdate(msbPhysDevice *pDev);
extern void hifiRecUpdate(msbPhysDevice *pDev);
extern int hifiMuPlay(ATime ptime, unsigned char *p, int plen, ACPtr ac);
extern int hifiMuRecord(ATime rtime, unsigned char *dp, int rlen, ACPtr ac);
extern int hifiPlay(ATime ptime, HSAMP *p, int plen, ACPtr ac);
extern int hifiRecord(ATime rtime, HSAMP *dp, int rlen, ACPtr ac);


extern int msbAssertRate(int rate);

extern int msbInit(char *devName, msbPhysDevice *pDev);
extern void msbClose(msbPhysDevice *pDev);

extern int msbQueryInputGain(AudioDevicePtr aDev, int *minp, int *maxp);
extern int msbQueryOutputGain(AudioDevicePtr aDev, int *minp, int *maxp);
extern int msbSelectInputGain(AudioDevicePtr aDev, int gdB );
extern int msbSelectOutputGain(AudioDevicePtr aDev, int gdB);

extern void msbChangeInput(AudioDevicePtr aDev, int onoff,
			   int nmask, int *omaskp, int *amaskp);
extern void msbChangeOutput(AudioDevicePtr aDev, int onoff,
			   int nmask, int *omaskp, int *amaskp);


extern void msb_update_times(msbPhysDevice *pDev);

extern void msb_record_start(msbPhysDevice *pDev);
extern void msb_record_stop(msbPhysDevice *pDev);
extern void msb_playback_start(msbPhysDevice *pDev);
extern void msb_playback_stop(msbPhysDevice *pDev);

#endif
