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

#ifndef	CONFDDA_H
#define	CONFDDA_H

#include <include/audio.h>
#include <include/audioproto.h>
#include <server/include/audiodev.h>
#include "sampletypes.h"
#include "ringbuffer.h"

#define	MAXPHYSDEVICES	3
#define DEFAULT_AUDIO_DEVICE "conference"

/* Give a hifi audio device, update current time in the audio device. */
#define HIFI_UPDATE_TIME(pDev)	(conf_update_times(pDev))

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
extern int mode_ulaw;

/*
  Hifi device types.  Used in calls to hifiInit()
*/
#define HIFI_STEREO	0		/* hifi stereo channel */
#define HIFI_LEFT	1		/* hifi left channel */
#define HIFI_RIGHT	2		/* hifi right channel */

#define CONFHISTMS 4000
#define CONFFUTMS 4000
#define CONFBUFMS (CONFHISTMS + CONFFUTMS)
#define MAXCONF 10                      /* number of simultaneous users */
/*
 * CONF physical device data
 */
typedef struct {
  int state;                           /* open or closed */
  int eventLog;                        /* log event messages? */
  AudioDevicePtr prim;                 /* primary aDev */

  ATime        time0;                  /* this is the master ATime */

/* record reference count, for stopping record dma when not needed */

  int          lastMilliClock;          /* for use when not doing DMA */
  /* for stereo devices, pointers to the DSP ring buffers */
  int	       sampleRate;		/* in samples per second */
  int          updateBandMs;            /* in ms */

  ACPtr        ac[MAXCONF];
  int          activeAC;                /* number of active ACs */
}  confPhysDevice;


/*
 * Hifi private data (for both stereo devices and mono devices)
 */
typedef struct {
/* specifies stereo, left, or right channel */
    int		channel;
    int         channel_type;  /* HIFI_STEREO, HIFI_LEFT or HIFI_RIGHT */
} HiFiPrivate;


typedef struct {
  ring_buffer   ring;     /* ringbuffer for play and record */
  int histSize;           /* amount of ring in the past */
  ATime lastPlay;         /* for matching sequential play */
  ATime lastRecord;       /* for matching sequential record */
  ATime timeEarliestValid;
  ATime timeLastValid;
/* AConvertRec playState;  /* state of playback compression */
/* AConvertRec recordState;  /* state of record compression */
} ACPrivate;

extern int nDev;
extern confPhysDevice physDevices[];

/* Procedure forward definitions */
extern ABool hifiInit(AudioDevicePtr aDev, int rate, int type);

extern void hifiUpdate(confPhysDevice *pDev);

extern int hifiMuPlay(ATime ptime, unsigned char *p, int plen, ACPtr ac);
extern int hifiMuRecord(ATime rtime, unsigned char *dp, int rlen, ACPtr ac);
extern int hifiPlay(ATime ptime, HSAMP *p, int plen, ACPtr ac);
extern int hifiRecord(ATime rtime, HSAMP *dp, int rlen, ACPtr ac);


extern int confInit(char *devName, confPhysDevice *pDev);
extern void confClose(confPhysDevice *pDev);

extern int confQueryInputGain(AudioDevicePtr aDev, int *minp, int *maxp);
extern int confQueryOutputGain(AudioDevicePtr aDev, int *minp, int *maxp);
extern int confSelectInputGain(AudioDevicePtr aDev, int gdB );
extern int confSelectOutputGain(AudioDevicePtr aDev, int gdB);

extern void confChangeInput(AudioDevicePtr aDev, int onoff,
			   int nmask, int *omaskp, int *amaskp);
extern void confChangeOutput(AudioDevicePtr aDev, int onoff,
			   int nmask, int *omaskp, int *amaskp);

extern void conf_update_times(confPhysDevice *pDev);

#endif
