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
#ifndef PHYSDEVICE_H
#define PHYSDEVICE_H

#include <include/audio.h>
#include <include/audioproto.h>
#include <server/include/audiodev.h>
#include "jv.h"
#include "jv_reg.h"

#include "dsp.h"

#define	MAXPHYSDEVICES	3
#define	DEFAULT_AUDIO_DEVICE	"/dev/jva0"

#ifdef	__alpha
#include <c_asm.h>
#define	MB()	asm("mb")
#else
#define	MB()	
#endif

/*
 * Hifi private data (for both stereo devices and mono devices)
 */
typedef struct {
    DSPTime	*devTimePtr;	/* User space direct to device time.	*/

/* for stereo devices, pointers to the DSP ring buffers */
    volatile CARD32   *leftPlayBuf;
    volatile CARD32   *leftRecBuf;
    volatile CARD32   *rightPlayBuf;
    volatile CARD32   *rightRecBuf;

    int		sampleRate;		/* in samples per second */
    int		guardBandTicks;		/* size of guard band, in ticks */
    int		guardBandMs;		/* size of guard band, in ms */
    int		updateBandTicks;	/* size of update band, in ticks */
    int		updateBandMs;		/* size of udpate band, in ms */

/* for mono devices, specifies left or right channel */
    int		channel;

/* for mono and stereo devices */
    AudioDevicePtr	prim;		/* pointer to primary stereo device */
} HiFiPrivate;

/*
  Hifi device types.  Used in calls to hifiInit()
*/
#define HIFI_STEREO	0		/* hifi stereo channel */
#define HIFI_LEFT	1		/* hifi left channel */
#define HIFI_RIGHT	2		/* hifi right channel */

/*
 * Lofi physical device data
 */
typedef struct {
    JvRegs 	*jv;
    int			fd;	

    /* ADevice state. */
    StateType		state;

    /* Events */
    ATime		lastEventTime;

    /* Telephone Line Interface */
    ATime 	TimeLastPinged;	/* Client must keep alive the hookswitch. */

    volatile CARD32 *mPtr;	/* Convenient handle to user space map of mem.*/

}  jvPhysDevice;

#endif
