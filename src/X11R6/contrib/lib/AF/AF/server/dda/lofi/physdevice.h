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
#include <io/tc/lofi.h>
#include <io/tc/lofi_reg.h>

#include "dsp.h"

#define	MAXPHYSDEVICES	3
#define	DEFAULT_AUDIO_DEVICE	"/dev/lofi"

#ifdef	__alpha
#include <c_asm.h>
#define	MB()	asm("mb")
#else
#define	MB()	
#endif

/* teleport mask - only low 2 bits of a 16 bit field are relevant */
/* until we hear otherwise, the ring detect bit is bit 4 */
#define TELERINGMASK 4
#define TELELOOPMASK 0
#define TELEMASKI (TELERINGMASK | TELELOOPMASK)
/* Teleport hookswitch control bits */
#define TELELEFTHOOK 2
#define TELERIGHTHOOK 1
#define TELEHOOK DSP_LCTL
/* Teleport line and local control */
#define TELELINEMASK 2
#define TELELOCALMASK 1
#define TELEMASKO 3

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
/* for Teleport support, LCS 10/6/93 */
/* These field are valid in the prim aDev. All bits are low true */
    AudioDevicePtr	chADev[2];	/* pointer to left &right devices */
    int			teleport;       /* this is a teleport device */
    int			offHook;        /* bit 0 is LEFT, bit 1 is RIGHT */
    int			localAndLine;   /* bit 0 is LINE, bit 1 is LOCAL */
    int			leftInput;      /* bit 0 is RING, bit 1 is LOOP */
    int			rightInput;     /* bit 0 is RING */
    int                 ringTimerActive[2];  /* we're checking a ring */
    ATime               ringETime[2];  /* time of ring onset */

} HiFiPrivate;

/*
  Hifi device types.  Used in calls to hifiInit()
  These are also used as array indices, so LEFT and RIGHT have to
  be 0 and 1.
*/
#define HIFI_STEREO	2		/* hifi stereo channel */
#define HIFI_LEFT	0		/* hifi left channel */
#define HIFI_RIGHT	1		/* hifi right channel */

/*
 * Codec channel private data
 */
typedef struct {
    DSPTime	*devTimePtr;	/* User space direct to device time.	*/

    volatile CARD32   *playBuf;
    volatile CARD32   *recBuf;

    int	playGain;	/* Gain in tx path dB.	    */
    int	recGain;	/* Gain in rx path dB.	    */

    /* Codec Registers info. */
    int		psflag;		/* CODEC_PRIMARY or CODEC_SECONDARY */

    float   ga;	    /* pre-amp gain.  0,6,12,18,24 dB boost.	*/
    float   x;	    /* Input filter.  Gain through filter.	*/
    float   gx;	    /* Input gain, 0 to 12 dB.			*/
    float   stg;	    /* Mix input to output, -infinity to 0 dB.	*/
    float   gr;	    /* Output gain, -70 to 0 dB.		*/
    float   ger;	    /* Post mix output gain, -70 to 18 dB.	*/
    float   r;	    /* Output filter.  Gain through filter.	*/
    float   atg[2];	    /* Tone generator amplitude.		*/
    int  	ftg[2];	    /* Tone generator frequencies.		*/
    int  	outSel;  /* Speaker, Ear, or both.			*/
    int  	inSel;   /* A or B.					*/
    ABool    Mute;	    /* Mute input flag.				*/
} CodecPrivate;


/*
 * Lofi physical device data
 */
typedef struct {
    struct lofi_info 	*lofi;
    int			fd;	

    /* ADevice state. */
    StateType		state;

    /* Events */
    ATime		lastEventTime;

    /* Telephone Line Interface */
    ATime 	TimeLastPinged;	/* Client must keep alive the hookswitch. */

    volatile CARD32 *mPtr;	/* Convenient handle to user space map of mem.*/

} lofiPhysDevice;


#endif






