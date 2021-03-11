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

#include <server/include/audiodev.h>

#define	MAXPHYSDEVICES	1
#define	DEFAULT_AUDIO_DEVICE	"/dev/bba"

/* Might be considered physical device information, but local
 * to a single pseudo device only.
 */
typedef struct {
    volatile unsigned long   *playBuf;
    volatile unsigned long   *recBuf;

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

typedef struct {
    struct bba_info 	*bba;
    int			fd;	

    /* ADevice state. */
    StateType		state;

    /* Events */
    ATime		lastEventTime;

    /* Telephone Line Interface */
    ATime 	TimeLastPinged;	/* Client must keep alive the hookswitch. */

} maxPhysDevice;


#endif
