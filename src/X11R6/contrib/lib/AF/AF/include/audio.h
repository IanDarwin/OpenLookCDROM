/* $Header: /crl/audio/AF/include/RCS/audio.h,v 1.14 1994/02/11 12:16:10 tml Exp $ */
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
#ifndef _AUDIO_H_
#define _AUDIO_H_

#define A_PROTOCOL	3		/* current protocol version */
#define A_PROTOCOL_REVISION 0		/* current minor version */

/* Resources */

typedef unsigned int AID;		/* was unsigned long. */

typedef AID AContext;
typedef AID ADevice;

typedef unsigned int AAtom;
typedef unsigned int ATime;
typedef unsigned int AMask;

/*
 * RESERVED RESOURCE AND CONSTANT DEFINITIONS
 */

#define ANone		0L	/* universal null resource or null atom */
#define ACurrentTime	0L	/* XXX is this right? special ATime */
#define AAnyPropertyType      0L /* special Atom, passed to GetProperty */
/*
 * ERROR CODES 
 */

#define ASuccess		   0	/* everything's okay */
#define ABadRequest	   1	/* bad request code */
#define ABadValue	   2	/* int parameter out of range */
#define ABadAtom		   3	/* parameter not an AAtom */
#define ABadMatch	   4	/* parameter mismatch */
#define ABadAccess	   5	/* depending on context */
#define ABadAlloc	   6	/* insufficient resources */
#define ABadAC		   7	/* parameter not a AC */
#define ABadIDChoice	   8	/* choice not in range or already used */
#define ABadName		   9	/* named thing doesn't exist */
#define ABadLength	  10	/* Request length incorrect */
#define ABadImplementation 11	/* server is defective */
#define ABadDevice 	  12	/* Unknown device. */

#define AFirstExtensionError	128
#define ALastExtensionError	255

/*
 * AC components: masks used in CreateAC, ChangeAC
 */
#define ACRecordGain	(1L<<0)	/* record gain */
#define ACPlayGain	(1L<<1) /* play gain */
#define ACStartTimeout	(1L<<2) /* start silence timeout, def. infinity */
#define ACEndSilence	(1L<<3) /* end silence timout, default to infinity */
#define ACPreemption	(1L<<4)	/* mix or pre-empt; default mix */
#define ACEncodingType	(1L<<5)	/* sample type */
#define ACChannels	(1L<<6)	/* number of channels */
#define ACEndian	(1L<<7)	/* endian-ness */

/* Timeout magic value */

#define InfiniteTimeout ~0L

/* ACPreemption */
#define Mix		0	/* default */
#define Preempt		1

/* channels */
#define Mono		1
#define Stereo		2
#define Quad		4

/* hook switch */
#define OnHook		0
#define OffHook		1

/* Property modes */

#define APropModeReplace         0
#define APropModePrepend         1
#define APropModeAppend          2


/* encodings, list will likely grow */
typedef enum {
    MU255 = 0,	    /* Log PCM representation, encoded in 8 bits.	*/
    ALAW = 1,	    /* Log PCM representation, encoded in 8 bits.	*/
    LIN16 = 2,	    /* Linear PCM, 16 bits,               		*/
    LIN32 = 3,	    /* Linear PCM, 32 bits,              		*/
    G721 = 4,       /* G.721, 64Kbps to/from 32Kbps.			*/
    G723 = 5,       /* G.723, 64Kbps to/from 24Kbps.			*/
    UNKNOWN_ENCODETYPE = 6,    /* who knows?                            */
    IMA = 7,        /* 4 bit/sample ADPCM                               */
    CRLADPCM2 = 8,  /* 2 bit/sample ADPCM                               */
    CRLADPCM3 = 9,  /* 3 bit/sample ADPCM                               */
    CRLADPCM4 = 10, /* 4 bit/sample ADPCM                               */
    LPC10 = 11,     /* LPC10e 2400 bps                                  */
    CELP1016 = 12,  /* 4800 bps                                         */
    IEEES = 13,     /* IEEE single precision -1.0 <= x < 1.0            */
    IEEED = 14,     /* IEEE double precision -1.0 <= x < 1.0            */
    G722_8 = 15,    /* 64 Kbps 7 KHz audio                              */
    MPEG = 16,      /* MPEG-audio                                       */
    GSM = 17,       /* 13 Kbps European digital cellular                */
    VSELP = 18,     /* 8 Kbps US digital cellular                       */
    LIN8U = 19,     /* 8 bits linear unsigned used for .voc files       */
    LIN8 = 20       /* 8 bits linear signed                             */
} AEncodeType;
/*
 * device types.
 */
#define CodecDevice 		0
#define HiFiDevice		1

/* Used in ChangeCloseDownMode */

#define ADestroyAll              0
#define ARetainPermanent         1
#define ARetainTemporary         2

/*
 * event type, list will likely grow 
 * errors are 0, replies are 1
 */
#define AError		0
#define AReply		1
#define APhoneRingEvent	2
#define APhoneDTMFEvent	3
#define APhoneLoopEvent	4
#define ADSPEvent	5
#define APhoneHookSwitchEvent 6
#define APropertyEvent  7
#define ALASTEvent	8

/*
 * Input Event Masks.  Used as event mask.
 */
#define ANoEventMask	0L
#define APhoneRingMask	(1L << 2)
#define APhoneDTMFMask	(1L << 3)
#define APhoneLoopMask	(1L << 4)
#define ADSPMask		(1L << 5)
#define APhoneHookSwitchMask	(1L << 6)
#define APropertyChangeMask	(1L << 7)

#define ANoEventMask	0L

/*
 * Macros to manipulate time.
 */
#define	TDELTA(Tnew,Told)	\
	((Tnew)-(Told))
#define	TFORWARD(Tnew,d)   ((Tnew)+(d))
#define	TBACKWARD(Tnew,d)  ((Tnew)-(d))

/* protocol families */

#define AFamilyInternet          0
#define AFamilyDECnet            1
#define AFamilyChaos             2

/* Property notification */

#define APropertyNewValue        0
#define APropertyDelete          1

/* for ChangeAccessControl */

#define AEnableAccess            1
#define ADisableAccess           0

/* for ChangeHosts */

#define AHostInsert		0
#define AHostDelete		1

/* for play and record requests */
#define ABigEndian		ATrue
#define ALittleEndian		AFalse
#define ABlock			ATrue
#define ANoBlock		AFalse

#endif

