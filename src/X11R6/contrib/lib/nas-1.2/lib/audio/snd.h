/*
 * <audio/snd.h>
 * 
 * This file contains interfaces to the .snd files.  It is not dependent on the
 * NCD-AUDIO service and can easily be used for other purposes.
 * 
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
 * THIS SOFTWARE IS PROVIDED 'AS-IS'.  NETWORK COMPUTING DEVICES, INC.,
 * DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING WITHOUT
 * LIMITATION ALL IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS FOR A
 * PARTICULAR PURPOSE, OR NONINFRINGEMENT.  IN NO EVENT SHALL NETWORK
 * COMPUTING DEVICES, INC., BE LIABLE FOR ANY DAMAGES WHATSOEVER, INCLUDING
 * SPECIAL, INCIDENTAL OR CONSEQUENTIAL DAMAGES, INCLUDING LOSS OF USE, DATA,
 * OR PROFITS, EVEN IF ADVISED OF THE POSSIBILITY THEREOF, AND REGARDLESS OF
 * WHETHER IN AN ACTION IN CONTRACT, TORT OR NEGLIGENCE, ARISING OUT OF OR IN
 * CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 * 
 * $NCDId: @(#)snd.h,v 1.11 1993/08/13 20:51:50 greg Exp $
 */

#ifndef _SND_H_
#define _SND_H_

#include <stdio.h>
#include <audio/audio.h>	/* for AuInt32 and AuUint32 */

/*
 * If we are being used outside the NCD-AUDIO sound library, then we'll need
 * some ANSIfication definitions.
 */
#ifndef NeedFunctionPrototypes
#if defined(FUNCPROTO) || __STDC__ || defined(__cplusplus) || defined(c_plusplus)
#define NeedFunctionPrototypes 1
#else
#define NeedFunctionPrototypes 0
#endif
#endif						/* NeedFunctionPrototypes */

#ifndef _SndConst
#if __STDC__ || defined(__cplusplus) || defined(c_plusplus) || (FUNCPROTO&4)
#define _SndConst const
#else
#define _SndConst				/**/
#endif
#endif						/* _SndConst */


/*****************************************************************************
 *				  DATA TYPES				     *
 *****************************************************************************/

#define SND_MAGIC_NUM				0x2e736e64	/* .snd */
#define SND_DATA_SIZE_UNKNOWN			0xffffffff

#define SND_FORMAT_UNSPECIFIED			0
#define SND_FORMAT_ULAW_8			1
#define SND_FORMAT_LINEAR_8			2
#define SND_FORMAT_LINEAR_16			3
#define SND_FORMAT_LINEAR_24			4
#define SND_FORMAT_LINEAR_32			5
#define SND_FORMAT_FLOAT_32			6
#define SND_FORMAT_FLOAT_64			7
#define SND_FORMAT_ULAW_COMPRESSED_8		23

typedef struct _SndHeader
{
    AuUint32   magic;			/* the value 0x2e736e64
						 * (ASCII ".snd") */
    AuUint32   dataOffset;			/* the offset, in octets, to
						 * the data part. The minimum
						 * valid number is 24
						 * (decimal). */
    AuUint32   dataSize;			/* the size in octets, of the
						 * data part. If unknown, the
						 * value 0xffffffff should be
						 * used. */
    /** the data format:
     *
     * value    format
     *   1      8-bit ISDN u-law
     *   2      8-bit linear PCM [REF-PCM]
     *   3      16-bit linear PCM
     *   4      24-bit linear PCM
     *   5      32-bit linear PCM
     *   6      32-bit IEEE floating point
     *   7      64-bit IEEE floating point
     *  23      8-bit ISDN u-law compressed
     *          using the CCITT G.721 ADPCM
     *          voice data format scheme.
     */
    AuUint32   format;

    AuUint32   sampleRate;			/* the number of
						 * samples/second (e.g.,
						 * 8000) */
    AuUint32   tracks;			/* the number of interleaved
						 * tracks (e.g., 1) */
}               SndHeader;

typedef struct
{
    SndHeader       h;
    char           *comment;
    FILE           *fp;
    int             writing;
}               SndInfo;

/*****************************************************************************
 *			       PUBLIC INTERFACES			     *
 *****************************************************************************/

extern SndInfo *
SndOpenFileForReading(
#if NeedFunctionPrototypes
		      _SndConst char *		/* file name */
#endif
);

extern SndInfo *
SndOpenFileForWriting(
#if NeedFunctionPrototypes
		      _SndConst char *,		/* file name */
		      SndInfo *			/* info */
#endif
);

extern int
SndCloseFile(
#if NeedFunctionPrototypes
	     SndInfo *				/* info */
#endif
);

extern int
SndReadFile(
#if NeedFunctionPrototypes
	    char *,				/* buffer */
	    int,				/* num bytes */
	    SndInfo *				/* info */
#endif
);

extern int
SndWriteFile(
#if NeedFunctionPrototypes
	     char *,				/* buffer */
	     int,				/* num bytes */
	     SndInfo *				/* info */
#endif
);

extern int
SndRewindFile(
#if NeedFunctionPrototypes
	      SndInfo *				/* info */
#endif
);

#endif						/* _SND_H_ */
