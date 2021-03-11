/*
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
 * $NCDId: @(#)wave.h,v 1.8 1993/08/13 20:52:08 greg Exp $
 */

/*
 * WAVE support library
 */

#ifndef _WAVE_H_
#define _WAVE_H_

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

#ifndef _WaveConst
#if __STDC__ || defined(__cplusplus) || defined(c_plusplus) || (FUNCPROTO&4)
#define _WaveConst const
#else
#define _WaveConst				/**/
#endif

#endif						/* _WaveConst */

#define RIFF_RiffID			"RIFF"
#define RIFF_WaveID			"WAVE"
#define RIFF_ListID			"LIST"
#define RIFF_ListInfoID			"INFO"

#define RIFF_InfoIcmtID			"ICMT"

#define RIFF_WaveFmtID			"fmt "
#define RIFF_WaveFmtSize		16
#define RIFF_WaveDataID			"data"

#define	RIFF_WAVE_FORMAT_PCM		0x0001	/* Microsoft PCM */
#define	RIFF_IBM_FORMAT_MULAW		0x0101	/* IBM mu-law format */
#define	RIFF_IBM_FORMAT_ALAW		0x0102	/* IBM a-law format */
#define	RIFF_IBM_FORMAT_ADPCM		0x0103	/* IBM AVC Adaptive
						 * Differential Pulse Code
						 * Modulation format */
typedef AuUint32 RIFF_FOURCC;

typedef struct
{
    RIFF_FOURCC     ckID;			/* chunk ID */
    AuInt32            ckSize;			/* chunk size, in bytes */
}               RiffChunk;

typedef struct
{
    FILE           *fp;
    char           *comment;
    short           channels,
                    bitsPerSample;
    AuInt32            sampleRate;
    AuUint32   dataOffset,
                    numSamples;

    /* private data */
    AuUint32 fileSize, dataSize, sizeOffset;
    unsigned int writing;
    short format;
}               WaveInfo;

/*****************************************************************************
 *			       PUBLIC INTERFACES			     *
 *****************************************************************************/

extern WaveInfo *
WaveOpenFileForReading(
#if NeedFunctionPrototypes
		       _WaveConst char *	/* file name */
#endif
);

extern WaveInfo *
WaveOpenFileForWriting(
#if NeedFunctionPrototypes
		       _WaveConst char *,	/* file name */
		       WaveInfo *		/* info */
#endif
);

extern int
WaveCloseFile(
#if NeedFunctionPrototypes
	      WaveInfo *			/* info */
#endif
);

extern int
WaveReadFile(
#if NeedFunctionPrototypes
	     char *,				/* buffer */
	     int,				/* num bytes */
	     WaveInfo *				/* info */
#endif
);

extern int
WaveWriteFile(
#if NeedFunctionPrototypes
	      char *,				/* buffer */
	      int,				/* num bytes */
	      WaveInfo *			/* info */
#endif
);

extern int
WaveRewindFile(
#if NeedFunctionPrototypes
	       WaveInfo *			/* info */
#endif
);
#endif						/* _WAVE_H_ */
