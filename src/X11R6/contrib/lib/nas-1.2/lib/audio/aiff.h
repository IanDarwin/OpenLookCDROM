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
 * $NCDId: @(#)aiff.h,v 1.2 1993/08/13 20:51:38 greg Exp $
 */

#ifndef _AIFF_H_
#define _AIFF_H_

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

#ifndef _AiffConst

#if __STDC__ || defined(__cplusplus) || defined(c_plusplus) || (FUNCPROTO&4)
#define _AiffConst const
#else
#define _AiffConst				/**/
#endif

#endif						/* _AiffConst */

#define AIFF_FormID			"FORM"
#define AIFF_AiffID			"AIFF"
#define AIFF_CommonID			"COMM"
#define AIFF_SoundDataID		"SSND"
#define AIFF_CommentID			"COMT"

#define AIFF_SizeofExtended		10
#define AIFF_SizeofCommentChunk		10
#define AIFF_SizeofCommonChunk		(8 + AIFF_SizeofExtended)
#define AIFF_SizeofSoundDataChunk	8

typedef AuUint32 AIFF_ID;
typedef short   AIFF_MARKER_ID;

typedef struct
{
    AIFF_ID         ckID;			/* chunk ID */
    AuInt32            ckSize;			/* chunk data size, in bytes */
}               AiffChunk;

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
    AuUint32   fileSize,
                    dataSize,
                    sizeOffset;
    unsigned int    writing;
}               AiffInfo;

/*****************************************************************************
 *			       PUBLIC INTERFACES			     *
 *****************************************************************************/

extern AiffInfo *
AiffOpenFileForReading(
#if NeedFunctionPrototypes
		       _AiffConst char *	/* file name */
#endif
);

extern AiffInfo *
AiffOpenFileForWriting(
#if NeedFunctionPrototypes
		       _AiffConst char *,	/* file name */
		       AiffInfo *		/* info */
#endif
);

extern int
AiffCloseFile(
#if NeedFunctionPrototypes
	      AiffInfo *			/* info */
#endif
);

extern int
AiffReadFile(
#if NeedFunctionPrototypes
	     char *,				/* buffer */
	     int,				/* num bytes */
	     AiffInfo *				/* info */
#endif
);

extern int
AiffWriteFile(
#if NeedFunctionPrototypes
	      char *,				/* buffer */
	      int,				/* num bytes */
	      AiffInfo *			/* info */
#endif
);

extern int
AiffRewindFile(
#if NeedFunctionPrototypes
	       AiffInfo *			/* info */
#endif
);
#endif						/* _AIFF_H_ */
