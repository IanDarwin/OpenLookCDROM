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
 * $NCDId: @(#)8svx.h,v 1.1 1994/01/11 02:20:36 greg Exp $
 */

#ifndef _SVX_H_
#define _SVX_H_

#include <audio/audio.h>			/* for AuInt32 and AuUint32 */

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

#ifndef _SvxConst

#if __STDC__ || defined(__cplusplus) || defined(c_plusplus) || (FUNCPROTO&4)
#define _SvxConst const
#else
#define _SvxConst				/**/
#endif

#endif						/* _SvxConst */

#define SVX_FormID			"FORM"
#define SVX_8svxID			"8SVX"
#define SVX_NameID			"NAME"
#define SVX_VhdrID			"VHDR"
#define SVX_BodyID			"BODY"

#define SVX_SizeofVhdrChunk		20

#define	SVX_MaxVolume			65536

typedef AuUint32 SVX_ID;

typedef struct
{
    SVX_ID          ckID;			/* chunk ID */
    AuInt32         ckSize;			/* chunk data size, in bytes */
}               SvxChunk;

typedef struct
{
    FILE           *fp;
    char           *comment;
    AuInt32         sampleRate;
    AuUint32        dataOffset,
                    numSamples;

    /* private data */
    AuUint32        fileSize,
                    dataSize,
                    sizeOffset;
    unsigned int    writing;
}               SvxInfo;

/*****************************************************************************
 *			       PUBLIC INTERFACES			     *
 *****************************************************************************/

extern SvxInfo *
SvxOpenFileForReading(
#if NeedFunctionPrototypes
		      _SvxConst char *		/* file name */
#endif
);

extern SvxInfo *
SvxOpenFileForWriting(
#if NeedFunctionPrototypes
		      _SvxConst char *,		/* file name */
		      SvxInfo *			/* info */
#endif
);

extern int
SvxCloseFile(
#if NeedFunctionPrototypes
	     SvxInfo *				/* info */
#endif
);

extern int
SvxReadFile(
#if NeedFunctionPrototypes
	    char *,				/* buffer */
	    int,				/* num bytes */
	    SvxInfo *				/* info */
#endif
);

extern int
SvxWriteFile(
#if NeedFunctionPrototypes
	     char *,				/* buffer */
	     int,				/* num bytes */
	     SvxInfo *				/* info */
#endif
);

extern int
SvxRewindFile(
#if NeedFunctionPrototypes
	      SvxInfo *				/* info */
#endif
);
#endif						/* _SVX_H_ */
