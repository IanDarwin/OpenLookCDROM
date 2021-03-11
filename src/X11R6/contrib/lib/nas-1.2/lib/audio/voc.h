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
 * $NCDId: @(#)voc.h,v 1.7 1994/02/12 02:27:29 greg Exp $
 */

#ifndef _VOC_H_
#define _VOC_H_

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

#ifndef _VocConst

#if __STDC__ || defined(__cplusplus) || defined(c_plusplus) || (FUNCPROTO&4)
#define _VocConst const
#else
#define _VocConst				/**/
#endif

#endif						/* _VocConst */

#define VOC_TERMINATOR		0
#define VOC_DATA		1
#define VOC_CONTINUE		2
#define VOC_SILENCE		3
#define VOC_MARKER		4
#define VOC_TEXT		5
#define VOC_REPEAT		6
#define VOC_REPEAT_END		7
#define VOC_EXTENDED		8

#define VOC_ID			"Creative Voice File\032"
#define VOC_ID_SIZE		20
#define VOC_DATA_OFFSET		26
#define	VOC_VERSION		0x10a
#define	VOC_VERSION_CHK		((~VOC_VERSION + 0x1234) & 0xffff)

typedef struct
{
    FILE           *fp;
    char           *comment;
    AuUint32        sampleRate,
                    dataOffset,
                    dataSize;
    unsigned int    compression,
                    tracks;

    /* private data */
    unsigned int    writing;
}               VocInfo;

/*****************************************************************************
 *			       PUBLIC INTERFACES			     *
 *****************************************************************************/

extern VocInfo *
VocOpenFileForReading(
#if NeedFunctionPrototypes
		      _VocConst char *		/* file name */
#endif
);

extern VocInfo *
VocOpenFileForWriting(
#if NeedFunctionPrototypes
		      _VocConst char *,		/* file name */
		      VocInfo *			/* info */
#endif
);

extern int
VocCloseFile(
#if NeedFunctionPrototypes
	     VocInfo *				/* info */
#endif
);

extern int
VocReadFile(
#if NeedFunctionPrototypes
	    char *,				/* buffer */
	    int,				/* num bytes */
	    VocInfo *				/* info */
#endif
);

extern int
VocWriteFile(
#if NeedFunctionPrototypes
	     char *,				/* buffer */
	     int,				/* num bytes */
	     VocInfo *				/* info */
#endif
);

extern int
VocRewindFile(
#if NeedFunctionPrototypes
	      VocInfo *				/* info */
#endif
);

#endif						/* _VOC_H_ */
