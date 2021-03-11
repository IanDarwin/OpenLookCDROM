/*
 * Copyright 1993 Network Computing Devices, Inc.
 *
 * Permission to use, copy, modify, distribute, and sell this software and
 * its documentation for any purpose is hereby granted without fee, provided
 * that the above copyright notice appear in all copies and that both that
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
 * $NCDId: @(#)fileutil.h,v 1.6 1993/08/13 20:51:48 greg Exp $
 */

#ifndef	_FILEUTIL_H_
#define	_FILEUTIL_H_

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

#endif				/* NeedFunctionPrototypes */

#ifndef _FileConst

#if __STDC__ || defined(__cplusplus) || defined(c_plusplus) || (FUNCPROTO&4)
#define _FileConst const
#else
#define _FileConst		/**/
#endif

#endif				/* _FileConst */

#define LITTLE_ENDIAN 		(*(char *) &endian)
#define BIG_ENDIAN 		(!LITTLE_ENDIAN)
static int      endian = 1;

/* byte swap a AuInt32 */
#define swapl(x, n) { n = ((char *) (x))[0];\
	     ((char *) (x))[0] = ((char *) (x))[3];\
	     ((char *) (x))[3] = n;\
	     n = ((char *) (x))[1];\
	     ((char *) (x))[1] = ((char *) (x))[2];\
	     ((char *) (x))[2] = n; }


/* byte swap a short */
#define swaps(x, n) { n = ((char *) (x))[0];\
		 ((char *) (x))[0] = ((char *) (x))[1];\
		 ((char *) (x))[1] = n; }

#ifndef mips
extern unsigned short
FileSwapS(
#if NeedFunctionPrototypes
			unsigned short
#endif
);
#endif	/* mips */

extern AuUint32
FileSwapL(
#if NeedFunctionPrototypes
			AuUint32
#endif
);

extern unsigned short
FileReadS(
#if NeedFunctionPrototypes
			FILE	*,
                        int			/* swap? */
#endif
);

extern AuUint32
FileReadL(
#if NeedFunctionPrototypes
			FILE	*,
                        int			/* swap? */
#endif
);

#ifndef mips
extern int
FileWriteS(
#if NeedFunctionPrototypes
                        unsigned short,
			FILE	*,
                        int			/* swap? */
#endif
);
#endif

extern int
FileWriteL(
#if NeedFunctionPrototypes
                        AuUint32,
			FILE	*,
                        int			/* swap? */
#endif
);

extern char	*
FileCommentFromFilename(
#if NeedFunctionPrototypes
                        _FileConst char	*		/* file name */
#endif
);

#endif				/* _FILEUTIL_H_ */
