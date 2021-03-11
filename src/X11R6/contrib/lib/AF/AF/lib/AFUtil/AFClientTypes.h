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
#ifndef AFCTYPES_H
#define AFCTYPES_H 1

#ifndef NULL
#define NULL 0L
#endif

#ifdef __alpha
typedef int            AF_INT32;
typedef unsigned int   AF_CARD32;
typedef unsigned int   AF_BITS32;
#else
typedef long           AF_INT32;
typedef unsigned long  AF_CARD32;
typedef unsigned long  AF_BITS32;
#endif

typedef short          AF_INT16;
typedef char           AF_INT8;

typedef unsigned short AF_CARD16;
typedef unsigned char  AF_CARD8;

typedef unsigned short	AF_BITS16;
typedef unsigned char	AF_BYTE;

typedef unsigned char	AF_BOOL;

#endif
