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
/* $Header: /crl/audio/AF/server/include/RCS/misc.h,v 1.14 1994/02/22 23:08:01 tml Exp $ */

#ifndef MISC_H
#define MISC_H 1

#include "audio.h"
#include "Amd.h"
/*
 *  Audio server internal definitions 
 *
 */

extern unsigned long globalSerialNumber;
extern unsigned long serverGeneration;

#ifndef NULL
#if __STDC__ && !NOSTDHDRS
#include <stddef.h>
#else
#define NULL            0
#endif
#endif

#define MAXDEVICES	12
#define MAXCLIENTS	128

typedef unsigned char *pointer;
typedef int ABool;
typedef unsigned long ATOM;

typedef ABool (*BoolProc)();
typedef int (*IntProc)();
typedef void (*VoidProc)();
typedef ATime (*TimeProc)();

#ifndef TRUE
#define TRUE 1
#define FALSE 0
#endif

#include "os.h" 	/* for ALLOCATE_LOCAL and DEALLOCATE_LOCAL */

#define NullBox ((BoxPtr)0)
#define MILLI_PER_MIN (1000 * 60)
#define MILLI_PER_SECOND (1000)

/* byte swap a CARD32 literal */
#define lswapl(x) ((((x) & 0xff) << 24) |\
		   (((x) & 0xff00) << 8) |\
		   (((x) & 0xff0000) >> 8) |\
		   (((x) >> 24) & 0xff))

/* byte swap a CARD16 literal */
#define lswaps(x) ((((x) & 0xff) << 8) | (((x) >> 8) & 0xff))

#define min(a, b) (((a) < (b)) ? (a) : (b))
#define max(a, b) (((a) > (b)) ? (a) : (b))
#define limit(low, x, high) \
 ((((x) < low)) ? (low) : (((x) < (high)) ? (x) : (high)))

#define abs(a) ((a) > 0 ? (a) : -(a))
#define fabs(a) ((a) > 0.0 ? (a) : -(a))	/* floating absolute value */
#define sign(x) ((x) < 0 ? -1 : ((x) > 0 ? 1 : 0))
/*
 * return the least significant bit in x which is set
 *
 * This works on 1's complement and 2's complement machines.
 * If you care about the extra instruction on 2's complement
 * machines, change to ((x) & (-(x)))
 */
#define lowbit(x) ((x) & (~(x) + 1))

#define MAXSHORT 32767
#define MINSHORT -MAXSHORT 


/* some macros to help swap requests, replies, and events */

#define LengthRestB(stuff) \
    (((unsigned long)stuff->length << 2) - sizeof(*stuff))

#define LengthRestS(stuff) \
    (((unsigned long)stuff->length << 1) - (sizeof(*stuff) >> 1))

#define LengthRestL(stuff) \
    ((unsigned long)stuff->length - (sizeof(*stuff) >> 2))

#define SwapRestS(stuff) \
    SwapShorts((CARD16 *)(stuff + 1), LengthRestS(stuff))

#define SwapRestL(stuff) \
    SwapLongs((CARD32 *)(stuff + 1), LengthRestL(stuff))

/* byte swap a CARD32 */
#define swapl(x, n) n = ((char *) (x))[0];\
		 ((char *) (x))[0] = ((char *) (x))[3];\
		 ((char *) (x))[3] = n;\
		 n = ((char *) (x))[1];\
		 ((char *) (x))[1] = ((char *) (x))[2];\
		 ((char *) (x))[2] = n;

/* byte swap a CARD16 */
#define swaps(x, n) n = ((char *) (x))[0];\
		 ((char *) (x))[0] = ((char *) (x))[1];\
		 ((char *) (x))[1] = n

/* copy long from src to dst byteswapping on the way */
#define cpswapl(src, dst) \
                 ((char *)&(dst))[0] = ((char *) &(src))[3];\
                 ((char *)&(dst))[1] = ((char *) &(src))[2];\
                 ((char *)&(dst))[2] = ((char *) &(src))[1];\
                 ((char *)&(dst))[3] = ((char *) &(src))[0];

/* copy short from src to dst byteswapping on the way */
#define cpswaps(src, dst)\
		 ((char *) &(dst))[0] = ((char *) &(src))[1];\
		 ((char *) &(dst))[1] = ((char *) &(src))[0];

extern void SwapLongs (CARD32 *list, unsigned long count);
extern void SwapShorts (CARD16 *list, unsigned long count);

#endif
