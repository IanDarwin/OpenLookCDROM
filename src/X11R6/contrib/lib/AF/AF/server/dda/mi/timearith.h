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
/* $header$ */

#ifndef	TIMEARITH_H
#define	TIMEARITH_H


#ifdef	__alpha
#include <c_asm.h>
#define	MB()	asm("mb")
#else
#define	MB()	
#endif

/* definitions regarding Time arithmetic */

/* x - y */
#define DIFF(x, y) ((int) (((int) (x)) - ((int) (y))))

/* x is earlier than y */
#define BEFORE(x, y) (DIFF(x, y) < 0)
/* x is later than y */
#define AFTER(x, y) (DIFF(x, y) > 0)
/* x is equal to or after a and before b */
#define BETWEEN(a, x, b) (AFTER((b), (x)) && (!BEFORE((x), (a))))

/* if x is BEFORE time, x = time */
#define KEEPUP(x, time) if (BEFORE((x), (time))) (x) = (time)


#endif
