/*
 * Copyright 1993 Network Computing Devices, Inc. Copyright (C) Siemens
 * Nixdorf Informationssysteme AG 1993
 * 
 * Permission to use, copy, modify, distribute, and sell this software and its
 * documentation for any purpose is hereby granted without fee, provided that
 * the above copyright notice appear in all copies and that both that
 * copyright notice and this permission notice appear in supporting
 * documentation, and that the name Network Computing Devices, Inc.  or
 * Siemens Nixdorf Informationssysteme AG not be used in advertising or
 * publicity pertaining to distribution of this software without specific,
 * written prior permission.
 * 
 * THIS SOFTWARE IS PROVIDED `AS-IS'.  NETWORK COMPUTING DEVICES, INC. AND
 * SIEMENS NIXDORF INFORMATIONSSYSTEME AG DISCLAIMS ALL WARRANTIES WITH
 * REGARD TO THIS SOFTWARE, INCLUDING WITHOUT LIMITATION ALL IMPLIED
 * WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE, OR
 * NONINFRINGEMENT.  IN NO EVENT SHALL NETWORK COMPUTING DEVICES, INC. NOR
 * SIEMENS NIXDORF INFORMATIONSSYSTEME AG BE LIABLE FOR ANY DAMAGES
 * WHATSOEVER, INCLUDING SPECIAL, INCIDENTAL OR CONSEQUENTIAL DAMAGES,
 * INCLUDING LOSS OF USE, DATA, OR PROFITS, EVEN IF ADVISED OF THE
 * POSSIBILITY THEREOF, AND REGARDLESS OF WHETHER IN AN ACTION IN CONTRACT,
 * TORT OR NEGLIGENCE, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
 * PERFORMANCE OF THIS SOFTWARE.
 * 
 * $NCDId: @(#)auvoxsvr4.h,v 1.1 1994/04/27 23:48:50 greg Exp $
 * 
 */

#ifndef _AUVOXSVR4_H_
#define _AUVOXSVR4_H_

#define	VENDOR_STRING		"AUVOXSVR4"
#define	VENDOR_RELEASE		1

/*
 * NOTE: The native format endianess should match that of the machine
 * running the audio server.
 */
#define auNativeFormat		AuFormatLinearSigned16LSB
#define auNativeBytesPerSample	2

#include <signal.h>

typedef int AuBlock;
#define	AuUnBlockAudio(_x)						      \
do									      \
{									      \
    if ((int) (_x) != (int) SIG_HOLD)					      \
	(void) sigset(SIGALRM, (void (*)(int))(_x));			      \
} while(0)

#define	AuBlockAudio()		(int) sigset(SIGALRM, SIG_HOLD)
#define AuProtectedMalloc(_s)	xalloc(_s)
#define AuProtectedFree(_p)	free(_p)

#endif /* !_AUVOXSVR4_H_ */
