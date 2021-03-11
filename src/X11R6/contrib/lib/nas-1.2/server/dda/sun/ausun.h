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
 * THIS SOFTWARE IS PROVIDED `AS-IS'.  NETWORK COMPUTING DEVICES, INC.,
 * DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING WITHOUT
 * LIMITATION ALL IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS FOR A
 * PARTICULAR PURPOSE, OR NONINFRINGEMENT.  IN NO EVENT SHALL NETWORK
 * COMPUTING DEVICES, INC., BE LIABLE FOR ANY DAMAGES WHATSOEVER, INCLUDING
 * SPECIAL, INCIDENTAL OR CONSEQUENTIAL DAMAGES, INCLUDING LOSS OF USE, DATA,
 * OR PROFITS, EVEN IF ADVISED OF THE POSSIBILITY THEREOF, AND REGARDLESS OF
 * WHETHER IN AN ACTION IN CONTRACT, TORT OR NEGLIGENCE, ARISING OUT OF OR IN
 * CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 * 
 * $NCDId: @(#)ausun.h,v 1.4 1994/04/27 17:50:09 greg Exp $
 */

#ifndef _AUSUN_H_
#define _AUSUN_H_

#define VENDOR_STRING		ausunVendorString
#define	VENDOR_RELEASE		1

#ifndef _AUSUN_C_
extern char *VENDOR_STRING;
#endif /* !_AUSUN_C_ */

/*
 * NOTE: The native format endianess should match that of the machine
 * running the audio server.
 */
#define auNativeFormat		AuFormatLinearSigned16MSB
#define auNativeBytesPerSample	2

#include <signal.h>

typedef int AuBlock;
#if defined(SYSV) || defined(SVR4)
#define	AuUnBlockAudio(_x)						      \
do									      \
{									      \
    if ((int) (_x) != (int) SIG_HOLD)					      \
	(void) sigset(SIGPOLL, (void (*)(int))(_x));			      \
} while(0)

#define	AuBlockAudio()		(int) sigset(SIGPOLL, SIG_HOLD)
#else
#define	AuUnBlockAudio(_x)	sigsetmask(_x)
#define	AuBlockAudio()		sigblock(sigmask(SIGPOLL))
#endif
#define AuProtectedMalloc(_s)	xalloc(_s)
#define AuProtectedFree(_p)	free(_p)

#endif /* !_AUSUN_H_ */
