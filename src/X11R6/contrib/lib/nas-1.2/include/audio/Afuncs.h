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
 * $NCDId: @(#)Afuncs.h,v 1.3 1994/03/02 19:57:41 greg Exp $
 */

/* Portions derived from */
/*
 * $XConsortium: Xfuncs.h,v 1.8 91/04/17 09:27:52 rws Exp $
 * 
 * Copyright 1990 by the Massachusetts Institute of Technology
 *
 * Permission to use, copy, modify, and distribute this software and its
 * documentation for any purpose and without fee is hereby granted, provided 
 * that the above copyright notice appear in all copies and that both that 
 * copyright notice and this permission notice appear in supporting 
 * documentation, and that the name of M.I.T. not be used in advertising
 * or publicity pertaining to distribution of the software without specific, 
 * written prior permission. M.I.T. makes no representations about the 
 * suitability of this software for any purpose.  It is provided "as is"
 * without express or implied warranty.
 *
 */

#ifndef _AFUNCS_H_
#define _AFUNCS_H_

#include <audio/Aosdefs.h>

#ifdef AU_USEBFUNCS
void bcopy();
void bzero();
int bcmp();
#else
#if (__STDC__ && !defined(AU_NOT_STDC_ENV) && !defined(sun) && !defined(macII)) || defined(SVR4) || defined(hpux) || defined(_IBMR2)
#include <string.h>
#define bcopy(b1,b2,len) memmove(b2, b1, (size_t)(len))
#define bzero(b,len) memset(b, 0, (size_t)(len))
#define bcmp(b1,b2,len) memcmp(b1, b2, (size_t)(len))
#else
#ifdef sgi
#include <bstring.h>
#else
#ifdef SYSV
#include <memory.h>
#if defined(_AUBCOPYFUNC) && !defined(macII) && !defined(SYSV386)
#define bcopy _AUBCOPYFUNC
#define _AUNEEDBCOPYFUNC
#endif
void bcopy();
#define bzero(b,len) memset(b, 0, len)
#define bcmp(b1,b2,len) memcmp(b1, b2, len)
#else /* bsd */
void bcopy();
void bzero();
int bcmp();
#endif /* SYSV */
#endif /* sgi */
#endif /* __STDC__ and relatives */
#endif /* AU_USEBFUNCS */

#endif /* _AFUNCS_H_ */
