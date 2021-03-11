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
 * $NCDId: @(#)Afuncproto.h,v 1.3 1993/01/13 21:23:50 jim Exp $
 */

/* Portions derived from */
/* $XConsortium: Xfuncproto.h,v 1.7 91/05/13 20:49:21 rws Exp $ */
/* 
 * Copyright 1989, 1991 by the Massachusetts Institute of Technology
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

/* Definitions to make function prototypes manageable */

#ifndef _AUFUNCPROTO_H_
#define _AUFUNCPROTO_H_

#ifndef NeedFunctionPrototypes
#if defined(FUNCPROTO) || __STDC__ || defined(__cplusplus) || defined(c_plusplus)
#define NeedFunctionPrototypes 1
#else
#define NeedFunctionPrototypes 0
#endif
#endif /* NeedFunctionPrototypes */

#ifndef NeedVarargsPrototypes
#if __STDC__ || defined(__cplusplus) || defined(c_plusplus) || (FUNCPROTO&2)
#define NeedVarargsPrototypes 1
#else
#define NeedVarargsPrototypes 0
#endif
#endif /* NeedVarargsPrototypes */

#ifndef _AuConst
#if __STDC__ || defined(__cplusplus) || defined(c_plusplus) || (FUNCPROTO&4)
#define _AuConst const
#else
#define _AuConst /**/
#endif
#endif /* _AuConst */

#if NeedFunctionPrototypes

#ifndef NeedNestedPrototypes
#if __STDC__ || defined(__cplusplus) || defined(c_plusplus) || (FUNCPROTO&8)
#define NeedNestedPrototypes 1
#else
#define NeedNestedPrototypes 0
#endif
#endif /* NeedNestedPrototypes */

#ifndef NeedWidePrototypes
#ifdef NARROWPROTO
#define NeedWidePrototypes 0
#else
#define NeedWidePrototypes 1		/* default to make interropt. easier */
#endif
#endif /* NeedWidePrototypes */

#endif /* NeedFunctionPrototypes */

#ifndef _AUFUNCPROTOBEGIN
#ifdef __cplusplus			/* for C++ V2.0 */
#define _AUFUNCPROTOBEGIN extern "C" {	/* do not leave open across includes */
#define _AUFUNCPROTOEND }
#else
#define _AUFUNCPROTOBEGIN
#define _AUFUNCPROTOEND
#endif
#endif /* _AUFUNCPROTOBEGIN */

#endif /* _AUFUNCPROTO_H_ */
