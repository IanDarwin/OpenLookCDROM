/* Copyright 1992 NEC Corporation, Tokyo, Japan.
 *
 * Permission to use, copy, modify, distribute and sell this software
 * and its documentation for any purpose is hereby granted without
 * fee, provided that the above copyright notice appear in all copies
 * and that both that copyright notice and this permission notice
 * appear in supporting documentation, and that the name of NEC
 * Corporation not be used in advertising or publicity pertaining to
 * distribution of the software without specific, written prior
 * permission.  NEC Corporation makes no representations about the
 * suitability of this software for any purpose.  It is provided "as
 * is" without express or implied warranty.
 *
 * NEC CORPORATION DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE,
 * INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN 
 * NO EVENT SHALL NEC CORPORATION BE LIABLE FOR ANY SPECIAL, INDIRECT OR
 * CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF 
 * USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR 
 * OTHER TORTUOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR 
 * PERFORMANCE OF THIS SOFTWARE. 
 */

/*
 * @(#) 102.1 $Id: widedef.h,v 1.9 1993/07/05 11:11:01 kon Exp $
 */

#ifndef _WIDEDEF_H_
#define _WIDEDEF_H_
     
#if __STDC__ || defined(SVR4) || defined(sun) /* This may be wrong. */
# if !defined(WCHAR16) && !defined(CANNA_WCHAR)
#  define HAVE_WCHAR_OPERATION
# endif
#endif

#ifdef HAVE_WCHAR_OPERATION
#ifndef nec_ews_svr2
/* replace widec.h instead, if SunOS 4.0 */
#include <stddef.h>
#endif
#endif /* HAVE_WCHAR_OPERATION */

#if !defined(WCHAR_T) && !defined(_WCHAR_T) && !defined(_WCHAR_T_) \
 && !defined(__WCHAR_T) && !defined(_GCC_WCHAR_T)
# ifdef WCHAR16
typedef unsigned short wchar_t;
# else
/* replace this with #include or typedef appropriate for your system */
typedef unsigned long wchar_t;
# endif
# define WCHAR_T
# define _WCHAR_T
# define _WCHAR_T_
# define __WCHAR_T
# define _GCC_WCHAR_T
#endif

#endif /* _WIDEDEF_H_ */
