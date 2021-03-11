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

/*
 *   To avoid namespace pollution when using the ULTRIX header files under the
 * DEC ANSI compiler, all user-visible header files were modifed to reference
 * ANSI-style predefined macro name rather than their traditional names
 * (__ultrix vice ultrix).  Every file which accesses a predefined macro name
 * must include this file before any other files are included or the macros
 * are tested.
 *
 *   In strict ANSI mode, the appropriate ANSI-style macros are already
 * defined and the redefinitions in this file will not be seen.  When using
 * pcc, the traditional macro names are defined and this file will define
 * ANSI-style equivalents of the traditional names.  When using the DEC C
 * compiler, both the traditional and ANSI predefined macro names are
 * available so the definitions in this file are not made visible.
 *
 */


#if !defined(__STDC__) && !defined(__DECC) && !defined(__ANSI_COMPAT)

#define __ANSI_COMPAT

#ifdef ultrix
#define	__ultrix      ultrix
#endif

#ifdef unix
#define	__unix        unix
#endif

#ifdef bsd4_2
#define	__bsd4_2      bsd4_2
#endif

#ifdef vax
#define __vax 	      vax
#endif

#ifdef VAX
#define __VAX 	      VAX
#endif

#ifdef mips
#define	__mips        mips
#endif

#ifdef host_mips
#define	__host_mips   host_mips
#endif

#ifdef MIPSEL
#define	__MIPSEL      MIPSEL
#endif

#ifdef MIPSEB
#define	__MIPSEB      MIPSEB
#endif

#ifdef SYSTEM_FIVE
#define	__SYSTEM_FIVE SYSTEM_FIVE
#endif

#ifdef POSIX
#define	__POSIX       POSIX
#endif

#ifdef GFLOAT
#define __GFLOAT	GFLOAT
#endif

#ifdef LANGUAGE_C
#define	__LANGUAGE_C  LANGUAGE_C
#endif

#ifdef vaxc
#define __vaxc	 vaxc
#define __VAXC   VAXC
#define __vax11c vax11c
#define __VAX11C VAX11C
#endif

#ifdef MOXIE
#define __MOXIE   MOXIE
#endif

#ifdef ULTRIX022
#define __ULTRIX022 ULTRIX022
#endif

#endif
