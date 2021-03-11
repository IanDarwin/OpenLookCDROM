/* @(#)$Header: /home/wesw/nwd/terminator/src/av/crl/audio/AF/lib/AFSox/RCS/amiga.h,v 1.4 1994/01/12 21:07:53 marvinw Exp $ */
/***********************************************************
$Copyright$,1994 by Tektronix, Inc., Wilsonville, Oregon.

                        All Rights Reserved

Permission to use, copy, modify, and distribute this software and its 
documentation for any purpose and without fee is hereby granted, 
provided that the above copyright notice appear in all copies and that
both that copyright notice and this permission notice appear in 
supporting documentation, and that the names of Tektronix or Tek not be
used in advertising or publicity pertaining to distribution of the
software without specific, written prior permission.  

TEKTRONIX DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING
ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL
TEKTRONIX BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR
ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,
WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION,
ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS
SOFTWARE.

******************************************************************/
#ifdef AMIGA

#include <fcntl.h>

#ifdef AMIGA_MC68881
#include <m68881.h>
#endif /* AMIGA_MC68881 */

#include "patchlvl.h"		/* yeah, I know it's not really a header...but why not? */

/* Following is a really screwy way of incorporating compile-time info into *
 * the binary as an Amiga version string.  Unfortunately, it was the only   *
 * method I could find.  --dgc, 13 Jan 93                                   */

#define AmiVerChars1	{'$', 'V', 'E', 'R', ':', ' ', 'S', 'o', 'u', 'n', 'd', ' ', 'E', 'x', 'c', 'h', 'a', 'n', 'g', 'e', ' ', 
#define AmiVerChars2	'6', '8', '0', '3', '0', 
#define AmiVerChars3	'/', 
#define AmiVerChars4	'6', '8', '8', '8', '1', 
#define AmiVerChars5	' ', 'P', 'a', 't', 'c', 'h', 'l', 'e', 'v', 'e', 'l', ' ', '0'+PATCHLEVEL, '\0'}
#ifdef AMIGA_MC68881
#ifdef AMIGA_MC68030
#define AmiVerChars	AmiVerChars1 AmiVerChars2 AmiVerChars3 AmiVerChars4 AmiVerChars5
#else
#define AmiVerChars	AmiVerChars1 AmiVerChars4 AmiVerChars5
#endif /* AMIGA_MC68030 */
#else
#ifdef AMIGA_MC68030
#define AmiVerChars	AmiVerChars1 AmiVerChars2 AmiVerChars5
#else
#define AmiVerChars	AmiVerChars1 AmiVerChars5
#endif /* AMIGA_MC68030 */
#endif /*AMIGA_MC68881*/

/* if you change these strings, be sure to change the size here! */
/* (and remember, sizeof() won't work)                           */
#define AmiVerSize 46

/* stdarg adjustments */
#ifndef va_dcl
#define va_dcl int va_alist;
#endif /* !va_dcl*/

/* BSD compat */
#include <string.h>
/* SAS/C does these; other might not */
#ifndef bcopy
#define	bcopy(from, to, len)	memmove(to, from, len)
#endif

/* SAS/C library code includes unlink().   *
 * If your compiler doesn't have unlink(), *
 * uncomment this section.                 */
/*
#ifndef unlink
#define	unlink		DeleteFile
#endif
*/

#endif /*AMIGA*/
