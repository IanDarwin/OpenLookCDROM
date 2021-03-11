/* @(#)$Header: /crl/audio/AF/extensions/include/RCS/aftime.h,v 1.2 1994/06/03 18:07:22 tml Exp $ */
/***********************************************************
Copyright 1993 by Tektronix, Inc., Wilsonville, Oregon.

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

#ifndef	_AFTIME_H_
#define _AFTIME_H_

#include <AF/audioproto.h>

#define A_GetAFTime		0

#define AFTimeNumberEvents	0
#define AFTimeNumberErrors	0

#ifndef _AFTIME_SERVER_

#ifdef __cplusplus		/* for C++ V2.0 */
extern "C" {
#endif

ABool AFTimeQueryExtension(
    AFAudioConn*	/* aud */,
    int*		/* event_basep */,
    int*		/* error_basep */
);

AStatus AFEGetTime(
    AFAudioConn*	/* aud */,
    ATime*		/* AFTime */
);

#ifdef __cplusplus		/* for C++ V2.0 */
}
#endif

#endif

#endif
