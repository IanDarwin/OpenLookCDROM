/* @(#)$Header: /home/wesw/Audio/crl/audio/AF/extensions/include/RCS/aftimestr.h,v 1.1 1993/09/28 23:02:01 wesw Exp $ */
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

#include "aftime.h"

#define AFTIMENAME "AFTime"

typedef struct {
    CARD8  reqType;		/* always AFTimeReqCode */
    CARD8  aftimeReqType;	/* always A_GetAFTime */
    CARD16 length B16;
} aGetAFTimeReq;
#define sz_aGetAFTimeReq 4

typedef struct {
    CARD8   type;		/* always A_Reply */
    CARD8   unused;		/* not used */
    CARD16  sequenceNumber B16;
    CARD32  length B32;		/* always 0 */
    CARD32  time B32;
    CARD32  pad1 B32;
    CARD32  pad2 B32;
    CARD32  pad3 B32;
    CARD32  pad4 B32;
    CARD32  pad5 B32;
} aGetAFTimeReply;

