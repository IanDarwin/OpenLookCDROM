#ifndef LINT
#ifdef RCS_ID
static char *rcsid = "@(#)$Header: /crl/audio/AF/lib/AF/RCS/QueryExt.c,v 1.3 1993/11/12 16:22:14 tml Exp $";
#endif
#endif
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
/* $XConsortium: XQuExt.c,v 11.18 91/01/06 11:47:31 rws Exp $ */
/* Copyright    Massachusetts Institute of Technology    1986	*/
/*
Permission to use, copy, modify, distribute, and sell this software and its
documentation for any purpose is hereby granted without fee, provided that
the above copyright notice appear in all copies and that both that
copyright notice and this permission notice appear in supporting
documentation, and that the name of M.I.T. not be used in advertising or
publicity pertaining to distribution of the software without specific,
written prior permission.  M.I.T. makes no representations about the
suitability of this software for any purpose.  It is provided "as is"
without express or implied warranty.
*/

#define NEED_REPLIES
#include "Alibint.h"

ABool AFQueryExtension(
    register AFAudioConn *aud,
    const char *name,
    int *major_opcode,  /* RETURN */
    int *first_event,   /* RETURN */
    int *first_error)	/* RETURN */
{       
    aQueryExtensionReply rep;
    register aQueryExtensionReq *req;

    LockConnection(aud);
    GetReq(QueryExtension, req);
    req->nbytes = name ? strlen(name) : 0;
    req->length += (req->nbytes+(unsigned)3)>>2;
    _ASend(aud, (char *)name, (long)req->nbytes);
    (void) _AReply (aud, (aReply *)&rep, 0, aTrue);
    *major_opcode = rep.major_opcode;
    *first_event = rep.first_event;
    *first_error = rep.first_error;
    UnlockConnection(aud);
    SyncHandle();
    return (rep.present);
}

