#ifndef LINT
#ifdef RCS_ID
static char *rcsid = "@(#)$Header: /crl/audio/AF/lib/AF/RCS/ListExt.c,v 1.3 1993/11/12 16:21:18 tml Exp $";
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
/* $XConsortium: XListExt.c,v 11.10 91/01/06 11:46:50 rws Exp $ */
/* Copyright    Massachusetts Institute of Technology    1986

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

char **AFListExtensions(
	register AFAudioConn *aud,
	int *nextensions	/* RETURN */
	)
{
	aListExtensionsReply rep;
	char **list;
	char *ch;
	register unsigned i;
	register int length;
	register aReq *req;
	register long rlen;

	LockConnection(aud);
	GetEmptyReq (ListExtensions, req);

	if (! _AReply (aud, (aReply *) &rep, 0, aFalse)) {
	    UnlockConnection(aud);
	    SyncHandle();
	    return (char **) NULL;
	}

	if (rep.nExtensions) {
	    list = (char **) Xmalloc (
                (unsigned)(rep.nExtensions * sizeof (char *)));
	    rlen = rep.length << 2;
	    ch = (char *) Xmalloc ((unsigned) rlen + 1);
                /* +1 to leave room for last null-terminator */

	    if ((!list) || (!ch)) {
		if (list) Xfree((char *) list);
		if (ch)   Xfree((char *) ch);
		_AEatData(aud, (unsigned long) rlen);
		UnlockConnection(aud);
		SyncHandle();
		return (char **) NULL;
	    }

	    _AReadPad (aud, ch, rlen);
	    /*
	     * unpack into null terminated strings.
	     */
	    length = *ch;
	    for (i = 0; i < rep.nExtensions; i++) {
		list[i] = ch+1;  /* skip over length */
		ch += length + 1; /* find next length ... */
		length = *ch;
		*ch = '\0'; /* and replace with null-termination */
	    }
	}
	else list = (char **) NULL;

	*nextensions = rep.nExtensions;
	UnlockConnection(aud);
	SyncHandle();
	return (list);
}

AFFreeExtensionList (char **list)
{
	if (list != NULL) {
	    Xfree (list[0]-1);
	    Xfree ((char *)list);
	}
}
