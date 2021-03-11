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

#define NEED_REPLIES
#include "Alibint.h"

AAtom *AFListProperties(AC ac, int *n_props)
{
    long nbytes;
    aListPropertiesReply rep;
    AAtom *properties;
    register aResourceReq *req;
    AFAudioConn *aud = ac->connection;

    LockConnection(aud);
    GetResReq(ListProperties, ac->acontext, req);
    if (!_AReply(aud, (aReply *)&rep, 0, aFalse)) {
	*n_props = 0;
	UnlockConnection(aud);
        SyncHandle();
	return ((AAtom *) NULL);
    }

    if (rep.nProperties) {
	nbytes = rep.nProperties * sizeof(AAtom);
	properties = (AAtom *) Xmalloc ((unsigned) nbytes);
	nbytes = rep.nProperties << 2;
	if (! properties) {
	    _AEatData(aud, (unsigned long) nbytes);
	    UnlockConnection(aud);
	    SyncHandle();
	    return (AAtom *) NULL;
	}
	_ARead32 (aud, (char *) properties, nbytes);
    }
    else properties = (AAtom *) NULL;

    *n_props = rep.nProperties;
    UnlockConnection(aud);
    SyncHandle();
    return (properties);
}
