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
 * $NCDId: @(#)AlibAsync.c,v 1.4 1993/01/29 20:47:18 lemke Exp $
 */

/* Portions derived from */
/* $XConsortium: XlibAsync.c,v 1.3 92/01/21 17:06:50 rws Exp $ */
/*

Copyright 1992 by the Massachusetts Institute of Technology

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

#include <audio/Alibint.h>
#include <audio/Aos.h>

/*ARGSUSED*/
AuBool
_AuAsyncErrorHandler(aud, rep, buf, len, data)
    register AuServer *aud;
    register auReply *rep;
    char *buf;
    int len;
    AuPointer data;
{
    register _AuAsyncErrorState *state;

    state = (_AuAsyncErrorState *)data;
    if (rep->generic.type == Au_Error &&
	(!state->error_code ||
	 rep->error.errorCode == state->error_code) &&
	(!state->major_opcode ||
	 rep->error.majorCode == state->major_opcode) &&
	(!state->minor_opcode ||
	 rep->error.minorCode == state->minor_opcode) &&
	(!state->min_sequence_number ||
	 (state->min_sequence_number <= aud->last_request_read)) &&
	(!state->max_sequence_number ||
	 (state->max_sequence_number >= aud->last_request_read))) {
	state->last_error_received = rep->error.errorCode;
	state->error_count++;
	return AuTrue;
    }
    return AuFalse;
}

void
_AuDoDeqAsyncHandler(aud, handler)
    AuServer *aud;
    register _AuAsyncHandler *handler;
{
    register _AuAsyncHandler **prev;
    register _AuAsyncHandler *async;

    for (prev = &aud->async_handlers;
	 (async = *prev) && (async != handler);
	 prev = &async->next)
    /* SUPPRESS 530 */
	;
    if (async)
	*prev = async->next;
}

char *
_AuGetAsyncReply(aud, replbuf, rep, buf, len, extra, discard)
    register AuServer *aud;
    register char *replbuf;
    register auReply *rep;
    char *buf;
    int len;
    int extra;
    AuBool discard;
{
    if (extra == 0) {
	if (discard && (rep->generic.length << 2) > len)
	    _AuEatData (aud, (rep->generic.length << 2) - len);
	return (char *)rep;
    }

    if (extra <= rep->generic.length) {
	int size = SIZEOF(auReply) + (extra << 2);
	if (size > len) {
	    bcopy(buf, replbuf, len);
	    _AuRead(aud, replbuf + len, size - len);
	    buf = replbuf;
	    len = size;
#ifdef MUSTCOPY
	} else {
	    bcopy(buf, replbuf, size);
	    buf = replbuf;
#endif
	}

	if (discard && rep->generic.length > extra &&
	    (rep->generic.length << 2) > len)
	    _AuEatData (aud, (rep->generic.length << 2) - len);

	return buf;
    }
    /* 
     *if we get here, then extra > rep->generic.length--meaning we
     * read a reply that's shorter than we expected.  This is an 
     * error,  but we still need to figure out how to handle it...
     */
    if ((rep->generic.length << 2) > len)
	_AuEatData (aud, (rep->generic.length << 2) - len);
    _AuIOError (aud);
    return (char *)rep;
}
