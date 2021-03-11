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
 * $NCDId: @(#)CloseSvr.c,v 1.4 1993/05/14 18:20:47 greg Exp $
 */

/* Portions derived from */
/* $XConsortium: XClDisplay.c,v 11.24 91/12/19 18:06:28 rws Exp $ */
/*

Copyright 1985, 1990 by the Massachusetts Institute of Technology

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

#include "Alibint.h"

/* 
 * AuCloseServer - AuSync the connection to the audio server, close the 
 * connection,
 * and free all associated storage.  Extension close procs should only free
 * memory and must be careful about the types of requests they generate.
 */

void
AuCloseServer (aud)
	register AuServer *aud;
{
	register _AuExtension *ext;

	_AuFreeBucketCache(aud);

	if (!(aud->flags & AuServerFlagsClosing))
	{
	    aud->flags |= AuServerFlagsClosing;
	    ext = aud->ext_procs;
	    while (ext) {	/* call out to any extensions interested */
		    if (ext->close_server != NULL) 
			    (*ext->close_server)(aud, &ext->codes);
		    ext = ext->next;
	    }    
	    AuSync(aud, 1);  /* throw away pending input events */
	}
	_AuDisconnectServer(aud->fd);
	_AuFreeServerStructure (aud);
	return;
}
