/* Copyright    Massachusetts Institute of Technology    1985	*/
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

#include "Alibint.h"

extern AFAudioConn *_AHeadOfAudioConnList;
/* 
 * AFClose - XSync the connection to the X Server, close the connection,
 * and free all associated storage.  This is the only routine that can be
 * called from or after an IOError handler, so the lower levels need to be able
 * to deal with broken connections.  Extension close procs should only free
 * memory and must be careful about the types of requests they generate.
 */

void
AFCloseAudioConn (register AFAudioConn *aud)
{
	register _AFExtension *ext;
	register AFAudioConn **dp = &_AHeadOfAudioConnList;
	register AFAudioConn *cp = _AHeadOfAudioConnList;
	extern void _AFreeQ();

	aud->flags |= AlibAudioConnClosing;
	AFSync(aud, 1);  /* throw away pending input events */
	ext = aud->ext_procs;
	while (ext) {		/* call out to any extensions interested */
		if (ext->close_audioconn != NULL) 
			(*ext->close_audioconn)(aud, &ext->codes);
		ext = ext->next;
	}    
        LockConnection(aud);
	_ADisconnectAudioConn(aud->fd);
	while (cp != NULL) {
		if (cp == aud) {
			*dp = cp->next;
			_AFreeAudioConnStructure (aud);
			break;
			}
		dp = &(cp->next);
		cp = *dp;
		}
	if (_AHeadOfAudioConnList == NULL) {
	    _AFreeQ ();
	}
	return;
}
