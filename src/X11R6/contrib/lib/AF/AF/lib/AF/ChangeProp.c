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

#include "Alibint.h"

void AFChangeProperty (
    AC ac,
    AAtom property,
    AAtom type,
    int format,  /* 8, 16, or 32 */
    int mode,  /* PropModeReplace, PropModePrepend, PropModeAppend */
    unsigned char *data,
    int nelements)
    {
    register AFAudioConn *aud = ac->connection;
    register aChangePropertyReq *req;
    register long len;

    LockConnection(aud);
    GetReq (ChangeProperty, req);
    req->ac = ac->acontext;
    req->property = property;
    req->type = type;
    req->format = format;
    req->mode = mode;
    if (nelements < 0) {
	req->nUnits = 0;
	req->format = 0; /* ask for garbage, get garbage */
    } else
	req->nUnits = nelements;
    
    switch (format) {
      case 8:
	len = req->length + (((long)nelements + 3)>>2);
	if (len <= 65535) {
	    req->length = len;
	    Data (aud, (char *)data, nelements);
	} /* else force BadLength */
        break;
 
      case 16:
	len = req->length + (((long)nelements + 1)>>1);
	if (len <= 65535) {
	    req->length = len;
	    len = (long)nelements << 1;
	    Data16 (aud, (short *) data, len);
	} /* else force BadLength */
	break;

      case 32:
	len = req->length + (long)nelements;
	if (len <= 65535) {
	    req->length = len;
	    len = (long)nelements << 2;
	    Data32 (aud, (long *) data, len);
	} /* else force BadLength */
	break;

      default:
        /* BadValue will be generated */ ;
      }

    UnlockConnection(aud);
    SyncHandle();
    }

