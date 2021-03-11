/* Copyright    Massachusetts Institute of Technology    1986	*/
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
/* this might be rightly reguarded an os dependent file */

#include "Alibint.h"

void
AFAddHost(register AFAudioConn *aud, AFHostAddress *host)
{
    register aChangeHostsReq *req;
    register int length = (host->length + 3) & ~0x3;	/* round up */

    LockConnection(aud);
    aud = aud->connection;
    GetReqExtra(ChangeHosts, length, req);
    req->mode = AHostInsert;
    req->hostFamily = host->family;
    req->hostLength = host->length;
    bcopy (host->address, (char *) NEXTPTR(req,aChangeHostsReq), host->length);
    UnlockConnection(aud);
    SyncHandle();
}

void
AFRemoveHost(register AFAudioConn *aud, AFHostAddress *host)
{
    register aChangeHostsReq *req;
    register int length = (host->length + 3) & ~0x3;	/* round up */

    LockConnection(aud);
    aud = aud->connection;
    GetReqExtra(ChangeHosts, length, req);
    req->mode = AHostDelete;
    req->hostFamily = host->family;
    req->hostLength = host->length;
    bcopy (host->address, (char *) NEXTPTR(req,aChangeHostsReq), host->length);
    UnlockConnection(aud);
    SyncHandle();
    }

void
AFAddHosts(register AFAudioConn *aud, AFHostAddress *hosts, int n)
{
    register int i;
    for (i = 0; i < n; i++) {
	AFAddHost(aud, &hosts[i]);
      }
}

void
AFRemoveHosts(register AFAudioConn *aud, AFHostAddress *hosts, int n)
{
    register int i;
    for (i = 0; i < n; i++) {
	AFRemoveHost(aud, &hosts[i]);
      }
}
