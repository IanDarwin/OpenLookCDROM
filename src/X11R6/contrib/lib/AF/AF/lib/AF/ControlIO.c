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


void AFEnableInput(AC ac, AMask mask, AMask *old_state, AMask *new_state)
{
    register AFAudioConn *aud = ac->connection; 
    register aEnableInputReq *req;
    aControlIOReply reply;

    LockConnection(aud);
    GetReq(EnableInput, req);
    req->mask = mask;
    req->ac = ac->acontext;
    _AReply(aud, (aReply *) &reply, 0, aTrue);
    UnlockConnection(aud);
    SyncHandle();
    *old_state = reply.oldState;
    *new_state = reply.newState;
    return;
}

void AFEnableOutput(AC ac, AMask mask, AMask *old_state, AMask *new_state)
{
    register AFAudioConn *aud = ac->connection; 
    register aEnableOutputReq *req;
    aControlIOReply reply;

    LockConnection(aud);
    GetReq(EnableOutput, req);
    req->mask = mask;
    req->ac = ac->acontext;
    _AReply(aud, (aReply *) &reply, 0, aTrue);
    UnlockConnection(aud);
    SyncHandle();
    *old_state = reply.oldState;
    *new_state = reply.newState;
    return;
}

void AFDisableInput(AC ac, AMask mask, AMask *old_state, AMask *new_state)
{
    register AFAudioConn *aud = ac->connection; 
    register aDisableInputReq *req;
    aControlIOReply reply;

    LockConnection(aud);
    GetReq(DisableInput, req);
    req->mask = mask;
    req->ac = ac->acontext;
    _AReply(aud, (aReply *) &reply, 0, aTrue);
    UnlockConnection(aud);
    SyncHandle();
    *old_state = reply.oldState;
    *new_state = reply.newState;
    return;
}

void AFDisableOutput(AC ac, AMask mask, AMask *old_state, AMask *new_state)
{
    register AFAudioConn *aud = ac->connection; 
    register aDisableOutputReq *req;
    aControlIOReply reply;

    LockConnection(aud);
    GetReq(DisableOutput, req);
    req->mask = mask;
    req->ac = ac->acontext;
    _AReply(aud, (aReply *) &reply, 0, aTrue);
    UnlockConnection(aud);
    SyncHandle();
    *old_state = reply.oldState;
    *new_state = reply.newState;
    return;
}


void AFEnablePassThrough(AC ac, ABool change, 
			 ABool *old_state, ABool *new_state)
{
    register AFAudioConn *aud = ac->connection; 
    register aEnablePassThroughReq *req;
    aControlIOReply reply;

    LockConnection(aud);
    GetReq(EnablePassThrough, req);
    req->mask = (CARD32) change;
    req->ac = ac->acontext;
    _AReply(aud, (aReply *) &reply, 0, aTrue);
    UnlockConnection(aud);
    SyncHandle();
    *old_state = reply.oldState;
    *new_state = reply.newState;
    return;
}

void AFDisablePassThrough(AC ac, ABool change, 
			  ABool *old_state, ABool *new_state)
{
    register AFAudioConn *aud = ac->connection; 
    register aDisablePassThroughReq *req;
    aControlIOReply reply;

    LockConnection(aud);
    GetReq(DisablePassThrough, req);
    req->mask = (CARD32) change;
    req->ac = ac->acontext;
    _AReply(aud, (aReply *) &reply, 0, aTrue);
    UnlockConnection(aud);
    SyncHandle();
    *old_state = reply.oldState;
    *new_state = reply.newState;
    return;
}

