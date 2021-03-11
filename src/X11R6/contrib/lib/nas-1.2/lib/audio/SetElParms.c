/*
 * Copyright 1993 Network Computing Devices, Inc.
 * 
 * Permission to use, copy, modify, distribute, and sell this software and its
 * documentation for any purpose is hereby granted without fee, provided that
 * the above copyright notice appear in all copies and that both that
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
 * $NCDId: @(#)SetElParms.c,v 1.1 1993/01/26 05:11:16 greg Exp $
 */

#include "Alibint.h"

void
AuSetElementParameters(aud, num_changes, parms, ret_status)
AuServer *aud;
int num_changes;
AuElementParameters *parms;
AuStatus *ret_status;
{
    register auSetElementParametersReq *req;
    int i, total_parameters = 0;
    auElementParameters p;

    if (ret_status)
	*ret_status = AuSuccess;

    
    for (i = 0; i < num_changes; i++)
	total_parameters += parms[i].num_parameters;

    _AuLockServer(aud);
    _AuGetReq(SetElementParameters, req, aud);

    req->numParameters = num_changes;
    req->length += (num_changes * SIZEOF(auElementParameters) +
		    total_parameters * sizeof(CARD32)) >> 2;

    for (i = 0; i < num_changes; i++, parms++)
    {
#undef xfer
#define xfer(x) p.x = parms->x
	xfer(flow);
	xfer(element_num);
	xfer(num_parameters);

	_AuData32(aud, &p, SIZEOF(auElementParameters));
	_AuData32(aud, parms->parameters,
		  sizeof(CARD32) * parms->num_parameters);
    }

    (void) _AuIfRoundTrip(aud, ret_status);
    _AuUnlockServer(aud);
    _AuSyncHandle(aud);
}
