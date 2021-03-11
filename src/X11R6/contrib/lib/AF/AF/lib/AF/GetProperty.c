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

int
AFGetProperty(
    AC ac,
    AAtom property,
    long offset, 
    long length,
    ABool delete,
    AAtom req_type,
    AAtom *actual_type		/* RETURN */,
    int *actual_format  	/* RETURN  8, 16, or 32 */,
    unsigned long *nitems 	/* RETURN  # of 8-, 16-, or 32-bit entities */,
    unsigned long *bytesafter	/* RETURN */,
    unsigned char **prop	/* RETURN */
	)
    {
    aGetPropertyReply reply;
    register aGetPropertyReq *req;
    register AFAudioConn *aud = ac->connection;

    LockConnection(aud);
    GetReq (GetProperty, req);
    req->ac = ac->acontext;
    req->property = property;
    req->type = req_type;
    req->c_delete = delete;
    req->longOffset = offset;
    req->longLength = length;
    
    if (!_AReply (aud, (aReply *) &reply, 0, aFalse)) {
	UnlockConnection(aud);
	SyncHandle();
	return (1);	/* not Success */
	}	

    *prop = (unsigned char *) NULL;
    if (reply.propertyType != ANone) {
	long nbytes, netbytes;
	switch (reply.format) {
      /* 
       * One extra byte is malloced than is needed to contain the property
       * data, but this last byte is null terminated and convenient for 
       * returning string properties, so the client doesn't then have to 
       * recopy the string to make it null terminated. 
       */
	  case 8:
	    nbytes = netbytes = reply.nItems;
	    if (*prop = (unsigned char *) Xmalloc ((unsigned)nbytes + 1))
		_AReadPad (aud, (char *) *prop, netbytes);
	    break;

	  case 16:
	    nbytes = reply.nItems * sizeof (short);
	    netbytes = reply.nItems << 1;
	    if (*prop = (unsigned char *) Xmalloc ((unsigned)nbytes + 1))
		_ARead16Pad (aud, (short *) *prop, netbytes);
	    break;

	  case 32:
	    nbytes = reply.nItems * sizeof (long);
	    netbytes = reply.nItems << 2;
	    if (*prop = (unsigned char *) Xmalloc ((unsigned)nbytes + 1))
		_ARead32 (aud, (long *) *prop, netbytes);
	    break;

	  default:
	    /*
	     * This part of the code should never be reached.  If it is,
	     * the server sent back a property with an invalid format.
	     * This is a BadImplementation error. 
	     */
	    {
		aError error;
		error.sequenceNumber = aud->request;
		error.type = A_Error;
		error.majorCode = A_GetProperty;
		error.minorCode = 0;
		error.errorCode = ABadImplementation;
		_AError(aud, &error);
	    }
	    netbytes = 0L;
	    break;
	}
	if (! *prop) {
	    _AEatData(aud, (unsigned long) netbytes);
	    UnlockConnection(aud);
	    SyncHandle();
	    return(ABadAlloc);	/* not Success */
	}
	(*prop)[nbytes] = '\0';
    }
    *actual_type = reply.propertyType;
    *actual_format = reply.format;
    *nitems = reply.nItems;
    *bytesafter = reply.bytesAfter;
    UnlockConnection(aud);
    SyncHandle();
    return(ASuccess);
}

