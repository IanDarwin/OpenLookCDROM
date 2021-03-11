/* $XConsortium: pex_ns.c,v 5.2 94/04/17 20:42:11 rws Exp $ */

/***********************************************************

Copyright (c) 1989, 1990, 1991  X Consortium

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL THE
X CONSORTIUM BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN
AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

Except as contained in this notice, the name of the X Consortium shall not be
used in advertising or otherwise to promote the sale, use or other dealings
in this Software without prior written authorization from the X Consortium.

Copyright 1989, 1990, 1991 by Sun Microsystems, Inc. 

                        All Rights Reserved

Permission to use, copy, modify, and distribute this software and its 
documentation for any purpose and without fee is hereby granted, 
provided that the above copyright notice appear in all copies and that
both that copyright notice and this permission notice appear in 
supporting documentation, and that the name of Sun Microsystems,
not be used in advertising or publicity pertaining to distribution of 
the software without specific, written prior permission.  

SUN MICROSYSTEMS DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, 
INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT 
SHALL SUN MICROSYSTEMS BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL 
DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,
WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION,
ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS
SOFTWARE.

******************************************************************/

/*
 *   PEX Name Set functions:
 *
 *   Section 8.1 -- Name Set Resource Management
 *	PEXCreateNameSet()	-- access via macro in phigspex.h
 *	PEXCopyNameSet()	    
 *	PEXDeleteNameSet()	-- access via macro in phigspex.h    
 *
 *   Section 8.2 -- Name Set Inquiry
 *	PEXGetNameSet()		    
 *
 *   Section 8.3 -- Name Set Modification
 *	PEXChangeNameSet()	    
 */

#include "pex_priv.h"

/*
 *   Section 8.1 -- Name Set Resource Management
 */

int
PEXCopyNameSet(display, src, dst)
Display	       *display;
pexNameSet	src;
pexNameSet	dst;
{
    int				 status = 0;
    Pex_srvr_info		*srvr;
    pexCopyNameSetReq		*req;
    
    if (srvr = PexEntryCheck(display, 1)) {
	LOCK_DISPLAY(display);
	PEX_REQUEST(CopyNameSet, display, PEX_OPCODE(srvr), req);
	req->src = src;
	req->dst = dst;
	status = 1;
	UNLOCK_DISPLAY(display);
    }
    PEX_SYNC_HANDLE(display);
    return status;
}


/*
 *   Section 8.2 -- Name Set Inquiry
 */

int
PEXGetNameSet(display, ns_id, num_names, names)
Display		*display;
pexNameSet	 ns_id;
CARD32		*num_names;	/* OUT */
pexName		*names[];	/* OUT */
{
    int				 status = 0;
    unsigned			 size;
    Pex_srvr_info		*srvr;
    pexGetNameSetReq		*req;
    pexGetNameSetReply		 reply;
    
    if (srvr = PexEntryCheck(display, 1)) {
	LOCK_DISPLAY(display);
	PEX_RESOURCE_ID_REQUEST(GetNameSet, display, 
		PEX_OPCODE(srvr), ns_id, req);
	
	status = _XReply(display, (xReply *)&reply, 0, xFalse);
	if (status) {
	    *num_names = reply.numNames;
	    ASSERT(reply.numNames == reply.length);
	    if( (size = reply.length * sizeof(CARD32)) > 0 ) {
		if ( *names = (pexName *)PEX_SCRATCH(srvr, size) )
		    _XRead(display, (char *)*names, (long)size);
		else {
		    status = 0;
		    PexClearReply(display, reply.length);
		}
	    }
	}
	UNLOCK_DISPLAY(display);
    }
    PEX_SYNC_HANDLE(display);
    return status;  
}

/*
 *   Section 8.3 -- Name Set Modification
 */

int
PEXChangeNameSet(display, ns_id, action, num_names, names)
Display		*display;
pexNameSet	 ns_id;
CARD16		 action;
CARD16		 num_names;
pexName		 names[];
{
    int				 status = 0;
    Pex_srvr_info		*srvr;
    pexChangeNameSetReq		*req;
    Pex_data_vec		vec[1];

    if ( srvr = PexEntryCheck( display, 1 ) ) {
	LOCK_DISPLAY(display);
	vec[0].size = num_names * sizeof(pexName);
	vec[0].padding = PADDING(vec[0].size);
	vec[0].data = (char *)names;
	PEX_VAR_REQUEST(ChangeNameSet, display, PEX_OPCODE(srvr),
	    vec[0].size + vec[0].padding, req);
	req->ns = ns_id;
	req->action = action;
	PEX_LOAD_VAR_REQUEST_DATA(display,1,vec)
	status = 1;
	UNLOCK_DISPLAY(display);
    }
    PEX_SYNC_HANDLE( display );
    return status;
}
