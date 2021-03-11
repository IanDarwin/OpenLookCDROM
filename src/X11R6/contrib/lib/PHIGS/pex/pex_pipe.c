/* $XConsortium: pex_pipe.c,v 5.2 94/04/17 20:42:12 rws Exp $ */

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
 *   PEX Pipeline Context Functions
 *
 *   Section 5.1 -- Pipeline Context Resource Management
 *	PEXCreatePipelineContext()	    
 *	PEXCopyPipelineContext()	    
 *	PEXDeletePipelineContext()	    -- access via macro in phigspex.h
 *
 *   Section 5.2 -- Pipeline Context Inquiry
 *	PEXGetPipelineContext()		    
 *
 *   Section 5.3 -- Pipeline Context Modification
 *	PEXChangePipelineContext()	    
 */

#include "pex_priv.h"

/* 
 *   Section 5.1 -- Pipeline Context Resource Management
 */
 
int
PEXCreatePipelineContext(display, pc_id, itemMask, length_of_items, items)
Display		    *display;
pexPC		     pc_id;
pexBitmask	     itemMask[PEXMSPipeline];
CARD32		     length_of_items;	/* size of 'items' in bytes */
Pointer		     items;
{
    int				 status = 0;
    Pex_srvr_info		*srvr;
    pexCreatePipelineContextReq	*req;
    Pex_data_vec		vec[1];

    if (srvr = PexEntryCheck(display, 1)) {
	LOCK_DISPLAY(display);
	vec[0].size = length_of_items;
	vec[0].padding = PADDING(vec[0].size);
	vec[0].data = (char *)items;
	PEX_VAR_REQUEST(CreatePipelineContext, display, PEX_OPCODE(srvr),
	    vec[0].size + vec[0].padding, req);
	PEX_FP_FORMAT(req->fpFormat);
	req->pc = pc_id;
	if ( itemMask )
	    bcopy( (char *)itemMask, (char *)req->itemMask,
		sizeof(req->itemMask) );
	else
	    bzero( (char *)req->itemMask, sizeof(req->itemMask) );
	PEX_LOAD_VAR_REQUEST_DATA(display,1,vec)
	status = 1;
	UNLOCK_DISPLAY(display);
    }
    PEX_SYNC_HANDLE(display);
    return status;
}



int
PEXCopyPipelineContext(display, src_pc, dst_pc, itemMask)
Display		    *display;
pexPC		     src_pc;
pexPC		     dst_pc;
pexBitmask	     itemMask[PEXMSPipeline];
{
    int				 status = 0;
    Pex_srvr_info		*srvr;
    pexCopyPipelineContextReq	*req;
    
    if (srvr = PexEntryCheck(display, 1)) {
	LOCK_DISPLAY(display);
	PEX_REQUEST(CopyPipelineContext, display, PEX_OPCODE(srvr), req);
	req->src = src_pc;
	req->dst = dst_pc;
	bcopy( (char *)itemMask, (char *)req->itemMask,
	    sizeof(req->itemMask) );
	status = 1;
	UNLOCK_DISPLAY(display);
    }
    PEX_SYNC_HANDLE(display);
    return status;
}


/* 
 *   Section 5.2 -- Pipeline Context Inquiry
 */
 
int
PEXGetPipelineContext(display, pc_id, itemMask, values)
Display		*display;
pexPC		 pc_id;
pexBitmask	 itemMask[PEXMSPipeline];
Pointer		*values;	/* OUT */
{
    int				 status = 0;
    unsigned			 size;
    Pex_srvr_info		*srvr;
    pexGetPipelineContextReq	*req;
    pexGetPipelineContextReply	 reply;
    
    if (srvr = PexEntryCheck(display, 1)) {
	LOCK_DISPLAY(display);
	PEX_REQUEST(GetPipelineContext, display, PEX_OPCODE(srvr), req);
	PEX_FP_FORMAT(req->fpFormat);
	req->pc = pc_id;
	bcopy( (char *)itemMask, (char *)req->itemMask,
	    sizeof(req->itemMask) );
	
	status = _XReply(display, (xReply *)&reply, 0, xFalse);
	if (status) {
	    if( (size = reply.length * sizeof(CARD32)) > 0 ) {
		if ( *values = PEX_SCRATCH(srvr, size) )
		    _XRead(display, (char *)*values, (long)size);
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
 *   Section 5.3 -- Pipeline Context Modification
 */

/* Caller needs to compose 'values', and supply it's length */
int
PEXChangePipelineContext(display, pc_id, itemMask, length_of_values, values)
Display		    *display;
pexPC		     pc_id;
pexBitmask	     itemMask[PEXMSPipeline];
CARD32		     length_of_values;	/* size of 'values' in bytes */
Pointer		     values;
{
    int				 status = 0;
    Pex_srvr_info		*srvr;
    pexChangePipelineContextReq	*req;
    Pex_data_vec		vec[1];
    
    if (srvr = PexEntryCheck(display, 1)) {
	LOCK_DISPLAY(display);
	vec[0].size = length_of_values;
	vec[0].padding = PADDING(vec[0].size);
	vec[0].data = (char *)values;
	PEX_VAR_REQUEST(ChangePipelineContext, display, PEX_OPCODE(srvr),
	    vec[0].size + vec[0].padding, req);
	PEX_FP_FORMAT(req->fpFormat);
	req->pc = pc_id;
	bcopy( (char *)itemMask, (char *)req->itemMask,
	    sizeof(req->itemMask) );
	PEX_LOAD_VAR_REQUEST_DATA(display,1,vec)
	status = 1;
	UNLOCK_DISPLAY(display);
    }
    PEX_SYNC_HANDLE(display);
    return status;
}
