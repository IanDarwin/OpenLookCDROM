/* $XConsortium: pex_rend.c,v 5.2 94/04/17 20:42:16 rws Exp $ */

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
 *   PEX Renderer Functions
 *
 *   Section 6.1 -- Renderer Resource Management 
 *	PEXCreateRenderer()		
 *	PEXFreeRenderer()		-- access via macro in phigspex.h
 *	
 *   Section 6.2 -- Renderer Modification
 *	PEXChangeRenderer()		
 *
 *   Section 6.3 -- Renderer Inquiry
 *	PEXGetRendererAttributes()	
 *	PEXGetRendererDynamics()	
 *
 *   Section 6.4 -- Client-Side Traversal Support
 *	PEXBeginRendering()		
 *	PEXEndRendering()		
 *	PEXBeginStructure()		
 *	PEXEndStructure()		-- access via macro in phigspex.h
 *
 *   Section 6.5 -- Rendering Commands
 *	PEXRenderOutputCommands()	
 *	PEXRenderNetwork()		
 */

#include "pex_priv.h"

/*
 *   Section 6.1 -- Renderer Resource Management
 */
int
PEXCreateRenderer(display, rdr_id, drawable, item_mask, length_of_items, items)
Display		    *display;
pexRenderer	     rdr_id;
Drawable	     drawable;
pexBitmask	     item_mask;
CARD32		     length_of_items;	/* size of 'items' in bytes */
Pointer		     items;
{
    int				 status = 0;
    Pex_srvr_info		*srvr;
    pexCreateRendererReq	*req;
    Pex_data_vec		vec[1];

    if (srvr = PexEntryCheck(display, 1)) {
	LOCK_DISPLAY(display);
	vec[0].size = length_of_items;
	vec[0].padding = PADDING(vec[0].size);
	vec[0].data = (char *)items;
	PEX_VAR_REQUEST(CreateRenderer, display, PEX_OPCODE(srvr),
	    vec[0].size + vec[0].padding, req);
	PEX_FP_FORMAT(req->fpFormat);
	req->rdr = rdr_id;
	req->itemMask = item_mask;
	req->drawable = drawable;
	PEX_LOAD_VAR_REQUEST_DATA(display,1,vec)
	status = 1;
	UNLOCK_DISPLAY(display);
    }
    PEX_SYNC_HANDLE(display);
    return status;
}



/*
 *   Section 6.2 -- Renderer Modification
 */
int
PEXChangeRenderer(display, rdr_id, item_mask, length_of_items, items)
Display		    *display;
pexRenderer	     rdr_id;
pexBitmask	     item_mask;
CARD32		     length_of_items;	/* size of 'items' in bytes */
Pointer		     items;
{
    int				 status = 0;
    Pex_srvr_info		*srvr;
    pexChangeRendererReq	*req;
    Pex_data_vec		vec[1];

    if (srvr = PexEntryCheck(display, 1)) {
	LOCK_DISPLAY(display);
	vec[0].size = length_of_items;
	vec[0].padding = PADDING(vec[0].size);
	vec[0].data = (char *)items;
	PEX_VAR_REQUEST(ChangeRenderer, display, PEX_OPCODE(srvr),
	    vec[0].size + vec[0].padding, req);
	PEX_FP_FORMAT(req->fpFormat);
	req->rdr = rdr_id;
	req->itemMask = item_mask;
	PEX_LOAD_VAR_REQUEST_DATA(display,1,vec)
	status = 1;
	UNLOCK_DISPLAY(display);
    }
    PEX_SYNC_HANDLE(display);
    return status;
}



/*
 *   Section 6.3 -- Renderer Inquiry
 */

int
PEXGetRendererAttributes(display, rdr_id, item_mask, items)
Display		*display;
pexRenderer	 rdr_id;
pexBitmask	 item_mask;
Pointer		*items;		/* OUT */
{
    int				     status = 0;
    unsigned			     size;
    Pex_srvr_info		    *srvr;
    pexGetRendererAttributesReq	    *req;
    pexGetRendererAttributesReply    reply;
    
    if (srvr = PexEntryCheck(display, 1)) {
	LOCK_DISPLAY(display);
	PEX_REQUEST(GetRendererAttributes, display, PEX_OPCODE(srvr), req);
	PEX_FP_FORMAT(req->fpFormat);
	req->rdr = rdr_id;
	req->itemMask = item_mask;
	
	status = _XReply(display, (xReply *)&reply, 0, xFalse);
	if (status) {
	    if ( (size = reply.length * sizeof(CARD32)) > 0 ) {
		if (*items = PEX_SCRATCH(srvr, size) )
		    _XRead(display, (char *)*items, (long)size);
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

int
PEXGetRendererDynamics(display, rdr_id, tables, namesets, attributes)
Display		*display;
pexRenderer	 rdr_id;
pexBitmask	*tables;	/* OUT */
pexBitmask	*namesets;	/* OUT */
pexBitmask	*attributes;	/* OUT */
{
    int				     status = 0;
    Pex_srvr_info		    *srvr;
    pexGetRendererDynamicsReq	    *req;
    pexGetRendererDynamicsReply	     reply;
    
    if (srvr = PexEntryCheck(display, 1)) {
	LOCK_DISPLAY(display);
	PEX_RESOURCE_ID_REQUEST(GetRendererDynamics, display, 
		PEX_OPCODE(srvr), rdr_id, req);
	
	status = _XReply(display, (xReply *)&reply, 0, xFalse);
	if (status) {
	    ASSERT(reply.length == 0);
	    *tables	= reply.tables;
	    *namesets	= reply.namesets;
	    *attributes	= reply.attributes;
	}
	UNLOCK_DISPLAY(display);
    }
    PEX_SYNC_HANDLE(display);
    return status;    
}


/*
 *   Section 6.4 -- Client-Side Traversal Support
 */
int
PEXBeginRendering(display, rdr_id, drawable)
Display		    *display;
pexRenderer	     rdr_id;
Drawable	     drawable;
{
    int				 status = 0;
    Pex_srvr_info		*srvr;
    pexBeginRenderingReq	*req;
    
    if (srvr = PexEntryCheck(display, 1)) {
	LOCK_DISPLAY(display);
	PEX_REQUEST(BeginRendering, display, PEX_OPCODE(srvr), req);
	req->rdr = rdr_id;
	req->drawable = drawable;
	status = 1;
	UNLOCK_DISPLAY(display);
    }
    PEX_SYNC_HANDLE(display);
    return status;
}

int
PEXEndRendering(display, rdr_id, flush_flag)
Display		    *display;
pexRenderer	     rdr_id;
CARD8		     flush_flag;
{
    int				 status = 0;
    Pex_srvr_info		*srvr;
    pexEndRenderingReq		*req;
    
    if (srvr = PexEntryCheck(display, 1)) {
	LOCK_DISPLAY(display);
	PEX_REQUEST(EndRendering, display, PEX_OPCODE(srvr), req);
	req->rdr = rdr_id;
	req->flushFlag = flush_flag;
	status = 1;
	UNLOCK_DISPLAY(display);
    }
    PEX_SYNC_HANDLE(display);
    return status;
}

int
PEXBeginStructure(display, rdr_id, struct_id)
Display		    *display;
pexRenderer	     rdr_id;
CARD32		     struct_id;
{
    int				 status = 0;
    Pex_srvr_info		*srvr;
    pexBeginStructureReq	*req;
    
    if (srvr = PexEntryCheck(display, 1)) {
	LOCK_DISPLAY(display);
	PEX_REQUEST(BeginStructure, display, PEX_OPCODE(srvr), req);
	req->rdr = rdr_id;
	req->sid = struct_id;
	status = 1;
	UNLOCK_DISPLAY(display);
    }
    PEX_SYNC_HANDLE(display);
    return status;
}


/*
 *   Section 6.5 -- Rendering Commands
 */


int
PEXRenderOutputCommands(display, rdr_id, num_ocs, length_of_oc_list, oc_list)
Display		   *display;
pexRenderer	    rdr_id;
CARD32		    num_ocs;
CARD32		    length_of_oc_list;  /* in bytes */
Pointer		    oc_list;
{
    int				 status = 0;
    Pex_srvr_info		*srvr;
    pexRenderOutputCommandsReq	*req;
    Pex_data_vec		vec[1];

    if (srvr = PexEntryCheck(display, 1)) {
	LOCK_DISPLAY(display);
	vec[0].size = length_of_oc_list;
	vec[0].padding = PADDING(vec[0].size);
	vec[0].data = (char *)oc_list;
	PEX_VAR_REQUEST(RenderOutputCommands, display, PEX_OPCODE(srvr),
	    vec[0].size + vec[0].padding, req);
	PEX_FP_FORMAT(req->fpFormat);
	req->rdr = rdr_id;
	req->numCommands = num_ocs;
	PEX_LOAD_VAR_REQUEST_DATA(display,1,vec)
	status = 1;
	UNLOCK_DISPLAY(display);
    }
    PEX_SYNC_HANDLE( display );
    return status;
}


int
PEXRenderNetwork(display, rdr_id, drawable, struct_id)
Display		    *display;
pexRenderer	     rdr_id;
Drawable	     drawable;
pexStructure	     struct_id;
{
    int				 status = 0;
    Pex_srvr_info		*srvr;
    pexRenderNetworkReq		*req;
    
    if (srvr = PexEntryCheck(display, 1)) {
	LOCK_DISPLAY(display);
	PEX_REQUEST(RenderNetwork, display, PEX_OPCODE(srvr), req);
	req->rdr = rdr_id;
	req->drawable = drawable;
	req->sid = struct_id;
	status = 1;
	UNLOCK_DISPLAY(display);
    }
    PEX_SYNC_HANDLE(display);
    return status;
}
