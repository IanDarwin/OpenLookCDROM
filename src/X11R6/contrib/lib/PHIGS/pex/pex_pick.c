/* $XConsortium: pex_pick.c,v 5.3 94/04/17 20:42:12 mor Exp $ */

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
 *   PEX Picking functions:
 *
 *   SECTION 7 -- RENDERER PICKING
 *
 *   Section 7.1 -- Pick One
 *	PEXBeginPickOne()
 *	PEXEndPickOne()
 *
 *   SECTION 12 -- WORKSTATION PICKING
 *
 *   Section 12.1 -- Pick Device Descriptors
 *	PEXGetPickDevice()	    
 *	PEXChangePickDevice()	    
 *
 *   Section 12.2 -- Pick Measure
 *	PEXCreatePickMeasure()	    
 *	PEXDeletePickMeasure()	    -- access via macro in phigspex.h
 *	PEXGetPickMeasure()	    
 *	PEXUpdatePickMeasure()	    
 *
 */

#include "pex_priv.h"



/*
 * Section 7.1 -- Pick One
 */

int PEXBeginPickOne (display, rdr_id, drawable, struct_id, method,
	pickrec_bytes, pickrec)

Display		*display;
pexRenderer	rdr_id;
Drawable	drawable;
CARD32		struct_id;
int		method;
CARD32		pickrec_bytes;
CARD8		pickrec[];

{
    int				status = 0;
    Pex_srvr_info		*srvr;
    pexBeginPickOneReq		*req;
    Pex_data_vec		vec[1];

    if (srvr = PexEntryCheck (display, 1)) {
	LOCK_DISPLAY (display);
	vec[0].size = pickrec_bytes;
	vec[0].padding = PADDING (vec[0].size);
	vec[0].data = (char *) pickrec;
	PEX_VAR_REQUEST (BeginPickOne, display, PEX_OPCODE (srvr),
		vec[0].size + vec[0].padding, req);
	PEX_FP_FORMAT (req->fpFormat);
	req->method = method;
	req->rdr = rdr_id;
	req->drawable = drawable;
	req->sid = struct_id;
	PEX_LOAD_VAR_REQUEST_DATA (display, 1, vec);
	status = 1;
	UNLOCK_DISPLAY (display);
    }
    PEX_SYNC_HANDLE (display);
    return status;
}


int PEXEndPickOne (display, rdr_id, better_pick, elements, num_elements)

Display			*display;
pexRenderer		rdr_id;
int			*better_pick;
pexPickElementRef	**elements;
int			*num_elements;

{
    int				status = 0;
    unsigned			size;
    Pex_srvr_info		*srvr;
    pexEndPickOneReq	    	*req;
    pexEndPickOneReply		reply;
    
    if (srvr = PexEntryCheck (display, 1)) {
	LOCK_DISPLAY (display);
	PEX_REQUEST (EndPickOne, display, PEX_OPCODE (srvr), req);
	req->rdr = rdr_id;
	
	status = _XReply (display, (xReply *) &reply, 0, xFalse);
	if (status) {
	    *better_pick = reply.betterPick;
	    *num_elements = reply.numPickElRefs;
	    if ((size = reply.length * sizeof(CARD32)) > 0) {
		if (*elements = (pexPickElementRef *) PEX_SCRATCH (srvr, size))
		    _XRead (display, (char *) *elements, (long) size);
		else {
		    status = 0;
		    PexClearReply (display, reply.length);
		}
	    }
	}
	UNLOCK_DISPLAY (display);
    }
    PEX_SYNC_HANDLE (display);
    return status;    
}



/*
 * Section 11.1 -- Pick Device Descriptors
 */

int
PEXGetPickDevice(display, wks_id, dev_type, item_mask, pick_attrs)
Display		*display;
pexPhigsWks	 wks_id;
pexEnumTypeIndex dev_type;
pexBitmask	 item_mask;
Pointer		*pick_attrs;	/* OUT */
{
    int				 status = 0;
    unsigned			 size;
    Pex_srvr_info		*srvr;
    pexGetPickDeviceReq		*req;
    pexGetPickDeviceReply	 reply;
    
    if (srvr = PexEntryCheck(display, 1)) {
	LOCK_DISPLAY(display);
	PEX_REQUEST(GetPickDevice, display, PEX_OPCODE(srvr), req);
	PEX_FP_FORMAT(req->fpFormat);
	req->wks = wks_id;
	req->devType = dev_type;
	req->itemMask = item_mask;
	
	status = _XReply(display, (xReply *)&reply, 0, xFalse);
	if (status) {
	    ASSERT(reply.length != 0);
	    if ( (size = reply.length * sizeof(CARD32)) > 0 ) {
		if ( *pick_attrs = PEX_SCRATCH(srvr, size) )
		    _XRead(display, (char *)*pick_attrs, (long)size);
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

/* Caller needs to compose 'values', and supply it's length */
int
PEXChangePickDevice(display, wks_id, dev_type, item_mask,
	length_of_values, values)
Display		    *display;
pexPhigsWks	     wks_id;
pexEnumTypeIndex     dev_type;
pexBitmask	     item_mask;
CARD32		     length_of_values;	/* size of 'values' in bytes */
Pointer		     values;
{
    int				 status = 0;
    Pex_srvr_info		*srvr;
    pexChangePickDeviceReq	*req;
    Pex_data_vec		vec[1];
    
    if (srvr = PexEntryCheck(display, 1)) {
	LOCK_DISPLAY(display);
	vec[0].size = length_of_values;
	vec[0].padding = PADDING(vec[0].size);
	vec[0].data = (char *)values;
	PEX_VAR_REQUEST(ChangePickDevice, display, PEX_OPCODE(srvr),
	    vec[0].size + vec[0].padding, req);
	PEX_FP_FORMAT(req->fpFormat);
	req->wks = wks_id;
	req->devType = dev_type;
	req->itemMask = item_mask;
	PEX_LOAD_VAR_REQUEST_DATA(display,1,vec)
	status = 1;
	UNLOCK_DISPLAY(display);
    }
    PEX_SYNC_HANDLE(display);
    return status;
}


/*
 * Section 11.2 -- Pick Measure Resource Management
 */

int
PEXCreatePickMeasure(display, wks_id, pick_measure, dev_type)
Display		    *display;
pexPhigsWks	     wks_id;
pexPickMeasure	     pick_measure;
pexEnumTypeIndex     dev_type;
{
    int				 status = 0;
    Pex_srvr_info		*srvr;
    pexCreatePickMeasureReq	*req;
    
    if (srvr = PexEntryCheck(display, 1)) {
	LOCK_DISPLAY(display);
	PEX_REQUEST(CreatePickMeasure, display, PEX_OPCODE(srvr), req);
	req->wks = wks_id;
	req->pm = pick_measure;
	req->devType = dev_type;
	status = 1;
	UNLOCK_DISPLAY(display);
    }
    PEX_SYNC_HANDLE(display);
    return status;
}


/*
 * Section 11.3 -- Pick Measure Inquiry
 */
int
PEXGetPickMeasure(display, pick_measure, item_mask, pick_attrs)
Display		*display;
pexPickMeasure	 pick_measure;
pexBitmask	 item_mask;
Pointer		*pick_attrs;	/* OUT */
{
    int				 status = 0;
    unsigned			 size;
    Pex_srvr_info		*srvr;
    pexGetPickMeasureReq	*req;
    pexGetPickMeasureReply	 reply;
    
    if (srvr = PexEntryCheck(display, 1)) {
	LOCK_DISPLAY(display);
	PEX_REQUEST(GetPickMeasure, display, PEX_OPCODE(srvr), req);
	req->pm = pick_measure;
	req->itemMask = item_mask;
	
	status = _XReply(display, (xReply *)&reply, 0, xFalse);
	if (status) {
	    if ( (size = reply.length * sizeof(CARD32)) > 0 ) {
		if ( *pick_attrs = PEX_SCRATCH(srvr, size) )
		    _XRead(display, (char *)*pick_attrs, (long)size);
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
 * Section 11.4 -- Pick Operations
 */
 
int
PEXUpdatePickMeasure(display, pick_measure, num_bytes, bytes)
Display		    *display;
pexPickMeasure	     pick_measure;
CARD32		     num_bytes;
CARD8		     bytes[];
{
    int				 status = 0;
    Pex_srvr_info		*srvr;
    pexUpdatePickMeasureReq	*req;
    Pex_data_vec		vec[1];
    
    if (srvr = PexEntryCheck(display, 1)) {
	LOCK_DISPLAY(display);
	vec[0].size = num_bytes;
	vec[0].padding = PADDING(vec[0].size);
	vec[0].data = (char *)bytes;
	PEX_VAR_REQUEST(UpdatePickMeasure, display, PEX_OPCODE(srvr),
	    vec[0].size + vec[0].padding, req);
	req->pm = pick_measure;
	req->numBytes = num_bytes;
	PEX_LOAD_VAR_REQUEST_DATA(display,1,vec)
	status = 1;
	UNLOCK_DISPLAY(display);
    }
    PEX_SYNC_HANDLE(display);
    return status;
}
