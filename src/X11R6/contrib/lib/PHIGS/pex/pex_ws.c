/* $XConsortium: pex_ws.c,v 5.3 94/04/17 20:42:19 hersh Exp $ */

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
 *   PEX Workstation functions:
 *
 *   Section 10.1 -- Phigs Workstation Resource Management
 *	PEXCreatePhigsWks()	    
 *	PEXDeletePhigsWks()	    -- access via macro in phigspex.h
 *
 *   Section 10.2 -- Phigs Workstation Resource Inquiry
 *	PEXGetWksInfo()		    
 *	PEXGetDynamics()	    
 *	PEXGetViewRep()		    
 *
 *   Section 10.3 -- PHIGS Workstation Manipulation
 *	PEXRedrawAllStructures()    -- access via macro in phigspex.h
 *	PEXUpdateWorkstation()	    -- access via macro in phigspex.h
 *	PEXRedrawClipRegion()	    
 *	PEXExecuteDeferredActions() -- access via macro in phigspex.h
 *	PEXSetViewPriority()	    
 *	PEXSetDisplayUpdateMode()   
 *	PEXMapDCtoWC()		    
 *	PEXMapWCtoDC()		    
 *
 *   Section 10.4 -- PHIGS Workstation Update Requests
 *	PEXSetViewRep()		    
 *	PEXSetWksWindow()	    
 *	PEXSetWksViewport()	    
 *	PEXSetHlhsrMode()	    
 *
 *   Section 10.5 -- Posting/Unposting Structures
 *	PEXPostStructure()	    
 *	PEXUnpostStructure()	    -- access via macro in phigspex.h
 *	PEXUnpostAllStructures()    
 *	PEXGetWksPostings()	    
 */

#include "pex_priv.h"

/*
 * Section 10.1 -- PHIGS Workstation Resource Management
 */

int
PEXCreatePhigsWks(display, wks, drawable, markerBundle, textBundle,
		    lineBundle, interiorBundle, edgeBundle, colourTable,
		    depthCueTable, lightTable, colourApproxTable, 
		    patternTable, textFontTable, highlightIncl, 
		    highlightExcl, invisIncl, invisExcl, bufferMode)
Display	       *display;
pexPhigsWks	wks;
Drawable        drawable;
pexLookupTable	markerBundle, textBundle, lineBundle, interiorBundle, 
		edgeBundle, colourTable, depthCueTable, lightTable, 
		colourApproxTable, patternTable, textFontTable;
pexNameSet	highlightIncl, highlightExcl, invisIncl, invisExcl;
CARD16		bufferMode;
{
    int				 status = 0;
    Pex_srvr_info		*srvr;
    pexCreatePhigsWksReq	*req;
    
    if (srvr = PexEntryCheck(display, 1)) {
	LOCK_DISPLAY(display);
	PEX_REQUEST(CreatePhigsWks, display, PEX_OPCODE(srvr), req);
	req->wks = wks;
	req->drawable = drawable;
	req->markerBundle = markerBundle;
	req->textBundle = textBundle;
	req->lineBundle = lineBundle;
	req->interiorBundle = interiorBundle;
	req->edgeBundle = edgeBundle;
	req->colourTable = colourTable;
	req->depthCueTable = depthCueTable;
	req->lightTable = lightTable;
	req->colourApproxTable = colourApproxTable;
	req->patternTable = patternTable;
	req->textFontTable = textFontTable;
	req->highlightIncl = highlightIncl;
	req->highlightExcl = highlightExcl;
	req->invisIncl = invisIncl;
	req->invisExcl = invisExcl;
	req->bufferMode = bufferMode;
        status = 1;
	UNLOCK_DISPLAY(display);
    }
    PEX_SYNC_HANDLE(display);
    return status;
}


/*
 * Section 10.2 -- PHIGS Workstation Inquiry
 */

int
PEXGetWksInfo(display, wks_id, item_mask, values)
Display		*display;
pexPhigsWks	 wks_id;
pexBitmask	item_mask[PEXMSGetWksInfo];
Pointer		*values;	/* OUT */
{
    int				 status = 0;
    unsigned			 size;
    Pex_srvr_info		*srvr;
    pexGetWksInfoReq		*req;
    pexGetWksInfoReply		 reply;
    
    if (srvr = PexEntryCheck(display, 1)) {
	LOCK_DISPLAY(display);
	PEX_REQUEST(GetWksInfo, display, PEX_OPCODE(srvr), req);
	PEX_FP_FORMAT(req->fpFormat);
	req->wks = wks_id;
	bcopy((char *)item_mask, (char *)req->itemMask, sizeof(req->itemMask));
	
	status = _XReply(display, (xReply *)&reply, 0, xFalse);
	if (status) {
	    if ( (size = reply.length * sizeof(CARD32)) > 0 ) {
		if ( *values = PEX_SCRATCH(srvr, size) )
		    _XRead(display, (char *)*values, (long)size );
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
PEXGetDynamics(display, drawable, reply)
Display		    *display;
Drawable	     drawable;
pexGetDynamicsReply *reply;	/* OUT */
{
    int				 status = 0;
    Pex_srvr_info		*srvr;
    pexGetDynamicsReq		*req;
    
    if (srvr = PexEntryCheck(display, 1)) {
	LOCK_DISPLAY(display);
	PEX_REQUEST(GetDynamics, display, PEX_OPCODE(srvr), req);
	req->drawable = drawable;
	
	status = _XReply(display, (xReply *)reply, 0, xFalse);
	if (status) {
	    ASSERT(reply->length == 0);
	}
	UNLOCK_DISPLAY(display);
    }
    PEX_SYNC_HANDLE(display);
    return status;  
}

int
PEXGetViewRep(display, wks_id, indx, view_update, view_reps)
Display		*display;
pexPhigsWks	 wks_id;
pexTableIndex	 indx;
CARD16		*view_update;	/* OUT */
pexViewRep	*view_reps[];	/* OUT - 1st is requested, 2nd is current */
{
    int				 status = 0;
    unsigned			 size;
    Pex_srvr_info		*srvr;
    pexGetViewRepReq		*req;
    pexGetViewRepReply		 reply;
    
    if (srvr = PexEntryCheck(display, 1)) {
	LOCK_DISPLAY(display);
	PEX_REQUEST(GetViewRep, display, PEX_OPCODE(srvr), req);
	PEX_FP_FORMAT(req->fpFormat);
	req->wks = wks_id;
	req->index = indx;
	
	status = _XReply(display, (xReply *)&reply, 0, xFalse);
	if (status) {
	    *view_update = reply.viewUpdate;
	    if ( (size = reply.length * sizeof(CARD32)) > 0 ) {
		if ( *view_reps = (pexViewRep *)PEX_SCRATCH(srvr, size) )
		    _XRead(display, (char *)*view_reps, (long)size);
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
 * Section 10.3 -- PHIGS Workstation Manipulation
 */

int
PEXRedrawClipRegion(display, wks_id, num_rects, rects)
Display		    *display;
pexPhigsWks	     wks_id;
CARD32		     num_rects;
pexDeviceRect	     rects[];
{
    int				 status = 0;
    Pex_srvr_info		*srvr;
    pexRedrawClipRegionReq	*req;
    Pex_data_vec		vec[1];
    
    if (srvr = PexEntryCheck(display, 1)) {
	LOCK_DISPLAY(display);
	vec[0].size = num_rects * sizeof(pexDeviceRect);
	vec[0].padding = PADDING(vec[0].size);
	vec[0].data = (char *)rects;
	PEX_VAR_REQUEST(RedrawClipRegion, display, PEX_OPCODE(srvr),
	    vec[0].size + vec[0].padding, req);
	req->wks = wks_id;
	req->numRects = num_rects;
	PEX_LOAD_VAR_REQUEST_DATA(display,1,vec)
	UNLOCK_DISPLAY(display);
	status = 1;
    }
    PEX_SYNC_HANDLE(display);
    return status;  
}

int
PEXSetViewPriority(display, wks_id, index1, index2, priority)
Display		    *display;
pexPhigsWks	     wks_id;
pexTableIndex	     index1;
pexTableIndex	     index2;
CARD16		     priority;
{
    int				 status = 0;
    Pex_srvr_info		*srvr;
    pexSetViewPriorityReq	*req;
    
    if (srvr = PexEntryCheck(display, 1)) {
	LOCK_DISPLAY(display);
	PEX_REQUEST(SetViewPriority, display, PEX_OPCODE(srvr), req);
	req->wks = wks_id;
	req->index1 = index1;
	req->index2 = index2;
	req->priority = priority;
	status = 1;
	UNLOCK_DISPLAY(display);
    }
    PEX_SYNC_HANDLE(display);
    return status;
}

int
PEXSetDisplayUpdateMode(display, wks_id, mode)
Display		    *display;
pexPhigsWks	     wks_id;
pexEnumTypeIndex     mode;
{
    int				 status = 0;
    Pex_srvr_info		*srvr;
    pexSetDisplayUpdateModeReq	*req;
    
    if (srvr = PexEntryCheck(display, 1)) {
	LOCK_DISPLAY(display);
	PEX_REQUEST(SetDisplayUpdateMode, display, PEX_OPCODE(srvr), req);
	req->wks = wks_id;
	req->displayUpdate = mode;
	status = 1;
	UNLOCK_DISPLAY(display);
    }
    PEX_SYNC_HANDLE(display);
    return status;
}

int
PEXMapDCtoWC(display, wks_id, num_coords, coords, view_index,
	    num_coords_out, coords_out)
Display		    *display;
pexPhigsWks	    wks_id;
CARD32		    num_coords;
pexDeviceCoord	    *coords;
CARD16		    *view_index;	/* OUT */
CARD32		    *num_coords_out;	/* OUT */
pexCoord3D	    **coords_out;	/* OUT */
{
    int				 status = 0;
    unsigned			 size;
    Pex_srvr_info		*srvr;
    pexMapDCtoWCReq		*req;
    pexMapDCtoWCReply		 reply;
    Pex_data_vec		vec[1];
    
    if (srvr = PexEntryCheck(display, 1)) {
	LOCK_DISPLAY(display);
	vec[0].size = num_coords * sizeof(pexDeviceCoord);
	vec[0].padding = PADDING(vec[0].size);
	vec[0].data = (char *)coords;
	PEX_VAR_REQUEST(MapDCtoWC, display, PEX_OPCODE(srvr),
	    vec[0].size + vec[0].padding, req);
	PEX_FP_FORMAT(req->fpFormat);
	req->wks = wks_id;
	req->numCoords = num_coords;
	PEX_LOAD_VAR_REQUEST_DATA(display,1,vec)
	
	status = _XReply(display, (xReply *)&reply, 0, xFalse);
	if (status) {
	    *view_index = reply.viewIndex;
	    *num_coords_out = reply.numCoords;
	    if ( (size = reply.length * sizeof(CARD32)) > 0 ) {
		if ( *coords_out = (pexCoord3D *)PEX_SCRATCH(srvr, size) )
		    _XRead(display, (char *)*coords_out, (long)size);
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
PEXMapWCtoDC(display, wks_id, view_index, num_coords, coords, 
	    num_coords_out, coords_out)
Display		    *display;
pexPhigsWks	     wks_id;
CARD16		     view_index;
CARD32		     num_coords;
pexCoord3D	     coords[];
CARD32		    *num_coords_out;	/* OUT */
pexDeviceCoord	    *coords_out[];	/* OUT */
{
    int				 status = 0;
    unsigned			 size;
    Pex_srvr_info		*srvr;
    pexMapWCtoDCReq		*req;
    pexMapWCtoDCReply		 reply;
    Pex_data_vec		vec[1];
    
    if (srvr = PexEntryCheck(display, 1)) {
	LOCK_DISPLAY(display);
	vec[0].size = num_coords * sizeof(pexCoord3D);
	vec[0].padding = PADDING(vec[0].size);
	vec[0].data = (char *)coords;
	PEX_VAR_REQUEST(MapWCtoDC, display, PEX_OPCODE(srvr),
	    vec[0].size + vec[0].padding, req);
	PEX_FP_FORMAT(req->fpFormat);
	req->wks = wks_id;
	req->index = view_index;
	req->numCoords = num_coords;
	PEX_LOAD_VAR_REQUEST_DATA(display,1,vec)

	status = _XReply(display, (xReply *)&reply, 0, xFalse);
	if (status) {
	    *num_coords_out = reply.numCoords;
	    if ( (size = reply.length * sizeof(CARD32)) > 0 ) {
		if (*coords_out = (pexDeviceCoord *)PEX_SCRATCH(srvr, size))
		    _XRead(display, (char *)*coords_out, (long)size);
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
 * Section 10.4 -- PHIGS Workstation Update Requests
 */

int
PEXSetViewRep(display, wks_id, view_rep)
Display		    *display;
pexPhigsWks	     wks_id;
pexViewRep	    *view_rep;
{
    int				 status = 0;
    Pex_srvr_info		*srvr;
    pexSetViewRepReq		*req;
    
    if (srvr = PexEntryCheck(display, 1)) {
	LOCK_DISPLAY(display);
	PEX_REQUEST(SetViewRep, display, PEX_OPCODE(srvr), req);
	PEX_FP_FORMAT(req->fpFormat);
	req->wks = wks_id;
	PEX_ASSIGN(req->viewRep, *view_rep);
	status = 1;
	UNLOCK_DISPLAY(display);
    }
    PEX_SYNC_HANDLE(display);
    return status;
}

int
PEXSetWksWindow(display, wks_id, npc_subvolume)
Display		    *display;
pexPhigsWks	     wks_id;
pexNpcSubvolume	    *npc_subvolume;
{
    int				 status = 0;
    Pex_srvr_info		*srvr;
    pexSetWksWindowReq		*req;
    
    if (srvr = PexEntryCheck(display, 1)) {
	LOCK_DISPLAY(display);
	PEX_REQUEST(SetWksWindow, display, PEX_OPCODE(srvr), req);
	PEX_FP_FORMAT(req->fpFormat);
	req->wks = wks_id;
	PEX_ASSIGN(req->npcSubvolume, *npc_subvolume);
	status = 1;
	UNLOCK_DISPLAY(display);
    }
    PEX_SYNC_HANDLE(display);
    return status;
}

int
PEXSetWksViewport(display, wks_id, viewport)
Display		    *display;
pexPhigsWks	     wks_id;
pexViewport	    *viewport;
{
    int				 status = 0;
    Pex_srvr_info		*srvr;
    pexSetWksViewportReq	*req;
    
    if (srvr = PexEntryCheck(display, 1)) {
	LOCK_DISPLAY(display);
	PEX_REQUEST(SetWksViewport, display, PEX_OPCODE(srvr), req);
	PEX_FP_FORMAT(req->fpFormat);
	req->wks = wks_id;
	PEX_ASSIGN(req->viewport, *viewport);
	status = 1;
	UNLOCK_DISPLAY(display);
    }
    PEX_SYNC_HANDLE(display);
    return status;
}

int
PEXSetHlhsrMode(display, wks_id, mode)
Display		    *display;
pexPhigsWks	     wks_id;
pexEnumTypeIndex     mode;
{
    int				 status = 0;
    Pex_srvr_info		*srvr;
    pexSetHlhsrModeReq		*req;
    
    if (srvr = PexEntryCheck(display, 1)) {
	LOCK_DISPLAY(display);
	PEX_REQUEST(SetHlhsrMode, display, PEX_OPCODE(srvr), req);
	req->wks = wks_id;
	req->mode = mode;
	status = 1;
	UNLOCK_DISPLAY(display);
    }
    PEX_SYNC_HANDLE(display);
    return status;
}



/*
 * Section 10.5 -- Posting/Unposting Structures
 */

int
PEXPostStructure(display, wks_id, struct_id, priority)
Display	       *display;
pexPhigsWks	wks_id;
pexStructure	struct_id;
PEXFLOAT		priority;
{
    int				 status = 0;
    Pex_srvr_info		*srvr;
    pexPostStructureReq		*req;
    
    if (srvr = PexEntryCheck(display, 1)) {
	LOCK_DISPLAY(display);
	PEX_REQUEST(PostStructure, display, PEX_OPCODE(srvr), req);
	PEX_FP_FORMAT(req->fpFormat);
	req->wks = wks_id;
	req->sid = struct_id;
	req->priority = priority;
	status = 1;
	UNLOCK_DISPLAY(display);
    }
    PEX_SYNC_HANDLE(display);
    return status;
}


int
PEXUnpostStructure(display, wks_id, struct_id)
Display	       *display;
pexPhigsWks	wks_id;
pexStructure	struct_id;
{
    int				 status = 0;
    Pex_srvr_info		*srvr;
    pexUnpostStructureReq	*req;
    
    if (srvr = PexEntryCheck(display, 1)) {
	LOCK_DISPLAY(display);
	PEX_REQUEST(UnpostStructure, display, PEX_OPCODE(srvr), req);
	req->wks = wks_id;
	req->sid = struct_id;
	status = 1;
	UNLOCK_DISPLAY(display);
    }
    PEX_SYNC_HANDLE(display);
    return status;
}

int
PEXGetWksPostings(display, struct_id, num_wks, wks_ids)
Display		*display;
pexStructure	 struct_id;
CARD32		*num_wks;	/* OUT */
pexPhigsWks	*wks_ids[];	/* OUT */
{
    int				 status = 0;
    unsigned			 size;
    Pex_srvr_info		*srvr;
    pexGetWksPostingsReq	*req;
    pexGetWksPostingsReply	 reply;
    
    if (srvr = PexEntryCheck(display, 1)) {
	LOCK_DISPLAY(display);
	PEX_RESOURCE_ID_REQUEST(GetWksPostings, display, 
	    PEX_OPCODE(srvr), struct_id, req);
	
	status = _XReply(display, (xReply *)&reply, 0, xFalse);
	if (status) {
	    *num_wks = reply.length;
	    if( (size = reply.length * sizeof(CARD32)) > 0 ) {
		if (*wks_ids = (pexPhigsWks *)PEX_SCRATCH(srvr, size))
		    _XRead(display, (char *)*wks_ids, (long)size);
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

