/* $XConsortium: pex_stru.c,v 5.2 94/04/17 20:42:17 rws Exp $ */

/***********************************************************

Copyright (c) 1989,1990, 1991  X Consortium

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

Copyright 1989,1990, 1991 by Sun Microsystems, Inc. 

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
 *   PEX Structures functions
 *
 *   Section 7.1 -- Structure Resource Management
 *	PEXCreateStructure()		-- access via macro in phigspex.h
 *	PEXCopyStructure()		
 *	PEXDestroyStructures()		
 *	
 *   Section 7.2 -- Structure Inquiry
 *	PEXGetStructureInfo()		
 *	PEXGetElementInfo()		
 *	PEXGetStructuresInNetwork()	
 *	PEXGetAncestors()		
 *	PEXGetDescendants()		
 *	PEXFetchElements()		
 *
 *   Section 7.3 -- Structure Resource Attribute Modification
 *	PEXSetEditingMode()		
 *	PEXSetElementPointer()		
 *	PEXSetElementPointerAtLabel()	
 *	PEXElementSearch()		
 *
 *   Section 7.4 -- Structure Editing
 *	PEXStoreElements()		
 *	PEXDeleteElements()		
 *	PEXDeleteElementsToLabel()	
 *	PEXDeleteElementsBetweenLabels()
 *	PEXCopyElements()		
 *	PEXChangeStructureRefs()	
 */
 

#include "pex_priv.h"

/* 
 *   Section 7.1 -- Structure Resource Management
 */

int
PEXCopyStructure(display, src, dst)
Display		    *display;
pexStructure	     src;
pexStructure	     dst;
{
    int				 status = 0;
    Pex_srvr_info		*srvr;
    pexCopyStructureReq		*req;
    
    if (srvr = PexEntryCheck(display, 1)) {
	LOCK_DISPLAY(display);
	PEX_REQUEST(CopyStructure, display, PEX_OPCODE(srvr), req);
	req->src = src;
	req->dst = dst;
	status = 1;
	UNLOCK_DISPLAY(display);
    }
    PEX_SYNC_HANDLE(display);
    return status;
}


int
PEXDestroyStructures( display, sids, num_ids )
Display		*display;
pexStructure	*sids;
int		 num_ids;
{
    int				status = 0;
    Pex_srvr_info		*srvr;
    pexDestroyStructuresReq	*req;
    Pex_data_vec		vec[1];

    if ( srvr = PexEntryCheck( display, 1 ) ) {
	LOCK_DISPLAY(display);
	vec[0].size = num_ids * sizeof(*sids);
	vec[0].padding = PADDING(vec[0].size);
	vec[0].data = (char *)sids;
	PEX_VAR_REQUEST(DestroyStructures, display, PEX_OPCODE(srvr),
	    vec[0].size + vec[0].padding, req);
	req->numStructures = num_ids;
	PEX_LOAD_VAR_REQUEST_DATA(display,1,vec)
	status = 1;
	UNLOCK_DISPLAY(display);
    }
    PEX_SYNC_HANDLE( display );
    return status;
}


/*
 *   Section 7.2 -- Structure Inquiry
 */
int
PEXGetStructureInfo( display, sid, e_mode, el_ptr, num_els, length, has_refs )
Display		*display;
pexStructure 	 sid;
CARD16		*e_mode;
CARD32		*el_ptr;
CARD32		*num_els;
CARD32		*length;
CARD16		*has_refs;
{
    int				status = 0;
    Pex_srvr_info		*srvr;
    pexGetStructureInfoReq	*req;
    pexGetStructureInfoReply	reply;

    if ( srvr = PexEntryCheck( display, 1 ) ) {
	LOCK_DISPLAY(display);
	PEX_REQUEST(GetStructureInfo, display, PEX_OPCODE(srvr), req);
	PEX_FP_FORMAT(req->fpFormat);
	req->sid = sid;
	req->itemMask = 0;
	if ( e_mode ) req->itemMask |= PEXEditMode;
	if ( el_ptr ) req->itemMask |= PEXElementPtr;
	if ( num_els ) req->itemMask |= PEXNumElements;
	if ( length ) req->itemMask |= PEXLengthStructure;
	if ( has_refs ) req->itemMask |= PEXHasRefs;
	status = _XReply( display, (xReply *)&reply, 0, xTrue );
	if( status ) {
	    if ( e_mode ) *e_mode = reply.editMode;
	    if ( el_ptr ) *el_ptr = reply.elementPtr;
	    if ( num_els ) *num_els = reply.numElements;
	    if ( length ) *length = reply.lengthStructure;
	    if ( has_refs ) *has_refs = reply.hasRefs;
	}
	UNLOCK_DISPLAY(display);
    }
    PEX_SYNC_HANDLE( display );
    return status;
}

int
PEXGetElementInfo( display, sid, w1, o1, w2, o2, lst, num_els )
Display		*display;
pexStructure	 sid;
CARD16		 w1, w2;
INT32		 o1, o2;
pexElementInfo **lst;
CARD32		*num_els;
{
    int				status = 0;
    unsigned			size;
    Pex_srvr_info		*srvr;
    pexGetElementInfoReq	*req;
    pexGetElementInfoReply	reply;

    if ( srvr = PexEntryCheck( display, 1 ) ) {
	LOCK_DISPLAY(display);
	PEX_REQUEST(GetElementInfo, display, PEX_OPCODE(srvr), req);
	PEX_FP_FORMAT(req->fpFormat);
	req->sid = sid;
	req->range.position1.whence = w1;
	req->range.position1.offset = o1;
	req->range.position2.whence = w2;
	req->range.position2.offset = o2;
	status = _XReply( display, (xReply *)&reply, 0, xFalse );
	if ( status ) {
	    *num_els = reply.numInfo;
	    if ( (size = reply.length * sizeof(CARD32)) > 0 ) {
		if (*lst = (pexElementInfo *)PEX_SCRATCH(srvr, size)) {
		    _XRead( display, (char *)*lst, (long)size);
		} else {
		    status = 0;
		    PexClearReply(display, reply.length);
		}
	    }
	}
	UNLOCK_DISPLAY(display);
    }
    PEX_SYNC_HANDLE( display );
    return status;
}

int
PEXGetStructuresInNetwork( display, sid, which, ids, count )
Display		*display;
pexStructure	 sid;
int		 which;
pexStructure   **ids;
int		*count;
{
    int					status = 0;
    unsigned				size;
    Pex_srvr_info			*srvr;
    pexGetStructuresInNetworkReq	*req;
    pexGetStructuresInNetworkReply	reply;

    *count = 0;
    if ( srvr = PexEntryCheck( display, 1 ) ) {
	LOCK_DISPLAY(display);
	PEX_REQUEST(GetStructuresInNetwork, display, PEX_OPCODE(srvr), req);
	req->sid = sid;
	req->which = which;
	status = _XReply( display, (xReply *)&reply, 0, xFalse );
	if( status ) {
	    *count = reply.numStructures;
	    if ( (size = reply.length * sizeof(CARD32)) > 0 ) {
		if (*ids = (pexStructure *)PEX_SCRATCH(srvr, size) )
		    _XRead( display, (char *)*ids, (long)size);
		else {
		    status = 0;
		    PexClearReply(display, reply.length);
		}
	    }
	}
	UNLOCK_DISPLAY(display);
    }
    PEX_SYNC_HANDLE( display );
    return status;
}

int
PEXGetAncestors(display, sid, pathOrder, pathDepth, numPaths, paths)
Display		*display;
pexStructure	 sid;
CARD16		 pathOrder;
CARD32		 pathDepth;
CARD32		*numPaths;	/* OUT */
Pointer		*paths;		/* OUT */
{
    int				 status = 0;
    unsigned			 size;
    Pex_srvr_info		*srvr;
    pexGetAncestorsReq		*req;
    pexGetAncestorsReply	 reply;
    
    if (srvr = PexEntryCheck(display, 1)) {
	LOCK_DISPLAY(display);
	PEX_REQUEST(GetAncestors, display, PEX_OPCODE(srvr), req);
	req->sid = sid;
	req->pathOrder = pathOrder;
	req->pathDepth = pathDepth;

	status = _XReply(display, (xReply *)&reply, 0, xFalse);
	if (status) {
	    *numPaths = reply.numPaths;
	    if ( (size = reply.length * sizeof(CARD32)) > 0 ) {
		if (*paths = PEX_SCRATCH(srvr, size))
		    _XRead(display, (char *)*paths, (long)size);
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
PEXGetDescendants(display, sid, pathOrder, pathDepth, numPaths, paths)
Display		*display;
pexStructure	 sid;
CARD16		 pathOrder;
CARD32		 pathDepth;
CARD32		*numPaths;	/* OUT */
Pointer		*paths;		/* OUT */
{
    int				 status = 0;
    unsigned			 size;
    Pex_srvr_info		*srvr;
    pexGetDescendantsReq	*req;
    pexGetDescendantsReply	 reply;
    
    if (srvr = PexEntryCheck(display, 1)) {
	LOCK_DISPLAY(display);
	PEX_REQUEST(GetDescendants, display, PEX_OPCODE(srvr), req);
	req->sid = sid;
	req->pathOrder = pathOrder;
	req->pathDepth = pathDepth;

	status = _XReply(display, (xReply *)&reply, 0, xFalse);
	if (status) {
	    *numPaths = reply.numPaths;
	    if ( (size = reply.length * sizeof(CARD32) ) > 0 ) {
		if (*paths = PEX_SCRATCH(srvr, size))
		    _XRead(display, (char *)*paths, (long)size);
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
PEXFetchElements(display, sid, w1, o1, w2, o2, el_data, num_els)
Display		*display;
pexStructure	 sid;
CARD16		 w1, w2;
INT32		 o1, o2;
char		**el_data;
CARD32		*num_els;
{
    int				status = 0;
    unsigned			size;
    Pex_srvr_info		*srvr;
    pexFetchElementsReq		*req;
    pexFetchElementsReply	reply;

    if ( srvr = PexEntryCheck( display, 1 ) ) {
	LOCK_DISPLAY(display);
	PEX_REQUEST(FetchElements, display, PEX_OPCODE(srvr), req);
	PEX_FP_FORMAT(req->fpFormat);
	req->sid = sid;
	req->range.position1.whence = w1;
	req->range.position1.offset = o1;
	req->range.position2.whence = w2;
	req->range.position2.offset = o2;
    	status = _XReply( display, (xReply *)&reply, 0, xFalse );
	if ( status ) {
	    *el_data = NULL;
	    *num_els = reply.numElements;
	    if ( (size = reply.length * sizeof(CARD32)) > 0 ) {
		if ( *el_data = (char *)PEX_SCRATCH(srvr, size) )
		    _XRead(display, (char *)*el_data, (long)size);
		else {
		    status = 0;
		    PexClearReply(display, reply.length);
		}
	    }
	}
	UNLOCK_DISPLAY(display);
    }
    PEX_SYNC_HANDLE( display );
    return status;
}


/*
 *   Section 7.3 -- Structure Resource Attribute Modification
 */

int
PEXSetEditingMode( display, sid, mode )
Display		*display;
pexStructure	 sid;
CARD16		 mode;
{
    int				status = 0;
    Pex_srvr_info		*srvr;
    pexSetEditingModeReq	*req;

    if ( srvr = PexEntryCheck( display, 1 ) ) {
	LOCK_DISPLAY(display);
	PEX_REQUEST(SetEditingMode, display, PEX_OPCODE(srvr), req);
	req->sid = sid;
	req->mode = mode;
	status = 1;
	UNLOCK_DISPLAY(display);
    }
    PEX_SYNC_HANDLE( display );
    return status;
}

int
PEXSetElementPointer( display, sid, whence, offset )
Display		*display;
pexStructure	 sid;
CARD16		 whence;
INT32		 offset;
{
    int				status = 0;
    Pex_srvr_info		*srvr;
    pexSetElementPointerReq	*req;

    if ( srvr = PexEntryCheck( display, 1 ) ) {
	LOCK_DISPLAY(display);
	PEX_REQUEST(SetElementPointer, display, PEX_OPCODE(srvr), req);
	req->sid = sid;
	req->position.whence = whence;
	req->position.offset = offset;
	status = 1;
	UNLOCK_DISPLAY(display);
    }
    PEX_SYNC_HANDLE( display );
    return status;
}

int
PEXSetElementPointerAtLabel(display, sid, label, offset)
Display		*display;
pexStructure	 sid;
INT32		 label;
INT32		 offset;
{
    int					status = 0;
    Pex_srvr_info			*srvr;
    pexSetElementPointerAtLabelReq	*req;

    if ( srvr = PexEntryCheck( display, 1 ) ) {
	LOCK_DISPLAY(display);
	PEX_REQUEST(SetElementPointerAtLabel, display, PEX_OPCODE(srvr), req);
	req->sid = sid;
	req->label = label;
	req->offset = offset;
	status = 1;
	UNLOCK_DISPLAY(display);
    }
    PEX_SYNC_HANDLE( display );
    return status;
}

int
PEXElementSearch(display, struct_id, whence, offset, direction, num_incls,
	num_excls, incl_list, excl_list, srch_status, found_offset)
Display		    *display;
pexStructure	     struct_id;
CARD16		     whence;
INT32		     offset;
CARD32		     direction;
CARD32		     num_incls;
CARD32		     num_excls;
CARD16		     incl_list[];
CARD16		     excl_list[];
CARD16		    *srch_status;   /* OUT */
CARD32		    *found_offset;  /* OUT */
{
    int				 status = 0, abytes;
    Pex_srvr_info		*srvr;
    pexElementSearchReq		*req;
    pexElementSearchReply	 reply;
    Pex_data_vec		vec[2];

    if (srvr = PexEntryCheck(display, 1)) {
	LOCK_DISPLAY(display);
	vec[0].size = num_incls*sizeof(CARD16);
	vec[0].padding =  PADDING(vec[0].size);
	vec[0].data = (char *)incl_list;
	vec[1].size = num_excls*sizeof(CARD16);
	vec[1].padding =  PADDING(vec[1].size);
	vec[1].data = (char *)excl_list;
	abytes = vec[0].size + vec[0].padding + vec[1].size + vec[1].padding;
	abytes = num_incls*sizeof(CARD16) + PADDING(num_incls*sizeof(CARD16)) +
		 num_excls*sizeof(CARD16) + PADDING(num_excls*sizeof(CARD16));
	PEX_VAR_REQUEST(ElementSearch, display, PEX_OPCODE(srvr), abytes, req);
	req->sid = struct_id;
	req->position.whence = whence;
	req->position.offset = offset;
	req->direction = direction;
	req->numIncls = num_incls;
	req->numExcls = num_excls;
	PEX_LOAD_VAR_REQUEST_DATA(display,2,vec)
	status = _XReply(display, (xReply *)&reply, 0, xFalse);
	if (status) {
	    ASSERT(reply.length == 0);
	    *srch_status = reply.status;
	    *found_offset = reply.foundOffset;
	}
	UNLOCK_DISPLAY(display);
    }
    PEX_SYNC_HANDLE(display);
    return status;
}



/*
 *   Section 7.4 -- Structure Editing
 */

int
PEXStoreElements(display, sid, num_ocs, length_of_oc_list, oc_list)
Display		   *display;
pexStructure	    sid;
CARD32		    num_ocs;
CARD32		    length_of_oc_list; /* of list in bytes */
char		    *oc_list;
{
    int				status = 0;
    Pex_srvr_info		*srvr;
    pexStoreElementsReq		*req;
    Pex_data_vec		vec[1];

    if ( srvr = PexEntryCheck(display, 1) ) {
	LOCK_DISPLAY(display);
	vec[0].size = length_of_oc_list;
	vec[0].padding = PADDING(vec[0].size);
	vec[0].data = (char *)oc_list;
	PEX_VAR_REQUEST(StoreElements, display, PEX_OPCODE(srvr),
	    vec[0].size + vec[0].padding, req);
	PEX_FP_FORMAT(req->fpFormat);
	req->sid = sid;
	req->numCommands = num_ocs;
	PEX_LOAD_VAR_REQUEST_DATA(display,1,vec)
	status = 1;
	UNLOCK_DISPLAY(display);
    }
    PEX_SYNC_HANDLE( display );
    return status;
}

int
PEXDeleteElements(display, sid, w1, o1, w2, o2)
Display		*display;
pexStructure	 sid;
CARD16		 w1, w2;
INT32		 o1, o2;
{
    int				status = 0;
    Pex_srvr_info		*srvr;
    pexDeleteElementsReq	*req;

    if ( srvr = PexEntryCheck( display, 1 ) ) {
	LOCK_DISPLAY(display);
	PEX_REQUEST(DeleteElements, display, PEX_OPCODE(srvr), req);
	req->sid = sid;
	req->range.position1.whence = w1;
	req->range.position1.offset = o1;
	req->range.position2.whence = w2;
	req->range.position2.offset = o2;
	status = 1;
	UNLOCK_DISPLAY(display);
    }
    PEX_SYNC_HANDLE( display );
    return status;
}


int
PEXDeleteElementsToLabel(display, sid, whence, offset, label)
Display		    *display;
pexStructure	     sid;
CARD16		     whence;
INT32		     offset;
INT32                label;
{
    int				 status = 0;
    Pex_srvr_info		*srvr;
    pexDeleteElementsToLabelReq	*req;
    
    if (srvr = PexEntryCheck(display, 1)) {
	LOCK_DISPLAY(display);
	PEX_REQUEST(DeleteElementsToLabel, display, PEX_OPCODE(srvr), req);
	req->sid = sid;
	req->label = label;
	req->position.whence = whence;
	req->position.offset = offset;
	status = 1;
	UNLOCK_DISPLAY(display);
    }
    PEX_SYNC_HANDLE(display);
    return status;
}

int
PEXDeleteBetweenLabels( display, sid, label1, label2 )
Display		*display;
pexStructure	 sid;
INT32		 label1, label2;
{
    int				status = 0;
    Pex_srvr_info		*srvr;
    pexDeleteBetweenLabelsReq	*req;

    if ( srvr = PexEntryCheck( display, 1 ) ) {
	LOCK_DISPLAY(display);
	PEX_REQUEST(DeleteBetweenLabels, display, PEX_OPCODE(srvr), req);
	req->sid = sid;
	req->label1 = label1;
	req->label2 = label2;
	status = 1;
	UNLOCK_DISPLAY(display);
    }
    PEX_SYNC_HANDLE( display );
    return status;
}

int
PEXCopyElements( display, src_sid, w1, o1, w2, o2, dest_sid, whence, offset )
Display	*display;
pexStructure		src_sid;
CARD16			w1, w2, whence;
INT32			o1, o2, offset;
pexStructure		dest_sid;
{
    int				status = 0;
    Pex_srvr_info		*srvr;
    pexCopyElementsReq		*req;

    if ( srvr = PexEntryCheck( display, 1 ) ) {
	LOCK_DISPLAY(display);
	PEX_REQUEST(CopyElements, display, PEX_OPCODE(srvr), req);
	req->src = src_sid;
	req->srcRange.position1.whence = w1;
	req->srcRange.position1.offset = o1;
	req->srcRange.position2.whence = w2;
	req->srcRange.position2.offset = o2;
	req->dst = dest_sid;
	req->dstPosition.whence = whence;
	req->dstPosition.offset = offset;
	status = 1;
	UNLOCK_DISPLAY(display);
    }
    PEX_SYNC_HANDLE( display );
    return status;
}


int
PEXChangeStructureRefs(display, old_id, new_id)
Display		    *display;
pexStructure	     old_id;
pexStructure	     new_id;
{
    int				 status = 0;
    Pex_srvr_info		*srvr;
    pexChangeStructureRefsReq	*req;
    
    if (srvr = PexEntryCheck(display, 1)) {
	LOCK_DISPLAY(display);
	PEX_REQUEST(ChangeStructureRefs, display, PEX_OPCODE(srvr), req);
	req->old_id = old_id;
	req->new_id = new_id;
	status = 1;
	UNLOCK_DISPLAY(display);
    }
    PEX_SYNC_HANDLE(display);
    return status;
}

