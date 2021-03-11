/* $XConsortium: pex_lut.c,v 5.2 94/04/17 20:42:10 rws Exp $ */

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
 *   PEX Lookup Table functions:
 *
 *   Section 4.1 -- Lookup Table Resource Management
 *	PEXCreateLookupTable()	    
 *	PEXCopyLookupTable()	    
 *	PEXDeleteLookupTable()	    -- access via macro in phigspex.h
 *
 *   Section 4.2 -- Lookup Table Inquiry
 *	PEXGetTableInfo()	    
 *	PEXGetPredefinedEntries()   
 *	PEXGetDefinedIndices()	    
 *	PEXGetTableEntry()	    
 *	PEXGetTableEntries()	    
 *
 *   Section 4.3 -- Lookup Table Modification
 *	PEXSetTableEntries()	    
 *	PEXDeleteTableEntries()	    
 */

#include "pex_priv.h"

/*
 *   Section 4.1 -- Lookup Table Resource Management
 */
 
int
PEXCreateLookupTable(display, drawable, lut_id, table_type)
Display	       *display;
Drawable        drawable;
pexLookupTable	lut_id;
pexTableType	table_type;
{
    int				 status = 0;
    Pex_srvr_info		*srvr;
    pexCreateLookupTableReq	*req;
    
    if (srvr = PexEntryCheck(display, 1)) {
	LOCK_DISPLAY(display);
	PEX_REQUEST(CreateLookupTable, display, PEX_OPCODE(srvr), req);
	req->drawableExample = drawable;
	req->lut = lut_id;
	req->tableType = table_type;
	status = 1;
	UNLOCK_DISPLAY(display);
    }
    PEX_SYNC_HANDLE(display);
    return status;
}

int
PEXCopyLookupTable(display, src_lut, dst_lut)
Display	       *display;
pexLookupTable	src_lut;
pexLookupTable	dst_lut;
{
    int				 status = 0;
    Pex_srvr_info		*srvr;
    pexCopyLookupTableReq	*req;
    
    if (srvr = PexEntryCheck(display, 1)) {
	LOCK_DISPLAY(display);
	PEX_REQUEST(CopyLookupTable, display, PEX_OPCODE(srvr), req);
	req->src = src_lut;
	req->dst = dst_lut;
	status = 1;
	UNLOCK_DISPLAY(display);
    }
    PEX_SYNC_HANDLE(display);
    return status;
}


/*
 *   Section 4.2 -- Lookup Table Inquiry
 */

int
PEXGetTableInfo(display, drawable, table_type, definable_entries,
	num_predefined, predefined_min, predefined_max)
Display		*display;
Drawable	 drawable;
pexTableType	 table_type;
CARD16		*definable_entries;
CARD16		*num_predefined;
INT16		*predefined_min;
INT16		*predefined_max;
{
    int				 status = 0;
    Pex_srvr_info		*srvr;
    pexGetTableInfoReq		*req;
    pexGetTableInfoReply	 reply;
    
    if (srvr = PexEntryCheck(display, 1)) {
	LOCK_DISPLAY(display);
	PEX_REQUEST(GetTableInfo, display, PEX_OPCODE(srvr), req);
	req->drawableExample = drawable;
	req->tableType = table_type;
	
	status = _XReply(display, (xReply *)&reply, 0, xTrue);
	if (status) {
	    *definable_entries = reply.definableEntries;
	    *num_predefined = reply.numPredefined;
	    *predefined_min = reply.predefinedMin;
	    *predefined_max = reply.predefinedMax;
	}
	UNLOCK_DISPLAY(display);
    }
    PEX_SYNC_HANDLE(display);
    return status;    	
}

int
PEXGetPredefinedEntries(display, drawable, table_type, start, count,
			entries, num_entries)
Display		*display;
Drawable	 drawable;
pexTableType	 table_type;
pexTableIndex	 start;
CARD16		 count;
Pointer		*entries;	/* OUT */
CARD16		*num_entries;	/* OUT */
{
    int				 status = 0;
    unsigned			 size;
    Pex_srvr_info		*srvr;
    pexGetPredefinedEntriesReq	*req;
    pexGetPredefinedEntriesReply reply;
    
    if (srvr = PexEntryCheck(display, 1)) {
	LOCK_DISPLAY(display);
	PEX_REQUEST(GetPredefinedEntries, display, PEX_OPCODE(srvr), req);
	PEX_FP_FORMAT(req->fpFormat);
	req->drawableExample = drawable;
	req->tableType = table_type;
	req->start = start;
	req->count = count;
	
	status = _XReply(display, (xReply *)&reply, 0, xFalse);
	if (status) {
	    *num_entries = reply.numEntries;
	    if ( (size = reply.length * sizeof(CARD32)) > 0 ) {
		if (*entries = PEX_SCRATCH(srvr, size) )
		    _XRead(display, (char *)*entries, (long)size);
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
PEXGetDefinedIndices(display, lut_id, num_indices, defined_indices)
Display		*display;
pexLookupTable	 lut_id;
CARD32		*num_indices;		    /* OUT */
pexTableIndex	*defined_indices[];	    /* OUT */
{
    int				 status = 0;
    unsigned			 size;
    Pex_srvr_info		*srvr;
    pexGetDefinedIndicesReq	*req;
    pexGetDefinedIndicesReply	 reply;
    
    if (srvr = PexEntryCheck(display, 1)) {
	LOCK_DISPLAY(display);
	PEX_RESOURCE_ID_REQUEST(GetDefinedIndices, display, PEX_OPCODE(srvr),
	    lut_id, req);
	
	status = _XReply(display, (xReply *)&reply, 0, xFalse);
	if (status) {
	    *num_indices = reply.numIndices;
	    if ( (size = reply.length * sizeof(CARD32)) > 0 ) {
		if ( *defined_indices =
			(pexTableIndex *)PEX_SCRATCH(srvr, size) )
		    _XRead(display, (char *)*defined_indices, (long)size);
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
PEXGetTableEntry(display, lut_id, indx, value_type, table_type, entry_status, 
	table_entry)
Display		*display;
pexLookupTable	 lut_id;
pexTableIndex	 indx;
CARD16		value_type;	/* SET/REALIZED */
CARD16		*table_type;	/* OUT */
CARD16		*entry_status;	/* OUT */
Pointer		*table_entry;	/* OUT */
{
    int				 status = 0;
    unsigned			 size;
    Pex_srvr_info		*srvr;
    pexGetTableEntryReq		*req;
    pexGetTableEntryReply	 reply;
    
    if (srvr = PexEntryCheck(display, 1)) {
	LOCK_DISPLAY(display);
	PEX_REQUEST(GetTableEntry, display, PEX_OPCODE(srvr), req);
	PEX_FP_FORMAT(req->fpFormat);
	req->lut = lut_id;
	req->index = indx;
	req->valueType = value_type;
	
	status = _XReply(display, (xReply *)&reply, 0, xFalse);
	if (status) {
	    *table_type = reply.tableType;
	    *entry_status = reply.status;
	    if ( (size = reply.length * sizeof(CARD32)) > 0 ) {
		if ( *table_entry = PEX_SCRATCH(srvr, size) )
		    _XRead(display, (char *)*table_entry, (long)size);
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
PEXGetTableEntries(display, lut_id, start, count, value_type, table_type,
    num_entries, table_entries)
Display		*display;
pexLookupTable	 lut_id;
pexTableIndex	 start;
CARD16		 count;
CARD16		value_type;	/* SET/REALIZED */
CARD16		*table_type;	/* OUT */
CARD32		*num_entries;	/* OUT */
Pointer		*table_entries;	/* OUT */
{
    int				 status = 0;
    unsigned			 size;
    Pex_srvr_info		*srvr;
    pexGetTableEntriesReq	*req;
    pexGetTableEntriesReply	 reply;
    
    if (srvr = PexEntryCheck(display, 1)) {
	LOCK_DISPLAY(display);
	PEX_REQUEST(GetTableEntries, display, PEX_OPCODE(srvr), req);
	PEX_FP_FORMAT(req->fpFormat);
	req->lut = lut_id;
	req->start = start;
	req->count = count;
	req->valueType = value_type;
	
	status = _XReply(display, (xReply *)&reply, 0, xFalse);
	if (status) {
	    *table_type = reply.tableType;
	    *num_entries = reply.numEntries;
	    if ( (size = reply.length * sizeof(CARD32)) > 0 ) {
		if (*table_entries = PEX_SCRATCH(srvr, size) )
		    _XRead(display, (char *)*table_entries, (long)size);
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
 *   Section 4.3 -- Lookup Table Modification
 */

/* Caller needs to compose 'entries', and supply it's length */
int
PEXSetTableEntries(display, lut, start, count, length_of_entries, entries)
Display		*display;
pexLookupTable	 lut;
pexTableIndex	 start;
CARD16		 count;
CARD32		 length_of_entries; /* size of 'entries' in bytes */
Pointer		 entries;
{
    int				 status = 0;
    Pex_srvr_info		*srvr;
    pexSetTableEntriesReq	*req;
    Pex_data_vec		vec[1];
    
    if (srvr = PexEntryCheck(display, 1)) {
	LOCK_DISPLAY(display);
	vec[0].size = length_of_entries;
	vec[0].padding = PADDING(vec[0].size);
	vec[0].data = (char *)entries;
	PEX_VAR_REQUEST(SetTableEntries, display, PEX_OPCODE(srvr),
	    vec[0].size + vec[0].padding, req);
	PEX_FP_FORMAT(req->fpFormat);
	req->lut = lut;
	req->start = start;
	req->count = count;
	PEX_LOAD_VAR_REQUEST_DATA(display,1,vec)
	status = 1;
	UNLOCK_DISPLAY(display);
    }
    PEX_SYNC_HANDLE(display);
    return status;
}

int
PEXDeleteTableEntries(display, lut_id, start, count)
Display	       *display;
pexLookupTable	lut_id;
pexTableIndex	start;
CARD16		count;
{
    int				 status = 0;
    Pex_srvr_info		*srvr;
    pexDeleteTableEntriesReq	*req;
    
    if (srvr = PexEntryCheck(display, 1)) {
	LOCK_DISPLAY(display);
	PEX_REQUEST(DeleteTableEntries, display, PEX_OPCODE(srvr), req);
	req->lut = lut_id;
	req->start = start;
	req->count = count;
	status = 1;
	UNLOCK_DISPLAY(display);
    }
    PEX_SYNC_HANDLE(display);
    return status;
}
