/* $XConsortium: pex_info.c,v 5.2 94/04/17 20:42:10 rws Exp $ */

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
 *   PEX Extension Information functions
 *
 *   Section 2.7 -- Extension Information
 *	PEXGetExtensionInfo()		
 *	PEXGetEnumeratedTypeInfo()	
 *	PEXGetImpDepConstants()		
 */
 
#include "pex_priv.h"


int
PEXGetExtensionInfo( display, cproto_major, cproto_minor, proto_major,
    proto_minor, vendor, release_number, subset_info )
    Display		*display;
    CARD16		cproto_major;
    CARD16		cproto_minor;
    CARD16		*proto_major;
    CARD16		*proto_minor;
    char		**vendor;
    CARD32		*release_number;
    CARD32		*subset_info;
{
    int				status = 0;
    unsigned			size;
    Pex_srvr_info		*srvr;
    pexGetExtensionInfoReq	*req;
    pexGetExtensionInfoReply	reply;

    if ( srvr = PexEntryCheck( display, 1 ) ) {
	LOCK_DISPLAY(display);
	PEX_REQUEST(GetExtensionInfo, display, PEX_OPCODE(srvr), req);
	req->clientProtocolMajor = cproto_major;
	req->clientProtocolMinor = cproto_minor;
	status = _XReply( display, (xReply *)&reply, 0, xFalse );
	if( status ) {
	    *proto_major = reply.majorVersion;
	    *proto_minor = reply.minorVersion;
	    *release_number = reply.release;
	    *subset_info = reply.subsetInfo;
	    size = MAX(reply.lengthName+1, reply.length*sizeof(CARD32));
	    if ( *vendor = Xmalloc( size ) ) {
		_XRead( display, *vendor,
		    (long)(reply.length * sizeof(CARD32)) );
		(*vendor)[reply.lengthName] = '\0';
	    }
	}
	UNLOCK_DISPLAY(display);
    }
    return status;
} 


int
PEXGetEnumeratedTypeInfo( display, drawable, num_enum_types, enum_types,
    item_mask, num_types, types )
    Display		*display;
    Drawable		drawable;
    CARD32		num_enum_types;
    CARD16		*enum_types;
    pexBitmask		item_mask;
    CARD32		*num_types;	/* OUT */
    Pointer		*types;		/* OUT */
{
    int					status = 0;
    Pex_srvr_info			*srvr;
    pexGetEnumeratedTypeInfoReq		*req;
    pexGetEnumeratedTypeInfoReply	reply;
    unsigned				size;
    Pex_data_vec			vec[1];

    if ( srvr = PexEntryCheck( display, 1 ) ) {
	LOCK_DISPLAY(display);
	vec[0].size = num_enum_types * sizeof(CARD16);
	vec[0].padding = PADDING(vec[0].size);
	vec[0].data = (char *)enum_types;
	PEX_VAR_REQUEST(GetEnumeratedTypeInfo, display, PEX_OPCODE(srvr),
	    vec[0].size + vec[0].padding, req);
	req->drawable = drawable;
	req->itemMask = item_mask;
	req->numEnums = num_enum_types;
	PEX_LOAD_VAR_REQUEST_DATA(display,1,vec)

	status = _XReply(display, (xReply *)&reply, 0, xFalse);
	if (status) {
	    *num_types = reply.numLists;
	    if ( (size = reply.length * sizeof(CARD32)) > 0 ) {
		if ( *types = PEX_SCRATCH(srvr, size) )
		    _XRead(display, (char *)*types, (long)size);
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
PEXGetImpDepConstants(display, drawable, num_names, names, constants)
    Display		*display;
    unsigned long	drawable;
    CARD16		num_names;
    CARD16		*names;
    Pointer		*constants; /** OUT - will be CARD32s or FLOATs **/
{
    int				status = 0;
    unsigned			size;
    Pex_srvr_info		*srvr;
    pexGetImpDepConstantsReq	*req;
    pexGetImpDepConstantsReply	reply;
    Pex_data_vec		vec[1];

    if ( srvr = PexEntryCheck( display, 1 ) ) {
	LOCK_DISPLAY(display);
	vec[0].size = num_names * sizeof(CARD16);
	vec[0].padding = PADDING(vec[0].size);
	vec[0].data = (char *)names;
	PEX_VAR_REQUEST(GetImpDepConstants, display, PEX_OPCODE(srvr),
	    vec[0].size + vec[0].padding, req);
	PEX_FP_FORMAT(req->fpFormat);
	req->drawable = drawable;
	req->numNames = num_names;
	PEX_LOAD_VAR_REQUEST_DATA(display,1,vec)

	status = _XReply(display, (xReply *)&reply, 0, xFalse);
	if (status) {
	    if ( (size = reply.length * sizeof(CARD32)) > 0 ) {
		if ( *constants = PEX_SCRATCH(srvr, size) )
		    _XRead( display, (char *)*constants, (long)size );
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
