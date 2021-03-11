/* $XConsortium: pex_font.c,v 5.3 94/04/17 20:42:09 hersh Exp $ */

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
 *   PEX Fonts functions
 *
 *   Section 12.1 -- PEX Font Resource Management
 *	PEXOpenFont()		    
 *	PEXCloseFont()		   -- access via macro in phigspex.h
 *
 *   Section 12.2 -- PEX Font Inquiry
 *	PEXQueryFont()		    
 *	PEXListFonts()		    
 *	PEXListFontsWithInfo()	    
 *	PEXQueryTextExtents()	    
 */

#include "pex_priv.h"

/* 
 *   Section 12.1 -- PEX Font Resource Management
 */


int
PEXOpenFont(display, font, fontName)
Display	    *display;
pexFont	     font;
char	    *fontName;
{
    int				 status = 0;
    Pex_srvr_info		*srvr;
    pexOpenFontReq		*req;
    CARD32			 nameLength = (CARD32)strlen(fontName);
    Pex_data_vec		vec[1];
    
    if (srvr = PexEntryCheck(display, 1)) {
	LOCK_DISPLAY(display);
	vec[0].size = nameLength;
	vec[0].padding = PADDING(nameLength);
	vec[0].data = fontName;
	PEX_VAR_REQUEST(OpenFont, display, PEX_OPCODE(srvr),
	    vec[0].size + vec[0].padding, req);
	req->font = font;
	req->numBytes = nameLength;
	PEX_LOAD_VAR_REQUEST_DATA(display,1,vec)
	status = 1;
	UNLOCK_DISPLAY(display);
    }
    PEX_SYNC_HANDLE(display);
    return status;
}


/*
 *   Section 12.2 -- PEX Font Inquiry
 */

int
PEXQueryFont(display, font, lengthFontInfo, fontInfo)
Display		*display;
pexFont		 font;
CARD32		*lengthFontInfo;    /* OUT */
Pointer		*fontInfo;	    /* OUT */
{
    int				 status = 0;
    unsigned			 size;
    Pex_srvr_info		*srvr;
    pexQueryFontReq		*req;
    pexQueryFontReply		 reply;
    
    if (srvr = PexEntryCheck(display, 1)) {
	LOCK_DISPLAY(display);
	PEX_REQUEST(QueryFont, display, PEX_OPCODE(srvr), req);
	req->font = font;
	
	status = _XReply(display, (xReply *)&reply, 0, xFalse);
	if (status) {
	    *lengthFontInfo = reply.lengthFontInfo;
	    if ( (size = reply.length * sizeof(CARD32)) > 0 ) {
		if ( *fontInfo = PEX_SCRATCH(srvr, size) )
		    _XRead(display, (char *)*fontInfo, (long)size );
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
PEXListFonts(display, maxNames, patternString, numStrings, strings)
Display		*display;
CARD16		 maxNames;
char		*patternString;
CARD32		*numStrings;	    /* OUT */
Pointer		*strings;	    /* OUT */
{
    int				 status = 0;
    int				 numChars = strlen(patternString);
    unsigned			 size;
    Pex_srvr_info		*srvr;
    pexListFontsReq		*req;
    pexListFontsReply		 reply;
    Pex_data_vec		vec[1];

    if (srvr = PexEntryCheck(display, 1)) {
	LOCK_DISPLAY(display);
	vec[0].size = numChars;
	vec[0].padding = PADDING(numChars);
	vec[0].data = patternString;
	PEX_VAR_REQUEST(ListFonts, display, PEX_OPCODE(srvr),
	    vec[0].size + vec[0].padding, req);
	req->maxNames = maxNames;
	req->numChars = numChars;
	PEX_LOAD_VAR_REQUEST_DATA(display,1,vec)

	status = _XReply(display, (xReply *)&reply, 0, xFalse);
	if (status) {
	    *numStrings = reply.numStrings;
	    if ( (size = reply.length * sizeof(CARD32)) > 0 ) {
		if ( *strings = PEX_SCRATCH(srvr, size) )
		    _XRead(display, (char *)*strings, (long)size );
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
PEXListFontsWithInfo(display, maxNames, patternString, 
	numStrings, stringsAndInfo)
Display		*display;
CARD16		 maxNames;
char		*patternString;
CARD32		*numStrings;	    /* OUT */
Pointer		*stringsAndInfo;    /* OUT */
{
    int				 status = 0;
    int				 numChars = strlen(patternString);
    unsigned			 size;
    Pex_srvr_info		*srvr;
    pexListFontsWithInfoReq	*req;
    pexListFontsWithInfoReply	 reply;
    Pex_data_vec		vec[1];
    
    if (srvr = PexEntryCheck(display, 1)) {
	LOCK_DISPLAY(display);
	vec[0].size = numChars;
	vec[0].padding = PADDING(numChars);
	vec[0].data = patternString;
	PEX_VAR_REQUEST(ListFontsWithInfo, display, PEX_OPCODE(srvr),
	    vec[0].size + vec[0].padding, req);
	req->maxNames = maxNames;
	req->numChars = numChars;
	PEX_LOAD_VAR_REQUEST_DATA(display,1,vec)
	
	status = _XReply(display, (xReply *)&reply, 0, xFalse);
	if (status) {
	    *numStrings = reply.numStrings;
	    if ( (size = reply.length * sizeof(CARD32)) > 0 ) {
		if ( *stringsAndInfo = PEX_SCRATCH(srvr, size) )
		    _XRead(display, (char *)*stringsAndInfo, (long)size );
		else {
		    status = 0;
		    PexClearReply(display, reply.length);
		}
	    }
	}
	UNLOCK_DISPLAY(display);
    }
    UNLOCK_DISPLAY(display);
    PEX_SYNC_HANDLE(display);
    return status;  
}

int
PEXQueryTextExtents(display, rid, font, text_path, char_expansion,
    char_spacing, char_height, horizontal, vertical, num_istrings,
    length_of_istring_list, istrings, extent_info)
Display			*display;
XID			 rid;
pexTableIndex		 font;
CARD16			 text_path;
PEXFLOAT			 char_expansion;
PEXFLOAT			 char_spacing;
PEXFLOAT			 char_height;
pexTextHAlignment	 horizontal;
pexTextVAlignment	 vertical;
CARD32			 num_istrings;
unsigned		 length_of_istring_list;	/* in bytes */
Pointer			 istrings;
pexExtentInfo		*extent_info[];	    /* OUT */
{
    int				 status = 0;
    unsigned			 size;
    Pex_srvr_info		*srvr;
    pexQueryTextExtentsReq	*req;
    pexQueryTextExtentsReply	 reply;
    Pex_data_vec		vec[1];
    
    if (srvr = PexEntryCheck(display, 1)) {
	LOCK_DISPLAY(display);
	vec[0].size = (int)length_of_istring_list;
	vec[0].padding = PADDING(vec[0].size);
	vec[0].data = (char *)istrings;
	PEX_VAR_REQUEST(QueryTextExtents, display, PEX_OPCODE(srvr),
	    vec[0].size + vec[0].padding, req);
	PEX_FP_FORMAT(req->fpFormat);
	req->textPath = text_path;
	req->id = rid;
	req->fontGroupIndex = font;
	req->charExpansion = char_expansion;
	req->charSpacing = char_spacing;
	req->charHeight = char_height;
	req->textAlignment.horizontal = horizontal;
	req->textAlignment.vertical = vertical;
	req->numStrings = num_istrings;
	PEX_LOAD_VAR_REQUEST_DATA(display,1,vec)

	status = _XReply(display, (xReply *)&reply, 0, xFalse);
	if (status) {
	    if ( (size = reply.length * sizeof(CARD32)) > 0 ) {
		if (*extent_info = (pexExtentInfo *)PEX_SCRATCH(srvr, size))
		    _XRead(display, (char *)*extent_info, (long)size);
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
