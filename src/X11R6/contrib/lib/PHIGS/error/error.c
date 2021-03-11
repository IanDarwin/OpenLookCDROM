/* $XConsortium: error.c,v 5.3 94/04/17 20:41:33 rws Exp $ */

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

#include "phg.h"
#include <X11/Xproto.h>
#include "PEX.h"
#include "PEXfuncs.h"

static Pint
map_error( err, codes )
    xError	*err;
    XExtCodes   *codes;
{
    Pint	errnum;

    /* Map X and PEX errors to a PHIGS error. */
    if ( err->errorCode < codes->first_error ) {
	/* It's likely to be an X error. */
	switch ( err->errorCode ) {
	    case BadRequest: errnum = PE_X_BAD_REQUEST; break;
	    case BadValue: errnum = PE_X_BAD_VALUE; break;
	    case BadWindow: errnum = PE_X_BAD_WINDOW; break;
	    case BadPixmap: errnum = PE_X_BAD_PIXMAP; break;
	    case BadAtom: errnum = PE_X_BAD_ATOM; break;
	    case BadCursor: errnum = PE_X_BAD_CURSOR; break;
	    case BadFont: errnum = PE_X_BAD_FONT; break;
	    case BadMatch: errnum = PE_X_BAD_MATCH; break;
	    case BadDrawable: errnum = PE_X_BAD_DRAWABLE; break;
	    case BadAccess: errnum = PE_X_BAD_ACCESS; break;
	    case BadAlloc: errnum = PE_X_BAD_ALLOC; break;
	    case BadColor: errnum = PE_X_BAD_COLOR; break;
	    case BadGC: errnum = PE_X_BAD_GC; break;
	    case BadIDChoice: errnum = PE_X_BAD_ID_CHOICE; break;
	    case BadName: errnum = PE_X_BAD_NAME; break;
	    case BadLength: errnum = PE_X_BAD_LENGTH; break;
	    case BadImplementation: errnum = PE_X_BAD_IMPL; break;
	    default: /* Don't recognize it. */
		errnum = 0;
		break;
	}
    } else {
	switch ( err->errorCode - codes->first_error ) {
	    case PEXColourTypeError: errnum = PE_PEX_CTE; break;
	    case PEXRendererStateError: errnum = PE_PEX_RSE; break;
	    case PEXFloatingPointFormatError: errnum = PE_PEX_FPFE; break;
	    case PEXLabelError: errnum = PE_PEX_LE; break;
	    case PEXLookupTableError: errnum = PE_PEX_LTE; break;
	    case PEXNameSetError: errnum = PE_PEX_NSE; break;
	    case PEXPathError: errnum = PE_PEX_PE; break;
	    case PEXFontError: errnum = PE_PEX_FE; break;
	    case PEXPhigsWksError: errnum = PE_PEX_PWE; break;
	    case PEXPickMeasureError: errnum = PE_PEX_PME; break;
	    case PEXPipelineContextError: errnum = PE_PEX_PCE; break;
	    case PEXRendererError: errnum = PE_PEX_RE; break;
	    case PEXSearchContextError: errnum = PE_PEX_SCE; break;
	    case PEXStructureError: errnum = PE_PEX_SE; break;
	    case PEXOutputCommandError: errnum = PE_PEX_OCE; break;
	    default: /* Don't recognize it. */
		errnum = 0;
		break;
	}
    }
    return errnum;
}

int
phg_err_pex_reply( display, err, codes, ret_code, erh )
    Display     *display;
    xError      *err;
    XExtCodes   *codes;
    int         *ret_code;
    Err_handle	erh;
{
    /* Suppress Xlib reporting of errors during _XReply(). */
    if ( erh->errno = map_error( err, codes ) ) {
	*ret_code = 0;	/* force null return to XReply() */
	return 1;
    } else
	return 0;	/* Let Xlib report it. */
}

int
phg_err_x( display, error )
    Display     *display;
    XErrorEvent	*error;
{
    /* TODO:
     * - How to map PEX/X errors to PHIGS errors.
     * - How to associate the correct PHIGS function id with the PEX error.
     * - The API shouldn't be stealing the X error handler.
     */
#ifdef NOT_RIGHT_YET
    Pint	errnum;
    XExtCodes   *codes;

    if ( (codes = PexGetExtCodes( display ))
	    && errnum = map_error( error, codes ) ) {
	ERR_BUF( erh, errnum );
    } else { /* must be an X error */
	(void)_XPrintDefaultError( display, error, stderr );
    }
#endif
    fprintf( stderr, "X Error: error code = %d, request code = %d,\
minor code = %d\nresource id = %d, serial = %d, last request = %d\n",
    error->error_code, error->request_code, error->minor_code,
    error->resourceid, error->serial, NextRequest(display)-1 );
    return 1; /* ? */
}

Err_handle
phg_erh_create()
{
    Err_handle		erh = NULL;

    if ( erh = (Err_handle)malloc( sizeof(Err_struct))) {
	erh->mode = PERR_ON;
	erh->err_state = PERR_ON;
	erh->cur_func_num = 0;
    }
    return erh;
}

int
phg_err_store_name( erh, err_file, ptr)
    Err_handle		erh;
    char		*err_file;
    char		**ptr;
{
    /* Store the error file name.  Note that err_file == NULL is valid,
     * it means use stderr
     */
    int	status = 0;

    if ( err_file) {
	if ( *ptr = (char*)malloc( (unsigned)(strlen(err_file) + 1))) {
	    strcpy( *ptr, err_file);
	    status = 1;
	}/* else {
	    ERR_HANDLE( ERR900, Pfn_openphigs, err_file);
	}*/
    } else {
	*ptr = NULL;
	status = 1;
    }
    return status;
}
