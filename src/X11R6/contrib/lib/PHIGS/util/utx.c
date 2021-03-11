/* $XConsortium: utx.c,v 5.3 94/04/17 20:42:24 rws Exp $ */

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
#include "PEX.h"
#include "PEXmacs.h"
#include "PEXfuncs.h"
#include "PEXproto.h"
#include "PEXtempl.h"
#include "phigspex.h"

int
phg_utx_pex_supported( display, pexinfo )
    Display		*display;
    Phg_pex_ext_info	*pexinfo;
{
    Phg_pex_ext_info	info;

    if ( !pexinfo )
	pexinfo = &info;

    if ( PEXGetExtensionInfo( display,
	    PEX_PROTO_MAJOR, PEX_PROTO_MINOR,
	    &pexinfo->major_version, &pexinfo->minor_version,
	    &pexinfo->vendor, &pexinfo->release_number,
	    &pexinfo->subset_info ) ) {
	if ( pexinfo == &info )
	    XFree( pexinfo->vendor );
	return 1;
    } else
	return 0;
}

Display*
phg_utx_open_pex_display( name, pexinfo, err )
    char		*name;
    Phg_pex_ext_info	*pexinfo;
    Pint		*err;
{
    Display		*display;
    char		*display_name;

    display_name = XDisplayName( name );
    if ( display = XOpenDisplay( display_name ) ) {
	if ( !PEXGetExtensionInfo( display,
		PEX_PROTO_MAJOR, PEX_PROTO_MINOR,
		&pexinfo->major_version, &pexinfo->minor_version,
		&pexinfo->vendor, &pexinfo->release_number,
		&pexinfo->subset_info ) ) {
	    XCloseDisplay( display );
	    display = (Display *)NULL;
	    *err = ERRN201;
	}
    } else
	*err = ERRN200;

    return display;
}

