/* $XConsortium: wsx.c,v 5.5 94/04/17 20:42:33 hersh Exp $ */

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

/* PEX/PHIGS workstation utility functions. */

#include "phg.h"
#include "cp.h"
#include "ws.h"
#include "PEX.h"
#include "PEXproto.h"
#include "PEXmacs.h"
#include "PEXfuncs.h"
#include "phigspex.h"


static void
shutdown_tool( ws )
    Ws	*ws;
{
    if ( ws->display ) {
	if ( ws->drawable_id ) {
	    XDestroyWindow( ws->display, ws->drawable_id );
	    ws->drawable_id = 0;
	}
    }
}

int
phg_wsx_setup_tool( ws, conn_info, wst )
    Ws			*ws;
    Phg_args_conn_info	*conn_info;
    Wst			*wst;
{
    int				status = 0;
    XSizeHints			size_hints;
    Wst_xwin_dt			*xdt = &wst->desc_tbl.xwin_dt;
    Display			*display = ws->display;
    XID				drawable_id;
    XEvent			event;
    Visual			best_visual;
    XVisualInfo			*best_info, template;
    int				nvisuals;
    XSetWindowAttributes	attrs;
    Pint			err;
    Phg_pex_ext_info		pex_info;
    Colormap			cmap;
    XColor                      screen, exact;

    /* Create the window. */
    if (!phg_wsx_find_best_visual(ws, wst, &best_visual, &cmap, &err)) {
	ERR_BUF(ws->erh, err);
	return status;
    }
    /* Need the depth of this visual */
    template.visualid = best_visual.visualid;
    best_info = XGetVisualInfo(display, (long)VisualIDMask, &template, &nvisuals);
    XAllocNamedColor(display, cmap, "WHITE", &screen, &exact);
    attrs.border_pixel = screen.pixel;
    XAllocNamedColor(display, cmap, "BLACK", &screen, &exact);
    attrs.background_pixel = screen.pixel;
    attrs.colormap = cmap;
    /* TODO: background and border colour attributes. */

    if ( !(drawable_id = XCreateWindow(display,
	RootWindow(display, DefaultScreen(display)),
	xdt->tool.x, xdt->tool.y, xdt->tool.width, xdt->tool.height,
	xdt->tool.border_width, best_info->depth,
	InputOutput, best_info->visual,
	CWColormap | CWBackPixel | CWBorderPixel, &attrs)) ) {
	ERR_BUF(ws->erh,ERRN203);
	return status;
    }

    /* Finish the setup. */
    size_hints.flags = USPosition | USSize;
    size_hints.x = xdt->tool.x;
    size_hints.y = xdt->tool.y;
    size_hints.width = xdt->tool.width;
    size_hints.height = xdt->tool.height;
    XSetStandardProperties( display, drawable_id, xdt->tool.label,
	xdt->tool.icon_label, None, NULL, 0, &size_hints );
    XSelectInput( display, drawable_id, (long)ExposureMask );
    XMapWindow( display, drawable_id );
    XSync( display, False );
    XWindowEvent( display, drawable_id, ExposureMask, &event );
    XSelectInput( display, drawable_id, (long)0 );
    ws->drawable_id = drawable_id;

    return 1;
}


static void
shutdown_drawable( ws )
    Ws		*ws;
{
    ws->drawable_id = 0;
}


void
phg_wsx_release_window( ws )
    Ws		*ws;
{
    if ( ws->type ) {
	switch ( ws->type->base_type ) {
	    case WST_BASE_TYPE_X_TOOL:
		shutdown_tool( ws );
		break;
	    case WST_BASE_TYPE_X_DRAWABLE:
		shutdown_drawable( ws );
		break;
	}
    }
}


void
phg_wsx_inq_text_extent( cph, args, ret, ws )
    Cp_handle			cph;
    Phg_args_q_text_extent	*args;
    Phg_ret			*ret;
    Ws				*ws;
{
    pexExtentInfo	*extents;
    unsigned		size;
    CARD32		*card32_p;
    pexMonoEncoding	*encodings;

    /* Convert the string to MONO_ENCODING format then issue a PEX Query
     * Text Extent request.  Assumes string is not NULL or zero-length.
     */
    /* TODO: Only works for one string and one monoencoding. */
    ret->err = 0;
    size = args->length - 1;	/* don't count the terminator */
    size += (size % 4 == 0) ? 0 : 4 - (size % 4);	/* padding */
    size += sizeof(pexMonoEncoding) + sizeof(CARD32);
    if ( !PHG_SCRATCH_SPACE( &ws->scratch, size + sizeof(CARD32) ) ) {
	ret->err = ERR900;
	return;
    }

    card32_p = (CARD32 *)ws->scratch.buf;
    encodings = (pexMonoEncoding *)(card32_p + 1);
    if ( phg_utx_encode_text( args->length, args->str, encodings ) ) {
	*card32_p = 1;	/* only one list of mono-encodings supported. */
	if ( !PEXQueryTextExtents( ws->display, ws->rid,
		(pexTableIndex)args->font,
		(CARD16)PEX_CONV_FROM_Ptxpath(args->path),
		(PEXFLOAT)args->char_expan, (PEXFLOAT)args->spacing,
		(PEXFLOAT)args->height,
		(pexTextHAlignment)PEX_CONV_FROM_Ptxhor(args->hor),
		(pexTextVAlignment)PEX_CONV_FROM_Ptxver(args->ver),
		(CARD32)1, size, (char *)ws->scratch.buf, &extents ) ) {
	    ret->err = ERR900; /* TODO: Use phg_pex_errno. */
	} else {
	    PEX_CONV_TO_Ppoint( &extents->lowerLeft,
		&ret->data.text_extent.rect.p )
	    PEX_CONV_TO_Ppoint( &extents->upperRight,
		&ret->data.text_extent.rect.q )
	    PEX_CONV_TO_Ppoint( &extents->concatpoint,
		&ret->data.text_extent.offset )
	}
    }
}


void
phg_wsx_inq_colr_map_meth_st( cph, args, ret, ws )
    Cp_handle			cph;
    Phg_args_q_colr_map_meth_st	*args;
    Phg_ret			*ret;
    Ws				*ws;
{
    Display		*dpy = ws->display;
    XWindowAttributes	attrs;
    unsigned long	pixel;
    unsigned int	nplanes, ncolors;
    unsigned long	plane_masks[24]; /* max for 24 bit colors */
    register int i;

    ret->err = 0;

    switch( args->map_method ) {
	/* TRUE and PSEUDO_N are done in c_binding */
	case PCOLR_MAP_PSEUDO:
	    /* what a hack! 
	     * phigs assumes the application has complete 
	     * control of the colormap. in X, colormaps are
	     * shared. So, the best we can do is to find
	     * out the max number of entries which can be
	     * allocated at this time and return that as
	     * the number of available entries.  There is
	     * no guarantee that these entries will still
	     * be free by the time the application tries
	     * to do something with them
	     * Colormap entries must be allocated in one
	     * chunk, so start large and work down.
	     */

	    XGetWindowAttributes(dpy, ws->drawable_id, &attrs);

	    ncolors = ws->type->desc_tbl.phigs_dt.out_dt.num_colours;
	    /* calculate the max number of planes available */
	    for (nplanes=0, i=1; i<ncolors; i=i<<1, nplanes++);

	    /* assume PseudoColor Visual and ColorSpace mapping */
	    while (nplanes && !XAllocColorCells(dpy, attrs.colormap, 
		               True, plane_masks, nplanes, &pixel, 1)) {
		nplanes--;
	    }
	    if (!nplanes)
	        ret->data.idata = 0;
	    else {
	        ret->data.idata = 1<<nplanes;
		for (i=1; i<nplanes; i++)
			plane_masks[0] |= plane_masks[i];
		XFreeColors(dpy, attrs.colormap, &pixel, 1, plane_masks[0]);
	    }
	break;
	case PCOLR_MAP_TRUE:
	case PCOLR_MAP_PSEUDO_N:
		ret->data.idata = 0;
	break;
    }
}
