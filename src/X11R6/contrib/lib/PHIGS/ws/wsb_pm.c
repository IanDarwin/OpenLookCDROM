/* $XConsortium: wsb_pm.c,v 5.3 94/04/17 20:42:32 hersh Exp $ */

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

/* PEX/PHIGS workstation utility functions for the B model (client side
 * workstations and structure storage).
 */

#include "phg.h"
#include "cp.h"
#include "ws.h"
#include "PEX.h"
#include "PEXproto.h"
#include "PEXmacs.h"
#include "PEXfuncs.h"
#include "phigspex.h"


void
wsb_destroy_notify( display, window, ws, event )
    Display	*display;
    Window	window;
    Ws		*ws;
    XEvent	*event;
{
    exit( 3 );
}


static void
wsb_handle_resize( display, window, ws, event )
    Display	*display;
    Window	window;
    Ws		*ws;
    XEvent	*event;
{
    XRectangle	old_rect;

    /* Resize or move event. */
    old_rect = ws->ws_rect;
    phg_wsx_update_ws_rect( ws );
    if ( WS_ANY_INP_DEV_ACTIVE(ws) )
	phg_ws_inp_resize( ws, &old_rect );
}


static void
wsb_handle_exposure( display, window, ws, first_event )
    Display	*display;
    Window	window;
    Ws		*ws;
    XEvent	*first_event;
{
    int			num_rects;
    pexDeviceRect	*pex_rects;
    XRectangle		*x_rects;
    pexBitmask		rmask;
    CARD32		*card32_p;

    /* Set the renderer's clip list and repaint the display surface without
     * updating the workstation state.
     */
    if ( num_rects = phg_wsx_build_exposure_rects( display, window, ws,
	    first_event, &pex_rects, &x_rects ) > 0 ) {
	/* Store the rect count at the head of the list. */
	if ( !(card32_p = (CARD32 *)malloc( (unsigned)(num_rects * 
								sizeof(pexDeviceRect) + sizeof(CARD32) )))) {
	    ERR_BUF( ws->erh, ERR900 );
	    return;
	}
	*card32_p = num_rects;
	bcopy( (char *)pex_rects, (char *)(card32_p + 1),
	    num_rects * sizeof(pexDeviceRect) );

	rmask = PEXRDClipList;
	(void)PEXChangeRenderer( display, ws->rid, rmask,
	    (CARD32)(sizeof(CARD32) + num_rects * sizeof(pexDeviceRect)),
	    (char *)card32_p );
	(*ws->repaint_all)( ws, PFLAG_COND, num_rects, x_rects );

	/* Reset the renderer's clip list. */ 
	*card32_p = 0;
	(void)PEXChangeRenderer( display, ws->rid, rmask, (CARD32)sizeof(CARD32),
	    (char *)card32_p );
	free( (char *)card32_p );
    }
}


static int
wsb_X_point_in_viewport( ws, pt )
    Ws		*ws;
    XPoint	*pt;
{
    int		status;
    Ppoint	dc_pt;

    phg_wsx_update_ws_rect( ws );
    WS_DRWBL_TO_DC2(ws, pt, &dc_pt);
    status = WS_PT_IN_LIMIT2(&ws->out_ws.model.b.ws_viewport, pt);
    return status;
}


void
phg_wsb_pm_close_ws( ws )
    Ws		*ws;
{
    if ( ws ) {
	/* Clean up all the PM-specific stuff. */
	phg_ntfy_unregister_window( ws->display, ws->drawable_id );
	if ( ws->type->desc_tbl.phigs_dt.ws_category == PCAT_OUTIN )
	    phg_ws_input_close( ws );
	if ( ws->shell )
	    phg_cpm_toolkit_close_ws( ws );
	if ( ws->input_overlay_window )
	    phg_wsx_destroy_overlay( ws->display,
		ws->input_overlay_window, ws->drawable_id );
	phg_wsb_close_ws( ws );
    }
}


Ws*
phg_wsb_pm_open_ws( cph, cp_args, ret, css_srvr )
    Cp_handle		cph;
    Phg_args		*cp_args;
    Phg_ret		*ret;
    Cpx_css_srvr	*css_srvr;
{
    Phg_args_open_ws	*args = &cp_args->data.open_ws;
    Ws			*ws;
    Display		*display;
    Phg_pex_ext_info	pex_info;
    long	event_mask = 0;

    if ( args->type->base_type == WST_BASE_TYPE_X_DRAWABLE ) {
	/* Need to connect to the display before opening the workstation. */
	if ( (display = phg_cpx_connection_exists( cph, CPX_BY_NAME,
		args->conn_info.display_name ))
	    || (display = XOpenDisplay( args->conn_info.display_name )) ) {
	    args->conn_info.display = display;
	    /* Instance the connection here to get the "opened_by_api" flag
	     * set correctly.  The wsb code will also instance the
	     * connection, so it has to be released below to prevent a
	     * double instance for the same workstation.
	     */
	    phg_cpx_instance_connection( cph, display, 1 );
	} else {
	    ret->err = ERRN201;
	    ERR_BUF( cph->erh, ERRN201 );
	    return (Ws *)NULL;
	}
    }

    ws = phg_wsb_open_ws( cph, cp_args, ret, css_srvr );
    phg_cpx_release_connection( cph, display ); /* see comment above */
    if ( !ws )
	return (Ws *)NULL;
    
    if ( !(ws->top_level = 
	    phg_cpm_toolkit_add_connection( cph, ws->display, &ret->err )) ) {
	phg_wsb_close_ws( ws );
	return (Ws *)NULL;
    }

    if ( !phg_cpm_toolkit_open_ws( ws ) ) {
	phg_wsb_close_ws( ws );
	return (Ws *)NULL;
    }
    phg_wsx_pm_create_message_win( ws );


    /* Initialize the input code if input is wanted. */
    if ( args->type->desc_tbl.phigs_dt.ws_category == PCAT_OUTIN ) {
	if ( ws->input_overlay_window = phg_wsx_create_overlay( ws ) ) {
	    ws->resolve_locator = phg_wsb_resolve_locator;
	    ws->resolve_stroke = phg_wsb_resolve_stroke;
	    ws->resolve_pick = phg_wsb_resolve_pick;
	    ws->map_initial_points = phg_wsb_map_initial_points;
	    ws->X_point_in_viewport = wsb_X_point_in_viewport;
	} else {
	    phg_cpm_toolkit_close_ws( ws );
	    phg_wsb_close_ws( ws );
	    return (Ws *)NULL;
	}
	if ( !phg_ws_input_init( ws, ws->cph->input_q ) ) {
	    phg_cpm_toolkit_close_ws( ws );
	    phg_wsx_destroy_overlay( ws->display, ws->input_overlay_window,
		ws->drawable_id );
	    phg_wsb_close_ws( ws );
	    return (Ws *)NULL;
	} else
	    ret->data.open_ws.overlay_id = ws->input_overlay_window;
    }

    /* Look for events that will require a PEXRedrawClipRegion() or a 
     * resize action on the main workstation drawable.
     */    
    event_mask = StructureNotifyMask;
    XSelectInput( ws->display, ws->drawable_id, event_mask );
    (void)phg_ntfy_register_event( ws->display, ws->drawable_id,
	ConfigureNotify, (caddr_t)ws, wsb_handle_resize );
    if ( ws->type->desc_tbl.xwin_dt.flags.handle_destroy )
	(void)phg_ntfy_register_event( ws->display, ws->drawable_id,
	    DestroyNotify, (caddr_t)ws, wsb_destroy_notify );
    if ( ws->type->desc_tbl.xwin_dt.flags.handle_expose ) {
	event_mask |= ExposureMask;
	XSelectInput( ws->display, ws->drawable_id, event_mask );
	(void)phg_ntfy_register_event( ws->display, ws->drawable_id,
	    Expose, (caddr_t)ws, wsb_handle_exposure );
    }

    /* Replace the close function with a PM-specific version. */
    ws->close = phg_wsa_pm_close_ws;

    /* Make it all happen before returning. */
    XFlush( ws->display );

    return ws;
}

