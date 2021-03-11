/* $XConsortium: wsa_pm.c,v 5.4 94/04/17 20:42:30 mor Exp $ */

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

/* PEX/PHIGS workstation utility functions for the A model (server side
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


static void
wsa_pm_destroy( ws )
    Ws		*ws;
{
    if ( ws ) {
	if ( ws->type->desc_tbl.phigs_dt.ws_category == PCAT_OUTIN )
	    phg_ws_input_close( ws );
	if ( ws->shell )
	    phg_cpm_toolkit_close_ws( ws );
	if ( ws->input_overlay_window )
	    phg_wsx_destroy_overlay( ws->display,
		ws->input_overlay_window, ws->drawable_id );
	phg_wsx_destroy( ws );
    }
}


static int
wsa_X_point_in_viewport( ws, pt )
    Ws		*ws;
    XPoint	*pt;
{
    int			status = 0;
    pexViewport		*vp;
    pexBitmask		mask[PEXMSGetWksInfo];

    bzero((char *)mask, sizeof(mask));   
    PEX_BITSET(mask, PEXPWCurWksViewport);
    if ( PEXGetWksInfo( ws->display, ws->rid, mask, (char **)&vp ) ) {
	status = pt->x >= vp->minval.x && pt->x <= vp->maxval.x
	      && pt->y >= vp->minval.y && pt->y <= vp->maxval.y;
    }
    return status;
}

void
wsa_pm_redraw_regions( ws, args )
    Ws				*ws;
    Phg_args_redraw_regions	*args;
{
    pexDeviceRect	*pex_rects;
    
    if ( args->num_regions <= 0 )
	return;

    if ( !(pex_rects = (pexDeviceRect *)PHG_SCRATCH_SPACE( &ws->scratch,
	    (unsigned)(args->num_regions * sizeof(*pex_rects)) )) ) {
	ERR_BUF( ws->erh, ERR900 );
	return;
    }

    phg_wsx_convert_rects( ws, args->num_regions, args->regions, pex_rects );
    (void)PEXRedrawClipRegion( ws->display, ws->rid,
	(CARD32)args->num_regions, pex_rects );
    if ( WS_ANY_INP_DEV_ACTIVE(ws) && ws->input_repaint )
	(*ws->input_repaint)( ws, args->num_regions, args->regions );
    XFlush( ws->display );
}

static void
wsa_pm_load_funcs( ws )
    register	Ws	*ws;
{
    /* Only need the ones the PM has responsibility for.  The input
     * initialization will add any needed for PHIGS input.
     */
    ws->close = phg_wsa_pm_close_ws;
    ws->X_point_in_viewport = wsa_X_point_in_viewport;
    ws->redraw_regions = wsa_pm_redraw_regions;
}

void
wsa_destroy_notify( display, window, ws, event )
    Display	*display;
    Window	window;
    Ws		*ws;
    XEvent	*event;
{
    exit( 3 );
}

void
wsa_configure_notify( display, window, ws, event )
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
wsa_handle_exposure( display, window, ws, first_event )
    Display	*display;
    Window	window;
    Ws		*ws;
    XEvent	*first_event;
{
    int			num_rects;
    pexDeviceRect	*pex_rects;
    XRectangle		*x_rects;
    
    /* For an expose event, we want to collect up all of the exposed
     * regions, send them down to PEXRedrawClipRegion, and repaint any
     * input echoes on the window.
     */
    if ( (num_rects = phg_wsx_build_exposure_rects( display, window, ws,
	    first_event, &pex_rects, &x_rects)) > 0 ) {
	(void)PEXRedrawClipRegion(display, ws->rid,
	    (CARD32)num_rects, pex_rects);
	if ( WS_ANY_INP_DEV_ACTIVE(ws) && ws->input_repaint )
	    (*ws->input_repaint)( ws, num_rects, x_rects );
	XFlush( display );
    }
}


static int
wsa_pm_init_input( ws )
    Ws		*ws;
{
    int		status = 0;

    if ( ws->input_overlay_window = phg_wsx_create_overlay( ws ) )
	ws->resolve_locator = phg_wsa_resolve_locator;
	ws->resolve_stroke = phg_wsa_resolve_stroke;
	ws->resolve_pick = phg_wsa_pm_resolve_pick;
	ws->pick_enable = phg_wsa_pm_pick_enable;
	ws->pick_disable = phg_wsa_pick_disable;
	ws->valid_pick_path = phg_wsa_pm_valid_pick_path;
	ws->map_initial_points = phg_wsa_pm_map_initial_points;
	if ( phg_ws_input_init( ws, ws->cph->input_q ) ) {
	    status = 1;
	}
    return status;
}


Ws*
phg_wsa_pm_open_ws( cph, cp_args, ret, css_srvr )
    Cp_handle		cph;
    Phg_args		*cp_args;
    Phg_ret		*ret;
    Cpx_css_srvr	*css_srvr;
{
    XWindowAttributes   wattr;
    Phg_args_open_ws	*args = &cp_args->data.open_ws;
    Ws			*ws;
    long	event_mask = 0;

    if ( !(ws = phg_wsx_create( cph, args, css_srvr )) ) {
	return ws;
    }

    /* Create a local copy of the WDT. */
    if ( !(ws->type = phg_wst_create( ws->erh, args->type,
	    (caddr_t *)NULL )) ) {
	phg_wsx_destroy( ws );
	return (Ws *)NULL;
    }
    ws->type->bound_status = WST_BOUND;

    ws->top_level = css_srvr->model.a.top_level;
    if ( !phg_cpm_toolkit_open_ws( ws ) ) {
	phg_wsx_destroy( ws );
	return (Ws *)NULL;
    }
    phg_wsx_pm_create_message_win( ws );

    /* Set the ws info needed in the PM. */
    ws->rid = args->wsrid;
    ws->display = css_srvr->display;
    ws->drawable_id = args->conn_info.drawable_id;
    wsa_pm_load_funcs( ws );

    XGetWindowAttributes( ws->display, ws->drawable_id, &wattr );
    WS_SET_WS_RECT( ws, &wattr )

    /* Initialize the input code if input is wanted. */
    if ( args->type->desc_tbl.phigs_dt.ws_category == PCAT_OUTIN ) {
	if ( !wsa_pm_init_input( ws ) ) {
	    wsa_pm_destroy( ws );
	    return (Ws *)NULL;
	}
    }

    /* Look for events that require some action on the PM's part. */
    event_mask = StructureNotifyMask;
    XSelectInput( ws->display, ws->drawable_id, event_mask );
    (void)phg_ntfy_register_event( ws->display, ws->drawable_id,
	ConfigureNotify, (caddr_t)ws, wsa_configure_notify );
    if ( ws->type->desc_tbl.xwin_dt.flags.handle_destroy )
	(void)phg_ntfy_register_event( ws->display, ws->drawable_id,
	    DestroyNotify, (caddr_t)ws, wsa_destroy_notify );

    if ( ws->type->desc_tbl.xwin_dt.flags.handle_expose ) {
	event_mask |= ExposureMask;
	XSelectInput( ws->display, ws->drawable_id, event_mask );
	(void)phg_ntfy_register_event( ws->display, ws->drawable_id,
	    Expose, (caddr_t)ws, wsa_handle_exposure );
    }

    /* Fill in the return data, although some's redundant for this case. */
    ret->data.open_ws.wstype = ws->type;
    ret->data.open_ws.wst_buffer = ws->type->buffer;
    ret->data.open_ws.wst_buffer_size = ws->type->buffer_size;
    ret->data.open_ws.drawable_id = ws->drawable_id;
    ret->data.open_ws.overlay_id = ws->input_overlay_window;

    /* Make it all happen before returning. */
    XFlush( ws->display );

    return ws;
}


void
phg_wsa_pm_close_ws( ws )
    Ws		*ws;
{
    if ( ws ) {
	if ( ws->display ) {
	    XFlush( ws->display );
	    if ( ws->drawable_id )
		phg_ntfy_unregister_window( ws->display, ws->drawable_id );
	}
	wsa_pm_destroy( ws );
    }
}


int
phg_wsa_pm_map_initial_points( ws, view, num_pts, wc_pts, dc_pts )
    Ws		*ws;
    Pint	view;
    Pint	*num_pts;
    Ppoint3	*wc_pts;
    XPoint	*dc_pts;
{
    CARD32		count = *num_pts;
    pexDeviceCoord	*dev_coords;
    pexCoord3D		scratch[20];	/* enough for most cases */

    register	pexCoord3D	*pex_wc_pts = (pexCoord3D *) NULL;
    register	int		i;

    /* Convert the WC points to PEX format and send them to MapWCtoDC. */
    if ( *num_pts <= 0 )
	return 0;
    if ( *num_pts <= sizeof(scratch)/sizeof(scratch[0]) )
	pex_wc_pts = scratch;
    else if ( *num_pts > sizeof(scratch)/sizeof(scratch[0]) ) {
	if ( !(pex_wc_pts = (pexCoord3D *)malloc( (unsigned)(count *
	    sizeof(pexCoord3D)) )) ) {
	    *num_pts = 0;
	    ERR_BUF( ws->erh, ERR900 );
	    return 0;
	}
    }

    for ( i = 0; i < count; i++ ) {
	PEX_CONV_FROM_Ppoint3(&wc_pts[i], &pex_wc_pts[i])
    }

    if ( ws->rid && PEXMapWCtoDC( ws->display, ws->rid, (CARD16)view,
		(CARD32)count, pex_wc_pts, &count, &dev_coords ) ) {
	*num_pts = count;
	for ( i = 0; i < count; i++ ) {
	    WS_DC_TO_DRWBL2( ws, &dev_coords[i], &dc_pts[i] );
	}

    } else
	*num_pts = 0;
    
    if ( pex_wc_pts && pex_wc_pts != scratch )
	free( (char *)pex_wc_pts );

    return 1;
}


int
phg_wsa_pm_resolve_pick( ws, dev, echo, dc_pt, pick )
    Ws			*ws;
    Ws_inp_pick		*dev;
    int			echo;
    pexDeviceCoord      *dc_pt;
    register Ppick	*pick;
{
    CARD16		status;
    CARD32		depth;
    pexPickPath		*path;
    Pecho_switch	esw;

    register int	i;

    pick->status = PIN_STATUS_NONE;
    pick->pick_path.depth = 0;
    pick->pick_path.path_list = (Ppick_path_elem *)NULL;

    /* Swap out the echo flag. */
    esw = dev->esw;
    dev->esw = echo ? PSWITCH_ECHO : PSWITCH_NO_ECHO;

    if ( !phg_wsa_resolve_pick( ws, dev, dc_pt, &status, &depth, &path ) ) {
	dev->esw = esw;
	return 0;

    } else if ( status == PEXOk && depth > 0 ) {
	dev->esw = esw;
	/* Convert the structure ids to the application's structure ids. */
	if ( !PHG_SCRATCH_SPACE( &ws->scratch,
		(unsigned)(depth * sizeof(Ppick_path_elem)) ) ) {
	    ERR_BUF( ws->erh, ERR900 );
	    return 0;
	}
	pick->status = PIN_STATUS_OK;
	pick->pick_path.depth = depth;
	pick->pick_path.path_list = (Ppick_path_elem *)ws->scratch.buf;
	for ( i = 0; i < pick->pick_path.depth; i++ ) {
	    pick->pick_path.path_list[i].struct_id =
		CPA_DECODE_STRUCT_ID( ws->css_srvr, path[i].sid );
	    pick->pick_path.path_list[i].elem_pos = path[i].offset;
	    pick->pick_path.path_list[i].pick_id = path[i].pickid;
	}
    } /* else no pick */

    return 1;
}


int
phg_wsa_pm_pick_enable( ws, dev )
    Ws			*ws;
    Ws_inp_pick		*dev;
{
    CARD32		init_path_size, *card32_p;

    register char	*init_path;

    init_path_size = 0;
    if ( dev->pick.status == PIN_STATUS_OK ) {
	register int		i;
	register pexPickPath	*path_el;

	/* Convert and encode the initial path. */
	init_path_size = sizeof(CARD32) +
	    dev->pick.pick_path.depth * sizeof(pexPickPath);
	if ( !PHG_SCRATCH_SPACE( &ws->scratch, init_path_size ) ) {
	    ERR_BUF( ws->erh, ERR900 );
	    return 0;
	}
	init_path = ws->scratch.buf;

	card32_p = (CARD32 *)init_path; init_path += sizeof(CARD32);
	*card32_p = dev->pick.pick_path.depth;
	path_el = (pexPickPath *)init_path;
	for ( i = 0; i < dev->pick.pick_path.depth; i++, path_el++ ) {
	    path_el->sid = CPA_ENCODE_STRUCT_ID( ws->css_srvr,
		dev->pick.pick_path.path_list[i].struct_id );
	    path_el->offset = dev->pick.pick_path.path_list[i].elem_pos;
	    path_el->pickid = dev->pick.pick_path.path_list[i].pick_id;
	    init_path += sizeof(pexPickPath);
	}
    }

    if ( !phg_wsa_pick_enable( ws, dev, init_path_size, init_path ) )
	return 0;

    return 1;
}


int
/* TODO: */ phg_wsa_pm_valid_pick_path( ws, pick )
    Ws		*ws;
    Ppick	*pick;
{
    return 1;
}
