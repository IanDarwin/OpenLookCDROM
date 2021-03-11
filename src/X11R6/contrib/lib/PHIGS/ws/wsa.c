/* $XConsortium: wsa.c,v 5.4 94/04/17 20:42:29 hersh Exp $ */

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
 * workstations and structure storage).  Code in this file must stay
 * independent of any PM code.
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
destroy_pex_ws_resource( ws )
    Ws		*ws;
{
    if ( ws->rid ) {
	PEXFreePhigsWks( ws->display, ws->rid );
	ws->rid = 0;
    }
}


static void
wsa_destroy_ws( ws )
    Ws		*ws;
{
    if ( ws ) {
	destroy_pex_ws_resource( ws );
	phg_wsx_destroy_LUTs( ws );
	if ( ws->out_ws.htab.view )
	    phg_ut_htab_destroy( ws->out_ws.htab.view, (void(*)())NULL );
	if ( ws->display )
	    XFlush( ws->display );
	phg_wsx_destroy( ws );
    }
}


static void
wsa_load_funcs( ws )
    register	Ws	*ws;
{
    /* Most workstation operations are implemented in the server, so the
     * Cpa layer just sends the requests directly.  Few actual wsa
     * operations are needed.
     */
    ws->close = phg_wsa_close_ws;
    ws->drawable_pick = phg_wsa_drawable_pick;
    ws->map_points = phg_wsa_map_points;
    ws->redraw_regions = phg_wsa_redraw_regions;
}


static int
create_pex_ws_resource( ws )
    register	Ws	*ws;
{
    int			status = 0;
    caddr_t		values;
    pexBitmask		mask[PEXMSGetWksInfo];
    pexViewport		vp, *current;
    Wst			*wst = ws->type;

    ws->rid = XAllocID(ws->display);
    PEXCreatePhigsWks( ws->display, ws->rid,
	ws->drawable_id, ws->out_ws.lut.marker, ws->out_ws.lut.text,
	ws->out_ws.lut.line, ws->out_ws.lut.interior, ws->out_ws.lut.edge,
	ws->out_ws.lut.colour, ws->out_ws.lut.depth_cue,
	ws->out_ws.lut.light_source, ws->out_ws.lut.colour_approx,
	ws->out_ws.lut.pattern, ws->out_ws.lut.font,
	ws->out_ws.nset.hlt_incl, ws->out_ws.nset.hlt_excl,
	ws->out_ws.nset.invis_incl, ws->out_ws.nset.invis_excl,
	(CARD16)PEX_CONV_PHIGS_BUF_MODE(wst->desc_tbl.xwin_dt.buffer_mode) );


    bzero((char *)mask, sizeof(mask));   
    PEX_BITSET(mask, PEXPWNumPriorities);
    if ( PEXGetWksInfo( ws->display, ws->rid, mask, &values ) ) {
	XWindowAttributes	wattr;

	status = 1;
	/* Set the WS to wait so that the initializations don't cause
	 * unnecessary work to bd done.
	 */
	(void)PEXSetDisplayUpdateMode( ws->display, ws->rid,
	    (pexEnumTypeIndex) PEXVisualizeNone );
	/* Fill in the WDT fields that depend on an open workstaton. */
	wst->desc_tbl.phigs_dt.out_dt.num_display_priorities = 
	    *(CARD32 *)values;
	XGetWindowAttributes( ws->display, ws->drawable_id,
	    &wattr );
	wst->desc_tbl.phigs_dt.dev_coords[0] = wattr.width;
	wst->desc_tbl.phigs_dt.dev_coords[1] = wattr.height;
	wst->desc_tbl.phigs_dt.dev_addrs_units[0] = wattr.width;
	wst->desc_tbl.phigs_dt.dev_addrs_units[1] = wattr.height;

	/* Set the initial viewport. */
	bzero((char *)mask, sizeof(mask));   
	PEX_BITSET(mask, PEXPWCurWksViewport);
	if ( PEXGetWksInfo( ws->display, ws->rid, mask, (char **)&current) ) {
	    vp.useDrawable = PEXOff;
	    vp.minval.x = 0.0;
	    vp.minval.y = 0.0;
	    vp.maxval.x = wst->desc_tbl.phigs_dt.dev_coords[0];
	    vp.maxval.y = wst->desc_tbl.phigs_dt.dev_coords[1];
	    vp.minval.z = current->minval.z;
	    vp.maxval.z = current->maxval.z;
	    (void)PEXSetWksViewport( ws->display, ws->rid, &vp );
	} else {
	    ERR_BUF( ws->erh, ERR900 );
	}
    } else {
	ERR_BUF( ws->erh, ERRN202 );
	ws->rid = 0;
    }
    return status;
}


static int
init_view_table( ws )
    Ws		*ws;
{
    pexViewRep		pvr;
    pexViewRep		view_zero;
    CARD32		count;
    CARD32              *indices;
    pexBitmask		mask[PEXMSGetWksInfo];
    caddr_t		buf;
    Hash_table		htab;

    register int 		i;
    register pexViewEntry	*pvb = &pvr.view;

    /* Set the predefined views to the values specified in the WDT,
     * and initialize all the other views to the same settings as view 0.
     */

    /* Set up for setting a lot of views to the same values as view 0. */
    phg_utx_view_entry_to_pex( &ws->type->desc_tbl.phigs_dt.default_views[0],
	&view_zero.view );

    /* Set all the views. */
    for ( i = 1; i < ws->type->desc_tbl.phigs_dt.num_view_indices; i++ ) {
	pvr.index = i;
	if ( i && i < ws->type->desc_tbl.phigs_dt.num_predefined_views ) {
	    /* Set a predefined view. */
	    phg_utx_view_entry_to_pex(
		&ws->type->desc_tbl.phigs_dt.default_views[i], pvb );
	    (void)PEXSetViewRep( ws->display, ws->rid, &pvr );
	} else {
	    /* Set a non-predefined view to the same setting as view 0. */
	    view_zero.index = i;
	    (void)PEXSetViewRep( ws->display, ws->rid, &view_zero );
	}
    }

    /* Update the view index hash table. */
    if ( !(ws->out_ws.htab.view = phg_ut_htab_create( 10 )) ) {
	ERR_BUF( ws->erh, ERR900 );
	return 0;
    }

    bzero( (char *)mask, sizeof(mask) ); 
    PEX_BITSET(mask, PEXPWDefinedViews);
    if ( !PEXGetWksInfo( ws->display, ws->rid, mask, &buf ) ) {
	ERR_BUF( ws->erh, ERR900 );	/* TODO: use phg_pex_errno */
	return 0;

    } else {
	count = *(CARD32 *)buf;
	indices = (CARD32 *)(buf + 4);
	for ( i = 0; i < count; i++ ) {
	    if ( !phg_ut_htab_add_entry( ws->out_ws.htab.view, (int)indices[i],
		    (caddr_t)NULL ) ) {
		ERR_BUF( ws->erh, ERR900 );
		return 0;
	    }
	}
    }

    return 1;
}


Ws*
phg_wsa_open_ws( cph, cp_args, ret, css_srvr )
    Cp_handle		cph;
    Phg_args		*cp_args;
    Phg_ret		*ret;
    Cpx_css_srvr	*css_srvr;
{
    Phg_args_open_ws	*args = &cp_args->data.open_ws;
    char		*avlist[4];
    Ws			*ws;
    Pint		err;
    pexEnumTypeIndex	mode;

    ret->err = -1;
    if ( !(ws = phg_wsx_create( cph, args, css_srvr )) )
	return ws;

    ws->display = css_srvr->display;
    switch ( args->type->base_type ) {
	case WST_BASE_TYPE_X_DRAWABLE:
	    ws->drawable_id = args->conn_info.drawable_id;
	    break;

	case WST_BASE_TYPE_X_TOOL:
	    if ( !phg_wsx_setup_tool( ws, &args->conn_info, args->type ) )
		goto abort;
	    break;
    }

    /* Build an accurate WDT after the window is open. */
    avlist[0] = PHIGS_X_DISPLAY_WINDOW;
	avlist[1] = (char *)ws->display;
	avlist[2] = (char *)ws->drawable_id;
    avlist[3] = (char *)0;
    if ( !(ws->type = phg_wst_create( cph->erh, args->type, avlist )) )
	goto abort;
    ws->type->wsid = ws->id;
    ws->type->bound_status = WST_BOUND;

    if ( !phg_wsx_create_LUTs( ws, 0, &ret->err ) ) {
	ERR_BUF( ws->erh, ret->err );
	goto abort;
    }

    if ( !phg_wsx_init_LUTs( ws, 0 ) )
	goto abort;

    if ( !phg_wsx_setup_colormap( ws, &err ) ) {
	ERR_BUF(ws->erh, err);
	goto abort;
    }

    if ( !create_pex_ws_resource( ws ) ) {
	ERR_BUF( ws->erh, ERRN202 );
	goto abort;
    }

    if ( !init_view_table( ws ) )
	goto abort;

    (void)PEXUpdateWorkstation( ws->display, ws->rid );
    mode = (pexEnumTypeIndex)phg_utx_map_update_state(
	ws->type->desc_tbl.phigs_dt.out_dt.deferral_mode,
	ws->type->desc_tbl.phigs_dt.out_dt.modification_mode );
    (void)PEXSetDisplayUpdateMode( ws->display, ws->rid, mode );
    XFlush( ws->display );

    /* Fill in the rest of the ws struct. */
    ws->flags.flush = 1;
    ws->current_colour_model =
	ws->type->desc_tbl.phigs_dt.out_dt.default_colour_model;
    ws->category = ws->type->desc_tbl.phigs_dt.ws_category;
    ws->out_ws.def_mode = ws->type->desc_tbl.phigs_dt.out_dt.deferral_mode;
    ws->out_ws.mod_mode = ws->type->desc_tbl.phigs_dt.out_dt.modification_mode;
    wsa_load_funcs( ws );

    /* Fill in the return data. */
    ret->err = 0;
    ret->data.open_ws.wstype = ws->type;
    ret->data.open_ws.wst_buffer = ws->type->buffer;
    ret->data.open_ws.wst_buffer_size = ws->type->buffer_size;
    ret->data.open_ws.drawable_id = ws->drawable_id;
    ret->data.open_ws.overlay_id = ws->input_overlay_window;

    return ws;

abort:
    wsa_destroy_ws( ws );
    return (Ws *)NULL;
}


void
phg_wsa_close_ws( ws )
    Ws		*ws;
{
    if ( ws ) {
	if ( ws->display ) {
	    if ( ws->rid )
		(void)PEXUpdateWorkstation( ws->display, ws->rid );
	    XFlush( ws->display );
	    if ( ws->drawable_id )
		phg_wsx_release_window( ws );
	}
	wsa_destroy_ws( ws );
    }
}


void
phg_wsa_drawable_pick( ws, args, ret )
    Ws				*ws;
    Phg_args_drawable_pick	*args;
    Phg_ret			*ret;
{
    Ws_inp_pick		dev;
    pexDeviceCoord	dc_pt;
    CARD16		status;
    CARD32		depth;
    pexPickPath		*path;
    Ppick_path		*pick;
    int			type_found = 0;

    register int	i;

    bzero( (char *)&dev, sizeof(dev) );
    ret->err = 0;
    ret->data.drawable_pick.status = PIN_STATUS_NONE;
    ret->data.drawable_pick.pick.depth = 0;

    /* Find a PEX pick device.  Preferred one is DC. */
    for ( i = 0; i < ws->type->desc_tbl.xwin_dt.num_pick_device_types; i++) {
	if ( ws->type->desc_tbl.xwin_dt.pick_device_types[i]
		== PEXPickDeviceDC_HitBox ) {
	    dev.dev_type = PEXPickDeviceDC_HitBox;
	    type_found = 1;
	    break;
	}
    }
    if ( !type_found ) {
	if ( ws->type->desc_tbl.xwin_dt.num_pick_device_types > 0 ) {
	    dev.dev_type = ws->type->desc_tbl.xwin_dt.pick_device_types[0];
	} else {
	    ERR_BUF( ws->erh, ERR350 );
	    ret->data.drawable_pick.status = PIN_STATUS_NONE;
	    ret->err = ERR350;
	    return;
	}
    }

    /* Dummy up a pick device and enable it. */
    dev.esw = args->esw;
    dev.e_volume = args->echo_volume;
    dev.pick.status = PIN_STATUS_NONE;
    dev.pet = args->pet;
    dev.order = args->order;
    dev.ap_size = args->ap_size;
    dev.filter.incl = ws->out_ws.nset.drawable_pick_incl;
    dev.filter.excl = ws->out_ws.nset.drawable_pick_excl;
    phg_wsx_set_name_set( ws, PHG_ARGS_FLT_DRAWABLE_PICK, 0,
	&args->filter.incl_set, &args->filter.excl_set );
    (void)phg_wsa_pick_enable( ws, &dev, (CARD32)0, (char *)NULL );

    /* Convert the point to DC and resolve the pick. */
    phg_wsx_update_ws_rect( ws );
    WS_DRWBL_TO_DC2(ws, &args->point, &dc_pt);
    if ( !phg_wsa_resolve_pick( ws, &dev, &dc_pt, &status, &depth, &path)){
	ret->data.drawable_pick.status = PIN_STATUS_NO_IN;
	goto clean_up;
    }

    if ( status == PEXOk && depth > 0 ) {
	/* Convert the structure ids to the application's structure ids. */
	if ( !PHG_SCRATCH_SPACE( &ws->scratch,
		(unsigned)(depth * sizeof(Ppick_path_elem)) ) ) {
	    ERR_BUF( ws->erh, ERR900 );
	    ret->data.drawable_pick.status = PIN_STATUS_NO_IN;
	    goto clean_up;
	}
	ret->data.drawable_pick.status = PIN_STATUS_OK;
	pick = &ret->data.drawable_pick.pick;
	pick->depth = depth;
	pick->path_list = (Ppick_path_elem *)ws->scratch.buf;
	for ( i = 0; i < depth; i++ ) {
	    pick->path_list[i].struct_id =
		CPA_DECODE_STRUCT_ID( ws->css_srvr, path[i].sid );
	    pick->path_list[i].elem_pos = path[i].offset;
	    pick->path_list[i].pick_id = path[i].pickid;
	}
    }

clean_up:
    phg_wsa_pick_disable( ws, &dev );
}


void
phg_wsa_map_points( ws, args, ret )
    Ws				*ws;
    Phg_args_map_points		*args;
    Phg_ret			*ret;
{
    pexDeviceCoord	*dc_pts;

    register int	i;

    ret->err = 0;
    ret->data.map_points.view_index = 0;
    ret->data.map_points.points.num_points = 0;
    ret->data.map_points.points.points = (Ppoint3 *)NULL;

    /* Allocate space for both the DC and WC points. */
    if ( !PHG_SCRATCH_SPACE( &ws->scratch,
	    (unsigned)(args->points.num_points
		* (sizeof(Ppoint) + sizeof(Ppoint3))) ) ) {
	ERR_BUF( ws->erh, ERR900 );
	return;
    }
    dc_pts = (pexDeviceCoord *)ws->scratch.buf;
    ret->data.map_points.points.points =
	(Ppoint3 *)(dc_pts + args->points.num_points);

    /* Convert the points to DC. */
    phg_wsx_update_ws_rect( ws );
    for ( i = 0; i < args->points.num_points; i++ ) {
	/* Z coord is already in DC. */
	WS_DRWBL_TO_DC2(ws, &args->points.points[i], &dc_pts[i]);
	dc_pts[i].z = args->points.points[i].z;
    }

    /* Convert the DC points to WC. */
    if ( !phg_wsa_resolve_stroke( ws, args->points.num_points, dc_pts, 0,
	    &ret->data.map_points.view_index,
	    &ret->data.map_points.points ) ) {
	ret->data.map_points.points.num_points = 0;
    }
}

void
phg_wsa_redraw_regions( ws, args )
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
    XFlush( ws->display );
}
