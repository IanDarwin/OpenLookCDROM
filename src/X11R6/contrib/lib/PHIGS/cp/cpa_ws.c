/* $XConsortium: cpa_ws.c,v 5.7 94/04/17 20:41:17 hersh Exp $ */

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

/* PEX/PHIGS open and close functions. */

#include "phg.h"
#include "cp.h"
#include "ws.h"
#include "cp_priv.h"
#include "PEX.h"
#include "PEXproto.h"
#include "PEXmacs.h"
#include "PEXfuncs.h"
#include "phigspex.h"


/* The functions in this file control the WS operations.  Since the WS is
 * in the server it's possible for the Cpa layer to send most WS requests
 * directly, so most operations currently do not need the WS layer
 * indirection.
 */

void
phg_cpa_ws_update( cph, cp_args, ws )
    Cp_handle	cph;
    Phg_args	*cp_args;
    Ws		*ws;
{
    Phg_args_ws_update	*args = &cp_args->data.ws_update;

    (void)PEXExecuteDeferredActions( ws->display, ws->rid );
    if ( args->flag == PFLAG_PERFORM )
	(void)PEXUpdateWorkstation( ws->display, ws->rid );
    XFlush( ws->display );
}

void
phg_cpa_ws_redraw_all( cph, cp_args, ws )
    Cp_handle	cph;
    Phg_args	*cp_args;
    Ws		*ws;
{
    (void)PEXExecuteDeferredActions( ws->display, ws->rid );
    /* No counterpart in PEX for the control flag. */
    (void)PEXRedrawAllStructures( ws->display, ws->rid );
    XFlush( ws->display );
}


void
phg_cpa_set_disp_state( cph, cp_args, ws )
    Cp_handle	cph;
    Phg_args	*cp_args;
    Ws		*ws;
{
    Phg_args_set_disp_state	*args = &cp_args->data.set_disp_state;
    pexEnumTypeIndex		mode;
    Ws				*tws;

    ws->out_ws.def_mode = args->mode;
    ws->out_ws.mod_mode = args->mod_mode;
    mode = (pexEnumTypeIndex)phg_utx_map_update_state( args->mode,
	args->mod_mode );
    (void)PEXSetDisplayUpdateMode( ws->display, ws->rid, mode );
    if ( !CPA_WS_BUFFERING( ws ) ) {
	/* See if we can buffer requests on this server. */
	if ( CPA_BUFFERING( ws->css_srvr ) ) {
	    CPX_FOR_ALL_WS( cph, tws ) {
		if ( tws->css_srvr == ws->css_srvr && tws->flags.flush )
		    break;
	    }
	    if ( !tws )
		ws->css_srvr->model.a.flags.flush = 0;
	} /* else, continue buffering */
    } else
	ws->css_srvr->model.a.flags.flush = 1;
}


void
phg_cpa_inq_disp_update_state( cph, cp_args, ret, ws )
    Cp_handle	cph;
    Phg_args	*cp_args;
    Phg_ret	*ret;
    Ws		*ws;
{
    caddr_t			buf;
    pexBitmask			mask[PEXMSGetWksInfo];
    Phg_ret_update_state	*st = &ret->data.update_state;

    ret->err = 0;
    bzero((char *)mask, sizeof(mask));   
    PEX_BITSET(mask,PEXPWVisualState);
    PEX_BITSET(mask,PEXPWDisplaySurface);
    if ( !PEXGetWksInfo( ws->display, ws->rid, mask, &buf ))
	ret->err = ERR900;	/* TODO: Use phg_pex_errno. */
    else {
	st->def_mode = ws->out_ws.def_mode;
	st->mod_mode = ws->out_ws.mod_mode;
	st->state = PEX_CONV_TO_Pvisualrep( *(CARD32 *)buf );
	buf += 4;
	st->display_surf = PEX_CONV_TO_Pdisp_surf_empty( *(CARD32 *)buf );
    }
}


Ws_handle
phg_cpa_open_ws( cph, cp_args, ret, css_srvr )
    Cp_handle		cph;
    Phg_args		*cp_args;
    Phg_ret		*ret;
    Cpx_css_srvr	*css_srvr;
{
    Phg_args_open_ws	*args = &cp_args->data.open_ws;
    Ws			*ws;

    if ( ws = phg_wsa_open_ws( cph, cp_args, ret, css_srvr ) ) {
	if ( ws->flags.flush )
	    css_srvr->model.a.flags.flush = 1;
	if ( ws->cph->flags.monitor_active ) {
	    args->type = ws->type;
	    args->wsrid = ws->rid;
	    args->conn_info.drawable_id = ws->drawable_id;
	    args->css_srvr_type = css_srvr->type;
	    args->dpy_resource_base = css_srvr->model.a.dpy_resource_base;
	    args->dpy_resource_shift = css_srvr->model.a.dpy_resource_shift;
	    phg_cpc_class_SPECIAL( cph, cp_args, ret );
	    if ( ret->err ) {
		(ws->close)( ws );
		ws = (Ws *)NULL;
	    } else {
		ws->input_overlay_window = ret->data.open_ws.overlay_id;
		/* Ignore the WDT returned from the monitor, it's a
		 * duplicate of the one sent to it.
		 */
		ret->data.open_ws.wstype = ws->type;
	    }
	}
    }

    return ws;
}


void
phg_cpa_close_ws( cph, cp_args, ws )
    Cp_handle	cph;
    Phg_args	*cp_args;
    Ws		*ws;
{
    if ( ws->cph->flags.monitor_active )
	phg_cpc_class_SPECIAL( cph, cp_args, (Phg_ret *)NULL );
    (*ws->close)( ws );
}


void
phg_cpa_post_struct( cph, cp_args, ws )
    Cp_handle	cph;
    Phg_args	*cp_args;
    Ws		*ws;
{
    Phg_args_post_struct	*args = &cp_args->data.post_struct;
    Cpa_struct_data		*stp;

    /* See if structure exists, create it if it doesn't. */
    if ( !(stp = phg_cpa_struct_exists( cph, ws->css_srvr, CPX_BY_SID,
	    args->struct_id, CPA_STRUCT_OP_CHECK )) )
	if ( stp = phg_cpa_create_struct( cph, ws->css_srvr, args->struct_id,
	    &cph->psl->edit_mode, 0 ) )
	    phg_cpa_link_struct( cph, ws->css_srvr, stp );

    if ( stp ) {
	(void)PEXPostStructure( ws->display, ws->rid, stp->xid,
	    (PEXFLOAT)args->disp_pri );
	WS_FLUSH( ws )
    }
}


void
phg_cpa_unpost_struct( cph, cp_args, ws )
    Cp_handle	cph;
    Phg_args	*cp_args;
    Ws		*ws;
{
    Cpa_struct_data		*stp;

    if ( (stp = phg_cpa_struct_exists( cph, ws->css_srvr, CPX_BY_SID,
	    cp_args->data.unpost_struct.struct_id, CPA_STRUCT_OP_CHECK )) ) {
	(void)PEXUnpostStructure( ws->display, ws->rid, stp->xid );
	WS_FLUSH( ws );
    }
}


void
phg_cpa_unpost_all( cph, cp_args, ws )
    Cp_handle	cph;
    Phg_args	*cp_args;
    Ws		*ws;
{
    (void)PEXUnpostAllStructures( ws->display, ws->rid );
    WS_FLUSH( ws );
}


void
phg_cpa_set_rep( cph, cp_args, ws )
    Cp_handle	cph;
    Phg_args	*cp_args;
    Ws		*ws;
{
    Phg_args_set_rep	*args = &cp_args->data.set_rep;

    switch ( args->type ) {
	case PHG_ARGS_LNREP:
	case PHG_ARGS_EXTLNREP:
	case PHG_ARGS_MKREP:
	case PHG_ARGS_EXTMKREP:
	case PHG_ARGS_TXREP:
	case PHG_ARGS_EXTTXREP:
	case PHG_ARGS_INTERREP:
	case PHG_ARGS_EXTINTERREP:
	case PHG_ARGS_EDGEREP:
	case PHG_ARGS_EXTEDGEREP:
	case PHG_ARGS_PTREP:
	case PHG_ARGS_EXTPTREP:
	case PHG_ARGS_DCUEREP:
	case PHG_ARGS_LIGHTSRCREP:
	case PHG_ARGS_COLRMAPREP:
	    phg_wsx_set_LUT_entry( ws, args->type, &args->rep,
		(Pgcolr*)NULL );
	    break;
	case PHG_ARGS_VIEWREP: {
	    pexViewRep		pex_view;

	    /* The binding verifies the index. */
	    pex_view.index = args->rep.index;
	    (void)phg_utx_view_entry_to_pex( &args->rep.bundl.viewrep,
		&pex_view.view );
	    (void)PEXSetViewRep( ws->display, ws->rid, &pex_view );
	    } break;
	case PHG_ARGS_COREP: {
	    Pgcolr	gcolr;

	    /* Store in current colour model. */
	    gcolr.type = ws->current_colour_model;
	    gcolr.val.general.x = args->rep.bundl.corep.rgb.red;
	    gcolr.val.general.y = args->rep.bundl.corep.rgb.green;
	    gcolr.val.general.z = args->rep.bundl.corep.rgb.blue;
	    phg_wsx_set_LUT_entry( ws, args->type, &args->rep, &gcolr );
	    } break;
    }
    WS_FLUSH( ws );
}


void
phg_cpa_inq_rep( cph, cp_args, ret, ws )
    Cp_handle	cph;
    Phg_args	*cp_args;
    Phg_ret	*ret;
    Ws		*ws;
{
    Phg_args_q_rep	*args = &cp_args->data.q_rep;

    ret->err = 0;
    switch ( args->rep_type ) {
	case PHG_ARGS_LNREP:
	case PHG_ARGS_EXTLNREP:
	case PHG_ARGS_MKREP:
	case PHG_ARGS_EXTMKREP:
	case PHG_ARGS_TXREP:
	case PHG_ARGS_EXTTXREP:
	case PHG_ARGS_INTERREP:
	case PHG_ARGS_EXTINTERREP:
	case PHG_ARGS_EDGEREP:
	case PHG_ARGS_EXTEDGEREP:
	case PHG_ARGS_PTREP:
	case PHG_ARGS_EXTPTREP:
	case PHG_ARGS_DCUEREP:
	case PHG_ARGS_LIGHTSRCREP:
	case PHG_ARGS_COLRMAPREP:
	    phg_wsx_inq_LUT_entry( ws, args->index, args->type,
		args->rep_type, ret, (Pgcolr *)NULL, (Pview_rep3 *)NULL );
	    break;
	case PHG_ARGS_COREP: {
	    Pgcolr		src_gcolr, gcolr;
	    Pcolr_rep	*cb = &ret->data.rep.corep;

	    /* Convert to current colour model. */
	    phg_wsx_inq_LUT_entry( ws, args->index, args->type,
		args->rep_type, ret, &src_gcolr, (Pview_rep3 *)NULL );
	    if ( !ret->err ) {
		gcolr.type = ws->current_colour_model;
		(void)phg_utx_convert_colour( &src_gcolr, &gcolr,
		    &ws->type->desc_tbl.phigs_dt.out_dt.chroma_info );
		cb->rgb.red = gcolr.val.general.x;
		cb->rgb.green = gcolr.val.general.y;
		cb->rgb.blue = gcolr.val.general.z;
	    }
	} break;
    }
}


void
phg_cpa_inq_view_rep( cph, cp_args, ret, ws )
    Cp_handle	cph;
    Phg_args	*cp_args;
    Phg_ret	*ret;
    Ws		*ws;
{
    Phg_args_q_view_rep	*args = &cp_args->data.q_view_rep;
    Phg_ret_view_rep	*view_rep = &ret->data.view_rep;
    CARD16		pex_update;
    pexViewRep		*pex_rep;
    Cp_a		*asrvr = &ws->css_srvr->model.a;

    ret->err = 0;
    if ( !PHG_SCRATCH_SPACE( &asrvr->scratch, 2*sizeof(Pview_rep3) ) )
	ret->err = ERR900;
    else if ( !PEXGetViewRep( ws->display, ws->rid,
	    (pexTableIndex)args->index, &pex_update, &pex_rep ) )
	ret->err = ERR900;	/* TODO: Use phg_pex_errno. */
    else {
	view_rep->update_state = PEX_CONV_TO_Pupdatest( pex_update );
	view_rep->cur_rep = (Pview_rep3 *)asrvr->scratch.buf;
	view_rep->req_rep = view_rep->cur_rep + 1;
	(void)phg_utx_view_entry_from_pex( &pex_rep[0].view,
	    view_rep->req_rep );
	(void)phg_utx_view_entry_from_pex( &pex_rep[1].view,
	    view_rep->cur_rep );
    }
}


void
phg_cpa_set_hlhsr_mode( cph, cp_args, ws )
    Cp_handle	cph;
    Phg_args	*cp_args;
    Ws		*ws;
{
    (void)PEXSetHlhsrMode( ws->display, ws->rid,
	(pexEnumTypeIndex)
	    PEX_CONV_PHIGS_HLHSR_MODE(cp_args->data.set_hlhsr_mode.mode ));
    WS_FLUSH( ws );
}


void
phg_cpa_inq_hlhsr_mode( cph, cp_args, ret, ws )
    Cp_handle	cph;
    Phg_args	*cp_args;
    Phg_ret	*ret;
    Ws		*ws;
{
    caddr_t		buf;
    pexBitmask		mask[PEXMSGetWksInfo];
    Phg_ret_hlhsr_mode	*hlhsr_mode = &ret->data.hlhsr_mode;

    ret->err = 0;
    bzero((char *)mask, sizeof(mask));   
    PEX_BITSET(mask, PEXPWHlhsrUpdate);
    PEX_BITSET(mask, PEXPWReqHlhsrMode);
    PEX_BITSET(mask, PEXPWCurHlhsrMode);
    if ( !PEXGetWksInfo( ws->display, ws->rid, mask, &buf ))
	ret->err = ERR900;	/* TODO: Use phg_pex_errno. */
    else {
	hlhsr_mode->state = PEX_CONV_TO_Pupdatest( *(CARD32*)buf );
	buf += 4;
	hlhsr_mode->req_mode = PEX_CONV_PEX_HLHSR_MODE( *(CARD32 *)buf );
	buf += 4;
	hlhsr_mode->cur_mode = PEX_CONV_PEX_HLHSR_MODE( *(CARD32 *)buf );
    }
}


void
phg_cpa_set_ws_win( cph, cp_args, ws )
    Cp_handle	cph;
    Phg_args	*cp_args;
    Ws		*ws;
{
    pexNpcSubvolume	sv, *current;
    pexBitmask		mask[PEXMSGetWksInfo];

    register	Phg_args_set_ws_winvp	*args = &cp_args->data.set_ws_winvp;
    register	pexNpcSubvolume		*svp = &sv;

    svp->minval.x = args->limits.x_min;
    svp->minval.y = args->limits.y_min;
    svp->maxval.x = args->limits.x_max;
    svp->maxval.y = args->limits.y_max;
    if ( args->two_d ) {
	bzero((char *)mask, sizeof(mask));   
	PEX_BITSET(mask, PEXPWReqNpcSubvolume);
	if ( PEXGetWksInfo(ws->display, ws->rid, mask, (char **)&current )) {
	    svp->minval.z = current->minval.z;
	    svp->maxval.z = current->maxval.z;
	} else {
	    /* TODO: Use phg_pex_errno instead of assuming error 900 */
	    ERR_BUF( ws->erh, ERR900 );
	    return;
	}
    } else {
	svp->minval.z = args->limits.z_min;
	svp->maxval.z = args->limits.z_max;
    }
    (void)PEXSetWksWindow( ws->display, ws->rid, svp );
    WS_FLUSH( ws );
}


void
phg_cpa_set_ws_vp( cph, cp_args, ws )
    Cp_handle	cph;
    Phg_args	*cp_args;
    Ws		*ws;
{
    pexViewport		vp, *current;
    pexBitmask		mask[PEXMSGetWksInfo];

    register	Phg_args_set_ws_winvp	*args = &cp_args->data.set_ws_winvp;
    register	pexViewport		*vpp = &vp;

    vpp->useDrawable = PEXOff;
    vpp->minval.x = args->limits.x_min;
    vpp->minval.y = args->limits.y_min;
    vpp->maxval.x = args->limits.x_max;
    vpp->maxval.y = args->limits.y_max;
    if ( args->two_d ) {
	bzero((char *)mask, sizeof(mask));   
	PEX_BITSET(mask, PEXPWReqWksViewport);
	if ( PEXGetWksInfo(ws->display, ws->rid, mask, (char **)&current) ) {
	    vpp->minval.z = current->minval.z;
	    vpp->maxval.z = current->maxval.z;
	} else {
	    /* TODO: Use phg_pex_errno instead of assuming error 900 */
	    ERR_BUF( ws->erh, ERR900 );
	    return;
	}
    } else {
	vpp->minval.z = args->limits.z_min;
	vpp->maxval.z = args->limits.z_max;
    }
    (void)PEXSetWksViewport( ws->display, ws->rid, vpp );
    WS_FLUSH( ws );
}


void
phg_cpa_inq_ws_xform( cph, cp_args, ret, ws )
    Cp_handle	cph;
    Phg_args	*cp_args;
    Phg_ret	*ret;
    Ws		*ws;
{
    caddr_t	data;
    pexBitmask	mask[PEXMSGetWksInfo];

    ret->err = 0;
    bzero((char *)mask, sizeof(mask));   
    PEX_BITSET(mask, PEXPWWksUpdate);
    PEX_BITSET(mask, PEXPWReqNpcSubvolume);
    PEX_BITSET(mask, PEXPWCurNpcSubvolume);
    PEX_BITSET(mask, PEXPWReqWksViewport);
    PEX_BITSET(mask, PEXPWCurWksViewport);
    if ( !PEXGetWksInfo( ws->display, ws->rid, mask, &data ))
	ret->err = ERR900;	/* TODO: Use phg_pex_errno. */
    else
	(void)phg_utx_ws_xform_from_pex( data, &ret->data.ws_xform );
}


void
phg_cpa_set_filter( cph, cp_args, ws )
    Cp_handle	cph;
    Phg_args	*cp_args;
    Ws		*ws;
{
    Phg_args_set_filter	*args = &cp_args->data.set_filter;

    if ( args->type == PHG_ARGS_FLT_PICK ) {
	if ( cph->flags.monitor_active )
	    /* Send it to the PM to set. */
	    phg_cpc_class_C( cph, cp_args );
    } else
	phg_wsx_set_name_set( ws, args->type, (Pint)0, &args->inc_set,
	    &args->exc_set );
    WS_FLUSH( ws );
}


void
phg_cpa_inq_filter( cph, cp_args, ret, ws )
    Cp_handle	cph;
    Phg_args	*cp_args;
    Phg_ret	*ret;
    Ws		*ws;
{
    phg_wsx_inq_name_set( ws, cp_args->data.q_filter.type, (Pint)0, ret );
}


void
phg_cpa_inq_posted( cph, cp_args, ret, ws )
    Cp_handle	cph;
    Phg_args	*cp_args;
    Phg_ret	*ret;
    Ws		*ws;
{
    Cp_a	*asrvr = &ws->css_srvr->model.a;
    caddr_t	buf;
    CARD32	count;
    pexBitmask	mask[PEXMSGetWksInfo];

    register	int			i;
    register	Pposted_struct		*postings;
    register	pexStructureInfo	*xpostings;
    register	Cpa_struct_data		*stp;

    ret->err = 0;
    bzero((char *)mask, sizeof(mask));   
    PEX_BITSET(mask, PEXPWPostedStructures);
    if ( !PEXGetWksInfo( ws->display, ws->rid, mask, &buf ) )
	ret->err = ERR900;	/* TODO: use phg_pex_errno. */
    else {
	count = *(CARD32 *)buf;
	xpostings = (pexStructureInfo *)(buf + sizeof(CARD32));
	if ( !PHG_SCRATCH_SPACE( &asrvr->scratch, count * sizeof(*postings)) )
	    ret->err = ERR900;
	else {
	    ret->data.postlist.num_postings = count;
	    postings = (Pposted_struct *)asrvr->scratch.buf;
	    ret->data.postlist.postings = postings;
	    for ( i = 0; i < count; i++ ) {
		postings[i].disp_pri = xpostings[i].priority;
		if ( stp = phg_cpa_struct_exists( cph, ws->css_srvr, CPX_BY_XID,
			(int)xpostings[i].sid, CPA_STRUCT_OP_CHECK ) )
		    postings[i].id = stp->sid;
		else
		    /* Shouldn't happen, but do something just in case. */
		    postings[i].id = xpostings[i].sid;
	    }
	}
    }
}


void
phg_cpa_set_view_input_priority( cph, cp_args, ws )
    Cp_handle	cph;
    Phg_args	*cp_args;
    Ws		*ws;
{
    Phg_args_set_view_input_prio  *args = &cp_args->data.set_view_input_prio;

    (void)PEXSetViewPriority( ws->display, ws->rid,
	(pexTableIndex)args->idx, (pexTableIndex)args->ref_idx,
	(CARD16)PEX_CONV_FROM_Pvpri(args->priority) );
}


void
phg_cpa_inq_indices( cph, cp_args, ret, ws )
    Cp_handle	cph;
    Phg_args	*cp_args;
    Phg_ret	*ret;
    Ws		*ws;
{
    caddr_t		buf;
    CARD32		count;
    CARD32       	*indices;

    register	int	i;
    register	Pint	*list;

    ret->err = 0;
    if ( cp_args->data.q_indices.type  != PHG_ARGS_VIEWREP ) {
	phg_wsx_inq_LUT_indices( ws, cp_args->data.q_indices.type, ret );

    } else {
	/* Need to get the view indices from the workstation resource. */
	pexBitmask	mask[PEXMSGetWksInfo];

	bzero( (char *)mask, sizeof(mask) );   
	PEX_BITSET(mask, PEXPWDefinedViews);
	if ( !PEXGetWksInfo( ws->display, ws->rid, mask, &buf ) )
	    ret->err = ERR900;	/* TODO: use phg_pex_errno. */
	else {
	    count = *(CARD32 *)buf;
	    indices = (CARD32 *)(buf + 4);
	    if ( count > 0 &&
		    !PHG_SCRATCH_SPACE( &ws->scratch, count * sizeof(Pint)) )
		ret->err = ERR900;
	    else {
		ret->data.int_list.num_ints = count;
		ret->data.int_list.ints = list = (Pint *)ws->scratch.buf;
		/* View entries come back as CARD32s
		 */
		for ( i = 0; i < count; i++, list++, indices++ )
		    *list = *indices;
	    }
	}
    }
}


void
phg_cpa_inq_text_extent( cph, cp_args, ret, ws )
    Cp_handle		cph;
    Phg_args		*cp_args;
    Phg_ret		*ret;
    Ws			*ws;
{
    phg_wsx_inq_text_extent( cph, &cp_args->data.q_text_extent, ret, ws );
}



void
phg_cpa_ws_drawable_pick( cph, cp_args, ret, ws )
    Cp_handle		cph;
    Phg_args		*cp_args;
    Phg_ret		*ret;
    Ws			*ws;
{
    ret->err = 0;
    if (ws->drawable_pick )
	(*ws->drawable_pick )( ws, &cp_args->data.drawable_pick, ret );
    else {
	ret->err = ERR350;
	ERR_BUF( cph->erh, ERR350 );
    }
}


void
phg_cpa_ws_map_points( cph, cp_args, ret, ws )
    Cp_handle		cph;
    Phg_args		*cp_args;
    Phg_ret		*ret;
    Ws			*ws;
{
    ret->err = 0;
    if (ws->map_points )
	(*ws->map_points )( ws, &cp_args->data.map_points, ret );
    else {
	ret->err = ERR350;
	ERR_BUF( cph->erh, ERR350 );
    }
}

void
phg_cpa_ws_redraw_regions( cph, cp_args, ws )
    Cp_handle		cph;
    Phg_args		*cp_args;
    Ws			*ws;
{
    if ( ws->redraw_regions )
	(*ws->redraw_regions)( ws, &cp_args->data.redraw_regions );
}

void
phg_cpa_ws_synch( cph, cp_args, ws )
    Cp_handle		cph;
    Phg_args		*cp_args;
    Ws			*ws;
{
    if ( ws->synch )
	(*ws->synch)( ws );
}


void
phg_cpa_inq_colr_map_meth_st( cph, cp_args, ret, ws )
    Cp_handle	cph;
    Phg_args	*cp_args;
    Phg_ret		*ret;
    Ws		*ws;
{
    phg_wsx_inq_colr_map_meth_st( cph, &cp_args->data.q_colr_map_meth_st, ret, ws );
}
