/* $XConsortium: cpb_clnt.c,v 5.4 94/04/17 20:41:18 rws Exp $ */

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
#include "cp.h"
#include "cp_priv.h"
#include "ws.h"
#include "PEX.h"
#include "PEXmacs.h"
#include "PEXfuncs.h"
#include "PEXproto.h"
#include "phigspex.h"


static void
cpbc_load_two_process_funcs( cph, css_srvr )
    Cp_handle		cph;
    Cpx_css_srvr	*css_srvr;
{
    /* Control functions. */
    css_srvr->destroy = phg_cpbc_destroy;
    css_srvr->set_err_hand_mode = phg_cpc_class_B;
    css_srvr->close_phigs = (void(*)())NULL;

    /* CSS functions.  The first batch are broadcast to the monitor by the
     * top level cpx functions so they are not defined in the client process.
     * Most of the rest get forwarded to the monitor.
     */
    css_srvr->add_el = phg_cpc_class_C;
    css_srvr->copy_all_els = phg_cpc_class_B;
    css_srvr->open_struct = phg_cpc_class_D;
    css_srvr->close_struct = phg_cpc_class_B;
    css_srvr->set_el_ptr = phg_cpc_class_B;
    css_srvr->set_edit_mode = phg_cpc_class_B;
    css_srvr->delete_el = phg_cpc_class_B;
    css_srvr->delete_struct = phg_cpc_class_B;
    css_srvr->delete_struct_net = phg_cpc_class_B;
    css_srvr->delete_all_structs = phg_cpc_class_B;
    css_srvr->change_struct_id = phg_cpc_class_B;
    css_srvr->change_struct_refs = phg_cpc_class_B;
    css_srvr->change_struct_idrefs = phg_cpc_class_B;
    css_srvr->el_search = phg_cpc_class_CD;
    css_srvr->inc_spa_search = phg_cpc_class_CE;
    css_srvr->inq_colour_model = (void(*)())NULL;
    css_srvr->inq_struct_status = phg_cpc_class_D;
    css_srvr->inq_struct_ids = phg_cpc_class_E;
    css_srvr->inq_el_ptr = phg_cpc_class_D;
    css_srvr->inq_el_type_size = phg_cpc_class_D;
    css_srvr->inq_el_content = phg_cpc_class_E;
    css_srvr->inq_wss_posted_to = phg_cpc_class_E;
    css_srvr->inq_hierarchy = phg_cpc_class_E;
    css_srvr->inq_text_extent = phg_cpc_class_CD;
    css_srvr->struct_exists = phg_cpbc_struct_exists;
    css_srvr->full_copy_to_type_a = phg_cpx_full_copy_b_to_a;
    css_srvr->full_copy_to_type_b = (int(*)())NULL;

    /* Archive functions. */
    css_srvr->ar_archive = phg_cpbc_ar_archive;

    /* Workstation functions. */
    css_srvr->open_ws = phg_cpbc_open_ws;
    css_srvr->close_ws = phg_cpbc_close_ws;
    css_srvr->ws_update = phg_cpc_class_B;
    css_srvr->ws_redraw_all = phg_cpc_class_B;
    css_srvr->set_disp_state = phg_cpc_class_B;
    css_srvr->message = phg_cpc_class_C;
    css_srvr->set_rep = phg_cpc_class_C;
    css_srvr->set_filter = phg_cpc_class_C;
    css_srvr->set_colour_model = phg_cpc_class_B;
    css_srvr->set_hlhsr_mode = phg_cpc_class_B;
    css_srvr->set_view_input_prio = phg_cpc_class_B;
    css_srvr->set_ws_win = phg_cpc_class_B;
    css_srvr->set_ws_vp = phg_cpc_class_B;
    css_srvr->post_struct = phg_cpc_class_B;
    css_srvr->unpost_struct = phg_cpc_class_B;
    css_srvr->unpost_all = phg_cpc_class_B;
    css_srvr->inq_indices = phg_cpc_class_E;
    css_srvr->inq_filter = phg_cpc_class_E;
    css_srvr->inq_posted = phg_cpc_class_E;
    css_srvr->inq_representation = phg_cpc_class_E;
    css_srvr->inq_view_rep = phg_cpc_class_E;
    css_srvr->inq_hlhsr_mode = phg_cpc_class_D;
    css_srvr->inq_disp_update_state = phg_cpc_class_D;
    css_srvr->inq_ws_xform = phg_cpc_class_D;
    css_srvr->inq_win_info = phg_cpc_class_D;
    css_srvr->inq_dpy_and_drawable = phg_cpx_inq_dpy_and_drawable;
    css_srvr->inp_init_dev = phg_cpc_class_C;
    css_srvr->inp_set_mode = phg_cpc_class_B;
    css_srvr->inp_request = phg_cpc_inp_request;
    css_srvr->inp_sample = phg_cpc_class_E;
    css_srvr->inq_inp_dev_state = phg_cpc_class_E;
    css_srvr->drawable_pick = phg_cpc_class_CE;
    css_srvr->map_points = phg_cpc_class_CE;
    css_srvr->redraw_regions = phg_cpc_class_C;
    css_srvr->ws_synch = phg_cpc_class_SPECIAL;
    css_srvr->inq_colr_map_meth_st = phg_cpc_class_D;
}

static void
cpbc_load_single_process_funcs( cph, css_srvr )
    Cp_handle		cph;
    Cpx_css_srvr	*css_srvr;
{
    css_srvr->destroy = phg_cpbc_destroy;
    css_srvr->open_ws = phg_cpb_open_ws;
    css_srvr->close_ws = phg_cpb_close_ws;
    css_srvr->ws_update = phg_cpb_ws_update;
    css_srvr->ws_redraw_all = phg_cpb_ws_redraw_all;
    css_srvr->set_disp_state = phg_cpb_set_disp_state;
    css_srvr->message = phg_cpb_message;
    css_srvr->set_rep = phg_cpb_set_rep;
    css_srvr->set_filter = phg_cpb_set_filter;
    css_srvr->set_colour_model = (void(*)())NULL;
    css_srvr->set_hlhsr_mode = phg_cpb_set_hlhsr_mode;
    css_srvr->set_view_input_prio = phg_cpb_set_view_input_priority;
    css_srvr->set_ws_win = phg_cpb_set_ws_win;
    css_srvr->set_ws_vp = phg_cpb_set_ws_vp;
    css_srvr->add_el = phg_cpb_add_el;
    css_srvr->copy_all_els = phg_cpb_copy_all_els;
    css_srvr->open_struct = phg_cpb_open_struct;
    css_srvr->close_struct = phg_cpb_close_struct;
    css_srvr->set_el_ptr = phg_cpb_set_el_ptr;
    css_srvr->set_edit_mode = phg_cpb_set_edit_mode;
    css_srvr->delete_el = phg_cpb_delete_el;
    css_srvr->delete_struct = phg_cpb_delete_struct;
    css_srvr->delete_struct_net = phg_cpb_delete_struct_net;
    css_srvr->delete_all_structs = phg_cpb_delete_all_structs;
    css_srvr->change_struct_id = phg_cpb_change_struct_id;
    css_srvr->change_struct_refs = phg_cpb_change_struct_refs;
    css_srvr->change_struct_idrefs = phg_cpb_change_struct_idrefs;
    css_srvr->post_struct = phg_cpb_post_struct;
    css_srvr->unpost_struct = phg_cpb_unpost_struct;
    css_srvr->unpost_all = phg_cpb_unpost_all;
    css_srvr->ar_archive = phg_cpbc_ar_archive;
    css_srvr->set_err_hand_mode = (void(*)())NULL;
    css_srvr->el_search = phg_cpb_el_search;
    css_srvr->inc_spa_search = phg_cpb_inc_spa_search;
    css_srvr->inq_colour_model = (void(*)())NULL;
    css_srvr->inq_struct_status = phg_cpb_inq_struct_status;
    css_srvr->inq_struct_ids = phg_cpb_inq_struct_ids;
    css_srvr->inq_el_ptr = phg_cpb_inq_el_ptr;
    css_srvr->inq_el_type_size = phg_cpb_inq_el_type_size;
    css_srvr->inq_el_content = phg_cpb_inq_el_content;
    css_srvr->inq_indices = phg_cpb_inq_indices;
    css_srvr->inq_filter = phg_cpb_inq_filter;
    css_srvr->inq_posted = phg_cpb_inq_posted;
    css_srvr->inq_wss_posted_to = phg_cpb_inq_wss_posted_to;
    css_srvr->inq_hierarchy = phg_cpb_inq_hierarchy;
    css_srvr->inq_representation = phg_cpb_inq_rep;
    css_srvr->inq_view_rep = phg_cpb_inq_view_rep;
    css_srvr->inq_hlhsr_mode = phg_cpb_inq_hlhsr_mode;
    css_srvr->inq_disp_update_state = phg_cpb_inq_disp_update_state;
    css_srvr->inq_ws_xform = phg_cpb_inq_ws_xform;
    css_srvr->inq_text_extent = phg_cpb_inq_text_extent;
    css_srvr->inq_win_info = phg_cpx_get_win_info;
    css_srvr->inq_dpy_and_drawable = phg_cpx_inq_dpy_and_drawable;
    css_srvr->close_phigs = (void(*)())NULL;
    css_srvr->struct_exists = phg_cpbc_struct_exists;
    css_srvr->full_copy_to_type_a = phg_cpx_full_copy_b_to_a;
    css_srvr->full_copy_to_type_b = (int(*)())NULL;	/* none needed */
    css_srvr->drawable_pick = phg_cpb_ws_drawable_pick;
    css_srvr->map_points = phg_cpb_ws_map_points;
    css_srvr->redraw_regions = phg_cpb_ws_redraw_regions;
    css_srvr->ws_synch = phg_cpb_ws_synch;
    css_srvr->inq_colr_map_meth_st = phg_cpb_inq_colr_map_meth_st;

    /* No PHIGS input in single process case. */
    css_srvr->inp_init_dev = (void(*)())NULL;
    css_srvr->inp_set_mode = (void(*)())NULL;
    css_srvr->inp_request = (void(*)())NULL;
    css_srvr->inp_sample = (void(*)())NULL;
    css_srvr->inq_inp_dev_state = (void(*)())NULL;
}


Cpx_css_srvr*
phg_cpbc_init( cph )
    Cp_handle		cph;
{
    Cpx_css_srvr	*css_srvr;
    int			create_css_flag;

    create_css_flag = cph->flags.monitor_active ? 0 : 1;
    if ( !(css_srvr = phg_cpb_create( cph, create_css_flag )) ) {
	ERR_BUF( cph->erh, ERR900 );

    } else {
	if ( cph->flags.monitor_active ) {
	    css_srvr->flags.uses_monitor_css = 1;
	    cpbc_load_two_process_funcs( cph, css_srvr );
	} else
	    cpbc_load_single_process_funcs( cph, css_srvr );
    }

    return css_srvr;
}


void
phg_cpbc_destroy( cph, css_srvr )
    Cp_handle		cph;
    Cpx_css_srvr	*css_srvr;
{
    if ( css_srvr->flags.uses_monitor_css ) {
	Phg_args	cp_args;

	cp_args.op = (unsigned)CP_FUNC_OP_DELETE_ALL_STRUCTS;
	phg_cpc_class_B( cph, &cp_args );
    }
    phg_cpb_destroy( css_srvr );
}


void
phg_cpbc_close_ws( cph, cp_args, ws )
    Cp_handle		cph;
    Phg_args		*cp_args;
    Ws			*ws;
{
    /* Shut it down in the monitor. */
    phg_cpc_class_SPECIAL( cph, cp_args, (Phg_ret *)NULL );
    /* Destroy the local stuff. */
    phg_wsx_destroy( ws );
}


Ws_handle
phg_cpbc_open_ws( cph, cp_args, ret, css_srvr )
    Cp_handle		cph;
    Phg_args		*cp_args;
    Phg_ret		*ret;
    Cpx_css_srvr	*css_srvr;
{
    int			startup = 0;
    Wst			*wst;
    Ws			*ws;
    Phg_args_open_ws	*args = &cp_args->data.open_ws;
    Phg_ret_open_ws	*retd = &ret->data.open_ws;

    /* Open a type B (no server SS) WS when the monitor is active. */

    args->css_srvr_type = css_srvr->type;
    /* Pass the command to the monitor. */
    phg_cpc_class_SPECIAL( cph, cp_args, ret );
    if ( ret->err )
	goto abort;
    else
	startup = 1;

    /* Build things and initialize fields needed locally. */
    if ( !(wst = phg_wst_create( cph->erh, retd->wstype, (caddr_t *)NULL )) )
	goto abort;
    wst->bound_status = WST_BOUND;

    if ( !(ws = phg_wsx_create( cph, args, css_srvr )) )
	goto abort;

    retd->wstype = ws->type = wst;
    ws->category = ws->type->desc_tbl.phigs_dt.ws_category;
    ws->current_colour_model =
	ws->type->desc_tbl.phigs_dt.out_dt.default_colour_model;
    ws->drawable_id = retd->drawable_id;
    ws->input_overlay_window = retd->overlay_id;

    /* There are virtually no workstation operations performed in
     * the client.
     */
    ws->close = phg_cpbc_close_ws;

    return ws;

abort:
    if ( ws )
	phg_wsx_destroy( ws );

    else if ( wst ) {
	wst->bound_status = WST_UNBOUND;
	phg_wst_destroy( wst );
    }

    if ( startup ) {
	/* Shut it down in the monitor. */
	Phg_args	dcp_args;

	dcp_args.op = (unsigned)CP_FUNC_OP_CLOSE_WS;
	dcp_args.data.idata = args->wsid;
	phg_cpc_class_SPECIAL( cph, &dcp_args, (Phg_ret *)NULL );
    }
    ret->err = -1;
    return (Ws *)NULL;
}


int
phg_cpbc_struct_exists( cph, css_srvr, sid )
    Cp_handle		cph;
    Cpx_css_srvr	*css_srvr;
    Pint		sid;
{
    Phg_args		cp_args;
    Phg_ret		ret;
    Pstruct_status	status;

    ret.err = 0;
    cp_args.data.idata = sid;
    CP_FUNC( cph, CP_FUNC_OP_INQ_STRUCT_STATUS, &cp_args, &ret );
    status = ret.err == 0 ?
	(Pstruct_status)ret.data.idata : PSTRUCT_STATUS_NON_EXISTENT;
    return (status == PSTRUCT_STATUS_NON_EXISTENT ? 0 : 1);
}


void
phg_cpbc_ar_archive( cph, arh, args, css_srvr )
    Cp_handle		cph;
    Ar_handle		arh;
    Phg_args_ar_info	*args;
    Cpx_css_srvr	*css_srvr;
{
    Phg_args		cp_args;
    Phg_ret		ret;

    register int	i, num_els = 0;
    register unsigned	struct_length = 0;
    register caddr_t	buffer = NULL;
    register unsigned	buf_size = 0;

    cp_args.op = (unsigned)CP_FUNC_OP_INQ_EL_CONTENT;
    for ( i = 0; i < args->data.num_ints; i++ ) { /* for all structs */
	cp_args.data.q_el_data.struct_id = args->data.ints[i];
	cp_args.data.q_el_data.el_id = 1;
	struct_length = 0;
	num_els = 0;
	ret.err = 0;
	do {	/* get element info and buffer it */
	    (*css_srvr->inq_el_content)( cph, &cp_args, &ret, css_srvr );
	    if ( ret.err == 0 ) {
		/* Get buffer space if buffer too small. */
		if ( buf_size <
			(struct_length + ret.data.el_info.pex_oc.size) ) {
		    if ( !buffer )	/* first element of first struct */
			buffer = malloc( buf_size = 
			    ret.data.el_info.pex_oc.size );
		    else
			buffer = realloc( buffer, buf_size =
			    struct_length + ret.data.el_info.pex_oc.size );
		    if ( !buffer ) {
			ERR_BUF( cph->erh, ERR900 );
			return;
		    }
		}
		/* Concatenate the element data to the current list. */
		bcopy( (char *)ret.data.el_info.pex_oc.oc,
		    &buffer[struct_length],
		    (int)ret.data.el_info.pex_oc.size );
		struct_length += ret.data.el_info.pex_oc.size;
		++num_els;
		++cp_args.data.q_el_data.el_id;
	    }
	} while ( ret.err == 0 );

	/* Write the entire structure to the archive. */
	if ( phg_ar_write_struct_to_archive( arh, args->data.ints[i], 
			args->resflag, (Pint)struct_length, (Pint)num_els, buffer ) ) {
	    ERR_BUF(cph->erh, ERR406);
	    if ( buffer )
				free( (char *)buffer );
	    return;
	}
    }

    if ( buffer )
	free( (char *)buffer );
}
