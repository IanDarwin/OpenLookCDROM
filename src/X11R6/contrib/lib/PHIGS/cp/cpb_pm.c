/* $XConsortium: cpb_pm.c,v 5.3 94/04/17 20:41:19 rws Exp $ */

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
#include "ws.h"
#include "cp_priv.h"
#include "PEX.h"
#include "PEXmacs.h"
#include "PEXfuncs.h"
#include "PEXproto.h"
#include "phigspex.h"


static void
cpb_pm_load_funcs( cph, css_srvr )
    Cp_handle		cph;
    Cpx_css_srvr	*css_srvr;
{
    css_srvr->destroy = phg_cpb_pm_destroy;
    css_srvr->set_err_hand_mode = (void(*)())NULL;
    css_srvr->close_phigs = (void(*)())NULL;
    css_srvr->full_copy_to_type_a = (int(*)())NULL;
    css_srvr->full_copy_to_type_b = (int(*)())NULL;

    css_srvr->open_ws = phg_wsb_pm_open_ws;
    css_srvr->close_ws = phg_cpb_pm_close_ws;
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
    css_srvr->post_struct = phg_cpb_post_struct;
    css_srvr->unpost_struct = phg_cpb_unpost_struct;
    css_srvr->unpost_all = phg_cpb_unpost_all;
    css_srvr->inq_colour_model = (void(*)())NULL;
    css_srvr->inq_indices = phg_cpb_inq_indices;
    css_srvr->inq_filter = phg_cpb_inq_filter;
    css_srvr->inq_posted = phg_cpb_inq_posted;
    css_srvr->inq_representation = phg_cpb_inq_rep;
    css_srvr->inq_view_rep = phg_cpb_inq_view_rep;
    css_srvr->inq_hlhsr_mode = phg_cpb_inq_hlhsr_mode;
    css_srvr->inq_disp_update_state = phg_cpb_inq_disp_update_state;
    css_srvr->inq_ws_xform = phg_cpb_inq_ws_xform;
    css_srvr->inq_win_info = phg_cpx_get_win_info;
    css_srvr->inq_dpy_and_drawable = (void(*)())NULL;
    css_srvr->drawable_pick = phg_cpb_ws_drawable_pick;
    css_srvr->map_points = phg_cpb_ws_map_points;
    css_srvr->redraw_regions = phg_cpb_ws_redraw_regions;
    css_srvr->ws_synch = phg_cpb_ws_synch;
    css_srvr->inq_colr_map_meth_st = phg_cpb_inq_colr_map_meth_st;

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
    css_srvr->el_search = phg_cpb_el_search;
    css_srvr->inc_spa_search = phg_cpb_inc_spa_search;
    css_srvr->inq_struct_status = phg_cpb_inq_struct_status;
    css_srvr->inq_struct_ids = phg_cpb_inq_struct_ids;
    css_srvr->inq_el_ptr = phg_cpb_inq_el_ptr;
    css_srvr->inq_el_type_size = phg_cpb_inq_el_type_size;
    css_srvr->inq_el_content = phg_cpb_inq_el_content;
    css_srvr->inq_wss_posted_to = phg_cpb_inq_wss_posted_to;
    css_srvr->inq_hierarchy = phg_cpb_inq_hierarchy;
    css_srvr->inq_text_extent = phg_cpb_inq_text_extent;

    /* No archive functions in PM. */
    css_srvr->ar_archive = (void(*)())NULL;

    /* Not used in the PM. */
    css_srvr->struct_exists = (int(*)())NULL;

    /* WS input functions are called directly by the CP in the PM. */
    css_srvr->inp_init_dev = (void(*)())NULL;
    css_srvr->inp_set_mode = (void(*)())NULL;
    css_srvr->inp_request = (void(*)())NULL;
    css_srvr->inp_sample = (void(*)())NULL;
    css_srvr->inq_inp_dev_state = (void(*)())NULL;
}


Cpx_css_srvr*
phg_cpb_pm_init( cph )
    Cp_handle		cph;
{
    Cpx_css_srvr	*css_srvr;

    if ( !(css_srvr = phg_cpb_create( cph, 1 )) ) {
	ERR_BUF( cph->erh, ERR900 );

    } else {
	css_srvr->flags.master = 1;
	cpb_pm_load_funcs( cph, css_srvr );
    }

    return css_srvr;
}


void
phg_cpb_pm_destroy( cph, css_srvr )
    Cp_handle		cph;
    Cpx_css_srvr	*css_srvr;
{
    phg_cpb_destroy( css_srvr );
}


void
phg_cpb_pm_close_ws( cph, cp_args, ws )
    Cp_handle	cph;
    Phg_args	*cp_args;
    Ws		*ws;
{
    if ( ws->type->desc_tbl.phigs_dt.ws_category == PCAT_OUTIN ) {
	if ( SIN_Q_OVERFLOWED((Sin_event_queue*)ws->in_ws.input_queue) ) {
	    ERR_BUF( cph->erh, ERR256);
	}
	phg_sin_q_flush_ws( ws->in_ws.input_queue, ws->id );
    }
    phg_cpb_close_ws( cph, cp_args, ws );
}

