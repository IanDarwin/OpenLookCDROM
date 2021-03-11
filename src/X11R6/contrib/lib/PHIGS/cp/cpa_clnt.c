/* $XConsortium: cpa_clnt.c,v 5.5 94/04/17 20:41:14 mor Exp $ */

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

#define NEED_EVENTS

#include <sys/types.h>
#include <sys/stat.h>
#include <X11/Xlibint.h>
#include "phg.h"
#include "cp.h"
#include "cp_priv.h"
#include "ar.h"
#include "PEX.h"
#include "PEXmacs.h"
#include "PEXfuncs.h"
#include "PEXproto.h"
#include "phigspex.h"


static void
cpa_load_funcs( cph, css_srvr )
    Cp_handle		cph;
    Cpx_css_srvr	*css_srvr;
{
    css_srvr->open_ws = phg_cpa_open_ws;
    css_srvr->close_ws = phg_cpa_close_ws;
    css_srvr->ws_update = phg_cpa_ws_update;
    css_srvr->ws_redraw_all = phg_cpa_ws_redraw_all;
    css_srvr->set_disp_state = phg_cpa_set_disp_state;
    css_srvr->set_rep = phg_cpa_set_rep;
    css_srvr->set_filter = phg_cpa_set_filter;
    css_srvr->set_colour_model = (void(*)())NULL;
    css_srvr->set_hlhsr_mode = phg_cpa_set_hlhsr_mode;
    css_srvr->set_view_input_prio = phg_cpa_set_view_input_priority;
    css_srvr->set_ws_win = phg_cpa_set_ws_win;
    css_srvr->set_ws_vp = phg_cpa_set_ws_vp;
    css_srvr->add_el = phg_cpa_add_el;
    css_srvr->copy_all_els = phg_cpa_copy_all_els;
    css_srvr->open_struct = phg_cpa_open_struct;
    css_srvr->close_struct = phg_cpa_close_struct;
    css_srvr->set_el_ptr = phg_cpa_set_el_ptr;
    css_srvr->set_edit_mode = phg_cpa_set_edit_mode;
    css_srvr->delete_el = phg_cpa_delete_el;
    css_srvr->delete_struct = phg_cpa_delete_struct;
    css_srvr->delete_struct_net = phg_cpa_delete_struct_net;
    css_srvr->delete_all_structs = phg_cpa_delete_all_structs;
    css_srvr->change_struct_id = phg_cpa_change_struct_id;
    css_srvr->change_struct_refs = phg_cpa_change_struct_refs;
    css_srvr->change_struct_idrefs = phg_cpa_change_struct_idrefs;
    css_srvr->post_struct = phg_cpa_post_struct;
    css_srvr->unpost_struct = phg_cpa_unpost_struct;
    css_srvr->unpost_all = phg_cpa_unpost_all;
    css_srvr->ar_archive = phg_cpa_ar_archive;
    css_srvr->set_err_hand_mode = (void(*)())NULL;
    css_srvr->el_search = phg_cpa_el_search;
    css_srvr->inc_spa_search = phg_cpa_inc_spa_search;
    css_srvr->inq_colour_model = (void(*)())NULL;
    css_srvr->inq_struct_status = phg_cpa_inq_struct_status;
    css_srvr->inq_struct_ids = phg_cpa_inq_struct_ids;
    css_srvr->inq_el_ptr = phg_cpa_inq_el_ptr;
    css_srvr->inq_el_type_size = phg_cpa_inq_el_type_size;
    css_srvr->inq_el_content = phg_cpa_inq_el_content;
    css_srvr->inq_indices = phg_cpa_inq_indices;
    css_srvr->inq_filter = phg_cpa_inq_filter;
    css_srvr->inq_posted = phg_cpa_inq_posted;
    css_srvr->inq_wss_posted_to = phg_cpa_inq_wss_posted_to;
    css_srvr->inq_hierarchy = phg_cpa_inq_hierarchy;
    css_srvr->inq_representation = phg_cpa_inq_rep;
    css_srvr->inq_view_rep = phg_cpa_inq_view_rep;
    css_srvr->inq_hlhsr_mode = phg_cpa_inq_hlhsr_mode;
    css_srvr->inq_disp_update_state = phg_cpa_inq_disp_update_state;
    css_srvr->inq_ws_xform = phg_cpa_inq_ws_xform;
    css_srvr->inq_text_extent = phg_cpa_inq_text_extent;
    css_srvr->inq_win_info = phg_cpx_get_win_info;
    css_srvr->inq_dpy_and_drawable = phg_cpx_inq_dpy_and_drawable;
    css_srvr->close_phigs = phg_cpa_close;
    css_srvr->struct_exists = phg_cpa_check_struct_exists;
    css_srvr->destroy = phg_cpa_destroy;
    css_srvr->full_copy_to_type_a = phg_cpx_full_copy_a_to_a;
    css_srvr->full_copy_to_type_b = phg_cpx_full_copy_a_to_b;
    css_srvr->drawable_pick = phg_cpa_ws_drawable_pick;
    css_srvr->map_points = phg_cpa_ws_map_points;
    css_srvr->inq_colr_map_meth_st = phg_cpa_inq_colr_map_meth_st;

    if ( cph->flags.monitor_active ) {
	css_srvr->inp_init_dev = phg_cpc_class_C;
	css_srvr->inp_set_mode = phg_cpc_class_B;
	css_srvr->inp_request = phg_cpc_inp_request;
	css_srvr->inp_sample = phg_cpc_class_E;
	css_srvr->inq_inp_dev_state = phg_cpc_class_E;
	css_srvr->message = phg_cpc_class_C;
	css_srvr->redraw_regions = phg_cpc_class_C;
    } else {
	css_srvr->redraw_regions = phg_cpa_ws_redraw_regions;
	css_srvr->inp_init_dev = (void(*)())NULL;
	css_srvr->inp_set_mode = (void(*)())NULL;
	css_srvr->inp_request = (void(*)())NULL;
	css_srvr->inp_sample = (void(*)())NULL;
	css_srvr->inq_inp_dev_state = (void(*)())NULL;
	css_srvr->message = (void(*)())NULL;
    }
}


Cpx_css_srvr*
phg_cpa_init( cph, display, pex_info )
    Cp_handle		cph;
    Display		*display;
    Phg_pex_ext_info	*pex_info;
{
    Cpx_css_srvr	*css_srvr;

    if ( !(css_srvr = (Cpx_css_srvr *)calloc(1,sizeof(Cpx_css_srvr)))) {
	ERR_BUF( cph->erh, ERR900 );

    } else if ( !(PHG_SCRATCH_SPACE( &css_srvr->model.a.scratch, 8096 ) ) ) {
	free( (char *)css_srvr );
	css_srvr = (Cpx_css_srvr *)NULL;
	ERR_BUF( cph->erh, ERR900 );

    } else {
	css_srvr->display = display;
	css_srvr->model.a.dpy_resource_base = display->resource_base;
	css_srvr->model.a.dpy_resource_shift = display->resource_shift;
	css_srvr->pex_info = *pex_info;
	css_srvr->type = CPX_SRVR_SS;
	cpa_load_funcs( cph, css_srvr );
    }

    return css_srvr;
}


void
phg_cpa_destroy( cph, css_srvr )
    Cp_handle		cph;
    Cpx_css_srvr	*css_srvr;
{
    /* Unlink and free any structures in the CSS. */
    phg_cpa_destroy_attached_structures( cph, css_srvr );
    phg_cpa_free_structure_lists( cph, css_srvr );

    /* Free any Incremental Spatial Search resources. */
    if ( css_srvr->model.a.iss_resources.search_context )
	PEXFreeSearchContext( css_srvr->display, 
	    css_srvr->model.a.iss_resources.search_context );
    if ( css_srvr->model.a.iss_resources.nrm_incl_filter )
	PEXFreeNameSet( css_srvr->display,
	    css_srvr->model.a.iss_resources.nrm_incl_filter );
    if ( css_srvr->model.a.iss_resources.nrm_excl_filter )
	PEXFreeNameSet( css_srvr->display,
	    css_srvr->model.a.iss_resources.nrm_excl_filter );
    if ( css_srvr->model.a.iss_resources.inv_incl_filter )
	PEXFreeNameSet( css_srvr->display,
	    css_srvr->model.a.iss_resources.inv_incl_filter );
    if ( css_srvr->model.a.iss_resources.inv_excl_filter )
	PEXFreeNameSet( css_srvr->display,
	    css_srvr->model.a.iss_resources.inv_excl_filter );

    /* Free the scratch space. */
    if ( css_srvr->model.a.scratch.size >  0 ) {
	free( (char *)css_srvr->model.a.scratch.buf );
	css_srvr->model.a.scratch.buf = NULL;
	css_srvr->model.a.scratch.size = 0;
    }

    free( (char *)css_srvr );
}


void
phg_cpa_close( cph, css_srvr )
    Cp_handle         cph;
    Cpx_css_srvr     *css_srvr;
{
    /* Ensure any pending requests are processed. */
    XFlush( css_srvr->display );
}


void
phg_cpa_ar_archive(cph, arh, args, css_srvr)
Cp_handle	     cph;
Ar_handle	     arh;
Phg_args_ar_info    *args;
Cpx_css_srvr	    *css_srvr;
{
    register int     i;
    CARD32	     num_elements, struct_length;
    pexElementInfo  *ptr;
    caddr_t	     buffer;
    int		     j;
    Cpa_struct_data *stp;

    for (i = 0; i < args->data.num_ints; i++) {
	if (!(stp = phg_cpa_struct_exists(cph, css_srvr, CPX_BY_SID, 
		args->data.ints[i], CPA_STRUCT_OP_CHECK))) {
	    ERR_BUF(cph->erh, ERR200);
	    return;

	} else {
	    if (!PEXGetStructureInfo(css_srvr->display, stp->xid, (CARD16*)NULL,
		    (CARD32*)NULL, (CARD32*)&num_elements, &struct_length,
		    (CARD16*)NULL) ) {
		return;	
		
	    } else if ( num_elements > 0 
		    && !PEXFetchElements(css_srvr->display, stp->xid, 
		    PEXBeginning, (INT32)1, PEXBeginning,
		    (INT32)num_elements, &buffer, &num_elements ) ) {
		free(buffer);	    
		return;
	    }
	    
	    /** Need to traverse these elements looking for an execute
	     ** structure.  If it's there, convert executed XID into
	     ** a PHIGS structure ID **/
	    ptr = (pexElementInfo *)buffer;
	    for (j = 0; j < num_elements; j++) {
		if (ptr->elementType == PEXOCExecuteStructure) {
		    pexExecuteStructure	*ex = (pexExecuteStructure *)ptr;
		    Cpa_struct_data *ex_stp;
		    ex_stp = phg_cpa_struct_exists( cph, css_srvr, CPX_BY_XID,
			(int)ex->id, CPA_STRUCT_OP_CHECK );
		    ex->id = ex_stp->sid;
		}
		ptr += ptr->length * sizeof(CARD32) / sizeof(*ptr);
	    }
	    
	    if (phg_ar_write_struct_to_archive(arh, stp->sid, 
					args->resflag, (Pint)struct_length * sizeof(CARD32), 
					(Pint)num_elements, buffer)) {
				ERR_BUF(cph->erh, ERR406);
				free(buffer);
				return;
	    }
	}
    }
}
