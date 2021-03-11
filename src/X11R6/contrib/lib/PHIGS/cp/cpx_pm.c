/* $XConsortium: cpx_pm.c,v 5.3 94/04/17 20:41:27 mor Exp $ */

/***********************************************************

Copyright (c) 1989,1990, 1991  X Consortium

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

Copyright (c) 1989,1990, 1991 by Sun Microsystems, Inc.

                        All Rights Reserved

Permission to use, copy, modify, and distribute this software and its 
documentation for any purpose and without fee is hereby granted, 
provided that the above copyright notice appear in all copies and that
both that copyright notice and this permission notice appear in 
supporting documentation, and that the names of Sun Microsystems,
and the X Consortium, not be used in advertising or publicity 
pertaining to distribution of the software without specific, written 
prior permission.  

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

static void
cpm_close( cph, args, ret)
    register Cp_handle		cph;
    register Phg_args		*args;
    register Phg_ret		*ret;
{
    /* Do nothing but return, the parent will kill this process */
}

static void
no_op()
{
    fprintf(stderr,"cp_monitor_func: Function not used\n");
}

void
phg_cpm_load_monitor_funcs( cph )
    Cp_handle	cph;
{
    register Cp_func	*f = cph->funcs;

    /* Control functions. */
    f[(int)CP_FUNC_OP_SET_ERR_HAND_MODE]	= phg_cpx_set_err_hand_mode;
    f[(int)CP_FUNC_OP_ERROR_SYNC]		= phg_cpx_sync_servers;
    f[(int)CP_FUNC_OP_EMERG_CLOSE]		= no_op;
    f[(int)CP_FUNC_OP_CLOSE_PHIGS]		= cpm_close;

    /* Workstation independent input functions. */
    f[(int)CP_FUNC_OP_INP_AWAIT]	= phg_cp_inp_await;
    f[(int)CP_FUNC_OP_FLUSH_DEV]	= phg_cp_inp_flush_dev;
    f[(int)CP_FUNC_OP_INQ_INP_OVERFLOW]	= phg_cp_inq_inp_overflow;
    f[(int)CP_FUNC_OP_INQ_MORE_EVENTS]		= phg_cp_inq_more_events;

    /* Workstation functions. */
    f[(int)CP_FUNC_OP_OPEN_WS]		= phg_cpm_open_ws;
    f[(int)CP_FUNC_OP_CLOSE_WS]		= phg_cpm_close_ws;
    f[(int)CP_FUNC_OP_WS_REDRAW_ALL]	= phg_cpx_ws_redraw_all;
    f[(int)CP_FUNC_OP_WS_UPDATE]	= phg_cpx_ws_update;
    f[(int)CP_FUNC_OP_SET_DISP_STATE]	= phg_cpx_ws_set_disp_state;
    f[(int)CP_FUNC_OP_MESSAGE]		= phg_cpx_message;
    f[(int)CP_FUNC_OP_SET_REP]		= phg_cpx_set_rep;
    f[(int)CP_FUNC_OP_SET_FILTER]	= phg_cpx_set_filter;
    f[(int)CP_FUNC_OP_SET_COLOUR_MODEL]	= phg_cpx_set_colour_model;
    f[(int)CP_FUNC_OP_SET_HLHSR_MODE]	= phg_cpx_set_hlhsr_mode;
    f[(int)CP_FUNC_OP_SET_VIEW_INPUT_PRIO] = phg_cpx_set_view_input_priority;
    f[(int)CP_FUNC_OP_SET_WS_WIN]	= phg_cpx_set_ws_win;
    f[(int)CP_FUNC_OP_SET_WS_VP]	= phg_cpx_set_ws_vp;
    f[(int)CP_FUNC_OP_POST_STRUCT]	= phg_cpx_post_struct;
    f[(int)CP_FUNC_OP_UNPOST_STRUCT]	= phg_cpx_unpost_struct;
    f[(int)CP_FUNC_OP_UNPOST_ALL]	= phg_cpx_unpost_all;
    f[(int)CP_FUNC_OP_INP_INIT_DEV]	= phg_cp_inp_init_dev;
    f[(int)CP_FUNC_OP_INP_SET_MODE]	= phg_cp_inp_set_mode;
    f[(int)CP_FUNC_OP_INP_REQUEST]	= phg_cp_inp_request;
    f[(int)CP_FUNC_OP_INP_SAMPLE]	= phg_cp_inp_sample;
    f[(int)CP_FUNC_OP_INQ_INDICES]		= phg_cpx_inq_indices;
    f[(int)CP_FUNC_OP_INQ_FILTER]		= phg_cpx_inq_filter;
    f[(int)CP_FUNC_OP_INQ_POSTED]		= phg_cpx_inq_posted;
    f[(int)CP_FUNC_OP_INQ_INP_DEV_STATE]	= phg_cp_inq_inp_dev_state;
    f[(int)CP_FUNC_OP_INQ_REPRESENTATION]	= phg_cpx_inq_rep;
    f[(int)CP_FUNC_OP_INQ_VIEW_REP]		= phg_cpx_inq_view_rep;
    f[(int)CP_FUNC_OP_INQ_HLHSR_MODE]		= phg_cpx_inq_hlhsr_mode;
    f[(int)CP_FUNC_OP_INQ_DISP_UPDATE_STATE]	= phg_cpx_inq_disp_update_state;
    f[(int)CP_FUNC_OP_INQ_COLOUR_MODEL]	= phg_cpx_inq_colour_model;
    f[(int)CP_FUNC_OP_INQ_WS_XFORM]		= phg_cpx_inq_ws_xform;
    f[(int)CP_FUNC_OP_DRAWABLE_PICK]		= phg_cpx_ws_drawable_pick;
    f[(int)CP_FUNC_OP_MAP_POINTS]		= phg_cpx_ws_map_points;
    f[(int)CP_FUNC_OP_REDRAW_REGIONS]	= phg_cpx_ws_redraw_regions;
    f[(int)CP_FUNC_OP_WS_SYNCH]		= phg_cpx_ws_synch;
    f[(int)CP_FUNC_OP_INQ_COLR_MAP_METH_ST]	= phg_cpx_inq_colr_map_meth_st;

    /* CSS functions. */
    f[(int)CP_FUNC_OP_ADD_EL]		= phg_cpm_add_el;
    f[(int)CP_FUNC_OP_COPY_ALL_ELS]	= phg_cpm_copy_all_els;
    f[(int)CP_FUNC_OP_OPEN_STRUCT]	= phg_cpm_open_struct;
    f[(int)CP_FUNC_OP_CLOSE_STRUCT]	= phg_cpm_close_struct;
    f[(int)CP_FUNC_OP_SET_EL_PTR]	= phg_cpm_set_el_ptr;
    f[(int)CP_FUNC_OP_SET_EDIT_MODE]	= phg_cpm_set_edit_mode;
    f[(int)CP_FUNC_OP_DELETE_EL]	= phg_cpm_delete_el;
    f[(int)CP_FUNC_OP_DELETE_STRUCT]	= phg_cpm_delete_struct;
    f[(int)CP_FUNC_OP_DELETE_STRUCT_NET] = phg_cpm_delete_struct_net;
    f[(int)CP_FUNC_OP_DELETE_ALL_STRUCTS] = phg_cpm_delete_all_structs;
    f[(int)CP_FUNC_OP_CHANGE_STRUCT_ID]	= phg_cpm_change_struct_id;
    f[(int)CP_FUNC_OP_CHANGE_STRUCT_REFS] = phg_cpm_change_struct_refs;
    f[(int)CP_FUNC_OP_CHANGE_STRUCT_IDREFS] = phg_cpm_change_struct_idrefs;
    f[(int)CP_FUNC_OP_EL_SEARCH]		= phg_cpx_el_search;
    f[(int)CP_FUNC_OP_INC_SPA_SEARCH]   	= phg_cpx_inc_spa_search;
    f[(int)CP_FUNC_OP_INQ_STRUCT_STATUS ]	= phg_cpx_inq_struct_status;
    f[(int)CP_FUNC_OP_INQ_STRUCT_IDS ]		= phg_cpx_inq_struct_ids;
    f[(int)CP_FUNC_OP_INQ_EL_PTR ]		= phg_cpx_inq_el_ptr;
    f[(int)CP_FUNC_OP_INQ_EL_TYPE_SIZE ]	= phg_cpx_inq_el_type_size;
    f[(int)CP_FUNC_OP_INQ_EL_CONTENT ]		= phg_cpx_inq_el_content;
    f[(int)CP_FUNC_OP_INQ_WSS_POSTED_TO]	= phg_cpm_inq_wss_posted_to;
    f[(int)CP_FUNC_OP_INQ_HIERARCHY]		= phg_cpx_inq_hierarchy;
    f[(int)CP_FUNC_OP_INQ_TEXT_EXTENT]		= phg_cpx_inq_text_extent;
    f[(int)CP_FUNC_OP_INQ_WIN_INFO]		= phg_cpx_inq_win_info;

    /* Archive functions. */
    f[(int)CP_FUNC_OP_AR_OPEN]		= no_op;
    f[(int)CP_FUNC_OP_AR_CLOSE]		= no_op;
    f[(int)CP_FUNC_OP_AR_ARCHIVE]	= no_op;
    f[(int)CP_FUNC_OP_AR_RETRIEVE]	= no_op;
    f[(int)CP_FUNC_OP_AR_DELETE]	= no_op;
    f[(int)CP_FUNC_OP_AR_GET_NAMES]	= no_op;
    f[(int)CP_FUNC_OP_AR_GET_HIERARCHY]	= no_op;
    f[(int)CP_FUNC_OP_INQ_CONFLICTING]		= no_op;
}


static void
cpm_rebuild_wst( args )
    Phg_args_open_ws	*args;
{
    Wst		old_wst, *wst = args->type;

    bcopy( (char *)wst, (char *)&old_wst, sizeof(Wst) );
    wst->desc_tbl.xwin_dt.display_name = args->wst_display_name;
    wst->buffer = args->wst_buffer;
    phg_wst_copy_buf_pointers( &old_wst, wst );
}


static Cpx_css_srvr*
cpm_get_css_srvr( cph, args )
    Cp_handle		cph;
    Phg_args_open_ws	*args;
{
    Cpx_css_srvr	*css_srvr = (Cpx_css_srvr *)NULL;
    Phg_pex_ext_info	pex_info;
    Pint		err = 0;
    Display		*display = (Display *)NULL;

    /* The CP has already figured out what type of server we're dealing
     * with, so there's no need to do it here.
     */
    assure(args->css_srvr_type==CPX_CLNT_SS||args->css_srvr_type==CPX_SRVR_SS);
    if ( args->css_srvr_type == CPX_CLNT_SS ) {
	/* A type B css server always exists in the PM. */
	css_srvr = phg_cpx_css_srvr_exists( cph,
	    CPX_BY_SS_TYPE, (caddr_t)CPX_CLNT_SS );
	assure(css_srvr != (Cpx_css_srvr *)NULL)

    } else if ( !(css_srvr = phg_cpx_css_srvr_exists( cph,
	    CPX_BY_NAME, args->conn_info.display_name )) ) {
	/* Try to create one. */
	if ( !(display = phg_utx_open_pex_display(
		args->conn_info.display_name, &pex_info, &err )) ) {
	    ERR_BUF( cph->erh, err );
	    return (Cpx_css_srvr *)NULL;
	}
	if ( cph->flags.err_sync )
	    XSynchronize( display, 1 );

	if ( !(css_srvr = phg_cpa_pm_init( cph, display,
		args->dpy_resource_base, args->dpy_resource_shift,
		&pex_info )) ) {
	    XCloseDisplay( display );
	    return (Cpx_css_srvr *)NULL;
	}

	if ( !(css_srvr->model.a.top_level =
		phg_cpm_toolkit_add_connection( cph, display, &err )) ) {
	    if ( css_srvr->destroy )
		(*css_srvr->destroy )( cph, css_srvr );
	    XCloseDisplay( display );
	    return (Cpx_css_srvr *)NULL;
	}

	phg_cpx_link_css_srvr( cph, css_srvr );
	phg_cpx_instance_connection( cph, display, 1 );
    }

    return css_srvr;
}


void
phg_cpm_open_ws( cph, cp_args, ret )
    register Cp_handle          cph;
    Phg_args                    *cp_args;
    Phg_ret                     *ret;
{
    Cpx_css_srvr	*css_srvr;
    Ws			*ws;
    Phg_args_open_ws	*args = &cp_args->data.open_ws;

    ret->err = -1;
    cpm_rebuild_wst( args );
    if ( css_srvr = cpm_get_css_srvr( cph, args ) ) {
	if ( ws = (*css_srvr->open_ws)( cph, cp_args, ret, css_srvr ) ) {
	    CPX_ADD_TO_LIST(Ws, cph->ws_list, ws)
	    ret->err = 0;
	} else
	    phg_cpx_release_css_srvr( cph, css_srvr );
    }
}

void
phg_cpm_close_ws( cph, cp_args )
    register Cp_handle          cph;
    Phg_args                    *cp_args;
{
    Ws			*ws;
    Cpx_css_srvr	*css_srvr;

    if ( ws = phg_cpx_ws_exists( cph, CPX_BY_WSID, (Cpx_css_srvr *)NULL,
	    cp_args->data.idata )) {
	css_srvr = ws->css_srvr;
	phg_cpx_unlink_ws( cph, ws );
	if ( css_srvr->close_ws )
	    (*css_srvr->close_ws)( cph, cp_args, ws );
	phg_cpx_release_css_srvr( cph, css_srvr );
    }
}


void
phg_cpm_open_struct( cph, cp_args, ret )
    register Cp_handle		cph;
    Phg_args			*cp_args;
    Phg_ret			*ret;
{
    Cpx_css_srvr	*css_srvr;

    css_srvr = phg_cpx_css_srvr_exists( cph,
	CPX_BY_SS_TYPE, (caddr_t)CPX_CLNT_SS );
    (*css_srvr->open_struct)( cph, cp_args, ret, css_srvr );
}


void
phg_cpm_add_el( cph, cp_args )
    register Cp_handle		cph;
    Phg_args			*cp_args;
{
    Cpx_css_srvr	*css_srvr;

    css_srvr = phg_cpx_css_srvr_exists( cph,
	    CPX_BY_SS_TYPE, (caddr_t)CPX_CLNT_SS );
    (*css_srvr->add_el)( cph, cp_args, css_srvr );
}


void
phg_cpm_close_struct( cph, cp_args )
    Cp_handle		cph;
    Phg_args		*cp_args;
{
    Cpx_css_srvr	*css_srvr;

    css_srvr = phg_cpx_css_srvr_exists( cph,
	    CPX_BY_SS_TYPE, (caddr_t)CPX_CLNT_SS );
    (*css_srvr->close_struct)( cph, cp_args, css_srvr );
}


void
phg_cpm_delete_all_structs( cph, cp_args )
    Cp_handle		cph;
    Phg_args		*cp_args;
{
    Cpx_css_srvr	*css_srvr;

    css_srvr = phg_cpx_css_srvr_exists( cph,
	    CPX_BY_SS_TYPE, (caddr_t)CPX_CLNT_SS );
    (*css_srvr->delete_all_structs)( cph, cp_args, css_srvr );
}


void
phg_cpm_delete_struct( cph, cp_args )
    register Cp_handle		cph;
    Phg_args			*cp_args;
{
    Cpx_css_srvr	*css_srvr;

    css_srvr = phg_cpx_css_srvr_exists( cph,
	    CPX_BY_SS_TYPE, (caddr_t)CPX_CLNT_SS );
    (*css_srvr->delete_struct)( cph, cp_args, css_srvr );
}


void
phg_cpm_set_edit_mode( cph, cp_args )
    register Cp_handle		cph;
    Phg_args			*cp_args;
{
    Cpx_css_srvr	*css_srvr;

    css_srvr = phg_cpx_css_srvr_exists( cph,
	    CPX_BY_SS_TYPE, (caddr_t)CPX_CLNT_SS );
    (*css_srvr->set_edit_mode)( cph, cp_args, css_srvr );
}


void
phg_cpm_set_el_ptr( cph, cp_args )
    register Cp_handle		cph;
    Phg_args			*cp_args;
{
    Cpx_css_srvr	*css_srvr;

    css_srvr = phg_cpx_css_srvr_exists( cph,
	    CPX_BY_SS_TYPE, (caddr_t)CPX_CLNT_SS );
    (*css_srvr->set_el_ptr)( cph, cp_args, css_srvr );
}


void
phg_cpm_copy_all_els( cph, cp_args )
    register Cp_handle		cph;
    Phg_args			*cp_args;
{
    Cpx_css_srvr	*css_srvr;

    css_srvr = phg_cpx_css_srvr_exists( cph,
	    CPX_BY_SS_TYPE, (caddr_t)CPX_CLNT_SS );
    (*css_srvr->copy_all_els)( cph, cp_args, css_srvr );
}


void
phg_cpm_delete_el( cph, cp_args )
    register Cp_handle		cph;
    Phg_args			*cp_args;
{
    Cpx_css_srvr	*css_srvr;

    css_srvr = phg_cpx_css_srvr_exists( cph,
	    CPX_BY_SS_TYPE, (caddr_t)CPX_CLNT_SS );
    (*css_srvr->delete_el)( cph, cp_args, css_srvr );
}


void
phg_cpm_delete_struct_net( cph, cp_args )
    register Cp_handle		cph;
    Phg_args			*cp_args;
{
    Cpx_css_srvr	*css_srvr;

    css_srvr = phg_cpx_css_srvr_exists( cph,
	    CPX_BY_SS_TYPE, (caddr_t)CPX_CLNT_SS );
    (*css_srvr->delete_struct_net)( cph, cp_args, css_srvr );
}


void
phg_cpm_inq_wss_posted_to( cph, cp_args, ret )
    Cp_handle		cph;
    Phg_args		*cp_args;
    Phg_ret		*ret;
{
    Cpx_css_srvr	*css_srvr;

    css_srvr = phg_cpx_css_srvr_exists( cph,
	    CPX_BY_SS_TYPE, (caddr_t)CPX_CLNT_SS );
    (*css_srvr->inq_wss_posted_to)( cph, cp_args, ret, css_srvr );
}


void
phg_cpm_change_struct_id( cph, cp_args )
    Cp_handle		cph;
    Phg_args		*cp_args;
{
    Cpx_css_srvr	*css_srvr;

    css_srvr = phg_cpx_css_srvr_exists( cph,
	    CPX_BY_SS_TYPE, (caddr_t)CPX_CLNT_SS );
    (*css_srvr->change_struct_id)( cph, cp_args, css_srvr );
}


void
phg_cpm_change_struct_refs( cph, cp_args )
    Cp_handle		cph;
    Phg_args		*cp_args;
{
    Cpx_css_srvr	*css_srvr;

    css_srvr = phg_cpx_css_srvr_exists( cph,
	    CPX_BY_SS_TYPE, (caddr_t)CPX_CLNT_SS );
    (*css_srvr->change_struct_refs)( cph, cp_args, css_srvr );
}


void
phg_cpm_change_struct_idrefs( cph, cp_args )
    Cp_handle		cph;
    Phg_args		*cp_args;
{
    Cpx_css_srvr	*css_srvr;

    css_srvr = phg_cpx_css_srvr_exists( cph,
	    CPX_BY_SS_TYPE, (caddr_t)CPX_CLNT_SS );
    (*css_srvr->change_struct_idrefs)( cph, cp_args, css_srvr );
}

