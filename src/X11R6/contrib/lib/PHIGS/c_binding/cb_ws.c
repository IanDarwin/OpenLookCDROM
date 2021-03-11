/* $XConsortium: cb_ws.c,v 5.7 94/04/17 20:40:58 mor Exp $ */

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

/* Workstation functions for the PHIGS C binding */

#include "phg.h"
#include "cp.h"
#include "cb_priv.h"
#include "ws_type.h"
#include "phg_dt.h"


static int
valid_connection_id( wst, connid )
    Wst 	*wst;	/* workstation type       */
    char 	*connid;	/* connection identifier  */
{
    int			status = 0;
    XWindowAttributes	wattr;

    switch( wst->base_type ) {
	case WST_BASE_TYPE_X_TOOL:
	    /* Nothing really to check.  The existence and PEX support of
	     * the server is checked in lower level code.
	     */
	    status = 1;
	    break;
	case WST_BASE_TYPE_X_DRAWABLE:
	    /* Ensure the display pointer and drawable id are non-zero and
	     * that the window exists.
	     */
	    if ( ((Pconnid_x_drawable *)connid)->display
		&& ((Pconnid_x_drawable *)connid)->drawable_id
		&& XGetWindowAttributes(
		   ((Pconnid_x_drawable *)connid)->display,
		   ((Pconnid_x_drawable *)connid)->drawable_id, &wattr ) )
		status = 1;
	    break;
    }
    return status;
}

void
popen_ws(ws_id, conn_id, ws_type)
    Pint	ws_id;		/* workstation identifier */
    Pconnid     conn_id;	/* connection identifier  */
    Pint	ws_type;	/* workstation type       */
{
    Wst				*wst = (Wst *)ws_type;
    Phg_args			cp_args;
    register Phg_args_open_ws	*args = &cp_args.data.open_ws;
    Phg_ret			ret;

    if (CB_ENTRY_CHECK( phg_cur_cph, ERR2, Pfn_open_ws)) {
	if ( phg_psl_inq_ws_open( phg_cur_cph->psl, ws_id)) {
	    ERR_REPORT( phg_cur_cph->erh, ERR53);

	} else if ( !phg_cb_wst_exists( wst)) {
	    ERR_REPORT( phg_cur_cph->erh, ERR52);

	} else if ( !valid_connection_id( wst, conn_id)) {
	    ERR_REPORT( phg_cur_cph->erh, ERR50);

	} else if ( !phg_psl_ws_free_slot( phg_cur_cph->psl)){
	    ERR_REPORT( phg_cur_cph->erh, ERR63);

	} else {
	    bzero( (char *)args, sizeof(*args) );
	    args->wsid = ws_id;
	    args->type = wst;
	    args->wst_display_name = wst->desc_tbl.xwin_dt.display_name;
	    args->wst_display_name_length =
		wst->desc_tbl.xwin_dt.display_name_length;
	    args->wst_buffer = wst->buffer;
	    args->wst_buffer_size = wst->buffer_size;
	    switch ( wst->base_type ) {
		case WST_BASE_TYPE_X_DRAWABLE:
		    args->conn_info.drawable_id =
			((Pconnid_x_drawable *)conn_id)->drawable_id;
		    args->conn_info.display =
			((Pconnid_x_drawable *)conn_id)->display;
		    args->conn_info.display_name =
			DisplayString(args->conn_info.display);
		    args->conn_info.display_name_length =
			strlen(args->conn_info.display_name) + 1;
		    break;
		case WST_BASE_TYPE_X_TOOL:
		    if ( conn_id ) {
			/* Use the specified connection. */
			args->conn_info.display_name = (char *)conn_id;
		    } else {
			args->conn_info.display_name = 
			    wst->desc_tbl.xwin_dt.display_name;
		    }
		    args->conn_info.display_name_length =
			strlen(args->conn_info.display_name) + 1;
		    break;
	    }

	    CP_FUNC( phg_cur_cph, CP_FUNC_OP_OPEN_WS, &cp_args, &ret);
	    if ( !ret.err) {
		if ( !phg_cp_add_wst( phg_cur_cph, ret.data.open_ws.wstype )) {
		    cp_args.data.idata = ws_id;
		    CP_FUNC( phg_cur_cph, CP_FUNC_OP_CLOSE_WS, &cp_args, NULL);
		    ERR_REPORT( phg_cur_cph->erh, ERR900);

		} else {
		    (void)phg_psl_add_ws( phg_cur_cph->psl, ws_id, conn_id,
			ret.data.open_ws.wstype);
		    PSL_WS_STATE( phg_cur_cph->psl) = PWS_ST_WSOP;
		}
	    }
	    ERR_FLUSH( phg_cur_cph->erh);
	}
    }
}

Psl_ws_info*
phg_cb_ws_open( cph, ws, fnid)
    Cp_handle	cph;
    Pint	ws;
    Pint	fnid;
{
    Psl_ws_info		*wsinfo = NULL;

    if (CB_ENTRY_CHECK( cph, ERR3, fnid)) {
        if ( PSL_WS_STATE( cph->psl) != PWS_ST_WSOP) {
	    ERR_REPORT( cph->erh, ERR3);

	} else if ( !(wsinfo = phg_psl_get_ws_info( cph->psl, ws))) {
	    ERR_REPORT( cph->erh, ERR54);
	}
    }
    return wsinfo;
}

void
pclose_ws( ws_id)
    Pint    ws_id;  /* workstation identifier       */
{
    Phg_args		cp_args;
    Psl_ws_info		*ws;

    if ( ws = phg_cb_ws_open( phg_cur_cph, ws_id, Pfn_close_ws)) {
	phg_cp_rmv_wst( phg_cur_cph, ws->wstype );
	cp_args.data.idata = ws_id;
	CP_FUNC( phg_cur_cph, CP_FUNC_OP_CLOSE_WS, &cp_args, NULL);
	/* psl_rem_ws() sets the ws state to WSCL if no more ws's open */
	phg_psl_rem_ws( phg_cur_cph->psl, ws_id);
    }
}

void
pmessage( ws, msg)
    Pint	ws;	/* workstation identifier	*/
    char	*msg;	/* message string	*/
{
    Phg_args		cp_args;
    Phg_args_message	*args = &cp_args.data.message;

    if ( phg_cb_ws_open( phg_cur_cph, ws, Pfn_message)) {
	args->wsid = ws;
	if ( args->msg = msg)
	    args->msg_length = strlen(msg) + 1;
	else
	    args->msg_length = 0;
	CP_FUNC( phg_cur_cph, CP_FUNC_OP_MESSAGE, &cp_args, NULL);
    }
}

void
ppost_struct( ws_id, struct_id, priority)
    Pint	ws_id;		/* workstation identifier	*/
    Pint	struct_id;	/* structure identifier	*/
    Pfloat	priority;	/* priority	*/
{
    Phg_args				cp_args;
    register Phg_args_post_struct	*args = &cp_args.data.post_struct;

    if ( phg_cb_ws_open( phg_cur_cph, ws_id, Pfn_post_struct) ) {
	args->wsid = ws_id;
	args->struct_id = struct_id;
	args->disp_pri = priority;
	CP_FUNC( phg_cur_cph, CP_FUNC_OP_POST_STRUCT, &cp_args, NULL);
    }
}

void
punpost_struct( ws_id, struct_id)
    Pint	ws_id;		/* workstation identifier	*/
    Pint	struct_id;	/* structure identifier	*/
{
    Phg_args				cp_args;
    register Phg_args_unpost_struct	*args = &cp_args.data.unpost_struct;

    if (CB_ENTRY_CHECK( phg_cur_cph, ERR3, Pfn_unpost_struct)) {
        if ( PSL_WS_STATE( phg_cur_cph->psl) != PWS_ST_WSOP) {
	    ERR_REPORT( phg_cur_cph->erh, ERR3);

	} else if ( !phg_psl_inq_ws_open( phg_cur_cph->psl, ws_id)) {
	    ERR_REPORT( phg_cur_cph->erh, ERR54);

	} else {
	    args->wsid = ws_id;
	    args->struct_id = struct_id;
	    CP_FUNC( phg_cur_cph, CP_FUNC_OP_UNPOST_STRUCT, &cp_args, NULL);
	}
    }
}

void
punpost_all_structs( ws_id)
    Pint	ws_id;	/* workstation identifier	*/
{
    Phg_args				cp_args;

    if (CB_ENTRY_CHECK( phg_cur_cph, ERR3, Pfn_unpost_all_structs)) {
        if ( PSL_WS_STATE( phg_cur_cph->psl) != PWS_ST_WSOP) {
	    ERR_REPORT( phg_cur_cph->erh, ERR3);

	} else if ( !phg_psl_inq_ws_open( phg_cur_cph->psl, ws_id)) {
	    ERR_REPORT( phg_cur_cph->erh, ERR54);

	} else {
	    cp_args.data.idata = ws_id;
	    CP_FUNC( phg_cur_cph, CP_FUNC_OP_UNPOST_ALL, &cp_args, NULL);
	}
    }
}

void
predraw_all_structs( ws, control_flag)
    Pint	ws;		/* workstation identifier	*/
    Pctrl_flag	control_flag;	/* controls the redraw of the structures */
{
    Phg_args			cp_args;
    Phg_args_ws_redraw_all	*args = &cp_args.data.ws_redraw_all;
    Psl_ws_info			*wsinfo;

    if ( wsinfo = phg_cb_ws_open( phg_cur_cph, ws, Pfn_redraw_all_structs)) {
	switch ( ((Wst*)wsinfo->wstype)->desc_tbl.phigs_dt.ws_category) {
	    case PCAT_OUTIN:
	    case PCAT_OUT:
	    case PCAT_MO:
		args->wsid = ws;
		args->flag = control_flag;
		CP_FUNC( phg_cur_cph, CP_FUNC_OP_WS_REDRAW_ALL, &cp_args,
		    NULL);
		break;

	    default:
		ERR_REPORT( phg_cur_cph->erh, ERR59);
		break;
	}
    }
}

void
pupd_ws( ws, regen_flag)
    Pint	ws;		/* workstation identifier	*/
    Pregen_flag	regen_flag;	/* when to do the regeneration	*/
{
    Phg_args			cp_args;
    Phg_args_ws_update		*args = &cp_args.data.ws_update;
    Psl_ws_info			*wsinfo;

    if ( wsinfo = phg_cb_ws_open( phg_cur_cph, ws, Pfn_upd_ws)) {
	switch ( ((Wst*)wsinfo->wstype)->desc_tbl.phigs_dt.ws_category) {
	    case PCAT_OUTIN:
	    case PCAT_OUT:
	    case PCAT_MO:
		args->wsid = ws;
		args->flag = regen_flag;
		CP_FUNC( phg_cur_cph, CP_FUNC_OP_WS_UPDATE, &cp_args, NULL);
		break;

	    default:
		ERR_REPORT( phg_cur_cph->erh, ERR59);
		break;
	}
    }
}

void
pset_disp_upd_st( ws, def_mode, mod_mode)
    Pint	ws;		/* workstation identifier	*/
    Pdefer_mode	def_mode;	/* deferral mode	*/
    Pmod_mode	mod_mode;	/* modification mode	*/
{
    Phg_args			cp_args;
    Phg_args_set_disp_state	*args = &cp_args.data.set_disp_state;
    Psl_ws_info			*wsinfo;

    if ( wsinfo = phg_cb_ws_open( phg_cur_cph, ws, Pfn_set_disp_upd_st)) {
	switch ( ((Wst*)wsinfo->wstype)->desc_tbl.phigs_dt.ws_category) {
	    case PCAT_OUTIN:
	    case PCAT_OUT:
	    case PCAT_MO:
		args->wsid = ws;
		args->mode = def_mode;
		args->mod_mode = mod_mode;
		CP_FUNC( phg_cur_cph, CP_FUNC_OP_SET_DISP_STATE, &cp_args,
		    NULL);
		break;

	    default:
		ERR_REPORT( phg_cur_cph->erh, ERR59);
		break;
	}
    }
}

static int
valid_clip_box( xy, f, b, lim)
    Pclip_ind		xy, f, b;
    register Plimit3	*lim;
{
    /* Don't check the limit unless the corresponding clip plane is on. */

    if ( xy == PIND_CLIP) {
	if (   !CB_IN_RANGE( PDT_NPC_XMIN, PDT_NPC_XMAX, lim->x_min)
	    || !CB_IN_RANGE( PDT_NPC_XMIN, PDT_NPC_XMAX, lim->x_max)
	    || !CB_IN_RANGE( PDT_NPC_YMIN, PDT_NPC_YMAX, lim->y_min)
	    || !CB_IN_RANGE( PDT_NPC_YMIN, PDT_NPC_YMAX, lim->y_max)) {
	    ERR_REPORT( phg_cur_cph->erh, ERR154);
	    return 0;

	} else if ( !( lim->x_min < lim->x_max  && lim->y_min < lim->y_max)) {
	    ERR_REPORT( phg_cur_cph->erh, ERR153);
	    return 0;
	}
    }

    if ( f == PIND_CLIP) {
	if ( !CB_IN_RANGE( PDT_NPC_ZMIN, PDT_NPC_ZMAX, lim->z_max)) {
	    ERR_REPORT( phg_cur_cph->erh, ERR154);
	    return 0;
	}
    }

    if ( b == PIND_CLIP) {
	if ( !CB_IN_RANGE( PDT_NPC_ZMIN, PDT_NPC_ZMAX, lim->z_min)) {
	    ERR_REPORT( phg_cur_cph->erh, ERR154);
	    return 0;

	} else if ( f == PIND_CLIP) {
	    if ( !( lim->z_min <= lim->z_max)) {
		ERR_REPORT( phg_cur_cph->erh, ERR153);
		return 0;
	    }
	}
    }
    return 1;
}

void
pset_view_rep3( ws, index, rep)
    Pint		ws;	/* workstation id	*/
    Pint		index;	/* view index	*/
    register Pview_rep3	*rep;	/* view representation	*/
{
    Phg_args			cp_args;
    register Phg_args_set_rep	*args = &cp_args.data.set_rep;
    Psl_ws_info			*wsinfo;
    Wst_phigs_dt		*dt;

    if ( wsinfo = phg_cb_ws_open( phg_cur_cph, ws, Pfn_set_view_rep3)) {
	dt = &((Wst*)wsinfo->wstype)->desc_tbl.phigs_dt;
	if ( dt->ws_category == PCAT_MI) {
	    ERR_REPORT( phg_cur_cph->erh, ERR57);

	} else if ( index < 1) {
	    ERR_REPORT( phg_cur_cph->erh, ERR115);

	} else if ( index >= dt->num_view_indices) {
	    ERR_REPORT( phg_cur_cph->erh, ERR150);

	} else if ( valid_clip_box( rep->xy_clip, rep->front_clip,
	    rep->back_clip, &rep->clip_limit)) {

	    args->wsid = ws;
	    args->type = PHG_ARGS_VIEWREP;
	    args->rep.index = index;
	    args->rep.bundl.viewrep = *rep;
	    CP_FUNC( phg_cur_cph, CP_FUNC_OP_SET_REP, &cp_args, NULL);
	}
    }
}

static int
valid_clip_rect( xy, lim)
    Pclip_ind		xy;
    register Plimit	*lim;
{
    if ( xy == PIND_CLIP) {
	if (   !CB_IN_RANGE( PDT_NPC_XMIN, PDT_NPC_XMAX, lim->x_min)
	    || !CB_IN_RANGE( PDT_NPC_XMIN, PDT_NPC_XMAX, lim->x_max)
	    || !CB_IN_RANGE( PDT_NPC_YMIN, PDT_NPC_YMAX, lim->y_min)
	    || !CB_IN_RANGE( PDT_NPC_YMIN, PDT_NPC_YMAX, lim->y_max)) {
	    ERR_REPORT( phg_cur_cph->erh, ERR154);
	    return 0;

	} else if ( !( lim->x_min < lim->x_max  && lim->y_min < lim->y_max)) {
	    ERR_REPORT( phg_cur_cph->erh, ERR153);
	    return 0;
	}
    }
    return 1;
}

static void
expand_transform( m2, m3)
    register Pmatrix	m2;
    register Pmatrix3	m3;
{
    m3[2][2] = 1.0;
    m3[0][2] = m3[1][2] = m3[2][0] = m3[2][1] = m3[2][3] = m3[3][2] = 0.0;
    m3[0][0] = m2[0][0]; m3[0][1] = m2[0][1]; m3[0][3] = m2[0][2];
    m3[1][0] = m2[1][0]; m3[1][1] = m2[1][1]; m3[1][3] = m2[1][2];
    m3[3][0] = m2[2][0]; m3[3][1] = m2[2][1]; m3[3][3] = m2[2][2];
}

void
pset_view_rep( ws, index, rep)
    Pint		ws;	/* workstation id	*/
    Pint		index;	/* view index	*/
    register Pview_rep	*rep;	/* view representation	*/
{
    Phg_args			cp_args;
    register Phg_args_set_rep	*args = &cp_args.data.set_rep;
    Psl_ws_info			*wsinfo;
    Wst_phigs_dt		*dt;

    if ( wsinfo = phg_cb_ws_open( phg_cur_cph, ws, Pfn_set_view_rep)) {
	dt = &((Wst*)wsinfo->wstype)->desc_tbl.phigs_dt;
	if ( dt->ws_category == PCAT_MI) {
	    ERR_REPORT( phg_cur_cph->erh, ERR57);

	} else if ( index < 1) {
	    ERR_REPORT( phg_cur_cph->erh, ERR115);

	} else if ( index >= dt->num_view_indices) {
	    ERR_REPORT( phg_cur_cph->erh, ERR150);

	} else if ( valid_clip_rect( rep->xy_clip, &rep->clip_limit)) {
	    register Pview_rep3		*r = &args->rep.bundl.viewrep;

	    /* Copy and expand the 2d rep to 3d. */
	    r->clip_limit.x_min = rep->clip_limit.x_min;
	    r->clip_limit.x_max = rep->clip_limit.x_max;
	    r->clip_limit.y_min = rep->clip_limit.y_min;
	    r->clip_limit.y_max = rep->clip_limit.y_max;
	    r->clip_limit.z_min = PDT_NPC_ZMIN;
	    r->clip_limit.z_max = PDT_NPC_ZMAX;
	    r->xy_clip = rep->xy_clip;
	    r->front_clip = PIND_CLIP;
	    r->back_clip = PIND_CLIP;
	    expand_transform( rep->ori_matrix, r->ori_matrix);
	    expand_transform( rep->map_matrix, r->map_matrix);

	    args->wsid = ws;
	    args->type = PHG_ARGS_VIEWREP;
	    args->rep.index = index;
	    CP_FUNC( phg_cur_cph, CP_FUNC_OP_SET_REP, &cp_args, NULL);
	}
    }
}

void
pset_view_tran_in_pri( ws, index, ref_index, priority)
    Pint	ws;		/* workstation id	*/
    Pint	index;		/* view index	*/
    Pint	ref_index;	/* reference view index	*/
    Prel_pri	priority;	/* relative priority	*/
{
    Phg_args			cp_args;
    register Phg_args_set_view_input_prio 
				*args = &cp_args.data.set_view_input_prio;
    Psl_ws_info			*wsinfo;
    Wst_phigs_dt		*dt;

    if ( wsinfo = phg_cb_ws_open( phg_cur_cph, ws, Pfn_set_view_tran_in_pri)) {
	dt = &((Wst*)wsinfo->wstype)->desc_tbl.phigs_dt;
	if ( dt->ws_category == PCAT_MI) {
	    ERR_REPORT( phg_cur_cph->erh, ERR57);

	} else if ( index < 0 || ref_index < 0) {
	    ERR_REPORT( phg_cur_cph->erh, ERR114);

	} else if ( index >= dt->num_view_indices) {
	    ERR_REPORT( phg_cur_cph->erh, ERR101);

	} else if ( ref_index >= dt->num_view_indices) {
	    ERR_REPORT( phg_cur_cph->erh, ERR101);

	} else if ( ref_index != index) {
	    args->wsid = ws;
	    args->idx = index;
	    args->ref_idx = ref_index;
	    args->priority = priority;
	    CP_FUNC( phg_cur_cph, CP_FUNC_OP_SET_VIEW_INPUT_PRIO, &cp_args,
	       NULL);
	}
    }
}

void
pset_ws_win3( ws, window)
    Pint		ws;		/* workstation id	*/
    register Plimit3	*window;	/* workstation window limits	*/
{
    Phg_args				cp_args;
    register Phg_args_set_ws_winvp	*args = &cp_args.data.set_ws_winvp;
    Psl_ws_info				*wsinfo;

    if ( wsinfo = phg_cb_ws_open( phg_cur_cph, ws, Pfn_set_ws_win3)) {
	if ( ((Wst*)wsinfo->wstype)->desc_tbl.phigs_dt.ws_category == PCAT_MI) {
	    ERR_REPORT( phg_cur_cph->erh, ERR57);

	} else if ( !CB_IN_RANGE( PDT_NPC_XMIN, PDT_NPC_XMAX, window->x_min)
	    || !CB_IN_RANGE( PDT_NPC_XMIN, PDT_NPC_XMAX, window->x_max)
	    || !CB_IN_RANGE( PDT_NPC_YMIN, PDT_NPC_YMAX, window->y_min)
	    || !CB_IN_RANGE( PDT_NPC_YMIN, PDT_NPC_YMAX, window->y_max)
	    || !CB_IN_RANGE( PDT_NPC_ZMIN, PDT_NPC_ZMAX, window->z_min)
	    || !CB_IN_RANGE( PDT_NPC_ZMIN, PDT_NPC_ZMAX, window->z_max) ) {
	    ERR_REPORT( phg_cur_cph->erh, ERR156);

	} else if ( !(window->x_min < window->x_max)
	    || !(window->y_min < window->y_max)
	    || !(window->z_min <= window->z_max) ) {
	    ERR_REPORT( phg_cur_cph->erh, ERR151);

	} else {
	    args->wsid = ws;
	    args->two_d = 0;
	    args->limits = *window;
	    CP_FUNC( phg_cur_cph, CP_FUNC_OP_SET_WS_WIN, &cp_args, NULL);
	}
    }
}

void
pset_ws_win( ws, window)
    Pint	ws;		/* workstation id	*/
    Plimit	*window;	/* workstation window limits	*/
{
    Phg_args				cp_args;
    register Phg_args_set_ws_winvp	*args = &cp_args.data.set_ws_winvp;
    Psl_ws_info				*wsinfo;

    if ( wsinfo = phg_cb_ws_open( phg_cur_cph, ws, Pfn_set_ws_win)) {
	if ( ((Wst*)wsinfo->wstype)->desc_tbl.phigs_dt.ws_category == PCAT_MI) {
	    ERR_REPORT( phg_cur_cph->erh, ERR57);

	} else if ( !CB_IN_RANGE( PDT_NPC_XMIN, PDT_NPC_XMAX, window->x_min)
	    || !CB_IN_RANGE( PDT_NPC_XMIN, PDT_NPC_XMAX, window->x_max)
	    || !CB_IN_RANGE( PDT_NPC_YMIN, PDT_NPC_YMAX, window->y_min)
	    || !CB_IN_RANGE( PDT_NPC_YMIN, PDT_NPC_YMAX, window->y_max)) {
	    ERR_REPORT( phg_cur_cph->erh, ERR156);

	} else if ( !(window->x_min < window->x_max)
	    || !(window->y_min < window->y_max)) {
	    ERR_REPORT( phg_cur_cph->erh, ERR151);

	} else {
	    args->wsid = ws;
	    args->two_d = ~0;
	    args->limits.x_min = window->x_min;
	    args->limits.x_max = window->x_max;
	    args->limits.y_min = window->y_min;
	    args->limits.y_max = window->y_max;
	    CP_FUNC( phg_cur_cph, CP_FUNC_OP_SET_WS_WIN, &cp_args, NULL);
	}
    }
}

void
pset_ws_vp3( ws, viewport)
    Pint	ws;		/* workstation id	*/
    Plimit3	*viewport;	/* workstation viewport limits	*/
{
    Phg_args				cp_args;
    register Phg_args_set_ws_winvp	*args = &cp_args.data.set_ws_winvp;
    Psl_ws_info				*wsinfo;
    Wst_phigs_dt			*dt;

    if ( wsinfo = phg_cb_ws_open( phg_cur_cph, ws, Pfn_set_ws_vp3)) {
	dt = &((Wst*)wsinfo->wstype)->desc_tbl.phigs_dt;
	phg_cb_update_DC_size( wsinfo );
	if ( dt->ws_category == PCAT_MI) {
	    ERR_REPORT( phg_cur_cph->erh, ERR57);

	} else if ( !phg_cur_cph->flags.ignore_DC_errors
	    && (!CB_IN_RANGE( 0.0, dt->dev_coords[0], viewport->x_min)
	    ||  !CB_IN_RANGE( 0.0, dt->dev_coords[0], viewport->x_max)
	    ||  !CB_IN_RANGE( 0.0, dt->dev_coords[1], viewport->y_min)
	    ||  !CB_IN_RANGE( 0.0, dt->dev_coords[1], viewport->y_max)
	    ||  !CB_IN_RANGE( 0.0, dt->dev_coords[2], viewport->z_min)
	    ||  !CB_IN_RANGE( 0.0, dt->dev_coords[2], viewport->z_max)) ) {
	    ERR_REPORT( phg_cur_cph->erh, ERR157);

	} else if ( !(viewport->x_min < viewport->x_max)
	    || !(viewport->y_min < viewport->y_max)
	    || !(viewport->z_min <= viewport->z_max) ) {
	    ERR_REPORT( phg_cur_cph->erh, ERR152);

	} else {
	    args->wsid = ws;
	    args->two_d = 0;
	    args->limits = *viewport;
	    CP_FUNC( phg_cur_cph, CP_FUNC_OP_SET_WS_VP, &cp_args, NULL);
	}
    }
}

void
pset_ws_vp( ws, viewport)
    Pint	ws;		/* workstation id	*/
    Plimit	*viewport;	/* workstation viewport limits	*/
{
    Phg_args				cp_args;
    register Phg_args_set_ws_winvp	*args = &cp_args.data.set_ws_winvp;
    Psl_ws_info				*wsinfo;
    Wst_phigs_dt		*dt;

    if ( wsinfo = phg_cb_ws_open( phg_cur_cph, ws, Pfn_set_ws_vp)) {
	dt = &((Wst*)wsinfo->wstype)->desc_tbl.phigs_dt;
	phg_cb_update_DC_size( wsinfo );
	if ( dt->ws_category == PCAT_MI) {
	    ERR_REPORT( phg_cur_cph->erh, ERR57);

	} else if ( !phg_cur_cph->flags.ignore_DC_errors
	    && (!CB_IN_RANGE( 0.0, dt->dev_coords[0], viewport->x_min)
	    || !CB_IN_RANGE( 0.0, dt->dev_coords[0], viewport->x_max)
	    || !CB_IN_RANGE( 0.0, dt->dev_coords[1], viewport->y_min)
	    || !CB_IN_RANGE( 0.0, dt->dev_coords[1], viewport->y_max)) ) {
	    ERR_REPORT( phg_cur_cph->erh, ERR157);

	} else if ( !(viewport->x_min < viewport->x_max)
	    || !(viewport->y_min < viewport->y_max)) {
	    ERR_REPORT( phg_cur_cph->erh, ERR152);

	} else {
	    args->wsid = ws;
	    args->two_d = ~0;
	    args->limits.x_min = viewport->x_min;
	    args->limits.x_max = viewport->x_max;
	    args->limits.y_min = viewport->y_min;
	    args->limits.y_max = viewport->y_max;
	    CP_FUNC( phg_cur_cph, CP_FUNC_OP_SET_WS_VP, &cp_args, NULL);
	}
    }
}


static 
void
set_up_filter(ws, filter, filter_type, function_code)
Pint	ws;			/* workstation identifier	*/
Pfilter *filter;                /* highl/invisib filter */
Phg_args_flt_type filter_type;	/* which filter type?	*/
int	 function_code;		/* Which Pfn_ code?	*/
{
    Phg_args				cp_args;
    Psl_ws_info				*wsinfo;
    register Phg_args_set_filter	*args = &cp_args.data.set_filter;

    if ( wsinfo = phg_cb_ws_open( phg_cur_cph, ws, function_code)) {
	switch ( ((Wst*)wsinfo->wstype)->desc_tbl.phigs_dt.ws_category) {
	    case PCAT_OUTIN:
	    case PCAT_OUT:
	    case PCAT_MO:
		args->wsid = ws;
		args->type = filter_type;
		args->inc_set = filter->incl_set;
		args->exc_set = filter->excl_set;
		CP_FUNC( phg_cur_cph, CP_FUNC_OP_SET_FILTER, &cp_args,
		    NULL);
		break;

	    default:
		ERR_REPORT( phg_cur_cph->erh, ERR59);
		break;
	}
    }
}


void
pset_highl_filter(ws, filter)
Pint	ws;	/* workstation identifier	*/
Pfilter *filter; /* highlighting filter          */
{
    set_up_filter(ws, filter, PHG_ARGS_FLT_HIGH, Pfn_set_highl_filter);
}

void
pset_invis_filter(ws, filter)
Pint	ws;	/* workstation identifier	*/
Pfilter *filter; /* highlighting filter          */
{
    set_up_filter(ws, filter, PHG_ARGS_FLT_INVIS, Pfn_set_invis_filter);
}



void
pset_hlhsr_mode( ws, mode)
    Pint	ws;	/* workstation id	*/
    Pint	mode;	/* HLHSR mode */
{
    Phg_args				cp_args;
    register Phg_args_set_hlhsr_mode	*args = &cp_args.data.set_hlhsr_mode;
    Psl_ws_info				*wsinfo;
    Wst_phigs_dt			*dt;
    int					i;

    if (wsinfo = phg_cb_ws_open(phg_cur_cph, ws, Pfn_set_hlhsr_mode)) {
	dt = &((Wst*)wsinfo->wstype)->desc_tbl.phigs_dt;
	if (!(dt->ws_category == PCAT_OUT ||
	      dt->ws_category == PCAT_OUTIN ||
	      dt->ws_category == PCAT_MO)) {
	    ERR_REPORT(phg_cur_cph->erh, ERR59);

	} 
	for (i = 0; i < dt->num_hlhsr_modes; i ++) {
	    if (mode == dt->hlhsr_modes[i]) {
		args->wsid = ws;
		args->mode = mode;
		CP_FUNC(phg_cur_cph, CP_FUNC_OP_SET_HLHSR_MODE, &cp_args,NULL);
		return;
	    }
	}
	/* if we get to here, the mode is not available on this workstation */
	ERR_REPORT(phg_cur_cph->erh, ERR111);
    }
}

void
pset_line_rep( ws, index, rep)
    Pint	ws;	/* workstation identifier	*/
    Pint	index;	/* polyline bundle index	*/
    Pline_bundle *rep;	/* polyline representation pointer	*/
{
    Phg_args			cp_args;
    register Phg_args_set_rep	*args = &cp_args.data.set_rep;
    register Wst_phigs_dt	*dt;
    
    if ( dt = phg_cb_check_set_rep( phg_cur_cph, Pfn_set_line_rep, ws, index,
	rep->colr_ind)) {

	if ( !phg_cb_int_in_list( rep->type,
	    dt->out_dt.num_linetypes, dt->out_dt.linetypes)) {
	    ERR_REPORT( phg_cur_cph->erh, ERR104);

	} else {
	    args->wsid = ws;
	    args->type = PHG_ARGS_LNREP;
	    args->rep.index = index;
	    args->rep.bundl.lnrep = *rep;
	    CP_FUNC( phg_cur_cph, CP_FUNC_OP_SET_REP, &cp_args, NULL);
	}
    }
}

void
pset_line_rep_plus( ws, index, rep)
    Pint	ws;	/* workstation identifier	*/
    Pint	index;	/* polyline bundle index	*/
    Pline_bundle_plus	*rep;	/* extended polyline representation pointer	*/
{
    Phg_args			cp_args;
    register Phg_args_set_rep	*args = &cp_args.data.set_rep;
    register Wst_phigs_dt	*dt;
    
    if ( dt = phg_cb_check_set_rep( phg_cur_cph, Pfn_set_line_rep_plus, ws,
	index, rep->colr.type == PINDIRECT ? rep->colr.val.ind : 1 ) ) {

	if ( !phg_cb_int_in_list( rep->type,
	    dt->out_dt.num_linetypes, dt->out_dt.linetypes)) {
	    ERR_REPORT( phg_cur_cph->erh, ERR104);

	} else if (!CB_COLOUR_MODEL_SUPPORTED(rep->colr.type)) {
	    ERR_REPORT( phg_cur_cph->erh, ERR110);

	} else if (!CB_LINE_SHADING_SUPPORTED(rep->shad_meth)) {
	    ERR_REPORT( phg_cur_cph->erh, ERR122);

	} else {
	    args->wsid = ws;
	    args->type = PHG_ARGS_EXTLNREP;
	    args->rep.index = index;
	    args->rep.bundl.extlnrep = *rep;
	    CP_FUNC( phg_cur_cph, CP_FUNC_OP_SET_REP, &cp_args, NULL);
	}
    }
}

/* SET POLYMARKER REPRESENTATION */
void
pset_marker_rep( ws, index, rep)
    Pint	ws;	/* workstation identifier	*/
    Pint	index;	/* polymarker bundle index	*/
    Pmarker_bundle	*rep;	/* polymarker representation pointer	*/
{
    Phg_args			cp_args;
    register Phg_args_set_rep	*args = &cp_args.data.set_rep;
    register Wst_phigs_dt	*dt;
    
    if ( dt = phg_cb_check_set_rep( phg_cur_cph, Pfn_set_marker_rep, ws, index,
	rep->colr_ind)) {

	if ( !phg_cb_int_in_list( rep->type,
	    dt->out_dt.num_marker_types, dt->out_dt.marker_types)) {
	    ERR_REPORT( phg_cur_cph->erh, ERR105);

	} else {
	    args->wsid = ws;
	    args->type = PHG_ARGS_MKREP;
	    args->rep.index = index;
	    args->rep.bundl.mkrep = *rep;
	    CP_FUNC( phg_cur_cph, CP_FUNC_OP_SET_REP, &cp_args, NULL);
	}
    }
}


/* SET EXTENDED POLYMARKER REPRESENTATION */
void
pset_marker_rep_plus( ws, index, rep)
    Pint	ws;	/* workstation identifier	*/
    Pint	index;	/* polymarker bundle index	*/
    Pmarker_bundle_plus	*rep;	/* polymarker representation pointer	*/
{
    Phg_args			cp_args;
    register Phg_args_set_rep	*args = &cp_args.data.set_rep;
    register Wst_phigs_dt	*dt;
    
    if ( dt = phg_cb_check_set_rep( phg_cur_cph, Pfn_set_marker_rep_plus, ws, index,
	rep->colr.type == PINDIRECT ? rep->colr.val.ind : 1)) {

	if ( !phg_cb_int_in_list( rep->type,
	    dt->out_dt.num_marker_types, dt->out_dt.marker_types)) {
	    ERR_REPORT( phg_cur_cph->erh, ERR105);
	} else if (!CB_COLOUR_MODEL_SUPPORTED(rep->colr.type)) {
	    ERR_REPORT(phg_cur_cph->erh, ERR110);
	} else {
	    args->wsid = ws;
	    args->type = PHG_ARGS_EXTMKREP;
	    args->rep.index = index;
	    args->rep.bundl.extmkrep = *rep;
	    CP_FUNC( phg_cur_cph, CP_FUNC_OP_SET_REP, &cp_args, NULL);
	}
    }
}


void
pset_text_rep( ws, index, rep)
    Pint	ws;	/* workstation identifier	*/
    Pint	index;	/* text bundle index	*/
    Ptext_bundle	*rep;	/* text representation pointer	*/
{
    int				phg_cb_valid_text_pair();
    Phg_args			cp_args;
    register Phg_args_set_rep	*args = &cp_args.data.set_rep;
    register Wst_phigs_dt	*dt;

    if ( dt = phg_cb_check_set_rep( phg_cur_cph, Pfn_set_text_rep, ws, index,
	rep->colr_ind)) {

	if ( !phg_cb_valid_text_pair( rep->font, rep->prec,
	    dt->out_dt.num_text_pairs[0], dt->out_dt.text_pairs[0])) {
	    ERR_REPORT( phg_cur_cph->erh, ERR106);

	} else {
	    args->wsid = ws;
	    args->type = PHG_ARGS_TXREP;
	    args->rep.index = index;
	    args->rep.bundl.txrep = *rep;
	    CP_FUNC( phg_cur_cph, CP_FUNC_OP_SET_REP, &cp_args, NULL);
	}
    }
}

void
pset_text_rep_plus( ws, index, rep)
    Pint	ws;	/* workstation identifier	*/
    Pint	index;	/* text bundle index	*/
    Ptext_bundle_plus	*rep;	/* text representation pointer	*/
{
    int				phg_cb_valid_text_pair();
    Phg_args			cp_args;
    register Phg_args_set_rep	*args = &cp_args.data.set_rep;
    register Wst_phigs_dt	*dt;

    if ( dt = phg_cb_check_set_rep( phg_cur_cph, Pfn_set_text_rep_plus, ws,
	index, rep->colr.type == PINDIRECT ? rep->colr.val.ind : 1 ) ) {

	if ( !phg_cb_valid_text_pair( rep->font, rep->prec,
	    dt->out_dt.num_text_pairs[0], dt->out_dt.text_pairs[0])) {
	    ERR_REPORT( phg_cur_cph->erh, ERR106);
	} else if (!CB_COLOUR_MODEL_SUPPORTED(rep->colr.type)) {
	    ERR_REPORT( phg_cur_cph->erh, ERR110);
	} else {
	    args->wsid = ws;
	    args->type = PHG_ARGS_EXTTXREP;
	    args->rep.index = index;
	    args->rep.bundl.exttxrep = *rep;
	    CP_FUNC( phg_cur_cph, CP_FUNC_OP_SET_REP, &cp_args, NULL);
	}
    }
}

/* SET EDGE REPRESENTATION */
void
pset_edge_rep( ws, index, rep)
    Pint	ws;	/* workstation identifier	*/
    Pint	index;	/* edge bundle index	*/
    Pedge_bundle	*rep;	/* edge representation pointer	*/
{
    Phg_args			cp_args;
    register Phg_args_set_rep	*args = &cp_args.data.set_rep;
    register Wst_phigs_dt	*dt;

    if ( dt = phg_cb_check_set_rep( phg_cur_cph, Pfn_set_edge_rep, ws, index,
	rep->colr_ind)) {

	if ( rep->flag == PEDGE_ON && !phg_cb_int_in_list( rep->type,
	    dt->out_dt.num_edge_types, dt->out_dt.edge_types)) {
	    ERR_REPORT( phg_cur_cph->erh, ERR107);

	} else {
	    args->wsid = ws;
	    args->type = PHG_ARGS_EDGEREP;
	    args->rep.index = index;
	    args->rep.bundl.edgerep = *rep;
	    CP_FUNC( phg_cur_cph, CP_FUNC_OP_SET_REP, &cp_args, NULL);
	}
    }
}


/* SET EXTENDED EDGE REPRESENTATION */
void
pset_edge_rep_plus( ws, index, rep)
    Pint		ws;	/* workstation identifier	*/
    Pint		index;	/* edge bundle index	*/
    Pedge_bundle_plus	*rep;	/* edge representation pointer	*/
{
    Phg_args			cp_args;
    register Phg_args_set_rep	*args = &cp_args.data.set_rep;
    register Wst_phigs_dt	*dt;

    if ( dt = phg_cb_check_set_rep( phg_cur_cph, Pfn_set_edge_rep_plus, ws,
	index, rep->colr.type == PINDIRECT ? rep->colr.val.ind : 1 ) ) {

	if ( rep->flag == PEDGE_ON && !phg_cb_int_in_list( rep->type,
	    dt->out_dt.num_edge_types, dt->out_dt.edge_types)) {
	    ERR_REPORT( phg_cur_cph->erh, ERR107);

	} else if (!CB_COLOUR_MODEL_SUPPORTED(rep->colr.type)) {
		    ERR_REPORT( phg_cur_cph->erh, ERR110);

	} else {
	    args->wsid = ws;
	    args->type = PHG_ARGS_EXTEDGEREP;
	    args->rep.index = index;
	    args->rep.bundl.extedgerep = *rep;
	    CP_FUNC( phg_cur_cph, CP_FUNC_OP_SET_REP, &cp_args, NULL);
	}
    }
}


/* SET INTERIOR REPRESENTATION */
void
pset_int_rep( ws, index, rep)
    Pint	ws;	/* workstation identifier	*/
    Pint	index;	/* interior bundle index	*/
    Pint_bundle	*rep;	/* interior representation pointer	*/
{
    Phg_args			cp_args;
    register Phg_args_set_rep	*args = &cp_args.data.set_rep;
    register Wst_phigs_dt	*dt;

    if ( dt = phg_cb_check_set_rep( phg_cur_cph, Pfn_set_int_rep, ws, index,
	rep->colr_ind)) {

	if ( !phg_cb_int_in_list( (Pint)rep->style,
					dt->out_dt.num_interior_styles,
					(Pint*)dt->out_dt.interior_styles)) {
	    ERR_REPORT( phg_cur_cph->erh, ERR108);

	} else if ( rep->style == PSTYLE_PAT && rep->style_ind < 1) {
	    ERR_REPORT( phg_cur_cph->erh, ERR112);

	} else {
	    args->wsid = ws;
	    args->type = PHG_ARGS_INTERREP;
	    args->rep.index = index;
	    args->rep.bundl.interrep = *rep;
	    CP_FUNC( phg_cur_cph, CP_FUNC_OP_SET_REP, &cp_args, NULL);
	}
    }
}


/* SET EXTENDED INTERIOR REPRESENTATION */
void
pset_int_rep_plus( wsid, index, rep )
    Pint		wsid;	/* workstation id */
    Pint		index;	/* interior index */
    Pint_bundle_plus	*rep;	/* representation */
{
    Phg_args			cp_args;
    register Phg_args_set_rep	*args = &cp_args.data.set_rep;
    register Wst_phigs_dt	*dt;

    if ( dt = phg_cb_check_set_rep( phg_cur_cph, Pfn_set_int_rep_plus, wsid,
	index, rep->colr.type == PINDIRECT ? rep->colr.val.ind : 1 ) ) {

	if ((rep->back_colr.type == PINDIRECT
		    ? rep->back_colr.val.ind : 1) < 0) {
	    ERR_REPORT( phg_cur_cph->erh, ERR113);

        } else if ((rep->refl_props.specular_colr.type == PINDIRECT
             ? rep->refl_props.specular_colr.val.ind : 1) < 0) {
             ERR_REPORT( phg_cur_cph->erh, ERR113);
 
        } else if ((rep->back_refl_props.specular_colr.type == PINDIRECT
             ? rep->back_refl_props.specular_colr.val.ind : 1) < 0) {
             ERR_REPORT( phg_cur_cph->erh, ERR113);

	} else if ( !phg_cb_int_in_list( (Pint)rep->style,
					dt->out_dt.num_interior_styles,
					(Pint*)dt->out_dt.interior_styles) ||
		    !phg_cb_int_in_list( (Pint)rep->back_style,
					dt->out_dt.num_interior_styles,
					(Pint*)dt->out_dt.interior_styles)) {
	    ERR_REPORT( phg_cur_cph->erh, ERR108);

	} else if (( rep->style == PSTYLE_PAT && rep->style_ind < 1) ||
		   ( rep->back_style == PSTYLE_PAT && rep->back_style_ind < 1)) {
	    ERR_REPORT( phg_cur_cph->erh, ERR112);

	} else if (!CB_COLOUR_MODEL_SUPPORTED(rep->colr.type) ||
		   !CB_COLOUR_MODEL_SUPPORTED(rep->back_colr.type) ||
		   !CB_COLOUR_MODEL_SUPPORTED(
			  rep->refl_props.specular_colr.type) ||
		   !CB_COLOUR_MODEL_SUPPORTED(
			  rep->back_refl_props.specular_colr.type)) {
	    
	    ERR_REPORT( phg_cur_cph->erh, ERR110);

	} else if (!CB_INT_SHADING_SUPPORTED(rep->shad_meth) ||
	           !CB_INT_SHADING_SUPPORTED(rep->back_shad_meth)) {
	    ERR_REPORT( phg_cur_cph->erh, ERR123);

	} else if (!CB_REFL_EQ_SUPPORTED(rep->refl_eqn) ||
	           !CB_REFL_EQ_SUPPORTED(rep->back_refl_eqn)) {
	    ERR_REPORT( phg_cur_cph->erh, ERR124);

	} else {
	    args->wsid = wsid;
	    args->type = PHG_ARGS_EXTINTERREP;
	    args->rep.index = index;
	    args->rep.bundl.extinterrep = *rep;
	    CP_FUNC( phg_cur_cph, CP_FUNC_OP_SET_REP, &cp_args, NULL);
	}
    }
}


static int
valid_pattern_colours( s, a)
    register int	s;
    register Pint	*a;
{
    while( s--) {
	if ( *a++ < 0)
	    return 0;
    }
    return 1;
}

/* SET PATTERN REPRESENTATION */
void
pset_pat_rep( ws, index, rep)
    Pint	ws;	/* workstation identifier	*/
    Pint	index;	/* pattern bundle index	*/
    Ppat_rep	*rep;	/* pattern representation pointer	*/
{
    Phg_args			cp_args;
    register Phg_args_set_rep	*args = &cp_args.data.set_rep;

    /* Pattern uses a different error code for an index less than one so we
     * have to fake out check_set_rep and check it ourselves.  Also need to
     * do this for the colour, that gets checked separately..
     */
    if ( phg_cb_check_set_rep( phg_cur_cph, Pfn_set_pat_rep, ws, 1, 1) ) {

	if ( index < 1) {
	    ERR_REPORT( phg_cur_cph->erh, ERR112);

	} else if ( rep->dims.size_x < 1 || rep->dims.size_y < 1) {
	    ERR_REPORT( phg_cur_cph->erh, ERR116);
	
	} else if ( !valid_pattern_colours( rep->dims.size_x * rep->dims.size_y,
		rep->colr_array)) {
	    ERR_REPORT( phg_cur_cph->erh, ERR113);

	} else {
	    args->wsid = ws;
	    args->type = PHG_ARGS_PTREP;
	    args->rep.index = index;
	    args->rep.bundl.ptrep = *rep;
	    CP_FUNC( phg_cur_cph, CP_FUNC_OP_SET_REP, &cp_args, NULL);
	}
    }
}


/* SET EXTENDED PATTERN REPRESENTATION */
void
pset_pat_rep_plus( ws, index, rep)
    Pint		ws;	/* workstation identifier	*/
    Pint		index;	/* pattern bundle index	*/
    Ppat_rep_plus	*rep;	/* pattern representation pointer	*/
{
    Phg_args			cp_args;
    register Phg_args_set_rep	*args = &cp_args.data.set_rep;

    /* Pattern uses a different error code for an index less than one so we
     * have to fake out check_set_rep and check it ourselves.  Also need to
     * do this for the colour, that gets checked separately..
     */
    if ( phg_cb_check_set_rep( phg_cur_cph, Pfn_set_pat_rep_plus, ws, 1, 1) ) {

	if ( index < 1) {
	    ERR_REPORT( phg_cur_cph->erh, ERR112);

	} else if (!CB_COLOUR_MODEL_SUPPORTED(rep->type)) {
	    ERR_REPORT( phg_cur_cph->erh, ERR110);

	} else if ( rep->dims.size_x < 1 || rep->dims.size_y < 1) {
	    ERR_REPORT( phg_cur_cph->erh, ERR116);
	
	} else if ( !phg_colours_valid( rep->dims.size_x * rep->dims.size_y,
		rep->type, rep->colr_array ) ) {
	    ERR_REPORT( phg_cur_cph->erh, ERR136);

	} else {
	    args->wsid = ws;
	    args->type = PHG_ARGS_EXTPTREP;
	    args->rep.index = index;
	    args->rep.bundl.extptrep = *rep;
	    CP_FUNC( phg_cur_cph, CP_FUNC_OP_SET_REP, &cp_args, NULL);
	}
    }
}



/* Inquiry Functions */

void
pinq_ws_st(ws_state)
    Pws_st	*ws_state;	/* OUT workstation state	*/
{
    if ( CB_ENTRY_CHECK( phg_cur_cph, 0, Pfn_INQUIRY))
	 *ws_state = PSL_WS_STATE( phg_cur_cph->psl);
    else
	 *ws_state = PWS_ST_WSCL;
}

void
pinq_ws_conn_type( ws, store, error_ind, conn_id, ws_type )
    Pint	ws;		/* workstation identifier	*/
    Pstore	store;		/* handle to Store object	*/
    Pint	*error_ind;	/* OUT error indicator		*/
    Pconnid     *conn_id;	/* OUT connection identifier	*/
    Pint	*ws_type;	/* OUT workstation type		*/
{
    Psl_ws_info		*ws_info;

    if ( !CB_ENTRY_CHECK( phg_cur_cph, 0, Pfn_INQUIRY) )
	*error_ind = ERR3;

    else if ( PSL_WS_STATE( phg_cur_cph->psl) != PWS_ST_WSOP)
	*error_ind = ERR3;

    else if ( !(ws_info = phg_psl_get_ws_info( phg_cur_cph->psl, ws)) )
	*error_ind = ERR54;

    else {
	*error_ind = 0;
	*ws_type = (Pint)ws_info->wstype;
	*conn_id = ws_info->connid;
    }
}

void
pinq_open_wss( length, start, error_ind, idlist, total_length)
    Pint	length;		/* length of application list	*/
    Pint	start;		/* starting position	*/
    Pint	*error_ind;	/* OUT error indicator	*/
    Pint_list	*idlist;	/* OUT list of ws ids	*/
    Pint	*total_length;	/* OUT length of list in PHIGS	*/
{
    Pint	cnt, wsids[MAX_NO_OPEN_WS];

    if ( !CB_ENTRY_CHECK( phg_cur_cph, 0, Pfn_INQUIRY)) {
	*error_ind = ERR2;

    } else {
	*error_ind = 0;
	cnt = phg_psl_inq_wsids( phg_cur_cph->psl, wsids);
	idlist->num_ints = 0;
	if ( (*total_length = cnt) > 0)
	    if (start < 0 || start >= cnt)
		*error_ind = ERR2201;
	    else if (length > 0) {
		idlist->num_ints = MIN( length, cnt - start);
		bcopy( (char*)&wsids[start], (char*)idlist->ints,
		    idlist->num_ints * sizeof(Pint));
	    } else if (length < 0)
		*error_ind = ERRN153;
    }
}


/* This function performs the inquiry for INQUIRE LIST OF xxx INDICES
 * and formats the results.
 * Error checking and cp_args set up must already be done.
 */
static void
cb_ws_inq_table_indices( cp_args, length, start,
			    error_ind, indices, total_length)
    Phg_args	*cp_args;	/* Pointer to cp_args set up by caller */
    Pint	 length;	/* length of application list	*/
    Pint	 start;		/* starting position	*/
    Pint	*error_ind;	/* OUT error indicator	*/
    Pint_list	*indices;	/* OUT list of bundle indices	*/
    Pint	*total_length;	/* OUT length of list in PHIGS	*/
{
    Phg_ret		ret;

    CP_FUNC( phg_cur_cph, CP_FUNC_OP_INQ_INDICES, cp_args, &ret);
    if ( ret.err) {
	*error_ind = ret.err;
    } else {
	*error_ind = 0;
	indices->num_ints = 0;
	if ( (*total_length = ret.data.int_list.num_ints) > 0) {
	    if (start < 0 || start >= ret.data.int_list.num_ints)
		*error_ind = ERR2201;
	    else if (length > 0) {
		indices->num_ints = MIN(length,
		    ret.data.int_list.num_ints - start);
		bcopy( (char*)&ret.data.int_list.ints[start],
			(char*)indices->ints,
			indices->num_ints * sizeof(Pint));
	    } else if (length < 0)
		*error_ind = ERRN153;
	}
    }
}


void
pinq_list_view_inds( ws, length, start, error_ind, indices, total_length)
    Pint	ws;		/* workstation identifier	*/
    Pint	length;		/* length of application list	*/
    Pint	start;		/* starting position	*/
    Pint	*error_ind;	/* OUT error indicator	*/
    Pint_list	*indices;	/* OUT list of view indices	*/
    Pint	*total_length;	/* OUT length of list in PHIGS	*/
{
    Phg_args		cp_args;
    Psl_ws_info		*wsinfo;

    if ( !CB_ENTRY_CHECK( phg_cur_cph, 0, Pfn_INQUIRY)) {
	*error_ind = ERR3;

    } else if ( PSL_WS_STATE( phg_cur_cph->psl) != PWS_ST_WSOP) {
	    *error_ind = ERR3;

    } else if ( !(wsinfo = phg_psl_get_ws_info( phg_cur_cph->psl, ws))) {
	*error_ind = ERR54;

    } else if ( ((Wst*)wsinfo->wstype)->desc_tbl.phigs_dt.ws_category == PCAT_MI) {
	*error_ind = ERR57;

    } else {
	    cp_args.data.q_indices.wsid = ws;
	    cp_args.data.q_indices.type = PHG_ARGS_VIEWREP;
	    cb_ws_inq_table_indices( &cp_args, length, start,
			    error_ind, indices, total_length);
    }
}

void
pinq_posted_structs(ws, length, start, error_ind, list, total_length)
Pint	ws;		/* workstation identifier	*/
Pint	length;		/* length of application list	*/
Pint	start;		/* starting position	*/
Pint	*error_ind;	/* OUT error indicator	*/
Pposted_struct_list	*list;	/* OUT list of posted structures	*/
Pint	*total_length;	/* OUT length of list in PHIGS	*/
{
    Phg_args		cp_args;
    Phg_ret		ret;
    Psl_ws_info		*wsinfo;
    Wst_phigs_dt	*dt;

    if ( !CB_ENTRY_CHECK( phg_cur_cph, 0, Pfn_INQUIRY)) {
	*error_ind = ERR3;

    } else if ( PSL_WS_STATE( phg_cur_cph->psl) != PWS_ST_WSOP) {
	    *error_ind = ERR3;

    } else if ( !(wsinfo = phg_psl_get_ws_info( phg_cur_cph->psl, ws))) {
	*error_ind = ERR54;

    } else {
	dt = &((Wst*)wsinfo->wstype)->desc_tbl.phigs_dt;
	if ( !(dt->ws_category == PCAT_OUT || 
	       dt->ws_category == PCAT_OUTIN ||
	       dt->ws_category == PCAT_MO) ) {
	    *error_ind = ERR59;

	} else {
	    cp_args.data.idata = ws;
	    ret.err = 0;
	    CP_FUNC( phg_cur_cph, CP_FUNC_OP_INQ_POSTED, &cp_args, &ret);
	    if ( ret.err) {
		*error_ind = ret.err;
	    } else {
		*error_ind = 0;
		list->num_postings = 0;
		if ( (*total_length = ret.data.postlist.num_postings) > 0)
		    if (start < 0 || start >= ret.data.postlist.num_postings)
			*error_ind = ERR2201;
		    else if (length > 0) {
			list->num_postings = MIN( length,
					    ret.data.postlist.num_postings - start);
			bcopy( (char*)&ret.data.postlist.postings[start],
			    (char*)list->postings,
			    list->num_postings * sizeof(Pposted_struct));
		    } else if (length < 0)
			*error_ind = ERRN153;
	    }
	}
    }
}

void
pinq_view_rep( ws, view_index, error_ind, update_state, cur_rep, req_rep)
    Pint	ws;		/* workstation identifier	*/
    Pint	view_index;	/* view index	*/
    Pint	*error_ind;	/* OUT error indicator	*/
    Pupd_st	*update_state;	/* OUT transformation update state	*/
    Pview_rep3	*cur_rep;	/* OUT current view representation	*/
    Pview_rep3	*req_rep;	/* OUT requested view representation	*/
{
    Phg_args		cp_args;
    Phg_ret		ret;
    Psl_ws_info		*wsinfo;
    Wst_phigs_dt	*dt;

    if ( !CB_ENTRY_CHECK( phg_cur_cph, 0, Pfn_INQUIRY)) {
	*error_ind = ERR3;

    } else if ( PSL_WS_STATE( phg_cur_cph->psl) != PWS_ST_WSOP) {
	    *error_ind = ERR3;

    } else if ( !(wsinfo = phg_psl_get_ws_info( phg_cur_cph->psl, ws))) {
	*error_ind = ERR54;

    } else {
	dt = &((Wst*)wsinfo->wstype)->desc_tbl.phigs_dt;
	if ( dt->ws_category == PCAT_MI) {
	    *error_ind = ERR57;

	} else if ( view_index < 0) {
		*error_ind = ERR114;

	} else if ( view_index >= dt->num_view_indices) {
	    *error_ind = ERR101;

	} else {
	    cp_args.data.q_view_rep.wsid = ws;
	    cp_args.data.q_view_rep.index = view_index;
	    CP_FUNC( phg_cur_cph, CP_FUNC_OP_INQ_VIEW_REP, &cp_args, &ret);
	    if ( ret.err) {
		*error_ind = ret.err;
	    } else {
		*error_ind = 0;
		*update_state = ret.data.view_rep.update_state;
		*req_rep = *ret.data.view_rep.req_rep;
		*cur_rep = *ret.data.view_rep.cur_rep;
	    }
	}
    }
}

void
pinq_ws_tran3( ws, error_ind, upd_st, req_win_lim, cur_win_lim, req_vp_lim, cur_vp_lim)
    Pint	ws;		/* workstation identifier	*/
    Pint	*error_ind;	/* OUT error indicator	*/
    Pupd_st     *upd_st;        /* OUT update state             */
    Plimit3     *req_win_lim;   /* OUT requested workstation window */
    Plimit3     *cur_win_lim;   /* OUT current workstation window   */
    Plimit3     *req_vp_lim;    /* OUT requested workstation viewport */
    Plimit3     *cur_vp_lim;    /* OUT current workstation viewport */
{
    Phg_args		cp_args;
    Phg_ret		ret;
    Psl_ws_info		*wsinfo;

    if ( !CB_ENTRY_CHECK( phg_cur_cph, 0, Pfn_INQUIRY)) {
	*error_ind = ERR3;

    } else if ( PSL_WS_STATE( phg_cur_cph->psl) != PWS_ST_WSOP) {
	    *error_ind = ERR3;

    } else if ( !(wsinfo = phg_psl_get_ws_info( phg_cur_cph->psl, ws))) {
	*error_ind = ERR54;

    } else if ( ((Wst*)wsinfo->wstype)->desc_tbl.phigs_dt.ws_category == PCAT_MI) {
	*error_ind = ERR57;

    } else {
	cp_args.data.idata = ws;
	CP_FUNC( phg_cur_cph, CP_FUNC_OP_INQ_WS_XFORM, &cp_args, &ret);
	if ( ret.err) {
	    *error_ind = ret.err;
	} else {
	    *error_ind = 0;
	    *upd_st = ret.data.ws_xform.state;
	    *req_win_lim = ret.data.ws_xform.req_window;
	    *cur_win_lim = ret.data.ws_xform.cur_window;
	    *req_vp_lim = ret.data.ws_xform.req_viewport;
	    *cur_vp_lim = ret.data.ws_xform.cur_viewport;
	}
    }
}

void
pinq_ws_tran( ws, error_ind, upd_st, req_win_lim, cur_win_lim, req_vp_lim, cur_vp_lim)
    Pint		ws;		/* workstation identifier	*/
    Pint		*error_ind;	/* OUT error indicator	        */
    Pupd_st             *upd_st;        /* OUT update state             */
    Plimit              *req_win_lim;   /* OUT requested workstation window */
    Plimit              *cur_win_lim;   /* OUT current workstation window   */
    Plimit              *req_vp_lim;    /* OUT requested workstation viewport */
    Plimit              *cur_vp_lim;    /* OUT current workstation viewport */
{
    Phg_args		cp_args;
    Phg_ret		ret;
    Psl_ws_info		*wsinfo;

    if ( !CB_ENTRY_CHECK( phg_cur_cph, 0, Pfn_INQUIRY)) {
	*error_ind = ERR3;

    } else if ( PSL_WS_STATE( phg_cur_cph->psl) != PWS_ST_WSOP) {
	    *error_ind = ERR3;

    } else if ( !(wsinfo = phg_psl_get_ws_info( phg_cur_cph->psl, ws))) {
	*error_ind = ERR54;

    } else if ( ((Wst*)wsinfo->wstype)->desc_tbl.phigs_dt.ws_category == PCAT_MI) {
	*error_ind = ERR57;

    } else {
	cp_args.data.idata = ws;
	CP_FUNC( phg_cur_cph, CP_FUNC_OP_INQ_WS_XFORM, &cp_args, &ret);
	if ( ret.err) {
	    *error_ind = ret.err;
	} else {
	    *error_ind = 0;
	    *upd_st = ret.data.ws_xform.state;
	    req_win_lim->x_max = ret.data.ws_xform.req_window.x_max;
	    req_win_lim->x_min = ret.data.ws_xform.req_window.x_min;
	    req_win_lim->y_max = ret.data.ws_xform.req_window.y_max;
	    req_win_lim->y_min = ret.data.ws_xform.req_window.y_min;
	    req_vp_lim->x_max = ret.data.ws_xform.req_viewport.x_max;
	    req_vp_lim->x_min = ret.data.ws_xform.req_viewport.x_min;
	    req_vp_lim->y_max = ret.data.ws_xform.req_viewport.y_max;
	    req_vp_lim->y_min = ret.data.ws_xform.req_viewport.y_min;

	    cur_win_lim->x_max = ret.data.ws_xform.cur_window.x_max;
	    cur_win_lim->x_min = ret.data.ws_xform.cur_window.x_min;
	    cur_win_lim->y_max = ret.data.ws_xform.cur_window.y_max;
	    cur_win_lim->y_min = ret.data.ws_xform.cur_window.y_min;
	    cur_vp_lim->x_max = ret.data.ws_xform.cur_viewport.x_max;
	    cur_vp_lim->x_min = ret.data.ws_xform.cur_viewport.x_min;
	    cur_vp_lim->y_max = ret.data.ws_xform.cur_viewport.y_max;
	    cur_vp_lim->y_min = ret.data.ws_xform.cur_viewport.y_min;
	}
    }
}

void
pinq_disp_upd_st(ws, error_ind, def_mode, mod_mode, disp_empty, state)
Pint		 ws;		/* workstation identifier       */
Pint		*error_ind;	/* OUT error indicator  */
Pdefer_mode	*def_mode;	/* OUT deferral mode    */
Pmod_mode	*mod_mode;	/* OUT modification mode        */
Pdisp_surf_empty	*disp_empty;	/* OUT display surface empty    */
Pvisual_st	*state;		/* OUT state of visual representation   */
{
    Phg_args		cp_args;
    Phg_ret		ret;
    Psl_ws_info		*wsinfo;
    Wst_phigs_dt	*dt;

    if ( !CB_ENTRY_CHECK( phg_cur_cph, 0, Pfn_INQUIRY)) {
	*error_ind = ERR3;

    } else if ( PSL_WS_STATE( phg_cur_cph->psl) != PWS_ST_WSOP) {
	    *error_ind = ERR3;

    } else if ( !(wsinfo = phg_psl_get_ws_info( phg_cur_cph->psl, ws))) {
	*error_ind = ERR54;

    } else {
	dt = &((Wst*)wsinfo->wstype)->desc_tbl.phigs_dt;
	if ( !(dt->ws_category == PCAT_OUT || 
	       dt->ws_category == PCAT_OUTIN ||
	       dt->ws_category == PCAT_MO) ) {
	    *error_ind = ERR59;

	} else {
	    cp_args.data.idata = ws;
	    ret.err = 0;
	    CP_FUNC( phg_cur_cph, CP_FUNC_OP_INQ_DISP_UPDATE_STATE,
		&cp_args, &ret);
	    if ( ret.err) {
		*error_ind = ret.err;
	    } else {
		*error_ind = 0;
		*def_mode = ret.data.update_state.def_mode;
		*mod_mode = ret.data.update_state.mod_mode;
		*disp_empty = ret.data.update_state.display_surf;
		*state = ret.data.update_state.state;
	    }
	}
    }
}


/* INQUIRE LIST OF POLYLINE INDICES */
void
pinq_list_line_inds( ws, length, start, error_ind, indices, total_length)
    Pint	ws;		/* workstation identifier	*/
    Pint	length;		/* length of application list	*/
    Pint	start;		/* starting position	*/
    Pint	*error_ind;	/* OUT error indicator	*/
    Pint_list	*indices;	/* OUT list of polyline indices	*/
    Pint	*total_length;	/* OUT length of list in PHIGS	*/
{
    Phg_args		cp_args;
    Psl_ws_info		*wsinfo;
    Wst_phigs_dt	*dt;

    if ( !CB_ENTRY_CHECK( phg_cur_cph, 0, Pfn_INQUIRY)) {
	*error_ind = ERR3;

    } else if ( PSL_WS_STATE( phg_cur_cph->psl) != PWS_ST_WSOP) {
	*error_ind = ERR3;

    } else if ( !(wsinfo = phg_psl_get_ws_info( phg_cur_cph->psl, ws))) {
	*error_ind = ERR54;

    } else {
	dt = &((Wst*)wsinfo->wstype)->desc_tbl.phigs_dt;
	if ( !(dt->ws_category == PCAT_OUT || 
	       dt->ws_category == PCAT_OUTIN ||
	       dt->ws_category == PCAT_MO) ) {
	    *error_ind = ERR59;

	} else {
	    cp_args.data.q_indices.wsid = ws;
	    cp_args.data.q_indices.type = PHG_ARGS_LNREP;
	    cb_ws_inq_table_indices( &cp_args, length, start,
			    error_ind, indices, total_length);
	}
    }
}


/* INQUIRE LIST OF LIGHT SOURCE INDICES */
void
pinq_list_light_src_inds( ws, length, start, error_ind, indices, total_length)
    Pint	ws;		/* workstation identifier	*/
    Pint	length;		/* length of application list	*/
    Pint	start;		/* starting position	*/
    Pint	*error_ind;	/* OUT error indicator	*/
    Pint_list	*indices;	/* OUT list of polyline indices	*/
    Pint	*total_length;	/* OUT length of list in PHIGS	*/
{
    Phg_args		cp_args;
    Psl_ws_info		*wsinfo;
    Wst_phigs_dt	*dt;

    if ( !CB_ENTRY_CHECK( phg_cur_cph, 0, Pfn_INQUIRY)) {
	*error_ind = ERR3;

    } else if ( PSL_WS_STATE( phg_cur_cph->psl) != PWS_ST_WSOP) {
	*error_ind = ERR3;

    } else if ( !(wsinfo = phg_psl_get_ws_info( phg_cur_cph->psl, ws))) {
	*error_ind = ERR54;

    } else {
	dt = &((Wst*)wsinfo->wstype)->desc_tbl.phigs_dt;
	if ( !(dt->ws_category == PCAT_OUT || 
	       dt->ws_category == PCAT_OUTIN ||
	       dt->ws_category == PCAT_MO) ) {
	    *error_ind = ERR59;

	} else {
	    cp_args.data.q_indices.wsid = ws;
	    cp_args.data.q_indices.type = PHG_ARGS_LIGHTSRCREP;
	    cb_ws_inq_table_indices( &cp_args, length, start,
			    error_ind, indices, total_length);
	}
    }
}


/* INQUIRE LIST OF DEPTH CUE INDICES */
void
pinq_list_dcue_inds( ws, length, start, error_ind, indices, total_length)
    Pint	ws;		/* workstation identifier	*/
    Pint	length;		/* length of application list	*/
    Pint	start;		/* starting position	*/
    Pint	*error_ind;	/* OUT error indicator	*/
    Pint_list	*indices;	/* OUT list of polyline indices	*/
    Pint	*total_length;	/* OUT length of list in PHIGS	*/
{
    Phg_args		cp_args;
    Psl_ws_info		*wsinfo;
    Wst_phigs_dt	*dt;

    if ( !CB_ENTRY_CHECK( phg_cur_cph, 0, Pfn_INQUIRY)) {
	*error_ind = ERR3;

    } else if ( PSL_WS_STATE( phg_cur_cph->psl) != PWS_ST_WSOP) {
	*error_ind = ERR3;

    } else if ( !(wsinfo = phg_psl_get_ws_info( phg_cur_cph->psl, ws))) {
	*error_ind = ERR54;

    } else {
	dt = &((Wst*)wsinfo->wstype)->desc_tbl.phigs_dt;
	if ( !(dt->ws_category == PCAT_OUT || 
	       dt->ws_category == PCAT_OUTIN ||
	       dt->ws_category == PCAT_MO) ) {
	    *error_ind = ERR59;
	} else {
	    cp_args.data.q_indices.wsid = ws;
	    cp_args.data.q_indices.type = PHG_ARGS_DCUEREP;
	    cb_ws_inq_table_indices( &cp_args, length, start,
			    error_ind, indices, total_length);
	}
    }
}


/* INQUIRE LIST OF POLYMARKER INDICES */
void
pinq_list_marker_inds( ws, length, start, error_ind, indices, total_length)
    Pint	ws;		/* workstation identifier	*/
    Pint	length;		/* length of application list	*/
    Pint	start;		/* starting position	*/
    Pint	*error_ind;	/* OUT error indicator	*/
    Pint_list	*indices;	/* OUT list of polymarker indices	*/
    Pint	*total_length;	/* OUT length of list in PHIGS	*/
{
    Phg_args		cp_args;
    Psl_ws_info		*wsinfo;
    Wst_phigs_dt	*dt;

    if ( !CB_ENTRY_CHECK( phg_cur_cph, 0, Pfn_INQUIRY)) {
	*error_ind = ERR3;

    } else if ( PSL_WS_STATE( phg_cur_cph->psl) != PWS_ST_WSOP) {
	*error_ind = ERR3;

    } else if ( !(wsinfo = phg_psl_get_ws_info( phg_cur_cph->psl, ws))) {
	*error_ind = ERR54;

    } else {
	dt = &((Wst*)wsinfo->wstype)->desc_tbl.phigs_dt;
	if ( !(dt->ws_category == PCAT_OUT || 
	       dt->ws_category == PCAT_OUTIN ||
	       dt->ws_category == PCAT_MO) ) {
	    *error_ind = ERR59;

	} else {
	    cp_args.data.q_indices.wsid = ws;
	    cp_args.data.q_indices.type = PHG_ARGS_MKREP;
	    cb_ws_inq_table_indices( &cp_args, length, start,
			    error_ind, indices, total_length);
	}
    }
}


/* INQUIRE LIST OF TEXT INDICES */
void
pinq_list_text_inds( ws, length, start, error_ind, indices, total_length)
    Pint	ws;		/* workstation identifier	*/
    Pint	length;		/* length of application list	*/
    Pint	start;		/* starting position	*/
    Pint	*error_ind;	/* OUT error indicator	*/
    Pint_list	*indices;	/* OUT list of text indices	*/
    Pint	*total_length;	/* OUT length of list in PHIGS	*/
{
    Phg_args		cp_args;
    Psl_ws_info		*wsinfo;
    Wst_phigs_dt	*dt;

    if ( !CB_ENTRY_CHECK( phg_cur_cph, 0, Pfn_INQUIRY)) {
	*error_ind = ERR3;

    } else if ( PSL_WS_STATE( phg_cur_cph->psl) != PWS_ST_WSOP) {
	*error_ind = ERR3;

    } else if ( !(wsinfo = phg_psl_get_ws_info( phg_cur_cph->psl, ws))) {
	*error_ind = ERR54;

    } else {
	dt = &((Wst*)wsinfo->wstype)->desc_tbl.phigs_dt;
	if ( !(dt->ws_category == PCAT_OUT || 
	       dt->ws_category == PCAT_OUTIN ||
	       dt->ws_category == PCAT_MO) ) {
	    *error_ind = ERR59;

	} else {
	    cp_args.data.q_indices.wsid = ws;
	    cp_args.data.q_indices.type = PHG_ARGS_TXREP;
	    cb_ws_inq_table_indices( &cp_args, length, start,
			    error_ind, indices, total_length);
	}
    }
}


/* INQUIRE LIST OF INTERIOR INDICES */
void
pinq_list_int_inds( ws, length, start, error_ind, indices, total_length)
    Pint	ws;		/* workstation identifier	*/
    Pint	length;		/* length of application list	*/
    Pint	start;		/* starting position	*/
    Pint	*error_ind;	/* OUT error indicator	*/
    Pint_list	*indices;	/* OUT list of interior indices	*/
    Pint	*total_length;	/* OUT length of list in PHIGS	*/
{
    Phg_args		cp_args;
    Psl_ws_info		*wsinfo;
    Wst_phigs_dt	*dt;

    if ( !CB_ENTRY_CHECK( phg_cur_cph, 0, Pfn_INQUIRY)) {
	*error_ind = ERR3;

    } else if ( PSL_WS_STATE( phg_cur_cph->psl) != PWS_ST_WSOP) {
	*error_ind = ERR3;

    } else if ( !(wsinfo = phg_psl_get_ws_info( phg_cur_cph->psl, ws))) {
	*error_ind = ERR54;

    } else {
	dt = &((Wst*)wsinfo->wstype)->desc_tbl.phigs_dt;
	if ( !(dt->ws_category == PCAT_OUT || 
	       dt->ws_category == PCAT_OUTIN ||
	       dt->ws_category == PCAT_MO) ) {
	    *error_ind = ERR59;

	} else {
	    cp_args.data.q_indices.wsid = ws;
	    cp_args.data.q_indices.type = PHG_ARGS_INTERREP;
	    cb_ws_inq_table_indices( &cp_args, length, start,
			    error_ind, indices, total_length);
	}
    }
}


/* INQUIRE LIST OF EDGE INDICES */
void
pinq_list_edge_inds( ws, length, start, error_ind, indices, total_length)
    Pint	ws;		/* workstation identifier	*/
    Pint	length;		/* length of application list	*/
    Pint	start;		/* starting position	*/
    Pint	*error_ind;	/* OUT error indicator	*/
    Pint_list	*indices;	/* OUT list of edge indices	*/
    Pint	*total_length;	/* OUT length of list in PHIGS	*/
{
    Phg_args		cp_args;
    Psl_ws_info		*wsinfo;
    Wst_phigs_dt	*dt;

    if ( !CB_ENTRY_CHECK( phg_cur_cph, 0, Pfn_INQUIRY)) {
	*error_ind = ERR3;

    } else if ( PSL_WS_STATE( phg_cur_cph->psl) != PWS_ST_WSOP) {
	*error_ind = ERR3;

    } else if ( !(wsinfo = phg_psl_get_ws_info( phg_cur_cph->psl, ws))) {
	*error_ind = ERR54;

    } else {
	dt = &((Wst*)wsinfo->wstype)->desc_tbl.phigs_dt;
	if ( !(dt->ws_category == PCAT_OUT || 
	       dt->ws_category == PCAT_OUTIN ||
	       dt->ws_category == PCAT_MO) ) {
	    *error_ind = ERR59;

	} else {
	    cp_args.data.q_indices.wsid = ws;
	    cp_args.data.q_indices.type = PHG_ARGS_EDGEREP;
	    cb_ws_inq_table_indices( &cp_args, length, start,
			    error_ind, indices, total_length);
	}
    }
}


/* INQUIRE LIST OF PATTERN INDICES */
void
pinq_list_pat_inds( ws, length, start, error_ind, indices, total_length)
    Pint	ws;		/* workstation identifier	*/
    Pint	length;		/* length of application list	*/
    Pint	start;		/* starting position	*/
    Pint	*error_ind;	/* OUT error indicator	*/
    Pint_list	*indices;	/* OUT list of pattern indices	*/
    Pint	*total_length;	/* OUT length of list in PHIGS	*/
{
    Phg_args		cp_args;
    Psl_ws_info		*wsinfo;
    Wst_phigs_dt	*dt;

    if ( !CB_ENTRY_CHECK( phg_cur_cph, 0, Pfn_INQUIRY)) {
	*error_ind = ERR3;

    } else if ( PSL_WS_STATE( phg_cur_cph->psl) != PWS_ST_WSOP) {
	*error_ind = ERR3;

    } else if ( !(wsinfo = phg_psl_get_ws_info( phg_cur_cph->psl, ws))) {
	*error_ind = ERR54;

    } else {
	dt = &((Wst*)wsinfo->wstype)->desc_tbl.phigs_dt;
	if ( !(dt->ws_category == PCAT_OUT || 
	       dt->ws_category == PCAT_OUTIN ||
	       dt->ws_category == PCAT_MO) ) {
	    *error_ind = ERR59;

	} else {
	    cp_args.data.q_indices.wsid = ws;
	    cp_args.data.q_indices.type = PHG_ARGS_PTREP;
	    cb_ws_inq_table_indices( &cp_args, length, start,
			    error_ind, indices, total_length);
	}
    }
}


/* INQUIRE LIST OF COLOUR INDICES */
void
pinq_list_colr_inds( ws, length, start, error_ind, indices, total_length)
    Pint	ws;		/* workstation identifier	*/
    Pint	length;		/* length of application list	*/
    Pint	start;		/* starting position	*/
    Pint	*error_ind;	/* OUT error indicator	*/
    Pint_list	*indices;	/* OUT list of colour indices	*/
    Pint	*total_length;	/* OUT length of list in PHIGS	*/
{
    Phg_args		cp_args;
    Psl_ws_info		*wsinfo;
    Wst_phigs_dt	*dt;

    if ( !CB_ENTRY_CHECK( phg_cur_cph, 0, Pfn_INQUIRY)) {
	*error_ind = ERR3;

    } else if ( PSL_WS_STATE( phg_cur_cph->psl) != PWS_ST_WSOP) {
	*error_ind = ERR3;

    } else if ( !(wsinfo = phg_psl_get_ws_info( phg_cur_cph->psl, ws))) {
	*error_ind = ERR54;

    } else {
	dt = &((Wst*)wsinfo->wstype)->desc_tbl.phigs_dt;
	if ( !(dt->ws_category == PCAT_OUT || 
	       dt->ws_category == PCAT_OUTIN ||
	       dt->ws_category == PCAT_MO) ) {
	    *error_ind = ERR59;

	} else {
	    cp_args.data.q_indices.wsid = ws;
	    cp_args.data.q_indices.type = PHG_ARGS_COREP;
	    cb_ws_inq_table_indices( &cp_args, length, start,
			    error_ind, indices, total_length);
	}
    }
}


/* INQUIRE TEXT REPRESENTATION */
void
pinq_text_rep( ws, index, type, error_ind, rep)
    Pint	 ws;		/* workstation identifier	*/
    Pint	 index;		/* text index	*/
    Pinq_type	 type;		/* type of returned value	*/
    Pint	 *error_ind;	/* OUT error indicator	*/
    Ptext_bundle *rep;		/* OUT text representation	*/
{
    Phg_args		cp_args;
    Phg_ret		ret;
    Psl_ws_info		*wsinfo;
    Wst_phigs_dt	*dt;

    if ( !CB_ENTRY_CHECK( phg_cur_cph, 0, Pfn_INQUIRY)) {
	*error_ind = ERR3;

    } else if ( PSL_WS_STATE( phg_cur_cph->psl) != PWS_ST_WSOP) {
	*error_ind = ERR3;

    } else if ( !(wsinfo = phg_psl_get_ws_info( phg_cur_cph->psl, ws))) {
	*error_ind = ERR54;

    } else {
	*error_ind = 0;
	dt = &((Wst*)wsinfo->wstype)->desc_tbl.phigs_dt;
	if ( !(dt->ws_category == PCAT_OUT || 
	       dt->ws_category == PCAT_OUTIN ||
	       dt->ws_category == PCAT_MO) ) {
	    *error_ind = ERR59;

	} else if ( index < 1) {
	    *error_ind = ERR100;
        }

	if (!*error_ind) {
	    cp_args.data.q_rep.wsid = ws;
	    cp_args.data.q_rep.index = index;
	    cp_args.data.q_rep.type = type;
	    cp_args.data.q_rep.rep_type = PHG_ARGS_EXTTXREP;
	    CP_FUNC( phg_cur_cph, CP_FUNC_OP_INQ_REPRESENTATION, &cp_args,
			&ret);
	    if ( ret.err )
		*error_ind = ret.err;
	    else if ( ret.data.rep.exttxrep.colr.type != PINDIRECT )
		*error_ind = ERR134;
	    else {
		rep->font = ret.data.rep.exttxrep.font;
		rep->prec = ret.data.rep.exttxrep.prec;
		rep->char_expan = ret.data.rep.exttxrep.char_expan;
		rep->char_space = ret.data.rep.exttxrep.char_space;
		rep->colr_ind = ret.data.rep.exttxrep.colr.val.ind;
	    }
	}
    }
}


/* INQUIRE EXTENDED TEXT REPRESENTATION */
void
pinq_text_rep_plus( ws, index, type, error_ind, rep)
    Pint	 ws;		/* workstation identifier	*/
    Pint	 index;		/* text index	*/
    Pinq_type	 type;		/* type of returned value	*/
    Pint	*error_ind;	/* OUT error indicator	*/
    Ptext_bundle_plus	*rep;		/* OUT extended text representation	*/
{
    Phg_args		cp_args;
    Phg_ret		ret;
    Psl_ws_info		*wsinfo;
    Wst_phigs_dt	*dt;

    if ( !CB_ENTRY_CHECK( phg_cur_cph, 0, Pfn_INQUIRY)) {
	*error_ind = ERR3;

    } else if ( PSL_WS_STATE( phg_cur_cph->psl) != PWS_ST_WSOP) {
	*error_ind = ERR3;

    } else if ( !(wsinfo = phg_psl_get_ws_info( phg_cur_cph->psl, ws))) {
	*error_ind = ERR54;

    } else {
	*error_ind = 0;
	dt = &((Wst*)wsinfo->wstype)->desc_tbl.phigs_dt;
	if ( !(dt->ws_category == PCAT_OUT || 
	       dt->ws_category == PCAT_OUTIN ||
	       dt->ws_category == PCAT_MO) ) {
	    *error_ind = ERR59;

	} else if ( index < 1) {
	    *error_ind = ERR100;
        }

	if (!*error_ind) {
	    cp_args.data.q_rep.wsid = ws;
	    cp_args.data.q_rep.index = index;
	    cp_args.data.q_rep.type = type;
	    cp_args.data.q_rep.rep_type = PHG_ARGS_EXTTXREP;
	    CP_FUNC( phg_cur_cph, CP_FUNC_OP_INQ_REPRESENTATION, &cp_args,
			&ret);
	    if ( ret.err) {
		*error_ind = ret.err;
	    } else {
		*rep = ret.data.rep.exttxrep;
	    }
	}
    }
}

/* INQUIRE INTERIOR REPRESENTATION */
void
pinq_int_rep( ws, index, type, error_ind, rep)
    Pint	 ws;		/* workstation identifier	*/
    Pint	 index;		/* interior index	*/
    Pinq_type	 type;		/* type of returned value	*/
    Pint	 *error_ind;	/* OUT error indicator	*/
    Pint_bundle	 *rep;		/* OUT interior representation	*/
{
    Phg_args		cp_args;
    Phg_ret		ret;
    Psl_ws_info		*wsinfo;
    Wst_phigs_dt	*dt;

    if ( !CB_ENTRY_CHECK( phg_cur_cph, 0, Pfn_INQUIRY)) {
	*error_ind = ERR3;

    } else if ( PSL_WS_STATE( phg_cur_cph->psl) != PWS_ST_WSOP) {
	*error_ind = ERR3;

    } else if ( !(wsinfo = phg_psl_get_ws_info( phg_cur_cph->psl, ws))) {
	*error_ind = ERR54;

    } else {
	*error_ind = 0;
	dt = &((Wst*)wsinfo->wstype)->desc_tbl.phigs_dt;
	if ( !(dt->ws_category == PCAT_OUT || 
	       dt->ws_category == PCAT_OUTIN ||
	       dt->ws_category == PCAT_MO) ) {
	    *error_ind = ERR59;

	} else if ( index < 1) {
	    *error_ind = ERR100;
        }

	if (!*error_ind) {
	    cp_args.data.q_rep.wsid = ws;
	    cp_args.data.q_rep.index = index;
	    cp_args.data.q_rep.type = type;
	    cp_args.data.q_rep.rep_type = PHG_ARGS_EXTINTERREP;
	    CP_FUNC( phg_cur_cph, CP_FUNC_OP_INQ_REPRESENTATION, &cp_args,
			&ret);
	    if ( ret.err )
		*error_ind = ret.err;
	    else if ( ret.data.rep.extinterrep.colr.type != PINDIRECT )
		*error_ind = ERR134;
	    else {
		rep->style = ret.data.rep.extinterrep.style;
		rep->style_ind = ret.data.rep.extinterrep.style_ind;
		rep->colr_ind = ret.data.rep.extinterrep.colr.val.ind;
	    }
	}
    }
}


/* INQUIRE EXTENDED INTERIOR REPRESENTATION */
void
pinq_int_rep_plus( ws, index, type, error_ind, rep)
    Pint	 	ws;		/* workstation identifier	*/
    Pint	 	index;		/* interior index	*/
    Pinq_type	 	type;		/* type of returned value	*/
    Pint		*error_ind;	/* OUT error indicator	*/
    Pint_bundle_plus	*rep;		/* OUT interior representation	*/
{
    Phg_args		cp_args;
    Phg_ret		ret;
    Psl_ws_info		*wsinfo;
    Wst_phigs_dt	*dt;

    if ( !CB_ENTRY_CHECK( phg_cur_cph, 0, Pfn_INQUIRY)) {
	*error_ind = ERR3;

    } else if ( PSL_WS_STATE( phg_cur_cph->psl) != PWS_ST_WSOP) {
	*error_ind = ERR3;

    } else if ( !(wsinfo = phg_psl_get_ws_info( phg_cur_cph->psl, ws))) {
	*error_ind = ERR54;

    } else {
	*error_ind = 0;
	dt = &((Wst*)wsinfo->wstype)->desc_tbl.phigs_dt;
	if ( !(dt->ws_category == PCAT_OUT || 
	       dt->ws_category == PCAT_OUTIN ||
	       dt->ws_category == PCAT_MO) ) {
	    *error_ind = ERR59;

	} else if ( index < 1) {
	    *error_ind = ERR100;
        }

	if (!*error_ind) {
	    cp_args.data.q_rep.wsid = ws;
	    cp_args.data.q_rep.index = index;
	    cp_args.data.q_rep.type = type;
	    cp_args.data.q_rep.rep_type = PHG_ARGS_EXTINTERREP;
	    CP_FUNC( phg_cur_cph, CP_FUNC_OP_INQ_REPRESENTATION, &cp_args,
			&ret);
	    if ( ret.err) {
		*error_ind = ret.err;
	    } else {
		*rep = ret.data.rep.extinterrep;
	    }
	}
    }
}


/* INQUIRE EDGE REPRESENTATION */
void
pinq_edge_rep( ws, index, type, error_ind, rep)
    Pint	 ws;		/* workstation identifier	*/
    Pint	 index;		/* edge index	*/
    Pinq_type	 type;		/* type of returned value	*/
    Pint	 *error_ind;	/* OUT error indicator	*/
    Pedge_bundle *rep;		/* OUT edge representation	*/
{
    Phg_args		cp_args;
    Phg_ret		ret;
    Psl_ws_info		*wsinfo;
    Wst_phigs_dt	*dt;

    if ( !CB_ENTRY_CHECK( phg_cur_cph, 0, Pfn_INQUIRY)) {
	*error_ind = ERR3;

    } else if ( PSL_WS_STATE( phg_cur_cph->psl) != PWS_ST_WSOP) {
	*error_ind = ERR3;

    } else if ( !(wsinfo = phg_psl_get_ws_info( phg_cur_cph->psl, ws))) {
	*error_ind = ERR54;

    } else {
	*error_ind = 0;
	dt = &((Wst*)wsinfo->wstype)->desc_tbl.phigs_dt;
	if ( !(dt->ws_category == PCAT_OUT || 
	       dt->ws_category == PCAT_OUTIN ||
	       dt->ws_category == PCAT_MO) ) {
	    *error_ind = ERR59;

	} else if ( index < 1) {
	    *error_ind = ERR100;
        }

	if (!*error_ind) {
	    cp_args.data.q_rep.wsid = ws;
	    cp_args.data.q_rep.index = index;
	    cp_args.data.q_rep.type = type;
	    cp_args.data.q_rep.rep_type = PHG_ARGS_EXTEDGEREP;
	    CP_FUNC( phg_cur_cph, CP_FUNC_OP_INQ_REPRESENTATION, &cp_args,
			&ret);
	    if ( ret.err)
		*error_ind = ret.err;
	    else if ( ret.data.rep.extedgerep.colr.type != PINDIRECT )
		*error_ind = ERR134;
	    else {
		rep->flag = ret.data.rep.extedgerep.flag;
		rep->type = ret.data.rep.extedgerep.type;
		rep->width = ret.data.rep.extedgerep.width;
		rep->colr_ind = ret.data.rep.extedgerep.colr.val.ind;
	    }
	}
    }
}


/* INQUIRE EXTENDED EDGE REPRESENTATION */
void
pinq_edge_rep_plus( ws, index, type, error_ind, rep)
    Pint	 	ws;		/* workstation identifier	*/
    Pint	 	index;		/* edge index	*/
    Pinq_type	 	type;		/* type of returned value	*/
    Pint		*error_ind;	/* OUT error indicator	*/
    Pedge_bundle_plus	*rep;		/* OUT edge representation	*/
{
    Phg_args		cp_args;
    Phg_ret		ret;
    Psl_ws_info		*wsinfo;
    Wst_phigs_dt	*dt;

    if ( !CB_ENTRY_CHECK( phg_cur_cph, 0, Pfn_INQUIRY)) {
	*error_ind = ERR3;

    } else if ( PSL_WS_STATE( phg_cur_cph->psl) != PWS_ST_WSOP) {
	*error_ind = ERR3;

    } else if ( !(wsinfo = phg_psl_get_ws_info( phg_cur_cph->psl, ws))) {
	*error_ind = ERR54;

    } else {
	*error_ind = 0;
	dt = &((Wst*)wsinfo->wstype)->desc_tbl.phigs_dt;
	if ( !(dt->ws_category == PCAT_OUT || 
	       dt->ws_category == PCAT_OUTIN ||
	       dt->ws_category == PCAT_MO) ) {
	    *error_ind = ERR59;

	} else if ( index < 1) {
	    *error_ind = ERR100;
        }

	if (!*error_ind) {
	    cp_args.data.q_rep.wsid = ws;
	    cp_args.data.q_rep.index = index;
	    cp_args.data.q_rep.type = type;
	    cp_args.data.q_rep.rep_type = PHG_ARGS_EXTEDGEREP;
	    CP_FUNC( phg_cur_cph, CP_FUNC_OP_INQ_REPRESENTATION, &cp_args,
			&ret);
	    if ( ret.err) {
		*error_ind = ret.err;
	    } else {
		*rep = ret.data.rep.extedgerep;
	    }
	}
    }
}

void
pinq_colr_rep( ws, index, type, error_ind, rep)
    Pint	 ws;		/* workstation identifier	*/
    Pint	 index;		/* colour index	*/
    Pinq_type	 type;		/* type of returned value	*/
    Pint	 *error_ind;	/* OUT error indicator	*/
    Pcolr_rep *rep;		/* OUT colour representation	*/
{
    Phg_args		cp_args;
    Phg_ret		ret;
    Psl_ws_info		*wsinfo;
    Wst_phigs_dt	*dt;

    if ( !CB_ENTRY_CHECK( phg_cur_cph, 0, Pfn_INQUIRY)) {
	*error_ind = ERR3;

    } else if ( PSL_WS_STATE( phg_cur_cph->psl) != PWS_ST_WSOP) {
	*error_ind = ERR3;

    } else if ( !(wsinfo = phg_psl_get_ws_info( phg_cur_cph->psl, ws))) {
	*error_ind = ERR54;

    } else {
	*error_ind = 0;
	dt = &((Wst*)wsinfo->wstype)->desc_tbl.phigs_dt;
	if ( !(dt->ws_category == PCAT_OUT || 
	       dt->ws_category == PCAT_OUTIN ||
	       dt->ws_category == PCAT_MO) ) {
	    *error_ind = ERR59;

	} else if ( index < 0) {
	    *error_ind = ERR113;
        }

	if (!*error_ind) {
	    cp_args.data.q_rep.wsid = ws;
	    cp_args.data.q_rep.index = index;
	    cp_args.data.q_rep.type = type;
	    cp_args.data.q_rep.rep_type = PHG_ARGS_COREP;
	    CP_FUNC( phg_cur_cph, CP_FUNC_OP_INQ_REPRESENTATION, &cp_args,
			&ret);
	    *error_ind = ret.err;
	    if ( !ret.err)
		*rep = ret.data.rep.corep;
	}
    }
}


/* INQUIRE PATTERN REPRESENTATION */
void
pinq_pat_rep(ws, index, type, store, error_ind, rep)
Pint		 ws;		/* workstation identifier	*/
Pint		 index;		/* pattern index		*/
Pinq_type	 type;		/* type of returned value	*/
Pstore		 store;		/* handle to Store object       */
Pint		*error_ind;	/* OUT error indicator		*/
Ppat_rep	**rep;		/* OUT pattern representation	*/
{
    Phg_args		cp_args;
    Phg_ret		ret;
    Psl_ws_info		*wsinfo;
    Wst_phigs_dt	*dt;

    if ( !CB_ENTRY_CHECK( phg_cur_cph, 0, Pfn_INQUIRY)) {
	*error_ind = ERR3;

    } else if ( PSL_WS_STATE( phg_cur_cph->psl) != PWS_ST_WSOP) {
	*error_ind = ERR3;

    } else if ( !(wsinfo = phg_psl_get_ws_info( phg_cur_cph->psl, ws))) {
	*error_ind = ERR54;

    } else {
	*error_ind = 0;
	dt = &((Wst*)wsinfo->wstype)->desc_tbl.phigs_dt;
	if ( !(dt->ws_category == PCAT_OUT || 
	       dt->ws_category == PCAT_OUTIN ||
	       dt->ws_category == PCAT_MO) ) {
	    *error_ind = ERR59;

	} else if ( index < 1) {
	    *error_ind = ERR112;
        }

        if (!*error_ind) {
	    cp_args.data.q_rep.wsid = ws;
	    cp_args.data.q_rep.index = index;
	    cp_args.data.q_rep.type = type;
	    cp_args.data.q_rep.rep_type = PHG_ARGS_EXTPTREP;
	    CP_FUNC( phg_cur_cph, CP_FUNC_OP_INQ_REPRESENTATION, &cp_args,
			&ret);
	    if ( ret.err )
		*error_ind = ret.err;
	    else if ( ret.data.rep.extptrep.type != PINDIRECT )
		*error_ind = ERR134;
	    else {
		int	i, size, num_colrs;
		size = (num_colrs = (ret.data.rep.extptrep.dims.size_x *
		    ret.data.rep.extptrep.dims.size_y)) * sizeof(Pint);
		if ( CB_STORE_SPACE( ((_Pstore *)store), size, error_ind ) ) {
		    *rep = &((_Pstore *)store)->data.pat_rep;
		    (*rep)->colr_array = (Pint *)((_Pstore *)store)->buf;
		    (*rep)->dims = ret.data.rep.extptrep.dims;
		    if ( num_colrs > 0 ) {
			for ( i = 0; i < num_colrs; i++ )
			    (*rep)->colr_array[i] =
				ret.data.rep.extptrep.colr_array[i].ind;
		    }
		}
	    }
	}
    }
}


void
pinq_pat_rep_plus(ws, index, type, store, error_ind, rep)
Pint		 ws;		/* workstation identifier	*/
Pint		 index;		/* pattern index		*/
Pinq_type	 type;		/* type of returned value	*/
Pstore		 store;		/* OUT pointer to buffer	*/
Pint		*error_ind;	/* OUT error indicator		*/
Ppat_rep_plus	**rep;		/* OUT pattern representation	*/
{
    Phg_args		cp_args;
    Phg_ret		ret;
    Psl_ws_info		*wsinfo;
    Wst_phigs_dt	*dt;

    if ( !CB_ENTRY_CHECK( phg_cur_cph, 0, Pfn_INQUIRY)) {
	*error_ind = ERR3;

    } else if ( PSL_WS_STATE( phg_cur_cph->psl) != PWS_ST_WSOP) {
	*error_ind = ERR3;

    } else if ( !(wsinfo = phg_psl_get_ws_info( phg_cur_cph->psl, ws))) {
	*error_ind = ERR54;

    } else {
	*error_ind = 0;
	dt = &((Wst*)wsinfo->wstype)->desc_tbl.phigs_dt;
	if ( !(dt->ws_category == PCAT_OUT || 
	       dt->ws_category == PCAT_OUTIN ||
	       dt->ws_category == PCAT_MO) ) {
	    *error_ind = ERR59;

	} else if ( index < 1) {
	    *error_ind = ERR112;
        }

        if (!*error_ind) {
	    cp_args.data.q_rep.wsid = ws;
	    cp_args.data.q_rep.index = index;
	    cp_args.data.q_rep.type = type;
	    cp_args.data.q_rep.rep_type = PHG_ARGS_EXTPTREP;
	    CP_FUNC( phg_cur_cph, CP_FUNC_OP_INQ_REPRESENTATION, &cp_args,
			&ret);
	    if ( !(*error_ind = ret.err) ) {
		int	size;
		size = (ret.data.rep.extptrep.dims.size_x *
		    ret.data.rep.extptrep.dims.size_y) * sizeof(Pcoval);
		if ( CB_STORE_SPACE( ((_Pstore *)store), size, error_ind ) ) {
		    *rep = &((_Pstore *)store)->data.ext_pat_rep;
		    (*rep)->colr_array = (Pcoval *)((_Pstore *)store)->buf;
		    (*rep)->dims = ret.data.rep.extptrep.dims;
		    (*rep)->type = ret.data.rep.extptrep.type;
		    if ( size > 0 )
			bcopy((char*)ret.data.rep.extptrep.colr_array,
			    (char *)(*rep)->colr_array, size );
		}
	    }
	}
    }
}

static void
inq_filter( type, ws, store, error_ind, filter)
    Phg_args_flt_type	type;
    Pint	 	ws;
    _Pstore		*store;
    Pint		*error_ind;
    Pfilter             **filter;
{
    Phg_args		cp_args;
    Phg_ret		ret;
    Phg_ret_filter	*filt = &ret.data.filter;
    Psl_ws_info		*wsinfo;
    Wst_phigs_dt	*dt;

    if ( !CB_ENTRY_CHECK( phg_cur_cph, 0, Pfn_INQUIRY)) {
	*error_ind = ERR3;

    } else if ( PSL_WS_STATE( phg_cur_cph->psl) != PWS_ST_WSOP) {
	*error_ind = ERR3;

    } else if ( !(wsinfo = phg_psl_get_ws_info( phg_cur_cph->psl, ws))) {
	*error_ind = ERR54;

    } else {
	dt = &((Wst*)wsinfo->wstype)->desc_tbl.phigs_dt;
	if ( !(dt->ws_category == PCAT_OUT || 
	       dt->ws_category == PCAT_OUTIN ||
	       dt->ws_category == PCAT_MO) ) {
	    *error_ind = ERR59;

	} else {
	    cp_args.data.q_filter.wsid = ws;
	    cp_args.data.q_filter.type = type;
	    CP_FUNC( phg_cur_cph, CP_FUNC_OP_INQ_FILTER, &cp_args, &ret);
	    *error_ind = 0;
	    if ( ret.err )
		*error_ind = ret.err;
	    else {
		int	size;

		*error_ind = 0;
		size = (filt->incl.num_ints + filt->excl.num_ints)
		    * sizeof(Pint);
		if ( CB_STORE_SPACE( store, size, error_ind ) ) {
		    *filter = &store->data.filter;
		    (*filter)->incl_set.num_ints = filt->incl.num_ints;
		    (*filter)->excl_set.num_ints = filt->excl.num_ints;
		    (*filter)->incl_set.ints = (Pint *)store->buf;
		    (*filter)->excl_set.ints = (*filter)->incl_set.ints
			+ (*filter)->incl_set.num_ints;
		    if ( filt->incl.num_ints > 0 ) {
			bcopy( (char*)&filt->incl.ints[0],
			      (char*)(*filter)->incl_set.ints,
			      (*filter)->incl_set.num_ints * sizeof(Pint));
		    }
		    if ( filt->excl.num_ints > 0 ) {
			bcopy( (char*)&filt->excl.ints[0],
			      (char*)(*filter)->excl_set.ints,
			      (*filter)->excl_set.num_ints * sizeof(Pint));
		    }
		}
	    }
	}
    }
}

void
pinq_highl_filter( ws, store, error_ind, highl_filter)
    Pint	 ws;		/* workstation identifier	*/
    Pstore       store;		/* handle to Store object       */
    Pint	 *error_ind;	/* OUT error indicator	        */
    Pfilter      **highl_filter;/* OUT highlighting filter      */
{
    inq_filter( PHG_ARGS_FLT_HIGH, ws, ((_Pstore *)store), error_ind, 
		highl_filter);
}

void
pinq_invis_filter( ws, store, error_ind, invis_filter)
    Pint	ws;		/* workstation identifier	*/
    Pstore       store;        /* handle to Store object       */
    Pint	 *error_ind;	/* OUT error indicator	        */
    Pfilter      **invis_filter;/* OUT invisibility filter      */
{
    inq_filter( PHG_ARGS_FLT_INVIS, ws, ((_Pstore *)store), error_ind, 
		invis_filter);
}

void
pinq_colr_model( ws, error_ind, model )
    Pint	ws;		/* workstation identifier	*/
    Pint	*error_ind;	/* OUT error indicator	*/
    Pint	*model;		/* OUT current colour model	*/
{
    Phg_args		cp_args;
    Phg_ret		ret;
    Psl_ws_info		*wsinfo;
    Wst_phigs_dt	*dt;

    if ( !CB_ENTRY_CHECK( phg_cur_cph, 0, Pfn_INQUIRY)) {
	*error_ind = ERR3;

    } else if ( PSL_WS_STATE( phg_cur_cph->psl) != PWS_ST_WSOP) {
	*error_ind = ERR3;

    } else if ( !(wsinfo = phg_psl_get_ws_info( phg_cur_cph->psl, ws))) {
	*error_ind = ERR54;

    } else {
	dt = &((Wst*)wsinfo->wstype)->desc_tbl.phigs_dt;
	if ( !(dt->ws_category == PCAT_OUT || 
	       dt->ws_category == PCAT_OUTIN ||
	       dt->ws_category == PCAT_MO) ) {
	    *error_ind = ERR59;

	} else {
	    cp_args.data.idata = ws;
	    CP_FUNC( phg_cur_cph, CP_FUNC_OP_INQ_COLOUR_MODEL, &cp_args, &ret);
	    if ( ret.err )
		*error_ind = ret.err;
	    else {
		*error_ind = 0;
		*model = ret.data.idata;
	    }
	}
    }
}

void
pinq_hlhsr_mode( ws, error_ind, state, cur_mode, req_mode )
    Pint	ws;		/* workstation identifier	*/
    Pint	*error_ind;	/* OUT error indicator	*/
    Pupd_st	*state;		/* OUT HLHSR update state	*/
    Pint	*cur_mode;	/* OUT current HLHSR mode	*/
    Pint	*req_mode;	/* OUT requested HLHSR mode	*/
{
    Phg_args		cp_args;
    Phg_ret		ret;
    Psl_ws_info		*wsinfo;
    Wst_phigs_dt	*dt;

    if ( !CB_ENTRY_CHECK( phg_cur_cph, 0, Pfn_INQUIRY)) {
	*error_ind = ERR3;

    } else if ( PSL_WS_STATE( phg_cur_cph->psl) != PWS_ST_WSOP) {
	*error_ind = ERR3;

    } else if ( !(wsinfo = phg_psl_get_ws_info( phg_cur_cph->psl, ws))) {
	*error_ind = ERR54;

    } else {
	dt = &((Wst*)wsinfo->wstype)->desc_tbl.phigs_dt;
	if ( dt->ws_category == PCAT_MI) {
	    *error_ind = ERR57;

	} else {
	    cp_args.data.idata = ws;
	    CP_FUNC( phg_cur_cph, CP_FUNC_OP_INQ_HLHSR_MODE, &cp_args, &ret);
	    if ( ret.err )
		*error_ind = ret.err;
	    else {
		*error_ind = 0;
		*state = ret.data.hlhsr_mode.state;
		*cur_mode = ret.data.hlhsr_mode.cur_mode;
		*req_mode = ret.data.hlhsr_mode.req_mode;
	    }
	}
    }
}

void
pinq_line_rep( ws, index, type, error_ind, rep)
    Pint	 ws;		/* workstation identifier	*/
    Pint	 index;		/* polyline index	*/
    Pinq_type	 type;		/* type of returned value	*/
    Pint	 *error_ind;	/* OUT error indicator	*/
    Pline_bundle *rep;		/* OUT polyline representation	*/
{
    Phg_args		cp_args;
    Phg_ret		ret;
    Psl_ws_info		*wsinfo;
    Wst_phigs_dt	*dt;

    if ( !CB_ENTRY_CHECK( phg_cur_cph, 0, Pfn_INQUIRY)) {
	*error_ind = ERR3;

    } else if ( PSL_WS_STATE( phg_cur_cph->psl) != PWS_ST_WSOP) {
	*error_ind = ERR3;

    } else if ( !(wsinfo = phg_psl_get_ws_info( phg_cur_cph->psl, ws))) {
	*error_ind = ERR54;

    } else {
	*error_ind = 0;
	dt = &((Wst*)wsinfo->wstype)->desc_tbl.phigs_dt;
	if ( !(dt->ws_category == PCAT_OUT || 
	       dt->ws_category == PCAT_OUTIN ||
	       dt->ws_category == PCAT_MO) ) {
	    *error_ind = ERR59;

	} else if ( index < 1) {
	    *error_ind = ERR100;
	}

	if (!*error_ind) {
	    cp_args.data.q_rep.wsid = ws;
	    cp_args.data.q_rep.index = index;
	    cp_args.data.q_rep.type = type;
	    cp_args.data.q_rep.rep_type = PHG_ARGS_EXTLNREP;
	    CP_FUNC( phg_cur_cph, CP_FUNC_OP_INQ_REPRESENTATION, &cp_args,
			&ret);
	    if ( ret.err)
		*error_ind = ret.err;
	    else if ( ret.data.rep.extlnrep.colr.type != PINDIRECT )
		*error_ind = ERR134;
	    else {
		rep->type = ret.data.rep.extlnrep.type;
		rep->width = ret.data.rep.extlnrep.width;
		rep->colr_ind = ret.data.rep.extlnrep.colr.val.ind;
	    }
	}
    }
}

void
pinq_line_rep_plus( ws, index, type, error_ind, rep)
    Pint	 ws;		/* workstation identifier	*/
    Pint	 index;		/* polyline index	*/
    Pinq_type	 type;		/* type of returned value	*/
    Pint	*error_ind;	/* OUT error indicator	*/
    Pline_bundle_plus	*rep;		/* OUT polyline representation	*/
{
    Phg_args		cp_args;
    Phg_ret		ret;
    Psl_ws_info		*wsinfo;
    Wst_phigs_dt	*dt;

    if ( !CB_ENTRY_CHECK( phg_cur_cph, 0, Pfn_INQUIRY)) {
	*error_ind = ERR3;

    } else if ( PSL_WS_STATE( phg_cur_cph->psl) != PWS_ST_WSOP) {
	*error_ind = ERR3;

    } else if ( !(wsinfo = phg_psl_get_ws_info( phg_cur_cph->psl, ws))) {
	*error_ind = ERR54;

    } else {
	*error_ind = 0;
	dt = &((Wst*)wsinfo->wstype)->desc_tbl.phigs_dt;
	if ( !(dt->ws_category == PCAT_OUT || 
	       dt->ws_category == PCAT_OUTIN ||
	       dt->ws_category == PCAT_MO) ) {
	    *error_ind = ERR59;

	} else if ( index < 1) {
	    *error_ind = ERR100;
        }

	if (!*error_ind) {
	    cp_args.data.q_rep.wsid = ws;
	    cp_args.data.q_rep.index = index;
	    cp_args.data.q_rep.type = type;
	    cp_args.data.q_rep.rep_type = PHG_ARGS_EXTLNREP;
	    CP_FUNC( phg_cur_cph, CP_FUNC_OP_INQ_REPRESENTATION, &cp_args,
			&ret);
	    if ( ret.err) {
		*error_ind = ret.err;
	    } else {
		*rep = ret.data.rep.extlnrep;
	    }
	}
    }
}

/* INQUIRE POLYMARKER REPRESENTATION */
void
pinq_marker_rep( ws, index, type, error_ind, rep)
    Pint	 ws;		/* workstation identifier	*/
    Pint	 index;		/* polymarker index	*/
    Pinq_type	 type;		/* type of returned value	*/
    Pint	 *error_ind;	/* OUT error indicator	*/
    Pmarker_bundle *rep;		/* OUT polymarker representation	*/
{
    Phg_args		cp_args;
    Phg_ret		ret;
    Psl_ws_info		*wsinfo;
    Wst_phigs_dt	*dt;

    if ( !CB_ENTRY_CHECK( phg_cur_cph, 0, Pfn_INQUIRY)) {
	*error_ind = ERR3;

    } else if ( PSL_WS_STATE( phg_cur_cph->psl) != PWS_ST_WSOP) {
	*error_ind = ERR3;

    } else if ( !(wsinfo = phg_psl_get_ws_info( phg_cur_cph->psl, ws))) {
	*error_ind = ERR54;

    } else {
	*error_ind = 0;
	dt = &((Wst*)wsinfo->wstype)->desc_tbl.phigs_dt;
	if ( !(dt->ws_category == PCAT_OUT || 
	       dt->ws_category == PCAT_OUTIN ||
	       dt->ws_category == PCAT_MO) ) {
	    *error_ind = ERR59;

	} else if ( index < 1) {
	    *error_ind = ERR100;
        }

	if (!*error_ind) {
	    cp_args.data.q_rep.wsid = ws;
	    cp_args.data.q_rep.index = index;
	    cp_args.data.q_rep.type = type;
	    cp_args.data.q_rep.rep_type = PHG_ARGS_EXTMKREP;
	    CP_FUNC( phg_cur_cph, CP_FUNC_OP_INQ_REPRESENTATION, &cp_args,
			&ret);
	    if ( ret.err )
		*error_ind = ret.err;
	    else if ( ret.data.rep.extmkrep.colr.type != PINDIRECT )
		*error_ind = ERR134;
	    else {
		rep->type = ret.data.rep.extmkrep.type;
		rep->size = ret.data.rep.extmkrep.size;
		if (ret.data.rep.extmkrep.colr.type == PINDIRECT)
		    rep->colr_ind = ret.data.rep.extmkrep.colr.val.ind;
		/* TODO: else ??? */
	    }
	}
    }
}


/* INQUIRE EXTENDED POLYMARKER REPRESENTATION */
void
pinq_marker_rep_plus( wsid, index, type, error_ind, rep)
    Pint	 wsid;		/* workstation identifier	 */
    Pint	 index;		/* polymarker index		 */
    Pinq_type	 type;		/* type of returned value	 */
    Pint	*error_ind;	/* OUT error indicator		 */
    Pmarker_bundle_plus	*rep;		/* OUT polymarker representation */
{
    Phg_args		cp_args;
    Phg_ret		ret;
    Psl_ws_info		*wsinfo;
    Wst_phigs_dt	*dt;

    if ( !CB_ENTRY_CHECK( phg_cur_cph, 0, Pfn_INQUIRY)) {
	*error_ind = ERR3;

    } else if ( PSL_WS_STATE( phg_cur_cph->psl) != PWS_ST_WSOP) {
	*error_ind = ERR3;

    } else if ( !(wsinfo = phg_psl_get_ws_info( phg_cur_cph->psl, wsid))) {
	*error_ind = ERR54;

    } else {
	*error_ind = 0;
	dt = &((Wst*)wsinfo->wstype)->desc_tbl.phigs_dt;
	if ( !(dt->ws_category == PCAT_OUT || 
	       dt->ws_category == PCAT_OUTIN ||
	       dt->ws_category == PCAT_MO) ) {
	    *error_ind = ERR59;

	} else if ( index < 1) {
	    *error_ind = ERR100;
        }

	if (!*error_ind) {
	    cp_args.data.q_rep.wsid = wsid;
	    cp_args.data.q_rep.index = index;
	    cp_args.data.q_rep.type = type;
	    cp_args.data.q_rep.rep_type = PHG_ARGS_EXTMKREP;
	    CP_FUNC( phg_cur_cph, CP_FUNC_OP_INQ_REPRESENTATION, &cp_args,
			&ret);
	    if ( ret.err) {
		*error_ind = ret.err;
	    } else {
		*rep = ret.data.rep.extmkrep;
	    }
	}
    }
}


static Pint
check_pseudo_N_colours( colr_model, colrs )
    Pint		colr_model;
    Pfloat_list_list	*colrs;
{
    Pint	err = 0;

    register int	i, j;

    switch ( colr_model ) {
	case PMODEL_RGB:
	case PMODEL_CIELUV:
	case PMODEL_HSV:
	case PMODEL_HLS:
	    if ( colrs->num_lists != 3 )
		err = ERR138;
	    else {
		for ( i = 0; i < colrs->num_lists; i++ )
		    for ( j = 0; j < colrs->lists[i].num_floats; j++ )
			if ( colrs->lists[i].floats[j] < 0.0
				|| colrs->lists[i].floats[j] > 1.0 )
			    return ERR136;
	    }
	    break;
	default:
	    err = ERR110;
	    break;
    }

    return err;
}


static int
colr_mapping_data_rec_valid( xdt, map_method, map_data )
    Wst_xwin_dt		*xdt;
    Pint		map_method;
    Pcolr_map_data	*map_data;
{
    Pint	err = 0;
    Pint	cm;

    switch ( map_method ) {
	case PCOLR_MAP_TRUE:
	    /* no data record contents */
	    break;

	case PCOLR_MAP_PSEUDO:
	    cm = map_data->meth_r2.colr_model;
	    if ( !phg_cb_int_in_list( cm, xdt->num_colour_approx_models,
		    xdt->colour_approx_models ) )
		err = ERR110;
	    else if ( (cm == PMODEL_RGB || cm == PMODEL_CIELUV || cm == PMODEL_HLS || cm == PMODEL_HSV)
		    && map_data->meth_r2.weights.num_floats != 3 )
		err = ERR138;
	    else if ( !phg_colours_valid( map_data->meth_r2.colrs.num_colr_reps,
		    map_data->meth_r2.colr_model,
		    (Pcolr_rep *)map_data->meth_r2.colrs.colr_reps ) )
		err = ERR136;
	    break;

	case PCOLR_MAP_PSEUDO_N:
	    if ( !phg_cb_int_in_list( map_data->meth_r3.colr_model,
		    xdt->num_colour_approx_models, 
		    xdt->colour_approx_models ) )
		err = ERR110;
	    else
		err = check_pseudo_N_colours( map_data->meth_r3.colr_model,
		    &map_data->meth_r3.colr_lists );
	    break;
    }

    if ( err ) {
	ERR_REPORT(phg_cur_cph->erh, err);
    }
    return (err == 0 ? 1 : 0);
}


/* SET COLOUR MAPPING REPRESENTATION */
void
pset_colr_map_rep( ws, index, map_method, map_data )
    Pint		ws;		/* workstation identifier	*/
    Pint		index;		/* depth cue bundle index	*/
    Pint		map_method;	/* mapping method */
    Pcolr_map_data	*map_data;	/* data record */
{
    Phg_args			cp_args;
    register Phg_args_set_rep	*args = &cp_args.data.set_rep;
    register Wst_phigs_dt	*dt;
    register Wst_xwin_dt	*xdt;
    Psl_ws_info			*wsinfo;

    if (CB_ENTRY_CHECK(phg_cur_cph, ERR3, Pfn_set_colr_map_rep)) {
    	if (PSL_WS_STATE(phg_cur_cph->psl) != PWS_ST_WSOP) {
	    ERR_REPORT(phg_cur_cph->erh, ERR3);
	    return;
	}
	else if (!(wsinfo = phg_psl_get_ws_info( phg_cur_cph->psl, ws))) {
	    ERR_REPORT(phg_cur_cph->erh, ERR54);
	    return;
	}

	dt = &((Wst*)wsinfo->wstype)->desc_tbl.phigs_dt;
	xdt = &((Wst*)wsinfo->wstype)->desc_tbl.xwin_dt;
	if ( !( dt->ws_category == PCAT_OUTIN || dt->ws_category == PCAT_OUT
	    || dt->ws_category == PCAT_MO) ) {
	    ERR_REPORT(phg_cur_cph->erh, ERR59);

	} else if ( index < 0 ) {
	    ERR_REPORT(phg_cur_cph->erh, ERR121);

	} else if ( !phg_cb_int_in_list( map_method,
	    dt->out_dt.num_colr_mapping_methods,
	    dt->out_dt.colr_mapping_methods ) ) {
	    ERR_REPORT(phg_cur_cph->erh, ERR126);
	
	} else if ( colr_mapping_data_rec_valid( xdt, map_method, map_data ) ) {
	    /* Error 125 is detected in the WS code. */
	    args->wsid = ws;
	    args->type = PHG_ARGS_COLRMAPREP;
	    args->rep.index = index;
	    args->rep.bundl.colrmaprep.method = map_method;
	    args->rep.bundl.colrmaprep.rec = *map_data;
	    CP_FUNC( phg_cur_cph, CP_FUNC_OP_SET_REP, &cp_args, NULL);
	}
    }
}


/* INQUIRE LIST OF COLOUR MAPPING INDICES */
void
pinq_list_colr_map_inds( ws, length, start, error_ind, indices, total_length)
    Pint	ws;		/* workstation identifier	*/
    Pint	length;		/* length of application list	*/
    Pint	start;		/* starting position	*/
    Pint	*error_ind;	/* OUT error indicator	*/
    Pint_list	*indices;	/* OUT list of view indices	*/
    Pint	*total_length;	/* OUT length of list in PHIGS	*/
{
    Phg_args		cp_args;
    Psl_ws_info		*wsinfo;

    if ( !CB_ENTRY_CHECK( phg_cur_cph, 0, Pfn_INQUIRY)) {
	*error_ind = ERR3;

    } else if ( PSL_WS_STATE( phg_cur_cph->psl) != PWS_ST_WSOP) {
	    *error_ind = ERR3;

    } else if ( !(wsinfo = phg_psl_get_ws_info( phg_cur_cph->psl, ws))) {
	*error_ind = ERR54;

    } else if ( ((Wst*)wsinfo->wstype)->desc_tbl.phigs_dt.ws_category == PCAT_MI) {
	*error_ind = ERR57;

    } else {
	    cp_args.data.q_indices.wsid = ws;
	    cp_args.data.q_indices.type = PHG_ARGS_COLRMAPREP;
	    cb_ws_inq_table_indices( &cp_args, length, start,
			    error_ind, indices, total_length);
    }
}


/* INQUIRE COLOUR MAPPING REPRESENTATION */
void
pinq_colr_map_rep( ws, index, type, store, error_ind, map_method, map_data)
    Pint	 	ws;		/* workstation identifier	*/
    Pint	 	index;		/* polyline index	*/
    Pinq_type	 	type;		/* type of returned value	*/
    Pstore		store;		/* store object */
    Pint		*error_ind;	/* OUT error indicator	*/
    Pint		*map_method;	/* OUT: mapping method */
    Pcolr_map_data	**map_data;	/* OUT: data record pointer */
{
    Phg_args		cp_args;
    Phg_ret		ret;
    Psl_ws_info		*wsinfo;
    Wst_phigs_dt	*dt;
    int			size = 0;

    if ( !CB_ENTRY_CHECK( phg_cur_cph, 0, Pfn_INQUIRY)) {
	*error_ind = ERR3;

    } else if ( PSL_WS_STATE( phg_cur_cph->psl) != PWS_ST_WSOP) {
	*error_ind = ERR3;

    } else if ( !(wsinfo = phg_psl_get_ws_info( phg_cur_cph->psl, ws))) {
	*error_ind = ERR54;
    } else {
	*error_ind = 0;
	dt = &((Wst*)wsinfo->wstype)->desc_tbl.phigs_dt;
	if ( !(dt->ws_category == PCAT_OUT || 
	       dt->ws_category == PCAT_OUTIN ||
	       dt->ws_category == PCAT_MO) ) {
	    *error_ind = ERR59;

	} else if ( index < 0 ) {
	    *error_ind = ERR121;
	}

	if ( !*error_ind ) {
	    cp_args.data.q_rep.wsid = ws;
	    cp_args.data.q_rep.index = index;
	    cp_args.data.q_rep.type = type;
	    cp_args.data.q_rep.rep_type = PHG_ARGS_COLRMAPREP;
	    CP_FUNC( phg_cur_cph, CP_FUNC_OP_INQ_REPRESENTATION, &cp_args,
			&ret);
	    if ( ret.err) {
		*error_ind = ret.err;
		return;
	    }

	    switch ( *map_method = ret.data.rep.colrmaprep.method ) {
		case PCOLR_MAP_TRUE:
		    /* no data record contents */
		    break;

		case PCOLR_MAP_PSEUDO:
		    size =
		       ret.data.rep.colrmaprep.rec.meth_r2.weights.num_floats
			    * sizeof(Pfloat)
			+ ret.data.rep.colrmaprep.rec.meth_r2.colrs.num_colr_reps
			    * sizeof(Pcolr_rep);
		    if ( CB_STORE_SPACE( ((_Pstore *)store), size, error_ind ) ) {
			*map_data = &((_Pstore *)store)->data.colr_map_rec;
			(*map_data)->meth_r2 =
			    ret.data.rep.colrmaprep.rec.meth_r2;
			(*map_data)->meth_r2.weights.floats =
			    (Pfloat *)((_Pstore *)store)->buf;
			(*map_data)->meth_r2.colrs.colr_reps = 
			    (Pcolr_rep *)
			    ((*map_data)->meth_r2.weights.floats +
				(*map_data)->meth_r2.weights.num_floats);
			bcopy(
			  (char *)ret.data.rep.colrmaprep.rec.meth_r2.weights.floats,
			    (char *)(*map_data)->meth_r2.weights.floats,
			    (*map_data)->meth_r2.weights.num_floats *
				sizeof(Pfloat) );
			bcopy(
			  (char *)ret.data.rep.colrmaprep.rec.meth_r2.colrs.colr_reps,
			    (char *)(*map_data)->meth_r2.colrs.colr_reps,
			    (*map_data)->meth_r2.colrs.num_colr_reps *
				sizeof(Pcolr_rep) );
		    }
		    break;

		case PCOLR_MAP_PSEUDO_N: {
		    Pfloat_list_list	*colrs;
		    Pfloat		*buf;
		    register int	i;

		    colrs = &ret.data.rep.colrmaprep.rec.meth_r3.colr_lists;
		    size = colrs->num_lists * sizeof(Pfloat_list);
		    for ( i = 0; i < colrs->num_lists; i++ )
			size += colrs->lists[i].num_floats * sizeof(Pfloat);
		    if ( CB_STORE_SPACE( ((_Pstore *)store), size, error_ind ) ) {
			*map_data = &((_Pstore *)store)->data.colr_map_rec;
			(*map_data)->meth_r3 =
			    ret.data.rep.colrmaprep.rec.meth_r3;
			(*map_data)->meth_r3.colr_lists.lists =
			    (Pfloat_list *)((_Pstore *)store)->buf;
			buf = (Pfloat *)
			    ((*map_data)->meth_r3.colr_lists.lists
				+ colrs->num_lists);
			for ( i = 0; i < colrs->num_lists; i++ ) {
			  (*map_data)->meth_r3.colr_lists.lists[i].num_floats
				= colrs->lists[i].num_floats;
			    (*map_data)->meth_r3.colr_lists.lists[i].floats
				= buf;
			    bcopy( (char *)colrs->lists[i].floats, (char *)buf,
				colrs->lists[i].num_floats * sizeof(Pfloat) );
			    buf += colrs->lists[i].num_floats;
			}
		    }
		} break;
	    }
	}
    }
}

/* INQUIRE COLOUR MAPPING STATE */
void
pinq_colr_map_st(ws, map_method, error_ind, map_st)
    Pint		ws;		/* workstation id	*/
    Pint		map_method;	/* mapping method	*/
    Pint		*error_ind;	/* OUT error indicator	*/
    Pcolr_map_st	*map_st;	/* OUT mapping state	*/
{
    Phg_args			cp_args;
    Phg_ret		        ret;
    register Wst_phigs_dt	*dt;
    Psl_ws_info			*wsinfo;

    if ( !CB_ENTRY_CHECK( phg_cur_cph, 0, Pfn_INQUIRY)) {
	*error_ind = ERR3;

    } else if ( PSL_WS_STATE( phg_cur_cph->psl) != PWS_ST_WSOP) {
	*error_ind = ERR3;

    } else if ( !(wsinfo = phg_psl_get_ws_info( phg_cur_cph->psl, ws))) {
	*error_ind = ERR54;
    } else {
	dt = &((Wst*)wsinfo->wstype)->desc_tbl.phigs_dt;
	if ( !(dt->ws_category == PCAT_OUT || 
	       dt->ws_category == PCAT_OUTIN ||
	       dt->ws_category == PCAT_MO) ) {
	    *error_ind = ERR59;

	} else if ( !phg_cb_int_in_list( map_method,
	    dt->out_dt.num_colr_mapping_methods,
	    dt->out_dt.colr_mapping_methods ) ) {
	    *error_ind = ERR126;
	
	} else {
	    *error_ind = 0;
            switch ( map_method ) {
                case PCOLR_MAP_TRUE:
		    map_st->int_data =
		      dt->out_dt.num_true_colours;
		break;
                case PCOLR_MAP_PSEUDO:
        	    cp_args.data.q_colr_map_meth_st.wsid = ws;
        	    cp_args.data.q_colr_map_meth_st.map_method = map_method;
        	    ret.err = 0;
        	    CP_FUNC( phg_cur_cph, CP_FUNC_OP_INQ_COLR_MAP_METH_ST, 
			     &cp_args, &ret);
        	    if ( ret.err) {
        		*error_ind = ret.err;
        	    } else 
                        map_st->int_data = ret.data.idata;
                break;
                case PCOLR_MAP_PSEUDO_N:
                    map_st->int_data = 0;
                break;
            }
	}
    }
}
