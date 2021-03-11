/* $XConsortium: cpx_ws.c,v 5.3 94/04/17 20:41:29 hersh Exp $ */

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

/* CPX WS functions used in both client and monitor processes. */

#include "phg.h"
#include "cp.h"
#include "ws.h"
#include "cp_priv.h"
#include "PEX.h"
#include "PEXproto.h"
#include "PEXmacs.h"
#include "PEXfuncs.h"
#include "phigspex.h"
#include "X11/Xatom.h"


/* open_ws is in cpx_clnt.c */
/* close_ws is in cpx_clnt.c */


void
phg_cpx_set_colour_model( cph, cp_args )
    Cp_handle	cph;
    Phg_args	*cp_args;
{
    Phg_args_set_colour_model	*args = &cp_args->data.set_colour_model;
    Ws				*ws;

    if ( ws = phg_cpx_ws_exists( cph, CPX_BY_WSID, (Cpx_css_srvr *)NULL,
	    args->wsid ) ) {
	if ( ws->css_srvr->set_colour_model )
	    (*ws->css_srvr->set_colour_model)( cph, cp_args, ws );
	else
	    ws->current_colour_model = args->model;
    }
}


void
phg_cpx_inq_colour_model( cph, cp_args, ret )
    Cp_handle	cph;
    Phg_args	*cp_args;
    Phg_ret	*ret;
{
    Ws		*ws;

    ret->err = 0;
    if ( ws = phg_cpx_ws_exists( cph, CPX_BY_WSID, (Cpx_css_srvr *)NULL,
	    cp_args->data.idata ) ) {
	if ( ws->css_srvr->inq_colour_model )
	    (*ws->css_srvr->inq_colour_model )( cph, cp_args, ret, ws );
	else
	    ret->data.idata = ws->current_colour_model;
    } else
	ret->err = ERR54;
}


void
phg_cpx_ws_update( cph, cp_args )
    Cp_handle   cph;
    Phg_args    *cp_args;
{
    Phg_args_ws_update	*args = &cp_args->data.ws_update;
    Ws			*ws;

    if ( ws = phg_cpx_ws_exists( cph, CPX_BY_WSID, (Cpx_css_srvr *)NULL,
	    args->wsid ) ) {
	(*ws->css_srvr->ws_update)( cph, cp_args, ws );
    }
}


void
phg_cpx_ws_redraw_all( cph, cp_args )
    Cp_handle   cph;
    Phg_args    *cp_args;
{
    Phg_args_ws_redraw_all	*args = &cp_args->data.ws_redraw_all;
    Ws				*ws;

    if ( ws = phg_cpx_ws_exists( cph, CPX_BY_WSID, (Cpx_css_srvr *)NULL,
	    args->wsid ) ) {
	(*ws->css_srvr->ws_redraw_all)( cph, cp_args, ws );
    }
}


void
phg_cpx_ws_set_disp_state( cph, cp_args )
    Cp_handle   cph;
    Phg_args    *cp_args;
{
    Phg_args_set_disp_state	*args = &cp_args->data.set_disp_state;
    Ws				*ws;

    if ( ws = phg_cpx_ws_exists( cph, CPX_BY_WSID, (Cpx_css_srvr *)NULL,
	    args->wsid ) ) {
	(*ws->css_srvr->set_disp_state)( cph, cp_args, ws );
    }
}


void
phg_cpx_inq_disp_update_state( cph, cp_args, ret )
    Cp_handle	cph;
    Phg_args	*cp_args;
    Phg_ret	*ret;
{
    Ws		*ws;

    ret->err = 0;
    if ( ws = phg_cpx_ws_exists( cph, CPX_BY_WSID, (Cpx_css_srvr *)NULL,
	    cp_args->data.idata ) )
	(*ws->css_srvr->inq_disp_update_state)( cph, cp_args, ret, ws );
    else
	ret->err = ERR54;
}


void
phg_cpx_post_struct( cph, cp_args )
    Cp_handle   cph;
    Phg_args    *cp_args;
{
    Phg_args_post_struct	*args = &cp_args->data.post_struct;
    Ws				*ws;

    if ( ws = phg_cpx_ws_exists( cph, CPX_BY_WSID, (Cpx_css_srvr *)NULL,
	    args->wsid ) )
	(*ws->css_srvr->post_struct)( cph, cp_args, ws );
}


void
phg_cpx_unpost_struct( cph, cp_args )
    Cp_handle   cph;
    Phg_args    *cp_args;
{
    Phg_args_unpost_struct	*args = &cp_args->data.unpost_struct;
    Ws				*ws;

    if ( ws = phg_cpx_ws_exists( cph, CPX_BY_WSID, (Cpx_css_srvr *)NULL,
	    args->wsid ) )
	(*ws->css_srvr->unpost_struct)( cph, cp_args, ws );
}


void
phg_cpx_unpost_all( cph, cp_args )
    Cp_handle   cph;
    Phg_args    *cp_args;
{
    Ws		*ws;

    if ( ws = phg_cpx_ws_exists( cph, CPX_BY_WSID, (Cpx_css_srvr *)NULL,
	    cp_args->data.idata ) )
	(*ws->css_srvr->unpost_all)( cph, cp_args, ws );
}


void
phg_cpx_set_rep( cph, cp_args )
    Cp_handle	cph;
    Phg_args	*cp_args;
{
    Ws		*ws;

    if ( ws = phg_cpx_ws_exists( cph, CPX_BY_WSID, (Cpx_css_srvr *)NULL,
	    cp_args->data.set_rep.wsid ) )
	(*ws->css_srvr->set_rep)( cph, cp_args, ws );
}


void
phg_cpx_inq_rep( cph, cp_args, ret )
    Cp_handle	cph;
    Phg_args	*cp_args;
    Phg_ret	*ret;
{
    Ws		*ws;

    ret->err = 0;
    if ( ws = phg_cpx_ws_exists( cph, CPX_BY_WSID, (Cpx_css_srvr *)NULL,
	    cp_args->data.q_rep.wsid ) )
	(*ws->css_srvr->inq_representation)( cph, cp_args, ret, ws );
    else
	ret->err = ERR54;
}


void
phg_cpx_inq_view_rep( cph, cp_args, ret )
    Cp_handle	cph;
    Phg_args	*cp_args;
    Phg_ret	*ret;
{
    Ws		*ws;

    ret->err = 0;
    if ( ws = phg_cpx_ws_exists( cph, CPX_BY_WSID, (Cpx_css_srvr *)NULL,
	    cp_args->data.q_view_rep.wsid ) )
	(*ws->css_srvr->inq_view_rep)( cph, cp_args, ret, ws );
    else
	ret->err = ERR54;
}


void
phg_cpx_set_hlhsr_mode( cph, cp_args )
    Cp_handle	cph;
    Phg_args	*cp_args;
{
    Ws		*ws;

    if ( ws = phg_cpx_ws_exists( cph, CPX_BY_WSID, (Cpx_css_srvr *)NULL,
	    cp_args->data.set_hlhsr_mode.wsid ) )
	(*ws->css_srvr->set_hlhsr_mode)( cph, cp_args, ws );
}


void
phg_cpx_set_ws_win( cph, cp_args )
    Cp_handle	cph;
    Phg_args	*cp_args;
{
    Ws		*ws;

    if ( ws = phg_cpx_ws_exists( cph, CPX_BY_WSID, (Cpx_css_srvr *)NULL,
	    cp_args->data.set_ws_winvp.wsid ) )
	(void)(*ws->css_srvr->set_ws_win)( cph, cp_args, ws );
}


void
phg_cpx_set_ws_vp( cph, cp_args )
    Cp_handle	cph;
    Phg_args	*cp_args;
{
    Ws		*ws;

    if ( ws = phg_cpx_ws_exists( cph, CPX_BY_WSID, (Cpx_css_srvr *)NULL,
	    cp_args->data.set_ws_winvp.wsid ) )
	(*ws->css_srvr->set_ws_vp)( cph, cp_args, ws );
}


void
phg_cpx_inq_ws_xform( cph, cp_args, ret )
    Cp_handle	cph;
    Phg_args	*cp_args;
    Phg_ret	*ret;
{
    Ws		*ws;

    ret->err = 0;
    if ( ws = phg_cpx_ws_exists( cph, CPX_BY_WSID, (Cpx_css_srvr *)NULL,
	    cp_args->data.idata ) )
	(*ws->css_srvr->inq_ws_xform)( cph, cp_args, ret, ws );
    else
	ret->err = ERR54;
}


void
phg_cpx_set_filter( cph, cp_args )
    Cp_handle	cph;
    Phg_args	*cp_args;
{
    Ws		*ws;

    if ( ws = phg_cpx_ws_exists( cph, CPX_BY_WSID, (Cpx_css_srvr *)NULL,
	    cp_args->data.set_filter.wsid ) )
	(*ws->css_srvr->set_filter)( cph, cp_args, ws );
}


void
phg_cpx_inq_filter( cph, cp_args, ret )
    Cp_handle	cph;
    Phg_args	*cp_args;
    Phg_ret	*ret;
{
    Ws		*ws;

    ret->err = 0;
    if ( ws = phg_cpx_ws_exists( cph, CPX_BY_WSID, (Cpx_css_srvr *)NULL,
	    cp_args->data.q_filter.wsid ) )
	(*ws->css_srvr->inq_filter)( cph, cp_args, ret, ws );
    else
	ret->err = ERR54;
}


void
phg_cpx_inq_posted( cph, cp_args, ret )
    Cp_handle	cph;
    Phg_args	*cp_args;
    Phg_ret	*ret;
{
    Ws		*ws;

    ret->err = 0;
    if ( ws = phg_cpx_ws_exists( cph, CPX_BY_WSID, (Cpx_css_srvr *)NULL,
	    cp_args->data.idata ) )
	(*ws->css_srvr->inq_posted)( cph, cp_args, ret, ws );
    else
	ret->err = ERR54;
}


void
phg_cpx_set_view_input_priority( cph, cp_args )
    Cp_handle	cph;
    Phg_args	*cp_args;
{
    Ws		*ws;

    if ( ws = phg_cpx_ws_exists( cph, CPX_BY_WSID, (Cpx_css_srvr *)NULL,
	    cp_args->data.set_view_input_prio.wsid ) )
	(*ws->css_srvr->set_view_input_prio)( cph, cp_args, ws );
}


void
phg_cpx_inq_indices( cph, cp_args, ret )
    Cp_handle	cph;
    Phg_args	*cp_args;
    Phg_ret	*ret;
{
    Ws		*ws;

    ret->err = 0;
    if ( ws = phg_cpx_ws_exists( cph, CPX_BY_WSID, (Cpx_css_srvr *)NULL,
	    cp_args->data.q_indices.wsid ) )
	(*ws->css_srvr->inq_indices)( cph, cp_args, ret, ws );
    else
	ret->err = ERR54;
}


void
phg_cpx_inq_hlhsr_mode( cph, cp_args, ret )
    Cp_handle	cph;
    Phg_args	*cp_args;
    Phg_ret	*ret;
{
    Ws		*ws;

    if ( ws = phg_cpx_ws_exists( cph, CPX_BY_WSID, (Cpx_css_srvr *)NULL,
	    cp_args->data.idata ) )
	(*ws->css_srvr->inq_hlhsr_mode)( cph, cp_args, ret, ws );
    else
	ret->err = ERR54;
}


void
phg_cpx_inq_win_info( cph, cp_args, ret )
    Cp_handle	cph;
    Phg_args	*cp_args;
    Phg_ret	*ret;
{
    Ws			*ws;

    if ( ws = phg_cpx_ws_exists( cph, CPX_BY_WSID, (Cpx_css_srvr *)NULL,
	    cp_args->data.idata ) ) {
	(*ws->css_srvr->inq_win_info)( cph, cp_args, ret, ws );
    } else
	ret->err = ERR54;
}


void
phg_cpx_inq_dpy_and_drawable( cph, cp_args, ret )
    Cp_handle	cph;
    Phg_args	*cp_args;
    Phg_ret	*ret;
{
    Ws		*ws;
    
    if ( ws = phg_cpx_ws_exists( cph, CPX_BY_WSID, (Cpx_css_srvr *)NULL,
	    cp_args->data.idata)) {
	ret->err = 0;
	ret->data.dpy_and_drawable.display     = ws->display;
	ret->data.dpy_and_drawable.drawable_id = ws->drawable_id;
	ret->data.dpy_and_drawable.overlay_id = ws->input_overlay_window;
	ret->data.dpy_and_drawable.display_name =
	    ws->type->desc_tbl.xwin_dt.display_name;
    } else
	ret->err = ERR54;
}


void
phg_cpx_message( cph, cp_args )
    Cp_handle	cph;
    Phg_args	*cp_args;
{
    Ws		*ws;
    
    if ( ws = phg_cpx_ws_exists( cph, CPX_BY_WSID, (Cpx_css_srvr *)NULL,
	    cp_args->data.message.wsid )) {
	if ( ws->css_srvr->message )
	    (*ws->css_srvr->message )( cph, cp_args, ws );
    }
}


void
phg_cpx_inp_init_dev( cph, cp_args )
    Cp_handle	cph;
    Phg_args	*cp_args;
{
    Ws		*ws;
    
    if ( ws = phg_cpx_ws_exists( cph, CPX_BY_WSID, (Cpx_css_srvr *)NULL,
	    cp_args->data.inp_init_dev.wsid )) {
	if ( ws->css_srvr->inp_init_dev)
	    (*ws->css_srvr->inp_init_dev)( cph, cp_args, ws );
    }
}


void
phg_cpx_inp_set_mode( cph, cp_args )
    Cp_handle	cph;
    Phg_args	*cp_args;
{
    Ws		*ws;
    
    if ( ws = phg_cpx_ws_exists( cph, CPX_BY_WSID, (Cpx_css_srvr *)NULL,
	    cp_args->data.inp_set_mode.wsid )) {
	if ( ws->css_srvr->inp_set_mode )
	    (*ws->css_srvr->inp_set_mode )( cph, cp_args, ws );
    }
}


void
phg_cpx_inp_request( cph, cp_args, ret )
    Cp_handle	cph;
    Phg_args	*cp_args;
    Phg_ret	*ret;
{
    Ws		*ws;
    
    ret->err = 0;
    if ( ws = phg_cpx_ws_exists( cph, CPX_BY_WSID, (Cpx_css_srvr *)NULL,
	    cp_args->data.inp_request.wsid )) {
	if ( ws->css_srvr->inp_request )
	    (*ws->css_srvr->inp_request )( cph, cp_args, ret, ws );
    }
}


void
phg_cpx_inp_sample( cph, cp_args, ret )
    Cp_handle	cph;
    Phg_args	*cp_args;
    Phg_ret	*ret;
{
    Ws		*ws;
    
    ret->err = 0;
    if ( ws = phg_cpx_ws_exists( cph, CPX_BY_WSID, (Cpx_css_srvr *)NULL,
	    cp_args->data.inp_sample.wsid )) {
	if ( ws->css_srvr->inp_sample )
	    (*ws->css_srvr->inp_sample )( cph, cp_args, ret, ws );
    }
}


void
phg_cpx_inq_inp_dev_state( cph, cp_args, ret )
    Cp_handle	cph;
    Phg_args	*cp_args;
    Phg_ret	*ret;
{
    Ws		*ws;
    
    ret->err = 0;
    if ( ws = phg_cpx_ws_exists( cph, CPX_BY_WSID, (Cpx_css_srvr *)NULL,
	    cp_args->data.q_inp_state.wsid )) {
	if ( ws->css_srvr->inq_inp_dev_state )
	    (*ws->css_srvr->inq_inp_dev_state )( cph, cp_args, ret, ws );
    }
}


void
phg_cpx_inq_text_extent( cph, cp_args, ret )
    Cp_handle		cph;
    Phg_args		*cp_args;
    Phg_ret		*ret;
{
    Ws		*ws;
    
    ret->err = 0;
    if ( ws = phg_cpx_ws_exists( cph, CPX_BY_WSID, (Cpx_css_srvr *)NULL,
	    (int)cp_args->data.q_text_extent.wsid )) {
	(*ws->css_srvr->inq_text_extent )( cph, cp_args, ret, ws );
    }
}


void
phg_cpx_ws_drawable_pick( cph, cp_args, ret )
    Cp_handle		cph;
    Phg_args		*cp_args;
    Phg_ret		*ret;
{
    Ws		*ws;
    
    ret->err = 0;
    if ( ws = phg_cpx_ws_exists( cph, CPX_BY_WSID, (Cpx_css_srvr *)NULL,
	    cp_args->data.drawable_pick.wsid )) {
	(*ws->css_srvr->drawable_pick )( cph, cp_args, ret, ws );
    }
}


void
phg_cpx_ws_map_points( cph, cp_args, ret )
    Cp_handle		cph;
    Phg_args		*cp_args;
    Phg_ret		*ret;
{
    Ws		*ws;
    
    ret->err = 0;
    if ( ws = phg_cpx_ws_exists( cph, CPX_BY_WSID, (Cpx_css_srvr *)NULL,
	    cp_args->data.map_points.wsid )) {
	(*ws->css_srvr->map_points )( cph, cp_args, ret, ws );
    }
}

void
phg_cpx_ws_redraw_regions( cph, cp_args )
    Cp_handle		cph;
    Phg_args		*cp_args;
{
    Ws		*ws;
    
    if ( ws = phg_cpx_ws_exists( cph, CPX_BY_WSID, (Cpx_css_srvr *)NULL,
	    cp_args->data.redraw_regions.wsid )) {
	(*ws->css_srvr->redraw_regions)( cph, cp_args, ws );
    }
}

void
phg_cpx_ws_synch( cph, cp_args )
    Cp_handle		cph;
    Phg_args		*cp_args;
{
    Ws		*ws;
    
    if ( ws = phg_cpx_ws_exists( cph, CPX_BY_WSID, (Cpx_css_srvr *)NULL,
	    cp_args->data.idata )) {
	if ( ws->css_srvr->ws_synch )
	    (*ws->css_srvr->ws_synch)( cph, cp_args, ws );
	XSync( ws->display, False );
    }
}

void
phg_cpx_inq_colr_map_meth_st( cph, cp_args, ret )
    Cp_handle	cph;
    Phg_args	*cp_args;
    Phg_ret	*ret;
{
    Ws		*ws;

    ret->err = 0;
    if ( ws = phg_cpx_ws_exists( cph, CPX_BY_WSID, (Cpx_css_srvr *)NULL,
	    cp_args->data.q_colr_map_meth_st.wsid ) )
	(*ws->css_srvr->inq_colr_map_meth_st)( cph, cp_args, ret, ws );
    else
	ret->err = ERR54;
}
