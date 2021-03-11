/* $XConsortium: cpb_css.c,v 5.2 94/04/17 20:41:19 rws Exp $ */

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
#include "css.h"
#include "ws.h"
#include "PEX.h"
#include "PEXmacs.h"
#include "PEXfuncs.h"
#include "PEXproto.h"
#include "phigspex.h"


void
phg_cpb_add_el( cph, cp_args, css_srvr )
    Cp_handle		cph;
    Phg_args		*cp_args;
    Cpx_css_srvr	*css_srvr;
{
    Phg_args_add_el	*args = &cp_args->data.add_el;
    Css_handle		cssh = css_srvr->model.b.cssh;

    register Css_ws_list	ws_list, wsptr;

    ws_list = CSS_GET_WS_ON( CSS_CUR_STRUCTP(cssh) );
    if ( ws_list && CSS_EDIT_MODE(cssh) == PEDIT_REPLACE) {
	/* Tell workstations that an element is being replaced. */
	for ( wsptr = ws_list; wsptr->wsh; wsptr++ )
	    if ( wsptr->wsh->delete_el_for_repl )
		(*wsptr->wsh->delete_el_for_repl)( wsptr->wsh );
    }
    /* Note: If phg_css_add_elem fails, function not "ignored": WS might
     * have undrawn the existing element.
     */

    if ( phg_css_add_elem( cssh, args ) ) {
	/* Tell workstations that an element has been added. */
	if ( ws_list) {
	    for ( ; ws_list->wsh; ws_list++ )
		(*ws_list->wsh->add_el)( ws_list->wsh );
	}
    }
}


void
phg_cpb_inq_el_type_size( cph, cp_args, ret, css_srvr )
    Cp_handle		cph;
    Phg_args		*cp_args;
    Phg_ret		*ret;
    Cpx_css_srvr	*css_srvr;
{
    phg_css_inq_el_type_size( css_srvr->model.b.cssh,
	cp_args->data.q_el_data.struct_id, cp_args->data.q_el_data.el_id,
	ret );
}


void
phg_cpb_inq_el_content( cph, cp_args, ret, css_srvr )
    Cp_handle		cph;
    Phg_args		*cp_args;
    Phg_ret		*ret;
    Cpx_css_srvr	*css_srvr;
{
    phg_css_inq_el_content( css_srvr->model.b.cssh,
	cp_args->data.q_el_data.struct_id, cp_args->data.q_el_data.el_id,
	ret );
}


void
phg_cpb_open_struct( cph, cp_args, ret, css_srvr )
    Cp_handle		cph;
    Phg_args		*cp_args;
    Phg_ret		*ret;
    Cpx_css_srvr	*css_srvr;
{
    if ( phg_css_open_struct( css_srvr->model.b.cssh, cp_args->data.idata ) )
	ret->err = 0;
    else
	ret->err = !0;
}


void
phg_cpb_close_struct( cph, cp_args, css_srvr )
    Cp_handle		cph;
    Phg_args		*cp_args;
    Cpx_css_srvr	*css_srvr;
{
    Struct_handle	str;

    register Css_ws_list	ws_list;

    ws_list = CSS_GET_WS_ON( CSS_CUR_STRUCTP(css_srvr->model.b.cssh) );
    str = phg_css_close_struct( css_srvr->model.b.cssh );
    if ( ws_list && str ) {
	for ( ; ws_list->wsh; ws_list++ )
	    if ( ws_list->wsh->close_struct )
		(*ws_list->wsh->close_struct)( ws_list->wsh, str );
    }
}


void
phg_cpb_delete_all_structs( cph, cp_args, css_srvr )
    Cp_handle		cph;
    Phg_args		*cp_args;
    Cpx_css_srvr	*css_srvr;
{
    register Ws_handle		wsh;

    CPX_FOR_ALL_WS( cph, wsh ) {
	if ( wsh->css_srvr == css_srvr )
	    (*wsh->delete_all_structs)( wsh );	
    }
    phg_css_delete_all_structs( css_srvr->model.b.cssh );
}


void
phg_cpb_delete_struct( cph, cp_args, css_srvr )
    Cp_handle		cph;
    Phg_args		*cp_args;
    Cpx_css_srvr	*css_srvr;
{
    Ws_handle		callback_list[MAX_NO_OPEN_WS];
    Css_handle		cssh = css_srvr->model.b.cssh;

    register Struct_handle	str;
    register Css_ws_list	ws_list;
    register Ws_handle 		*wsp = callback_list;

    if ( str = CSS_STRUCT_EXISTS( cssh, cp_args->data.idata ) ) {
	ws_list = CSS_GET_WS_ON( str );
	if ( ws_list ) {
	    for ( ; ws_list->wsh; ws_list++ ) {
		if ( (*ws_list->wsh->delete_struct)( ws_list->wsh, str,
			WS_PRE_CSS_DELETE ) )
		    /* add to the callback list */
		    *wsp++ = ws_list->wsh;
	    }
	}

	phg_css_delete_struct( cssh, str );

	/* Call the workstations on the callback list. */
	while ( wsp-- != callback_list )
	    (*(*wsp)->delete_struct)( *wsp, str, WS_POST_CSS_DELETE );
    }
}


void
phg_cpb_delete_struct_net( cph, cp_args, css_srvr )
    register Cp_handle		cph;
    Phg_args			*cp_args;
    Cpx_css_srvr		*css_srvr;
{
    Css_handle			cssh = css_srvr->model.b.cssh;
    Phg_args_del_struct_net	*args = &cp_args->data.del_struct_net;
    Ws_handle		 	callback_list[ MAX_NO_OPEN_WS ];

    register Struct_handle	str;
    register Css_ws_list	ws_list;
    register Ws_handle		*wsp = callback_list;

    if ( str = CSS_STRUCT_EXISTS( cssh, args->id ) ) {
	ws_list = CSS_GET_WS_ON( str );
	if ( ws_list ) {
	    for ( ; ws_list->wsh; ws_list++ ) {
		if( (*ws_list->wsh->delete_struct_net)( ws_list->wsh, str,
			args->flag, WS_PRE_CSS_DELETE ) )
		    /* add to the callback list */
		    *wsp++ = ws_list->wsh;
	    }
	}

	phg_css_delete_net( cssh, str, args->flag );

	/* Call the workstations on the callback list. */
	while ( wsp-- != callback_list )
	    (void)(*(*wsp)->delete_struct_net)( (*wsp), str, args->flag,
		WS_POST_CSS_DELETE);
    }
}


void
phg_cpb_set_edit_mode( cph, cp_args, css_srvr )
    register Cp_handle		cph;
    Phg_args			*cp_args;
    Cpx_css_srvr		*css_srvr;
{
    CSS_SET_EDIT_MODE( css_srvr->model.b.cssh, (Pedit_mode)cp_args->data.idata);
}


void
phg_cpb_set_el_ptr( cph, cp_args, css_srvr )
    register Cp_handle		cph;
    Phg_args			*cp_args;
    Cpx_css_srvr		*css_srvr;
{
    El_handle		ep;
    Phg_args_set_el_ptr	*args = &cp_args->data.set_el_ptr;

    register Css_ws_list	ws_list;

    ws_list = CSS_GET_WS_ON( CSS_CUR_STRUCTP( css_srvr->model.b.cssh ) );
    ep = phg_css_set_ep( css_srvr->model.b.cssh, args->op, args->data );
    if ( ws_list && ep ) {
	for ( ; ws_list->wsh; ws_list++ ) {
	    if ( ws_list->wsh->move_ep )
		(*ws_list->wsh->move_ep)( ws_list->wsh, ep );
	}
    }
}


void
phg_cpb_copy_all_els( cph, cp_args, css_srvr )
    register Cp_handle		cph;
    Phg_args			*cp_args;
    Cpx_css_srvr		*css_srvr;
{
    El_handle		ep;
    Struct_handle	str;
    Css_handle		cssh = css_srvr->model.b.cssh;

    register Css_ws_list	ws_list;

    if ( str = CSS_STRUCT_EXISTS( cssh, cp_args->data.idata ) ) {
	ws_list = CSS_GET_WS_ON( CSS_CUR_STRUCTP( cssh ) );
	/* Get the element pointer before it changes. */
	ep = CSS_CUR_ELP( cssh );
	if ( phg_css_copy_struct( cssh, str ) && ep && ws_list ) {
	    for ( ; ws_list->wsh; ws_list++ )
		(*ws_list->wsh->copy_struct)( ws_list->wsh, ep );
	}
    }
}


void
phg_cpb_delete_el( cph, cp_args, css_srvr )
    register Cp_handle		cph;
    Phg_args			*cp_args;
    Cpx_css_srvr		*css_srvr;
{
    Phg_args_del_el	*args = &cp_args->data.del_el;
    El_handle		ep1, ep2;
    Ws_handle	 	callback_list[MAX_NO_OPEN_WS];
    Struct_handle	structh;

    Css_ws_list		ws_list;
    Ws_handle		*wsp = callback_list;
    Css_handle		cssh = css_srvr->model.b.cssh;

    structh = (args->op == PHG_ARGS_EMPTY_STRUCT) ? 
	CSS_STRUCT_EXISTS( cssh, args->data.struct_id )
	    : CSS_CUR_STRUCTP( cssh );
    ws_list = CSS_GET_WS_ON( structh );
    phg_css_el_delete_list( cssh, args->op, &args->data, &ep1, &ep2 );
    if ( ep1 && ep2 ) {
	if ( ws_list ) {
	    for ( ; ws_list->wsh; ws_list++ ) {
		if ( (*ws_list->wsh->delete_el)(ws_list->wsh, structh, 
			ep1, ep2, WS_PRE_CSS_DELETE ) )
		    *wsp++ = ws_list->wsh;
	    }
	}
/* clh */
    phg_css_delete_el( cssh, args->op, &args->data, ep1, ep2 );
/* clh */

	while ( wsp-- != callback_list ) {
	    (void)(*(*wsp)->delete_el)((*wsp), structh, ep1, ep2, 
		WS_POST_CSS_DELETE );
	}
    }
}


void
phg_cpb_inq_el_ptr( cph, cp_args, ret, css_srvr )
    Cp_handle		cph;
    Phg_args		*cp_args;	/* should be NULL */
    Phg_ret		*ret;
    Cpx_css_srvr	*css_srvr;
{
    ret->data.idata = CSS_INQ_EL_INDEX( css_srvr->model.b.cssh );
}


void
phg_cpb_inq_struct_status( cph, cp_args, ret, css_srvr )
    Cp_handle		cph;
    Phg_args		*cp_args;
    Phg_ret		*ret;
    Cpx_css_srvr		*css_srvr;
{
    phg_css_inq_struct_status( css_srvr->model.b.cssh,
	cp_args->data.idata, ret);
}


void
phg_cpb_inq_struct_ids( cph, cp_args, ret, css_srvr )
    Cp_handle		cph;
    Phg_args		*cp_args;	/* should be NULL */
    Phg_ret		*ret;
    Cpx_css_srvr	*css_srvr;
{
    phg_css_inq_struct_ids( css_srvr->model.b.cssh, ret );
}


void
phg_cpb_el_search( cph, cp_args, ret, css_srvr )
    Cp_handle		cph;
    Phg_args		*cp_args;
    Phg_ret		*ret;
    Cpx_css_srvr	*css_srvr;
{
    Phg_args_el_search	*args = &cp_args->data.el_search;

    phg_css_el_search( css_srvr->model.b.cssh, args->struct_id,
	args->start_el, args->dir, &args->incl, &args->excl, ret );
}


void
phg_cpb_inq_wss_posted_to( cph, cp_args, ret, css_srvr )
    Cp_handle		cph;
    Phg_args		*cp_args;
    Phg_ret		*ret;
    Cpx_css_srvr	*css_srvr;
{
    phg_css_inq_ws_posted( css_srvr->model.b.cssh, cp_args->data.idata, ret);
}


void
phg_cpb_inq_hierarchy( cph, cp_args, ret, css_srvr )
    Cp_handle		cph;
    Phg_args		*cp_args;
    Phg_ret		*ret;
    Cpx_css_srvr	*css_srvr;
{
    Phg_args_q_hierarchy	*hier = &cp_args->data.q_hierarchy;

    phg_css_inq_hierarchy( css_srvr->model.b.cssh, hier->dir,
	hier->struct_id, hier->order, hier->depth, ret );
}


static void
cpb_change_struct_id_or_ref( cssh, cp_args, func )
    Css_handle		cssh;
    Phg_args		*cp_args;
    Css_ws_list		(*func)();
{
    Struct_handle		orig, new;
    Phg_args_change_struct	*args = &cp_args->data.change_struct;

    register Css_ws_list	ws_list;

    orig = CSS_STRUCT_EXISTS( cssh, args->orig_id );
    new = CSS_STRUCT_EXISTS( cssh, args->new_id );
    if ( !(orig && new && orig == new) ) {
	ws_list = (*func)( cssh, args, orig, new, args->posted );
	if ( ws_list )
	    for ( ; ws_list->wsh; ws_list++ )
		(*ws_list->wsh->conditional_redraw)( ws_list->wsh );
    }
}

void
phg_cpb_change_struct_id( cph, cp_args, css_srvr )
    Cp_handle		cph;
    Phg_args		*cp_args;
    Cpx_css_srvr	*css_srvr;
{
    cpb_change_struct_id_or_ref( css_srvr->model.b.cssh, cp_args,
	phg_css_change_struct_id );
}


void
phg_cpb_change_struct_refs( cph, cp_args, css_srvr )
    Cp_handle		cph;
    Phg_args		*cp_args;
    Cpx_css_srvr	*css_srvr;
{
    cpb_change_struct_id_or_ref( css_srvr->model.b.cssh, cp_args,
	phg_css_change_struct_refs );
}


void
phg_cpb_change_struct_idrefs( cph, cp_args, css_srvr )
    Cp_handle		cph;
    Phg_args		*cp_args;
    Cpx_css_srvr	*css_srvr;
{
    cpb_change_struct_id_or_ref( css_srvr->model.b.cssh, cp_args,
	phg_css_change_struct_idrefs );
}


void
/* TODO: */ phg_cpb_inc_spa_search( cph, cp_args, ret, css_srvr )
    Cp_handle		cph;
    Phg_args		*cp_args;
    Phg_ret		*ret;
    Cpx_css_srvr	*css_srvr;
{
    ret->err = ERRN500;
}
