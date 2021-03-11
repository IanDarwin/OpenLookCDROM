/* $XConsortium: cpx.h,v 5.2 94/04/17 20:41:44 rws Exp $ */

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

#ifndef PHG_CPX_H_INCLUDED
#define PHG_CPX_H_INCLUDED

#include "cpa.h"
#include "cpb.h"

typedef struct Cpx_css_srvr {
    Cpx_css_srvr_type	type;
    Display		*display;	/* NULL on CLNT_SS servers */
    Phg_pex_ext_info	pex_info;	/* not used on CLNT_SS servers */
    struct {
	unsigned master: 1;
	unsigned uses_monitor_css: 1;
    }			flags;
    union {
	Cp_a	a;	/* server support of workstations and structures */
	Cp_b	b;	/* client side SS in this process (PM or client) */
    }				model;
    struct Cpx_css_srvr	*next;

    Ws_handle		(*open_ws)();
    void		(*close_ws)();
    void		(*ws_redraw_all)();
    void		(*ws_update)();
    void		(*set_disp_state)();
    void		(*message)();
    void		(*set_rep)();
    void		(*set_filter)();
    void		(*set_colour_model)();
    void		(*set_hlhsr_mode)();
    void		(*set_view_input_prio)();
    void		(*set_ws_win)();
    void		(*set_ws_vp)();
    void		(*add_el)();
    void		(*copy_all_els)();
    void		(*open_struct)();
    void		(*close_struct)();
    void		(*set_el_ptr)();
    void		(*set_edit_mode)();
    void		(*delete_el)();
    void		(*delete_struct)();
    void		(*delete_struct_net)();
    void		(*delete_all_structs)();
    void		(*change_struct_id)();
    void		(*change_struct_refs)();
    void		(*change_struct_idrefs)();
    void		(*post_struct)();
    void		(*unpost_struct)();
    void		(*unpost_all)();
    void		(*ar_archive)();
    void		(*inp_init_dev)();
    void		(*inp_set_mode)();
    void		(*inp_request)();
    void		(*inp_sample)();
    void		(*set_err_hand_mode)();
    void		(*el_search)();
    void		(*inc_spa_search)();

    /* inquiry functions */
    void		(*inq_colour_model)();
    void		(*inq_struct_status)();
    void		(*inq_struct_ids)();
    void		(*inq_el_ptr)();
    void		(*inq_el_type_size)();
    void		(*inq_el_content)();
    void		(*inq_indices)();
    void		(*inq_filter)();
    void		(*inq_posted)();
    void		(*inq_inp_dev_state)();
    void		(*inq_wss_posted_to)();
    void		(*inq_hierarchy)();
    void		(*inq_representation)();
    void		(*inq_view_rep)();
    void		(*inq_hlhsr_mode)();
    void		(*inq_disp_update_state)();
    void		(*inq_ws_xform)();
    void		(*inq_text_extent)();
    void		(*inq_colr_map_meth_st)();

    void		(*close_phigs)();
    
    int			(*struct_exists)();

    /* Other non-PHIGS functions */
    void		(*destroy)();
    int			(*full_copy_to_type_a)();
    int			(*full_copy_to_type_b)();
    void		(*inq_win_info)();
    void		(*inq_dpy_and_drawable)();
    void		(*drawable_pick)();
    void		(*map_points)();
    void		(*redraw_regions)();
    void		(*ws_synch)();
} Cpx_css_srvr;


/* List handling utilities. */
#define CPX_ADD_TO_LIST( _t, _l, _i ) \
    {register _t **_p; for ( _p = &(_l); *_p; _p = &(*_p)->next ); *_p=(_i);}

#define CPX_FOR_ALL_DISPLAYS(_c,_s) \
    for ( (_s) = (_c)->displays; (_s); (_s) = (_s)->next )

#define CPX_FOR_ALL_SERVERS(_c,_s) \
    for ( (_s) = (_c)->css_srvr_list; (_s); (_s) = (_s)->next )

#define CPX_FOR_ALL_SS_SERVERS(_c,_s) \
    CPX_FOR_ALL_SERVERS(_c,_s) { \
	if ( (_s)->type == CP_PEX_SRVR_SS )
#define CPX_END_FOR_ALL_SS_SERVERS	}

#define CPX_MASTER_SERVER( _c, _ms ) \
    { \
	CPX_FOR_ALL_SERVERS(_c,_ms) \
	    if ( (_ms)->flags.master ) break; \
    }

#define CPX_FOR_ALL_WS(_c,_w) \
    for ( (_w) = (_c)->ws_list; (_w); (_w) = (_w)->next )

#define CPX_WSID_FROM_DRAWABLE(_cph, _draw_id, _ws_id)		\
    {								\
	register Ws *_wsp;				\
	for (_wsp = (_cph)->ws_list; _wsp; _wsp = _wsp->next)	\
	    if ( _wsp->drawable_id == (_draw_id) )			\
		break;						\
	(_ws_id) = (_wsp ? _wsp->wsrid : NULL);		\
    }

#define PADDING(n) ( ((n)%4 == 0) ? 0 : 4 - ((n)%4) )

#define CPX_BY_SID	1
#define CPX_BY_WSID	1
#define CPX_BY_XID	2
#define CPX_BY_DISPLAY	3
#define CPX_BY_NAME	4
#define CPX_BY_SS_TYPE	5

/* CP vector functions */
extern void
    /* Used by client side only, in alphabetic order. */
    phg_cpxc_add_el(),
    phg_cpxc_close_struct(),
    phg_cpxc_change_struct_id(),
    phg_cpxc_close(),
    phg_cpxc_close_ws(),
    phg_cpxc_copy_all_els(),
    phg_cpxc_delete_all_structs(),
    phg_cpxc_delete_el(),
    phg_cpxc_delete_struct(),
    phg_cpxc_delete_struct_net(),
    phg_cpxc_emerg_close(),
    phg_cpxc_error_sync(),
    phg_cpxc_inq_wss_posted_to(),
    phg_cpxc_open_struct(),
    phg_cpxc_open_ws(),
    phg_cpxc_set_edit_mode(),
    phg_cpxc_set_el_ptr(),

    /* Used by monitor only, in alphabetic order. */
    phg_cpm_open_ws(),
    phg_cpm_close_ws(),
    phg_cpm_open_struct(),
    phg_cpm_add_el(),
    phg_cpm_change_struct_id(),
    phg_cpm_change_struct_refs(),
    phg_cpm_change_struct_idrefs(),
    phg_cpm_close_struct(),
    phg_cpm_copy_all_els(),
    phg_cpm_delete_all_structs(),
    phg_cpm_delete_el(),
    phg_cpm_delete_struct(),
    phg_cpm_delete_struct_net(),
    phg_cpm_inq_wss_posted_to(),
    phg_cpm_set_edit_mode(),
    phg_cpm_set_el_ptr(),

    /* Used in both client and monitor, in alphabetic order. */
    phg_cpx_ar_archive(),
    phg_cpx_ar_close(),
    phg_cpx_ar_delete(),
    phg_cpx_ar_get_hierarchy(),
    phg_cpx_ar_get_names(),
    phg_cpx_ar_open(),
    phg_cpx_ar_retrieve(),
    phg_cpx_change_struct_refs(),
    phg_cpx_change_struct_idrefs(),
    phg_cpx_dummy_await_event(),
    phg_cpx_dummy_flush_dev(),
    phg_cpx_dummy_inq_overflow(),
    phg_cpx_dummy_inq_more_events(),
    phg_cpx_el_search(),
    phg_cpx_inc_spa_search(),
    phg_cpx_inp_init_dev(),
    phg_cpx_inp_request(),
    phg_cpx_inp_sample(),
    phg_cpx_inp_set_mode(),
    phg_cpx_inq_ar_conflicting(),
    phg_cpx_inq_colour_model(),
    phg_cpx_inq_colr_map_meth_st(),
    phg_cpx_inq_disp_update_state(),
    phg_cpx_inq_dpy_and_drawable(),
    phg_cpx_inq_el_content(),
    phg_cpx_inq_el_ptr(),
    phg_cpx_inq_el_type_size(),
    phg_cpx_inq_filter(),
    phg_cpx_inq_hierarchy(),
    phg_cpx_inq_hlhsr_mode(),
    phg_cpx_inq_indices(),
    phg_cpx_inq_inp_dev_state(),
    phg_cpx_inq_posted(),
    phg_cpx_inq_rep(),
    phg_cpx_inq_struct_ids(),
    phg_cpx_inq_struct_status(),
    phg_cpx_inq_text_extent(),
    phg_cpx_inq_view_rep(),
    phg_cpx_inq_win_info(),
    phg_cpx_inq_ws_xform(),
    phg_cpx_message(),
    phg_cpx_post_struct(),
    phg_cpx_set_colour_model(),
    phg_cpx_set_err_hand_mode(),
    phg_cpx_set_filter(),
    phg_cpx_set_hlhsr_mode(),
    phg_cpx_set_rep(),
    phg_cpx_set_view_input_priority(),
    phg_cpx_set_ws_win(),
    phg_cpx_set_ws_vp(),
    phg_cpx_unpost_all(),
    phg_cpx_unpost_struct(),
    phg_cpx_ws_drawable_pick(),
    phg_cpx_ws_map_points(),
    phg_cpx_ws_redraw_all(),
    phg_cpx_ws_redraw_regions(),
    phg_cpx_ws_set_disp_state(),
    phg_cpx_ws_synch(),
    phg_cpx_ws_update()
    ;

/* Startup functions. */
extern Cpx_css_srvr*	phg_cpa_init();
extern Cpx_css_srvr*	phg_cpa_pm_init();
extern Cpx_css_srvr*	phg_cpb_init();
extern Cpx_css_srvr*	phg_cpbc_init();
extern Cpx_css_srvr*	phg_cpb_pm_init();
extern Cpx_css_srvr*	phg_cpb_create();

/* Connection utilities. */
extern void		phg_cpx_link_css_srvr();
extern void		phg_cpx_unlink_css_srvr();
extern void		phg_cpx_sync_servers();
extern void		phg_cpx_destroy_all_css_servers();
extern void		phg_cpx_unlink_ws();
extern int		phg_cpx_full_copy_a_to_a();
extern int		phg_cpx_full_copy_a_to_b();
extern int		phg_cpx_full_copy_b_to_a();
extern Display*		phg_cpx_connection_exists();
extern void		phg_cpx_instance_connection();
extern void		phg_cpx_release_connection();

extern void		phg_cpx_disconnect_all_servers();
extern Cpx_css_srvr*	phg_cpx_css_srvr_exists();
extern void		phg_cpx_release_css_srvr();

/* Toolkit handlers. */
extern Widget      	phg_cpm_toolkit_add_connection();
extern int      	phg_cpm_toolkit_open_ws();
extern void     	phg_cpm_toolkit_close_ws();

/* Ws utilities */
extern Ws_handle	phg_cpx_ws_exists();
extern int		phg_cpx_load_structures();
extern void             phg_cpx_get_win_info();

/* Other. */
extern Cp_handle	phg_cpxc_open();
extern int		phg_cpx_full_copy_a_to_a();
extern int		phg_cpx_el_data_to_pex();

#endif
