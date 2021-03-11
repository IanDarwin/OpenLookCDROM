/* $XConsortium: cpa.h,v 5.3 94/04/17 20:41:40 rws Exp $ */

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

#ifndef PHG_CPA_H_INCLUDED
#define PHG_CPA_H_INCLUDED

/* Types for PHIGS/PEX CPA module. */

#ifdef DEBUG
#define CPA_STRUCT_HASH_SIZE	10
#else
#define CPA_STRUCT_HASH_SIZE	100
#endif

/* This value is the base structure id used in the encoding of the PEX
 * structure ids.  It is added to the application's structure ids during
 * the encoding.  This leaves the values below it available for ids not
 * associated with structures.
 */
#define CPA_STRUCT_BASE_ID	100000

typedef struct Cpa_struct_data {
    Pint			sid;
    XID				xid;
    struct Cpa_struct_data	*next;
} Cpa_struct_data;

/* Structures are accessed by both the client and the PM.
 * The client creates and deletes the structures, the PM just queries them
 * and maps them to and from the application's structure ids.  In order to
 * perform the translation correctly, the PM needs the resource shift and
 * resource base of the display used by the client to create the
 * structures.
 */
typedef struct { /* Data internal to the CPA module */
    long		dpy_resource_base;	/* of the controlling dpy */
    int			dpy_resource_shift;	/* of the controlling dpy */
    Cpa_struct_data	*struct_lists[CPA_STRUCT_HASH_SIZE];
    struct {
	unsigned flush: 1;	/* must flush PEX buffer before returning */
    }				flags;
    Phg_scratch		scratch;
    Widget		top_level;	/* only in PM */
    struct {
	XID	search_context;
	XID	nrm_incl_filter;
	XID	nrm_excl_filter;
	XID	inv_incl_filter;
	XID	inv_excl_filter;
    }			iss_resources; /* to prevent always re-creating them */
} Cp_a;

/* Then structure id encoding macros.  Note that they assume the
 * application's structure id is positive.  A violation of this assumption
 * isn't catastrophic, it will just put the PEX structure id outside the
 * range reserved for them.
 */
#define CPA_ENCODE_STRUCT_ID( _css_srvr, _sid ) \
    ((_css_srvr)->model.a.dpy_resource_base + ((CPA_STRUCT_BASE_ID + \
	(_sid)) << (_css_srvr)->model.a.dpy_resource_shift))

#define CPA_DECODE_STRUCT_ID( _css_srvr, _xsid ) \
    ((((_xsid) - (_css_srvr)->model.a.dpy_resource_base) >> \
	(_css_srvr)->model.a.dpy_resource_shift) - CPA_STRUCT_BASE_ID) 

#define CPA_FIRST_STRUCT_ID( _css_srvr ) \
    CPA_ENCODE_STRUCT_ID((_css_srvr), 0)

#define CPA_FOR_ALL_STRUCTS( _s, _st ) \
    { register int _i; \
    for ( (_i) = 0; (_i) < CPA_STRUCT_HASH_SIZE; (_i)++ ) { \
	for ( (_st) = (_s)->model.a.struct_lists[_i]; \
	    (_st); (_st) = (_st)->next ) {
#define CPA_END_FOR_ALL_STRUCTS	}}}

#define CPA_ALLOC_STRUCT(_sp) \
    (_sp = (Cpa_struct_data*)calloc(1,sizeof(Cpa_struct_data)))

#define CPA_CREATE_OPEN_STRUCT( _cph, _css_srvr ) \
    phg_cpa_create_struct( _cph, _css_srvr, (_cph)->psl->open_struct, \
	&(_cph)->psl->edit_mode, (Pint)0 )

/* Request buffering:  There are two levels, workstation and server.  If
 * a workstation is in WAIT/NIVE mode then strictly workstation-related
 * requests can be buffered.  If all workstations on a server are in
 * WAIT/NIVE mode then other requests can be buffered too.
 */
#define CPA_FLUSH( _css_srvr ) \
    if ( (_css_srvr)->model.a.flags.flush ){_XFlush( (_css_srvr)->display );}

#define CPA_BUFFERING( _css_srvr ) ((_css_srvr)->model.a.flags.flush)
#define CPA_WS_BUFFERING( _ws ) ((_ws)->flags.flush)

#define CPA_STRUCT_OP_CHECK	0
#define CPA_STRUCT_OP_CREATE	1
#define CPA_STRUCT_OP_REMOVE	2
#define CPA_STRUCT_OP_DESTROY	3

/* CPA vector functions */
extern	void		phg_cpa_add_el();
extern	void		phg_cpa_ar_archive();
extern  int		phg_cpa_check_struct_exists();
extern	void		phg_cpa_change_struct_id();
extern	void		phg_cpa_change_struct_refs();
extern	void		phg_cpa_change_struct_idrefs();
extern	void		phg_cpa_close();
extern	void		phg_cpa_close_struct();
extern	void		phg_cpa_close_ws();
extern	void		phg_cpa_copy_all_els();
extern	void		phg_cpa_delete_struct();
extern	void		phg_cpa_delete_all_structs();
extern	void		phg_cpa_delete_el();
extern	void		phg_cpa_delete_struct_net();
extern	void		phg_cpa_destroy();
extern	void		phg_cpa_el_search();
extern	void		phg_cpa_inc_spa_search();
extern	void		phg_cpa_inq_colr_map_meth_st();
extern	void		phg_cpa_inq_disp_update_state();
extern	void		phg_cpa_inq_el_content();
extern	void		phg_cpa_inq_el_type_size();
extern	void		phg_cpa_inq_el_ptr();
extern	void		phg_cpa_inq_filter();
extern	void		phg_cpa_inq_hierarchy();
extern	void		phg_cpa_inq_hlhsr_mode();
extern	void		phg_cpa_inq_indices();
extern	void		phg_cpa_inq_posted();
extern	void		phg_cpa_inq_rep();
extern	void		phg_cpa_inq_struct_status();
extern	void		phg_cpa_inq_struct_ids();
extern	void		phg_cpa_inq_text_extent();
extern	void		phg_cpa_inq_view_rep();
extern	void		phg_cpa_inq_win_info();
extern	void		phg_cpa_inq_ws_xform();
extern	void		phg_cpa_inq_wss_posted_to();
extern	void		phg_cpa_open_struct();
extern	Ws_handle	phg_cpa_open_ws();
extern	void		phg_cpa_post_struct();
extern	void		phg_cpa_ws_redraw_all();
extern	void		phg_cpa_set_disp_state();
extern	void		phg_cpa_set_edit_mode();
extern	void		phg_cpa_set_el_ptr();
extern	void		phg_cpa_set_filter();
extern	void		phg_cpa_set_hlhsr_mode();
extern	void		phg_cpa_set_rep();
extern	void		phg_cpa_set_view_input_priority();
extern	void		phg_cpa_set_ws_win();
extern	void		phg_cpa_set_ws_vp();
extern	void		phg_cpa_unpost_all();
extern	void		phg_cpa_unpost_struct();
extern	void		phg_cpa_ws_update();
extern	void		phg_cpa_ws_drawable_pick();
extern	void		phg_cpa_ws_map_points();
extern	void		phg_cpa_ws_redraw_regions();
extern	void		phg_cpa_ws_map_points();
extern	void		phg_cpa_ws_synch();

/* PM functions */
extern void		phg_cpa_pm_destroy();
extern Ws_handle	phg_cpa_pm_open_ws();
extern void		phg_cpa_pm_close_ws();
extern	void		phg_cpa_pm_set_filter();
extern	void		phg_cpa_pm_message();


/* Structure and element utilities. */
extern void		phg_cpa_free_structure_lists();
extern void		phg_cpa_destroy_attached_structures();
extern int		phg_cpa_build_struct_id_list();
extern Cpa_struct_data*	phg_cpa_struct_exists();
extern Cpa_struct_data*	phg_cpa_open_struct_in_list();
extern int		phg_cpa_struct_list_empty();
extern void		phg_cpa_link_struct();
extern Cpa_struct_data*	phg_cpa_create_struct();
extern int		phg_cpa_load_structures();
extern int		phg_cpa_copy_struct_to_server();

#endif
