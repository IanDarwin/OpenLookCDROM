/* $XConsortium: phgretdata.h,v 5.2 94/04/17 20:41:48 rws Exp $ */

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

#ifndef PHGRETDATA_H_INCLUDED
#define PHGRETDATA_H_INCLUDED

/* Return Data typedefs for the CP functions */

typedef struct {
    Pelem_type	type;
    Pint	size;
} Phg_ret_el_type_size;

typedef struct {
    Pevent		id;
    Phg_inp_event_data	data;
} Phg_ret_inp_event;

typedef union {
    Pin_status	istat;
    Pin_status	chstat;
    Pin_status	pkstat;
} Phg_ret_inp_req_stat;

typedef struct {
    int				brake;	/* (sic),  !0 if break occurred */
    Phg_ret_inp_req_stat	status;
    Phg_ret_inp_event		event;
} Phg_ret_inp_request;

typedef struct {
    Pint_list	counts;
    Pint	num_pairs;
    Pelem_ref	*paths;
} Phg_ret_hierarchy;

typedef struct {
    Prect	rect;
    Ppoint	offset;
} Phg_ret_text_extent;

typedef struct {
    Pupd_st		update_state;
    /* Use indirection for the reps to keep Phg_ret small. */
    Pview_rep3		*cur_rep;
    Pview_rep3		*req_rep;
} Phg_ret_view_rep;

typedef struct {
    Pupd_st		update_state;
    /* Use indirection for the reps to keep Phg_ret small. */
    Pview_rep3		*cur_rep;
    Pview_rep3		*req_rep;
} Phg_ret_xview_rep;

typedef union {
    Pline_bundle_plus	extlnrep;
    Pmarker_bundle_plus	extmkrep;
    Ptext_bundle_plus	exttxrep;
    Pint_bundle_plus	extinterrep;
    Pedge_bundle_plus	extedgerep;
    Ppat_rep_plus	extptrep;
    Pcolr_rep		corep;
    Pdcue_bundle        dcuerep;
    Plight_src_bundle   lightsrcrep;
    Phg_colr_map_rep	colrmaprep;
} Phg_ret_rep;

typedef struct {
    Pdefer_mode		def_mode;
    Pmod_mode		mod_mode;
    Pdisp_surf_empty	display_surf;
    Pvisual_st		state;
} Phg_ret_update_state;

typedef struct {
    int			length;	/* of choice string list */
    char		*strings;
    Pchoicest3		state;
} Phg_ret_choice_state;

typedef struct {
    int			length;	/* of string */
    Pstringst3		state;
} Phg_ret_string_state;

typedef union {
    Plocst3			loc;
    Ppickst3			pick;
    Pstrokest3			stroke;
    Pvalst3			val;
    Phg_ret_choice_state	choice;
    Phg_ret_string_state	string;
} Phg_ret_inp_state;

typedef struct {
    Pint_list	incl;
    Pint_list	excl;
} Phg_ret_filter;

typedef struct {
    Pupd_st	state;
    Pint	cur_mode;
    Pint	req_mode;
} Phg_ret_hlhsr_mode;

typedef struct {
    Pdisp_space_size3	display_size;
} Phg_ret_win_info;

typedef struct {
    Psearch_status	status;
    Pint		found_el;
} Phg_ret_el_search;

typedef struct {
	Pelem_ref_list	path;
} Phg_ret_inc_spa_search;

typedef struct {
    Display	*display;
    XID		 drawable_id;
    XID		 overlay_id;
    char	*display_name;
} Phg_ret_dpy_and_drawable;

typedef struct {
    XID			drawable_id;
    XID			overlay_id;	/* only if category is OUTIN */
    Wst			*wstype;
    char		*wst_buffer;
    int			wst_buffer_size;
} Phg_ret_open_ws;

typedef struct {
    Pelem_type		op;
    Phg_pex_oc		pex_oc;
} Phg_ret_q_content;

typedef struct {
    Pupd_st	state;	/* update state */
    Plimit3	req_window;	/* requested workstation window */
    Plimit3	cur_window;	/* current workstation window */
    Plimit3	req_viewport;	/* requested workstation viewport */
    Plimit3	cur_viewport;	/* current workstation viewport */
} Phg_ret_ws_tran3;

typedef struct {
    Pin_status		status;
    Ppick_path		pick;
} Phg_ret_drawable_pick;

typedef struct {
    Pint		view_index;
    Ppoint_list3	points;
} Phg_ret_map_points;

typedef union {
    Pint			idata;
    Pfloat			fdata;
    Pint_list			int_list;
    Pposted_struct_list		postlist;
    Phg_ret_open_ws		open_ws;
    Phg_ret_ws_tran3		ws_xform;
    Phg_ret_q_content		el_info;
    Phg_ret_el_type_size	el_type_size;
    Phg_ret_inp_request		inp_request;
    Phg_ret_inp_event		inp_event;
    Phg_ret_hierarchy		hierarchy;
    Phg_ret_text_extent		text_extent;
    Phg_ret_view_rep		view_rep;
    Phg_ret_xview_rep		xview_rep;
    Phg_ret_rep			rep;
    Phg_ret_update_state	update_state;
    Phg_ret_inp_state		inp_state;
    Phg_ret_filter		filter;
    Phg_ret_hlhsr_mode		hlhsr_mode;
    Pevent       		oflow_event;
    Phg_ret_win_info		win_info;
    Phg_ret_el_search		el_search;
    Phg_ret_inc_spa_search	inc_spa_search;
    Phg_ret_dpy_and_drawable	dpy_and_drawable;
    Phg_ret_drawable_pick	drawable_pick;
    Phg_ret_map_points		map_points;
} Phg_ret_data;

typedef struct {
    Pint		err;
    Phg_ret_data	data;
} Phg_ret;


#endif
