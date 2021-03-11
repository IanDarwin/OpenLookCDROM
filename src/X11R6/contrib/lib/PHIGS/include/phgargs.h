/* $XConsortium: phgargs.h,v 5.4 94/04/17 20:41:47 hersh Exp $ */

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

#ifndef PHGARGS_H_INCLUDED
#define PHGARGS_H_INCLUDED

/* Argument typedefs for the CP functions */

typedef struct {
    XID		drawable_id;
    Display	*display;
    char	*display_name;
    int		display_name_length;
} Phg_args_conn_info;

typedef struct {
    Pint		wsid;
    XID			wsrid;
    Phg_args_conn_info	conn_info;
    Cpx_css_srvr_type	css_srvr_type;
    Wst			*type;
    char		*wst_display_name;
    int			wst_display_name_length;
    char		*wst_buffer;
    int			wst_buffer_size;
    long		dpy_resource_base;	/* type A only, CP assigned*/
    int			dpy_resource_shift;	/* type A only, CP assigned*/
} Phg_args_open_ws;

typedef struct {
    Pint	wsid;
    Pctrl_flag	flag;
} Phg_args_ws_redraw_all;

typedef struct {
    Pint	wsid;
    Pregen_flag	flag;
} Phg_args_ws_update;

typedef struct {
    Pint	wsid;
    Pdefer_mode	mode;
    Pmod_mode	mod_mode;
} Phg_args_set_disp_state;

typedef struct {
    Pint	wsid;
    char	*msg;
    Pint	msg_length;
} Phg_args_message;

typedef enum {
    PHG_ARGS_LNREP,
    PHG_ARGS_EXTLNREP,
    PHG_ARGS_MKREP,
    PHG_ARGS_EXTMKREP,
    PHG_ARGS_TXREP,
    PHG_ARGS_EXTTXREP,
    PHG_ARGS_EFREP,
    PHG_ARGS_INTERREP,
    PHG_ARGS_EXTINTERREP,
    PHG_ARGS_EDGEREP,
    PHG_ARGS_EXTEDGEREP,
    PHG_ARGS_PTREP,
    PHG_ARGS_EXTPTREP,
    PHG_ARGS_COREP, 
    PHG_ARGS_VIEWREP,
    PHG_ARGS_DCUEREP,
    PHG_ARGS_LIGHTSRCREP,
    PHG_ARGS_COLRMAPREP
} Phg_args_rep_type;

typedef struct {
    Pint		index;
    union {
        Pline_bundle		lnrep;
        Pline_bundle_plus	extlnrep;
        Pmarker_bundle		mkrep;
        Pmarker_bundle_plus	extmkrep;
        Ptext_bundle		txrep;
        Ptext_bundle_plus	exttxrep;
        Pint_bundle		interrep;
        Pint_bundle_plus	extinterrep;
        Pedge_bundle		edgerep;
        Pedge_bundle_plus	extedgerep;
        Ppat_rep		ptrep;
        Ppat_rep_plus		extptrep;
        Pcolr_rep		corep;
        Pview_rep3  		viewrep;
	Plight_src_bundle	lightsrcrep;
	Pdcue_bundle      	dcuerep;
	Phg_colr_map_rep	colrmaprep;
    }			bundl;
} Phg_args_rep_data;

typedef struct {
    Pint		wsid;
    Phg_args_rep_type	type;
    Phg_args_rep_data	rep;
} Phg_args_set_rep;

typedef enum {
    PHG_ARGS_FLT_HIGH,
    PHG_ARGS_FLT_INVIS,
    PHG_ARGS_FLT_DRAWABLE_PICK,
    PHG_ARGS_FLT_PICK
} Phg_args_flt_type;

typedef struct {
    Pint		wsid;
    Pint		devid;	/* pick filter only */
    Phg_args_flt_type	type;
    Pint_list		inc_set;
    Pint_list		exc_set;
} Phg_args_set_filter;

typedef struct {
    Pint	wsid;
    Pint	model;
} Phg_args_set_colour_model;

typedef struct {
    Pint	wsid;
    Pint	mode;
} Phg_args_set_hlhsr_mode;

typedef struct {
    Pint	wsid;
    Pint	idx;
    Pint	ref_idx;
    Prel_pri	priority;
} Phg_args_set_view_input_prio;

typedef struct {
    Pint	wsid;
    Pint	two_d;	/* ~0 if two dimensional version */
    Plimit3	limits;
} Phg_args_set_ws_winvp;

typedef enum {
    PHG_ARGS_SETEP_ABS,
    PHG_ARGS_SETEP_REL,
    PHG_ARGS_SETEP_LABEL,
    PHG_ARGS_SETEP_PICK_ID
} Phg_args_set_ep_op;

typedef struct {
    Phg_args_set_ep_op	op;
    Pint		data;
} Phg_args_set_el_ptr;

typedef enum {
    PHG_ARGS_DEL_CURRENT,
    PHG_ARGS_DEL_RANGE,
    PHG_ARGS_DEL_LABEL,
    PHG_ARGS_EMPTY_STRUCT
} Phg_args_del_el_op;

typedef union {
    struct {
        Pint	ep1;
        Pint	ep2;
    } ep_values;
    struct {
        Pint	label1;
        Pint	label2;
    } label_range;
    Pint	struct_id;
} Phg_args_del_el_data;

typedef struct {
    Phg_args_del_el_op		op;
    Phg_args_del_el_data	data;
} Phg_args_del_el;

typedef struct {
    Pint	id;
    Pref_flag	flag;
} Phg_args_del_struct_net;

typedef struct {
    Pint	orig_id;
    Pint	new_id;
    int		posted;	/* assigned by CP, not the binding */
} Phg_args_change_struct;

typedef struct {
    Pint	wsid;
    Pint	struct_id;
    Pfloat	disp_pri;
} Phg_args_post_struct;

typedef struct {
    Pint	wsid;
    Pint	struct_id;
} Phg_args_unpost_struct;

typedef struct {
    char	*fname;
    Pint	name_length;
    Pint	arid;
} Phg_args_ar_open;

typedef enum {
    PHG_ARGS_AR_STRUCTS,
    PHG_ARGS_AR_NETWORKS,
    PHG_ARGS_AR_ALL
} Phg_args_ar_op;

typedef struct {
    Pint		arid;
    Phg_args_ar_op	op;
    Pint_list		data;
    Pconf_res		resflag;
} Phg_args_ar_info;

typedef enum {
    PHG_ARGS_INP_LOC3,
    PHG_ARGS_INP_LOC,
    PHG_ARGS_INP_STK3,
    PHG_ARGS_INP_STK,
    PHG_ARGS_INP_VAL3,
    PHG_ARGS_INP_VAL,
    PHG_ARGS_INP_CHC3,
    PHG_ARGS_INP_CHC,
    PHG_ARGS_INP_PIK3,
    PHG_ARGS_INP_PIK,
    PHG_ARGS_INP_STR3,
    PHG_ARGS_INP_STR
} Phg_args_idev_class;

typedef union {
        struct {
            Ploc3	init;
            Ploc_data3	rec;
        } loc;
        struct {
            Pstroke3		init;
            Pstroke_data3	rec;
        } stk;
        struct {
            Pfloat	init;
	    int		counts[4];	/* lengths of label strings */
            Pval_data	rec;
        } val;
        struct {
            Pin_status	status;
            Pint		init;
	    int			string_list_size;
            Pchoice_data	rec;
        } cho;
        struct {
            Ppick	init;
            Ppick_data	rec;
            Ppath_order	porder;
        } pik;
        struct {
	    Phg_string	init;
            Pstring_data	rec;
        } str;
} Phg_args_init_data;

typedef struct {
    Pint		wsid;
    Phg_args_idev_class	class;
    Pint		dev;
    Pint		pet;
    Plimit3		echo_volume;
    Phg_args_init_data	data;
} Phg_args_inp_init_dev;

typedef struct {
    Phg_args_idev_class	class;
    Pint		dev;
    Pop_mode		mode;
    Pecho_switch	echo;
} Phg_args_set_mode_data;

typedef struct {
    Pint			wsid;
    Phg_args_set_mode_data	data;
} Phg_args_inp_set_mode;

typedef struct {
    Pint		wsid;
    Phg_args_idev_class	class;
    Pint		dev;
} Phg_args_inp_request;

typedef struct {
    Pint		wsid;
    Phg_args_idev_class	class;
    Pint		dev;
} Phg_args_inp_sample;

typedef struct {
    Pint	wsid;
    Pin_class	class;
    Pint	dev;
} Phg_args_inp_flush;

typedef struct {
    Pint		wsid;
    Phg_args_idev_class	class;
    Pint		dev;
    Pinq_type		inq_type; /* locator, stroke and pick only */
} Phg_args_q_inp_state;

typedef struct {
    Pint	struct_id;
    Pint	el_id;		/* el_id < 0 means current element */
} Phg_args_q_el_data;

typedef enum {
    PHG_ARGS_HIER_ANCESTORS,
    PHG_ARGS_HIER_DESCENDANTS
} Phg_args_hierarchy_dir;

typedef struct {
    Phg_args_hierarchy_dir	dir;
    Pint			struct_id;
    Ppath_order			order;
    Pint			depth;
} Phg_args_q_hierarchy;		/* ancestors and descendants */

typedef struct {
    Pint			arid;
    Phg_args_q_hierarchy	hier;
} Phg_args_q_ar_hierarchy;

typedef struct {
    XID				wsid;
    Pint			font;
    Pfloat			char_expan;
    Pfloat			spacing;
    Pfloat			height;
    Ptext_path			path;
    Phor_text_align		hor;
    Pvert_text_align		ver;
    char			*str;
    Pint			length;
} Phg_args_q_text_extent;

typedef struct {
    Pint		wsid;
    Phg_args_rep_type	type;
} Phg_args_q_indices;

typedef enum {
    PHG_ARGS_CONF_ALL,
    PHG_ARGS_CONF_NET
} Phg_args_conf_op;

typedef struct {
    Phg_args_conf_op	op;
    Pint		arid;
    Pint		struct_id;
    Pstruct_net_source	src;
} Phg_args_q_conflicting;

typedef struct {
    Pint		wsid;
    Pint		index;
} Phg_args_q_view_rep;

typedef struct {
    Pint		wsid;
    Pint		index;
} Phg_args_q_view;

typedef struct {
    Pint		wsid;
    Pint		index;
    Pinq_type		type;		/* set or realized */
    Phg_args_rep_type	rep_type;
} Phg_args_q_rep;

typedef struct {
    Pint		wsid;
    Phg_args_flt_type	type;
} Phg_args_q_filter;

typedef struct {
    Pint		struct_id;
    Pint		start_el;
    Psearch_dir		dir;
    Pelem_type_list	incl;
    Pelem_type_list	excl;
} Phg_args_el_search;

typedef struct {
    Ppoint3		ref_pt;
    Pfloat		distance;
    Pint		ceiling;
    Pclip_ind		mclip_flag;
    Pelem_ref_list	start_path;
    Pfilter		nrm_filt;
    Pfilter		inv_filt;
} Phg_args_inc_spa_search;

typedef struct {
    Pelem_type		el_type;
    Phg_pex_oc		pex_oc;
} Phg_args_add_el;

typedef struct {
    Pint		wsid;
    Ppoint		point;
    Pfloat		ap_size;
    Ppath_order		order;
    Pint		pet;
    Pecho_switch	esw;
    Plimit3		echo_volume;
    Pfilter		filter;
} Phg_args_drawable_pick; /* drawable point to pick */

typedef struct {
    Pint		wsid;
    Ppoint_list3	points;
} Phg_args_map_points;	/* drawable points to WC */

typedef struct {
    Pint		wsid;
    Pint		num_regions;
    XRectangle		*regions;
} Phg_args_redraw_regions;

typedef struct {
    Pint		wsid;
    Pint		map_method;
} Phg_args_q_colr_map_meth_st;

typedef union {
    Pint				idata;
    Pfloat				fdata;
    Phg_args_add_el			add_el;
    Phg_args_ar_info			ar_info;
    Phg_args_ar_open			ar_open;
    Phg_args_change_struct		change_struct;
    Phg_args_del_el			del_el;
    Phg_args_del_struct_net		del_struct_net;
    Phg_args_inp_flush			inp_flush;
    Phg_args_inp_init_dev		inp_init_dev;
    Phg_args_inp_request		inp_request;
    Phg_args_inp_sample			inp_sample;
    Phg_args_inp_set_mode		inp_set_mode;
    Phg_args_message			message;
    Phg_args_post_struct		post_struct;
    Phg_args_set_colour_model		set_colour_model;
    Phg_args_set_disp_state		set_disp_state;
    Phg_args_set_el_ptr			set_el_ptr;
    Phg_args_set_ep_op			set_ep_op;
    Phg_args_set_filter			set_filter;
    Phg_args_set_hlhsr_mode		set_hlhsr_mode;
    Phg_args_set_rep			set_rep;
    Phg_args_set_view_input_prio	set_view_input_prio;
    Phg_args_set_ws_winvp		set_ws_winvp;
    Phg_args_unpost_struct		unpost_struct;
    Phg_args_open_ws			open_ws;
    Phg_args_ws_redraw_all		ws_redraw_all;
    Phg_args_ws_update			ws_update;
    Phg_args_el_search			el_search;
    Phg_args_inc_spa_search		inc_spa_search;

    Phg_args_q_el_data			q_el_data;
    Phg_args_q_hierarchy		q_hierarchy;
    Phg_args_q_ar_hierarchy		q_ar_hierarchy;
    Phg_args_q_text_extent		q_text_extent;
    Phg_args_q_indices			q_indices;
    Phg_args_q_conflicting		q_conflicting;
    Phg_args_q_view_rep			q_view_rep;
    Phg_args_q_rep			q_rep;
    Phg_args_q_inp_state		q_inp_state;
    Phg_args_q_filter			q_filter;
    Phg_args_drawable_pick		drawable_pick;
    Phg_args_map_points			map_points;
    Phg_args_redraw_regions		redraw_regions;
    Phg_args_q_colr_map_meth_st		q_colr_map_meth_st;
} Phg_arg_data;

typedef struct {
    unsigned		op;
    Pint		funcnum;	/* PHIGS function number */
    Phg_arg_data	data;
    /* Below fields only used in two-process model. */
    unsigned int	data_size;	/* size of pre data */
} Phg_args;

#endif
