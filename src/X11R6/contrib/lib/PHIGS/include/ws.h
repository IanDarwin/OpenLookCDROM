/* $XConsortium: ws.h,v 5.4 94/04/17 20:41:57 hersh Exp $ */

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

#ifndef PHG_WS_H_INCLUDED
#define PHG_WS_H_INCLUDED

#include "ws_inp.h"
#include <X11/extensions/multibuf.h>

/* The maximum number of views for client-side workstations. */
#define WS_MAX_VIEWS	20

#define WS_FLUSH( _ws ) \
    if ( (_ws)->flags.flush ){_XFlush( (_ws)->display );}

#define WS_DC_TO_DRWBL2( _wsh, _dcp, _dwp ) \
    ((_dwp)->x = (_dcp)->x, \
     (_dwp)->y = (_wsh)->ws_rect.height - (_dcp)->y)

#define WS_DRWBL_TO_DC2( _wsh, _dwp, _dcp ) \
    ((_dcp)->x = (_dwp)->x, \
     (_dcp)->y = (_wsh)->ws_rect.height - (_dwp)->y)

#define WS_SET_WS_RECT( _wsh, _wattr ) \
    {(_wsh)->ws_rect.x = (_wattr)->x; \
     (_wsh)->ws_rect.y = (_wattr)->y; \
     (_wsh)->ws_rect.width = (_wattr)->width; \
     (_wsh)->ws_rect.height = (_wattr)->height; \
    }

#define WS_DC_TO_NPC2(_wsxf, _dc, _npc) \
    (_npc)->x = ( (_dc)->x - (_wsxf)->offset.x) / (_wsxf)->scale.x; \
    (_npc)->y = ( (_dc)->y - (_wsxf)->offset.y) / (_wsxf)->scale.y;

#define WS_DC_TO_NPC(_wsxf, _dc, _npc) \
    (_npc)->x = ( (_dc)->x - (_wsxf)->offset.x) / (_wsxf)->scale.x; \
    (_npc)->y = ( (_dc)->y - (_wsxf)->offset.y) / (_wsxf)->scale.y; \
    (_npc)->z = ( (_dc)->z - (_wsxf)->offset.z) / (_wsxf)->scale.z;

#define WS_NPC_TO_DC(_wsxf, _npc, _dc) \
    (_dc)->x = (_npc)->x * (_wsxf)->scale.x + (_wsxf)->offset.x; \
    (_dc)->y = (_npc)->y * (_wsxf)->scale.y + (_wsxf)->offset.y; \
    (_dc)->z = (_npc)->z * (_wsxf)->scale.z + (_wsxf)->offset.z;

#define WS_PT_IN_LIMIT( lim, pt) \
    (  (pt)->x >= (lim)->x_min && (pt)->x <= (lim)->x_max \
    && (pt)->y >= (lim)->y_min && (pt)->y <= (lim)->y_max \
    && (pt)->z >= (lim)->z_min && (pt)->z <= (lim)->z_max)

#define WS_PT_IN_LIMIT2( lim, pt) \
    (  (pt)->x >= (lim)->x_min && (pt)->x <= (lim)->x_max \
    && (pt)->y >= (lim)->y_min && (pt)->y <= (lim)->y_max)

#define WS_ANY_INP_DEV_ACTIVE( _wsh ) \
    ((_wsh)->num_active_input_devs > 0)

#define WS_MAX_3(_x, _y, _z)	\
    (((_x) > (_y)) ? (((_x) > (_z)) ? (_x) : (_z)) : (((_y) > (_z)) ? (_y) : (_z)))

/*
 * These are the "times" when an action occurs. For example,
 * closing a structure could be the time in ASTI (ATI). Or,
 * starting a request is BIL for this workstation and BIG for
 * the others
 */

typedef enum {
    PHG_TIME_NOW,	 	/* the time is "now" */
    PHG_TIME_BIG, 		/* begin interaction globally */
    PHG_TIME_BIL, 		/* begin interaction locally */
    PHG_TIME_ATI,  		/* at the some time */
    PHG_TIME_NUM
} Ws_update_time;

/*
 * update actions: what can be done given the current update mode
 * and update time.
 */
typedef enum {
    PHG_UPDATE_ACCURATE, 	/* make the display accurate */
    PHG_UPDATE_UWOR,	 	/* do an update w/o regeneration */
    PHG_UPDATE_UQUM,	 	/* do a quick update */
    PHG_UPDATE_NOTHING,	 	/* do nothing (a deferral) */
    PHG_UPDATE_IF_IG,	 	/* if(global interaction) ACCURATE, else ASTI */
    PHG_UPDATE_IF_IL,	 	/* if(local interaction) ACCURATE, else ASTI */
    PHG_UPDATE_IF_INCORRECT, 	/* if (!PVISUAL_ST_CORRECT) make the display accurate */
    ASSURE_CORRECT, 		/* if (!PVISUAL_ST_CORRECT) make the display accurate */
    PHG_UPDATE_NUM
} Ws_update_action;

#define NUM_DEFERRAL 5		/* ASAP, BNIG, BNIL, ASTI, WAIT */
#define NUM_MODIFICATION 3	/* NIVE, UWOR, UQUM */

/* Idiom to be used after
 * WSB_CHECK_FOR_INTERACTION_UNDERWAY(ws, &out_ws->now_action)
 */
#define case_PHG_UPDATE_ACCURATE_or_IF_Ix                               \
                        case PHG_UPDATE_ACCURATE:                       \
                        case PHG_UPDATE_IF_IG:                          \
                        case PHG_UPDATE_IF_IL

#define WSB_CHECK_FOR_INTERACTION_UNDERWAY(ws, now_action_ptr) 	\
    { if( *(now_action_ptr) == PHG_UPDATE_IF_IG ||	\
	*(now_action_ptr) == PHG_UPDATE_IF_IL )	\
	phg_wsb_resolve_now_action(ws, now_action_ptr); }

/* When empty, dummy elements point to each other:
 * (lowest.higher = &highest) and (highest.lower = &lowest)
 * Typical call:  WS_NONE_POSTED(&out_ws->posted)
 */
#define WSB_NONE_POSTED(posted_ptr)	\
	 ( (posted_ptr)->lowest.higher == &(posted_ptr)->highest )
#define WSB_SOME_POSTED(posted_ptr)	\
	 ( (posted_ptr)->lowest.higher != &(posted_ptr)->highest )
#define WSB_CHECK_POSTED(posted_ptr)	\
	assure(   ((posted_ptr)->lowest.higher == &(posted_ptr)->highest)   \
		== ((posted_ptr)->highest.lower == &(posted_ptr)->lowest) )

typedef struct _Ws_post_str {
    Pfloat              disp_pri;
    Struct_handle       structh;
    struct _Ws_post_str *lower;
    struct _Ws_post_str *higher;
} Ws_post_str;

typedef struct {
    /* Posted structures: a doubly-linked list, ordered by posting priority.
     * Dummy structure elements are on both ends of the list.
     * highest.lower is the highest-priority true posted structure on this WS.
     * lowest.higher is the lowest-priority true posted structure on this WS.
     */
    Ws_post_str lowest;			/* lowest.lower is always NULL */
    Ws_post_str highest;		/* highest.higher is always NULL */
} Ws_posted_structs;

typedef enum {
    WS_PRE_CSS_DELETE,
    WS_POST_CSS_DELETE
} Ws_delete_flag;

typedef	Ws_update_action
    Ws_action_table[PHG_TIME_NUM][NUM_MODIFICATION][NUM_DEFERRAL];

typedef Ws_action_table *Ws_action_table_ptr;

typedef enum {
    WS_INV_NOT_CURRENT,
    WS_INV_CURRENT,
    WS_INV_NOT_INVERTIBLE
} Ws_inverse_state;

typedef struct {
    Pupd_st		pending;
    Pmatrix3		vom;		/* view orientation matrix */
    Pmatrix3		vmm;		/* view mapping matrix */
    Plimit3		clip_limit;	/* clip limits */
    Pclip_ind		xy_clip;	/* x-y clip control */
    Pclip_ind		back_clip;	/* back plane clipping */
    Pclip_ind		front_clip;	/* front plane clipping */
    Ws_inverse_state	npc_to_wc_state;
    Pmatrix3		npc_to_wc;	/* inverse transform, check
					 * npc_to_wc_state to see if it's
					 * current.  */
} Ws_view_entry;

typedef struct {
    Pint	id;
    Pview_rep3	view;
} Ws_pending_view;

typedef struct {
    int		higher; /* index of higher priority than this one. */
    int		lower;  /* index of lower priority than this one. */
} Ws_view_priority;

typedef struct {
    Ppoint3	scale;
    Ppoint3	offset;
} Ws_xform;


/* API state list data for type A (server-side) workstations.  */
typedef struct {
    Pint		unused;
} Wsa_output_ws;


/* API state list data for type B (client-side) workstations.  */
typedef struct {
    Css_handle	cssh;
    XID		pipeline;
    Pvisual_st	vis_rep;		/* state of visual representation */
    Pdisp_surf_empty	surf_state;		/* Display surface empty */
    Plimit3	ws_window;		/* current ws window and viewport */
    Plimit3	ws_viewport;
    Pupd_st	ws_window_pending;	/* if a new ws transform is pending */
    Pupd_st	ws_viewport_pending;
    Plimit3	req_ws_window;		/* requested ws window and viewport */
    Plimit3	req_ws_viewport;
    Ws_xform	ws_xform;		/* scale and offset of ws xform */
    int		num_pending_views;	/* number of pending views */
    Ws_pending_view	*pending_views;	/* pending views */
    int		num_views;		/* number of views */
    Ws_view_entry	*views;		/* view table */
    Pint	top_view;		/* highest priority view */
    Ws_view_priority	*view_priorities; /* array of relative priorities.
					   * The "higher" field of the
					   * highest priority view is set to
					   * -1, as is the "lower" field of
					   * the lowest priority view.
					   */
    Ws_posted_structs	posted;		/* List of posted structures */
    Ws_update_action  now_action;	/* cached value for what to do at
					 * time "now". avoids a table lookup.
					 */
    Ws_action_table_ptr	 update_action_table;	/* table which controls
					         * what, if anything,
					         * is to be done to
					         * update the screen.
					         */
    /* HLHSR stuff */
    Pupd_st		hlhsr_mode_pending;	/* new hlhsr mode is pending */
    int			req_hlhsr_mode;		/* requested hlhsr mode */
    int			cur_hlhsr_mode;		/* current hlhsr mode */
    /* Double Buffer Stuff */
    int                 front;                  /* index of front drawable */
    int                 back;                  /* index of back drawable */
    int                 has_double_buffer;     /* Boolean for buffer presence */
    Multibuffer         double_drawable[2];    /* the two buffers */
} Wsb_output_ws;

typedef struct {
    Pdefer_mode		def_mode;		/* deferral mode */
    Pmod_mode		mod_mode;		/* modification mode */
    struct {	/* Lookup Table Ids */
	XID	marker,
		line,
		text,
		interior,
		edge,
		colour,
		depth_cue,
		light_source,
		colour_approx,
		pattern,
		font,
		view;	/* not used by type A workstations */
    }				lut;
    struct {	/* Hash Table Ids */
	Hash_table	marker,
			line,
			text,
			interior,
			edge,
			colour,
			depth_cue,
			light_source,
			colour_approx,
			pattern,
			font,
			view;	/* not used by type A workstations */
    }				htab;

    struct {	/* Name Set Ids */
	XID	hlt_incl,
		hlt_excl,
		invis_incl,
		invis_excl,
		drawable_pick_incl,
		drawable_pick_excl;
    }				nset;
    union {
	Wsa_output_ws	a;
	Wsb_output_ws	b;
    }			model;
} Ws_output_ws;

typedef struct _Ws {
    Pint		id;
    Wst			*type;
    Pws_cat		category;
    Ws_output_ws	out_ws;
    Ws_input_ws		in_ws;
    Pint		current_colour_model;
    int			num_active_input_devs;

    Cp_handle		cph;
    Err_handle		erh;
    Phg_scratch		scratch;

    /* PEX info */
    Cpx_css_srvr	*css_srvr;
    Display		*display;	/* Currently this is always the
					 * same as css_srvr->display when
					 * the css_srvr is associated with
					 * a display.
					 */
    XID		 	rid;		/* resource id, WS or Renderer */
    Drawable		drawable_id;
    Window		input_overlay_window;
    XRectangle		ws_rect;
    Pint		true_colr_map_count;
    unsigned long	true_colr_map_base_pixel;
    Widget		top_level;		/* only in PM */
    Widget		shell;			/* only in PM */
    Widget		msg_shell;		/* only in PM */
    Widget		msg_label;		/* only in PM */
    struct {
	unsigned flush: 1;	/* if == 1, don't buffer X/PEX requests */
    }			flags;
    struct _Ws			*next;		/* link for CP list */

    /* Not all of these are used by all devices. */
    void	(*close)();
    void	(*redraw_all)();
    void	(*conditional_redraw)();
    void	(*repaint_all)();
    void	(*make_requested_current)();
    void	(*update)();
    void	(*set_disp_update_state)();
    void	(*message)();
    void	(*set_rep)();
    void	(*set_filter)();
    void	(*set_colour_model)();
    void	(*set_hlhsr_mode)();
    void	(*set_view_input_priority)();
    void	(*set_ws_window)();
    void	(*set_ws_vp)();
    void	(*delete_el_for_repl)();
    void	(*add_el)();
    void	(*copy_struct)();
    void	(*close_struct)();
    void	(*move_ep)();
    int 	(*delete_el)();
    int 	(*delete_struct)();
    int 	(*delete_struct_net)();
    void 	(*delete_all_structs)();
    void	(*post)();
    void	(*unpost)();
    void	(*unpost_all)();
    void	(*change_posting)();
    Ws_handle	(*bnig_update)();

    void	(*inq_view_indices)();	/* Inquiry functions */
    void	(*inq_bundle_indices)();
    void	(*inq_posted)();
    void	(*inq_representation)();
    void	(*inq_view_rep)();
    void	(*inq_ws_xform)();
    void	(*inq_disp_update_state)();
    void	(*inq_filter)();
    void	(*inq_hlhsr_mode)();
    void	(*inq_colour_model)();

    void	(*set_device_mode)();	/* Input functions */
    void	(*init_device)();
    void	(*request_device)();
    void	(*sample_device)();
    void	(*inq_inp_dev_state)();
    int		(*X_point_in_viewport)();
    void	(*input_repaint)();
    int		(*map_initial_points)();
    int		(*resolve_locator)();
    int		(*resolve_stroke)();
    int		(*resolve_pick)();
    int		(*pick_enable)();
    void	(*pick_disable)();
    int		(*valid_pick_path)();
    void	(*drawable_pick)();
    void	(*map_points)();
    void	(*redraw_regions)();
    void	(*synch)();
} Ws;

/* WS utilities */
extern int	phg_wsx_create_LUTs();
extern int	phg_wsx_init_LUTs();
extern void	phg_wsx_destroy_LUTs();
extern void	phg_wsx_set_LUT_entry();
extern void	phg_wsx_inq_LUT_entry();
extern void	phg_wsx_inq_LUT_indices();
extern void	phg_wsx_set_name_set();
extern void	phg_wsx_inq_name_set();
extern int	phg_wsx_setup_tool();
extern int	phg_wsx_setup_colormap();
extern caddr_t	phg_wsx_colr_mapping_entry_from_pex();
extern int	phg_wsx_get_true_colors();
extern int	phg_wsx_find_best_visual();
extern void	phg_wsx_release_window();
extern Window	phg_wsx_create_overlay();
extern void	phg_wsx_destroy_overlay();
extern Ws*	phg_wsx_create();
extern void	phg_wsx_destroy();
extern void	phg_wsx_update_ws_rect();
extern void	phg_wsx_inq_text_extent();
extern int	phg_wsx_build_exposure_rects();
extern void	phg_wsx_synch();
extern void	phg_wsx_convert_rects();
extern void	phg_wsx_lut_update_htab();
extern void	phg_wsx_inq_colr_map_meth_st();

extern void	phg_wsx_pm_create_message_win();

/* Workstation functions for server structure storage. */
extern Ws*	phg_wsa_open_ws();
extern void	phg_wsa_close_ws();

extern int	phg_wsa_resolve_locator();
extern int	phg_wsa_resolve_stroke();
extern int	phg_wsa_resolve_pick();
extern int	phg_wsa_pick_enable();
extern void	phg_wsa_pick_disable();
extern void	phg_wsa_drawable_pick();
extern void	phg_wsa_map_points();
extern void	phg_wsa_redraw_regions();
    /* PM only */
extern Ws*	phg_wsa_pm_open_ws();
extern void	phg_wsa_pm_close_ws();
extern int	phg_wsa_pm_resolve_pick();
extern int	phg_wsa_pm_pick_enable();
extern int	phg_wsa_pm_map_initial_points();
extern int	phg_wsa_pm_valid_pick_path();

/* Workstation functions for client-side structure storage. */
extern Ws*	phg_wsb_open_ws();
extern void	phg_wsb_close_ws();
extern Ws*	phg_wsb_pm_open_ws();
extern void	phg_wsb_pm_close_ws();
extern void	phg_wsb_add_el();
extern void	phg_wsb_close_struct();
extern void	phg_wsb_delete_all_structs();
extern int	phg_wsb_delete_struct();
extern int	phg_wsb_delete_struct_net();
extern void	phg_wsb_move_ep();
extern void	phg_wsb_copy_struct();
extern int	phg_wsb_delete_el();
extern void	phg_wsb_redraw_all();
extern void	phg_wsb_conditional_redraw();
extern void	phg_wsb_make_requested_current();
extern void	phg_wsb_repaint_all();
extern void	phg_wsb_traverse_all_postings();
extern void	phg_wsb_traverse_net();
extern void	phg_wsb_resolve_now_action();
extern void	phg_wsb_update();
extern void	phg_wsb_set_disp_update_state();
extern void	phg_wsb_set_hlhsr_mode();
extern void	phg_wsb_set_ws_window();
extern void	phg_wsb_set_ws_vp();
extern void	phg_wsb_set_view_input_priority();
extern void	phg_wsb_set_rep();
extern void	phg_wsb_set_filter();
extern void	phg_wsb_copy_struct();
extern void	phg_wsb_close_struct();
extern int	phg_wsb_asti_update();
extern void	phg_wsb_post();
extern void	phg_wsb_unpost();
extern void	phg_wsb_unpost_all();
extern void	phg_wsb_change_posting();
extern void	phg_wsb_inq_view_indices();
extern void	phg_wsb_inq_posted();
extern void	phg_wsb_inq_view_rep();
extern void	phg_wsb_inq_ws_xform();
extern void	phg_wsb_inq_disp_update_state();
extern void	phg_wsb_inq_hlhsr_mode();
extern void	phg_wsb_inq_rep();
extern int	phg_wsb_resolve_locator();
extern int	phg_wsb_resolve_stroke();
extern int	phg_wsb_resolve_pick();
extern int	phg_wsb_map_initial_points();
extern void	phg_wsb_handle_resize();
extern void	phg_wsb_handle_exposure();
extern void	phg_wsb_inq_filter();
extern void	phg_wsb_drawable_pick();
extern void	phg_wsb_map_points();
extern void	phg_wsb_redraw_regions();

#endif	/* PHG_WS_H_INCLUDED */
