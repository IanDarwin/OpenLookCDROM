/* $XConsortium: ws_type.h,v 5.7 94/04/17 20:41:59 hersh Exp $ */

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

#ifndef PHG_WS_TYPE_H_INCLUDED
#define PHG_WS_TYPE_H_INCLUDED

#include "phg_dt.h"

/* Upper bounds on table sizes. */
/* space for NUM_STYLES * 2 widths * 2 types */
#define WST_MAX_NUM_CHARSETS		1

#define WST_MAX_NUM_POSSIBLE_ATTRS	5	/* Number in Pattrs enum */

/* The below constants are the maximum number of devices in a class.
 * They are used to allocate a block of storage for all the devices
 * of a given class.
 */
#define WST_MAX_NUM_LOCATOR_DEVS	3
#define WST_MAX_NUM_STROKE_DEVS		3
#define WST_MAX_NUM_PICK_DEVS		3
#define WST_MAX_NUM_VALUATOR_DEVS	12
#define WST_MAX_NUM_CHOICE_DEVS		3
#define WST_MAX_NUM_STRING_DEVS		1

/* The below constant is the maximum number of prompt/echo types any one
 * device can support.  It is used to allocate a block of storage for
 * the pet list of a each device.
 */
#define WST_MAX_NUM_PETS		10

/* The below are default input device data record values. */
#define WST_DEFAULT_VALUATOR_LABEL	"value:"
#define WST_DEFAULT_VALUATOR_FORMAT	"%8.3g"
#define WST_DEFAULT_VALUATOR_LOW_LABEL	"[%8.3g]"
#define WST_DEFAULT_VALUATOR_HIGH_LABEL	"[%8.3g]"

/* Max number of HLHSR modes and ids */
#define WST_MAX_NUM_HLHSR_MODES		5
#define WST_MAX_NUM_HLHSR_IDS		5

/* Minimum number of predefined table entries */
#define	WST_MIN_PREDEF_LINE_REPS	5
#define	WST_MIN_PREDEF_MARKER_REPS	5
#define	WST_MIN_PREDEF_TEXT_REPS	6
#define	WST_MIN_PREDEF_INTERIOR_REPS	5
#define	WST_MIN_PREDEF_EDGE_REPS	5
#define	WST_MIN_PREDEF_LIGHT_REPS	2
#define	WST_MIN_PREDEF_DEPTH_CUE_REPS	2
#define	WST_MIN_PREDEF_COLR_MAP_REPS	1
#define	WST_MIN_PREDEF_VIEW_REPS	6

/* 
 * Below are the base workstation types.  User-types are derived from these
 * types.
 */

typedef enum {
    WST_BASE_TYPE_X_TOOL,
    WST_BASE_TYPE_X_DRAWABLE
    /* others could be other metafiles, printers, etc */
} Wst_base_type;

typedef struct {
    Pint	number;			/* number of set of attributes used */
    Pattrs	attrs[WST_MAX_NUM_POSSIBLE_ATTRS]; /* list of attributes used */
} Wst_gdp_attrs;

typedef struct {
    Pint	charset;	/* index of character set */
    Pint	width;		/* width of character set */
} Wst_char_set_info;

/* output workstation description table */

typedef struct {
    Pws_class		ws_class;	/* vector, raster, other */

    /* Dynamic Modification control IRG = implicit regen; IMM = immediate */
    Pdyn_mod		view_rep;
    Pdyn_mod		polyline_bundle_rep;
    Pdyn_mod		polymarker_bundle_rep;
    Pdyn_mod		text_bundle_rep;
    Pdyn_mod		interior_bundle_rep;
    Pdyn_mod		edge_bundle_rep;
    Pdyn_mod		pattern_rep;
    Pdyn_mod		colour_rep;
    Pdyn_mod		ws_xform;
    Pdyn_mod		highlight_filter;
    Pdyn_mod		invis_filter;
    Pdyn_mod		hlhsr_mode;
    Pdyn_mod		light_source_rep;
    Pdyn_mod		dcue_rep;
    Pdyn_mod		colour_mapping_rep;

    Pdefer_mode		deferral_mode;
    Pmod_mode		modification_mode;

    /* Polylines */
    Pint		num_linetypes;
    Pint		*linetypes; /* see BUFFERED below */
    Pint		num_linewidths;
    Pfloat		nominal_linewidth;	
    Pfloat		min_linewidth;
    Pfloat		max_linewidth;
    Pint		num_predefined_polyline_indices;
    Pint		min_predef_polyline_index, /* PEX info only */
			max_predef_polyline_index; /* PEX info only */
    Pline_bundle_plus
		default_polyline_bundle_table[WST_MIN_PREDEF_LINE_REPS];
    Pint		num_polyline_shading_methods;
    Pint		*polyline_shading_methods; /* see BUFFERED below */

    /* Polymarkers */
    Pint		num_marker_types;
    Pint		*marker_types; /* see BUFFERED below */
    Pint		num_marker_sizes;
    Pfloat		nominal_marker_size;	
    Pfloat		min_marker_size;
    Pfloat		max_marker_size;
    Pint		num_predefined_polymarker_indices;
    Pint		min_predef_polymarker_index, /* PEX info only */
			max_predef_polymarker_index; /* PEX info only */
    Pmarker_bundle_plus
		default_polymarker_bundle_table[WST_MIN_PREDEF_MARKER_REPS];

    /* Text */
    Pint		num_charsets;
    Wst_char_set_info	charsets[WST_MAX_NUM_CHARSETS];
    Pint		num_text_pairs[WST_MAX_NUM_CHARSETS];
    Pint		max_num_text_fonts;	/* PEX info only */
    Pint		min_predef_text_font;	/* PEX info only */
    Pint		max_predef_text_font;	/* PEX info only */
    Ptext_font_prec	*text_pairs[WST_MAX_NUM_CHARSETS]; /* BUFFERED */
    Pint		num_char_expansion_factors;
    Pfloat		min_char_expansion_factor;
    Pfloat		max_char_expansion_factor;
    Pint		num_char_heights;
    Pfloat		min_char_height;
    Pfloat		max_char_height;
    Pint		num_predefined_text_indices;
    Pint		min_predef_text_index, /* PEX info only */
			max_predef_text_index; /* PEX info only */
    Ptext_bundle_plus	default_text_bundle_table[WST_MIN_PREDEF_TEXT_REPS];
    Pint		num_annotation_styles;
    Pint		*annotation_styles; /* see BUFFERED below */

    /* Interiors */
    Pint		num_interior_styles;
    Pint_style		*interior_styles; /* see BUFFERED below */
    Pint		num_hatch_styles;
    Pint		*hatch_styles; /* see BUFFERED below */
    Pint		num_predefined_interior_indices;
    Pint		min_predef_interior_index, /* PEX info only */
			max_predef_interior_index; /* PEX info only */
    Pint_bundle_plus
		default_interior_bundle_table[WST_MIN_PREDEF_INTERIOR_REPS];
    Pint		num_interior_shading_methods;
    Pint		*interior_shading_methods; /* see BUFFERED below */
    Pint		num_refeqs;
    Pint		*refl_eqns; /* see BUFFERED below */

    /* Edges */
    Pint		num_edge_types;
    Pint		*edge_types; /* see BUFFERED below */
    Pint		num_edgewidths;
    Pfloat		nominal_edgewidth;	
    Pfloat		min_edgewidth;
    Pfloat		max_edgewidth;
    Pint		num_predefined_edge_indices;
    Pint		min_predef_edge_index, /* PEX info only */
			max_predef_edge_index; /* PEX info only */
    Pedge_bundle_plus	default_edge_bundle_table[WST_MIN_PREDEF_EDGE_REPS];

    /* Patterns */
    Pint		num_pattern_types;
    Pint		min_predef_pattern_index, /* PEX info only */
			max_predef_pattern_index; /* PEX info only */
    Ppat_rep_plus	*pattern_types; /* see BUFFERED below */

    /* Depth Cue */
    Pint		num_predefined_depth_cue_indices;
    Pint		min_predef_depth_cue_index, /* PEX info only */
			max_predef_depth_cue_index; /* PEX info only */
    Pdcue_bundle
		default_depth_cue_table[WST_MIN_PREDEF_DEPTH_CUE_REPS];

    /* Light Sources */
    Pint		num_light_src_types;
    Pint		*light_src_types; /* see BUFFERED below */
    Pint		max_light_src;
    Pint		min_predef_light_src_index, /* PEX info only */
			max_predef_light_src_index; /* PEX info only */
    Pint		num_predefined_light_src_indices;
    Plight_src_bundle
		default_light_src_table[WST_MIN_PREDEF_LIGHT_REPS];

    /* Colour */
    Pint		num_colour_models;
    Pint		*colour_models; /* see BUFFERED below */
    Pint		default_colour_model;
    Pint		num_rendering_colour_models;
    Pint		*rendering_colour_models; /* see BUFFERED below */
    Phg_chroma_info	chroma_info;
    Pint		num_colours;
    Pcolr_avail		colour_availability;
    Pint		num_predefined_colours;
    Pint		min_predef_colour_index, /* PEX info only */
			max_predef_colour_index; /* PEX info only */
    Pcolr_rep		*default_colour_table; /* see BUFFERED below */
    Pint		num_colr_mapping_methods;
    Pint		*colr_mapping_methods; /* BUFFERED */
    Pint		num_predefined_colr_mapping_indices;
    Pint		min_predef_colr_mapping_index, /* PEX info only */
			max_predef_colr_mapping_index, /* PEX info only */
			num_true_colours;              /* PEX info only */
    Phg_colr_map_rep	
		/* default is TRUE COLOR mapping, which doesn't have data
		 * so don't need buffer space for data
		 * TODO: use buffer space so PSEUDO(_N) could
		 * be in default table
		 */
		default_colr_mapping_table[WST_MIN_PREDEF_COLR_MAP_REPS]; 

    /* Curves and Surfaces */
    Pint		max_nurb_order;
    Pint		max_trim_curve_order;
    Pint		num_curve_approx_types;
    Pint		*curve_approx_types; /* see BUFFERED below */
    Pint		num_surface_approx_types;
    Pint		*surface_approx_types; /* see BUFFERED below */
    Pint		num_trim_curve_approx_types;
    Pint		*trim_curve_approx_types; /* see BUFFERED below */
    Pint		num_para_surf_characs;
    Pint		*para_surf_characs; /* see BUFFERED below */

    /* GDP's */
    Pint		num_gdp3;
    Pint		*gdp3_ids; /* see BUFFERED below */
    Wst_gdp_attrs	*gdp3_attrs; /* see BUFFERED below */
    Pint		num_gdp;
    Pint		*gdp_ids; /* see BUFFERED below */
    Wst_gdp_attrs	*gdp_attrs; /* see BUFFERED below */

    /* GSE's */
    Pint		num_gse;
    Pint		*gse_ids; /* see BUFFERED below */

    /* Limits -- these are "maximum number of . . ." */
    Pint		num_display_priorities;
    Pint		num_polyline_bundle_entries;
    Pint		num_polymarker_bundle_entries;
    Pint		num_text_bundle_entries;
    Pint		num_interior_bundle_entries;
    Pint		num_edge_bundle_entries;
    Pint		num_depth_cue_bundle_entries;
    Pint		num_colr_mapping_entries;
    Pint		num_light_src_bundle_entries;
    Pint		num_pattern_table_entries;
    Pint		num_colour_indices;	/* ..._indices becuse they're */
    /* 0-based tables have num_... indices, numbered 0 thru (num_... - 1) */

    /* Dynamic modification */
    Pdyn_mod		struct_content_mod;
    Pdyn_mod		post;
    Pdyn_mod		unpost;
    Pdyn_mod		struct_delete;
    Pdyn_mod		ref_mod;

} Wst_output_wsdt;

/* input workstation description table */

typedef enum {
    WST_LOC_TYPE_POINTER_BUTTON_1,
    WST_LOC_TYPE_POINTER_BUTTON_2,
    WST_LOC_TYPE_POINTER_BUTTON_3
} Wst_loc_type;

typedef struct {
    Ppoint3	position;		/* initial position */
    Pint	num_pets;
    Pint	pets[WST_MAX_NUM_PETS]; /* list of prompt and echo types */
    Plimit3	e_volume;		/* echo volume */
    Ploc_data3	record;			/* locator data record */
    Wst_loc_type	type;
} Wst_defloc;	/* flattened version of Pdefloc3 */

typedef enum {
    WST_STROKE_TYPE_POINTER_BUTTON_1,
    WST_STROKE_TYPE_POINTER_BUTTON_2,
    WST_STROKE_TYPE_POINTER_BUTTON_3
} Wst_stroke_type;

typedef struct {
    Pint	max_bufsize;		/* maximum buffer size */
    Pint	num_pets;		/* number of pets */
    Pint	pets[WST_MAX_NUM_PETS]; /* list of prompt and echo types */
    Plimit3	e_volume;		/* echo volume */
    Pstroke_data3	record;			/* stroke data record */
    Wst_stroke_type	type;
} Wst_defstroke;

typedef enum {
    WST_PICK_TYPE_POINTER_BUTTON_1,
    WST_PICK_TYPE_POINTER_BUTTON_2,
    WST_PICK_TYPE_POINTER_BUTTON_3
} Wst_pick_type;

typedef struct {
    Ppath_order	order;			/* path order */
    Pint	num_pets;		/* number of pets */
    Pint	pets[WST_MAX_NUM_PETS]; /* list of prompt and echo types */
    Plimit3	e_volume;		/* echo volume */
    Ppick_data3	record;			/* pick data record */
    Wst_pick_type	type;
} Wst_defpick;	/* flattened version of Pdefpick3 */

typedef enum {
    WST_VAL_TYPE_SLIDER
} Wst_val_type;

typedef struct {
    Pfloat	value;			/* initial value */
    Pint	num_pets;		/* number of pets */
    Pint	pets[WST_MAX_NUM_PETS]; /* list of prompt and echo types */
    Plimit3	e_volume;		/* echo volume */
    Pval_data3	record;			/* valuator data record */
    Wst_val_type	type;
} Wst_defval;	/* flattened version of Pdefval3 */

typedef enum {
    WST_CHOICE_TYPE_LIST
} Wst_choice_type;

typedef struct {
    Pint	choices;		/* max number of choices */
    Pint	num_pets;		/* number of pets */
    Pint	pets[WST_MAX_NUM_PETS]; /* list of prompt and echo types */
    Plimit3	e_volume;		/* echo volume */
    Pchoice_data3	record;			/* choice data record */
    Wst_choice_type	type;
} Wst_defchoice;	/* flattened version of Pdefchoice3 */

typedef enum {
    WST_STRING_TYPE_WINDOW
} Wst_string_type;

typedef struct {
    Pint	max_bufsize;		/* maximum buffer size */
    Pint	num_pets;		/* number of pets */
    Pint	pets[WST_MAX_NUM_PETS]; /* list of prompt and echo types */
    Plimit3	e_volume;		/* echo volume */
    Pstring_data3	record;			/* string data record */
    Wst_string_type	type;
} Wst_defstring;	/* flattened version of Pdefstring3 */

/* TODO:  If some devices cannot be supported (for instance when a pick
 * device is not supported by the server) then this fixed-size array
 * approach is inefficient.
 */
typedef struct {
    Pnum_in		num_devs;
    Wst_defloc		locators[WST_MAX_NUM_LOCATOR_DEVS];
    Wst_defstroke	strokes[WST_MAX_NUM_STROKE_DEVS];
    Wst_defpick		picks[WST_MAX_NUM_PICK_DEVS];
    Wst_defval		valuators[WST_MAX_NUM_VALUATOR_DEVS];
    Wst_defchoice	choices[WST_MAX_NUM_CHOICE_DEVS];
    Wst_defstring	strings[WST_MAX_NUM_STRING_DEVS];
} Wst_input_wsdt;

/* Workstation description table for either input or output ws */
typedef struct {
    Pws_cat		ws_category;
    Pdc_units		dev_coord_units;
    Pfloat		dev_coords[3];
    Pint		dev_addrs_units[3];
    Pint		num_hlhsr_ids;
    Pint		hlhsr_ids[WST_MAX_NUM_HLHSR_IDS];
    Pint		hlhsr_depth;
    Pint		num_hlhsr_modes;
    Pint		*hlhsr_modes; /* BUFFERED */
    Pint		num_view_indices;	/* max number of views */
    Pint		num_predefined_views;
    Pint		min_predef_view_index, /* PEX info only */
			max_predef_view_index; /* PEX info only */
    Pview_rep3		default_views[WST_MIN_PREDEF_VIEW_REPS];
    Wst_output_wsdt	out_dt;
    Wst_input_wsdt	in_dt;
} Wst_phigs_dt;

typedef struct {
    int                 x, y;
    unsigned int	width, height;
    unsigned int	border_width;
    unsigned long	background;
    char		label[PHIGS_MAX_NAME_LEN+1];
    char		icon_label[PHIGS_MAX_NAME_LEN+1];
} Wst_xtool_dt;

typedef struct {
    int			display_name_length;
    char		*display_name;
    Phigs_DC_model	dc_model;
    struct {
	unsigned  handle_expose: 1;
	unsigned  handle_destroy: 1;
    }			flags;
    /* PEX info that doesn't correspond to PHIGS.  Keep it around. */
    Pint		max_num_names_for_nameset;
    Pint		num_display_update_modes;
    Pint		*display_update_modes; /* BUFFERED */
    Pint		dithering_supported;
    Pint		transparency_supported;
    Pint		best_colour_approx;
    Pint		num_colour_approx_models;
    Pint		*colour_approx_models; /* BUFFERED */
    Pint		num_pick_device_types;
    Pint		*pick_device_types; /* BUFFERED */
    Pint		num_pick_pets;
    Pint		*pick_pets; /* BUFFERED */
    Pint		buffer_mode;
    Atom		colormap_property_atom;
    Wst_xtool_dt	tool;
} Wst_xwin_dt;

typedef struct {
    Wst_phigs_dt	phigs_dt;
    Wst_xwin_dt		xwin_dt;
} Wst_dt;

typedef enum {
    WST_BOUND,		/* not bound to a workstation yet */
    WST_UNBOUND,	/* has been bound to a workstation */
    WST_GLOBAL		/* one of the globals supplied by the SI */
} Wst_bound_status;

typedef struct _Wst {
    Wst_base_type	base_type;
    Wst_bound_status	bound_status;
    Err_handle		erh;
    Pint		wsid;	/* Only if bound to an open workstation */
    struct { 
	unsigned no_input_category; /* when phigsmonitor not available */
    }                   flags;
    Wst_dt		desc_tbl;
    /* BUFFERED: This buffer holds most all the variable length data in the
     * description table.  Info buffered here is labelled above with the
     * word BUFFERED.  Those items should not be allocated or freed
     * individually.
     * IMPORTANT: Update phg_wst_copy() in the ws_type directory
     * when a buffered field is added or removed from the structure
     * definition.
     */
    caddr_t		buffer;	/* for variable length data */
    int			buffer_size; /* size of buffer, in bytes */
} Wst;

extern caddr_t		phg_wst_set_attrs();
extern caddr_t		phg_wst_get_attr();
extern caddr_t 		phg_wst_init();
extern void 		phg_wst_destroy();
extern Wst		*phg_wst_create();
extern int 		phg_wst_copy();
extern void		phg_wst_copy_buf_pointers();

#endif /* PHG_WS_TYPE_H_INCLUDED */
