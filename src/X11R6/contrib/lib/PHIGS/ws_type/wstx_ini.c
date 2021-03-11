/* $XConsortium: wstx_ini.c,v 5.10 94/04/17 20:42:38 rws Exp $ */

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

#include <fcntl.h>
#include "phg.h"
#include "phg_dt.h"
#include "alloc.h"
#include "Xatom.h"

/* 
 * These are default workstation types.  They are global to the phigs
 * application.
 */

static Wst	ws_type_x_tool;
Pint		phigs_ws_type_x_tool = (Pint)&ws_type_x_tool;
static Wst	ws_type_x_drawable;
Pint		phigs_ws_type_x_drawable = (Pint)&ws_type_x_drawable;


static void
init_output_ws_dt( out_ws_dt )
    register Wst_output_wsdt	*out_ws_dt;
{   
    register int	i;

    out_ws_dt->ws_class	= PCLASS_RASTER;
    out_ws_dt->deferral_mode = PDEFER_ASAP;
    out_ws_dt->modification_mode = PMODE_UQUM;
    out_ws_dt->default_colour_model = PMODEL_RGB;

    out_ws_dt->num_charsets = 1;
    assure(out_ws_dt->num_charsets <= WST_MAX_NUM_CHARSETS);
    out_ws_dt->charsets[0].charset = PCS_ASCII;
    out_ws_dt->charsets[0].width = 1;

    out_ws_dt->num_char_expansion_factors = 0;
    out_ws_dt->min_char_expansion_factor = 0;
    out_ws_dt->max_char_expansion_factor = MAXFLOAT;
    out_ws_dt->num_char_heights = 0;
    out_ws_dt->min_char_height = 0;
    out_ws_dt->max_char_height = MAXFLOAT;

    out_ws_dt->num_display_priorities = 0;	/* determined when ws opened */

    /* Initialize the predefined table entries. */
    {
	Pline_bundle_plus	*lb = out_ws_dt->default_polyline_bundle_table;

	out_ws_dt->num_predefined_polyline_indices =
	    WST_MIN_PREDEF_LINE_REPS;
	lb[0].type = PLINE_SOLID;
	lb[0].width = 1.0;
	lb[0].colr.type = PINDIRECT; lb[0].colr.val.ind = 1;
	lb[0].shad_meth = PSD_NONE;
	lb[0].approx_type = PCURV_WS_DEP; lb[0].approx_val = 1.0;
	/* Copy these settings to the rest of the entries. */
	for ( i = 1; i < WST_MIN_PREDEF_LINE_REPS; i++ )
	    lb[i] = lb[0];
	/* Make the other 4 entries distinguishable, using values that PEX
	 * is required to support so that there's no problem getting them.
	 */
	lb[1].type = PLINE_DASH;
	lb[2].type = PLINE_DOT;
	lb[3].type = PLINE_DASH_DOT;
	lb[4].width = 2.0;
    }
    {
	Pmarker_bundle_plus *mb = out_ws_dt->default_polymarker_bundle_table;

	out_ws_dt->num_predefined_polymarker_indices =
	    WST_MIN_PREDEF_MARKER_REPS;
	mb[0].type = PMARKER_DOT;
	mb[0].size = 1.0;
	mb[0].colr.type = PINDIRECT; mb[0].colr.val.ind = 1;
	/* Copy these settings to the rest of the entries. */
	for ( i = 1; i < WST_MIN_PREDEF_MARKER_REPS; i++ )
	    mb[i] = mb[0];
	/* Make the other 4 entries distinguishable, using values that PEX
	 * is required to support so that there's no problem getting them.
	 */
	mb[1].type = PMARKER_PLUS;
	mb[2].type = PMARKER_ASTERISK;
	mb[3].type = PMARKER_CIRCLE;
	mb[4].type = PMARKER_CROSS;
    }
    {
	Ptext_bundle_plus *tb = out_ws_dt->default_text_bundle_table;

	out_ws_dt->num_predefined_text_indices = WST_MIN_PREDEF_TEXT_REPS;
	tb[0].font = PFONT_MONO;
	tb[0].prec = PPREC_STROKE;
	tb[0].colr.type = PINDIRECT; tb[0].colr.val.ind = 1;
	tb[0].char_expan = 1.0;
	tb[0].char_space = 0.0;
	/* Copy these settings to the rest of the entries. */
	for ( i = 1; i < WST_MIN_PREDEF_TEXT_REPS; i++ )
	    tb[i] = tb[0];
	/* Make the other 5 entries distinguishable, using values that PEX
	 * is required to support so that there's no problem getting them.
	 */
	tb[1].char_expan = 1.5;
	tb[2].char_expan = 2.0;
	tb[3].char_expan = 2.5;
	tb[4].char_expan = 3.0;
	tb[5].char_expan = 3.5;
    }
    {
	Pint_bundle_plus *ib = out_ws_dt->default_interior_bundle_table;

	out_ws_dt->num_predefined_interior_indices =
	    WST_MIN_PREDEF_INTERIOR_REPS;
	ib[0].style = PSTYLE_SOLID;
	ib[0].style_ind = 1;
	ib[0].colr.type = PINDIRECT; ib[0].colr.val.ind = 1;
	ib[0].refl_eqn = PREFL_NONE;
	ib[0].shad_meth = PSD_NONE;
	ib[0].refl_props.ambient_coef = 1.0;
	ib[0].refl_props.diffuse_coef = 1.0;
	ib[0].refl_props.specular_coef = 0.0;
	ib[0].refl_props.specular_exp = 0.0;
	ib[0].refl_props.specular_colr.type = PINDIRECT;
	    ib[0].refl_props.specular_colr.val.ind = 1;
	ib[0].back_style = PSTYLE_SOLID;
	ib[0].back_style_ind = 1;
	ib[0].back_colr.type = PINDIRECT; ib[0].back_colr.val.ind = 1;
	ib[0].back_refl_eqn = PREFL_NONE;
	ib[0].back_shad_meth = PSD_NONE;
	ib[0].back_refl_props = ib[0].refl_props;
	ib[0].approx_type = PSURF_WS_DEP;
	ib[0].approx_val[0] = ib[0].approx_val[1] = 1.0;

	/* Copy these settings to the rest of the entries. */
	for ( i = 1; i < WST_MIN_PREDEF_INTERIOR_REPS; i++ )
	    ib[i] = ib[0];
	/* Make the other 4 entries distinguishable, using values that PEX
	 * is required to support so that there's no problem getting them.
	 */
	ib[1].style = PSTYLE_HOLLOW; ib[1].back_style = PSTYLE_HOLLOW;
	ib[2].style = PSTYLE_EMPTY; ib[2].back_style = PSTYLE_EMPTY;
	ib[3].style = PSTYLE_SOLID; ib[3].back_style = PSTYLE_HOLLOW;
	ib[4].style = PSTYLE_SOLID; ib[4].back_style = PSTYLE_EMPTY;
    }
    {
	Pedge_bundle_plus	*eb = out_ws_dt->default_edge_bundle_table;

	out_ws_dt->num_predefined_edge_indices = WST_MIN_PREDEF_EDGE_REPS;
	eb[0].flag = PEDGE_ON;
	eb[0].type = PLINE_SOLID;
	eb[0].width = 1.0;
	eb[0].colr.type = PINDIRECT; eb[0].colr.val.ind = 1;
	/* Copy these settings to the rest of the entries. */
	for ( i = 1; i < WST_MIN_PREDEF_EDGE_REPS; i++ )
	    eb[i] = eb[0];
	/* Make the other 4 entries distinguishable, using values that PEX
	 * is required to support so that there's no problem getting them.
	 */
	eb[1].type = PLINE_DASH;
	eb[2].type = PLINE_DOT;
	eb[3].type = PLINE_DASH_DOT;
	eb[4].width = 2.0;
    }
    {
	Pdcue_bundle	*dc = out_ws_dt->default_depth_cue_table;

	out_ws_dt->num_predefined_depth_cue_indices =
	    WST_MIN_PREDEF_DEPTH_CUE_REPS;
	dc[0].mode = PSUPPRESSED;
	dc[0].ref_planes[0] = 0.0; dc[0].ref_planes[1] = 1.0;
	dc[0].scaling[0] = 0.0; dc[0].scaling[1] = 1.0;
	dc[0].colr.type = PINDIRECT; dc[0].colr.val.ind = 0;
	/* Copy these settings to the rest of the entries. */
	for ( i = 1; i < WST_MIN_PREDEF_DEPTH_CUE_REPS; i++ )
	    dc[i] = dc[0];
	/* Make the other entry distinguishable, using values that PEX
	 * is required to support so that there's no problem getting them.
	 */
	dc[1].mode = PALLOWED;
    }
    {
	Plight_src_bundle *ls = out_ws_dt->default_light_src_table;

	out_ws_dt->num_predefined_light_src_indices =
	    WST_MIN_PREDEF_LIGHT_REPS;
	ls[0].type = PLIGHT_AMBIENT;
	ls[0].rec.ambient.colr.type = PINDIRECT;
	    ls[0].rec.ambient.colr.val.ind = 1;
	/* Copy these settings to the rest of the entries. */
	for ( i = 1; i < WST_MIN_PREDEF_LIGHT_REPS; i++ )
	    ls[i] = ls[0];
    }
    {
	Phg_colr_map_rep *cm = out_ws_dt->default_colr_mapping_table;

	out_ws_dt->num_predefined_colr_mapping_indices =
	    WST_MIN_PREDEF_COLR_MAP_REPS;
	cm[0].method = PCOLR_MAP_TRUE;
	/* no data for this method */
	/* Copy these settings to the rest of the entries. */
	for ( i = 1; i < WST_MIN_PREDEF_COLR_MAP_REPS; i++ )
	    cm[i] = cm[0];
    }
}

static void
init_input_ws_dt( ws_dt )
    register Wst_phigs_dt	*ws_dt;
{   
    register Wst_input_wsdt	*in_ws_dt = &ws_dt->in_dt;
    register Plimit3		*evol;
    register int		i;

    in_ws_dt->num_devs.loc = 3;
    assure(in_ws_dt->num_devs.loc <= WST_MAX_NUM_LOCATOR_DEVS);
    in_ws_dt->num_devs.stroke = 1;
    assure(in_ws_dt->num_devs.stroke <= WST_MAX_NUM_STROKE_DEVS);
    in_ws_dt->num_devs.pick = 1;
    assure(in_ws_dt->num_devs.pick <= WST_MAX_NUM_PICK_DEVS);
    in_ws_dt->num_devs.val = 12;
    assure(in_ws_dt->num_devs.val <= WST_MAX_NUM_VALUATOR_DEVS);
    in_ws_dt->num_devs.choice = 3;
    assure(in_ws_dt->num_devs.choice <= WST_MAX_NUM_CHOICE_DEVS);
    in_ws_dt->num_devs.string = 1;
    assure(in_ws_dt->num_devs.string <= WST_MAX_NUM_STRING_DEVS);

    {
	Wst_defloc	*loc;

	loc = in_ws_dt->locators;
	/* i is the device number, not the array position. */
	for ( i = 1; i <= in_ws_dt->num_devs.loc; i++, loc++ ) {
	    loc->position.x = loc->position.y = loc->position.z = 0.0;
	    evol = &loc->e_volume;
	    switch ( i ) {	/* associate a type with each device */
		default:
		case 1: loc->type = WST_LOC_TYPE_POINTER_BUTTON_1; break;
		case 2: loc->type = WST_LOC_TYPE_POINTER_BUTTON_2; break;
		case 3: loc->type = WST_LOC_TYPE_POINTER_BUTTON_3; break;
	    }
	    switch ( loc->type ) {
		case WST_LOC_TYPE_POINTER_BUTTON_1:
		case WST_LOC_TYPE_POINTER_BUTTON_2:
		case WST_LOC_TYPE_POINTER_BUTTON_3:
		    evol->x_min = evol->y_min = evol->z_min = 0.0;
		    evol->x_max = ws_dt->dev_coords[0];
		    evol->y_max = ws_dt->dev_coords[1];
		    evol->z_max = ws_dt->dev_coords[2];
		    loc->num_pets = 1;
		    loc->pets[0] = 1;
		    break;
	    }
	}
    }

    {
	Wst_defstroke		*stk;
	Pstroke_data3		*rec;

	stk = in_ws_dt->strokes;
	/* i is the device number, not the array position. */
	for ( i = 1; i <= in_ws_dt->num_devs.stroke; i++, stk++) {
	    evol = &stk->e_volume;
	    switch ( i ) {
		default:
		case 1: stk->type = WST_STROKE_TYPE_POINTER_BUTTON_1; break;
		case 2: stk->type = WST_STROKE_TYPE_POINTER_BUTTON_2; break;
		case 3: stk->type = WST_STROKE_TYPE_POINTER_BUTTON_3; break;
	    }
	    switch ( stk->type ) {
		case WST_STROKE_TYPE_POINTER_BUTTON_1:
		case WST_STROKE_TYPE_POINTER_BUTTON_2:
		case WST_STROKE_TYPE_POINTER_BUTTON_3:
		    evol->x_min = evol->y_min = evol->z_min = 0.0;
		    evol->x_max = ws_dt->dev_coords[0];
		    evol->y_max = ws_dt->dev_coords[1];
		    evol->z_max = ws_dt->dev_coords[2];
		    stk->max_bufsize = 200;
		    stk->num_pets = 1;
		    stk->pets[0] = 1;
		    rec = &stk->record;
		    rec->buffer_size = 100;
		    rec->init_pos = 1;
		    rec->x_interval = rec->y_interval = rec->z_interval = 0.0;
		    rec->time_interval = 0.0;
		    break;
	    }
	}
    }

    {
	Wst_defpick		*pick;

	/* This only sets up the default pick device data, whether or not
	 * any pick devices are supported is determined when the WDT is
	 * built for a particular display or window.
	 */
	pick = in_ws_dt->picks;
	/* i is the device number, not the array position. */
	for ( i = 1; i <= in_ws_dt->num_devs.pick; i++, pick++ ) {
	    pick->order = PORDER_TOP_FIRST;
	    evol = &pick->e_volume;
	    switch ( i ) {
		default:
		case 1: pick->type = WST_PICK_TYPE_POINTER_BUTTON_1; break;
		case 2: pick->type = WST_PICK_TYPE_POINTER_BUTTON_2; break;
		case 3: pick->type = WST_PICK_TYPE_POINTER_BUTTON_3; break;
	    }
	    switch ( pick->type ) {
		case WST_PICK_TYPE_POINTER_BUTTON_1:
		case WST_PICK_TYPE_POINTER_BUTTON_2:
		case WST_PICK_TYPE_POINTER_BUTTON_3:
		    evol->x_min = evol->y_min = evol->z_min = 0.0;
		    evol->x_max = ws_dt->dev_coords[0];
		    evol->y_max = ws_dt->dev_coords[1];
		    evol->z_max = ws_dt->dev_coords[2];
		    /* TODO: Get pets from PEX. */
		    pick->num_pets = 1;
		    pick->pets[0] = 1;
		    break;
	    }
	}
    }

    {
	Wst_defval		*val;
	Pval_data3		*rec;

	val = in_ws_dt->valuators;
	/* i is the device number, not the array position. */
	for ( i = 1; i <= in_ws_dt->num_devs.val; i++, val++ ) {
	    val->value = 0.0;
	    evol = &val->e_volume;
	    rec = &val->record;
	    switch ( i ) {
		default:
		case 1: val->type = WST_VAL_TYPE_SLIDER; break;
		case 2: val->type = WST_VAL_TYPE_SLIDER; break;
		case 3: val->type = WST_VAL_TYPE_SLIDER; break;
		case 4: val->type = WST_VAL_TYPE_SLIDER; break;
		case 5: val->type = WST_VAL_TYPE_SLIDER; break;
		case 6: val->type = WST_VAL_TYPE_SLIDER; break;
		case 7: val->type = WST_VAL_TYPE_SLIDER; break;
		case 8: val->type = WST_VAL_TYPE_SLIDER; break;
		case 9: val->type = WST_VAL_TYPE_SLIDER; break;
		case 10: val->type = WST_VAL_TYPE_SLIDER; break;
		case 11: val->type = WST_VAL_TYPE_SLIDER; break;
		case 12: val->type = WST_VAL_TYPE_SLIDER; break;
	    }
	    switch ( val->type ) {
		case WST_VAL_TYPE_SLIDER:
		    evol->x_min = evol->y_min = evol->z_min = 0.0;
		    evol->x_max = ws_dt->dev_coords[0];
		    evol->y_max = ws_dt->dev_coords[1];
		    evol->z_max = ws_dt->dev_coords[2];
		    rec->low = 0.0;
		    rec->high = 1.0;
		    val->num_pets = 2;
		    val->pets[0] = 1;
		    val->pets[1] = -1;
		    break;
	    }
	}
    }

    {
	Wst_defchoice		*cho;

	cho = in_ws_dt->choices;
	/* i is the device number, not the array position. */
	for ( i = 1; i <= in_ws_dt->num_devs.choice; i++, cho++ ) {
	    evol = &cho->e_volume;
	    switch ( i ) {
		default:
		case 1: cho->type = WST_CHOICE_TYPE_LIST; break;
		case 2: cho->type = WST_CHOICE_TYPE_LIST; break;
		case 3: cho->type = WST_CHOICE_TYPE_LIST; break;
	    }
	    switch ( cho->type ) {
		case WST_CHOICE_TYPE_LIST:
		    evol->x_min = evol->y_min = evol->z_min = 0.0;
		    evol->x_max = ws_dt->dev_coords[0];
		    evol->y_max = ws_dt->dev_coords[1];
		    evol->z_max = ws_dt->dev_coords[2];
		    cho->num_pets = 2;
		    cho->pets[0] = 1;
		    cho->pets[1] = 3;
		    cho->choices = 100;
		    break;
	    }
	}
    }

    {
	Wst_defstring		*str;
	Pstring_data3		*rec;

	str = in_ws_dt->strings;
	/* i is the device number, not the array position. */
	for ( i = 1; i <= in_ws_dt->num_devs.string; i++, str++) {
	    evol = &str->e_volume;
	    str->max_bufsize = 1024;
	    rec = &str->record;
	    switch ( i ) {
		default:
		case 1: str->type = WST_STRING_TYPE_WINDOW; break;
	    }
	    switch ( str->type ) {
		case WST_STRING_TYPE_WINDOW:
		    evol->x_min = evol->y_min = evol->z_min = 0.0;
		    evol->x_max = ws_dt->dev_coords[0];
		    evol->y_max = ws_dt->dev_coords[1];
		    evol->z_max = ws_dt->dev_coords[2];
		    str->num_pets = 1;
		    str->pets[0] = 1;
		    rec->buffer_size = str->max_bufsize;
		    rec->init_pos = 1;
		    break;
	    }
	}
    }
}

static void
init_views( ws_dt )
    register Wst_phigs_dt	*ws_dt;
{
    register Pview_rep3		*view = ws_dt->default_views;
    register int		i;

    ws_dt->num_predefined_views = WST_MIN_PREDEF_VIEW_REPS;

    phg_mat_identity( view[0].ori_matrix );
    phg_mat_identity( view[0].map_matrix );
    view[0].clip_limit.x_min = view[0].clip_limit.y_min =
	view[0].clip_limit.z_min = 0.0;
    view[0].clip_limit.x_max = view[0].clip_limit.y_max =
	view[0].clip_limit.z_max = 1.0;
    view[0].xy_clip = view[0].back_clip = view[0].front_clip = PIND_CLIP;
    for ( i = 1; i < WST_MIN_PREDEF_VIEW_REPS; i++ )
	view[i] = view[0];

    /* Predefined some interesting views. */
    /* View 1: parallel front view in lower left corner of ws window.
     * vrp = (0,0,0), vup = <0,1,0>, vpn = <0,0,1>, prp = (0.5,0.5,5.0)
     * win x limits = [0,1], win y limits = [0,1]
     * view plane = 0.0, front plane = 1.0, back plane = 0.0
     * vp x limits = [.1,.4], vp y limits = [.1,.4], vp z limits = [0,1]
     */
    view[1].map_matrix[0][0] = 0.3;
    view[1].map_matrix[0][3] = 0.1;
    view[1].map_matrix[1][1] = 0.3;
    view[1].map_matrix[1][3] = 0.1;

    /* View 2: top view in upper left corner of ws window.
     * vrp = (0,0,0), vup = <0,0,-1>, vpn = <0,1,0>, prp = (0.5,-0.5,5.0)
     * win x limits = [0,1], win y limits = [-1,0]
     * view plane = 0.0, front plane = 1.0, back plane = 0.0
     * vp x limits = [.1,.4], vp y limits = [.6,.9], vp z limits = [0,1]
     */
    view[2].map_matrix[0][0] = 0.3;
    view[2].map_matrix[0][3] = 0.1;
    view[2].map_matrix[1][1] = 0.3;
    view[2].map_matrix[1][3] = 0.9;
    view[2].ori_matrix[1][1] =  0.0;
    view[2].ori_matrix[1][2] = -1.0;
    view[2].ori_matrix[2][1] =  1.0;
    view[2].ori_matrix[2][2] =  0.0;

    /* View 3: right side view in lower right corner of ws window.
     * vrp = (0,0,0), vup = <0,1,0>, vpn = <1,0,0>, prp = (-0.5,0.5,5.0)
     * win x limits = [-1,0], win y limits = [0,1]
     * view plane = 0.0, front plane = 1.0, back plane = 0.0
     * vp x limits = [.6,.9], vp y limits = [.1,.4], vp z limits = [0,1]
     */
    view[3].map_matrix[0][0] = 0.3;
    view[3].map_matrix[0][3] = 0.9;
    view[3].map_matrix[1][1] = 0.3;
    view[3].map_matrix[1][3] = 0.1;
    view[3].ori_matrix[0][0] =  0.0;
    view[3].ori_matrix[0][2] = -1.0;
    view[3].ori_matrix[2][0] =  1.0;
    view[3].ori_matrix[2][2] =  0.0;

    /* View 4: off-axis view in upper right corner of ws window.
     * vrp = (0,0,0), vup = <0,1,0>, vpn = <1,1,1>, prp = (0,0,5)
     * win x limits = [-a,a], win y limits = [-a,a], a = 1/sqrt(2)
     * view plane = 0.0, front plane = sqrt(3), back plane = 0.0
     * vp x limits = [.6,.9], vp y limits = [.6,.9], vp z limits = [0,1]
     */
    view[4].map_matrix[0][0] = 0.3 / sqrt(2.0);
    view[4].map_matrix[1][1] = 0.3 / sqrt(2.0);
    view[4].map_matrix[2][2] = 1.0 / sqrt(3.0);
    view[4].map_matrix[0][3] = 0.75;
    view[4].map_matrix[1][3] = 0.75;
    view[4].ori_matrix[0][0] =
	-(view[4].ori_matrix[0][2] = -1.0/sqrt(2.0));
    view[4].ori_matrix[2][0] =
	view[4].ori_matrix[2][1] =
	view[4].ori_matrix[2][2] =  1.0/sqrt(3.0);
    view[4].ori_matrix[1][0] =
	view[4].ori_matrix[1][2] = -1.0/(sqrt(3.0) * sqrt(2.0));
    view[4].ori_matrix[1][1] = -2.0 * view[4].ori_matrix[1][0];

    /* View 5: off-axis perspective view in whole of ws window.
     * vrp = (0,0,0), vup = <0,1,0>, vpn = <1,1,1>, prp = (0,0,20)
     * win x limits = [-a,a], win y limits = [-a,a], a = 1/sqrt(2)
     * view plane = 10.0, front plane = sqrt(3), back plane = 0.0
     * vp x limits = [0,1], vp y limits = [0,1], vp z limits = [0,1]
     */
    bcopy( (char *)view[4].ori_matrix, (char *)view[5].ori_matrix,
	sizeof(Pmatrix3) );
    view[5].map_matrix[0][0] =  0.5 / sqrt(2.0);
    view[5].map_matrix[1][1] =  0.5 / sqrt(2.0);
    view[5].map_matrix[0][2] = -0.025;
    view[5].map_matrix[1][2] = -0.025;
    view[5].map_matrix[2][2] = 1.0/sqrt(3.0) - 1.0/20.0;
    view[5].map_matrix[0][3] = 0.5;
    view[5].map_matrix[1][3] = 0.5;
    view[5].map_matrix[3][2] = -1.0/20.0;
}

static void
init_phigs_ws_dt( ws_dt, category )
    register Wst_phigs_dt	*ws_dt;
    Pws_cat			category;
{   
    ws_dt->ws_category = category;
    ws_dt->dev_coord_units	= PDC_OTHER;
    ws_dt->dev_coords[0]	= PHIGS_DEFAULT_TOOL_WIDTH;
    ws_dt->dev_coords[1]	= PHIGS_DEFAULT_TOOL_HEIGHT;
    ws_dt->dev_coords[2]	= PHIGS_DEFAULT_DC_DEPTH;
    /* have to fill these in properly when the ws is opened */
    ws_dt->dev_addrs_units[0]	= PHIGS_DEFAULT_TOOL_WIDTH;
    ws_dt->dev_addrs_units[1]	= PHIGS_DEFAULT_TOOL_HEIGHT;
    ws_dt->dev_addrs_units[2]	= 1;

    ws_dt->num_hlhsr_ids = 2;
    ws_dt->hlhsr_ids[0] = PHIGS_HLHSR_ID_OFF;
    ws_dt->hlhsr_ids[1] = PHIGS_HLHSR_ID_ON;

    init_views( ws_dt );

    switch ( ws_dt->ws_category) {
	case PCAT_OUTIN:
	    init_output_ws_dt( &ws_dt->out_dt);
	    init_input_ws_dt( ws_dt);
	    break;

	case PCAT_OUT:
	    init_output_ws_dt( &ws_dt->out_dt);
	    break;

	case PCAT_IN:
	    init_input_ws_dt( ws_dt);
	    break;

	case PCAT_MO:
	    init_output_ws_dt( &ws_dt->out_dt );
	    break;
    }
}

/* This is stupid, atoms don't go away when the client exits, and here we
 * are always creating an atom which is usually never used.  Moreover,
 * process id is not unique when clients come from multiple machines.
 * Why does this silliness exist? -rws
 */
static char*
get_cmap_prop_atom( wst, display ) 
   Wst			*wst;
   Display		*display;
{
    static char cmap_prop_name[PHIGS_MAX_NAME_LEN + 1];
    int		pid;

    pid = getpid();
    sprintf (cmap_prop_name, "PEX_SI_RGB_API_MAP_%d", pid); 
    return (char *)XInternAtom( display, cmap_prop_name, False );
}

static void
set_rmdb_xwin_attrs( wst, rmdb, name, class )
    Wst		*wst;
    XrmDatabase	rmdb;
    char	*name, *class;	/* must have room to add resource names */
{
    char	*str_type;
    XrmValue	value;
    char	*attrs[9];
    char	*name_end = name + strlen( name );
    char	*class_end = class + strlen( class );

    strcpy( name_end, ".bufMode" );
    strcpy( class_end, ".BufMode" );
    if ( XrmGetResource( rmdb, name, class, &str_type, &value ) == True ) {
	attrs[0] = (char *) PHIGS_X_BUF_MODE;
	attrs[2] = NULL;
	if ( !strcmp( "single", value.addr ) ) {
	    attrs[1] = (char *)PHIGS_BUF_SINGLE;
	    phg_wst_set_attrs( wst, attrs );
	} else if ( !strcmp( "double", value.addr ) ) {
	    attrs[1] = (char *)PHIGS_BUF_DOUBLE;
	    phg_wst_set_attrs( wst, attrs );
	}
    }

    *name_end = '\0';
    *class_end = '\0';
}

static void
set_rmdb_dwbl_attrs( wst, rmdb, name, class )
    Wst		*wst;
    XrmDatabase	rmdb;
    char	*name, *class;	/* must have room to add resource names */
{
    set_rmdb_xwin_attrs( wst, rmdb, name, class );
}

static void
set_rmdb_tool_attrs( wst, rmdb, name, class )
    Wst		*wst;
    XrmDatabase	rmdb;
    char	*name, *class;	/* must have room to add resource names */
{
    char	*str_type;
    XrmValue	value;
    char	*attrs[9];
    int		attr_index;
    char	*name_end = name + strlen( name );
    char	*class_end = class + strlen( class );

    set_rmdb_xwin_attrs( wst, rmdb, name, class );

    strcpy( name_end, ".geometry" );
    strcpy( class_end, ".Geometry" );
    if ( XrmGetResource( rmdb, name, class,
	    &str_type, &value ) == True ) {
	int		mask, x, y; 
	unsigned int    w, h;

	mask = XParseGeometry( value.addr, &x, &y, &w, &h );

	/* Set all the geometry values in one call. */
	attr_index = 0;
	if ( mask & XValue ) {
	    attrs[attr_index++] = (char *) PHIGS_TOOL_X;
	    attrs[attr_index++] = (char *)x;
	}
	if ( mask & YValue ) {
	    attrs[attr_index++] = (char *) PHIGS_TOOL_Y;
	    attrs[attr_index++] = (char *)y;
	}
	if ( mask & WidthValue ) {
	    attrs[attr_index++] = (char *) PHIGS_TOOL_WIDTH;
	    attrs[attr_index++] = (char *)w;
	}
	if ( mask & HeightValue ) {
	    attrs[attr_index++] = (char *) PHIGS_TOOL_HEIGHT;
	    attrs[attr_index++] = (char *)h;
	}

	attrs[attr_index] = NULL;	/* terminate the list */
	if ( !(mask & NoValue) )
	    phg_wst_set_attrs( wst, attrs );
    }

    strcpy( name_end, ".label" );
    strcpy( class_end, ".Label" );
    if ( XrmGetResource( rmdb, name, class,
	    &str_type, &value ) == True ) {
	attrs[0] = (char*) PHIGS_TOOL_LABEL;
	attrs[1] = value.addr;
	attrs[2] = NULL;
	phg_wst_set_attrs( wst, attrs );
    }

    strcpy( name_end, ".iconLabel" );
    strcpy( class_end, ".IconLabel" );
    if ( XrmGetResource( rmdb, name, class,
	    &str_type, &value ) == True ) {
	attrs[0] = (char*) PHIGS_TOOL_ICON_LABEL;
	attrs[1] = value.addr;
	attrs[2] = NULL;
	phg_wst_set_attrs( wst, attrs );
    }

    *name_end = '\0';
    *class_end = '\0';
}

static caddr_t
init_xwin_dt( wst, type, display )
   Wst			*wst;
   Wst_base_type	type;
   Display		*display;
{
    static char *default_xwin_attrs[] = {
	/* The first two attr/value pairs must stay at the head of the list
	 * and in the same order because they're referenced again below.
	 */
	(char*) PHIGS_X_DISPLAY, NULL,
	(char*) PHIGS_X_CMAP_PROP_ATOM, (char *)0,
	(char*) PHIGS_X_HANDLE_EXPOSE, (char *)TRUE,
	(char*) PHIGS_X_HANDLE_DESTROY, (char *)TRUE,
	(char*) PHIGS_DC_MODEL, (char *)PHIGS_DC_LIMITS_ADJUST_TO_WINDOW,
	(char*) 0
    };

    static char *default_dwbl_attrs[] = {
	(char*) PHIGS_X_BUF_MODE, (char *)PHIGS_BUF_SINGLE,
	(char*) 0
    };

    static char *default_tool_attrs[] = {
	(char*) PHIGS_X_BUF_MODE, (char *)PHIGS_BUF_SINGLE,
	(char*) PHIGS_TOOL_X, (char *) PHIGS_DEFAULT_TOOL_X,
	(char*) PHIGS_TOOL_Y, (char *) PHIGS_DEFAULT_TOOL_Y,
	(char*) PHIGS_TOOL_WIDTH, (char *) PHIGS_DEFAULT_TOOL_WIDTH,
	(char*) PHIGS_TOOL_HEIGHT, (char *) PHIGS_DEFAULT_TOOL_HEIGHT,
	(char*) PHIGS_TOOL_LABEL, (char*) PHIGS_DEFAULT_TOOL_LABEL,
	(char*) PHIGS_TOOL_ICON_LABEL, (char*) PHIGS_DEFAULT_TOOL_ICON_LABEL,
	(char*) PHIGS_TOOL_BORDER_WIDTH,
	    (char*) PHIGS_DEFAULT_TOOL_BORDER_WIDTH,
	(char*) 0
    };

    caddr_t	status = NULL;

    switch ( type ) {
        case WST_BASE_TYPE_X_DRAWABLE:
	    default_xwin_attrs[1] = (char *)display;
	    default_xwin_attrs[3] = get_cmap_prop_atom( wst, display );
	    if ( status = phg_wst_set_attrs( wst, default_xwin_attrs ) )
		status = phg_wst_set_attrs( wst, default_dwbl_attrs );
	    break;

	case WST_BASE_TYPE_X_TOOL:
	    default_xwin_attrs[1] = (char *)display;
	    default_xwin_attrs[3] = get_cmap_prop_atom( wst, display );
	    if ( status = phg_wst_set_attrs( wst, default_xwin_attrs ) )
		status = phg_wst_set_attrs( wst, default_tool_attrs );

	    break;
    }

    return status;
}

caddr_t
phg_wst_init( erh, display, name, class, rmdb, category )
    Err_handle	erh;
    Display	*display;
    char	*name, *class;
    XrmDatabase	rmdb;
    Pws_cat	category;
{
    caddr_t	status;
    char	*name_buf = NULL, *class_buf;
    unsigned	size, name_len, class_len;

    if (!rmdb) {
	(void) XGetDefault (display, name, "hackToGetDefaults");
	rmdb = XrmGetDatabase (display);
    }
    if ( rmdb ) {
	/* Set up for getting resource database values. */
	name_len = strlen( name );
	class_len = strlen( class );
	size = name_len + class_len + 2 /* for terminators */;
	size += 200;	/* extra room for resource name */
	if ( name_buf = malloc( size ) ) {
	    class_buf = name_buf + name_len + 1 + 100;
	    strcpy( name_buf, name );
	    strcpy( class_buf, class );
	} else {
	    ERR_BUF( erh, ERR900 );
	    return status;
	}
    }

    ws_type_x_tool.erh = erh;
    ws_type_x_tool.base_type = WST_BASE_TYPE_X_TOOL;
    ws_type_x_tool.bound_status = WST_UNBOUND;
    init_phigs_ws_dt( &ws_type_x_tool.desc_tbl.phigs_dt, category );
    status = init_xwin_dt( &ws_type_x_tool, WST_BASE_TYPE_X_TOOL, display );
    if ( rmdb )
	set_rmdb_tool_attrs( &ws_type_x_tool, rmdb, name_buf, class_buf );
    ws_type_x_tool.bound_status = WST_GLOBAL;
    if ( category == PCAT_OUT )
	/* Don't allow category changes that involve input. */
	ws_type_x_tool.flags.no_input_category = 1;

    ws_type_x_drawable.erh = erh;
    ws_type_x_drawable.base_type = WST_BASE_TYPE_X_DRAWABLE;
    ws_type_x_drawable.bound_status = WST_UNBOUND;
    init_phigs_ws_dt( &ws_type_x_drawable.desc_tbl.phigs_dt, category );
    status = init_xwin_dt( &ws_type_x_drawable, WST_BASE_TYPE_X_DRAWABLE,
	display );
    if ( rmdb )
	set_rmdb_dwbl_attrs( &ws_type_x_drawable, rmdb, name_buf, class_buf );
    ws_type_x_drawable.bound_status = WST_GLOBAL;
    if ( category == PCAT_OUT )
	/* Don't allow category changes that involve input. */
	ws_type_x_drawable.flags.no_input_category = 1;

    if ( name_buf )
	free( name_buf );

    return status;
}


void
phg_wst_destroy( wst )
   Wst	*wst;
{
    if ( wst->buffer ) {
	free( wst->buffer );
	wst->buffer_size = 0;
	wst->buffer = NULL;
    }

    if ( wst->bound_status != WST_GLOBAL )
	free( (char *)wst );
}

#ifdef CAT2
#undef CAT2
#endif

#if (__STDC__ && !defined(UNIXCPP)) || defined(ANSICPP)
#define CAT2(a,b) a##b
#else
#define CAT2(a,b) a/**/b
#endif

#define NEW_POINTER(_b,_o,_t) \
    (CAT2(dst_,_b)->_o = (_t *) \
	(dst->buffer + (((caddr_t)(CAT2(src_,_b)->_o)) - src->buffer)))

void
phg_wst_copy_buf_pointers( src, dst )
    Wst		*src, *dst;
{
    register	Wst_phigs_dt	*dst_pdt = &dst->desc_tbl.phigs_dt;
    register	Wst_xwin_dt	*dst_xdt = &dst->desc_tbl.xwin_dt;
    register	Wst_output_wsdt	*dst_odt = &dst->desc_tbl.phigs_dt.out_dt;
    register	Wst_phigs_dt	*src_pdt = &src->desc_tbl.phigs_dt;
    register	Wst_xwin_dt	*src_xdt = &src->desc_tbl.xwin_dt;
    register	Wst_output_wsdt	*src_odt = &src->desc_tbl.phigs_dt.out_dt;
    register	int		i;

    NEW_POINTER(odt,colour_models,Pint);
    NEW_POINTER(odt,rendering_colour_models,Pint);
    NEW_POINTER(pdt,hlhsr_modes,Pint);
    NEW_POINTER(odt,marker_types,Pint);
    NEW_POINTER(odt,annotation_styles,Pint);
    NEW_POINTER(odt,interior_styles,Pint_style);
    NEW_POINTER(odt,hatch_styles,Pint);
    NEW_POINTER(odt,linetypes,Pint);
    NEW_POINTER(odt,edge_types,Pint);
    NEW_POINTER(odt,refl_eqns,Pint);
    NEW_POINTER(odt,polyline_shading_methods,Pint);
    NEW_POINTER(odt,interior_shading_methods,Pint);
    NEW_POINTER(odt,light_src_types,Pint);
    NEW_POINTER(odt,curve_approx_types,Pint);
    NEW_POINTER(odt,surface_approx_types,Pint);
    NEW_POINTER(odt,trim_curve_approx_types,Pint);
    NEW_POINTER(odt,para_surf_characs,Pint);
    NEW_POINTER(odt,colr_mapping_methods,Pint);
    NEW_POINTER(odt,gdp_ids,Pint);
    NEW_POINTER(odt,gdp3_ids,Pint);
    NEW_POINTER(odt,gse_ids,Pint);
    NEW_POINTER(xdt,display_update_modes,Pint);
    NEW_POINTER(xdt,colour_approx_models,Pint);
    NEW_POINTER(odt,pattern_types,Ppat_rep_plus);
    NEW_POINTER(odt,default_colour_table,Pcolr_rep);
    NEW_POINTER(odt,gdp3_attrs,Wst_gdp_attrs);
    NEW_POINTER(odt,gdp_attrs,Wst_gdp_attrs);
    NEW_POINTER(xdt,pick_device_types,Pint);
    NEW_POINTER(xdt,pick_pets,Pint);
    NEW_POINTER(xdt,colour_approx_models,Pint);
    NEW_POINTER(xdt,display_update_modes,Pint);
    for ( i = 0; i < WST_MAX_NUM_CHARSETS; i++ ) {
	NEW_POINTER(odt,text_pairs[i],Ptext_font_prec);
    }
}

int
phg_wst_copy( src, dst )
    Wst		*src, *dst;
{
    ALLOC_DECLARE(5);

    *dst = *src;
    if ( src->desc_tbl.xwin_dt.display_name ) {
	dst->desc_tbl.xwin_dt.display_name_length = 
	    strlen(src->desc_tbl.xwin_dt.display_name) + 1;
	if ( !ALLOCATED( dst->desc_tbl.xwin_dt.display_name = 
		Malloc( dst->desc_tbl.xwin_dt.display_name_length) ) )
	    goto abort;
	else
	    (void)strcpy( dst->desc_tbl.xwin_dt.display_name,
		src->desc_tbl.xwin_dt.display_name );
    }

    if ( src->buffer && src->buffer_size > 0 ) {
	if ( !ALLOCATED( dst->buffer = Malloc( src->buffer_size ) ) )
	    goto abort;
	else
	    bcopy( src->buffer, dst->buffer,
		dst->buffer_size = src->buffer_size );
    }

    phg_wst_copy_buf_pointers( src, dst );

    return 1;

abort:
    ALLOC_FREE;
    dst->desc_tbl.xwin_dt.display_name_length = 0;
    dst->desc_tbl.xwin_dt.display_name = NULL;
    dst->buffer = NULL;
    return 0;
}
#undef CAT2
