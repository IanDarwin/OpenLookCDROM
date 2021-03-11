/* $XConsortium: wstx_atr.c,v 5.6 94/04/17 20:42:37 hersh Exp $ */

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


/* Attribute manipulation routines for the ws_type code.
 */

#include "phg.h"
#include "PEX.h"
#include "PEXproto.h"
#include "PEXmacs.h"
#include "PEXfuncs.h"
#include "PEXtempl.h"
#include "phigspex.h"
#include <X11/Xresource.h>

static void
adjust_echo_volumes( dc, in_dt )
    Pfloat		dc[3];
    Wst_input_wsdt	*in_dt;
{
    Wst_defloc		*loc = in_dt->locators;
    Wst_defstroke	*stk = in_dt->strokes;
    Wst_defpick		*pick = in_dt->picks;
    Wst_defval		*val = in_dt->valuators;
    Wst_defchoice	*cho = in_dt->choices;
    Wst_defstring	*str = in_dt->strings;

    register	int		i;

    for (i = 0; i < in_dt->num_devs.loc; i++, loc++) {
	loc->e_volume.x_max = dc[0];
	loc->e_volume.y_max = dc[1];
	loc->e_volume.z_max = dc[2];
    }
    for (i = 0; i < in_dt->num_devs.stroke; i++, stk++) {
	stk->e_volume.x_max = dc[0];
	stk->e_volume.y_max = dc[1];
	stk->e_volume.z_max = dc[2];
    }
    for (i = 0; i < in_dt->num_devs.pick; i++, pick++) {
	pick->e_volume.x_max = dc[0];
	pick->e_volume.y_max = dc[1];
	pick->e_volume.z_max = dc[2];
    }
    for (i = 0; i < in_dt->num_devs.val; i++, val++) {
	val->e_volume.x_max = dc[0];
	val->e_volume.y_max = dc[1];
	val->e_volume.z_max = dc[2];
    }
    for (i = 0; i < in_dt->num_devs.choice; i++, cho++) {
	cho->e_volume.x_max = dc[0];
	cho->e_volume.y_max = dc[1];
	cho->e_volume.z_max = dc[2];
    }
    for (i = 0; i < in_dt->num_devs.string; i++, str++) {
	str->e_volume.x_max = dc[0];
	str->e_volume.y_max = dc[1];
	str->e_volume.z_max = dc[2];
    }
}

/* CAREFUL: The order of items in this list must correspond to the accesses
 * in the code below.
 */
static CARD16 all_enum_types [] = {
    PEXETColourType,
    PEXETHlhsrMode,
    PEXETMarkerType,
    PEXETATextStyle,
    PEXETInteriorStyle,
    PEXETHatchStyle,
    PEXETLineType,
    PEXETSurfaceEdgeType,
    PEXETReflectionModel,
    PEXETPolylineInterpMethod,
    PEXETSurfaceInterpMethod,
    PEXETLightType,
    PEXETCurveApproxMethod,
    PEXETSurfaceApproxMethod,
    PEXETTrimCurveApproxMethod,
    PEXETParaSurfCharacteristics,
    PEXETColourApproxType,
    PEXETRenderingColourModel,
    PEXETGDP,
    PEXETGDP3,
    PEXETGSE,

    PEXETDisplayUpdateMode,
    PEXETColourApproxModel,

    PEXETPickDeviceType,
    PEXETPromptEchoType
};
#define NUM_ENUMS (sizeof(all_enum_types)/sizeof(all_enum_types[0])) 

static int
init_enum_counts( wst, counts )
    Wst		*wst;
    register	CARD32		*counts; /* set all to 0 if NULL */
{
    unsigned	size = 0;
    Wst_dt	*wdt = &wst->desc_tbl;
    Wst_xwin_dt	*xdt = &wst->desc_tbl.xwin_dt;

    register	Wst_output_wsdt	*odt = &wdt->phigs_dt.out_dt;

    /* CAREFUL: "counts" has to be accessed in the same order as the types
     * listed in "all_enum_types" above.
     */
    /* odt->num_colour_models  and odt->num_colr_mapping_methods
     * may be modified later on, 
     * see init_enum_data 
     */
    size += odt->num_colour_models = counts ? *counts++ : 0;
    size += wdt->phigs_dt.num_hlhsr_modes = counts ? *counts++ : 0;
    size += odt->num_marker_types = counts ? *counts++ : 0;
    size += odt->num_annotation_styles = counts ? *counts++ : 0;
    size += odt->num_interior_styles = counts ? *counts++ : 0;
    size += odt->num_hatch_styles = counts ? *counts++ : 0;
    size += odt->num_linetypes = counts ? *counts++ : 0;
    size += odt->num_edge_types = counts ? *counts++ : 0;
    size += odt->num_refeqs = counts ? *counts++ : 0;
    size += odt->num_polyline_shading_methods = counts ? *counts++ : 0;
    size += odt->num_interior_shading_methods = counts ? *counts++ : 0;
    size += odt->num_light_src_types = counts ? *counts++ : 0;
    size += odt->num_curve_approx_types = counts ? *counts++ : 0;
    size += odt->num_surface_approx_types = counts ? *counts++ : 0;
    size += odt->num_trim_curve_approx_types = counts ? *counts++ : 0;
    size += odt->num_para_surf_characs = counts ? *counts++ : 0;
    size += odt->num_colr_mapping_methods = counts ? *counts++ : 0;
    size += odt->num_rendering_colour_models = counts ? *counts++ : 0;
    size += odt->num_gdp = counts ? *counts++ : 0;
    size += odt->num_gdp3 = counts ? *counts++ : 0;
    size += odt->num_gse = counts ? *counts++ : 0;

    size += wdt->xwin_dt.num_display_update_modes = counts ? * counts++ : 0;
    size += wdt->xwin_dt.num_colour_approx_models = counts ? * counts++ : 0;

    size += xdt->num_pick_device_types = counts ? *counts++ : 0;
    size += xdt->num_pick_pets = counts ? *counts++ : 0;

    size *= sizeof(Pint);
    size += (odt->num_gdp + odt->num_gdp3) * sizeof(Wst_gdp_attrs);

    return size;
}


#define ODD( _v ) ((_v) % 2 != 0)

static Pint*
init_enum_data( wst, enums, buf )
		Wst			*wst;
    register	pexEnumTypeIndex	*enums;
		Pint			*buf;
{
    Wst_phigs_dt	*pdt = &wst->desc_tbl.phigs_dt;
    Wst_xwin_dt		*xdt = &wst->desc_tbl.xwin_dt;
    CARD32		count;

    register	int		i;
    register	Wst_output_wsdt	*odt = &wst->desc_tbl.phigs_dt.out_dt;

    /* CAREFUL: "enums" has to be accessed in the same order as the types
     * listed in "all_enum_types" above.
     */
    /* Colour models.  PEX treats INDIRECT as a colour model but PHIGS
     * doesn't, so we have to avoid putting INDIRECT in the list.
     */
    odt->colour_models = buf;
    odt->num_colour_models = 0;
    count = *((CARD32 *)enums); 
    enums += sizeof(CARD32) / sizeof(*enums);
    for ( i = 0; i < count; i++, buf++, enums++ )
	if ( (*enums) != PEXIndexedColour ) {
	    odt->colour_models[odt->num_colour_models] = *enums;
	    ++odt->num_colour_models;
	}
    if ( ODD(count) ) ++enums;

    pdt->hlhsr_modes = buf;
    pdt->num_hlhsr_modes = *((CARD32 *)enums); 
    enums += sizeof(CARD32) / sizeof(*enums);
    for ( i = 0; i < pdt->num_hlhsr_modes; i++, buf++, enums++ )
	pdt->hlhsr_modes[i] = PEX_CONV_PEX_HLHSR_MODE(*enums);
    if ( ODD(pdt->num_hlhsr_modes) ) ++enums;

    odt->marker_types = buf;
    odt->num_marker_types = *((CARD32 *)enums); 
    enums += sizeof(CARD32) / sizeof(*enums);
    for ( i = 0; i < odt->num_marker_types; i++, buf++, enums++ )
	odt->marker_types[i] = *enums;
    if ( ODD(odt->num_marker_types) ) ++enums;

    odt->annotation_styles = buf;
    odt->num_annotation_styles = *((CARD32 *)enums); 
    enums += sizeof(CARD32) / sizeof(*enums);
    for ( i = 0; i < odt->num_annotation_styles; i++, buf++, enums++ )
	odt->annotation_styles[i] = *enums;
    if ( ODD(odt->num_annotation_styles) ) ++enums;

    odt->interior_styles = (Pint_style *)buf;
    odt->num_interior_styles = *((CARD32 *)enums); 
    enums += sizeof(CARD32) / sizeof(*enums);
    for ( i = 0; i < odt->num_interior_styles; i++, buf++, enums++ )
	odt->interior_styles[i] = PEX_CONV_TO_Pinterstyle(*enums);
    if ( ODD(odt->num_interior_styles) ) ++enums;

    odt->hatch_styles = buf;
    odt->num_hatch_styles = *((CARD32 *)enums);
    enums += sizeof(CARD32) / sizeof(*enums);
    for ( i = 0; i < odt->num_hatch_styles; i++, buf++, enums++ )
	odt->hatch_styles[i] = *enums;
    if ( ODD(odt->num_hatch_styles) ) ++enums;

    odt->linetypes = buf;
    odt->num_linetypes = *((CARD32 *)enums); 
    enums += sizeof(CARD32) / sizeof(*enums);
    for ( i = 0; i < odt->num_linetypes; i++, buf++, enums++ )
	odt->linetypes[i] = *enums;
    if ( ODD(odt->num_linetypes) ) ++enums;

    odt->edge_types = buf;
    odt->num_edge_types = *((CARD32 *)enums); 
    enums += sizeof(CARD32) / sizeof(*enums);
    for ( i = 0; i < odt->num_edge_types; i++, buf++, enums++ )
	odt->edge_types[i] = *enums;
    if ( ODD(odt->num_edge_types) ) ++enums;

    odt->refl_eqns = buf;
    odt->num_refeqs = *((CARD32 *)enums);
    enums += sizeof(CARD32) / sizeof(*enums);
    for ( i = 0; i < odt->num_refeqs; i++, buf++, enums++ )
	odt->refl_eqns[i] = *enums;
    if ( ODD(odt->num_refeqs) ) ++enums;

    odt->polyline_shading_methods = buf;
    odt->num_polyline_shading_methods = *((CARD32 *)enums); 
    enums += sizeof(CARD32) / sizeof(*enums);
    for ( i = 0; i < odt->num_polyline_shading_methods; i++, buf++, enums++ )
	odt->polyline_shading_methods[i] = *enums;
    if ( ODD(odt->num_polyline_shading_methods) ) ++enums;

    odt->interior_shading_methods = buf;
    odt->num_interior_shading_methods = *((CARD32 *)enums);
    enums += sizeof(CARD32) / sizeof(*enums);
    for ( i = 0; i < odt->num_interior_shading_methods; i++, buf++, enums++ )
	odt->interior_shading_methods[i] = *enums;
    if ( ODD(odt->num_interior_shading_methods) ) ++enums;

    odt->light_src_types = buf;
    odt->num_light_src_types = *((CARD32 *)enums);
    enums += sizeof(CARD32) / sizeof(*enums);
    for ( i = 0; i < odt->num_light_src_types; i++, buf++, enums++ )
	odt->light_src_types[i] = *enums;
    if ( ODD(odt->num_light_src_types) ) ++enums;

    odt->curve_approx_types = buf;
    odt->num_curve_approx_types = *((CARD32 *)enums);
    enums += sizeof(CARD32) / sizeof(*enums);
    for ( i = 0; i < odt->num_curve_approx_types; i++, buf++, enums++ )
	odt->curve_approx_types[i] = *enums;
    if ( ODD(odt->num_curve_approx_types) ) ++enums;

    odt->surface_approx_types = buf;
    odt->num_surface_approx_types = *((CARD32 *)enums); 
    enums += sizeof(CARD32) / sizeof(*enums);
    for ( i = 0; i < odt->num_surface_approx_types; i++, buf++, enums++ )
	odt->surface_approx_types[i] = *enums;
    if ( ODD(odt->num_surface_approx_types) ) ++enums;

    odt->trim_curve_approx_types = buf;
    odt->num_trim_curve_approx_types = *((CARD32 *)enums); 
    enums += sizeof(CARD32) / sizeof(*enums);
    for ( i = 0; i < odt->num_trim_curve_approx_types; i++, buf++, enums++ )
	odt->trim_curve_approx_types[i] = *enums;
    if ( ODD(odt->num_trim_curve_approx_types) ) ++enums;

    odt->para_surf_characs = buf;
    odt->num_para_surf_characs = *((CARD32 *)enums); 
    enums += sizeof(CARD32) / sizeof(*enums);
    for ( i = 0; i < odt->num_para_surf_characs; i++, buf++, enums++ )
	odt->para_surf_characs[i] = *enums;
    if ( ODD(odt->num_para_surf_characs) ) ++enums;

    odt->colr_mapping_methods = buf;
    odt->num_colr_mapping_methods = count = *((CARD32 *)enums); 
    enums += sizeof(CARD32) / sizeof(*enums);
    for ( i = 0; i < odt->num_colr_mapping_methods; i++, buf++, enums++ ) {
	odt->colr_mapping_methods[i] = PEX_CONV_COLRMAP_METHOD(*enums);
    	/* PEX colour space can map to PHIGS TRUE and PSEUDO-N 
	 * PSEUDO-N is best used when a DirectColor visual
	 * is being used. 
	 * PEX-SI does not support DirectColor visuals, so skips this
	if (odt->colr_mapping_methods[i] == PCOLR_MAP_TRUE) {
		odt->colr_mapping_methods[++i] = PCOLR_MAP_TRUE;
		odt->num_colr_mapping_methods++;
	}
	 */
    }
    if ( ODD(count) ) ++enums;

    odt->rendering_colour_models = buf;
    odt->num_rendering_colour_models = *((CARD32 *)enums); 
    enums += sizeof(CARD32) / sizeof(*enums);
    for ( i = 0; i < odt->num_rendering_colour_models; i++, buf++, enums++ )
	odt->rendering_colour_models[i] = *enums;
    if ( ODD(odt->num_rendering_colour_models) ) ++enums;

    odt->gdp_ids = buf;
    odt->num_gdp = *((CARD32 *)enums); 
    enums += sizeof(CARD32) / sizeof(*enums);
    odt->gdp_attrs = (Wst_gdp_attrs *)(odt->gdp_ids + odt->num_gdp);
    for ( i = 0; i < odt->num_gdp; i++, buf++, enums++ ) {
	odt->gdp_ids[i] = *enums;
	/* TODO: GDP attribute classes -- protocol omission. */
	odt->gdp_attrs[i].number = 1;	/* TODO: don't hardcode */
	odt->gdp_attrs[i].attrs[0] = PATTR_LINE;	/* TODO: don't hardcode */
    }
    if ( ODD(odt->num_gdp) ) ++enums;
    assure(sizeof(Wst_gdp_attrs) % sizeof(*buf) == 0);
    buf += (odt->num_gdp * sizeof(Wst_gdp_attrs))/sizeof(*buf);

    odt->gdp3_ids = buf;
    odt->num_gdp3 = *((CARD32 *)enums);
    enums += sizeof(CARD32) / sizeof(*enums);
    odt->gdp3_attrs = (Wst_gdp_attrs *)(odt->gdp3_ids + odt->num_gdp3);
    for ( i = 0; i < odt->num_gdp3; i++, buf++, enums++ ) {
	odt->gdp3_ids[i] = *enums;
	/* TODO: GDP3 attribute classes -- protocol omission. */
	odt->gdp3_attrs[i].number = 1;	/* TODO: don't hardcode */
	odt->gdp3_attrs[i].attrs[0] = PATTR_LINE; /* TODO: don't hardcode*/
    }
    if ( ODD(odt->num_gdp3) ) ++enums;
    buf += (odt->num_gdp3 * sizeof(Wst_gdp_attrs))/sizeof(*buf);

    odt->gse_ids = buf;
    odt->num_gse = *((CARD32 *)enums); 
    enums += sizeof(CARD32) / sizeof(*enums);
    for ( i = 0; i < odt->num_gse; i++, buf++, enums++ )
	odt->gse_ids[i] = *enums;
    if ( ODD(odt->num_gse) ) ++enums;
    
    xdt->display_update_modes = buf;
    xdt->num_display_update_modes = *((CARD32 *)enums);
    enums += sizeof(CARD32) / sizeof(*enums);
    for ( i = 0; i < xdt->num_display_update_modes; i++, buf++, enums++ )
	xdt->display_update_modes[i] = *enums;
    if ( ODD(xdt->num_display_update_modes) ) ++enums;
    
    xdt->colour_approx_models = buf;
    xdt->num_colour_approx_models = *((CARD32 *)enums);
    enums += sizeof(CARD32) / sizeof(*enums);
    for ( i = 0; i < xdt->num_colour_approx_models; i++, buf++, enums++ )
	xdt->colour_approx_models[i] = *enums;
    if ( ODD(xdt->num_colour_approx_models) ) ++enums;
    
    xdt->pick_device_types = buf;
    xdt->num_pick_device_types = *((CARD32 *)enums);
    enums += sizeof(CARD32) / sizeof(*enums);
    for ( i = 0; i < xdt->num_pick_device_types; i++, buf++, enums++ )
	xdt->pick_device_types[i] = *enums;
    if ( ODD(xdt->num_pick_device_types) ) ++enums;
    
    xdt->pick_pets = buf;
    xdt->num_pick_pets = *((CARD32 *)enums);
    enums += sizeof(CARD32) / sizeof(*enums);
    for ( i = 0; i < xdt->num_pick_pets; i++, buf++, enums++ )
	xdt->pick_pets[i] = *enums;
    if ( ODD(xdt->num_pick_pets) ) ++enums;
    
    return buf;
}
#undef ODD


/* These have to stay in the same order as accessed below. */
static CARD16 all_idc_types[] = {
    PEXIDMaxEdgeWidth,
    PEXIDMaxLineWidth,
    PEXIDMaxMarkerSize,
    PEXIDMaxNonAmbientLights,
    PEXIDMaxNURBOrder,
    PEXIDMaxTrimCurveOrder,
    PEXIDMinEdgeWidth,
    PEXIDMinLineWidth,
    PEXIDMinMarkerSize,
    PEXIDNominalEdgeWidth,
    PEXIDNominalLineWidth,
    PEXIDNominalMarkerSize,
    PEXIDNumSupportedEdgeWidths,
    PEXIDNumSupportedLineWidths,
    PEXIDNumSupportedMarkerSizes,
    PEXIDChromaticityRedU,
    PEXIDChromaticityRedV,
    PEXIDLuminanceRed,
    PEXIDChromaticityGreenU,
    PEXIDChromaticityGreenV,
    PEXIDLuminanceGreen,
    PEXIDChromaticityBlueU,
    PEXIDChromaticityBlueV,
    PEXIDLuminanceBlue,
    PEXIDChromaticityWhiteU,
    PEXIDChromaticityWhiteV,
    PEXIDLuminanceWhite,
    PEXIDDitheringSupported,
    PEXIDBestColourApproximation,
    PEXIDTransparencySupported,
    PEXIDMaxNameSetNames
};
#define NUM_IDCS (sizeof(all_idc_types)/sizeof(all_idc_types[0])) 


static void
init_idc( wdt, idcs )
		Wst_dt		*wdt;
    register	CARD32		*idcs;
{
    register	Wst_output_wsdt	*odt = &wdt->phigs_dt.out_dt;

    /* explicit casts to type Pfloat aren't really necessary */  
    odt->max_edgewidth = (Pfloat)*idcs++;
    odt->max_linewidth = (Pfloat)*idcs++;
    odt->max_marker_size = (Pfloat)*idcs++;
    odt->max_light_src = *idcs++;
    odt->max_nurb_order = *idcs++;
    odt->max_trim_curve_order = *idcs++;
    odt->min_edgewidth = (Pfloat)*idcs++;
    odt->min_linewidth = (Pfloat)*idcs++;
    odt->min_marker_size = (Pfloat)*idcs++;
    odt->nominal_edgewidth = (Pfloat)*idcs++;
    odt->nominal_linewidth = (Pfloat)*idcs++;
    odt->nominal_marker_size = (Pfloat)*idcs++;
    odt->num_edgewidths = *idcs++;
    odt->num_linewidths = *idcs++;
    odt->num_marker_sizes = *idcs++;
    odt->chroma_info.flags.coefs_calculated = 0;
    /* cast CARD32s over to PEXFLOAT, 
	implicit cast to Pfloat on assignment */
    odt->chroma_info.xr = *((PEXFLOAT *)idcs);
    idcs += sizeof(PEXFLOAT)/sizeof(CARD32);
    odt->chroma_info.yr = *((PEXFLOAT *)idcs);
    idcs += sizeof(PEXFLOAT)/sizeof(CARD32);
    odt->chroma_info.Yr = *((PEXFLOAT *)idcs);
    idcs += sizeof(PEXFLOAT)/sizeof(CARD32);
    odt->chroma_info.xg = *((PEXFLOAT *)idcs);
    idcs += sizeof(PEXFLOAT)/sizeof(CARD32);
    odt->chroma_info.yg = *((PEXFLOAT *)idcs);
    idcs += sizeof(PEXFLOAT)/sizeof(CARD32);
    odt->chroma_info.Yg = *((PEXFLOAT *)idcs);
    idcs += sizeof(PEXFLOAT)/sizeof(CARD32);
    odt->chroma_info.xb = *((PEXFLOAT *)idcs);
    idcs += sizeof(PEXFLOAT)/sizeof(CARD32);
    odt->chroma_info.yb = *((PEXFLOAT *)idcs);
    idcs += sizeof(PEXFLOAT)/sizeof(CARD32);
    odt->chroma_info.Yb = *((PEXFLOAT *)idcs);
    idcs += sizeof(PEXFLOAT)/sizeof(CARD32);
    odt->chroma_info.xw = *((PEXFLOAT *)idcs);
    idcs += sizeof(PEXFLOAT)/sizeof(CARD32);
    odt->chroma_info.yw = *((PEXFLOAT *)idcs);
    idcs += sizeof(PEXFLOAT)/sizeof(CARD32);
    odt->chroma_info.Yw = *((PEXFLOAT *)idcs);
    idcs += sizeof(PEXFLOAT)/sizeof(CARD32);

    wdt->xwin_dt.dithering_supported = *idcs++;
    wdt->xwin_dt.best_colour_approx = *idcs++;
    wdt->xwin_dt.transparency_supported = *idcs++;
    wdt->xwin_dt.max_num_names_for_nameset = *idcs++;
}


static int
size_of_predefined_patterns( display, window, odt )
    Display		*display;
    Window		window;
    Wst_output_wsdt	*odt;
{
    caddr_t		pbuf;
    CARD16		count;
    int			size = 0;
    pexPatternEntry	*ppb;

    register	int		i;

    if ( PEXGetPredefinedEntries( display, window, PEXPatternLUT,
	    (pexTableIndex)odt->min_predef_pattern_index,
	    (CARD16)odt->num_pattern_types, &pbuf, &count ) ) {
	ppb = (pexPatternEntry *)pbuf;
	for ( i = 0; i < odt->num_pattern_types; i++, ppb++ ) {
	    size += ppb->numx * ppb->numy * sizeof(Pcoval);
	}
    }
    return size;
}

static int
init_table_sizes( display, window, wst )
   Display	*display;
   Window	window;
   Wst		*wst;
{
    int		size = 0;
    INT16	pmin, pmax;
    CARD16	max_entries, num_predef;

    register	Wst_output_wsdt	*odt = &wst->desc_tbl.phigs_dt.out_dt;

    /* Zero everything to avoid problems from inquiry failures. */
    odt->num_pattern_types = 0;
    odt->num_predefined_colours = 0;

    /* Limits */
    odt->num_polyline_bundle_entries = 0;
    odt->num_polymarker_bundle_entries = 0;
    odt->num_text_bundle_entries = 0;
    odt->num_interior_bundle_entries = 0;
    odt->num_edge_bundle_entries = 0;
    odt->num_pattern_table_entries = 0;
    odt->num_depth_cue_bundle_entries = 0;
    odt->num_light_src_bundle_entries = 0;
    odt->num_colour_indices = 0;
    odt->num_colr_mapping_entries = 0;
    odt->num_text_pairs[0] = 0;
    wst->desc_tbl.phigs_dt.num_view_indices = 0;

    if ( PEXGetTableInfo( display, window,
	PEXLineBundleLUT, &max_entries, &num_predef, &pmin, &pmax ) ) {
	odt->num_polyline_bundle_entries = max_entries;
	odt->min_predef_polyline_index = pmin;
	odt->max_predef_polyline_index = pmax;
    }
    if ( PEXGetTableInfo( display, window,
	PEXMarkerBundleLUT, &max_entries, &num_predef, &pmin, &pmax ) ) {
	odt->num_polymarker_bundle_entries = max_entries;
	odt->min_predef_polymarker_index = pmin;
	odt->max_predef_polymarker_index = pmax;
    }
    if ( PEXGetTableInfo( display, window,
	PEXTextBundleLUT, &max_entries, &num_predef, &pmin, &pmax ) ) {
	odt->num_text_bundle_entries = max_entries;
	odt->min_predef_text_index = pmin;
	odt->max_predef_text_index = pmax;
    }
    if ( PEXGetTableInfo( display, window,
	PEXTextFontLUT, &max_entries, &num_predef, &pmin, &pmax ) ) {
	/* num_text pairs is the number of fonts for now, but it's
	 * modified below in init_predef_data() to reflect the number of
	 * font pairs.
	 */
	odt->num_text_pairs[0] = num_predef;/* only number of fonts for now */
	odt->max_num_text_fonts = max_entries;
	odt->min_predef_text_font = pmin;
	odt->max_predef_text_font = pmax;
	/* Size has to account for all three precisions. */
	size += num_predef * 3 * sizeof(Ptext_font_prec);
    }
    if ( PEXGetTableInfo( display, window,
	PEXInteriorBundleLUT, &max_entries, &num_predef, &pmin, &pmax ) ) {
	odt->num_interior_bundle_entries = max_entries;
	odt->min_predef_interior_index = pmin;
	odt->max_predef_interior_index = pmax;
    }
    if ( PEXGetTableInfo( display, window,
	PEXEdgeBundleLUT, &max_entries, &num_predef, &pmin, &pmax ) ) {
	odt->num_edge_bundle_entries = max_entries;
	odt->min_predef_edge_index = pmin;
	odt->max_predef_edge_index = pmax;
    }
    if ( PEXGetTableInfo( display, window,
	PEXPatternLUT, &max_entries, &num_predef, &pmin, &pmax ) ) {
	odt->num_pattern_types = num_predef;
	odt->num_pattern_table_entries = max_entries;
	odt->min_predef_pattern_index = pmin;
	odt->max_predef_pattern_index = pmax;
	size += num_predef * sizeof(Ppat_rep_plus)
	    + size_of_predefined_patterns( display, window, odt );
    }
    if ( PEXGetTableInfo( display, window,
	PEXDepthCueLUT, &max_entries, &num_predef, &pmin, &pmax ) ) {
	odt->num_depth_cue_bundle_entries = max_entries;
	odt->min_predef_depth_cue_index = pmin;
	odt->max_predef_depth_cue_index = pmax;
    }
    if ( PEXGetTableInfo( display, window,
	PEXLightLUT, &max_entries, &num_predef, &pmin, &pmax ) ) {
	odt->num_light_src_bundle_entries = max_entries;
	odt->min_predef_light_src_index = pmin;
	odt->max_predef_light_src_index = pmax;
    }
    if ( PEXGetTableInfo( display, window,
	PEXViewLUT, &max_entries, &num_predef, &pmin, &pmax ) ) {
	wst->desc_tbl.phigs_dt.min_predef_view_index = pmin;
	wst->desc_tbl.phigs_dt.max_predef_view_index = pmax;
	wst->desc_tbl.phigs_dt.num_view_indices = max_entries;
	assure( max_entries >= WST_MIN_PREDEF_VIEW_REPS);
    }
    if ( PEXGetTableInfo( display, window,
	PEXColourLUT, &max_entries, &num_predef, &pmin, &pmax ) ) {
	odt->num_predefined_colours = num_predef;
	odt->num_colour_indices = max_entries;
	odt->min_predef_colour_index = pmin;
	odt->max_predef_colour_index = pmax;
	size += num_predef * sizeof(Pcolr_rep);
    }
    if ( PEXGetTableInfo( display, window,
	PEXColourApproxLUT, &max_entries, &num_predef, &pmin, &pmax ) ) {
	odt->num_colr_mapping_entries = max_entries;
	odt->min_predef_colr_mapping_index = pmin;
	odt->max_predef_colr_mapping_index = pmax;
    }

    return size;
}


static void
init_dynamics( wst, dyn )
   		Wst			*wst;
    register	pexGetDynamicsReply	*dyn;
{
    register	Wst_output_wsdt	*odt = &wst->desc_tbl.phigs_dt.out_dt;

    odt->view_rep = PEX_CONV_TO_Pmodtype(dyn->viewRep);
    odt->polyline_bundle_rep = PEX_CONV_TO_Pmodtype(dyn->lineBundle);
    odt->polymarker_bundle_rep = PEX_CONV_TO_Pmodtype(dyn->markerBundle);
    odt->text_bundle_rep = PEX_CONV_TO_Pmodtype(dyn->textBundle);
    odt->interior_bundle_rep = PEX_CONV_TO_Pmodtype(dyn->interiorBundle);
    odt->edge_bundle_rep = PEX_CONV_TO_Pmodtype(dyn->edgeBundle);
    odt->pattern_rep = PEX_CONV_TO_Pmodtype(dyn->patternTable);
    odt->colour_rep = PEX_CONV_TO_Pmodtype(dyn->colourTable);
    odt->ws_xform = PEX_CONV_TO_Pmodtype(dyn->wksTransform);
    odt->highlight_filter = PEX_CONV_TO_Pmodtype(dyn->highlightFilter);
    odt->invis_filter = PEX_CONV_TO_Pmodtype(dyn->invisibilityFilter);
    odt->hlhsr_mode = PEX_CONV_TO_Pmodtype(dyn->HlhsrMode);
    odt->struct_content_mod = PEX_CONV_TO_Pmodtype(dyn->structureModify);
    odt->post = PEX_CONV_TO_Pmodtype(dyn->postStructure);
    odt->unpost = PEX_CONV_TO_Pmodtype(dyn->unpostStructure);
    odt->struct_delete = PEX_CONV_TO_Pmodtype(dyn->deleteStructure);
    odt->ref_mod = PEX_CONV_TO_Pmodtype(dyn->referenceModify);
    odt->light_source_rep = PEX_CONV_TO_Pmodtype(dyn->lightTable);
    odt->dcue_rep = PEX_CONV_TO_Pmodtype(dyn->depthCueTable);
    odt->colour_mapping_rep = PEX_CONV_TO_Pmodtype(dyn->colourApproxTable);
}


static void
init_predef_data( display, window, wst, buf )
    Display	*display;
    Window	window;
    Wst		*wst;
    caddr_t	buf;
{
    caddr_t	pbuf;
    CARD16	count;

    register	int		i;
    register	Wst_output_wsdt	*odt = &wst->desc_tbl.phigs_dt.out_dt;

    if ( PEXGetPredefinedEntries( display, window,
	    PEXTextFontLUT, (pexTableIndex)odt->min_predef_text_font,
	    (CARD16)odt->num_text_pairs[0], &pbuf, &count ) ) {
	Ptext_font_prec	*fb = (Ptext_font_prec *)buf;
	pexFont		*pfb = (pexFont *)pbuf;
	odt->text_pairs[0] = fb;
	assure(odt->num_text_pairs[0] == count);
	count = odt->num_text_pairs[0];
	/* Number of text pairs is three per font.  Prior to this point the
	 * "num_text_pairs" field only indicates the number of fonts.
	 */
	odt->num_text_pairs[0] *= 3;
	for ( i = 0; i < count; i++, fb += 3, buf += 3 * sizeof(*fb) ) {
	    fb[0].font = fb[1].font = fb[2].font
		= odt->min_predef_text_font + i;
	    fb[0].prec = PPREC_STRING;
	    fb[1].prec = PPREC_STROKE;
	    fb[2].prec = PPREC_CHAR;
	}
    }
    if ( PEXGetPredefinedEntries( display, window,
	    PEXPatternLUT, (pexTableIndex)odt->min_predef_pattern_index,
	    (CARD16)odt->num_pattern_types, &pbuf, &count ) ) {
	Ppat_rep_plus	*pb = (Ppat_rep_plus *)buf;
	pexPatternEntry	*ppb = (pexPatternEntry *)pbuf;
	odt->pattern_types = pb;
	assure(odt->num_pattern_types == count);
	count = odt->num_pattern_types;
	for ( i = 0; i < count; i++, pb++, buf += sizeof(*pb) ) {
	    ppb = (pexPatternEntry *)
		phg_utx_pattern_entry_from_pex( ppb, pb );
	}
    }
    if ( PEXGetPredefinedEntries( display, window,
	    PEXColourLUT, (pexTableIndex)odt->min_predef_colour_index,
	    (CARD16)odt->num_predefined_colours, &pbuf, &count ) ) {
	Pgcolr			gcolr;
	Pcolr_rep		*cb = (Pcolr_rep *)buf;
	pexColourSpecifier	*pcb = (pexColourSpecifier *)pbuf;
	odt->default_colour_table = cb;
	assure(odt->num_predefined_colours == count);
	count = odt->num_predefined_colours;
	for ( i = 0; i < count; i++, cb++, buf += sizeof(*cb) ) {
	    pcb = (pexColourSpecifier *)
		phg_utx_colour_entry_from_pex( pcb, &gcolr );
		cb->rgb.red = gcolr.val.general.x;
		cb->rgb.green = gcolr.val.general.y;
		cb->rgb.blue = gcolr.val.general.z;
	}
    }
}



static void
update_default_pick_data( wst )
    Wst		*wst;
{
    Wst_input_wsdt	*in_ws_dt = &wst->desc_tbl.phigs_dt.in_dt;

    register	int		i, j;
    register	Wst_defpick	*pick;
    register	Wst_xwin_dt	*xdt = &wst->desc_tbl.xwin_dt;

    /* See if the server can actually support the desired pick devices.
     * Back them out of the WDT if it can't.
     * TODO: Current code assumes that at least pick device type 1 is
     * supported on the server if the server supports any pick device
     * types.
     */

    if ( xdt->num_pick_device_types <= 0 ) {
	in_ws_dt->num_devs.pick = 0;
	return;
    }

    pick = in_ws_dt->picks;
    for ( i = 0; i < in_ws_dt->num_devs.pick; i++, pick++ ) {
	/* Update all the PETS according to what's supported. */
	for ( j = 0; j < xdt->num_pick_pets && j < WST_MAX_NUM_PETS; j++ )
	    pick->pets[j] = xdt->pick_pets[j];
    }
}


static int
phg_wst_new_display( wst, display, visual, window )
    Wst		*wst;
    Display	*display;
    Visual	*visual;
    Window	window;
{
    CARD32			count, *counts, *idc_data;
    int				status = 0;
    caddr_t			buf;
    pexEnumTypeIndex		*enum_data;
    pexGetDynamicsReply		dynamics;

    register	int		size;
    register	Wst_xwin_dt	*xdt = &wst->desc_tbl.xwin_dt;

    /* TODO: Save the old info (if any) so it can be restored if this
     * routine fails.
     */
    wst->buffer_size = 0;
    xdt->display_name = NULL;

    /* Get all the PEX info that applies to WDT's. */
    size = strlen( DisplayString(display) ) + 1;
    if ( !(xdt->display_name = Malloc( size ) ) ) {
	ERR_BUF( wst->erh,ERR900 );
	goto abort;
    }
    xdt->display_name_length = size;
    (void)strcpy( xdt->display_name, DisplayString(display) );

    /* Determine availability of colour. */
    switch ( visual->class ) {
	case StaticColor:
	case PseudoColor:
	    wst->desc_tbl.phigs_dt.out_dt.colour_availability = PAVAIL_COLR;
	    wst->desc_tbl.phigs_dt.out_dt.num_colours = visual->map_entries;
	    break;
	case TrueColor:
	case DirectColor:
	    wst->desc_tbl.phigs_dt.out_dt.colour_availability = PAVAIL_COLR;
	    wst->desc_tbl.phigs_dt.out_dt.num_colours = visual->map_entries;
	    break;
	case StaticGray:
	case GrayScale:
	    wst->desc_tbl.phigs_dt.out_dt.colour_availability = PAVAIL_MONOCHR;
	    wst->desc_tbl.phigs_dt.out_dt.num_colours = visual->map_entries;
	    break;
	default:
	    wst->desc_tbl.phigs_dt.out_dt.colour_availability = PAVAIL_MONOCHR;
	    wst->desc_tbl.phigs_dt.out_dt.num_colours = visual->map_entries;
	    break;
    }

    /* Load Implementatation Dependent Constants. */
    if ( !PEXGetImpDepConstants( display, window,
	    NUM_IDCS, all_idc_types, (char **)&idc_data ) )
	/* TODO: Report the correct error. */
	goto abort;
    init_idc( &wst->desc_tbl, idc_data );

    /* Load dynamics. */
    if (!PEXGetDynamics( display, window, &dynamics ))
	/* TODO: Report the correct error. */
	goto abort;
    init_dynamics( wst, &dynamics );

    /* Get all the table and list sizes so we can do one large memory
     * allocation instead of a bunch of small ones.
     */
    if ( !PEXGetEnumeratedTypeInfo( display, window, (CARD32)NUM_ENUMS,
	    all_enum_types, (pexBitmask)0, &count, (char **)&counts )
	    || count != (CARD32)NUM_ENUMS )
	/* TODO: Report the correct error. */
	goto abort;

    size = init_enum_counts( wst, counts );
    size += init_table_sizes( display, window, wst );
    wst->buffer_size = size;
    if ( size > 0 && !(wst->buffer = calloc( 1, (unsigned)size )) )
	goto abort;

    /* Load Enumerated Type Info. */
    if ( !PEXGetEnumeratedTypeInfo( display, window, (CARD32)NUM_ENUMS,
	    all_enum_types, (pexBitmask)1, &count, (char **)&enum_data )
	    || count != (CARD32)NUM_ENUMS ) {
	/* TODO: Report the correct error. */
	goto abort;
    }
    buf = (caddr_t)init_enum_data( wst, enum_data, (Pint *)wst->buffer );
    init_predef_data( display, window, wst, buf );
    update_default_pick_data( wst );
    status = 1;

    return status;

abort:
    if ( xdt->display_name ) {
	free( xdt->display_name );
	xdt->display_name = NULL;
    }
    if ( wst->buffer_size > 0 ) {
	if ( wst->buffer )
	    free( wst->buffer );
	wst->buffer_size = 0;
    }

    return 0;
}


/* 
 * Get Attribute Utilities
 */

static caddr_t
get_phigs_desc_tbl_attr( wst, attr, arg )
    Wst			*wst;
    Phigs_ws_type_attr	attr;
    char		*arg;
{
    register Wst_phigs_dt	*dt;
    register caddr_t		status = NULL;

    dt = &wst->desc_tbl.phigs_dt;
    switch ( attr) {
	case PHG_WS_CATEGORY:
	    status = (caddr_t)dt->ws_category;
	    break;

	case PHG_BASE_NAME:
	    switch ( wst->base_type) {
		case WST_BASE_TYPE_X_TOOL:
		    status = (caddr_t)PHIGS_X_TOOL;
		    break;
		case WST_BASE_TYPE_X_DRAWABLE:
		    status = (caddr_t)PHIGS_X_DRAWABLE;
		    break;
	    }
	    break;
    }

    return status;
}

static caddr_t
get_xwin_attr( wst, attr, arg )
    Wst			*wst;
    Phigs_ws_type_attr	attr;
    char		*arg;
{
    register Wst_xwin_dt		*st;
    register caddr_t			status;

    st = &wst->desc_tbl.xwin_dt;
    switch ( attr ) {
	case PHG_X_DISPLAY_NAME:
	    status = (caddr_t)st->display_name;
	    break;
	case PHG_X_BUF_MODE:
	    status = (caddr_t)st->buffer_mode;
	    break;
	case PHG_X_HANDLE_EXPOSE:
	    status = (caddr_t)(st->flags.handle_expose ? TRUE : FALSE);
	    break;
	case PHG_X_HANDLE_DESTROY:
	    status = (caddr_t)(st->flags.handle_destroy ? TRUE : FALSE);
	    break;
	case PHG_DC_MODEL:
	    status = (caddr_t)st->dc_model;
	    break;
	case PHG_X_CMAP_PROP_ATOM:
	    status = (caddr_t)st->colormap_property_atom;
	    break;
	default:
	    status = get_phigs_desc_tbl_attr( wst, attr, arg );
	    break;
    }
    return status;
}

static caddr_t
get_xtool_attr( wst, attr, arg )
    Wst			*wst;
    Phigs_ws_type_attr	attr;
    char		*arg;
{
    register Wst_xtool_dt		*st;
    register caddr_t			status;

    st = &wst->desc_tbl.xwin_dt.tool;
    switch ( attr ) {
	case PHG_TOOL_X:
	    status = (caddr_t)st->x;
	    break;
	case PHG_TOOL_Y:
	    status = (caddr_t)st->y;
	    break;
	case PHG_TOOL_WIDTH:
	    status = (caddr_t)st->width;
	    break;
	case PHG_TOOL_HEIGHT:
	    status = (caddr_t)st->height;
	    break;
	case PHG_TOOL_LABEL:
	    status = (caddr_t)st->label;
	    break;
	case PHG_TOOL_ICON_LABEL:
	    status = (caddr_t)st->icon_label;
	    break;
	case PHG_TOOL_BORDER_WIDTH:
	    status = (caddr_t)st->border_width;
	    break;
	default:
	    status = get_xwin_attr( wst, attr, arg );
	    break;
    }
    return status;
}

caddr_t
phg_wst_get_attr( wst, attr, arg )
    Wst			*wst;
    Phigs_ws_type_attr	attr;
    char		*arg;
{
    register caddr_t	status;

    switch ( wst->base_type) {
	case WST_BASE_TYPE_X_TOOL:
	    status = get_xtool_attr( wst, attr, arg );
	    break;
	case WST_BASE_TYPE_X_DRAWABLE:
	    status = get_xwin_attr( wst, attr, arg );
	    break;
	default:
	    status = NULL;
	    break;
    }
    return status;
}


/*
 * Set Attribute Utilities
 */

static caddr_t
set_phigs_desc_tbl_attr( wst, args )
    Wst		*wst;
    char	**args;
{
    register Wst_phigs_dt	*dt = &wst->desc_tbl.phigs_dt;
    caddr_t			status = (caddr_t)wst;
    Pws_cat			category;

    switch ( (int)args[0]) {
	case PHG_WS_CATEGORY:
	    category = (Pws_cat)args[1];
	    /* Don't allow nonsensical changes. */
	    if ( dt->ws_category == PCAT_OUT || dt->ws_category == PCAT_OUTIN ) {
		if ( category == PCAT_OUTIN && !wst->flags.no_input_category )
		    dt->ws_category = category;
		else if ( category == PCAT_OUT )
		    dt->ws_category = category;
	    } break;
    }

    return status;
}

static void
clear_xwin_buffers( wst )
    Wst			*wst;
{
    if ( wst->desc_tbl.xwin_dt.display_name ) {
	free( wst->desc_tbl.xwin_dt.display_name );
	wst->desc_tbl.xwin_dt.display_name = NULL;
    }
    if ( wst->buffer ) {
	free( wst->buffer );
	wst->buffer = NULL;
    }
}

static caddr_t
set_xwin_attr( wst, args )
    Wst			*wst;
    char		**args;
{
    caddr_t			status = (caddr_t)wst;
    Display			*display;
    Window			window;
    Phg_pex_ext_info		pexinfo;
    XWindowAttributes		wattr;
    Pint			err;

    switch ( (int)args[0] ) {
	case PHG_X_DISPLAY_WINDOW:
	    display = (Display *)args[1];
	    if (!phg_utx_pex_supported( display, (Phg_pex_ext_info*)NULL )) {
		ERR_BUF( wst->erh, ERRN201 );
		break;
	    }
	    clear_xwin_buffers( wst );
	    window = (Window)args[2];
	    if ( !XGetWindowAttributes( display, window, &wattr ) ) {
		status = (caddr_t)NULL;
		ERR_BUF( wst->erh,ERRN303 );
	    } else if ( !phg_wst_new_display(wst,display,wattr.visual,window) )
		status = (caddr_t)NULL;
	    else {
		wst->desc_tbl.phigs_dt.dev_coords[0] = wattr.width;
		wst->desc_tbl.phigs_dt.dev_coords[1] = wattr.height;
		wst->desc_tbl.phigs_dt.dev_addrs_units[0] = wattr.width;
		wst->desc_tbl.phigs_dt.dev_addrs_units[1] = wattr.height;
		if ( wst->desc_tbl.phigs_dt.ws_category == PCAT_OUTIN )
		    adjust_echo_volumes( wst->desc_tbl.phigs_dt.dev_coords,
			&wst->desc_tbl.phigs_dt.in_dt );
	    }
	    break;
	case PHG_X_DISPLAY:
	    display = (Display *)args[1];
	    if ( !phg_utx_pex_supported( display, (Phg_pex_ext_info*)NULL ) ) {
		ERR_BUF( wst->erh, ERRN201 );
		break;
	    }
	    clear_xwin_buffers( wst );
	    if ( !phg_wst_new_display( wst, display,
		    DefaultVisual(display,DefaultScreen(display)),
		    DefaultRootWindow(display) ) )
		status = (caddr_t)NULL;
	    break;
	case PHG_X_DISPLAY_NAME:
	    clear_xwin_buffers( wst );
	    if ( !(display = phg_utx_open_pex_display( args[1], &pexinfo,
		    &err )) ) {
		status = (caddr_t)NULL;
		ERR_BUF( wst->erh, err );
	    } else if ( !phg_wst_new_display( wst, display,
			DefaultVisual(display,DefaultScreen(display)),
			DefaultRootWindow(display) ) ) {
		    status = (caddr_t)NULL;
	    }
	    break;

	case PHG_X_BUF_MODE:
	    wst->desc_tbl.xwin_dt.buffer_mode = (Pint)args[1];
	    break;

	case PHG_X_HANDLE_EXPOSE:
	    wst->desc_tbl.xwin_dt.flags.handle_expose =
		args[1] == (char *)TRUE ? 1 : 0;
	    break;

	case PHG_X_HANDLE_DESTROY:
	    wst->desc_tbl.xwin_dt.flags.handle_destroy =
		args[1] == (char *)TRUE ? 1 : 0;
	    break;

	case PHG_DC_MODEL:
	    wst->desc_tbl.xwin_dt.dc_model = (Phigs_DC_model)args[1];
	    break;

	case PHG_X_CMAP_PROP_ATOM:
	    wst->desc_tbl.xwin_dt.colormap_property_atom = (Atom)args[1];
	    break;

	default:
	    status = set_phigs_desc_tbl_attr( wst, args );
	    break;
    }

    return status;
}

static caddr_t
set_xtool_attrs( wst, avlist )
    Wst			*wst;
    Phg_attr_avlist	avlist;
{
    caddr_t		status = (caddr_t)wst;
    Wst_phigs_dt	*dt = &wst->desc_tbl.phigs_dt;

    register char		**args;
    register Wst_xtool_dt	*xtt = &wst->desc_tbl.xwin_dt.tool;

    for ( args = (char**)avlist; *args && status; args = phg_attr_next(args) ) {
        switch ( (int)args[0] ) {
            case PHG_TOOL_X:
		xtt->x = (int)args[1];
                break;
            case PHG_TOOL_Y:
		xtt->y = (int)args[1];
                break;
            case PHG_TOOL_WIDTH:
		xtt->width = (unsigned int)args[1];
		wst->desc_tbl.phigs_dt.dev_coords[0] = xtt->width;
		wst->desc_tbl.phigs_dt.dev_addrs_units[0] = xtt->width;
		if ( wst->desc_tbl.phigs_dt.ws_category == PCAT_OUTIN )
		    adjust_echo_volumes( wst->desc_tbl.phigs_dt.dev_coords,
			&wst->desc_tbl.phigs_dt.in_dt );
                break;
            case PHG_TOOL_HEIGHT:
		xtt->height = (unsigned int)args[1];
		wst->desc_tbl.phigs_dt.dev_coords[1] = xtt->height;
		wst->desc_tbl.phigs_dt.dev_addrs_units[1] = xtt->height;
		if ( wst->desc_tbl.phigs_dt.ws_category == PCAT_OUTIN )
		    adjust_echo_volumes( wst->desc_tbl.phigs_dt.dev_coords,
			&wst->desc_tbl.phigs_dt.in_dt );
                break;
	    case PHG_TOOL_LABEL:
		xtt->label[0] = '\0';
		if ( args[1] )
		    strncat( xtt->label, args[1], PHIGS_MAX_NAME_LEN );
		break;
	    case PHG_TOOL_ICON_LABEL:
		xtt->icon_label[0] = '\0';
		if ( args[1] )
		    strncat( xtt->icon_label, args[1], PHIGS_MAX_NAME_LEN );
		break;
	    case PHG_TOOL_BORDER_WIDTH:
		xtt->border_width = (unsigned int)args[1];
		break;

            default:
		status = set_xwin_attr( wst, args );
                break;
	}
    }
    return status;
}

caddr_t
phg_wst_set_attrs( wst, avlist )
    Wst			*wst;
    Phg_attr_avlist	avlist;
{
    caddr_t	status;

    switch ( wst->base_type) {
	case WST_BASE_TYPE_X_TOOL:
	case WST_BASE_TYPE_X_DRAWABLE:
	    status = set_xtool_attrs( wst, avlist );
	    break;

	default:
	    status = NULL;
	    break;
    }

    return status;
}


Wst*
phg_wst_create( erh, base, avlist )
    Err_handle		erh;
    Wst			*base;
    caddr_t		*avlist;	/* can be NULL */
{
    Wst			*wst;

    if ( !(wst = (Wst*)calloc( 1, sizeof(Wst) )) ) {
	ERR_BUF( erh, ERR900 );
    } else {
	wst->erh = erh;
	if ( !phg_wst_copy( base, wst ) ) {
	    ERR_BUF( erh, ERR900 );
	    free( (char*)wst);
	    wst = NULL;
	}

	if ( wst ) {
	    wst->bound_status = WST_UNBOUND;
	    if ( avlist && !phg_wst_set_attrs( wst, avlist ) ) {
		free( (char*)wst);
		wst = NULL;
	    }
	}
    }
    return wst;
}
