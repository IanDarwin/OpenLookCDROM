/* $XConsortium: bifbuild.c,v 5.5 94/04/17 20:44:20 rws Exp $ */
/*

Copyright (c) 1989, 1990, 1991  X Consortium

Permission is hereby granted, free of charge, to any person obtaining
a copy of this software and associated documentation files (the
"Software"), to deal in the Software without restriction, including
without limitation the rights to use, copy, modify, merge, publish,
distribute, sublicense, and/or sell copies of the Software, and to
permit persons to whom the Software is furnished to do so, subject to
the following conditions:

The above copyright notice and this permission notice shall be included
in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
IN NO EVENT SHALL THE X CONSORTIUM BE LIABLE FOR ANY CLAIM, DAMAGES OR
OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,
ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
OTHER DEALINGS IN THE SOFTWARE.

Except as contained in this notice, the name of the X Consortium shall
not be used in advertising or otherwise to promote the sale, use or
other dealings in this Software without prior written authorization
from the X Consortium.

*/
/***********************************************************
Copyright (c) 1989,1990, 1991 by Sun Microsystems, Inc.

						All Rights Reserved

Permission to use, copy, modify, and distribute this software and its
documentation for any purpose and without fee is hereby granted,
provided that the above copyright notice appear in all copies and that
both that copyright notice and this permission notice appear in
supporting documentation, and that the names of Sun Microsystems and
X Consortium not be used in advertising or publicity
pertaining to distribution of the software without specific, written
prior permission.

SUN MICROSYSTEMS DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE,
INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT
SHALL SUN MICROSYSTEMS BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL
DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,
WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION,
ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS
SOFTWARE.

******************************************************************/

/*--------------------------------------------------------------------*\
|
|  Copyright (C) 1989,1990, 1991, National Computer Graphics Association
|
|  Permission is granted to any individual or institution to use, copy, or
|  redistribute this software so long as it is not sold for profit, provided
|  this copyright notice is retained.
|
|                         Developed for the
|                National Computer Graphics Association
|                         2722 Merrilee Drive
|                         Fairfax, VA  22031
|                           (703) 698-9600
|
|                                by
|                 SimGraphics Engineering Corporation
|                    1137 Huntington Drive  Unit A
|                      South Pasadena, CA  91030
|                           (213) 255-0900
|---------------------------------------------------------------------
|
| Author        :	SimGraphics Engineering Corportation
|
| File          :	bifbuild.c
| Date          :	Fri Feb  9 10:46:55 PST 1990
| Project       :	PLB
| Description   :	Contain the BUILD/EXECUTE functions for turning
|			parser date into BIF entities and dealing
|			with them appropriately.
|                                                                    
| Status        :	Version 1.0
|
| Revisions     :	
|
|	4/4/89		JMZ SimGEC: Fixes to bif_matrix3,
|			bif_gtransform3, bif_ltransform3,
|			bif_clear, bif_rotate3
|
|	2/90		Staff SimGEC: Background color/colorindex
|
|       2/90            MFC Tektronix, Inc.: PEX-SI API implementation.
|                       
|       5/90            MFC Tektronix, Inc.: PEX-SI API Binding change.
|
|      12/90            MFC Tektronix, Inc.: PEX-SI PEX5R1 Release.
|
\*--------------------------------------------------------------------- */
    
/*--------------------------------------------------------------------*\
|	Table of Contents
|
|	int bif_mktype(BIF_INT)
|		:	Receive a MARKER_TYPE entity from the parser
|	int bif_linetype(BIF_INT)
|		:	Receive a LINE_TYPE entity from the parser
|	int bif_lineshading(BIF_INT)
|		:	Receive a LINE_TYPE entity from the parser
|	int bif_linewidth(BIF_REAL)
|		:	Receive a LINE_WIDTH entity from the parser
|	int bif_intstyle(BIF_INT)
|		:	Receive an INTERIOR_STYLE entity from the parser
|	int bif_intpatternindex(BIF_INT)
|		:	Receive an INTERIOR_PATTERN_INDEX entity from the
|	int bif_intshade(BIF_INT)
|		:	Receive an INTERIOR_SHADING entity from the parser
|	int bif_intlight(BIF_INT)
|		:	Receive an INTERIOR_LIGHTING entity from the parser
|	int bif_curve_approx_criteria (BIF_INT, BIF_REAL)
|		:	Receive an INTERIOR_LIGHTING entity from the parser
|	int bif_curve_approx_criteria (BIF_INT,BIF_REAL)
|		:	Receive an INTERIOR_LIGHTING entity from the parser
|	int bif_curve_approx_criteria (BIF_INT,BIF_REAL)
|		:	Receive an INTERIOR_LIGHTING entity from the parser
|	int bif_surfprop(BIF_REAL, BIF_REAL, BIF_REAL, BIF_REAL,
|				BIF_REAL, BIF_REAL, BIF_REAL, BIF_REAL)
|		:	Receive a SURFACE_PROPERTIES entity from the parser
|	int bif_bkfprop(BIF_REAL, BIF_REAL, BIF_REAL, BIF_REAL,
|				BIF_REAL, BIF_REAL, BIF_REAL, BIF_REAL)
|		:	Receive a BACKFACE_PROPERTIES entity from the parser
|	int bif_bkfprocessing(BIF_INT,BIF_INT)
|		:	Receive a BACK_FACE_PROCESSING entity from
|	int bif_edgeflag(BIF_INT)
|		:	Receive an EDGE_FLAG entity from the parser
|	int bif_edgetype(BIF_INT)
|		:	Receive an EDGE_TYPE entity from the parser
|	int bif_edgewidth(BIF_REAL)
|		:	Receive an EDGE_WIDTH entity from the parser
|	int bif_textfont(BIF_INT)
|		:	Receive a TEXT_FONT entity from the parser
|	int bif_textprec(BIF_INT)
|		:	Receive a TEXT_PREC entity from the parser
|	int bif_textpath(BIF_INT)
|		:	Receive a TEXT_PATH entity from the parser
|	int bif_textalign(BIF_INT, BIF_INT)
|		:	Receive a TEXT_ALIGN entity from the parser
|	int bif_charheight(BIF_REAL)
|		:	Receive a CHAR_HEIGHT entity from the parser
|	int bif_charexp(BIF_REAL)
|		:	Receive a CHAR_EXP entity from the parser
|	int bif_charspace(BIF_REAL)
|		:	Receive a CHAR_SPACE entity from the parser
|	int bif_charupvector(BIF_REAL, BIF_REAL)
|		:	Receive a CHAR_UP_VECTOR entity from the parser
|	int bif_annotextcharheight(BIF_REAL)
|		:	Receive a ANNO_TEXT_CHAR_HEIGHT entity from the parser
|	int bif_annotextcharupvector(BIF_REAL, BIF_REAL)
|		:	Receive a ANNO_TEXT_CHAR_UP_VECTOR entity from
|	int bif_annotextstyle(BIF_INT)
|		:	Receive a ANNO_TEXT_STYLE entity from the parser
|	int bif_definelight(BIF_INT)
|		:	Begin / End receiving a DEFINE_LIGHT entity from
|	int bif_lightbasic(BIF_INT, BIF_REAL, BIF_REAL, BIF_REAL)
|		:	Receive basic data for DEFINE_LIGHT entity from
|	int bif_ldtranform(BIF_INT)
|		:	Receive the matrix id of the Light Definition
|	int bif_lightoption(BIF_INT, BIF_REAL, BIF_REAL, BIF_REAL,
|				BIF_REAL, BIF_REAL, BIF_REAL, BIF_REAL,
|				BIF_REAL, BIF_REAL, BIF_REAL)
|		:	Receive data for DEFINE_LIGHT entity from the
|	int bif_lightstate(BIF_INT)
|		:	Begin / End receiving a LIGHT_STATE entity from
|	int bif_definedepthcue(BIF_INT, BIF_INT, BIF_REAL, BIF_REAL,
|				BIF_REAL, BIF_REAL, BIF_REAL, BIF_REAL,
|				BIF_REAL)
|		:	Receive the results of a DEFINE_DEPTHCUE
|	int bif_depthcueindex(BIF_INT)
|		:	Receive a DEPTHCUE_INDEX entity from the parser
|	int bif_hlhsremoval(BIF_INT)
|		:	Receive a HLHS_REMOVAL entity from the parser
|	int bif_definecolor(BIF_INT, BIF_REAL, BIF_REAL, BIF_REAL)
|		:	Receive a DEFINE_COLOR entity from the parser
|	bif_backgroundcolor(BIF_REAL,BIF_REAL,BIF_REAL)
|		:	Define the background true color components
|	bif_backgroundcolorindex(BIF_INT)
|		:	Define the background color index
|	int bif_identity3(BIF_INT)
|		:	Receive an IDENTITY3 entity from the parser
|	int bif_concatmatrix3(BIF_INT, BIF_INT, BIF_INT)
|		:	Receive a CONCAT_MATRIX3 entity from the parser
|	int bif_invertmatrix3(BIF_INT)
|		:	Receive an INVERT_MATRIX3 entity from the parser
|	int bif_rotate3( BIF_INT, BIF_REAL, BIF_INT, BIF_INT )
|		:	Receive a ROTATE3 entity from the parser
|	BIF_INT bif_rotatexyz3(BIF_INT,BIF_REAL,BIF_REAL, BIF_REAL,BIF_INT)
|		:	Receive a ROTATE_XYZ3 entity from the parser
|	int bif_translate3(BIF_INT , BIF_REAL, BIF_REAL, BIF_REAL, BIF_INT)
|		:	Receive a TRANSLATE3 entity from the parser
|	int bif_scale3(BIF_INT , BIF_REAL, BIF_REAL, BIF_REAL, BIF_INT)
|		:	Receive a SCALE3 entity from the parser
|	int bif_matrix3(BIF_INT, *BIF_REAL, BIF_INT)
|		:	Receive a MATRIX3 entity from the parser
|	int bif_getmatrix3(BIF_INT, BIF_INT, BIF_INT)
|		:	Receive a GET_MATRIX3 entity from the parser
|	int bif_pushmatrix3()
|		:	Receive a PUSH_MATRIX3 entity from the parser
|	int bif_popmatrix3()
|		:	Receive a POP_MATRIX3 entity from the parser
|	int bif_gtransform3(*BIF_REAL)
|		:	Receive a GLOBAL_TRANSFORMATION3 entity from the
|	int bif_ltransform3(*BIF_REAL, BIF_INT)
|		:	Receive a LOCAL_TRANSFORMATION3 entity from the
|	int bif_applytoglobal3(BIF_INT)
|		:	Receive an APPLY_TO_GLOBAL3 entity from the parser
|	int bif_applytolocal3(BIF_INT, BIF_INT)
|		:	Receive an APPLY_TO_LOCAL3 entity from the parser
|	int bif_cleargeom()
|		:	Receive a CLEAR_GEOMETRY entity from the parser.
|	int bif_reportfile(*char)
|		:	Receive a REPORT_FILE entity from the parser.
|	int bif_begintest(BIF_INT)
|		:	Receive a BEGIN_TEST entity from the parser
|	int bif_endtest()
|		:	Receive an END_TEST entity from the parser
|	int bif_pause()
|		:	Receive a PAUSE entity from the parser
|	int bif_sleep(BIF_INT)
|		:	Receive a SLEEP entity from the parser
|	int indexRange(int, int, int, int)
|		:	Check the supplied index against the range
|
\*--------------------------------------------------------------------*/
    
/*--------------------------------------------------------------------*\
|	Include files 
\*--------------------------------------------------------------------*/
#include <stdio.h>
#include "biftypes.h"
#include "bifbuild.h"
#include "new_ents.h"
#include "bifparse.h"
#include "db_tools.h"
#include "doentity.h"
#include "bifmacro.h"
#include "globals.h"
#include "ph_map.h"
#include "macfunct.h"
#include "brftypes.h"
#include "brfexption.h"
#include <X11/Xfuncs.h>
    
    
/*--------------------------------------------------------------------*\
|Local #define
\*--------------------------------------------------------------------*/
    
/*--------------------------------------------------------------------*\
| Local MACROS    
\*--------------------------------------------------------------------*/
    
/*--------------------------------------------------------------------*\
| External Symbols
\*--------------------------------------------------------------------*/
    void noop_function();
    
/*--------------------------------------------------------------------*\
| Local global variables
\*--------------------------------------------------------------------*/
    /* Useful statics */
    /* Temporary entity storage */
    static BIF_All temp_ent;
    static BIF_Begintest current_test;
    static BIF_Traverser_state temp_state;
    static BIF_Traverser_state *temp_state_ptr = &temp_state;
    static BRF_state brf_state;
    static int background_color_index = -1;
    
    /* The identity matrix */
    static float ident_matrix[4][4] =
{
    1., 0., 0., 0.,
    0., 1., 0., 0.,
    0., 0., 1., 0.,
    0., 0., 0., 1.,
};


/*--------------------------------------------------------------------*\
| BEGIN PROCEDURE CODE
\*--------------------------------------------------------------------*/

/* ****************************************
 * Primitives Attributes
 * ************************************** */

/* ***************** */
/* Marker Attributes */
/* ***************** */
/*----------------------------------------------------------------------*\
| Procedure	:	int bif_mktype(BIF_INT)
|------------------------------------------------------------------------|
| Description	:	Receive a MARKER_TYPE entity from the parser
|			pass it on to a generic build, store/execute
|			function.
|------------------------------------------------------------------------|
| Return	:	Error Code
\*----------------------------------------------------------------------*/
int bif_mktype(marker_type)
    BIF_INT marker_type;
{
    static entSize = sizeof(BIF_Index);
    /* Build, Store/Execute the entity */
    /* NOTE: + 1 to map BIF markertypes to PHIGs markertypes */
    bif_index((int)(marker_type+1),entSize,MARKER_TYPE,do_markertype,
	      pset_marker_type);
} /* End procedure bif_mktype */

/* MACRO-GENERATED FUNCTIONS */
/* APPROX: MARKER_SIZE integer multiples only */
    MF_BIF_SIZE(bif_mkscale,MARKER_SIZE,do_markersize,pset_marker_size)
    
    MF_TRUE_COLOR(bif_mkcolor,MARKER_COLOR,do_markercolor,pset_marker_colr)
    
    MF_MAP_INDEX(bif_mkcolorindex,MARKER_COLOR_INDEX,
		 do_markercolorindex,pset_marker_colr)
    
/* *************** */
/* Line Attributes */
/* *************** */
/*----------------------------------------------------------------------*\
| Procedure	:	int bif_linetype(BIF_INT)
|------------------------------------------------------------------------|
| Description	:	Receive a LINE_TYPE entity from the parser
|			pass it on to a generic build, store/execute
|			function.
|------------------------------------------------------------------------|
| Return	:	Error Code
\*----------------------------------------------------------------------*/
    int bif_linetype(line_type)
    BIF_INT line_type;
{
    static int entSize = sizeof(BIF_Index);
    /* Build, Store/Execute the entity */
    /* NOTE: + 1 to map BIF linetypes to PHIGs linetypes */
    bif_index((int)(line_type+1),entSize,LINE_TYPE,do_linetype,
	      pset_linetype);
    
} /* End procedure bif_linetype */

/*----------------------------------------------------------------------*\
| Procedure	:	int bif_lineshading(BIF_INT)
|------------------------------------------------------------------------|
| Description	:	Receive a LINE_TYPE entity from the parser
|			pass it on to a generic build, store/execute
|			function.
|------------------------------------------------------------------------|
| Return	:	Error Code
\*----------------------------------------------------------------------*/
int bif_lineshading(line_shade)
    BIF_INT line_shade;
{
    /* Build, Store/Execute the entity */
    /* NOTE: + 1 to map BIF lineshading to PHIGs lineshading  */
    bif_index((int)(line_shade+1),sizeof(BIF_Index),
	      LINE_SHADING,do_lineshading, pset_line_shad_meth);
    
} /* End procedure bif_lineshading */

/*----------------------------------------------------------------------*\
| Procedure	:	int bif_linewidth(BIF_REAL)
|------------------------------------------------------------------------|
| Description	:	Receive a LINE_WIDTH entity from the parser
|			pass it on to a generic build, store/execute
|			function.
|------------------------------------------------------------------------|
| Return	:	Error Code
\*----------------------------------------------------------------------*/
int bif_linewidth(line_width)
    BIF_REAL line_width;
{
    /* Build, Store/Execute the entity */
    bif_size((float)line_width,sizeof(BIF_Size),
	     LINE_WIDTH,do_linewidth,pset_linewidth);
    
#ifdef EXTERNALNOTE
    /* Alliant has a hardware problem with line widths > 1.
       The resultant line is deviod of any defined line type,
       and will no longer Z buffer. Since this is a temporary concern,
       merely issuing a warning to inform the user that what he sees 
       on the graphics device may not be a problem with his PLB file. This
       is a good example of 'catch all' exception handling! */
#endif
    
} /* End procedure bif_linewidth */

/* MACRO-GENERATED FUNCTIONS */
    MF_TRUE_COLOR(bif_linecolor,LINE_COLOR,do_linecolor,pset_line_colr)
    
    MF_MAP_INDEX(bif_linecolorindex,LINE_COLOR_INDEX,do_linecolorindex,
		 pset_line_colr)
    
/* ****************** */
/* Polygon Attributes */
/* ****************** */
/*----------------------------------------------------------------------*\
| Procedure	:	int bif_intstyle(BIF_INT)
|------------------------------------------------------------------------|
| Description	:	Receive an INTERIOR_STYLE entity from the parser
|			pass it on to a generic build, store/execute
|			function.
|------------------------------------------------------------------------|
| Return	:	Error Code
\*----------------------------------------------------------------------*/
    int bif_intstyle(style_index)
    BIF_INT style_index;
{
    int sty_index;
    /* Build, Store/Execute the entity */
    sty_index = REMAP_INTSTYLE(style_index);
    /* NONSUP: interior style PATTERN */
    /* Must also do backface interior style */
    bif_2_index(sty_index, sizeof(BIF_Index),
		INTERIOR_STYLE, do_interiorstyle,
		pset_int_style,
		pset_back_int_style);
} /* End procedure bif_intstyle */

/*----------------------------------------------------------------------*\
| Procedure	:	int bif_intpatternindex(BIF_INT)
|------------------------------------------------------------------------|
| Description	:	Receive an INTERIOR_PATTERN_INDEX entity from the
|			parser pass it on to a generic build,
|			store/execute function.
|------------------------------------------------------------------------|
| Return	:	Error Code
\*----------------------------------------------------------------------*/
int bif_intpatternindex(pattern_index)
    BIF_INT pattern_index;
{
    /* Build, Store/Execute the entity */
    /* Must also do back interior style index */
    bif_2_index((int)pattern_index, sizeof(BIF_Index),
		INTERIOR_PATTERN_INDEX, do_interiorpatternindex, 
		pset_int_style_ind,
		pset_back_int_style_ind);
    
} /* End procedure bif_intpatternindex */

/*----------------------------------------------------------------------*\
| Procedure	:	int bif_intshade(BIF_INT)
|------------------------------------------------------------------------|
| Description	:	Receive an INTERIOR_SHADING entity from the parser
|			pass it on to a generic build, store/execute
|			function.
|------------------------------------------------------------------------|
| Return	:	Error Code
\*----------------------------------------------------------------------*/
int bif_intshade(shade_type)
    BIF_INT shade_type;
{
    /* Build, Store/Execute the entity */
    /* BIF shade types match the values for the GX4000 */
    /* Must also do backface interior shading */
    bif_2_index((int)shade_type, sizeof(BIF_Index),
		INTERIOR_SHADING, do_interiorshading,
		pset_int_shad_meth,
		pset_back_int_shad_meth);

} /* End procedure bif_intshade */

/*----------------------------------------------------------------------*\
| Procedure	:	int bif_intlight(BIF_INT)
|------------------------------------------------------------------------|
| Description	:	Receive an INTERIOR_LIGHTING entity from the parser
|			pass it on to a generic build, store/execute
|			function.
|------------------------------------------------------------------------|
| Return	:	Error Code
\*----------------------------------------------------------------------*/
int bif_intlight(light_type)
    BIF_INT light_type;
{
    /* Build, Store/Execute the entity */
    /* BIF shade types match the values for the GX4000 */
    /* Must also do back interior reflectance eq */
    bif_2_index((int)light_type, sizeof(BIF_Index),
		INTERIOR_LIGHTING, do_interiorlighting,
		pset_refl_eqn,
		pset_back_refl_eqn);

} /* End procedure bif_intlight */

/*----------------------------------------------------------------------*\
| Procedure	:	int bif_curve_approx_criteria (BIF_INT, BIF_REAL)
|------------------------------------------------------------------------|
| Description	:	Receive an INTERIOR_LIGHTING entity from the parser
|			pass it on to a generic build, store/execute
|			function.
|------------------------------------------------------------------------|
| Return	:	Error Code
\*----------------------------------------------------------------------*/
bif_curve_approx_criteria ( icriteria, approx_value ) /* ver 1.0 */
    BIF_INT icriteria;
    BIF_REAL approx_value;
{
#ifdef TEST_PRINT
    printf(" bif_curve_approx_criteria: icriteria %ld, approx_value %f\n",
	   icriteria, approx_value);
#endif /* ifdef TEST_PRINT */
}

/*----------------------------------------------------------------------*\
| Procedure	:	int bif_curve_approx_criteria (BIF_INT,BIF_REAL)
|------------------------------------------------------------------------|
| Description	:	Receive an INTERIOR_LIGHTING entity from the parser
|			pass it on to a generic build, store/execute
|			function.
|------------------------------------------------------------------------|
| Return	:	Error Code
\*----------------------------------------------------------------------*/
bif_surface_approx_criteria ( icriteria, u_approx_value, v_approx_value )
    BIF_INT icriteria;						/* ver 1.0 */
    BIF_REAL u_approx_value;
    BIF_REAL v_approx_value;
{
#ifdef TEST_PRINT
    printf(" bif_trimcurve_approx_criteria : icriteria %ld, u_approx_value %f\n",
	   icriteria, u_approx_value);
    printf(" v_approx_value %f\n", v_approx_value);
#endif /* ifdef TEST_PRINT */
}

/*----------------------------------------------------------------------*\
| Procedure	:	int bif_curve_approx_criteria (BIF_INT,BIF_REAL)
|------------------------------------------------------------------------|
| Description	:	Receive an INTERIOR_LIGHTING entity from the parser
|			pass it on to a generic build, store/execute
|			function.
|------------------------------------------------------------------------|
| Return	:	Error Code
\*----------------------------------------------------------------------*/
bif_trimcurve_approx_criteria ( icriteria, approx_value )  /* ver 1.0 */
    BIF_INT icriteria;
    BIF_REAL approx_value;
{
#ifdef TEST_PRINT
    printf(" bif_trimcurve_approx_criteria : icriteria %ld, approx_value %f\n",
	   icriteria, approx_value);
#endif /* ifdef TEST_PRINT */
}




/* MACRO-GENERATED FUNCTIONS */
    MF_TRUE_COLOR(bif_intcolor,INTERIOR_COLOR,do_interiorcolor,pset_int_colr)
    
    MF_MAP_INDEX(bif_intcolorindex,INTERIOR_COLOR_INDEX,
		 do_interiorcolorindex, pset_int_colr)
    
    MF_MAP_INDEX(bif_bkfintcolorindex,BACKFACE_INTERIOR_COLOR_INDEX,
		 do_backfaceinteriorcolorindex, pset_back_int_colr)
    
    MF_TRUE_COLOR(bif_bkfintcolor, BACKFACE_INTERIOR_COLOR,
		  do_backfaceinteriorcolor, pset_back_int_colr)
    
/*----------------------------------------------------------------------*\
| Procedure	:	int bif_surfprop(BIF_REAL, BIF_REAL, BIF_REAL,
|					BIF_REAL, BIF_REAL, BIF_REAL,
|					BIF_REAL, BIF_REAL)
|------------------------------------------------------------------------|
| Description	:	Receive a SURFACE_PROPERTIES entity from the parser
|			pass it on to a generic build, store/execute
|			function.
|
|	ambient, diffuse, specular	Reflectance coefficents
|	c1, c2, c3			Specular color
|	highlight			Specular exponent
|	transparency			Transparency coefficient
|------------------------------------------------------------------------|
| Return	:	Error Code
\*----------------------------------------------------------------------*/
int bif_surfprop(ambient, diffuse, specular, c1, c2, c3,
		 highlight, transparency)
    BIF_REAL ambient, diffuse, specular, c1, c2, c3, highlight, transparency;
{
    static int ent_size = sizeof(BIF_Surfaceproperties);
    BIF_All *ent;
    
#ifdef TEST_PRINT
    printf("SURFACE_PROPERTIES :\n");
    printf("ambient, diffuse, specular %f %f %f\n",
	   ambient, diffuse, specular);
    printf("c1, c2, c3 %f %f %f\n",c1, c2, c3);
    printf("highlight, transparency %f %f\n",
	   highlight, transparency);
#endif /* TEST_PRINT */
#ifndef PRINT_ONLY
    /* Fill the temp_ent then allocate the entity */
    temp_ent.surfaceproperties.props.ambient_coef = ambient;
    temp_ent.surfaceproperties.props.diffuse_coef = diffuse;
    temp_ent.surfaceproperties.props.specular_coef = specular;
    
    temp_ent.surfaceproperties.props.specular_colr.type =
	wk_info.color_model;
    temp_ent.surfaceproperties.props.specular_colr.val.general.x = c1;
    temp_ent.surfaceproperties.props.specular_colr.val.general.y = c2;
    temp_ent.surfaceproperties.props.specular_colr.val.general.z = c3;
    temp_ent.surfaceproperties.props.specular_exp = highlight;
    
    ent = new_generic(&temp_ent,ent_size,SURFACE_PROPERTIES,
		      do_surfaceproperties);
    
    /* Error check for ent == NULL ( FATAL ) */
    ENT_ERROR(ent);
    
    /* Build or Execute */
    Traverse(traverser_state, ent);
    
#ifdef TEST_PRINT
    fprintf(stderr,"bifbuild pset_refl_props ambient = %f\n",
	    temp_ent.surfaceproperties.props.ambient_coef);
    fprintf(stderr,"bifbuild pset_refl_props diffuse = %f\n",
	    temp_ent.surfaceproperties.props.diffuse_coef);
    fprintf(stderr,"bifbuild pset_refl_props specular = %f\n",
	    temp_ent.surfaceproperties.props.specular_coef);
    fprintf(stderr,"bifbuild pset_refl_props color model = %d\n",
	    temp_ent.surfaceproperties.props.specular_colr.type);
    fprintf(stderr,"bifbuild pset_refl_props specularcolor[0] = %f\n",
	    temp_ent.surfaceproperties.props.specular_colr.val.general.x);
    fprintf(stderr,"bifbuild pset_refl_props specularcolor[1] = %f\n",
	    temp_ent.surfaceproperties.props.specular_colr.val.general.y);
    fprintf(stderr,"bifbuild pset_refl_props specularcolor[2] = %f\n",
	    temp_ent.surfaceproperties.props.specular_colr.val.general.z);
    fprintf(stderr,"bifbuild pset_refl_props highlight = %f\n",
	    temp_ent.surfaceproperties.props.specular_exp);
#endif /* TEST_PRINT */
    
    
    
    
#ifdef USING_PHIGS
    /* Call the entity in PHIGS */
    pset_refl_props(&ent->surfaceproperties.props);
#endif /* USING_PHIGS */
    
    /* Release Non-Retained Entities */
    Free_NRE(traverser_state, ent);
    
#endif /* PRINT_ONLY */
} /* End procedure bif_surfprop */

/*----------------------------------------------------------------------*\
| Procedure	:	int bif_bkfprop(BIF_REAL, BIF_REAL, BIF_REAL,
|					BIF_REAL, BIF_REAL, BIF_REAL,
|					BIF_REAL, BIF_REAL)
|------------------------------------------------------------------------|
| Description	:	Receive a BACKFACE_PROPERTIES entity from the parser
|			pass it on to a generic build, store/execute
|			function.
|
|	ambient, diffuse, specular	Reflectance coefficents
|	c1, c2, c3			Specular color
|	highlight			Specular exponent
|	transparency			Transparency coefficient
|------------------------------------------------------------------------|
| Return	:	Error Code
\*----------------------------------------------------------------------*/
int bif_bkfprop(ambient, diffuse, specular, c1, c2, c3,
		highlight, transparency)
    BIF_REAL ambient, diffuse, specular, c1, c2, c3, highlight, transparency;
{
    static int ent_size = sizeof(BIF_Surfaceproperties);
    BIF_All *ent;
    
#ifdef TEST_PRINT
    printf("BACKFACE_PROPERTIES :\n");
    printf("ambient, diffuse, specular %f %f %f\n",
	   ambient, diffuse, specular);
    printf("c1, c2, c3 %f %f %f\n",c1, c2, c3);
    printf("highlight, transparency %f %f %f\n",
	   highlight, transparency);
#endif /* TEST_PRINT */
#ifndef PRINT_ONLY
    /* Fill the temp_ent then allocate the entity */
    temp_ent.surfaceproperties.props.ambient_coef = ambient;
    temp_ent.surfaceproperties.props.diffuse_coef = diffuse;
    temp_ent.surfaceproperties.props.specular_coef = specular;
    
    temp_ent.surfaceproperties.props.specular_colr.type = 
	wk_info.color_model;
    temp_ent.surfaceproperties.props.specular_colr.val.general.x = c1;
    temp_ent.surfaceproperties.props.specular_colr.val.general.y = c2;
    temp_ent.surfaceproperties.props.specular_colr.val.general.z = c3;
    temp_ent.surfaceproperties.props.specular_exp = highlight;
    
    ent = new_generic(&temp_ent,ent_size,BACKFACE_PROPERTIES,
		      do_backfaceproperties);
    
    /* Error check for ent == NULL ( FATAL ) */
    ENT_ERROR(ent);
    
    /* Build or Execute */
    Traverse(traverser_state, ent);
    
#ifdef USING_PHIGS
    /* Call the entity in PHIGS */
    pset_back_refl_props(&ent->surfaceproperties.props);
#endif /* USING_PHIGS */
    
    /* Release Non-Retained Entities */
    Free_NRE(traverser_state, ent);
    
#endif /* PRINT_ONLY */
} /* End procedure bif_surfprop */

/*--------------------------------------------------------------------*\
| Procedure     :	int bif_bkfprocessing(BIF_INT,BIF_INT)
|---------------------------------------------------------------------
| Description	:	Receive a BACK_FACE_PROCESSING entity from
|			the parser pass it on to a generic build,
|			store/execute function.
|---------------------------------------------------------------------
| Return        :	
\*--------------------------------------------------------------------*/
int bif_bkfprocessing(ident,cull)
    BIF_INT ident, cull;
    
{
    static int ent_size = sizeof(BIF_Backfaceprocessing);
    BIF_All *ent;
#ifdef TEST_PRINT
    printf("BACKFACE_PROCESSING :\n");
    printf("identify, cull %d %d\n", ident, cull);
#endif /* TEST_PRINT */
#ifndef PRINT_ONLY
    /* Fill the temp_ent then allocate the entity */
    temp_ent.backfaceprocessing.g_mode = (Pdisting_mode)ident;
    temp_ent.backfaceprocessing.cull_mode = (Pcull_mode)cull;
    
    ent = new_generic(&temp_ent,ent_size,BACKFACE_PROCESSING,
		      do_backfaceprocessing);
    
    /* Error check for ent == NULL ( FATAL ) */
    ENT_ERROR(ent);
    
    /* Build or Execute */
    Traverse(traverser_state, ent);
    
#ifdef USING_PHIGS
    /* Call the entity in PHIGS */
    pset_face_disting_mode(ent->backfaceprocessing.g_mode);
    pset_face_cull_mode(ent->backfaceprocessing.cull_mode);
#endif /* USING_PHIGS */
    
    /* Release Non-Retained Entities */
    Free_NRE(traverser_state, ent);
    
#endif /* PRINT_ONLY */
} /* End bif_bkfprocessing() */

/*----------------------------------------------------------------------*\
| Procedure	:	int bif_edgeflag(BIF_INT)
|------------------------------------------------------------------------|
| Description	:	Receive an EDGE_FLAG entity from the parser
|			pass it on to a generic build,
|			store/execute function
|
|	ENABLE		Enable edge display
|	DISABLE		Disable edge display
|------------------------------------------------------------------------|
| Return	:	Error Code
\*----------------------------------------------------------------------*/
int bif_edgeflag(edge_flag)
    BIF_INT edge_flag;
{
    int edg_flag;
    /* Build, Store/Execute the entity */
    edg_flag = REMAP_EDGEFLAG(edge_flag);
    bif_index(edg_flag, sizeof(BIF_Index),
	      EDGE_FLAG,do_edgeflag,pset_edge_flag);
    
} /* End procedure bif_edgeflag */

/*----------------------------------------------------------------------*\
| Procedure	:	int bif_edgetype(BIF_INT)
|------------------------------------------------------------------------|
| Description	:	Receive an EDGE_TYPE entity from the parser
|			Set the line style for edges.  Pass it on to a
|			generic build, store/execute function.
|------------------------------------------------------------------------|
| Return	:	Error Code
\*----------------------------------------------------------------------*/
int bif_edgetype(edge_type)
    BIF_INT edge_type;
{
    /* Build, Store/Execute the entity */
    /* NONSUP: Any value other than solid (BIF value = 0) */
    /* NOTE: + 1 to map BIF edgetypes to PHIGs edgetypes */
    bif_index((int)(edge_type+1), sizeof(BIF_Index),
	      EDGE_TYPE,do_edgetype,pset_edgetype);
    
} /* End procedure bif_edgetype */

/*----------------------------------------------------------------------*\
| Procedure	:	int bif_edgewidth(BIF_REAL)
|------------------------------------------------------------------------|
| Description	:	Receive an EDGE_WIDTH entity from the parser
|			Pass it on to a generic build, store/execute
|			function.
|------------------------------------------------------------------------|
| Return	:	Error Code
\*----------------------------------------------------------------------*/
int bif_edgewidth(scale_factor)
    BIF_REAL scale_factor;
{
    /* Build, Store/Execute the entity */
    bif_size((float)scale_factor,sizeof(BIF_Size),
	     EDGE_WIDTH,do_edgewidth,pset_edgewidth);
    
} /* End procedure bif_edgewidth */

/* MACRO-GENERATED FUNCTIONS */
    MF_TRUE_COLOR(bif_edgecolor, EDGE_COLOR, do_edgecolor, pset_edge_colr)
    
    MF_MAP_INDEX(bif_edgecolorindex,EDGE_COLOR_INDEX,
		 do_edgecolorindex, pset_edge_colr)
    
    
/* *************** */
/* Text Attributes */
/* *************** */
/*----------------------------------------------------------------------*\
| Procedure	:	int bif_textfont(BIF_INT)
|------------------------------------------------------------------------|
| Description	:	Receive a TEXT_FONT entity from the parser
|			pass it on to a generic build, store/execute
|			function.
|------------------------------------------------------------------------|
| Return	:	Error Code
\*----------------------------------------------------------------------*/
    int bif_textfont(font_id)
    BIF_INT font_id;
{
    int fnt_id;
    /* Build, Store/Execute the entity */
    /* NONSUP: Only one font is supported (BIF font -2) */
    fnt_id = REMAP_TEXTFONT(font_id);
    bif_index(fnt_id, sizeof(BIF_Index),
	      TEXT_FONT,do_textfont,pset_text_font);
    
} /* End procedure bif_textfont */

/*----------------------------------------------------------------------*\
| Procedure	:	int bif_textprec(BIF_INT)
|------------------------------------------------------------------------|
| Description	:	Receive a TEXT_PREC entity from the parser
|			pass it on to a generic build, store/execute
|			function.
|
|	STRING	"String" Precision	Good
|	CHAR	"Character" Precision	Better
|	STROKE	"Stroke" Precision	Best
|------------------------------------------------------------------------|
| Return	:	Error Code
\*----------------------------------------------------------------------*/
int bif_textprec(precision)
    BIF_INT precision;
{
    /* Build, Store/Execute the entity */
    REMAP_TEXTPREC(precision);
    /* NONSUP: ONLY BIF_STROKE (BIF token STROKE) supported */
    bif_index((int)precision, sizeof(BIF_Index),
	      TEXT_PREC,do_textprec,pset_text_prec);
    
} /* End procedure bif_textprec */

/* MACRO-GENERATED FUNCTIONS */
    MF_TRUE_COLOR(bif_textcolor, TEXT_COLOR, do_textcolor, pset_text_colr)
    
    MF_MAP_INDEX(bif_textcolorindex,TEXT_COLOR_INDEX,
		 do_textcolorindex, pset_text_colr)
    
/*----------------------------------------------------------------------*\
| Procedure	:	int bif_textpath(BIF_INT)
|------------------------------------------------------------------------|
| Description	:	Receive a TEXT_PATH entity from the parser
|			pass it on to a generic build, store/execute
|			function.
|------------------------------------------------------------------------|
| Return	:	Error Code
\*----------------------------------------------------------------------*/
int bif_textpath(path_index)
    BIF_INT path_index;
{
    extern void fxsattxp();
    /* Build, Store/Execute the entity */
    /* NOTE: - 1 from the BIF values match the PHIGs enumerated values */
    bif_index((int)(path_index-1), sizeof(BIF_Index),
	      TEXT_PATH,do_textpath,fxsattxp);
    
} /* End procedure bif_textpath */

/*----------------------------------------------------------------------*\
| Procedure	:	int bif_textalign(BIF_INT, BIF_INT)
|------------------------------------------------------------------------|
| Description	:	Receive a TEXT_ALIGN entity from the parser
|			store/execute as appropriate.
|
|	horizontal_alignment	Horizontal just. see Table 3.3-10
|	vertical_alignment	Vertical   just. see Table 3.3-11
|------------------------------------------------------------------------|
| Return	:	Error Code
\*----------------------------------------------------------------------*/
int bif_textalign(horizontal_alignment, vertical_alignment)
    BIF_INT horizontal_alignment, vertical_alignment;
{
    static int ent_size = sizeof(BIF_Textalign);
    BIF_All *ent;
    
#ifdef TEST_PRINT
    printf("TEXT_ALIGN: Set to %d %d\n",horizontal_alignment,
	   vertical_alignment);
#endif /* TEST_PRINT */
#ifndef PRINT_ONLY
    /* Fill the temp_ent then allocate and copy the entity */
    /* BIF values match the PHIGs enumerated values */
    temp_ent.textalign.text_align.hor = (Phor_text_align)horizontal_alignment;
    temp_ent.textalign.text_align.vert = (Pvert_text_align)vertical_alignment;
    ent = new_generic(&temp_ent,ent_size,TEXT_ALIGN, do_textalign);
    
    /* Error check for ent == NULL ( FATAL ) */
    ENT_ERROR(ent);
    
    /* Build or Execute */
    Traverse(traverser_state, ent);
    
#ifdef USING_PHIGS
    /* Call the entity in PHIGS */
    /* MERGE: BIF merges the text3 and anno text3 attributes here */
    fxsattxal(&ent->textalign.text_align);
#endif /* USING_PHIGS */
    
    /* Release Non-Retained Entities */
    Free_NRE(traverser_state, ent);
    
#endif /* PRINT_ONLY */
} /* End procedure bif_textalign */

/*----------------------------------------------------------------------*\
| Procedure	:	int bif_charheight(BIF_REAL)
|------------------------------------------------------------------------|
| Description	:	Receive a CHAR_HEIGHT entity from the parser
|			pass it on to a generic build, store/execute
|			function.
|------------------------------------------------------------------------|
| Return	:	Error Code
\*----------------------------------------------------------------------*/
int bif_charheight(height)
    BIF_REAL height;
{
    /* Build, Store/Execute the entity */
    bif_size((float)height,sizeof(BIF_Size),
	     CHAR_HEIGHT,do_charheight,pset_char_ht);
    
} /* End procedure bif_charheight */

/*----------------------------------------------------------------------*\
| Procedure	:	int bif_charexp(BIF_REAL)
|------------------------------------------------------------------------|
| Description	:	Receive a CHAR_EXP entity from the parser
|			pass it on to a generic build, store/execute
|			function.
|------------------------------------------------------------------------|
| Return	:	Error Code
\*----------------------------------------------------------------------*/
int bif_charexp(exp)
    BIF_REAL exp;
{
    /* Build, Store/Execute the entity */
    bif_size((float)exp,sizeof(BIF_Size),
	     CHAR_EXP,do_charexp,pset_char_expan);
    
} /* End procedure bif_charexp */

/*----------------------------------------------------------------------*\
| Procedure	:	int bif_charspace(BIF_REAL)
|------------------------------------------------------------------------|
| Description	:	Receive a CHAR_SPACE entity from the parser
|			pass it on to a generic build, store/execute
|			function.
|------------------------------------------------------------------------|
| Return	:	Error Code
\*----------------------------------------------------------------------*/
int bif_charspace(space)
    BIF_REAL space;
{
    /* Build, Store/Execute the entity */
    bif_size((float)space,sizeof(BIF_Size),
	     CHAR_SPACE,do_charspace,pset_char_space);
    
} /* End procedure bif_charspace */

/*----------------------------------------------------------------------*\
| Procedure	:	int bif_charupvector(BIF_REAL, BIF_REAL)
|------------------------------------------------------------------------|
| Description	:	Receive a CHAR_UP_VECTOR entity from the parser
|
|	x y	Character up vector components
|------------------------------------------------------------------------|
| Return	:	Error Code
\*----------------------------------------------------------------------*/
int bif_charupvector(x, y)
    BIF_REAL x, y;
{
    static int ent_size = sizeof(BIF_Charupvector);
    BIF_All *ent;
    
#ifdef TEST_PRINT
    printf("CHAR_UP_VECTOR : Set to %f %f\n",x, y);
#endif /* TEST_PRINT */
#ifndef PRINT_ONLY
    /* Allocate the entity */
    temp_ent.charupvector.up_vect.delta_x = x;
    temp_ent.charupvector.up_vect.delta_y = y;
    ent = new_generic(&temp_ent,ent_size,CHAR_UP_VECTOR,
		      do_charupvector);
    
    /* Error check for ent == NULL ( FATAL ) */
    ENT_ERROR(ent);
    
    /* Build or Execute */
    Traverse(traverser_state, ent);
    
#ifdef USING_PHIGS
    /* Call the entity in PHIGS */
    pset_char_up_vec(&ent->charupvector.up_vect);
#endif /* USING_PHIGS */
    
    /* Release Non-Retained Entities */
    Free_NRE(traverser_state, ent);
    
#endif /* PRINT_ONLY */
} /* End procedure bif_charupvector */

/*----------------------------------------------------------------------*\
| Procedure	:	int bif_annotextcharheight(BIF_REAL)
|------------------------------------------------------------------------|
| Description	:	Receive a ANNO_TEXT_CHAR_HEIGHT entity from the parser
|			pass it on to a generic build, store/execute
|			function.
|------------------------------------------------------------------------|
| Return	:	Error Code
\*----------------------------------------------------------------------*/
int bif_annotextcharheight(height)
    BIF_REAL height;
{
    /* Build, Store/Execute the entity */
    bif_size((float)height,sizeof(BIF_Size),
	     ANNO_TEXT_CHAR_HEIGHT,
	     do_annotextcharheight,pset_anno_char_ht);
    
} /* End procedure bif_annotextcharheight */

/*----------------------------------------------------------------------*\
| Procedure	:	int bif_annotextcharupvector(BIF_REAL, BIF_REAL)
|------------------------------------------------------------------------|
| Description	:	Receive a ANNO_TEXT_CHAR_UP_VECTOR entity from
|			the parser.
|
|		x y	Character up vector components
|------------------------------------------------------------------------|
| Return	:	Error Code
\*----------------------------------------------------------------------*/
int bif_annotextcharupvector(x, y)
    BIF_REAL x, y;
{
    static int ent_size = sizeof(BIF_Charupvector);
    BIF_All *ent;
    
#ifdef TEST_PRINT
    printf("ANNO_TEXT_CHAR_UP_VECTOR : Set to %f %f\n",x, y);
#endif /* TEST_PRINT */
#ifndef PRINT_ONLY
    /* Allocate the entity */
    temp_ent.charupvector.up_vect.delta_x = x;
    temp_ent.charupvector.up_vect.delta_y = y;
    ent = new_generic(&temp_ent,ent_size,ANNO_TEXT_CHAR_UP_VECTOR,
		      do_annotextcharupvector);
    
    /* Error check for ent == NULL ( FATAL ) */
    ENT_ERROR(ent);
    
    /* Build or Execute */
    Traverse(traverser_state, ent);
    
#ifdef USING_PHIGS
    /* Call the entity in PHIGS */
    pset_anno_char_up_vec(&ent->charupvector.up_vect);
#endif /* USING_PHIGS */
    
    /* Release Non-Retained Entities */
    Free_NRE(traverser_state, ent);
    
#endif /* PRINT_ONLY */
} /* End procedure bif_annotextcharupvector */

/*----------------------------------------------------------------------*\
| Procedure	:	int bif_annotextstyle(BIF_INT)
|------------------------------------------------------------------------|
| Description	:	Receive a ANNO_TEXT_STYLE entity from the parser
|			pass it on to a generic build, store/execute
|			function.
|------------------------------------------------------------------------|
| Return	:	Error Code
\*----------------------------------------------------------------------*/
int bif_annotextstyle(style)
    BIF_INT style;
{
    /* Build, Store/Execute the entity */
    /* BIF anno types match the values for the GX4000 */
    bif_index((int)style, sizeof(BIF_Index),
	      ANNO_TEXT_STYLE,do_annotextstyle,pset_anno_style);
    
} /* End procedure bif_annotextstyle */

/* ******************** */
/* Rendering Attributes */
/* ******************** */

/* ****************************** */
/* Lights and lighting Attributes */
/* ****************************** */

/*----------------------------------------------------------------------*\
| Procedure	:	int bif_definelight(BIF_INT)
|------------------------------------------------------------------------|
| Description	:	Begin / End receiving a DEFINE_LIGHT entity from
|			the parser
|	BIF_P_BEGIN 	begin entity
|	BIF_P_END	end entity
|------------------------------------------------------------------------|
| Return	:	Error Code
\*----------------------------------------------------------------------*/
int bif_definelight(begin_or_end)
    BIF_INT begin_or_end;
{
    static int ent_size = sizeof(BIF_Definelight);
    BIF_All *ent;
    int ierr;
#ifdef TEST_PRINT
    BEGEND(definelight);
    fflush(stdout);
#endif /* TEST_PRINT */
#ifndef PRINT_ONLY
    switch ( begin_or_end )
    {
      case BIF_P_BEGIN :
	  /* Initialize entity */
	  temp_ent.definelight.ld_trans_id = -1; /* No transform */
	  /* default light type is ambient */
	  temp_ent.definelight.light_type = BIF_AMBIENT;
	  break;
	  
	case BIF_P_END :
	    /* Check / Limit / Warn / Index Range Exception */
	    ierr = indexRange(DEFINE_LIGHT,
			      (BIF_INT)temp_ent.definelight.light_number,
			      1, BIF_MAX_LIGHTS);
	  
	    if ( ierr ) /* Substitute default value for error condition */
		temp_ent.definelight.light_number = 1;

	    /* Allocate the entity */
	    ent = new_generic(&temp_ent, ent_size,
			      DEFINE_LIGHT, do_definelight);

	    /* Error check for ent == NULL ( FATAL ) */
	    ENT_ERROR(ent);
	      
	    /* Build or Execute */
	    Traverse(traverser_state, ent);
	      
#ifdef USING_PHIGS
	    /* Called by the BIF execute traverser */
	    /* No PHIGS call here */
#endif /* USING_PHIGS */
	      
	    /* Release Non-Retained Entities */
	    Free_NRE(traverser_state, ent);
      }
    
    
#endif /* PRINT_ONLY */
} /* End procedure bif_definelight */

/*----------------------------------------------------------------------*\
| Procedure	:	int bif_lightbasic(BIF_INT,
|					BIF_REAL, BIF_REAL, BIF_REAL)
|------------------------------------------------------------------------|
| Description	:	Receive basic data for DEFINE_LIGHT entity from
|			the parser.
|	light_number	Light table entry to define
|	c1, c2, c3	Light color
|------------------------------------------------------------------------|
| Return	:	Error Code
\*----------------------------------------------------------------------*/
int bif_lightbasic(light_number, c1, c2, c3)
    BIF_INT light_number;
    BIF_REAL c1, c2, c3;
{
    
#ifdef TEST_PRINT
    printf("Lightbasic: for %d color %f %f %f \n",light_number, c1, c2, c3);
#endif /* TEST_PRINT */
#ifndef PRINT_ONLY
    temp_ent.definelight.light_number   = (int)light_number;
    temp_ent.definelight.color_model    = wk_info.color_model;
    temp_ent.definelight.light_color[0] = (float)c1;
    temp_ent.definelight.light_color[1] = (float)c2;
    temp_ent.definelight.light_color[2] = (float)c3;
#endif /* PRINT_ONLY */
    
} /* End bif_lightbasic */

/*----------------------------------------------------------------------*\
| Procedure	:	int bif_ldtranform(BIF_INT)
|------------------------------------------------------------------------|
| Description	:	Receive the matrix id of the Light Definition
|			transformation.
|
|	mat_id		Matrix table entry to define the defining coord.
|			space for the current light;
|------------------------------------------------------------------------|
| Return	:	Error Code
\*----------------------------------------------------------------------*/
int bif_ldtransform(mat_id)
    BIF_INT mat_id;
{
    
#ifdef TEST_PRINT
    printf("Ldtransform: Matrix id %d\n",mat_id);
#endif /* TEST_PRINT */
    
#ifndef PRINT_ONLY
    ERROR_MATRIX_ID(mat_id,DEFINE_LIGHT);
    temp_ent.definelight.ld_trans_id = mat_id;
#endif /* PRINT_ONLY */
    
} /* End new_ldtransform */

/*----------------------------------------------------------------------*\
| Procedure	:	int bif_lightoption(BIF_INT,
|					BIF_REAL, BIF_REAL, BIF_REAL,
|					BIF_REAL, BIF_REAL, BIF_REAL,
|					BIF_REAL, BIF_REAL,
|					BIF_REAL, BIF_REAL)
|------------------------------------------------------------------------|
| Description	:	Receive data for DEFINE_LIGHT entity from the
|			parser that defines non-ambient lights
|
|	light_type	Type of light defined
|	x, y, z		Light position
|	u, v, w		Light direction
|	e		Concentration exponent			
|	s		Spread angle
|	a, b		Attenuation coefficients
|------------------------------------------------------------------------|
| Return	:	Error Code
\*----------------------------------------------------------------------*/
int bif_lightoption(light_type, x , y , z , u , v , w , e , s , a , b)
    BIF_INT light_type;
    BIF_REAL x, y, z;
    BIF_REAL u, v, w;
    BIF_REAL e;
    BIF_REAL s;
    BIF_REAL a, b;
{
#ifdef TEST_PRINT
    printf("Lightoption : Light type %d\n",(int)light_type);
    printf("Position  %f %f %f \n",x, y, z);
    printf("Direction %f %f %f \n",u, v, w);
    printf("Expon Spread %f %f \n",e, s);
    printf("Attenuation  %f %f \n",a, b);
#endif /* TEST_PRINT */
#ifndef PRINT_ONLY
    /* NONSUP: Attenuation Factors (BIF tokens POSITIONAL, SPOT) */
    temp_ent.definelight.light_type   = REMAP_LIGHTTYPE(light_type);
    temp_ent.definelight.direction[0] = u;
    temp_ent.definelight.direction[1] = v;
    temp_ent.definelight.direction[2] = w;
    temp_ent.definelight.position[0]  = x;
    temp_ent.definelight.position[1]  = y;
    temp_ent.definelight.position[2]  = z;
    temp_ent.definelight.attenuation[0] = a;
    temp_ent.definelight.attenuation[1] = b;
    temp_ent.definelight.exponent = e;
    temp_ent.definelight.spread = s;
    
#endif /* PRINT_ONLY */
} /* End procedure bif_lightoption */


/*----------------------------------------------------------------------*\
| Procedure	:	int bif_lightstate(BIF_INT)
|------------------------------------------------------------------------|
| Description	:	Begin / End receiving a LIGHT_STATE entity from
|			the parser.
|
|	BIF_P_BEGIN 	begin entity
|			Initialize group handler for data groups ACTIVATE_LIST
|			and DEACTIVATE_LIST
|
|	BIF_P_END	end entity
|			Store accumulated lists in entity structure.
|------------------------------------------------------------------------|
| Return	:	Error Code
\*----------------------------------------------------------------------*/
int bif_lightstate(begin_or_end)
    BIF_INT begin_or_end;
{
    BIF_Lightstate *ent;
    Group_description *group_list;
    int num_groups;
    
#ifdef TEST_PRINT
    BEGEND(lightstate );
#endif /* TEST_PRINT */
#ifndef PRINT_ONLY
    switch (begin_or_end) {
      case BIF_P_BEGIN:
	/* Start of a lightstate */
	init_groups();
	break;
	
      case BIF_P_END:
	num_groups = end_groups(&group_list);
	ent = new_lightstate(num_groups, group_list);
	ENT_ERROR(ent);
	
	/* Store or execute */		
	Traverse(traverser_state, ent);
	
	/* Send to PHIGs */
#ifdef USING_PHIGS
#ifdef TEST_PRINT
	{
	    int i;
	    for ( i = 0 ;  i < ent->activation.num_ints ; i++ )
		printf("Switching on  Light # %d\n", ent->activation.ints[i]);
	    for ( i = 0 ;  i < ent->deactivation.num_ints ; i++ )
		printf("Switching off Light # %d\n", ent->deactivation.ints[i]);
	}
#endif /* TEST_PRINT */
	pset_light_src_state(&ent->activation,&ent->deactivation);
#endif /* USING_PHIGS */
	Free_NRE(traverser_state, ent);
    }
#endif /* PRINT_ONLY */
} /* End procedure bif_lightstate */

/* ********************** */
/* Depth Cueing           */
/* ********************** */
/*--------------------------------------------------------------------*\
| Procedure     :	int bif_definedepthcue(BIF_INT, BIF_INT,
|				BIF_REAL, BIF_REAL,
|				BIF_REAL, BIF_REAL,
|				BIF_REAL, BIF_REAL, BIF_REAL)
|---------------------------------------------------------------------
| Description   :	Receive the results of a DEFINE_DEPTHCUE
|			from the parser.
|---------------------------------------------------------------------
| Return        :	Error Code (Not Implemented)
\*--------------------------------------------------------------------*/
int bif_definedepthcue(entry_id, flag, near, far, nScale, fScale, 
		       c1, c2,c3)
    
    BIF_INT entry_id;
    BIF_INT flag;
    BIF_REAL near, far;
    BIF_REAL nScale, fScale;
    BIF_REAL c1, c2,c3;
    
{
    static int ent_size = sizeof(BIF_DefineDepthCue);
    BIF_All *ent;
    BIF_DefineDepthCue *dent;
    int ierr;
#ifdef TEST_PRINT
    char *key_w, *find_keyword_token(); 
    
    key_w = find_keyword_token(flag);
    printf("DEFINE_DEPTHCUE: Entry %d (%s)\n", entry_id, key_w);
#endif /* TEST_PRINT */
#ifndef PRINT_ONLY
    dent = &temp_ent.definedepthcue;
    dent->ind               = (int)entry_id;
    dent->rep.mode          = (Pdcue_mode)REMAP_DCMODE(flag);
    dent->rep.ref_planes[0] = (Pfloat)far;     /* back plane in npc  */
    dent->rep.ref_planes[1] = (Pfloat)near;    /* front plane in npc */
    dent->rep.scaling[0]   = (Pfloat)fScale;  /* back scale factor  */
    dent->rep.scaling[1]   = (Pfloat)nScale;  /* front scale factor */
    dent->rep.colr.type   = wk_info.color_model;
    dent->rep.colr.val.general.x = c1;
    dent->rep.colr.val.general.y = c2;
    dent->rep.colr.val.general.z = c3;
    
    /* Check / Limit / Warn / Index Range Exception */
    ierr = indexRange(DEFINE_DEPTHCUE, (BIF_INT)dent->ind,
		      1, BIF_MAX_DCRS);
    
    if ( ierr ) /* Substitute default value */
	dent->ind = 0;

    /* Allocate the entity */
    ent = new_generic(&temp_ent, ent_size,
		      DEFINE_DEPTHCUE, do_definedepthcue);
	
    /* Error check for ent == NULL ( FATAL ) */
    ENT_ERROR(ent);

    /* Build or Execute */
    Traverse(traverser_state, ent);
	
#ifdef USING_PHIGS
    /* Called by the BIF execute traverser */
    /* No PHIGS call here */
#endif /* USING_PHIGS */
	
    /* Release Non-Retained Entities */
    Free_NRE(traverser_state, ent);
#endif /* PRINT_ONLY */
} /* End of procedure bif_definedepthcue */

/*----------------------------------------------------------------------*\
| Procedure	:	int bif_depthcueindex(BIF_INT)
|------------------------------------------------------------------------|
| Description	:	Receive a DEPTHCUE_INDEX entity from the parser
|
|	dc_index	the index of the depthcue entry
|			to make the current entry.
|------------------------------------------------------------------------|
| Return	:	Error Code
\*----------------------------------------------------------------------*/
int bif_depthcueindex(dc_index)
    BIF_INT dc_index;
{
    int ierr;
    /* Check / Limit / Warn / Index Range Exception */
    ierr = indexRange(DEFINE_DEPTHCUE, dc_index,
		      1, BIF_MAX_DCRS);
    
    if ( ierr ) /* Substitute default value */
	dc_index = 0;

    /* Build, Store/Execute the entity */
    bif_index((int)dc_index, sizeof(BIF_Index),
	      DEPTHCUE_INDEX, do_depthcueindex, pset_dcue_ind);
    
} /* End procedure bif_depthcueindex */

/* ********************** */
/* Hidden Surface Removal */
/* ********************** */
/*----------------------------------------------------------------------*\
  | Procedure	:	int bif_hlhsremoval(BIF_INT)
  |------------------------------------------------------------------------|
  | Description	:	Receive a HLHS_REMOVAL entity from the parser
  |			pass it on to a generic build, store/execute
  |			function.
  |
  |	hlhs_flag	HL/HS switch; see Table 3.4-1
  |			BIF_DISABLE		Disable HL/HS removal
  |			BIF_ENABLE		Enable HL/HS removal
  |------------------------------------------------------------------------|
  | Return	:	Error Code
  \*----------------------------------------------------------------------*/
int bif_hlhsremoval(hlhs_flag)
    BIF_INT hlhs_flag;
{
    int hl_flag;
    /* Build, Store/Execute the entity */
    hl_flag = REMAP_HLHS(hlhs_flag);
    bif_index(hl_flag, sizeof(BIF_Index),
	      HLHS_REMOVAL, do_hlhsremoval, pset_hlhsr_id);
    
} /* End procedure bif_hlhsremoval */

/* ********************** */
/* Color Model Attributes */
/* ********************** */

/*----------------------------------------------------------------------*\
| Procedure	:	int bif_definecolor(BIF_INT,
|					BIF_REAL, BIF_REAL, BIF_REAL)
|------------------------------------------------------------------------|
| Description	:	Receive a DEFINE_COLOR entity from the parser
|
|	map_index	Color map table entry to define
|	c1, c2, c3	True-color components for entry
|------------------------------------------------------------------------|
| Return	:	Error Code
\*----------------------------------------------------------------------*/
int bif_definecolor(map_index, c1, c2, c3)
    BIF_INT map_index;
    BIF_REAL c1, c2, c3;
{
    static int ent_size = sizeof(BIF_Definecolor);
    BIF_All *ent;
    int ierr;
    
#ifdef TEST_PRINT
    printf("DEFINE_COLOR: Color %d set to %f %f %f\n",
	   map_index, c1, c2, c3);
#endif /* TEST_PRINT */
#ifndef PRINT_ONLY
    /* Allocate the entity */
    /* indexRange reporting for definecolor */
    ierr = indexRange(DEFINE_COLOR, map_index,
		      0, (BIF_INT)wk_info.cmap_size);
    if ( ierr ) /* Substitute default value */
	map_index = 1;

    temp_ent.definecolor.ind = map_index;
    temp_ent.definecolor.color_model = wk_info.color_model;
    temp_ent.definecolor.rep.rgb.red      = c1;
    temp_ent.definecolor.rep.rgb.green      = c2;
    temp_ent.definecolor.rep.rgb.blue      = c3;
    ent = new_generic(&temp_ent,ent_size,
		      DEFINE_COLOR,do_definecolor);
	
    /* Error check for ent == NULL ( FATAL ) */
    ENT_ERROR(ent);
	
    /* Build or Execute */
    Traverse(traverser_state, ent);
	
#ifdef USING_PHIGS
    /*----------------------------------------------------*\
    | If we are redefining the entry that defines
    | the background we must ALSO redfine the
    | background.
    \*----------------------------------------------------*/
    if ( map_index == background_color_index )
	bif_backgroundcolorindex(map_index);
	
    /* Cannot be contained within PHIGS structures */
    /* Called by the BIF execute traverser */
    /* No PHIGS call here */
#endif /* USING_PHIGS */
	
    /* Release Non-Retained Entities */
    Free_NRE(traverser_state, ent);
    
#endif /* PRINT_ONLY */
} /* End procedure bif_definecolor */

/*--------------------------------------------------------------------*\
| Procedure     :	bif_backgroundcolor(BIF_REAL,BIF_REAL,BIF_REAL)
|---------------------------------------------------------------------
| Description   :	Define the background true color components
|---------------------------------------------------------------------
| Return        :	
\*--------------------------------------------------------------------*/
bif_backgroundcolor(c1, c2, c3)
    BIF_REAL c1, c2, c3;
    
{/* bif_backgroundcolor */
    float comp1, comp2, comp3;
    comp1 = (float)c1;
    comp2 = (float)c2;
    comp3 = (float)c3;
    
    /*------------------------------------------------------------*\
    | Disconnect background from the color map
    \*------------------------------------------------------------*/
    background_color_index = -1;
#ifdef USING_PHIGS
    /*------------------------------------------------------------*\
    | We set the background color by redefining map entry 0
    \*------------------------------------------------------------*/
    {
	Pcolr_rep rep;
	
	if (user_background) {
	    PLB_EXCEPTION(BIF_EX_BACKCOLOR);
	}
	else {
	    rep.rgb.red = comp1;
	    rep.rgb.green = comp2;
	    rep.rgb.blue = comp3;
	    pset_colr_rep((Pint)bench_setup.workid, 0, &rep);
	}
    }
#endif /* USING_PHIGS */
}/* bif_backgroundcolor */


/*--------------------------------------------------------------------*\
| Procedure     :	bif_backgroundcolorindex(BIF_INT)
|---------------------------------------------------------------------
| Description   :	Define the background color index
|---------------------------------------------------------------------
| Return        :	
\*--------------------------------------------------------------------*/
bif_backgroundcolorindex(lindex)
    BIF_INT lindex;
    
{/* bif_backgroundcolorindex */
    int indx;
    int type;
    int errind;
    float comp1, comp2, comp3;
    indx = (int)lindex;
    
    if ( indexRange(BACKGROUND_COLOR_INDEX, indx,
		    0, wk_info.cmap_size ) ) /* Substitute default value */
	indx = 1;

    background_color_index = indx;
#ifdef USING_PHIGS
    {
	Pcolr_rep rep;
	Pint errind;
	    
	if (user_background) {
	    PLB_EXCEPTION(BIF_EX_BACKCOLOR);
	}
	else {
	    /*----------------------------------------------------*\
	    | Offset the map index "as usual" with PHIGS/BIF
	    \*----------------------------------------------------*/
	    indx++;
	    /*----------------------------------------------------*\
	    |	Get the color we are "into"
	    \*----------------------------------------------------*/
	    
	    pinq_colr_rep((Pint)bench_setup.workid, (Pint)indx,
			  PINQ_SET, &errind, &rep);
	    
	    /*----------------------------------------------------*\
	    |	We set the background color using map entry 0
	    \*----------------------------------------------------*/
	    
	    pset_colr_rep((Pint)bench_setup.workid, (Pint)0, &rep);
	}
    }
#endif /* USING_PHIGS */
}/* bif_backgroundcolorindex */


/* ****************************************
 * Coordinate Transformation Entities
 * ************************************** */

/*----------------------------------------------------------------------*\
| Procedure	:	int bif_identity3(BIF_INT)
|------------------------------------------------------------------------|
| Description	:	Receive an IDENTITY3 entity from the parser
|			Implemented as a MATRIX3 / REPLACE
|
|	matrix_id	ID of matrix table entry
|------------------------------------------------------------------------|
| Return	:	Error Code
\*----------------------------------------------------------------------*/
int bif_identity3(matrix_id)
    BIF_INT matrix_id;
{
    static int ent_size = sizeof(BIF_Matrix3);
    BIF_All *ent;
    
#ifdef TEST_PRINT
    printf("IDENTITY3 : Set matrix %d to the identity matrix\n",matrix_id);
#endif /* TEST_PRINT */
#ifndef PRINT_ONLY
    /* Check the matrix_id */
    ERROR_MATRIX_ID(matrix_id,IDENTITY3);
    
    /* Allocate the entity */
    temp_ent.matrix3.matrix_id = matrix_id;
    temp_ent.matrix3.concat_type = BIF_REPLACE;
    copy44f(ident_matrix,temp_ent.matrix3.matrix);
    ent = new_generic(&temp_ent,ent_size,MATRIX3,do_matrix3);
    
    /* Error check for ent == NULL ( FATAL ) */
    ENT_ERROR(ent);
    
    /* Build or Execute */
    Traverse(traverser_state, ent);
    
#ifdef USING_PHIGS
    /* Cannot be contained within PHIGS structures */
    /* Called by the BIF execute traverser */
    /* No PHIGS call here */
#endif /* USING_PHIGS */
    
    /* Release Non-Retained Entities */
    Free_NRE(traverser_state, ent);
    
#endif /* PRINT_ONLY */
} /* End procedure bif_identity3 */

/*----------------------------------------------------------------------*\
| Procedure	:	int bif_concatmatrix3(BIF_INT, BIF_INT, BIF_INT)
|------------------------------------------------------------------------|
| Description	:	Receive a CONCAT_MATRIX3 entity from the parser
|
|	matrix_from	ID of "from" matrix table entry
|	matrix_to	ID of "to" matrix table entry
|	concat		PRECONCAT | POSTCONCAT | REPLACE
|------------------------------------------------------------------------|
| Return	:	Error Code
\*----------------------------------------------------------------------*/
int bif_concatmatrix3(matrix_from, matrix_to, concat)
    BIF_INT matrix_from, matrix_to, concat;
{
    static int ent_size = sizeof(BIF_Concatmatrix3);
    BIF_All *ent;
#ifdef TEST_PRINT
    printf("CONCAT_MATRIX3 : Concat %d and %d. Operation %d\n",
	   matrix_from, matrix_to, concat);
    fflush(stdout);
#endif /* TEST_PRINT */
#ifndef PRINT_ONLY
    /* Check the matrix_id's */
    ERROR_MATRIX_ID(matrix_from,CONCAT_MATRIX3);
    ERROR_MATRIX_ID(matrix_to,CONCAT_MATRIX3);
    
    /* Allocate the entity */
    temp_ent.concatmatrix3.matrix_id_from = matrix_from;
    temp_ent.concatmatrix3.matrix_id_to   = matrix_to;
    temp_ent.concatmatrix3.concat_type    = REMAP_CONCAT(concat);
    ent = new_generic(&temp_ent,ent_size,CONCAT_MATRIX3,
		      do_concatmatrix3);
    
    /* Error check for ent == NULL ( FATAL ) */
    ENT_ERROR(ent);
    
    /* Build or Execute */
    Traverse(traverser_state, ent);
    
#ifdef USING_PHIGS
    /* Cannot be contained within PHIGS structures */
    /* Called by the BIF execute traverser */
    /* No PHIGS call here */
#endif /* USING_PHIGS */
    
    /* Release Non-Retained Entities */
    Free_NRE(traverser_state, ent);
    
#endif /* PRINT_ONLY */
} /* End procedure bif_concatmatrix3 */

/*----------------------------------------------------------------------*\
| Procedure	:	int bif_invertmatrix3(BIF_INT)
|------------------------------------------------------------------------|
| Description	:	Receive an INVERT_MATRIX3 entity from the parser
|
|	matrix_id	ID of matrix table entry to invert
|------------------------------------------------------------------------|
| Return	:	Error Code
\*----------------------------------------------------------------------*/
int bif_invertmatrix3(matrix_id)
    BIF_INT matrix_id;
{
    static int ent_size = sizeof(BIF_Invertmatrix3);
    BIF_All *ent;
#ifdef TEST_PRINT
    printf("INVERT_MATRIX3 : Invert matrix %d\n",matrix_id);
#endif /* TEST_PRINT */
#ifndef PRINT_ONLY
    /* Check the matrix_id */
    ERROR_MATRIX_ID(matrix_id,INVERT_MATRIX3);
    
    /* Allocate the entity */
    temp_ent.invertmatrix3.matrix_id = matrix_id;
    ent = new_generic(&temp_ent,ent_size,INVERT_MATRIX3,
		      do_invertmatrix3);
    
    /* Error check for ent == NULL ( FATAL ) */
    ENT_ERROR(ent);
    
    /* Build or Execute */
    Traverse(traverser_state, ent);
    
#ifdef USING_PHIGS
    /* Cannot be contained within PHIGS structures */
    /* Called by the BIF execute traverser */
    /* No PHIGS call here */
#endif /* USING_PHIGS */
    
    /* Release Non-Retained Entities */
    Free_NRE(traverser_state, ent);
    
#endif /* PRINT_ONLY */
} /* End procedure bif_invertmatrix3 */

/*----------------------------------------------------------------------*\
| Procedure	:	int bif_rotate3( BIF_INT, BIF_REAL,
|						BIF_INT, BIF_INT )
|------------------------------------------------------------------------|
| Description	:	Receive a ROTATE3 entity from the parser
|			Implemented as a MATRIX3 / concat
|
|	matrix_id	ID of matrix table entry to alter
|	angle		Angle of rotation
|	axis		X_AXIS | Y_AXIS | Z_AXIS
|	concat		PRECONCAT | POSTCONCAT | REPLACE
|
|------------------------------------------------------------------------|
| Return	:	Error Code
\*----------------------------------------------------------------------*/
int bif_rotate3( matrix_id, angle, axis, concat )
    BIF_INT matrix_id;
    BIF_REAL angle;
    BIF_INT axis, concat;
{
    static int ent_size = sizeof(BIF_Matrix3);
    BIF_All *ent;
    matrix4 amx;
#ifdef TEST_PRINT
    printf("ROTATE3 : id %d by %f on %d. Op=%d\n",
	   matrix_id, angle, axis, concat);
#endif /* TEST_PRINT */
#ifndef PRINT_ONLY
    /* Check the matrix_id */
    ERROR_MATRIX_ID(matrix_id,ROTATE3);
    
    /* Fill the temp entity */
    switch( axis )
    {
      case X_AXIS :
	  mx_rot_x( (float)angle, amx );
	  break;
	case Y_AXIS :
	    mx_rot_y( (float)angle, amx );
	  break;
	case Z_AXIS :
	    mx_rot_z( (float)angle, amx );
	  break;
      }
    temp_ent.matrix3.matrix_id   = matrix_id;
    temp_ent.matrix3.concat_type = REMAP_CONCAT(concat);
    copy44f( amx, temp_ent.matrix3.matrix );
    ent = new_generic( &temp_ent, ent_size, MATRIX3, do_matrix3 );
    
    /* Error check for ent == NULL ( FATAL ) */
    ENT_ERROR(ent);
    
    /* Build or Execute */
    Traverse(traverser_state, ent);
    
#ifdef USING_PHIGS
    /* Cannot be contained within PHIGS structures */
    /* Called by the BIF execute traverser */
    /* No PHIGS call here */
#endif /* USING_PHIGS */
    
    /* Release Non-Retained Entities */
    Free_NRE(traverser_state, ent);
    
#endif /* PRINT_ONLY */
} /* End procedure bif_rotate3 */

/*----------------------------------------------------------------------*\
| Procedure	:	BIF_INT bif_rotatexyz3(BIF_INT,BIF_REAL,BIF_REAL,
|						BIF_REAL,BIF_INT)
|------------------------------------------------------------------------|
| Description	:	Receive a ROTATE_XYZ3 entity from the parser
|		Implemented as a MATRIX3 / concat
|
|	matrix_id	ID of matrix table entry to alter
|	angle_x, angle_y, angle_z
|			Angles of rotations
|	concat		PRECONCAT | POSTCONCAT | REPLACE
|------------------------------------------------------------------------|
| Return	:	Error Code
\*----------------------------------------------------------------------*/
int bif_rotatexyz3(matrix_id, angle_x, angle_y, angle_z, concat)
    BIF_INT matrix_id;
    BIF_REAL angle_x,angle_y,angle_z;
    BIF_INT concat;
{
    static int ent_size = sizeof(BIF_Matrix3);
    BIF_All *ent;
    matrix4 amx;
#ifdef TEST_PRINT
    printf("ROTATEXYZ3 : id %d by %f %f %f. Op=%d\n",
	   matrix_id, angle_x, angle_y, angle_z, concat);
#endif /* TEST_PRINT */
#ifndef PRINT_ONLY
    /* Check the matrix_id */
    ERROR_MATRIX_ID(matrix_id,ROTATE_XYZ3);
    
    /* Fill the temp entity */
    mx_euler_matrix( (float)angle_x, (float)angle_y,
		    (float)angle_z, amx );
    temp_ent.matrix3.matrix_id   = matrix_id;
    temp_ent.matrix3.concat_type = REMAP_CONCAT(concat);
    copy44f( amx, temp_ent.matrix3.matrix );
    ent = new_generic( &temp_ent, ent_size, MATRIX3, do_matrix3 );
    
    /* Error check for ent == NULL ( FATAL ) */
    ENT_ERROR(ent);
    
    /* Build or Execute */
    Traverse(traverser_state, ent);
    
#ifdef USING_PHIGS
    /* Cannot be contained within PHIGS structures */
    /* Called by the BIF execute traverser */
    /* No PHIGS call here */
#endif /* USING_PHIGS */
    
    /* Release Non-Retained Entities */
    Free_NRE(traverser_state, ent);
    
#endif /* PRINT_ONLY */
} /* End procedure bif_rotatexyz3 */

/*----------------------------------------------------------------------*\
| Procedure	:	int bif_translate3(BIF_INT ,
|					BIF_REAL, BIF_REAL, BIF_REAL,
|					BIF_INT)
|------------------------------------------------------------------------|
| Description	:	Receive a TRANSLATE3 entity from the parser
|		Implemented as a MATRIX3 / concat
|
|	matrix_id	ID of matrix table entry to alter
|	tx, ty, tz	Translation vector 
|	concat		PRECONCAT | POSTCONCAT | REPLACE
|------------------------------------------------------------------------|
| Return	:	Error Code
\*----------------------------------------------------------------------*/
int bif_translate3(matrix_id, tx, ty, tz, concat)
    BIF_INT matrix_id;
    BIF_REAL tx, ty, tz;
    BIF_INT concat;
{
    static int ent_size = sizeof(BIF_Matrix3);
    BIF_All *ent;
    float amx[4][4];
#ifdef TEST_PRINT
    printf("TRANSLATE3 : id %d by %f %f %f. Op=%d\n",
	   matrix_id, tx, ty, tz, concat);
#endif /* TEST_PRINT */
#ifndef PRINT_ONLY
    /* Check the matrix_id */
    ERROR_MATRIX_ID(matrix_id,TRANSLATE3);
    
    /* Fill the temp entity */
    mx_translate( (float)tx, (float)ty, (float)tz, amx );
    temp_ent.matrix3.matrix_id   = matrix_id;
    temp_ent.matrix3.concat_type = REMAP_CONCAT(concat);
    copy44f( amx, temp_ent.matrix3.matrix );
    ent = new_generic( &temp_ent, ent_size, MATRIX3, do_matrix3 );
    
    /* Error check for ent == NULL ( FATAL ) */
    ENT_ERROR(ent);
    
    /* Build or Execute */
    Traverse(traverser_state, ent);
    
#ifdef USING_PHIGS
    /* Cannot be contained within PHIGS structures */
    /* Called by the BIF execute traverser */
    /* No PHIGS call here */
#endif /* USING_PHIGS */
    
    /* Release Non-Retained Entities */
    Free_NRE(traverser_state, ent);
    
#endif /* PRINT_ONLY */
} /* End procedure bif_globaltransformation3 */


/*----------------------------------------------------------------------*\
| Procedure	:	int bif_scale3(BIF_INT , BIF_REAL, BIF_REAL,
|					BIF_REAL, BIF_INT)
|------------------------------------------------------------------------|
| Description	:	Receive a SCALE3 entity from the parser
|			Implemented as a MATRIX3 / concat
|
|	matrix_id	ID of matrix table entry to alter
|	sx, sy, sz	Scaling vector 
|	concat		PRECONCAT | POSTCONCAT | REPLACE
|------------------------------------------------------------------------|
| Return	:	Error Code
\*----------------------------------------------------------------------*/
int bif_scale3(matrix_id, sx, sy, sz, concat)
    BIF_INT matrix_id;
    BIF_REAL sx, sy, sz;
    BIF_INT concat;
{
    static int ent_size = sizeof(BIF_Matrix3);
    BIF_All *ent;
    float amx[4][4];
#ifdef TEST_PRINT
    printf("SCALE3 : id %d by %f %f %f. Op=%d\n",
	   matrix_id, sx, sy, sz, concat);
#endif /* TEST_PRINT */
#ifndef PRINT_ONLY
    /* Check the matrix_id */
    ERROR_MATRIX_ID(matrix_id,SCALE3);
    
    /* Fill the temp entity */
    mx_scale( (float)sx, (float)sy, (float)sz, amx );
    temp_ent.matrix3.matrix_id   = matrix_id;
    temp_ent.matrix3.concat_type = REMAP_CONCAT(concat);
    copy44f( amx, temp_ent.matrix3.matrix );
    ent = new_generic( &temp_ent, ent_size, MATRIX3, do_matrix3 );
    
    /* Error check for ent == NULL ( FATAL ) */
    ENT_ERROR(ent);
    
    /* Build or Execute */
    Traverse(traverser_state, ent);
    
#ifdef USING_PHIGS
    /* Cannot be contained within PHIGS structures */
    /* Called by the BIF execute traverser */
    /* No PHIGS call here */
#endif /* USING_PHIGS */
    
    /* Release Non-Retained Entities */
    Free_NRE(traverser_state, ent);
    
#endif /* PRINT_ONLY */
} /* End procedure bif_scale3 */

/*----------------------------------------------------------------------*\
| Procedure	:	int bif_matrix3(BIF_INT, *BIF_REAL, BIF_INT)
|------------------------------------------------------------------------|
| Description	:	Receive a MATRIX3 entity from the parser
|
|	matrix_id	ID of matrix table entry to alter
|	amatrix		Matrix to use 
|	concat		PRECONCAT | POSTCONCAT | REPLACE
|------------------------------------------------------------------------|
| Return	:	Error Code
\*----------------------------------------------------------------------*/
int bif_matrix3(matrix_id, amatrix, concat)
    BIF_INT matrix_id;
    BIF_REAL  amatrix[4][4];
    BIF_INT concat;
{
    static int ent_size = sizeof(BIF_Matrix3);
    BIF_All *ent;
#ifdef TEST_PRINT
    printf("MATRIX3 : id %d  Op=%d Matrix:\n", matrix_id, concat);
    PRINT_MATRIX44(amatrix);
#endif /* TEST_PRINT */
#ifndef PRINT_ONLY
    /* Check the matrix_id */
    ERROR_MATRIX_ID(matrix_id,MATRIX3);
    
    /* Fill the temp entity */
    temp_ent.matrix3.matrix_id   = matrix_id;
    temp_ent.matrix3.concat_type = REMAP_CONCAT(concat);
    Transpose44( amatrix, temp_ent.matrix3.matrix );
    ent = new_generic(&temp_ent, ent_size, MATRIX3, do_matrix3);
    
    /* Error check for ent == NULL ( FATAL ) */
    ENT_ERROR(ent);
    
    /* Build or Execute */
    Traverse(traverser_state, ent);
    
#ifdef USING_PHIGS
    /* Cannot be contained within PHIGS structures */
    /* Called by the BIF execute traverser */
    /* No PHIGS call here */
#endif /* USING_PHIGS */
    
    /* Release Non-Retained Entities */
    Free_NRE(traverser_state, ent);
    
#endif /* PRINT_ONLY */
} /* End procedure bif_matrix3 */

/*----------------------------------------------------------------------*\
| Procedure	:	int bif_getmatrix3(BIF_INT, BIF_INT, BIF_INT)
|------------------------------------------------------------------------|
| Description	:	Receive a GET_MATRIX3 entity from the parser
|
|	matrix_id	ID of matrix table entry to alter
|	get_this	System Matrix to use 
|	concat		PRECONCAT | POSTCONCAT | REPLACE
|------------------------------------------------------------------------|
| Return	:	Error Code
\*----------------------------------------------------------------------*/
int bif_getmatrix3(matrix_id, get_this, concat)
    BIF_INT matrix_id;
    BIF_INT get_this;
    BIF_INT concat;
{
    static int ent_size = sizeof(BIF_Getmatrix3);
    BIF_All *ent;
    char *from_mat, *mat_op;
    char *find_keyword_token();
    from_mat = find_keyword_token(get_this);
    mat_op   = find_keyword_token(concat);
#ifdef TEST_PRINT
    printf("GET_MATRIX3 : Matrix id %d from %s Op= %s \n",
	   matrix_id, from_mat, mat_op);
#endif /* TEST_PRINT */
#ifndef PRINT_ONLY
    /* Check the matrix_id */
    ERROR_MATRIX_ID(matrix_id,GET_MATRIX3);
    
    /* Fill the temp_ent then allocate and copy the entity */
    temp_ent.getmatrix3.matrix_id   = matrix_id;
    temp_ent.getmatrix3.get_matrix  = REMAP_GETMAT(get_this);
    temp_ent.getmatrix3.concat_type = REMAP_CONCAT(concat);
    ent = new_generic(&temp_ent, ent_size, GET_MATRIX3,
		      do_getmatrix3);
    
    /* Error check for ent == NULL ( FATAL ) */
    ENT_ERROR(ent);
    
    /* Build or Execute */
    Traverse(traverser_state, ent);
    
#ifdef USING_PHIGS
    /* Cannot be contained within PHIGS structures */
    /* Called by the BIF execute traverser */
    /* No PHIGS call here */
#endif /* USING_PHIGS */
    
    /* Release Non-Retained Entities */
    Free_NRE(traverser_state, ent);
    
#endif /* PRINT_ONLY */
} /* End procedure bif_getmatrix3 */


/*--------------------------------------------------------------------*\
| Procedure     :	int bif_pushmatrix3()
|---------------------------------------------------------------------
| Description   :	Receive a PUSH_MATRIX3 entity from the parser
|			Build, then store or execute the appropriate
|			entity.  The bif_entity only manipulates the 
|			matrix stack ( on matrix entry 0 )
|---------------------------------------------------------------------
| Return        :	Error Code (Not Implemented)
\*--------------------------------------------------------------------*/
int bif_pushmatrix3()
    
{
    static int ent_size = sizeof(BIF_No_data);
    BIF_All *ent;
    
#ifdef TEST_PRINT
    printf("PUSH_MATRIX3:\n");
#endif /* TEST_PRINT */
#ifndef PRINT_ONLY
    /* Allocate and fill the entity */
    ent = new_generic(&temp_ent, ent_size, PUSH_MATRIX3,
		      do_pushmatrix3);
    
    /* Error check for ent == NULL ( FATAL ) */
    ENT_ERROR(ent);
    
    /* Build or Execute */
    Traverse(traverser_state, ent);
    
#ifdef USING_PHIGS
    /* Cannot be contained within PHIGS structures */
    /* Called by the BIF execute traverser */
    /* No PHIGS call here */
#endif /* USING_PHIGS */
    
    /* Release Non-Retained Entities */
    Free_NRE(traverser_state, ent);
    
#endif /* PRINT_ONLY */
} /* End bif_pushmatrix3() */

/*--------------------------------------------------------------------*\
| Procedure     :	int bif_popmatrix3()
|---------------------------------------------------------------------
| Description   :	Receive a POP_MATRIX3 entity from the parser
|			Build, then store or execute the appropriate
|			entity.  The bif_entity only manipulates the 
|			matrix stack ( on matrix entry 0 )
|---------------------------------------------------------------------
| Return        :	Error Code (Not Implemented)
\*--------------------------------------------------------------------*/
int bif_popmatrix3()
    
{
    static int ent_size = sizeof(BIF_No_data);
    BIF_All *ent;
    
#ifdef TEST_PRINT
    printf("POP_MATRIX3:\n");
#endif /* TEST_PRINT */
#ifndef PRINT_ONLY
    /* Allocate and fill the entity */
    ent = new_generic(&temp_ent, ent_size, POP_MATRIX3,
		      do_popmatrix3);
    
    /* Error check for ent == NULL ( FATAL ) */
    ENT_ERROR(ent);
    
    /* Build or Execute */
    Traverse(traverser_state, ent);
    
#ifdef USING_PHIGS
    /* Cannot be contained within PHIGS structures */
    /* Called by the BIF execute traverser */
    /* No PHIGS call here */
#endif /* USING_PHIGS */
    
    /* Release Non-Retained Entities */
    Free_NRE(traverser_state, ent);
    
#endif /* PRINT_ONLY */
} /* End bif_popmatrix3() */


/*----------------------------------------------------------------------*\
| Procedure	:	int bif_gtransform3(*BIF_REAL)
|------------------------------------------------------------------------|
| Description	:	Receive a GLOBAL_TRANSFORMATION3 entity from the
|			parser amatrix	Matrix to use 
|------------------------------------------------------------------------|
| Return	:	Error Code
\*----------------------------------------------------------------------*/
int bif_gtransform3(amatrix)
    BIF_REAL  amatrix[4][4];
{
#ifdef USING_PHIGS
    Pmatrix3 tmp_xform;
#endif
    static int ent_size = sizeof(BIF_Globaltransformation3);
    BIF_All *ent;
#ifdef TEST_PRINT
    printf("GLOBAL_TRANSFORMATION3 : Matrix:\n");
    PRINT_MATRIX44(amatrix);
#endif /* TEST_PRINT */
#ifndef PRINT_ONLY
    /* Fill the temp entity */
    Transpose44( amatrix, temp_ent.globaltransformation3.xform);
    ent = new_generic(&temp_ent, ent_size, GLOBAL_TRANSFORMATION3,
		      do_globaltransformation3);
    
    /* Error check for ent == NULL ( FATAL ) */
    ENT_ERROR(ent);
    
    /* Build or Execute */
    Traverse(traverser_state, ent);
    
#ifdef USING_PHIGS
    Cpmatrix44(amatrix, tmp_xform); /* Convert from BIF_REAL to Pmatrix3 */
    pset_global_tran3(tmp_xform);
#endif /* USING_PHIGS */
    
    /* Release Non-Retained Entities */
    Free_NRE(traverser_state, ent);
    
#endif /* PRINT_ONLY */
} /* End procedure bif_gtransform3 */

/*----------------------------------------------------------------------*\
| Procedure	:	int bif_ltransform3(*BIF_REAL, BIF_INT)
|------------------------------------------------------------------------|
| Description	:	Receive a LOCAL_TRANSFORMATION3 entity from the
|			parser amatrix	Matrix to use 
|
|	concat		PRECONCAT | POSTCONCAT | REPLACE
|------------------------------------------------------------------------|
| Return	:	Error Code
\*----------------------------------------------------------------------*/
int bif_ltransform3(amatrix, concat)
    BIF_REAL  amatrix[4][4];
    BIF_INT concat;
{
#ifdef USING_PHIGS
    Pmatrix3 tmp_xform;
#endif
    static int ent_size = sizeof(BIF_Localtransformation3);
    BIF_All *ent;
#ifdef TEST_PRINT
    printf("LOCAL_TRANSFORMATION3 : Op=%d Matrix:\n", concat);
    PRINT_MATRIX44(amatrix);
#endif /* TEST_PRINT */
#ifndef PRINT_ONLY
    /* Fill the temp entity */
    Transpose44( amatrix, temp_ent.localtransformation3.xform );
    temp_ent.localtransformation3.compose_type =
	(Pcompose_type)REMAP_CONCAT(concat);
    ent = new_generic(&temp_ent, ent_size, 
		      LOCAL_TRANSFORMATION3, do_localtransformation3);
    
    /* Error check for ent == NULL ( FATAL ) */
    ENT_ERROR(ent);
    
    /* Build or Execute */
    Traverse(traverser_state, ent);
    
#ifdef USING_PHIGS
    Cpmatrix44(amatrix, tmp_xform); /* Convert from BIF_REAL to Pmatrix3 */
    pset_local_tran3(tmp_xform, ent->localtransformation3.compose_type );
#endif /* USING_PHIGS */
    
    /* Release Non-Retained Entities */
    Free_NRE(traverser_state, ent);
    
#endif /* PRINT_ONLY */
} /* End procedure bif_ltransform3 */

/*----------------------------------------------------------------------*\
| Procedure	:	int bif_applytoglobal3(BIF_INT)
|------------------------------------------------------------------------|
| Description	:	Receive an APPLY_TO_GLOBAL3 entity from the parser
|
|	matrix_id	ID of matrix table entry to apply to global
|------------------------------------------------------------------------|
| Return	:	Error Code
\*----------------------------------------------------------------------*/
int bif_applytoglobal3(matrix_id)
    BIF_INT matrix_id;
{
    static int ent_size = sizeof(BIF_Applytoglobal3);
    BIF_All *ent;
    BIF_Applytoglobal3 *app;
    BIF_Beginstructure *str_ptr;
#ifdef TEST_PRINT
    printf("APPLY_TO_GLOBAL3 : id %d  \n", matrix_id);
#endif /* TEST_PRINT */
#ifndef PRINT_ONLY
    /* Check the matrix_id */
    ERROR_MATRIX_ID(matrix_id,APPLY_TO_GLOBAL3);
    
    /* Fill the temp entity */
    app = &temp_ent.applytoglobal3;
    app->matrix_id    = matrix_id;
    if ( (str_ptr = traverser_state->open_structure) != NULL )
	app->structure_id = str_ptr->structure_id;
    else
	app->structure_id = bench_setup.nrs_stid;
    app->matrix_label = getNextLabel();
    
    /* Allocate the entity */
    ent = new_generic(&temp_ent, ent_size,
		      APPLY_TO_GLOBAL3,do_applytoglobal3);
#ifdef TEST_PRINT
    printf("push_level %d  \n", traverser_state->push_level);
#endif /* TEST_PRINT */
    
    /* Error check for ent == NULL ( FATAL ) */
    ENT_ERROR(ent);
    
#ifdef USING_PHIGS
    /* Insert Label must be before the BIF traversal */
    plabel(app->matrix_label);
    {
	Pmatrix3 mx;
	mx_identity(mx);
	pset_global_tran3(mx);
    }
#endif /* USING_PHIGS */
    /* Store or Execute */
    Traverse(traverser_state, ent);
    
#endif /* PRINT_ONLY */
} /* End procedure bif_applytoglobal3 */

/*----------------------------------------------------------------------*\
| Procedure	:	int bif_applytolocal3(BIF_INT, BIF_INT)
|------------------------------------------------------------------------|
| Description	:	Receive an APPLY_TO_LOCAL3 entity from the parser
|
|	matrix_id	ID of matrix table entry to apply to local
|	concat	PRECONCAT | POSTCONCAT | REPLACE
|------------------------------------------------------------------------|
| Return	:	Error Code
\*----------------------------------------------------------------------*/
int bif_applytolocal3(matrix_id, concat)
    BIF_INT matrix_id;
    BIF_INT concat;
{
    static int ent_size = sizeof(BIF_Applytolocal3);
    BIF_All *ent;
    BIF_Applytolocal3 *app;
    BIF_Beginstructure *str_ptr;
#ifdef TEST_PRINT
    printf("APPLY_TO_LOCAL3 : id %d  Op=%d\n", matrix_id, concat);
#endif /* TEST_PRINT */
#ifndef PRINT_ONLY
    /* Check the matrix_id */
    ERROR_MATRIX_ID(matrix_id,APPLY_TO_LOCAL3);
    
    /* Fill the temp entity */
    app = &temp_ent.applytolocal3;
    app->matrix_id    = matrix_id;
    app->concat_type  = REMAP_CONCAT(concat);
    if ( (str_ptr = traverser_state->open_structure) != NULL )
	app->structure_id = str_ptr->structure_id;
    else
	app->structure_id = bench_setup.nrs_stid;
    app->matrix_label = getNextLabel();
    
    /* Allocate the entity */
    ent = new_generic(&temp_ent, ent_size,
		      APPLY_TO_LOCAL3,do_applytolocal3);
    
    /* Error check for ent == NULL ( FATAL ) */
    ENT_ERROR(ent);
    
#ifdef USING_PHIGS
    /* Insert Label must be before the BIF traversal */
    plabel(app->matrix_label);
    {
	Pmatrix3 mx;
	Pcompose_type contype;
	mx_identity(mx);
	contype = (Pcompose_type)BIF_PRECONCAT;
	pset_local_tran3(mx,contype);
    }
#endif /* USING_PHIGS */
    /* Store or Execute */
    Traverse(traverser_state, ent);
    
#endif /* PRINT_ONLY */
} /* End procedure bif_applytolocal3 */

/*--------------------------------------------------------------------*\
|	* ***************************************** *
|		BIF Verb File
|	* ***************************************** *
\*--------------------------------------------------------------------*/

/*--------------------------------------------------------------------*\
|	* ***************************************** *
|		Input / Output
|	* ***************************************** *
\*--------------------------------------------------------------------*/


/*--------------------------------------------------------------------*\
|	Read Geom is a function of the parser
\*--------------------------------------------------------------------*/

/*----------------------------------------------------------------------*\
| Procedure	:	int bif_cleargeom()
|------------------------------------------------------------------------|
| Description	:	Receive a CLEAR_GEOMETRY entity from the parser.
|			Delete all structures from memory.
|------------------------------------------------------------------------|
| Return	:	Error Code
\*----------------------------------------------------------------------*/
int bif_cleargeom()
{
#ifdef TEST_PRINT
    printf("CLEAR_GEOMETRY :\n");
#endif /* TEST_PRINT */
#ifndef PRINT_ONLY
    db_clear_all();
    
#ifdef USING_PHIGS
    /* Nothing Here.... db_clear_all also does PEMST as well as free the 
       BIF structures		 */
#endif /* USING_PHIGS */
    
#endif /* PRINT_ONLY */
} /* End procedure bif_cleargeom */

/*----------------------------------------------------------------------*\
| Procedure	:	int bif_reportfile(*char)
|------------------------------------------------------------------------|
| Description	:	Receive a REPORT_FILE entity from the parser.
|			Specifies new report file.
|------------------------------------------------------------------------|
| Return	:	Error Code
\*----------------------------------------------------------------------*/
int bif_reportfile(file_name)
    char *file_name;
{
#ifdef TEST_PRINT
    printf("REPORT_FILE : %s\n",file_name);
#endif /* TEST_PRINT */
#ifndef PRINT_ONLY
    
    
    close_reportfile();
    open_reportfile(file_name);
    
    
#endif /* PRINT_ONLY */
} /* End procedure bif_reportfile */

/*--------------------------------------------------------------------*\
|	* ***************************************** *
|		Test Control
|	* ***************************************** *
\*--------------------------------------------------------------------*/

/*----------------------------------------------------------------------*\
| Procedure	:	int bif_begintest(BIF_INT)
|------------------------------------------------------------------------|
| Description	:	Receive a BEGIN_TEST entity from the parser
|
|	n_repetitions	Number of frames
|------------------------------------------------------------------------|
| Return	:	Error Code
\*----------------------------------------------------------------------*/
int bif_begintest(n_repetitions)
    BIF_INT n_repetitions;
{
    int structure_id;
    BIF_Beginstructure *ent;
#ifdef TEST_PRINT
    printf("BEGIN_TEST : repeats %d \n",n_repetitions);
#endif /* TEST_PRINT */
#ifndef PRINT_ONLY
    /* Get the pointer to the structure ( allocated if new ) */
    structure_id = bench_setup.test_stid;
    ent = db_get_str((BIF_INT)structure_id);
    
    /*  Check if we ran out of memory */
    ENT_ERROR(ent);
    
    /* Store the Current test state */
    current_test.n_repetitions = n_repetitions;
    current_test.structure_id   = structure_id;
    current_test.structure_ptr  = ent;
    current_test.structure_ptr->expanded = 0;
    
    /* Clear the Structure of its last contents */
    free_all_list(ent->top_of_list);
    ent->top_of_list = NULL;
    
    /* Open the Structure in the BIF Database / set the traverser */
    /* to the build mode */
    do_beginstructure(traverser_state, ent);
    
    /* ************************************************************* */
    /* Non-Phigs: Insert Border BIF Structure                        */
    /*      Traverse(traverser_state, border_structure.top_of_list ) */
    /* ************************************************************* */
    
#ifdef USING_PHIGS
    /* Close the Non-Retained Structure */
    fxclns();
    traverser_state->nrs_state = 0;
    
    /* ************************************************************ */
    /* Do the PHIGS calls that initialize the test_structure        */
    /* ************************************************************ */
    /* Empty the Test Structure */
    pempty_struct((Pint)structure_id);
    
    /* Open the Test Structure */
    popen_struct((Pint)structure_id);
    /* Insert the "Basestate" the Test Structure */
    pcopy_all_elems_struct((Pint)bench_setup.base_state_stid);
#endif /* USING_PHIGS */
#endif /* PRINT_ONLY */
} /* End procedure bif_begintest */

/*----------------------------------------------------------------------*\
| Procedure: int bif_endtest()
|------------------------------------------------------------------------|
| Description: Receive an END_TEST entity from the parser
|------------------------------------------------------------------------|
| Return: Error Code
\*----------------------------------------------------------------------*/
int bif_endtest()
{
    BIF_All *bif_list;
    int indx;
    slList tol; /* Top of list for CALL expansion */
#ifdef TEST_PRINT
    printf("END_TEST :\n");
    fflush(stdout);
#endif /* TEST_PRINT */
#ifndef PRINT_ONLY
    /* End the insertion of entities into the BIF test structure */
    /* Put the traverser into EXECUTE mode */
    do_endstructure(traverser_state,NULL);
    
#ifdef USING_PHIGS
    /* Close and the test structure */
    pclose_struct();
    
#ifdef TEST_PRINT
    printf("ppost(%d, %d, 1.0)\n",bench_setup.workid,current_test.structure_id);
    fflush(stdout);
#endif /* TEST_PRINT */
    
#ifdef EXTERNALNOTE
    /* it was discovered that the border around the test window
       was adding a substantial overhead on earlier versions
       of PLB. Because this overhead is mostly text, it tends to
       bias the PLB towards machines with faster text operations.
       The answer is to unpost the border structure just prior to the
       test loop itself, then post the test structure. After the test is run,
       the border is re-posted and the workstation updated. This presents
       the final frame with border to the user. At this point, the test
       structure is un-posted, and vanishes on the next operation.*/
#endif
    
    /* UN-post the border structure if there is one */
    if (bench_setup.border_stid)
	punpost_struct((Pint)bench_setup.workid,(Pint)bench_setup.border_stid);

#endif /* USING_PHIGS */
    
    /* Push the traverser state */
    traverser_state->currentFrame = 1;
#ifdef SYSV
    memcpy((char *)temp_state_ptr,(char *)traverser_state,
	   sizeof(BIF_Traverser_state));
#else
    bcopy((char *)traverser_state, (char *)temp_state_ptr,
	  sizeof(BIF_Traverser_state));
#endif
    /* Disable Basestate edits */
    temp_state_ptr->push_level = 1;
    bif_list = current_test.structure_ptr->top_of_list;
    
#ifndef REFER_STRUCTURE_EXISTS
    /* Expand the CALL's */
    expandStructure(current_test.structure_ptr);
    
    /* Allow instance tracing editing */
    temp_state_ptr->tol = &tol;
    temp_state_ptr->eol = &tol;
    tol.next = NULL;
    tol.data = current_test.structure_id;
    
#endif /* REFER_STRUCTURE_EXISTS */
    
#ifdef USING_PHIGS
    ppost_struct((Pint)bench_setup.workid,
		 (Pint)current_test.structure_id,(Pfloat)1.);
#endif /* USING_PHIGS */
    /* **************************** */
    /* Do the statistical traverser */
    /* **************************** */
    brf_state.brf_start_frame = 1;
    brf_state.brf_end_frame = current_test.n_repetitions;
    brf_state.brf_num_frames = current_test.n_repetitions;
    brf_init ( );
    brf_traverser( traverser_state, bif_list, &brf_state);
    brf_report(&brf_state ); /* generates the body of report */
    /* **************** */
    /* Start stop watch */
    /* **************** */
    brf_start_stopwatch();
    /* **************** */
    /* Do the test loop */
    /* **************** */
    for ( indx = 0 ; indx < current_test.n_repetitions ; indx++ )
    {
	Traverse(temp_state_ptr,bif_list);
	
#ifdef USING_PHIGS
	pupd_ws((Pint)bench_setup.workid,(Pregen_flag)PFLAG_PERFORM);
#endif /* USING_PHIGS */
	
	temp_state_ptr->currentFrame++;
	
    }
    /* **************************************** */
    /* Stop stop watch and print timing results */
    /* **************************************** */
    brf_stop_stopwatch(&brf_state);
    /* **************************************** */
    
#ifdef USING_PHIGS
    /* RE-post the border structure if it exists */
    if (bench_setup.border_stid) {
	ppost_struct((Pint)bench_setup.workid,(Pint)bench_setup.border_stid,
		     (Pfloat)1.);
	pupd_ws((Pint)bench_setup.workid,(Pregen_flag)PFLAG_PERFORM);
    }
    /* Open the Non-Reatained Structure */
    fxopns();
    traverser_state->nrs_state = 1;
#endif /* USING_PHIGS */
    
#endif /* PRINT_ONLY */
} /* End procedure bif_endtest */

/*----------------------------------------------------------------------*\
| Procedure: int bif_pause()
|------------------------------------------------------------------------|
| Description: Receive a PAUSE entity from the parser
|------------------------------------------------------------------------|
| Return: Error Code
\*----------------------------------------------------------------------*/
int bif_pause()
{
#ifdef TEST_PRINT
    printf("PAUSE :\n");
#endif /* TEST_PRINT */
#ifndef PRINT_ONLY
#ifdef USING_PHIGS
    fxpaus(&bench_setup.workid);
#else
    /* Insert a mouse or keyboard wait */
    fxpaus(&bench_setup.workid);
#endif /* USING_PHIGS */
#endif /* PRINT_ONLY */
} /* End procedure bif_pause */
/*----------------------------------------------------------------------*\
| Procedure	:	int bif_sleep(BIF_INT)
|------------------------------------------------------------------------|
| Description	:	Receive a SLEEP entity from the parser
|
|	sleep	Sleep period in seconds
|------------------------------------------------------------------------|
| Return	:	Error Code
\*----------------------------------------------------------------------*/
int bif_sleep(sleep_time)
    BIF_INT sleep_time;
{
#ifdef TEST_PRINT
    printf("SLEEP : zzzzzzz for %d sec.\n",sleep_time);
#endif /* TEST_PRINT */
#ifndef PRINT_ONLY
    sleep((unsigned)sleep_time);
#endif /* PRINT_ONLY */
} /* End procedure bif_sleep */

/*--------------------------------------------------------------------*\
| Procedure     :	int indexRange(int, int, int, int)
|---------------------------------------------------------------------
| Description   :	Check the supplied index against the range
|			Generate warning message at file load time.
|---------------------------------------------------------------------
| Return        :	-1 --> Value  < min
|			 0 --> min <= Value  <= max
|			+1 --> Value  > max
\*--------------------------------------------------------------------*/
int indexRange(token, indx, min, max)
    BIF_INT token, indx, min, max;
    
{
    int retCode;
    char *key_w, *find_keyword_token(); 
    char buffy[255];
    if ( indx > max )
    {
	retCode =  1;
	key_w = find_keyword_token(token); 
	sprintf(buffy,"%s: Index value %d > Max. (%d)",
		key_w,indx,max);
	yyerror(buffy);
    }
    else if ( indx < min )
    {
	retCode = -1;
	key_w = find_keyword_token(token); 
	sprintf(buffy,"%s: Index value %d < Min. (%d)",
		key_w,indx,min);
	yyerror(buffy);
    }
    else
    {
	retCode = 0;
    }
    
    return(retCode);
} /* End indexRange() */
