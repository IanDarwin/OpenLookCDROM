/* $XConsortium: basestat.c,v 5.3 94/04/17 20:44:16 hersh Exp $ */
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
supporting documentation, and that Sun Microsystems
not be used in advertising or publicity
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
| Author        :	MJF / SimGraphics Engineering Corportation
|
| File          :	basestat.c
| Date          :	Fri Feb  9 10:46:55 PST 1990
| Project       :	PLB
| Description   :	
|		 These functions (eg. do_markertype) will be called by 
|		 execute_traverser any time their bif namesake (ie. 
|		 MARKER_TYPE) is encountered within a bif verb file or
|		 if their bif namesake is encountered within a bif 
|		 structure which in encountered within a bif test loop.  
|		 These functions should be executed only if the push_level
|		 at the time encountered is 0.  The push_level is 
|		 incremented each time the traverser begins a called 
|		 structure (ie. attributes are pushed), and decremented 
|		 each time the traverser returns from  a called structure. 
|
|		 If a non-retained structure is open the non-retained 
|		 must be closed before proceeding and reopened when 
|		 finished.
| Status        :	Version 1.0
|
| Revisions     :	
|	1/90		Various bugs found and solved
|
|       2/90            MFC Tektronix, Inc.: PEX-SI API implementation.
|
|       5/90            MFC Tektronix, Inc.: PEX-SI API Binding change.
|
|      12/90            MFC Tektronix, Inc.: PEX-SI PEX5R1 Release.
|
\*--------------------------------------------------------------------*/

/*--------------------------------------------------------------------*\
|	Table of Contents
|
| ifdef USING_PHIGS
|	do_2_generic_i(*BIF_Traverser_state, int, *int,
|                       (*void)(), (*void)(), int)
|		:	Generic integer value basestate update function
|	do_generic_i(*BIF_Traverser_state, int, *int, (*void)(), int)
|		:	Generic integer value basestate update function
|	do_generic_f(*BIF_Traverser_state, int, *int, (*void)(), float)
|		:	Generic float value basestate update function
|	do_generic_c3(*BIF_Traverser_state, *BIF_All, *int, *int, *int,
|			(*void)())
|		:	Generic true color basestate update function
|	do_generic_colorindex(*BIF_Traverser_state, int, *int, *int,
|			*int, (*void)(), int)
|		:	Generic index color basestate update function
| endif USING_PHIGS
|
|	do_markertype(* BIF_Traverser_state, *BIF_All)
|		:	Edit basestate value for this BIF attribute
|	do_markersize(* BIF_Traverser_state, *BIF_All)
|		:	Edit basestate value for this BIF attribute
|	do_markercolorindex(* BIF_Traverser_state, *BIF_All)
|		:	Edit basestate value for this BIF attribute
|	do_linetype(* BIF_Traverser_state, *BIF_All)
|		:	Edit basestate value for this BIF attribute
|	do_linewidth(* BIF_Traverser_state, *BIF_All)
|		:	Edit basestate value for this BIF attribute
|	do_linecolorindex(* BIF_Traverser_state, *BIF_All)
|		:	Edit basestate value for this BIF attribute
|	do_interiorstyle(* BIF_Traverser_state, *BIF_All)
|		:	Edit basestate value for this BIF attribute
|	do_interiorcolorindex(* BIF_Traverser_state, *BIF_All)
|		:	Edit basestate value for this BIF attribute
|	do_interiorshading(* BIF_Traverser_state, *BIF_All)
|		:	Edit basestate value for this BIF attribute
|	do_interiorlighting(* BIF_Traverser_state, *BIF_All)
|		:	Edit basestate value for this BIF attribute
|	do_surfaceproperties(* BIF_Traverser_state, *BIF_All)
|		:	Edit basestate value for this BIF attribute
|	do_edgeflag(* BIF_Traverser_state, *BIF_All)
|		:	Edit basestate value for this BIF attribute
|	do_edgewidth(* BIF_Traverser_state, *BIF_All)
|		:	Edit basestate value for this BIF attribute
|	do_textfont(* BIF_Traverser_state, *BIF_All)
|		:	Edit basestate value for this BIF attribute
|	do_textprec(* BIF_Traverser_state, *BIF_All)
|		:	Edit basestate value for this BIF attribute
|	do_textcolorindex(* BIF_Traverser_state, *BIF_All)
|		:	Edit basestate value for this BIF attribute
|	do_textpath(* BIF_Traverser_state, *BIF_All)
|		:	Edit basestate value for this BIF attribute
|	do_textalign(* BIF_Traverser_state, *BIF_All)
|		:	Edit basestate value for this BIF attribute
|	do_charheight(* BIF_Traverser_state, *BIF_All)
|		:	Edit basestate value for this BIF attribute
|	do_charupvector(* BIF_Traverser_state,
|		:	Edit basestate value for this BIF attribute
|	do_hlhsremoval(* BIF_Traverser_state, *BIF_All)
|		:	Edit basestate value for this BIF attribute
|	do_globaltransformation3(* BIF_Traverser_state,
|		:	Edit basestate value for this BIF attribute
|	do_localtransformation3(* BIF_Traverser_state, *BIF_All)
|		:	Edit basestate value for this BIF attribute
|	do_applytoglobal3(* BIF_Traverser_state, *BIF_All)
|		:	Edit basestate value for this BIF attribute
|	do_applytolocal3(* BIF_Traverser_state, *BIF_All)
|		:	Edit basestate value for this BIF attribute
|	do_interiorcolor(* BIF_Traverser_state, *BIF_All)
|		:	Edit basestate value for this BIF attribute
|	do_markercolor(* BIF_Traverser_state, *BIF_All)
|		:	Edit basestate value for this BIF attribute
|	do_linecolor(* BIF_Traverser_state, *BIF_All)
|		:	Edit basestate value for this BIF attribute
|	do_textcolor(* BIF_Traverser_state, *BIF_All)
|		:	Edit basestate value for this BIF attribute
|	do_backfaceinteriorcolor(* BIF_Traverser_state, *BIF_All)
|		:	Edit basestate value for this BIF attribute
|	do_edgecolor(* BIF_Traverser_state, *BIF_All)
|		:	Edit basestate value for this BIF attribute
|	do_annotextstyle(* BIF_Traverser_state, *BIF_All)
|		:	Edit basestate value for this BIF attribute
|	do_backfaceinteriorcolorindex( * BIF_Traverser_state, *BIF_All)
|		:	Edit basestate value for this BIF attribute
|	do_annotextcharheight(* BIF_Traverser_state, *BIF_All)
|		:	Edit basestate value for this BIF attribute
|	do_charexp(* BIF_Traverser_state, *BIF_All)
|		:	Edit basestate value for this BIF attribute
|	do_charspace(* BIF_Traverser_state, *BIF_All)
|		:	Edit basestate value for this BIF attribute
|	do_depthcueindex(* BIF_Traverser_state, *BIF_All)
|		:	Edit basestate value for this BIF attribute
|	do_edgecolorindex(* BIF_Traverser_state, *BIF_All)
|		:	Edit basestate value for this BIF attribute
|	do_edgetype(* BIF_Traverser_state, *BIF_All)
|		:	Edit basestate value for this BIF attribute
|	do_interiorpatternindex(* BIF_Traverser_state, *BIF_All)
|		:	Edit basestate value for this BIF attribute
|	do_lineshading(* BIF_Traverser_state, *BIF_All)
|		:	Edit basestate value for this BIF attribute
|	do_annotextcharupvector(* BIF_Traverser_state, *BIF_All)
|		:	Edit basestate value for this BIF attribute
|	do_backfaceprocessing(* BIF_Traverser_state, *BIF_All)
|		:	Edit basestate value for this BIF attribute
|	do_backfaceproperties(* BIF_Traverser_state, *BIF_All)
|		:	Edit basestate value for this BIF attribute
|	do_lightstate(* BIF_Traverser_state, *BIF_All)
|		:	Edit basestate value for this BIF attribute
|	do_activeview(* BIF_Traverser_state, *BIF_All)
|		:	Edit basestate value for this BIF attribute
|	void init_base_state(int, int) 
|		:	Set up the base_state structure. 
|
\*--------------------------------------------------------------------*/

/*--------------------------------------------------------------------*\
|Include files  
\*--------------------------------------------------------------------*/
#include <stdio.h>
#include "biftypes.h"
#include "bifmacro.h"
#include "globals.h"
#include "ph_map.h"


/*----------------------------------------------------------------------*\
|Local #define
\*----------------------------------------------------------------------*/
#ifndef FALSE
#define FALSE 0
#endif
#ifndef TRUE
#define TRUE (!FALSE)
#endif

#define BBS_PSVWI    1 		/* Set base active view		*/
#define BBS_PSCHH    2 		/* char_height			*/
#define BBS_PSCHUP   3 		/* char_up_vector 		*/
#define BBS_PSEDCI   4		/* edge_color_index		*/
#define BBS_PSEDFG   5 		/* edge_flag			*/
#define BBS_PSEWSC   6 		/* edge_width			*/
#define BBS_PSGLT3   7 		/* global_transformation3	*/
#define BBS_PXSZBM   8 		/* hlhs_removal			*/
#define BBS_PSICI    9 		/* interior_color_index		*/
#define BBS_PSIRE   10 		/* interior_lighting		*/
#define BBS_PSISM   11 		/* interior_shading		*/
#define BBS_FPSBFCI 12		/* backface_interior_color_index*/
#define BBS_PSPLCI  13 		/* line_color_index		*/
#define BBS_PSLN    14 		/* line_type			*/
#define BBS_PSLWSC  15 		/* line_width			*/
#define BBS_PSLOT3  16 		/* local_transformation3	*/
#define BBS_PSPMCI  17 		/* marker_color_index		*/
#define BBS_PSMKSC  18 		/* marker_size			*/
#define BBS_PSMK    19 		/* marker_type			*/
#define BBS_PSAP    20 		/* surface_properties		*/
#define BBS_PSTXAL  21 		/* text_align			*/
#define BBS_PSTXCI  22 		/* text_color_index		*/
#define BBS_PSTXFN  23 		/* text_font			*/
#define BBS_PSTXP   24 		/* text_path			*/
#define BBS_PSTXPR  25 		/* text_prec			*/
#define BBS_PSIS    26		/* interior_style		*/
#define BBS_PSICO   27		/* interior_color		*/
#define BBS_PSPLCO  28		/* marker_color			*/
#define BBS_PSPMCO  29		/* line_color			*/
#define BBS_PSTXCO  30		/* text_color			*/
#define BBS_FPSBFIC 31		/* backface_interior_color	*/
#define BBS_PSECO   32		/* edge_color			*/
#define BBS_PSANS   33		/*				*/
#define BBS_PSATCH  34		/*				*/
#define BBS_PSCHXP  35		/*				*/
#define BBS_PSCHSP  36		/*				*/
#define BBS_PSDCI   37		/*				*/
#define BBS_PSEDT   38		/*				*/
#define BBS_FPSIPI  39		/*				*/
#define BBS_PSPLSM  40		/*				*/
#define BBS_PSATCHUP  41	/*				*/
#define BBS_FPBFPR  42		/*				*/
#define BBS_FPBFP   43		/*				*/
#define BBS_PSLSS   44 		/* light_state			*/
#define BBS_END     45 		/* end of base state		*/


#define   DEFAULT_AL3_CONCATTYPE 		BIF_REPLACE
#define   DEFAULT_AL3matrix   			identityMatrix
#define   DEFAULT_ANNOTCHUPV_X   		0.0
#define   DEFAULT_ANNOTCHUPV_Y   		1.0
#define   DEFAULT_ANNOTEXTCHARHEIGHT   		0.01
#define   DEFAULT_ANNOTEXTSTYLE 		1
#define   DEFAULT_ATG3matrix 			identityMatrix
#define   DEFAULT_BACKFACEINTERIORCOLORINDEX   	1
#define   DEFAULT_BACKFACEP_CULLF   		0
#define   DEFAULT_BACKFACEP_IDENTIFYF   	0
#define   DEFAULT_CHAREXP   			1.0
#define   DEFAULT_CHARHEIGHT   			0.01
#define   DEFAULT_CHARSPACE   			0
#define   DEFAULT_CHARUPCLASS_VEC_X   		0.0
#define   DEFAULT_CHARUPCLASS_VEC_Y   		1.0
#define   DEFAULT_COLORMODEL			BIF_RGB
#define   DEFAULT_DEPTHCUEINDEX   		0
#define   DEFAULT_EDGECOLORINDEX   		1
#define   DEFAULT_EDGEFLAG   			BIF_OFF
#define   DEFAULT_EDGETYPE   			1
#define   DEFAULT_EDGEWIDTH   			1.0
#define   DEFAULT_GT3matrix   			identityMatrix
#define   DEFAULT_HLHSREMOVAL   		BIF_HLHS_DISABLE
#define   DEFAULT_INTERIORCOLORINDEX   		1
#define   DEFAULT_INTERIORLIGHTING   		1
#define   DEFAULT_INTERIORPATTERNINDEX   	1
#define   DEFAULT_INTERIORSHADING   		1
#define   DEFAULT_INTERIORSTYLE   		BIF_HOLLOW
#define   DEFAULT_LINECOLORINDEX   		1
#define   DEFAULT_LINESHADING   		0
#define   DEFAULT_LINETYPE   			1
#define   DEFAULT_LINEWIDTH   			1.0
#define   DEFAULT_LT3_CONCATTYPE   		BIF_REPLACE
#define   DEFAULT_LT3matrix   			identityMatrix
#define   DEFAULT_MARKERCOLORINDEX   		1
#define   DEFAULT_MARKERSIZE   			1.0
#define   DEFAULT_MARKERTYPE   			3
#define   DEFAULT_SP_AMBIENT   			0.2
#define   DEFAULT_SP_COLOR_MODEL   		BIF_RGB
#define   DEFAULT_SP_DEFFUSE   			0.8
#define   DEFAULT_SP_HIGHLIGHT   		0.0
#define   DEFAULT_SP_SPECULAR   		0.0
#define   DEFAULT_SP_SPEC_COLOR1  		1.0
#define   DEFAULT_SP_SPEC_COLOR2  		1.0
#define   DEFAULT_SP_SPEC_COLOR3  		1.0
#define   DEFAULT_SP_TRANSPARENCY  		1.0
#define   DEFAULT_TEXTALIGN_H   		0
#define   DEFAULT_TEXTALIGN_V   		0
#define   DEFAULT_TEXTCOLORINDEX   		1
#define   DEFAULT_TEXTFONT   			1
#define   DEFAULT_TEXTPATH   			1
#define   DEFAULT_TEXTPREC 			BIF_STRING
/* We don't allow for a default COLOR TRIPLET 'cause PHIGS don't like it! */
#define   DEFAULT_TRUEC1 			-1.0
#define   DEFAULT_TRUEC2 			-1.0
#define   DEFAULT_TRUEC3 			-1.0
#define   DEFAULT_VIEWINDEX 			0

/*--------------------------------------------------------------------*\
|	Static Variables
\*--------------------------------------------------------------------*/
static matrix4 identityMatrix = 
{
	1., 0., 0., 0.,
	0., 1., 0., 0.,
	0., 0., 1., 0.,
	0., 0., 0., 1.
};

/*--------------------------------------------------------------------*\
| BEGIN PROCEDURE CODE
\*--------------------------------------------------------------------*/

/*--------------------------------------------------------------------*\
| Local Utility Functions
\*--------------------------------------------------------------------*/
#ifdef USING_PHIGS
/*--------------------------------------------------------------------*\
| Procedure     :	do_2_generic_i(*BIF_Traverser_state, int, *int, 
|					(*void)(), (*void)(), int)
|---------------------------------------------------------------------
| Description   :	Generic integer value basestate update function
|                       to support two calls per value
|---------------------------------------------------------------------
| Return        :	None.
\*--------------------------------------------------------------------*/
do_2_generic_i(traverser_state,inew_value,label,figs1_call,figs2_call,idefault)
BIF_Traverser_state *traverser_state;
int	inew_value;
int	label;
void	(*figs1_call)(),(*figs2_call)();
int	idefault;

{/* do_2_generic_i */
	int next_label; 
  
	if (traverser_state->push_level == 0) {
	    next_label = label + 1; 
	    if (traverser_state->nrs_state)  fxclns();
	    popen_struct((Pint)base_state_stid); 

	    pset_elem_ptr((Pint)0);
	    pdel_elems_labels((Pint)label,(Pint)next_label); 
	    if ( inew_value != idefault ) {
		(*figs1_call)((Pint)inew_value); 
	        (*figs2_call)((Pint)inew_value);
	    }

	    pclose_struct(); 
	    if (traverser_state->nrs_state)  fxopns(); 
	}
}/* do_2_generic_i */

/*--------------------------------------------------------------------*\
| Procedure     :	do_generic_i(*BIF_Traverser_state, int, *int, 
|					(*void)(), int)
|---------------------------------------------------------------------
| Description   :	Generic integer value basestate update function
|---------------------------------------------------------------------
| Return        :	None.
\*--------------------------------------------------------------------*/
do_generic_i(traverser_state,inew_value,label,figs_call,idefault)
BIF_Traverser_state *traverser_state;
int	inew_value;
int	label;
void	(*figs_call)();
int	idefault;

{/* do_generic_i */
	int next_label; 
  
	if (traverser_state->push_level == 0) {
	    next_label = label + 1; 
	    if (traverser_state->nrs_state)  fxclns();
	    popen_struct((Pint)base_state_stid); 

	    pset_elem_ptr((Pint)0);
	    pdel_elems_labels((Pint)label,(Pint)next_label); 
	    if ( inew_value != idefault ) 
		(*figs_call)((Pint)inew_value); 

	    pclose_struct(); 
	    if (traverser_state->nrs_state)  fxopns(); 
	}
}/* do_generic_i */

/*--------------------------------------------------------------------*\
| Procedure     :	do_generic_f(*BIF_Traverser_state, int, *int, 
|					(*void)(), float)
|---------------------------------------------------------------------
| Description   :	Generic float value basestate update function
|---------------------------------------------------------------------
| Return        :	
\*--------------------------------------------------------------------*/
do_generic_f(traverser_state,fnew_value,label,figs_call,fdefault)
BIF_Traverser_state *traverser_state;
float	fnew_value;
int	label;
void	(*figs_call)();
float	fdefault;

{
  int next_label; 
  if (traverser_state->push_level == 0) {
    next_label = label + 1; 
    if (traverser_state->nrs_state)  fxclns(); 
    popen_struct((Pint)base_state_stid); 
    pset_elem_ptr((Pint)0);
    pdel_elems_labels((Pint)label,(Pint)next_label); 
    if ( fnew_value != fdefault ) 
      (*figs_call)(fnew_value); 
    pclose_struct(); 
    if (traverser_state->nrs_state)  fxopns(); 
  }
}

/*--------------------------------------------------------------------*\
| Procedure     :	do_generic_c3(*BIF_Traverser_state, *BIF_All,
|					*int, *int, *int, (*void)())
|---------------------------------------------------------------------
| Description   :	Generic true color basestate update function
|---------------------------------------------------------------------
| Return        :	
\*--------------------------------------------------------------------*/
do_generic_c3(traverser_state,entity,label,figs_call)
BIF_Traverser_state *traverser_state;
BIF_All *entity;
int	label;
void	(*figs_call)();

{/* do_generic_c3 */
    int next_label,color_model; 
    Pgcolr gcolor;

    if (traverser_state->push_level == 0) {
	color_model = entity->truecolor.color_model;
    
	next_label = label + 1; 
	if (traverser_state->nrs_state)  fxclns(); 
	popen_struct((Pint)base_state_stid); 

	pset_elem_ptr((Pint)0);
	pdel_elems_labels((Pint)label,(Pint)next_label); 
	/* WORKING: ADD CONVERSIONS FOR DIFFERENT COLOR_MODELS */
	if ( color_model != DEFAULT_COLORMODEL || 
	    entity->truecolor.color[0] != DEFAULT_TRUEC1 || 
	    entity->truecolor.color[1] != DEFAULT_TRUEC2 || 
	    entity->truecolor.color[2] != DEFAULT_TRUEC3) {
      
	    gcolor.type = color_model;
	    gcolor.val.general.x = (Pfloat)entity->truecolor.color[0];
	    gcolor.val.general.y = (Pfloat)entity->truecolor.color[1];
	    gcolor.val.general.z = (Pfloat)entity->truecolor.color[2];
	    (*figs_call)(&gcolor);
	}

	pclose_struct();

	if (traverser_state->nrs_state)  fxopns(); 
    }
}/* do_generic_c3 */

/*--------------------------------------------------------------------*\
| Procedure     :	do_generic_colorindex(*BIF_Traverser_state, int,
|				*int, *int, *int, (*void)(), int)
|---------------------------------------------------------------------
| Description   :	Generic index color basestate update function
|---------------------------------------------------------------------
| Return        :	
\*--------------------------------------------------------------------*/
do_generic_colorindex(traverser_state,inew_value,label,
			figs_call,idefault)
BIF_Traverser_state *traverser_state;
int	inew_value;
int	label;
void	(*figs_call)();
int	idefault;

{/* do_generic_colorindex */
    int next_label; 
    Pgcolr gcolor;
    if (traverser_state->push_level == 0) {
	next_label = label + 1; 
	if (traverser_state->nrs_state)  fxclns(); 
	popen_struct((Pint)base_state_stid); 

	pset_elem_ptr((Pint)0);
	pdel_elems_labels((Pint)label,(Pint)next_label); 
	if ( inew_value != idefault ) 
	    gcolor.type = PINDIRECT;
	    gcolor.val.ind = (Pint)inew_value;
	    (*figs_call)(&gcolor);
    
	pclose_struct();
	if (traverser_state->nrs_state)  fxopns(); 
    }
}/* do_generic_colorindex */
#endif /* ifdef USING_PHIGS */

/*----------------------------------------------------------------------*\
| Procedure     :	do_markertype(* BIF_Traverser_state, *BIF_All)
|------------------------------------------------------------------------
| Description   :	Edit basestate value for this BIF attribute
|		 Patches the contents of the entity markertype to the
|		 phigs base_state structure.  For further details 
|		 see the Descriptions block at the top of this file.
|------------------------------------------------------------------------
| Return        :	
\*----------------------------------------------------------------------*/
int do_markertype( traverser_state, entity )
BIF_Traverser_state *traverser_state;
BIF_All *entity;

{
#ifdef USING_PHIGS
	do_generic_i(traverser_state,entity->markertype.markertype,
			BBS_PSMK,pset_marker_type, DEFAULT_MARKERTYPE);
#else /* ifdef USING_PHIGS */
#endif /* ifdef USING_PHIGS */
	return(0);
} /* End do_markertype */

/*----------------------------------------------------------------------*\
| Procedure     :	do_markersize(* BIF_Traverser_state, *BIF_All)
|------------------------------------------------------------------------
| Description   :	Edit basestate value for this BIF attribute
|		 Patches the contents of the entity markersize to the
|		 phigs base_state structure.  For further details 
|		 see the Descriptions block at the top of this file.
|------------------------------------------------------------------------
| Return        :	
\*----------------------------------------------------------------------*/
int do_markersize( traverser_state, entity )
BIF_Traverser_state *traverser_state;
BIF_All *entity;
{
#ifdef USING_PHIGS
	do_generic_f(traverser_state,entity->markersize.size,
			BBS_PSMKSC,pset_marker_size, DEFAULT_MARKERSIZE);
#else /* ifdef USING_PHIGS */
#endif /* ifdef USING_PHIGS */
	return(0);
} /* End do_markersize */

/*----------------------------------------------------------------------*\
| Procedure     :	do_markercolorindex(* BIF_Traverser_state,
|						*BIF_All)
|------------------------------------------------------------------------
| Description   :	Edit basestate value for this BIF attribute
|		 Patches the contents of the entity markercolorindex to 
|		 the phigs base_state structure.  For further details 
|		 see the Descriptions block at the top of this file.
|------------------------------------------------------------------------
| Return        :	
\*----------------------------------------------------------------------*/
int do_markercolorindex( traverser_state, entity )
BIF_Traverser_state *traverser_state;
BIF_All *entity;
{
#ifdef USING_PHIGS
	do_generic_colorindex(traverser_state,entity->ind.ind,
			      BBS_PSPMCI, pset_marker_colr,
			      DEFAULT_MARKERCOLORINDEX);
#else /* ifdef USING_PHIGS */
#endif /* ifdef USING_PHIGS */
	return(0);
} /* End do_markercolorindex */

/*----------------------------------------------------------------------*\
| Procedure     :	do_linetype(* BIF_Traverser_state, *BIF_All)
|------------------------------------------------------------------------
| Description   :	Edit basestate value for this BIF attribute
|		 Patches the contents of the entity linetype to the
|		 phigs base_state structure.  For further details 
|		 see the Descriptions block at the top of this file.
|------------------------------------------------------------------------
| Return        :	
\*----------------------------------------------------------------------*/
int do_linetype( traverser_state, entity )
BIF_Traverser_state *traverser_state;
BIF_All *entity;
{
#ifdef USING_PHIGS
	do_generic_i(traverser_state,entity->linetype.linetype,
			BBS_PSLN,pset_linetype, DEFAULT_LINETYPE);
#else /* ifdef USING_PHIGS */
#endif /* ifdef USING_PHIGS */
	return(0);
} /* End do_linetype */

/*----------------------------------------------------------------------*\
| Procedure     :	do_linewidth(* BIF_Traverser_state, *BIF_All)
|------------------------------------------------------------------------
| Description   :	Edit basestate value for this BIF attribute
|		 Patches the contents of the entity linewidth to the
|		 phigs base_state structure.  For further details 
|		 see the Descriptions block at the top of this file.
|------------------------------------------------------------------------
| Return        :	
\*----------------------------------------------------------------------*/
int do_linewidth( traverser_state, entity )
BIF_Traverser_state *traverser_state;
BIF_All *entity;
{
#ifdef USING_PHIGS
	do_generic_f(traverser_state,entity->linewidth.width,
			BBS_PSLWSC,pset_linewidth, DEFAULT_LINEWIDTH);
#else /* ifdef USING_PHIGS */
#endif /* ifdef USING_PHIGS */
	return(0);
} /* End do_linewidth */

/*----------------------------------------------------------------------*\
| Procedure     :	do_linecolorindex(* BIF_Traverser_state,
|						*BIF_All)
|------------------------------------------------------------------------
| Description   :	Edit basestate value for this BIF attribute
|		 Patches the contents of the entity linecolorindex to the
|		 phigs base_state structure.  For further details 
|		 see the Descriptions block at the top of this file.
|------------------------------------------------------------------------
| Return        :	
\*----------------------------------------------------------------------*/
int do_linecolorindex( traverser_state, entity )
BIF_Traverser_state *traverser_state;
BIF_All *entity;
{
#ifdef USING_PHIGS
	do_generic_colorindex(traverser_state,entity->ind.ind,
			      BBS_PSPLCI, pset_line_colr,
			      DEFAULT_LINECOLORINDEX);
#else /* ifdef USING_PHIGS */
#endif /* ifdef USING_PHIGS */
	return(0);
} /* End do_linecolorindex */

/*----------------------------------------------------------------------*\
| Procedure     :	do_interiorstyle(* BIF_Traverser_state,
|						*BIF_All)
|------------------------------------------------------------------------
| Description   :	Edit basestate value for this BIF attribute
|		 Patches the contents of the entity interiorcolorindex to 
|		 the phigs base_state structure.  For further details 
|		 see the Descriptions block at the top of this file.
|------------------------------------------------------------------------
| Return        :	
\*----------------------------------------------------------------------*/
int do_interiorstyle( traverser_state, entity )
BIF_Traverser_state *traverser_state;
BIF_All *entity;
{
#ifdef USING_PHIGS
	/* Do back interior style also */
	do_2_generic_i(traverser_state,entity->interiorstyle.ind,
		       BBS_PSIS,pset_int_style,
		       pset_back_int_style,
		       DEFAULT_INTERIORSTYLE);
#else /* ifdef USING_PHIGS */
#endif /* ifdef USING_PHIGS */
	return(0);
} /* End do_interiorstyle */

/*----------------------------------------------------------------------*\
| Procedure     :	do_interiorcolorindex(* BIF_Traverser_state,
|						*BIF_All)
|------------------------------------------------------------------------
| Description   :	Edit basestate value for this BIF attribute
|		 Patches the contents of the entity interiorcolorindex to 
|		 the phigs base_state structure.  For further details 
|		 see the Descriptions block at the top of this file.
|------------------------------------------------------------------------
| Return        :	
\*----------------------------------------------------------------------*/
int do_interiorcolorindex( traverser_state, entity )
BIF_Traverser_state *traverser_state;
BIF_All *entity;
{
#ifdef USING_PHIGS
	do_generic_colorindex(traverser_state,entity->ind.ind,
			      BBS_PSICI, pset_int_colr,
			      DEFAULT_INTERIORCOLORINDEX);
#else /* ifdef USING_PHIGS */
#endif /* ifdef USING_PHIGS */
	return(0);
} /* End do_interiorcolorindex */

/*----------------------------------------------------------------------*\
| Procedure     :	do_interiorshading(* BIF_Traverser_state,
|						*BIF_All)
|------------------------------------------------------------------------
| Description   :	Edit basestate value for this BIF attribute
|		 Patches the contents of the entity interiorshading
|	to the phigs base_state structure.  For further details 
|	see the Descriptions block at the top of this file.
|------------------------------------------------------------------------
| Return        :	
\*----------------------------------------------------------------------*/
int do_interiorshading( traverser_state, entity )
BIF_Traverser_state *traverser_state;
BIF_All *entity;
{
#ifdef USING_PHIGS
	/* Do back interior shading also */
	do_2_generic_i(traverser_state,entity->interiorshading.meth,
		       BBS_PSISM,pset_int_shad_meth,
		       pset_back_int_shad_meth,
		       DEFAULT_INTERIORSHADING);
#else /* ifdef USING_PHIGS */
#endif /* ifdef USING_PHIGS */
	return(0);
} /* End do_interiorshading */

/*----------------------------------------------------------------------*\
| Procedure     :	do_interiorlighting(* BIF_Traverser_state,
|						*BIF_All)
|------------------------------------------------------------------------
| Description   :	Edit basestate value for this BIF attribute
|		 Patches the contents of the entity interiorlighting to 
|		 the phigs base_state structure.  For further details 
|		 see the Descriptions block at the top of this file.
|------------------------------------------------------------------------
| Return        :	
\*----------------------------------------------------------------------*/
int do_interiorlighting( traverser_state, entity )
BIF_Traverser_state *traverser_state;
BIF_All *entity;
{
#ifdef USING_PHIGS
	/* Do back interior lighting also */
	do_2_generic_i(traverser_state,entity->interiorlighting.equation,
		       BBS_PSIRE,pset_refl_eqn,
		       pset_back_refl_eqn,
		       DEFAULT_INTERIORLIGHTING);
#else /* ifdef USING_PHIGS */
#endif /* ifdef USING_PHIGS */
	return(0);
} /* End do_interiorlighting */

/*----------------------------------------------------------------------*\
| Procedure     :	do_surfaceproperties(* BIF_Traverser_state,
|						*BIF_All)
|------------------------------------------------------------------------
| Description   :	Edit basestate value for this BIF attribute
|		 Patches the contents of the entity surfaceproperties to
|		 the phigs base_state structure.  For further details 
|		 see the Descriptions block at the top of this file.
|------------------------------------------------------------------------
| Return        :	
\*----------------------------------------------------------------------*/
int do_surfaceproperties( traverser_state, entity )
BIF_Traverser_state *traverser_state;
BIF_All *entity;
{
    float ambient, diffuse, specular;
    int color_model;
    float specular_color[3]; /* Specular color components */
    float highlight, transparency;
    int next_label;

#ifdef USING_PHIGS
    if (traverser_state->push_level == 0) {
	ambient = entity->surfaceproperties.props.ambient_coef;
	diffuse = entity->surfaceproperties.props.diffuse_coef;
	specular = entity->surfaceproperties.props.specular_coef;
	color_model =
	    entity->surfaceproperties.props.specular_colr.type;
	switch (color_model) {
	  case PINDIRECT:
	    specular_color[0] = 
		(float)entity->surfaceproperties.props.specular_colr.val.ind;
	    break;
	  default:
	    specular_color[0] = 
		entity->surfaceproperties.props.specular_colr.val.general.x;
	    specular_color[1] = 
		entity->surfaceproperties.props.specular_colr.val.general.y;
	    specular_color[2] = 
		entity->surfaceproperties.props.specular_colr.val.general.z;
	    break;
	}
	highlight = entity->surfaceproperties.props.specular_exp;
	transparency = 0.0;

	next_label = BBS_PSAP + 1;
	if (traverser_state->nrs_state)  fxclns();
	popen_struct((Pint)base_state_stid);

	pset_elem_ptr((Pint)0);
	pdel_elems_labels((Pint)BBS_PSAP,(Pint)next_label); 

#ifdef TEMPOUT
	if ( ambient != DEFAULT_SP_AMBIENT ||
	    diffuse  != DEFAULT_SP_DEFFUSE ||
	    specular != DEFAULT_SP_SPECULAR ||
	    color_model != DEFAULT_SP_COLOR_MODEL ||
	    specular_color[0] != DEFAULT_SP_SPEC_COLOR1 ||
	    specular_color[1] != DEFAULT_SP_SPEC_COLOR2 ||
	    specular_color[2] != DEFAULT_SP_SPEC_COLOR3 ||
	    highlight != DEFAULT_SP_HIGHLIGHT ||
	    transparency != DEFAULT_SP_TRANSPARENCY
	    )
#endif
	    pset_refl_props((Prefl_props *)&entity->surfaceproperties.props);

	pclose_struct();
	if (traverser_state->nrs_state)  fxopns();
    }
#else /* ifdef USING_PHIGS */
#endif /* ifdef USING_PHIGS */
  return(0);
} /* End do_surfaceproperties */

/*----------------------------------------------------------------------*\
| Procedure     :	do_edgeflag(* BIF_Traverser_state, *BIF_All)
|------------------------------------------------------------------------
| Description   :	Edit basestate value for this BIF attribute
|		 Patches the contents of the entity edgeflag to the
|		 phigs base_state structure.  For further details 
|		 see the Descriptions block at the top of this file.
|------------------------------------------------------------------------
| Return        :	
\*----------------------------------------------------------------------*/
int do_edgeflag( traverser_state, entity )
BIF_Traverser_state *traverser_state;
BIF_All *entity;
{
#ifdef USING_PHIGS
	do_generic_i(traverser_state,entity->edgeflag.edge_flag,
			BBS_PSEDFG,pset_edge_flag, DEFAULT_EDGEFLAG);
#else /* ifdef USING_PHIGS */
#endif /* ifdef USING_PHIGS */
	return(0);
} /* End do_edgeflag */

/*----------------------------------------------------------------------*\
| Procedure     :	do_edgewidth(* BIF_Traverser_state, *BIF_All)
|------------------------------------------------------------------------
| Description   :	Edit basestate value for this BIF attribute
|		 Patches the contents of the entity edgewidth to the
|		 phigs base_state structure.  For further details 
|		 see the Descriptions block at the top of this file.
|------------------------------------------------------------------------
| Return        :	
\*----------------------------------------------------------------------*/
int do_edgewidth( traverser_state, entity )
BIF_Traverser_state *traverser_state;
BIF_All *entity;
{
#ifdef USING_PHIGS
	do_generic_f(traverser_state,entity->edgewidth.scale,
			BBS_PSEWSC,pset_edgewidth, DEFAULT_EDGEWIDTH);
#else /* ifdef USING_PHIGS */
#endif /* ifdef USING_PHIGS */
	return(0);
} /* End do_edgewidth */

/*----------------------------------------------------------------------*\
| Procedure     :	do_textfont(* BIF_Traverser_state, *BIF_All)
|------------------------------------------------------------------------
| Description   :	Edit basestate value for this BIF attribute
|		 Patches the contents of the entity textfont to the
|		 phigs base_state structure.  For further details 
|		 see the Descriptions block at the top of this file.
|------------------------------------------------------------------------
| Return        :	
\*----------------------------------------------------------------------*/
int do_textfont( traverser_state, entity )
BIF_Traverser_state *traverser_state;
BIF_All *entity;
{
#ifdef USING_PHIGS
	do_generic_i(traverser_state,entity->textfont.font,
			BBS_PSTXFN,pset_text_font, DEFAULT_TEXTFONT);
#else /* ifdef USING_PHIGS */
#endif /* ifdef USING_PHIGS */
	return(0);
} /* End do_textfont */

/*----------------------------------------------------------------------*\
| Procedure     :	do_textprec(* BIF_Traverser_state, *BIF_All)
|------------------------------------------------------------------------
| Description   :	Edit basestate value for this BIF attribute
|		 Patches the contents of the entity textprec to the
|		 phigs base_state structure.  For further details 
|		 see the Descriptions block at the top of this file.
|------------------------------------------------------------------------
| Return        :	
\*----------------------------------------------------------------------*/
int do_textprec( traverser_state, entity )
BIF_Traverser_state *traverser_state;
BIF_All *entity;
{
#ifdef USING_PHIGS
	do_generic_i(traverser_state,entity->textprec.precision,
			BBS_PSTXPR,pset_text_prec, DEFAULT_TEXTPREC);
#else /* ifdef USING_PHIGS */
#endif /* ifdef USING_PHIGS */
	return(0);
} /* End do_textprec */

/*----------------------------------------------------------------------*\
| Procedure     :	do_textcolorindex(* BIF_Traverser_state, *BIF_All)
|------------------------------------------------------------------------
| Description   :	Edit basestate value for this BIF attribute
|		 Patches the contents of the entity textcolorindex to the
|		 phigs base_state structure.  For further details 
|		 see the Descriptions block at the top of this file.
|------------------------------------------------------------------------
| Return        :	
\*----------------------------------------------------------------------*/
int do_textcolorindex( traverser_state, entity )
BIF_Traverser_state *traverser_state;
BIF_All *entity;
{
#ifdef USING_PHIGS
	do_generic_colorindex(traverser_state,entity->ind.ind,
			      BBS_PSTXCI, pset_text_colr,
			      DEFAULT_TEXTCOLORINDEX);
#else /* ifdef USING_PHIGS */
#endif /* ifdef USING_PHIGS */
	return(0);
} /* End do_textcolorindex */

/*----------------------------------------------------------------------*\
| Procedure     :	do_textpath(* BIF_Traverser_state, *BIF_All)
|------------------------------------------------------------------------
| Description   :	Edit basestate value for this BIF attribute
|		 Patches the contents of the entity textpath to the
|		 phigs base_state structure.  For further details 
|		 see the Descriptions block at the top of this file.
|------------------------------------------------------------------------
| Return        :	
\*----------------------------------------------------------------------*/
int do_textpath( traverser_state, entity )
BIF_Traverser_state *traverser_state;
BIF_All *entity;
{
#ifdef USING_PHIGS
	extern void fxsattxp();
	do_generic_i(traverser_state,entity->textpath.path_list,
			BBS_PSTXP,fxsattxp, DEFAULT_TEXTPATH);
#else /* ifdef USING_PHIGS */
#endif /* ifdef USING_PHIGS */
	return(0);
} /* End do_textpath */

/*----------------------------------------------------------------------*\
| Procedure     :	do_textalign(* BIF_Traverser_state, *BIF_All)
|------------------------------------------------------------------------
| Description   :	Edit basestate value for this BIF attribute
|		 Patches the contents of the entity textalign to the
|		 phigs base_state structure.  For further details 
|		 see the Descriptions block at the top of this file.
|------------------------------------------------------------------------
| Return        :	
\*----------------------------------------------------------------------*/
int do_textalign( traverser_state, entity )
BIF_Traverser_state *traverser_state;
BIF_All *entity;
{
  int horizontal_alignment, vertical_alignment;
  int next_label;
#ifdef USING_PHIGS
  if (traverser_state->push_level == 0) {
    next_label = BBS_PSTXAL + 1;
    if (traverser_state->nrs_state)  fxclns();
    horizontal_alignment = (int)entity->textalign.text_align.hor;
    vertical_alignment = (int)entity->textalign.text_align.vert;
    popen_struct((Pint)base_state_stid);
    pset_elem_ptr((Pint)0);
    pdel_elems_labels((Pint)BBS_PSTXAL,(Pint)next_label); 
    if ( horizontal_alignment!= DEFAULT_TEXTALIGN_H || 
	vertical_alignment != DEFAULT_TEXTALIGN_V ) 
      fxsattxal((Ptext_align *)&entity->textalign.text_align);
    pclose_struct();
    if (traverser_state->nrs_state)  fxopns();
  }
#else /* ifdef USING_PHIGS */
#endif /* ifdef USING_PHIGS */
	return(0);
} /* End do_textalign */

/*----------------------------------------------------------------------*\
| Procedure     :	do_charheight(* BIF_Traverser_state, *BIF_All)
|------------------------------------------------------------------------
| Description   :	Edit basestate value for this BIF attribute
|		 Patches the contents of the entity charheight to the
|		 phigs base_state structure.  For further details 
|		 see the Descriptions block at the top of this file.
|------------------------------------------------------------------------
| Return        :	
\*----------------------------------------------------------------------*/
int do_charheight( traverser_state, entity )
BIF_Traverser_state *traverser_state;
BIF_All *entity;
{
#ifdef USING_PHIGS
	do_generic_f(traverser_state,entity->charheight.height,
			BBS_PSCHH,pset_char_ht, DEFAULT_CHARHEIGHT);
#else /* ifdef USING_PHIGS */
#endif /* ifdef USING_PHIGS */
	return(0);
} /* End do_charheight */

/*----------------------------------------------------------------------*\
| Procedure     :	do_charupvector(* BIF_Traverser_state,
*BIF_All)
|------------------------------------------------------------------------
| Description   :	Edit basestate value for this BIF attribute
|		 Patches the contents of the entity charupvector to the
|		 phigs base_state structure.  For further details 
|		 see the Descriptions block at the top of this file.
|------------------------------------------------------------------------
| Return        :	
\*----------------------------------------------------------------------*/
int do_charupvector( traverser_state, entity )
BIF_Traverser_state *traverser_state;
BIF_All *entity;
{
  float x, y;
  int next_label;
#ifdef USING_PHIGS
  if (traverser_state->push_level == 0) {
    next_label = BBS_PSCHUP + 1;
    if (traverser_state->nrs_state)  fxclns();
    x = entity->charupvector.up_vect.delta_x;
    y = entity->charupvector.up_vect.delta_y;
    popen_struct((Pint)base_state_stid); 
    pset_elem_ptr((Pint)0);
    pdel_elems_labels((Pint)BBS_PSCHUP,(Pint)next_label); 
    if ( x != DEFAULT_CHARUPCLASS_VEC_X || 
	y != DEFAULT_CHARUPCLASS_VEC_Y 
	) 
      pset_char_up_vec((Pvec *)&entity->charupvector.up_vect);
    pclose_struct();
    if (traverser_state->nrs_state)  fxopns();
  }
#else /* ifdef USING_PHIGS */
#endif /* ifdef USING_PHIGS */
	return(0);
} /* End do_charupvector */

/*----------------------------------------------------------------------*\
| Procedure     :	do_hlhsremoval(* BIF_Traverser_state, *BIF_All)
|------------------------------------------------------------------------
| Description   :	Edit basestate value for this BIF attribute
|		 Patches the contents of the entity hlhsremoval to the
|		 phigs base_state structure.  For further details 
|		 see the Descriptions block at the top of this file.
|------------------------------------------------------------------------
| Return        :	
\*----------------------------------------------------------------------*/
int do_hlhsremoval( traverser_state, entity )
BIF_Traverser_state *traverser_state;
BIF_All *entity;
{
#ifdef USING_PHIGS
	do_generic_i(traverser_state,entity->hlhsremoval.id,
			BBS_PXSZBM,pset_hlhsr_id, DEFAULT_HLHSREMOVAL);
#else /* ifdef USING_PHIGS */
#endif /* ifdef USING_PHIGS */
	return(0);
} /* End do_hlhsremoval */


/*----------------------------------------------------------------------*\
| Procedure     :	do_globaltransformation3(* BIF_Traverser_state,
*BIF_All)
|------------------------------------------------------------------------
| Description   :	Edit basestate value for this BIF attribute
|		 Patches the contents of the entity globaltransformation3 
|		 to the phigs base_state structure.  For further details 
|		 see the Descriptions block at the top of this file.
|------------------------------------------------------------------------
| Return        :	
\*----------------------------------------------------------------------*/
int do_globaltransformation3( traverser_state, entity )
BIF_Traverser_state *traverser_state;
BIF_All *entity;
{
  float matrix[4][4];
  int next_label;
#ifdef USING_PHIGS
  if (traverser_state->push_level == 0) {
    next_label = BBS_PSGLT3 + 1;
    if (traverser_state->nrs_state)  fxclns();
    Transpose44(entity->globaltransformation3.xform,matrix);
    popen_struct((Pint)base_state_stid); 
    pset_elem_ptr((Pint)0);
    pdel_elems_labels((Pint)BBS_PSGLT3,(Pint)next_label); 
    if ( !mx_comp(entity->globaltransformation3.xform,DEFAULT_GT3matrix) )
      pset_global_tran3(matrix);
    pclose_struct();
    if (traverser_state->nrs_state)  fxopns();
  }
#else /* ifdef USING_PHIGS */
#endif /* ifdef USING_PHIGS */

  /*------------------------------------------------------------*\
  |	Update the data in the traverser_state to reflect this
  |	matrix operation on G and C.
  \*------------------------------------------------------------*/
  /* Global    */
  copy44f(entity->globaltransformation3.xform,traverser_state->global);
  /* Composite */
  mx_mult(traverser_state->local,traverser_state->global,
	  traverser_state->composite);
  
  return(0);
} /* End do_globaltransformation3 */

/*----------------------------------------------------------------------*\
| Procedure     :	do_localtransformation3(* BIF_Traverser_state,
|						*BIF_All)
|------------------------------------------------------------------------
| Description   :	Edit basestate value for this BIF attribute
|		 Patches the contents of the entity localtransformation3 
|		 to the phigs base_state structure.  For further details 
|		 see the Descriptions block at the top of this file.
|------------------------------------------------------------------------
| Return        :	
\*----------------------------------------------------------------------*/
int do_localtransformation3( traverser_state, entity )
BIF_Traverser_state *traverser_state;
BIF_All *entity;
{
  float matrix[4][4];
  int concat_type;
  int next_label;
  concat_type = (int)entity->localtransformation3.compose_type;
#ifdef USING_PHIGS
  if (traverser_state->push_level == 0) {
    next_label = BBS_PSLOT3 + 1;
    if (traverser_state->nrs_state)  fxclns();
    Transpose44(entity->localtransformation3.xform,matrix);
    popen_struct((Pint)base_state_stid);
    pset_elem_ptr((Pint)0);
    pdel_elems_labels((Pint)BBS_PSLOT3,(Pint)next_label); 
    if ( !mx_comp(entity->localtransformation3.xform,DEFAULT_LT3matrix) ||
	concat_type != DEFAULT_LT3_CONCATTYPE 
	)
      pset_local_tran3(matrix,concat_type);
    pclose_struct();
		if (traverser_state->nrs_state)  fxopns();
	}
#else /* ifdef USING_PHIGS */
#endif /* ifdef USING_PHIGS */

	/*------------------------------------------------------------*\
	|	Update the data in the traverser_state to reflect this
	|	matrix operation on G, L, and C.
	\*------------------------------------------------------------*/
	/* Local     */
	matcat(entity->localtransformation3.xform,
	       traverser_state->local,concat_type);
	/* Composite */
	mx_mult(traverser_state->local,traverser_state->global,
		traverser_state->composite);

	return(0);
} /* End do_localtransformation3 */

/*----------------------------------------------------------------------*\
| Procedure     :	do_applytoglobal3(* BIF_Traverser_state,
|						*BIF_All)
|------------------------------------------------------------------------
| Description   :	Edit basestate value for this BIF attribute
|		 Patches the contents of the entity localtransformation3 
|		 to the phigs base_state structure.  For further details 
|		 see the Descriptions block at the top of this file.
|------------------------------------------------------------------------
| Return        :	
\*----------------------------------------------------------------------*/
int do_applytoglobal3( traverser_state, entity )
BIF_Traverser_state *traverser_state;
BIF_All *entity;
{
  float matrix[4][4];
  int matrix_id;
  int next_label;
  matrix_id = entity->applytoglobal3.matrix_id;

#ifdef USING_PHIGS
  if (traverser_state->nrs_state)  fxclns();
  Transpose44(matrix_table[matrix_id],matrix);
#ifdef TEST_PRINT
#ifdef TEST_PRINT2
  printf("A2G3: mx_id = %d label = %d\n",matrix_id,
	 entity->applytoglobal3.matrix_label);
  PRINT_MATRIX44f(matrix);
#endif /* TEST_PRINT2 */
#endif /* TEST_PRINT */
  if (traverser_state->push_level == 0) {
      next_label = BBS_PSGLT3 + 1;
      popen_struct((Pint)base_state_stid);
      pset_elem_ptr((Pint)0);
      pdel_elems_labels((Pint)BBS_PSGLT3,(Pint)next_label); 
      if ( !mx_comp(matrix_table[matrix_id],DEFAULT_ATG3matrix))
	pset_global_tran3(matrix);
      pclose_struct();
  }

#ifdef REFER_STRUCTURE_EXISTS
	/* Ordinary PHIGs Open */
  popen_struct((Pint)entity->applytoglobal3.structure_id);
  pset_elem_ptr((Pint)0);
#else /* REFER_STRUCTURE_EXISTS */
	/*------------------------------------------------------------*\
	|	Using the link list built up by call and execute open
	|	and shift to the appropriate BIF structure in the 
	|	expanded PHIGs structures.
	\*------------------------------------------------------------*/
  openUsingLinkList(traverser_state->tol);
#endif /* REFER_STRUCTURE_EXISTS */
  pset_elem_ptr_label((Pint)entity->applytoglobal3.matrix_label);
  poffset_elem_ptr((Pint)1);
  pdel_elem();
  pset_global_tran3(matrix);
  pclose_struct();
  if (traverser_state->nrs_state)  fxopns();
#else /* ifdef USING_PHIGS */
#endif /* ifdef USING_PHIGS */

	/*------------------------------------------------------------*\
	|	Update the data in the traverser_state to reflect this
	|	matrix operation on G and C.
	\*------------------------------------------------------------*/
  /* Global    */
  copy44f(matrix_table[matrix_id],traverser_state->global);
  /* Composite */
  mx_mult(traverser_state->local,traverser_state->global,
	  traverser_state->composite);

  return(0);
} /* End do_applytoglobal3 */

/*----------------------------------------------------------------------*\
| Procedure     :	do_applytolocal3(* BIF_Traverser_state,
|						*BIF_All)
|------------------------------------------------------------------------
| Description   :	Edit basestate value for this BIF attribute
|		 Patches the contents of the entity localtransformation3 
|		 to the phigs base_state structure.  For further details 
|		 see the Descriptions block at the top of this file.
|------------------------------------------------------------------------
| Return        :	
\*----------------------------------------------------------------------*/
int do_applytolocal3( traverser_state, entity )
BIF_Traverser_state *traverser_state;
BIF_All *entity;
{
    float matrix[4][4];
    int matrix_id;
    int concat_type;
    int next_label;
    matrix_id = entity->applytolocal3.matrix_id;
    concat_type = entity->applytolocal3.concat_type;
#ifdef USING_PHIGS
    if (traverser_state->nrs_state)  fxclns();
    Transpose44(matrix_table[matrix_id],matrix);
#ifdef TEST_PRINT
#ifdef TEST_PRINT2
    printf("A2L3: mx_id = %d concat = %d label = %d\n",matrix_id, concat_type,
	   entity->applytolocal3.matrix_label);
PRINT_MATRIX44f(matrix);
#endif /* TEST_PRINT2 */
#endif /* TEST_PRINT */
    if (traverser_state->push_level == 0) {
	next_label = BBS_PSLOT3 + 1;
	popen_struct((Pint)base_state_stid);

	pset_elem_ptr((Pint)0);
	pdel_elems_labels((Pint)BBS_PSLOT3,(Pint)next_label); 
	if ( !mx_comp(matrix_table[matrix_id],DEFAULT_AL3matrix)||
	    concat_type != DEFAULT_AL3_CONCATTYPE)
	    pset_local_tran3(matrix,concat_type);

	pclose_struct();
    }
/* WORKING: Add the CALL Structure "fancy open" here */
	
#ifdef REFER_STRUCTURE_EXISTS
    /* Ordinary PHIGs Open */
    popen_struct((Pint)entity->applytolocal3.structure_id);

    pset_elem_ptr((Pint)0);
#else /* REFER_STRUCTURE_EXISTS */
    /*------------------------------------------------------------*\
    | Using the link list built up by call and execute open
    | and shift to the appropriate BIF structure in the 
    | expanded PHIGs structures.
    \*------------------------------------------------------------*/
    openUsingLinkList(traverser_state->tol);
#endif /* REFER_STRUCTURE_EXISTS */
    pset_elem_ptr_label((Pint)entity->applytolocal3.matrix_label);
    poffset_elem_ptr((Pint)1);
    pdel_elem();
    pset_local_tran3(matrix,concat_type);

    pclose_struct();
    if (traverser_state->nrs_state)  fxopns();
#else /* ifdef USING_PHIGS */
#endif /* ifdef USING_PHIGS */

    /*------------------------------------------------------------*\
    | Update the data in the traverser_state to reflect this
    | matrix operation on G, L, and C.
    \*------------------------------------------------------------*/
    /* Local     */
    matcat(matrix_table[matrix_id],
	   traverser_state->local,concat_type);
    /* Composite */
    mx_mult(traverser_state->local,traverser_state->global,
	    traverser_state->composite);

  return(0);
} /* End do_applytolocal3 */

/*--------------------------------------------------------------------*\
| Procedure     :	do_interiorcolor(* BIF_Traverser_state,
|						*BIF_All)
|----------------------------------------------------------------------
| Description   :	Edit basestate value for this BIF attribute
|		 Patches the contents of the entity interiorcolor to the
|		  phigs base_state structure.  For further details 
|		  see the Descriptions block at the top of this file.
|----------------------------------------------------------------------
| Return        :	
\*--------------------------------------------------------------------*/
int do_interiorcolor( traverser_state, entity )
BIF_Traverser_state *traverser_state;
BIF_All *entity;
{
#ifdef USING_PHIGS
	do_generic_c3(traverser_state,entity,BBS_PSICO, pset_int_colr);
#else /* ifdef USING_PHIGS */
#endif /* ifdef USING_PHIGS */
	return(0);
} /* End do_markertype */

/*--------------------------------------------------------------------*\
| Procedure     :	do_markercolor(* BIF_Traverser_state, *BIF_All)
|----------------------------------------------------------------------
| Description   :	Edit basestate value for this BIF attribute
|		 Patches the contents of the entity markercolor to the
|		  phigs base_state structure.  For further details 
|		  see the Descriptions block at the top of this file.
|----------------------------------------------------------------------
| Return        :	
\*--------------------------------------------------------------------*/
int do_markercolor( traverser_state, entity )
BIF_Traverser_state *traverser_state;
BIF_All *entity;
{
#ifdef USING_PHIGS
	do_generic_c3(traverser_state,entity,BBS_PSPMCO, pset_marker_colr);
#else /* ifdef USING_PHIGS */
#endif /* ifdef USING_PHIGS */
	return(0);
} /* End do_markertype */

/*--------------------------------------------------------------------*\
| Procedure     :	do_linecolor(* BIF_Traverser_state, *BIF_All)
|----------------------------------------------------------------------
| Description   :	Edit basestate value for this BIF attribute
|		 Patches the contents of the entity linecolor to the
|		  phigs base_state structure.  For further details 
|		  see the Descriptions block at the top of this file.
|----------------------------------------------------------------------
| Return        :	
\*--------------------------------------------------------------------*/
int do_linecolor( traverser_state, entity )
BIF_Traverser_state *traverser_state;
BIF_All *entity;
{
#ifdef USING_PHIGS
	do_generic_c3(traverser_state,entity,BBS_PSPLCO, pset_line_colr);
#else /* ifdef USING_PHIGS */
#endif /* ifdef USING_PHIGS */
	return(0);
} /* End do_markertype */

/*--------------------------------------------------------------------*\
| Procedure     :	do_textcolor(* BIF_Traverser_state, *BIF_All)
|----------------------------------------------------------------------
| Description   :	Edit basestate value for this BIF attribute
|		 Patches the contents of the entity textcolor to the
|		  phigs base_state structure.  For further details 
|		  see the Descriptions block at the top of this file.
|----------------------------------------------------------------------
| Return        :	
\*--------------------------------------------------------------------*/
int do_textcolor( traverser_state, entity )
BIF_Traverser_state *traverser_state;
BIF_All *entity;
{
#ifdef USING_PHIGS
	do_generic_c3(traverser_state,entity,BBS_PSTXCO, pset_text_colr);
#else /* ifdef USING_PHIGS */
#endif /* ifdef USING_PHIGS */
	return(0);
} /* End do_markertype */

/*--------------------------------------------------------------------*\
| Procedure     :	do_backfaceinteriorcolor(* BIF_Traverser_state,
|						*BIF_All)
|----------------------------------------------------------------------
| Description   :	Edit basestate value for this BIF attribute
|		 Patches the contents of the entity 
|		  backfaceinteriorcolor to the
|		  phigs base_state structure.  For further details 
|		  see the Descriptions block at the top of this file.
|----------------------------------------------------------------------
| Return        :	
\*--------------------------------------------------------------------*/
int do_backfaceinteriorcolor( traverser_state, entity )
BIF_Traverser_state *traverser_state;
BIF_All *entity;
{
#ifdef USING_PHIGS
	do_generic_c3(traverser_state,entity,BBS_FPSBFIC, pset_back_int_colr);
#else /* ifdef USING_PHIGS */
#endif /* ifdef USING_PHIGS */
	return(0);
} /* End do_markertype */

/*--------------------------------------------------------------------*\
| Procedure     :	do_edgecolor(* BIF_Traverser_state, *BIF_All)
|----------------------------------------------------------------------
| Description   :	Edit basestate value for this BIF attribute
|		 Patches the contents of the entity edgecolor to the
|		  phigs base_state structure.  For further details 
|		  see the Descriptions block at the top of this file.
|----------------------------------------------------------------------
| Return        :	
\*--------------------------------------------------------------------*/
int do_edgecolor( traverser_state, entity )
BIF_Traverser_state *traverser_state;
BIF_All *entity;
{
#ifdef USING_PHIGS
	do_generic_c3(traverser_state,entity,BBS_PSECO, pset_edge_colr);
#else /* ifdef USING_PHIGS */
#endif /* ifdef USING_PHIGS */
	return(0);
} /* End do_markertype */

/*--------------------------------------------------------------------*\
| Procedure     :	do_annotextstyle(* BIF_Traverser_state,
|						*BIF_All)
|----------------------------------------------------------------------
| Description   :	Edit basestate value for this BIF attribute
|		 Patches the contents of the entity annotextstyle to the
|		  phigs base_state structure.  For further details 
|		  see the Descriptions block at the top of this file.
|----------------------------------------------------------------------
| Return        :	
\*--------------------------------------------------------------------*/
int do_annotextstyle( traverser_state, entity )
BIF_Traverser_state *traverser_state;
BIF_All *entity;
{
#ifdef USING_PHIGS
	do_generic_i(traverser_state, entity->ind.ind,
		     BBS_PSANS, pset_anno_style, 
		     DEFAULT_ANNOTEXTSTYLE);
#else /* ifdef USING_PHIGS */
#endif /* ifdef USING_PHIGS */
	return(0);
} /* End do_annoteststyle */

/*--------------------------------------------------------------------*\
| Procedure     :	do_backfaceinteriorcolorindex(
|					* BIF_Traverser_state, *BIF_All)
|----------------------------------------------------------------------
| Description   :	Edit basestate value for this BIF attribute
|		 Patches the contents of the entity backfaceinteriorcolorindex to the
|		  phigs base_state structure.  For further details 
|		  see the Descriptions block at the top of this file.
|----------------------------------------------------------------------
| Return        :	
\*--------------------------------------------------------------------*/
int do_backfaceinteriorcolorindex( traverser_state, entity )
BIF_Traverser_state *traverser_state;
BIF_All *entity;
{
#ifdef USING_PHIGS
	do_generic_colorindex(traverser_state,entity->ind.ind,
			      BBS_FPSBFCI, pset_back_int_colr,
			      DEFAULT_BACKFACEINTERIORCOLORINDEX);
#else /* ifdef USING_PHIGS */
#endif /* ifdef USING_PHIGS */
	return(0);
} /* End do_backfaceinteriorcolorindex */

/*--------------------------------------------------------------------*\
| Procedure     :	do_annotextcharheight(* BIF_Traverser_state,
|						*BIF_All)
|----------------------------------------------------------------------
| Description   :	Edit basestate value for this BIF attribute
|		 Patches the contents of the entity annotextcharheight to the
|		  phigs base_state structure.  For further details 
|		  see the Descriptions block at the top of this file.
|----------------------------------------------------------------------
| Return        :	
\*--------------------------------------------------------------------*/
int do_annotextcharheight( traverser_state, entity )
BIF_Traverser_state *traverser_state;
BIF_All *entity;
{
#ifdef USING_PHIGS
	do_generic_f(traverser_state,entity->size.size,
		     BBS_PSATCH,pset_anno_char_ht,
		     DEFAULT_ANNOTEXTCHARHEIGHT);
#else /* ifdef USING_PHIGS */
#endif /* ifdef USING_PHIGS */
	return(0);
} /* End do_annotextcharheight */

/*--------------------------------------------------------------------*\
| Procedure     :	do_charexp(* BIF_Traverser_state, *BIF_All)
|----------------------------------------------------------------------
| Description   :	Edit basestate value for this BIF attribute
|		 Patches the contents of the entity charexp to the
|		  phigs base_state structure.  For further details 
|		  see the Descriptions block at the top of this file.
|----------------------------------------------------------------------
| Return        :	
\*--------------------------------------------------------------------*/
int do_charexp( traverser_state, entity )
BIF_Traverser_state *traverser_state;
BIF_All *entity;
{
#ifdef USING_PHIGS
	do_generic_f(traverser_state,entity->size.size,
			BBS_PSCHXP,pset_char_expan, DEFAULT_CHAREXP);
#else /* ifdef USING_PHIGS */
#endif /* ifdef USING_PHIGS */
	return(0);
} /* End do_charexp */

/*--------------------------------------------------------------------*\
| Procedure     :	do_charspace(* BIF_Traverser_state, *BIF_All)
|----------------------------------------------------------------------
| Description   :	Edit basestate value for this BIF attribute
|		 Patches the contents of the entity charspace to the
|		  phigs base_state structure.  For further details 
|		  see the Descriptions block at the top of this file.
|----------------------------------------------------------------------
| Return        :	
\*--------------------------------------------------------------------*/
int do_charspace( traverser_state, entity )
BIF_Traverser_state *traverser_state;
BIF_All *entity;
{
#ifdef USING_PHIGS
	do_generic_f(traverser_state,entity->size.size,
			BBS_PSCHSP,pset_char_space, DEFAULT_CHARSPACE);
#else /* ifdef USING_PHIGS */
#endif /* ifdef USING_PHIGS */
	return(0);
} /* End do_charspace */

/*--------------------------------------------------------------------*\
| Procedure     :	do_depthcueindex(* BIF_Traverser_state,
|						*BIF_All)
|----------------------------------------------------------------------
| Description   :	Edit basestate value for this BIF attribute
|		 Patches the contents of the entity depthcueindex to the
|		  phigs base_state structure.  For further details 
|		  see the Descriptions block at the top of this file.
|----------------------------------------------------------------------
| Return        :	
\*--------------------------------------------------------------------*/
int do_depthcueindex( traverser_state, entity )
BIF_Traverser_state *traverser_state;
BIF_All *entity;
{
#ifdef USING_PHIGS
	do_generic_i(traverser_state,entity->ind.ind,
			BBS_PSDCI,pset_dcue_ind, DEFAULT_DEPTHCUEINDEX);
#else /* ifdef USING_PHIGS */
#endif /* ifdef USING_PHIGS */
	return(0);
} /* End do_depthcueindex */

/*--------------------------------------------------------------------*\
| Procedure     :	do_edgecolorindex(* BIF_Traverser_state,
|						*BIF_All)
|----------------------------------------------------------------------
| Description   :	Edit basestate value for this BIF attribute
|		 Patches the contents of the entity edgecolorindex to the
|		  phigs base_state structure.  For further details 
|		  see the Descriptions block at the top of this file.
|----------------------------------------------------------------------
| Return        :	
\*--------------------------------------------------------------------*/
int do_edgecolorindex( traverser_state, entity )
BIF_Traverser_state *traverser_state;
BIF_All *entity;
{
#ifdef USING_PHIGS
	do_generic_colorindex(traverser_state,entity->ind.ind,
			      BBS_PSEDCI, pset_edge_colr,
			      DEFAULT_EDGECOLORINDEX);
#else /* ifdef USING_PHIGS */
#endif /* ifdef USING_PHIGS */
	return(0);
} /* End do_edgecolorindex */

/*--------------------------------------------------------------------*\
| Procedure     :	do_edgetype(* BIF_Traverser_state, *BIF_All)
|----------------------------------------------------------------------
| Description   :	Edit basestate value for this BIF attribute
|		 Patches the contents of the entity edgetype to the
|		  phigs base_state structure.  For further details 
|		  see the Descriptions block at the top of this file.
|----------------------------------------------------------------------
| Return        :	
\*--------------------------------------------------------------------*/
int do_edgetype( traverser_state, entity )
BIF_Traverser_state *traverser_state;
BIF_All *entity;
{
#ifdef USING_PHIGS
	do_generic_i(traverser_state,entity->ind.ind,
			BBS_PSEDT,pset_edgetype, DEFAULT_EDGETYPE);
#else /* ifdef USING_PHIGS */
#endif /* ifdef USING_PHIGS */
	return(0);
} /* End do_edgetype */

/*--------------------------------------------------------------------*\
| Procedure     :	do_interiorpatternindex(* BIF_Traverser_state,
|						*BIF_All)
|----------------------------------------------------------------------
| Description   :	Edit basestate value for this BIF attribute
|		 Patches the contents of the entity interiorpatternindex to the
|		  phigs base_state structure.  For further details 
|		  see the Descriptions block at the top of this file.
|----------------------------------------------------------------------
| Return        :	
\*--------------------------------------------------------------------*/
int do_interiorpatternindex( traverser_state, entity )
BIF_Traverser_state *traverser_state;
BIF_All *entity;
{
#ifdef USING_PHIGS
	/* Do back interior pattern index also */
	do_2_generic_i(traverser_state,entity->ind.ind,
		       BBS_FPSIPI,pset_int_style_ind,
		       pset_back_int_style_ind,
		       DEFAULT_INTERIORPATTERNINDEX);
#else /* ifdef USING_PHIGS */
#endif /* ifdef USING_PHIGS */
	return(0);
} /* End do_interiorpatternindex */

/*--------------------------------------------------------------------*\
| Procedure     :	do_lineshading(* BIF_Traverser_state, *BIF_All)
|----------------------------------------------------------------------
| Description   :	Edit basestate value for this BIF attribute
|		 Patches the contents of the entity lineshading to the
|		  phigs base_state structure.  For further details 
|		  see the Descriptions block at the top of this file.
|----------------------------------------------------------------------
| Return        :	
\*--------------------------------------------------------------------*/
int do_lineshading( traverser_state, entity )
BIF_Traverser_state *traverser_state;
BIF_All *entity;
{
#ifdef USING_PHIGS
	do_generic_i(traverser_state,entity->ind.ind,
		     BBS_PSPLSM,pset_line_shad_meth,
		     DEFAULT_LINESHADING);
#else /* ifdef USING_PHIGS */
#endif /* ifdef USING_PHIGS */
	return(0);
} /* End do_lineshading */

/*--------------------------------------------------------------------*\
| Procedure     :	do_annotextcharupvector(* BIF_Traverser_state,
|						*BIF_All)
|----------------------------------------------------------------------
| Description   :	Edit basestate value for this BIF attribute
|		 Patches the contents of the entity annotextcharupvector to the
|		  phigs base_state structure.  For further details 
|		  see the Descriptions block at the top of this file.
|----------------------------------------------------------------------
| Return        :	
\*--------------------------------------------------------------------*/
int do_annotextcharupvector( traverser_state, entity )
BIF_Traverser_state *traverser_state;
BIF_All *entity;
{
  float x, y;
  int next_label;
#ifdef USING_PHIGS
  if (traverser_state->push_level == 0) {
    next_label = BBS_PSATCHUP + 1;
    if (traverser_state->nrs_state)  fxclns();
    x = entity->charupvector.up_vect.delta_x;
    y = entity->charupvector.up_vect.delta_y;
    popen_struct((Pint)base_state_stid);
    pset_elem_ptr((Pint)0);
    pdel_elems_labels((Pint)BBS_PSATCHUP,(Pint)next_label); 
    if ( x	!= DEFAULT_ANNOTCHUPV_X || 
	y	!= DEFAULT_ANNOTCHUPV_Y ) 
      pset_anno_char_up_vec((Pvec *)&entity->charupvector.up_vect);
    pclose_struct();
    if (traverser_state->nrs_state)  fxopns();
  }
#else /* ifdef USING_PHIGS */
#endif /* ifdef USING_PHIGS */
  return(0);
} /* End do_annotextcharupvector */


/*--------------------------------------------------------------------*\
| Procedure     :	do_backfaceprocessing(* BIF_Traverser_state,
|						*BIF_All)
|----------------------------------------------------------------------
| Description   :	Edit basestate value for this BIF attribute
|		 Patches the contents of the entity backfaceprocessing to the
|		  phigs base_state structure.  For further details 
|		  see the Descriptions block at the top of this file.
|----------------------------------------------------------------------
| Return        :	
\*--------------------------------------------------------------------*/
int do_backfaceprocessing ( traverser_state, entity )
BIF_Traverser_state *traverser_state;
BIF_All *entity;
{
  int next_label;
#ifdef USING_PHIGS
  if (traverser_state->push_level == 0) {
    next_label = BBS_FPBFPR + 1;
    if (traverser_state->nrs_state)  fxclns();
    popen_struct((Pint)base_state_stid);
    pset_elem_ptr((Pint)0);
    pdel_elems_labels((Pint)BBS_FPBFPR,(Pint)next_label); 
    if ( (int)(entity->backfaceprocessing.g_mode)
	!= DEFAULT_BACKFACEP_IDENTIFYF || 
	(int)(entity->backfaceprocessing.cull_mode)
	!= DEFAULT_BACKFACEP_CULLF ) {
	pset_face_disting_mode(entity->backfaceprocessing.g_mode);
	pset_face_cull_mode(entity->backfaceprocessing.cull_mode);
    }
    pclose_struct();
    if (traverser_state->nrs_state)  fxopns();
  }
#else /* ifdef USING_PHIGS */
#endif /* ifdef USING_PHIGS */
	return(0);
} /* End do_backfaceprocessing */

/*--------------------------------------------------------------------*\
| Procedure     :	do_backfaceproperties(* BIF_Traverser_state,
|						*BIF_All)
|----------------------------------------------------------------------
| Description   :	Edit basestate value for this BIF attribute
|		 Patches the contents of the entity backfaceproperties to the
|		  phigs base_state structure.  For further details 
|		  see the Descriptions block at the top of this file.
|----------------------------------------------------------------------
| Return        :	
\*--------------------------------------------------------------------*/
int do_backfaceproperties( traverser_state, entity )
BIF_Traverser_state *traverser_state;
BIF_All *entity;
{
  float ambient, diffuse, specular;
  int color_model; 
  float specular_color[3]; /* Specular color components */
  float highlight, transparency;
  int next_label;

#ifdef USING_PHIGS
  if (traverser_state->push_level == 0) {
    ambient 	= entity->surfaceproperties.props.ambient_coef;
    diffuse 	= entity->surfaceproperties.props.diffuse_coef;
    specular 	= entity->surfaceproperties.props.specular_coef;
    color_model =
      entity->surfaceproperties.props.specular_colr.type;
    switch (color_model) {
    case PINDIRECT:
      specular_color[0] = 
	(float)entity->surfaceproperties.props.specular_colr.val.ind;
      break;
    default:
      specular_color[0] = 
	(float)entity->surfaceproperties.props.specular_colr.val.general.x;
      specular_color[1] = 
	(float)entity->surfaceproperties.props.specular_colr.val.general.y;
      specular_color[2] = 
	(float)entity->surfaceproperties.props.specular_colr.val.general.z;
      break;
    }
    highlight 	 = (float)entity->surfaceproperties.props.specular_exp;
    transparency = 0.0;

    next_label = BBS_FPBFP + 1;
    if (traverser_state->nrs_state)  fxclns();
    popen_struct((Pint)base_state_stid);
    pset_elem_ptr((Pint)0);
    pdel_elems_labels((Pint)BBS_FPBFP,(Pint)next_label); 
    if ( 	ambient 	!=   DEFAULT_SP_AMBIENT || 
	diffuse		!=   DEFAULT_SP_DEFFUSE ||
	specular	!=   DEFAULT_SP_SPECULAR ||
	color_model	!=   DEFAULT_SP_COLOR_MODEL ||
	specular_color[0]!=  DEFAULT_SP_SPEC_COLOR1 ||
	specular_color[1]!=  DEFAULT_SP_SPEC_COLOR2 ||
	specular_color[2]!=  DEFAULT_SP_SPEC_COLOR3 ||
	highlight	!=   DEFAULT_SP_HIGHLIGHT ||
	transparency	!=   DEFAULT_SP_TRANSPARENCY)
      pset_back_refl_props(&entity->surfaceproperties.props);
    pclose_struct();
    if (traverser_state->nrs_state)  fxopns();
  }
#else /* ifdef USING_PHIGS */
#endif /* ifdef USING_PHIGS */
  return(0);
} /* End do_backfaceproperties */


/*----------------------------------------------------------------------*\
| Procedure     :	do_lightstate(* BIF_Traverser_state, *BIF_All)
|------------------------------------------------------------------------
| Description   :	Edit basestate value for this BIF attribute
|		 Patches the contents of the entity lightstate to the
|		 phigs base_state structure.  For further details 
|		 see the Descriptions block at the top of this file.
|------------------------------------------------------------------------
| Return        :	
\*----------------------------------------------------------------------*/
int do_lightstate( traverser_state, entity )
BIF_Traverser_state *traverser_state;
BIF_All *entity;
{
    int next_label;

#ifdef USING_PHIGS
    if (traverser_state->push_level == 0) {
	next_label = BBS_PSLSS + 1;
	if (traverser_state->nrs_state)  fxclns();
    
#ifdef EXTERNALNOTE

	The following block of code has been changed to reflect an error
	in the original code for the GPC benchmark.

	The code to update the base state active lights list is redundant 
	and is no longer needed. The original call to pslss passed the 
	incorrect light pointers, which is why lighting declared outside
	the test loop would not work. The corrected pslss needs to be 
	communicated to all users of the current source code.
#endif


	popen_struct((Pint)base_state_stid);

	pset_elem_ptr((Pint)0);
	pdel_elems_labels((Pint)BBS_PSLSS,(Pint)next_label); 

	pset_light_src_state((Pint_list *)&entity->lightstate.activation,
			  (Pint_list *)&entity->lightstate.deactivation);

	pclose_struct();
    
	if (traverser_state->nrs_state)  fxopns();
    }
#else /* USING_PHIGS */
#endif /* USING_PHIGS */
  return(0);
} /* End do_lightstate */

/*----------------------------------------------------------------------*\
| Procedure     :	do_activeview(* BIF_Traverser_state, *BIF_All)
|------------------------------------------------------------------------
| Description   :	Edit basestate value for this BIF attribute
|------------------------------------------------------------------------
| Return        :	Error code
\*----------------------------------------------------------------------*/
int do_activeview( traverser_state, entity )
BIF_Traverser_state *traverser_state;
BIF_All *entity;
{
	/* Set this because GET_MATRIX3 needs it */
	traverser_state->id_active_view = entity->ind.ind;

#ifdef USING_PHIGS
	do_generic_i(traverser_state,entity->ind.ind,
			BBS_PSVWI,pset_view_ind, DEFAULT_VIEWINDEX);
#else /* ifdef USING_PHIGS */
#endif /* ifdef USING_PHIGS */
	/* Return Error Code */
	return(0);  /* No Error */
} /* End do_activeview */


/*----------------------------------------------------------------------*\
| Procedure     :	void init_base_state(int, int) 
|--------------------------------------------------------------------------|
| Description   :	Set up the base_state structure. 
|	This structure contains all the 
|	attributes which are allowed outside of bif structures but not 
|	allowed outside of phigs structures
|--------------------------------------------------------------------------|
| Return:                                                                  |
 \*----------------------------------------------------------------------*/
/*ARGSUSED*/
void init_base_state(base_state_stid,base_state_vid)
int base_state_stid, base_state_vid;
{

#ifdef USING_PHIGS
        Pgcolr gcolr;

	/* Always use general color which supports indirect and RGB */
	gcolr.type = PINDIRECT;
	gcolr.val.ind = 2;

	/*------------------------------------------------------------*\
	|	Make the sure the base is empty
	\*------------------------------------------------------------*/
	pempty_struct((Pint)base_state_stid);


	popen_struct((Pint)base_state_stid);
		plabel((Pint) BBS_PSVWI);  /* Use Default active view	*/
		fxnoop(1);		   /*				*/
		plabel((Pint)BBS_PSCHH);   /* char_height		*/
		fxnoop(1);		   /* pschh   char_height	*/
		plabel((Pint)BBS_PSCHUP);  /* char_up_vector 		*/
		fxnoop(2);		   /* pschup  char_up_vector 	*/
		plabel((Pint)BBS_PSEDCI);  /* edgecolorindex		*/
		pset_edge_colr(&gcolr);	   /*				*/
		plabel((Pint)BBS_PSEDFG);  /* edge_flag			*/
		fxnoop(1);		   /* psedfg  edge_flag		*/
		plabel((Pint)BBS_PSEWSC);  /* edge_width		*/
		fxnoop(1);		   /* psewsc  edge_width	*/
		plabel((Pint)BBS_PSGLT3);  /* global_transformation3	*/
		fxnoop(16);		   /* psgmt3  global_transformation3*/
		plabel((Pint)BBS_PXSZBM);  /* hlhs_removal		*/
		fxnoop(2);		   /* pshir  hlhs_removal	*/
		plabel((Pint)BBS_PSICI);   /* interior_color_index	*/
		pset_int_colr(&gcolr);     /* psici   interior_color_index*/
	        plabel((Pint)BBS_PSIRE);   /* interior_lighting	        */
		fxnoop(2);		   /* psilm   interior_lighting	*/
		plabel((Pint)BBS_PSISM);   /* interior_shading		*/
		fxnoop(2);		   /* psism   interior_shading	*/
		plabel((Pint)BBS_FPSBFCI); /* backfaceinteriorcolorindex*/
		pset_back_int_colr(&gcolr); /*				*/
		plabel((Pint)BBS_PSPLCI);  /* line_color_index		*/
		pset_line_colr(&gcolr);	   /* psplci  line_color_index	*/
		plabel((Pint)BBS_PSLN);	   /* line_type			*/
		fxnoop(1);		   /* psln    line_type		*/
		plabel((Pint)BBS_PSLWSC);  /* line_width		*/
		fxnoop(1);		   /* pslwsc  line_width	*/
		plabel((Pint)BBS_PSLOT3);  /* local_transformation3     */
		fxnoop(17);		   /* pslmt3  local_transformation3*/
		plabel((Pint)BBS_PSPMCI);  /* marker_color_index	*/
		pset_marker_colr(&gcolr);  /* pspmci  marker_color_index*/
		plabel((Pint)BBS_PSMKSC);  /* marker_size		*/
		fxnoop(1);		   /* psmksc  marker_size	*/
		plabel((Pint)BBS_PSMK);	   /* marker_type		*/
		fxnoop(1);		   /* psmk    marker_type	*/
		plabel((Pint)BBS_PSAP);	   /* surface_properties	*/
		fxnoop(7);		   /* pssp    surface_properties*/
		plabel((Pint)BBS_PSTXAL);  /* text_align		*/
		fxnoop(2);		   /* fxsattxal  text_align	*/
		plabel((Pint)BBS_PSTXCI);  /* text_color_index		*/
		pset_text_colr(&gcolr);	   /* pstxci  text_color_index	*/
		plabel((Pint)BBS_PSTXFN);  /* text_font			*/
		fxnoop(1);		   /* pstxfn  text_font		*/
		plabel((Pint)BBS_PSTXP);   /* text_path			*/
		fxnoop(1);		   /* fxsattxp   text_path	*/
		plabel((Pint)BBS_PSTXPR);  /* text_prec			*/
		fxnoop(1);		   /* pstxpr  text_prec		*/
		plabel((Pint)BBS_PSIS);	   /* interior_style		*/
		fxnoop(2);		   /* psis    interior_style	*/
		plabel((Pint)BBS_PSICO);   /* interior_color		*/
		fxnoop(3);		   /* 				*/
		plabel((Pint)BBS_PSPLCO);  /* marker_color		*/
		fxnoop(3);		   /* 				*/
		plabel((Pint)BBS_PSPMCO);  /* line_color		*/
		fxnoop(3);		   /* 				*/
		plabel((Pint)BBS_PSTXCO);  /* text_color		*/
		fxnoop(3);		   /* 				*/
		plabel((Pint)BBS_FPSBFIC); /* do_backfaceinteriorcolor  */
		fxnoop(1);		   /* 	    			*/
		plabel((Pint)BBS_PSECO);   /* edge_color		*/
		fxnoop(3);		   /* 				*/
		plabel((Pint)BBS_PSANS);   /* annotextstyle		*/
		fxnoop(1);		   /*				*/
		plabel((Pint)BBS_PSATCH);  /* annotextcharheight	*/
		fxnoop(1);		   /*				*/
		plabel((Pint)BBS_PSCHXP);  /* charexp			*/
		fxnoop(1);		   /*				*/
		plabel((Pint)BBS_PSCHSP);  /* charspace			*/
		fxnoop(1);		   /*				*/
		plabel((Pint)BBS_PSDCI);   /* depthcueindex		*/
		fxnoop(1);		   /*				*/
		plabel((Pint)BBS_PSEDT);   /* edgetype			*/
		fxnoop(1);		   /*				*/
		plabel((Pint)BBS_FPSIPI);  /* interiorpatternindex	*/
		fxnoop(2);		   /*				*/
		plabel((Pint)BBS_PSPLSM);  /* lineshading		*/
		fxnoop(1);		   /*				*/
		plabel((Pint)BBS_PSATCHUP);/*				*/
		fxnoop(2);		   /*				*/
		plabel((Pint)BBS_FPBFPR);  /*				*/
		fxnoop(1);		   /*				*/
		plabel((Pint)BBS_FPBFP);   /*				*/
		fxnoop(7);		   /* pssp    surface_properties*/
		plabel((Pint)BBS_PSLSS);   /* light_state		*/ 
		fxnoop(20);		   /* pslss   light_state	*/

		plabel((Pint)BBS_END );	   /* end of base state		*/
	pclose_struct();
	setNextLabel(BBS_END + 1);
#endif /* USING_PHIGS */
}
