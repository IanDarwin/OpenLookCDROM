/* $XConsortium: bifbuild.h,v 5.1 91/02/16 10:06:55 rws Exp $ */

/*
 */

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
| Author        :	jmz / SimGraphics Engineering Corportation
|
| File          :	bifbuild.h
| Date          :	3/15/89
| Project       :	PLB
|
| Description	:	Contain enumeration values for various BIF
|			and PHIGS items found in bifbuild.c ( and other
|			files as well )
|
| Status	:	Version 1.0
|			Many of the #defines are unused.  Also some
|			of the BIF_<> values have been "set" to work
|			with GX4000 PHIGS.
|
| Revisions     :	
|
\*--------------------------------------------------------------------*/

/*--------------------------------------------------------------------*\
|	Include files
\*--------------------------------------------------------------------*/

/*--------------------------------------------------------------------*\
|	Local #define 
\*--------------------------------------------------------------------*/


/* GX4000 TABLE SIZE Limits */
#define BIF_MAX_LIGHTS		8
#define MAX_BIF_LIGHTS		BIF_MAX_LIGHTS
#define BIF_MAX_DCRS		20
#define BIF_P_BEGIN		0
#define BIF_P_END		1
#define BIF_END_OF_GROUP	-9999

#define BIF_ENABLE		0
#define BIF_DISABLE		1

/* INVOKE AT FRAME STYLE */
#define BIF_CALL		0
#define BIF_EXECUTE		1

/* GET MATRIX PARAMETERS */
#define BIF_VIEW_MAPPING	1
#define BIF_VIEW_ORIENTATION	2
#define BIF_GLOBAL_MODELLING	3
#define BIF_LOCAL_MODELLING	4
#define BIF_COMPOSITE_MODELLING	5

/* Remap to the correct ENUM value */
#define BIF_TRUE_COLOR TRUE_COLOR


/* PHIGS ENUMERATED VALUES */

/* Colour Source Flag*/
#define FIG_PDIRECT	Fi0
#define FIG_PINDEXD	Fi1

/* Colour Source Id */
#define FIG_PCOPL	Fi0
#define FIG_PCOINT	Fi1
#define FIG_PCOEDG	Fi2
#define FIG_PCOTXT	Fi3
#define FIG_PCOPM	Fi4
#define FIG_PCOBK	Fi5

/* Colour Inquiry Type */
#define FIG_PINQ_SET	Fi0
#define FIG_PREALI	Fi1

/* TEXT ALIGNMENT */
#define FIG_PCHARP	Fi1
#define FIG_PACENT	Fi2
#define FIG_PAHALF	Fi3

/* Update State */
#define	FIG_PDEFER_WAITD	Fi4
#define FIG_PMODE_NIVE	Fi0

/* Update Switch */
#define FIG_PPERFO	Fi1

/* GDP3 option */
#define FIG_WRITEIMAGE	Fim1
#define FIG_PIXFUNC	Fim2


/* Interior style */
#define BIF_HOLLOW		0
#define BIF_SOLID		1
#define BIF_PATTERN		2
#define BIF_EMPTY		4

/* Edge Flag */
#define BIF_OFF			0
#define BIF_ON			1

/* Text Precision */
#define BIF_STRING		0
#define BIF_CHAR		1
#define BIF_STROKE		2

/* Light Types */
#define BIF_AMBIENT		1
#define BIF_DIRECTIONAL		2
#define BIF_POSITIONAL		3
#define BIF_SPOT		4

/* HLHSR modes */
#define BIF_HLHS_DISABLE	0
#define BIF_HLHS_ENABLE		1

/* Color Models */
#define BIF_RGB			1
#define BIF_CIE			2
#define BIF_HSV			3
#define BIF_HLS			4

/* Matrix Manipulation */
#define BIF_PRECONCAT		0
#define BIF_POSTCONCAT		1
#define BIF_REPLACE		2

/* Projection Type */
#define BIF_PARALLEL		0
#define BIF_PERSPECTIVE		1

/* View Spec. Info */
#define BIF_NO_AREA_MATCH	0
#define BIF_NO_CLIP		0
#define BIF_CLIP		1

/* Pixel Update Functions */
#define BIF_PF_ADD		3
#define BIF_PF_AND		5
#define BIF_PF_CLEAR		4
#define BIF_PF_INVERT		14
#define BIF_PF_NAND		17
#define BIF_PF_NOOP		9
#define BIF_PF_NOR		12
#define BIF_PF_OR		11
#define BIF_PF_REPLACE		0
#define BIF_PF_SET		18
#define BIF_PF_SUBTRACT_DEST	1
#define BIF_PF_SUBTRACT_SOURCE	2
#define BIF_PF_XOR		10

/* bifbuild.c functions */
int bif_activeview();
int bif_applytoglobal3();
int bif_applytolocal3();
int bif_backgroundcolorindex();
int bif_begintest();
int bif_begstr();
int bif_callstr();
int bif_charheight();
int bif_charupvector();
int bif_cleargeom();
int bif_colormodel();
int bif_concatmatrix3();
int bif_contour();
int bif_definecolor();
int bif_definelight();
int bif_defviewspec();
int bif_edgeflag();
int bif_edgewidth();
int bif_endstr();
int bif_endtest();
int bif_execstr();
int bif_faset3();
int bif_fasetdata3();
int bif_group();
int bif_gtransform3();
int bif_hlhsremoval();
int bif_identity3();
int bif_intcolorindex();
int bif_intlight();
int bif_intlist();
int bif_intshade();
int bif_invertmatrix3();
int bif_lightbasic();
int bif_lightoption();
int bif_lightstate();
int bif_line3();
int bif_linecolorindex();
int bif_linetype();
int bif_linewidth();
int bif_ltransform3();
int bif_marker3();
int bif_matrix3();
int bif_mkcolorindex();
int bif_mkscale();
int bif_mktype();
int bif_pause();
int bif_polydata3();
int bif_polygon3();
int bif_readgeom();
int bif_rotate3();
int bif_rotatexyz3();
int bif_scale3();
int bif_sleep();
int bif_surfprop();
int bif_text3();
int bif_textalign();
int bif_textcolorindex();
int bif_textfont();
int bif_textpath();
int bif_textprec();
int bif_translate3();
int bif_triplet();
int bif_viewmap3();
int bif_viewmapbasic();
int bif_viewmapmatch();
int bif_vieworient3();
int bif_viewspec();
