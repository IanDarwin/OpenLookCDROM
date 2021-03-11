/* $XConsortium: biftypes.h,v 5.4 94/04/17 20:44:23 rws Exp $ */
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
Copyright 1989, 1990, 1991 by Sun Microsystems, Inc.

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
|
|  Copyright (C) 1989, 1990, 1991, National Computer Graphics Association
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
| File          :	biftypes.h
| Date          :	3/16/89
| Project       :	PLB
|
| Description	:	Contains the type information for PLB in general
|			and in in specific the BIF entities.  
|			Data structure are typically a mirror of the
|			BIF file specification, but may be augmented
|			or rearranged to meet the the specific need of the 
|			needed Graphics Calls 
|
| Status	:	Version 1.0
|			May contain some obsolete data types.
|
| Revisions     :
|
|       2/90            MFC Tektronix, Inc.: PEX-SI API implementation.
|
|       5/90            MFC Tektronix, Inc.: PEX-SI API Binding change.
|
|      12/90            MFC Tektronix, Inc.: PEX-SI PEX5R1 Release.
|
\*--------------------------------------------------------------------*/

#ifndef MAIN_FUNCTION
typedef char *StringType; 
#endif /* MAIN_FUNCTION */

#ifndef FALSE
#define FALSE ( 1 == 0 )
#endif

#ifndef TRUE
#define TRUE ( 1 == 1 )
#endif

#include "bifbuild.h"

#include <phigs/phigs.h>

/* PARSER OUTPUT TYPES */
#define BIF_REAL double
#define BIF_INT long

/* Optional data Groups */
#define VCOLORT		0
#define VCOLORI		1
#define VNORM		2
#define FCOLORT		3
#define FCOLORI		4
#define FNORM		5
#define EDATA		6

/* Parser Data List Buffer Type */
typedef union
{
	float Float;
	int   Int;
} Real_int_union;

/* Call structure Link List Element */
typedef struct slList
{
	struct slList *next;
	int	data;
} slList;

typedef float vector3[3];
typedef float vector4[4];

typedef float Matrix4[4][4];
typedef float matrix4[4][4];
typedef struct
{
	long name;  /* Group name */
	int type;   /* type of data in this list
			GT_LIST_F3 = 0 = float[3]
			GT_LIST_I  = 1 = int
			GT_CONT_F3 = 2 = contour float[3]
			GT_CONT_I  = 3 = contour int
		    */
	int number; /* Size of this list */
		    /* type of data in this list
			GT_LIST_F3 number of triplets
			GT_LIST_I  number of ints
			GT_CONT_F3 number of contours
			GT_CONT_I  number of contours
		    */
	Real_int_union	*list;
} Group_description;

/* All structures contain the following */
#define BIF_ENTITY_HEADER \
	int            entity_type;\
	union BIF_All  *next;\
	int            (*handler)();\
	int		exception;

/* All withdata structures contain the following */
#define STD_DATA_BODY	                    \
        int             numCoords;          \
        int             numFacets;          \
        int             numEdges;           \
        int	        vertex_data_flag;   \
        int	        facet_data_flag;    \
	int	        edge_data_flag;	    \
        int             colour_model;       \
	int	        with_data_flag[7];  \
        Pfacet_vdata_arr3 vertex_data;        \
        Pfacet_data_arr3  facet_data;         \
        Pedge_data_arr    edge_data;

/* ****************** */
/* General Structures */
/* ****************** */
typedef struct
{
	BIF_ENTITY_HEADER
} BIF_No_data;

typedef struct
{
	BIF_ENTITY_HEADER
	int number;
	Ppoint *points;
} BIF_Simple_double;

typedef struct
{
	BIF_ENTITY_HEADER
	int number;
	Ppoint3 *points;
} BIF_Simple_triple;

typedef struct
{
	BIF_ENTITY_HEADER
	STD_DATA_BODY
} BIF_Withdata3;

typedef struct
{
	BIF_ENTITY_HEADER
	int color_model;
	vector3 color;
} BIF_True_color;

typedef struct
{
        BIF_ENTITY_HEADER
        int ind;
} BIF_Index;

typedef struct
{
	BIF_ENTITY_HEADER
	float size;
} BIF_Size;

typedef struct
{
	BIF_ENTITY_HEADER
	float scale;
} BIF_Scale_factor;

typedef struct
{
        BIF_ENTITY_HEADER
        int matrix_id;
        float x, y, z;
        int concat_type;
} BIF_Matrix_op_xyz;


/* BIF Geometry file type definitions */
/* 4.1 Structures */
typedef struct
{
	BIF_ENTITY_HEADER
	int structure_id;
	union BIF_All *top_of_list;
	int next_label;
	int expanded;
} BIF_Beginstructure;

typedef BIF_No_data BIF_Endstructure;

/* 4.2 Graphics primitives */
/* Markers */
typedef BIF_Simple_double BIF_Marker;
typedef BIF_Simple_triple BIF_Marker3;

/* Lines */
typedef BIF_Simple_double BIF_Line;
typedef struct
{
	BIF_ENTITY_HEADER
	STD_DATA_BODY
	Pline_vdata_list3 vdata;
} BIF_Line3;

/* Polygons */
typedef BIF_Simple_double BIF_Polygon;
typedef struct
{
	BIF_ENTITY_HEADER
	STD_DATA_BODY
	Pfacet_data3 fdata;
	Pfacet_vdata_arr3 vdata;
} BIF_Polygon3;

typedef struct
{
	BIF_ENTITY_HEADER
	int numContours;
	Ppoint_list *sets;
} BIF_Fillareaset;

typedef struct
{
	BIF_ENTITY_HEADER
	STD_DATA_BODY
	int numContours;
	Pfacet_data3 fdata;
	Pedge_data_list *edata;
	Pfacet_vdata_list3 *vdata;
} BIF_Fillareaset3;

typedef struct
{
	BIF_ENTITY_HEADER
	STD_DATA_BODY
	Pfacet_data_arr3 fdata;
	Pfacet_vdata_arr3 vdata;
} BIF_Triangle3;

typedef struct
{
	BIF_ENTITY_HEADER
	STD_DATA_BODY
	Pint_size dim;
	Pfacet_data_arr3 fdata;
	Pfacet_vdata_arr3 vdata;
} BIF_Quadmesh3;

typedef struct
{
        BIF_ENTITY_HEADER
        STD_DATA_BODY
	Pfacet_data_arr3 fdata;
	Pedge_data_list_list *edata;
	Pint_list_list *vlist;
	Pfacet_vdata_list3 vdata;
} BIF_Indexpolygons3;


/* 4.2.6 Text */
typedef struct
{
	BIF_ENTITY_HEADER
	char *text_string;
	Ppoint text_pt;
} BIF_Text;

typedef struct
{
	BIF_ENTITY_HEADER
	char *text_string;
	Ppoint3 text_pt;
	Pvec3 dir[2];
} BIF_Text3;

typedef struct
{
	BIF_ENTITY_HEADER
	char *text_string;
	Ppoint3 ref_pt;
	Ppoint3 anno_offset;
} BIF_Anno_text3;

/* GEN Primitives */
typedef struct
{
	BIF_ENTITY_HEADER
	int numLong, numLat;
	int exact;
	float radius;
	vector3 center;
	vector3 scale;
	vector3 norm;
} BIF_Sphere3, BIF_Circle, BIF_Circle3;

/* Pixel Primitives */
typedef struct
{
	BIF_ENTITY_HEADER
	vector3		upperLeft;
	int numRows, numColumns;
	int pixelUpdateFunction;
	int bifMapType;
	int numCoords;
	unsigned char	*red;
	unsigned char	*green;
	unsigned char	*blue;
} BIF_Pixelmap3;


/* WORKING: NURBS not defined internally */
typedef struct
{
	BIF_ENTITY_HEADER
	int dummy1,dummy2;
} BIF_Nubc;
				
typedef struct
{
	BIF_ENTITY_HEADER
	int dummy1,dummy2;
} BIF_Nubs;
/* WORKING */




/* 4.3 Primitive attributes */ 
/* 4.3.1 Marker attributes */
typedef struct
{
	BIF_ENTITY_HEADER
	int markertype;
} BIF_Markertype;

typedef BIF_Size BIF_Markersize;

/* 4.3.2 Line attributes */
typedef struct
{
	BIF_ENTITY_HEADER
	int linetype;
} BIF_Linetype;

typedef struct
{
	BIF_ENTITY_HEADER
	Pfloat width;
} BIF_Linewidth;

/* 4.3.3 Polygon attributes */
typedef struct
{
	BIF_ENTITY_HEADER
	int ind;
} BIF_Interiorstyle;

typedef struct
{
	BIF_ENTITY_HEADER
	int meth;
} BIF_Interiorshading;

typedef struct
{
	BIF_ENTITY_HEADER
	int equation;
} BIF_Interiorlighting;

typedef struct
{
	BIF_ENTITY_HEADER
	Prefl_props props;
} BIF_Surfaceproperties;

typedef struct
{
	BIF_ENTITY_HEADER
	Pdisting_mode g_mode;
	Pcull_mode cull_mode;
} BIF_Backfaceprocessing;

typedef struct
{
	BIF_ENTITY_HEADER
	Pedge_flag edge_flag;
} BIF_Edgeflag;

typedef BIF_Scale_factor BIF_Edgewidth;

/* 4.3.4 Text attributes */
typedef struct
{
	BIF_ENTITY_HEADER
	int font;
} BIF_Textfont;

typedef struct
{
	BIF_ENTITY_HEADER
	Ptext_prec precision;
} BIF_Textprec;

typedef struct
{
	BIF_ENTITY_HEADER
	Ptext_path path_list;
} BIF_Textpath;

typedef struct
{
	BIF_ENTITY_HEADER
	Ptext_align text_align;
} BIF_Textalign;

typedef struct
{
	BIF_ENTITY_HEADER
	Pfloat height;
} BIF_Charheight;

typedef struct
{
	BIF_ENTITY_HEADER
        Pvec up_vect;
} BIF_Charupvector;

/* 4.4 Rendering attributes */
/* 4.4.1 Lights and lighting attributes */
typedef struct
{
	BIF_ENTITY_HEADER
	int light_number;
	int ld_trans_id;		/* Light Definition Matrix */
	int light_type;         /* Ambient Default */
	int color_model;
	float light_color[3];   /* c1, c2, c3 */
	float direction[3];     /* u,  v,  w  */
	float position[3];      /* x,  y,  z  */
	float attenuation[2];   /* a,  b      */
	float exponent, spread; /* e,  s      */
} BIF_Definelight;

typedef struct
{
	BIF_ENTITY_HEADER
	Pint_list activation;
	Pint_list deactivation;
} BIF_Lightstate;

/* 4.4.2 Depthcue */
typedef struct
{
	BIF_ENTITY_HEADER
	int ind;
	Pdcue_bundle rep;
} BIF_DefineDepthCue;


/* 4.4.3 Hidden line and hidden surface removal */
typedef struct
{
	BIF_ENTITY_HEADER
	int id;
} BIF_Hlhsremoval;
/* 4.4.4 Color model attributes */

typedef struct
{
	BIF_ENTITY_HEADER
	int ind;
	int color_model;
	Pcolr_rep rep;
} BIF_Definecolor;

typedef struct
{
	BIF_ENTITY_HEADER
	int model;
} BIF_Colormodel;

/* 4.4.5 Display surface background color */
/* 4.5 Coordinate transformations */
/* 4.5.2.1 General matrix table utilities */
typedef struct
{
	BIF_ENTITY_HEADER
	int matrix_id;
} BIF_Identity3;

typedef struct
{
	BIF_ENTITY_HEADER
	int matrix_id_from;
	int matrix_id_to;
	int concat_type;
} BIF_Concatmatrix3;

typedef struct
{
	BIF_ENTITY_HEADER
	int matrix_id;
} BIF_Invertmatrix3;

/* 4.5.2.2 Rotation, translation, and scaling  */
/* 4.5.2.2.1. Rotation transformations */
typedef struct
{
	BIF_ENTITY_HEADER
	int matrix_id;
	float angle;
	int axis;
	int concat_type;
} BIF_Rotate3;

typedef BIF_Matrix_op_xyz BIF_Rotatexyz3;

typedef BIF_Matrix_op_xyz BIF_Translate3;

typedef BIF_Matrix_op_xyz BIF_Scale3;

typedef struct
{
	BIF_ENTITY_HEADER
	int matrix_id;
	float matrix[4][4];
	int concat_type;
} BIF_Matrix3;

typedef struct
{
	BIF_ENTITY_HEADER
	int matrix_id;
	int get_matrix;
	int concat_type;
} BIF_Getmatrix3;

/* 4.5.3 The Composite Modelling transformation */
/* 4.5.3.1 Using explicitly defined transformations */
typedef struct
{
	BIF_ENTITY_HEADER
	Pmatrix3 xform;
} BIF_Globaltransformation3;

typedef struct
{
	BIF_ENTITY_HEADER
        Pmatrix3 xform;
	Pcompose_type compose_type;
} BIF_Localtransformation3;

/* 4.5.3.2 Using computed transformations */
typedef struct
{
	BIF_ENTITY_HEADER
	int matrix_id;
	int structure_id;
	int matrix_label;
} BIF_Applytoglobal3;

typedef struct
{
	BIF_ENTITY_HEADER
	int matrix_id;
	int concat_type;
	int structure_id;
	int matrix_label;
} BIF_Applytolocal3;

/* 4.5.4 The view specification */
/* 4.5.4.1 View Matrix Utilities */
typedef struct
{
	BIF_ENTITY_HEADER
	int matrix_id;
	float view_reference[3];
	float view_normal[3];
	float view_up[3];
} BIF_Vieworientation3;

typedef struct
{
	BIF_ENTITY_HEADER
	int   matrix_id;
	float uvMinMax[4];  /* umin,umax,vmin,vmax */
	float front_plane, back_plane;
	int proj_type;
	float proj_reference[3];
	Pmatrix matrix;
	int match_type;
	float match_aspect;
} BIF_Viewmapping3;

typedef struct
{
	BIF_ENTITY_HEADER
	int id_view_spec;
	int id_view_orientation;
	int id_view_mapping;
	int xy_clip_flag;
	int front_clip_flag;
	int back_clip_flag;
	float ndcMinMax[6];
} BIF_Defineviewspecification;

typedef struct
{
	BIF_ENTITY_HEADER
	int id_view_spec;
	float radius_of_view;
	int   proj_type;
	Pview_rep3 rep;
} BIF_Defaultviewspecification;

typedef struct
{
	BIF_ENTITY_HEADER
	int id_view_spec;
} BIF_Activeview;

/* 4.6 Structure hierarchy */
/* 4.6.1 Invoking other structures */
typedef struct
{
	BIF_ENTITY_HEADER
	int 			structure_id;
	BIF_Beginstructure 	*structure_ptr;
	int			startLabel;
	int			endLabel;
} BIF_Executestructure, BIF_Callstructure;


/* 5. BIF Verb file */
/* 5.1 Input/output */
typedef struct
{
	BIF_ENTITY_HEADER
	char *bif_file;
} BIF_Readgeometry;

typedef BIF_No_data BIF_Cleargeometry;

typedef struct
{
	BIF_ENTITY_HEADER
	int			structureID;
	int			invokeStyle;
	int			startFrame;
	int			endFrame;
	int			startLabel;
	int			endLabel;
	BIF_Executestructure 	invoke;
} BIF_InvokeAtFrame;

/* 5.3 Test control */
typedef struct
{
	BIF_ENTITY_HEADER
	int 			structure_id;
	BIF_Beginstructure 	*structure_ptr;
	int n_repetitions;
} BIF_Begintest;

typedef BIF_No_data BIF_Endtest;

typedef BIF_No_data BIF_Pause;

typedef struct
{
	BIF_ENTITY_HEADER
	int sleep;
} BIF_Sleep;


/* ****************** */
/* Generic Data Types */
/* ****************** */
/* Things stored in any entity */
typedef BIF_No_data BIF_Any;
typedef BIF_Withdata3 BIF_Any_withdata;

/* ************************** */
/* Traverser State Structures */
/* ************************** */
/* Working: Make sure we have this correctly initialized */
typedef struct
{
	BIF_ENTITY_HEADER
	BIF_Beginstructure	*open_structure;
	union BIF_All 		*insert_after;
	int   push_level;	/* Attribute stacking level */
	int   nrs_state;	/* Is the Non-retained structure open */
	int   id_active_view;
	int   currentFrame;
	slList	*eol, *tol;	/* Call structure list (for edits)*/
	float global[4][4];
	float local[4][4];
	float composite[4][4];
} BIF_Traverser_state;

/* Union of all entities for easy recasting */
typedef union BIF_All
{
	int				entity_type;
	BIF_Any				any;
	BIF_Any_withdata		anywithdata;
	BIF_Activeview			activeview;
	BIF_Anno_text3			anno_text3;
	BIF_Applytoglobal3		applytoglobal3;
	BIF_Applytolocal3		applytolocal3;
	BIF_Backfaceprocessing		backfaceprocessing;
	BIF_Beginstructure		beginstructure;
	BIF_Begintest			begintest;
	BIF_Callstructure		callstructure;
	BIF_Charheight			charheight;
	BIF_Charupvector		charupvector;
	BIF_Circle			circle;
	BIF_Circle3			circle3;
	BIF_Cleargeometry		cleargeometry;
	BIF_Colormodel			colormodel;
	BIF_Concatmatrix3		concatmatrix3;
	BIF_Defaultviewspecification	defaultviewspecification;
	BIF_DefineDepthCue		definedepthcue;
	BIF_Definecolor			definecolor;
	BIF_Definelight			definelight;
	BIF_Defineviewspecification	defineviewspecification;
	BIF_Edgeflag			edgeflag;
	BIF_Edgewidth			edgewidth;
	BIF_Endstructure		endstructure;
	BIF_Endtest			endtest;
	BIF_Executestructure		executestructure;
	BIF_Fillareaset			fillareaset;
	BIF_Fillareaset3		fillareaset3;
	BIF_Getmatrix3			getmatrix3;
	BIF_Globaltransformation3	globaltransformation3;
	BIF_Hlhsremoval			hlhsremoval;
	BIF_Identity3			identity3;
	BIF_Index			ind;
	BIF_Indexpolygons3		indexpolygons3;
	BIF_Interiorlighting		interiorlighting;
	BIF_Interiorshading		interiorshading;
	BIF_Interiorstyle		interiorstyle;
	BIF_Invertmatrix3		invertmatrix3;
	BIF_InvokeAtFrame		invokeatframe;
	BIF_Lightstate			lightstate;
	BIF_Line			line;
	BIF_Line3			line3;
	BIF_Linetype			linetype;
	BIF_Linewidth			linewidth;
	BIF_Localtransformation3	localtransformation3;
	BIF_Marker			marker;
	BIF_Marker3			marker3;
	BIF_Markersize			markersize;
	BIF_Markertype			markertype;
	BIF_Matrix3			matrix3;
	BIF_Matrix_op_xyz		matrix_op_xyz;
	BIF_No_data			no_data;
	BIF_Nubc			nubc;
	BIF_Nubs			nubs;
	BIF_Pause			pause;
	BIF_Pixelmap3			pixelmap3;
	BIF_Polygon			polygon;
	BIF_Polygon3			polygon3;
	BIF_Quadmesh3			quadmesh3;
	BIF_Readgeometry		readgeometry;
	BIF_Rotate3			rotate3;
	BIF_Rotatexyz3			rotatexyz3;
	BIF_Scale3			scale3;
	BIF_Scale_factor		scale_factor;
	BIF_Simple_double		simple_double;
	BIF_Simple_triple		simple_triple;
	BIF_Size			size;
	BIF_Sleep			sleep;
	BIF_Sphere3			sphere3;
	BIF_Surfaceproperties		surfaceproperties;
	BIF_Text			text;
	BIF_Text3			text3;
	BIF_Textalign			textalign;
	BIF_Textfont			textfont;
	BIF_Textpath			textpath;
	BIF_Textprec			textprec;
	BIF_Translate3			translate3;
	BIF_Traverser_state		traverser_state;
	BIF_True_color			truecolor;
	BIF_Viewmapping3		viewmapping3;
	BIF_Vieworientation3		vieworientation3;
	BIF_Withdata3			withdata3;

} BIF_All;

/* Union of all STD_DATA_BODY entities for easy recasting */
typedef union BIF_All_withdata
{
	int			entity_type;
	BIF_Any_withdata	anywithdata;
	BIF_Withdata3		withdata3;
	BIF_Line3		line3;
	BIF_Polygon3		polygon3;
	BIF_Fillareaset3	fillareaset3;
	BIF_Triangle3		triangle3;
	BIF_Quadmesh3		quadmesh3;
	BIF_Indexpolygons3	indexpolygons3;
}BIF_All_withdata;
