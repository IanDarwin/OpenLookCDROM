/* $XConsortium: brftypes.h,v 5.1 91/02/16 10:07:24 rws Exp $ */

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
| Author        :	SimGraphics Engineering Corportation
|
| File          :	brftypes.h
| Date          :	Fri Feb  9 10:46:55 PST 1990
| Project       :	PLB
| Description   :	Defines the structure types for the BRF
| Status        :	Version 1.0
|
| Revisions     :	
|
\*--------------------------------------------------------------------*/

typedef struct 
{
	int	brf_flag;
	int	brf_start_frame;
	int	brf_end_frame;
	int	brf_num_frames;
} BRF_state;

#define BRF_COLLECT 0
#define BRF_INITIALIZE 1
#define BRF_REPORT 2
#define BRF_REPORT2 3
#define BRF_REPORT3 4

#define BRF_ENTITY_HEADER \
	int            entity_type;\
	union brf_all *next;\
	union brf_all *brf_totals; \
	int            (*handler)(); \
	char		*name; \
	int		times_called;\
	int		exceptions;\
	int		optional_data[7]; /* optional data groups */


#define BRF_DATA_BODY	\
	int		numCoords; \
	int		numFacets; \
	int		numEdges; \
	int		numContours; \
	int		vcolr; \
	int		vnorm; \
	int		fcolr; \
	int		fnorm; \
	int		evisi; 

/* ****************** */
/* General Structures */
/* ****************** */
typedef struct
{
	BRF_ENTITY_HEADER
} BRF_No_data;

typedef struct
{
	BRF_ENTITY_HEADER
	BRF_DATA_BODY
} BRF_With_data;

typedef BRF_No_data BRF_Any;
typedef BRF_With_data BRF_Any_With_Data;

typedef BRF_With_data BRF_Label;
typedef BRF_With_data BRF_Marker;
typedef BRF_With_data BRF_Marker3;
typedef BRF_With_data BRF_Line;
typedef BRF_With_data BRF_Line3;
typedef BRF_With_data BRF_Text;
typedef BRF_With_data BRF_Text3;
typedef BRF_With_data BRF_Annotationtext3;
typedef BRF_With_data BRF_Pixelmap3;
typedef BRF_With_data BRF_Gensphere3;
typedef BRF_With_data BRF_Gencircle;
typedef BRF_With_data BRF_Gencircle3;
typedef BRF_With_data BRF_Polygon;
typedef BRF_With_data BRF_Polygon3;
typedef BRF_With_data BRF_Fillareaset;
typedef BRF_With_data BRF_Fillareaset3;
typedef BRF_With_data BRF_Triangle3;
typedef BRF_With_data BRF_Quadmesh3;
typedef BRF_With_data BRF_Indexpolygons3;
typedef BRF_No_data BRF_Linetype;
typedef BRF_No_data BRF_Interiorshading;
typedef BRF_No_data BRF_Interiorlighting;
typedef BRF_No_data BRF_Backfaceprocessing;
typedef BRF_No_data BRF_Edgeflag;
typedef BRF_No_data BRF_Textfont;
typedef BRF_No_data BRF_Textprec;
typedef BRF_No_data BRF_Lightstate;
typedef BRF_No_data BRF_Depthcueindex;
typedef BRF_No_data BRF_Hlhsremoval;
typedef BRF_No_data BRF_Readgeometryfile;
typedef BRF_No_data BRF_Cleargeometry;
typedef BRF_No_data BRF_Beginstructure;
typedef BRF_No_data BRF_Endstructure;
typedef BRF_No_data BRF_Executestructure;
typedef BRF_No_data BRF_Callstructure;
typedef BRF_No_data BRF_Invokeatframe;
typedef BRF_No_data BRF_Defineviewspecification;
typedef BRF_No_data BRF_Definelight;
typedef BRF_No_data BRF_Markertype;
typedef BRF_No_data BRF_Markersize;
typedef BRF_No_data BRF_Markercolor;
typedef BRF_No_data BRF_Markercolorindex;
typedef BRF_No_data BRF_Linewidth;
typedef BRF_No_data BRF_Linecolor;
typedef BRF_No_data BRF_Linecolorindex;
typedef BRF_No_data BRF_Lineshading;
typedef BRF_No_data BRF_Interiorstyle;
typedef BRF_No_data BRF_Interiorpatternindex;
typedef BRF_No_data BRF_Interiorcolor;
typedef BRF_No_data BRF_Interiorcolorindex;
typedef BRF_No_data BRF_Backfaceinteriorcolor;
typedef BRF_No_data BRF_Backfaceinteriorcolorindex;
typedef BRF_No_data BRF_Surfaceproperties;
typedef BRF_No_data BRF_Backfaceproperties;
typedef BRF_No_data BRF_Edgetype;
typedef BRF_No_data BRF_Edgewidth;
typedef BRF_No_data BRF_Edgecolor;
typedef BRF_No_data BRF_Edgecolorindex;
typedef BRF_No_data BRF_Textcolor;
typedef BRF_No_data BRF_Textcolorindex;
typedef BRF_No_data BRF_Textpath;
typedef BRF_No_data BRF_Textalign;
typedef BRF_No_data BRF_Charheight;
typedef BRF_No_data BRF_Charexp;
typedef BRF_No_data BRF_Charspace;
typedef BRF_No_data BRF_Charupvector;
typedef BRF_No_data BRF_Annotextcharheight;
typedef BRF_No_data BRF_Annotextcharupvector;
typedef BRF_No_data BRF_Annotextstyle;
typedef BRF_No_data BRF_Identity3;
typedef BRF_No_data BRF_Concatmatrix3;
typedef BRF_No_data BRF_Invertmatrix3;
typedef BRF_No_data BRF_Rotate3;
typedef BRF_No_data BRF_Rotatexyz3;
typedef BRF_No_data BRF_Translate3;
typedef BRF_No_data BRF_Scale3;
typedef BRF_No_data BRF_Matrix3;
typedef BRF_No_data BRF_Getmatrix3;
typedef BRF_No_data BRF_Pushmatrix3;
typedef BRF_No_data BRF_Popmatrix3;
typedef BRF_No_data BRF_Globaltransformation3;
typedef BRF_No_data BRF_Localtransformation3;
typedef BRF_No_data BRF_Applytoglobal3;
typedef BRF_No_data BRF_Applytolocal3;
typedef BRF_No_data BRF_Vieworientation3;
typedef BRF_No_data BRF_Viewmapping3;
typedef BRF_No_data BRF_Activeview;
typedef BRF_No_data BRF_Begintest;
typedef BRF_No_data BRF_Endtest;
typedef BRF_No_data BRF_Pause;
typedef BRF_No_data BRF_Sleep;
typedef BRF_No_data BRF_Definecolor;
typedef BRF_No_data BRF_Backgroundcolor;
typedef BRF_No_data BRF_Backgroundcolorindex;
typedef BRF_No_data BRF_Defaultviewspecification;
typedef BRF_No_data BRF_Definedepthcue;
typedef BRF_No_data BRF_Configuration;

/* Union of all entities for easy recasting */
typedef union brf_all
{
	int entity_type;
	BRF_Any any;
	BRF_Any_With_Data any_with_data;

	BRF_Beginstructure beginstructure;
	BRF_Endstructure endstructure;
	BRF_Label label;
	BRF_Marker marker;
	BRF_Marker3 marker3;
	BRF_Line line;
	BRF_Line3 line3;
	BRF_Polygon polygon;
	BRF_Polygon3 polygon3;
	BRF_Fillareaset fillareaset;
	BRF_Fillareaset3 fillareaset3;
	BRF_Triangle3 triangle3;
	BRF_Quadmesh3 quadmesh3;
	BRF_Indexpolygons3 indexpolygons3;
	BRF_Gensphere3 gensphere3;
	BRF_Gencircle gencircle;
	BRF_Gencircle3 gencircle3;
	BRF_Text text;
	BRF_Text3 text3;
	BRF_Annotationtext3 annotationtext3;
	BRF_Pixelmap3 pixelmap3;
	BRF_Markertype markertype;
	BRF_Markersize markersize;
	BRF_Markercolor markercolor;
	BRF_Markercolorindex markercolorindex;
	BRF_Linetype linetype;
	BRF_Linewidth linewidth;
	BRF_Linecolor linecolor;
	BRF_Linecolorindex linecolorindex;
	BRF_Lineshading lineshading;
	BRF_Interiorstyle interiorstyle;
	BRF_Interiorpatternindex interiorpatternindex;
	BRF_Interiorcolor interiorcolor;
	BRF_Interiorcolorindex interiorcolorindex;
	BRF_Backfaceinteriorcolor backfaceinteriorcolor;
	BRF_Backfaceinteriorcolorindex backfaceinteriorcolorindex;
	BRF_Interiorshading interiorshading;
	BRF_Interiorlighting interiorlighting;
	BRF_Surfaceproperties surfaceproperties;
	BRF_Backfaceproperties backfaceproperties;
	BRF_Backfaceprocessing backfaceprocessing;
	BRF_Edgeflag edgeflag;
	BRF_Edgetype edgetype;
	BRF_Edgewidth edgewidth;
	BRF_Edgecolor edgecolor;
	BRF_Edgecolorindex edgecolorindex;
	BRF_Textfont textfont;
	BRF_Textprec textprec;
	BRF_Textcolor textcolor;
	BRF_Textcolorindex textcolorindex;
	BRF_Textpath textpath;
	BRF_Textalign textalign;
	BRF_Charheight charheight;
	BRF_Charexp charexp;
	BRF_Charspace charspace;
	BRF_Charupvector charupvector;
	BRF_Annotextcharheight annotextcharheight;
	BRF_Annotextcharupvector annotextcharupvector;
	BRF_Annotextstyle annotextstyle;
	BRF_Lightstate lightstate;
	BRF_Depthcueindex depthcueindex;
	BRF_Hlhsremoval hlhsremoval;
	BRF_Identity3 identity3;
	BRF_Concatmatrix3 concatmatrix3;
	BRF_Invertmatrix3 invertmatrix3;
	BRF_Rotate3 rotate3;
	BRF_Rotatexyz3 rotatexyz3;
	BRF_Translate3 translate3;
	BRF_Scale3 scale3;
	BRF_Matrix3 matrix3;
	BRF_Getmatrix3 getmatrix3;
	BRF_Pushmatrix3 pushmatrix3;
	BRF_Popmatrix3 popmatrix3;
	BRF_Globaltransformation3 globaltransformation3;
	BRF_Localtransformation3 localtransformation3;
	BRF_Applytoglobal3 applytoglobal3;
	BRF_Applytolocal3 applytolocal3;
	BRF_Vieworientation3 vieworientation3;
	BRF_Viewmapping3 viewmapping3;
	BRF_Activeview activeview;
	BRF_Executestructure executestructure;
	BRF_Callstructure callstructure;
	BRF_Readgeometryfile readgeometryfile;
	BRF_Cleargeometry cleargeometry;
	BRF_Begintest begintest;
	BRF_Endtest endtest;
	BRF_Pause pause;
	BRF_Sleep sleep;
	BRF_Invokeatframe invokeatframe;
	BRF_Definecolor definecolor;
	BRF_Backgroundcolor backgroundcolor;
	BRF_Backgroundcolorindex backgroundcolorindex;
	BRF_Defineviewspecification defineviewspecification;
	BRF_Defaultviewspecification defaultviewspecification;
	BRF_Definelight definelight;
	BRF_Definedepthcue definedepthcue;
	BRF_Configuration configuration;
} brf_all;

typedef union brf_all *BRF_table;
