/* $XConsortium: brf_set.h,v 5.2 91/10/21 14:33:04 eswu Exp $ */

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
| File          :	brf_set.h
| Date          :	Fri Feb  9 10:46:55 PST 1990
| Project       :	PLB
| Description   :	Initializes the BRF structures.
| Status        :	Version 1.0
|
| Revisions     :	
|
\*--------------------------------------------------------------------*/
BRF_Any_With_Data total_with_data =
{
	0,
	NULL,
	NULL,
	do_brfgeneric,
	"POLY TOTALS", 0,
	0,0,0,0,0,0
};
BRF_Label brf_label =
{
	LABEL,
	NULL,
	(union brf_all*)&total_with_data,
	do_brfgeneric,
	"LABEL", 0,
	0,0,0,0,0,0
};

BRF_Marker brf_marker =
{
	MARKER,
	NULL,
	(union brf_all*)&total_with_data,
	do_brfgeneric,
	"MARKER", 0,
	0,0,0,0,0,0
};

BRF_Marker3 brf_marker3 =
{
	MARKER3,
	NULL,
	(union brf_all*)&total_with_data,
	do_brfgeneric,
	"MARKER3", 0,
	0,0,0,0,0,0
};

BRF_Line brf_line =
{
	LINE,
	NULL,
	(union brf_all*)&total_with_data,
	do_brfgeneric,
	"LINE", 0,
	0,0,0,0,0,0
};

BRF_Line3 brf_line3 =
{
	LINE3,
	NULL,
	(union brf_all*)&total_with_data,
	do_brfgeneric,
	"LINE3", 0,
	0,0,0,0,0,0
};

BRF_Text brf_text =
{
	TEXT,
	NULL,
	(union brf_all*)&total_with_data,
	do_brfgeneric,
	"TEXT", 0,
	0,0,0,0,0,0
};

BRF_Text3 brf_text3 =
{
	TEXT3,
	NULL,
	(union brf_all*)&total_with_data,
	do_brfgeneric,
	"TEXT3", 0,
	0,0,0,0,0,0
};

BRF_Annotationtext3 brf_annotationtext3 =
{
	ANNOTATION_TEXT3,
	NULL,
	(union brf_all*)&total_with_data,
	do_brfgeneric,
	"ANNOTATION_TEXT3", 0,
	0,0,0,0,0,0
};

BRF_Pixelmap3 brf_pixelmap3 =
{
	PIXEL_MAP3,
	NULL,
	(union brf_all*)&total_with_data,
	do_brfgeneric,
	"PIXEL_MAP3", 0,
	0,0,0,0,0,0
};

BRF_Gensphere3 brf_gensphere3 =
{
	GEN_SPHERE3,
	NULL,
	(union brf_all*)&total_with_data,
	do_brfgeneric,
	"GEN_SPHERE3", 0,
	0,0,0,0,0,0
};

BRF_Gencircle brf_gencircle =
{
	GEN_CIRCLE,
	NULL,
	(union brf_all*)&total_with_data,
	do_brfgeneric,
	"GEN_CIRCLE", 0,
	0,0,0,0,0,0
};

BRF_Gencircle3 brf_gencircle3 =
{
	GEN_CIRCLE3,
	NULL,
	(union brf_all*)&total_with_data,
	do_brfgeneric,
	"GEN_CIRCLE3", 0,
	0,0,0,0,0,0
};

BRF_Polygon brf_polygon =
{
	POLYGON,
	NULL,
	(union brf_all*)&total_with_data,
	do_brfpolygon,
	"POLYGON", 0,
	0,0,0,0,0,0
};

BRF_Polygon3 brf_polygon3 =
{
	POLYGON3,
	NULL,
	(union brf_all*)&total_with_data,
	do_brfpolygon3,
	"POLYGON3", 0,
	0,0,0,0,0,0
};

BRF_Fillareaset brf_fillareaset =
{
	FILL_AREA_SET,
	NULL,
	(union brf_all*)&total_with_data,
	do_brffillareaset,
	"FILL_AREA_SET", 0,
	0,0,0,0,0,0
};

BRF_Fillareaset3 brf_fillareaset3 =
{
	FILL_AREA_SET3,
	NULL,
	(union brf_all*)&total_with_data,
	do_brffillareaset3,
	"FILL_AREA_SET3", 0,
	0,0,0,0,0,0
};

BRF_Triangle3 brf_triangle3 =
{
	TRIANGLE3,
	NULL,
	(union brf_all*)&total_with_data,
	do_brftriangle3,
	"TRIANGLE3", 0,
	0,0,0,0,0,0
};

BRF_Quadmesh3 brf_quadmesh3 =
{
	QUAD_MESH3,
	NULL,
	(union brf_all*)&total_with_data,
	do_brfquadmesh3,
	"QUAD_MESH3", 0,
	0,0,0,0,0,0
};

BRF_Indexpolygons3 brf_indexpolygons3 =
{
	INDEX_POLYGONS3,
	NULL,
	(union brf_all*)&total_with_data,
	do_brfindexpolygons3,
	"INDEX_POLYGONS3", 0,
	0,0,0,0,0,0
};

BRF_Linetype brf_linetype =
{
	LINE_TYPE,
	NULL,
	NULL,
	do_brfgeneric,
	"LINE_TYPE", 0,
};

BRF_Interiorshading brf_interiorshading =
{
	INTERIOR_SHADING,
	NULL,
	NULL,
	do_brfgeneric,
	"INTERIOR_SHADING", 0,
};

BRF_Interiorlighting brf_interiorlighting =
{
	INTERIOR_LIGHTING,
	NULL,
	NULL,
	do_brfgeneric,
	"INTERIOR_LIGHTING", 0,
};

BRF_Backfaceprocessing brf_backfaceprocessing =
{
	BACKFACE_PROCESSING,
	NULL,
	NULL,
	do_brfgeneric,
	"BACKFACE_PROCESSING", 0,
};

BRF_Edgeflag brf_edgeflag =
{
	EDGE_FLAG,
	NULL,
	NULL,
	do_brfgeneric,
	"EDGE_FLAG", 0,
};

BRF_Textfont brf_textfont =
{
	TEXT_FONT,
	NULL,
	NULL,
	do_brfgeneric,
	"TEXT_FONT", 0,
};

BRF_Textprec brf_textprec =
{
	TEXT_PREC,
	NULL,
	NULL,
	do_brfgeneric,
	"TEXT_PREC", 0,
};

BRF_Lightstate brf_lightstate =
{
	LIGHT_STATE,
	NULL,
	NULL,
	do_brfgeneric,
	"LIGHT_STATE", 0,
};

BRF_Depthcueindex brf_depthcueindex =
{
	DEPTHCUE_INDEX,
	NULL,
	NULL,
	do_brfgeneric,
	"DEPTHCUE_INDEX", 0,
};

BRF_Hlhsremoval brf_hlhsremoval =
{
	HLHS_REMOVAL,
	NULL,
	NULL,
	do_brfgeneric,
	"HLHS_REMOVAL", 0,
};

BRF_Readgeometryfile brf_readgeometryfile =
{
	READ_GEOMETRY_FILE,
	NULL,
	NULL,
	do_brfgeneric,
	"READ_GEOMETRY_FILE", 0,
};

BRF_Cleargeometry brf_cleargeometry =
{
	CLEAR_GEOMETRY,
	NULL,
	NULL,
	do_brfgeneric,
	"CLEAR_GEOMETRY", 0,
};

BRF_Beginstructure brf_beginstructure =
{
	BEGIN_STRUCTURE,
	NULL,
	NULL,
	do_brfgeneric,
	"BEGIN_STRUCTURE", 0,
};

BRF_Endstructure brf_endstructure =
{
	END_STRUCTURE,
	NULL,
	NULL,
	do_brfgeneric,
	"END_STRUCTURE", 0,
};

BRF_Executestructure brf_executestructure =
{
	EXECUTE_STRUCTURE,
	NULL,
	NULL,
	do_brfexecutestructure,
	"EXECUTE_STRUCTURE", 0,
};

BRF_Callstructure brf_callstructure =
{
	CALL_STRUCTURE,
	NULL,
	NULL,
	do_brfcallstructure,
	"CALL_STRUCTURE", 0,
};

BRF_Invokeatframe brf_invokeatframe =
{
	INVOKE_AT_FRAME,
	NULL,
	NULL,
	do_brfinvokeatframe,
	"INVOKE_AT_FRAME", 0,
};

BRF_Defineviewspecification brf_defineviewspecification =
{
	DEFINE_VIEW_SPECIFICATION,
	NULL,
	NULL,
	do_brfgeneric,
	"DEFINE_VIEW_SPECIFICATION", 0,
};

BRF_Definelight brf_definelight =
{
	DEFINE_LIGHT,
	NULL,
	NULL,
	do_brfgeneric,
	"DEFINE_LIGHT", 0,
};

BRF_Markertype brf_markertype =
{
	MARKER_TYPE,
	NULL,
	NULL,
	do_brfgeneric,
	"MARKER_TYPE", 0,
};

BRF_Markersize brf_markersize =
{
	MARKER_SIZE,
	NULL,
	NULL,
	do_brfgeneric,
	"MARKER_SIZE", 0,
};

BRF_Markercolor brf_markercolor =
{
	MARKER_COLOR,
	NULL,
	NULL,
	do_brfgeneric,
	"MARKER_COLOR", 0,
};

BRF_Markercolorindex brf_markercolorindex =
{
	MARKER_COLOR_INDEX,
	NULL,
	NULL,
	do_brfgeneric,
	"MARKER_COLOR_INDEX", 0,
};

BRF_Linewidth brf_linewidth =
{
	LINE_WIDTH,
	NULL,
	NULL,
	do_brfgeneric,
	"LINE_WIDTH", 0,
};

BRF_Linecolor brf_linecolor =
{
	LINE_COLOR,
	NULL,
	NULL,
	do_brfgeneric,
	"LINE_COLOR", 0,
};

BRF_Linecolorindex brf_linecolorindex =
{
	LINE_COLOR_INDEX,
	NULL,
	NULL,
	do_brfgeneric,
	"LINE_COLOR_INDEX", 0,
};

BRF_Lineshading brf_lineshading =
{
	LINE_SHADING,
	NULL,
	NULL,
	do_brfgeneric,
	"LINE_SHADING", 0,
};

BRF_Interiorstyle brf_interiorstyle =
{
	INTERIOR_STYLE,
	NULL,
	NULL,
	do_brfgeneric,
	"INTERIOR_STYLE", 0,
};

BRF_Interiorpatternindex brf_interiorpatternindex =
{
	INTERIOR_PATTERN_INDEX,
	NULL,
	NULL,
	do_brfgeneric,
	"INTERIOR_PATTERN_INDEX", 0,
};

BRF_Interiorcolor brf_interiorcolor =
{
	INTERIOR_COLOR,
	NULL,
	NULL,
	do_brfgeneric,
	"INTERIOR_COLOR", 0,
};

BRF_Interiorcolorindex brf_interiorcolorindex =
{
	INTERIOR_COLOR_INDEX,
	NULL,
	NULL,
	do_brfgeneric,
	"INTERIOR_COLOR_INDEX", 0,
};

BRF_Backfaceinteriorcolor brf_backfaceinteriorcolor =
{
	BACKFACE_INTERIOR_COLOR,
	NULL,
	NULL,
	do_brfgeneric,
	"BACKFACE_INTERIOR_COLOR", 0,
};

BRF_Backfaceinteriorcolorindex brf_backfaceinteriorcolorindex =
{
	BACKFACE_INTERIOR_COLOR_INDEX,
	NULL,
	NULL,
	do_brfgeneric,
	"BACKFACE_INTERIOR_COLOR_INDEX", 0,
};

BRF_Surfaceproperties brf_surfaceproperties =
{
	SURFACE_PROPERTIES,
	NULL,
	NULL,
	do_brfgeneric,
	"SURFACE_PROPERTIES", 0,
};

BRF_Backfaceproperties brf_backfaceproperties =
{
	BACKFACE_PROPERTIES,
	NULL,
	NULL,
	do_brfgeneric,
	"BACKFACE_PROPERTIES", 0,
};

BRF_Edgetype brf_edgetype =
{
	EDGE_TYPE,
	NULL,
	NULL,
	do_brfgeneric,
	"EDGE_TYPE", 0,
};

BRF_Edgewidth brf_edgewidth =
{
	EDGE_WIDTH,
	NULL,
	NULL,
	do_brfgeneric,
	"EDGE_WIDTH", 0,
};

BRF_Edgecolor brf_edgecolor =
{
	EDGE_COLOR,
	NULL,
	NULL,
	do_brfgeneric,
	"EDGE_COLOR", 0,
};

BRF_Edgecolorindex brf_edgecolorindex =
{
	EDGE_COLOR_INDEX,
	NULL,
	NULL,
	do_brfgeneric,
	"EDGE_COLOR_INDEX", 0,
};

BRF_Textcolor brf_textcolor =
{
	TEXT_COLOR,
	NULL,
	NULL,
	do_brfgeneric,
	"TEXT_COLOR", 0,
};

BRF_Textcolorindex brf_textcolorindex =
{
	TEXT_COLOR_INDEX,
	NULL,
	NULL,
	do_brfgeneric,
	"TEXT_COLOR_INDEX", 0,
};

BRF_Textpath brf_textpath =
{
	TEXT_PATH,
	NULL,
	NULL,
	do_brfgeneric,
	"TEXT_PATH", 0,
};

BRF_Textalign brf_textalign =
{
	TEXT_ALIGN,
	NULL,
	NULL,
	do_brfgeneric,
	"TEXT_ALIGN", 0,
};

BRF_Charheight brf_charheight =
{
	CHAR_HEIGHT,
	NULL,
	NULL,
	do_brfgeneric,
	"CHAR_HEIGHT", 0,
};

BRF_Charexp brf_charexp =
{
	CHAR_EXP,
	NULL,
	NULL,
	do_brfgeneric,
	"CHAR_EXP", 0,
};

BRF_Charspace brf_charspace =
{
	CHAR_SPACE,
	NULL,
	NULL,
	do_brfgeneric,
	"CHAR_SPACE", 0,
};

BRF_Charupvector brf_charupvector =
{
	CHAR_UP_VECTOR,
	NULL,
	NULL,
	do_brfgeneric,
	"CHAR_UP_VECTOR", 0,
};

BRF_Annotextcharheight brf_annotextcharheight =
{
	ANNO_TEXT_CHAR_HEIGHT,
	NULL,
	NULL,
	do_brfgeneric,
	"ANNO_TEXT_CHAR_HEIGHT", 0,
};

BRF_Annotextcharupvector brf_annotextcharupvector =
{
	ANNO_TEXT_CHAR_UP_VECTOR,
	NULL,
	NULL,
	do_brfgeneric,
	"ANNO_TEXT_CHAR_UP_VECTOR", 0,
};

BRF_Annotextstyle brf_annotextstyle =
{
	ANNO_TEXT_STYLE,
	NULL,
	NULL,
	do_brfgeneric,
	"ANNO_TEXT_STYLE", 0,
};

BRF_Identity3 brf_identity3 =
{
	IDENTITY3,
	NULL,
	NULL,
	do_brfgeneric,
	"IDENTITY3", 0,
};

BRF_Concatmatrix3 brf_concatmatrix3 =
{
	CONCAT_MATRIX3,
	NULL,
	NULL,
	do_brfgeneric,
	"CONCAT_MATRIX3", 0,
};

BRF_Invertmatrix3 brf_invertmatrix3 =
{
	INVERT_MATRIX3,
	NULL,
	NULL,
	do_brfgeneric,
	"INVERT_MATRIX3", 0,
};

BRF_Rotate3 brf_rotate3 =
{
	ROTATE3,
	NULL,
	NULL,
	do_brfgeneric,
	"ROTATE3", 0,
};

BRF_Rotatexyz3 brf_rotatexyz3 =
{
	ROTATE_XYZ3,
	NULL,
	NULL,
	do_brfgeneric,
	"ROTATE_XYZ3", 0,
};

BRF_Translate3 brf_translate3 =
{
	TRANSLATE3,
	NULL,
	NULL,
	do_brfgeneric,
	"TRANSLATE3", 0,
};

BRF_Scale3 brf_scale3 =
{
	SCALE3,
	NULL,
	NULL,
	do_brfgeneric,
	"SCALE3", 0,
};

BRF_Matrix3 brf_matrix3 =
{
	MATRIX3,
	NULL,
	NULL,
	do_brfgeneric,
	"MATRIX3", 0,
};

BRF_Getmatrix3 brf_getmatrix3 =
{
	GET_MATRIX3,
	NULL,
	NULL,
	do_brfgeneric,
	"GET_MATRIX3", 0,
};

BRF_Pushmatrix3 brf_pushmatrix3 =
{
	PUSH_MATRIX3,
	NULL,
	NULL,
	do_brfgeneric,
	"PUSH_MATRIX3", 0,
};

BRF_Popmatrix3 brf_popmatrix3 =
{
	POP_MATRIX3,
	NULL,
	NULL,
	do_brfgeneric,
	"POP_MATRIX3", 0,
};

BRF_Globaltransformation3 brf_globaltransformation3 =
{
	GLOBAL_TRANSFORMATION3,
	NULL,
	NULL,
	do_brfgeneric,
	"GLOBAL_TRANSFORMATION3", 0,
};

BRF_Localtransformation3 brf_localtransformation3 =
{
	LOCAL_TRANSFORMATION3,
	NULL,
	NULL,
	do_brfgeneric,
	"LOCAL_TRANSFORMATION3", 0,
};

BRF_Applytoglobal3 brf_applytoglobal3 =
{
	APPLY_TO_GLOBAL3,
	NULL,
	NULL,
	do_brfgeneric,
	"APPLY_TO_GLOBAL3", 0,
};

BRF_Applytolocal3 brf_applytolocal3 =
{
	APPLY_TO_LOCAL3,
	NULL,
	NULL,
	do_brfgeneric,
	"APPLY_TO_LOCAL3", 0,
};

BRF_Vieworientation3 brf_vieworientation3 =
{
	VIEW_ORIENTATION3,
	NULL,
	NULL,
	do_brfgeneric,
	"VIEW_ORIENTATION3", 0,
};

BRF_Viewmapping3 brf_viewmapping3 =
{
	VIEW_MAPPING3,
	NULL,
	NULL,
	do_brfgeneric,
	"VIEW_MAPPING3", 0,
};

BRF_Activeview brf_activeview =
{
	ACTIVE_VIEW,
	NULL,
	NULL,
	do_brfgeneric,
	"ACTIVE_VIEW", 0,
};

BRF_Begintest brf_begintest =
{
	BEGIN_TEST,
	NULL,
	NULL,
	do_brfgeneric,
	"BEGIN_TEST", 0,
};

BRF_Endtest brf_endtest =
{
	END_TEST,
	NULL,
	NULL,
	do_brfgeneric,
	"END_TEST", 0,
};

BRF_Pause brf_pause =
{
	PAUSE,
	NULL,
	NULL,
	do_brfgeneric,
	"PAUSE", 0,
};

BRF_Sleep brf_sleep =
{
	SLEEP,
	NULL,
	NULL,
	do_brfgeneric,
	"SLEEP", 0,
};

BRF_Definecolor brf_definecolor =
{
	DEFINE_COLOR,
	NULL,
	NULL,
	do_brfgeneric,
	"DEFINE_COLOR", 0,
};

BRF_Backgroundcolor brf_backgroundcolor =
{
	BACKGROUND_COLOR,
	NULL,
	NULL,
	do_brfgeneric,
	"BACKGROUND_COLOR", 0,
};

BRF_Backgroundcolorindex brf_backgroundcolorindex =
{
	BACKGROUND_COLOR_INDEX,
	NULL,
	NULL,
	do_brfgeneric,
	"BACKGROUND_COLOR_INDEX", 0,
};

BRF_Defaultviewspecification brf_defaultviewspecification =
{
	DEFAULT_VIEW_SPECIFICATION,
	NULL,
	NULL,
	do_brfgeneric,
	"DEFAULT_VIEW_SPECIFICATION", 0,
};

BRF_Definedepthcue brf_definedepthcue =
{
	DEFINE_DEPTHCUE,
	NULL,
	NULL,
	do_brfgeneric,
	"DEFINE_DEPTHCUE", 0,
};

BRF_Configuration brf_configuration =
{
	CONFIGURATION,
	NULL,
	NULL,
	do_brfgeneric,
	"CONFIGURATION", 0,
};


BRF_table brf_table[] = 
{
	(BRF_table) &brf_beginstructure,
	(BRF_table) &brf_endstructure,
	(BRF_table) &brf_label,
	(BRF_table) &brf_marker,
	(BRF_table) &brf_marker3,
	(BRF_table) &brf_line,
	(BRF_table) &brf_line3,
	(BRF_table) &brf_polygon,
	(BRF_table) &brf_polygon3,
	(BRF_table) &brf_fillareaset,
	(BRF_table) &brf_fillareaset3,
	(BRF_table) &brf_triangle3,
	(BRF_table) &brf_quadmesh3,
	(BRF_table) &brf_indexpolygons3,
	(BRF_table) &brf_gensphere3,
	(BRF_table) &brf_gencircle,
	(BRF_table) &brf_gencircle3,
	(BRF_table) &brf_text,
	(BRF_table) &brf_text3,
	(BRF_table) &brf_annotationtext3,
	(BRF_table) &brf_pixelmap3,
	(BRF_table) &brf_markertype,
	(BRF_table) &brf_markersize,
	(BRF_table) &brf_markercolor,
	(BRF_table) &brf_markercolorindex,
	(BRF_table) &brf_linetype,
	(BRF_table) &brf_linewidth,
	(BRF_table) &brf_linecolor,
	(BRF_table) &brf_linecolorindex,
	(BRF_table) &brf_lineshading,
	(BRF_table) &brf_interiorstyle,
	(BRF_table) &brf_interiorpatternindex,
	(BRF_table) &brf_interiorcolor,
	(BRF_table) &brf_interiorcolorindex,
	(BRF_table) &brf_backfaceinteriorcolor,
	(BRF_table) &brf_backfaceinteriorcolorindex,
	(BRF_table) &brf_interiorshading,
	(BRF_table) &brf_interiorlighting,
	(BRF_table) &brf_surfaceproperties,
	(BRF_table) &brf_backfaceproperties,
	(BRF_table) &brf_backfaceprocessing,
	(BRF_table) &brf_edgeflag,
	(BRF_table) &brf_edgetype,
	(BRF_table) &brf_edgewidth,
	(BRF_table) &brf_edgecolor,
	(BRF_table) &brf_edgecolorindex,
	(BRF_table) &brf_textfont,
	(BRF_table) &brf_textprec,
	(BRF_table) &brf_textcolor,
	(BRF_table) &brf_textcolorindex,
	(BRF_table) &brf_textpath,
	(BRF_table) &brf_textalign,
	(BRF_table) &brf_charheight,
	(BRF_table) &brf_charexp,
	(BRF_table) &brf_charspace,
	(BRF_table) &brf_charupvector,
	(BRF_table) &brf_annotextcharheight,
	(BRF_table) &brf_annotextcharupvector,
	(BRF_table) &brf_annotextstyle,
	(BRF_table) &brf_lightstate,
	(BRF_table) &brf_depthcueindex,
	(BRF_table) &brf_hlhsremoval,
	(BRF_table) &brf_identity3,
	(BRF_table) &brf_concatmatrix3,
	(BRF_table) &brf_invertmatrix3,
	(BRF_table) &brf_rotate3,
	(BRF_table) &brf_rotatexyz3,
	(BRF_table) &brf_translate3,
	(BRF_table) &brf_scale3,
	(BRF_table) &brf_matrix3,
	(BRF_table) &brf_getmatrix3,
	(BRF_table) &brf_pushmatrix3,
	(BRF_table) &brf_popmatrix3,
	(BRF_table) &brf_globaltransformation3,
	(BRF_table) &brf_localtransformation3,
	(BRF_table) &brf_applytoglobal3,
	(BRF_table) &brf_applytolocal3,
	(BRF_table) &brf_vieworientation3,
	(BRF_table) &brf_viewmapping3,
	(BRF_table) &brf_activeview,
	(BRF_table) &brf_executestructure,
	(BRF_table) &brf_callstructure,
	(BRF_table) &brf_readgeometryfile,
	(BRF_table) &brf_cleargeometry,
	(BRF_table) &brf_begintest,
	(BRF_table) &brf_endtest,
	(BRF_table) &brf_pause,
	(BRF_table) &brf_sleep,
	(BRF_table) &brf_invokeatframe,
	(BRF_table) &brf_definecolor,
	(BRF_table) &brf_backgroundcolor,
	(BRF_table) &brf_backgroundcolorindex,
	(BRF_table) &brf_defineviewspecification,
	(BRF_table) &brf_defaultviewspecification,
	(BRF_table) &brf_definelight,
	(BRF_table) &brf_definedepthcue,
	(BRF_table) &brf_configuration};

int brf_table_size = (sizeof(brf_table) / sizeof(BRF_table));
