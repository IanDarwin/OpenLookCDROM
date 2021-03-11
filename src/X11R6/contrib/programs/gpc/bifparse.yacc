/*========================================================================
bifparse.y
	yacc grammar input file for yacc demonstration.

========================================================================*/

/*----------------*/
/* C declarations */
/*----------------*/

%{
/*--------------------------------------------------------------------*\
|
|  Copyright (C) 1989, 1990, 1991,1990, National Computer Graphics Association
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
| Author        :	mjf / SimGraphics Engineering Corportation
|
| File          :	bifparse.y
| Date          :	Fri Feb  9 10:46:55 PST 1990
| Project       :	PLB
| Description   :	
| Status        :	Version 1.0
|
| Revisions     :	
|
\*--------------------------------------------------------------------*/

#include <stdio.h>

#ifdef SUN4
#include <strings.h>
#else
#include <string.h>
#endif

#include <ctype.h>
#include "bifbuild.h"

#define STRLENGTH 80
#define REPEAT_TABLE_SIZE 30

#ifndef FALSE
#define FALSE 0
#endif /* ifndef FALSE */
#ifndef TRUE
#define TRUE (!FALSE)
#endif /* ifndef TRUE */

#define CB_ERM1 "Additional attempt to set color model ignored"
#define CB_ERM2 "Additional attempt to set buffer mode ignored"
#define CB_ERM3 "Additional attempt to set color mode ignored"
#define CB_ERM4 "Additional attempt to set window size ignored"
#define FAS_ERM1 "Additional VERTEX_COLOR set in FILL_AREA_SET3 ignored."
#define FAS_ERM2 "Additional VERTEX_COLOR_INDICES in FILL_AREA_SET3 ignored."
#define FAS_ERM3 "Additional VERTEX_NORMALS set in FILL_AREA_SET3 ignored."
#define FAS_ERM4 "Additional EDGE_VISIBILITY set in FILL_AREA_SET3 ignored."
#define FAS_ERM5 "Additional FACET_COLORS set in FILL_AREA_SET3 ignored."
#define FAS_ERM6 "Additional FACET_COLOR_INDICES in FILL_AREA_SET3 ignored."
#define FAS_ERM7 "Additional FACET_NORMALS set in FILL_AREA_SET3 ignored."
#define GS_ERM1 "Additional CENTER in GEN_SPHERE3 ignored."
#define GS_ERM2 "Additional RADIUS in GEN_SPHERE3 ignored."
#define GS_ERM3 "Additional SCALE_FACTOR in GEN_SPHERE3 ignored."
#define GC_ERM1 "Additional CENTER in GEN_CIRCLE ignored."
#define GC_ERM2 "Additional RADIUS in GEN_CIRCLE ignored."
#define GC3_ERM1 "Additional CENTER in GEN_CIRCLE3 ignored."
#define GC3_ERM2 "Additional RADIUS in GEN_CIRCLE3 ignored."
#define GC3_ERM3 "Additional NORMAL in GEN_CIRCLE3 ignored."
#define WD3B_error(A) \
{ \
  char temp[STRLENGTH]; \
  sprintf(temp,"Additional %s in %s ignored.", A, with_data_name ); \
  yyerror(temp); \
}
				
typedef char StringType[STRLENGTH];

static char *with_data_name;
extern FILE *active_file, *last_file;
extern char filenm[128], last_filenm[128];
extern int lineno,last_lineno;


%}

/*-------------------*/
/* yacc declarations */
/*-------------------*/

%start  bif_file

%union	{
	long		l;
	double		f;
	StringType	str;
	}

%token	<l>	LONG
%token	<f>	REAL
%token	<str>	QSTRING

/*--------------------------------------------------------------------*\
|	Structures
\*--------------------------------------------------------------------*/
%token		BEGIN_STRUCTURE
%token		END_STRUCTURE

/*--------------------------------------------------------------------*\
|	Graphics Primitives
\*--------------------------------------------------------------------*/
%token		LABEL
%token		MARKER
%token		MARKER3
%token		LINE
%token		LINE3
%token		POLYGON
%token		POLYGON3
%token		FILL_AREA_SET
%token		FILL_AREA_SET3
%token		TRIANGLE3
%token		QUAD_MESH3
%token		INDEX_POLYGONS3
%token		GEN_SPHERE3
%token		GEN_CIRCLE
%token		GEN_CIRCLE3
%token		TEXT
%token		TEXT3
%token		ANNOTATION_TEXT3
%token		PIXEL_MAP3
%token		NON_UNIFORM_BSPLINE_CURVE       		/* ver 1.0 */
%token		NON_UNIFORM_BSPLINE_SURFACE       		/* ver 1.0 */

/*--------------------------------------------------------------------*\
|	Primitive Attributes
\*--------------------------------------------------------------------*/
%token		CURVE_APPROXIMATION_CRITERIA			/* ver 1.0 */
%token		TRIMCURVE_APPROXIMATION_CRITERIA		/* ver 1.0 */
%token		SURFACE_APPROXIMATION_CRITERIA			/* ver 1.0 */
%token		MARKER_TYPE
%token		MARKER_SIZE
%token		MARKER_COLOR
%token		MARKER_COLOR_INDEX
%token		LINE_TYPE
%token		LINE_WIDTH
%token		LINE_COLOR
%token		LINE_COLOR_INDEX
%token		LINE_SHADING
%token		INTERIOR_STYLE
%token		INTERIOR_PATTERN_INDEX
%token		INTERIOR_COLOR
%token		INTERIOR_COLOR_INDEX
%token		BACKFACE_INTERIOR_COLOR
%token		BACKFACE_INTERIOR_COLOR_INDEX
%token		INTERIOR_SHADING
%token		INTERIOR_LIGHTING
%token		SURFACE_PROPERTIES
%token		BACKFACE_PROPERTIES
%token		BACKFACE_PROCESSING
%token		EDGE_FLAG
%token		EDGE_TYPE
%token		EDGE_WIDTH
%token		EDGE_COLOR
%token		EDGE_COLOR_INDEX
%token		TEXT_FONT
%token		TEXT_PREC
%token		TEXT_COLOR
%token		TEXT_COLOR_INDEX
%token		TEXT_PATH
%token		TEXT_ALIGN
%token		CHAR_HEIGHT
%token		CHAR_EXP
%token		CHAR_SPACE
%token		CHAR_UP_VECTOR
%token		ANNO_TEXT_CHAR_HEIGHT
%token		ANNO_TEXT_CHAR_UP_VECTOR
%token		ANNO_TEXT_STYLE

/*--------------------------------------------------------------------*\
|	Rendering Attributes
\*--------------------------------------------------------------------*/
		/*	DEFINE_LIGHT:		MOVED*/
%token		LIGHT_STATE
%token		DEPTHCUE_INDEX
%token		HLHS_REMOVAL
		/*	DEFINE_COLOR:		MOVED*/
		/*	DEFINE_COLOR_RAMP:	DELETED*/
		/*	COLOR_MODEL:		DELETED*/
		/*	BACKGROUND_COLOR_INDEX:	MOVED*/

/*--------------------------------------------------------------------*\
|	Matrix Manipulation Entities
\*--------------------------------------------------------------------*/
%token		IDENTITY3
%token		CONCAT_MATRIX3
%token		INVERT_MATRIX3
%token		ROTATE3
%token		ROTATE_XYZ3
%token		TRANSLATE3
%token		SCALE3
%token		MATRIX3
%token		GET_MATRIX3
%token		PUSH_MATRIX3
%token		POP_MATRIX3
%token		GLOBAL_TRANSFORMATION3
%token		LOCAL_TRANSFORMATION3
%token		APPLY_TO_GLOBAL3
%token		APPLY_TO_LOCAL3
%token		VIEW_ORIENTATION3
%token		VIEW_MAPPING3
		/*	DEFINE_VIEW_SPECIFICATION:	MOVED*/
		/*	DEFAULT_VIEW_SPECIFICATION:	MOVED*/
%token		ACTIVE_VIEW

/*--------------------------------------------------------------------*\
|	Structure Hierarchy
\*--------------------------------------------------------------------*/
%token		EXECUTE_STRUCTURE
%token		CALL_STRUCTURE

/*--------------------------------------------------------------------*\
|	Verb File Entities
\*--------------------------------------------------------------------*/
%token		READ_GEOMETRY_FILE
%token		CLEAR_GEOMETRY
%token		BEGIN_TEST
%token		END_TEST
%token		PAUSE
%token		SLEEP
%token		INVOKE_AT_FRAME
%token		DEFINE_COLOR			/* MOVED+DELTA */
%token		BACKGROUND_COLOR
%token		BACKGROUND_COLOR_INDEX		/*MOVED*/
%token		DEFINE_VIEW_SPECIFICATION	/*MOVED*/
%token		DEFAULT_VIEW_SPECIFICATION	/*MOVED*/
%token		DEFINE_LIGHT			/*MOVED+DELTA*/
%token		DEFINE_DEPTHCUE
%token		CONFIGURATION

/*--------------------------------------------------------------------*\
|	Additional Keywords
\*--------------------------------------------------------------------*/
%token		KNOTS				/* ver 1.0 */
%token		CTRL_POINTS			/* ver 1.0 */
%token		UKNOTS				/* ver 1.0 */
%token		VKNOTS				/* ver 1.0 */
%token		TRIMLOOP			/* ver 1.0 */
%token		TRIMCURVE			/* ver 1.0 */
%token		RATIONAL			/* ver 1.0 */
%token		NON_RATIONAL			/* ver 1.0 */
%token		IGNORE_GROUP
%token		VERTEX_COLORS
%token		VERTEX_NORMALS
%token		VERTEX_COLOR_INDICES
%token		FACET_COLORS
%token		FACET_NORMALS
%token		FACET_COLOR_INDICES
%token		VERTEX_COORDINATES
%token		EDGE_VISIBILITY
%token		ENABLE
%token		DISABLE
%token		STRING
%token		CHAR
%token		STROKE
%token		WORLD
%token		MODELLING
%token		AMBIENT_LIGHT
%token		DIRECTIONAL_LIGHT
%token		POSITIONAL_LIGHT
%token		SPOT_LIGHT
%token		LD_TRANSFORM
%token		ACTIVATE_LIST
%token		DEACTIVATE_LIST
		/*	HLHS_REMOVAL	*/
%token		HLHSRID
%token		HLHS_DISABLE
%token		HLHS_ENABLE
%token		MAKE_RAMP
%token		COLOR_LIST
%token		RGB
%token		CIE
%token		HSV
%token		HLS
%token		PRECONCAT
%token		POSTCONCAT
%token		REPLACE
%token		X_AXIS
%token		Y_AXIS
%token		Z_AXIS
%token		PERSPECTIVE
%token		PARALLEL
%token		MATCH_VIEW_AREA
%token		ADJUST_X
%token		ADJUST_Y
%token		GROW
%token		SHRINK
%token		XY_CLIP
%token		NO_XY_CLIP
%token		FRONT_CLIP
%token		NO_FRONT_CLIP
%token		BACK_CLIP
%token		NO_BACK_CLIP
%token		HOLLOW
%token		SOLID
%token		PATTERN
%token		EMPTY
%token		VIEW_MAPPING
%token		VIEW_ORIENTATION
%token		GLOBAL_MODELLING
%token		LOCAL_MODELLING
%token		COMPOSITE_MODELLING
%token		SPECIFY_REPORT_FILE
%token		TO
%token		END
%token		EXECUTE
%token		CALL
%token		WINDOW_SIZE
%token		DOUBLE_BUFFER
%token		SINGLE_BUFFER
%token		TRUE_COLOR
%token		PSEUDO_COLOR
%token		FACET_COLOR_INDICES
%token		FACET_CONNECTIVITY
%token		EXACT
%token		CENTER
%token		RADIUS
%token		SCALE_FACTORS
%token		NORMAL
%token		TEXT_DIRECTION
%token		ADD
%token		AND
%token		CLEAR
%token		INVERT
%token		NAND
%token		NOOP
%token		NOR
%token		OR
%token		SET
%token		SUBTRACT_DEST
%token		SUBTRACT_SOURCE
%token		XOR
%token		PIXEL_VALUES
%token		INTENSITY_VALUES
%token		PIXEL_VALUE_SEGMENTS
%token		INTENSITY_VALUE_SEGMENTS
%token		FIRST_FILE
%token		END_GEOM_FILE
%token	<str>	UNRECOGNIZED_KEYWORD


%type  <f>	real_num
%type  <l>	rational_non_rational		/* ver 1.0 */
%type  <l>	pre_post_replace		
%type  <l>	matrix_to_get
%type  <l>	axis_flag			
%type  <l>	perspective_flag
%type  <l>	adjust_flag			
%type  <l>	xy_clip_flag			
%type  <l>	front_clip_flag			
%type  <l>	back_clip_flag			
%type  <l>	interior_keyword
%type  <l>	exe_or_call
%type  <l>	pixel_map3_list_type
%type  <l>	replace_function
/*---------------*/
/* Grammar Rules */
/*---------------*/

%%

/* ------------------------------------------------------------------ */
bif_file                :	/* empty */
			|	FIRST_FILE configuration bif_verb_file
				{ close_reportfile(); }
			|	bif_verb_file
				{ close_reportfile(); }
			|       FIRST_FILE b_configuration bif_geometry_file
				{ close_reportfile(); }
			;

bif_verb_file           :       bif_verb_file_entity
			|       bif_verb_file   bif_verb_file_entity
			;

bif_geometry_file       :       bif_struct_entity
			|       bif_geometry_file bif_struct_entity
			;

bif_verb_file_entity    :       bif_geometry_entity
			|       bif_verb_entity
			|       bif_test_structure
			;

bif_struct_entity       :       struct_header struct_body struct_end
			;
/* ------------------------------------------------------------------ */
struct_header           :       BEGIN_STRUCTURE LONG ';'
				{ (void)bif_begstr($2); }
			;

struct_body             :       /* empty */
			|       struct_body bif_geometry_entity
			;

/* ------------------------------------------------------------------ */
struct_end              :       END_STRUCTURE ';'
				{ (void)bif_endstr(); }

bif_test_structure      :       test_header test_prefix 
				test_body test_end
			;
/* ------------------------------------------------------------------ */
test_header             :       BEGIN_TEST LONG ';'
				{ (void)bif_begintest($2); }
			;
test_prefix		:	/* empty */
			|	test_prefix  define_light
			|	test_prefix  define_view_specification
			;

test_body               :       /* empty */
			|       test_body  bif_geometry_entity
			|	test_body  invoke_at_frame	
			;

test_end                :       END_TEST ';'
				{ (void)bif_endtest(); }
			;
/* ------------------------------------------------------------------ */
bif_geometry_entity     : 	label
			|	marker
			|	marker3 	
			|	line
			|	line3 
			|	polygon 
			| 	polygon3 
			|	fill_area_set
			|	fill_area_set3
			|	triangle3
			|	quad_mesh3
			|	index_polygons3
			|	gen_sphere3
			|	gen_circle
			|	gen_circle3
			|	text
			|	text3
			|	annotation_text
			|	pixel_map3
			|	non_uniform_bspline_curve	 /* rev 1.0 */
			|	non_uniform_bspline_surface	 /* rev 1.0 */
			|	curve_approximation_criteria	 /* rev 1.0 */
			|	trimcurve_approximation_criteria /* rev 1.0 */
			|	surface_approximation_criteria	 /* rev 1.0 */
			|	marker_type
			|	marker_size
			|	marker_color
			|	marker_color_index
			|	line_type
			|	line_width
			|	line_color
			|	line_color_index
			|	line_shading
			|	interior_style
			|	interior_pattern_index
			|	interior_color
			|	interior_color_index
			|	backface_interior_color
			|	backface_interior_color_index
			|	interior_shading
			|	interior_lighting
			|	surface_properties
			|	backface_properties
			|	backface_processing
			|	edge_flag
			|	edge_type
			|	edge_width
			|	edge_color
			|	edge_color_index
			|	text_font
			|	text_prec
			|	text_color
			|	text_color_index
			|	text_path
			|	text_align
			|	char_height
			|	char_exp
			|	char_space
			|	char_up_vector
			|	anno_text_char_height
			|	anno_text_char_up_vector
			|	anno_text_style	
			|	light_state
			|	depthcue_index
			|	hlhs_removal
			|	identity3
			|	concat_matrix3
			|	invert_matrix3
			|	rotate3
			|	rotate_xyz3
			|	translate3
			|	scale3
			|	matrix3
			|	get_matrix3
			|	push_matrix3
			|	pop_matrix3
			|	global_transformation3
			|	local_transformation3
			|	apply_to_global3
			|	apply_to_local3
			|	view_orientation3
			|	view_mapping3
			|	active_view
			|	execute_structure
			|	call_structure
			|	unrecognized_entity
			;	

bif_verb_entity         :       read_geometry_file
			|       clear_geometry
			|	specify_report_file
			|       pause
			|       sleep
			|	define_color
			|	background_color
			|	background_color_index
			|	define_view_specification  
			|	default_view_specification
			|	define_depthcue	
			|	define_light
			;
/* ------------------------------------------------------------------ */
label                  	:       LABEL   /* rev 0.8 */
				LONG
				';'
				{ (void)bif_label($2); } /* rev 0.8 */
			;
/* ------------------------------------------------------------------ */
marker                  :       MARKER /* rev 0.8 */
				{ (void) bif_marker(BIF_P_BEGIN); } /*rev 0.8*/
				bif_pair ';'
				{ (void)bif_marker(BIF_P_END  ); }
			;
/* ------------------------------------------------------------------ */
marker3                 :       MARKER3
				{ (void)bif_marker3(BIF_P_BEGIN); }
				bif_triplet ';'
				{ (void)bif_marker3(BIF_P_END  ); }
			;

bif_pair               	:       real_num real_num  /* rev 0.8 */
				{
				 (void)bif_pair($1,$2);  /* rev 0.8 */
				}
			|       bif_pair real_num real_num 
				{(void)bif_pair($2,$3); }
			;

bif_triplet             :       real_num real_num real_num
				{
				 (void)bif_triplet($1,$2,$3); 
				}
			|       bif_triplet real_num real_num real_num
				{(void)bif_triplet($2,$3,$4);}
			;

bif_intlist             :       /* empty */
			|       bif_intlist LONG
				{ bif_intlist($2); }
			;
/* ------------------------------------------------------------------ */
line                    :       LINE /* rev 0.8 */
				{ (void)
				   bif_line(BIF_P_BEGIN); } /* rev 0.8 */
				bif_pair ';'
				{ (void)bif_line(BIF_P_END  ); }
			;
/* ------------------------------------------------------------------ */
line3                   :       LINE3
				{ 
				  bif_line3(BIF_P_BEGIN); 
				  not_repeated(-1); 
				  with_data_name = "LINE3";
				}
				vertex_coordinates line3_body ';'
				{ 
				  not_repeated(-1); 
				  bif_line3(BIF_P_END  ); 
				}
			;

line3_body	        :       /* empty */
			|	line3_body vertex_colors   
			|	line3_body vertex_color_indices
			;

/* ------------------------------------------------------------------ */
polygon                 :       POLYGON /* rev 0.8 */
				{ (void)
				   bif_polygon(BIF_P_BEGIN); } /* rev 0.8 */
				bif_pair ';'
				{ (void)bif_polygon(BIF_P_END  ); }

/* ------------------------------------------------------------------ */
polygon3              	:       POLYGON3
				{ 
				  bif_polygon3 (BIF_P_BEGIN); 
				  not_repeated(-1); 
				  with_data_name = "POLYGON3";
				} 
				vertex_coordinates with_data3_body ';'
				{ 
				  not_repeated(-1); 
				  (void)bif_polygon3 (BIF_P_END  ); 
				} 
			;

vertex_coordinates      :       { (void)bif_group(VERTEX_COORDINATES); }
				bif_triplet 
				{ (void)bif_group(BIF_END_OF_GROUP); }
			;

with_data3_body         :       /* empty */
			|	with_data3_body vertex_colors   
			|	with_data3_body vertex_color_indices
			|	with_data3_body vertex_normals
			|	with_data3_body facet_colors    
			|	with_data3_body facet_color_indices
			|	with_data3_body facet_normals
			;

vertex_colors		:       VERTEX_COLORS  
				{ 
				  if(not_repeated(1)) 
				  bif_group(VERTEX_COLORS); 
				  else 
				  { 
				     (void)bif_group(IGNORE_GROUP);  
				     WD3B_error("VERTEX_COLORS"); 
				  } 
				} 
				'{' bif_triplet '}' 
				{ (void)bif_group(BIF_END_OF_GROUP); } 
			; 

vertex_color_indices	:       VERTEX_COLOR_INDICES  
				{ 
				  if(not_repeated(2)) 
				  bif_group(VERTEX_COLOR_INDICES); 
				  else 
				  { 
				     (void)bif_group(IGNORE_GROUP);  
				     WD3B_error("VERTEX_COLOR_INDICES"); 
				  } 
				} 
				'{' bif_intlist '}' 
				{ (void)bif_group(BIF_END_OF_GROUP); } 
			; 


vertex_normals		:       VERTEX_NORMALS  
				{ 
				  if(not_repeated(3)) 
				  bif_group(VERTEX_NORMALS); 
				  else 
				  { 
				     (void)bif_group(IGNORE_GROUP);  
				     WD3B_error("VERTEX_NORMALS"); 
				  } 
				} 
				'{' bif_triplet '}' 
				{ (void)bif_group(BIF_END_OF_GROUP); } 
			; 


facet_colors		:       FACET_COLORS  
				{ 
				  if(not_repeated(4)) 
				  bif_group(FACET_COLORS); 
				  else 
				  { 
				     (void)bif_group(IGNORE_GROUP);  
				     WD3B_error("FACET_COLORS"); 
				  } 
				} 
				'{' bif_triplet '}' 
				{ (void)bif_group(BIF_END_OF_GROUP); } 
			; 


facet_color_indices	:       FACET_COLOR_INDICES  
				{ 
				  if(not_repeated(5)) 
				  bif_group(FACET_COLOR_INDICES); 
				  else 
				  { 
				     (void)bif_group(IGNORE_GROUP);  
				     WD3B_error("FACET_COLOR_INDICES"); 
				  } 
				} 
				'{' bif_intlist '}' 
				{ (void)bif_group(BIF_END_OF_GROUP); } 
			; 


facet_normals		:       FACET_NORMALS  
				{ 
				  if(not_repeated(6)) 
				  bif_group(FACET_NORMALS); 
				  else 
				  { 
				     (void)bif_group(IGNORE_GROUP);  
				     WD3B_error("FACET_NORMALS"); 
				  } 
				} 
				'{' bif_triplet '}' 
				{ (void)bif_group(BIF_END_OF_GROUP); } 
			; 

/* ------------------------------------------------------------------ */
				/* OLD: remove bif_faset3 if no other ref. */
fill_area_set		:       FILL_AREA_SET /* rev 0.8 */
				{ 
				  (void)bif_faset (BIF_P_BEGIN);  /* rev 0.8 */
				  (void)bif_group(VERTEX_COORDINATES); 
				}
				bif_pair_set
				';'
				{ 
				  (void)bif_group(BIF_END_OF_GROUP); 
				  (void)bif_faset (BIF_P_END  ); 
				}
			;

bif_intlist_set         :       '{' /* rev 0.8 */
				{ (void)bif_contour(BIF_P_BEGIN); }
			        bif_intlist
				'}'
				{ (void)bif_contour(BIF_P_END  ); }
			|	bif_intlist_set
				'{'
				{ (void)bif_contour(BIF_P_BEGIN); }
			        bif_intlist
				'}'
				{ (void)bif_contour(BIF_P_END  ); }
			;

bif_pair_set            :       '{'  /* rev 0.8 */
				{ (void)bif_contour(BIF_P_BEGIN); }
				bif_pair
				'}'
				{ (void)bif_contour(BIF_P_END  ); }
			|       bif_pair_set
				'{'
				{ (void)bif_contour(BIF_P_BEGIN); }
				bif_pair
				'}'
				{ (void)bif_contour(BIF_P_END  ); }
			;

bif_triplet_set         :       '{'
				{ (void)bif_contour(BIF_P_BEGIN); }
				bif_triplet
				'}'
				{ (void)bif_contour(BIF_P_END  ); }
			|       bif_triplet_set
				'{'
				{ (void)bif_contour(BIF_P_BEGIN); }
				bif_triplet
				'}'
				{ (void)bif_contour(BIF_P_END  ); }
			;
/* ------------------------------------------------------------------ */
fill_area_set3          :       FILL_AREA_SET3
				{ 
				  (void)bif_faset3(BIF_P_BEGIN); 
				  (void)bif_group(VERTEX_COORDINATES); 
				}
				bif_triplet_set 
				{ 
				   (void)bif_group(BIF_END_OF_GROUP); 
				   (void)not_repeated(-1); 
			 	}
				fasetdata3_body ';'
				{ 
				   (void)not_repeated(-1); 
				   (void)bif_faset3 (BIF_P_END  );
			 	}
			;

fasetdata3_body         :      /* empty */ 
			|	fasetdata3_body vertex_colors_set
			|	fasetdata3_body vertex_color_indices_set
			|	fasetdata3_body vertex_normals_set
			|	fasetdata3_body edge_visibility_set
			|	fasetdata3_body facet_colors_set
			|	fasetdata3_body facet_color_indices_set
			|	fasetdata3_body facet_normals_set
			;

vertex_colors_set       :       VERTEX_COLORS
				{ if(not_repeated(1))
				     (void)bif_group(VERTEX_COLORS); 
				  else
				  {
				     (void)bif_group(IGNORE_GROUP); /*rev 0.8*/
				     yyerror(FAS_ERM1);
				  }
				}
				'{' bif_triplet_set '}'
				{ (void)bif_group(BIF_END_OF_GROUP); }
			;

vertex_color_indices_set :       VERTEX_COLOR_INDICES
				{ if(not_repeated(2))
				     (void)bif_group(VERTEX_COLOR_INDICES);
				  else
				  {
				     (void)bif_group(IGNORE_GROUP); /*rev 0.8*/
				     yyerror(FAS_ERM2);
				  }
				}
				'{' bif_intlist_set '}'
				{ (void)bif_group(BIF_END_OF_GROUP); }
			;

vertex_normals_set      :       VERTEX_NORMALS
				{ if(not_repeated(3))
				     (void)bif_group(VERTEX_NORMALS);
				  else
				  {
				     (void)bif_group(IGNORE_GROUP); 
				     yyerror(FAS_ERM3);
				  }
				}
				'{' bif_triplet_set '}'
				{ (void)bif_group(BIF_END_OF_GROUP); }
			;

edge_visibility_set     :       EDGE_VISIBILITY
				{ if(not_repeated(4))
				     (void)bif_group(EDGE_VISIBILITY);
				  else
				  {
				     (void)bif_group(IGNORE_GROUP); 
				     yyerror(FAS_ERM4);
				  }
				}
				'{' bif_intlist_set '}'
				{ (void)bif_group(BIF_END_OF_GROUP); }
			;

facet_colors_set        :       FACET_COLORS
				{ if(not_repeated(5))
				     (void)bif_group(FACET_COLORS);
				  else
				  {
				     (void)bif_group(IGNORE_GROUP); 
				     yyerror(FAS_ERM5);
				  }
				}
				'{' bif_triplet '}'
				{ (void)bif_group(BIF_END_OF_GROUP); }
			;

facet_color_indices_set :       FACET_COLOR_INDICES
				{ if(not_repeated(6))
				     (void)bif_group(FACET_COLOR_INDICES);
				  else
				  {
				     (void)bif_group(IGNORE_GROUP); 
				     yyerror(FAS_ERM6);
				  }
				}
				'{' bif_intlist '}'
				{ (void)bif_group(BIF_END_OF_GROUP); }
			;

facet_normals_set       :       FACET_NORMALS
				{ if(not_repeated(7))
				     (void)bif_group(FACET_NORMALS);
				  else
				  {
				     (void)bif_group(IGNORE_GROUP); 
				     yyerror(FAS_ERM7);
				  }
				}
				'{' bif_triplet '}'
				{ (void)bif_group(BIF_END_OF_GROUP); }
			;

/* ------------------------------------------------------------------ */
triangle3 		:       TRIANGLE3 /* rev 0.8 */
				{
				  bif_triangle3 (BIF_P_BEGIN);/* rev 0.8 */
				  not_repeated(-1); 
				  with_data_name = "TRIANGLE3";
				} 
				vertex_coordinates with_data3_body ';'
				{
				  not_repeated(-1); 
				  (void)bif_triangle3 (BIF_P_END  ); 
				}
			;
/* ------------------------------------------------------------------ */
quad_mesh3		:	QUAD_MESH3 /* rev 0.8 */
				LONG LONG
				{
				  (void)bif_quad_mesh3 (BIF_P_BEGIN);/*rev 0.8*/
				  (void)bif_quadmeshorder($2,$3);    /*rev 0.8*/
				  not_repeated(-1); 
				  with_data_name = "QUAD_MESH3";
				}
				vertex_coordinates with_data3_body ';'
				{
				  not_repeated(-1); 
				  (void)bif_quad_mesh3 (BIF_P_END  ); 
				}
			;
/* ------------------------------------------------------------------ */
index_polygons3		:	INDEX_POLYGONS3 /* rev 0.8 */
				{
				  bif_indexpoly3 (BIF_P_BEGIN); /* rev 0.8 */
				  not_repeated(-1); 
				  with_data_name = "INDEX_POLYGON3";
				}
				vertex_coordinates 
				facet_connectivity
				index_polygons3_body ';'
				{
				  not_repeated(-1); 
				  (void)bif_indexpoly3 (BIF_P_END  ); 
				}
			;

index_polygons3_body    :       /* empty */
			|	index_polygons3_body edge_visibility   
			|	index_polygons3_body vertex_colors   
			|	index_polygons3_body vertex_color_indices
			|	index_polygons3_body vertex_normals
			|	index_polygons3_body facet_colors    
			|	index_polygons3_body facet_color_indices
			|	index_polygons3_body facet_normals
			;


edge_visibility		:	EDGE_VISIBILITY
				{ 
				  if(not_repeated(7)) 
				  bif_group(EDGE_VISIBILITY); 
				  else 
				  { 
				     (void)bif_group(IGNORE_GROUP);  
				     WD3B_error("EDGE_VISIBILITY"); 
				  } 
				} 
				'{' bif_intlist_set '}' 
				{ (void)bif_group(BIF_END_OF_GROUP); } 
			; 

			;

facet_connectivity	:	FACET_CONNECTIVITY /* rev 0.8 */
				{(void)bif_group(FACET_CONNECTIVITY);}
								/*rev 0.8*/
				'{' bif_intlist_set '}'
				{(void)bif_group(BIF_END_OF_GROUP); }
			;
/* ------------------------------------------------------------------ */
gen_sphere3		:	GEN_SPHERE3 /* rev 0.8 */
				LONG LONG
				{
				   (void)bif_sphere3(BIF_P_BEGIN); /* rev 0.8 */
				   (void)bif_sphereorder($2,$3);
				   (void)not_repeated(-1); 
				}
				gen_sphere3_body
				';'
				{
				   (void)not_repeated(-1); 
				   (void)bif_sphere3(BIF_P_END); 
				}
			;

gen_sphere3_body	:	/* empty */
			|	gen_sphere3_body EXACT
				{ 
				  bif_exact(TRUE);
				}
			|	gen_sphere3_body CENTER 
				'{' real_num real_num real_num '}'
				{ 
				  if(not_repeated(1)) 
				     bif_center($4,$5,$6); /* rev 0.8 */
				  else yyerror(GS_ERM1);
				}
			|	gen_sphere3_body RADIUS 
				'{' real_num  '}'
				{ 
				  if(not_repeated(2)) 
				     bif_radius($4); /* rev 0.8 */
				  else yyerror(GS_ERM2);
				}
			|	gen_sphere3_body SCALE_FACTORS 
				'{' real_num real_num real_num '}'
				{ 
				  if(not_repeated(3)) 
				     bif_scalefactors($4,$5,$6); /* rev 0.8 */
				  else yyerror(GS_ERM3);
				}
			;
/* ------------------------------------------------------------------ */
gen_circle		:	GEN_CIRCLE /* rev 0.8: */
				LONG
				{
				   (void)bif_gencircle(BIF_P_BEGIN); /*rev 0.8*/
				   (void)bif_circleedges($2); /* rev 0.8 */
				   (void)not_repeated(-1); 
				}
				gen_circle_body
				';'
				{
				   (void)not_repeated(-1); 
				   (void)bif_gencircle(BIF_P_END);
				}
			;

gen_circle_body	:		/* empty */
			|	gen_circle_body EXACT
				{ 
				  bif_exact(TRUE);
				}
			|	gen_circle_body CENTER 
				'{' real_num real_num '}'
				{ 
				  if(not_repeated(1)) 
				     bif_center($4,$5,0.0); /* rev 0.8 */
				  else yyerror(GC_ERM1);
				}
			|	gen_circle_body RADIUS 
				'{' real_num '}'
				{ 
				  if(not_repeated(2)) 
				     bif_radius($4); /* rev 0.8 */
				  else yyerror(GC_ERM2);
				}
			;
/* ------------------------------------------------------------------ */
gen_circle3		:	GEN_CIRCLE3 /* rev 0.8: */
				LONG
				{
				   (void)bif_gencircle3(BIF_P_BEGIN);/*rev 0.8*/
				   (void)bif_circleedges($2); /* rev 0.8 +3 */
				   (void)not_repeated(-1); 
				}
				gen_circle3_body
				';'
				{
				   (void)not_repeated(-1); 
				   (void)bif_gencircle3(BIF_P_END);
				}
			;

gen_circle3_body	:	/* empty */
			|	gen_circle3_body EXACT
				{ 
				  bif_exact(TRUE);
				}
			|	gen_circle3_body CENTER 
				'{' real_num real_num real_num '}'
				{ 
				  if(not_repeated(1)) 
				     bif_center($4,$5,$6); /* rev 0.8 */
				  else yyerror(GC3_ERM1);
				}
			|	gen_circle3_body RADIUS 
				'{' real_num  '}'
				{ 
				  if(not_repeated(2)) 
				     bif_radius($4); /* rev 0.8 */
				  else yyerror(GC3_ERM2);
				}
			|	gen_circle3_body NORMAL 
				'{' real_num real_num real_num  '}'
				{ 
				  if(not_repeated(3)) 
				     bif_normal($4,$5,$6); /* rev 0.8 */
				  else yyerror(GC3_ERM3);
				}
			;
/* ------------------------------------------------------------------ */
text			:	TEXT /* rev 0.8 */
				real_num real_num
				QSTRING
				';'
				{bif_text($2,$3,$4);} /* rev 0.8 */
			;
/* ------------------------------------------------------------------ */
text3                   :       TEXT3
				real_num real_num real_num
				QSTRING
				';'
				{ (void)bif_text3($2,$3,$4,$5, /*rev 0.8 delta*/
					FALSE,0.,0.,0.,0.,0.,0.); }  /*rev 0.8 delta*/
			|	TEXT3
				real_num real_num real_num
				QSTRING
				TEXT_DIRECTION
				'{'
				real_num real_num real_num
				real_num real_num real_num
				'}'
				';'
				{ (void)bif_text3($2,$3,$4,$5, /*rev 0.8 delta*/
				TRUE,$8,$9,$10,$11,$12,$13);}  /*rev 0.8 delta*/
			;
/* ------------------------------------------------------------------ */
annotation_text		:	ANNOTATION_TEXT3
				real_num real_num real_num
				real_num real_num real_num
				QSTRING
				';'
				{bif_annotationtext($2,$3,$4, /* rev 0.8 */
				$5,$6,$7,$8);}
			;
/* ------------------------------------------------------------------ */
/*
#ifdef EXTERNAL_NOTE
	The pixel_map3 entry has been modified to used the generated
	entities own "final" storage for the pixel map.
	bif_pixelmap3(BEGIN,..) initializes the pixel_map space, and
	lets bif_fillpixelbuffer know where to store the pixel values.
#endif 
*/
pixel_map3		:	PIXEL_MAP3 /* rev 0.8 */
				real_num real_num real_num
				LONG LONG
				replace_function
				pixel_map3_list_type
				{
				   bif_pixelmap3(BIF_P_BEGIN,
				   		$2,$3,$4, $5,$6,$7,$8);
				}
				'{' pixel_list '}'
				pixel_pseudo_color
				';'
				{
				  bif_pixelmap3(BIF_P_END,
						0,0,0,0,0,0,0 );
				}
			;

replace_function	:	/* empty */ { $$ = REPLACE ; } /* rev 0.8 */
			|	ADD	{$$=ADD;}	/* rev 0.8 */
			|	AND	{$$=AND;}	/* rev 0.8 */
			|	CLEAR	{$$=CLEAR;}	/* rev 0.8 */
			|	INVERT	{$$=INVERT;}	/* rev 0.8 */
			|	NAND	{$$=NAND;}	/* rev 0.8 */
			|	NOOP	{$$=NOOP;}	/* rev 0.8 */
			|	NOR	{$$=NOR;}	/* rev 0.8 */
			|	OR	{$$=OR;}	/* rev 0.8 */
			|	REPLACE	{$$=REPLACE;}	/* rev 0.8 */
			|	SET	{$$=SET;}	/* rev 0.8 */
			|	SUBTRACT_DEST   {$$=SUBTRACT_DEST;}  /*rev 0.8*/
			|	SUBTRACT_SOURCE	{$$=SUBTRACT_SOURCE;}/*rev 0.8*/
			|	XOR	{$$=XOR;}	/* rev 0.8 */
			;

pixel_map3_list_type	:	PIXEL_VALUES
				{ $$ = PIXEL_VALUES ;             } /* rev 0.8*/
			|	INTENSITY_VALUES
				{ $$ = INTENSITY_VALUES ;         } /* rev 0.8*/
			|	PIXEL_VALUE_SEGMENTS
				{ $$ = PIXEL_VALUE_SEGMENTS ;     } /* rev 0.8*/
			|	INTENSITY_VALUE_SEGMENTS 
				{ $$ = INTENSITY_VALUE_SEGMENTS ; } /* rev 0.8*/
			;

pixel_list              :       /* empty */
			|       pixel_list LONG
				{ bif_fillpixelbuffer($2); }
			;

pixel_pseudo_color	:	/* empty */
			|	PSEUDO_COLOR
				{ bif_group(PSEUDO_COLOR); }
				'{' bif_intlist '}'
				{ bif_group(BIF_END_OF_GROUP); }
			;
/*--------------------------------------------------------------------*\
|	 NON_UNIFORM_BSPLINE_CURVE : rev 1.0
\*--------------------------------------------------------------------*/
non_uniform_bspline_curve       :       NON_UNIFORM_BSPLINE_CURVE       
					rational_non_rational
					LONG
					{ 
				  	   (void)bif_nubc(BIF_P_BEGIN);
						/*rev1.0*/ 
					   (void)bif_nubcholder($2,$3);
						/*rev1.0*/
				  	   (void)bif_group(CTRL_POINTS); 
						/* rev 1.0 CTRL_POINTS*/
					}
					bif_real_list
					{ 
				   	   (void)bif_group(BIF_END_OF_GROUP); 
			 		}
					knots
					{ 
				   	   (void)bif_nubc (BIF_P_END);
							/*rev 1.0*/
			 		}
					';'
				;

knots				:	/*empty*/
				|
					KNOTS
					{ 
				  	   (void)bif_group(KNOTS); 
						     /* rev 1.0 KNOTS*/
			 		}
					'{' bif_real_list '}'
					
					{ 
				   	   (void)bif_group(BIF_END_OF_GROUP); 
			 		}
				;

rational_non_rational		:	RATIONAL	{ $$ = RATIONAL; }
				|	NON_RATIONAL	{ $$ = NON_RATIONAL; }
				;

bif_real_list             	:       /* empty */
				|       bif_real_list real_num 
					{ bif_real_list($2); } 
					   /*  rev 1.0 collector */
				;

/*--------------------------------------------------------------------*\
|	 NON_UNIFORM_BSPLINE_SURFACE rev 1.0
\*--------------------------------------------------------------------*/
non_uniform_bspline_surface    :       NON_UNIFORM_BSPLINE_SURFACE       
					rational_non_rational
					LONG
					LONG
					LONG LONG
					{ 
				  	   (void)bif_nubs(BIF_P_BEGIN); 
				  	   not_repeated(-1); 
				  	   with_data_name = 
					        "NON_UNIFORM_BSPLINE_SURFACE";
					   (void)bif_nubsholder($2,$3,
								$4,$5,$6); 
				  	   (void)bif_group(CTRL_POINTS); 
					}
					bif_real_list
					{ 
				   	   (void)bif_group(BIF_END_OF_GROUP); 
			 		}
					bspline_surf_body
					{ 
				   	   (void)bif_nubs (BIF_P_END  );
							/* rev 1.0*/
			 		}
					';'
				;       

bspline_surf_body		:	/* empty */
				|	bspline_surf_body uknots   
				|	bspline_surf_body vknots
				|	bspline_surf_body trimloop
				;

trimloop			:	TRIMLOOP
					{
				  	   (void)bif_group(TRIMLOOP); 
				   	   (void)bif_group(BIF_END_OF_GROUP); 
					}
					'{' 
					trimcurve_list
					'}' 
				;

trimcurve_list          	:       /* empty */
				|       trimcurve_list trimcurve
				;

trimcurve			:       TRIMCURVE       
					'{' 
					rational_non_rational
					LONG
					{ 
				  	   (void)bif_group(TRIMCURVE); 
					   bif_intlist($3);
					   bif_intlist($4);
				   	   (void)bif_group(BIF_END_OF_GROUP); 
				  	   (void)bif_group(CTRL_POINTS); 
					}
					bif_real_list
					{ 
				   	   (void)bif_group(BIF_END_OF_GROUP); 
			 		}
					knots
					'}' 
				;

uknots				:	UKNOTS
					{ 
					  if(not_repeated(1)) 
					  bif_group(UKNOTS); 
					      /* rev 1.0 UKNOTS*/
					  else 
					  { 
					     (void)bif_group(IGNORE_GROUP);  
					     WD3B_error("VERTEX_COLORS"); 
					  } 
					} 
					'{' bif_real_list '}'
					{ 
				   	   (void)bif_group(BIF_END_OF_GROUP); 
			 		}
				;

vknots				:	VKNOTS
					{ 
					  if(not_repeated(2)) 
					  bif_group(VKNOTS); 
					      /* rev 1.0 VKNOTS*/
					  else 
					  { 
					     (void)bif_group(IGNORE_GROUP);  
					     WD3B_error("VERTEX_COLORS"); 
					  } 
					} 
					'{' bif_real_list '}'
					{ 
				   	   (void)bif_group(BIF_END_OF_GROUP); 
			 		}
				;

/*--------------------------------------------------------------------*\
|	 	CURVE_APPROXIMATION_CRITERIA rev 1.0
\*--------------------------------------------------------------------*/
curve_approximation_criteria	:	CURVE_APPROXIMATION_CRITERIA
					LONG
					real_num 
					';'
					{
					   bif_curve_approx_criteria ($2,$3); 
					}/*  rev 1.0 */
				;

/*--------------------------------------------------------------------*\
|	 	TRIMCURVE_APPROXIMATION_CRITERIA rev 1.0
\*--------------------------------------------------------------------*/
trimcurve_approximation_criteria :	TRIMCURVE_APPROXIMATION_CRITERIA
					LONG
					real_num 
					';'
					{
					   bif_trimcurve_approx_criteria ($2,$3); 
					}/*  rev 1.0 */
				;

/*--------------------------------------------------------------------*\
|	 	SURFACE_APPROXIMATION_CRITERIA rev 1.0
\*--------------------------------------------------------------------*/
surface_approximation_criteria	:	SURFACE_APPROXIMATION_CRITERIA
					LONG
					real_num 
					real_num 
					';'
					{
					   bif_surface_approx_criteria ($2,$3,$4); 
					}/*  rev 1.0 */
				;
/* ------------------------------------------------------------------ */

/* ------------------------------------------------------------------ */
marker_type             :       MARKER_TYPE
				LONG ';'
				{ (void)bif_mktype($2); }
			;
/* ------------------------------------------------------------------ */
marker_size             :       MARKER_SIZE
				real_num ';'
				{ (void)bif_mkscale($2); }
			;
/* ------------------------------------------------------------------ */
marker_color			:	MARKER_COLOR /* rev 0.8 */
					real_num real_num real_num ';'
					{ (void)
					  bif_mkcolor($2,$3,$4); }/* rev 0.8 */
				;
/* ------------------------------------------------------------------ */
marker_color_index              :       MARKER_COLOR_INDEX
					LONG ';'
					{ (void)bif_mkcolorindex($2); }
				;
/* ------------------------------------------------------------------ */
line_type                       :       LINE_TYPE
					LONG ';'
					{ (void)bif_linetype($2); }
				;
/* ------------------------------------------------------------------ */
line_width                      :       LINE_WIDTH
					real_num ';'
					{ (void)bif_linewidth($2); }
				;
/* ------------------------------------------------------------------ */
line_color			:	LINE_COLOR
					real_num real_num real_num ';'
					{ (void)
					  bif_linecolor($2,$3,$4); }/*rev 0.8*/
				;
/* ------------------------------------------------------------------ */
line_color_index                :       LINE_COLOR_INDEX
					LONG ';'
					{ (void)bif_linecolorindex($2); }
				;
/* ------------------------------------------------------------------ */
line_shading			:	LINE_SHADING /* rev 0.8 */
					LONG ';'
					{ (void)
					  bif_lineshading($2); } /* rev 0.8 */
				;
/* ------------------------------------------------------------------ */
interior_style            	:       INTERIOR_STYLE
					interior_keyword ';'
					{ (void)
					   bif_intstyle($2); } /* CHANGED */
				;

interior_keyword		:	HOLLOW  { $$ = HOLLOW ; } /* rev 0.8 */
				|	SOLID   { $$ = SOLID  ; } /* rev 0.8 */
				|	PATTERN { $$ = PATTERN; } /* rev 0.8 */
				|	EMPTY   { $$ = EMPTY  ; } /* rev 0.8 */
				;
/* ------------------------------------------------------------------ */
interior_pattern_index	:	INTERIOR_PATTERN_INDEX /* rev 0.8 */
				LONG ';'
				{ 
				  (void)bif_intpatternindex($2);/* rev 0.8 */
				}
			;
/* ------------------------------------------------------------------ */
interior_color		:	INTERIOR_COLOR 	/* rev 0.8 */
				real_num real_num real_num ';'
				{ 
				  (void)bif_intcolor($2,$3,$4);/* rev 0.8 */
				}
			;
/* ------------------------------------------------------------------ */
interior_color_index            :       INTERIOR_COLOR_INDEX
					LONG ';'
					{ (void)bif_intcolorindex($2); }
				;
/* ------------------------------------------------------------------ */
backface_interior_color		:	BACKFACE_INTERIOR_COLOR /* rev 0.8 */
					real_num real_num real_num ';'
					{ (void)
					   bif_bkfintcolor($2,$3,$4); }
					   /* rev 0.8 */
				;
/* ------------------------------------------------------------------ */
backface_interior_color_index	:	BACKFACE_INTERIOR_COLOR_INDEX /* rev 0.8 */
					LONG ';'
					{ (void)
					   bif_bkfintcolorindex($2); }
					   /* rev 0.8 */
				;
/* ------------------------------------------------------------------ */
interior_shading                :       INTERIOR_SHADING
					LONG ';'
					{ (void)bif_intshade($2); }
				;
/* ------------------------------------------------------------------ */
interior_lighting               :       INTERIOR_LIGHTING
					LONG ';'
					{ (void)bif_intlight($2); }
				;
/* ------------------------------------------------------------------ */
surface_properties              :       SURFACE_PROPERTIES
					real_num real_num real_num
					real_num real_num real_num
					real_num real_num
					';'
					{ (void)bif_surfprop($2,$3,
					  $4,$5,$6,$7,
					  $8,$9); }
				;
/* ------------------------------------------------------------------ */
backface_properties		:	BACKFACE_PROPERTIES /* rev 0.8 */
					real_num real_num real_num
					real_num real_num real_num
					real_num real_num
					';'
					{ (void)bif_bkfprop($2,$3, /* rev 0.8 */
					  $4,$5,$6,$7,
					  $8,$9); }
				;
/* ------------------------------------------------------------------ */
backface_processing		:	BACKFACE_PROCESSING /* rev 0.8 */
					LONG LONG ';'
					{ (void)
					   bif_bkfprocessing($2,$3); }
					   /* rev 0.8 */
				;
/* ------------------------------------------------------------------ */
edge_flag                       :       EDGE_FLAG
					ENABLE ';'
					{ (void)bif_edgeflag(ENABLE ); }
				|	EDGE_FLAG
					DISABLE ';'
					{ (void)bif_edgeflag(DISABLE ); }
				;
/* ------------------------------------------------------------------ */
edge_type			:	EDGE_TYPE /* rev 0.8 */
					LONG ';'
					{ (void)bif_edgetype($2);} /*rev 0.8*/
				;
/* ------------------------------------------------------------------ */
edge_width                      :       EDGE_WIDTH
					real_num ';'
					{ (void)bif_edgewidth($2); }
				;
/* ------------------------------------------------------------------ */
edge_color		:	EDGE_COLOR /* rev 0.8 */
				real_num real_num real_num ';'
				{ 
				  (void)bif_edgecolor($2,$3,$4);/* rev 0.8 */
				} 
			;
/* ------------------------------------------------------------------ */
edge_color_index	:	EDGE_COLOR_INDEX /* rev 0.8 */
				LONG ';'
				{ 
				  (void)bif_edgecolorindex($2);/* rev 0.8 */
				} 
			;
/* ------------------------------------------------------------------ */
text_font                       :       TEXT_FONT
					LONG ';'
					{ (void)bif_textfont($2); }
				;
/* ------------------------------------------------------------------ */
text_prec                       :       TEXT_PREC
					STRING ';'
					{ (void)bif_textprec(STRING ); }
				|	TEXT_PREC
					CHAR ';'
					{ (void)bif_textprec(CHAR ); }
				|	TEXT_PREC
					STROKE ';'
					{ (void)bif_textprec(STROKE ); }
				;
/* ------------------------------------------------------------------ */
text_color			:	TEXT_COLOR /* rev 0.8 */
					real_num real_num real_num ';'
					{ (void)
					   bif_textcolor($2,$3,$4); } 
							/* rev 0.8 */
				;
/* ------------------------------------------------------------------ */
text_color_index                :       TEXT_COLOR_INDEX
					LONG ';'
					{ (void)bif_textcolorindex($2); }
				;
/* ------------------------------------------------------------------ */
text_path                       :       TEXT_PATH
					LONG ';'
					{ (void)bif_textpath($2); }
				;

/* ------------------------------------------------------------------ */
text_align                      :       TEXT_ALIGN
					LONG LONG ';'
					{ (void)bif_textalign($2, $3); }
				;

/* ------------------------------------------------------------------ */
char_height                     :       CHAR_HEIGHT
					real_num ';'
					{ (void)bif_charheight($2); }
				;
/* ------------------------------------------------------------------ */
char_exp			:	CHAR_EXP /* rev 0.8 */
					real_num ';'
					{ (void)bif_charexp($2); } /* rev 0.8 */
				;
/* ------------------------------------------------------------------ */
char_space			:	CHAR_SPACE /* rev 0.8 */
					real_num ';'
					{ (void)bif_charspace($2); } 
								/* rev 0.8 */
				;
					
/* ------------------------------------------------------------------ */
char_up_vector                  :       CHAR_UP_VECTOR
					real_num real_num ';'
					{ (void)bif_charupvector($2, $3); }
				;
/* ------------------------------------------------------------------ */
anno_text_char_height		:	ANNO_TEXT_CHAR_HEIGHT	
					real_num ';'
					{ (void)
				   bif_annotextcharheight($2); } /* rev 0.8 */
				;
/* ------------------------------------------------------------------ */
anno_text_char_up_vector	:	ANNO_TEXT_CHAR_UP_VECTOR
					real_num real_num ';'
					{ (void)
				   bif_annotextcharupvector($2,$3); }/*rev 0.8*/
				;
/* ------------------------------------------------------------------ */
anno_text_style			:	ANNO_TEXT_STYLE /* rev 0.8 */
					LONG ';'
					{ (void)
				   	   bif_annotextstyle($2); } /*rev 0.8*/
				;
/* ------------------------------------------------------------------ */
				/* NOTE: This section requires an order
					to the lists this restriction may
					be removed in later releases  */

light_state                     :       LIGHT_STATE
					{(void)bif_lightstate(BIF_P_BEGIN);}
					ACTIVATE_LIST
					{(void)bif_group(ACTIVATE_LIST);}
					'{' bif_intlist '}'
					{(void)bif_group(BIF_END_OF_GROUP);}
					DEACTIVATE_LIST
					{(void)bif_group(DEACTIVATE_LIST);}
					'{' bif_intlist '}'
					{(void)bif_group(BIF_END_OF_GROUP);}
					';'
					{(void)bif_lightstate(BIF_P_END  );}
				;
/* ------------------------------------------------------------------ */
depthcue_index			:	DEPTHCUE_INDEX /* rev 0.8 */
					LONG ';'
					{ (void)
					   bif_depthcueindex($2); } /*rev 0.8*/
				;
/* ------------------------------------------------------------------ */
hlhs_removal                    :       HLHS_REMOVAL
					HLHS_DISABLE
					';'
					{(void)bif_hlhsremoval(HLHS_DISABLE);}
				|       HLHS_REMOVAL
					HLHS_ENABLE
					';'
					{(void)bif_hlhsremoval(HLHS_ENABLE);}
				;

/* ------------------------------------------------------------------ */
identity3                       :       IDENTITY3
					LONG
					';'
					{(void)bif_identity3($2);}
				;
/* ------------------------------------------------------------------ */
concat_matrix3                  :       CONCAT_MATRIX3
					LONG LONG pre_post_replace
					';'
					{(void)bif_concatmatrix3($2,$3,$4);}
				;

pre_post_replace		:	PRECONCAT	{ $$ = PRECONCAT; }
				|	POSTCONCAT	{ $$ = POSTCONCAT; }
				|	REPLACE		{ $$ = REPLACE; }
				;
/* ------------------------------------------------------------------ */
invert_matrix3                  :       INVERT_MATRIX3
					LONG
					';'
					{(void)bif_invertmatrix3($2);}
				;
/* ------------------------------------------------------------------ */
rotate3                         :       ROTATE3
					LONG
					real_num 
					axis_flag 
					pre_post_replace
					';'
					{(void)bif_rotate3($2,$3,$4,$5);}
				;

axis_flag			:	X_AXIS	{ $$ = X_AXIS; }
				|	Y_AXIS	{ $$ = Y_AXIS; }
				|	Z_AXIS	{ $$ = Z_AXIS; }
				;
/* ------------------------------------------------------------------ */
rotate_xyz3                     :       ROTATE_XYZ3
					LONG 
					real_num real_num real_num 
					pre_post_replace
					';'
					{(void)bif_rotatexyz3($2,$3,$4,$5,$6);}
				;
/* ------------------------------------------------------------------ */
translate3                      :       TRANSLATE3
					LONG 
					real_num real_num real_num 
					pre_post_replace
					';'
					{(void)bif_translate3($2,$3,$4,$5,$6);}
				;
/* ------------------------------------------------------------------ */
scale3                          :       SCALE3
					LONG 
					real_num real_num real_num 
					pre_post_replace
					';'
					{(void)bif_scale3($2,$3,$4,$5,$6);}
				;
/* ------------------------------------------------------------------ */
matrix3                         :       MATRIX3
					LONG
					'{' 
					real_num real_num real_num real_num 
					'}'
					'{' 
					real_num real_num real_num real_num
					'}'
					'{' 
					real_num real_num real_num real_num 
					'}'
					'{' 
					real_num real_num real_num real_num 
					'}'
					pre_post_replace
					';'
					{
					 double amatrix[4][4];
					 amatrix[0][0] = $4;
					 amatrix[0][1] = $5;
					 amatrix[0][2] = $6;
					 amatrix[0][3] = $7;

					 amatrix[1][0] = $10;
					 amatrix[1][1] = $11;
					 amatrix[1][2] = $12;
					 amatrix[1][3] = $13;

					 amatrix[2][0] = $16;
					 amatrix[2][1] = $17;
					 amatrix[2][2] = $18;
					 amatrix[2][3] = $19;

					 amatrix[3][0] = $22;
					 amatrix[3][1] = $23;
					 amatrix[3][2] = $24;
					 amatrix[3][3] = $25;
					 (void)bif_matrix3($2,amatrix,$27);
					}
				;

/* ------------------------------------------------------------------ */
get_matrix3		:	GET_MATRIX3 /* rev 0.8 */
				LONG 
				matrix_to_get
				pre_post_replace ';'
				{ 
				  (void)bif_getmatrix3($2,$3,$4);/* rev 0.8 */
				} 
			;

matrix_to_get		:	VIEW_MAPPING  /* rev 0.8 */
				   { $$ = VIEW_MAPPING; }
			|	VIEW_ORIENTATION   /* rev 0.8 */
				   { $$ = POSTCONCAT; }
			|	GLOBAL_MODELLING	  /* rev 0.8 */
				   { $$ = GLOBAL_MODELLING; }
			|	LOCAL_MODELLING  /* rev 0.8 */
				   { $$ = LOCAL_MODELLING; }
			|	COMPOSITE_MODELLING  /* rev 0.8 */
				   { $$ = COMPOSITE_MODELLING; }
			;
/* ------------------------------------------------------------------ */
push_matrix3			:	PUSH_MATRIX3 ';'
					{(void)bif_pushmatrix3(); } /*rev 0.8*/
				;
/* ------------------------------------------------------------------ */
pop_matrix3			:	POP_MATRIX3 ';'
					{(void)bif_popmatrix3(); } /* rev 0.8 */
				;
/* ------------------------------------------------------------------ */
global_transformation3          :       GLOBAL_TRANSFORMATION3
					'{' 
					real_num real_num real_num real_num 
					'}'
					'{' 
					real_num real_num real_num real_num 
					'}'
					'{' 
					real_num real_num real_num real_num 
					'}'
					'{' 
					real_num real_num real_num real_num 
					'}'
					';'
					{
					 double amatrix[4][4];
					 amatrix[0][0] = $3;
					 amatrix[0][1] = $4;
					 amatrix[0][2] = $5;
					 amatrix[0][3] = $6;

					 amatrix[1][0] = $9;
					 amatrix[1][1] = $10;
					 amatrix[1][2] = $11;
					 amatrix[1][3] = $12;

					 amatrix[2][0] = $15;
					 amatrix[2][1] = $16;
					 amatrix[2][2] = $17;
					 amatrix[2][3] = $18;

					 amatrix[3][0] = $21;
					 amatrix[3][1] = $22;
					 amatrix[3][2] = $23;
					 amatrix[3][3] = $24;

					 (void)bif_gtransform3(amatrix);
					}
				;

/* ------------------------------------------------------------------ */
local_transformation3           :       LOCAL_TRANSFORMATION3
					'{' 
					real_num real_num real_num real_num 
					'}'
					'{' 
					real_num real_num real_num real_num 
					'}'
					'{' 
					real_num real_num real_num real_num 
					'}'
					'{' 
					real_num real_num real_num real_num 
					'}'
					pre_post_replace
					';'
					{
					 double amatrix[4][4];
					 amatrix[0][0] = $3;
					 amatrix[0][1] = $4;
					 amatrix[0][2] = $5;
					 amatrix[0][3] = $6;

					 amatrix[1][0] = $9;
					 amatrix[1][1] = $10;
					 amatrix[1][2] = $11;
					 amatrix[1][3] = $12;

					 amatrix[2][0] = $15;
					 amatrix[2][1] = $16;
					 amatrix[2][2] = $17;
					 amatrix[2][3] = $18;

					 amatrix[3][0] = $21;
					 amatrix[3][1] = $22;
					 amatrix[3][2] = $23;
					 amatrix[3][3] = $24;

					 (void)bif_ltransform3(amatrix,$26);
					}
				;


/* ------------------------------------------------------------------ */
apply_to_global3                :       APPLY_TO_GLOBAL3
					LONG
					';'
					{(void)bif_applytoglobal3($2);}
				;

/* ------------------------------------------------------------------ */
apply_to_local3                 :       APPLY_TO_LOCAL3
					LONG pre_post_replace
					';'
					{(void)bif_applytolocal3($2,$3);}
				;

/* ------------------------------------------------------------------ */
view_orientation3               :       VIEW_ORIENTATION3
					LONG 
					real_num real_num real_num
					real_num real_num real_num
					real_num real_num real_num
					';'
					{
					 double vrp[3],vpn[3],vup[3];
					 vrp[0] = $3;
					 vrp[1] = $4;
					 vrp[2] = $5;

					 vpn[0] = $6;
					 vpn[1] = $7;
					 vpn[2] = $8;

					 vup[0] = $9;
					 vup[1] = $10;
					 vup[2] = $11;

					 (void)bif_vieworient3($2,vrp,vpn,vup);
					}
				;

/* ------------------------------------------------------------------ */
view_mapping3                   :       VIEW_MAPPING3
					LONG
					real_num real_num real_num real_num
					perspective_flag
					real_num real_num real_num
					real_num real_num 
					{
					 double prp[3];
					 (void)bif_viewmap3(BIF_P_BEGIN);
					 prp[0] = $8;
					 prp[1] = $9;
					 prp[2] = $10;
					 (void)bif_viewmapbasic($2, $3, $4,
							   $5, $6, $7,prp,
							   $11,$12 );
					}
					match_view_area
					';'
					{(void)bif_viewmap3(BIF_P_END  );}
				;

perspective_flag		:	PERSPECTIVE	{ $$ = PERSPECTIVE; }
				|	PARALLEL	{ $$ = PARALLEL; }
				;

match_view_area                 :       /* empty */
				|       MATCH_VIEW_AREA
					'{' 
					  real_num real_num real_num real_num 
					  adjust_flag 
					'}'
					{
					 (void)bif_viewmapmatch($3,$4,$5,$6,$7);
					}
				;

adjust_flag			:	ADJUST_X	{ $$ = ADJUST_X; }
				|	ADJUST_Y	{ $$ = ADJUST_Y; }
				|	GROW		{ $$ = GROW; }
				|	SHRINK		{ $$ = SHRINK; }
				;

/* ------------------------------------------------------------------ */
active_view                     :       ACTIVE_VIEW
					LONG
					';'
					{(void)bif_activeview($2);}
				;

/* ------------------------------------------------------------------ */
execute_structure               :       EXECUTE_STRUCTURE
					LONG
					';'
					{(void)bif_execstr($2);}
				;

/* ------------------------------------------------------------------ */
call_structure                  :       CALL_STRUCTURE
					LONG
					';'
					{(void)bif_callstr($2);}
				;

/* ------------------------------------------------------------------ */
unrecognized_entity		:	UNRECOGNIZED_KEYWORD
					{ 
					  char temp[STRLENGTH]; 
					  sprintf(temp,
					  "Unsupported entity (%s) ", $1);
 					  yyerror(temp);
					}
					error
					/* any_token_set */
					';'
					{ 
					  char temp[STRLENGTH]; 
					  sprintf(temp,
					  "End of Unsupported entity (%s) ", 
					   $1);
 					  yyerror(temp);
					}
				;



/* ------------------------------------------------------------------ */
				/* CHANGE: This bif entity will now except
					one or more geometry files to be
					read */
read_geometry_file              :       READ_GEOMETRY_FILE
					{(void)collapseAllStructures();}
					geom_file_names
					{(void)expandAllStructures();}
					';'
				;
/* ------------------------------------------------------------------ */
geom_file_names                 :       QSTRING            
					{(void)bif_readgeom($1);}
					bif_geometry_file 
					END_GEOM_FILE 
				|       geom_file_names
					QSTRING
					{(void)bif_readgeom($2);}
					bif_geometry_file 
					END_GEOM_FILE 
				;
/* ------------------------------------------------------------------ */
clear_geometry                  :       CLEAR_GEOMETRY
					';'
					{(void)bif_cleargeom();}
				;

/* ------------------------------------------------------------------ */
specify_report_file		:	SPECIFY_REPORT_FILE
					QSTRING	
					';'
					{(void)bif_reportfile($2);}
				;
/* ------------------------------------------------------------------ */
pause                           :       PAUSE
					';'
					{(void)bif_pause();}
				;

/* ------------------------------------------------------------------ */
sleep                           :       SLEEP
					LONG
					';'
					{(void)bif_sleep($2);}
				;
/* ------------------------------------------------------------------ */
invoke_at_frame		:	INVOKE_AT_FRAME
				invoke_at_frame_body
				';'
			;

invoke_at_frame_body	:	/* empty */
			|	invoke_at_frame_body 	/* rev 0.8 */
				LONG
				exe_or_call 		/* rev 0.8 */
				LONG
				{ (void)bif_invokeatframe /* rev 0.8 */
				   ($2,$3,$4,$4); } 	/* rev 0.8 */
			|	invoke_at_frame_body
				LONG
				exe_or_call
				LONG
				TO LONG
				{ (void)bif_invokeatframe /* rev 0.8 */
				   ($2,$3,$4,$6); } 	/* rev 0.8 */
			|	invoke_at_frame_body
				LONG
				exe_or_call
				LONG
				TO END
				{ (void)bif_invokeatframe /* rev 0.8 */
				   ($2,$3,$4,-1); } 	/* rev 0.8 */
			;

exe_or_call		:	EXECUTE { $$ = EXECUTE ; } /* rev 0.8 */
			|	CALL    { $$ = CALL  ; } /* rev 0.8 */
			;

/* ------------------------------------------------------------------ */
define_color                    :       DEFINE_COLOR
					bif_table_color
					';'
				;

bif_table_color                	:       LONG  /* rev 0.8 */
					real_num real_num real_num
					{
					(void) bif_definecolor /* rev 0.8 */
					   ($1,$2,$3,$4); 
					}
				|       bif_table_color LONG
					real_num real_num real_num 
					{
					(void) bif_definecolor
					   ($2,$3,$4,$5); 
					}
				;
/* ------------------------------------------------------------------ */
background_color		:	BACKGROUND_COLOR /* rev 0.8 */
					real_num real_num real_num
					';'
					{(void)bif_backgroundcolor /* rev 0.8 */
					   ($2,$3,$4);}
				;
/* ------------------------------------------------------------------ */
background_color_index          :       BACKGROUND_COLOR_INDEX
					LONG
					';'
					{(void)bif_backgroundcolorindex($2);}
				;
/* ------------------------------------------------------------------ */
define_view_specification       :       DEFINE_VIEW_SPECIFICATION
					LONG LONG LONG
					/* real_num real_num    */
					xy_clip_flag
					front_clip_flag
					back_clip_flag
					real_num real_num   real_num real_num
					';'
					{(void)bif_viewspec($2, $3,  $4,
							    /*  $5, $6,   */
							    $5, 
							    $6,
							    $7,
							    $8 ,$9 , $10,$11);}
				;

xy_clip_flag			:	XY_CLIP		{ $$ = XY_CLIP; }
				|	NO_XY_CLIP	{ $$ = NO_XY_CLIP; }
				;

front_clip_flag			:	FRONT_CLIP	{ $$ = FRONT_CLIP; }
				|	NO_FRONT_CLIP	{ $$ = NO_FRONT_CLIP; }
				;

back_clip_flag			:	BACK_CLIP	{ $$ = BACK_CLIP; }
				|	NO_BACK_CLIP	{ $$ = NO_BACK_CLIP; }
				;

/* ------------------------------------------------------------------ */
default_view_specification      :       DEFAULT_VIEW_SPECIFICATION
					LONG real_num perspective_flag
					';'
					{
					 (void)bif_defviewspec($2,$3,$4);
					}
				;
/* ------------------------------------------------------------------ */
define_light            :       DEFINE_LIGHT
				{(void)bif_definelight(BIF_P_BEGIN);}
				LONG
				real_num real_num real_num
				light_type ld_transform ';'
				{
				 (void)bif_lightbasic($3,$4,$5,$6);
				 (void)bif_definelight(BIF_P_END  );
				}
			;

/* OLD:  this routine is no longer used:    bif_lightcoord(WORLD);   */

light_type              :       /* empty */
			|       AMBIENT_LIGHT
			|       DIRECTIONAL_LIGHT
				'{' real_num real_num real_num '}'
				{
				 (void)
				 bif_lightoption(DIRECTIONAL_LIGHT, 
				     0., 0., 0.,
				     $3, $4, $5,
				     0., 0.,
				     0., 0. );
			        }
			|       POSITIONAL_LIGHT
				'{' real_num 
				    real_num 
				    real_num 
				    real_num 
				    real_num '}'
				{(void)
				 bif_lightoption(POSITIONAL_LIGHT, 
				     $3, $4, $5,
				     0., 0., 0.,
				     0., 0.,
				     $6, $7 );}
			|       SPOT_LIGHT
				'{' real_num real_num real_num
				    real_num real_num real_num
				    real_num real_num
				    real_num real_num '}'
				{(void)
				 bif_lightoption(SPOT_LIGHT, 
				     $3, $4, $5,
				     $6, $7, $8,
				     $9, $10,
				     $11,$12);}
			;

ld_transform		:	/* empty */
			|	LD_TRANSFORM
				'{' LONG '}'
				{ (void)bif_ldtransform($3); }
			;
/* ------------------------------------------------------------------ */
define_depthcue		:	DEFINE_DEPTHCUE /* rev 0.8 */
				LONG DISABLE
				';'
				{(void)
				   bif_definedepthcue /* rev 0.8 CHANGED: JMZ */
				   ($2, DISABLE, 0.,0., 0.,0., 0.,0.,0.0); }
			|	DEFINE_DEPTHCUE
				LONG ENABLE
				'{' 
				real_num real_num
				real_num real_num
				real_num real_num real_num
				'}' 
				';'
				{(void)
				   bif_definedepthcue /* rev 0.8 CHANGED: JMZ */
				   ($2, ENABLE, $5,$6, $7,$8, $9,$10,$11); }
			;
/* ------------------------------------------------------------------ */
b_configuration		:	/* empty */
				{
				   bif_defaultconfig();
				   bif_openwk();
				}
			;

configuration		:	/* empty */
				{
				   bif_defaultconfig();
				   bif_openwk();
				}
			|	CONFIGURATION
				{
				   (void)not_repeated(-1);
				   bif_defaultconfig();
				}
				configuration_body
				';'
				{
				   bif_openwk();
				   (void)not_repeated(-1);
				}
			;

configuration_body	:	/* empty */
			|	configuration_body RGB
				{ 
				  if(not_repeated(1)) bif_colormodel(RGB);
				  else yyerror(CB_ERM1);
				}
			|	configuration_body CIE
				{ 
				  if(not_repeated(1)) bif_colormodel(CIE);
				  else yyerror(CB_ERM1);
				}
			|	configuration_body HSV
				{ 
				  if(not_repeated(1)) bif_colormodel(HSV);
				  else yyerror(CB_ERM1);
				}
			|	configuration_body HLS
				{ 
				  if(not_repeated(1))
				     bif_colormodel(HLS); /* rev 0.8 mod. */
				  else yyerror(CB_ERM1);
				}
			|	configuration_body DOUBLE_BUFFER
				{ if(not_repeated(2))
			     	     bif_buffermode(DOUBLE_BUFFER); /*rev 0.8*/
				  else
				     yyerror(CB_ERM2);
				}
			|	configuration_body SINGLE_BUFFER
				{ if(not_repeated(2))
				     bif_buffermode(SINGLE_BUFFER); 
				  else
				     yyerror(CB_ERM2);
				}
			|	configuration_body TRUE_COLOR
				{ if(not_repeated(3))
			     	     bif_colormode(TRUE_COLOR); /* rev 0.8 */
				  else
				     yyerror(CB_ERM3);
				}
			|	configuration_body PSEUDO_COLOR
				{ if(not_repeated(3))
			     	     bif_colormode(PSEUDO_COLOR); /* rev 0.8 */
				  else
				     yyerror(CB_ERM3);
				}
			|	configuration_body WINDOW_SIZE LONG LONG
				{ if(not_repeated(4))
		   	             (void)bif_window($3,$4);
				  else
				     yyerror(CB_ERM4);
				}
			;
			
/* ------------------------------------------------------------------ */
/* ------------------------------------------------------------------ */
/* ------------------------------------------------------------------ */
real_num		:	REAL { $$ = $1; }
			|	LONG { $$ = (double)$1; }
			;



%%

/* ------------------------------------------------------------------ */
/* not_repeated is a utility used by entities in yacc to allow        */
/* optional date groups to be specified in any order but to detect   */
/* if an optional group is specified more than once.                  */ 
/*                                                                    */ 
/*  code        < 0:       Reinitialize the repeat_table              */ 
/*  code        > REPEAT_TABLE_SIZE -1: is an error.                  */ 
/*  return_code = FALSE:   The optional group has been repeated       */
/*  return_code = TRUE:    The optional group has not been repeated   */
/* ------------------------------------------------------------------ */
int not_repeated(code)
int code;
{
	int return_code;
	static int repeat_table[REPEAT_TABLE_SIZE];
	int i;

	return_code = TRUE;
	if (code < 0)
	{
		for(i=0;i<REPEAT_TABLE_SIZE;i++)
			repeat_table[i]=TRUE;
	}
	else if (code > REPEAT_TABLE_SIZE-1)
	{
	   yyerror
	   ("Program error:  Attempt to index beyond end of repeat_table.");
	   yyerror
	   ("             :  Entity option has been ignored.");
		return_code = FALSE;
	}
	else
	{
		if(repeat_table[code])  repeat_table[code] = FALSE;
		else   return_code = FALSE;
	}
	return(return_code);
}
