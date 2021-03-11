/* $XConsortium: bifmacro.h,v 5.2 91/07/10 08:27:25 rws Exp $ */

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
| File          :	bifmacro.h
| Date          :	3/17/89
| Project       :	PLB
|
| Description	:	Useful macros, from vector copying to
|			yacc-token-to-BIF-constants mappings
|
| Status     	: 	Version 1.0
|			Not completely organized.  
|			Some macros could use extra
|			()'s to make them "bullet-proof"
|
| Revisions     :	
|	4/4/89		Added macro Transpose44
|	2/90		Error Macros upgraded to meet ANSI cpp spec.
|
\*--------------------------------------------------------------------*/

/* ---------------------------------------------------------------------*\
| Local MACROS                                        
\*--------------------------------------------------------------------- */

/* Fill the top of an entity */
#define HEADER(ent,type,hf,next_ent) \
{\
	ent->entity_type = type;\
	ent->handler     = hf;\
	ent->next        = next_ent;\
	ent->exception	 = 0;\
}

/* Generalized Traverser Call */
#define	Traverse(traverser_state,entity) \
	(*traverser_state->handler)(traverser_state,entity)

/* Free all Non Retained Entities */
#define	Free_NRE(traverser_state, entity) \
{\
	if (traverser_state->open_structure == NULL )\
		free(entity);\
} /* End macro Free_NRE */

/* Return the traverser mode */
#define	Build_mode(traverser_state) \
	 (traverser_state->open_structure == NULL ? 1 : 0 )

#define Cpvec3f(a,b) \
{ \
	b[0] = a[0] ; \
	b[1] = a[1] ; \
	b[2] = a[2] ; \
} /* End macro Cpvec3f */

#define DOTVEC3F(a,b) \
( \
	b[0] * a[0] + \
	b[1] * a[1] + \
	b[2] * a[2]  \
) /* End macro DOTVEC3F */

#define Cpmatrix44(mata, matb) \
{\
	matb[0][0]=mata[0][0]; matb[0][1]=mata[0][1]; \
	matb[0][2]=mata[0][2]; matb[0][3]=mata[0][3]; \
	matb[1][0]=mata[1][0]; matb[1][1]=mata[1][1]; \
	matb[1][2]=mata[1][2]; matb[1][3]=mata[1][3]; \
	matb[2][0]=mata[2][0]; matb[2][1]=mata[2][1]; \
	matb[2][2]=mata[2][2]; matb[2][3]=mata[2][3]; \
	matb[3][0]=mata[3][0]; matb[3][1]=mata[3][1]; \
	matb[3][2]=mata[3][2]; matb[3][3]=mata[3][3]; \
} /* End macro Cpmatrix44 */

#define MAX_VAL(a,b) (( (a) > (b) ) ? (a) : (b)  )
#define MIN_VAL(a,b) (( (a) < (b) ) ? (a) : (b)  )

#ifdef EXTERNAL_NOTE
	The error reporting macros have been modified to conform with
	the ANSI C macro argument replacement specifications.
#endif /* EXTERNAL_NOTE */
#define ERROR_MATRIX_ID(mat_id,entity) \
{\
	if ( !(-1 < mat_id && mat_id < MATRIX_TABLE_SIZE ) ) \
	{ \
		char buffy[255];\
		char *find_keyword_token();\
		\
		sprintf(buffy, "%s: matrix_id %d out of range.",\
			find_keyword_token(entity), mat_id);\
		yyerror(buffy);\
		\
		sprintf(buffy, "Valid range is 0 <= matrix_id < %d.",\
				MATRIX_TABLE_SIZE); \
		yyerror(buffy);\
		exit(-1); \
	}\
} /* End macro Error_matrix_id */


#define ERROR(what) \
{\
	yyerror(what);\
	exit(-1);\
} /* End macro ERROR */

#define ENT_ERROR(ent) \
{\
	if ( ent == NULL ) \
			ERROR("OUT_OF_MEMORY");\
}

#define BEGEND(what) \
{\
	switch ( begin_or_end )\
	{\
	case BIF_P_BEGIN:\
		printf("Beginning %s \n", what);\
		break;\
	case BIF_P_END:\
		printf("End %s \n", what);\
		break;\
	}\
} /* End macro BEGEND */

#define PRINT_MATRIX44f(mat) \
{\
	printf("Row1: %f %f %f %f\n",mat[0][0],mat[0][1],mat[0][2],mat[0][3]);\
	printf("Row2: %f %f %f %f\n",mat[1][0],mat[1][1],mat[1][2],mat[1][3]);\
	printf("Row3: %f %f %f %f\n",mat[2][0],mat[2][1],mat[2][2],mat[2][3]);\
	printf("Row4: %f %f %f %f\n",mat[3][0],mat[3][1],mat[3][2],mat[3][3]);\
} /* End macro PRINT_MATRIX44 */

#define PRINT_MATRIX44(mat) \
{\
	printf("Row1: %lf %lf %lf %lf\n",mat[0][0],mat[0][1],mat[0][2],mat[0][3]);\
	printf("Row2: %lf %lf %lf %lf\n",mat[1][0],mat[1][1],mat[1][2],mat[1][3]);\
	printf("Row3: %lf %lf %lf %lf\n",mat[2][0],mat[2][1],mat[2][2],mat[2][3]);\
	printf("Row4: %lf %lf %lf %lf\n",mat[3][0],mat[3][1],mat[3][2],mat[3][3]);\
} /* End macro PRINT_MATRIX44 */

#define REMAP_CMODEL(cmodel) \
	( ((cmodel) == RGB ) ?  BIF_RGB : \
	( ((cmodel) == CIE ) ?  BIF_CIE : \
	( ((cmodel) == HSV ) ?  BIF_HSV : \
				BIF_HLS ) ) )

#define REMAP_CONCAT(concat) \
	( ( (concat) == PRECONCAT  ) ? BIF_PRECONCAT  : \
	( ( (concat) == POSTCONCAT ) ? BIF_POSTCONCAT : \
			BIF_REPLACE ) )

#define REMAP_PROJ(proj_type) \
	( ( (proj_type) == PARALLEL  ) ? BIF_PARALLEL  : \
		BIF_PERSPECTIVE )

#define REMAP_CLIP(clip) \
	( ( ( (clip) == XY_CLIP ) || ( (clip) == FRONT_CLIP )  || \
	    ( (clip) == BACK_CLIP ) ) ? BIF_CLIP  : BIF_NO_CLIP )

#define REMAP_INTSTYLE(style) \
	( ((style) == HOLLOW ) ? BIF_HOLLOW : \
	( ((style) == SOLID  ) ? BIF_SOLID  : \
	( ((style) == PATTERN) ? BIF_PATTERN: \
				 BIF_EMPTY ) ) )

#define REMAP_EDGEFLAG(flag) \
	( ( (flag) == ENABLE  ) ? BIF_ON  : BIF_OFF )

#define REMAP_TEXTFONT(flag) \
	( ( (flag) == -1  ) ? 1  : 1 )

#define REMAP_TEXTPREC(prec) \
	( ( (prec) == STRING  ) ? BIF_STRING  : \
	( ( (prec) == CHAR    ) ? BIF_CHAR    : \
				  BIF_STROKE  ) )

#define REMAP_LIGHTTYPE(type) \
	( ( (type) == AMBIENT_LIGHT     ) ? BIF_AMBIENT     : \
	( ( (type) == DIRECTIONAL_LIGHT ) ? BIF_DIRECTIONAL : \
	( ( (type) == POSITIONAL_LIGHT  ) ? BIF_POSITIONAL  : \
					    BIF_SPOT ) ) )

#define REMAP_DCMODE(flag) \
	( ( (flag) == ENABLE  ) ? BIF_ON  : BIF_OFF )

#define REMAP_HLHS(flag) \
	( ( (flag) == HLHS_ENABLE  ) ? BIF_HLHS_ENABLE  : \
				       BIF_HLHS_DISABLE )

#define REMAP_GETMAT(get) \
	( ( (get) == VIEW_MAPPING     ) ? BIF_VIEW_MAPPING     :\
	( ( (get) == VIEW_ORIENTATION ) ? BIF_VIEW_ORIENTATION :\
	( ( (get) == GLOBAL_MODELLING ) ? BIF_GLOBAL_MODELLING :\
	( ( (get) == LOCAL_MODELLING  ) ? BIF_LOCAL_MODELLING  :\
				BIF_COMPOSITE_MODELLING ) ) ) )

#define REMAP_CSFID(colType) \
	( ( (colType) == LINE_COLOR     ) ? FIG_PCOPL	:\
	( ( (colType) == INTERIOR_COLOR ) ? FIG_PCOINT	:\
	( ( (colType) == EDGE_COLOR     ) ? FIG_PCOEDG	:\
	( ( (colType) == TEXT_COLOR     ) ? FIG_PCOTXT	:\
	( ( (colType) == MARKER_COLOR   ) ? FIG_PCOPM	:\
					    FIG_PCOBK ) ) ) ) )

#define REMAP_CSFIDINDEX(colType) \
	( ( (colType) == LINE_COLOR_INDEX     ) ? FIG_PCOPL	:\
	( ( (colType) == INTERIOR_COLOR_INDEX ) ? FIG_PCOINT	:\
	( ( (colType) == EDGE_COLOR_INDEX     ) ? FIG_PCOEDG	:\
	( ( (colType) == TEXT_COLOR_INDEX     ) ? FIG_PCOTXT	:\
	( ( (colType) == MARKER_COLOR_INDEX   ) ? FIG_PCOPM	:\
						  FIG_PCOBK ) ) ) ) )

#define REMAP_INVOKE(flag) \
	( ( (flag) == CALL  ) ? BIF_CALL  : BIF_EXECUTE )


#define REMAP_PIXFUNC(func) \
	( ( (func) == ADD		) ?  BIF_PF_ADD           :\
	( ( (func) == AND		) ?  BIF_PF_AND           :\
	( ( (func) == CLEAR		) ?  BIF_PF_CLEAR         :\
	( ( (func) == INVERT		) ?  BIF_PF_INVERT        :\
	( ( (func) == NAND		) ?  BIF_PF_NAND          :\
	( ( (func) == NOOP		) ?  BIF_PF_NOOP          :\
	( ( (func) == NOR		) ?  BIF_PF_NOR           :\
	( ( (func) == OR 		) ?  BIF_PF_OR            :\
	( ( (func) == REPLACE		) ?  BIF_PF_REPLACE       :\
	( ( (func) == SET		) ?  BIF_PF_SET           :\
	( ( (func) == SUBTRACT_DEST	) ?  BIF_PF_SUBTRACT_DEST :\
	( ( (func) == SUBTRACT_SOURCE	) ?  BIF_PF_SUBTRACT_SOURCE :\
					XOR ) ) ) ) ) ) ) ) ) ) ) )

#define Cp16f(a,b) \
{ \
	b[ 0] = a[ 0] ; \
	b[ 1] = a[ 1] ; \
	b[ 2] = a[ 2] ; \
	b[ 3] = a[ 3] ; \
	b[ 4] = a[ 4] ; \
	b[ 5] = a[ 5] ; \
	b[ 6] = a[ 6] ; \
	b[ 7] = a[ 7] ; \
	b[ 8] = a[ 8] ; \
	b[ 9] = a[ 9] ; \
	b[10] = a[10] ; \
	b[11] = a[11] ; \
	b[12] = a[12] ; \
	b[13] = a[13] ; \
	b[14] = a[14] ; \
	b[15] = a[15] ; \
} /* End macro Cp16f */


#define Transpose44(mata, matb) \
{\
/* Diagonal Terms  ( Copy ) */ \
	matb[0][0]=mata[0][0]; matb[1][1]=mata[1][1]; \
	matb[2][2]=mata[2][2]; matb[3][3]=mata[3][3]; \
/* Off-Diagonal Terms  ( Swap ) */ \
	matb[0][1]=mata[1][0]; matb[1][0]=mata[0][1];\
	matb[0][2]=mata[2][0]; matb[2][0]=mata[0][2]; \
	matb[0][3]=mata[3][0]; matb[3][0]=mata[0][3]; \
	\
	matb[1][2]=mata[2][1]; matb[2][1]=mata[1][2]; \
	matb[1][3]=mata[3][1]; matb[3][1]=mata[1][3]; \
	\
	matb[2][3]=mata[3][2]; matb[3][2]=mata[2][3]; \
} /* End macro Transpose44 */

/* Pass a vector as pointers to its components */
#define Fpass3f(v) &v[0], &v[1], &v[2] 
#define Fpass4f(v) &v[0], &v[1], &v[2], &v[3]
#define Fpass6f(v) &v[0], &v[1], &v[2], &v[3], &v[4], &v[5]
