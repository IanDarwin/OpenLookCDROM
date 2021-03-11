/* $XConsortium: doentity.c,v 5.3 94/04/17 20:44:34 rws Exp $ */
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
Copyright(c) 1989,1990, 1991 by Sun Microsystems, Inc.

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
| File          :	doentity.c
| Date          :	Fri Feb  9 10:46:55 PST 1990
| Project       :	PLB
| Description   :	contain the entity handling function for 
|			all but those who can alter the base_state.
|
| Status        :	Version 1.0
|
| Revisions     :	
|	2/89		Staff, SimGEC: Remove obsolete functions
|			do_backgroundcolor, do_backgroundcolorindex,
|			and do_colormodel.
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
|	do_beginstructure(*BIF_Traverser_state, *BIF_all)
|		:	begin a structure. Change the traverser_state to
|	do_endstructure(*BIF_Traverser_state, *BIF_all)
|		:	end the current structure by change the
|	do_marker(*BIF_Traverser_state, *BIF_all)
|		:	Do the traversal of a marker entity.
|	do_marker3( *BIF_Traverser_state, *BIF_All )
|		:	Do the traversal of a marker3 entity.
|	do_line( *BIF_Traverser_state, *BIF_All )
|		:	Do the traversal of a line entity.  
|	do_line3( *BIF_Traverser_state, *BIF_All )
|		:	Do the traversal of a line3 entity.  
|	do_polygon( *BIF_Traverser_state, *BIF_All )
|		:	Do the traversal of a polygon entity.  
|	do_polygon3( *BIF_Traverser_state, *BIF_All )
|		:	Do the traversal of a polygon3 entity.  
|	do_fillareaset( *BIF_Traverser_state, *BIF_All )
|		:	Do the traversal of a fillareaset entity.  
|	do_fillareaset3( *BIF_Traverser_state, *BIF_All)
|		:	Do the traversal of a fillareaset3 entity.  
|	do_triangle3( *BIF_Traverser_state, *BIF_All )
|		:	Do the traversal of a triangle3 entity.  
|	do_quadmesh3( *BIF_Traverser_state, *BIF_All )
|		:	Do the traversal of a quadmesh3 entity.  
|	do_indexpolygons3( *BIF_Traverser_state, *BIF_All )
|		:	Do the traversal of a indexpolygons3 entity.  
|	do_text( *BIF_Traverser_state, *BIF_All )
|		:	Do the traversal of a text entity 
|	do_text3( *BIF_Traverser_state, *BIF_All )
|		:	Do the traversal of a text3 entity 
|	do_annotext3( *BIF_Traverser_state, *BIF_All )
|		:	Do the traversal of a annotext3 entity 
|	do_definelight(*BIF_Traverser_state, *BIF_all)
|		:	perform the PHIGS / native graphic calls that
|	do_definedepthcue(*BIF_Traverser_state, *BIF_all)
|		:	Define an entry in the depthcue table.
|	do_definecolor(*BIF_Traverser_state, *BIF_all)
|		:	Define a color in the color map
|	do_concatmatrix3(*BIF_Traverser_state, *BIF_all)
|		:	Concatenate 2 entries in the matrix table
|	do_invertmatrix3(*BIF_Traverser_state, *BIF_all)
|		:	Invert an entry in the matrix table
|	do_matrix3(*BIF_Traverser_state, *BIF_all)
|		:	apply a matrix to an entry of the matrix table
|	do_getmatrix3(*BIF_Traverser_state, *BIF_all)
|		:	load the contents of a "system" matrix into the
|	int do_pushmatrix3(*BIF_Traverser_state, *BIF_all)
|		:	Push the stack, copy the contents of matrix
|	int do_popmatrix3(*BIF_Traverser_state, *BIF_all)
|		:	Pop the stack, copy the contents of the top of
|	do_defineviewspecification(*BIF_Traverser_state, *BIF_all)
|		:	Define an entry in the view table
|	do_defaultviewspecification(*BIF_Traverser_state, *BIF_all)
|		:	define a default view table entry
|	do_executestructure(*BIF_Traverser_state, *BIF_all)
|		:	push the traverser state and call the
|	do_callstructure(*BIF_Traverser_state, *BIF_all)
|		:	call the specified structure
|	do_invokeatframe(*BIF_Traverser_state, *BIF_all)
|		:	execute invokeatframe editing and conditional BIF
|	int fitViewMapToNDC(*float, matrix4);
|		:	Transform a viewmapping matrix that maps to the
|
\*--------------------------------------------------------------------*/

/*--------------------------------------------------------------------*\
|	Include files
\*--------------------------------------------------------------------*/
#include <stdio.h>
#include "bifbuild.h"
#include "biftypes.h"
#include "bifmacro.h"
#include "db_tools.h"
#include "globals.h"
#include "ph_map.h"
#include <X11/Xfuncs.h>


/* ---------------------------------------------------------------------*\
|Local #define                                                           |
\*--------------------------------------------------------------------- */

/* ---------------------------------------------------------------------*\
| Local MACROS                                                           |
\*--------------------------------------------------------------------- */

/* ---------------------------------------------------------------------*\
| Local global variables                                                 |
\*--------------------------------------------------------------------- */
extern void print44f();
extern build_traverser();
extern execute_traverser();

/*--------------------------------------------------------------------*\
|	BEGIN PROCEDURE CODE
\*--------------------------------------------------------------------*/

/*----------------------------------------------------------------------*\
| Procedure     :	do_beginstructure(*BIF_Traverser_state, *BIF_all)
|------------------------------------------------------------------------
| Description   :	begin a structure. Change the traverser_state to
|			"BUILD" by changing the handler function to the
|			build traverser.  Save the open structure and
|			insertion point in the traversal state.
|------------------------------------------------------------------------
| Return        :	None
\*----------------------------------------------------------------------*/
int do_beginstructure( traverser_state, entity )
BIF_Traverser_state *traverser_state;
BIF_All *entity;
{
/* Open the structure */
	traverser_state->open_structure = &entity->beginstructure;
	traverser_state->insert_after
		= end_of_list(entity->beginstructure.top_of_list);

/* Set the traversal function to the build_function */
	traverser_state->handler = build_traverser;

/* Return Error Code */
	return(0);  /* No Error */
} /* End do_beginstructure */

/*----------------------------------------------------------------------*\
| Procedure     :	do_endstructure(*BIF_Traverser_state, *BIF_all)
|------------------------------------------------------------------------
| Description   :	end the current structure by change the
|			traverser_state to "EXECUTE" by changing the
|			handler function to the execute traverser.  Set
|			the open structure and insertion point to NULL
|			in the traversal state.
|------------------------------------------------------------------------
| Return        :	Error code
\*----------------------------------------------------------------------*/
int do_endstructure( traverser_state, entity )
BIF_Traverser_state *traverser_state;
BIF_All *entity;
{
	/* Close the structure */
		traverser_state->open_structure = NULL;
		traverser_state->insert_after   = NULL;

	/* Set the traversal function to the execute_function */
		traverser_state->handler = execute_traverser;
		
/* Return Error Code */
	return(0);  /* No Error */
} /* End do_endstructure */

/*----------------------------------------------------------------------*\
| Procedure     :	do_marker(*BIF_Traverser_state, *BIF_all)
|------------------------------------------------------------------------|
| Description   :	Do the traversal of a marker entity.
|------------------------------------------------------------------------|
| Return        :	Error code
\*----------------------------------------------------------------------*/
int do_marker( traverser_state, entity )
BIF_Traverser_state *traverser_state;
BIF_All *entity;
{
#ifdef USING_PHIGS
/* No Action for Primitives */
#else 
/* Insert Native Graphics Calls Here */
#endif
/* Return Error Code */
	return(0);  /* No Error */
} /* End procedure do_marker */

/*----------------------------------------------------------------------*\
| Procedure     :	do_marker3( *BIF_Traverser_state, *BIF_All )
|------------------------------------------------------------------------|
| Description   :	Do the traversal of a marker3 entity.
|------------------------------------------------------------------------|
| Return        :	Error code
\*----------------------------------------------------------------------*/
int do_marker3( traverser_state, entity )
BIF_Traverser_state *traverser_state;
BIF_All *entity;
{
#ifdef USING_PHIGS
/* No Action for Primitives */
#else 
/* Insert Native Graphics Calls Here */
#endif
/* Return Error Code */
	return(0);  /* No Error */
} /* End procedure do_marker3 */

/*----------------------------------------------------------------------*\
| Procedure     :	do_line( *BIF_Traverser_state, *BIF_All )
|------------------------------------------------------------------------|
| Description   :	Do the traversal of a line entity.  
|------------------------------------------------------------------------|
| Return        :	Error code
\*----------------------------------------------------------------------*/
int do_line( traverser_state, entity )
BIF_Traverser_state *traverser_state;
BIF_All *entity;
{
#ifdef USING_PHIGS
/* No Action for Primitives */
#else 
/* Insert Native Graphics Calls Here */
#endif
/* Return Error Code */
	return(0);  /* No Error */
} /* End procedure do_line */

/*----------------------------------------------------------------------*\
| Procedure     :	do_line3( *BIF_Traverser_state, *BIF_All )
|------------------------------------------------------------------------|
| Description   :	Do the traversal of a line3 entity.  
|------------------------------------------------------------------------|
| Return        :	Error code
\*----------------------------------------------------------------------*/
int do_line3( traverser_state, entity )
BIF_Traverser_state *traverser_state;
BIF_All *entity;
{
#ifdef USING_PHIGS
/* No Action for Primitives */
#else 
/* Insert Native Graphics Calls Here */
#endif
/* Return Error Code */
	return(0);  /* No Error */
} /* End procedure do_line3 */

/*----------------------------------------------------------------------*\
| Procedure     :	do_polygon( *BIF_Traverser_state, *BIF_All )
|------------------------------------------------------------------------|
| Description   :	Do the traversal of a polygon entity.  
|------------------------------------------------------------------------|
| Return        :	Error code
\*----------------------------------------------------------------------*/
int do_polygon( traverser_state, entity )
BIF_Traverser_state *traverser_state;
BIF_All *entity;
{
#ifdef USING_PHIGS
/* No Action for Primitives */
#else 
/* Insert Native Graphics Calls Here */
#endif
/* Return Error Code */
	return(0);  /* No Error */
} /* End procedure do_polygon */

/*----------------------------------------------------------------------*\
| Procedure     :	do_polygon3( *BIF_Traverser_state, *BIF_All )
|------------------------------------------------------------------------|
| Description   :	Do the traversal of a polygon3 entity.  
|------------------------------------------------------------------------|
| Return        :	Error code
\*----------------------------------------------------------------------*/
int do_polygon3( traverser_state, entity )
BIF_Traverser_state *traverser_state;
BIF_All *entity;
{
#ifdef USING_PHIGS
/* No Action for Primitives */
#else 
/* Insert Native Graphics Calls Here */
#endif
/* Return Error Code */
	return(0);  /* No Error */
} /* End procedure do_polygon3 */

/*----------------------------------------------------------------------*\
| Procedure     :	do_fillareaset( *BIF_Traverser_state, *BIF_All )
|------------------------------------------------------------------------|
| Description   :	Do the traversal of a fillareaset entity.  
|------------------------------------------------------------------------|
| Return        :	Error code
\*----------------------------------------------------------------------*/
int do_fillareaset( traverser_state, entity )
BIF_Traverser_state *traverser_state;
BIF_All *entity;
{
#ifdef USING_PHIGS
/* No Action for Primitives */
#else 
/* Insert Native Graphics Calls Here */
#endif
/* Return Error Code */
	return(0);  /* No Error */
} /* End procedure do_fillareaset */

/*----------------------------------------------------------------------*\
| Procedure     :	do_fillareaset3( *BIF_Traverser_state, *BIF_All)
|------------------------------------------------------------------------|
| Description   :	Do the traversal of a fillareaset3 entity.  
|------------------------------------------------------------------------|
| Return        :	Error code
\*----------------------------------------------------------------------*/
int do_fillareaset3( traverser_state, entity )
BIF_Traverser_state *traverser_state;
BIF_All *entity;
{
#ifdef USING_PHIGS
/* No Action for Primitives */
#else 
/* Insert Native Graphics Calls Here */
#endif
/* Return Error Code */
	return(0);  /* No Error */
} /* End procedure do_fillareaset3 */

/*--------------------------------------------------------------------*\
| Procedure     :	do_triangle3( *BIF_Traverser_state, *BIF_All )
|----------------------------------------------------------------------|
| Description   :	Do the traversal of a triangle3 entity.  
|----------------------------------------------------------------------|
| Return        :	Error code
\*--------------------------------------------------------------------*/
int do_triangle3( traverser_state, entity )
BIF_Traverser_state *traverser_state;
BIF_All *entity;
{
#ifdef USING_PHIGS
/* No Action for Primitives */
#else 
/* Insert Native Graphics Calls Here */
#endif
/* Return Error Code */
	return(0);  /* No Error */
} /* End procedure do_triangle3 */

/*--------------------------------------------------------------------*\
| Procedure     :	do_quadmesh3( *BIF_Traverser_state, *BIF_All )
|----------------------------------------------------------------------|
| Description   :	Do the traversal of a quadmesh3 entity.  
|----------------------------------------------------------------------|
| Return        :	Error code
\*--------------------------------------------------------------------*/
int do_quadmesh3( traverser_state, entity )
BIF_Traverser_state *traverser_state;
BIF_All *entity;
{
#ifdef USING_PHIGS
/* No Action for Primitives */
#else 
/* Insert Native Graphics Calls Here */
#endif
/* Return Error Code */
	return(0);  /* No Error */
} /* End procedure do_quadmesh3 */

/*--------------------------------------------------------------------*\
| Procedure     :	do_indexpolygons3( *BIF_Traverser_state,
|						*BIF_All )
|----------------------------------------------------------------------|
| Description   :	Do the traversal of a indexpolygons3 entity.  
|----------------------------------------------------------------------|
| Return        :	Error code
\*--------------------------------------------------------------------*/
int do_indexpolygons3( traverser_state, entity )
BIF_Traverser_state *traverser_state;
BIF_All *entity;
{
#ifdef USING_PHIGS
/* No Action for Primitives */
#else 
/* Insert Native Graphics Calls Here */
#endif
/* Return Error Code */
	return(0);  /* No Error */
} /* End procedure do_indexpolygons3 */

/*----------------------------------------------------------------------*\
| Procedure     :	do_text( *BIF_Traverser_state, *BIF_All )
|------------------------------------------------------------------------
| Description   :	Do the traversal of a text entity 
|------------------------------------------------------------------------
| Return        :	Error code
\*----------------------------------------------------------------------*/
int do_text( traverser_state, entity )
BIF_Traverser_state *traverser_state;
BIF_All *entity;
{
#ifdef USING_PHIGS
/* No Action for Primitives */
#else 
/* Insert Native Graphics Calls Here */
#endif
/* Return Error Code */
	return(0);  /* No Error */
}/* End do_text */

/*----------------------------------------------------------------------*\
| Procedure     :	do_text3( *BIF_Traverser_state, *BIF_All )
|------------------------------------------------------------------------
| Description   :	Do the traversal of a text3 entity 
|------------------------------------------------------------------------
| Return        :	Error code
\*----------------------------------------------------------------------*/
int do_text3( traverser_state, entity )
BIF_Traverser_state *traverser_state;
BIF_All *entity;
{
#ifdef USING_PHIGS
/* No Action for Primitives */
#else 
/* Insert Native Graphics Calls Here */
#endif
/* Return Error Code */
	return(0);  /* No Error */
}/* End do_text3 */

/*----------------------------------------------------------------------*\
| Procedure     :	do_annotext3( *BIF_Traverser_state, *BIF_All )
|------------------------------------------------------------------------
| Description   :	Do the traversal of a annotext3 entity 
|------------------------------------------------------------------------
| Return        :	Error code
\*----------------------------------------------------------------------*/
int do_annotext3( traverser_state, entity )
BIF_Traverser_state *traverser_state;
BIF_All *entity;
{
#ifdef USING_PHIGS
/* No Action for Primitives */
#else 
/* Insert Native Graphics Calls Here */
#endif
/* Return Error Code */
	return(0);  /* No Error */
}/* End do_annotext3 */

/*----------------------------------------------------------------------*\
| Procedure     :	do_definelight(*BIF_Traverser_state, *BIF_all)
|------------------------------------------------------------------------
| Description   :	perform the PHIGS / native graphic calls that
|			define a light.
|------------------------------------------------------------------------
| Return        :	
\*----------------------------------------------------------------------*/
int do_definelight( traverser_state, entity )
BIF_Traverser_state *traverser_state;
BIF_All *entity;
{

  BIF_Definelight *dent;
  vector4 position;
  vector4 noXfPosition;
  vector4 direction;
  vector4 noXfDirection;
  Plight_src_bundle rep;

  /* Useful local data */
  dent = &entity->definelight;
  copyvec3f(dent->position, noXfPosition );
  noXfPosition[3] = 1;
  copyvec3f(dent->direction,noXfDirection);
  noXfDirection[3] = 1;
  
  
  /* See if a light def. transform has been specified */
  if ( dent->ld_trans_id != -1 ) {
    /* Transform the defining point and direction */
    mx_44mult(matrix_table[dent->ld_trans_id],noXfPosition,
	      position);
    mx_44mult(matrix_table[dent->ld_trans_id],noXfDirection,
	      direction);
  }
  else {
    copyvec3f(noXfPosition , position );
    copyvec3f(noXfDirection, direction);
  }

#ifdef USING_PHIGS
	/*------------------------------------------------------------*\
	|	Build the PHIGSs record for this entity
	\*------------------------------------------------------------*/
  /* Common Data */
  rep.type = dent->light_type;
  
  switch (dent->light_type) {
  case BIF_AMBIENT:
    /* Colour */
    rep.rec.ambient.colr.type = (Pint)dent->color_model;
    rep.rec.ambient.colr.val.general.x = (Pfloat)dent->light_color[0];
    rep.rec.ambient.colr.val.general.y = (Pfloat)dent->light_color[1];
    rep.rec.ambient.colr.val.general.z = (Pfloat)dent->light_color[2];
    break;

  case BIF_DIRECTIONAL:
    /* Colour */
    rep.rec.directional.colr.type = (Pint)dent->color_model;
    rep.rec.directional.colr.val.general.x =
      (Pfloat)dent->light_color[0];
    rep.rec.directional.colr.val.general.y = 
      (Pfloat)dent->light_color[1];
    rep.rec.directional.colr.val.general.z = 
      (Pfloat)dent->light_color[2];

    /* Direction */
    rep.rec.directional.dir.delta_x = direction[0];
    rep.rec.directional.dir.delta_y = direction[1];
    rep.rec.directional.dir.delta_z = direction[2];
    break;

  case BIF_POSITIONAL:
    /* Colour */
    rep.rec.positional.colr.type = (Pint)dent->color_model;
    rep.rec.positional.colr.val.general.x = 
      (Pfloat)dent->light_color[0];
    rep.rec.positional.colr.val.general.y = 
      (Pfloat)dent->light_color[1];
    rep.rec.positional.colr.val.general.z = 
      (Pfloat)dent->light_color[2];

    /* Position */
    rep.rec.positional.pos.x = position[0];
    rep.rec.positional.pos.y = position[1];
    rep.rec.positional.pos.z = position[2];

    /* Attenuation */
    rep.rec.positional.coef[0] = dent->attenuation[0];
    rep.rec.positional.coef[1] = dent->attenuation[1];
    break;

  case BIF_SPOT:
    /* Colour */
    rep.rec.spot.colr.type = (Pint)dent->color_model;
    rep.rec.spot.colr.val.general.x = (Pfloat)dent->light_color[0];
    rep.rec.spot.colr.val.general.y = (Pfloat)dent->light_color[1];
    rep.rec.spot.colr.val.general.z = (Pfloat)dent->light_color[2];

    /* Position */
    rep.rec.spot.pos.x = position[0];
    rep.rec.spot.pos.y = position[1];
    rep.rec.spot.pos.z = position[2];

    /* Direction */
    rep.rec.spot.dir.delta_x = direction[0];
    rep.rec.spot.dir.delta_y = direction[1];
    rep.rec.spot.dir.delta_z = direction[2];

    /* Concentration exponent */
    rep.rec.spot.exp = dent->exponent;

    /* Attenuation */
    rep.rec.spot.coef[0] = dent->attenuation[0];
    rep.rec.spot.coef[1] = dent->attenuation[1];

    /* Spread angle */
    rep.rec.spot.angle = dent->spread * 0.01745329; /* Degrees to Radians */
    break;
    }

#ifdef TEST_PRINT2
  printf("pset_light_src_rep(%d, %d, %d, and...)\n", bench_setup.workid, 
	 entity->definelight.light_number,
	 entity->definelight.light_type);
  fflush(stdout);
#endif /* TEST_PRINT2 */

  pset_light_src_rep((Pint)bench_setup.workid,
		  (Pint)entity->definelight.light_number,&rep);
#else /* USING PHIGS */
	/* Insert Native Graphics Calls Here */
#endif /* USING PHIGS */

} /* End do_definelight */

/*--------------------------------------------------------------------*\
| Procedure     :	do_definedepthcue(*BIF_Traverser_state,
|						*BIF_all)
|---------------------------------------------------------------------
| Description   :	Define an entry in the depthcue table.
|---------------------------------------------------------------------
| Return        :	Error code
\*--------------------------------------------------------------------*/
int do_definedepthcue( traverser_state, entity )
BIF_Traverser_state *traverser_state;
BIF_All *entity;

{
  BIF_DefineDepthCue *dent;
  dent = &entity->definedepthcue;
#ifdef USING_PHIGS
  pset_dcue_rep((Pint)bench_setup.workid,(Pint)dent->ind,&dent->rep);
#endif /* USING_PHIGS */
}

/*----------------------------------------------------------------------*\
| Procedure     :	do_definecolor(*BIF_Traverser_state, *BIF_all)
|------------------------------------------------------------------------
| Description   :	Define a color in the color map
|------------------------------------------------------------------------
| Return        :	
\*----------------------------------------------------------------------*/
int do_definecolor( traverser_state, entity )
BIF_Traverser_state *traverser_state;
BIF_All *entity;
{
	int index;
#ifdef USING_PHIGS
#ifdef EXTERNALNOTE
        For PHIGS, Index is incremented by one so that color '0' does
        not step on the workstation background color.
#endif
        index = entity->definecolor.ind + 1;

	pset_colr_rep((Pint)bench_setup.workid, (Pint)index, 
		      &entity->definecolor.rep);
#endif

} /* End do_definecolor */

#ifdef EXTERNALNOTE
	do_backgroundcolor and do_backgroundcolorindex no longer exist
	as background color is set only by verbs.
	The background color (index) functions now are in bifbuild.c
#endif

/*----------------------------------------------------------------------*\
| Procedure     :	do_concatmatrix3(*BIF_Traverser_state, *BIF_all)
|------------------------------------------------------------------------
| Description   :	Concatenate 2 entries in the matrix table
|------------------------------------------------------------------------
| Return        :	Error code
\*----------------------------------------------------------------------*/
int do_concatmatrix3( traverser_state, entity )
BIF_Traverser_state *traverser_state;
BIF_All *entity;

{
	Pmatrix3 *mat_to, *mat_from;
	BIF_Concatmatrix3 *cent;

	cent   = &entity->concatmatrix3;
	mat_to   = (Pmatrix3 *)matrix_table[cent->matrix_id_to];
	mat_from = (Pmatrix3 *)matrix_table[cent->matrix_id_from];
#ifdef TEST_PRINT
#ifdef TEST_PRINT2
printf("CCT3: mx_to = %d , mx_from = %d concat = %d \n",
	cent->matrix_id_to, cent->matrix_id_from, cent->concat_type );
print44f(*mat_to);
print44f(*mat_from);
#endif /* TEST_PRINT2 */
#endif /* TEST_PRINT */

	matcat(*mat_from, *mat_to, cent->concat_type);

#ifdef TEST_PRINT
#ifdef TEST_PRINT2
printf("Results: \n");
print44f(mat_to);
#endif /* TEST_PRINT2 */
#endif /* TEST_PRINT */

/* Return Error Code */
	return(0);  /* No Error */

} /* End do_concatmatrix3 */

/*----------------------------------------------------------------------*\
| Procedure     :	do_invertmatrix3(*BIF_Traverser_state, *BIF_all)
|------------------------------------------------------------------------
| Description   :	Invert an entry in the matrix table
|------------------------------------------------------------------------
| Return        :	
\*----------------------------------------------------------------------*/
int do_invertmatrix3( traverser_state, entity )
BIF_Traverser_state *traverser_state;
BIF_All *entity;
{
	float mx[4][4];

#ifdef USING_PHIGS
	mx_invert( matrix_table[ entity->invertmatrix3.matrix_id ], mx );
	copy44f( mx, matrix_table[ entity->invertmatrix3.matrix_id ]);
#endif

} /* End do_invertmatrix3 */

/*----------------------------------------------------------------------*\
| Procedure     :	do_matrix3(*BIF_Traverser_state, *BIF_all)
|------------------------------------------------------------------------
| Description   :	apply a matrix to an entry of the matrix table
|------------------------------------------------------------------------
| Return        :	Error code
\*----------------------------------------------------------------------*/
int do_matrix3( traverser_state, entity )
BIF_Traverser_state *traverser_state;
BIF_All *entity;

{
	/* Update the specified matrix table entry */
	matcat(entity->matrix3.matrix, 
	       matrix_table[ entity->matrix3.matrix_id ],
	       entity->matrix3.concat_type );

/* Return Error Code */
	return(0);  /* No Error */
} /* End do_matrix3 */

/*----------------------------------------------------------------------*\
| Procedure     :	do_getmatrix3(*BIF_Traverser_state, *BIF_all)
|------------------------------------------------------------------------
| Description   :	load the contents of a "system" matrix into the
|			matrix table.
|------------------------------------------------------------------------
| Return        :	Error code
\*----------------------------------------------------------------------*/
int do_getmatrix3( traverser_state, entity )
BIF_Traverser_state *traverser_state;
BIF_All *entity;

{
	BIF_Getmatrix3 *gent;
	gent = &entity->getmatrix3;

	/*------------------------------------------------------------*\
	|	Get a apply the requested matrix
	\*------------------------------------------------------------*/
	switch (gent->get_matrix)
	{
	case BIF_VIEW_MAPPING:
	/* Update the specified matrix table entry */
		matcat(	vm_matrix_public[traverser_state->id_active_view],
		        matrix_table[ gent->matrix_id ],
			gent->concat_type );
		break;

	case BIF_VIEW_ORIENTATION:
	/* Update the specified matrix table entry */
		matcat(	vo_matrix_public[traverser_state->id_active_view],
		        matrix_table[ gent->matrix_id ],
			gent->concat_type );
		break;

	case BIF_GLOBAL_MODELLING:
	/* Update the specified matrix table entry */
		matcat(	traverser_state->global,
		        matrix_table[ gent->matrix_id ],
			gent->concat_type );
		break;

	case BIF_LOCAL_MODELLING:
	/* Update the specified matrix table entry */
		matcat(	traverser_state->local,
		        matrix_table[ gent->matrix_id ],
			gent->concat_type );
		break;

	case BIF_COMPOSITE_MODELLING:
	/* Update the specified matrix table entry */
		matcat(	traverser_state->composite,
		        matrix_table[ gent->matrix_id ],
		        gent->concat_type );
		break;
	}


	/* Return Error Code */
	return(0);  /* No Error */
} /* End do_getmatrix3 */

/*--------------------------------------------------------------------*\
| Procedure     :	int do_pushmatrix3(*BIF_Traverser_state,
|						*BIF_all)
|---------------------------------------------------------------------
| Description   :	Push the stack, copy the contents of matrix
|			table entry zero to the top of the stack
|---------------------------------------------------------------------
| Return        :	Error code (Not Implemented)
\*--------------------------------------------------------------------*/
int do_pushmatrix3( traverser_state, entity )
BIF_Traverser_state *traverser_state;
BIF_All *entity;

{
	if (matrix_stack_depth < MATRIX_STACK_SIZE )
	{
		/* Push and Copy */
		copy44f(matrix_table[0],
			matrix_stack[matrix_stack_depth]);
		matrix_stack_depth++;
	}
	else
	{
		ERROR("FATAL: Matrix Stack Overflow.");
	}

} /* End do_pushmatrix3() */

/*--------------------------------------------------------------------*\
| Procedure     :	int do_popmatrix3(*BIF_Traverser_state,
|						*BIF_all)
|---------------------------------------------------------------------
| Description   :	Pop the stack, copy the contents of the top of
|			the stack to matrix table entry zero.
|---------------------------------------------------------------------
| Return        :	Error code (Not Implemented)
\*--------------------------------------------------------------------*/
int do_popmatrix3( traverser_state, entity )
BIF_Traverser_state *traverser_state;
BIF_All *entity;

{
	if (matrix_stack_depth > 0 )
	{
		/* Pop and Copy */
		matrix_stack_depth--;
		copy44f( matrix_stack[matrix_stack_depth],
			matrix_table[0]);
	}
	else
	{
		ERROR("FATAL: Matrix Stack Underflow.");
	}

} /* End do_popmatrix3() */

/*----------------------------------------------------------------------*\
| Procedure     :	do_defineviewspecification(*BIF_Traverser_state,
|							*BIF_all)
|------------------------------------------------------------------------
| Description   :	Define an entry in the view table
|------------------------------------------------------------------------
| Return        :	Error code
\*----------------------------------------------------------------------*/
int do_defineviewspecification( traverser_state, entity )
BIF_Traverser_state *traverser_state;
BIF_All *entity;

{
  BIF_Defineviewspecification *vent;
  Pview_rep3 rep;

  /* Set a convenient pointer */
  vent = &entity->defineviewspecification;

/* Set View Specification */
#ifdef USING_PHIGS
	/*------------------------------------------------------------*\
	|	Remap the npc limits of the viewmapping matrix to map
	|	to the ndc limits requested.
	\*------------------------------------------------------------*/
  copy44f( matrix_table[vent->id_view_mapping],rep.map_matrix);
  /* TESTING: vvvvvvv */
  fitViewMapToNDC(vent->ndcMinMax, rep.map_matrix);

  copy44f(matrix_table[vent->id_view_orientation],
	  rep.ori_matrix);
  rep.clip_limit.x_min = vent->ndcMinMax[0];
  rep.clip_limit.x_max = vent->ndcMinMax[1];
  rep.clip_limit.y_min = vent->ndcMinMax[2];
  rep.clip_limit.y_max = vent->ndcMinMax[3];
  rep.clip_limit.z_min = vent->ndcMinMax[4];
  rep.clip_limit.z_max = vent->ndcMinMax[5];
  rep.xy_clip = (Pclip_ind)vent->xy_clip_flag;
  rep.back_clip = (Pclip_ind)vent->back_clip_flag;
  rep.front_clip = (Pclip_ind)vent->front_clip_flag;
	/*------------------------------------------------------------*\
	|	Do the Define the view specification
	\*------------------------------------------------------------*/
  pset_view_rep3((Pint)bench_setup.workid,(Pint)vent->id_view_spec,
	       &rep);
#else /* USING_PHIGS */
/* Insert Native Graphics Calls Here */
#endif /* USING_PHIGS */


	/*------------------------------------------------------------*\
	|	Save "public" copies because GET_MATRIX3 needs it
	\*------------------------------------------------------------*/
  copy44f(matrix_table[vent->id_view_orientation],
	  vo_matrix_public[vent->id_view_spec]);
  copy44f(matrix_table[vent->id_view_mapping],
	  vm_matrix_public[vent->id_view_spec]);

/* Return Error Code */
  return(0);  /* No Error */
} /* End do_defineviewspecification */

/*----------------------------------------------------------------------*\
| Procedure     :	do_defaultviewspecification(*BIF_Traverser_state,
|							*BIF_all)
|------------------------------------------------------------------------
| Description   :	define a default view table entry
|------------------------------------------------------------------------
| Return        :	Error code
\*----------------------------------------------------------------------*/
int do_defaultviewspecification( traverser_state, entity )
BIF_Traverser_state *traverser_state;
BIF_All *entity;

{
  BIF_Defaultviewspecification *vent;

  /* Set a convenient pointer */
  vent = &entity->defaultviewspecification;

	/*------------------------------------------------------------*\
	|	Set View Specification from computed data
	\*------------------------------------------------------------*/
#ifdef USING_PHIGS
	/*------------------------------------------------------------*\
	|	Do the Define the view specification
	\*------------------------------------------------------------*/
  pset_view_rep3((Pint)bench_setup.workid,(Pint)vent->id_view_spec,
	       &vent->rep);
#else /* USING_PHIGS */
/* Insert Native Graphics Calls Here */
#endif /* USING_PHIGS */

	/*------------------------------------------------------------*\
	|	Save "public" copies because GET_MATRIX3 needs it
	\*------------------------------------------------------------*/
  copy44f(vent->rep.ori_matrix,
	  vo_matrix_public[vent->id_view_spec]);
  copy44f(vent->rep.map_matrix,
	  vm_matrix_public[vent->id_view_spec]);


/* Return Error Code */
  return(0);  /* No Error */
} /* End do_defaultviewspecification */

/*----------------------------------------------------------------------*\
| Procedure     :	do_executestructure(*BIF_Traverser_state,
|						*BIF_all)
|------------------------------------------------------------------------
| Description   :	push the traverser state and call the
|			specified structure
|------------------------------------------------------------------------
| Return        :	Error code
\*----------------------------------------------------------------------*/
int do_executestructure( traverser_state, entity )
BIF_Traverser_state *traverser_state;
BIF_All *entity;
{
	BIF_Traverser_state child_state;
	slList callList;

/* Push the attributes */
#ifdef SYSV
	memcpy((char* )&child_state,(char *)traverser_state,
			sizeof(BIF_Traverser_state));
#else
	bcopy((char *)traverser_state,(char* )&child_state,
			sizeof(BIF_Traverser_state));
#endif
	/*------------------------------------------------------------*\
	|	Push around the matrices of the child
	\*------------------------------------------------------------*/
	/* global matrix <- composite matrix */
	copy44f(child_state.composite, child_state.global);
	/* local matrix  <- identity matrix */
	mx_identity(child_state.local);

#ifndef REFER_STRUCTURE_EXISTS
	/*------------------------------------------------------------*\
	|	Code for the CALL_STRUCTURE requirements
	|	Start a new invocation label linked list
	|
	|	Since the labels are unique except for copies (which
	|	may not be recursive) a linked list consisting of the
	|	actual PHIGs structure containing the current instance
	|	of the CALLed structure and the unique starting labels
	|	of the hierarchical CALL invocations, provides a
	|	path to a given label within a CALLed structure.
	\*------------------------------------------------------------*/
	child_state.tol = &callList;
	child_state.eol = &callList;
	callList.next = NULL;
	callList.data = entity->executestructure.structure_id;
#endif /* REFER_STRUCTURE_EXISTS */

	/* Call the traverser */
	(*child_state.handler)(&child_state,
			entity->executestructure.structure_ptr->top_of_list);


/* Return Error Code */
	return(0);  /* No Error */
} /* End do_executestructure */

/*----------------------------------------------------------------------*\
| Procedure     :	do_callstructure(*BIF_Traverser_state, *BIF_all)
|------------------------------------------------------------------------
| Description   :	call the specified structure
|------------------------------------------------------------------------
| Return        :	Error code
\*----------------------------------------------------------------------*/
int do_callstructure( traverser_state, entity )
BIF_Traverser_state *traverser_state;
BIF_All *entity;
{
	slList link;
	slList *saveEol;
#ifndef REFER_STRUCTURE_EXISTS
	/* Code for the CALL_STRUCTURE requirements */
	/* update the invocation label linked list  */
	/* Push the current end of list */
	saveEol = traverser_state->eol;

	/* Link previous' next to current */
	traverser_state->eol->next = &link;

	/* Let children know the current parent */
	traverser_state->eol = &link;
#endif /* REFER_STRUCTURE_EXISTS */

	/* Fill in the data for this link */
	link.next = NULL;
	link.data = entity->callstructure.startLabel;

/* Call the traverser */
	Traverse(traverser_state,
			entity->callstructure.structure_ptr->top_of_list);

#ifndef REFER_STRUCTURE_EXISTS
	/* Restore the previous eol and unlink this call */
	traverser_state->eol = saveEol;
	traverser_state->eol->next = NULL;
#endif /* REFER_STRUCTURE_EXISTS */

/* Return Error Code */
	return(0);  /* No Error */
} /* End do_callstructure */

/*----------------------------------------------------------------------*\
| Procedure     :	do_invokeatframe(*BIF_Traverser_state, *BIF_all)
|------------------------------------------------------------------------
| Description   :	execute invokeatframe editing and conditional BIF
|		traversal of the specified structure in the specified
|		style.
|------------------------------------------------------------------------
| Return        :	Error code
\*----------------------------------------------------------------------*/
int do_invokeatframe( traverser_state, entity )
BIF_Traverser_state *traverser_state;
BIF_All *entity;
{
	BIF_InvokeAtFrame *fent;
	int frameNum;
	fent = &entity->invokeatframe;

	/*------------------------------------------------------------*\
	|	Find out if this an interesting frame to us
	\*------------------------------------------------------------*/
	frameNum = traverser_state->currentFrame;
	if ( 	( fent->startFrame <= frameNum ) &&
		( ( fent->endFrame == -1 ) ||
		  ( frameNum <= fent->endFrame ) ) )
	{
		/*----------------------------------------------------*\
		|	Inovocation IS active
		\*----------------------------------------------------*/
#ifdef USING_PHIGS
		/*----------------------------------------------------*\
		|	Make sure the test-loop knows the status has
		|	changed.
		\*----------------------------------------------------*/
		if ( fent->startFrame == frameNum ) 
		{
			/*--------------------------------------------*\
			|	First frame... do the insert.
			|	Make sure the test-loop PHIGs structure
			|	knows the status has changed.
			\*--------------------------------------------*/
			popen_struct((Pint)bench_setup.test_stid);
			pset_elem_ptr((Pint)0);
			pset_elem_ptr_label((Pint)fent->startLabel);
			if ( fent->invokeStyle == BIF_CALL )
				/*------------------------------------*\
				|	Copy the contents of the CALL'd
				|	structure into the test-loop
				|	structure.
				\*------------------------------------*/
			  pcopy_all_elems_struct((Pint)fent->structureID);
			else
				/*------------------------------------*\
				|	Insert an execute structure
				|	element into the test-loop
				|	structure.
				\*------------------------------------*/
			  pexec_struct((Pint)fent->structureID);
			pclose_struct();
		}
#endif /* USING_PHIGS */

		/* Call the traverser */
/* NOTE: may require supporting code for the CALL_STRUCTURE requirements */
		Traverse(traverser_state, (BIF_All *)&fent->invoke);
	}
#ifdef USING_PHIGS
	else if ( ( fent->endFrame != -1 ) &&
		  ( frameNum == ( fent->endFrame +1 ) ) )
	{
		/*--------------------------------------------*\
		|	First frame after range... delete!!!
		|	Make sure the test-loop PHIGs structure
		|	knows the status has changed.
		\*--------------------------------------------*/
		popen_struct((Pint)bench_setup.test_stid);
		pset_elem_ptr((Pint)0);
		pdel_elems_labels((Pint)fent->startLabel,
				(Pint)fent->endLabel);
		pclose_struct();
	}
#endif /* USING_PHIGS */


/* Return Error Code */
	return(0);  /* No Error */
} /* End do_invokeatframe */

/*--------------------------------------------------------------------*\
| Procedure     :	int fitViewMapToNDC(*float, matrix4);
|---------------------------------------------------------------------
| Description   :	Transform a viewmapping matrix that maps to the
|			unit cube, to map to the ndc region given.
|---------------------------------------------------------------------
| Return        :	Error Code (Not Implemented)
\*--------------------------------------------------------------------*/
int fitViewMapToNDC(ndcMinMax, vmap_mat)
float ndcMinMax[6];
matrix4 vmap_mat;

{
	matrix4 remap, temp;

	/* Define the remmaping matrix (which is just a scale and
		translate matrix) */

	/* Clear it to the identity */
	mx_identity(remap);

	/* The scale part */
	remap[0][0] = ndcMinMax[1] - ndcMinMax[0];
	remap[1][1] = ndcMinMax[3] - ndcMinMax[2];
	remap[2][2] = ndcMinMax[5] - ndcMinMax[4];

	/* The translate part, needs to be a transpose for PEX */
	remap[0][3] = ndcMinMax[0];
	remap[1][3] = ndcMinMax[2];
	remap[2][3] = ndcMinMax[4];

	/* Update vmap_mat, needs to be multiplied as a transpose */
	mx_mult(remap,vmap_mat,temp);
	copy44f(temp,vmap_mat);

} /* End fitViewMapToNDC() */
