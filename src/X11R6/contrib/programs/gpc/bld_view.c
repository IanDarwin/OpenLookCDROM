/* $XConsortium: bld_view.c,v 5.3 94/04/17 20:44:25 hersh Exp $ */
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
supporting documentation, and Sun Microsystems
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
| Author        :	John M. Zulauf
|
| File          :	bld_view.c
| Date          :	Thu Jun 29 20:28:01 PDT 1989
| Project       :	PLB
| Description   :	The build, store/execute functions for "view"
|					entities
| Status        :	Version 1.0
|
| Revisions     :
|
|	6/89		Staff SimGEC: Extracted from bifbuild.c
|			(PLBDEMO), ANSI/ISO PHIGs Viewing Calls
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
|	int bif_vieworient3(BIF_INT, BIF_REAL, BIF_REAL, BIF_REAL)
|		:	Receive a VIEW_ORIENTATION3 entity from the parser
|	int bif_viewmap3(BIF_INT)
|		:	Begin / End receiving a VIEW_MAPPING3 entity from
|	int bif_viewmapbasic(	BIF_INT, BIF_REAL, BIF_REAL, BIF_REAL,
|				BIF_REAL, BIF_INT, *BIF_REAL, BIF_REAL,
|				BIF_REAL)
|		:	Receive basic data for a VIEW_MAPPING3 entity
|	int bif_viewmapmatch(	BIF_REAL, BIF_REAL, BIF_REAL, BIF_REAL,
|				BIF_INT )
|		:	Receive aspect ratio match data for VIEW_MAPPING3 
|	int bif_viewspec(BIF_INT, BIF_INT, BIF_INT, BIF_INT, BIF_INT,
|				BIF_INT, BIF_REAL, BIF_REAL, BIF_REAL,
|				BIF_REAL)
|		:	Receive a SET_VIEW_SPECIFICATION entity from the parser
|	int bif_defviewspec(BIF_INT, BIF_REAL, BIF_INT )
|		:	Receive a DEFAULT_VIEW_SPECIFICATION entity from
|	int bif_activeview(BIF_INT)
|		:	Receive an ACTIVE_VIEW entity from the parser
|
\*--------------------------------------------------------------------*/

/*---------------------------------------------------------------------*\
|	Include files 
\*--------------------------------------------------------------------- */
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


/*--------------------------------------------------------------------*\
|Local #define
\*--------------------------------------------------------------------*/

/*--------------------------------------------------------------------*\
| External Symbols
\*--------------------------------------------------------------------*/
int noop_function();

/*--------------------------------------------------------------------*\
| Local global variables
\*--------------------------------------------------------------------*/
/* Useful statics */
/* Temporary entity storage */
static BIF_All temp_ent;
static BIF_All temp_ent2;

/*----------------------------------------------------------------------*\
| Procedure	:	int bif_vieworient3(BIF_INT, BIF_REAL, BIF_REAL,
|						BIF_REAL)
|------------------------------------------------------------------------|
| Description	:	Receive a VIEW_ORIENTATION3 entity from the parser
|
|	matrix_id	ID of matrix table entry hold result
|	vrp		View reference point
|	vpn		View plane normal
|	vup		View up vector
|------------------------------------------------------------------------|
| Return	:	Error Code
\*----------------------------------------------------------------------*/
int bif_vieworient3(matrix_id, vrp, vpn, vup)
BIF_INT matrix_id;
BIF_REAL  vrp[3], vpn[3], vup[3];

{
	static int ent_size = sizeof(BIF_Matrix3);
	BIF_All *ent;
	float f_vrp[3], f_vpn[3], f_vup[3];

#ifdef TEST_PRINT
	printf("VIEW_ORIENTATION3: id %d\n", matrix_id);
	printf("view_ref  %f %f %f\n", vrp[0], vrp[1], vrp[2]);
	printf("view_norm %f %f %f\n", vpn[0], vpn[1], vpn[2]);
	printf("view_up   %f %f %f\n", vup[0], vup[1], vup[2]);
#endif /* TEST_PRINT */
#ifndef PRINT_ONLY
	/* Check the matrix_id */
	ERROR_MATRIX_ID(matrix_id,VIEW_ORIENTATION3);

	/* Fill the buffer */
	temp_ent.matrix3.matrix_id   = matrix_id;
	temp_ent.matrix3.concat_type = BIF_REPLACE;

	/* Compute the View orientation matrix */
	Cpvec3f(vrp, f_vrp);
	Cpvec3f(vpn, f_vpn);
	Cpvec3f(vup, f_vup);
	compute_vieworient_matrix(f_vrp, f_vpn, f_vup,
		temp_ent.matrix3.matrix);

/* Allocate the entity */
	ent = new_generic(&temp_ent, ent_size, MATRIX3, do_matrix3);

/* Error check for ent == NULL ( FATAL ) */
	ENT_ERROR(ent);

/* Build or Execute */
	Traverse(traverser_state, ent);

#ifdef USING_PHIGS
/* Matrix Table Function Only */
/* No PHIGS call here */
#endif /* USING_PHIGS */

/* Release Non-Retained Entities */
	Free_NRE(traverser_state, ent);

#endif /* PRINT_ONLY */
} /* End procedure bif_vieworient3 */

/*----------------------------------------------------------------------*\
| Procedure	:	int bif_viewmap3(BIF_INT)
|------------------------------------------------------------------------|
| Description	:	Begin / End receiving a VIEW_MAPPING3 entity from
|			the parser.  Store the received data in the
|			entity structure.
|
|	BIF_P_BEGIN 	begin entity
|	BIF_P_END	end entity
|------------------------------------------------------------------------|
| Return	:	Error Code
\*----------------------------------------------------------------------*/
int bif_viewmap3(begin_or_end)
BIF_INT begin_or_end;
{
  static int ent_size = sizeof(BIF_Matrix3);
  static float npcMinMax[6] = 
    {
      0., 1.,
      0., 1.,
      0., 1.
      };

  BIF_All *ent;
  int ierrind;
  Pview_map3 mapping;
	
#ifdef TEST_PRINT
  BEGEND(viewmap3);
#endif /* TEST_PRINT */
#ifndef PRINT_ONLY
  switch ( begin_or_end )
    {
    case BIF_P_BEGIN :
      /* Initialize entity */
      temp_ent.viewmapping3.match_type = GROW;
      temp_ent.viewmapping3.match_aspect = wk_info.aspect_ratio;
      break;
      
    case BIF_P_END :
      /* Adjust the UV window of for correct images */
      adjust_window(temp_ent.viewmapping3.uvMinMax,
		    temp_ent.viewmapping3.match_type,
		    temp_ent.viewmapping3.match_aspect);
      
      /* Computed & store the result using the Matrix3 entity */
      temp_ent2.matrix3.matrix_id =
	temp_ent.viewmapping3.matrix_id;
      temp_ent2.matrix3.concat_type = BIF_REPLACE;
#ifdef USING_PHIGS
		/*----------------------------------------------------*\
		|	Call the evaluate view mapping matrix utility
		|	The view mapping matrices are generated with an
		|	assumed a unit cube npc mapping.
		|	This matrix will be altered to map to the
		|	correct area of the workstation window when 
		|	the view specification call is made.
		\*----------------------------------------------------*/
      mapping.win.x_min = temp_ent.viewmapping3.uvMinMax[0];
      mapping.win.x_max = temp_ent.viewmapping3.uvMinMax[1];
      mapping.win.y_min = temp_ent.viewmapping3.uvMinMax[2];
      mapping.win.y_max = temp_ent.viewmapping3.uvMinMax[3];
      mapping.proj_vp.x_min = npcMinMax[0];
      mapping.proj_vp.x_max = npcMinMax[1];
      mapping.proj_vp.y_min = npcMinMax[2];
      mapping.proj_vp.y_max = npcMinMax[3];
      mapping.proj_vp.z_min = npcMinMax[4];
      mapping.proj_vp.z_max = npcMinMax[5];
      mapping.proj_type = (Pproj_type)temp_ent.viewmapping3.proj_type;
      mapping.proj_ref_point.x = temp_ent.viewmapping3.proj_reference[0];
      mapping.proj_ref_point.y = temp_ent.viewmapping3.proj_reference[1];
      mapping.proj_ref_point.z = temp_ent.viewmapping3.proj_reference[2];
      mapping.view_plane = 0.0;
      mapping.back_plane = temp_ent.viewmapping3.back_plane;
      mapping.front_plane = temp_ent.viewmapping3.front_plane;
      peval_view_map_matrix3(&mapping,&ierrind,temp_ent2.matrix3.matrix);
#else /* USING_PHIGS */
		/*----------------------------------------------------*\
		|	Native Graphics Routine to generate viewmapping
		|	matrix here.
		\*----------------------------------------------------*/
      /* So it has a defined value for testing */
      mx_identity( temp_ent2.matrix3.matrix );
#endif /* USING_PHIGS */

      /* Allocate the entity */
      ent = new_generic(&temp_ent2, ent_size, MATRIX3, do_matrix3);

      /* Error check for ent == NULL ( FATAL ) */
      ENT_ERROR(ent);

      /* Build or Execute */
      Traverse(traverser_state, ent);

      /* Release Non-Retained Entities */
      Free_NRE(traverser_state, ent);

      break;
    }

#endif /* PRINT_ONLY */
} /* End procedure bif_viewmap3 */

/*----------------------------------------------------------------------*\
| Procedure	:	int bif_viewmapbasic(BIF_INT, BIF_REAL, BIF_REAL,
|						BIF_REAL, BIF_REAL,
|						BIF_INT, *BIF_REAL,
|						BIF_REAL, BIF_REAL)
|------------------------------------------------------------------------|
| Description	:	Receive basic data for a VIEW_MAPPING3 entity
|			from the parser.
|
|	matrix_id	ID of matrix table entry hold result
|	x_min, x_max,
|	y_min, y_max	UV window
|	proj_type	Projection type: PERSPECTIVE | PARALLEL
|	prp		Projection reference point
|	front_plane	Front Clipping Location ( VRC )
|	back_plane	Back Clipping Location ( VRC )
|------------------------------------------------------------------------|
| Return	:	Error Code
\*----------------------------------------------------------------------*/
int bif_viewmapbasic(matrix_id, x_min, x_max, y_min, y_max, proj_type, prp, 
			front_plane, back_plane)
BIF_INT matrix_id;
BIF_REAL  x_min, x_max, y_min, y_max;
BIF_INT proj_type;
BIF_REAL  prp[3];
BIF_REAL front_plane, back_plane;
{
#ifdef TEST_PRINT
	printf("viewmapbasic: id %d\n", matrix_id);
	printf("x_window  %f %f\n", x_min, x_max);
	printf("y_window  %f %f\n", y_min, y_max);
	printf("proj type %d\n", proj_type);
	printf("proj ref  %f %f %f\n", prp[0], prp[1], prp[2]);
	printf("clipping  %f %f\n", front_plane, back_plane);
#endif /* TEST_PRINT */
#ifndef PRINT_ONLY
/* Check the matrix_id */
	ERROR_MATRIX_ID(matrix_id,VIEW_MAPPING3);

	temp_ent.viewmapping3.matrix_id = matrix_id;
	temp_ent.viewmapping3.uvMinMax[0] = x_min;
	temp_ent.viewmapping3.uvMinMax[1] = x_max;
	temp_ent.viewmapping3.uvMinMax[2] = y_min;
	temp_ent.viewmapping3.uvMinMax[3] = y_max;
	temp_ent.viewmapping3.proj_type = REMAP_PROJ(proj_type);
	Cpvec3f(prp, temp_ent.viewmapping3.proj_reference);
	temp_ent.viewmapping3.front_plane     = front_plane;
	temp_ent.viewmapping3.back_plane      = back_plane;
#endif /* PRINT_ONLY */
} /* End procedure bif_viewmapbasic */

/*----------------------------------------------------------------------*\
| Procedure	:	int bif_viewmapmatch(BIF_REAL, BIF_REAL,
|						BIF_REAL, BIF_REAL,
|						BIF_INT )
|------------------------------------------------------------------------|
| Description	:	Receive aspect ratio match data for VIEW_MAPPING3 
|			entity from the parser.
|
|	vw_area_xmin, vw_area_xmax, vw_area_ymin, vw_area_ymax
|			Dimensions of view-area to match
|	match_type	ADJUST_X | ADJUST_Y | GROW | SHRINK
|------------------------------------------------------------------------|
| Return	:	Error Code
\*----------------------------------------------------------------------*/
int bif_viewmapmatch(vw_area_xmin, vw_area_xmax, vw_area_ymin,
				vw_area_ymax, match_type)
BIF_REAL vw_area_xmin, vw_area_xmax;
BIF_REAL vw_area_ymin, vw_area_ymax;
BIF_INT match_type;
{
	float u_size, v_size;
#ifdef TEST_PRINT
	printf("viewmapmatch:");
	printf("x_view_area  %f %f\n", vw_area_xmin, vw_area_xmax);
	printf("y_view_area  %f %f\n", vw_area_ymin, vw_area_ymax);
	printf("match type  %d\n", match_type);
#endif /* TEST_PRINT */
#ifndef PRINT_ONLY
	temp_ent.viewmapping3.match_type    = match_type;
	/* Aspect Ratio To Match */
	u_size =  vw_area_xmax - vw_area_xmin;
	v_size =  vw_area_ymax - vw_area_ymin;
	v_size = (v_size != 0.) ? v_size : 1.0 ;
	temp_ent.viewmapping3.match_aspect = (u_size / v_size) *
		wk_info.aspect_ratio;

#endif /* PRINT_ONLY */
} /* End procedure bif_viewmapmatch */

/*----------------------------------------------------------------------*\
| Procedure	:	int bif_viewspec(BIF_INT, BIF_INT, BIF_INT, 
|		 		BIF_INT, BIF_INT,BIF_INT,
|				BIF_REAL, BIF_REAL, BIF_REAL, BIF_REAL)
|------------------------------------------------------------------------|
| Description	:	Receive a SET_VIEW_SPECIFICATION entity from the parser
|
|	id_view_spec		View Table entry ID
|	id_view_orientation	ID ofstored view orientation matrix
|	id_view_mapping		ID of stored dview mapping  matrix
|	xy_clip_flag		Enable/Disable XY Clipping
|	front_clip_flag		Enable / Disable Front Clipping
|	back_clip_flag		Enable / Disable Back Clipping
|	vw_area_xmin vw_area_xmax	X extrema of view area
|	vw_area_ymin vw_area_ymax	Y extrema of view area
|------------------------------------------------------------------------|
| Return	:	Error Code
\*----------------------------------------------------------------------*/
int bif_viewspec(id_view_spec, id_view_orientation, id_view_mapping, 
	 	xy_clip_flag, front_clip_flag, back_clip_flag,
		vw_area_xmin, vw_area_xmax, vw_area_ymin, vw_area_ymax)
BIF_INT id_view_spec, id_view_orientation, id_view_mapping; 
BIF_INT xy_clip_flag, front_clip_flag, back_clip_flag;
BIF_REAL vw_area_xmin, vw_area_xmax, vw_area_ymin, vw_area_ymax;

{
	static int ent_size = sizeof(BIF_Defineviewspecification);
	BIF_Defineviewspecification *vent;
	BIF_All *ent;
	int ierr;
		
#ifdef TEST_PRINT
	printf("SET_VIEW_SPECIFICATION : view id %d\n", id_view_spec);
	printf("view and mapping matrix id %d %d\n",
			id_view_orientation, id_view_mapping);
	printf("clip flags  %d %d %d\n",
			xy_clip_flag, front_clip_flag, back_clip_flag);
	printf("x_view_area  %f %f\n", vw_area_xmin, vw_area_xmax);
	printf("y_view_area  %f %f\n", vw_area_ymin, vw_area_ymax);
#endif /* TEST_PRINT */
#ifndef PRINT_ONLY
	/* Check the matrix ID's */
	ERROR_MATRIX_ID(id_view_orientation,DEFINE_VIEW_SPECIFICATION);
	ERROR_MATRIX_ID(id_view_mapping,DEFINE_VIEW_SPECIFICATION);

	/* ************************************** */
	/* Fill the View Specification Parameters */
	/* ************************************** */
	vent = &temp_ent.defineviewspecification;
	vent->id_view_spec = id_view_spec;
	vent->id_view_orientation = id_view_orientation;
	vent->id_view_mapping     = id_view_mapping;

	/* Clipping Flags:	Check the Remaps against the ENUMs */
	vent->xy_clip_flag        = REMAP_CLIP(xy_clip_flag);
	vent->front_clip_flag     = REMAP_CLIP(front_clip_flag);
	vent->back_clip_flag      = REMAP_CLIP(back_clip_flag);

	/* View area : Normalized BIF Display Coord. */
	if ( wk_info.aspect_ratio != 1.0) {
	    if ( wk_info.aspect_ratio < 1.0) { /* Shrink X */
		vent->ndcMinMax[0] = vw_area_xmin * wk_info.aspect_ratio;
		vent->ndcMinMax[1] = vw_area_xmax * wk_info.aspect_ratio;
		vent->ndcMinMax[2] = vw_area_ymin;
		vent->ndcMinMax[3] = vw_area_ymax;
	    }
	    else { /* Shrink Y */
		vent->ndcMinMax[0] = vw_area_xmin;
		vent->ndcMinMax[1] = vw_area_xmax;
		vent->ndcMinMax[2] = vw_area_ymin / wk_info.aspect_ratio;
		vent->ndcMinMax[3] = vw_area_ymax / wk_info.aspect_ratio;
	    }
	}
	else {
	    vent->ndcMinMax[0] = vw_area_xmin;
	    vent->ndcMinMax[1] = vw_area_xmax;
	    vent->ndcMinMax[2] = vw_area_ymin;
	    vent->ndcMinMax[3] = vw_area_ymax;
	}
	vent->ndcMinMax[4] = 0.;
	vent->ndcMinMax[5] = 1.;

	/* Can Use but NOT set view index 0 */
	ierr = indexRange(DEFINE_VIEW_SPECIFICATION,
			  (BIF_INT)vent->id_view_spec ,
			  1, VIEW_TABLE_SIZE );

	if ( ierr ) /* Substitue default value */
	    vent->id_view_spec = 1;

	/* Allocate the entity */
	ent = new_generic( &temp_ent, ent_size,
			  DEFINE_VIEW_SPECIFICATION,
			  do_defineviewspecification );

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
} /* End procedure bif_viewspec */

/*----------------------------------------------------------------------*\
| Procedure	:	int bif_defviewspec(BIF_INT, BIF_REAL, BIF_INT )
|------------------------------------------------------------------------|
| Description	:	Receive a DEFAULT_VIEW_SPECIFICATION entity from
|			the parser.
|
|	id_view_spec	View Table entry ID
|	radius_of_view	Radius of visible sphere
|	proj_type	Projection type: BIF_PERSPECTIVE | BIF_PARALLEL
|------------------------------------------------------------------------|
| Return	:	Error Code
\*----------------------------------------------------------------------*/
int bif_defviewspec(id_view_spec, radius_of_view, proj_type )
BIF_INT id_view_spec;
BIF_REAL radius_of_view;
BIF_INT proj_type; 

{
	static int ent_size = sizeof(BIF_Defaultviewspecification);
	BIF_All *ent;
	BIF_Defaultviewspecification *vent;
	int ierr;

	ent_size += sizeof(Pview_rep3);
#ifdef TEST_PRINT
	printf("DEFAULT_VIEW_SPECIFICATION: viewid %d radius %f proj_type %d\n",
			id_view_spec, radius_of_view, proj_type );
#endif /* TEST_PRINT */
#ifndef PRINT_ONLY
	/* Allocate the entity */
	/* Store the supplied data */
	vent = &temp_ent.defaultviewspecification;
	vent->id_view_spec = id_view_spec;
	vent->radius_of_view= radius_of_view;
	vent->proj_type= REMAP_PROJ(proj_type);

/* Compute the rest of the data */
	compute_default_view((BIF_Defaultviewspecification *)&temp_ent,
			     wk_info.aspect_ratio);

	/* Can Use but NOT set view index 0 */
	ierr = indexRange(DEFAULT_VIEW_SPECIFICATION,
			  (BIF_INT)vent->id_view_spec ,
			  1, VIEW_TABLE_SIZE );

	if ( ierr ) /* Substitute default value */
	    vent->id_view_spec = 1;

	ent = new_generic( &temp_ent, ent_size,
			  DEFAULT_VIEW_SPECIFICATION,
			  do_defaultviewspecification );

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
} /* End procedure bif_defviewspec */

/*----------------------------------------------------------------------*\
| Procedure	:	int bif_activeview(BIF_INT)
|------------------------------------------------------------------------|
| Description	:	Receive an ACTIVE_VIEW entity from the parser
|
|	id_view_spec	View Table entry ID to select
|------------------------------------------------------------------------|
| Return	:	Error Code
\*----------------------------------------------------------------------*/
int bif_activeview(id_view_spec)
BIF_INT id_view_spec;

{
	int ierr;
	/* Build, Store/Execute the entity */
	/* Check the view ie against the range of valid values */
	/* Can Use but NOT set view index 0 */
	ierr = indexRange(ACTIVE_VIEW,
			  (BIF_INT)id_view_spec , 0, VIEW_TABLE_SIZE );

	if ( ierr ) /* Substitute default value */
	    id_view_spec = 1;

	bif_index((int)id_view_spec, sizeof(BIF_Index),
		  ACTIVE_VIEW, do_activeview, pset_view_ind );

} /* End procedure bif_activeview */


