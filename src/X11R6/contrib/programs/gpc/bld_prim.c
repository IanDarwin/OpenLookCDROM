/* $XConsortium: bld_prim.c,v 5.4 94/04/17 20:44:24 hersh Exp $ */
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

/*#define TEST_PRINT*/
/*#define TEST_PRINT2*/
#define PPRECCHECK 1
/*#define PRINT_ONLY*/
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
| File          :	bld_prim.c
| Date          :	Tue Jun 20 11:27:08 PDT 1989
| Project       :	PLB
| Description   :	entity build/exectute functions for the graphics
|			primtive entities
| Status        :	Version 1.0
|
| Revisions     :
|	1/21/89 	Paul Chek DEC:
|			- insert an additional argument in call to
|			pfad3 to specify the facet color. It works for
|			DEC PHIGS+, but not with Alliant GX4000 PHIGS+.
|
|	1/89		Staff SimGEC: Various bug fixes.
|
|	2/89		MFR/JMZ SimGEC: Rewrite of bif_pixelmap3 
|			"guts" of pixmap output moved to fakefigs.c
|
|	2/89		MJF SimGEC: Added NUB support 
|			
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
|	int bif_marker(BIF_INT)
|		:	Begin / End receiving a MARKER entity
|	int bif_marker3(BIF_INT)
|		:	Begin / End receiving a MARKER3 entity
|	int bif_line(BIF_INT)
|		:	Begin / End receiving a LINE entity from the parser
|	int bif_line3(BIF_INT)
|		:	Begin / End receiving a LINE3 entity from the parser
|	int bif_polygon(BIF_INT)
|		:	Begin / End receiving a POLYGON entity from the parser
|	int bif_polygon3(BIF_INT)
|		:	Begin / End receiving a POLYDATA3 entity from the parser
|	int bif_faset(BIF_INT)
|		:	Begin / End receiving a FILLAREASET entity from the
|	int bif_faset3(BIF_INT)
|		:	Receive a FILLAREASET3 entity from the parser.
|	int bif_triangle3(BIF_INT)
|		:	Begin / End receiving a TRIANGLE3 entity from the parser
|	int bif_quad_mesh3(BIF_INT)
|		:	Begin / End receiving a QUADMESH3 entity from the parser
|	int bif_quadmeshorder(BIF_INT, BIF_INT)
|		:	Receive the size of the current quadmesh from
|	int bif_indexpolygons3(BIF_INT)
|		:	Receive a INDEX_POLYGONS3 entity from the parser. 
|	int bif_text(BIF_REAL, BIF_REAL, * char)
|		:	Receive a TEXT entity from the parser
|	int bif_text3(BIF_REAL, BIF_REAL, BIF_REAL, *char, int, BIF_REAL, 
|		BIF_REAL, BIF_REAL, BIF_REAL, BIF_REAL, BIF_REAL)
|		:	Receive a TEXT3 entity from the parser
|	int bif_annotationtext(BIF_REAL, BIF_REAL, BIF_REAL, BIF_REAL, 
|		BIF_REAL, BIF_REAL, *char)
|		:	Receive a ANNOTATION_TEXT3 entity from the parser
|	int bif_sphere3(BIF_INT)
|		:	Begin / End receiving a SPHERE3 entity
|	int bif_gencircle(BIF_INT)
|		:	Begin / End receiving a CIRCLE entity
|	int bif_gencircle3(BIF_INT)
|		:	Begin / End receiving a CIRCLE entity
|	int bif_sphereorder(BIF_INT, BIF_INT)
|		:	Receive the size of the current sphere from
|	int bif_circleedges(BIF_INT)
|		:	Receive the size of the current circle(3) from
|	int bif_exact(BIF_INT)
|		:	Set the GEN_* exact parameter.
|	int bif_center(BIF_REAL, BIF_REAL, BIF_REAL)
|		:	Set the GEN_* center parameter.
|	int bif_radius(BIF_REAL);
|		:	Set the GEN_* radius parameter.
|	int bif_scalefactors(BIF_REAL, BIF_REAL, BIF_REAL)
|		:	Set the GEN_* scale parameter.
|	int bif_normal(BIF_REAL, BIF_REAL, BIF_REAL)
|		:	Set the GEN_* normal parameter.
|	int bif_pixelmap3(BIF_INT)
|		:	Begin / End receiving a PIXEL_MAP3 entity from
|	int bif_nubc (BIF_INT)
| 		:	Begin / End receiving a NON_UNIFORM_BSPLINE_CURVE 
|	int bif_nubs (BIF_INT) 
| 		:	Begin / End receiving a NON_UNIFORM_BSPLINE_surfaces 
|	int bif_nubcholder (BIF_INT,BIF_INT) 
| 		:	Receive the rational/non_rationsl flag for 
|	int bif_nubsholder(BIF_INT,BIF_INT,BIF_INT,BIF_INT,BIF_INT ) 
| 		:	Receive the rational/non_rationsl flag for 
|
\*--------------------------------------------------------------------*/

/*--------------------------------------------------------------------*\
|	Include files 
\*--------------------------------------------------------------------*/
#include <stdio.h>
#include <X11/Xosdefs.h>
#ifndef X_NOT_STDC_ENV
#include <stdlib.h>
#else
char *malloc();
#endif
#if defined(macII) && !defined(__STDC__)  /* stdlib.h fails to define these */
char *malloc();
#endif /* macII */
#include "biftypes.h"
#include "bifbuild.h"
#include "new_ents.h"
#include "bifparse.h"
#include "db_tools.h"
#include "doentity.h"
#include "bifmacro.h"
#include "globals.h"
#include "ph_map.h"

/*--------------------------------------------------------------------*\
|Local #define
\*--------------------------------------------------------------------*/
#ifndef FALSE
#define FALSE 0
#endif
#ifndef TRUE
#define TRUE (!FALSE)
#endif

/*--------------------------------------------------------------------*\
|	Local Macros
\*--------------------------------------------------------------------*/

/*--------------------------------------------------------------------*\
|	Local Globals
\*--------------------------------------------------------------------*/
static BIF_All temp_ent;
static int numRows, numColumns;
static int static_iorder;
static int static_rational_non;
static int static_iuorder;
static int static_ivorder;
static int static_m;
static int static_n;

/*--------------------------------------------------------------------*\
| Procedure     :	int bif_marker(BIF_INT)
|---------------------------------------------------------------------
| Description   :	Begin / End receiving a MARKER entity
|	BIF_P_BEGIN 	Initialize a triple list for marker locations
|	BIF_P_END	Store the point list in the entity structure
|---------------------------------------------------------------------
| Return        :	Error Code (Not Implemented)
\*--------------------------------------------------------------------*/

int bif_marker(begin_or_end)
BIF_INT begin_or_end;
{
	int size;
	BIF_Marker *ent;
	Real_int_union *trip_list;
#ifdef TEST_PRINT
	BEGEND(marker);
#endif /* TEST_PRINT */
#ifndef PRINT_ONLY
	switch (begin_or_end)
	{
	case BIF_P_BEGIN:
	/* Start of a MARKER */
		init_triplets();
		break;

	case  BIF_P_END:
		size = end_triplets(&trip_list);
		ent = new_marker(size,trip_list);
		ENT_ERROR(ent);

	/* Store or execute */		
		Traverse(traverser_state, ent);

	/* Send to PHIGs */
#ifdef USING_PHIGS
		{
		    Ppoint_list points;

		    points.num_points = ent->number;
		    points.points = ent->points;
		    ppolymarker(&points);
		}
#endif /* USING_PHIGS */
		Free_NRE(traverser_state, ent);
		break;
	}
#endif /* PRINT_ONLY */
} /* End procedure bif_marker */

/*--------------------------------------------------------------------*\
| Procedure     :	int bif_marker3(BIF_INT)
|---------------------------------------------------------------------
| Description   :	Begin / End receiving a MARKER3 entity
|	BIF_P_BEGIN 	Initialize a triple list for marker locations
|	BIF_P_END	Store the point list in the entity structure
|---------------------------------------------------------------------
| Return        :	Error Code (Not Implemented)
\*--------------------------------------------------------------------*/

int bif_marker3(begin_or_end)
BIF_INT begin_or_end;
{
	int size;
	BIF_Marker3 *ent;
	Real_int_union *trip_list;
#ifdef TEST_PRINT
	BEGEND(marker3);
#endif /* TEST_PRINT */
#ifndef PRINT_ONLY
	switch (begin_or_end)
	{
	case BIF_P_BEGIN:
	/* Start of a MARKER3 */
		init_triplets();
		break;

	case BIF_P_END:
		size = end_triplets(&trip_list);
		ent = new_marker3(size,trip_list);
		ENT_ERROR(ent);

	/* Store or execute */		
		Traverse(traverser_state, ent);

	/* Send to PHIGs */
#ifdef USING_PHIGS
		{
		    Ppoint_list3 points;

		    points.num_points = ent->number;
		    points.points = ent->points;
		    ppolymarker3(&points);
		}
#endif /* USING_PHIGS */
		Free_NRE(traverser_state, ent);
		break;
	}
#endif /* PRINT_ONLY */
} /* End procedure bif_marker3 */

/*--------------------------------------------------------------------*\
| Procedure	:	int bif_line(BIF_INT)
|---------------------------------------------------------------------
| Description	:	Begin / End receiving a LINE entity from the parser
|		BIF_P_BEGIN 	Init a triple list for line vertices
|		BIF_P_END 	Store the vertices in the entity
|---------------------------------------------------------------------
| Return	:	Error Code (Not Implemented)
\*--------------------------------------------------------------------*/
int bif_line(begin_or_end)
BIF_INT begin_or_end;

{
	int size;
	BIF_Line *ent;
	Real_int_union *trip_list;
#ifdef TEST_PRINT
	BEGEND(line);
#endif /* TEST_PRINT */
#ifndef PRINT_ONLY
	switch (begin_or_end)
	{
	case BIF_P_BEGIN:
	/* Start of a LINE */
		init_triplets();
		break;

	case BIF_P_END:
		size = end_triplets(&trip_list);
		ent = new_line(size,trip_list);
		ENT_ERROR(ent);

	/* Store or execute */		
		Traverse(traverser_state, ent);

#ifdef USING_PHIGS
		{ 
		    Ppoint_list points;

		    points.num_points = ent->number;
		    points.points = ent->points;
		    ppolyline(&points);
		}
#endif /* USING_PHIGS */
		Free_NRE(traverser_state, ent);

	}
#endif /* PRINT_ONLY */
} /* End procedure bif_line */

/*--------------------------------------------------------------------*\
| Procedure	:	 int bif_line3(BIF_INT)
|---------------------------------------------------------------------
| Description	:	Begin / End receiving a LINE3 entity from the parser
|		BIF_P_BEGIN 	Init the group list for LINE3 data
|		BIF_P_END 	Store the LINE3 data in the entity
|
|		LINE3 may contain per vertex color data.
|---------------------------------------------------------------------
| Return	:	Error Code (Not Implemented)
\*--------------------------------------------------------------------*/
int bif_line3(begin_or_end)
BIF_INT begin_or_end;

{

	BIF_Line3 *ent;

	int num_groups;
	Group_description *group_list;

#ifdef TEST_PRINT
	BEGEND(line3);
#endif /* TEST_PRINT */
#ifndef PRINT_ONLY
	switch (begin_or_end)
	{
	case BIF_P_BEGIN:
	/* Start of a LINE3 */
		init_groups();
		break;

	case BIF_P_END:
		num_groups = end_groups(&group_list);
		ent = new_line3(num_groups, group_list);
#ifdef TEST_PRINT
printf("line3: %d groups\n", num_groups);
#endif /* TEST_PRINT */
		ENT_ERROR(ent);

	/* Store or execute */		
		Traverse(traverser_state, ent);

#ifdef USING_PHIGS
		/* Send to PHIGs */

		if (ent->vertex_data_flag)
		    ppolyline_set3_data(ent->vertex_data_flag,
					ent->colour_model,1,&ent->vdata);
		else {
		    Ppoint_list3 points;

		    points.num_points = ent->vdata.num_vertices;
		    points.points = ent->vdata.vertex_data.points;

		    ppolyline3(&points);
		}

#endif /* USING_PHIGS */
		Free_NRE(traverser_state, ent);
	}
#endif /* PRINT_ONLY */
} /* End procedure bif_line3 */

/*----------------------------------------------------------------------*\
| Procedure	:	int bif_polygon(BIF_INT)
|------------------------------------------------------------------------|
| Description	:	Begin / End receiving a POLYGON entity from the parser
|	BIF_P_BEGIN 	begin entity
|		Initialize a triple list for incoming polygon contour
|	BIF_P_END	end entity
|		Store the received point list in the entity structure
|------------------------------------------------------------------------|
| Return	:	Error Code
\*----------------------------------------------------------------------*/
int bif_polygon(begin_or_end)
BIF_INT begin_or_end;
{
	int size;
	BIF_Polygon *ent;
	Real_int_union *trip_list;
#ifdef TEST_PRINT
	BEGEND(polygon);
#endif /* TEST_PRINT */
#ifndef PRINT_ONLY
	switch (begin_or_end)
	{
	case BIF_P_BEGIN:
	/* Start of a POLYGON */
		init_triplets();
		break;
	case BIF_P_END:
		size = end_triplets(&trip_list);
		ent = new_polygon(size,trip_list);
		ENT_ERROR(ent);

	/* Store or execute */		
		Traverse(traverser_state, ent);

	/* Send to PHIGs */
#ifdef USING_PHIGS
		{
		    Ppoint_list_list poly_list_list;
		    Ppoint_list poly_list;
		    poly_list_list.num_point_lists = (Pint)1;
		    poly_list.num_points = ent->number;
		    poly_list.points = ent->points;
		    poly_list_list.point_lists = &poly_list;
		    pfill_area_set(&poly_list_list);
		}
#endif /* USING_PHIGS */
		Free_NRE(traverser_state, ent);
	}
#endif /* PRINT_ONLY */
} /* End procedure bif_polygon */

/*----------------------------------------------------------------------*\
| Procedure	:	int bif_polygon3(BIF_INT)
|------------------------------------------------------------------------|
| Description	:	Begin / End receiving a POLYDATA3 entity from the parser
|	BIF_P_BEGIN 	begin entity
|		Initialize group handler for incoming polydata contour
|		and optional data groups:
|		VERTEX_COLORS, VERTEX_NORMALS, FACET_COLORS, 
|		FACET_NORMALS 
|	BIF_P_END		end entity
|		Store the received point list and optional data in the
|		entity structure.
|------------------------------------------------------------------------|
| Return	:	Error Code
\*----------------------------------------------------------------------*/
int bif_polygon3(begin_or_end)
BIF_INT begin_or_end;
{
	BIF_Polygon3 *ent;
	Group_description *group_list;
	int num_groups;

#ifdef TEST_PRINT
	BEGEND(polygon3);
#endif /* TEST_PRINT */
#ifndef PRINT_ONLY
	switch (begin_or_end)
	{
	case BIF_P_BEGIN:
	/* Start of a polygon3 */
		init_groups();
		break;

	case BIF_P_END:
		num_groups = end_groups(&group_list);
#ifdef TEST_PRINT2
fflush(stderr);
printf("polygon3: %d groups\n", num_groups);
fflush(stdout);
#endif /* TEST_PRINT2 */
		ent = new_polygon3(num_groups, group_list);
		ENT_ERROR(ent);

	/* Store or execute */		
		Traverse(traverser_state, ent);

	/* Send to PHIGs */
#ifdef USING_PHIGS
#ifdef TEST_PRINT2
printf("flags %d %d %d\n",
	ent->facet_data_flag, ent->vertex_data_flag, ent->colour_model );
printf("numFacets %d\n", ent->numFacets);
#endif /* TEST_PRINT */
		{
		    Pfacet_vdata_list3 vdata;

		    vdata.num_vertices = ent->numCoords;
		    vdata.vertex_data = ent->vdata;
		    pfill_area_set3_data(ent->facet_data_flag,0,
					 ent->vertex_data_flag,
					 ent->colour_model,&ent->fdata,1,
					 (Pedge_data_list *)NULL,&vdata);
		}
#endif /* USING_PHIGS */
		Free_NRE(traverser_state, ent);
	}
#endif /* PRINT_ONLY */
} /* End procedure bif_polygon3 */

/*--------------------------------------------------------------------*\
| Procedure	:	int bif_faset(BIF_INT)
|---------------------------------------------------------------------
| Description	:	Begin / End receiving a FILLAREASET entity from the
|			parser
|	BIF_P_BEGIN 	Initialize the triple list for the 
|			fillareaset contours.  Initialize the
|			contour handler.
|
|	BIF_P_END	Store the contour list in the entity.
|---------------------------------------------------------------------
| Return	:	Error Code
\*--------------------------------------------------------------------*/
int bif_faset(begin_or_end)
BIF_INT begin_or_end;
{
	BIF_Fillareaset *ent;
	int num_contours;
	Real_int_union *cont_list;

#ifdef TEST_PRINT
	BEGEND(fillareaset );
#endif /* TEST_PRINT */
#ifndef PRINT_ONLY
	switch (begin_or_end)
	{
	case BIF_P_BEGIN:
	/* Start of a fillareaset */
		init_contours(begin_or_end);
		break;
	case BIF_P_END:
		num_contours = end_contours(&cont_list);
		ent = new_fillareaset(num_contours, cont_list);
		ENT_ERROR(ent);

	/* Store or execute */		
		Traverse(traverser_state, ent);

	/* Send to PHIGs */
#ifdef USING_PHIGS
	{
		Ppoint_list_list poly_list_list;
		poly_list_list.num_point_lists = ent->numContours;
		poly_list_list.point_lists = ent->sets;
		pfill_area_set(&poly_list_list);
	}
#endif /* USING_PHIGS */
		Free_NRE(traverser_state, ent);
	}
#endif /* PRINT_ONLY */
} /* End procedure bif_faset */

/*--------------------------------------------------------------------*\
| Procedure	:	int bif_faset3(BIF_INT)
|---------------------------------------------------------------------
| Description	:	Receive a FILLAREASET3 entity from the parser.
|
|	BIF_P_BEGIN	Initialize group handler for the
|			incoming fill area set contours and
|			optional data groups.
|
|	BIF_P_END	Store the received contours and
|			optional data in the entity structure.
|---------------------------------------------------------------------
| Return	:	Error Code
\*--------------------------------------------------------------------*/
int bif_faset3(begin_or_end)
BIF_INT begin_or_end;
{
	BIF_Fillareaset3 *ent;
	Group_description *group_list;
	int num_groups;
#ifdef TEST_PRINT
	BEGEND(fillareaset3);
#endif /* TEST_PRINT */
#ifndef PRINT_ONLY
	switch (begin_or_end)
	{
	case BIF_P_BEGIN:
		/* Start of a fillarea3 */
		init_groups();
		break;

	case BIF_P_END:
		num_groups = end_groups(&group_list);
		ent = new_fillareaset3(num_groups, group_list);
		ENT_ERROR(ent);

		/* Store or execute */		
		Traverse(traverser_state, ent);

#ifdef USING_PHIGS
		pfill_area_set3_data(ent->facet_data_flag,ent->edge_data_flag,
				     ent->vertex_data_flag,
				     ent->colour_model,&ent->fdata,
				     ent->numContours,ent->edata,
				     ent->vdata);
#ifdef TEST_PRINT2
	printf("v pfill_area_set3_data\n");
	fflush(stdout);
#endif /* TEST_PRINT2 */

#endif /* USING_PHIGS */

		Free_NRE(traverser_state, ent);
	}

#endif /* PRINT_ONLY */
} /* End procedure bif_faset3 */

/*--------------------------------------------------------------------*\
| Procedure	:	int bif_triangle3(BIF_INT)
|---------------------------------------------------------------------
| Description	:	Begin / End receiving a TRIANGLE3 entity from the parser
|	BIF_P_BEGIN 	Initialize group handler for incoming triangle3
|			points and optional data groups
|
|	BIF_P_END	Store the points and optional data in the
|			entity.
|---------------------------------------------------------------------
| Return	:	Error Code
\*--------------------------------------------------------------------*/
int bif_triangle3(begin_or_end)
BIF_INT begin_or_end;
{
	BIF_Triangle3 *ent;
	Group_description *group_list;
	int num_groups;

#ifdef TEST_PRINT
	BEGEND(triangle3);
#endif /* TEST_PRINT */
#ifndef PRINT_ONLY
	switch (begin_or_end)
	{
	case BIF_P_BEGIN:
	/* Start of a triangle3 */
		init_groups();
		break;

	case BIF_P_END:
		num_groups = end_groups(&group_list);
		ent = new_triangle3(num_groups, group_list);
		ENT_ERROR(ent);

	/* Store or execute */		
		Traverse(traverser_state, ent);

	/* Send to PHIGs (fakefigs.c on the GX4000) */
#ifdef USING_PHIGS
		ptri_strip3_data(ent->facet_data_flag,ent->vertex_data_flag,
			  ent->colour_model,ent->numCoords,
			  &ent->fdata,&ent->vdata);
#endif /* USING_PHIGS */
		Free_NRE(traverser_state, ent);
	}
#endif /* PRINT_ONLY */
} /* End procedure bif_triangle3 */

/*--------------------------------------------------------------------*\
| Procedure	:	int bif_quad_mesh3(BIF_INT)
|---------------------------------------------------------------------
| Description	:	Begin / End receiving a QUADMESH3 entity from the parser
|	BIF_P_BEGIN 	Initialize group handler for incoming quadmesh3
|			points and optional data groups
|
|	BIF_P_END	Store the points and optional data in the
|			entity.
|---------------------------------------------------------------------
| Return	:	Error Code
\*--------------------------------------------------------------------*/
int bif_quad_mesh3(begin_or_end)
BIF_INT begin_or_end;
{
	BIF_Quadmesh3 *ent;
	Group_description *group_list;
	int num_groups;

#ifdef TEST_PRINT
	BEGEND(quadmesh3);
#endif /* TEST_PRINT */
#ifndef PRINT_ONLY
	switch (begin_or_end)
	{
	case BIF_P_BEGIN:
	/* Start of a quadmesh3 */
		init_groups();
		break;

	case BIF_P_END:
		num_groups = end_groups(&group_list);
		ent = new_quadmesh3(numRows, numColumns,
			num_groups, group_list);

		ENT_ERROR(ent);

	/* Store or execute */		
		Traverse(traverser_state, ent);

	/* Send to PHIGs */
#ifdef TEST_PRINT
printf("bif_qm: fxqmd3\n");
printf("qmd flags %d %d %d\n",
	ent->facet_data_flag, ent->vertex_data_flag, ent->colour_model );
#endif /* TEST_PRINT */
#ifdef USING_PHIGS
		pquad_mesh3_data(ent->facet_data_flag,
			   ent->vertex_data_flag,
			   ent->colour_model,&ent->dim,
			   &ent->fdata,&ent->vdata);
#endif /* USING_PHIGS */
		Free_NRE(traverser_state, ent);
	}
#endif /* PRINT_ONLY */
} /* End procedure bif_quadmesh3 */

/*--------------------------------------------------------------------*\
| Procedure     :	int bif_quadmeshorder(BIF_INT, BIF_INT)
|---------------------------------------------------------------------
| Description   :	Receive the size of the current quadmesh from
|			the parser. Save them in static variables
|			numRows and numColumns.
|---------------------------------------------------------------------
| Return        :	Error Code (Not Implemented.)
\*--------------------------------------------------------------------*/
int bif_quadmeshorder(nRows, nCols)
BIF_INT nRows, nCols;

{
#ifdef TEST_PRINT
	printf("quadmeshorder: %d %d\n", nRows, nCols);
#endif /* TEST_PRINT */
	numRows    = nRows;
	numColumns = nCols;
} /* End bif_quadmeshorder() */


/*--------------------------------------------------------------------*\
| Procedure	:	int bif_indexpolygons3(BIF_INT)
|---------------------------------------------------------------------
| Description	:	Receive a INDEX_POLYGONS3 entity from the parser. 
|
|	BIF_P_BEGIN	Initialize group handler for the
|			incoming indexpolygons3 and
|			optional data groups.
|
|	BIF_P_END	Store the received contours and
|			optional data in the entity structure.
|---------------------------------------------------------------------
| Return	:	Error Code
\*--------------------------------------------------------------------*/
int bif_indexpoly3(begin_or_end)
BIF_INT begin_or_end;
{
	BIF_Indexpolygons3 *ent;
	Group_description *group_list;
	int num_groups;
#ifdef TEST_PRINT
	BEGEND(indexpolygons3);
#endif /* TEST_PRINT */
#ifndef PRINT_ONLY
	switch (begin_or_end)
	{
	case BIF_P_BEGIN:
		/* Start of a indexpolygons3 */
		init_groups();
		break;

	case BIF_P_END:
		num_groups = end_groups(&group_list);
		ent = new_indexpolygons3(num_groups, group_list);
		ENT_ERROR(ent);

		/* Store or execute */		
		Traverse(traverser_state, ent);

		/* Send to PHIGs */
#ifdef USING_PHIGS
#ifdef TEST_PRINT
		printf("pphd3 flags %d %d %d\n",
		       ent->facet_data_flag, ent->vertex_data_flag,
		       ent->colour_model );
		fflush(stdout);
#endif /* TEST_PRINT */
		pset_of_fill_area_set3_data(ent->facet_data_flag,
					    ent->edge_data_flag,
					    ent->vertex_data_flag,
					    ent->colour_model,ent->numFacets,
					    &ent->fdata,ent->edata,
					    ent->vlist,&ent->vdata);
#endif /* USING_PHIGS */
		Free_NRE(traverser_state, ent);
	}
#endif /* PRINT_ONLY */
} /* End procedure bif_indexpoly3 */

/*----------------------------------------------------------------------*\
| Procedure	:	int bif_text(BIF_REAL, BIF_REAL, * char)
|------------------------------------------------------------------------|
| Description	:	Receive a TEXT entity from the parser
|		x, y		Location of the text (MC)
|		qtext_string	Contents of text.
|------------------------------------------------------------------------|
| Return	:	Error Code
\*----------------------------------------------------------------------*/
int bif_text(x, y, qtext_string)
BIF_REAL x, y;
char *qtext_string;
{

	BIF_Text *ent;
#ifdef TEST_PRINT
	printf("Text: at %f %f display %s\n",x, y, qtext_string);
#endif /* TEST_PRINT */
#ifndef PRINT_ONLY
	ent = new_text((float)x, (float)y, qtext_string);
	ENT_ERROR(ent);
	Traverse(traverser_state, ent);

#ifdef USING_PHIGS
	ptext(&ent->text_pt,ent->text_string);
#endif /* USING_PHIGS */
#endif /* PRINT_ONLY */
} /* End procedure bif_text */

/*--------------------------------------------------------------------*\
| Procedure	:	int bif_text3(BIF_REAL, BIF_REAL, BIF_REAL, 
|					*char, int,
|					BIF_REAL, BIF_REAL, BIF_REAL,
|					BIF_REAL, BIF_REAL, BIF_REAL)
|---------------------------------------------------------------------
| Description	:	Receive a TEXT3 entity from the parser
|		x, y, z		Location of the text (MC)
|		qtext_string	Contents of text.
|---------------------------------------------------------------------
| Return	:	Error Code
\*--------------------------------------------------------------------*/
int bif_text3(x, y, z, qtext_string,
	flag, tbx, tby, tbz, tux, tuy, tuz)
BIF_REAL x, y, z;
char *qtext_string;
int flag;
BIF_REAL tbx, tby, tbz;
BIF_REAL tux, tuy, tuz;

{

	BIF_Text3 *ent;
#ifdef TEST_PRINT
	printf("Text3: at %f %f %f display %s\n",x,y,z,qtext_string);
	if ( flag )
		printf("Text directions: base %f %f %f up %f %f %f\n",
			tbx, tby, tbz, tux, tuy, tuz);
#endif /* TEST_PRINT */
#ifndef PRINT_ONLY
	if ( flag == FALSE )
	{
		tbx = 1.; tby = 0.; tbz = 0.;
		tux = 0.; tuy = 1.; tuz = 0.;
	}

	ent = new_text3((float)x, (float)y, (float)z, qtext_string,
		(float)tbx, (float)tby, (float)tbz,
		(float)tux, (float)tuy, (float)tuz);
	ENT_ERROR(ent);
	Traverse(traverser_state, ent);
#ifdef TEST_PRINT
	printf("Text3: after new_text3 display %s\n",ent->text_string);
#endif /* TEST_PRINT */
#ifdef EXTERNALNOTE
	Changed the order of arguments in the following ptx3 call.
#endif
#ifdef USING_PHIGS

	ptext3(&ent->text_pt,ent->dir,ent->text_string);

#endif /* USING_PHIGS */
#endif /* PRINT_ONLY */
} /* End procedure bif_text3 */

/*--------------------------------------------------------------------*\
| Procedure	:	int bif_annotationtext(BIF_REAL, BIF_REAL,
|					BIF_REAL, BIF_REAL, BIF_REAL,
|					BIF_REAL, *char)
|---------------------------------------------------------------------
| Description	:	Receive a ANNOTATION_TEXT3 entity from the parser
|		 x,  y,  z	Location of the text (MC)
|		ox, oy, oz	Offset of the text (NPC)
|		qtext_string	Contents of text.
|---------------------------------------------------------------------
| Return	:	Error Code
\*--------------------------------------------------------------------*/
int bif_annotationtext(x, y, z, ox, oy, oz, qtext_string)
BIF_REAL  x,  y,  z;
BIF_REAL ox, oy, oz;
char *qtext_string;

{

	BIF_Anno_text3 *ent;
#ifdef TEST_PRINT
	printf("Anno_text3: at %f %f %f display %s\n",x,y,z,qtext_string);
	printf("Offset %f %f %f\n", ox, oy, oz);
#endif /* TEST_PRINT */
#ifndef PRINT_ONLY

	ent = new_annotext3((float)x, (float)y, (float)z,
		(float)ox, (float)oy, (float)oz, qtext_string);

	ENT_ERROR(ent);
	Traverse(traverser_state, ent);

#ifdef USING_PHIGS
	panno_text_rel3(&ent->ref_pt,&ent->anno_offset,
				 ent->text_string);
#endif /* USING_PHIGS */
#endif /* PRINT_ONLY */
} /* End procedure bif_annotationtext */
/*--------------------------------------------------------------------*\
| Procedure     :	int bif_sphere3(BIF_INT)
|---------------------------------------------------------------------
| Description   :	Begin / End receiving a SPHERE3 entity
|	BIF_P_BEGIN 	Initialize the sphere optional data
|	BIF_P_END	Generate a quadmesh to do the display
|---------------------------------------------------------------------
| Return        :	Error Code (Not Implemented)
\*--------------------------------------------------------------------*/

int bif_sphere3(begin_or_end)
BIF_INT begin_or_end;
{
#ifdef TEST_PRINT
	BEGEND(sphere3);
#endif /* TEST_PRINT */
#ifndef PRINT_ONLY
	switch (begin_or_end)
	{
	case BIF_P_BEGIN:
		/* Start of a GEN_SPHERE3 */
		/* Initialize the default values */
		bif_exact(FALSE);
		bif_center(0.,0.,0.);
		bif_radius(1.0);
		bif_scalefactors(1.0,1.0,1.0);
		break;

	case  BIF_P_END:
		/*----------------------------------------------------*\
		|	The generated sphere3 is stored as a quadmesh.
		|	(For the reference port only no "non-EXACT"
		|	method is supported)
		|	genSphere3Mesh generate points to the buffers 	
	|	(using bif_triplet) just like the parser would.
		|	bif_quadmesh3(BIF_P_END) stores/executes the
		|	generated data as appropriate.
		\*----------------------------------------------------*/
		genSphere3Mesh(&temp_ent.sphere3);

		break;
	}
#endif /* PRINT_ONLY */
} /* End procedure bif_sphere3 */

/*--------------------------------------------------------------------*\
| Procedure     :	int bif_gencircle(BIF_INT)
|---------------------------------------------------------------------
| Description   :	Begin / End receiving a CIRCLE entity
|	BIF_P_BEGIN 	Initialize the circle optional data
|	BIF_P_END	Generate a polygon with enough sides.
|---------------------------------------------------------------------
| Return        :	Error Code (Not Implemented)
\*--------------------------------------------------------------------*/

int bif_gencircle(begin_or_end)
BIF_INT begin_or_end;
{
#ifdef TEST_PRINT
	BEGEND(circle);
#endif /* TEST_PRINT */
#ifndef PRINT_ONLY
	switch (begin_or_end)
	{
	case BIF_P_BEGIN:
		/* Start of a GEN_CIRCLE */
		/* Initialize the default values */
		bif_exact(FALSE);
		bif_center(0.,0.,0.);
		bif_radius(1.0);
		bif_scalefactors(1.0,1.0,1.0);
		break;

	case  BIF_P_END:
		/*----------------------------------------------------*\
		|	The generated circle is stored as a polygon.
		|	genCirclePoly generate points to the buffers 
		|	(using bif_pair) just like the parser would.
		|	bif_polygon(BIF_P_END) stores/executes the
		|	generated data as appropriate.
		\*----------------------------------------------------*/
		genCirclePoly(&temp_ent.circle);
	break;
	}
#endif /* PRINT_ONLY */
} /* End procedure bif_gencircle */

/*--------------------------------------------------------------------*\
| Procedure     :	int bif_gencircle3(BIF_INT)
|---------------------------------------------------------------------
| Description   :	Begin / End receiving a CIRCLE entity
|	BIF_P_BEGIN 	Initialize the circle3 optional data
|	BIF_P_END	Generate a polygon with enough sides.
|---------------------------------------------------------------------
| Return        :	Error Code (Not Implemented)
\*--------------------------------------------------------------------*/

int bif_gencircle3(begin_or_end)
BIF_INT begin_or_end;
{
#ifdef TEST_PRINT
	BEGEND(circle3);
#endif /* TEST_PRINT */
#ifndef PRINT_ONLY
	switch (begin_or_end)
	{
	case BIF_P_BEGIN:
		/* Start of a GEN_CIRCLE3 */
		/* Initialize the default values */
		bif_exact(FALSE);
		bif_center(0.,0.,0.);
		bif_normal(0.,0.,1.);
		bif_radius(1.0);
		bif_scalefactors(1.0,1.0,1.0);
		break;

	case  BIF_P_END:
		/*----------------------------------------------------*\
		|	The generated circle3 is stored as a polygon3.
		|	genCircle3Poly generate points to the buffers 
		|	(using bif_triplets) just like the parser would .
		|	bif_polygon3(BIF_P_END) stores/executes the
		|	generated data as appropriate.
		\*----------------------------------------------------*/
		genCircle3Poly(&temp_ent.circle3);

		break;
	}
#endif /* PRINT_ONLY */
} /* End procedure bif_gencircle3 */

/*--------------------------------------------------------------------*\
| Procedure     :	int bif_sphereorder(BIF_INT, BIF_INT)
|---------------------------------------------------------------------
| Description   :	Receive the size of the current sphere from
|			the parser. Save them in static temp_ent<dot>
|			numLong and numLat.
|---------------------------------------------------------------------
| Return        :	Error Code (Not Implemented.)
\*--------------------------------------------------------------------*/
int bif_sphereorder(nLong, nLat)
BIF_INT nLong, nLat;

{
	temp_ent.sphere3.numLong = nLong;
	temp_ent.sphere3.numLat  = nLat;
} /* End bif_sphereorder() */

/*--------------------------------------------------------------------*\
| Procedure     :	int bif_circleedges(BIF_INT)
|---------------------------------------------------------------------
| Description   :	Receive the size of the current circle(3) from
|			the parser. Save it in static:
|			temp_ent.sphere3.numLong
|				(numLat is ignored for circles.)
|---------------------------------------------------------------------
| Return        :	Error Code (Not Implemented.)
\*--------------------------------------------------------------------*/
int bif_circleedges(nLong)
BIF_INT nLong;

{
	temp_ent.sphere3.numLong = nLong;
	temp_ent.sphere3.numLat  = 1; 
} /* End bif_circleedges() */

/*--------------------------------------------------------------------*\
| Procedure     :	int bif_exact(BIF_INT)
|---------------------------------------------------------------------
| Description   :	Set the GEN_* exact parameter.
|			Save it in static:
|				temp_ent.sphere3.exact
|---------------------------------------------------------------------
| Return        :	Error Code (Not Implemented.)
\*--------------------------------------------------------------------*/
int bif_exact(exact)
BIF_INT exact;

{
	temp_ent.sphere3.exact    = exact;
} /* End bif_exact() */

/*--------------------------------------------------------------------*\
| Procedure     :	int bif_center(BIF_REAL, BIF_REAL, BIF_REAL)
|---------------------------------------------------------------------
| Description   :	Set the GEN_* center parameter.
|			Save it in static:
|				temp_ent.sphere3.center
|---------------------------------------------------------------------
| Return        :	Error Code (Not Implemented.)
\*--------------------------------------------------------------------*/
int bif_center(x_center, y_center, z_center)
BIF_REAL x_center, y_center, z_center;

{
	temp_ent.sphere3.center[0]    = (float)x_center;
	temp_ent.sphere3.center[1]    = (float)y_center;
	temp_ent.sphere3.center[2]    = (float)z_center;
} /* End bif_center() */

/*--------------------------------------------------------------------*\
| Procedure     :	int bif_radius(BIF_REAL);
|---------------------------------------------------------------------
| Description   :	Set the GEN_* radius parameter.
|			Save it in static:
|				temp_ent.sphere3.radius
|---------------------------------------------------------------------
| Return        :	Error Code (Not Implemented.)
\*--------------------------------------------------------------------*/
int bif_radius(radius)
BIF_REAL radius;

{
	temp_ent.sphere3.radius    = (float)radius;
} /* End bif_radius() */

/*--------------------------------------------------------------------*\
| Procedure     :	int bif_scalefactors(BIF_REAL, BIF_REAL, BIF_REAL)
|---------------------------------------------------------------------
| Description   :	Set the GEN_* scale parameter.
|			Save it in static:
|				temp_ent.sphere3.scale
|---------------------------------------------------------------------
| Return        :	Error Code (Not Implemented.)
\*--------------------------------------------------------------------*/
int bif_scalefactors(x_scale, y_scale, z_scale)
BIF_REAL x_scale, y_scale, z_scale;

{
	temp_ent.sphere3.scale[0]    = (float)x_scale;
	temp_ent.sphere3.scale[1]    = (float)y_scale;
	temp_ent.sphere3.scale[2]    = (float)z_scale;
} /* End bif_scalefactors() */

/*--------------------------------------------------------------------*\
| Procedure     :	int bif_normal(BIF_REAL, BIF_REAL, BIF_REAL)
|---------------------------------------------------------------------
| Description   :	Set the GEN_* normal parameter.
|			Save it in static:
|				temp_ent.sphere3.norm
|---------------------------------------------------------------------
| Return        :	Error Code (Not Implemented.)
\*--------------------------------------------------------------------*/
int bif_normal(x_normal, y_normal, z_normal)
BIF_REAL x_normal, y_normal, z_normal;

{
	temp_ent.sphere3.norm[0]    = (float)x_normal;
	temp_ent.sphere3.norm[1]    = (float)y_normal;
	temp_ent.sphere3.norm[2]    = (float)z_normal;
} /* End bif_normal() */

/*----------------------------------------------------------------------*\
| Procedure	:	int bif_pixelmap3(BIF_INT)
|------------------------------------------------------------------------|
| Description	:	Begin / End receiving a PIXEL_MAP3 entity from
|			the parser.
|	BIF_P_BEGIN 	begin entity
|		Initialize group handler for incoming pixelmap
|		and optional data groups: PSEUDO_COLOR
|		Make the BIF entity... and post the channels to the 
|		pixel catching routine.
|	BIF_P_END		end entity
|		Store the received point list and optional data in the
|		entity structure.
|------------------------------------------------------------------------|
| Return	:	Error Code
\*----------------------------------------------------------------------*/
int bif_pixelmap3(begin_or_end, x, y, z, ixsize, iysize, repFun, mapType)
BIF_INT begin_or_end;
BIF_REAL x, y, z;
BIF_INT ixsize, iysize;
BIF_INT repFun;
BIF_INT mapType;

{
	BIF_Pixelmap3 *ent;
	static BIF_Pixelmap3 *saved_ent = NULL;
	Group_description *group_list;
	int num_groups;
	int i; /* generic counter */

#define NUM_PIX_DGS	1
	static int pixScan[NUM_PIX_DGS] = 
	{
		PSEUDO_COLOR
	};
	
	static int listSize[NUM_PIX_DGS];
	static Real_int_union *pixList[NUM_PIX_DGS];

	int numPix, packSize, numPack, ierr;
	char *packed;
	int status = 0;	/* general return value */

#ifdef TEST_PRINT
	BEGEND(pixelmap3);
	fprintf(stderr,"into pixel_map3.\n");
	fflush(stderr);
#endif /* TEST_PRINT */
#ifndef PRINT_ONLY
	switch (begin_or_end)
	{
	case BIF_P_BEGIN:
	/* Start of a pixelmap3 */

		/* Fill the tempory entity with the necessary info */
		temp_ent.pixelmap3.upperLeft[0]        = (float)x;
		temp_ent.pixelmap3.upperLeft[1]        = (float)y;
		temp_ent.pixelmap3.upperLeft[2]        = (float)z;
		temp_ent.pixelmap3.numColumns          = (int)ixsize;
		temp_ent.pixelmap3.numRows             = (int)iysize;
		temp_ent.pixelmap3.pixelUpdateFunction =
				REMAP_PIXFUNC((int)repFun);
		temp_ent.pixelmap3.bifMapType          = (int)mapType;

		init_groups();

		ent = new_pixelmap3( &temp_ent.pixelmap3);
		ENT_ERROR(ent);

		bif_initpixelbuffer(ent->numRows, ent->numColumns,
				ent->bifMapType, ent->red,
				ent->green, ent->blue);
		saved_ent = ent;

		break;

	case BIF_P_END:
		/*----------------------------------------------------*\
		|	Pick up where we left off... the buffer should
		|	now be full.
		\*----------------------------------------------------*/
		ent = saved_ent;
		ENT_ERROR(ent);

		/*----------------------------------------------------*\
		|	Find the DG's and compute the sizes
		\*----------------------------------------------------*/
		num_groups = end_groups(&group_list);
		scanForGroups(num_groups, group_list, NUM_PIX_DGS,
				pixScan, listSize, pixList);

		/*----------------------------------------------------*\
		|	Finish up the pixel buffer
		\*----------------------------------------------------*/
		bif_endpixelbuffer(listSize[0]/3, pixList[0]);

	/* Store or execute */		
		Traverse(traverser_state, ent);

	/* Send to PHIGs */
#ifdef USING_PHIGS
		/*----------------------------------------------------*\
		|	Set the pixel update function
		\*----------------------------------------------------*/
		fxpixelup(ent->pixelUpdateFunction);

		/*----------------------------------------------------*\
		|	PIXEL_MAP3: prepare for the phigs Calls
		|	Malloc "Enough Space" for one channel of
		|	data (packed), share the workspace for the 
		|	three channels, if true color.
		|	256 is safety margin
		\*----------------------------------------------------*/
		numPix = ent->numRows * ent->numColumns;
		packSize = sizeof(char) * ( numPix +256 );
		numPack = packSize / 80;
		packed = malloc ( packSize );
		ENT_ERROR(packed);
		
		if ( wk_info.color_mode == BIF_TRUE_COLOR )
		{/* True color pixmaps */
			/*--------------------------------------------*\
			|	Output R, G, and B
			\*--------------------------------------------*/
			fxpixmap3(ent->upperLeft,
				ent->numColumns, ent->numRows,
				1, ent->red, numPack, packed);

			fxpixmap3(ent->upperLeft,
				ent->numColumns, ent->numRows,
				2, ent->green, numPack, packed);

			fxpixmap3(ent->upperLeft,
				ent->numColumns, ent->numRows,
				3, ent->blue, numPack, packed);
		}/* True color pixmaps */
		else
		{/* pseudo color pixmaps */
			/*--------------------------------------------*\
			|	Output R to all channels
			\*--------------------------------------------*/
			fxpixmap3(ent->upperLeft,
				ent->numColumns, ent->numRows,
				0, ent->red, numPack, packed);
		}/* pseudo color pixmaps */

		/*----------------------------------------------------*\
		|	Free the workspace.
		\*----------------------------------------------------*/
		free(packed);

		/*----------------------------------------------------*\
		|	Reset the pixel update function
		\*----------------------------------------------------*/
		fxpixelup(BIF_PF_REPLACE);
#endif /* USING_PHIGS */

		Free_NRE(traverser_state, ent);
		saved_ent = NULL;
	}
#endif /* PRINT_ONLY */
} /* End procedure bif_pixelmap3 */

/*--------------------------------------------------------------------*\
| Procedure	: int bif_nubc (BIF_INT)
|---------------------------------------------------------------------
| Description	: Begin / End receiving a NON_UNIFORM_BSPLINE_CURVE 
|		  entity from the parser
|		BIF_P_BEGIN 	Init the group list for 
|				NON_UNIFORM_BSPLINE_CURVE data
|		BIF_P_END 	Store the NON_UNIFORM_BSPLINE_CURVE  
|				data in the entity
|
|---------------------------------------------------------------------
| Return	: Error Code (Not Implemented)
\*--------------------------------------------------------------------*/
int bif_nubc (begin_or_end) /* ver 1.0 */
BIF_INT begin_or_end;
{

	BIF_Nubc *ent;	/* WORKING: new type */

	int num_groups;
	Group_description *group_list;

#ifdef    TEST_PRINT
	BEGEND(non_uniform_bspline_curve (nubc));
	/*------------*/
#endif /* TEST_PRINT */
      /*------------*/


#ifndef PRINT_ONLY
	switch (begin_or_end)
	{
	case BIF_P_BEGIN:
	/* Start of a LINE3 */
		init_groups();
		break;

	case BIF_P_END:
		num_groups = end_groups(&group_list);
		ent = new_nubc(num_groups, group_list);/* WORKING: new type */
#	ifdef    TEST_PRINT
		printf("non_uniform_bspline_curve : %d groups\n", num_groups);
	       /*------------*/
#	endif /* TEST_PRINT */
	     /*------------*/
	        /* can not make this call till memory is being allocated
		ENT_ERROR(ent);
		*/

	/*------------------------------------------------------------*\
	|  Store or execute					       | 
	\*------------------------------------------------------------*/
	        /* can not make this call till memory is being allocated
		Traverse(traverser_state, ent);
		*/

#	ifdef    USING_PHIGS
		/*----------------------------------------------------*\
		|  Send to PHIGs:				       |
		|  Put Phigs call for non_uniform_bspline_curve here   |
		\*----------------------------------------------------*/

	       /*-------------*/
#	endif /* USING_PHIGS */
	     /*-------------*/

	        /* can not make this call till memory is being allocated
		Free_NRE(traverser_state, ent);
		*/
	}
	/*-------------*/
#endif /* PRINT_ONLY  */
      /*-------------*/
} /* End procedure bif_nubc */

/*--------------------------------------------------------------------*\
| Procedure	: int bif_nubs (BIF_INT)
|---------------------------------------------------------------------
| Description	: Begin / End receiving a NON_UNIFORM_BSPLINE_surfaces 
|		  entity from the parser
|		BIF_P_BEGIN 	Init the group list for 
|				NON_UNIFORM_BSPLINE_surfaces data
|		BIF_P_END 	Store the NON_UNIFORM_BSPLINE_surfaces  
|				data in the entity
|
|---------------------------------------------------------------------
| Return	: Error Code (Not Implemented)
\*--------------------------------------------------------------------*/
int bif_nubs (begin_or_end) /* ver 1.0 */
BIF_INT begin_or_end;
{
	BIF_Nubs *ent;	/* WORKING: new type */

	int num_groups;
	Group_description *group_list;

#ifdef    TEST_PRINT
	BEGEND(non_uniform_bspline_surfaces (nubs));
	/*------------*/
#endif /* TEST_PRINT */
      /*------------*/


#ifndef PRINT_ONLY
	switch (begin_or_end)
	{
	case BIF_P_BEGIN:
	/* Start of a LINE3 */
		init_groups();
		break;

	case BIF_P_END:
		num_groups = end_groups(&group_list);
		ent = new_nubs(num_groups, group_list);/* WORKING: new type */
#	ifdef    TEST_PRINT
		printf("non_uniform_bspline_surfaces : %d groups\n",num_groups);
	       /*------------*/
#	endif /* TEST_PRINT */
	     /*------------*/
	        /* can not make this call till memory is being allocated
		ENT_ERROR(ent);
		*/

	/*------------------------------------------------------------*\
	|  Store or execute					       | 
	\*------------------------------------------------------------*/
	        /* can not make this call till memory is being allocated
		Traverse(traverser_state, ent);
		*/

#	ifdef    USING_PHIGS
		/*----------------------------------------------------*\
		|  Send to PHIGs:				       |
		|  Put Phigs call for non_uniform_bspline_surfaces here|
		\*----------------------------------------------------*/

	       /*-------------*/
#	endif /* USING_PHIGS */
	     /*-------------*/

	        /* can not make this call till memory is being allocated
		Free_NRE(traverser_state, ent);
		*/
	}
	/*-------------*/
#endif /* PRINT_ONLY  */
      /*-------------*/
} /* End procedure bif_nubs */

/*--------------------------------------------------------------------*\
| Procedure     :	int bif_nubcholder(BIF_INT, BIF_INT)
|---------------------------------------------------------------------
| Description   :	Receive the rational/non_rationsl flag for 
|			and the order for NON_UNIFORM_BSPLINE_CURVE
|---------------------------------------------------------------------
| Return        :	Error Code (Not Implemented.)
\*--------------------------------------------------------------------*/
int bif_nubcholder (rational_non,iorder) /* ver 1.0 */
BIF_INT rational_non,iorder;
{
#ifdef TEST_PRINT
	printf("nubcholder: ");
	if (rational_non == RATIONAL)
		printf("RATIONAL order is %d\n",iorder);
	else
		printf("NON_RATIONAL order is %d\n",iorder);
#endif /* TEST_PRINT */
	static_rational_non = rational_non;
	static_iorder = iorder;
} /* End bif_nubcholder() */


/*--------------------------------------------------------------------*\
| Procedure     :	int bif_nubsholder(BIF_INT, BIF_INT, BIF_INT,
|					   BIF_INT, BIF_INT, BIF_INT)
|---------------------------------------------------------------------
| Description   :	Receive the rational/non_rationsl flag for 
|			and the order for NON_UNIFORM_BSPLINE_CURVE
|---------------------------------------------------------------------
| Return        :	Error Code (Not Implemented.)
\*--------------------------------------------------------------------*/
int bif_nubsholder ( rational_non,iuorder,ivorder,m,n ) /* ver 1.0 */
BIF_INT rational_non,iuorder,ivorder,m,n;
{
#ifdef TEST_PRINT
	printf("nubsholder: ");
	if (rational_non == RATIONAL)
		printf("RATIONAL iuorder=%d ivorder=%d m=%d n=%d\n",
		iuorder,ivorder,m,n);
	else
		printf("NON_RATIONAL iuorder=%d ivorder=%d m=%d n=%d\n", 
		iuorder,ivorder,m,n);
#endif /* TEST_PRINT */
	static_rational_non = rational_non;
	static_iuorder = iuorder;
	static_ivorder = ivorder;
	static_m = m;
	static_n = n;
} /* End bif_nubsholder() */
