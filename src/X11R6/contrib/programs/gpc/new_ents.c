/* $XConsortium: new_ents.c,v 5.4 94/04/17 20:44:41 rws Exp $ */
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
| Author        :	nde/mjf/jmz / SimGraphics Engineering Corportation
|
| File          :	new_ents.c
| Date          :	3/16/89
| Project       :	PLB
| Description   :	
| Status        :	Version 1.0
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

/*--------------------------------------------------------------------*\
|	Table of Contents
|
|	BIF_Simple_triple *new_simple_triple( int, *Real_int_union )
|		:	Allocate and fill a simple_triple structure, and
|	BIF_Simple_double *new_simple_double(
|		:	Allocate and fill a simple_double structure,
|	BIF_Beginstructure *new_beginstructure(int)
|		:	Allocates a new BIF_Beginstructure entity for the
|	BIF_Marker *new_marker(int, *Real_int_union)
|		:	Allocate and fill a marker structure.  All space
|	BIF_Marker3 *new_marker3(int, *Real_int_union)
|		:	Allocate and fill a marker3 structure.  All space is
|	BIF_Line *new_line(int, *Real_int_union)
|		:	Allocate and fill a line structure.  All space
|	BIF_Line3 *new_line3(int, *Group_description)
|		:	Allocate and fill a LINE3 entity w/ optional data
|	BIF_Polygon *new_polygon(int, *Real_int_union)
|		:	Allocate and fill a polygon structure.  All
|	BIF_polygon3 *new_polygon3(int,
|		:	Allocate and fill a POLYGON3 entity w/ optional
|	BIF_Fillareaset *new_fillareaset (int, *Real_int_union)
|		:	Allocate and fill a fillareaset structure.  
|	BIF_fillareaset3 *new_fillareaset3(int, *Group_description)
|		:	Allocate and fill a FILLAREASET3 entity with
|	BIF_Triangle3 *new_triangle3(int, *Group_description)
|		:	Allocate and fill a TRIANGLE3 entity with
|	BIF_Quadmesh3 *new_quadmesh3(int, int, int, *Group_description)
|		:	Allocate and fill a QUADMESH3 entity with
|	BIF_Indexpolygons3 *new_indexpolygons3(int, *Group_description)
|		:	Allocate and fill a INDEX_POLYGONS3 entity with
|	BIF_Text *new_text(float, float, float, *char) 
|		:	Allocates and fills a text structure 
|	BIF_Text3 *new_text3(float, float, float, *char) 
|		:	Allocates and fills a text3 structure 
|	BIF_Anno_text3 *new_annotext3( float, float, float,
|					float, float, float, *char) 
|		:	Allocates and fills a annotext3 structure 
|	BIF_Lightstate *new_lightstate(int, *Group_description)
|		:	Allocates and fills a LIGHT_STATE entity
|	BIF_All *new_generic(*BIF_All, int, int, (*int)())
|		:	Allocate a new generic BIF entity
|	int scanForGroups( int, Group_description *, int, int *,
|				int *, Real_int_union *,)
|		:	scans a group headers list for the groups
|	int fillVec(int, Real_int_union *, char *, int, *vector[] )
|		:	Assign the malloc'd space to the array and
|	int fillInt(int, Real_int_union *, char *, int, *int[], int )
|		:	Assign the malloc'd space to the array and
|	errorSize(char *, int, int, int)
|		:	report if an actual group size is not correct
|	int scanWithData(*char, int, int, int, int, *Group_description )
|		:	Scan the groups for the standard ODG's
|	int scanWithDataSet(*char, int, int, int, int, *Group_description )
|		:	Scan the groups for the standard ODG Sets
|	int loadWithData(*BIF_Withdata3, *char)
|		:	Load the scan'd (see scanWithData) groups into
|	int loadWithDataSet(*BIF_Withdata3, *char)
|		:	Load the scan'd (see scanWithData) groups into
|	int makeVertexFlag(int, int, int)
|		:	add the contribution values that make up
|	int makeFacetFlag(int, int, int)
|		:	add the contribution values that make up
|	int makeEdgeFlag(int, int, int)
|		:	add the contribution values that make up
|	int makeWithDataFlag(* int)
|		:	Set the fields in the with_data_flag array
|	BIF_Withdata3 *new_withdata3(	*char, int, (*int)(), int, int,
|					int, int, *Real_int_union,
|					*int, *int, *int, int,
|					*Group_description)
|		:	Allocate and fill a Withdata entity with optional data
|	BIF_Pixelmap3 *new_pixelmap3(* BIF_Pixelmap3)
|		:	Allocate a Pixelmap3 entity
|	BIF_Nubc *new_nubc(int, *Group_description)
|		:	Allocate and fill a NON_UNIFORM_BSPLINE_CURVE
|	BIF_Nubs *new_nubs(int, *Group_description)
|		:	Allocate and fill a NON_UNIFORM_BSPLINE_SURFACE
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
#include "bifbuild.h"
#include "biftypes.h"
#include "bifparse.h"
#include "new_ents.h"
#include "doentity.h"
#include "db_tools.h"
#include "bifmacro.h"
#include "ph_map.h"
#include "globals.h"
#include <X11/Xfuncs.h>

/*--------------------------------------------------------------------*\
|	Local Macros
\*--------------------------------------------------------------------*/
#define ALLOC(type) (type *)malloc( sizeof( type ) )
#define ALLOCV(type,vfl) (type *)malloc( sizeof( type ) + vfl )

#define STDSIZE(ent,nc,nf,ne) \
{\
	ent->numCoords = nc;\
	ent->numFacets = nf;\
	ent->numEdges  = ne;\
}

#ifdef EXTERNAL_NOTE
	MAX_VAL has been moved to bifmacro.h
	A slight fix to the macro.
#endif /* EXTERNAL_NOTE */
#define ODGSIZE(flag,number,size) \
	( ( flag ? MAX_VAL(1,number) : 1 ) * size)


/*--------------------------------------------------------------------*\
|	Local #define Constants
\*--------------------------------------------------------------------*/
#define NUM_STD_ODGS 7
#define STD_ODGS_V 3
#define STD_ODGS_F 6
#define STD_ODGS_E 7

/*--------------------------------------------------------------------*\
|	Local global variables (static)
\*--------------------------------------------------------------------*/
static int scanList[NUM_STD_ODGS] = 
{
	VERTEX_COLORS,	 VERTEX_COLOR_INDICES,
	VERTEX_NORMALS,

	FACET_COLORS,		FACET_COLOR_INDICES,
	FACET_NORMALS,

	EDGE_VISIBILITY
};
static int sizeofList[NUM_STD_ODGS] = 
{
	sizeof(Pcoval),	        sizeof(Pcoval),
	sizeof(Pvec3),
	sizeof(Pcoval),	        sizeof(Pcoval),
	sizeof(Pvec3),
	sizeof(Pedge_flag)
};

#ifdef EXTERNAL_NOTE
	recSizeList is a new field defining the size of the data records
	for each group.  Useful for the fillareaset3 primitive.
#endif
static int recSizeList[NUM_STD_ODGS] = 
{
	3,			1,
	3,
	3,			1,
	3,
	1
};

static int sizeList[NUM_STD_ODGS];
static Real_int_union *listList[NUM_STD_ODGS];

#define NUM_DATA_FLAGS 4
static int sizeofFacetdata[NUM_DATA_FLAGS] =
{
	0, sizeof(Pcoval), sizeof(Pvec3), sizeof(Pconorm3)
};

static int sizeofVertexdata[NUM_DATA_FLAGS] =
{
	sizeof(Ppoint3), sizeof(Pptco3), sizeof(Pptnorm3), sizeof(Pptconorm3)
};

#define NUM_GROUPS_COORD     1
	static int coordGroup[NUM_GROUPS_COORD] = { VERTEX_COORDINATES };
	static int coordSize[NUM_GROUPS_COORD]; 
	static Real_int_union *coordList[NUM_GROUPS_COORD];


/*--------------------------------------------------------------------*\
|	BEGIN PROCEDURE CODE
\*--------------------------------------------------------------------*/


/*----------------------------------------------------------------------*\
| Procedure     :	BIF_Simple_triple *new_simple_triple( int,
|						*Real_int_union )
|------------------------------------------------------------------------|
| Description   :	Allocate and fill a simple_triple structure, and
|			the three arrays of floats which are pointed to
|			from within this structure.  All space is
|			allocated at once, allowing a single free to 
|			delete the structure.
|------------------------------------------------------------------------|
| Return        :	The pointer to the new structure.
\*----------------------------------------------------------------------*/
BIF_Simple_triple *new_simple_triple(size,trip_list)
int size;
Real_int_union *trip_list;

{
	char *ptr;
	BIF_Simple_triple *ent;
	int total;
	int indx;
	Ppoint3 *points;

	/*------------------------------------------------------------*\
	|	Define memory requirement
	\*------------------------------------------------------------*/
	total = sizeof(BIF_Simple_triple) + size * 3 * sizeof(Pfloat);
	ptr = malloc((unsigned)total);
	if ( ptr != NULL )
	{
		/*----------------------------------------------------*\
		|	Initialize the structure
		\*----------------------------------------------------*/
		ent = (BIF_Simple_triple *)ptr;
		ptr += sizeof(BIF_Simple_triple);
		ent->number = size;
		if ( size > 0 )
		{
			/*--------------------------------------------*\
			| Fill the structure with pointer to Ppoint3 
			| The space for the points was allocated with
			| the same malloc as this structure.
			\*--------------------------------------------*/
		        ent->points = (Ppoint3 *)ptr;

			/*--------------------------------------------*\
			| Fill array of Ppoint structures.
			\*--------------------------------------------*/
			points = ent->points;
			for ( indx = 0 ; indx < size ; indx++, points++ )
			{
				points->x = (trip_list++)->Float;
				points->y = (trip_list++)->Float;
				points->z = (trip_list++)->Float;
			}
		}
		else
		{
			/*--------------------------------------------*\
			| Empty Ppoint3 array
			\*--------------------------------------------*/
		        ent->points = NULL;
		}
	}
	/*------------------------------------------------------------*\
	|	Return the generated entity 
	\*------------------------------------------------------------*/
	return( ent );

} /* End procedure new_simple_triple */

/*----------------------------------------------------------------------*\
| Procedure     :	BIF_Simple_double *new_simple_double(
						int, *Real_int_union)
|------------------------------------------------------------------------|
| Description   :	Allocate and fill a simple_double structure,
|			and the two arrays of floats which are pointed
|			to from within this structure.  All space is
|			allocated at once, allowing a single free to 
|			delete the structure.
|------------------------------------------------------------------------|
| Return        :	The pointer to the new structure.
\*----------------------------------------------------------------------*/
BIF_Simple_double *new_simple_double(size,trip_list)
int size;
Real_int_union *trip_list;

{
	char *ptr;
	BIF_Simple_double *ent;
	int total;
	int indx;
	Ppoint *points;

	/*------------------------------------------------------------*\
	|	Define memory requirement
	\*------------------------------------------------------------*/
	total = sizeof(BIF_Simple_double) + size * 2 * sizeof(float);
	ptr = malloc((unsigned)total);
	if ( ptr != NULL )
	{
		/*----------------------------------------------------*\
		|	Initialize the structure
		\*----------------------------------------------------*/
		ent = (BIF_Simple_double *)ptr;
		ptr += sizeof(BIF_Simple_double);
		ent->number = size;
		if ( size > 0 )
		{
			/*--------------------------------------------*\
			| The space for the Ppoint array of structures
			| was allocated with the same malloc as this
			| structure.
			\*--------------------------------------------*/
		        ent->points = (Ppoint *)ptr;

			/*--------------------------------------------*\
			| Fill array of Ppoint structures.
			\*--------------------------------------------*/
			points = ent->points;
			for ( indx = 0 ; indx < size ; indx++, points++ )
			{
				points->x = (trip_list++)->Float;
				points->y = (trip_list++)->Float;
				/* Skip the "Z" values */
				trip_list++;
			}
		}
		else
		{
			/*--------------------------------------------*\
			| Empty array of Ppoint structures
			\*--------------------------------------------*/
			ent->points = NULL;
		}
	}
	/*------------------------------------------------------------*\
	|	Return the generated entity 
	\*------------------------------------------------------------*/
	return( ent );

} /* End procedure new_simple_double */

/*----------------------------------------------------------------------*\
| Procedure     :	BIF_Beginstructure *new_beginstructure(int)
|------------------------------------------------------------------------
| Description   :	Allocates a new BIF_Beginstructure entity for the
|		structures database.
|------------------------------------------------------------------------
| Return        :	The address of the allocated structure
\*----------------------------------------------------------------------*/
BIF_Beginstructure *new_beginstructure( structure_id )
int structure_id;

{

BIF_Beginstructure  *ent;

	ent = ALLOC( BIF_Beginstructure );
	if ( ent != NULL )
	{
		HEADER(ent, BEGIN_STRUCTURE, do_beginstructure, NULL);

		ent->structure_id = structure_id;
		ent->top_of_list = NULL;
		ent->expanded = FALSE;
	}

	return( ent );

} /* End new_beginstructure */

/*----------------------------------------------------------------------*\
| Procedure     :	BIF_Marker *new_marker(int, *Real_int_union)
|------------------------------------------------------------------------|
| Description   :	Allocate and fill a marker structure.  All space
|			is allocated at once, allowing a single free to 
|			delete the structure.
|------------------------------------------------------------------------|
| Return        :	The pointer to the new structure.
\*----------------------------------------------------------------------*/
BIF_Marker *new_marker(size,trip_list)
int size;
Real_int_union *trip_list;

{
	BIF_Marker *ent;
	ent = (BIF_Marker *)new_simple_double(size,trip_list);

	if ( ent != NULL )
	{
		HEADER(ent, MARKER, do_marker, NULL);
	}

	return(ent);
} /* End procedure new_marker */

/*----------------------------------------------------------------------*\
| Procedure     :	BIF_Marker3 *new_marker3(int, *Real_int_union)
|------------------------------------------------------------------------|
| Description   :	Allocate and fill a marker3 structure.  All space is
|			allocated at once, allowing a single free to 
|			delete the structure.
|------------------------------------------------------------------------|
| Return        :	The pointer to the new structure.
\*----------------------------------------------------------------------*/
BIF_Marker3 *new_marker3(size,trip_list)
int size;
Real_int_union *trip_list;

{
	BIF_Marker3 *ent;
	ent = (BIF_Marker3 *)new_simple_triple(size,trip_list);

	if ( ent != NULL )
	{
		HEADER(ent, MARKER3, do_marker3, NULL);
	}
	return(ent);
} /* End procedure new_marker3 */

/*--------------------------------------------------------------------*\
| Procedure     :	BIF_Line *new_line(int, *Real_int_union)
|---------------------------------------------------------------------
| Description   :	Allocate and fill a line structure.  All space
|			is allocated at once, allowing a single free to 
|			delete the structure.
|---------------------------------------------------------------------
| Return        :	The pointer to the new structure.
\*--------------------------------------------------------------------*/
BIF_Line *new_line(size,trip_list)
int size;
Real_int_union *trip_list;

{
	BIF_Line *ent;
	ent = (BIF_Line *)new_simple_double(size,trip_list);

	{
		HEADER(ent, LINE, do_line, NULL);
	}
	return(ent);
} /* End procedure new_line */

/*--------------------------------------------------------------------*\
| Procedure     :	BIF_Line3 *new_line3(int, *Group_description)
|----------------------------------------------------------------------|
| Description   :	Allocate and fill a LINE3 entity w/ optional data
|----------------------------------------------------------------------|
| Return        :	The pointer to the new structure ( or NULL on error )
\*--------------------------------------------------------------------*/
BIF_Line3 *new_line3(numGroups, groups)
int numGroups;
Group_description *groups;
{
	static char *entName = "LINE3";
	static int vFlagData[3] = { 1, 1, 0 };
	static int fFlagData[3] = { 0, 0, 0 };
	static int eFlagData[1] = { 0 };

	int numCoords,numFacets,numEdges;

	BIF_Line3 *ent;

#ifdef TEST_PRINT
	fflush(stderr);
	printf("in new_line3 %d \n",numGroups);
	fflush(stdout);
#endif /* TEST_PRINT */
	/*------------------------------------------------------------*\
	|	Get the entity specific data groups
	\*------------------------------------------------------------*/
	scanForGroups(numGroups,groups,NUM_GROUPS_COORD,coordGroup,
		coordSize,coordList);

	/*------------------------------------------------------------*\
	|	Set the standard sizes
	\*------------------------------------------------------------*/
	numCoords = coordSize[0];
	numFacets = 0;
	numEdges  = 0;

	ent = (BIF_Line3 *)new_withdata3(
		entName, LINE3, do_line3,
		sizeof(BIF_Line3), numCoords, numFacets, numEdges,
		coordList[0], vFlagData, fFlagData, eFlagData,
		numGroups, groups);

	/* Finish off the entity */
	if ( ent != NULL )
	{
		/* Fill in the data fields */
		ent->vdata.num_vertices = numCoords;
		ent->vdata.vertex_data.points = ent->vertex_data.points;
	}

	/*------------------------------------------------------------*\
	|	Return the Entity Address
	\*------------------------------------------------------------*/
	return(ent);

} /* End procedure new_line3 */

/*--------------------------------------------------------------------*\
| Procedure     :	BIF_Polygon *new_polygon(int, *Real_int_union)
|----------------------------------------------------------------------|
| Description   :	Allocate and fill a polygon structure.  All
|			space is allocated at once, allowing a single
|			free to delete the structure.
|----------------------------------------------------------------------|
| Return        :	The pointer to the new structure.
\*--------------------------------------------------------------------*/
BIF_Polygon *new_polygon(size,trip_list)
int size;
Real_int_union *trip_list;

{
	BIF_Polygon *ent;
	ent = (BIF_Polygon *)new_simple_double(size,trip_list);

	if ( ent != NULL )
	{
		HEADER(ent,POLYGON,do_polygon,NULL);
	}
	return(ent);
} /* End procedure new_polygon */

/*--------------------------------------------------------------------*\
| Procedure     :	BIF_polygon3 *new_polygon3(int,
						*Group_description)
|----------------------------------------------------------------------|
| Description   :	Allocate and fill a POLYGON3 entity w/ optional
|			data (if any )
|----------------------------------------------------------------------|
| Return        :	The pointer to the new structure ( or NULL on error )
\*--------------------------------------------------------------------*/
BIF_Polygon3 *new_polygon3(numGroups, groups)
int numGroups;
Group_description *groups;

{
	static char *entName = "POLYGON3";
	static int vFlagData[3] = { 1, 1, 2 };
	static int fFlagData[3] = { 1, 1, 2 };
	static int eFlagData[1] = { 0 };

	int size, numCoords,numFacets,numEdges;

	BIF_Polygon3 *ent;

#ifdef TEST_PRINT
	fflush(stderr);
	printf("in new_polygon3 %d \n",numGroups);
	fflush(stdout);
#endif /* TEST_PRINT */
	/*------------------------------------------------------------*\
	|	Get the entity specific data groups
	\*------------------------------------------------------------*/
	scanForGroups(numGroups,groups,NUM_GROUPS_COORD,coordGroup,
		coordSize,coordList);

	/*------------------------------------------------------------*\
	|	Set the standard sizes
	\*------------------------------------------------------------*/
	numCoords = coordSize[0];
	numFacets = 1;
	numEdges  = numCoords;

	ent = (BIF_Polygon3 *)new_withdata3(
		entName, POLYGON3, do_polygon3,
		sizeof(BIF_Polygon3), numCoords, numFacets, numEdges,
		coordList[0], vFlagData, fFlagData, eFlagData,
		numGroups, groups);

	/* Finish off the entity */
	if ( ent != NULL )
	{
		/* Fill in the data fields */
	
		/* Fill in the facet data */
		size = sizeofFacetdata[ent->facet_data_flag];
		if (size) {
#ifdef SYSV
		  memcpy((char *)&ent->fdata.colr,
			 (char *)ent->facet_data.colrs, size);
#else
		  bcopy((char *)ent->facet_data.colrs,
			(char *)&ent->fdata.colr, size);
#endif
		}
		/* Fill in the vertex data */
		ent->vdata.points = ent->vertex_data.points;
	}

	/*------------------------------------------------------------*\
	|	Return the Entity Address
	\*------------------------------------------------------------*/
	return(ent);

} /* End procedure new_polygon3 */

/*--------------------------------------------------------------------*\
| Procedure     :	BIF_Fillareaset *new_fillareaset (int,
|						*Real_int_union)
|---------------------------------------------------------------------
| Description   :	Allocate and fill a fillareaset structure.  
|			All space is allocated at once, allowing a
|			single free to delete the structure.
|---------------------------------------------------------------------
| Return        :	The pointer to the new structure.
\*--------------------------------------------------------------------*/
BIF_Fillareaset *new_fillareaset(numContour, contList)
int numContour;
Real_int_union *contList;

{
	int i, j, numCoords;
	int contSize;
	int vfLength;
	Ppoint_list *psets;
	Ppoint *pnts;
	char *ptr;
	BIF_Fillareaset *ent;

/* Find the size of the Fillareaset in memory */
	numCoords = contour_counter(numContour,contList,3);
	
/* Malloc the memory */
	vfLength = numContour * sizeof(Ppoint_list)
		+ numCoords * sizeof(Ppoint);
	
	ent = ALLOCV(BIF_Fillareaset, vfLength);

	if ( ent != NULL )
	{
		/* Fill the structure */
		HEADER(ent,FILL_AREA_SET,do_fillareaset,NULL);

		ent->numContours = numContour;
		ptr = ((char *)ent) + sizeof(BIF_Fillareaset);

		/* Assign memory to the Ppoint_list structures */
		ent->sets = (Ppoint_list *)ptr;
		psets = ent->sets;
		ptr += numContour * sizeof(Ppoint_list);

		/* Assign memory to the Ppoint lists */
		pnts = (Ppoint *)ptr;

		for ( i = 0; i < numContour; i++ )
		{
			/* The contour size list */
			contSize = (contList++)->Int;
			psets->num_points = contSize;
			psets->points = pnts;

			/* The point arrays */
			for ( j = 0; j < contSize ; j++ )
			{
				pnts->x = (contList++)->Float;
				pnts->y = (contList++)->Float;
				contList++; /* Skip the Z */
				pnts++;
			}
			psets++;
		}
	}
	else
	/* Malloc Failed */
		ent = NULL;

	return(ent);
} /* End procedure new_fillareaset */

#ifdef EXTERNAL_NOTE
	The new_fillareaset3 routine has been completely rewritten
	to reflect the differences in the structure of the data groups
	(contours vs. straight lists) vs. all of the other "withData"
	BIF primitives.
#endif /*EXTERNAL_NOTE */

void loadWithDataSet();

/*--------------------------------------------------------------------*\
| Procedure     :	BIF_fillareaset3 *new_fillareaset3(int,
|						*Group_description)
|----------------------------------------------------------------------|
| Description   :	Allocate and fill a FILLAREASET3 entity with
|			optional data (if any )
|----------------------------------------------------------------------|
| Return        :	The pointer to the new structure ( or NULL on error )
\*--------------------------------------------------------------------*/
BIF_Fillareaset3 *new_fillareaset3(numGroups, groups)
int numGroups;
Group_description *groups;

{
	static char *entName = "FILLAREASET3";
	static int vFlagData[3] = { 1, 1, 2 };
	static int fFlagData[3] = { 1, 1, 2 };
	static int eFlagData[1] = { 1 };

	int numCoords,numFacets,numEdges;
	int numContours, entBodySize, vfLength;
	int i, contSize, vsize;
	Real_int_union *contList;
	Pedge_data_list *elist;
	Pfacet_vdata_list3 *vlist;
	Ppoint3 *pnts;
	Pedge_flag *epnts;
	char *ptr, *vptr, *fptr, *eptr;

	BIF_Fillareaset3 *ent;

#ifdef TEST_PRINT
	fflush(stderr);
	printf("in new_fillareaset3 %d \n",numGroups);
	fflush(stdout);
#endif /* TEST_PRINT */
	/*------------------------------------------------------------*\
	|	Get the vertex coordinate data groups
	\*------------------------------------------------------------*/
	scanForGroups(numGroups,groups,NUM_GROUPS_COORD,coordGroup,
		coordSize,coordList);

	/*------------------------------------------------------------*\
	|	Set the standard sizes
	\*------------------------------------------------------------*/
	numContours = coordSize[0];
	numCoords   = contour_counter(coordSize[0],coordList[0],3);
	numFacets   = 1;
	numEdges    = numCoords;

	/*------------------------------------------------------------*\
	|	The basic size....
	\*------------------------------------------------------------*/
	entBodySize = sizeof(BIF_Fillareaset3);	/* Body Size */
	vfLength = ( MAX_VAL(1,numCoords) * sizeof(Ppoint3) ) +
	    ( MAX_VAL(1,numContours) * sizeof(Pedge_data_list) ) +
	    ( MAX_VAL(1,numContours) * sizeof(Pfacet_vdata_list3) );
	    
	/*------------------------------------------------------------*\
	|	Find the ODG's and compute the sizes
	\*------------------------------------------------------------*/
	vfLength += scanWithDataSet(entName, numCoords, numFacets,
				    numEdges, numGroups, groups);

	/*------------------------------------------------------------*\
	|	Allocate the entity and fill it
	\*------------------------------------------------------------*/
	ent = (BIF_Fillareaset3 *)malloc(entBodySize+vfLength);
	if ( ent != NULL )
	{
		/*----------------------------------------------------*\
		|	Fill entity specific data
		|	Fixed Length Fields
		\*----------------------------------------------------*/
		HEADER(ent,FILL_AREA_SET3, do_fillareaset3,NULL);
		STDSIZE(ent, numCoords, numFacets, numEdges);
		ent->numContours = numContours;

		if ((sizeList[1] && vFlagData[1]) ||
		    (sizeList[4] && fFlagData[1]))
		    ent->colour_model = PINDIRECT;
		else
		    ent->colour_model = wk_info.color_model;

		/*----------------------------------------------------*\
		|	Vertex and Fact Data Flags
		\*----------------------------------------------------*/
		ent->vertex_data_flag = 
			makeVertexFlag(vFlagData[0],vFlagData[1],vFlagData[2]);
		ent->facet_data_flag =
			makeFacetFlag(fFlagData[0],fFlagData[1],fFlagData[2]);
		ent->edge_data_flag = 
			makeEdgeFlag(eFlagData[0]);
		
		makeWithDataFlag(ent->with_data_flag);

		/*----------------------------------------------------*\
		|	Fill the variable length fields
		\*----------------------------------------------------*/

		/*----------------------------------------------------*\
		|	Fill the Coords and the facet length array.
		\*----------------------------------------------------*/
		ptr = ((char *)ent) + entBodySize;

		/* Adjust to point at the Pedge_data_list structs */
		elist = ent->edata = (Pedge_data_list *)ptr;
		ptr += sizeof(Pedge_data_list) * numContours;

		/* Adjust to point to the Pfacet_vdata_list3 structs */
		vlist = ent->vdata = (Pfacet_vdata_list3 *)ptr;
		ptr += sizeof(Pfacet_vdata_list3) * numContours;

		pnts = (Ppoint3 *)ptr;

		/* Fill in the point data */
		fillVecSet(coordSize[0], coordList[0], ptr, numCoords,
			   sizeofVertexdata[ent->vertex_data_flag]);

		/* Where the rest of the vertex data will go */
		vptr = ptr + sizeof(Ppoint3);

		/* Where to put facet data */
		fptr = (char *)&ent->fdata.colr;

		/* Where to put edges */
		eptr = ptr + (MAX_VAL(1,numCoords) *
			      sizeofVertexdata[ent->vertex_data_flag]);
		epnts = (Pedge_flag *)eptr;

		/*----------------------------------------------------*\
		|	Fill the Optional Data fields
		\*----------------------------------------------------*/
		loadWithDataSet(ent, vptr, fptr, eptr);

		/*---------------------------------------------------*\
		|       Fill in the structure lists
		\*---------------------------------------------------*/
		vsize = sizeofVertexdata[ent->vertex_data_flag];
		contList =coordList[0];

		for ( i = 0; i < numContours; i++ )
		{
			contSize = (contList++)->Int;

			vlist->num_vertices = contSize;
			vlist->vertex_data.points = pnts;
			pnts = (Ppoint3 *)(((char *)pnts) + vsize * contSize);
			contList += contSize * 3;
			vlist++;
			if (ent->edge_data_flag) {
			  elist->num_edges = contSize;
			  elist->edgedata.edges = epnts;
			  epnts += contSize;
			  elist++;
			}
		}

	}

	/*------------------------------------------------------------*\
	|	Return the Entity Address
	\*------------------------------------------------------------*/
	return(ent);

} /* End procedure new_fillareaset3 */


/*--------------------------------------------------------------------*\
| Procedure     :	BIF_Triangle3 *new_triangle3(int,
|						*Group_description)
|----------------------------------------------------------------------|
| Description   :	Allocate and fill a TRIANGLE3 entity with
|			optional data (if any )
|----------------------------------------------------------------------|
| Return        :	The pointer to the new structure ( or NULL on error )
\*--------------------------------------------------------------------*/
BIF_Triangle3 *new_triangle3(numGroups, groups)
int numGroups;
Group_description *groups;

{
	static char *entName = "TRIANGLE3";
	static int vFlagData[3] = { 1, 1, 2 };
	static int fFlagData[3] = { 1, 1, 2 };
	static int eFlagData[1] = { 0 };

	int numCoords,numFacets,numEdges;

	BIF_Triangle3 *ent;

#ifdef TEST_PRINT
	fflush(stderr);
	printf("in new_triangle3 %d \n",numGroups);
	fflush(stdout);
#endif /* TEST_PRINT */
	/*------------------------------------------------------------*\
	|	Get the entity specific data groups
	\*------------------------------------------------------------*/
	scanForGroups(numGroups,groups,NUM_GROUPS_COORD,coordGroup,
		coordSize,coordList);

	/*------------------------------------------------------------*\
	|	Set the standard sizes
	\*------------------------------------------------------------*/
	numCoords = coordSize[0];
	numFacets = numCoords -2;
	numEdges  = numCoords;

	ent = (BIF_Triangle3 *)new_withdata3(
		entName, TRIANGLE3, do_triangle3,
		sizeof(BIF_Triangle3), numCoords, numFacets, numEdges,
		coordList[0], vFlagData, fFlagData, eFlagData,
		numGroups, groups);

	/* Finish off the entity */
	if ( ent != NULL )
	{
		/* Fill in the data fields */
		ent->fdata.colrs = ent->facet_data.colrs;
		ent->vdata.points = ent->vertex_data.points;
	}

	/*------------------------------------------------------------*\
	|	Return the Entity Address
	\*------------------------------------------------------------*/
	return(ent);

} /* End procedure new_triangle3 */

/*--------------------------------------------------------------------*\
| Procedure     :	BIF_Quadmesh3 *new_quadmesh3(int, int, int,
|						*Group_description)
|----------------------------------------------------------------------|
| Description   :	Allocate and fill a QUADMESH3 entity with
|			optional data (if any )
|----------------------------------------------------------------------|
| Return        :	The pointer to the new structure ( or NULL on error )
\*--------------------------------------------------------------------*/
BIF_Quadmesh3 *new_quadmesh3(numRows, numColumns, numGroups, groups)
int numRows, numColumns;
int numGroups;
Group_description *groups;

{
	static char *entName = "QUAD_MESH3";
	static int vFlagData[3] = { 1, 1, 2 };
	static int fFlagData[3] = { 1, 1, 2 };
	static int eFlagData[1] = { 0 };

	int numNeeded, numCoords, numFacets, numEdges;

	BIF_Quadmesh3 *ent;

#ifdef TEST_PRINT
	fflush(stderr);
	printf("in new_quadmesh3 %d \n",numGroups);
	fflush(stdout);
#endif /* TEST_PRINT */
	/*------------------------------------------------------------*\
	|	Get the entity specific data groups
	\*------------------------------------------------------------*/
	scanForGroups(numGroups,groups,NUM_GROUPS_COORD,coordGroup,
		coordSize,coordList);


	/*------------------------------------------------------------*\
	|	Set the standard sizes
	\*------------------------------------------------------------*/
	numCoords = coordSize[0];
	numFacets = ( numRows -1 )* ( numColumns - 1);
	numEdges  = numCoords;

	/*------------------------------------------------------------*\
	|	Check the size of the coords
	\*------------------------------------------------------------*/
	numNeeded = (numRows * numColumns);
	errorSize(entName, numNeeded, numCoords, VERTEX_COORDINATES);

	/*------------------------------------------------------------*\
	|	Allocate and fill the standard with data body
	\*------------------------------------------------------------*/
	ent = (BIF_Quadmesh3 *)new_withdata3(
		entName, QUAD_MESH3, do_quadmesh3,
		sizeof(BIF_Quadmesh3), numCoords, numFacets, numEdges,
		coordList[0], vFlagData, fFlagData, eFlagData,
		numGroups, groups);
	
	/* Finish off the entity */
	if ( ent != NULL )
	{
		ent->dim.size_x    = numColumns;
		ent->dim.size_y    = numRows;

		/* Fill in the data fields */
		ent->fdata.colrs = ent->facet_data.colrs;
		ent->vdata.points = ent->vertex_data.points;
	}

	/*------------------------------------------------------------*\
	|	Return the Entity Address
	\*------------------------------------------------------------*/
	return(ent);


} /* End procedure new_quadmesh3 */

/*--------------------------------------------------------------------*\
| Procedure     :	BIF_Indexpolygons3 *new_indexpolygons3(int,
|						*Group_description)
|----------------------------------------------------------------------|
| Description   :	Allocate and fill a INDEX_POLYGONS3 entity with
|			optional data (if any )
|----------------------------------------------------------------------|
| Return        :	The pointer to the new structure ( or NULL on error )
\*--------------------------------------------------------------------*/
BIF_Indexpolygons3 *new_indexpolygons3(numGroups, groups)
int numGroups;
Group_description *groups;

{
	static char *entName = "INDEX_POLYGONS3";
	static int vFlagData[3] = { 1, 1, 2 };
	static int fFlagData[3] = { 1, 1, 2 };
	static int eFlagData[1] = { 1 };

#define NUM_INDEXPOLY_DGS 2
	static int scanListL[NUM_INDEXPOLY_DGS] = 
	{
		VERTEX_COORDINATES,
		FACET_CONNECTIVITY
	};
	static int sizeListL[NUM_INDEXPOLY_DGS];
	static Real_int_union *listListL[NUM_INDEXPOLY_DGS];

	int connListSize;
	int ip3SizePlus, numCoords, numFacets, numEdges;

	Pedge_data_list *elst;
	Pint_list *vlst;
	Pint *vconns;
	Pedge_flag *edges;
	int i, j, facetSize;
	char *ptr;
	Real_int_union *connList;

	BIF_Indexpolygons3 *ent;

#ifdef TEST_PRINT
	fflush(stderr);
	printf("in new_indexpolygons3 %d \n",numGroups);
	fflush(stdout);
#endif /* TEST_PRINT */

	/*------------------------------------------------------------*\
	|	Get the entity specific data groups
	\*------------------------------------------------------------*/
	scanForGroups(numGroups,groups,NUM_INDEXPOLY_DGS,scanListL,
		sizeListL,listListL);


	/*------------------------------------------------------------*\
	|	Set the standard sizes
	\*------------------------------------------------------------*/
	numCoords = sizeListL[0];
	numFacets = sizeListL[1];
	numEdges = contour_counter(sizeListL[1],listListL[1],1);

	connListSize = numFacets * sizeof(Pedge_data_list_list) +
	  numFacets * sizeof(Pedge_data_list) + numFacets *
	  sizeof(Pint_list_list) + numFacets * sizeof(Pint_list) +
	  numEdges * sizeof(Pint);
	ip3SizePlus = sizeof(BIF_Indexpolygons3) + connListSize;

	/*------------------------------------------------------------*\
	|	Get the std withdata body loaded
	\*------------------------------------------------------------*/
	ent = (BIF_Indexpolygons3 *)new_withdata3(
		entName, INDEX_POLYGONS3, do_indexpolygons3,
		ip3SizePlus, numCoords, numFacets, numEdges,
		listListL[0], vFlagData, fFlagData, eFlagData,
		numGroups, groups);
	
	/*------------------------------------------------------------*\
	|	Finish off the entity
	\*------------------------------------------------------------*/
	if ( ent != NULL )
	{
		/*----------------------------------------------------*\
		|	Convert the facet and fill connectivity list
		\*----------------------------------------------------*/
		ptr = ((char *)ent) + sizeof(BIF_Indexpolygons3);

		ent->fdata.colrs = ent->facet_data.colrs;
		ent->vdata.num_vertices = numCoords;
		ent->vdata.vertex_data.points = ent->vertex_data.points;

		ent->edata = (Pedge_data_list_list *)ptr;
		ptr += numFacets * sizeof(Pedge_data_list_list);
		elst = (Pedge_data_list *)ptr;
		ptr += numFacets * sizeof(Pedge_data_list);

		ent->vlist = (Pint_list_list *)ptr;
		ptr += numFacets * sizeof(Pint_list_list);
		vlst = (Pint_list *)ptr;
		ptr += numFacets * sizeof(Pint_list);

		vconns = (Pint *)ptr;
		edges = ent->edge_data.edges;
		connList =listListL[1];

		for ( i = 0; i < numFacets; i++ )
		{
		  ent->vlist[i].num_lists = 1;
		  ent->vlist[i].lists = vlst++;
		  facetSize = (connList++)->Int;
		  ent->vlist[i].lists[0].num_ints = facetSize;
		  ent->vlist[i].lists[0].ints = vconns;
		  if (ent->edge_data_flag) {
		    ent->edata[i].num_lists = 1;
		    ent->edata[i].edgelist = elst++;
		    ent->edata[i].edgelist[0].num_edges = facetSize;
		    ent->edata[i].edgelist[0].edgedata.edges = edges;
		    edges += facetSize;
		  }

		  for (j = 0; j < facetSize; j++) {
		    *(vconns++) = (connList++)->Int;
		  }
	      }
	}

	/*------------------------------------------------------------*\
	|	Return the Entity Address
	\*------------------------------------------------------------*/
	return(ent);

} /* End procedure new_indexpolygons3 */

/*----------------------------------------------------------------------*\
| Procedure     :	BIF_Text *new_text(float, float, float, *char) 
|------------------------------------------------------------------------
| Description   :	Allocates and fills a text structure 
|------------------------------------------------------------------------
| Return        :	The pointer to the new structure
\*----------------------------------------------------------------------*/
BIF_Text *new_text( x, y, qtext_string ) 
float x, y;
char *qtext_string;

{
	BIF_Text 	*ent;
	char 		*ptr;
	int		total;

	total = sizeof( BIF_Text ) +
		( strlen( qtext_string ) + 1 ) * sizeof( char );

	ptr = malloc((unsigned)total);
	ent = (BIF_Text *)ptr;
	if ( ptr != NULL )
	{
	/* Fill in the entity data */
		HEADER(ent, TEXT, do_text, NULL);

		ent->text_pt.x = x;
		ent->text_pt.y = y;
		ent->text_string = ptr + sizeof(BIF_Text);
		strcpy( ent->text_string, qtext_string );
	}

	return( ent );
}/* End new_text */

/*----------------------------------------------------------------------*\
| Procedure     :	BIF_Text3 *new_text3(float, float, float, *char) 
|------------------------------------------------------------------------
| Description   :	Allocates and fills a text3 structure 
|------------------------------------------------------------------------
| Return        :	The pointer to the new structure
\*----------------------------------------------------------------------*/
BIF_Text3 *new_text3( x, y, z, qtext_string,
	tbx, tby, tbz, tux, tuy, tuz) 

float x, y, z;
char *qtext_string;
float tbx, tby, tbz, tux, tuy, tuz;

{
	BIF_Text3 	*ent;
	char 		*ptr;
	int		total;

	total = sizeof( BIF_Text3 ) +
		( strlen( qtext_string ) + 1 ) * sizeof( char );

	ptr = malloc((unsigned)total);
	ent = (BIF_Text3 *)ptr;
	if ( ptr != NULL )
	{
	/* Fill in the entity data */
		HEADER(ent, TEXT3, do_text3, NULL);

		ent->text_pt.x = x;
		ent->text_pt.y = y;
		ent->text_pt.z = z;

		ent->dir[0].delta_x = tbx;
		ent->dir[0].delta_y = tby;
		ent->dir[0].delta_z = tbz;

		ent->dir[1].delta_x = tux;
		ent->dir[1].delta_y = tuy;
		ent->dir[1].delta_z = tuz;

		ent->text_string = ptr + sizeof(BIF_Text3);
		strcpy( ent->text_string, qtext_string );
	}

	return( ent );
}/* End new_text3 */

/*----------------------------------------------------------------------*\
| Procedure     :	BIF_Anno_text3 *new_annotext3(
|						float, float, float,
|						float, float, float,
|						*char) 
|------------------------------------------------------------------------
| Description   :	Allocates and fills a annotext3 structure 
|------------------------------------------------------------------------
| Return        :	The pointer to the new structure
\*----------------------------------------------------------------------*/
BIF_Anno_text3 *new_annotext3( x, y, z, ox, oy, oz, qtext_string)

float  x,  y,  z;
float ox, oy, oz;
char *qtext_string;

{
	BIF_Anno_text3 	*ent;
	char 		*ptr;
	int		total;

	total = sizeof( BIF_Anno_text3 ) +
		( strlen( qtext_string ) + 1 ) * sizeof( char );

	ptr = malloc((unsigned)total);
	ent = (BIF_Anno_text3 *)ptr;
	if ( ptr != NULL )
	{
	/* Fill in the entity data */
		HEADER(ent,ANNOTATION_TEXT3,do_annotext3,NULL);

		ent->ref_pt.x  = x;
		ent->ref_pt.y  = y;
		ent->ref_pt.z  = z;

		ent->anno_offset.x = ox;
		ent->anno_offset.y = oy;
		ent->anno_offset.z = oz;

		ent->text_string = ptr + sizeof(BIF_Anno_text3);
		strcpy( ent->text_string, qtext_string );
	}

	return( ent );
}/* End new_annotext3 */

/*----------------------------------------------------------------------*\
| Procedure     :	BIF_Lightstate *new_lightstate(int,
|						*Group_description)
|------------------------------------------------------------------------
| Description   :	Allocates and fills a LIGHT_STATE entity
|------------------------------------------------------------------------
| Return        :	The pointer to the new structure
\*----------------------------------------------------------------------*/
BIF_Lightstate *new_lightstate( num_groups, grp_header )
int num_groups;
Group_description *grp_header;

{
	int i, group;
	int number_to_on, number_to_off;
	Real_int_union *onlist, *offlist;
	int error, numskip;
	char *ptr;
	BIF_Lightstate *ent;
	Pint *listp;

/* Find the Size of it */
	number_to_on  = 0;
	number_to_off = 0;
	onlist = NULL;
	offlist = NULL;
	for ( group = 0 ; group < num_groups ; group++ )
	{
		switch (grp_header[group].name)
		{
		case ACTIVATE_LIST :
			number_to_on = grp_header[group].number;
			onlist = grp_header[group].list;
			break;
		case DEACTIVATE_LIST :
			number_to_off = grp_header[group].number;
			offlist = grp_header[group].list;
			break;
		}

	}

/* The two extra integers are for the case of
  number_to_off or number_to_off = 0 */
	ptr = malloc(sizeof(BIF_Lightstate) + 
		     2 * sizeof(Pint_list) +
		     (number_to_on + number_to_off) * 
		     sizeof(Pint));
	if ( ptr != NULL )
	{
		ent = (BIF_Lightstate *)ptr;
		ptr += sizeof(BIF_Lightstate);
		ent->activation.ints = (Pint *)ptr;
		/* Fill the entity structures */
		/* The header */
		HEADER(ent, LIGHT_STATE, do_lightstate, NULL);
		/* The lists */	
		/* The On List */
		listp = ent->activation.ints;
		numskip = 0;
		for ( i = 0; i < number_to_on; i++ )
		{
			*listp = onlist[i].Int;

			/* Check the index */
			error = indexRange(LIGHT_STATE, (BIF_INT)(*listp),
				1, BIF_MAX_LIGHTS);
			if ( error != 0 )
				/* Skip baad lights */
				numskip++;
			else
				listp++;
		}
		ent->activation.num_ints = number_to_on - numskip;
		ptr += ent->activation.num_ints * sizeof(Pint);
		ent->deactivation.ints = (Pint *)ptr;

		/* The Off List */
		listp = ent->deactivation.ints;
		numskip = 0;
		for ( i = 0; i < number_to_off; i++ )
		{
			*listp = offlist[i].Int;

			/* Check the index */
			error = indexRange(LIGHT_STATE, (BIF_INT)(*listp),
				1, BIF_MAX_LIGHTS);
			if ( error != 0 )
				/* Skip baad lights */
				numskip++;
			else
				listp++;
		}
		ent->deactivation.num_ints = number_to_off - numskip;

	}

	return( ent );

} /* End new_lightstate */

/*----------------------------------------------------------------------*\
| Procedure     :	BIF_All *new_generic(*BIF_All, int, int, (*int)())
|------------------------------------------------------------------------
| Description   :	Allocate a new generic BIF entity
|------------------------------------------------------------------------
| Return        :	The pointer to the new structure
\*----------------------------------------------------------------------*/
BIF_All *new_generic( buffer, size, type, handler )

BIF_All *buffer;
int size;
int type;
int (*handler)();

{

	BIF_All *ent;
	BIF_Any *enta;
/* Allocate the memory */
	ent = (BIF_All *)malloc((unsigned)size);
	if ( ent != NULL )
	{
	/* Copy from the buffer  ( the body data ) */
#ifdef SYSV
		memcpy((char *)ent, (char *)buffer, size);
#else
		bcopy((char *)buffer, (char *)ent, size);
#endif
			
	/* Fill the entity header */
		enta = &ent->any;
		HEADER(enta, type, handler, NULL);
	}

	return( ent );

} /* End new_generic */

/*--------------------------------------------------------------------*\
| Procedure     :	int scanForGroups( int, Group_description *,
|				int, int *, int *, Real_int_union *,)
|---------------------------------------------------------------------
| Description   :	scans a group headers list for the groups
|			whoses name are given in the scanList.
|			Store their size and starting location in
|			sizeList and listList respectively.
|---------------------------------------------------------------------
| Return        :	Error Code (not Implemented)
\*--------------------------------------------------------------------*/

int scanForGroups(numGroups,groups,numScan,scanList,sizeList,listList)
int numGroups;
Group_description *groups;

int numScan;
int scanList[];

int sizeList[];
Real_int_union *listList[];

{
	int scan, group, found;


	/*------------------------------------------------------------*\
	|	For each of the requested groups scan the groups
	|	in the groups list.
	\*------------------------------------------------------------*/
	for ( scan = 0 ; scan < numScan  ; scan++ )
	{
		/* Initialize the scan */
		found = 0;
		sizeList[scan] = 0;
		listList[scan] = (Real_int_union *)NULL;

		for ( group=0 ; group < numGroups && !found ; group++ )
		{
			/* When a match is found save the size etc. */
			if (groups[group].name == scanList[scan])
			{
				found = 1;
				sizeList[scan] = groups[group].number;
				listList[scan] = groups[group].list;
			}
		}
	}
}

/*--------------------------------------------------------------------*\
| Procedure     :	int fillVec(int, Real_int_union *,
|			char *, int, *vector[] )
|---------------------------------------------------------------------
| Description   :	Assign the malloc'd space to the array and
|			fill it.  If the supplied triplet list is too
|			short, the final entry is duplicated to the end
|			of the array.
|---------------------------------------------------------------------
| Return        :	The number of bytes used by the filled array.
\*--------------------------------------------------------------------*/
void fillVec(actSize, tripList, ptr, reqSize, adSize)
int actSize;
Real_int_union *tripList;
char * ptr;
int reqSize;
int adSize;
{
	int i;
	Pvec3 *vecs, *dupeMe;

	/* Establish the base of the array */
	vecs = (Pvec3 *)ptr;

	/* Fill It */
	if ( actSize > 0 )
	{

		/* Copy the data */
		for ( i = 0 ; i < actSize ; i++ )
		{
			vecs->delta_x = (tripList++)->Float;
			vecs->delta_y = (tripList++)->Float;
			vecs->delta_z = (tripList++)->Float;
			vecs = (Pvec3 *)(((char *)vecs) + adSize);
		}

		/* Dupe the last entry ( if needed ) */
		dupeMe = (Pvec3 *)(((char *)vecs) - adSize);
		for ( i = actSize ; i < reqSize ; i++ )
		{
			vecs->delta_x = dupeMe->delta_x;
			vecs->delta_y = dupeMe->delta_y;
			vecs->delta_z = dupeMe->delta_z;
			vecs = (Pvec3 *)(((char *)vecs) + adSize);
		}
	}
	else
	{
	/* Default Empty List has one zero entry */
	/* To possible reference from bombing    */
		vecs->delta_x = 0.0;
		vecs->delta_y = 0.0;
		vecs->delta_z = 0.0;
	}

} /* End fillVec() */

/*--------------------------------------------------------------------*\
| Procedure     :	int fillInt(int, Real_int_union *,
|			char *, int, *int[], int )
|---------------------------------------------------------------------
| Description   :	Assign the malloc'd space to the array and
|			fill it.  If the supplied integer list is too
|			short, the final entry is duplicated to the end
|			of the array.
|---------------------------------------------------------------------
| Return        :	The number of bytes used by the filled array.
\*--------------------------------------------------------------------*/
void fillInt(actSize, tripList, ptr, reqSize, adSize, colorInd)
int actSize;
Real_int_union *tripList;
char * ptr;
int reqSize;
int adSize;
int colorInd;

{
	int i, *vals, *dupeMe;

	/* Establish the base of the array */
	vals = (int *)ptr;

	/* Fill It */
	if ( actSize > 0 )
	{

	    /* Copy the data */
	    for ( i = 0 ; i < actSize ; i++ )
	    {	
#ifdef EXTERNALNOTE
		/* index is incremented by one so that color '0' does
		   not step on the default BG color. */
#endif
		*vals = (tripList++)->Int + colorInd;
		vals = (int *)(((char *)vals) + adSize);
	    }

	    /* Dupe the last entry ( if needed ) */
	    dupeMe = (int *)(((char *)vals) - adSize);
	    for ( i = actSize ; i < reqSize ; i++ )
	    {
		*vals = *dupeMe;
		vals = (int *)(((char *)vals) + adSize);
	    }
	}
	else
	{
	/* Default Empty List has one zero entry */
	/* To possible reference from bombing    */
		*vals = 0;
	}

} /* End fillInt() */

/*--------------------------------------------------------------------*\
| Procedure     :	errorSize(char *, int, int, int)
|---------------------------------------------------------------------
| Description   :	report if an actual group size is not correct
|---------------------------------------------------------------------
| Return        :	Error Code:
|				 0 no error
|				-1 error, error. sterilize, sterilize.
\*--------------------------------------------------------------------*/
int errorSize(entName,correctSize, actualSize, groupNameToken)
char *entName;
int correctSize, actualSize, groupNameToken;

{
	int retCode;
	char buffy[256];
	char *find_keyword_token();

	if ( actualSize && ( correctSize != actualSize ) )
	{
		/* Send an error message */
		sprintf(buffy,
			"In %s: %s size error (%d vs. %d).\n",
			entName,find_keyword_token((BIF_INT)groupNameToken),
			actualSize, correctSize);
		yyerror(buffy);
		retCode = -1;
	}
	else
		retCode = 0;
	
	return(retCode);
} /* End errorSize */

/*--------------------------------------------------------------------*\
| Procedure     :	int scanWithData(*char, int, int, int, int,
|						*Group_description )
|---------------------------------------------------------------------
| Description   :	Scan the groups for the standard ODG's
|			filling the static arrays sizeList and listList
|---------------------------------------------------------------------
| Return        :	The number of bytes needed by the ODG's
\*--------------------------------------------------------------------*/

int scanWithData(entName,numCoords,numFacets,numEdges,
	numGroups,groups)
				
char *entName;
int numCoords,numFacets,numEdges;
int numGroups;
Group_description *groups;

{
	int odgLength, i;
	int numRec;

	/*------------------------------------------------------------*\
	|	Find the out how many data groups we have:
	\*------------------------------------------------------------*/
	scanForGroups(numGroups,groups,NUM_STD_ODGS,scanList,
		sizeList,listList);

	/*------------------------------------------------------------*\
	|	Accumulate memory req's of the ODG's
	|	Check sizes of the ODG's
	|	Minimum Size req. is 1*sizeof(<ODG record>)
	\*------------------------------------------------------------*/
	odgLength = 0;
	for( i=0 ; i < STD_ODGS_V ; i++)
	{
		odgLength += ODGSIZE(sizeList[i], numCoords,
			sizeofList[i]);
		if ( sizeList[i] )
			odgLength += ( numCoords * sizeofList[i] );
		errorSize(entName, numCoords, sizeList[i], scanList[i]);
	}

	for( i= STD_ODGS_V ; i <  STD_ODGS_F ; i++)
	{
		odgLength += ODGSIZE(sizeList[i], numFacets,
			sizeofList[i]);
		errorSize(entName, numFacets, sizeList[i], scanList[i]);
	}

	for( i= STD_ODGS_F ; i <  STD_ODGS_E ; i++)
	{
		odgLength += ODGSIZE(sizeList[i], numEdges,
			sizeofList[i]);
		numRec = contour_counter(sizeList[i],listList[i],
					recSizeList[i]);
		errorSize(entName, numEdges, numRec, scanList[i]);
	}

	/*------------------------------------------------------------*\
	|	Return the size of the ODG's (in bytes)
	\*------------------------------------------------------------*/
	return(odgLength);

} /* End scanWithData */

/*--------------------------------------------------------------------*\
| Procedure     :	int scanWithDataSet(*char, int, int, int, int,
|						*Group_description )
|---------------------------------------------------------------------
| Description   :	Scan the groups for the standard ODG Sets
|			filling the static arrays sizeList and listList
|---------------------------------------------------------------------
| Return        :	The number of bytes needed by the ODG's
\*--------------------------------------------------------------------*/
int scanWithDataSet(entName,numCoords,numFacets,numEdges,
	numGroups,groups)
				
char *entName;
int numCoords,numFacets,numEdges;
int numGroups;
Group_description *groups;

{
	int odgLength, i;
	int numRec;

	/*------------------------------------------------------------*\
	|	Find the out how many data groups we have:
	\*------------------------------------------------------------*/
	scanForGroups(numGroups,groups,NUM_STD_ODGS,scanList,
		sizeList,listList);

	/*------------------------------------------------------------*\
	|	Accumulate memory req's of the ODG's
	|	Check sizes of the ODG's
	|	Minimum Size req. is 1*sizeof(<ODG record>)
	\*------------------------------------------------------------*/
	odgLength = 0;
	for( i=0 ; i < STD_ODGS_V ; i++)
	{
		odgLength += ODGSIZE(sizeList[i], numCoords,
			sizeofList[i]);
		if ( sizeList[i] )
			odgLength += ( numCoords * sizeofList[i] );
		numRec = contour_counter(sizeList[i],listList[i],
					recSizeList[i]);
		errorSize(entName, numCoords, numRec, scanList[i]);
	}

	for( i= STD_ODGS_V ; i <  STD_ODGS_F ; i++)
	{
		odgLength += ODGSIZE(sizeList[i], numFacets,
			sizeofList[i]);
		errorSize(entName, numFacets, sizeList[i], scanList[i]);
	}

	for( i= STD_ODGS_F ; i <  STD_ODGS_E ; i++)
	{
		odgLength += ODGSIZE(sizeList[i], numEdges,
			sizeofList[i]);
		numRec = contour_counter(sizeList[i],listList[i],
					recSizeList[i]);
		errorSize(entName, numEdges, numRec, scanList[i]);
	}

	/*------------------------------------------------------------*\
	|	Return the size of the ODG's (in bytes)
	\*------------------------------------------------------------*/
	return(odgLength);

} /* End scanWithDataSet */


/*--------------------------------------------------------------------*\
| Procedure     :	int loadWithData(ent, vptr, fptr, eptr);
|---------------------------------------------------------------------
| Description   :	Load the scan'd (see scanWithData) groups into
|			the STD_DATA_BODY
|---------------------------------------------------------------------
| Return        :	The amount of startPtr used in loadWithData
|			(in bytes).
\*--------------------------------------------------------------------*/
void loadWithData(ent, vptr, fptr, eptr)
BIF_Withdata3 *ent;
char *vptr, *fptr, *eptr;

{
	/*------------------------------------------------------------*\
	|	Fill the Vertex Colors
	\*------------------------------------------------------------*/
        if (sizeList[0]) {
	  fillVec(sizeList[0], listList[0], vptr, ent->numCoords,
		  sizeofVertexdata[ent->vertex_data_flag]);
	  vptr += sizeof(Pcoval);
	}
		
	/*------------------------------------------------------------*\
	|	Fill the Vertex Color Indices
	\*------------------------------------------------------------*/
        if (sizeList[1]) {
	  fillInt(sizeList[1], listList[1], vptr, ent->numCoords,
		  sizeofVertexdata[ent->vertex_data_flag], 1);
	  vptr += sizeof(Pcoval);
	}
		
	/*------------------------------------------------------------*\
	|	Fill the Vertex Normals
	\*------------------------------------------------------------*/
        if (sizeList[2]) {
	  fillVec(sizeList[2], listList[2], vptr, ent->numCoords,
		  sizeofVertexdata[ent->vertex_data_flag]);
	  vptr += sizeof(Pvec3);
	}
		
	/*------------------------------------------------------------*\
	|	Fill the Facet Colors
	\*------------------------------------------------------------*/
        if (sizeList[3]) {
	  fillVec(sizeList[3], listList[3], fptr, ent->numFacets,
		  sizeofFacetdata[ent->facet_data_flag]);
	  fptr += sizeof(Pcoval);
	}
		
	/*------------------------------------------------------------*\
	|	Fill the Facet Color Indices
	\*------------------------------------------------------------*/
        if (sizeList[4]) {
	  fillInt(sizeList[4], listList[4], fptr, ent->numFacets,
		  sizeofFacetdata[ent->facet_data_flag], 1);
	  fptr += sizeof(Pcoval);
	}
		
	/*------------------------------------------------------------*\
	|	Fill the Facet Normals
	\*------------------------------------------------------------*/
        if (sizeList[5]) {
	  fillVec(sizeList[5], listList[5], fptr, ent->numFacets,
		  sizeofFacetdata[ent->facet_data_flag]);
	  fptr += sizeof(Pvec3);
	}
		
	/*------------------------------------------------------------*\
	|	Fill the Edge Data
	\*------------------------------------------------------------*/
        if (sizeList[6]) {
	  fillInt(sizeList[6], listList[6], eptr, ent->numEdges,
		  sizeof(Pedge_flag), 0);
	}
		
	/*------------------------------------------------------------*\
	|	Return the Entity Address
	\*------------------------------------------------------------*/

} /* End procedure loadWithData */

/*--------------------------------------------------------------------*\
| Procedure     :    int loadWithDataSet(*BIF_Withdata3, *char, *char, *char)
|---------------------------------------------------------------------
| Description   :    Load the scan'd (see scanWithData) groups into
|		     the STD_DATA_BODY for "set type" groups...
|		     specifically for FILLAREASET3
|---------------------------------------------------------------------
| Return        :    The amount of startPtr used in loadWithDataSet
|		     (in bytes).
\*--------------------------------------------------------------------*/
void loadWithDataSet(ent, vptr, fptr, eptr)
BIF_Withdata3 *ent;
char *vptr, *fptr, *eptr;

{
	/*------------------------------------------------------------*\
	|	Fill the Vertex Colors
	\*------------------------------------------------------------*/
        if (sizeList[0]) {
	    fillVecSet(sizeList[0], listList[0], vptr, ent->numCoords,
		       sizeofVertexdata[ent->vertex_data_flag]);
	    vptr += sizeof(Pcoval);
	}
		
	/*------------------------------------------------------------*\
	|	Fill the Vertex Color Indices
	\*------------------------------------------------------------*/
	if (sizeList[1]) {
	    fillIntSet(sizeList[1], listList[1], vptr, ent->numCoords,
		       sizeofVertexdata[ent->vertex_data_flag], 1);
	    vptr += sizeof(Pcoval);
	}
		
	/*------------------------------------------------------------*\
	|	Fill the Vertex Normals
	\*------------------------------------------------------------*/
	if (sizeList[2]) {
	    fillVecSet(sizeList[2], listList[2], vptr, ent->numCoords,
		       sizeofVertexdata[ent->vertex_data_flag]);
	    vptr += sizeof(Pvec3);
	}
		
	/*------------------------------------------------------------*\
	|	Fill the Facet Colors
	\*------------------------------------------------------------*/
	if (sizeList[3]) {
	    fillVec(sizeList[3], listList[3], fptr, ent->numFacets,
		    sizeofFacetdata[ent->facet_data_flag]);
	    fptr += sizeof(Pcoval);
	}
		
	/*------------------------------------------------------------*\
	|	Fill the Facet Color Indices
	\*------------------------------------------------------------*/
	if (sizeList[4]) {
	    fillInt(sizeList[4], listList[4], fptr, ent->numFacets,
		    sizeofFacetdata[ent->facet_data_flag], 1);
	    fptr += sizeof(Pcoval);
	}
		
	/*------------------------------------------------------------*\
	|	Fill the Facet Normals
	\*------------------------------------------------------------*/
	if (sizeList[5]) {
	    fillVec(sizeList[5], listList[5], fptr, ent->numFacets,
		    sizeofFacetdata[ent->facet_data_flag]);
	    fptr += sizeof(Pvec3);
	}
		
	/*------------------------------------------------------------*\
	|	Fill the Edge Data
	\*------------------------------------------------------------*/
	if (sizeList[6]) {
	    fillIntSet(sizeList[6], listList[6], eptr, ent->numEdges,
		       sizeof(Pedge_flag), 0);
	}		

} /* End procedure loadWithDataSet */

/*--------------------------------------------------------------------*\
| Procedure     :	int makeVertexFlag(int, int, int)
|---------------------------------------------------------------------
| Description   :	add the contribution values that make up
|			the vertex data flag.
|
|			The assumption is that the flag is composed of
|			the sum of powers of two, (arithmetic bit
|			setting).
|---------------------------------------------------------------------
| Return        :	the sum of the contributions of the "active"
|			ODG's
\*--------------------------------------------------------------------*/
int makeVertexFlag(colors,indices,normals)
int colors,indices,normals;

{
	return ( (sizeList[0] ? colors  : 0 )	/* Colors  */
		+(sizeList[1] ? indices : 0 ) 	/* Indices */
		+(sizeList[2] ? normals : 0 ) );	/* Normals */

} /* End makeVertexFlag */

/*--------------------------------------------------------------------*\
| Procedure     :	int makeFacetFlag(int, int, int)
|---------------------------------------------------------------------
| Description   :	add the contribution values that make up
|			the facet data flag.
|
|			The assumption is that the flag is composed of
|			the sum of powers of two, (arithmetic bit
|			setting).
|---------------------------------------------------------------------
| Return        :	the sum of the contributions of the "active"
|			ODG's
\*--------------------------------------------------------------------*/
int makeFacetFlag(colors,indices,normals)
int colors,indices,normals;

{
	return ( (sizeList[3] ? colors  : 0 )	/* Colors  */
		+(sizeList[4] ? indices : 0 ) 	/* Indices */
		+(sizeList[5] ? normals : 0 ) );	/* Normals */

} /* makeFacetFlag */

/*--------------------------------------------------------------------*\
| Procedure     :	int makeEdgeFlag(int)
|---------------------------------------------------------------------
| Description   :	add the contribution values that make up
|			the vertex data flag.
|
|			The assumption is that the flag is composed of
|			the sum of powers of two, (arithmetic bit
|			setting).
|---------------------------------------------------------------------
| Return        :	the sum of the contributions of the "active"
|			ODG's
\*--------------------------------------------------------------------*/
int makeEdgeFlag(edges)
int edges;

{
	return ( (sizeList[6] ? edges  : 0 ) );	/* Edges  */

} /* End makeEdgeFlag */

/*--------------------------------------------------------------------*\
| Procedure     :	int makeWithDataFlag(* int)
|---------------------------------------------------------------------
| Description   :	Set the fields in the with_data_flag array
|			given.
|---------------------------------------------------------------------
| Return        :	Error Code
\*--------------------------------------------------------------------*/
int makeWithDataFlag(flag)
int *flag;

{
	flag[VCOLORT] = ( sizeList[0] > 0 ) ? TRUE : FALSE;
	flag[VCOLORI] = ( sizeList[1] > 0 ) ? TRUE : FALSE;
	flag[VNORM]   = ( sizeList[2] > 0 ) ? TRUE : FALSE;
	flag[FCOLORT] = ( sizeList[3] > 0 ) ? TRUE : FALSE;
	flag[FCOLORI] = ( sizeList[4] > 0 ) ? TRUE : FALSE;
	flag[FNORM  ] = ( sizeList[5] > 0 ) ? TRUE : FALSE;
	flag[EDATA]   = ( sizeList[6] > 0 ) ? TRUE : FALSE;
} /* End makeWithDataFlag() */

/*--------------------------------------------------------------------*\
| Procedure     :	BIF_Withdata3 *new_withdata3( *char, int,
|					(*int)(), int, int, int, int,
|					*Real_int_union, *int, *int,
|					*int, int, *Group_description)
|----------------------------------------------------------------------|
| Description   :	Allocate and fill a Withdata entity with optional data
|		(if any ) this is called from some other (line3, 
|		polygon3, et. al.) "new" entity routine, which finds
|		the Coords stuff and supplied the correct info & sizes. 
|----------------------------------------------------------------------|
| Return        :	The pointer to the new structure ( or NULL on error )
\*--------------------------------------------------------------------*/
BIF_Withdata3 *new_withdata3(entName,entType,entHandler,entBodySize,
		numCoords,numFacets,numEdges,
		coordList, vFlagData,fFlagData,eFlagData,
		numGroups, groups)

char *entName;
int entType;
int (*entHandler)();
int entBodySize;			/* Including extra arrays */
int numCoords,numFacets,numEdges;
Real_int_union coordList[];
int vFlagData[],fFlagData[],eFlagData[];
int numGroups;
Group_description *groups;

{
	BIF_Withdata3 *ent;
	char *ptr, *vptr, *fptr, *eptr;
	int vfLength;

#ifdef TEST_PRINT
	fflush(stderr);
	printf("in new_withdata3 %d \n",numGroups);
	fflush(stdout);
#endif /* TEST_PRINT */

	/*------------------------------------------------------------*\
	|	Find the ODG's and compute the sizes
	\*------------------------------------------------------------*/
	vfLength =( MAX_VAL(1,numCoords) *  sizeof(Ppoint3) );
	vfLength += scanWithData(entName,numCoords,numFacets, numEdges,
			numGroups, groups);

	/*------------------------------------------------------------*\
	|	Allocate the entity and fill it
	\*------------------------------------------------------------*/
	ent = (BIF_Withdata3 *)malloc((unsigned)(entBodySize+vfLength));
	if ( ent != NULL )
	{
		/*----------------------------------------------------*\
		|	Fill entity specific data
		|	Fixed Length Fields
		\*----------------------------------------------------*/
		HEADER(ent,entType,entHandler,NULL);
		STDSIZE(ent,numCoords,numFacets, numEdges);

		if ((sizeList[1] && vFlagData[1]) ||
		    (sizeList[4] && fFlagData[1]))
		    ent->colour_model = PINDIRECT;
		else
		    ent->colour_model = wk_info.color_model;

		/*----------------------------------------------------*\
		|	Vertex and Fact Data Flags
		\*----------------------------------------------------*/
		ent->vertex_data_flag =
			makeVertexFlag(vFlagData[0],vFlagData[1],vFlagData[2]);
		ent->facet_data_flag =
			makeFacetFlag(fFlagData[0],fFlagData[1],fFlagData[2]);
		ent->edge_data_flag = makeEdgeFlag(eFlagData[0]);
		
		makeWithDataFlag(ent->with_data_flag);

		/*----------------------------------------------------*\
		|	Fill the variable length fields
		\*----------------------------------------------------*/
		ptr = ((char *)ent) + entBodySize;

		/* Fill the Coords */
		ent->vertex_data.points = (Ppoint3 *)ptr;
		fillVec(numCoords, coordList, ptr, numCoords,
			sizeofVertexdata[ent->vertex_data_flag]);

		/* Adjust to address for loading vertex data */
		vptr = ptr + sizeof(Ppoint3);

		fptr = ptr + (MAX_VAL(1,numCoords) *
			      sizeofVertexdata[ent->vertex_data_flag]);
		ent->facet_data.colrs = (Pcoval *)fptr;

		eptr = fptr + (MAX_VAL(1,numFacets) *
			       sizeofFacetdata[ent->facet_data_flag]);
		ent->edge_data.edges = (Pedge_flag *) eptr;

		/* Fill the With Data fields */
		loadWithData(ent, vptr, fptr, eptr);
	}

	/*------------------------------------------------------------*\
	|	Return the Entity Address
	\*------------------------------------------------------------*/
#ifdef TEST_PRINT
	printf("new_withdata3 $\n");
	fflush(stdout);
#endif /* TEST_PRINT */
	return(ent);

} /* End procedure new_WithData3 */

/*--------------------------------------------------------------------*\
| Procedure     :	BIF_Pixelmap3 *new_pixelmap3(* BIF_Pixelmap3)
|---------------------------------------------------------------------
| Description   :	Allocate a Pixelmap3 entity
|			The pixelmap will be filled be the parser using
|			the bif_pixellist function.
|---------------------------------------------------------------------
| Return        :	The pointer to the new structure ( or NULL on error )
\*--------------------------------------------------------------------*/
BIF_Pixelmap3 *new_pixelmap3( tempEnt )
BIF_Pixelmap3 *tempEnt;

{
	BIF_Pixelmap3 *ent;
	char *ptr;
	int numPixels, oneChannelSize, vfLength;
	int numChannels;

	/*------------------------------------------------------------*\
	|	Allocate the entity and fill it
	\*------------------------------------------------------------*/
	numPixels = tempEnt->numRows * tempEnt->numColumns;
	oneChannelSize = sizeof(unsigned char) * numPixels;

#ifdef TEST_PRINT
fprintf(stderr,"numPixels: %d \n",numPixels);
#endif /* TEST_PRINT */

	if ( wk_info.color_mode == BIF_TRUE_COLOR )
		numChannels = 3;
	else
		numChannels = 1;

	if ( numChannels == 3 )
		/* RGB Components */
		vfLength = 3 * oneChannelSize;
	else
		/* Pseudo color */
		vfLength = 1 * oneChannelSize;

	ent = ALLOCV(BIF_Pixelmap3,vfLength);

	if ( ent != NULL )
	{
		/*----------------------------------------------------*\
		|	Fill entity specific data
		|	Fixed Length Fields
		\*----------------------------------------------------*/
#ifdef SYSV
		memcpy((char *)ent, (char *)tempEnt,
			sizeof(BIF_Pixelmap3));
#else
		bcopy((char *)tempEnt, (char *)ent, sizeof(BIF_Pixelmap3));
#endif
		HEADER(ent,PIXEL_MAP3,do_pixelmap3,NULL);
		ent->numCoords    = 1;

		/*----------------------------------------------------*\
		|	Apportion the memory to the RGB arrays
		\*----------------------------------------------------*/
		ptr = ((char *)ent) + sizeof(BIF_Pixelmap3);

		ent->red = (unsigned char *)ptr;

		if ( numChannels == 3 )
		{
			ptr += oneChannelSize;
			ent->green = (unsigned char *)ptr;
			ptr += oneChannelSize;
			ent->blue  = (unsigned char *)ptr;
		}
		else
		{
			ent->green = (unsigned char *)ptr;
			ent->blue  = (unsigned char *)ptr;
		}
	}

	/*------------------------------------------------------------*\
	|	Return the Entity Address
	\*------------------------------------------------------------*/
	return(ent);

} /* End procedure new_Pixelmap3 */



/*--------------------------------------------------------------------*\
| Procedure     :	BIF_Nubc *new_nubc(int, *Group_description)
|----------------------------------------------------------------------|
| Description   :	Allocate and fill a NON_UNIFORM_BSPLINE_CURVE
|			entity with optional data
|			(if any )
|----------------------------------------------------------------------|
| Return        :	The pointer to the new structure ( or NULL on error )
\*--------------------------------------------------------------------*/
BIF_Nubc *new_nubc(numGroups, groups)
int numGroups;
Group_description *groups;

{
}

/*--------------------------------------------------------------------*\
| Procedure     :	BIF_Nubs *new_nubs(int, *Group_description)
|----------------------------------------------------------------------|
| Description   :	Allocate and fill a NON_UNIFORM_BSPLINE_SURFACE
|			entity with optional data (if any )
|----------------------------------------------------------------------|
| Return        :	The pointer to the new structure ( or NULL on error )
\*--------------------------------------------------------------------*/
BIF_Nubs *new_nubs(numGroups, groups)
int numGroups;
Group_description *groups;

{
}
