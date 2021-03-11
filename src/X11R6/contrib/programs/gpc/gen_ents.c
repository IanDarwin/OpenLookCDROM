/* $XConsortium: gen_ents.c,v 5.4 94/04/17 20:44:36 rws Exp $ */
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
| File		:	gen_ents.c
| Date		:	Sun Jun 25 17:02:09 PDT 1989
| Project       :	PLB
| Description	:	Routines that take GEN_* primitive entities
|			and generate BIF entities as if the generated
|			data had originated from the parser.
| Status        :	Version 1.0
|
| Revisions     :	
|
|      12/90            MFC Tektronix, Inc.: PEX-SI PEX5R1 Release.
|
\*--------------------------------------------------------------------*/
#include <stdio.h>
#include <math.h>
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
#include "bifparse.h"
#include "bifmacro.h"


/*--------------------------------------------------------------------*\
| Procedure	:	int genSphere3Mesh(BIF_Sphere3 *);
|---------------------------------------------------------------------
| Description	:	The generated sphere3 is stored as a quadmesh.
|			(For the reference port no "non-EXACT"
|			method is supported)
|			genSphere3Mesh generates points to the buffers 
|			(using bif_triplet) just like the parser would.
|			bif_quadmesh3(BIF_P_END) stores/executes the
|			generated data as appropriate.
|---------------------------------------------------------------------
| Return	:	Error Code (Not Implemented)
\*--------------------------------------------------------------------*/
int genSphere3Mesh(ent)
BIF_Sphere3 *ent;

{
	int i, numCoords, numFacets;
	vector3 *coords, *normals, *facet_normals;

	/*------------------------------------------------------------*\
	|	Generate the data
	|
	|	The zero meridianal cut is repeated as the first and the
	|	last row in the quadmesh.
	\*------------------------------------------------------------*/
	numCoords = (ent->numLat + 2) * (ent->numLong + 1);
	numFacets = (ent->numLat + 1) * ent->numLong;
	coords  = (vector3 *)malloc(sizeof(vector3)*numCoords);
	normals = (vector3 *)malloc(sizeof(vector3)*numCoords);
	facet_normals = (vector3 *)malloc(sizeof(vector3)*numFacets);

	if ( (coords == NULL) || (normals == NULL) ||
		(facet_normals == NULL) )
		ENT_ERROR(NULL);

	calcSphere(ent->numLong,ent->numLat+2,
		ent->radius,ent->center,ent->scale,
		coords,normals,facet_normals);

	/*------------------------------------------------------------*\
	|	Send the quadmesh ( as if you were the parser )
	\*------------------------------------------------------------*/
	/* WORKING: Facets @ the pole "wink" in bad colors for flat */
	/* WORKING: This could be a number of things... from pphd3 to */
	/* WORKING: fxqmd3 */
	/* The bug is in the vertex / facets themselves. If you disable */
	/* all the normals, the hole still appears */

	bif_quad_mesh3(BIF_P_BEGIN);
	bif_quadmeshorder(	(BIF_INT)ent->numLong+1,
				(BIF_INT)ent->numLat+2);

	/* Pass the Vertex coordinates */
	bif_group(VERTEX_COORDINATES);
	for ( i = 0 ; i < numCoords ; i++)
		bif_triplet(	(BIF_REAL)coords[i][0],
				(BIF_REAL)coords[i][1],
				(BIF_REAL)coords[i][2]);
	bif_group(BIF_END_OF_GROUP);

	/* Pass the Vertex normals */
	bif_group(VERTEX_NORMALS);
	for ( i = 0 ; i < numCoords ; i++)
		bif_triplet(	(BIF_REAL)normals[i][0],
				(BIF_REAL)normals[i][1],
				(BIF_REAL)normals[i][2]);
	bif_group(BIF_END_OF_GROUP);

	/* Pass the Facet normals */
	bif_group(FACET_NORMALS);
	for ( i = 0 ; i < numFacets ; i++)
		bif_triplet(	(BIF_REAL)facet_normals[i][0],
				(BIF_REAL)facet_normals[i][1],
				(BIF_REAL)facet_normals[i][2]);
	bif_group(BIF_END_OF_GROUP);
	/* End the quadmesh */
	bif_quad_mesh3(BIF_P_END);

	/*------------------------------------------------------------*\
	|	Free the malloc'd memory
	\*------------------------------------------------------------*/
	free((char *)coords);
	free((char *)normals);
	free((char *)facet_normals);

} /* End genSphere3Mesh() */

/*--------------------------------------------------------------------*\
| Procedure	:	int calcSphere(int, int, float, float, vector3,
|					vector3)
|---------------------------------------------------------------------
| Description	:	Calculate the vertices, normals and facets
|			vertex normals for a longitudinal/latitudinal
|			tesselation of a sphere.
|
|			The points are stored as a series of
|			longitudinal cuts from north to south, east to
|			west.  numLat is the number of points along  a
|			longitudinal cut.
|
|			The zero meridianal cut is repeated at the end
|			to allow closure (of a quadmesh).
|
|---------------------------------------------------------------------
| Return	:	Error Code (not implemented)
\*--------------------------------------------------------------------*/
/* Inputs */
int calcSphere(numLong, numLat, radius, center, scale,
		coords, normals, f_norms)
int numLong, numLat; 		/* The number of long. and lat. cuts */
float radius;			/* Overall Radius of the sphere */
vector3 center;			/* The center of the calc'd sphere */
vector3 scale;			/* The scale factors applied to the
					overall radius */

/* Outputs */
vector3 *coords, *normals, *f_norms;

{
	int longi, lat;
	double along, dlong, alat, dlat;
	double alongh, alath ;
	double sinlong, coslong;
	double sinlongh, coslongh;
	double sinlat , coslat ;
	double sinlath, coslath;
	double radx, rady, radz;
	double nscax, nscay, nscaz;
	static double two_pi = 6.2831853071796;

	/* Get the steps */
	dlong  = two_pi / (double)numLong;
	dlat   = two_pi / (double)(numLat-1) / (double)2.;

	/* Create the scaled radii */
	radx = (double)radius * (double)scale[0];
	rady = (double)radius * (double)scale[1];
	radz = (double)radius * (double)scale[2];

	/* Create the normal scale factors */
	nscax = (double)scale[1] * (double)scale[2];
	nscay = (double)scale[0] * (double)scale[2];
	nscaz = (double)scale[0] * (double)scale[1];

	/* The Longitudinal Cuts */
	for ( longi = 0 ; longi <= numLong ; longi++ )
	{
		/* Longitudinal Quantities */
		along   = (double)longi * dlong;
		sinlong = (double)sin(along);
		coslong = (double)cos(along);


		/* At the next half station */
		alongh   = ((double)longi + 0.5) * dlong;
		sinlongh = sin(alongh);
		coslongh = cos(alongh);
#ifdef TEST_PRINT 
printf("sinlongh %6.3f coslongh %6.3f \n ", sinlongh, coslongh);
#endif

		/* The Latitudinal Cuts */
		for ( lat  = 0 ; lat  < numLat ; lat ++ )
		{
			/* Latitudinal Quantities */
			alat    = (double)lat  * dlat ;
			sinlat  = sin(alat );
			coslat  = cos(alat );

			/* Compute the vertex */
			coords[0][0] = radx * coslong * sinlat +
					(double)center[0];
			coords[0][1] = rady * coslat +
					(double)center[1];
			coords[0][2] = radz * sinlong * sinlat +
					(double)center[2];
#ifdef TEST_PRINT
printf("%6.3f %6.3f %6.3f ", coords[0][0], coords[0][1], coords[0][2]);
#endif
			coords++;

			/* Compute the vertex normal */
			normals[0][0] = nscax * coslong * sinlat;
			normals[0][1] = nscay * coslat;
			normals[0][2] = nscaz * sinlong * sinlat;
			unitvec3f(normals[0]);
#ifdef TEST_PRINT
printf("%6.3f %6.3f %6.3f ", normals[0][0], normals[0][1], normals[0][2]);
#endif
			normals++;

			if ( (longi < numLong)  && (lat < (numLat - 1)) ) 
			{
			/* Compute the facet normal */
				alath   = ((double)lat + 0.5) * dlat;
				sinlath = sin(alath);
				coslath = cos(alath);

				f_norms[0][0] = nscax * coslongh * sinlath;
				f_norms[0][1] = nscay * coslath;
				f_norms[0][2] = nscaz * sinlongh * sinlath;
				unitvec3f(f_norms[0]);

#ifdef TEST_PRINT
printf(" %6.3f %6.3f %6.3f", f_norms[0][0], f_norms[0][1], f_norms[0][2]);
#endif

				f_norms++;
			}
#ifdef TEST_PRINT
printf("\n");
#endif

		} /* End for lat */
	} /* End for longi */
}

/*--------------------------------------------------------------------*\
| Procedure	:	int genCirclePoly(BIF_Circle *);
|---------------------------------------------------------------------
| Description	:	The generated circle is stored as a polygon.
|			(For the reference port no "non-EXACT"
|			method is supported)
|			genCirclePoly generates points to the buffers 
|			(using bif_pair) just like the parser would.
|			bif_polygon(BIF_P_END) stores/executes the
|			generated data as appropriate.
|---------------------------------------------------------------------
| Return	:	Error Code (Not Implemented)
\*--------------------------------------------------------------------*/
int genCirclePoly(ent)
BIF_Circle *ent;

{
	int i;
	double x, y;
	double ang, delang;
	static double two_pi = 6.2831853071796;

	/* Compute the arc size */
	delang = two_pi / (double)ent->numLong;

	/* Start the polygon */
	bif_polygon(BIF_P_BEGIN);

	/* Pass the points */
	for ( i=0 ; i < ent->numLong ; i++)
	{
		/* Compute the values */
		ang = (double)i * delang;
		x = (double)ent->radius * cos(ang) +
			(double)ent->center[0];
		y = (double)ent->radius * sin(ang) +
			(double)ent->center[1];

		/* Pass the values */
		bif_pair((BIF_REAL)x, (BIF_REAL)y);
	}
	/* End the polygon */
	bif_polygon(BIF_P_END);

	
} /* End genCirclePoly() */

/*--------------------------------------------------------------------*\
| Procedure	:	int genCircle3Poly(BIF_Circle3 *);
|---------------------------------------------------------------------
| Description	:	The generated circle3 is stored as a polygon3.
|			(For the reference port no "non-EXACT"
|			method is supported)
|			genCircle3Poly generates points to the buffers 
|			(using bif_triplet) just like the parser would.
|			bif_polygon3(BIF_P_END) stores/executes the
|			generated data as appropriate.
|---------------------------------------------------------------------
| Return	:	Error Code (Not Implemented)
\*--------------------------------------------------------------------*/
int genCircle3Poly(ent)
BIF_Circle3 *ent;

{
	int i;
	double ang, delang;
	static double two_pi = 6.2831853071796;
	matrix4 genMat;
	vector4 genPoint, xfPoint;

	/* Make the generation space coordinate system */
	makeCircle3Mat(ent->norm,ent->center,genMat);

	/* Compute the arc size */
	delang = two_pi / (double)ent->numLong;

	/* Start the polygon3 */
	bif_polygon3(BIF_P_BEGIN);

	/* Pass the coordinates */
	bif_group(VERTEX_COORDINATES);
	genPoint[2] = 0;
	genPoint[3] = 1;
	for ( i=0 ; i < ent->numLong ; i++)
	{
		/* Compute the values */
		ang = (double)i * delang;
		genPoint[0] = (double)ent->radius * cos(ang) ;
		genPoint[1] = (double)ent->radius * sin(ang) ;

		/* Transform the point */
		mx_44mult(genMat,genPoint,xfPoint);

		/* Pass the values */
		bif_triplet((BIF_REAL)xfPoint[0], 
			(BIF_REAL)xfPoint[1],
			(BIF_REAL)xfPoint[2]);
	}
	bif_group(BIF_END_OF_GROUP);

	/* Pass the Vertex Normals */
	bif_group(VERTEX_NORMALS);

	genPoint[0] = 0;
	genPoint[1] = 0;
	genPoint[2] = 1;
	genPoint[4] = 1;
	mx_44mult(genMat,genPoint,xfPoint);
	for ( i=0 ; i < ent->numLong ; i++)
		bif_triplet((BIF_REAL)xfPoint[0], 
			(BIF_REAL)xfPoint[1],
			(BIF_REAL)xfPoint[2]);

	bif_group(BIF_END_OF_GROUP);

	/* Pass the Facet Normal */
	bif_group(FACET_NORMALS);
	bif_triplet((BIF_REAL)xfPoint[0], (BIF_REAL)xfPoint[1],
			(BIF_REAL)xfPoint[2]);
	bif_group(BIF_END_OF_GROUP);

	/* End the polygon3 */
	bif_polygon3(BIF_P_END);

} /* End genCircle3Poly() */

/*--------------------------------------------------------------------*\
| Procedure	:	int makeCircle3Mat(vector3,vector3,matrix4)
|---------------------------------------------------------------------
| Description	:	Form the transformation needed for generating
|			the CIRCLE3 primitive as spec'd.
|
|			normal maps to +z in the generation coord.
|			system.  center maps to the orientation.  The
|			directions of the inplane vectors are arbitrary.
|---------------------------------------------------------------------
| Return	:	Error Code (not Implemented)
\*--------------------------------------------------------------------*/
int makeCircle3Mat(normal,center,mat)
vector3 normal, center;
matrix4 mat;

{
	static vector3 x_axe = { 1., 0., 0.};
	static vector3 y_axe = { 0., 1., 0.};
	static vector3 z_axe = { 0., 0., 1.};
	double dotvec3f();

	vector3 x_dir, y_dir, z_dir, other;
	float xDot, yDot, zDot;

	/*------------------------------------------------------------*\
	|	find the basis vectors for the generation space
	\*------------------------------------------------------------*/
	/* Find the most "normal" axis */
	xDot = (float)fabs(dotvec3f(normal,x_axe));
	yDot = (float)fabs(dotvec3f(normal,y_axe));
	zDot = (float)fabs(dotvec3f(normal,z_axe));
	if 	(( xDot > yDot ) || ( xDot > zDot ))
		copyvec3f(x_axe,other);
	else if	(( yDot > xDot ) || ( yDot > zDot ))
		copyvec3f(y_axe,other);
	else
		copyvec3f(z_axe,other);

        /* Cover the cases where the normal runs on an axis -- new */
        if (normal[0] == 0 && normal[1] == 0) copyvec3f(x_axe,other); /*new*/
        if (normal[0] == 0 && normal[2] == 0) copyvec3f(x_axe,other); /*new*/
        if (normal[1] == 0 && normal[2] == 0) copyvec3f(y_axe,other); /*new*/

	/* Generate the basis vectors */
	copyvec3f(normal,z_dir);
	crossvec3f(other,normal,x_dir);
	crossvec3f(z_dir,x_dir,y_dir);

	/* Normalize them suckers.... */
	unitvec3f(x_dir);
	unitvec3f(y_dir);
	unitvec3f(z_dir);

	/*------------------------------------------------------------*\
	|	Load the rows of the matrix with the basis vectors
	\*------------------------------------------------------------*/
	mat[0][0] = x_dir[0];
	mat[0][1] = x_dir[1];
	mat[0][2] = x_dir[2];
	mat[0][3] = 0.0;

	mat[1][0] = y_dir[0];
	mat[1][1] = y_dir[1];
	mat[1][2] = y_dir[2];
	mat[1][3] = 0.0;

#ifdef EXTERNALNOTES
	The following matrices were originally coded as ...

	mat[1][0] = z_dir[0];
	mat[1][1] = z_dir[1];
	mat[1][2] = z_dir[2];
	mat[1][3] = 0.0;

#endif

	mat[2][0] = z_dir[0];
	mat[2][1] = z_dir[1];
	mat[2][2] = z_dir[2];
	mat[2][3] = 0.0;

	/* The centering offset */
	mat[3][0] = center[0];
	mat[3][1] = center[1];
	mat[3][2] = center[2];
	mat[3][3] = 1.0;

} /* End makeCircle3Mat */
