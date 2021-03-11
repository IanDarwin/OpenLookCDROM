/* $XConsortium: fillcont.c,v 5.3 94/04/17 20:44:36 rws Exp $ */
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
| Author        :	jmz / SimGraphics Engineering Corportation
|
| File          :	fillcont.c
| Date          :	Thu Feb  8 15:13:51 PST 1990
| Project       :	PLB
| Description	:	Contour buffer -to- array  copy routines
| Status	:	Version 1.0
|
| Revisions     :	
|
|      12/90            MFC Tektronix, Inc.: PEX-SI PEX5R1 Release.
|
\*--------------------------------------------------------------------*/

/*--------------------------------------------------------------------*\
|	Table of Contents
|
|	int fillVecSet(int, Real_int_union *, char *, int, vector **)
|		:	Assign the malloc'd space to the array and
|	int fillIntSet(int, Real_int_union *, char *, int, int **
|		:	Assign the malloc'd space to the array and
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

/*--------------------------------------------------------------------*\
| Procedure     :	int fillVecSet(int, Real_int_union *,
|			char *, int, int)
|---------------------------------------------------------------------
| Description   :	Assign the malloc'd space to the array and
|			fill it.  If the supplied contour list is too
|			short, the final entry is duplicated to the end
|			of the array.
|---------------------------------------------------------------------
| Return        :	The number of bytes used by the filled array.
\*--------------------------------------------------------------------*/
void fillVecSet(numConts, contList, ptr, reqSize, adSize)
int		numConts;
Real_int_union	*contList;
char		*ptr;
int		reqSize;
int		adSize;

{
	int i, j, actSize, contSize;
	Pvec3 *vecs, *dupeMe;

	/* Establish the base of the array */
	vecs = (Pvec3 *)ptr;

	/* Fill It */
	actSize = 0;
	if ( numConts  > 0 )
	{
		/* Copy the data */
		for ( i = 0; i < numConts; i++ )
		{
			/* The contour size list */
			contSize = (contList++)->Int;
			actSize += contSize;

			/* The vector arrays */
			for ( j = 0; j < contSize ; j++ )
			{
				vecs->delta_x = (contList++)->Float;
				vecs->delta_y = (contList++)->Float;
				vecs->delta_z = (contList++)->Float;
				vecs = (Pvec3 *)(((char *)vecs) + adSize);
			}
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

} /* End fillVecSet() */

/*--------------------------------------------------------------------*\
| Procedure     :	int fillIntSet(int, Real_int_union *,
|			char *, int, int, int)
|---------------------------------------------------------------------
| Description   :	Assign the malloc'd space to the array and
|			fill it.  If the supplied contour list is too
|			short, the final entry is duplicated to the end
|			of the array.
|---------------------------------------------------------------------
| Return        :	The number of bytes used by the filled array.
\*--------------------------------------------------------------------*/
int fillIntSet(numConts, contList, ptr, reqSize, adSize, colorInd)
int		numConts;
Real_int_union	*contList;
char		*ptr;
int		reqSize;
int		adSize;
int		colorInd;

{
	int i, j, actSize, contSize;
	int *vals, *dupeMe;

	/* Establish the base of the array */
	vals = (int *)ptr;

	/* Fill It */
	actSize = 0;
	if ( numConts  > 0 )
	{
	    /* Copy the data */
	    for ( i = 0 ; i < numConts ; i++ )
	    {	
		/* The contour size list */
		contSize = (contList++)->Int;
		actSize += contSize;

#ifdef EXTERNALNOTE
		/* index is incremented by one so that color '0' does
		   not step on the default BG color. */
#endif
		for ( j = 0; j < contSize ; j++ )
		{
		    *vals = (contList++)->Int + colorInd;
		    vals = (int *)(((char *)vals) + adSize);
		}
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
	/* To keep possible reference from bombing    */
		*vals = 0;
	}

} /* End fillIntSet() */

