/* $XConsortium: maptools.c,v 5.3 94/04/17 20:44:39 hersh Exp $ */
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
| File          :	maptools.c
| Date          :	Fri Feb  9 10:46:55 PST 1990
| Project       :	PLB
| Description	:	Utilities for view mapping and
|			view specification entities 
| Status        :	Version 1.0
|
| Revisions     :	
|	6/28/89		ISO PHIGS View Calls		JMZ
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
|	void adjust_window(*float, int, float )
|		:	Adjust the specified window to for the physical
|	void compute_default_view( *BIF_Defaultviewspecification, *float)
|		:	Compute and complete a default view specification
|	int compute_vieworient_matrix(*float, *float, *float, *float)
|		:	Compute a view orientation matrix based on the 
|
\*--------------------------------------------------------------------*/

/*--------------------------------------------------------------------*\
|	Include files
\*--------------------------------------------------------------------*/

#include "biftypes.h"
#include "bifparse.h"
#include "bifbuild.h"
#include "bifmacro.h"
#include "ph_map.h"


/* ---------------------------------------------------------------------*\
| Local global variables                                                   |
\*--------------------------------------------------------------------- */

static float ident_matrix[4][4] =
{
	1., 0., 0., 0.,
	0., 1., 0., 0.,
	0., 0., 1., 0.,
	0., 0., 0., 1.,
};

/*--------------------------------------------------------------------*\
|	BEGIN PROCEDURE CODE
\*--------------------------------------------------------------------*/

/*--------------------------------------------------------------------*\
| Procedure	:	void adjust_window(*float, int, float )
|-----------------------------------------------------------------------
| Description	:	Adjust the specified window to for the physical
|			size of the display.  Eliminate the Stretch /
|			squash effect. Match_type defines the type
|			of adjustment to perform.
|-----------------------------------------------------------------------
| Return	:	None.
\*--------------------------------------------------------------------*/
void adjust_window(uvWindow, match_type, aspect_ratio )
float uvWindow[];
int match_type;
float aspect_ratio;

{
	float aspect, u_size, u_avg, v_size, v_avg;

	if ( match_type != BIF_NO_AREA_MATCH )
	{
		/*----------------------------------------------------*\
		|	Basic Size Information
		\*----------------------------------------------------*/
		u_avg  = (uvWindow[1] + uvWindow[0]) / 2.;
		u_size =  uvWindow[1] - uvWindow[0];
		v_avg  = (uvWindow[3] + uvWindow[2]) / 2.;
		v_size =  uvWindow[3] - uvWindow[2];
		/* Aspect Ratio */
		if ( v_size != 0. )
			aspect = u_size / v_size;
		else
			aspect = u_size;

		/*----------------------------------------------------*\
		|	Do the size correction
		\*----------------------------------------------------*/
		switch ( match_type )
		{
		case ADJUST_X:
			/* Set the x size of the window for a match */
			u_size =  v_size * aspect_ratio;
			break;

		case ADJUST_Y:
			/* Set the y size of the window for a match */
			v_size =  u_size / aspect_ratio;
			break;

		case GROW:
			/* Find which one to GROW */
			if ( aspect < aspect_ratio )
				/*------------------------------------*\
				|	Grow X to match
				\*------------------------------------*/
				u_size =  v_size * aspect_ratio;
			else
				/*------------------------------------*\
				|	Grow Y to match
				\*------------------------------------*/
				v_size =  u_size / aspect_ratio;

			break;

	
		case SHRINK:
			/* Find which one to SHRINK */
			if ( aspect > aspect_ratio )
				/*------------------------------------*\
				|	Shrink X size for a match
				\*------------------------------------*/
				u_size =  v_size * aspect_ratio;
			else
				/*------------------------------------*\
				|	Shrink Y size for a match
				\*------------------------------------*/
				v_size =  u_size / aspect_ratio;

			break;

		}

		/*----------------------------------------------------*\
		|	Generate a new window based on the new sizes
		\*----------------------------------------------------*/
		uvWindow[0] = u_avg - u_size/2.;
		uvWindow[1] = u_avg + u_size/2.;
		uvWindow[2] = v_avg - v_size/2.;
		uvWindow[3] = v_avg + v_size/2.;
	}

} /* End procedure adjust_window */

/*--------------------------------------------------------------------*\
| Procedure	:	void compute_default_view(
|				*BIF_Defaultviewspecification, *float)
|-----------------------------------------------------------------------
| Description	:	Compute and complete a default view specification
| 			Adjust the specified window to for the physical
|			size of the display.  Eliminate the Stretch /
|			squash effect. 
|			View looks at objects of a given radius on the
|			origin.
|
|-----------------------------------------------------------------------
| Return	:	None.
\*--------------------------------------------------------------------*/
void compute_default_view(ent, aspect_ratio )
BIF_Defaultviewspecification *ent;
float aspect_ratio;

{
	static Ppoint3 d_vrp = { 0., 0., 0. };  /* View the origin */
	static Pvec3 d_vpn = { 0., 0., 1. };  /* Look along -z   */
	static Pvec3 d_vup = { 0., 1., 0. };  /* y is vertical   */
	static float npcMinMax[6] = 
	{
		0., 1.,
		0., 1.,
		0., 1.
	};

	float front_plane, back_plane;
	float uvMinMax[4];
	float proj_reference[3];
	int ierr;
	Pview_map3 mapping;

	/*------------------------------------------------------------*\
	|	Compute the View orientation matrix
	|	store it in the defaultviewspec entity
	\*------------------------------------------------------------*/
#ifdef USING_PHIGS
	/* Using PHIGS Utilities */
	peval_view_ori_matrix3(&d_vrp,&d_vpn,&d_vup,&ierr,
				    ent->rep.ori_matrix);

#else /* USING_PHIGS */
	/* Using Native Utilities or application code */
	/* set to identity */
	mx_identity(ent->rep.ori_matrix);

#endif /* USING_PHIGS */

	/*------------------------------------------------------------*\
	|	Compute the View mapping parameters
	|	First the window in the view space based on radius
	\*------------------------------------------------------------*/
	/* The view window type */
	uvMinMax[0] = -ent->radius_of_view;
	uvMinMax[1] =  ent->radius_of_view;
	uvMinMax[2] = -ent->radius_of_view;
	uvMinMax[3] =  ent->radius_of_view;
	adjust_window(uvMinMax, GROW, aspect_ratio );


	/* The projection reference point */
	proj_reference[0] = 0.;
	proj_reference[1] = 0.;
	/* Check BIF_PARALLEL againts GX4000 ENUMs */
	if ( ent->proj_type == BIF_PARALLEL )
		/* Viewer offset for orthographic projections */
		proj_reference[2] = 3.0 * ent->radius_of_view;
	else
		/* Viewer offset for perspective projections */
		proj_reference[2] = 2.13 * ent->radius_of_view;

	/* The clipping planes */
	front_plane     =  2.0 * ent->radius_of_view;
	back_plane      = -2.0 * ent->radius_of_view;

	/*------------------------------------------------------------*\
	|	Compute the View Mapping Matrix
	\*------------------------------------------------------------*/
#ifdef USING_PHIGS
	/* Using PHIGS Utilities */
	mapping.win.x_min = uvMinMax[0];
	mapping.win.x_max = uvMinMax[1];
	mapping.win.y_min = uvMinMax[2];
	mapping.win.y_max = uvMinMax[3];
	mapping.proj_vp.x_min = npcMinMax[0];
	mapping.proj_vp.y_min = npcMinMax[2];
	if (aspect_ratio != 1.0) {
	    if (aspect_ratio < 1.0) { /* Shrink X */
		mapping.proj_vp.x_max = npcMinMax[1] * aspect_ratio;
		mapping.proj_vp.y_max = npcMinMax[3];
	    }
	    else { /* Shrink Y */
		mapping.proj_vp.x_max = npcMinMax[1];
		mapping.proj_vp.y_max = npcMinMax[3] / aspect_ratio;
	    }
	}
	else {
	    mapping.proj_vp.x_max = npcMinMax[1];
	    mapping.proj_vp.y_max = npcMinMax[3];
	}
	mapping.proj_vp.z_min = npcMinMax[4];
	mapping.proj_vp.z_max = npcMinMax[5];
	mapping.proj_type = (Pproj_type)ent->proj_type;
	mapping.proj_ref_point.x = proj_reference[0];
	mapping.proj_ref_point.y = proj_reference[1];
	mapping.proj_ref_point.z = proj_reference[2];
	mapping.view_plane = 0.0;
	mapping.back_plane = back_plane;
	mapping.front_plane = front_plane;
	peval_view_map_matrix3(&mapping,&ierr,
				ent->rep.map_matrix);
#else /* USING_PHIGS */
	/* Using Native Utilities or application code */
	/* set to identity (to make sure it HAS a value */
	mx_identity(ent->rep.map_matrix);

#endif /* USING_PHIGS */

	/*------------------------------------------------------------*\
	|	Fill the View Specification Parameters
	\*------------------------------------------------------------*/
	/* Clipping Flags */
	ent->rep.xy_clip    = (Pclip_ind)BIF_CLIP;
	ent->rep.front_clip = (Pclip_ind)BIF_CLIP;
	ent->rep.back_clip  = (Pclip_ind)BIF_CLIP;

	/* View area : Normalized BIF Display Coord. */
	ent->rep.clip_limit.x_min = 0.;
	ent->rep.clip_limit.y_min = 0.;
	if (aspect_ratio != 1.0) {
	    if (aspect_ratio < 1.0) { /* Shrink X */
		ent->rep.clip_limit.x_max = 1. * aspect_ratio;
		ent->rep.clip_limit.y_max = 1.;
	    }
	    else { /* Shrink Y */
		ent->rep.clip_limit.x_max = 1.;
		ent->rep.clip_limit.y_max = 1. / aspect_ratio;
	    }
	}
	else {
	    ent->rep.clip_limit.x_max = 1.;
	    ent->rep.clip_limit.y_max = 1.;
	}
	ent->rep.clip_limit.z_min = 0.;
	ent->rep.clip_limit.z_max = 1.;

} /* End procedure compute_default_view */

/*--------------------------------------------------------------------*\
| Procedure	:	int compute_vieworient_matrix(*float, *float,
|						*float, *float)
|---------------------------------------------------------------------
| Description	:	Compute a view orientation matrix based on the 
|			three vectors given:
|			--------------------------------
|			vrp	View reference point
|			vpn	View plane normal
|			vup	View up vector
|			matrix	matrix to hold result
|---------------------------------------------------------------------
| Return	:	Error Code ( -1 Error, 0 else.)
\*--------------------------------------------------------------------*/
int compute_vieworient_matrix(vrp, vpn, vup, matrix)
float *vrp, *vpn, *vup;
Pmatrix3 matrix;

{
	int ierr;
	/*------------------------------------------------------------*\
	|	Compute the View orientation matrix
	|	store it in the matrix specified
	\*------------------------------------------------------------*/
#ifdef USING_PHIGS
	/* Using PHIGS Utilities */
	peval_view_ori_matrix3((Ppoint3 *)vrp,(Pvec3 *)vpn,
				    (Pvec3 *)vup,&ierr,matrix);

#else /* USING_PHIGS */
	/* Using Native Utilities or application code */
	/* set to identity so it HAS a value */
	mx_identity(matrix);
	ierr = 0;

#endif /* USING_PHIGS */

	return(ierr);

}
