/* $XConsortium: cb_nurb.c,v 5.3 94/04/17 20:40:52 rws Exp $ */

/***********************************************************

Copyright (c) 1989, 1990, 1991  X Consortium

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL THE
X CONSORTIUM BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN
AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

Except as contained in this notice, the name of the X Consortium shall not be
used in advertising or otherwise to promote the sale, use or other dealings
in this Software without prior written authorization from the X Consortium.

Copyright 1989, 1990, 1991 by Sun Microsystems, Inc. 

                        All Rights Reserved

Permission to use, copy, modify, and distribute this software and its 
documentation for any purpose and without fee is hereby granted, 
provided that the above copyright notice appear in all copies and that
both that copyright notice and this permission notice appear in 
supporting documentation, and that the name of Sun Microsystems,
not be used in advertising or publicity pertaining to distribution of 
the software without specific, written prior permission.  

SUN MICROSYSTEMS DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, 
INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT 
SHALL SUN MICROSYSTEMS BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL 
DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,
WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION,
ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS
SOFTWARE.

******************************************************************/

/* NURB functions for the PHIGS C binding */

#include "phg.h"
#include "cp.h"
#include "cb_priv.h"
#include "alloc.h"

#define RANGE_INCONSISTENT( _min, _max, _order, _knots ) \
    ( (_min) >= (_max) \
	|| (_min) < (_knots)->floats[(_order) - 1] \
	|| (_max) > (_knots)->floats[(_knots)->num_floats - (_order)] \
    )

static int
values_are_non_decreasing( n, value )
    int             n;
    Pfloat         *value;
{
    --n;			/* for N knots, N-1 comparisons will do */
    while ( --n >= 0 ) {
	if ( value[1] < value[0] )
	    return 0;
	else
	    value++;
    }
    return 1;
}


/* NON-UNIFORM B-SPLINE CURVE */
void
pnuni_bsp_curv( order, knots, rationality, cpoints, min, max )
    Pint		order;		/* spline order */
    Pfloat_list		*knots;		/* list of knots */
    Prational		rationality;
    Ppoint_list34	*cpoints;	/* list of 3D or 4D  control points */
    Pfloat		min, max;	/* parameter range */
{
    Phg_args		cp_args;
    Phg_el_data		ed;
    Phg_args_add_el	*args = &cp_args.data.add_el;

    register	Phg_nurb_curve_data	*curve;

    if ( CB_ENTRY_CHECK(phg_cur_cph, ERR5, Pfn_nuni_bsp_curv) ) {
	if ( PSL_STRUCT_STATE(phg_cur_cph->psl) != PSTRUCT_ST_STOP ) {
	    ERR_REPORT(phg_cur_cph->erh, ERR5);

	} else if ( order < 1 ) {
	    ERR_REPORT(phg_cur_cph->erh, ERR500);

	} else if ( cpoints->num_points < order ) {
	    ERR_REPORT(phg_cur_cph->erh, ERR501);

	} else if ( knots->num_floats < cpoints->num_points + order ) {
	    ERR_REPORT(phg_cur_cph->erh, ERR502);

	} else if ( !values_are_non_decreasing(knots->num_floats,
		knots->floats) ) {
	    ERR_REPORT(phg_cur_cph->erh, ERR503);

	} else if ( min >= max
	    || min < knots->floats[order - 1]
	    || max > knots->floats[knots->num_floats - order] ) {
	    ERR_REPORT(phg_cur_cph->erh, ERR506);

	} else {
	    /* Set up the fixed-length header fields */
	    args->el_type = PELEM_NUNI_BSP_CURVE;
	    curve = &ed.nurb_curve.data;
	    curve->order = order;
	    curve->rationality = rationality;
	    curve->tstart = min;
	    curve->tend = max;
	    curve->knots = *knots;
	    curve->npts = cpoints->num_points;
	    if ( rationality == PNON_RATIONAL )
		curve->points = (Ppoint4 *)cpoints->points.point3d;
	    else
		curve->points = cpoints->points.point4d;
	    if ( CB_BUILD_OC(args->el_type, &ed, &args->pex_oc) )
		CP_FUNC(phg_cur_cph, CP_FUNC_OP_ADD_EL, &cp_args, NULL);
	}
    }
}


/* SET CURVE APPROXIMATION CRITERIA */
void
pset_curve_approx( type, value )
    Pint            type;
    Pfloat          value;
{
    Phg_args		cp_args;
    Phg_el_data		ed;
    Phg_args_add_el	*args = &cp_args.data.add_el;

    if ( CB_ENTRY_CHECK(phg_cur_cph, ERR5, Pfn_set_curve_approx) ) {
	if ( PSL_STRUCT_STATE(phg_cur_cph->psl) != PSTRUCT_ST_STOP ) {
	    ERR_REPORT(phg_cur_cph->erh, ERR5);

	} else {
	    args->el_type = PELEM_CURVE_APPROX_CRIT;
	    ed.curv_approx.type = type;
	    ed.curv_approx.value = value;
	    if ( CB_BUILD_OC(args->el_type, &ed, &args->pex_oc) )
		CP_FUNC(phg_cur_cph, CP_FUNC_OP_ADD_EL, &cp_args, NULL);
	}
    }
}


static void
count_trim_curve_data( surf, nloops, tloops )
    register	Phg_nurb_surf_data	*surf;
    		Pint			nloops;
    		Ptrimcurve_list		*tloops;/* source */
{
    register	Ptrimcurve_list	*loop;
    register	Ptrimcurve	*tc;
    register	int		j, i;

    /* Count everything. */
    surf->num_tcurves = 0;
    surf->num_tknots = 0;
    surf->num_2D_tpoints = 0;
    surf->num_3D_tpoints = 0;
    for ( i = 0, loop = tloops; i < nloops; i++, loop++ ) {
	surf->num_tcurves += loop->num_curves;
	for ( j = 0, tc = loop->curves; j < loop->num_curves; j++, tc++ ) {
	    surf->num_tknots += tc->knots.num_floats;
	    if ( tc->rationality == PRATIONAL )
		surf->num_3D_tpoints += tc->cpts.num_points;
	    else
		surf->num_2D_tpoints += tc->cpts.num_points;
	}
    }
}

/* NON-UNIFORM B-SPLINE SURFACE */
void
pnuni_bsp_surf( uorder, vorder, uknots, vknots, rationality, grid,
    nloops, tloops )
    Pint		uorder;		/* U spline order */
    Pint		vorder;		/* V spline order */
    Pfloat_list		*uknots;	/* U knots */
    Pfloat_list		*vknots;	/* V knots */
    Prational		rationality;	/* rationality selector */
    Ppoint_grid34	*grid;		/* grid of 3D or 4D  control points */
    Pint		nloops;		/* number of trim curve loops */
    Ptrimcurve_list	*tloops;	/* trim curve loops */
{
    Phg_args		cp_args;
    Phg_el_data		ed;
    Phg_args_add_el	*args = &cp_args.data.add_el;

    register	Phg_nurb_surf_data	*surf = &ed.nurb_surf.data;
    register	Ppcs_dims		*npts = &grid->num_points;

#ifdef DIAGNOSTIC
    print_nurb_surface( uorder, vorder, uknots, vknots, rationality,
	grid, nloops, tloops );
#endif

    if ( CB_ENTRY_CHECK(phg_cur_cph, ERR5, Pfn_nuni_bsp_surf) ) {
	if ( PSL_STRUCT_STATE(phg_cur_cph->psl) != PSTRUCT_ST_STOP ) {
	    ERR_REPORT(phg_cur_cph->erh, ERR5);

	} else if ( uorder < 1 || vorder < 1 ) {
	    ERR_REPORT(phg_cur_cph->erh, ERR500);

	} else if ( npts->u_dim < uorder || npts->v_dim < vorder ) {
	    ERR_REPORT(phg_cur_cph->erh, ERR501);

	} else if ( uknots->num_floats < npts->u_dim + uorder
	    ||  vknots->num_floats < npts->v_dim + vorder ) {
	    ERR_REPORT(phg_cur_cph->erh, ERR502);

	} else if (!values_are_non_decreasing(uknots->num_floats,
		uknots->floats)
	    ||  !values_are_non_decreasing(vknots->num_floats,
		vknots->floats)) {
	    ERR_REPORT(phg_cur_cph->erh, ERR503);

	/* TODO: Check for errors 507 through 512. */
	} else {
	    args->el_type = PELEM_NUNI_BSP_SURF;
	    surf->u_order = uorder;
	    surf->v_order = vorder;
	    surf->rationality = rationality;
	    surf->uknots = *uknots;
	    surf->vknots = *vknots;
	    surf->npts = *npts;
	    surf->nloops = nloops;
	    /* grid array assumed contiguous in memory */
	    if ( rationality == PNON_RATIONAL )
		surf->grid = (Ppoint4 *)grid->points.point3d;
	    else
		surf->grid = grid->points.point4d;
	    if ( surf->nloops > 0 ) {
		surf->trimloops = tloops;
		count_trim_curve_data( surf, nloops, tloops );
	    } else {
		surf->trimloops = (Ptrimcurve_list *)NULL;
		surf->num_tcurves = 0;
		surf->num_tknots = 0;
		surf->num_3D_tpoints = 0;
		surf->num_2D_tpoints = 0;
	    }

	    if ( CB_BUILD_OC(args->el_type, &ed, &args->pex_oc) )
		CP_FUNC(phg_cur_cph, CP_FUNC_OP_ADD_EL, &cp_args, NULL);
	}
    }
}


/* SET SURFACE APPROXIMATION CRITERIA */
void
pset_surf_approx( type, uvalue, vvalue )
    Pint	type;
    Pfloat	uvalue;
    Pfloat	vvalue;
{
    Phg_args		cp_args;
    Phg_el_data		ed;
    Phg_args_add_el	*args = &cp_args.data.add_el;

    if (CB_ENTRY_CHECK(phg_cur_cph, ERR5, Pfn_set_surf_approx)) {
	if (PSL_STRUCT_STATE(phg_cur_cph->psl) != PSTRUCT_ST_STOP) {
	    ERR_REPORT(phg_cur_cph->erh, ERR5);

	} else {
	    args->el_type = PELEM_SURF_APPROX_CRIT;
	    ed.surf_approx.type = type;
	    ed.surf_approx.u_val = uvalue;
	    ed.surf_approx.v_val = vvalue;
	    if ( CB_BUILD_OC(args->el_type, &ed, &args->pex_oc) )
		CP_FUNC(phg_cur_cph, CP_FUNC_OP_ADD_EL, &cp_args, NULL);
	}
    }
}


static int
print_nurb_surface( uorder, vorder, uknots, vknots, rationality, grid,
    nloops, tloops )
    Pint		uorder;		/* U spline order */
    Pint		vorder;		/* V spline order */
    Pfloat_list		*uknots;	/* U knots */
    Pfloat_list		*vknots;	/* V knots */
    Prational		rationality;	/* rationality selector */
    Ppoint_grid34	*grid;		/* grid of 3D or 4D  control points */
    Pint		nloops;		/* number of trim curve loops */
    Ptrimcurve_list	*tloops;	/* trim curve loops */
{
    register int	i, j, k, npts;
    Ptrimcurve		*crv;

    fprintf( stderr, "order = %d %d, rationality = %s\n",
	uorder, vorder,
	rationality == PRATIONAL ? "RATIONAL" : "NON_RATIONAL");
    fprintf( stderr, "%d U knots\n", uknots->num_floats );
    for ( i = 0; i < uknots->num_floats; i++ )
	fprintf( stderr, "\t[%d]  %f\n", i, uknots->floats[i] );
    fprintf( stderr, "%d V knots\n", vknots->num_floats );
    for ( i = 0; i < vknots->num_floats; i++ )
	fprintf( stderr, "\t[%d]  %f\n", i, vknots->floats[i] );

    npts = grid->num_points.u_dim * grid->num_points.v_dim;
    fprintf( stderr, "U dim = %d, V dim = %d\n",
	grid->num_points.u_dim, grid->num_points.v_dim );
    for ( i = 0; i < npts; i++ )
	if ( rationality == PRATIONAL )
	    fprintf( stderr, "\t[%d] %f %f %f %f\n", i,
		grid->points.point4d[i].x,
		grid->points.point4d[i].y,
		grid->points.point4d[i].z,
		grid->points.point4d[i].w );
	else
	    fprintf( stderr, "\t[%d] %f %f %f\n", i,
		grid->points.point3d[i].x,
		grid->points.point3d[i].y,
		grid->points.point3d[i].z );
    
    fprintf( stderr, "\n%d trim loops\n", nloops );
    for ( i = 0; i < nloops; i++ ) {
	fprintf( stderr, "\nloop %d, %d curves\n", i, tloops[i].num_curves );
	for ( j = 0; j < tloops[i].num_curves; j++ ) {
	    crv = &tloops[i].curves[j];
	    fprintf( stderr, "\n\tcurve %d: order %d\n", j, crv->order );
	    fprintf( stderr, "\tvisibility = %s, ",
		crv->visible == PEDGE_ON ? "ON" : "OFF" );
	    fprintf( stderr, "rationality = %s\n",
		crv->rationality == PRATIONAL ? "RATIONAL" : "NON RATIONAL");

	    fprintf( stderr, "\trange = (%f, %f)\n", crv->tmin, crv->tmax );
	    fprintf( stderr, "\t%d knots\n", crv->knots.num_floats );
	    for ( k = 0; k < crv->knots.num_floats; k++ )
		fprintf( stderr, "\t\t%f\n", crv->knots.floats[k] );

	    fprintf( stderr, "\t%d control points\n", crv->cpts.num_points );
	    if ( crv->rationality == PRATIONAL ) {
		for ( k = 0; k < crv->cpts.num_points; k++ )
		    fprintf( stderr, "\t\t[%d] %f %f %f\n", k,
			crv->cpts.points.point3d[k].x,
			crv->cpts.points.point3d[k].y,
			crv->cpts.points.point3d[k].z );
	    } else {
		for ( k = 0; k < crv->cpts.num_points; k++ )
		    fprintf( stderr, "\t\t[%d] %f %f\n", k,
			crv->cpts.points.point2d[k].x,
			crv->cpts.points.point2d[k].y );
	    }
	}
    }
}
