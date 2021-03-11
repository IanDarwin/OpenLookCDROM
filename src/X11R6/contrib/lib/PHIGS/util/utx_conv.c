/* $XConsortium: utx_conv.c,v 5.13 94/04/17 20:42:25 rws Exp $ */

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

/* PEX/PHIGS utilities. */

#include "phg.h"
#include "cp.h"
#include "ws.h"
#include "PEX.h"
#include "PEXmacs.h"
#include "PEXfuncs.h"
#include "PEXproto.h"
#include "PEXtempl.h"
#include "phigspex.h"


int
phg_utx_ptlst4_to_pex( count, pts, buf )
    register	int		count;
    register	Ppoint4		*pts;
    register	pexCoord4D	*buf;
{
    int		size = count * sizeof(pexCoord4D);

    for ( ; count; count--, buf++, pts++ ) {
	PEX_CONV_FROM_Ppoint4( pts, buf )
    }
    return size;
}

int
phg_utx_ptlst4_from_pex( count, buf, pts )
    register	int		count;
    register	pexCoord4D	*buf;
    register	Ppoint4		*pts;
{
    int		size = count * sizeof(Ppoint4);

    for ( ; count; count--, buf++, pts++ ) {
	PEX_CONV_TO_Ppoint4( buf, pts )
    }
    return size;
}


int
phg_utx_ptlst3_to_pex( count, pts, buf )
    register	int		count;
    register	Ppoint3		*pts;
    register	pexCoord3D	*buf;
{
    int		size = count * sizeof(pexCoord3D);

    for ( ; count; count--, buf++, pts++ ) {
	PEX_CONV_FROM_Ppoint3( pts, buf )
    }
    return size;
}

int
phg_utx_ptlst3_from_pex( count, buf, pts )
    register	int		count;
    register	pexCoord3D	*buf;
    register	Ppoint3		*pts;
{
    int		size = count * sizeof(Ppoint3);

    for ( ; count; count--, buf++, pts++ ) {
	PEX_CONV_TO_Ppoint3( buf, pts )
    }
    return size;
}


int
phg_utx_ptlst_to_pex( count, pts, buf )
    register	int		count;
    register	Ppoint		*pts;
    register	pexCoord2D	*buf;
{
    int		size = count * sizeof(pexCoord2D);

    for ( ; count; count--, buf++, pts++ ) {
	PEX_CONV_FROM_Ppoint( pts, (pexCoord2D*)buf )
    }
    return size;
}

int
phg_utx_ptlst_from_pex( count, buf, pts )
    register	int		count;
    register	pexCoord2D	*buf;
    register	Ppoint		*pts;
{
    int		size = count * sizeof(Ppoint);

    for ( ; count; count--, buf++, pts++ ) {
	PEX_CONV_TO_Ppoint( buf, pts )
    }
    return size;
}


int
phg_utx_vdata_size( vtype, count, colour_type )
    Pint	vtype;
    Pint	count;
    Pint	colour_type;
{
    int		size = 0;

    switch ( vtype ) {
	case PVERT_COORD:
	case PVERT_COORD_COLOUR:
	    size = count * sizeof(pexCoord3D);
	    break;
	case PVERT_COORD_NORMAL:
	case PVERT_COORD_COLOUR_NORMAL:
	    size = count * (sizeof(pexCoord3D) + sizeof(pexVector3D));
	    break;
    }

    if ( vtype == PVERT_COORD_COLOUR || vtype == PVERT_COORD_COLOUR_NORMAL) {
	switch ( colour_type ) {
	    case PINDIRECT: size += count * sizeof(pexIndexedColour); break;
	    case PMODEL_RGB: size += count * sizeof(pexRgbFloatColour); break;
	    case PMODEL_CIELUV: size += count * sizeof(pexCieColour); break;
	    case PMODEL_HSV: size += count * sizeof(pexHsvColour); break;
	    case PMODEL_HLS: size += count * sizeof(pexHlsColour); break;
	}
    }
    return size;
}


static int
fdata_size( ftype, count, colour_type )
    Pint	ftype;
    Pint	count;
    Pint	colour_type;
{
    int		size = 0;

    switch ( ftype ) {
	case PFACET_NORMAL:
	case PFACET_COLOUR_NORMAL:
	    size = count * sizeof(pexVector3D);
	    break;
    }

    if ( ftype == PFACET_COLOUR || ftype == PFACET_COLOUR_NORMAL) {
	switch ( colour_type ) {
	    case PINDIRECT: size += count * sizeof(pexIndexedColour); break;
	    case PMODEL_RGB: size += count * sizeof(pexRgbFloatColour); break;
	    case PMODEL_CIELUV: size += count * sizeof(pexCieColour); break;
	    case PMODEL_HSV: size += count * sizeof(pexHsvColour); break;
	    case PMODEL_HLS: size += count * sizeof(pexHlsColour); break;
	}
    }
    return size;
}


/** If you don't want edges to go in, let eflag == 0 **/
int
phg_utx_vdata_to_pex( num_lists, cntflag, vflag, eflag, colour_type, 
	vdata, edata, buf )
    register	int		num_lists;
    		Pint		cntflag; /* store list counts if !0 */
    		Pint		vflag;
		Pint		eflag;
    		Pint		colour_type;
    register	Pfacet_vdata_list3	*vdata;
		Pedge_data_list	*edata;
    register	char		*buf;
{
    int		size = 0, colour_size;
    pexCoord3D	*bufpt;
    pexVector3D	*bufvec;
    CARD32	*buf32;

    register	int		i, j;

    if ( cntflag )
	size = num_lists * sizeof(CARD32);

    switch ( vflag ) {
	case PVERT_COORD:
	    for ( i = 0; i < num_lists; i++ ) {
		if ( cntflag ) {
		    buf32 = (CARD32 *)buf;
		    *buf32 = vdata[i].num_vertices;
		    buf += sizeof(*buf32);
		}
		size += vdata[i].num_vertices * 
		    (sizeof(pexCoord3D) + (eflag ? sizeof(CARD32) : 0));
		for ( j = 0; j < vdata[i].num_vertices; j++ ) {
		    bufpt = (pexCoord3D *)buf; buf += sizeof(*bufpt);
		    PEX_CONV_FROM_Ppoint3( &vdata[i].vertex_data.points[j],
			bufpt )
		    if (eflag) {
			buf32 = (CARD32 *)buf;
			*buf32 =
			    PEX_CONV_FROM_Pedgef(edata[i].edgedata.edges[j]);
			buf += sizeof(*buf32);
		    }
		}
	    }
	    break;
	case PVERT_COORD_NORMAL:
	    for ( i = 0; i < num_lists; i++ ) {
		if ( cntflag ) {
		    buf32 = (CARD32 *)buf;
		    *buf32 = vdata[i].num_vertices;
		    buf += sizeof(*buf32);
		}
		size += vdata[i].num_vertices *
		    (sizeof(pexCoord3D) + sizeof(pexVector3D) 
			+ (eflag ? sizeof(CARD32) : 0));
		for ( j = 0; j < vdata[i].num_vertices; j++ ) {
		    bufpt = (pexCoord3D *)buf; buf += sizeof(*bufpt);
		    PEX_CONV_FROM_Ppoint3(
			&vdata[i].vertex_data.ptnorms[j].point, bufpt )
		    bufvec = (pexVector3D *)buf; buf += sizeof(*bufvec);
		    PEX_CONV_FROM_Pvec3(
			&vdata[i].vertex_data.ptnorms[j].norm, bufvec )
		    if (eflag) {
			buf32 = (CARD32 *)buf;
			*buf32 =
			    PEX_CONV_FROM_Pedgef(edata[i].edgedata.edges[j]);
			buf += sizeof(*buf32);
		    }
		}
	    }
	    break;
	case PVERT_COORD_COLOUR:
	    colour_size = PEX_COLOUR_SIZE(colour_type);
	    for ( i = 0; i < num_lists; i++ ) {
		if ( cntflag ) {
		    buf32 = (CARD32 *)buf;
		    *buf32 = vdata[i].num_vertices;
		    buf += sizeof(*buf32);
		}
		size += vdata[i].num_vertices *
		    (sizeof(pexCoord3D) + colour_size + 
					  (eflag ? sizeof(CARD32) : 0));
		for ( j = 0; j < vdata[i].num_vertices; j++ ) {
		    bufpt = (pexCoord3D *)buf; buf += sizeof(*bufpt);
		    PEX_CONV_FROM_Ppoint3(
			&vdata[i].vertex_data.ptcolrs[j].point, bufpt )
		    PEX_CONV_FROM_Pcoval(colour_type,
			&vdata[i].vertex_data.ptcolrs[j].colr, buf )
		    if (eflag) {
			buf32 = (CARD32 *)buf;
			*buf32 =
			    PEX_CONV_FROM_Pedgef(edata[i].edgedata.edges[j]);
			buf += sizeof(*buf32);
		    }
		}
	    }
	    break;
	case PVERT_COORD_COLOUR_NORMAL:
	    colour_size = PEX_COLOUR_SIZE(colour_type);
	    buf32 = (CARD32 *)buf;
	    for ( i = 0; i < num_lists; i++ ) {
		if ( cntflag ) {
		    buf32 = (CARD32 *)buf;
		    *buf32 = vdata[i].num_vertices;
		    buf += sizeof(*buf32);
		}
		size += vdata[i].num_vertices *
		    (sizeof(pexCoord3D) + colour_size + 
		     sizeof(pexVector3D) + (eflag ? sizeof(CARD32) : 0));
		for ( j = 0; j < vdata[i].num_vertices; j++ ) {
		    bufpt = (pexCoord3D *)buf; buf += sizeof(*bufpt);
		    PEX_CONV_FROM_Ppoint3(
			&vdata[i].vertex_data.ptconorms[j].point, bufpt )
		    PEX_CONV_FROM_Pcoval(colour_type,
			&vdata[i].vertex_data.ptconorms[j].colr, buf )
		    bufvec = (pexVector3D *)buf; buf += sizeof(pexVector3D);
		    PEX_CONV_FROM_Pvec3(
			&vdata[i].vertex_data.ptconorms[j].norm, bufvec )
		    if (eflag) {
			buf32 = (CARD32 *)buf;
			*buf32 =
			    PEX_CONV_FROM_Pedgef(edata[i].edgedata.edges[j]);
			buf += sizeof(*buf32);
		    }
		}
	    }
	    break;
    }
    return size;
}

/* Returns updated pointer after all the stuff has been read. */
char *
phg_utx_vdata_from_pex( num_sets, cntflag, vflag, vdata, eflag, edata, cm, buf )
    Pint		num_sets;	/* IN: */
    int			cntflag;	/* IN: non-zero if counts in buffer */
    Pint		vflag;		/* IN: PHIGS vertex flag */
    Pfacet_vdata_list3	*vdata;		/* IN/OUT: converted vertex data */
    Pint		 eflag;		/* IN: PHIGS edge flag */
    Pedge_data_list	*edata;		/* IN: ignored if eflag == PEDGE_NONE */
    Pint		 cm;		/* IN: PHIGS colour model */
    char		*buf;		/* IN: PEX buffer */
{
    int		i, j;
    Pedge_flag	*edgep;
    char	*voutbuf;
    
    if ( edata )
	edgep = (Pedge_flag *)(edata + num_sets);

    if ( cntflag )
	/* Vertex data gets put after all the vdata structures. */
	voutbuf = (char *)(vdata + num_sets);

    switch (vflag) {
	case PVERT_COORD: {
	    Ppoint3	    *ptPtr;

	    if ( cntflag )
		ptPtr = (Ppoint3 *)voutbuf;

	    for ( i = 0; i < num_sets; i++ ) {
		if ( cntflag ) {
		    vdata[i].num_vertices = (*(CARD32 *)buf);
		    if ( eflag != PEDGE_NONE )
			edata[i].num_edges = vdata[i].num_vertices;
		    buf += sizeof(CARD32);
		    vdata[i].vertex_data.points = ptPtr;
		} else {
		    /* Caller assigned the counts and point arrays. */
		    ptPtr = vdata[i].vertex_data.points;
		}

		if ( eflag != PEDGE_NONE )
		    edata[i].edgedata.edges = edgep;

		for ( j = 0; j < vdata[i].num_vertices; j++, ptPtr++ ) {
		    PEX_CONV_TO_Ppoint3((pexCoord3D *)buf, ptPtr);
		    buf += sizeof(pexCoord3D);
		    if (eflag == PEDGE_VISIBILITY) {
			*edgep++ = PEX_CONV_TO_Pedgef(*(CARD32 *)buf);
			buf += sizeof(CARD32);
		    }
		}
	    }
	  } break;

	case PVERT_COORD_COLOUR: {
	    Pptco3	*ptPtr;
	    int		colour_size = PEX_COLOUR_SIZE(cm);

	    if ( cntflag )
		ptPtr = (Pptco3 *)voutbuf;

	    for ( i = 0; i < num_sets; i++ ) {
		if ( cntflag ) {
		    vdata[i].num_vertices = (*(CARD32 *)buf);
		    if ( eflag != PEDGE_NONE )
			edata[i].num_edges = vdata[i].num_vertices;
		    buf += sizeof(CARD32);
		    vdata[i].vertex_data.ptcolrs = ptPtr;
		} else {
		    /* Caller assigned the counts and array pointers. */
		    ptPtr = vdata[i].vertex_data.ptcolrs;
		}

		if ( eflag != PEDGE_NONE )
		    edata[i].edgedata.edges = edgep;

		for ( j = 0; j < vdata[i].num_vertices; j++, ptPtr++ ) {
		    PEX_CONV_TO_Ppoint3((pexCoord3D *)buf, &ptPtr->point);
		    buf += sizeof(pexCoord3D);
		    PEX_CONV_TO_Pcoval(cm, buf, &ptPtr->colr);
		    buf += colour_size;
		    if (eflag == PEDGE_VISIBILITY) {
			*edgep++ = PEX_CONV_TO_Pedgef(*(CARD32 *)buf);
			buf += sizeof(CARD32);
		    }
		}
	    }
	  } break;

	case PVERT_COORD_NORMAL: {
	    Pptnorm3	    *ptPtr;

	    if ( cntflag )
		ptPtr = (Pptnorm3 *)voutbuf;

	    for ( i = 0; i < num_sets; i++ ) {
		if ( cntflag ) {
		    vdata[i].num_vertices = (*(CARD32 *)buf);
		    if ( eflag != PEDGE_NONE )
			edata[i].num_edges = vdata[i].num_vertices;
		    buf += sizeof(CARD32);
		    vdata[i].vertex_data.ptnorms = ptPtr;
		} else {
		    /* Caller assigned the counts and array pointers. */
		    ptPtr = vdata[i].vertex_data.ptnorms;
		}

		if ( eflag != PEDGE_NONE )
		    edata[i].edgedata.edges = edgep;

		for ( j = 0; j < vdata[i].num_vertices; j++, ptPtr++ ) {
		    PEX_CONV_TO_Ppoint3((pexCoord3D *)buf, &ptPtr->point);
		    buf += sizeof(pexCoord3D);
		    PEX_CONV_TO_Pvec3((pexVector3D *)buf, &ptPtr->norm);
		    buf += sizeof(pexVector3D);
		    if (eflag == PEDGE_VISIBILITY) {
			*edgep++ = PEX_CONV_TO_Pedgef(*(CARD32 *)buf);
			buf += sizeof(CARD32);
		    }
		}
	    }
	  } break;

	case PVERT_COORD_COLOUR_NORMAL: {
	    Pptconorm3	*ptPtr;
	    int		colour_size = PEX_COLOUR_SIZE(cm);

	    if ( cntflag )
		ptPtr = (Pptconorm3 *)voutbuf;

	    for ( i = 0; i < num_sets; i++ ) {
		if ( cntflag ) {
		    vdata[i].num_vertices = (*(CARD32 *)buf);
		    if ( eflag != PEDGE_NONE )
			edata[i].num_edges = vdata[i].num_vertices;
		    buf += sizeof(CARD32);
		    vdata[i].vertex_data.ptconorms = ptPtr;
		} else {
		    /* Caller assigned the counts and array pointers. */
		    ptPtr = vdata[i].vertex_data.ptconorms;
		}

		if ( eflag != PEDGE_NONE )
		    edata[i].edgedata.edges = edgep;

		for ( j = 0; j < vdata[i].num_vertices; j++, ptPtr++ ) {
		    PEX_CONV_TO_Ppoint3((pexCoord3D *)buf, &ptPtr->point);
		    buf += sizeof(pexCoord3D);
		    PEX_CONV_TO_Pcoval(cm, buf, &ptPtr->colr);
		    buf += colour_size;
		    PEX_CONV_TO_Pvec3((pexVector3D *)buf, &ptPtr->norm);
		    buf += sizeof(pexVector3D);
		    if (eflag == PEDGE_VISIBILITY) {
			*edgep++ = PEX_CONV_TO_Pedgef(*(CARD32 *)buf);
			buf += sizeof(CARD32);
		    }
		}
	    }
	  } break;
    }
    
    return(buf);
}


static int
fdata_to_pex( fflag, colour_type, fdata, buf )
    		Pint			fflag;
    		Pint			colour_type;
    register	Phg_facet_data_list3	*fdata;
    register	char			*buf;
{
    int		size = 0;
    pexVector3D	*bufvec;

    register	int	i;

    switch ( fflag ) {
	case PFACET_NORMAL:
	    size = fdata->num_facets * sizeof(pexVector3D);
	    for ( i = 0; i < fdata->num_facets; i++ ) {
		bufvec = (pexVector3D *)buf;
		PEX_CONV_FROM_Pvec3(&fdata->facetdata.norms[i],bufvec)
		buf += sizeof(*bufvec);
	    }
	    break;
	case PFACET_COLOUR:
	    size = fdata->num_facets * PEX_COLOUR_SIZE(colour_type);
	    for ( i = 0; i < fdata->num_facets; i++ ) {
		PEX_CONV_FROM_Pcoval(colour_type,
		    &fdata->facetdata.colrs[i], buf )
	    }
	    break;
	case PFACET_COLOUR_NORMAL:
	    size = fdata->num_facets
		* (sizeof(pexVector3D) + PEX_COLOUR_SIZE(colour_type));
	    for ( i = 0; i < fdata->num_facets; i++ ) {
		PEX_CONV_FROM_Pcoval(colour_type,
		    &fdata->facetdata.conorms[i].colr, buf )
		bufvec = (pexVector3D *)buf;
		PEX_CONV_FROM_Pvec3(
		    &fdata->facetdata.conorms[i].norm, bufvec )
		buf += sizeof(*bufvec);
	    }
	    break;
    }
    return size;
}


/* returns the pex buffer pointer after all this stuff has been read from it */
static char *
fdata_from_pex(fflag, num_facets, cm, buf, fdata )
    Pint		fflag;		/* IN: PHIGS facet flag */
    int			num_facets;	/* IN: number of facets */
    Pint		cm;		/* IN: PHIGS colour model */
    char		*buf;		/* IN: PEX buffer to read from */
    Pfacet_data_arr3	*fdata;		/* IN/OUT: PHIGS structure to fill up */
{
    int	    i;

    for ( i = 0; i < num_facets; i++ ) {
	switch (fflag) {
	    case PFACET_COLOUR :
		PEX_CONV_TO_Pcoval(cm, buf, &fdata->colrs[i]);
		buf += PEX_COLOUR_SIZE(cm);
		break;
	    case PFACET_NORMAL :
		PEX_CONV_TO_Pvec3((pexVector3D *)buf, &fdata->norms[i]);
		buf += sizeof(pexVector3D);
		break;
	    case PFACET_COLOUR_NORMAL :
		PEX_CONV_TO_Pcoval(cm, buf, &fdata->conorms[i].colr);
		buf += PEX_COLOUR_SIZE(cm);
		PEX_CONV_TO_Pvec3((pexVector3D *)buf, 
		    &fdata->conorms[i].norm);
		buf += sizeof(pexVector3D);
		break;
	}
    }
    return(buf);
}


static void
trim_curves_to_pex( surf, buf )
    register	Phg_nurb_surf_data	*surf;
    register	char			*buf;
{
		CARD32		*num_curves = (CARD32*)buf;
    register	int		i, j, k;
    register	Ptrimcurve	*crv;
    register	pexTrimCurve	*pcrv;
    register	PEXFLOAT		*pknots;

    for  ( i = 0; i < surf->nloops; i++ ) {
	num_curves = (CARD32 *)buf; buf += sizeof(CARD32);
	*num_curves = (CARD32)surf->trimloops[i].num_curves;
	for ( j = 0; j < surf->trimloops[i].num_curves; j++ ) {
	    crv = &surf->trimloops[i].curves[j];
	    pcrv = (pexTrimCurve *)buf; buf += sizeof(pexTrimCurve);
	    pcrv->type = PEX_CONV_FROM_Prational(crv->rationality);
	    pcrv->visibility = PEX_CONV_FROM_Pedge_flag(crv->visible);
	    pcrv->order = crv->order;
	    pcrv->tMin = crv->tmin;
	    pcrv->tMax = crv->tmax;
	    pcrv->numCoord = crv->cpts.num_points;
	    pcrv->numKnots = crv->knots.num_floats;
	    pcrv->approxMethod = crv->approx_type;
	    pcrv->tolerance = crv->approx_val;
	    pknots = (PEXFLOAT *)buf;
	    for ( k = 0; k < crv->knots.num_floats; k++ ) {
		pknots[k] = crv->knots.floats[k];
		buf += sizeof(PEXFLOAT);
	    }
	    if ( crv->rationality == PRATIONAL ) {
		pexCoord3D	*ppt;
		Ppoint3		*pt = crv->cpts.points.point3d;
		for ( k = 0; k < crv->cpts.num_points; k++, pt++ ) {
		    ppt = (pexCoord3D *)buf;
		    PEX_CONV_FROM_Ppoint3(pt,ppt);
		    buf += sizeof(pexCoord3D);
		}
	    } else {
		pexCoord2D	*ppt = (pexCoord2D *)buf;
		Ppoint		*pt = crv->cpts.points.point2d;
		for ( k = 0; k < crv->cpts.num_points; k++, pt++ ) {
		    ppt = (pexCoord2D *)buf;
		    PEX_CONV_FROM_Ppoint(pt,ppt)
		    buf += sizeof(pexCoord2D);
		}
	    }
	}
    }
}


/* Utility function to compute the size of a list of trimming loops. */
static int
trim_curves_size( oc )
    pexNurbSurface	*oc;
{
    char		*pexptr = (char *)(oc + 1);
    char		*ptr;
    int			 totalSize;
    int			 i, j;
    int			 totCurves = 0;
    int			 totKnots = 0;
    int			 totPoints = 0;
    
    totalSize = oc->numLists * sizeof(Ptrimcurve_list);
    /* skip over knots and grid points */
    pexptr += oc->numUknots * oc->numVknots * sizeof(PEXFLOAT);
    pexptr += oc->mPts * oc->nPts * 
	  ((oc->type == PEXRational)
	      ? sizeof(pexCoord4D) : sizeof(pexCoord3D));

    /** Count everything first **/
    ptr = pexptr;
    for ( i = 0; i < oc->numLists; i++ ) {
	int	numCurves;
	
	totCurves += (numCurves = *(CARD32 *)ptr);
	ptr += sizeof(CARD32);
	for ( j = 0; j < numCurves; j++ ) {
	    pexTrimCurve    *xtc = (pexTrimCurve *)ptr;
	    totKnots += xtc->numKnots;
	    totPoints += xtc->numCoord;
	    ptr += sizeof(pexTrimCurve) + xtc->numKnots * sizeof(PEXFLOAT) +
		 xtc->numCoord * (xtc->type == PEXRational
		     ? sizeof(pexCoord3D) : sizeof(pexCoord2D));
	}
    }
    
    totalSize = oc->numLists * sizeof(Ptrimcurve_list) +
	totCurves * sizeof(Ptrimcurve) + totKnots * sizeof(Pfloat) +
	totPoints * sizeof(Ppoint3);    /* upper bound */

    return(totalSize);
}


/* Utility function to convert pex trimming curves to PHIGS format. */
static void
trim_curves_from_pex( oc, buf, ed )
    pexNurbSurface	*oc;	/* IN: the pex oc */
    char		*buf;	/* IN: block of space we can work with */
    Pelem_data		*ed;	/* IN/OUT: the destination for the data */
{
    register	int		i, j, k;
    register	PEXFLOAT		*xknot;
    register	Ptrimcurve	*crv;
    register	pexTrimCurve	*xtc;
    register	char		*pexptr;
    register	Ptrimcurve_list	*loop;
    
    /* Skip over knots and grid points. */
    pexptr = (char *)(oc + 1);
    pexptr += oc->numUknots * oc->numVknots * sizeof(PEXFLOAT);
    pexptr += oc->mPts * oc->nPts * ((oc->type == PEXRational)
	? sizeof(pexCoord4D) : sizeof(pexCoord3D));

    ed->nurb_surf.num_trim_loops = oc->numLists;
    loop = ed->nurb_surf.trim_loops = (Ptrimcurve_list *)buf;
    buf += ed->nurb_surf.num_trim_loops * sizeof(Ptrimcurve_list);

    for ( i = 0; i < oc->numLists; loop++, i++ ) {
	loop->num_curves = *(CARD32 *)pexptr;
	pexptr += sizeof(CARD32);
	crv = loop->curves = (Ptrimcurve *)buf;
	buf += loop->num_curves * sizeof(Ptrimcurve);
	for ( j = 0; j < loop->num_curves; j++, crv++ ) {
	    xtc = (pexTrimCurve *)pexptr;
	    pexptr += sizeof(pexTrimCurve);
	    crv->visible = PEX_CONV_TO_Pedge_flag(xtc->visibility);
	    crv->rationality = PEX_CONV_TO_Prational(xtc->type);
	    crv->order = xtc->order;
	    crv->approx_type = xtc->approxMethod;
	    crv->approx_val = xtc->tolerance;
	    crv->knots.num_floats = xtc->numKnots;
	    crv->knots.floats = (Pfloat *)buf;
	    buf += crv->knots.num_floats * sizeof(Pfloat);
	    xknot = (PEXFLOAT *)pexptr;
	    pexptr += crv->knots.num_floats * sizeof(PEXFLOAT);
	    for ( k = 0; k < crv->knots.num_floats; k++ )
		crv->knots.floats[k] = *xknot++;
	    crv->tmin = xtc->tMin;
	    crv->tmax = xtc->tMax;
	    crv->cpts.num_points = xtc->numCoord;
	    if ( xtc->type == PEXRational ) {
		crv->cpts.points.point3d = (Ppoint3 *)buf;
		buf += crv->cpts.num_points * sizeof(Ppoint3);
		for ( k = 0; k < crv->cpts.num_points; k++ ) {
		    PEX_CONV_TO_Ppoint3((pexCoord3D *)pexptr,
			&crv->cpts.points.point3d[k]);
		    pexptr += sizeof(pexCoord3D);
		}
	    } else {
		crv->cpts.points.point2d = (Ppoint *)buf;
		buf += crv->cpts.num_points * sizeof(Ppoint);
		for ( k = 0; k < crv->cpts.num_points; k++ ) {
		    PEX_CONV_TO_Ppoint((pexCoord2D *)pexptr,
			&crv->cpts.points.point2d[k]);
		    pexptr += sizeof(pexCoord2D);
		}
	    }
	}
    }
}


Pelem_type
phg_utx_pex_eltype_to_phigs( pex_el_type )
    CARD16	pex_el_type;
{
    Pelem_type	type;

    switch ( pex_el_type ) {
	case PEXOCMarkerType: type = PELEM_MARKER_TYPE; break;
	case PEXOCMarkerScale: type = PELEM_MARKER_SIZE; break;
	case PEXOCMarkerColourIndex: type = PELEM_MARKER_COLR_IND; break;
	case PEXOCMarkerColour: type = PELEM_MARKER_COLR; break;
	case PEXOCMarkerBundleIndex: type = PELEM_MARKER_IND; break;
	case PEXOCTextFontIndex: type = PELEM_TEXT_FONT; break;
	case PEXOCTextPrecision: type = PELEM_TEXT_PREC; break;
	case PEXOCCharExpansion: type = PELEM_CHAR_EXPAN; break;
	case PEXOCCharSpacing: type = PELEM_CHAR_SPACE; break;
	case PEXOCTextColourIndex: type = PELEM_TEXT_COLR_IND; break;
	case PEXOCTextColour: type = PELEM_TEXT_COLR; break;
	case PEXOCCharHeight: type = PELEM_CHAR_HT; break;
	case PEXOCCharUpVector: type = PELEM_CHAR_UP_VEC; break;
	case PEXOCTextPath: type = PELEM_TEXT_PATH; break;
	case PEXOCTextAlignment: type = PELEM_TEXT_ALIGN; break;
	case PEXOCAtextHeight: type = PELEM_ANNO_CHAR_HT; break;
	case PEXOCAtextUpVector: type = PELEM_ANNO_CHAR_UP_VEC;
	    break;
	case PEXOCAtextPath: type = PELEM_ANNO_PATH; break;
	case PEXOCAtextAlignment: type = PELEM_ANNO_ALIGN; break;
	case PEXOCAtextStyle: type = PELEM_ANNO_STYLE; break;
	case PEXOCTextBundleIndex: type = PELEM_TEXT_IND; break;
	case PEXOCLineType: type = PELEM_LINETYPE; break;
	case PEXOCLineWidth: type = PELEM_LINEWIDTH; break;
	case PEXOCLineColourIndex: type = PELEM_LINE_COLR_IND; break;
	case PEXOCLineColour: type = PELEM_LINE_COLR; break;
	case PEXOCCurveApproximation: type = PELEM_CURVE_APPROX_CRIT;
	   break;
	case PEXOCPolylineInterp: type = PELEM_LINE_SHAD_METH; break;
	case PEXOCLineBundleIndex: type = PELEM_LINE_IND; break;
	case PEXOCInteriorStyle: type = PELEM_INT_STYLE; break;
	case PEXOCInteriorStyleIndex: type = PELEM_INT_STYLE_IND; break;
	case PEXOCSurfaceColourIndex: type = PELEM_INT_COLR_IND; break;
	case PEXOCSurfaceColour: type = PELEM_INT_COLR; break;
	case PEXOCSurfaceReflAttr: type = PELEM_REFL_PROPS; break;
	case PEXOCSurfaceReflModel: type = PELEM_INT_REFL_EQN;
	    break;
	case PEXOCSurfaceInterp: type = PELEM_INT_SHAD_METH; break;
	case PEXOCBfInteriorStyle: type = PELEM_BACK_INT_STYLE; break;
	case PEXOCBfInteriorStyleIndex: type = PELEM_BACK_INT_STYLE_IND;
	    break;
	case PEXOCBfSurfaceColour: type = PELEM_BACK_INT_COLR; break;
	case PEXOCBfSurfaceReflAttr: type = PELEM_BACK_REFL_PROPS; break;
	case PEXOCBfSurfaceReflModel: type = PELEM_BACK_INT_REFL_EQN;
	    break;
	case PEXOCBfSurfaceInterp: type = PELEM_BACK_INT_SHAD_METH; break;
	case PEXOCSurfaceApproximation: type = PELEM_SURF_APPROX_CRIT;
	    break;
	case PEXOCCullingMode: type = PELEM_FACE_CULL_MODE; break;
	case PEXOCDistinguishFlag: type = PELEM_FACE_DISTING_MODE; break;
	case PEXOCPatternSize: type = PELEM_PAT_SIZE; break;
	case PEXOCPatternRefPt: type = PELEM_PAT_REF_POINT; break;
	case PEXOCPatternAttr: type = PELEM_PAT_REF_POINT_VECS;
	    break;
	case PEXOCInteriorBundleIndex: type = PELEM_INT_IND; break;
	case PEXOCSurfaceEdgeFlag: type = PELEM_EDGE_FLAG; break;
	case PEXOCSurfaceEdgeType: type = PELEM_EDGETYPE; break;
	case PEXOCSurfaceEdgeWidth: type = PELEM_EDGEWIDTH; break;
	case PEXOCSurfaceEdgeColourIndex: type = PELEM_EDGE_COLR_IND; break;
	case PEXOCSurfaceEdgeColour: type = PELEM_EDGE_COLR; break;
	case PEXOCEdgeBundleIndex: type = PELEM_EDGE_IND; break;
	case PEXOCSetAsfValues: type = PELEM_INDIV_ASF; break;
	case PEXOCLocalTransform: type = PELEM_LOCAL_MODEL_TRAN3;
	    break;
	case PEXOCLocalTransform2D: type = PELEM_LOCAL_MODEL_TRAN;
	   break;
	case PEXOCGlobalTransform: type = PELEM_GLOBAL_MODEL_TRAN3;
	    break;
	case PEXOCGlobalTransform2D: type = PELEM_GLOBAL_MODEL_TRAN;
	    break;
	case PEXOCModelClipVolume: type = PELEM_MODEL_CLIP_VOL3; break;
	case PEXOCModelClipVolume2D: type = PELEM_MODEL_CLIP_VOL; break;
	case PEXOCModelClip: type = PELEM_MODEL_CLIP_IND; break;
	case PEXOCRestoreModelClip: type = PELEM_RESTORE_MODEL_CLIP_VOL;
	    break;
	case PEXOCViewIndex: type = PELEM_VIEW_IND; break;
	case PEXOCLightState: type = PELEM_LIGHT_SRC_STATE; break;
	case PEXOCDepthCueIndex: type = PELEM_DCUE_IND; break;
	case PEXOCPickId: type = PELEM_PICK_ID; break;
	case PEXOCHlhsrIdentifier: type = PELEM_HLHSR_ID; break;
	case PEXOCAddToNameSet: type = PELEM_ADD_NAMES_SET; break;
	case PEXOCRemoveFromNameSet: type = PELEM_REMOVE_NAMES_SET; break;
	case PEXOCExecuteStructure: type = PELEM_EXEC_STRUCT; break;
	case PEXOCLabel: type = PELEM_LABEL; break;
	case PEXOCApplicationData: type = PELEM_APPL_DATA; break;
	case PEXOCGse: type = PELEM_GSE; break;
	case PEXOCMarker: type = PELEM_POLYMARKER3; break;
	case PEXOCMarker2D: type = PELEM_POLYMARKER; break;
	case PEXOCText: type = PELEM_TEXT3; break;
	case PEXOCText2D: type = PELEM_TEXT; break;
	case PEXOCAnnotationText: type = PELEM_ANNO_TEXT_REL3; break;
	case PEXOCAnnotationText2D: type = PELEM_ANNO_TEXT_REL; break;
	case PEXOCPolyline: type = PELEM_POLYLINE3; break;
	case PEXOCPolyline2D: type = PELEM_POLYLINE; break;
	case PEXOCPolylineSet: type = PELEM_POLYLINE_SET3_DATA; break;
	case PEXOCNurbCurve: type = PELEM_NUNI_BSP_CURVE; break;
	case PEXOCFillArea: type = PELEM_FILL_AREA3; break;
	case PEXOCFillArea2D: type = PELEM_FILL_AREA; break;
	case PEXOCFillAreaSet: type = PELEM_FILL_AREA_SET3; break;
	case PEXOCFillAreaSet2D: type = PELEM_FILL_AREA_SET; break;
	case PEXOCExtFillAreaSet: type = PELEM_FILL_AREA_SET3_DATA; break;
	case PEXOCTriangleStrip: type = PELEM_TRI_STRIP3_DATA; break;
	case PEXOCQuadrilateralMesh: type = PELEM_QUAD_MESH3_DATA; break;
	case PEXOCSOFAS: type = PELEM_SET_OF_FILL_AREA_SET3_DATA; break;
	case PEXOCNurbSurface: type = PELEM_NUNI_BSP_SURF; break;
	case PEXOCCellArray: type = PELEM_CELL_ARRAY3; break;
	case PEXOCCellArray2D: type = PELEM_CELL_ARRAY; break;
	case PEXOCGdp: type = PELEM_GDP3; break;
	case PEXOCGdp2D: type = PELEM_GDP; break;
	case PEXOCColourApproxIndex: type = PELEM_COLR_MAP_IND; break;
	case PEXOCExtCellArray: type = PELEM_CELL_ARRAY3_PLUS; break;

	case PEXOCAll: type = PELEM_ALL; break;
	case PEXOCNil: type = PELEM_NIL; break;

	/* No correspondence in PHIGS: */
	case PEXOCBfSurfaceColourIndex:
	    type = PELEM_NIL; break;
    }
    return type;
}

int
phg_utx_bitmask_to_intlst( size, mask, lst )
    int		size;
    CARD32	mask;
    Pint	*lst;
{
    while ( size-- ) {
	if ( mask & 1L << size ) {
	    *lst++ = size;
	}
    }
}

int
phg_utx_count_bits( mask )
    unsigned	mask;
{
    register int tot = (mask % 2);
	
    while (mask)
	tot += ((mask >>= 1) % 2);
    return (tot);
}


caddr_t
phg_utx_interior_bndl_from_pex( pib, ib )
    register	pexInteriorBundleEntry	*pib;
    register	Pint_bundle_plus		*ib;
{
    register	caddr_t			pbuf;
    register	Pextmpl_colour_spec	*cspec;

    ib->style = PEX_CONV_TO_Pinterstyle(pib->interiorStyle);
    ib->style_ind = pib->interiorStyleIndex;
    ib->refl_eqn = pib->reflectionModel;
    ib->shad_meth = pib->surfaceInterp;

    ib->back_style = PEX_CONV_TO_Pinterstyle(pib->bfInteriorStyle);
    ib->back_style_ind = pib->bfInteriorStyleIndex;
    ib->back_refl_eqn = pib->bfReflectionModel;
    ib->back_shad_meth = pib->bfSurfaceInterp;

    ib->approx_type = pib->surfaceApprox.approxMethod;
    ib->approx_val[0] = pib->surfaceApprox.uTolerance;
    ib->approx_val[1] = pib->surfaceApprox.vTolerance;

    pbuf = (caddr_t)(pib + 1);
    cspec = (Pextmpl_colour_spec *)pbuf;
    PEX_CONV_TO_Pgcolr( cspec, &ib->colr );
    pbuf += sizeof(pexColourSpecifier) + PEX_COLOUR_SIZE(ib->colr.type);
    ib->refl_props.ambient_coef = ((pexReflectionAttr *)pbuf)->ambient;
    ib->refl_props.diffuse_coef = ((pexReflectionAttr *)pbuf)->diffuse;
    ib->refl_props.specular_coef = ((pexReflectionAttr *)pbuf)->specular;
    ib->refl_props.specular_exp = ((pexReflectionAttr *)pbuf)->specularConc;
    cspec = (Pextmpl_colour_spec *)&((pexReflectionAttr *)pbuf)->specularColour;
    PEX_CONV_TO_Pgcolr( cspec, &ib->refl_props.specular_colr );
    pbuf += sizeof(pexReflectionAttr) +
	PEX_COLOUR_SIZE( ib->refl_props.specular_colr.type );

    cspec = (Pextmpl_colour_spec *)pbuf;
    PEX_CONV_TO_Pgcolr( cspec, &ib->back_colr );
    pbuf += sizeof(pexColourSpecifier) + PEX_COLOUR_SIZE(ib->back_colr.type);
    ib->back_refl_props.ambient_coef = ((pexReflectionAttr *)pbuf)->ambient;
    ib->back_refl_props.diffuse_coef = ((pexReflectionAttr *)pbuf)->diffuse;
    ib->back_refl_props.specular_coef = ((pexReflectionAttr *)pbuf)->specular;
    ib->back_refl_props.specular_exp =
	((pexReflectionAttr *)pbuf)->specularConc;
    cspec = (Pextmpl_colour_spec *)&((pexReflectionAttr *)pbuf)->specularColour;
    PEX_CONV_TO_Pgcolr( cspec, &ib->back_refl_props.specular_colr );
    pbuf += sizeof(pexReflectionAttr) +
	PEX_COLOUR_SIZE( ib->back_refl_props.specular_colr.type );
    return( pbuf );
}


caddr_t
phg_utx_interior_bndl_to_pex( ib, pib )
    register	Pint_bundle_plus	*ib;
    register	pexInteriorBundleEntry	*pib;
{
    register	caddr_t			pbuf;
    register	Pextmpl_colour_spec	*cspec;
    register	pexReflectionAttr	*refattrs;

    pib->interiorStyle = PEX_CONV_FROM_Pinterstyle(ib->style);
    pib->interiorStyleIndex = ib->style_ind;
    pib->reflectionModel = ib->refl_eqn;
    pib->surfaceInterp = ib->shad_meth;

    pib->bfInteriorStyle = PEX_CONV_FROM_Pinterstyle(ib->back_style);
    pib->bfInteriorStyleIndex = ib->back_style_ind;
    pib->bfReflectionModel = ib->back_refl_eqn;
    pib->bfSurfaceInterp = ib->back_shad_meth;

    pib->surfaceApprox.approxMethod = ib->approx_type;
    pib->surfaceApprox.uTolerance = ib->approx_val[0];
    pib->surfaceApprox.vTolerance = ib->approx_val[1];

    pbuf = (caddr_t)(pib + 1);
    cspec = (Pextmpl_colour_spec *)pbuf;
    PEX_CONV_FROM_Pgcolr( &ib->colr, cspec );
    pbuf += sizeof(pexColourSpecifier) + PEX_COLOUR_SIZE(ib->colr.type);
    refattrs = (pexReflectionAttr *)pbuf;
    refattrs->ambient = ib->refl_props.ambient_coef;
    refattrs->diffuse = ib->refl_props.diffuse_coef;
    refattrs->specular = ib->refl_props.specular_coef;
    refattrs->specularConc = ib->refl_props.specular_exp;
    refattrs->transmission = 0.0;
    cspec = (Pextmpl_colour_spec *)&refattrs->specularColour;
    PEX_CONV_FROM_Pgcolr( &ib->refl_props.specular_colr, cspec );
    pbuf += sizeof(pexReflectionAttr) +
	PEX_COLOUR_SIZE( ib->refl_props.specular_colr.type );

    cspec = (Pextmpl_colour_spec *)pbuf;
    PEX_CONV_FROM_Pgcolr( &ib->back_colr, cspec );
    pbuf += sizeof(pexColourSpecifier) + PEX_COLOUR_SIZE(ib->back_colr.type);
    refattrs = (pexReflectionAttr *)pbuf;
    refattrs->ambient = ib->back_refl_props.ambient_coef;
    refattrs->diffuse = ib->back_refl_props.diffuse_coef;
    refattrs->specular = ib->back_refl_props.specular_coef;
    refattrs->specularConc = ib->back_refl_props.specular_exp;
    refattrs->transmission = 0.0;
    cspec = (Pextmpl_colour_spec *)&refattrs->specularColour;
    PEX_CONV_FROM_Pgcolr( &ib->back_refl_props.specular_colr, cspec );
    pbuf += sizeof(pexReflectionAttr) +
	PEX_COLOUR_SIZE( ib->back_refl_props.specular_colr.type );
    return( pbuf );
}


caddr_t
phg_utx_line_bndl_from_pex( plb, lb )
    register	pexLineBundleEntry	*plb;
    register	Pline_bundle_plus	*lb;
{
    register	Pextmpl_colour_spec	*cspec;

    lb->type = plb->lineType;
    lb->width = plb->lineWidth;
    lb->shad_meth = plb->polylineInterp;
    lb->approx_type = plb->curveApprox.approxMethod;
    lb->approx_val = plb->curveApprox.tolerance;
    cspec = (Pextmpl_colour_spec *)&plb->lineColour;
    PEX_CONV_TO_Pgcolr( cspec, &lb->colr );
    return( (caddr_t)plb + sizeof(*plb) + PEX_COLOUR_SIZE(lb->colr.type));
}

caddr_t
phg_utx_line_bndl_to_pex( lb, plb )
    register	Pline_bundle_plus	*lb;
    register	pexLineBundleEntry	*plb;
{
    register	Pextmpl_colour_spec	*cspec;

    plb->lineType = lb->type;
    plb->lineWidth = lb->width;
    plb->polylineInterp = lb->shad_meth;
    plb->curveApprox.approxMethod = lb->approx_type;
    plb->curveApprox.tolerance = lb->approx_val;
    cspec = (Pextmpl_colour_spec *)&plb->lineColour;
    PEX_CONV_FROM_Pgcolr( &lb->colr, cspec );
    return( (caddr_t)plb + sizeof(*plb) + PEX_COLOUR_SIZE(lb->colr.type));
}


caddr_t
phg_utx_marker_bndl_from_pex( pmb, mb )
    register	pexMarkerBundleEntry	*pmb;
    register	Pmarker_bundle_plus	*mb;
{
    register	Pextmpl_colour_spec	*cspec;

    mb->type = pmb->markerType;
    mb->size = pmb->markerScale;
    cspec = (Pextmpl_colour_spec *)&pmb->markerColour;
    PEX_CONV_TO_Pgcolr( cspec, &mb->colr );
    return( (caddr_t)pmb + sizeof(*pmb) + PEX_COLOUR_SIZE(mb->colr.type));
}

caddr_t
phg_utx_marker_bndl_to_pex( mb, pmb )
    register	Pmarker_bundle_plus	*mb;
    register	pexMarkerBundleEntry	*pmb;
{
    register	Pextmpl_colour_spec	*cspec;

    pmb->markerType = mb->type;
    pmb->markerScale = mb->size;
    cspec = (Pextmpl_colour_spec *)&pmb->markerColour;
    PEX_CONV_FROM_Pgcolr( &mb->colr, cspec );
    return( (caddr_t)pmb + sizeof(*pmb) + PEX_COLOUR_SIZE(mb->colr.type));
}


caddr_t
phg_utx_text_bndl_from_pex( ptb, tb )
    register	pexTextBundleEntry	*ptb;
    register	Ptext_bundle_plus	*tb;
{
    register	Pextmpl_colour_spec	*cspec;

    tb->font = ptb->textFontIndex;
    tb->prec = PEX_CONV_TO_Ptxprec(ptb->textPrecision);
    tb->char_expan = ptb->charExpansion;
    tb->char_space = ptb->charSpacing;
    cspec = (Pextmpl_colour_spec *)&ptb->textColour;
    PEX_CONV_TO_Pgcolr( cspec, &tb->colr );
    return( (caddr_t)ptb + sizeof(*ptb) + PEX_COLOUR_SIZE(tb->colr.type));
}

caddr_t
phg_utx_text_bndl_to_pex( tb, ptb )
    register	Ptext_bundle_plus	*tb;
    register	pexTextBundleEntry	*ptb;
{
    register	Pextmpl_colour_spec	*cspec;

    ptb->textFontIndex = tb->font;
    ptb->textPrecision = PEX_CONV_FROM_Ptxprec(tb->prec);
    ptb->charExpansion = tb->char_expan;
    ptb->charSpacing = tb->char_space;
    cspec = (Pextmpl_colour_spec *)&ptb->textColour;
    PEX_CONV_FROM_Pgcolr( &tb->colr, cspec );
    return( (caddr_t)ptb + sizeof(*ptb) + PEX_COLOUR_SIZE(tb->colr.type));
}


caddr_t
phg_utx_edge_bndl_from_pex( peb, eb )
    register	pexEdgeBundleEntry	*peb;
    register	Pedge_bundle_plus	*eb;
{
    register	Pextmpl_colour_spec	*cspec;

    eb->flag = PEX_CONV_TO_Pedgef(peb->edges);
    eb->type = peb->edgeType;
    eb->width = peb->edgeWidth;
    cspec = (Pextmpl_colour_spec *)&peb->edgeColour;
    PEX_CONV_TO_Pgcolr( cspec, &eb->colr );
    return( (caddr_t)peb + sizeof(*peb) + PEX_COLOUR_SIZE(eb->colr.type));
}

caddr_t
phg_utx_edge_bndl_to_pex( eb, peb )
    register	Pedge_bundle_plus	*eb;
    register	pexEdgeBundleEntry	*peb;
{
    register	Pextmpl_colour_spec	*cspec;

    peb->edges = PEX_CONV_FROM_Pedgef(eb->flag);
    peb->edgeType = eb->type;
    peb->edgeWidth = eb->width;
    cspec = (Pextmpl_colour_spec *)&peb->edgeColour;
    PEX_CONV_FROM_Pgcolr( &eb->colr, cspec );
    return( (caddr_t)peb + sizeof(*peb) + PEX_COLOUR_SIZE(eb->colr.type));
}


caddr_t
phg_utx_pattern_entry_from_pex( ppb, pb )
    register	pexPatternEntry		*ppb;
    register	Ppat_rep_plus		*pb;
{
    register	int	i, count, colour_size;
    register	char	*src;
    register	Pcoval	*dest;

    pb->dims.size_x = ppb->numx;
    pb->dims.size_y = ppb->numy;
    pb->type = PEX_CONV_PEX_COLOUR_TYPE( ppb->colourType );
    colour_size = PEX_COLOUR_SIZE(pb->type);
    count = pb->dims.size_x * pb->dims.size_y;
    src = (char *)(ppb + 1);
    dest = pb->colr_array;
    for ( i = 0; i < count; i++, dest++ ) {
	PEX_CONV_TO_Pcoval( pb->type, src, dest )
	src += colour_size;
    }
    return( (caddr_t)ppb + sizeof(*ppb) + count * colour_size );
}

caddr_t
phg_utx_pattern_entry_to_pex( pb, ppb )
    register	Ppat_rep_plus		*pb;
    register	pexPatternEntry		*ppb;
{
    register	int	i, count, colour_size;
    register	Pcoval	*src;
    register	char	*dest;

    ppb->numx = pb->dims.size_x;
    ppb->numy = pb->dims.size_y;
    ppb->colourType = PEX_CONV_PHIGS_COLOUR_TYPE( pb->type );
    colour_size = PEX_COLOUR_SIZE(pb->type);
    count = ppb->numx * ppb->numy;
    src = pb->colr_array;
    dest = (char *)(ppb + 1);
    for ( i = 0; i < count; i++, src++ ) {
	PEX_CONV_FROM_Pcoval( pb->type, src, dest )
    }
    return( (caddr_t)ppb + sizeof(*ppb) + count * colour_size );
}


caddr_t
phg_utx_dcue_entry_from_pex( pdb, db )
    register	pexDepthCueEntry	*pdb;
    register	Pdcue_bundle		*db;
{
    register	Pextmpl_colour_spec	*cspec;

    db->mode = PEX_CONV_TO_Pdcuemode(pdb->mode);
    db->ref_planes[0] = pdb->backPlane;
    db->ref_planes[1] = pdb->frontPlane;
    db->scaling[0] = pdb->backScaling;
    db->scaling[1] = pdb->frontScaling;
    cspec = (Pextmpl_colour_spec *)&pdb->depthCueColour;
    PEX_CONV_TO_Pgcolr( cspec, &db->colr );
    return( (caddr_t)pdb + sizeof(*pdb) + PEX_COLOUR_SIZE(db->colr.type));
}

caddr_t
phg_utx_dcue_entry_to_pex( db, pdb )
    register	Pdcue_bundle		*db;
    register	pexDepthCueEntry	*pdb;
{
    register	Pextmpl_colour_spec	*cspec;

    pdb->mode = PEX_CONV_FROM_Pdcuemode(db->mode);
    pdb->backPlane = db->ref_planes[0];
    pdb->frontPlane = db->ref_planes[1];
    pdb->backScaling = db->scaling[0];
    pdb->frontScaling = db->scaling[1];
    cspec = (Pextmpl_colour_spec *)&pdb->depthCueColour;
    PEX_CONV_FROM_Pgcolr( &db->colr, cspec );
    return( (caddr_t)pdb + sizeof(*pdb) + PEX_COLOUR_SIZE(db->colr.type));
}


caddr_t
phg_utx_light_entry_from_pex( plsb, lsb )
    register	pexLightEntry		*plsb;
    register	Plight_src_bundle	*lsb;
{
    int		colour_size = 0;

    register	Pextmpl_colour_spec	*cspec;

    cspec = (Pextmpl_colour_spec *)&plsb->lightColour;
    switch ( lsb->type = plsb->lightType ) {
	case PLIGHT_AMBIENT:
	    PEX_CONV_TO_Pgcolr( cspec, &lsb->rec.ambient.colr );
	    colour_size = PEX_COLOUR_SIZE(lsb->rec.ambient.colr.type);
	    break;
	case PLIGHT_DIRECTIONAL:
	    PEX_CONV_TO_Pvec3( &plsb->direction, &lsb->rec.directional.dir );
	    PEX_CONV_TO_Pgcolr( cspec, &lsb->rec.directional.colr );
	    colour_size = PEX_COLOUR_SIZE(lsb->rec.directional.colr.type);
	    break;
	case PLIGHT_POSITIONAL:
	    lsb->rec.positional.coef[0] = plsb->attenuation1;
	    lsb->rec.positional.coef[1] = plsb->attenuation2;
	    PEX_CONV_TO_Ppoint3( &plsb->point, &lsb->rec.positional.pos );
	    PEX_CONV_TO_Pgcolr( cspec, &lsb->rec.positional.colr );
	    colour_size = PEX_COLOUR_SIZE(lsb->rec.positional.colr.type);
	    break;
	case PLIGHT_SPOT:
	    lsb->rec.spot.coef[0] = plsb->attenuation1;
	    lsb->rec.spot.coef[1] = plsb->attenuation2;
	    lsb->rec.spot.exp = plsb->concentration;
	    lsb->rec.spot.angle = plsb->spreadAngle;
	    PEX_CONV_TO_Ppoint3( &plsb->point, &lsb->rec.spot.pos );
	    PEX_CONV_TO_Pvec3( &plsb->direction, &lsb->rec.spot.dir );
	    PEX_CONV_TO_Pgcolr( cspec, &lsb->rec.spot.colr );
	    colour_size = PEX_COLOUR_SIZE(lsb->rec.spot.colr.type);
	    break;
	default:
	    /* Can't deal with unknown light types. */
	    IFDEBUG( fprintf(stderr,
		"API: Unrecognized light type: file %s, line %d\n",
		__FILE__, __LINE__);
	    )
	    break;
    }

    return( (caddr_t)plsb + sizeof(*plsb) + colour_size);
}


caddr_t
phg_utx_light_entry_to_pex( lsb, plsb )
    register	Plight_src_bundle	*lsb;
    register	pexLightEntry		*plsb;
{
    int		colour_type;

    register	Pextmpl_colour_spec	*cspec;

    cspec = (Pextmpl_colour_spec *)&plsb->lightColour;
    switch ( plsb->lightType = lsb->type ) {
	case PLIGHT_AMBIENT:
	    PEX_CONV_FROM_Pgcolr( &lsb->rec.ambient.colr, cspec );
	    colour_type = lsb->rec.ambient.colr.type;
	    break;
	case PLIGHT_DIRECTIONAL:
	    PEX_CONV_FROM_Pvec3(&lsb->rec.directional.dir,&plsb->direction);
	    PEX_CONV_FROM_Pgcolr( &lsb->rec.directional.colr, cspec );
	    colour_type = lsb->rec.directional.colr.type;
	    break;
	case PLIGHT_POSITIONAL:
	    plsb->attenuation1 = lsb->rec.positional.coef[0];
	    plsb->attenuation2 = lsb->rec.positional.coef[1];
	    PEX_CONV_FROM_Ppoint3( &lsb->rec.positional.pos, &plsb->point );
	    PEX_CONV_FROM_Pgcolr( &lsb->rec.positional.colr, cspec );
	    colour_type = lsb->rec.positional.colr.type;
	    break;
	case PLIGHT_SPOT:
	    plsb->attenuation1 = lsb->rec.spot.coef[0];
	    plsb->attenuation2 = lsb->rec.spot.coef[1];
	    plsb->concentration = lsb->rec.spot.exp;
	    plsb->spreadAngle = lsb->rec.spot.angle;
	    PEX_CONV_FROM_Ppoint3( &lsb->rec.spot.pos, &plsb->point );
	    PEX_CONV_FROM_Pvec3( &lsb->rec.spot.dir, &plsb->direction);
	    PEX_CONV_FROM_Pgcolr( &lsb->rec.spot.colr, cspec );
	    colour_type = lsb->rec.spot.colr.type;
	    break;
    }
    return( (caddr_t)plsb + sizeof(*plsb) + PEX_COLOUR_SIZE(colour_type));
}


caddr_t
phg_utx_view_entry_from_pex( pvb, vb )
    register	pexViewEntry	*pvb;
    register	Pview_rep3  	*vb;
{
    vb->xy_clip = PEX_CONV_TO_Pclip(pvb->clipFlags & 0x0001);
    vb->back_clip = PEX_CONV_TO_Pclip(pvb->clipFlags & 0x0002);
    vb->front_clip = PEX_CONV_TO_Pclip(pvb->clipFlags & 0x0004);
    PEX_CONV_TO_Pmatrix3(pvb->orientation,vb->ori_matrix);
    PEX_CONV_TO_Pmatrix3(pvb->mapping,vb->map_matrix);
    vb->clip_limit.x_min = pvb->clipLimits.minval.x;
    vb->clip_limit.y_min = pvb->clipLimits.minval.y;
    vb->clip_limit.z_min = pvb->clipLimits.minval.z;
    vb->clip_limit.x_max = pvb->clipLimits.maxval.x;
    vb->clip_limit.y_max = pvb->clipLimits.maxval.y;
    vb->clip_limit.z_max = pvb->clipLimits.maxval.z;
    return ( (caddr_t)pvb + sizeof(*pvb) );
}

caddr_t
phg_utx_view_entry_to_pex( vb, pvb )
    register	Pview_rep3  	*vb;
    register	pexViewEntry	*pvb;
{
    pvb->clipFlags = vb->xy_clip == PIND_CLIP ? 0x0001 : 0;
    pvb->clipFlags |= vb->back_clip == PIND_CLIP ? 0x0002 : 0;
    pvb->clipFlags |= vb->front_clip == PIND_CLIP ? 0x0004 : 0;
    PEX_CONV_FROM_Pmatrix3( vb->ori_matrix,pvb->orientation);
    PEX_CONV_FROM_Pmatrix3( vb->map_matrix,pvb->mapping);
    pvb->clipLimits.minval.x = vb->clip_limit.x_min;
    pvb->clipLimits.minval.y = vb->clip_limit.y_min;
    pvb->clipLimits.minval.z = vb->clip_limit.z_min;
    pvb->clipLimits.maxval.x = vb->clip_limit.x_max;
    pvb->clipLimits.maxval.y = vb->clip_limit.y_max;
    pvb->clipLimits.maxval.z = vb->clip_limit.z_max;
    return ( (caddr_t)pvb + sizeof(*pvb) );
}


caddr_t
phg_utx_colour_entry_from_pex( pcb, cb )
    register	pexColourSpecifier	*pcb;
    register	Pgcolr			*cb;
{
    pexColour	*pc = (pexColour *)(pcb + 1);

    /* Colour type PINDIRECT not applicable for this function. */
    switch ( pcb->colourType ) { 
	case PEXRgbFloatColour:
	    cb->type = PMODEL_RGB;
	    cb->val.general.x = pc->format.rgbFloat.red;
	    cb->val.general.y = pc->format.rgbFloat.green;
	    cb->val.general.z = pc->format.rgbFloat.blue;
	    break;
	case PEXCieFloatColour:
	    cb->type = PMODEL_CIELUV;
	    cb->val.general.x = pc->format.cieFloat.x;
	    cb->val.general.y = pc->format.cieFloat.y;
	    cb->val.general.z = pc->format.cieFloat.z;
	    break;
	case PEXHsvFloatColour:
	    cb->type = PMODEL_HSV;
	    cb->val.general.x = pc->format.hsvFloat.hue;
	    cb->val.general.y = pc->format.hsvFloat.saturation;
	    cb->val.general.z = pc->format.hsvFloat.value;
	    break;
	case PEXHlsFloatColour:
	    cb->type = PMODEL_HLS;
	    cb->val.general.x = pc->format.hlsFloat.hue;
	    cb->val.general.y = pc->format.hlsFloat.lightness;
	    cb->val.general.z = pc->format.hlsFloat.saturation;
	    break;
    }

    return ( (caddr_t)pcb + sizeof(*pcb) + PEX_COLOUR_SIZE(cb->type) );
}


caddr_t
phg_utx_colour_entry_to_pex( cb, pcb )
    register	Pgcolr			*cb;
    register	pexColourSpecifier	*pcb;
{
    pexColour	*pc = (pexColour *)(pcb + 1);

    /* Colour type PINDIRECT not applicable for this function. */
    switch ( cb->type ) { 
	case PMODEL_RGB:
	    pcb->colourType = PEXRgbFloatColour;
	    pc->format.rgbFloat.red = cb->val.general.x;
	    pc->format.rgbFloat.green = cb->val.general.y;
	    pc->format.rgbFloat.blue = cb->val.general.z;
	    break;
	case PMODEL_CIELUV:
	    pcb->colourType = PEXCieFloatColour;
	    pc->format.cieFloat.x = cb->val.general.x;
	    pc->format.cieFloat.y = cb->val.general.y;
	    pc->format.cieFloat.z = cb->val.general.z;
	    break;
	case PMODEL_HSV:
	    pcb->colourType = PEXHsvFloatColour;
	    pc->format.hsvFloat.hue = cb->val.general.x;
	    pc->format.hsvFloat.saturation = cb->val.general.y;
	    pc->format.hsvFloat.value = cb->val.general.z;
	    break;
	case PMODEL_HLS:
	    pcb->colourType = PEXHlsFloatColour;
	    pc->format.hlsFloat.hue = cb->val.general.x;
	    pc->format.hlsFloat.lightness = cb->val.general.y;
	    pc->format.hlsFloat.saturation = cb->val.general.z;
	    break;
    }
    return ( (caddr_t)pcb + sizeof(*pcb) + PEX_COLOUR_SIZE(cb->type) );
}



int
phg_utx_lut_entry_size( type, rep, gcolr )
    Phg_args_rep_type	type;
    Phg_args_rep_data	*rep;
    Pgcolr		*gcolr;	/* for colour rep only */
{
    int		size = 0;

    switch ( type ) {
	case PHG_ARGS_LNREP:
	case PHG_ARGS_EXTLNREP:
	    size = sizeof(pexLineBundleEntry)
		+ PEX_COLOUR_SIZE(rep->bundl.extlnrep.colr.type);
	    break;
	case PHG_ARGS_MKREP:
	case PHG_ARGS_EXTMKREP:
	    size = sizeof(pexMarkerBundleEntry)
		+ PEX_COLOUR_SIZE(rep->bundl.extmkrep.colr.type);
	    break;
	case PHG_ARGS_TXREP:
	case PHG_ARGS_EXTTXREP:
	    size = sizeof(pexTextBundleEntry)
		+ PEX_COLOUR_SIZE(rep->bundl.exttxrep.colr.type);
	    break;
	case PHG_ARGS_INTERREP:
	case PHG_ARGS_EXTINTERREP:
	    size = sizeof(pexInteriorBundleEntry)
		+ 2 * sizeof(pexReflectionAttr)
		+ 2 * sizeof(pexColourSpecifier)
		+ PEX_COLOUR_SIZE(rep->bundl.extinterrep.colr.type)
		+ PEX_COLOUR_SIZE(rep->bundl.extinterrep.back_colr.type)
		+ PEX_COLOUR_SIZE(
		    rep->bundl.extinterrep.refl_props.specular_colr.type)
		+ PEX_COLOUR_SIZE(
		    rep->bundl.extinterrep.back_refl_props.specular_colr.type);
	    break;
	case PHG_ARGS_EDGEREP:
	case PHG_ARGS_EXTEDGEREP:
	    size = sizeof(pexEdgeBundleEntry)
		+ PEX_COLOUR_SIZE(rep->bundl.extedgerep.colr.type);
	    break;
	case PHG_ARGS_PTREP:
	case PHG_ARGS_EXTPTREP:
	    size = sizeof(pexPatternEntry)
		+ (rep->bundl.extptrep.dims.size_x
		    * rep->bundl.extptrep.dims.size_y
		    * PEX_COLOUR_SIZE(rep->bundl.extptrep.type));
	    break;
	case PHG_ARGS_COREP:
	    size = sizeof(pexColourSpecifier) + PEX_COLOUR_SIZE(gcolr->type);
	    break;
	case PHG_ARGS_DCUEREP:
	    size = sizeof(pexDepthCueEntry)
		+ PEX_COLOUR_SIZE(rep->bundl.dcuerep.colr.type);
	    break;
	case PHG_ARGS_LIGHTSRCREP: {
	    Plight_src_rec	*rec = &rep->bundl.lightsrcrep.rec;
	    size = sizeof(pexLightEntry);
	    switch ( rep->bundl.lightsrcrep.type ) {
		case PLIGHT_AMBIENT:
		    size += PEX_COLOUR_SIZE(rec->ambient.colr.type);
		    break;
		case PLIGHT_DIRECTIONAL:
		    size += PEX_COLOUR_SIZE(rec->directional.colr.type);
		    break;
		case PLIGHT_POSITIONAL:
		    size += PEX_COLOUR_SIZE(rec->positional.colr.type);
		    break;
		case PLIGHT_SPOT:
		    size += PEX_COLOUR_SIZE(rec->spot.colr.type);
		    break;
		default:
		    break;
	    }
	    } break;
	case PHG_ARGS_VIEWREP:
	    size = sizeof(pexViewEntry);
	    break;
	case PHG_ARGS_COLRMAPREP:
	    size = sizeof(pexColourApproxEntry);
	    break;
    }
    return size;
}


caddr_t
phg_utx_ws_xform_from_pex( buf, ws_xform )
    register	caddr_t			buf;
    register	Phg_ret_ws_tran3	*ws_xform;
{
    ws_xform->state = PEX_CONV_TO_Pupdatest(*(CARD32 *)buf);
    buf += 4;
    ws_xform->req_window.x_min = ((pexNpcSubvolume *)buf)->minval.x;
    ws_xform->req_window.y_min = ((pexNpcSubvolume *)buf)->minval.y;
    ws_xform->req_window.z_min = ((pexNpcSubvolume *)buf)->minval.z;
    ws_xform->req_window.x_max = ((pexNpcSubvolume *)buf)->maxval.x;
    ws_xform->req_window.y_max = ((pexNpcSubvolume *)buf)->maxval.y;
    ws_xform->req_window.z_max = ((pexNpcSubvolume *)buf)->maxval.z;
    buf += sizeof(pexNpcSubvolume);
    ws_xform->cur_window.x_min = ((pexNpcSubvolume *)buf)->minval.x;
    ws_xform->cur_window.y_min = ((pexNpcSubvolume *)buf)->minval.y;
    ws_xform->cur_window.z_min = ((pexNpcSubvolume *)buf)->minval.z;
    ws_xform->cur_window.x_max = ((pexNpcSubvolume *)buf)->maxval.x;
    ws_xform->cur_window.y_max = ((pexNpcSubvolume *)buf)->maxval.y;
    ws_xform->cur_window.z_max = ((pexNpcSubvolume *)buf)->maxval.z;
    buf += sizeof(pexNpcSubvolume);
    ws_xform->req_viewport.x_min = ((pexViewport *)buf)->minval.x;
    ws_xform->req_viewport.y_min = ((pexViewport *)buf)->minval.y;
    ws_xform->req_viewport.z_min = ((pexViewport *)buf)->minval.z;
    ws_xform->req_viewport.x_max = ((pexViewport *)buf)->maxval.x;
    ws_xform->req_viewport.y_max = ((pexViewport *)buf)->maxval.y;
    ws_xform->req_viewport.z_max = ((pexViewport *)buf)->maxval.z;
    buf += sizeof(pexViewport);
    ws_xform->cur_viewport.x_min = ((pexViewport *)buf)->minval.x;
    ws_xform->cur_viewport.y_min = ((pexViewport *)buf)->minval.y;
    ws_xform->cur_viewport.z_min = ((pexViewport *)buf)->minval.z;
    ws_xform->cur_viewport.x_max = ((pexViewport *)buf)->maxval.x;
    ws_xform->cur_viewport.y_max = ((pexViewport *)buf)->maxval.y;
    ws_xform->cur_viewport.z_max = ((pexViewport *)buf)->maxval.z;
    return buf += sizeof(pexViewport);
}


#define MAX3(a,b,c) \
    (((a) >= (b) && (a) >= (c)) ? a : \
	(((b) >= (a) && (b) >= (c)) ? b : c))

#define MIN3(a,b,c) \
    (((a) <= (b) && (a) <= (c)) ? a : \
	(((b) <= (a) && (b) <= (c)) ? b : c))

static float
invert_hue(n1, n2, hue)
float n1, n2, hue;
{
    if (hue > 360.0)
	hue -= 360.0;
    else if (hue < 0.0)
	hue += 360.0;
    if (hue < 60.0)
	return (n1 + (n2 - n1) * hue / 60.0);
    else if (hue < 180.0)
	return (n2);
    else if (hue < 240.0)
	return (n1 + (n2 - n1) * (240.0 - hue) / 60.0);
    else
	return (n1);
}

/* Colour conversion routine.  Convert the src Pgcolr to the type
 * specified by dst->type, and store in dst->val.  Types assumed
 * to be PMODEL_RGB, PMODEL_CIELUV, PMODEL_HSV, or PMODEL_HLS, but not PINDIRECT.
 *   If CIE is not involved, parameters after dst are ignored.
 *   If CIE is involved, then a handle to a Phg_chroma_info structure
 * should be passed in.
 *   RGB <-> CIE conversion data according to Annex I, Section 7.2 of draft 
 * PHIGS Standard ISO/IEC 9592-1:1988(E).  TODO:Assumption is that 1931 CIE
 * space is used, where a triplet is (x,y,Y), as opposed to 1976 CIELUV.
 *   RGB <-> HLS and RGB <-> HSV conversion routines from Foley & van Dam.
 */
 
int
phg_utx_convert_colour(src, dst, c)
register Pgcolr		    *src;
register Pgcolr		    *dst;
register Phg_chroma_info    *c;
{
    
    Pfloat	XC, YC, ZC, TC;
    Pgcolr	tmp;

    if (src->type == dst->type) {
	dst->val.general.x = src->val.general.x;
	dst->val.general.y = src->val.general.y;
	dst->val.general.z = src->val.general.z;
	return(1);
    }

    /* calculate and cache needed CIE values if necessary */
    if (c && !c->flags.coefs_calculated &&
	    (src->type == PMODEL_CIELUV || dst->type == PMODEL_CIELUV)) {

	Pfloat   kd;

	c->zr = 1.0 - c->xr - c->yr;
	c->zg = 1.0 - c->xg - c->yg;
	c->zb = 1.0 - c->xb - c->yb;
	
	if (c->yw == 0.0)	/* avoid division by zero */
	    return(0);
	    
	c->Xw = (c->xw/c->yw) * c->Yw;
	c->Zw = ((1.0 - c->xw - c->yw) / c->yw) * c->Yw;

	kd = c->xr * (c->yg * c->zb - c->yb * c->zg) +
	     c->xg * (c->yb * c->zr - c->yr * c->zb) +
	     c->xb * (c->yr * c->zg - c->yg * c->zr);

	if (kd == 0.0)		/* avoid division by zero */
	    return(0);

	c->k1 = (c->yg * c->zb - c->yb * c->zg) / kd;
	c->k2 = (c->xb * c->zg - c->xg * c->zb) / kd;
	c->k3 = (c->xg * c->yb - c->xb * c->yg) / kd;
	c->k4 = (c->yb * c->zr - c->yr * c->zb) / kd;
	c->k5 = (c->xr * c->zb - c->xb * c->zr) / kd;
	c->k6 = (c->xb * c->yr - c->xr * c->yb) / kd;
	c->k7 = (c->yr * c->zg - c->yg * c->zr) / kd;
	c->k8 = (c->xg * c->zr - c->xr * c->zg) / kd;
	c->k9 = (c->xr * c->yg - c->xg * c->yr) / kd;
	c->Tr = c->k1 * c->Xw + c->k2 * c->Yw + c->k3 * c->Zw;
	c->Tg = c->k4 * c->Xw + c->k5 * c->Yw + c->k6 * c->Zw;
	c->Tb = c->k7 * c->Xw + c->k8 * c->Yw + c->k9 * c->Zw;
	c->flags.coefs_calculated = 1;
    }

    /* presume that caller has ensured that neither are PINDIRECT */
    switch (src->type) {
	case PMODEL_RGB :
	{
	    Pfloat r = src->val.general.x;
	    Pfloat g = src->val.general.y;
	    Pfloat b = src->val.general.z;
	    Pfloat max, min, rc, gc, bc;
	    
	    switch (dst->type) {
		case PMODEL_CIELUV:
		    XC = c->xr * c->Tr * r + 
			 c->xg * c->Tg * g + 
			 c->xb * c->Tb * b;
		    YC = c->yr * c->Tr * r + 
			 c->yg * c->Tg * g +
			 c->yb * c->Tb * b;
		    ZC = c->zr * c->Tr * r +
			 c->zg * c->Tg * g +
			 c->zb * c->Tb * b;
		    TC = XC + YC + ZC;
		    dst->val.general.x = XC / TC;   /* x */
		    dst->val.general.y = YC / TC;   /* y */
		    dst->val.general.z = YC;	    /* Y: luminance */
		    return(1);
		    
		case PMODEL_HLS:
		    max = MAX3(r, g, b);
		    min = MIN3(r, g, b);
		    dst->val.general.y = (max + min) / 2;   /* lightness */
		    if (max == min) {
			/** Achromatic case, hue is actually undefined **/
			dst->val.general.x = 0.0;   /* hue */
			dst->val.general.z = 0.0;   /* saturation */
		    } else {
			/** Chromatic case **/
			
			/* calculate saturation */
			if (dst->val.general.y <= 0.5)
			    dst->val.general.z = (max - min) / (max + min);
			else
			    dst->val.general.z = (max - min) / (2-max-min);
			    
			/* calculate hue */
			rc = (max - r) / (max - min);
			gc = (max - g) / (max - min);
			bc = (max - b) / (max - min);
			if (max == r)	/* between yellow and magenta */
			    dst->val.general.x = bc - gc;
			else if (max == g)  /* between cyan and yellow */
			    dst->val.general.x = 2 + rc - bc;
			else /* max == b, between magenta and cyan */
			    dst->val.general.x = 4 + gc - rc;
			dst->val.general.x *= 60.0;
			if (dst->val.general.x < 0.0)
			    dst->val.general.x += 360.0;
		    }
		    return(1);
		    
		case PMODEL_HSV:
		    max = MAX3(r, g, b);
		    min = MIN3(r, g, b);
		    dst->val.general.z = max;	/* value */
		    dst->val.general.y =	/* saturation */
			((max != 0.0) ? (max - min) / max : 0.0);
		    if (dst->val.general.y == 0.0)  /* achromatic */
			dst->val.general.x = 0.0;   /* actually undefined */
		    else {	/* chromatic */
			rc = (max - r) / (max - min);
			gc = (max - g) / (max - min);
			bc = (max - b) / (max - min);
			if (max == r)	/* between yellow and magenta */
			    dst->val.general.x = bc - gc;
			else if (max == g)  /* between cyan and yellow */
			    dst->val.general.x = 2 + rc - bc;
			else /* max == b, between magenta and cyan */
			    dst->val.general.x = 4 + gc - rc;
			dst->val.general.x *= 60.0;
			if (dst->val.general.x < 0.0)
			    dst->val.general.x += 360.0;
		    }
		    return(1);
	    }
	}

	case PMODEL_CIELUV :
	    switch (dst->type) {
		case PMODEL_RGB:
		    YC = src->val.general.z;
		    XC = (src->val.general.x / src->val.general.y) * YC;
		    ZC = ((1.0 - src->val.general.x - src->val.general.y) /
			   src->val.general.y) * YC;
		    dst->val.general.x = (c->k1 / c->Tr) * XC +
					 (c->k2 / c->Tr) * YC +
					 (c->k3 / c->Tr) * ZC;
		    dst->val.general.y = (c->k4 / c->Tg) * XC +
					 (c->k5 / c->Tg) * YC +
					 (c->k6 / c->Tg) * ZC;
		    dst->val.general.z = (c->k7 / c->Tb) * XC +
					 (c->k8 / c->Tb) * YC +
					 (c->k9 / c->Tb) * ZC;
		    return(1);

		case PMODEL_HLS:
		    tmp.type = PMODEL_RGB;
		    if (!phg_utx_convert_colour(src, &tmp, c))
			return(0);
		    if (!phg_utx_convert_colour(&tmp, dst, c))
			return(0);
		    return(1);
		    
		case PMODEL_HSV:
		    tmp.type = PMODEL_RGB;
		    if (!phg_utx_convert_colour(src, &tmp, c))
			return(0);
		    if (!phg_utx_convert_colour(&tmp, dst, c))
			return(0);
		    return(1);
	    }
	    
	case PMODEL_HLS :
	{
	    Pfloat h = src->val.general.x;
	    Pfloat l = src->val.general.y;
	    Pfloat s = src->val.general.z;
	    Pfloat m1, m2;

	    switch (dst->type) {
		case PMODEL_RGB:
		    if (l < 0.5)
			m2 = l * (1 + s);
		    else
			m2 = l + s - l*s;
		    m1 = 2*l - m2;
		    if (s == 0.0) {
			dst->val.general.x = dst->val.general.y =
					     dst->val.general.z = l;
		    } else {
			dst->val.general.x = invert_hue(m1, m2, h + 120.0);
			dst->val.general.y = invert_hue(m1, m2, h);
			dst->val.general.z = invert_hue(m1, m2, h - 120.0);
		    }
		    return(1);
		    
		case PMODEL_CIELUV:
		    tmp.type = PMODEL_RGB;
		    if (!phg_utx_convert_colour(src, &tmp, c))
			return(0);
		    if (!phg_utx_convert_colour(&tmp, dst, c))
			return(0);
		    return(1);
		case PMODEL_HSV:
		    tmp.type = PMODEL_RGB;
		    if (!phg_utx_convert_colour(src, &tmp, c))
			return(0);
		    if (!phg_utx_convert_colour(&tmp, dst, c))
			return(0);
		    return(1);
	    }
	}
	case PMODEL_HSV :
	{
	    Pfloat h = src->val.general.x;
	    Pfloat s = src->val.general.y;
	    Pfloat v = src->val.general.z;

	    switch (dst->type) {
		case PMODEL_RGB:
		    if (s == 0.0) {
			dst->val.general.x = dst->val.general.y =
					     dst->val.general.z = v;
		    } else {
			Pint	i;
			Pfloat	f, p, q, t;
			if (h == 360.0)
			    h = 0.0;
			h /= 60;
			i = (int)floor((double)h);
			f = h - i;
			p = v * (1 - s);
			q = v * (1 - s*f);
			t = v * (1 - s * (1 - f));
			switch (i) {
			    case 0 :
				dst->val.general.x = v;
				dst->val.general.y = t;
				dst->val.general.z = p;
				break;
			    case 1 :
				dst->val.general.x = q;
				dst->val.general.y = v;
				dst->val.general.z = p;
				break;
			    case 2 :
				dst->val.general.x = p;
				dst->val.general.y = v;
				dst->val.general.z = t;
				break;
			    case 3 :
				dst->val.general.x = p;
				dst->val.general.y = q;
				dst->val.general.z = v;
				break;
			    case 4 :
				dst->val.general.x = t;
				dst->val.general.y = p;
				dst->val.general.z = v;
				break;
			    case 5 :
				dst->val.general.x = v;
				dst->val.general.y = p;
				dst->val.general.z = q;
				break;
			}
		    }
		    return(1);

		case PMODEL_CIELUV:
		    tmp.type = PMODEL_RGB;
		    if (!phg_utx_convert_colour(src, &tmp, c))
			return(0);
		    if (!phg_utx_convert_colour(&tmp, dst, c))
			return(0);
		    return(1);

		case PMODEL_HLS:
		    tmp.type = PMODEL_RGB;
		    if (!phg_utx_convert_colour(src, &tmp, c))
			return(0);
		    if (!phg_utx_convert_colour(&tmp, dst, c))
			return(0);
		    return(1);
	    }
	}
    }
    return(1);
}


int
phg_utx_map_update_state( def_mode, mod_mode )
    Pdefer_mode		def_mode;
    Pmod_mode		mod_mode;
{
    int		mode;

    /* BNIG and BNIL have no counterpart in PEX, implement them as ASAP. */
    switch ( mod_mode ) {
	case PMODE_NIVE:
	    switch ( def_mode ) {
		case PDEFER_BNIG: case PDEFER_BNIL: case PDEFER_ASAP:
		    mode = PEXVisualizeEach; break;
		case PDEFER_ASTI:
		    mode = PEXVisualizeWhenever; break;
		case PDEFER_WAIT:
		    mode = PEXVisualizeNone; break;
	    }
	    break;
	case PMODE_UWOR:
	    switch ( def_mode ) {
		case PDEFER_ASAP: case PDEFER_BNIG: case PDEFER_BNIL:
		    mode = PEXVisualizeEach; break;
		case PDEFER_ASTI: case PDEFER_WAIT:
		    mode = PEXVisualizeEasy; break;
	    }
	    break;
	case PMODE_UQUM:
	    switch ( def_mode ) {
		case PDEFER_ASAP: case PDEFER_BNIG: case PDEFER_BNIL:
		    mode = PEXVisualizeEach; break;
		case PDEFER_ASTI: case PDEFER_WAIT:
		    mode = PEXSimulateSome; break;
	    }
	    break;
    }
    return mode;
}

#undef MIN3
#undef MAX3


#if (__STDC__ && !defined(UNIXCPP)) || defined(ANSICPP)
#define VAR_LENGTH(_l,_n) (4 * (_l) - sizeof(pex##_n))
#else
#define VAR_LENGTH(_l,_n) (4 * (_l) - sizeof(pex/**/_n))
#endif

#define PEX_OPT_DATA_SIZE(_ct, _attr)					\
    ((((_attr) & PEXGAColour) ?						\
		((_ct) == PEXIndexedColour ? sizeof(pexIndexedColour) :	\
		  ((_ct) == PEXRgb8Colour ? sizeof(pexRgb8Colour) :	\
		    ((_ct) == PEXRgb16Colour ? sizeof(pexRgb16Colour) :	\
		      sizeof(pexRgbFloatColour)))) : 0) +		\
     (((_attr) & PEXGANormal) ? 3 * sizeof(PEXFLOAT) : 0) +		\
     (((_attr) & PEXGAEdges) ? sizeof(CARD32) : 0))

#define PEX_VERTEX_SIZE(_ct, _va)   \
    (3 * sizeof(PEXFLOAT) + PEX_OPT_DATA_SIZE(_ct, _va))

#define PHIGS_VERTEX_SIZE(_va)						    \
    ((_va) == 0x0000 ? sizeof(Ppoint3) :				    \
	(_va) == PEXGAColour ? sizeof(Pptco3) :				    \
	    (_va) == PEXGANormal ? sizeof(Pptnorm3) : sizeof(Pptconorm3))

#define PHIGS_FACET_SIZE(_fa)						    \
    ((_fa) == 0x0000 ? 0 :						    \
	(_fa) == PEXGAColour ? sizeof(Pcoval) :				    \
	    (_fa) == PEXGANormal ? sizeof(Pvec3) : sizeof(Pconorm3))

#define NUM_VERTS_IN_PEX_FAREA_SET(_ct, _va, _fa, _totSize, _nLists) 	    \
      ((_totSize - PEX_OPT_DATA_SIZE(_ct, _fa) - sizeof(CARD32) * _nLists)  \
	    / PEX_VERTEX_SIZE(_ct, _va))
    
#if (__STDC__ && !defined(UNIXCPP)) || defined(ANSICPP)
#define RHEADER(_n) ((pex##_n *)oc)
#else
#define RHEADER(_n) ((pex/**/_n *)oc)
#endif

static int
encoded_text_size( num_encodings, buf )
    CARD16		num_encodings;
    char		*buf;
{
    int			i, size = 0;
    pexMonoEncoding	*encoding;

    /* TODO: account for character size. */
    for ( i = 0; i < num_encodings; i++ ) {
	encoding = (pexMonoEncoding *)buf;
	size += encoding->numChars;
	buf += sizeof(pexMonoEncoding) + encoding->numChars
	    + PADDING(encoding->numChars);
    }
    return (size > 0 ? size + 1 : 0);
}

static int
sofa_size( sofa )
    pexSOFAS	*sofa;
{
    int		size = 0;

    size = sofa->numVertices * PHIGS_VERTEX_SIZE(sofa->vertexAttributes)
	+ sofa->numFAS * PHIGS_FACET_SIZE(sofa->FAS_Attributes)
	+ sofa->numFAS * sizeof(Pint_list_list)
	+ sofa->numContours * sizeof(Pint_list)
	+ sofa->numEdges * sizeof(Pint);
    if ( sofa->edgeAttributes )
	size += sofa->numFAS * sizeof(Pedge_data_list_list)
	    + sofa->numContours * sizeof(Pedge_data_list)
	    + sofa->numEdges * sizeof(Pedge_flag);
    return size;
}

int
phg_utx_compute_el_size( el_info, oc )
    pexElementInfo	*el_info;	/* IN: the element info */
    pexElementInfo	*oc;		/* IN: the full OC, needed for some
    					       of the more complicated OCs.
					       Not needed for the others. */
{
    int				count, size;
    int	    			numLists, numVerts, mPts, nPts;
    int	    			numUknots, numVknots;
    pexBitmask    		vertAttr, facetAttr;
    pexColourType 		ct;
    pexCoordType  		coordType;

    /* Not all element types have a "size."  Only the ones with variable
     * sized data do.
     */
    size = 0;
    switch ( el_info->elementType ) {
	case PEXOCNil:
	    size = 0;
	    break;
	case PEXOCPolyline:
	    count = VAR_LENGTH(el_info->length,Polyline) / sizeof(pexCoord3D);
	    size = count * sizeof(Ppoint3);
	    break;
	case PEXOCPolyline2D:
	    count = VAR_LENGTH(el_info->length,Polyline2D) / sizeof(pexCoord2D);
	    size = count * sizeof(Ppoint);
	    break;
	case PEXOCMarker:
	    count = VAR_LENGTH(el_info->length,Marker) / sizeof(pexCoord3D);
	    size = count * sizeof(Ppoint3);
	    break;
	case PEXOCMarker2D:
	    count = VAR_LENGTH(el_info->length,Marker2D) / sizeof(pexCoord2D);
	    size = count * sizeof(Ppoint);
	    break;
	case PEXOCFillArea:
	    count = VAR_LENGTH(el_info->length,FillArea) / sizeof(pexCoord3D);
	    size = count * sizeof(Ppoint3);
	    break;
	case PEXOCFillArea2D:
	    count = VAR_LENGTH(el_info->length,FillArea2D) / sizeof(pexCoord2D);
	    size = count * sizeof(Ppoint);
	    break;
	case PEXOCCellArray:
	    count = VAR_LENGTH(el_info->length,CellArray)
		/ sizeof(pexTableIndex);
	    size = count * sizeof(Pint);
	    break;
	case PEXOCCellArray2D:
	    count = VAR_LENGTH(el_info->length,CellArray2D)
		/ sizeof(pexTableIndex);
	    size = count * sizeof(Pint);
	    break;
	case PEXOCApplicationData:
	    size = VAR_LENGTH(el_info->length,ApplicationData);
	    break;
	case PEXOCGse:
	    size = VAR_LENGTH(el_info->length,Gse);
	    break;
	case PEXOCAddToNameSet:
	case PEXOCRemoveFromNameSet:
	    count = VAR_LENGTH(el_info->length,AddToNameSet) / sizeof(pexName);
	    size = count * sizeof(Pint);
	    break;
	case PEXOCModelClipVolume:
	    count = VAR_LENGTH(el_info->length,ModelClipVolume)
		/ sizeof(pexHalfSpace);
	    size = count * sizeof(Phalf_space3);
	    break;
	case PEXOCModelClipVolume2D:
	    count = VAR_LENGTH(el_info->length,ModelClipVolume2D)
		/ sizeof(pexHalfSpace2D);
	    size = count * sizeof(Phalf_space);
	    break;

	/* Need the actual data for these. */
	case PEXOCFillAreaSet:
	    assure(oc != NULL);
	    numLists = RHEADER(FillAreaSet)->numLists;
	    numVerts = (VAR_LENGTH(el_info->length,FillAreaSet)
		- numLists * sizeof(CARD32)) / sizeof(pexCoord3D);
	    size = numLists * sizeof(Ppoint_list3) +
		numVerts * sizeof(Ppoint3);
	    break;

	case PEXOCFillAreaSet2D:
	    assure(oc != NULL);
	    numLists = RHEADER(FillAreaSet2D)->numLists;
	    numVerts = (VAR_LENGTH(el_info->length,FillAreaSet2D)
		- numLists * sizeof(CARD32)) / sizeof(pexCoord2D);
	    size = numLists * sizeof(Ppoint_list) +
		numVerts * sizeof(Ppoint);
	    break;

	case PEXOCPolylineSet:
	    assure(oc != NULL);
	    numLists = RHEADER(PolylineSet)->numLists;
	    vertAttr = RHEADER(PolylineSet)->vertexAttribs;
	    ct       = RHEADER(PolylineSet)->colourType;
	    numVerts = (VAR_LENGTH(el_info->length,PolylineSet)
		- numLists * sizeof(CARD32)) / 
	    PEX_VERTEX_SIZE(ct, vertAttr);
	    size = numLists * sizeof(Pline_vdata_list3) + 
		numVerts * (vertAttr ? sizeof(Pptco3) : sizeof(Ppoint3));
	    break;
		      
	case PEXOCExtFillAreaSet:
	    assure(oc != NULL);
	    vertAttr	= RHEADER(ExtFillAreaSet)->vertexAttribs;
	    facetAttr = RHEADER(ExtFillAreaSet)->facetAttribs;
	    ct	= RHEADER(ExtFillAreaSet)->colourType;
	    numLists	= RHEADER(ExtFillAreaSet)->numLists;
	    numVerts  = NUM_VERTS_IN_PEX_FAREA_SET(ct, vertAttr, facetAttr,
		VAR_LENGTH(el_info->length,ExtFillAreaSet), numLists);
	    size = ((vertAttr & PEXGAEdges) ?
		    numLists * sizeof(Pedge_data_list) + 
		    numVerts * sizeof(Pedge_flag) : 0) +
		numLists * sizeof(Pfacet_vdata_list3) +
		numVerts * PHIGS_VERTEX_SIZE(vertAttr);
	    break;

	case PEXOCText:
	    assure(oc != NULL);
	    size = encoded_text_size( RHEADER(Text)->numEncodings,
		(char *)(RHEADER(Text) + 1) );
	    break;
	case PEXOCText2D:
	    assure(oc != NULL);
	    size = encoded_text_size( RHEADER(Text2D)->numEncodings,
		(char *)(RHEADER(Text2D) + 1) );
	break;
	case PEXOCAnnotationText:
	    assure(oc != NULL);
	    size = encoded_text_size(
		RHEADER(AnnotationText)->numEncodings,
		(char*)(RHEADER(AnnotationText)+1));
	    break;
	case PEXOCAnnotationText2D:
	    assure(oc != NULL);
	    size = encoded_text_size(
		RHEADER(AnnotationText2D)->numEncodings,
		(char*)(RHEADER(AnnotationText2D)+1));
	    break;

	case PEXOCTriangleStrip:
	    assure(oc != NULL);
	    vertAttr	= RHEADER(TriangleStrip)->vertexAttribs;
	    facetAttr = RHEADER(TriangleStrip)->facetAttribs;
	    ct	= RHEADER(TriangleStrip)->colourType;
	    numVerts	= RHEADER(TriangleStrip)->numVertices;
	    size = (numVerts - 2) * PHIGS_FACET_SIZE(facetAttr) +
		numVerts * PHIGS_VERTEX_SIZE(vertAttr);
	    break;
		      
	case PEXOCQuadrilateralMesh:
	    assure(oc != NULL);
	    vertAttr	= RHEADER(QuadrilateralMesh)->vertexAttribs;
	    facetAttr = RHEADER(QuadrilateralMesh)->facetAttribs;
	    ct	= RHEADER(QuadrilateralMesh)->colourType;
	    nPts	= RHEADER(QuadrilateralMesh)->nPts;
	    mPts	= RHEADER(QuadrilateralMesh)->mPts;
	    size = nPts * mPts * PHIGS_VERTEX_SIZE(vertAttr) +
		(nPts - 1) * (mPts - 1) * PHIGS_FACET_SIZE(facetAttr);
	    break;

	case PEXOCSOFAS:
	    assure(oc != NULL);
	    size = sofa_size( RHEADER(SOFAS) );
	    break;
		     
	case PEXOCExtCellArray:
	    assure(oc != NULL);
	    size = RHEADER(ExtCellArray)->dx * 
		RHEADER(ExtCellArray)->dy * sizeof(Pcoval);
	    break;

	case PEXOCNurbCurve:
	    assure(oc != NULL);
	    numUknots	= RHEADER(NurbCurve)->numKnots;
	    coordType = RHEADER(NurbCurve)->coordType;
	    numVerts	= RHEADER(NurbCurve)->numPoints;
	    size  = numUknots * sizeof(Pfloat) +
		numVerts * ((coordType == PEXRational) 
		    ? sizeof(Ppoint4) : sizeof(Ppoint3));
	    break;
		      
	case PEXOCNurbSurface:
	    assure(oc != NULL);
	    coordType = RHEADER(NurbSurface)->type;
	    numUknots	= RHEADER(NurbSurface)->numUknots;
	    numVknots	= RHEADER(NurbSurface)->numVknots;
	    mPts	= RHEADER(NurbSurface)->mPts;
	    nPts	= RHEADER(NurbSurface)->nPts;
	    numLists  = RHEADER(NurbSurface)->numLists;
	    size  = (numUknots + numVknots) * sizeof(Pfloat) +
		mPts * nPts * ((coordType == PEXRational)
		    ? sizeof(Ppoint4) : sizeof(Ppoint3))
			+ trim_curves_size( (pexNurbSurface *)oc );
	    break;
		      
	case PEXOCLightState: 
	    assure(oc != NULL);
	    /* The count is NOT implicit in el length because of
	     * variable padding.
	     */
	    size = (RHEADER(LightState)->numEnable + 
		RHEADER(LightState)->numDisable) * sizeof(Pint);
	    break;

	case PEXOCGdp:
	    assure(oc != NULL);
	    size = RHEADER(Gdp)->numPoints * sizeof(pexCoord3D)
		+ RHEADER(Gdp)->numBytes;
	    break;

	case PEXOCGdp2D:
	    assure(oc != NULL);
	    size = RHEADER(Gdp2D)->numPoints * sizeof(pexCoord2D)
		+ RHEADER(Gdp2D)->numBytes;
	    break;

	case PEXOCParaSurfCharacteristics: 
	    switch ( RHEADER(ParaSurfCharacteristics)->characteristics ) {
		case PEXPSCNone:
		case PEXPSCImpDep:
		case PEXPSCIsoCurves:
		    size = 0;
		    break;
		case PEXPSCMcLevelCurves:
		case PEXPSCWcLevelCurves: {
		    pexPSC_LevelCurves	*drec;

		    assure(oc != NULL);
		    drec = (pexPSC_LevelCurves *)
			(RHEADER(ParaSurfCharacteristics) + 1);
		    size = drec->numberIntersections * sizeof(Pfloat);
		} break;
	    }
	    break;
    }
    return size;
}


static void
sofa_data_from_pex( pexptr, sofa, ed, buf )
    char	*pexptr;
    pexSOFAS	*sofa;
    Pelem_data	*ed;
    char	*buf;
{
    CARD8	*edge;
    CARD16	*vi;

    register	int	i, j, k;

    if ( ed->sofas3.fflag != PFACET_NONE ) {
	ed->sofas3.fdata.colrs = (Pcoval *)buf;
	pexptr = fdata_from_pex( ed->sofas3.fflag,
	    ed->sofas3.num_sets, ed->sofas3.colr_model, pexptr,
	    &ed->sofas3.fdata );
	buf += ed->sofas3.num_sets *
	    PHIGS_FACET_SIZE(sofa->FAS_Attributes);
    }

    /* Vertex data */
    ed->sofas3.vdata.vertex_data.points = (Ppoint3 *)buf;
    pexptr = phg_utx_vdata_from_pex( 1, 0, ed->sofas3.vflag,
	&ed->sofas3.vdata, PEDGE_NONE, (Pedge_data_list *)NULL,
	ed->sofas3.colr_model, pexptr );
    buf += ed->sofas3.vdata.num_vertices *
	PHIGS_VERTEX_SIZE(sofa->vertexAttributes);

    /* Connectivity and edge data */
    if ( ed->sofas3.eflag != PEDGE_NONE ) {
	edge = (CARD8 *)pexptr;
	vi = (CARD16 *)(edge + sofa->numEdges
	    + PADDING(sofa->numEdges * sizeof(CARD8)));
    } else {
	edge = (CARD8 *)NULL;
	vi = (CARD16 *)pexptr;
    } 

    ed->sofas3.vlist = (Pint_list_list *)buf;
    buf += sofa->numFAS * sizeof(Pint_list_list);
    for ( i = 0; i < sofa->numFAS; i++ ) {
	ed->sofas3.vlist[i].num_lists = (Pint)*vi++;
	ed->sofas3.vlist[i].lists = (Pint_list*)buf;
	buf += ed->sofas3.vlist[i].num_lists * sizeof(Pint_list);
	for ( j = 0; j < ed->sofas3.vlist[i].num_lists; j++ ) {
	    ed->sofas3.vlist[i].lists[j].num_ints = (Pint)*vi++;
	    ed->sofas3.vlist[i].lists[j].ints = (Pint *)buf;
	    buf += ed->sofas3.vlist[i].lists[j].num_ints * sizeof(Pint);
	    for ( k = 0; k < ed->sofas3.vlist[i].lists[j].num_ints; k++ )
		ed->sofas3.vlist[i].lists[j].ints[k] = *vi++;
	}
    }

    if ( ed->sofas3.eflag != PEDGE_NONE ) {
	ed->sofas3.edata = (Pedge_data_list_list *)buf;
	buf += ed->sofas3.num_sets * sizeof(Pedge_data_list_list);
	for ( i = 0; i < ed->sofas3.num_sets; i++ ) {
	    ed->sofas3.edata[i].num_lists = ed->sofas3.vlist[i].num_lists;
	    ed->sofas3.edata[i].edgelist = (Pedge_data_list *)buf;
	    buf += ed->sofas3.edata[i].num_lists * sizeof(Pedge_data_list);
	    for ( j = 0; j < ed->sofas3.edata[i].num_lists; j++ ) {
		ed->sofas3.edata[i].edgelist[j].num_edges
		    = ed->sofas3.vlist[i].lists[j].num_ints;
		ed->sofas3.edata[i].edgelist[j].edgedata.edges
		    = (Pedge_flag *)buf;
		buf += ed->sofas3.edata[i].edgelist[j].num_edges
		    * sizeof(Pedge_flag);
		for (k = 0;k < ed->sofas3.edata[i].edgelist[j].num_edges;k++ )
		    ed->sofas3.edata[i].edgelist[j].edgedata.edges[k]
			= *edge++ == PEXOn ? PEDGE_ON : PEDGE_OFF;
	    }
	}
    }
}

static void
decode_encoded_text( num_encodings, encodings, string )
    CARD16		num_encodings;	/* IN: */
    caddr_t		encodings;	/* IN: */
    char		*string;	/* IN/OUT: decoded string */
{
    int			i, size = 0;
    pexMonoEncoding	*encoding;

    /* TODO: account for other character sets. */
    for ( i = 0; i < num_encodings; i++ ) {
	encoding = (pexMonoEncoding *)encodings;
	size += encoding->numChars;
	bcopy( (char *)(encoding + 1), string, (int)encoding->numChars );
	encodings += sizeof(pexMonoEncoding) + encoding->numChars +
	    PADDING(encoding->numChars);
    }

    /* Terminate the string. */
    if ( size > 0 )
	string[size] = '\0';
}

void
phg_utx_el_data_from_pex( oc, buf, ed )
    pexElementInfo	*oc;	/* IN: PEX output command */
    caddr_t		buf;	/* IN/OUT: buffer for variable length data */
    /* WARNING: some fields have changed */Pelem_data		*ed;	/* IN/OUT: place to put converted data */
{
    unsigned		size;
    Pextmpl_colour_spec	*cspec;
    char 		*pexptr;
    Pfacet_vdata_list3	vlist;

    register	int	i, j, count;

    switch ( oc->elementType ) {
	/* All these replies just contain one CARD16. */
	case PEXOCLineBundleIndex:
	case PEXOCMarkerBundleIndex:
	case PEXOCTextBundleIndex:
	case PEXOCInteriorBundleIndex:
	case PEXOCEdgeBundleIndex:
	case PEXOCLineColourIndex:
	case PEXOCMarkerColourIndex:
	case PEXOCTextColourIndex:
	case PEXOCSurfaceColourIndex:
	case PEXOCSurfaceEdgeColourIndex:
	case PEXOCTextFontIndex:
	case PEXOCInteriorStyleIndex:
	case PEXOCViewIndex:
	case PEXOCDepthCueIndex:
	case PEXOCBfInteriorStyleIndex:
	case PEXOCColourApproxIndex:
	    ed->int_data = RHEADER(MarkerBundleIndex)->index;
	    break;

	/* All these replies just contain one INT16. */
	case PEXOCLineType:
	case PEXOCMarkerType:
	case PEXOCSurfaceEdgeType:
	case PEXOCAtextStyle:
	case PEXOCSurfaceReflModel:
	case PEXOCBfSurfaceReflModel:
	case PEXOCPolylineInterp:
	case PEXOCSurfaceInterp:
	case PEXOCBfSurfaceInterp:
	case PEXOCRenderingColourModel:
	    ed->int_data = RHEADER(LineType)->lineType;
	    break;

	case PEXOCSurfaceEdgeFlag:
	    ed->edge_flag
		= PEX_CONV_TO_Pedgef(RHEADER(SurfaceEdgeFlag)->onoff);
	    break;

	/* All these replies just contain one CARD32. */
	case PEXOCHlhsrIdentifier:
	case PEXOCLabel:
	case PEXOCExecuteStructure:
	case PEXOCPickId:
	    ed->int_data = RHEADER(Label)->label;
	    break;

	/* All these replies just contain one float. */
	case PEXOCLineWidth:
	case PEXOCMarkerScale:
	case PEXOCSurfaceEdgeWidth:
	case PEXOCCharExpansion:
	case PEXOCCharSpacing:
	case PEXOCCharHeight:
	case PEXOCAtextHeight:
	    ed->float_data = RHEADER(LineWidth)->width;
	    break;

	/* All these contain (only) an enumerated value in PHIGS but a
	 * CARD16 in PEX.  The enumeration values don't necessarily line
	 * up with the PEX constants.
	 */
	case PEXOCTextPrecision:
	    ed->text_prec = 
		PEX_CONV_TO_Ptxprec(RHEADER(TextPrecision)->precision);
	    break;
	case PEXOCTextPath:
	case PEXOCAtextPath:
	    ed->text_path = PEX_CONV_TO_Ptxpath(RHEADER(TextPath)->path);
	    break;
	case PEXOCInteriorStyle:
	case PEXOCBfInteriorStyle:
	    ed->int_style = 
		PEX_CONV_TO_Pinterstyle(RHEADER(InteriorStyle)->interiorStyle);
	    break;
	case PEXOCDistinguishFlag:
	    ed->disting_mode = 
		PEX_CONV_TO_Pdistgmode(RHEADER(DistinguishFlag)->distinguish);
	    break;
	case PEXOCCullingMode:
	    ed->cull_mode = 
		PEX_CONV_TO_Pcullmode(RHEADER(CullingMode)->cullMode);
	    break;
	case PEXOCModelClip:
	    ed->clip_ind = (RHEADER(ModelClip)->onoff == PEXClip) ?
		PIND_CLIP : PIND_NO_CLIP;
	    break;

	/* All these contain just a colour. */
	case PEXOCLineColour:
	case PEXOCMarkerColour:
	case PEXOCTextColour:
	case PEXOCSurfaceEdgeColour:
	case PEXOCSurfaceColour:
	case PEXOCBfSurfaceColour:
	    cspec = (Pextmpl_colour_spec *)&(RHEADER(MarkerColour)->colourSpec);
	    PEX_CONV_TO_Pgcolr( cspec, &ed->colr )
	    break; 

	/* There is no commonality in these. */
	case PEXOCPolyline:
	case PEXOCMarker:
	    ed->point_list3.num_points =
		VAR_LENGTH(oc->length,Polyline) / sizeof(pexCoord3D);
	    ed->point_list3.points = (Ppoint3 *)buf;
	    (void)phg_utx_ptlst3_from_pex( ed->point_list3.num_points,
		(pexCoord3D *)(RHEADER(Polyline) + 1),
		ed->point_list3.points );
	    break;

	case PEXOCPolyline2D:
	case PEXOCMarker2D:
	    ed->point_list.num_points =
		VAR_LENGTH(oc->length,Polyline2D) / sizeof(pexCoord2D);
	    ed->point_list.points = (Ppoint *)buf;
	    (void)phg_utx_ptlst_from_pex( ed->point_list.num_points,
		(pexCoord2D *)(RHEADER(Polyline2D) + 1),
		ed->point_list.points );
	    break;

	case PEXOCFillArea:
	    ed->point_list3.num_points =
		VAR_LENGTH(oc->length,FillArea) / sizeof(pexCoord3D);
	    ed->point_list3.points = (Ppoint3 *)buf;
	    (void)phg_utx_ptlst3_from_pex( ed->point_list3.num_points,
		(pexCoord3D *)(RHEADER(FillArea) + 1),
		ed->point_list3.points );
	    break;

	case PEXOCFillArea2D:
	    ed->point_list.num_points =
		VAR_LENGTH(oc->length,FillArea2D) / sizeof(pexCoord2D);
	    ed->point_list.points = (Ppoint *)buf;
	    (void)phg_utx_ptlst_from_pex( ed->point_list.num_points,
		(pexCoord2D *)(RHEADER(FillArea2D) + 1),
		ed->point_list.points );
	    break;

	case PEXOCText:
	    PEX_CONV_TO_Ppoint3(&RHEADER(Text)->origin,&ed->text3.pos)
	    PEX_CONV_TO_Pvec3(&RHEADER(Text)->vector1,&ed->text3.dir[0])
	    PEX_CONV_TO_Pvec3(&RHEADER(Text)->vector2,&ed->text3.dir[1])
	    if ( RHEADER(Text)->numEncodings > 0 ) {
		ed->text3.char_string = (char *)buf;
		decode_encoded_text( RHEADER(Text)->numEncodings,
		    (caddr_t)(RHEADER(Text) + 1), ed->text3.char_string );
	    } else
		ed->text3.char_string = (char *)NULL;
	    break;

	case PEXOCText2D:
	    PEX_CONV_TO_Ppoint(&RHEADER(Text2D)->origin,&ed->text.pos)
	    if ( RHEADER(Text2D)->numEncodings > 0 ) {
		ed->text.char_string = (char *)buf;
		decode_encoded_text( RHEADER(Text2D)->numEncodings,
		    (caddr_t)(RHEADER(Text2D) + 1), ed->text.char_string );
	    } else
		ed->text.char_string = (char *)NULL;
	    break;

	case PEXOCAnnotationText:
	    PEX_CONV_TO_Ppoint3(&RHEADER(AnnotationText)->origin,
		&ed->anno_text_rel3.ref_point)
	    PEX_CONV_TO_Pvec3(&RHEADER(AnnotationText)->offset,
		&ed->anno_text_rel3.offset)
	    if ( RHEADER(AnnotationText)->numEncodings > 0 ) {
		ed->anno_text_rel3.char_string = (char *)buf;
		decode_encoded_text( RHEADER(AnnotationText)->numEncodings,
		    (caddr_t)(RHEADER(AnnotationText) + 1),
		    ed->anno_text_rel3.char_string );
	    } else
		ed->anno_text_rel3.char_string = (char *)NULL;
	    break;

	case PEXOCAnnotationText2D:
	    PEX_CONV_TO_Ppoint(&RHEADER(AnnotationText2D)->origin,
		&ed->anno_text_rel.ref_point)
	    PEX_CONV_TO_Pvec(&RHEADER(AnnotationText2D)->offset,
		&ed->anno_text_rel.offset)
	    if ( RHEADER(AnnotationText2D)->numEncodings > 0 ) {
		ed->anno_text_rel.char_string = (char *)buf;
		decode_encoded_text( RHEADER(AnnotationText2D)->numEncodings,
		    (caddr_t)(RHEADER(AnnotationText2D) + 1),
		    ed->anno_text_rel.char_string );
	    } else
		ed->anno_text_rel.char_string = (char *)NULL;
	    break;

	case PEXOCFillAreaSet: {
	    Ppoint3	*pts3;

	    ed->point_list_list3.num_point_lists = 
		RHEADER(FillAreaSet)->numLists;
	    if ( RHEADER(FillAreaSet)->numLists > 0 ) {
		ed->point_list_list3.point_lists = (Ppoint_list3 *)buf;
		pts3 = (Ppoint3 *)(ed->point_list_list3.point_lists
		    + RHEADER(FillAreaSet)->numLists);
		pexptr = (char *)(RHEADER(FillAreaSet) + 1);
		for ( i = 0; i < RHEADER(FillAreaSet)->numLists; i++ ) {
		    count = ed->point_list_list3.point_lists[i].num_points = 
			(*(CARD32 *)pexptr);
		    ed->point_list_list3.point_lists[i].points =  pts3;
		    pexptr += sizeof(CARD32);
		    for ( j = 0; j < count; j++, pts3++ ) {
			PEX_CONV_TO_Ppoint3((pexCoord3D *)pexptr, pts3);
			pexptr += sizeof(pexCoord3D);
		    }
		}
	    }
	} break;		

	case PEXOCFillAreaSet2D: {
	    Ppoint	*pts;

	    ed->point_list_list.num_point_lists = 
		RHEADER(FillAreaSet2D)->numLists;
	    if ( RHEADER(FillAreaSet2D)->numLists > 0 ) {
		ed->point_list_list.point_lists = (Ppoint_list *)buf;
		pts = (Ppoint *)(ed->point_list_list.point_lists
		    + RHEADER(FillAreaSet2D)->numLists);
		pexptr = (char *)(RHEADER(FillAreaSet2D) + 1);
		for ( i = 0; i < RHEADER(FillAreaSet2D)->numLists; i++ ) {
		    count = ed->point_list_list.point_lists[i].num_points = 
			(Pint)(*(CARD32 *)pexptr);
		    ed->point_list_list.point_lists[i].points =  pts;
		    pexptr += sizeof(CARD32);
		    for ( j = 0; j < count; j++, pts++ ) {
			PEX_CONV_TO_Ppoint((pexCoord2D *)pexptr, pts);
			pexptr += sizeof(pexCoord2D);
		    }
		}
	    }
	} break;		
			
	case PEXOCCellArray:
	    ed->cell_array3.colr_array.dims.size_x = RHEADER(CellArray)->dx;
	    ed->cell_array3.colr_array.dims.size_y = RHEADER(CellArray)->dy;
	    PEX_CONV_TO_Ppoint3(&RHEADER(CellArray)->point1,
		&ed->cell_array3.paral.p)
	    PEX_CONV_TO_Ppoint3(&RHEADER(CellArray)->point2,
		&ed->cell_array3.paral.q)
	    PEX_CONV_TO_Ppoint3(&RHEADER(CellArray)->point3,
		&ed->cell_array3.paral.r)
	    count = ed->cell_array3.colr_array.dims.size_x * ed->cell_array3.colr_array.dims.size_y;
	    ed->cell_array3.colr_array.colr_array = (Pint *)buf;
	    pexptr = (char *)(RHEADER(CellArray) + 1);
	    for ( i = 0; i < count; i++ )
		ed->cell_array3.colr_array.colr_array[i] = ((CARD16*)pexptr)[i];
	    break;
	    
	case PEXOCCellArray2D:
	    ed->cell_array.colr_array.dims.size_x = RHEADER(CellArray2D)->dx;
	    ed->cell_array.colr_array.dims.size_y = RHEADER(CellArray2D)->dy;
	    PEX_CONV_TO_Ppoint(&RHEADER(CellArray2D)->point1,
		&ed->cell_array.rect.p)
	    PEX_CONV_TO_Ppoint(&RHEADER(CellArray2D)->point2,
		&ed->cell_array.rect.q)
	    count = ed->cell_array.colr_array.dims.size_x * ed->cell_array.colr_array.dims.size_y;
	    ed->cell_array.colr_array.colr_array = (Pint *)buf;
	    pexptr = (char *)(RHEADER(CellArray2D) + 1);
	    for ( i = 0; i < count; i++ )
		ed->cell_array.colr_array.colr_array[i] = ((CARD16*)pexptr)[i];
	    break;
	    
	case PEXOCExtCellArray:
	    ed->cell_array_plus.colr_array.dims.size_x = RHEADER(ExtCellArray)->dx;
	    ed->cell_array_plus.colr_array.dims.size_y = RHEADER(ExtCellArray)->dy;
	    ed->cell_array_plus.colr_array.type = 
		PEX_CONV_PEX_COLOUR_TYPE(RHEADER(ExtCellArray)->colourType);
	    PEX_CONV_TO_Ppoint3(&RHEADER(ExtCellArray)->point1,
		&ed->cell_array_plus.paral.p)
	    PEX_CONV_TO_Ppoint3(&RHEADER(ExtCellArray)->point2,
		&ed->cell_array_plus.paral.q)
	    PEX_CONV_TO_Ppoint3(&RHEADER(ExtCellArray)->point3,
		&ed->cell_array_plus.paral.r)

	    count = ed->cell_array_plus.colr_array.dims.size_x * ed->cell_array_plus.colr_array.dims.size_y;
	    ed->cell_array_plus.colr_array.colr_array = (Pcoval *)buf;
	    pexptr = (char *)(RHEADER(ExtCellArray) + 1);
	    size = PEX_COLOUR_SIZE(ed->cell_array_plus.colr_array.type);
	    for ( i = 0; i < count; i++ ) {
		PEX_CONV_TO_Pcoval(ed->cell_array_plus.colr_array.type,
		    pexptr, &ed->cell_array_plus.colr_array.colr_array[i]);
		pexptr += size;
	    }
	    break;
	    
	case PEXOCPolylineSet:
	    ed->plsd3.vflag = 
		PEX_CONV_TO_VertexFlag(RHEADER(PolylineSet)->vertexAttribs);
	    ed->plsd3.colr_model = 
		PEX_CONV_PEX_COLOUR_TYPE(RHEADER(PolylineSet)->colourType);
	    ed->plsd3.npl = RHEADER(PolylineSet)->numLists;
	    if ( RHEADER(PolylineSet)->numLists > 0 ) {
		ed->plsd3.vdata = (Pline_vdata_list3 *)buf;
		pexptr = (char *)(RHEADER(PolylineSet) + 1);
		(void)phg_utx_vdata_from_pex( ed->plsd3.npl, !0,
		    ed->plsd3.vflag, (Pfacet_vdata_list3*)ed->plsd3.vdata,
		    PEDGE_NONE, (Pedge_data_list *)NULL,
		    ed->plsd3.colr_model, pexptr );
	    }
	    break;

	case PEXOCExtFillAreaSet: {
	    ed->fasd3.fflag = 
		PEX_CONV_TO_FacetFlag(RHEADER(ExtFillAreaSet)->facetAttribs);
	    ed->fasd3.eflag =
		RHEADER(ExtFillAreaSet)->vertexAttribs & PEXGAEdges
		    ? PEDGE_VISIBILITY : PEDGE_NONE;
	    ed->fasd3.vflag = PEX_CONV_TO_VertexFlag(
		RHEADER(ExtFillAreaSet)->vertexAttribs &~PEXGAEdges);
	    ed->fasd3.colr_model = 
		PEX_CONV_PEX_COLOUR_TYPE(RHEADER(ExtFillAreaSet)->colourType);
	    ed->fasd3.nfa =
		RHEADER(ExtFillAreaSet)->numLists;

	    pexptr = (char *)(RHEADER(ExtFillAreaSet) + 1);
	    if ( ed->fasd3.fflag != PFACET_NONE ) {
		Pfacet_data_arr3   fdata;
		/* Dummy up fdata to pass to the conversion utility. */
		fdata.colrs = &ed->fasd3.fdata.colr;
		pexptr = fdata_from_pex( ed->fasd3.fflag, 1,
		    ed->fasd3.colr_model, pexptr, &fdata ); 
	    }

	    ed->fasd3.vdata = (Pfacet_vdata_list3 *)buf;
	    if (ed->fasd3.eflag == PEDGE_NONE )
		ed->fasd3.edata = (Pedge_data_list *)NULL;
	    else {
		int	vdata_size;
		count = NUM_VERTS_IN_PEX_FAREA_SET(
		    RHEADER(ExtFillAreaSet)->colourType,
		    RHEADER(ExtFillAreaSet)->vertexAttribs,
		    RHEADER(ExtFillAreaSet)->facetAttribs,
		    VAR_LENGTH(oc->length,ExtFillAreaSet),
		    ed->fasd3.nfa);
		vdata_size = count *
		    PHIGS_VERTEX_SIZE(RHEADER(ExtFillAreaSet)->vertexAttribs);
		ed->fasd3.edata = (Pedge_data_list *)(buf + vdata_size
		    + ed->fasd3.nfa * sizeof(Pfacet_vdata_list3));
	    }
	    (void)phg_utx_vdata_from_pex( ed->fasd3.nfa, !0,
		ed->fasd3.vflag, ed->fasd3.vdata,
		ed->fasd3.eflag, ed->fasd3.edata,
		ed->fasd3.colr_model, pexptr);
	} break;
	    
	case PEXOCTriangleStrip:
	    ed->tsd3.fflag = 
		PEX_CONV_TO_FacetFlag(RHEADER(TriangleStrip)->facetAttribs);
	    ed->tsd3.vflag = 
		PEX_CONV_TO_VertexFlag(RHEADER(TriangleStrip)->vertexAttribs);
	    ed->tsd3.colr_model = 
		PEX_CONV_PEX_COLOUR_TYPE(RHEADER(TriangleStrip)->colourType);
	    ed->tsd3.nv = RHEADER(TriangleStrip)->numVertices;

	    pexptr = (char *)(RHEADER(TriangleStrip) + 1);
	    if ( ed->tsd3.fflag != PFACET_NONE ) {
		int	num_facets, fdata_size;

		ed->tsd3.fdata.colrs = (Pcoval *)buf;
		num_facets = (RHEADER(TriangleStrip)->numVertices - 2);
		fdata_size = num_facets
		    * PHIGS_FACET_SIZE(RHEADER(TriangleStrip)->facetAttribs);
		ed->tsd3.vdata.points = (Ppoint3 *)(buf + fdata_size);
		pexptr = fdata_from_pex( ed->tsd3.fflag, num_facets,
		    ed->tsd3.colr_model, pexptr, &ed->tsd3.fdata );
	    } else
		ed->tsd3.vdata.points = (Ppoint3 *)buf;
	    vlist.num_vertices = ed->tsd3.nv;
	    vlist.vertex_data.points = ed->tsd3.vdata.points;
	    (void)phg_utx_vdata_from_pex( 1, 0, ed->tsd3.vflag,
		&vlist, PEDGE_NONE, (Pedge_data_list *)NULL,
		ed->tsd3.colr_model, pexptr );
	break;
		    
	case PEXOCQuadrilateralMesh:
	    ed->qmd3.fflag = PEX_CONV_TO_FacetFlag(
		RHEADER(QuadrilateralMesh)->facetAttribs);
	    ed->qmd3.vflag = PEX_CONV_TO_VertexFlag(
		RHEADER(QuadrilateralMesh)->vertexAttribs);
	    ed->qmd3.colr_model = PEX_CONV_PEX_COLOUR_TYPE(
		RHEADER(QuadrilateralMesh)->colourType);
	    ed->qmd3.dim.size_x = RHEADER(QuadrilateralMesh)->mPts;
	    ed->qmd3.dim.size_y = RHEADER(QuadrilateralMesh)->nPts;

	    pexptr = (char *)(RHEADER(QuadrilateralMesh) + 1);
	    if ( ed->qmd3.fflag != PFACET_NONE ) {
		int	num_facets, fdata_size;

		ed->qmd3.fdata.colrs = (Pcoval *)buf;
		num_facets =
		    (ed->qmd3.dim.size_x - 1) * (ed->qmd3.dim.size_y - 1);
		fdata_size = num_facets *
		    PHIGS_FACET_SIZE(RHEADER(QuadrilateralMesh)->facetAttribs);
		ed->qmd3.vdata.points =
		    (Ppoint3 *)(buf + fdata_size);
		pexptr = fdata_from_pex( ed->qmd3.fflag, num_facets,
		    ed->qmd3.colr_model, pexptr, &ed->qmd3.fdata );
	    } else
		ed->qmd3.vdata.points = (Ppoint3 *)buf;
	    vlist.num_vertices = ed->qmd3.dim.size_x * ed->qmd3.dim.size_y;
	    vlist.vertex_data.points = ed->qmd3.vdata.points;
	    (void)phg_utx_vdata_from_pex( 1, 0, ed->qmd3.vflag,
		&vlist, PEDGE_NONE, (Pedge_data_list *)NULL,
		ed->qmd3.colr_model, pexptr );
	    break;

	case PEXOCSOFAS:
	    ed->sofas3.fflag =
		PEX_CONV_TO_FacetFlag( RHEADER(SOFAS)->FAS_Attributes);
	    ed->sofas3.eflag = RHEADER(SOFAS)->edgeAttributes ?
		PEDGE_VISIBILITY : PEDGE_NONE;
	    ed->sofas3.vflag =
		PEX_CONV_TO_VertexFlag( RHEADER(SOFAS)->vertexAttributes);
	    ed->sofas3.colr_model =
		PEX_CONV_PEX_COLOUR_TYPE( RHEADER(SOFAS)->colourType);
	    ed->sofas3.num_sets = RHEADER(SOFAS)->numFAS;
	    ed->sofas3.vdata.num_vertices = RHEADER(SOFAS)->numVertices;

	    pexptr = (char *)(RHEADER(SOFAS) + 1);
	    (void)sofa_data_from_pex( pexptr, RHEADER(SOFAS),ed,buf);
	  break;
		    
	case PEXOCNurbCurve:
	    ed->nurb_curve.order = RHEADER(NurbCurve)->curveOrder;
	    ed->nurb_curve.rationality = 
		PEX_CONV_TO_Prational(RHEADER(NurbCurve)->coordType);
	    ed->nurb_curve.min = (Pfloat)RHEADER(NurbCurve)->tmin;
	    ed->nurb_curve.max = (Pfloat)RHEADER(NurbCurve)->tmax;
	    ed->nurb_curve.knots.num_floats = RHEADER(NurbCurve)->numKnots;
	    ed->nurb_curve.cpts.num_points = RHEADER(NurbCurve)->numPoints;

	    pexptr = (char *)(RHEADER(NurbCurve) + 1);
	    ed->nurb_curve.knots.floats = (Pfloat *)buf;
	    ed->nurb_curve.cpts.points.point4d = (Ppoint4 *)(buf + 
		ed->nurb_curve.knots.num_floats * sizeof(Pfloat));

	    for ( i = 0; i < RHEADER(NurbCurve)->numKnots; i++ ) {
		ed->nurb_curve.knots.floats[i] = *(Pfloat *)pexptr;
		pexptr += sizeof(PEXFLOAT);
	    }

	    if (ed->nurb_curve.rationality == PRATIONAL) {
		for ( i = 0; i < RHEADER(NurbCurve)->numPoints; i++ ) {
		    PEX_CONV_TO_Ppoint4((pexCoord4D *)pexptr, 
			&ed->nurb_curve.cpts.points.point4d[i]);
		    pexptr += sizeof(pexCoord4D);
		}
	    } else {
		for ( i = 0; i < RHEADER(NurbCurve)->numPoints; i++ ) {
		    PEX_CONV_TO_Ppoint3((pexCoord3D *)pexptr,
			&ed->nurb_curve.cpts.points.point3d[i]);
		    pexptr += sizeof(pexCoord3D);
		}
	    }
	    break;
	
	case PEXOCNurbSurface:
	    ed->nurb_surf.u_order = RHEADER(NurbSurface)->uOrder;
	    ed->nurb_surf.v_order = RHEADER(NurbSurface)->vOrder;
	    ed->nurb_surf.rationality =
		PEX_CONV_TO_Prational(RHEADER(NurbSurface)->type);
	    ed->nurb_surf.uknots.num_floats =RHEADER(NurbSurface)->numUknots;
	    ed->nurb_surf.vknots.num_floats =RHEADER(NurbSurface)->numVknots;
	    ed->nurb_surf.grid.num_points.u_dim = RHEADER(NurbSurface)->mPts;
	    ed->nurb_surf.grid.num_points.v_dim = RHEADER(NurbSurface)->nPts;
	    ed->nurb_surf.num_trim_loops = RHEADER(NurbSurface)->numLists;

	    pexptr = (char *)(RHEADER(NurbSurface) + 1);
	    ed->nurb_surf.uknots.floats = (Pfloat *)buf;
	    ed->nurb_surf.vknots.floats = ed->nurb_surf.uknots.floats
		+ ed->nurb_surf.uknots.num_floats;
	    buf = (char *)(ed->nurb_surf.vknots.floats
		+ ed->nurb_surf.vknots.num_floats);

	    for ( i = 0; i < ed->nurb_surf.uknots.num_floats; i++ ) {
		ed->nurb_surf.uknots.floats[i] = *((Pfloat *)pexptr);
		pexptr += sizeof(PEXFLOAT);
	    }
	    for (i = 0; i < ed->nurb_surf.vknots.num_floats; i++) {
		ed->nurb_surf.vknots.floats[i] = *((Pfloat *)pexptr);
		pexptr += sizeof(PEXFLOAT); 
	    }

	    count = ed->nurb_surf.grid.num_points.u_dim *
		ed->nurb_surf.grid.num_points.v_dim;
	    if (ed->nurb_surf.rationality == PRATIONAL) {
		ed->nurb_surf.grid.points.point4d = (Ppoint4 *)buf;
		buf = (char *)(ed->nurb_surf.grid.points.point4d
		    + count);
		for ( i = 0; i < count; i++ ) {
		    PEX_CONV_TO_Ppoint4((pexCoord4D *)pexptr,
			&ed->nurb_surf.grid.points.point4d[i]);
		    pexptr += sizeof(pexCoord4D);
		}
	    } else {
		ed->nurb_surf.grid.points.point3d = (Ppoint3 *)buf;
		buf = (char *)(ed->nurb_surf.grid.points.point3d
		    + count);
		for ( i = 0; i < count; i++ ) {
		    PEX_CONV_TO_Ppoint3((pexCoord3D *)pexptr,
			&ed->nurb_surf.grid.points.point3d[i]);
		    pexptr += sizeof(pexCoord3D);
		}
	    }

	    if ( RHEADER(NurbSurface)->numLists > 0 )
		trim_curves_from_pex( RHEADER(NurbSurface), buf, ed );
	    break;

	case PEXOCGdp:
	    ed->gdp3.id = RHEADER(Gdp)->gdpId;
	    ed->gdp3.point_list.num_points = RHEADER(Gdp)->numPoints;
	    pexptr = (char *)(RHEADER(Gdp) + 1);
	    if ( RHEADER(Gdp)->numPoints > 0 ) {
		ed->gdp3.point_list.points = (Ppoint3 *)buf;
		for ( i = 0; i < RHEADER(Gdp)->numPoints; i++ ) {
		    PEX_CONV_TO_Ppoint3((pexCoord3D *)pexptr,
			&ed->gdp3.point_list.points[i]);
		    pexptr += sizeof(pexCoord3D);
		}
		buf += RHEADER(Gdp)->numPoints * sizeof(Ppoint3);
	    }
	    ed->gdp3.data.unsupp.size = RHEADER(Gdp)->numBytes;
	    ed->gdp.data.unsupp.data = buf;
	    if ( RHEADER(Gdp)->numBytes > 0 )
		bcopy( pexptr, ed->gdp3.data.unsupp.data,
		    ed->gdp3.data.unsupp.size );
	    break;

	case PEXOCGdp2D:
	    ed->gdp.id = RHEADER(Gdp2D)->gdpId;
	    ed->gdp.point_list.num_points = RHEADER(Gdp2D)->numPoints;
	    pexptr = (char *)(RHEADER(Gdp2D) + 1);
	    if ( RHEADER(Gdp2D)->numPoints > 0 ) {
		ed->gdp.point_list.points = (Ppoint *)buf;
		for ( i = 0; i < RHEADER(Gdp2D)->numPoints; i++ ) {
		    PEX_CONV_TO_Ppoint((pexCoord2D *)pexptr,
			&ed->gdp.point_list.points[i]);
		    pexptr += sizeof(pexCoord2D);
		}
		buf += RHEADER(Gdp2D)->numPoints * sizeof(Ppoint);
	    }
	    ed->gdp.data.unsupp.size = RHEADER(Gdp2D)->numBytes;
	    ed->gdp.data.unsupp.data = buf;
	    if ( RHEADER(Gdp2D)->numBytes > 0 )
		bcopy( pexptr, ed->gdp.data.unsupp.data,
		    ed->gdp.data.unsupp.size );
	    break;
	    
	case PEXOCTextAlignment:
	case PEXOCAtextAlignment:
	    ed->text_align.vert =
		PEX_CONV_TO_Ptxver(RHEADER(TextAlignment)->alignment.vertical);
	    ed->text_align.hor = PEX_CONV_TO_Ptxhor(
		RHEADER(TextAlignment)->alignment.horizontal);
	    break;
	case PEXOCCharUpVector:
	case PEXOCAtextUpVector:
	    PEX_CONV_TO_Pvec(&RHEADER(CharUpVector)->up, &ed->char_up_vec)
	    break;

	case PEXOCPatternSize:
	    PEX_CONV_TO_Pfloat_size(&RHEADER(PatternSize)->size,
		&ed->pat_size)
	    break;

	case PEXOCPatternAttr:
	    PEX_CONV_TO_Ppoint3(&RHEADER(PatternAttr)->refPt,
		&ed->pat_ref_point_vecs.ref_point)
	    PEX_CONV_TO_Pvec3(&RHEADER(PatternAttr)->vector1,
		&ed->pat_ref_point_vecs.ref_vec[0])
	    PEX_CONV_TO_Pvec3(&RHEADER(PatternAttr)->vector2,
		&ed->pat_ref_point_vecs.ref_vec[1])
	    break;

	case PEXOCPatternRefPt:
	    PEX_CONV_TO_Ppoint(&RHEADER(PatternRefPt)->point,
		&ed->pat_ref_point)
	    break;

	case PEXOCAddToNameSet:
	case PEXOCRemoveFromNameSet:
	    ed->names.num_ints =
		VAR_LENGTH(oc->length,AddToNameSet) / sizeof(pexName);
	    ed->names.ints = (Pint *)buf;
	    pexptr = (char *)(RHEADER(AddToNameSet) + 1);
	    for ( i = 0; i < ed->names.num_ints; i++ ) {
		ed->names.ints[i] = *((CARD32*)pexptr);
		pexptr += sizeof(CARD32);
	    }
	    break;

	case PEXOCSetAsfValues:
	    PEX_CONV_TO_Pattrid(RHEADER(SetAsfValues)->attribute,
		ed->asf.id)
	    ed->asf.source = PEX_CONV_TO_Pasf(RHEADER(SetAsfValues)->source);
	    break;

	case PEXOCLocalTransform:
	    ed->local_tran3.compose_type =
		PEX_CONV_TO_Pcomptype(RHEADER(LocalTransform)->compType);
	    for ( i = 0; i < 4; i++ ) for ( j = 0; j < 4; j++ )
		ed->local_tran3.matrix[i][j] =
		    RHEADER(LocalTransform)->matrix[i][j];
	    break;

	case PEXOCLocalTransform2D:
	    ed->local_tran.compose_type =
		PEX_CONV_TO_Pcomptype(RHEADER(LocalTransform2D)->compType);
	    for ( i = 0; i < 3; i++ ) for ( j = 0; j < 3; j++ )
		ed->local_tran.matrix[i][j] =
		    RHEADER(LocalTransform2D)->matrix3X3[i][j];
	    break;

	case PEXOCGlobalTransform:
	    for ( i = 0; i < 4; i++ ) for ( j = 0; j < 4; j++ )
		ed->global_tran3[i][j]
		    = RHEADER(GlobalTransform)->matrix[i][j];
	    break;

	case PEXOCGlobalTransform2D:
	    for ( i = 0; i < 3; i++ ) for ( j = 0; j < 3; j++ )
		ed->global_tran[i][j]
		    = RHEADER(GlobalTransform2D)->matrix3X3[i][j];
	    break;

	case PEXOCModelClipVolume:
	    ed->model_clip3.op = RHEADER(ModelClipVolume)->modelClipOperator;
	    ed->model_clip3.half_spaces.num_half_spaces =
		RHEADER(ModelClipVolume)->numHalfSpaces;
	    pexptr = (char *)(RHEADER(ModelClipVolume) + 1);
	    ed->model_clip3.half_spaces.half_spaces = (Phalf_space3 *)buf;
	    for (i = 0; i < ed->model_clip3.half_spaces.num_half_spaces; i++) {
		PEX_CONV_TO_Ppoint3((pexCoord3D*)pexptr,
		    &ed->model_clip3.half_spaces.half_spaces[i].point)
		pexptr += sizeof(pexCoord3D);
		PEX_CONV_TO_Pvec3((pexVector3D*)pexptr,
		    &ed->model_clip3.half_spaces.half_spaces[i].norm)
		pexptr += sizeof(pexVector3D);
	    }
	    break;

	case PEXOCModelClipVolume2D:
	    ed->model_clip.op = RHEADER(ModelClipVolume2D)->modelClipOperator;
	    ed->model_clip.half_spaces.num_half_spaces =
		RHEADER(ModelClipVolume2D)->numHalfSpaces;
	    pexptr = (char *)(RHEADER(ModelClipVolume2D) + 1);
	    ed->model_clip.half_spaces.half_spaces = (Phalf_space *)buf;
	    for (i = 0; i < ed->model_clip.half_spaces.num_half_spaces; i++) {
		PEX_CONV_TO_Ppoint((pexCoord2D*)pexptr,
		    &ed->model_clip.half_spaces.half_spaces[i].point)
		pexptr += sizeof(pexCoord2D);
		PEX_CONV_TO_Pvec((pexVector2D*)pexptr,
		    &ed->model_clip.half_spaces.half_spaces[i].norm)
		pexptr += sizeof(pexVector2D);
	    }
	    break;

	case PEXOCSurfaceReflAttr:
	case PEXOCBfSurfaceReflAttr:
	    ed->props.ambient_coef =
		RHEADER(SurfaceReflAttr)->reflectionAttr.ambient;
	    ed->props.diffuse_coef =
		RHEADER(SurfaceReflAttr)->reflectionAttr.diffuse;
	    ed->props.specular_coef =
		RHEADER(SurfaceReflAttr)->reflectionAttr.specular;
	    ed->props.specular_exp =
		RHEADER(SurfaceReflAttr)->reflectionAttr.specularConc;
	    cspec = (Pextmpl_colour_spec *)
		&RHEADER(SurfaceReflAttr)->reflectionAttr.specularColour;
	    PEX_CONV_TO_Pgcolr( cspec, &ed->props.specular_colr )
	    break;

	case PEXOCCurveApproximation:
	    ed->curv_approx.type =
		RHEADER(CurveApproximation)->approx.approxMethod;
	    ed->curv_approx.value =
		RHEADER(CurveApproximation)->approx.tolerance;
	    break;

	case PEXOCSurfaceApproximation:
	    ed->surf_approx.type =
		RHEADER(SurfaceApproximation)->approx.approxMethod;
	    ed->surf_approx.u_val =
		RHEADER(SurfaceApproximation)->approx.uTolerance;
	    ed->surf_approx.v_val =
		RHEADER(SurfaceApproximation)->approx.vTolerance;
	    break;

	case PEXOCLightState:
	    ed->lss.activation.num_ints = RHEADER(LightState)->numEnable;
	    ed->lss.deactivation.num_ints = RHEADER(LightState)->numDisable;
	    ed->lss.activation.ints = (Pint *)buf;
	    ed->lss.deactivation.ints = ed->lss.activation.ints
		+ ed->lss.activation.num_ints;
	    pexptr = (char *)(RHEADER(LightState) + 1);
	    for ( i = 0; i < ed->lss.activation.num_ints; i++ ) {
		ed->lss.activation.ints[i] = *((pexTableIndex *)pexptr);
		pexptr += sizeof(pexTableIndex);
	    }
	    pexptr += PADDING(sizeof(pexTableIndex)
		* ed->lss.activation.num_ints);
	    for ( i = 0; i < ed->lss.deactivation.num_ints; i++ ) {
		ed->lss.deactivation.ints[i] = *((pexTableIndex *)pexptr);
		pexptr += sizeof(pexTableIndex);
	    }
	    break;

	case PEXOCApplicationData:
	    ed->appl_data.size = RHEADER(ApplicationData)->numElements;
	    ed->appl_data.data = buf;
	    pexptr = (char *)(RHEADER(ApplicationData) + 1);
	    bcopy( pexptr, (char *)ed->appl_data.data, ed->appl_data.size );
	    break;

	case PEXOCGse:
	    ed->gse.id = RHEADER(Gse)->id;
	    ed->gse.data.unsupp.size = RHEADER(Gse)->numElements;
	    if ( RHEADER(Gse)->numElements > 0 ) {
		ed->gse.data.unsupp.data = buf;
		pexptr = (char *)(RHEADER(Gse) + 1);
		bcopy( pexptr, ed->gse.data.unsupp.data,
		    ed->gse.data.unsupp.size );
	    }
	    break;

	case PEXOCParaSurfCharacteristics: 
	    ed->para_surf_characs.type =
		RHEADER(ParaSurfCharacteristics)->characteristics;
	    pexptr = (char *)(RHEADER(ParaSurfCharacteristics) + 1);
	    switch ( ed->para_surf_characs.type ) {
		case PSC_NONE:
		case PSC_WS_DEP:
		    /* No data */
		    break;
		case PSC_ISOPARAMETRIC_CURVES: {
		    pexPSC_IsoparametricCurves	*drec;

		    drec = (pexPSC_IsoparametricCurves *)
			(RHEADER(ParaSurfCharacteristics) + 1);
		    switch ( drec->placementType ) {
			default:
			case PEXICUniformPlacement:
			    ed->para_surf_characs.data.psc_3.placement =
				PCP_UNIFORM;
			    break;
			case PEXICNonuniformPlacement:
			    ed->para_surf_characs.data.psc_3.placement =
				PCP_NON_UNIFORM;
			    break;
		    }
		    ed->para_surf_characs.data.psc_3.u_count = drec->numUcurves;
		    ed->para_surf_characs.data.psc_3.u_count = drec->numVcurves;
		} break;
		case PSC_LEVEL_CURVES_MC:
		case PSC_LEVEL_CURVES_WC: {
		    pexPSC_LevelCurves	*drec;
		    PEXFLOAT		*params;

		    drec = (pexPSC_LevelCurves *)
			(RHEADER(ParaSurfCharacteristics) + 1);
		    PEX_CONV_TO_Ppoint3(&drec->origin,
			&ed->para_surf_characs.data.psc_4.origin)
		    PEX_CONV_TO_Pvec3(&drec->direction,
			&ed->para_surf_characs.data.psc_4.direction)
		    ed->para_surf_characs.data.psc_4.params.num_floats =
			drec->numberIntersections;
		    ed->para_surf_characs.data.psc_4.params.floats =
			(Pfloat *)buf;
		    params = (PEXFLOAT *)(drec + 1);
		    for ( i = 0; i < drec->numberIntersections; i ++ )
			ed->para_surf_characs.data.psc_4.params.floats[i] =
			    params[i];
		} break;
	    }
	    break;
    }
}


#if (__STDC__ && !defined(UNIXCPP)) || defined(ANSICPP)
#define HEADER(_n) ((pex##_n *)pex_oc->oc)
#else
#define HEADER(_n) ((pex/**/_n *)pex_oc->oc)
#endif

#if (__STDC__ && !defined(UNIXCPP)) || defined(ANSICPP)
#define TYPE_AND_SIZE(_n) oc_type = PEXOC##_n; hdr_size = sizeof(pex##_n);
#else
#define TYPE_AND_SIZE(_n) oc_type = PEXOC/**/_n; hdr_size = sizeof(pex/**/_n);
#endif

int
phg_utx_encode_text( length, string, encoding )
    Pint		length;
    char		*string;
    pexMonoEncoding	*encoding;
{
    int		num_encodings;

    /* TODO: More than one encoding. */
    num_encodings = 1;
    encoding->characterSet = 0;
    encoding->characterSetWidth = PEXCSByte;
    encoding->encodingState = 0;
    encoding->numChars = (length > 1 ? length - 1 : length);
    if ( encoding->numChars > 0 )
	bcopy( (char *)string, (char *)(encoding + 1),
	    (int)encoding->numChars );
    return num_encodings;
}


static char*
sofa_edges_to_pex( num_sets, num_edges, edata, buf )
    Pint			num_sets;
    int				num_edges;
    Pedge_data_list_list	*edata;
    char			*buf;
{
    register	int	i, j, k;
    register	CARD8	*flags = (CARD8 *)buf;

    for ( i = 0; i < num_sets; i++ )
	for ( j = 0; j < edata[i].num_lists; j++ )
	    for ( k = 0; k < edata[i].edgelist[j].num_edges; k++, flags++ )
		*flags = edata[i].edgelist[j].edgedata.edges[k]
		    == PEDGE_ON ? (CARD8)1 : (CARD8)0;
    flags += PADDING(num_edges);
    return (char *)flags;
}


static char*
sofa_connectivity_to_pex( num_sets, num_contours, num_edges, conn, buf )
    Pint		num_sets;
    int			num_contours;
    int			num_edges;
    Pint_list_list	*conn;
    char		*buf;
{
    register	int	i, j, k;
    register	CARD16	*ip = (CARD16 *)buf;

    for ( i = 0; i < num_sets; i++ ) {
	*ip++ = (CARD16)conn[i].num_lists;
	for ( j = 0; j < conn[i].num_lists; j++ ) {
	    *ip++ = (CARD16)conn[i].lists[j].num_ints;
	    for ( k = 0; k < conn[i].lists[j].num_ints; k++, ip++ )
		*ip = (CARD16)conn[i].lists[j].ints[k];
	}
    }

    /* Add padding. */
    if ( (num_sets + num_edges +num_contours) % 2 != 0 )  ++ip;
    return (char *)ip;
}


int
phg_utx_build_pex_oc( erh, el_type, ed, scratch, pex_oc )
    Err_handle		erh;
    Pelem_type		el_type;
    Phg_el_data		*ed;
    Phg_scratch		*scratch;
    Phg_pex_oc		*pex_oc;
{
    int			i, count;
    int			hdr_size = 0, data_size = 0, pad = 0;
    CARD16		oc_type;
    char		*buf;
    CARD32		*bufp32;
    pexTableIndex	*bufptblind;
    PEXFLOAT		*bufpfloat;
    Pextmpl_colour_spec	*cspec;

    /* Fill in the header and calculate the length. */
    switch ( el_type ) {
	case PELEM_POLYLINE3:
	    TYPE_AND_SIZE(Polyline)
	    data_size = ed->ptlst3.num_points * sizeof(pexCoord3D);
	    break;
	case PELEM_POLYLINE:
	    TYPE_AND_SIZE(Polyline2D)
	    data_size = ed->ptlst.num_points * sizeof(pexCoord2D);
	    break;
	case PELEM_POLYMARKER3:
	    TYPE_AND_SIZE(Marker)
	    data_size = ed->ptlst3.num_points * sizeof(pexCoord3D);
	    break;
	case PELEM_POLYMARKER:
	    TYPE_AND_SIZE(Marker2D)
	    data_size = ed->ptlst.num_points * sizeof(pexCoord2D);
	    break;
	case PELEM_TEXT3:
	    TYPE_AND_SIZE(Text)
	    data_size = ed->text3.length > 1
		? ed->text3.length - 1 : ed->text3.length;
	    data_size += sizeof(pexMonoEncoding);
	    pad = PADDING(data_size);
	    break;
	case PELEM_TEXT:
	    TYPE_AND_SIZE(Text2D)
	    data_size = ed->text.length > 1
		? ed->text.length - 1 : ed->text.length;
	    data_size += sizeof(pexMonoEncoding);
	    pad = PADDING(data_size);
	    break;
	case PELEM_ANNO_TEXT_REL3:
	    TYPE_AND_SIZE(AnnotationText)
	    data_size = ed->anno_text_rel3.length > 1
		? ed->anno_text_rel3.length - 1 : ed->anno_text_rel3.length;
	    data_size += sizeof(pexMonoEncoding);
	    pad = PADDING(data_size);
	    break;
	case PELEM_ANNO_TEXT_REL:
	    TYPE_AND_SIZE(AnnotationText2D)
	    data_size = ed->anno_text_rel.length > 1
		? ed->anno_text_rel.length - 1 : ed->anno_text_rel.length;
	    data_size += sizeof(pexMonoEncoding);
	    pad = PADDING(data_size);
	    break;
	case PELEM_FILL_AREA3:
	    TYPE_AND_SIZE(FillArea)
	    data_size = ed->ptlst3.num_points * sizeof(pexCoord3D);
	    break;
	case PELEM_FILL_AREA:
	    TYPE_AND_SIZE(FillArea2D)
	    data_size = ed->ptlst.num_points * sizeof(pexCoord2D);
	    break;
	case PELEM_FILL_AREA_SET3:
	    TYPE_AND_SIZE(FillAreaSet)
	    data_size = ed->fa_set3.num_sets * sizeof(CARD32);
	    data_size += ed->fa_set3.total_pts * sizeof(pexCoord3D);
	    break;
	case PELEM_FILL_AREA_SET:
	    TYPE_AND_SIZE(FillAreaSet2D)
	    data_size = ed->fa_set.num_sets * sizeof(CARD32);
	    data_size += ed->fa_set.total_pts * sizeof(pexCoord2D);
	    break;
	case PELEM_CELL_ARRAY3:
	    TYPE_AND_SIZE(CellArray);
	    data_size = ed->cell_array3.dim.size_x *
		ed->cell_array3.dim.size_y * sizeof(pexTableIndex);
	    pad = PADDING(data_size);
	    break;
	case PELEM_CELL_ARRAY:
	    TYPE_AND_SIZE(CellArray2D);
	    data_size = ed->cell_array.dim.size_x *
		ed->cell_array.dim.size_y * sizeof(pexTableIndex);
	    pad = PADDING(data_size);
	    break;
	case PELEM_POLYLINE_SET3_DATA:
	    TYPE_AND_SIZE(PolylineSet)
	    data_size = ed->pl_set3_d.num_sets * sizeof(CARD32);
	    data_size += phg_utx_vdata_size( ed->pl_set3_d.vflag,
		ed->pl_set3_d.num_vertices, ed->pl_set3_d.colour_model );
	    break;
	case PELEM_FILL_AREA_SET3_DATA:
	    data_size = ed->fa_set3_d.num_sets * sizeof(CARD32);
	    data_size += fdata_size( ed->fa_set3_d.fflag, 1,
		ed->fa_set3_d.colour_model );
	    data_size += phg_utx_vdata_size( ed->fa_set3_d.vflag,
		ed->fa_set3_d.num_vertices, ed->fa_set3_d.colour_model );
	    TYPE_AND_SIZE(ExtFillAreaSet)
	    if ( ed->fa_set3_d.eflag != PEDGE_NONE )
		data_size += ed->fa_set3_d.num_vertices * sizeof(CARD32);
	    break;
	case PELEM_TRI_STRIP3_DATA:
	    TYPE_AND_SIZE(TriangleStrip)
	    data_size = fdata_size( ed->tri_strip3.fflag,
		ed->tri_strip3.fdata.num_facets, ed->tri_strip3.colour_model );
	    data_size += phg_utx_vdata_size( ed->tri_strip3.vflag,
		ed->tri_strip3.vdata.num_vertices, ed->tri_strip3.colour_model );
	    break;
	case PELEM_QUAD_MESH3_DATA:
	    TYPE_AND_SIZE(QuadrilateralMesh)
	    data_size = fdata_size( ed->quad_mesh3.fflag,
		ed->quad_mesh3.fdata.num_facets, ed->quad_mesh3.colour_model );
	    data_size += phg_utx_vdata_size( ed->quad_mesh3.vflag,
		ed->quad_mesh3.vdata.num_vertices, ed->quad_mesh3.colour_model);
	    break;
	case PELEM_SET_OF_FILL_AREA_SET3_DATA:
	    TYPE_AND_SIZE(SOFAS)
	    data_size = fdata_size( ed->sofas3.fflag,
		ed->sofas3.fdata.num_facets, ed->sofas3.colour_model );
	    data_size += phg_utx_vdata_size( ed->sofas3.vflag,
		ed->sofas3.vdata.num_vertices, ed->sofas3.colour_model);
	    data_size += ed->sofas3.num_sets * sizeof(CARD16)
		+ ed->sofas3.num_contours * sizeof(CARD16)
		+ ed->sofas3.num_vindices * sizeof(CARD16);
	    pad = PADDING(ed->sofas3.num_sets * sizeof(CARD16)
		+ ed->sofas3.num_contours * sizeof(CARD16)
		+ ed->sofas3.num_vindices * sizeof(CARD16));
	    if ( ed->sofas3.eflag != PEDGE_NONE ) {
		data_size += ed->sofas3.num_vindices * sizeof(CARD8);
		pad += PADDING(ed->sofas3.num_vindices * sizeof(CARD8));
	    }
	    break;
	case PELEM_NUNI_BSP_CURVE:
	    TYPE_AND_SIZE(NurbCurve)
	    data_size = ed->nurb_curve.data.knots.num_floats * sizeof(PEXFLOAT);
	    data_size += ed->nurb_curve.data.npts *
		(ed->nurb_curve.data.rationality == PRATIONAL ?
		    sizeof(pexCoord4D) : sizeof(pexCoord3D));
	    break;
	case PELEM_NUNI_BSP_SURF:
	    TYPE_AND_SIZE(NurbSurface)
	    data_size  = ed->nurb_surf.data.uknots.num_floats * sizeof(PEXFLOAT);
	    data_size += ed->nurb_surf.data.vknots.num_floats * sizeof(PEXFLOAT);
	    count = ed->nurb_surf.data.npts.u_dim
		* ed->nurb_surf.data.npts.v_dim;
	    data_size += count *
		(ed->nurb_surf.data.rationality == PRATIONAL ?
		    sizeof(pexCoord4D) : sizeof(pexCoord3D));
	    if ( ed->nurb_surf.data.nloops > 0 ) {
		data_size += ed->nurb_surf.data.nloops * sizeof(CARD32);
		data_size += ed->nurb_surf.data.num_tcurves *
		    sizeof(pexTrimCurve);
		data_size += ed->nurb_surf.data.num_tknots * sizeof(PEXFLOAT);
		data_size += ed->nurb_surf.data.num_3D_tpoints
		    * sizeof(pexCoord3D);
		data_size += ed->nurb_surf.data.num_2D_tpoints
		    * sizeof(pexCoord2D);
	    }
	    break;
	    
	case PELEM_CELL_ARRAY3_PLUS:
	    TYPE_AND_SIZE(ExtCellArray)
	    data_size = ed->ext_cell_arr3.dim.size_x *
			ed->ext_cell_arr3.dim.size_y *
			PEX_COLOUR_SIZE(ed->ext_cell_arr3.colour_model);
	    pad = PADDING(data_size);
	    break;
	    
	case PELEM_GDP3:
	    TYPE_AND_SIZE(Gdp)
	    data_size = ed->gdp3.pts.num_points * sizeof(pexCoord3D);
	    data_size += ed->gdp3.rec.unsupp.size;
	    pad = PADDING(ed->gdp3.rec.unsupp.size);
	    break;

	case PELEM_GDP:
	    TYPE_AND_SIZE(Gdp2D)
	    data_size = ed->gdp.pts.num_points * sizeof(pexCoord2D);
	    data_size += ed->gdp.rec.unsupp.size;
	    pad = PADDING(ed->gdp.rec.unsupp.size);
	    break;

	case PELEM_APPL_DATA:
	    TYPE_AND_SIZE(ApplicationData)
	    data_size = ed->appl_data.size;
	    pad = PADDING(data_size);
	    break;

	case PELEM_GSE:
	    TYPE_AND_SIZE(Gse)
	    data_size = ed->gse.rec.unsupp.size;
	    pad = PADDING(ed->gse.rec.unsupp.size);
	    break;

	case PELEM_ADD_NAMES_SET:
	    TYPE_AND_SIZE(AddToNameSet)
	    data_size = ed->name_set.num_ints * sizeof(pexName);
	    break;
	case PELEM_REMOVE_NAMES_SET:
	    TYPE_AND_SIZE(RemoveFromNameSet)
	    data_size = ed->name_set.num_ints * sizeof(pexName);
	    break;

	/* All the following just contain fixed-size data. */
	case PELEM_LINE_IND: TYPE_AND_SIZE(LineBundleIndex) break;
	case PELEM_MARKER_IND: TYPE_AND_SIZE(MarkerBundleIndex) break;
	case PELEM_TEXT_IND: TYPE_AND_SIZE(TextBundleIndex) break;
	case PELEM_INT_IND: TYPE_AND_SIZE(InteriorBundleIndex) break;
	case PELEM_EDGE_IND: TYPE_AND_SIZE(EdgeBundleIndex) break;

	case PELEM_LINE_COLR_IND: TYPE_AND_SIZE(LineColourIndex) break;
	case PELEM_MARKER_COLR_IND: TYPE_AND_SIZE(MarkerColourIndex) break;
	case PELEM_TEXT_COLR_IND: TYPE_AND_SIZE(TextColourIndex) break;
	case PELEM_INT_COLR_IND: TYPE_AND_SIZE(SurfaceColourIndex) break;
	case PELEM_EDGE_COLR_IND: TYPE_AND_SIZE(SurfaceEdgeColourIndex) break;

	case PELEM_TEXT_FONT: TYPE_AND_SIZE(TextFontIndex) break;
	case PELEM_ANNO_STYLE: TYPE_AND_SIZE(AtextStyle) break;
	case PELEM_INT_STYLE_IND: TYPE_AND_SIZE(InteriorStyleIndex) break;
	case PELEM_VIEW_IND: TYPE_AND_SIZE(ViewIndex) break;
	case PELEM_DCUE_IND: TYPE_AND_SIZE(DepthCueIndex) break;
	case PELEM_BACK_INT_STYLE_IND:
	    TYPE_AND_SIZE(BfInteriorStyleIndex) break;
	case PELEM_COLR_MAP_IND: TYPE_AND_SIZE(ColourApproxIndex) break;
	
	case PELEM_LINETYPE: TYPE_AND_SIZE(LineType) break;
	case PELEM_MARKER_TYPE: TYPE_AND_SIZE(MarkerType) break;
	case PELEM_EDGETYPE: TYPE_AND_SIZE(SurfaceEdgeType) break;
	case PELEM_INT_REFL_EQN:
	    TYPE_AND_SIZE(SurfaceReflModel) break;
	case PELEM_BACK_INT_REFL_EQN:
	    TYPE_AND_SIZE(BfSurfaceReflModel) break;
	case PELEM_LINE_SHAD_METH: TYPE_AND_SIZE(PolylineInterp) break;
	case PELEM_INT_SHAD_METH: TYPE_AND_SIZE(SurfaceInterp) break;
	case PELEM_BACK_INT_SHAD_METH:
	    TYPE_AND_SIZE(BfSurfaceInterp) break;
	case PELEM_RENDERING_COLR_MODEL:
	    TYPE_AND_SIZE(RenderingColourModel) break;

	/* All these requests contain just one CARD32 (plus the header). */
	case PELEM_HLHSR_ID: TYPE_AND_SIZE(HlhsrIdentifier) break;
	case PELEM_LABEL: TYPE_AND_SIZE(Label) break;
	case PELEM_EXEC_STRUCT: TYPE_AND_SIZE(ExecuteStructure) break;
	case PELEM_PICK_ID: TYPE_AND_SIZE(PickId) break;

	case PELEM_LINEWIDTH: TYPE_AND_SIZE(LineWidth) break;
	case PELEM_MARKER_SIZE: TYPE_AND_SIZE(MarkerScale) break;
	case PELEM_CHAR_EXPAN: TYPE_AND_SIZE(CharExpansion) break;
	case PELEM_CHAR_SPACE: TYPE_AND_SIZE(CharSpacing) break;
	case PELEM_CHAR_HT: TYPE_AND_SIZE(CharHeight) break;
	case PELEM_ANNO_CHAR_HT:
	    TYPE_AND_SIZE(AtextHeight) break;
	case PELEM_EDGEWIDTH: TYPE_AND_SIZE(SurfaceEdgeWidth) break;
	
	case PELEM_TEXT_PREC: TYPE_AND_SIZE(TextPrecision) break;
	case PELEM_TEXT_PATH: TYPE_AND_SIZE(TextPath) break;
	case PELEM_ANNO_PATH: TYPE_AND_SIZE(AtextPath) break;
	case PELEM_INT_STYLE: TYPE_AND_SIZE(InteriorStyle) break;
	case PELEM_BACK_INT_STYLE: TYPE_AND_SIZE(BfInteriorStyle) break;
	case PELEM_FACE_DISTING_MODE: TYPE_AND_SIZE(DistinguishFlag) break;
	case PELEM_FACE_CULL_MODE: TYPE_AND_SIZE(CullingMode) break;

	case PELEM_TEXT_COLR:
	    TYPE_AND_SIZE(TextColour)
	    data_size += PEX_COLOUR_SIZE( ed->colour.type );
	    break;
	case PELEM_MARKER_COLR:
	    TYPE_AND_SIZE(MarkerColour)
	    data_size += PEX_COLOUR_SIZE( ed->colour.type );
	    break;
	case PELEM_EDGE_COLR:
	    TYPE_AND_SIZE(SurfaceEdgeColour)
	    data_size += PEX_COLOUR_SIZE( ed->colour.type );
	    break;
	case PELEM_LINE_COLR:
	    TYPE_AND_SIZE(LineColour)
	    data_size += PEX_COLOUR_SIZE( ed->colour.type );
	    break;
	case PELEM_INT_COLR:
	    TYPE_AND_SIZE(SurfaceColour)
	    data_size += PEX_COLOUR_SIZE( ed->colour.type );
	    break;
	case PELEM_BACK_INT_COLR:
	    TYPE_AND_SIZE(BfSurfaceColour)
	    data_size += PEX_COLOUR_SIZE( ed->colour.type );
	    break;

	case PELEM_TEXT_ALIGN: TYPE_AND_SIZE(TextAlignment) break;
	case PELEM_ANNO_ALIGN: TYPE_AND_SIZE(AtextAlignment) break;
	case PELEM_CHAR_UP_VEC: TYPE_AND_SIZE(CharUpVector) break;
	case PELEM_ANNO_CHAR_UP_VEC:
	    TYPE_AND_SIZE(AtextUpVector) break;

	case PELEM_PAT_SIZE: TYPE_AND_SIZE(PatternSize) break;
	case PELEM_PAT_REF_POINT_VECS:
	    TYPE_AND_SIZE(PatternAttr) break;
	case PELEM_PAT_REF_POINT: TYPE_AND_SIZE(PatternRefPt) break;

	case PELEM_EDGE_FLAG: TYPE_AND_SIZE(SurfaceEdgeFlag) break;
	case PELEM_INDIV_ASF: TYPE_AND_SIZE(SetAsfValues) break;

	case PELEM_LOCAL_MODEL_TRAN3:
	    TYPE_AND_SIZE(LocalTransform) break;
	case PELEM_LOCAL_MODEL_TRAN:
	    TYPE_AND_SIZE(LocalTransform2D) break;
	case PELEM_GLOBAL_MODEL_TRAN3:
	    TYPE_AND_SIZE(GlobalTransform) break;
	case PELEM_GLOBAL_MODEL_TRAN:
	    TYPE_AND_SIZE(GlobalTransform2D) break;

	case PELEM_MODEL_CLIP_VOL3:
	    TYPE_AND_SIZE(ModelClipVolume)
	    data_size = ed->mclip_vol3.hsplst.num_half_spaces
		* (sizeof(pexCoord3D) + sizeof(pexVector3D));
	    break;
	case PELEM_MODEL_CLIP_VOL:
	    TYPE_AND_SIZE(ModelClipVolume2D)
	    data_size = ed->mclip_vol.hsplst.num_half_spaces
		* (sizeof(pexCoord2D) + sizeof(pexVector2D));
	    break;
	case PELEM_MODEL_CLIP_IND:
	    TYPE_AND_SIZE(ModelClip) break;
	case PELEM_RESTORE_MODEL_CLIP_VOL:
	    TYPE_AND_SIZE(RestoreModelClip) break;

	case PELEM_REFL_PROPS:
	    TYPE_AND_SIZE(SurfaceReflAttr)
	    data_size += PEX_COLOUR_SIZE( ed->refl_props.specular_colr.type);
	    break;
	case PELEM_BACK_REFL_PROPS:
	    TYPE_AND_SIZE(BfSurfaceReflAttr)
	    data_size += PEX_COLOUR_SIZE( ed->refl_props.specular_colr.type);
	    break;

	case PELEM_CURVE_APPROX_CRIT:
	    TYPE_AND_SIZE(CurveApproximation) break;
	case PELEM_SURF_APPROX_CRIT:
	    TYPE_AND_SIZE(SurfaceApproximation) break;

	case PELEM_LIGHT_SRC_STATE: 
	    TYPE_AND_SIZE(LightState)
	    data_size = (ed->light_state.act_set.num_ints
		+ ed->light_state.deact_set.num_ints) * sizeof(pexTableIndex)
		+ PADDING(ed->light_state.act_set.num_ints * 
						    sizeof(pexTableIndex))
		+ PADDING(ed->light_state.deact_set.num_ints * 
						    sizeof(pexTableIndex));
	    break;

	case PELEM_PARA_SURF_CHARACS: 
	    TYPE_AND_SIZE(ParaSurfCharacteristics)
	    switch ( ed->psc.type ) {
		case PSC_NONE:
		case PSC_WS_DEP:
		    break;
		case PSC_ISOPARAMETRIC_CURVES:
		    data_size = sizeof(pexPSC_IsoparametricCurves);
		    break;
		case PSC_LEVEL_CURVES_MC:
		case PSC_LEVEL_CURVES_WC:
		    data_size = sizeof(pexPSC_LevelCurves) +
			ed->psc.data.psc_4.params.num_floats * sizeof(PEXFLOAT);
		    break;
	    }
	    break;
    }

    if ( pad )
	data_size += pad;
    if ( (hdr_size + data_size) > 0 ) {
	if ( !PHG_SCRATCH_SPACE(scratch,hdr_size+data_size) ) {
	    ERR_BUF(erh, ERR900);
	    return 0;
	} else {
	    assure((hdr_size+data_size)%4 == 0)
	    pex_oc->size = hdr_size + data_size;
	    pex_oc->oc = (pexElementInfo *)scratch->buf;
	    pex_oc->oc->elementType = oc_type;
	    pex_oc->oc->length = (pex_oc->size) / sizeof(CARD32);
	}
    }

    /* Copy the data to the buffer.  Be sure not to bump up the buffer
     * pointer until all the header data is loaded with the HEADER macro.
     */
    buf = ((char *)pex_oc->oc) + hdr_size;
    switch ( el_type ) {
	case PELEM_POLYLINE3:
	case PELEM_POLYMARKER3:
	    (void)phg_utx_ptlst3_to_pex( ed->ptlst3.num_points,
		ed->ptlst3.points, (pexCoord3D *)buf);
	    break;
	case PELEM_POLYLINE:
	case PELEM_POLYMARKER:
	    (void)phg_utx_ptlst_to_pex( ed->ptlst.num_points, ed->ptlst.points,
		(pexCoord2D *)buf );
	    break;
	case PELEM_FILL_AREA3:
	    HEADER(FillArea)->shape = PEXUnknownShape;
	    HEADER(FillArea)->ignoreEdges = True;
	    (void)phg_utx_ptlst3_to_pex( ed->ptlst3.num_points,
		ed->ptlst3.points, (pexCoord3D *)buf);
	    break;
	case PELEM_FILL_AREA:
	    HEADER(FillArea2D)->shape = PEXUnknownShape;
	    HEADER(FillArea2D)->ignoreEdges = True;
	    (void)phg_utx_ptlst_to_pex( ed->ptlst.num_points, ed->ptlst.points,
		(pexCoord2D *)buf );
	    break;
	case PELEM_TEXT3:
	    PEX_CONV_FROM_Ppoint3(&ed->text3.pt,&HEADER(Text)->origin)
	    PEX_CONV_FROM_Pvec3(&ed->text3.dir[0],&HEADER(Text)->vector1)
	    PEX_CONV_FROM_Pvec3(&ed->text3.dir[1],&HEADER(Text)->vector2)
	    HEADER(Text)->numEncodings = phg_utx_encode_text(
		ed->text3.length, ed->text3.string, (pexMonoEncoding *)buf );
	    break;
	case PELEM_TEXT:
	    PEX_CONV_FROM_Ppoint(&ed->text.pt,&HEADER(Text2D)->origin)
	    HEADER(Text2D)->numEncodings = phg_utx_encode_text(
		ed->text.length, ed->text.string, (pexMonoEncoding *)buf );
	    break;
	case PELEM_ANNO_TEXT_REL3:
	    PEX_CONV_FROM_Ppoint3(&ed->anno_text_rel3.ref_pt,
		&HEADER(AnnotationText)->origin)
	    PEX_CONV_FROM_Ppoint3(&ed->anno_text_rel3.offset,
		&HEADER(AnnotationText)->offset)
	    HEADER(AnnotationText)->numEncodings =
		phg_utx_encode_text( ed->anno_text_rel3.length,
		    ed->anno_text_rel3.string, (pexMonoEncoding *)buf );
	    break;
	case PELEM_ANNO_TEXT_REL:
	    PEX_CONV_FROM_Ppoint(&ed->anno_text_rel.ref_pt,
		&HEADER(AnnotationText2D)->origin)
	    PEX_CONV_FROM_Ppoint(&ed->anno_text_rel.offset,
		&HEADER(AnnotationText2D)->offset)
	    HEADER(AnnotationText2D)->numEncodings =
		phg_utx_encode_text( ed->anno_text_rel.length,
		    ed->anno_text_rel.string, (pexMonoEncoding *)buf );
	    break;
	case PELEM_FILL_AREA_SET3:
	    HEADER(FillAreaSet)->shape = PEXUnknownShape;
	    HEADER(FillAreaSet)->contourHint = PEXUnknownContour;
	    HEADER(FillAreaSet)->ignoreEdges = False;
	    HEADER(FillAreaSet)->numLists = ed->fa_set3.num_sets;
	    for ( i = 0; i < ed->fa_set3.num_sets; i++ ) {
		bufp32 = (CARD32 *)buf; buf += sizeof(CARD32);
		*bufp32 = ed->fa_set3.sets[i].num_points;
		buf += phg_utx_ptlst3_to_pex( ed->fa_set3.sets[i].num_points,
		    ed->fa_set3.sets[i].points, (pexCoord3D *)buf );
	    }
	    break;
	case PELEM_FILL_AREA_SET:
	    HEADER(FillAreaSet2D)->shape = PEXUnknownShape;
	    HEADER(FillAreaSet2D)->contourHint = PEXUnknownContour;
	    HEADER(FillAreaSet2D)->ignoreEdges = False;
	    HEADER(FillAreaSet2D)->numLists = ed->fa_set.num_sets;
	    for ( i = 0; i < ed->fa_set.num_sets; i++ ) {
		bufp32 = (CARD32 *)buf; buf += sizeof(CARD32);
		*bufp32 = ed->fa_set.sets[i].num_points;
		buf += phg_utx_ptlst_to_pex( ed->fa_set.sets[i].num_points,
		    ed->fa_set.sets[i].points, (pexCoord2D *)buf );
	    }
	    break;
	case PELEM_CELL_ARRAY3:
	    PEX_CONV_FROM_Ppoint3(&ed->cell_array3.paral.p,
		&HEADER(CellArray)->point1)
	    PEX_CONV_FROM_Ppoint3(&ed->cell_array3.paral.q,
		&HEADER(CellArray)->point2)
	    PEX_CONV_FROM_Ppoint3(&ed->cell_array3.paral.r,
		&HEADER(CellArray)->point3)
	    HEADER(CellArray)->dx = ed->cell_array3.dim.size_x;
	    HEADER(CellArray)->dy = ed->cell_array3.dim.size_y;
	    count = ed->cell_array3.dim.size_x * ed->cell_array3.dim.size_y;
	    bufptblind = (pexTableIndex *)buf;
	    for ( i = 0; i < count; i++ ) {
		*bufptblind++ = ed->cell_array3.colr[i];
		buf += sizeof(pexTableIndex);
	    }
	    break;
	case PELEM_CELL_ARRAY:
	    PEX_CONV_FROM_Ppoint(&ed->cell_array.rect.p,
		&HEADER(CellArray2D)->point1)
	    PEX_CONV_FROM_Ppoint(&ed->cell_array.rect.q,
		&HEADER(CellArray2D)->point2)
	    HEADER(CellArray2D)->dx = ed->cell_array.dim.size_x;
	    HEADER(CellArray2D)->dy = ed->cell_array.dim.size_y;
	    count = ed->cell_array.dim.size_x * ed->cell_array.dim.size_y;
	    bufptblind = (pexTableIndex *)buf;
	    for ( i = 0; i < count; i++ ) {
		*bufptblind++ = ed->cell_array.colr[i];
		buf += sizeof(pexTableIndex);
	    }
	    break;

	case PELEM_CELL_ARRAY3_PLUS:
	    HEADER(ExtCellArray)->colourType =
		PEX_CONV_PHIGS_COLOUR_TYPE(ed->ext_cell_arr3.colour_model);
	    PEX_CONV_FROM_Ppoint3(&ed->ext_cell_arr3.paral.p,
		&HEADER(ExtCellArray)->point1)
	    PEX_CONV_FROM_Ppoint3(&ed->ext_cell_arr3.paral.q,
		&HEADER(ExtCellArray)->point2)
	    PEX_CONV_FROM_Ppoint3(&ed->ext_cell_arr3.paral.r,
		&HEADER(ExtCellArray)->point3)
	    HEADER(ExtCellArray)->dx = ed->ext_cell_arr3.dim.size_x;
	    HEADER(ExtCellArray)->dy = ed->ext_cell_arr3.dim.size_y;
	    count = ed->ext_cell_arr3.dim.size_x * ed->ext_cell_arr3.dim.size_y;
	    for ( i = 0; i < count; i++ ) {
		PEX_CONV_FROM_Pcoval(ed->ext_cell_arr3.colour_model,
		    &ed->ext_cell_arr3.colr[i], buf)
	    }
	    break;

	case PELEM_POLYLINE_SET3_DATA:
	    HEADER(PolylineSet)->colourType
		= PEX_CONV_PHIGS_COLOUR_TYPE( ed->pl_set3_d.colour_model );
	    HEADER(PolylineSet)->vertexAttribs
		= PEX_CONV_FROM_VertexFlag( ed->pl_set3_d.vflag );
	    HEADER(PolylineSet)->numLists = ed->pl_set3_d.num_sets;
	    (void)phg_utx_vdata_to_pex( ed->pl_set3_d.num_sets, !0,
		ed->pl_set3_d.vflag, 0, ed->pl_set3_d.colour_model,
		ed->pl_set3_d.vdata, (Pedge_data_list *)NULL, buf );
	    break;
	case PELEM_FILL_AREA_SET3_DATA:
	    HEADER(ExtFillAreaSet)->shape = PEXUnknownShape;
	    HEADER(ExtFillAreaSet)->ignoreEdges = False;
	    HEADER(ExtFillAreaSet)->contourHint = PEXUnknownContour;
	    HEADER(ExtFillAreaSet)->numLists = ed->fa_set3_d.num_sets;
	    HEADER(ExtFillAreaSet)->colourType
		= PEX_CONV_PHIGS_COLOUR_TYPE( ed->fa_set3_d.colour_model );
	    HEADER(ExtFillAreaSet)->vertexAttribs
		= PEX_CONV_FROM_VertexFlag( ed->fa_set3_d.vflag );
	    if ( ed->fa_set3_d.eflag != PEDGE_NONE )
		HEADER(ExtFillAreaSet)->vertexAttribs |= PEXGAEdges;
	    HEADER(ExtFillAreaSet)->facetAttribs
		= PEX_CONV_FROM_FacetFlag( ed->fa_set3_d.fflag );
	    if ( ed->fa_set3_d.fflag != PFACET_NONE ) {
		Phg_facet_data_list3	fdatalst;
		fdatalst.num_facets = 1;
		fdatalst.facetdata.norms = &(ed->fa_set3_d.fdata.norm);
		buf += fdata_to_pex( ed->fa_set3_d.fflag,
		    ed->fa_set3_d.colour_model, &fdatalst, buf );
	    }
	    buf += phg_utx_vdata_to_pex( ed->fa_set3_d.num_sets, !0,
		ed->fa_set3_d.vflag, ed->fa_set3_d.eflag,
		ed->fa_set3_d.colour_model,
		ed->fa_set3_d.vdata, ed->fa_set3_d.edata, buf );
	    break;
	case PELEM_TRI_STRIP3_DATA:
	    HEADER(TriangleStrip)->colourType
		= PEX_CONV_PHIGS_COLOUR_TYPE( ed->tri_strip3.colour_model );
	    HEADER(TriangleStrip)->vertexAttribs
		= PEX_CONV_FROM_VertexFlag( ed->tri_strip3.vflag );
	    HEADER(TriangleStrip)->facetAttribs
		= PEX_CONV_FROM_FacetFlag( ed->tri_strip3.fflag );
	    HEADER(TriangleStrip)->numVertices
		= ed->tri_strip3.vdata.num_vertices;
	    if ( ed->tri_strip3.fflag != PFACET_NONE )
		buf += fdata_to_pex( ed->tri_strip3.fflag,
		    ed->tri_strip3.colour_model, &ed->tri_strip3.fdata, buf );
	    (void)phg_utx_vdata_to_pex( 1, 0, ed->tri_strip3.vflag, 0,
		ed->tri_strip3.colour_model, &ed->tri_strip3.vdata,
		(Pedge_data_list *)NULL, buf );
	    break;
	case PELEM_QUAD_MESH3_DATA:
	    HEADER(QuadrilateralMesh)->colourType
		= PEX_CONV_PHIGS_COLOUR_TYPE( ed->quad_mesh3.colour_model );
	    HEADER(QuadrilateralMesh)->vertexAttribs
		= PEX_CONV_FROM_VertexFlag( ed->quad_mesh3.vflag );
	    HEADER(QuadrilateralMesh)->facetAttribs
		= PEX_CONV_FROM_FacetFlag( ed->quad_mesh3.fflag );
	    HEADER(QuadrilateralMesh)->mPts = ed->quad_mesh3.dim.size_x;
	    HEADER(QuadrilateralMesh)->nPts = ed->quad_mesh3.dim.size_y;
	    if ( ed->quad_mesh3.fflag != PFACET_NONE ) {
		buf += fdata_to_pex( ed->quad_mesh3.fflag,
		    ed->quad_mesh3.colour_model, &ed->quad_mesh3.fdata, buf );
	    }
	    (void)phg_utx_vdata_to_pex( 1, 0, ed->quad_mesh3.vflag, 0,
		ed->quad_mesh3.colour_model, &ed->quad_mesh3.vdata,
		(Pedge_data_list *)NULL, buf );
	    break;
	case PELEM_SET_OF_FILL_AREA_SET3_DATA:
	    HEADER(SOFAS)->shape = PEXUnknownShape;
	    HEADER(SOFAS)->colourType
		= PEX_CONV_PHIGS_COLOUR_TYPE( ed->sofas3.colour_model );
	    HEADER(SOFAS)->FAS_Attributes
		= PEX_CONV_FROM_FacetFlag( ed->sofas3.fflag );
	    HEADER(SOFAS)->vertexAttributes
		= PEX_CONV_FROM_VertexFlag( ed->sofas3.vflag );
	    HEADER(SOFAS)->contourHint = PEXUnknownContour;
	    HEADER(SOFAS)->contourCountsFlag = 0;
	    HEADER(SOFAS)->numFAS = ed->sofas3.num_sets;
	    HEADER(SOFAS)->numVertices = ed->sofas3.vdata.num_vertices;
	    HEADER(SOFAS)->numEdges = ed->sofas3.num_vindices;
	    HEADER(SOFAS)->numContours = ed->sofas3.num_contours;
	    if ( ed->sofas3.fflag != PFACET_NONE ) {
		buf += fdata_to_pex( ed->sofas3.fflag,
		    ed->sofas3.colour_model, &ed->sofas3.fdata, buf );
	    }
	    buf += phg_utx_vdata_to_pex( 1, 0,
		ed->sofas3.vflag, 0, ed->sofas3.colour_model,
		&ed->sofas3.vdata, (Pedge_data_list *)NULL, buf );
	    if ( ed->sofas3.eflag != PEDGE_NONE ) {
		HEADER(SOFAS)->edgeAttributes = 1;
		buf = sofa_edges_to_pex( ed->sofas3.num_sets,
		    ed->sofas3.num_vindices, ed->sofas3.edata, buf );
	    } else
		HEADER(SOFAS)->edgeAttributes = 0;
	    buf = sofa_connectivity_to_pex( ed->sofas3.num_sets,
		ed->sofas3.num_contours, ed->sofas3.num_vindices,
		ed->sofas3.vlist, buf );
	    break;

	case PELEM_NUNI_BSP_CURVE:
	    HEADER(NurbCurve)->curveOrder = ed->nurb_curve.data.order;
	    HEADER(NurbCurve)->tmin = ed->nurb_curve.data.tstart;
	    HEADER(NurbCurve)->tmax = ed->nurb_curve.data.tend;
	    HEADER(NurbCurve)->numKnots = ed->nurb_curve.data.knots.num_floats;
	    HEADER(NurbCurve)->numPoints = ed->nurb_curve.data.npts;
	    HEADER(NurbCurve)->coordType
		= PEX_CONV_FROM_Prational(ed->nurb_curve.data.rationality);
	    bufpfloat = (PEXFLOAT *)buf;
	    for ( i = 0; i < ed->nurb_curve.data.knots.num_floats; i++ )
		*bufpfloat++ = ed->nurb_curve.data.knots.floats[i];
	    buf = (char *)bufpfloat;
	    if ( ed->nurb_curve.data.rationality == PRATIONAL )
		(void)phg_utx_ptlst4_to_pex( ed->nurb_curve.data.npts,
		    ed->nurb_curve.data.points, (pexCoord4D *)buf );
	    else
		(void)phg_utx_ptlst3_to_pex( ed->nurb_curve.data.npts,
		    (Ppoint3*)ed->nurb_curve.data.points, (pexCoord3D *)buf );
	    break;
	case PELEM_NUNI_BSP_SURF:
	    HEADER(NurbSurface)->uOrder = ed->nurb_surf.data.u_order;
	    HEADER(NurbSurface)->vOrder = ed->nurb_surf.data.v_order;
	    HEADER(NurbSurface)->numUknots =
		ed->nurb_surf.data.uknots.num_floats;
	    HEADER(NurbSurface)->numVknots =
		ed->nurb_surf.data.vknots.num_floats;
	    HEADER(NurbSurface)->mPts = ed->nurb_surf.data.npts.u_dim;
	    HEADER(NurbSurface)->nPts = ed->nurb_surf.data.npts.v_dim;
	    HEADER(NurbSurface)->type
		= PEX_CONV_FROM_Prational(ed->nurb_surf.data.rationality);
	    HEADER(NurbSurface)->numLists = ed->nurb_surf.data.nloops;
	    bufpfloat = (PEXFLOAT *)buf;
	    for ( i = 0; i < ed->nurb_surf.data.uknots.num_floats; i++ )
		*bufpfloat++ = ed->nurb_surf.data.uknots.floats[i];
	    for ( i = 0; i < ed->nurb_surf.data.vknots.num_floats; i++ )
		*bufpfloat++ = ed->nurb_surf.data.vknots.floats[i];
	    buf = (char *)bufpfloat;
	    if ( ed->nurb_surf.data.rationality == PRATIONAL )
		buf += phg_utx_ptlst4_to_pex( count, ed->nurb_surf.data.grid,
		    (pexCoord4D *)buf );
	    else
		buf += phg_utx_ptlst3_to_pex( count,
		    (Ppoint3*)ed->nurb_surf.data.grid, (pexCoord3D *)buf );
	    if ( ed->nurb_surf.data.nloops > 0 )
		trim_curves_to_pex( &ed->nurb_surf.data, buf );
	    break;

	case PELEM_GDP3:
	    HEADER(Gdp)->gdpId = ed->gdp3.id;
	    HEADER(Gdp)->numPoints = ed->gdp3.pts.num_points;
	    if ( ed->gdp3.pts.num_points > 0 )
		buf += phg_utx_ptlst3_to_pex( ed->gdp3.pts.num_points,
		    (Ppoint3 *)ed->gdp3.pts.points, (pexCoord3D *)buf );
	    HEADER(Gdp)->numBytes = ed->gdp3.rec.unsupp.size;
	    if ( ed->gdp3.rec.unsupp.size > 0 )
		bcopy( ed->gdp3.rec.unsupp.data, buf,
		    ed->gdp3.rec.unsupp.size );
	    break;

	case PELEM_GDP:
	    HEADER(Gdp2D)->gdpId = ed->gdp.id;
	    HEADER(Gdp2D)->numPoints = ed->gdp.pts.num_points;
	    if ( ed->gdp.pts.num_points > 0 )
		buf += phg_utx_ptlst_to_pex( ed->gdp.pts.num_points,
		    (Ppoint *)ed->gdp.pts.points, (pexCoord2D *)buf );
	    HEADER(Gdp2D)->numBytes = ed->gdp.rec.unsupp.size;
	    if ( ed->gdp.rec.unsupp.size > 0 )
		bcopy( ed->gdp.rec.unsupp.data, buf,
		    ed->gdp.rec.unsupp.size );
	    break;

	/* All these requests just contain one CARD16. */
	case PELEM_LINE_IND:
	case PELEM_MARKER_IND:
	case PELEM_TEXT_IND:
	case PELEM_INT_IND:
	case PELEM_EDGE_IND:
	case PELEM_LINE_COLR_IND:
	case PELEM_MARKER_COLR_IND:
	case PELEM_TEXT_COLR_IND:
	case PELEM_INT_COLR_IND:
	case PELEM_EDGE_COLR_IND:
	case PELEM_TEXT_FONT:
	case PELEM_INT_STYLE_IND:
	case PELEM_VIEW_IND:
	case PELEM_DCUE_IND:
	case PELEM_BACK_INT_STYLE_IND:
	case PELEM_COLR_MAP_IND:
	    HEADER(MarkerBundleIndex)->index = ed->idata;
	    break;

	/* These all contain one 16-bit integer. */
	case PELEM_LINETYPE:
	case PELEM_MARKER_TYPE:
	case PELEM_EDGETYPE:
	case PELEM_ANNO_STYLE:
	case PELEM_INT_REFL_EQN:
	case PELEM_BACK_INT_REFL_EQN:
	case PELEM_LINE_SHAD_METH:
	case PELEM_INT_SHAD_METH:
	case PELEM_BACK_INT_SHAD_METH:
	case PELEM_RENDERING_COLR_MODEL:
	    HEADER(LineType)->lineType = ed->idata;
	    break;

	/* These all contain one 32-bit value. */
	case PELEM_HLHSR_ID:
	    HEADER(HlhsrIdentifier)->hlhsrID = ed->idata;
	    break;
	case PELEM_PICK_ID:
	    HEADER(PickId)->pickId = ed->idata;
	    break;
	case PELEM_EXEC_STRUCT:
	    HEADER(ExecuteStructure)->id = ed->idata;
	    break;
	case PELEM_LABEL:
	    HEADER(Label)->label = ed->idata;
	    break;

	/* All these requests just contain one float. */
	case PELEM_LINEWIDTH:
	case PELEM_MARKER_SIZE:
	case PELEM_EDGEWIDTH:
	case PELEM_CHAR_EXPAN:
	case PELEM_CHAR_SPACE:
	case PELEM_CHAR_HT:
	case PELEM_ANNO_CHAR_HT:
	    HEADER(LineWidth)->width = ed->fdata;
	    break;

	/* All these contain (only) a enumerated value in PHIGS but a CARD16
	 * in PEX.  The enumeration values don't necessarily line up with
	 * the PEX constants.
	 */
	case PELEM_TEXT_PREC:
	    HEADER(TextPrecision)->precision
		= PEX_CONV_FROM_Ptxprec(ed->txprec);
	    break;
	case PELEM_TEXT_PATH:
	case PELEM_ANNO_PATH:
	    HEADER(TextPath)->path
		= PEX_CONV_FROM_Ptxpath(ed->txpath);
	    break;
	case PELEM_INT_STYLE:
	case PELEM_BACK_INT_STYLE:
	    HEADER(InteriorStyle)->interiorStyle
		= PEX_CONV_FROM_Pinterstyle(ed->interstyle);
	    break;
	case PELEM_FACE_DISTING_MODE:
	    HEADER(DistinguishFlag)->distinguish
		= PEX_CONV_FROM_Pdistgmode((Pdisting_mode)ed->distgmode);
	    break;
	case PELEM_FACE_CULL_MODE:
	    HEADER(CullingMode)->cullMode
		= PEX_CONV_FROM_Pcullmode((Pcull_mode)ed->cullmode);
	    break;
	case PELEM_MODEL_CLIP_IND:
	    HEADER(ModelClip)->onoff = (ed->clip_ind == PIND_CLIP) ?
		PEXClip : PEXNoClip;
	    break;
	case PELEM_EDGE_FLAG:
	    HEADER(SurfaceEdgeFlag)->onoff = PEX_CONV_FROM_Pedgef(ed->edgef);
	    break;

	/* All these contain just a colour. */
	case PELEM_LINE_COLR:
	case PELEM_MARKER_COLR:
	case PELEM_TEXT_COLR:
	case PELEM_EDGE_COLR:
	case PELEM_INT_COLR:
	case PELEM_BACK_INT_COLR:
	    cspec = (Pextmpl_colour_spec *)&(HEADER(MarkerColour)->colourSpec);
	    PEX_CONV_FROM_Pgcolr( &ed->colour, cspec )
	    break;

	/* These have no commonality. */
	case PELEM_TEXT_ALIGN:
	case PELEM_ANNO_ALIGN:
	    HEADER(TextAlignment)->alignment.vertical
		= PEX_CONV_FROM_Ptxver(ed->txalign.vert);
	    HEADER(TextAlignment)->alignment.horizontal
		= PEX_CONV_FROM_Ptxhor(ed->txalign.hor);
	    break;

	case PELEM_CHAR_UP_VEC:
	case PELEM_ANNO_CHAR_UP_VEC:
	    PEX_CONV_FROM_Pvec(&ed->char_up,&HEADER(CharUpVector)->up)
	    break;

	case PELEM_PAT_SIZE:
	    PEX_CONV_FROM_Ppoint(&ed->pt,&HEADER(PatternSize)->size)
	    break;
	case PELEM_PAT_REF_POINT_VECS:
	    PEX_CONV_FROM_Ppoint3(&ed->pat_pt_vecs.pt,
		&HEADER(PatternAttr)->refPt)
	    PEX_CONV_FROM_Pvec3(&ed->pat_pt_vecs.vecs[0],
		&HEADER(PatternAttr)->vector1)
	    PEX_CONV_FROM_Pvec3(&ed->pat_pt_vecs.vecs[1],
		&HEADER(PatternAttr)->vector2)
	    break;
	case PELEM_PAT_REF_POINT:
	    PEX_CONV_FROM_Ppoint(&ed->pt,&HEADER(PatternRefPt)->point)
	    break;

	case PELEM_ADD_NAMES_SET:
	case PELEM_REMOVE_NAMES_SET: {
	    pexName	*bufpname = (pexName *)buf;
	    for ( i = 0; i < ed->name_set.num_ints; i++ ) {
		*bufpname++ = ed->name_set.ints[i];
		buf += sizeof(*bufpname);
	    }
	    } break;

	case PELEM_INDIV_ASF:
	    PEX_CONV_FROM_Pattrid(ed->asf_info.attr_id,
		HEADER(SetAsfValues)->attribute)
	    HEADER(SetAsfValues)->source =
		PEX_CONV_FROM_Pasf(ed->asf_info.asf);
	    break;

	case PELEM_LOCAL_MODEL_TRAN3:
	    HEADER(LocalTransform)->compType
		= PEX_CONV_FROM_Pcomptype(ed->local_xform3.comptype);
	    bufpfloat = &HEADER(LocalTransform)->matrix[0][0];
	    for ( i = 0; i < 16; i++ )
		*bufpfloat++ = ((Pfloat*)(ed->local_xform3.mat3))[i];
	    break;
	case PELEM_LOCAL_MODEL_TRAN:
	    HEADER(LocalTransform2D)->compType
		= PEX_CONV_FROM_Pcomptype(ed->local_xform.comptype);
	    bufpfloat = &HEADER(LocalTransform2D)->matrix3X3[0][0];
	    for ( i = 0; i < 9; i++ )
		*bufpfloat++ = ((Pfloat*)(ed->local_xform.mat))[i];
	    break;
	case PELEM_GLOBAL_MODEL_TRAN3:
	    bufpfloat = &HEADER(GlobalTransform)->matrix[0][0];
	    for ( i = 0; i < 16; i++ )
		*bufpfloat++ = ((Pfloat*)(ed->mat3))[i];
	    break;
	case PELEM_GLOBAL_MODEL_TRAN:
	    bufpfloat = &HEADER(GlobalTransform2D)->matrix3X3[0][0];
	    for ( i = 0; i < 9; i++ )
		*bufpfloat++ = ((Pfloat*)(ed->mat))[i];
	    break;

	case PELEM_MODEL_CLIP_VOL3:
	    HEADER(ModelClipVolume)->modelClipOperator = ed->mclip_vol3.op;
	    HEADER(ModelClipVolume)->numHalfSpaces
		= ed->mclip_vol3.hsplst.num_half_spaces;
	    for ( i = 0; i < ed->mclip_vol3.hsplst.num_half_spaces; i++ ) {
		PEX_CONV_FROM_Ppoint3(&ed->mclip_vol3.hsplst.half_spaces[i].point,
		    (pexCoord3D*)buf)
		buf += sizeof(pexCoord3D);
		PEX_CONV_FROM_Pvec3(&ed->mclip_vol3.hsplst.half_spaces[i].norm,
		    (pexVector3D*)buf)
		buf += sizeof(pexVector3D);
	    }
	    break;
	case PELEM_MODEL_CLIP_VOL:
	    HEADER(ModelClipVolume2D)->modelClipOperator = ed->mclip_vol.op;
	    HEADER(ModelClipVolume2D)->numHalfSpaces
		= ed->mclip_vol.hsplst.num_half_spaces;
	    for ( i = 0; i < ed->mclip_vol.hsplst.num_half_spaces; i++ ) {
		PEX_CONV_FROM_Ppoint(&ed->mclip_vol.hsplst.half_spaces[i].point,
		    (pexCoord2D*)buf)
		buf += sizeof(pexCoord2D);
		PEX_CONV_FROM_Pvec(&ed->mclip_vol.hsplst.half_spaces[i].norm,
		    (pexVector2D*)buf)
		buf += sizeof(pexVector2D);
	    }
	    break;

	case PELEM_REFL_PROPS:
	case PELEM_BACK_REFL_PROPS:
	    HEADER(SurfaceReflAttr)->reflectionAttr.ambient =
		ed->refl_props.ambient_coef;
	    HEADER(SurfaceReflAttr)->reflectionAttr.diffuse =
		ed->refl_props.diffuse_coef;
	    HEADER(SurfaceReflAttr)->reflectionAttr.specular =
		ed->refl_props.specular_coef;
	    HEADER(SurfaceReflAttr)->reflectionAttr.specularConc =
		ed->refl_props.specular_exp;
	    HEADER(SurfaceReflAttr)->reflectionAttr.transmission = 0.0;
	    cspec = (Pextmpl_colour_spec *)
		&HEADER(SurfaceReflAttr)->reflectionAttr.specularColour;
	    PEX_CONV_FROM_Pgcolr( &ed->refl_props.specular_colr, cspec )
	    break;
	   
	case PELEM_CURVE_APPROX_CRIT:
	    HEADER(CurveApproximation)->approx.approxMethod
		= ed->curv_approx.type;
	    HEADER(CurveApproximation)->approx.tolerance
		= ed->curv_approx.value;
	    break;
	case PELEM_SURF_APPROX_CRIT:
	    HEADER(SurfaceApproximation)->approx.approxMethod
		= ed->surf_approx.type;
	    HEADER(SurfaceApproximation)->approx.uTolerance
		= ed->surf_approx.u_val;
	    HEADER(SurfaceApproximation)->approx.vTolerance
		= ed->surf_approx.v_val;
	    break;
	   
	case PELEM_LIGHT_SRC_STATE:
	    HEADER(LightState)->numEnable = ed->light_state.act_set.num_ints;
	    HEADER(LightState)->numDisable = ed->light_state.deact_set.num_ints;

	    for ( i = 0; i < ed->light_state.act_set.num_ints; i++) {
		*(pexTableIndex *)buf = 
			(pexTableIndex)ed->light_state.act_set.ints[i];
		buf += sizeof(pexTableIndex);
	    }
	    buf += PADDING(ed->light_state.act_set.num_ints * 
				           sizeof(pexTableIndex));
	    for ( i = 0; i < ed->light_state.deact_set.num_ints; i++) {
		*(pexTableIndex *)buf = 
			(pexTableIndex)ed->light_state.deact_set.ints[i];
		buf += sizeof(pexTableIndex);
	    }
	    buf += PADDING(ed->light_state.deact_set.num_ints *
					   sizeof(pexTableIndex));
	    break;

	case PELEM_APPL_DATA:
	    HEADER(ApplicationData)->numElements = ed->appl_data.size;
	    bcopy( ed->appl_data.data, buf, ed->appl_data.size );
	    break;

	case PELEM_GSE:
	    HEADER(Gse)->id = ed->gse.id;
	    HEADER(Gse)->numElements = ed->gse.rec.unsupp.size;
	    if ( ed->gse.rec.unsupp.size > 0 )
		bcopy( ed->gse.rec.unsupp.data, buf,
		    ed->gse.rec.unsupp.size );
	    break;

	case PELEM_PARA_SURF_CHARACS: 
	    HEADER(ParaSurfCharacteristics)->characteristics = ed->psc.type;
	    HEADER(ParaSurfCharacteristics)->length = data_size;
	    switch ( ed->psc.type ) {
		case PSC_NONE:
		case PSC_WS_DEP:
		    /* No data */
		    break;

		case PSC_ISOPARAMETRIC_CURVES: {
		    pexPSC_IsoparametricCurves	*drec;

		    drec = (pexPSC_IsoparametricCurves *)buf;
		    switch ( ed->psc.data.psc_3.placement ) {
			case PCP_UNIFORM:
			    drec->placementType =  PEXICUniformPlacement;
			    break;
			case PCP_NON_UNIFORM:
			    drec->placementType =  PEXICNonuniformPlacement;
			    break;
		    }
		    drec->numUcurves = ed->psc.data.psc_3.u_count;
		    drec->numVcurves = ed->psc.data.psc_3.v_count;
		} break;

		case PSC_LEVEL_CURVES_MC:
		case PSC_LEVEL_CURVES_WC: {
		    pexPSC_LevelCurves	*drec;

		    drec = (pexPSC_LevelCurves *)buf;
		    PEX_CONV_FROM_Ppoint( &ed->psc.data.psc_4.origin,
			&drec->origin)
		    PEX_CONV_FROM_Pvec(&ed->psc.data.psc_4.direction,
			&drec->direction)
		    drec->numberIntersections =
			ed->psc.data.psc_4.params.num_floats;
		    bufpfloat = (PEXFLOAT *)(drec + 1);
		    for (i = 0; i < ed->psc.data.psc_4.params.num_floats; i++)
			*bufpfloat++ = ed->psc.data.psc_4.params.floats[i];
		} break;
	    }
	    break;

	/* No data for the following. */
	case PELEM_RESTORE_MODEL_CLIP_VOL:
	    break;
    }

    return 1;
}
#undef HEADER
#undef TYPE_AND_SIZE
