/* $XConsortium: cb_fill.c,v 5.3 94/04/17 20:40:47 hersh Exp $ */

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

/* Fill area functions for the PHIGS C binding */

#include "phg.h"
#include "cp.h"
#include "cb_priv.h"



/* FILL AREA 3 */
void
pfill_area3(point_list)
    Ppoint_list3	*point_list;
{
    Phg_args		cp_args;
    Phg_el_data		ed;
    Phg_args_add_el	*args = &cp_args.data.add_el;
    
    if (CB_ENTRY_CHECK(phg_cur_cph, ERR5, Pfn_fill_area3)) {
	if (PSL_STRUCT_STATE(phg_cur_cph->psl) != PSTRUCT_ST_STOP) {
	    ERR_REPORT(phg_cur_cph->erh, ERR5);
	}
	else if (point_list->num_points < 0) {
	    ERR_REPORT(phg_cur_cph->erh, ERRN150);
	}
	else {
	    args->el_type = PELEM_FILL_AREA3;
	    ed.ptlst3 = *point_list;
	    if ( CB_BUILD_OC(args->el_type, &ed, &args->pex_oc) )
		CP_FUNC( phg_cur_cph, CP_FUNC_OP_ADD_EL, &cp_args, NULL);
	}
    }
}


/* FILL AREA */
void
pfill_area(point_list)
    Ppoint_list	*point_list;
{
    Phg_args		cp_args;
    Phg_el_data		ed;
    Phg_args_add_el	*args = &cp_args.data.add_el;
    
    if (CB_ENTRY_CHECK(phg_cur_cph, ERR5, Pfn_fill_area)) {
	if (PSL_STRUCT_STATE(phg_cur_cph->psl) != PSTRUCT_ST_STOP) {
	    ERR_REPORT(phg_cur_cph->erh, ERR5);
	}
	else if (point_list->num_points < 0) {
	    ERR_REPORT(phg_cur_cph->erh, ERRN150);
	}
	else {
	    args->el_type = PELEM_FILL_AREA;
	    ed.ptlst = *point_list;
	    if ( CB_BUILD_OC(args->el_type, &ed, &args->pex_oc) )
		CP_FUNC( phg_cur_cph, CP_FUNC_OP_ADD_EL, &cp_args, NULL);
	}
    }
}


/* FILL AREA SET 3 */
void
pfill_area_set3(point_list_list)
Ppoint_list_list3	*point_list_list;	/* list of 3d point lists */
{
    Phg_args		cp_args;
    Phg_el_data		ed;
    Phg_args_add_el	*args = &cp_args.data.add_el;

    register	int	i;
    
    if (CB_ENTRY_CHECK(phg_cur_cph, ERR5, Pfn_fill_area_set3)) {
	if (PSL_STRUCT_STATE(phg_cur_cph->psl) != PSTRUCT_ST_STOP) {
	    ERR_REPORT(phg_cur_cph->erh, ERR5);

	} else {
	    if ( point_list_list->num_point_lists < 0 ) {
		ERR_REPORT( phg_cur_cph->erh, ERRN150);

	    } else {
		args->el_type = PELEM_FILL_AREA_SET3;
		ed.fa_set3.num_sets = point_list_list->num_point_lists;
		ed.fa_set3.sets = point_list_list->point_lists;
		ed.fa_set3.total_pts = 0;
		for ( i = 0; i < point_list_list->num_point_lists; i++ ) {
		    if ( point_list_list->point_lists[i].num_points < 0 ) {
			ERR_REPORT(phg_cur_cph->erh, ERRN150);
			return;
		    } else
			ed.fa_set3.total_pts += 
			    point_list_list->point_lists[i].num_points;
		}
		if ( CB_BUILD_OC(args->el_type, &ed, &args->pex_oc) )
		    CP_FUNC(phg_cur_cph, CP_FUNC_OP_ADD_EL, &cp_args, NULL);
	    }
	}
    }
}


/* FILL AREA SET 3 WITH DATA */
void
pfill_area_set3_data( fflag, eflag, vflag, colour_model, fdata, nfa,
    edata, vdata)
Pint           	fflag;      	/* what data is specified per facet */
Pint           	eflag;          /* what data is specified per edge */
Pint           	vflag;          /* what data is specified per vertex */
Pint           	colour_model;   /* colour model */
Pfacet_data3     *fdata;     	/* facet data */
Pint		nfa;		/* number of fill areas in the set */
Pedge_data_list	*edata;		/* edge data list */
Pfacet_vdata_list3	*vdata;         /* facet vertex data list */
{
    Phg_args		cp_args;
    Phg_el_data		ed;
    Phg_args_add_el	*args = &cp_args.data.add_el;

    register int	i;
    
    if (CB_ENTRY_CHECK(phg_cur_cph, ERR5, Pfn_fill_area_set3_data)) {
	if (PSL_STRUCT_STATE(phg_cur_cph->psl) != PSTRUCT_ST_STOP) {
	    ERR_REPORT(phg_cur_cph->erh, ERR5);
	    return;

	} else if (nfa < 0) {
	    ERR_REPORT(phg_cur_cph->erh, ERRN164);
	    return;

	} else if ((fflag < PFACET_NONE) || (fflag > PFACET_COLOUR_NORMAL)) {
	    ERR_REPORT(phg_cur_cph->erh, ERRN161);

	} else if ((eflag < PEDGE_NONE) || (eflag > PEDGE_VISIBILITY)) {
	    ERR_REPORT(phg_cur_cph->erh, ERRN163);

	} else if ((vflag < PVERT_COORD)
		|| (vflag > PVERT_COORD_COLOUR_NORMAL)) {
	    ERR_REPORT(phg_cur_cph->erh, ERRN162);

	} else {
	    args->el_type = PELEM_FILL_AREA_SET3_DATA;
	    ed.fa_set3_d.fflag = fflag;
	    ed.fa_set3_d.vflag = vflag;
	    ed.fa_set3_d.eflag = eflag;
	    ed.fa_set3_d.colour_model = colour_model;
	    ed.fa_set3_d.num_sets = nfa;
	    ed.fa_set3_d.vdata = vdata;
	    if ( fflag != PFACET_NONE )
		ed.fa_set3_d.fdata = *fdata;
	    ed.fa_set3_d.edata =
		eflag != PEDGE_NONE ? edata : (Pedge_data_list *)NULL;

	    /* Compute the total number of vertices. */
	    ed.fa_set3_d.num_vertices = 0;
	    for ( i = 0; i < nfa; i++ )
		ed.fa_set3_d.num_vertices += vdata[i].num_vertices;

	    /* Check the edge flags. */
	    if (eflag != PEDGE_NONE) {
		int	num_edges = 0;
		for ( i = 0; i < nfa; i++ )
		    num_edges += edata[i].num_edges;
		if ( num_edges != ed.fa_set3_d.num_vertices ) {
		    ERR_REPORT( phg_cur_cph->erh, ERR513);
		    return;
		}
	    }
	    if ( CB_BUILD_OC(args->el_type, &ed, &args->pex_oc) )
		CP_FUNC( phg_cur_cph, CP_FUNC_OP_ADD_EL, &cp_args, NULL);
	}
    }
}


/* FILL AREA SET */
void
pfill_area_set(point_list_list)
Ppoint_list_list	*point_list_list;	/* list of 2d point lists */
{
    Phg_args		cp_args;
    Phg_el_data		ed;
    Phg_args_add_el	*args = &cp_args.data.add_el;

    register	int	i;
    
    if (CB_ENTRY_CHECK(phg_cur_cph, ERR5, Pfn_fill_area_set)) {
	if (PSL_STRUCT_STATE(phg_cur_cph->psl) != PSTRUCT_ST_STOP) {
	    ERR_REPORT(phg_cur_cph->erh, ERR5);

	} else {
	    if ( point_list_list->num_point_lists < 0 ) {
		ERR_REPORT( phg_cur_cph->erh, ERRN150);

	    } else {
		args->el_type = PELEM_FILL_AREA_SET;
		ed.fa_set.num_sets = point_list_list->num_point_lists;
		ed.fa_set.sets = point_list_list->point_lists;
		ed.fa_set.total_pts = 0;
		for ( i = 0; i < point_list_list->num_point_lists; i++ ) {
		    if ( point_list_list->point_lists[i].num_points < 0 ) {
			ERR_REPORT(phg_cur_cph->erh, ERRN150);
			return;
		    } else
			ed.fa_set.total_pts += 
			    point_list_list->point_lists[i].num_points;
		}
		if ( CB_BUILD_OC(args->el_type, &ed, &args->pex_oc) )
		    CP_FUNC(phg_cur_cph, CP_FUNC_OP_ADD_EL, &cp_args, NULL);
	    }
	}
    }
}


/* TRIANGLE STRIP 3 WITH DATA */
void
ptri_strip3_data(fflag, vflag, colour_model, nv, fdata, vdata)
Pint           	fflag;      	/* what data is specified per facet */
Pint           	vflag;          /* what data is specified per vertex */
Pint           	colour_model;   /* colour model */
Pint		nv;		/* number of vertices */
Pfacet_data_arr3  *fdata;     	/* facet data */
Pfacet_vdata_arr3 *vdata;         /* facet vertex data */
{
    Phg_args		cp_args;
    Phg_el_data		ed;
    Phg_args_add_el	*args = &cp_args.data.add_el;
    
    if (CB_ENTRY_CHECK(phg_cur_cph, ERR5, Pfn_tri_strip3_data)) {
	if (PSL_STRUCT_STATE(phg_cur_cph->psl) != PSTRUCT_ST_STOP) {
	    ERR_REPORT(phg_cur_cph->erh, ERR5);
	    return;
	}
	else if (nv < 0) {
	    ERR_REPORT(phg_cur_cph->erh, ERRN164);
	    return;
	}
	else if ((fflag < PFACET_NONE) || (fflag > PFACET_COLOUR_NORMAL)) {
	    ERR_REPORT(phg_cur_cph->erh, ERRN161);
	}
	else if ((vflag < PVERT_COORD) || (vflag > PVERT_COORD_COLOUR_NORMAL)) {
	    ERR_REPORT(phg_cur_cph->erh, ERRN162);
	}
	else {
	    args->el_type = PELEM_TRI_STRIP3_DATA;
	    ed.tri_strip3.fflag = fflag;
	    ed.tri_strip3.vflag = vflag;
	    ed.tri_strip3.colour_model = colour_model;
	    if ( fflag != PFACET_NONE ) {
	    	ed.tri_strip3.fdata.num_facets = nv - 2;
		ed.tri_strip3.fdata.facetdata = *fdata;
	    }
	    ed.tri_strip3.vdata.num_vertices = nv;
	    ed.tri_strip3.vdata.vertex_data = *vdata;
	    if ( CB_BUILD_OC(args->el_type, &ed, &args->pex_oc) )
		CP_FUNC( phg_cur_cph, CP_FUNC_OP_ADD_EL, &cp_args, NULL);
	}
    }
}

	
/* QUADRILATERAL MESH 3 WITH DATA */
void
pquad_mesh3_data(fflag, vflag, colour_model, dim, fdata, vdata)
Pint           	fflag;      	/* what data is specified per facet */
Pint           	vflag;          /* what data is specified per vertex */
Pint           	colour_model;   /* colour model */
Pint_size	*dim;		/* dimension of vertices */
Pfacet_data_arr3  *fdata;     	/* facet data */
Pfacet_vdata_arr3	*vdata;         /* facet vertex data */
{
    Phg_args		cp_args;
    Phg_el_data		ed;
    Phg_args_add_el	*args = &cp_args.data.add_el;
    
    if (CB_ENTRY_CHECK(phg_cur_cph, ERR5, Pfn_quad_mesh3_data)) {
	if (PSL_STRUCT_STATE(phg_cur_cph->psl) != PSTRUCT_ST_STOP) {
	    ERR_REPORT(phg_cur_cph->erh, ERR5);
	    return;
	}
	else if ((dim->size_x < 0) || (dim->size_y < 0)) {
	    ERR_REPORT(phg_cur_cph->erh, ERRN164);
	    return;
	}
	else if ((fflag < PFACET_NONE) || (fflag > PFACET_COLOUR_NORMAL)) {
	    ERR_REPORT(phg_cur_cph->erh, ERRN161);
	}
	else if ((vflag < PVERT_COORD) || (vflag > PVERT_COORD_COLOUR_NORMAL)) {
	    ERR_REPORT(phg_cur_cph->erh, ERRN162);
	}
	else {
	    args->el_type = PELEM_QUAD_MESH3_DATA;
	    ed.quad_mesh3.fflag = fflag;
	    ed.quad_mesh3.vflag = vflag;
	    ed.quad_mesh3.colour_model = colour_model;
	    ed.quad_mesh3.dim = *dim;
	    if ( fflag != PFACET_NONE ) {
	    	ed.quad_mesh3.fdata.num_facets =
		    (dim->size_x - 1) * (dim->size_y - 1);
	    	ed.quad_mesh3.fdata.facetdata = *fdata;
	    }
	    ed.quad_mesh3.vdata.num_vertices = dim->size_x * dim->size_y;
	    ed.quad_mesh3.vdata.vertex_data = *vdata;
	    if ( CB_BUILD_OC(args->el_type, &ed, &args->pex_oc) )
		CP_FUNC( phg_cur_cph, CP_FUNC_OP_ADD_EL, &cp_args, NULL);
	}
    }
}

	
/* SET OF FILL AREA SET 3 WITH DATA */
void
pset_of_fill_area_set3_data( fflag, eflag, vflag, colour_model, num_sets,
	fdata, edata, vlist, vdata )
    Pint		fflag;		/* data per facet flag */
    Pint		eflag;		/* data per edge flag */
    Pint		vflag;		/* data per vertex flag */
    Pint		colour_model;	/* colour model */
    Pint		num_sets;	/* number of fill area sets */
    Pfacet_data_arr3	*fdata;     	/* facet data */
    Pedge_data_list_list *edata;	/* array of L(L(E)) edge data */
    Pint_list_list	*vlist;		/* array of L(L(I)) vertex indices */
    Pfacet_vdata_list3	*vdata;         /* facet vertex data */
{
    Phg_args		cp_args;
    Phg_el_data		ed;
    Phg_args_add_el	*args = &cp_args.data.add_el;

    register int		i, j, k;
    
    if (CB_ENTRY_CHECK(phg_cur_cph, ERR5, Pfn_set_of_fill_area_set3_data)) {
	if (PSL_STRUCT_STATE(phg_cur_cph->psl) != PSTRUCT_ST_STOP) {
	    ERR_REPORT(phg_cur_cph->erh, ERR5);
	    return;
	}
	else if ( num_sets < 0 ) {
	    ERR_REPORT(phg_cur_cph->erh, ERRN164);
	    return;
	}
	else if ((fflag < PFACET_NONE) || (fflag > PFACET_COLOUR_NORMAL)) {
	    ERR_REPORT(phg_cur_cph->erh, ERRN161);
	}
	else if ((eflag < PEDGE_NONE) || (eflag > PEDGE_VISIBILITY)) {
	    ERR_REPORT(phg_cur_cph->erh, ERRN163);
	}
	else if ((vflag < PVERT_COORD) || (vflag > PVERT_COORD_COLOUR_NORMAL)) {
	    ERR_REPORT(phg_cur_cph->erh, ERRN162);
	}
	else {
	    args->el_type = PELEM_SET_OF_FILL_AREA_SET3_DATA;
	    ed.sofas3.fflag = fflag;
	    ed.sofas3.eflag = eflag;
	    ed.sofas3.vflag = vflag;
	    ed.sofas3.colour_model = colour_model;
	    ed.sofas3.num_sets = num_sets;
	    ed.sofas3.vdata = *vdata;
	    ed.sofas3.vlist = vlist;
	    if ( fflag != PFACET_NONE ) {
	    	ed.sofas3.fdata.num_facets = num_sets;
	    	ed.sofas3.fdata.facetdata = *fdata;
	    }

	    /* Compute the total number of contours and indices. */
	    ed.sofas3.num_contours = 0;
	    ed.sofas3.num_vindices = 0;
	    for ( i = 0; i < num_sets ; i++ ) {
		ed.sofas3.num_contours += vlist[i].num_lists;
		for ( j = 0; j < vlist[i].num_lists; j++ ) {
		    ed.sofas3.num_vindices += vlist[i].lists[j].num_ints;
		    /* Check for out of range vertex indices. */
		    for ( k = 0; k < vlist[i].lists[j].num_ints; k++ )
			if ( vlist[i].lists[j].ints[k] >= vdata->num_vertices
				|| vlist[i].lists[j].ints[k] < 0 ) {
			    ERR_REPORT( phg_cur_cph->erh, ERR504);
			}
		}
	    }

	    if ( eflag != PEDGE_NONE ) {
		int	num_flags;
	    	ed.sofas3.edata = edata;
		/* Compute and check the total number of edge flags. */
		for ( i = 0, num_flags = 0; i < num_sets ; i++ ) {
		    for ( j = 0; j < edata[i].num_lists; j++ )
			num_flags += edata[i].edgelist[j].num_edges;
		}
		if ( num_flags != ed.sofas3.num_vindices ) {
		    ERR_REPORT( phg_cur_cph->erh, ERR513);
		    return;
		}
	    }
	    if ( CB_BUILD_OC(args->el_type, &ed, &args->pex_oc) )
		CP_FUNC( phg_cur_cph, CP_FUNC_OP_ADD_EL, &cp_args, NULL);
	}
    }
}


/* SET INTERIOR INDEX */
void
pset_int_ind(index)
Pint	index;	/* interior index	*/
{
    Phg_args		cp_args;
    Phg_el_data		ed;
    Phg_args_add_el	*args = &cp_args.data.add_el;
    
    if (CB_ENTRY_CHECK(phg_cur_cph, ERR5, Pfn_set_int_ind)) {
	if (PSL_STRUCT_STATE(phg_cur_cph->psl) != PSTRUCT_ST_STOP) {
	    ERR_REPORT(phg_cur_cph->erh, ERR5);

	} else if (index < 1) {
	    ERR_REPORT(phg_cur_cph->erh, ERR100);

	} else {
	    args->el_type = PELEM_INT_IND;
	    ed.idata = index;
	    if ( CB_BUILD_OC(args->el_type, &ed, &args->pex_oc) )
		CP_FUNC(phg_cur_cph, CP_FUNC_OP_ADD_EL, &cp_args, NULL);
	}
    }
}


/* SET EDGE INDEX */
void
pset_edge_ind(index)
Pint	index;	/* edge index	*/
{
    Phg_args		cp_args;
    Phg_el_data		ed;
    Phg_args_add_el	*args = &cp_args.data.add_el;
    
    if (CB_ENTRY_CHECK(phg_cur_cph, ERR5, Pfn_set_edge_ind)) {
	if (PSL_STRUCT_STATE(phg_cur_cph->psl) != PSTRUCT_ST_STOP) {
	    ERR_REPORT(phg_cur_cph->erh, ERR5);

	} else if (index < 1) {
	    ERR_REPORT(phg_cur_cph->erh, ERR100);

	} else {
	    args->el_type = PELEM_EDGE_IND;
	    ed.idata = index;
	    if ( CB_BUILD_OC(args->el_type, &ed, &args->pex_oc) )
		CP_FUNC( phg_cur_cph, CP_FUNC_OP_ADD_EL, &cp_args, NULL);
	}
    }
}


/* SET INTERIOR STYLE */
void
pset_int_style(style)
Pint_style	style;	/* interior style	*/
{
    Phg_args		cp_args;
    Phg_el_data		ed;
    Phg_args_add_el	*args = &cp_args.data.add_el;
    
    if (CB_ENTRY_CHECK(phg_cur_cph, ERR5, Pfn_set_int_style)) {
	if (PSL_STRUCT_STATE(phg_cur_cph->psl) != PSTRUCT_ST_STOP) {
	    ERR_REPORT(phg_cur_cph->erh, ERR5);

	} else {
	    args->el_type = PELEM_INT_STYLE;
	    ed.interstyle = style;
	    if ( CB_BUILD_OC(args->el_type, &ed, &args->pex_oc) )
		CP_FUNC( phg_cur_cph, CP_FUNC_OP_ADD_EL, &cp_args, NULL);
	}
    }
}


/* SET BACK INTERIOR STYLE */
void
pset_back_int_style(style)
Pint_style	style;	/* back interior style	*/
{
    Phg_args		cp_args;
    Phg_el_data		ed;
    Phg_args_add_el	*args = &cp_args.data.add_el;
    
    if (CB_ENTRY_CHECK(phg_cur_cph, ERR5, Pfn_set_back_int_style)) {
	if (PSL_STRUCT_STATE(phg_cur_cph->psl) != PSTRUCT_ST_STOP) {
	    ERR_REPORT(phg_cur_cph->erh, ERR5);

	} else {
	    args->el_type = PELEM_BACK_INT_STYLE;
	    ed.interstyle = style;
	    if ( CB_BUILD_OC(args->el_type, &ed, &args->pex_oc) )
		CP_FUNC(phg_cur_cph, CP_FUNC_OP_ADD_EL, &cp_args, NULL);
	}
    }
}


/* SET INTERIOR STYLE INDEX */
void
pset_int_style_ind(index)
Pint	index;	/* interior style index	*/
{
    Phg_args		cp_args;
    Phg_el_data		ed;
    Phg_args_add_el	*args = &cp_args.data.add_el;
    
    if (CB_ENTRY_CHECK(phg_cur_cph, ERR5, Pfn_set_int_style_ind)) {
	if (PSL_STRUCT_STATE(phg_cur_cph->psl) != PSTRUCT_ST_STOP) {
	    ERR_REPORT(phg_cur_cph->erh, ERR5);

	} else {
	    args->el_type = PELEM_INT_STYLE_IND;
	    ed.idata = index;
	    if ( CB_BUILD_OC(args->el_type, &ed, &args->pex_oc) )
		CP_FUNC(phg_cur_cph, CP_FUNC_OP_ADD_EL, &cp_args, NULL);
	}
    }
}


/* SET BACK INTERIOR STYLE INDEX */
void
pset_back_int_style_ind(index)
Pint	index;	/* back interior sytle index	*/
{
    Phg_args		cp_args;
    Phg_el_data		ed;
    Phg_args_add_el	*args = &cp_args.data.add_el;

    if (CB_ENTRY_CHECK(phg_cur_cph, ERR5, Pfn_set_back_int_style_ind)) {
	if (PSL_STRUCT_STATE(phg_cur_cph->psl) != PSTRUCT_ST_STOP) {
	    ERR_REPORT(phg_cur_cph->erh, ERR5);

	} else {
	    args->el_type = PELEM_BACK_INT_STYLE_IND;
	    ed.idata = index;
	    if ( CB_BUILD_OC(args->el_type, &ed, &args->pex_oc) )
		CP_FUNC( phg_cur_cph, CP_FUNC_OP_ADD_EL, &cp_args, NULL);
	}
    }
}


/* SET INTERIOR COLOUR INDEX */
void
pset_int_colr_ind(index)
Pint	index;	/* interior colour index	*/
{
    Phg_args		cp_args;
    Phg_el_data		ed;
    Phg_args_add_el	*args = &cp_args.data.add_el;
    
    if (CB_ENTRY_CHECK(phg_cur_cph, ERR5, Pfn_set_int_colr_ind)) {
	if (PSL_STRUCT_STATE(phg_cur_cph->psl) != PSTRUCT_ST_STOP) {
	    ERR_REPORT(phg_cur_cph->erh, ERR5);

	} else if (index < 0) {
	    ERR_REPORT(phg_cur_cph->erh, ERR113);

	} else {
	    args->el_type = PELEM_INT_COLR_IND;
	    ed.idata = index;
	    if ( CB_BUILD_OC(args->el_type, &ed, &args->pex_oc) )
		CP_FUNC(phg_cur_cph, CP_FUNC_OP_ADD_EL, &cp_args, NULL);
	}
    }
}


/* SET INTERIOR COLOUR */
void
pset_int_colr(colour)
Pgcolr	*colour;	/* interior colour */
{
    Phg_args		cp_args;
    Phg_el_data		ed;
    Phg_args_add_el	*args = &cp_args.data.add_el;
    
    if (CB_ENTRY_CHECK(phg_cur_cph, ERR5, Pfn_set_int_colr)) {
	if (PSL_STRUCT_STATE(phg_cur_cph->psl) != PSTRUCT_ST_STOP) {
	    ERR_REPORT(phg_cur_cph->erh, ERR5);

	} else {
	    args->el_type = PELEM_INT_COLR;
	    ed.colour = *colour;
	    if ( CB_BUILD_OC(args->el_type, &ed, &args->pex_oc) )
		CP_FUNC(phg_cur_cph, CP_FUNC_OP_ADD_EL, &cp_args, NULL);
	}
    }
}


/* SET BACK INTERIOR COLOUR */
void
pset_back_int_colr(colour)
Pgcolr	*colour;	/* back interior colour */
{
    Phg_args		cp_args;
    Phg_el_data		ed;
    Phg_args_add_el	*args = &cp_args.data.add_el;
    
    if (CB_ENTRY_CHECK(phg_cur_cph, ERR5, Pfn_set_back_int_colr)) {
	if (PSL_STRUCT_STATE(phg_cur_cph->psl) != PSTRUCT_ST_STOP) {
	    ERR_REPORT(phg_cur_cph->erh, ERR5);

	} else {
	    args->el_type = PELEM_BACK_INT_COLR;
	    ed.colour = *colour;
	    if ( CB_BUILD_OC(args->el_type, &ed, &args->pex_oc) )
		CP_FUNC(phg_cur_cph, CP_FUNC_OP_ADD_EL, &cp_args, NULL);
	}
    }
}


/* SET EDGE FLAG */
void
pset_edge_flag(edge_flag)
Pedge_flag	edge_flag;	/* edge flag	*/
{
    Phg_args		cp_args;
    Phg_el_data		ed;
    Phg_args_add_el	*args = &cp_args.data.add_el;
    
    if (CB_ENTRY_CHECK(phg_cur_cph, ERR5, Pfn_set_edge_flag)) {
	if (PSL_STRUCT_STATE(phg_cur_cph->psl) != PSTRUCT_ST_STOP) {
	    ERR_REPORT(phg_cur_cph->erh, ERR5);

	} else {
	    args->el_type = PELEM_EDGE_FLAG;
	    ed.edgef = edge_flag;
	    if ( CB_BUILD_OC(args->el_type, &ed, &args->pex_oc) )
		CP_FUNC(phg_cur_cph, CP_FUNC_OP_ADD_EL, &cp_args, NULL);
	}
    }
}


/* SET EDGE TYPE */
void
pset_edgetype(edgetype)
Pint	edgetype;	/* edgetype	*/
{
    Phg_args		cp_args;
    Phg_el_data		ed;
    Phg_args_add_el	*args = &cp_args.data.add_el;
    
    if (CB_ENTRY_CHECK(phg_cur_cph, ERR5, Pfn_set_edgetype)) {
	if (PSL_STRUCT_STATE(phg_cur_cph->psl) != PSTRUCT_ST_STOP) {
	    ERR_REPORT(phg_cur_cph->erh, ERR5);

	} else {
	    args->el_type = PELEM_EDGETYPE;
	    ed.idata = edgetype;
	    if ( CB_BUILD_OC(args->el_type, &ed, &args->pex_oc) )
		CP_FUNC(phg_cur_cph, CP_FUNC_OP_ADD_EL, &cp_args, NULL);
	}
    }
}


/* SET EDGE WIDTH SCALE FACTOR */
void
pset_edgewidth(scale)
Pfloat	scale;	/* edgewidth scale factor	*/
{
    Phg_args		cp_args;
    Phg_el_data		ed;
    Phg_args_add_el	*args = &cp_args.data.add_el;
    
    if (CB_ENTRY_CHECK(phg_cur_cph, ERR5, Pfn_set_edgewidth)) {
	if (PSL_STRUCT_STATE(phg_cur_cph->psl) != PSTRUCT_ST_STOP) {
	    ERR_REPORT(phg_cur_cph->erh, ERR5);

	} else {
	    args->el_type = PELEM_EDGEWIDTH;;
	    ed.fdata = scale;
	    if ( CB_BUILD_OC(args->el_type, &ed, &args->pex_oc) )
		CP_FUNC( phg_cur_cph, CP_FUNC_OP_ADD_EL, &cp_args, NULL);
	}
    }
}


/* SET EDGE COLOUR INDEX */
void
pset_edge_colr_ind(index)
Pint	index;	/* edge colour index	*/
{
    Phg_args		cp_args;
    Phg_el_data		ed;
    Phg_args_add_el	*args = &cp_args.data.add_el;
    
    if (CB_ENTRY_CHECK(phg_cur_cph, ERR5, Pfn_set_edge_colr_ind)) {
	if (PSL_STRUCT_STATE(phg_cur_cph->psl) != PSTRUCT_ST_STOP) {
	    ERR_REPORT(phg_cur_cph->erh, ERR5);

	} else if (index < 0) {
	    ERR_REPORT(phg_cur_cph->erh, ERR113);	    

	} else {
	    args->el_type = PELEM_EDGE_COLR_IND;
	    ed.idata = index;
	    if ( CB_BUILD_OC(args->el_type, &ed, &args->pex_oc) )
		CP_FUNC( phg_cur_cph, CP_FUNC_OP_ADD_EL, &cp_args, NULL);
	}
    }
}


/* SET EDGE COLOUR */
void
pset_edge_colr(colour)
Pgcolr	*colour;	/* edge colour */
{
    Phg_args		cp_args;
    Phg_el_data		ed;
    Phg_args_add_el	*args = &cp_args.data.add_el;
    
    if (CB_ENTRY_CHECK(phg_cur_cph, ERR5, Pfn_set_edge_colr)) {
	if (PSL_STRUCT_STATE(phg_cur_cph->psl) != PSTRUCT_ST_STOP) {
	    ERR_REPORT(phg_cur_cph->erh, ERR5);

	} else {
	    args->el_type = PELEM_EDGE_COLR;
	    ed.colour = *colour;
	    if ( CB_BUILD_OC(args->el_type, &ed, &args->pex_oc) )
		CP_FUNC( phg_cur_cph, CP_FUNC_OP_ADD_EL, &cp_args, NULL);
	}
    }
}


/* SET PATTERN SIZE */
void
pset_pat_size(size_x, size_y)
    Pfloat	size_x;
    Pfloat	size_y;
{
    Phg_args		cp_args;
    Phg_el_data		ed;
    Phg_args_add_el	*args = &cp_args.data.add_el;
    
    if (CB_ENTRY_CHECK(phg_cur_cph, ERR5, Pfn_set_pat_size)) {
	if (PSL_STRUCT_STATE(phg_cur_cph->psl) != PSTRUCT_ST_STOP) {
	    ERR_REPORT(phg_cur_cph->erh, ERR5);

	} else {
	    args->el_type = PELEM_PAT_SIZE;
	    ed.pt.x = size_x;
	    ed.pt.y = size_y;
	    if ( CB_BUILD_OC(args->el_type, &ed, &args->pex_oc) )
		CP_FUNC( phg_cur_cph, CP_FUNC_OP_ADD_EL, &cp_args, NULL);
	}
    }
}


/* SET PATTERN REFERENCE POINT VECTORS */
void
pset_pat_ref_point_vecs(ref_pt, pat_ref_vec)
Ppoint3	*ref_pt;	/* pattern reference point	*/
Pvec3	pat_ref_vec[2];/* direction vector 1; X axis of pattern	*/
{
    Phg_args		cp_args;
    Phg_el_data		ed;
    Phg_args_add_el	*args = &cp_args.data.add_el;
    
    if (CB_ENTRY_CHECK(phg_cur_cph, ERR5, Pfn_set_pat_ref_point_vecs)) {
	if (PSL_STRUCT_STATE(phg_cur_cph->psl) != PSTRUCT_ST_STOP) {
	    ERR_REPORT(phg_cur_cph->erh, ERR5);

	} else {
	    args->el_type = PELEM_PAT_REF_POINT_VECS;
	    ed.pat_pt_vecs.pt = *ref_pt;
	    ed.pat_pt_vecs.vecs[0] = pat_ref_vec[0];
	    ed.pat_pt_vecs.vecs[1] = pat_ref_vec[1];
	    if ( CB_BUILD_OC(args->el_type, &ed, &args->pex_oc) )
		CP_FUNC( phg_cur_cph, CP_FUNC_OP_ADD_EL, &cp_args, NULL);
	}
    }
}


/* SET PATTERN REFERENCE POINT */
void
pset_pat_ref_point(ref_pt)
Ppoint	*ref_pt;	/* pattern reference point	*/
{
    Phg_args		cp_args;
    Phg_el_data		ed;
    Phg_args_add_el	*args = &cp_args.data.add_el;
    
    if (CB_ENTRY_CHECK(phg_cur_cph, ERR5, Pfn_set_pat_ref_point)) {
	if (PSL_STRUCT_STATE(phg_cur_cph->psl) != PSTRUCT_ST_STOP) {
	    ERR_REPORT(phg_cur_cph->erh, ERR5);

	} else {
	    args->el_type = PELEM_PAT_REF_POINT;
	    ed.pt = *ref_pt;
	    if ( CB_BUILD_OC(args->el_type, &ed, &args->pex_oc) )
		CP_FUNC( phg_cur_cph, CP_FUNC_OP_ADD_EL, &cp_args, NULL);
	}
    }
}


/* SET INTERIOR SHADING METHOD */
void
pset_int_shad_meth( method )
    Pint	method;		/* shading method */
{
    Phg_args		cp_args;
    Phg_el_data		ed;
    Phg_args_add_el	*args = &cp_args.data.add_el;

    if (CB_ENTRY_CHECK(phg_cur_cph, ERR5, Pfn_set_int_shad_meth)) {
        if (PSL_STRUCT_STATE(phg_cur_cph->psl) != PSTRUCT_ST_STOP) {    
            ERR_REPORT(phg_cur_cph->erh, ERR5); 

        } else {
            args->el_type = PELEM_INT_SHAD_METH;
            ed.idata = method;
	    if ( CB_BUILD_OC(args->el_type, &ed, &args->pex_oc) )
		CP_FUNC( phg_cur_cph, CP_FUNC_OP_ADD_EL, &cp_args, NULL);
        }
    } 
}


/* SET BACK INTERIOR SHADING METHOD */
void
pset_back_int_shad_meth( method )
    Pint	method;		/* back shading method */
{
    Phg_args		cp_args;
    Phg_el_data		ed;
    Phg_args_add_el	*args = &cp_args.data.add_el;

    if (CB_ENTRY_CHECK(phg_cur_cph, ERR5, Pfn_set_back_int_shad_meth)) {
        if (PSL_STRUCT_STATE(phg_cur_cph->psl) != PSTRUCT_ST_STOP) {    
            ERR_REPORT(phg_cur_cph->erh, ERR5); 

        } else {
            args->el_type = PELEM_BACK_INT_SHAD_METH;
            ed.idata = method;
	    if ( CB_BUILD_OC(args->el_type, &ed, &args->pex_oc) )
		CP_FUNC( phg_cur_cph, CP_FUNC_OP_ADD_EL, &cp_args, NULL);
        }
    } 
}


/* SET FACE DISTINGUISHING MODE */
void
pset_face_disting_mode(mode)
Pdisting_mode	mode;	/* distinguishing mode */
{
    Phg_args		cp_args;
    Phg_el_data		ed;
    Phg_args_add_el	*args = &cp_args.data.add_el;

    if (CB_ENTRY_CHECK(phg_cur_cph, ERR5, Pfn_set_face_disting_mode)) {
    	if (PSL_STRUCT_STATE(phg_cur_cph->psl) != PSTRUCT_ST_STOP) {
	    ERR_REPORT(phg_cur_cph->erh, ERR5);

	} else {
	    args->el_type = PELEM_FACE_DISTING_MODE;
	    ed.idata = (int)mode;
	    if ( CB_BUILD_OC(args->el_type, &ed, &args->pex_oc) )
		CP_FUNC( phg_cur_cph, CP_FUNC_OP_ADD_EL, &cp_args, NULL);
	}
    }
}


/* SET FACE CULLING MODE */
void
pset_face_cull_mode(mode)
Pcull_mode	mode;	/* culling mode */
{
    Phg_args		cp_args;
    Phg_el_data		ed;
    Phg_args_add_el	*args = &cp_args.data.add_el;

    if (CB_ENTRY_CHECK(phg_cur_cph, ERR5, Pfn_set_face_cull_mode)) {
    	if (PSL_STRUCT_STATE(phg_cur_cph->psl) != PSTRUCT_ST_STOP) {
	    ERR_REPORT(phg_cur_cph->erh, ERR5);

	} else {
	    args->el_type = PELEM_FACE_CULL_MODE;
	    ed.idata = (int)mode;
	    if ( CB_BUILD_OC(args->el_type, &ed, &args->pex_oc) )
		CP_FUNC( phg_cur_cph, CP_FUNC_OP_ADD_EL, &cp_args, NULL);
	}
    }
}


/* SET AREA PROPERTIES */
void
pset_refl_props(properties)
Prefl_props	*properties;	/* surface area properties */
{   
    Phg_args		cp_args;
    Phg_el_data		ed;
    Phg_args_add_el	*args = &cp_args.data.add_el;
    
    if (CB_ENTRY_CHECK(phg_cur_cph, ERR5, Pfn_set_area_prop)) {
	if (PSL_STRUCT_STATE(phg_cur_cph->psl) != PSTRUCT_ST_STOP) {
	    ERR_REPORT(phg_cur_cph->erh, ERR5);	

	} else {
	    args->el_type = PELEM_REFL_PROPS;
	    ed.refl_props = *properties;
	    if ( CB_BUILD_OC(args->el_type, &ed, &args->pex_oc) )
		CP_FUNC( phg_cur_cph, CP_FUNC_OP_ADD_EL, &cp_args, NULL);
	}
    }
}


/* SET BACK AREA PROPERTIES */
void
pset_back_refl_props(properties)
Prefl_props	*properties;	/* back surface area properties */
{   
    Phg_args		cp_args;
    Phg_el_data		ed;
    Phg_args_add_el	*args = &cp_args.data.add_el;
    
    if (CB_ENTRY_CHECK(phg_cur_cph, ERR5, Pfn_set_back_area_prop)) {
	if (PSL_STRUCT_STATE(phg_cur_cph->psl) != PSTRUCT_ST_STOP) {
	    ERR_REPORT(phg_cur_cph->erh, ERR5);	

	} else {
	    args->el_type = PELEM_BACK_REFL_PROPS;
	    ed.refl_props = *properties;
	    if ( CB_BUILD_OC(args->el_type, &ed, &args->pex_oc) )
		CP_FUNC( phg_cur_cph, CP_FUNC_OP_ADD_EL, &cp_args, NULL);
	}
    }
}

