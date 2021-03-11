/* $XConsortium: cb_plin.c,v 5.2 94/04/17 20:40:52 rws Exp $ */

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

/* Polyline functions for the PHIGS C binding */

#include "phg.h"
#include "cp.h"
#include "cb_priv.h"

void
ppolyline3( point_list )
    Ppoint_list3	*point_list;	/* list of points */
{
    Phg_args		cp_args;
    Phg_el_data		ed;
    Phg_args_add_el	*args = &cp_args.data.add_el;
    
    if (CB_ENTRY_CHECK(phg_cur_cph, ERR5, Pfn_polyline3)) {
	if (PSL_STRUCT_STATE(phg_cur_cph->psl) != PSTRUCT_ST_STOP) {
	    ERR_REPORT(phg_cur_cph->erh, ERR5);	
	}
	else if (point_list->num_points < 0) {
	    ERR_REPORT(phg_cur_cph->erh, ERRN150);	
	}
	else {
	    args->el_type = PELEM_POLYLINE3;
	    ed.ptlst3 = *point_list;
	    if ( CB_BUILD_OC(args->el_type, &ed, &args->pex_oc) )
		CP_FUNC(phg_cur_cph, CP_FUNC_OP_ADD_EL, &cp_args, NULL);
	}
    }
}

void
ppolyline_set3_data(vflag, colour_model, npl, vdata)
Pint	        	vflag;		/* data per vertex flag */
Pint	        	colour_model;	/* colour model */
Pint	        	npl;		/* number of polylines in the set */
Pline_vdata_list3	*vdata;		/* pointer to line vertext data list */
{
    Phg_args		cp_args;
    Phg_el_data		ed;
    Phg_args_add_el	*args = &cp_args.data.add_el;

    register int	i;
    
    if (CB_ENTRY_CHECK(phg_cur_cph, ERR5, Pfn_polyline_set3_data)) {
	if (PSL_STRUCT_STATE(phg_cur_cph->psl) != PSTRUCT_ST_STOP) {
	    ERR_REPORT(phg_cur_cph->erh, ERR5);	

	} else if (npl < 0) {
	    ERR_REPORT(phg_cur_cph->erh, ERRN150);	

	} else {
	    args->el_type = PELEM_POLYLINE_SET3_DATA;
	    ed.pl_set3_d.vflag = vflag;
	    ed.pl_set3_d.colour_model = colour_model;
	    ed.pl_set3_d.num_sets = npl;
	    ed.pl_set3_d.vdata = (Pfacet_vdata_list3 *)vdata;
	    /* Compute the total number of vertices. */
	    ed.pl_set3_d.num_vertices = 0;
	    for ( i = 0; i < npl; i++ )
		ed.pl_set3_d.num_vertices += vdata[i].num_vertices;
	    if ( CB_BUILD_OC(args->el_type, &ed, &args->pex_oc) )
		CP_FUNC( phg_cur_cph, CP_FUNC_OP_ADD_EL, &cp_args, NULL);
	}
    }
}


void
ppolyline( point_list )
    Ppoint_list		*point_list;
{   
    Phg_args		cp_args;
    Phg_el_data		ed;
    Phg_args_add_el	*args = &cp_args.data.add_el;
    
    if (CB_ENTRY_CHECK(phg_cur_cph, ERR5, Pfn_polyline)) {
	if (PSL_STRUCT_STATE(phg_cur_cph->psl) != PSTRUCT_ST_STOP) {
	    ERR_REPORT(phg_cur_cph->erh, ERR5);	
	}
	else if (point_list->num_points < 0) {
	    ERR_REPORT(phg_cur_cph->erh, ERRN150);	
	}
	else {
	    args->el_type = PELEM_POLYLINE;
	    ed.ptlst = *point_list;
	    if ( CB_BUILD_OC(args->el_type, &ed, &args->pex_oc) )
		CP_FUNC( phg_cur_cph, CP_FUNC_OP_ADD_EL, &cp_args, NULL);
	}
    }
}



void
pset_linetype(linetype)
Pint	linetype;	/* linetype	*/
{   
    Phg_args		cp_args;
    Phg_el_data		ed;
    Phg_args_add_el	*args = &cp_args.data.add_el;
    
    if (CB_ENTRY_CHECK(phg_cur_cph, ERR5, Pfn_set_linetype)) {
	if (PSL_STRUCT_STATE(phg_cur_cph->psl) != PSTRUCT_ST_STOP) {
	    ERR_REPORT(phg_cur_cph->erh, ERR5);	
	}
	else {
	    args->el_type = PELEM_LINETYPE;
	    ed.idata = linetype;
	    if ( CB_BUILD_OC(args->el_type, &ed, &args->pex_oc) )
		CP_FUNC( phg_cur_cph, CP_FUNC_OP_ADD_EL, &cp_args, NULL);
	}
    }
}


void
pset_linewidth(width)
Pfloat	width;	/* linewidth scale factor	*/
{   
    Phg_args		cp_args;
    Phg_el_data		ed;
    Phg_args_add_el	*args = &cp_args.data.add_el;

    if (CB_ENTRY_CHECK(phg_cur_cph, ERR5, Pfn_set_linewidth)) {
	if (PSL_STRUCT_STATE(phg_cur_cph->psl) != PSTRUCT_ST_STOP) {
	    ERR_REPORT(phg_cur_cph->erh, ERR5);	
	}
	else {
	    args->el_type = PELEM_LINEWIDTH;
	    ed.fdata = width;
	    if ( CB_BUILD_OC(args->el_type, &ed, &args->pex_oc) )
		CP_FUNC(phg_cur_cph, CP_FUNC_OP_ADD_EL, &cp_args, NULL);
	}
    }
}



void
pset_line_colr_ind(colour)
Pint	colour;	/* polyline colour index	*/
{   
    Phg_args		cp_args;
    Phg_el_data		ed;
    Phg_args_add_el	*args = &cp_args.data.add_el;
    
    if (CB_ENTRY_CHECK(phg_cur_cph, ERR5, Pfn_set_line_colr_ind)) {
	if (PSL_STRUCT_STATE(phg_cur_cph->psl) != PSTRUCT_ST_STOP) {
	    ERR_REPORT(phg_cur_cph->erh, ERR5);	
	}
	else if (colour < 0) {
	    ERR_REPORT(phg_cur_cph->erh, ERR113);
	}
	else {
	    args->el_type = PELEM_LINE_COLR_IND;
	    ed.idata = colour;
	    if ( CB_BUILD_OC(args->el_type, &ed, &args->pex_oc) )
		CP_FUNC( phg_cur_cph, CP_FUNC_OP_ADD_EL, &cp_args, NULL);
	}
    }
}

void
pset_line_colr(colour)
Pgcolr	*colour;	/* polyline colour */
{   
    Phg_args		cp_args;
    Phg_el_data		ed;
    Phg_args_add_el	*args = &cp_args.data.add_el;
    
    if (CB_ENTRY_CHECK(phg_cur_cph, ERR5, Pfn_set_line_colr)) {
	if (PSL_STRUCT_STATE(phg_cur_cph->psl) != PSTRUCT_ST_STOP) {
	    ERR_REPORT(phg_cur_cph->erh, ERR5);	
	}
	else {
	    args->el_type = PELEM_LINE_COLR;
	    ed.colour = *colour;
	    if ( CB_BUILD_OC(args->el_type, &ed, &args->pex_oc) )
		CP_FUNC( phg_cur_cph, CP_FUNC_OP_ADD_EL, &cp_args, NULL);
	}
    }
}

void
pset_line_ind( index)
    Pint	index;	/* polyline index	*/
{   
    Phg_args		cp_args;
    Phg_el_data		ed;
    Phg_args_add_el	*args = &cp_args.data.add_el;
    
    if (CB_ENTRY_CHECK(phg_cur_cph, ERR5, Pfn_set_line_ind)) {
	if (PSL_STRUCT_STATE(phg_cur_cph->psl) != PSTRUCT_ST_STOP) {
	    ERR_REPORT(phg_cur_cph->erh, ERR5);	
	}
	else if (index < 1) {
	    ERR_REPORT(phg_cur_cph->erh, ERR100);	
	}
	else {
	    args->el_type = PELEM_LINE_IND;
	    ed.idata = index;
	    if ( CB_BUILD_OC(args->el_type, &ed, &args->pex_oc) )
		CP_FUNC( phg_cur_cph, CP_FUNC_OP_ADD_EL, &cp_args, NULL);
	}
    }
}


void
pset_line_shad_meth( method )
   Pint		method;	/* shading method */
{
    Phg_args		cp_args;
    Phg_el_data		ed;
    Phg_args_add_el	*args = &cp_args.data.add_el;

    if (CB_ENTRY_CHECK(phg_cur_cph, ERR5, Pfn_set_line_shad_meth)) {
	if (PSL_STRUCT_STATE(phg_cur_cph->psl) != PSTRUCT_ST_STOP) {
	    ERR_REPORT(phg_cur_cph->erh, ERR5);	
	}
	else {
	    args->el_type = PELEM_LINE_SHAD_METH;
	    ed.idata = method;
	    if ( CB_BUILD_OC(args->el_type, &ed, &args->pex_oc) )
		CP_FUNC( phg_cur_cph, CP_FUNC_OP_ADD_EL, &cp_args, NULL);
	}
    }
}
