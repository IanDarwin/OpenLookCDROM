/* $XConsortium: cb_pmrk.c,v 5.2 94/04/17 20:40:53 rws Exp $ */

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

/* Polymarker functions for the PHIGS C binding */


#include "phg.h"
#include "cp.h"
#include "cb_priv.h"

/* POLYMARKER 3 */
void
ppolymarker3( point_list )
    Ppoint_list3	*point_list;	/* list of points */
{   
    Phg_args		cp_args;
    Phg_el_data		ed;
    Phg_args_add_el	*args = &cp_args.data.add_el;
    
    if (CB_ENTRY_CHECK(phg_cur_cph, ERR5, Pfn_polymarker3)) {
	if (PSL_STRUCT_STATE(phg_cur_cph->psl) != PSTRUCT_ST_STOP) {
	    ERR_REPORT(phg_cur_cph->erh, ERR5);	
	}
	else if (point_list->num_points < 0) {
	    ERR_REPORT(phg_cur_cph->erh, ERRN150);	
	}
	else {
	    args->el_type = PELEM_POLYMARKER3;
	    ed.ptlst3 = *point_list;
	    if ( CB_BUILD_OC(args->el_type, &ed, &args->pex_oc) )
		CP_FUNC( phg_cur_cph, CP_FUNC_OP_ADD_EL, &cp_args, NULL);
	}
    }
}


/* POLYMARKER */
void
ppolymarker( point_list )
    Ppoint_list		*point_list;
{   
    Phg_args		cp_args;
    Phg_el_data		ed;
    Phg_args_add_el	*args = &cp_args.data.add_el;
    
    if (CB_ENTRY_CHECK(phg_cur_cph, ERR5, Pfn_polymarker)) {
	if (PSL_STRUCT_STATE(phg_cur_cph->psl) != PSTRUCT_ST_STOP) {
	    ERR_REPORT(phg_cur_cph->erh, ERR5);	
	}
	else if (point_list->num_points < 0) {
	    ERR_REPORT(phg_cur_cph->erh, ERRN150);	
	}
	else {
	    args->el_type = PELEM_POLYMARKER;
	    ed.ptlst = *point_list;
	    if ( CB_BUILD_OC(args->el_type, &ed, &args->pex_oc) )
		CP_FUNC( phg_cur_cph, CP_FUNC_OP_ADD_EL, &cp_args, NULL);
	}
    }
}


/* SET POLYMARKER TYPE */
void
pset_marker_type(markertype)
Pint	markertype;	/* markertype	*/
{   
    Phg_args		cp_args;
    Phg_el_data		ed;
    Phg_args_add_el	*args = &cp_args.data.add_el;
    
    if (CB_ENTRY_CHECK(phg_cur_cph, ERR5, Pfn_set_marker_type)) {
	if (PSL_STRUCT_STATE(phg_cur_cph->psl) != PSTRUCT_ST_STOP) {
	    ERR_REPORT(phg_cur_cph->erh, ERR5);	
	}
	else {
	    args->el_type = PELEM_MARKER_TYPE;
	    ed.idata = markertype;
	    if ( CB_BUILD_OC(args->el_type, &ed, &args->pex_oc) )
		CP_FUNC( phg_cur_cph, CP_FUNC_OP_ADD_EL, &cp_args, NULL);
	}
    }
}


/* SET POLYMARKER SCALE FACTOR */
void
pset_marker_size(size)
Pfloat	size;	/* markersize scale factor	*/
{   
    Phg_args		cp_args;
    Phg_el_data		ed;
    Phg_args_add_el	*args = &cp_args.data.add_el;
    
    if (CB_ENTRY_CHECK(phg_cur_cph, ERR5, Pfn_set_marker_size)) {
	if (PSL_STRUCT_STATE(phg_cur_cph->psl) != PSTRUCT_ST_STOP) {
	    ERR_REPORT(phg_cur_cph->erh, ERR5);	
	}
	else {
	    args->el_type = PELEM_MARKER_SIZE;
	    ed.fdata = size;
	    if ( CB_BUILD_OC(args->el_type, &ed, &args->pex_oc) )
		CP_FUNC(phg_cur_cph, CP_FUNC_OP_ADD_EL, &cp_args, NULL);
	}
    }
}


/* SET POLYMARKER COLOUR INDICES */
void
pset_marker_colr_ind(colour)
Pint	colour;	/* polymarker colour index	*/
{   
    Phg_args		cp_args;
    Phg_el_data		ed;
    Phg_args_add_el	*args = &cp_args.data.add_el;
    
    if (CB_ENTRY_CHECK(phg_cur_cph, ERR5, Pfn_set_marker_colr_ind)) {
	if (PSL_STRUCT_STATE(phg_cur_cph->psl) != PSTRUCT_ST_STOP) {
	    ERR_REPORT(phg_cur_cph->erh, ERR5);	
	}
	else if (colour < 0) {
	    ERR_REPORT(phg_cur_cph->erh, ERR113);
	}
	else {
	    args->el_type = PELEM_MARKER_COLR_IND;
	    ed.idata = colour;
	    if ( CB_BUILD_OC(args->el_type, &ed, &args->pex_oc) )
		CP_FUNC( phg_cur_cph, CP_FUNC_OP_ADD_EL, &cp_args, NULL);
	}
    }
}


/* SET POLYMARKER COLOUR */
void
pset_marker_colr(colour)
Pgcolr	*colour;	/* polymarker colour */
{   
    Phg_args		cp_args;
    Phg_el_data		ed;
    Phg_args_add_el	*args = &cp_args.data.add_el;
    
    if (CB_ENTRY_CHECK(phg_cur_cph, ERR5, Pfn_set_marker_colr)) {
	if (PSL_STRUCT_STATE(phg_cur_cph->psl) != PSTRUCT_ST_STOP) {
	    ERR_REPORT(phg_cur_cph->erh, ERR5);	
	}
	else {
	    args->el_type = PELEM_MARKER_COLR;
	    ed.colour = *colour;
	    if ( CB_BUILD_OC(args->el_type, &ed, &args->pex_oc) )
		CP_FUNC( phg_cur_cph, CP_FUNC_OP_ADD_EL, &cp_args, NULL);
	}
    }
}


/* SET POLYMARKER INDEX */
void
pset_marker_ind(index)
Pint	index;	/* polymarker index	*/
{   
    Phg_args		cp_args;
    Phg_el_data		ed;
    Phg_args_add_el	*args = &cp_args.data.add_el;
    
    if (CB_ENTRY_CHECK(phg_cur_cph, ERR5, Pfn_set_marker_ind)) {
	if (PSL_STRUCT_STATE(phg_cur_cph->psl) != PSTRUCT_ST_STOP) {
	    ERR_REPORT(phg_cur_cph->erh, ERR5);	
	}
	else if (index < 1) {
	    ERR_REPORT(phg_cur_cph->erh, ERR100);	
	}
	else {
	    args->el_type = PELEM_MARKER_IND;
	    ed.idata = index;
	    if ( CB_BUILD_OC(args->el_type, &ed, &args->pex_oc) )
		CP_FUNC( phg_cur_cph, CP_FUNC_OP_ADD_EL, &cp_args, NULL);
	}
    }
}

