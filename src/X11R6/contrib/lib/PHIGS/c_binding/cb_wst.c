/* $XConsortium: cb_wst.c,v 5.10 94/04/17 20:41:00 mor Exp $ */

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

/* Workstation type functions for the PHIGS C binding */

#include "phg.h"
#include "cp.h"
#include "cb_priv.h"
#include <X11/Xfuncproto.h>
#if NeedVarargsPrototypes
# include <stdarg.h>
# define Va_start(a,b) va_start(a,b)
#else
# include <varargs.h>
# define Va_start(a,b) va_start(a)
#endif

#if NeedVarargsPrototypes
caddr_t
phigs_ws_type_set( Wst *wst, ... )
#else
/*VARARGS2*/
caddr_t
phigs_ws_type_set( wst, va_alist )
    Wst	*wst;
    va_dcl
#endif
{
    caddr_t	avlist[PHG_ATTR_STANDARD_SIZE], status = NULL;
    va_list     valist;

    if ( CB_ENTRY_CHECK( phg_cur_cph, ERR2, Pfn_phigs_ws_type_set)) {
	if ( !phg_cb_wst_exists( wst)) {
	    ERR_REPORT( phg_cur_cph->erh, ERR52);

	} else if ( wst->bound_status != WST_UNBOUND) {
	    /* Only operate on UNBOUND types */
	    ERR_REPORT( phg_cur_cph->erh, ERRN100);

	} else {
	    Va_start( valist,wst);
	    phg_attr_make( avlist, PHG_ATTR_STANDARD_SIZE, valist);
	    va_end( valist);
	    status = phg_wst_set_attrs( wst, avlist);
	}
    }
    return status;
}

caddr_t
phigs_ws_type_get( wst, attr, arg)
    Wst			*wst;
    Phigs_ws_type_attr	attr;
    char		*arg;
{
    register caddr_t	status = NULL;

    if ( CB_ENTRY_CHECK( phg_cur_cph, ERR2, Pfn_phigs_ws_type_get)) {
	if ( !phg_cb_wst_exists( wst)) {
	    ERR_REPORT( phg_cur_cph->erh, ERR52);

	} else {
	    status = phg_wst_get_attr( wst, attr, arg);
	}
    }
    return status;
}

#if NeedVarargsPrototypes
Pint
phigs_ws_type_create( Pint base_type, ... )
#else
/*VARARGS2*/
Pint
phigs_ws_type_create( base_type, va_alist )
    Pint	base_type;
    va_dcl
#endif
{
    Wst		*base = (Wst *)base_type;
    Wst		*wst = (Wst *)NULL;
    caddr_t	avlist[PHG_ATTR_STANDARD_SIZE], status = NULL;
    va_list     valist;

    if ( CB_ENTRY_CHECK( phg_cur_cph, ERR2, Pfn_phigs_ws_type_create)) {
	if ( !phg_cb_wst_exists( base )) {
	    ERR_REPORT( phg_cur_cph->erh, ERR52);

	} else {
	    Va_start( valist,base_type );
	    phg_attr_make( avlist, PHG_ATTR_STANDARD_SIZE, valist );
	    va_end( valist );

	    if ( wst = phg_wst_create( phg_cur_cph->erh, base, avlist ) ) {
		if ( !phg_cp_add_wst( phg_cur_cph, wst ) ) {
		    ERR_REPORT( phg_cur_cph->erh, ERR900 );
		    phg_wst_destroy( wst );
		    wst = NULL;
		}
	    }
	}
    }
    return (Pint)wst;
}

void
phigs_ws_type_destroy( wst)
    Wst		*wst;
{
    if ( CB_ENTRY_CHECK( phg_cur_cph, ERR2, Pfn_phigs_ws_type_destroy)){
	if ( !phg_cb_wst_exists( wst)) {
	    ERR_REPORT( phg_cur_cph->erh, ERR52);

	} else if ( wst->bound_status != WST_UNBOUND) {
	    /* Only operate on UNBOUND types */
	    ERR_REPORT( phg_cur_cph->erh, ERRN100);

	} else {
	    phg_cp_rmv_wst( phg_cur_cph, wst);
	    phg_wst_destroy( wst );
	}
    }
}

void
pinq_list_avail_ws_types( length, start, error_ind, types, length_list)
    Pint                length;         /* length of application list */
    Pint                start;      /* starting position    */
    Pint		*error_ind;	/* OUT error indicator	*/
    Pint_list		*types;		/* OUT list of ws types	*/
    Pint		*length_list;	/* OUT length of list in PHIGS */
{
    register Cp_wst_list_entry		*node;
    int			                t;

    if ( !CB_ENTRY_CHECK( phg_cur_cph, 0, Pfn_INQUIRY)) {
	*error_ind = ERR2;

    } else {
	*error_ind = 0;
	*length_list = 0;
	types->num_ints = 0;
	for ( node = phg_cur_cph->wst_list; node; node = node->next )
	    (*length_list)++;

	if ( *length_list > 0 ) {
	    if ( start < 0 || start >= *length_list )
		*error_ind = ERR2201;
	    else if ( length > 0 ) {
		types->num_ints = MIN(length, *length_list - start);
                node = phg_cur_cph->wst_list;
		/* skip past wst not asked for */
		for (t = 0; t < start; t++) node = node->next;
		for (t = 0; t < types->num_ints; t++) {
		    types->ints[t] = (Pint)node->wst;
		    node = node->next;
		}
	    } else if ( length < 0 )
		*error_ind = ERRN153;
	}
    }
}

int
phg_cb_wst_exists( wst)
    Wst		*wst;
{
    register Cp_wst_list_entry		*node;

    node = phg_cur_cph->wst_list;
    while ( node) {
	if ( wst == node->wst)
	    return 1;
	else
	    node = node->next;
    }
    return 0;	/* type not found */
}

void
pinq_num_disp_pris( ws_type, error_ind, num_pri)
    Pint	ws_type;	/* workstation type	*/
    Pint	*error_ind;	/* OUT error indicator	*/
    Pint	*num_pri;	/* OUT number of display priorities	*/
{
    Wst				*wst = (Wst *)ws_type;
    Wst_phigs_dt		*dt;

    if ( !CB_ENTRY_CHECK( phg_cur_cph, 0, Pfn_INQUIRY)) {
	*error_ind = ERR2;

    } else if ( !phg_cb_wst_exists( wst)) {
	*error_ind = ERR52;
    
    } else if ( wst->bound_status != WST_BOUND ) {
	*error_ind = ERR51;

    } else if ( !((dt = &wst->desc_tbl.phigs_dt)->ws_category == PCAT_OUT
	|| dt->ws_category == PCAT_OUTIN || dt->ws_category == PCAT_MO) ) {
	*error_ind = ERR59;

    } else if ( dt->ws_category == PCAT_MO ) {
	*error_ind = ERR62;

    } else {
	*error_ind = 0;
	*num_pri = dt->out_dt.num_display_priorities;
    }
}

void
pinq_ws_cat(ws_type, error_ind, category)
    Pint	ws_type;		/* workstation type	*/
    Pint	*error_ind;	/* OUT error indicator	*/
    Pws_cat	*category;	/* OUT workstation category	*/
{
    Wst				*wst = (Wst *)ws_type;

    /* Error 51 is not generated by this function. */

    if ( !CB_ENTRY_CHECK( phg_cur_cph, 0, Pfn_INQUIRY)) {
	*error_ind = ERR2;

    } else if ( !phg_cb_wst_exists( wst )) {
	*error_ind = ERR52;

    } else {
	*error_ind = 0;
	*category = wst->desc_tbl.phigs_dt.ws_category;
    }
}

static void
get_ws_disp_size( wst, error_ind, size )
    Wst		*wst;
    Pint	*error_ind;
    Pdisp_space_size3	*size;
{
    Phg_args			cp_args;
    Phg_ret			ret;

    if ( wst->desc_tbl.xwin_dt.dc_model == PHIGS_DC_LIMITS_FIXED ) {
	Wst_phigs_dt		*dt = &wst->desc_tbl.phigs_dt;

	size->dc_units = dt->dev_coord_units;
	size->size_raster.size_x = dt->dev_addrs_units[0];
	size->size_raster.size_y = dt->dev_addrs_units[1];
	size->size_raster.size_z = dt->dev_addrs_units[2];
	size->size_dc.size_x = dt->dev_coords[0];
	size->size_dc.size_y = dt->dev_coords[1];
	size->size_dc.size_z = dt->dev_coords[2];
    } else {
	/* Assumes the ws is open, which it is if the wst is BOUND. */
	cp_args.data.idata = wst->wsid;
	CP_FUNC( phg_cur_cph, CP_FUNC_OP_INQ_WIN_INFO, &cp_args, &ret);
	if ( ret.err) {
		*error_ind = ret.err;

	} else {
	    *error_ind = 0;
	    *size = ret.data.win_info.display_size;
	}
    }
}

static void
inquire_display_size(wst, error_ind, size )
    Wst			*wst;	/* workstation type	*/
    Pint		*error_ind;	/* OUT error indicator	*/
    Pdisp_space_size3	*size;		/* OUT display size	*/
{
    /* Error 51 and 62 are not generated by this function */

    Wst_phigs_dt		*dt;

    if ( !CB_ENTRY_CHECK( phg_cur_cph, 0, Pfn_INQUIRY)) {
	*error_ind = ERR2;

    } else if ( !phg_cb_wst_exists( wst)) {
	*error_ind = ERR52;

    } else if ( (dt = &wst->desc_tbl.phigs_dt)->ws_category == PCAT_MI ) {
	*error_ind = ERR57;

    } else {
	*error_ind = 0;
	switch ( wst->base_type ) {
	    case WST_BASE_TYPE_X_DRAWABLE:
		if ( wst->bound_status != WST_BOUND ) {
		    *error_ind = ERR51;
		} else {
		    get_ws_disp_size( wst, error_ind, size );
		}
		break;
	    case WST_BASE_TYPE_X_TOOL:
		if ( wst->bound_status != WST_BOUND ) {
		    size->dc_units = dt->dev_coord_units;
		    size->size_raster.size_x = dt->dev_addrs_units[0];
		    size->size_raster.size_y = dt->dev_addrs_units[1];
		    size->size_raster.size_z = dt->dev_addrs_units[2];
		    size->size_dc.size_x = dt->dev_coords[0];
		    size->size_dc.size_y = dt->dev_coords[1];
		    size->size_dc.size_z = dt->dev_coords[2];
		} else {
		    get_ws_disp_size( wst, error_ind,  size );
		}
		break;
	}
    }
}

void
pinq_disp_space_size3(type, error_ind, size)
    Pint		type;		/* workstation type	*/
    Pint		*error_ind;	/* OUT error indicator	*/
    Pdisp_space_size3	*size;		/* OUT display size	*/
{
    inquire_display_size( (Wst *)type, error_ind, size );
}

void
pinq_disp_space_size(ws_type, error_ind, size)
    Pint		ws_type;	/* workstation type	*/
    Pint		*error_ind;	/* OUT error indicator	*/
    Pdisp_space_size	*size;		/* OUT display size	*/
{
    Wst			*wst = (Wst *)ws_type;
    Pdisp_space_size3	size3;

    inquire_display_size( wst, error_ind, &size3 );
    if ( *error_ind == 0 ) {
	size->dc_units = size3.dc_units;
	size->size_dc.size_x = size3.size_dc.size_x;
	size->size_dc.size_y = size3.size_dc.size_y;
	size->size_raster.size_x = size3.size_raster.size_x;
	size->size_raster.size_y = size3.size_raster.size_y;
    }
}

void
pinq_hlhsr_id_facs( ws_type, length, start, error_ind, ids, length_list )
    Pint	ws_type;	/* workstation type	*/
    Pint	length;		/* length of id list	*/
    Pint	start;		/* starting position of id list	*/
    Pint	*error_ind;	/* OUT error indicator	*/
    Pint_list	*ids;		/* OUT list of HLHSR ids	*/
    Pint	*length_list;	/* OUT len of id list in PHIGS	*/
{
    Wst			*wst = (Wst *)ws_type;
    Wst_phigs_dt	*dt;

    if ( !CB_ENTRY_CHECK( phg_cur_cph, 0, Pfn_INQUIRY)) {
	*error_ind = ERR2;

    } else if ( !phg_cb_wst_exists( wst)) {
	*error_ind = ERR52;

    } else if ( wst->bound_status != WST_BOUND) {
	*error_ind = ERR51;

    } else if ( (dt = &wst->desc_tbl.phigs_dt)->ws_category == PCAT_MI ) {
	*error_ind = ERR57;

    } else if ( dt->ws_category == PCAT_MO ) {
	*error_ind = ERR62;

    } else {
	*error_ind = 0;
	*length_list = dt->num_hlhsr_ids;
	ids->num_ints = 0;
	if ( dt->num_hlhsr_ids > 0) {
	    if ( start < 0 || start >= dt->num_hlhsr_ids )
		*error_ind = ERR2201;
	    else if ( length > 0 ) {
		ids->num_ints = MIN( length, dt->num_hlhsr_ids - start);
		bcopy( (char*)&dt->hlhsr_ids[start], (char*)ids->ints, 
		    ids->num_ints * sizeof(Pint));
	    } else if ( length < 0 )
		*error_ind = ERRN153;
	}
    }
}

void
pinq_hlhsr_mode_facs( ws_type, length, start, error_ind, modes, length_list )
    Pint	ws_type;	/* workstation type	*/
    Pint	length;		/* length of id list	*/
    Pint	start;		/* starting position of id list	*/
    Pint	*error_ind;	/* OUT error indicator	*/
    Pint_list	*modes;		/* OUT list of HLHSR modes	*/
    Pint	*length_list;	/* OUT len of id list in PHIGS	*/
{
    Wst			*wst = (Wst *)ws_type;
    Wst_phigs_dt	*dt;

    if ( !CB_ENTRY_CHECK( phg_cur_cph, 0, Pfn_INQUIRY)) {
	*error_ind = ERR2;

    } else if ( !phg_cb_wst_exists( wst)) {
	*error_ind = ERR52;

    } else if ( wst->bound_status != WST_BOUND) {
	*error_ind = ERR51;

    } else if ( (dt = &wst->desc_tbl.phigs_dt)->ws_category == PCAT_MI ) {
	*error_ind = ERR57;

    } else if ( dt->ws_category == PCAT_MO ) {
	*error_ind = ERR62;

    } else {
	*error_ind = 0;
	*length_list = dt->num_hlhsr_modes;
	modes->num_ints = 0;
	if ( dt->num_hlhsr_modes > 0 ) {
	    if ( start < 0 || start >= dt->num_hlhsr_modes )
		*error_ind = ERR2201;
	    else if ( length > 0 ) {
		modes->num_ints = MIN( length, dt->num_hlhsr_modes - start);
		bcopy( (char*)&dt->hlhsr_modes[start], (char*)modes->ints,
		    modes->num_ints * sizeof(Pint));
	    } else if ( length < 0 )
		*error_ind = ERRN153;
	}
    }
}

void
pinq_view_facs(ws_type, error_ind, num)
    Pint	ws_type;	/* workstation type	*/
    Pint	*error_ind;	/* OUT error indicator	*/
    Pint	*num;		/* OUT number of predefined view indices	*/
{
    /* Error 51 is not generated by this function. */

    Wst			*wst = (Wst *)ws_type;
    Wst_phigs_dt	*dt;

    if ( !CB_ENTRY_CHECK( phg_cur_cph, 0, Pfn_INQUIRY)) {
	*error_ind = ERR2;

    } else if ( !phg_cb_wst_exists( wst)) {
	*error_ind = ERR52;

    } else if ( (dt = &wst->desc_tbl.phigs_dt)->ws_category == PCAT_MI ) {
	*error_ind = ERR57;

    } else if ( dt->ws_category == PCAT_MO ) {
	*error_ind = ERR62;

    } else {
	*error_ind = 0;
	*num = dt->num_predefined_views;
    }
}

void
pinq_pred_view_rep(ws_type, index, error_ind, rep)
    Pint	ws_type;	/* workstation type	*/
    Pint	index;		/* predefined view index	*/
    Pint	*error_ind;	/* OUT error indicator	*/
    Pview_rep3 	*rep;		/* OUT view representation	*/
{
    /* Error 51 is not generated by this function. */

    Wst			*wst = (Wst *)ws_type;
    Wst_phigs_dt	*dt;

    if ( !CB_ENTRY_CHECK( phg_cur_cph, 0, Pfn_INQUIRY)) {
	*error_ind = ERR2;

    } else if ( !phg_cb_wst_exists( wst)) {
	*error_ind = ERR52;

    } else if ( (dt = &wst->desc_tbl.phigs_dt)->ws_category == PCAT_MI ) {
	*error_ind = ERR57;

    } else if ( index < 0 ) {
	*error_ind = ERR114;

    } else if ( index >= dt->num_predefined_views) {
	*error_ind = ERR101;

    } else if ( dt->ws_category == PCAT_MO ) {
	*error_ind = ERR62;

    } else {
	*error_ind = 0;
	*rep = dt->default_views[index];
    }
}

void
pinq_ws_class(ws_type, error_ind, class)
    Pint	ws_type;	/* workstation type	*/
    Pint	*error_ind;	/* OUT error indicator	*/
    Pws_class	*class;		/* OUT workstation class	*/
{
    /* Error 51 and 62 are not generated by this function. */

    Wst			*wst = (Wst *)ws_type;
    Wst_phigs_dt	*dt;

    if ( !CB_ENTRY_CHECK( phg_cur_cph, 0, Pfn_INQUIRY)) {
	*error_ind = ERR2;

    } else if ( !phg_cb_wst_exists( wst)) {
	*error_ind = ERR52;

    } else if ( !((dt = &wst->desc_tbl.phigs_dt)->ws_category == PCAT_OUT
	|| dt->ws_category == PCAT_OUTIN || dt->ws_category == PCAT_MO) ) {
	*error_ind = ERR59;

    } else {
	*error_ind = 0;
	*class = dt->out_dt.ws_class;
    }
}

void
pinq_dyns_ws_attrs(ws_type, error_ind, attr)
    Pint		ws_type;	/* workstation type	*/
    Pint		*error_ind;	/* OUT error indicator	*/
    Pdyns_ws_attrs	*attr;		/* OUT attributes dynamics 	*/
{
    /* Error 51 and 62 are not generated by this function. */

    Wst			*wst = (Wst *)ws_type;
    Wst_phigs_dt	*dt;

    if ( !CB_ENTRY_CHECK( phg_cur_cph, 0, Pfn_INQUIRY)) {
	*error_ind = ERR2;

    } else if ( !phg_cb_wst_exists( wst)) {
	*error_ind = ERR52;

    } else if ( !((dt = &wst->desc_tbl.phigs_dt)->ws_category == PCAT_OUT
	|| dt->ws_category == PCAT_OUTIN || dt->ws_category == PCAT_MO) ) {
	*error_ind = ERR59;

    } else {
	*error_ind = 0;
	attr->line_bundle = dt->out_dt.polyline_bundle_rep;
	attr->marker_bundle = dt->out_dt.polymarker_bundle_rep;
	attr->text_bundle = dt->out_dt.text_bundle_rep;
	attr->int_bundle = dt->out_dt.interior_bundle_rep;
	attr->edge_bundle = dt->out_dt.edge_bundle_rep;
	attr->pat_rep = dt->out_dt.pattern_rep;
	attr->colr_rep = dt->out_dt.colour_rep;
	attr->view_rep = dt->out_dt.view_rep;
	attr->ws_tran = dt->out_dt.ws_xform;
	attr->highl_filter = dt->out_dt.highlight_filter;
	attr->invis_filter = dt->out_dt.invis_filter;
	attr->hlhsr_mode = dt->out_dt.hlhsr_mode;
    }
}

void
pinq_def_disp_upd_st(ws_type, error_ind, def_mode, mod_mode)
    Pint		ws_type;	/* workstation type	*/
    Pint		*error_ind;	/* OUT error indicator	*/
    Pdefer_mode		*def_mode;      /* OUT deferral mode    */
    Pmod_mode		*mod_mode;      /* OUT modification mode */
{
    /* Error 51 and 62 are not generated by this function. */

    Wst			*wst = (Wst *)ws_type;
    Wst_phigs_dt	*dt;

    if ( !CB_ENTRY_CHECK( phg_cur_cph, 0, Pfn_INQUIRY)) {
	*error_ind = ERR2;

    } else if ( !phg_cb_wst_exists( wst)) {
	*error_ind = ERR52;

    } else if ( !((dt = &wst->desc_tbl.phigs_dt)->ws_category == PCAT_OUT
	|| dt->ws_category == PCAT_OUTIN || dt->ws_category == PCAT_MO) ) {
	*error_ind = ERR59;

    } else {
	*error_ind = 0;
	*def_mode = dt->out_dt.deferral_mode;
	*mod_mode = dt->out_dt.modification_mode;
    }
}


/* INQUIRE POLYLINE FACILITIES */
void
pinq_line_facs(ws_type, length, start, error_ind, facilities, total_length)
    Pint	ws_type;	/* workstation type	*/
    Pint	length;		/* length of application list	*/
    Pint	start;		/* starting position	*/
    Pint	*error_ind;	/* OUT error indicator	*/
    Pline_facs *facilities;	/* OUT polyline facilities	*/
    Pint	*total_length;	/* OUT length of list in PHIGS	*/
{
    /* Error 51 is not generated by this function. */

    Wst			*wst = (Wst *)ws_type;
    Wst_phigs_dt	*dt;

    if ( !CB_ENTRY_CHECK( phg_cur_cph, 0, Pfn_INQUIRY)) {
	*error_ind = ERR2;

    } else if ( !phg_cb_wst_exists( wst)) {
	*error_ind = ERR52;

    } else if ( !((dt = &wst->desc_tbl.phigs_dt)->ws_category == PCAT_OUT
	|| dt->ws_category == PCAT_OUTIN || dt->ws_category == PCAT_MO) ) {
	*error_ind = ERR59;

    } else if ( dt->ws_category == PCAT_MO ) {
	*error_ind = ERR62;

    } else {
	*error_ind = 0;
	*total_length = dt->out_dt.num_linetypes;
	facilities->types.num_ints = 
	    MIN( length, dt->out_dt.num_linetypes - start);
	if ( facilities->types.num_ints > 0) {
	    bcopy( (char*)&dt->out_dt.linetypes[start],
	        (char*)facilities->types.ints, 
		facilities->types.num_ints * sizeof(Pint));
	}
	facilities->num_widths = dt->out_dt.num_linewidths;
	facilities->nom_width = dt->out_dt.nominal_linewidth;
	facilities->min_width = dt->out_dt.min_linewidth;
	facilities->max_width = dt->out_dt.max_linewidth;
	facilities->num_pred_inds = dt->out_dt.num_predefined_polyline_indices;
    }
}


void
pinq_line_facs_plus(ws_type, lt_length, lt_start, sm_length, sm_start,
	error_ind, tot_lt_length, tot_sm_length, facilities)
    Pint	ws_type;	  /* workstation type	*/
    Pint	lt_length;	  /* number of line types to return */
    Pint	lt_start;	  /* starting position	*/
    Pint 	sm_length;	  /* number of shading methods to return */
    Pint	sm_start;	  /* starting position	*/
    Pint	*error_ind;	  /* OUT error indicator */
    Pint	*tot_lt_length; /* OUT length of line type list in PHIGS */
    Pint	*tot_sm_length; /* OUT length of shading method list in PHIGS*/
    Pline_facs_plus *facilities; /* OUT extended polyline facilities */
{
    /* Error 51 is not generated by this function. */

    Wst			*wst = (Wst *)ws_type;
    Wst_phigs_dt	*dt;

    if ( !CB_ENTRY_CHECK( phg_cur_cph, 0, Pfn_INQUIRY)) {
	*error_ind = ERR2;

    } else if ( !phg_cb_wst_exists( wst)) {
	*error_ind = ERR52;

    } else if ( !((dt = &wst->desc_tbl.phigs_dt)->ws_category == PCAT_OUT
	|| dt->ws_category == PCAT_OUTIN) ) {
	*error_ind = ERR62;

    } else {
	*error_ind = 0;
	*tot_lt_length = dt->out_dt.num_linetypes;
	*tot_sm_length = dt->out_dt.num_polyline_shading_methods;
	facilities->types.num_ints = 
	    MIN( lt_length, dt->out_dt.num_linetypes - lt_start);
	facilities->shads.num_ints = 
	    MIN( sm_length, dt->out_dt.num_polyline_shading_methods - sm_start);
	if ( facilities->types.num_ints > 0) {
	    bcopy( (char*)&dt->out_dt.linetypes[lt_start],
	        (char*)facilities->types.ints, 
		facilities->types.num_ints * sizeof(Pint));
	}
	if ( facilities->shads.num_ints > 0) {
	    bcopy( (char*)&dt->out_dt.polyline_shading_methods[sm_start],
	        (char*)facilities->shads.ints, 
		facilities->shads.num_ints * sizeof(Pint));
	}
	facilities->num_widths = dt->out_dt.num_linewidths;
	facilities->nom_width = dt->out_dt.nominal_linewidth;
	facilities->min_width = dt->out_dt.min_linewidth;
	facilities->max_width = dt->out_dt.max_linewidth;
	facilities->num_pred_inds = dt->out_dt.num_predefined_polyline_indices;
    }
}


/* INQUIRE PREDEFINED POLYLINE REPRESENTATION */
void
pinq_pred_line_rep(ws_type, index, error_ind, bundle)
    Pint		ws_type;	/* workstation type	*/
    Pint		index;		/* predefined index	*/
    Pint		*error_ind;	/* OUT error indicator	*/
    Pline_bundle	*bundle;	/* OUT predefined polyline rep	*/
{
    /* Error 51 and 62 are not generated by this function. */

    Wst			*wst = (Wst *)ws_type;
    Wst_phigs_dt	*dt;

    if ( !CB_ENTRY_CHECK( phg_cur_cph, 0, Pfn_INQUIRY)) {
	*error_ind = ERR2;

    } else if ( !phg_cb_wst_exists( wst)) {
	*error_ind = ERR52;

    } else if ( !((dt = &wst->desc_tbl.phigs_dt)->ws_category == PCAT_OUT
	|| dt->ws_category == PCAT_OUTIN || dt->ws_category == PCAT_MO) ) {
	*error_ind = ERR59;

    } else if ( index < 1 ) {
	*error_ind = ERR100;

    } else if ( index > dt->out_dt.num_predefined_polyline_indices) {
	*error_ind = ERR102;

    } else {
	*error_ind = 0;
	bundle->type = dt->out_dt.default_polyline_bundle_table[index-1].type;
	bundle->width = dt->out_dt.default_polyline_bundle_table[index-1].width;
	bundle->colr_ind =
	    dt->out_dt.default_polyline_bundle_table[index-1].colr.val.ind;
    }
}


/* INQUIRE PREDEFINED EXTENDED POLYLINE REPRESENTATION */
void
pinq_pred_line_rep_plus(ws_type, index, error_ind, bundle)
    Pint		ws_type;		/* workstation type	*/
    Pint		index;		/* predefined index	*/
    Pint		*error_ind;	/* OUT error indicator	*/
    Pline_bundle_plus	*bundle;	/* OUT predefined polyline rep	*/
{
    /* Error 51 and 62 are not generated by this function. */

    Wst			*wst = (Wst *)ws_type;
    Wst_phigs_dt	*dt;

    if ( !CB_ENTRY_CHECK( phg_cur_cph, 0, Pfn_INQUIRY)) {
	*error_ind = ERR2;

    } else if ( !phg_cb_wst_exists( wst)) {
	*error_ind = ERR52;

    } else if ( !((dt = &wst->desc_tbl.phigs_dt)->ws_category == PCAT_OUT
	|| dt->ws_category == PCAT_OUTIN || dt->ws_category == PCAT_MO) ) {
	*error_ind = ERR59;

    } else if ( index < 1 ) {
	*error_ind = ERR100;

    } else if ( index > dt->out_dt.num_predefined_polyline_indices) {
	*error_ind = ERR102;

    } else {
	*error_ind = 0;
	*bundle = dt->out_dt.default_polyline_bundle_table[index-1];
    }
}

void
pinq_marker_facs(ws_type, length, start, error_ind, facilities, total_length)
    Pint	ws_type;		/* workstation type	*/
    Pint	length;		/* length of application list	*/
    Pint	start;		/* starting position	*/
    Pint	*error_ind;	/* OUT error indicator	*/
    Pmarker_facs	*facilities;	/* OUT polymarker facilities	*/
    Pint	*total_length;	/* OUT length of list in PHIGS	*/
{
    /* Error 51 is not generated by this function. */

    Wst			*wst = (Wst *)ws_type;
    Wst_phigs_dt	*dt;

    if ( !CB_ENTRY_CHECK( phg_cur_cph, 0, Pfn_INQUIRY)) {
	*error_ind = ERR2;

    } else if ( !phg_cb_wst_exists( wst)) {
	*error_ind = ERR52;

    } else if ( !((dt = &wst->desc_tbl.phigs_dt)->ws_category == PCAT_OUT
	|| dt->ws_category == PCAT_OUTIN || dt->ws_category == PCAT_MO) ) {
	*error_ind = ERR59;

    } else if ( dt->ws_category == PCAT_MO ) {
	*error_ind = ERR62;

    } else {
	*error_ind = 0;
	*total_length = dt->out_dt.num_marker_types;
	facilities->types.num_ints = 
	    MIN( length, dt->out_dt.num_marker_types - start);
	if ( facilities->types.num_ints > 0) {
	    bcopy( (char*)&dt->out_dt.marker_types[start], 
		(char*)facilities->types.ints, 
		facilities->types.num_ints * sizeof(Pint));
	}
	facilities->num_sizes = dt->out_dt.num_marker_sizes;
	facilities->nom_size = dt->out_dt.nominal_marker_size;
	facilities->min_size = dt->out_dt.min_marker_size;
	facilities->max_size = dt->out_dt.max_marker_size;
	facilities->num_pred_inds = dt->out_dt.num_predefined_polymarker_indices;
    }
}


/* INQUIRE PREDEDINED POLYMARKER REPRESENTATION */
void
pinq_pred_marker_rep(ws_type, index, error_ind, bundle)
    Pint		ws_type;	/* workstation type	*/
    Pint		index;		/* predefined index	*/
    Pint		*error_ind;	/* OUT error indicator	*/
    Pmarker_bundle	*bundle;	/* OUT predefined polymarker rep	*/
{
    /* Error 51 and 62 are not generated by this function. */

    Wst			*wst = (Wst *)ws_type;
    Wst_phigs_dt	*dt;

    if ( !CB_ENTRY_CHECK( phg_cur_cph, 0, Pfn_INQUIRY)) {
	*error_ind = ERR2;

    } else if ( !phg_cb_wst_exists( wst )) {
	*error_ind = ERR52;

    } else if ( !((dt = &wst->desc_tbl.phigs_dt)->ws_category == PCAT_OUT
	|| dt->ws_category == PCAT_OUTIN || dt->ws_category == PCAT_MO) ) {
	*error_ind = ERR59;

    } else if ( index < 1 ) {
	*error_ind = ERR100;

    } else if ( index > dt->out_dt.num_predefined_polymarker_indices) {
	*error_ind = ERR102;

    } else {
	Pmarker_bundle_plus *mb = &dt->out_dt.default_polymarker_bundle_table[index-1];

	*error_ind = 0;
	bundle->type   = mb->type;
	bundle->size   = mb->size;
	if (mb->colr.type == PINDIRECT)
	    bundle->colr_ind = mb->colr.val.ind;
	/* TODO: else .. */
    }
}


/* INQUIRE PREDEDINED EXTENDED POLYMARKER REPRESENTATION */
void
pinq_pred_marker_rep_plus(ws_type, index, error_ind, bundle)
    Pint		ws_type;		/* workstation type	*/
    Pint		index;		/* predefined index	*/
    Pint		*error_ind;	/* OUT error indicator	*/
    Pmarker_bundle_plus	*bundle;	/* OUT predefined polymarker rep	*/
{
    /* Error 51 and 62 not generated by this function. */

    Wst				*wst = (Wst *)ws_type;
    Wst_phigs_dt		*dt;

    if ( !CB_ENTRY_CHECK( phg_cur_cph, 0, Pfn_INQUIRY)) {
	*error_ind = ERR2;

    } else if ( !phg_cb_wst_exists( wst )) {
	*error_ind = ERR52;

    } else if ( !((dt = &wst->desc_tbl.phigs_dt)->ws_category == PCAT_OUT
	|| dt->ws_category == PCAT_OUTIN || dt->ws_category == PCAT_MO) ) {
	*error_ind = ERR59;

    } else if ( index < 1 ) {
	*error_ind = ERR100;

    } else if ( index > dt->out_dt.num_predefined_polymarker_indices) {
	*error_ind = ERR102;

    } else {
	*error_ind = 0;
	*bundle = dt->out_dt.default_polymarker_bundle_table[index-1];
    }
}

int
phg_cb_charset_index(dt,charset_no)
Wst_phigs_dt	*dt;
Pint		charset_no;
{
    int	i;


    for(i=0; i< dt->out_dt.num_charsets; i++) {
	if (dt->out_dt.charsets[i].charset == charset_no) {
	    return i;
	}
    }
    return -1;

}

void
pinq_text_facs(ws_type, length, start, error_ind, facilities, total_length)
Pint	ws_type;		/* workstation type	*/
Pint	length;		/* length of application list	*/
Pint	start;		/* starting position	*/
Pint	*error_ind;	/* OUT error indicator	*/
Ptext_facs *facilities;	/* OUT text facilities	*/
Pint	*total_length;	/* OUT length of list in PHIGS	*/
{
    /* Error 51 is not generated by this function. */

    Wst				*wst = (Wst *)ws_type;
    Wst_phigs_dt		*dt;
    int				charset_index;

    if ( !CB_ENTRY_CHECK( phg_cur_cph, 0, Pfn_INQUIRY)) {
	*error_ind = ERR2;

    } else if ( !phg_cb_wst_exists( wst )) {
	*error_ind = ERR52;

    } else if ( !((dt = &wst->desc_tbl.phigs_dt)->ws_category == PCAT_OUT
	|| dt->ws_category == PCAT_OUTIN || dt->ws_category == PCAT_MO) ) {
	*error_ind = ERR59;

    } else if ( dt->ws_category == PCAT_MO ) {
	*error_ind = ERR62;

    } else {
	*error_ind = 0;
	/* Since the character set is not specified by this function, use
	 * ASCII, which is in slot 0 of the text pair table */
	charset_index = phg_cb_charset_index(dt,PCS_ASCII);
	*total_length = dt->out_dt.num_text_pairs[charset_index];
	facilities->num_font_precs = MIN(length,
	    dt->out_dt.num_text_pairs[charset_index] - start);
	if ( facilities->num_font_precs > 0) {
	    bcopy( (char*)&dt->out_dt.text_pairs[charset_index][start], 
			(char*)facilities->font_precs,
		facilities->num_font_precs * sizeof(Ptext_font_prec));
	}
	facilities->num_char_hts = dt->out_dt.num_char_heights;
	facilities->min_char_ht = dt->out_dt.min_char_height;
	facilities->max_char_ht = dt->out_dt.max_char_height;
	facilities->num_char_expans = dt->out_dt.num_char_expansion_factors;
	facilities->min_char_expan = dt->out_dt.min_char_expansion_factor;
	facilities->max_char_expan = dt->out_dt.max_char_expansion_factor;
	facilities->num_pred_inds = dt->out_dt.num_predefined_text_indices;
    }
}

/* INQUIRE PREDEFINED TEXT REPRESENTATION */
void
pinq_pred_text_rep(ws_type, index, error_ind, bundle)
Pint		ws_type;		/* workstation type	*/
Pint		index;		/* predefined index	*/
Pint		*error_ind;	/* OUT error indicator	*/
Ptext_bundle	*bundle;	/* OUT predefined text rep	*/
{
    /* Error 51 and 62 are not generated by this function. */

    Wst				*wst = (Wst *)ws_type;
    Wst_phigs_dt		*dt;

    if ( !CB_ENTRY_CHECK( phg_cur_cph, 0, Pfn_INQUIRY)) {
	*error_ind = ERR2;

    } else if ( !phg_cb_wst_exists( wst )) {
	*error_ind = ERR52;

    } else if ( !((dt = &wst->desc_tbl.phigs_dt)->ws_category == PCAT_OUT
	|| dt->ws_category == PCAT_OUTIN || dt->ws_category == PCAT_MO) ) {
	*error_ind = ERR59;

    } else if ( index < 1 ) {
	*error_ind = ERR100;

    } else if ( index > dt->out_dt.num_predefined_text_indices) {
	*error_ind = ERR102;

    } else {
	Ptext_bundle_plus	*tb = &dt->out_dt.default_text_bundle_table[index-1];

	*error_ind = 0;
	bundle->font = tb->font;
	bundle->prec = tb->prec;
	bundle->char_expan = tb->char_expan;
	bundle->char_space = tb->char_space;
	if (tb->colr.type == PINDIRECT) 
	    bundle->colr_ind = tb->colr.val.ind;
	/* TODO: else .... */
    }
}


/* INQUIRE PREDEFINED EXTENDED TEXT REPRESENTATION */
void
pinq_pred_text_rep_plus(ws_type, index, error_ind, bundle)
Pint		ws_type;		/* workstation type	*/
Pint		index;		/* predefined index	*/
Pint		*error_ind;	/* OUT error indicator	*/
Ptext_bundle_plus	*bundle;	/* OUT predefined text rep	*/
{
    /* Error 51 and 62 are not generated by this function. */

    Wst				*wst = (Wst *)ws_type;
    Wst_phigs_dt		*dt;

    if ( !CB_ENTRY_CHECK( phg_cur_cph, 0, Pfn_INQUIRY)) {
	*error_ind = ERR2;

    } else if ( !phg_cb_wst_exists( wst )) {
	*error_ind = ERR52;

    } else if ( !((dt = &wst->desc_tbl.phigs_dt)->ws_category == PCAT_OUT
	|| dt->ws_category == PCAT_OUTIN || dt->ws_category == PCAT_MO) ) {
	*error_ind = ERR59;

    } else if ( index < 1 ) {
	*error_ind = ERR100;

    } else if ( index > dt->out_dt.num_predefined_text_indices) {
	*error_ind = ERR102;

    } else {
	*error_ind = 0;
	*bundle = dt->out_dt.default_text_bundle_table[index-1];
    }
}

void
pinq_anno_facs(ws_type, length, start, error_ind, styles, total_length,
    num_char_hts, min_char_ht, max_char_ht)
Pint	ws_type;		/* workstation type	*/
Pint	length;		/* length of application list	*/
Pint	start;		/* starting position	*/
Pint	*error_ind;	/* OUT error indicator	*/
Pint_list *styles;	/* OUT list annotation styles	*/
Pint	*total_length;	/* OUT length of list in PHIGS	*/
Pint	*num_char_hts;	/* OUT number of character heights */
Pfloat	*min_char_ht;
Pfloat	*max_char_ht;
{
    /* Error 51 is not generated by this function. */

    Wst				*wst = (Wst *)ws_type;
    Wst_phigs_dt		*dt;

    if ( !CB_ENTRY_CHECK( phg_cur_cph, 0, Pfn_INQUIRY)) {
	*error_ind = ERR2;

    } else if ( !phg_cb_wst_exists( wst )) {
	*error_ind = ERR52;

    } else if ( !((dt = &wst->desc_tbl.phigs_dt)->ws_category == PCAT_OUT
	|| dt->ws_category == PCAT_OUTIN || dt->ws_category == PCAT_MO) ) {
	*error_ind = ERR59;

    } else if ( dt->ws_category == PCAT_MO ) {
	*error_ind = ERR62;

    } else {
	*error_ind = ERR0;
	*num_char_hts = dt->out_dt.num_char_heights;
	*min_char_ht = dt->out_dt.min_char_height;
	*max_char_ht = dt->out_dt.max_char_height;
	styles->num_ints = 0;
	if ( (*total_length = dt->out_dt.num_annotation_styles) > 0) {
	    if (start < 0 || start >= dt->out_dt.num_annotation_styles)
		*error_ind = ERR2201;
	    else if (length > 0) {
		styles->num_ints = MIN( length,
				    dt->out_dt.num_annotation_styles - start);
		bcopy( (char*)&dt->out_dt.annotation_styles[start],
			(char*)styles->ints, 
			styles->num_ints * sizeof(Pint));
	    } else if (length < 0)
		*error_ind = ERRN153;
	}
    }
}


/* INQUIRE INTERIOR FACILITIES */
void
pinq_int_facs(ws_type, h_len, h_st, error_ind, facil, tot_h_len)
Pint		ws_type;		/* workstation type	*/
Pint		h_len;		/* length of hatch style list	*/
Pint		h_st;		/* starting position	*/
Pint		*error_ind;	/* OUT error indicator	*/
Pint_facs	*facil;		/* OUT interior facilities	*/
Pint		*tot_h_len;	/* OUT len of hatch list in PHIGS	*/
{
    /* Error 51 is not generated by this function. */

    Wst				*wst = (Wst *)ws_type;
    Wst_phigs_dt		*dt;

    if ( !CB_ENTRY_CHECK( phg_cur_cph, 0, Pfn_INQUIRY)) {
	*error_ind = ERR2;

    } else if ( !phg_cb_wst_exists( wst )) {
	*error_ind = ERR52;

    } else if ( !((dt = &wst->desc_tbl.phigs_dt)->ws_category == PCAT_OUT
	|| dt->ws_category == PCAT_OUTIN || dt->ws_category == PCAT_MO) ) {
	*error_ind = ERR59;

    } else if ( dt->ws_category == PCAT_MO ) {
	*error_ind = ERR62;

    } else {
	*error_ind = 0;
	facil->num_int_styles = MIN(5, dt->out_dt.num_interior_styles);
	if ( facil->num_int_styles > 0) {
	    bcopy( (char*)dt->out_dt.interior_styles,
		(char*)facil->int_styles, 
		facil->num_int_styles * sizeof(Pint_style));
	}
	*tot_h_len = dt->out_dt.num_hatch_styles;
	facil->hatch_styles.num_ints = MIN(h_len, dt->out_dt.num_hatch_styles - h_st);
	if ( facil->hatch_styles.num_ints > 0) {
	    bcopy( (char*)&dt->out_dt.hatch_styles[h_st],
		(char*)facil->hatch_styles.ints, 
		facil->hatch_styles.num_ints * sizeof(Pint));
	}
	facil->num_pred_inds = dt->out_dt.num_predefined_interior_indices;
    }
}


/* INQUIRE EXTENDED INTERIOR FACILITIES */
void
pinq_int_facs_plus( ws_type, is_length, is_start, hs_length, hs_start,
    re_length, re_start, sh_length, sh_start, error_ind, facil,
    tot_is_length, tot_hs_length, tot_re_length, tot_sh_length )
    Pint	ws_type;		/* workstation type */
    Pint	is_length;	/* length of application's interior style list*/
    Pint        is_start;	/* starting position */
    Pint	hs_length;	/* length of application's hatch style list */
    Pint        hs_start;	/* starting position */
    Pint	re_length;	/* length of application's reflectance
				   equation list */
    Pint        re_start;	/* starting position */
    Pint	sh_length;	/* length of application's shading method list*/
    Pint        sh_start;	/* starting position */
    Pint	*error_ind;	/* OUT error indicator */
    Pint_facs_plus	*facil;	/* extended interior facilities */
    Pint	*tot_is_length;	/* total length of interior styles list */
    Pint	*tot_hs_length;	/* total length of hatch styles list */
    Pint	*tot_re_length;	/* total length of reflectance equation list */
    Pint	*tot_sh_length;	/* total length of shading method list */
{
    /* Error 51 is not generated by this function. */

    Wst				*wst = (Wst *)ws_type;
    Wst_phigs_dt		*dt;

    if ( !CB_ENTRY_CHECK( phg_cur_cph, 0, Pfn_INQUIRY)) {
	*error_ind = ERR2;

    } else if ( !phg_cb_wst_exists( wst )) {
	*error_ind = ERR52;

    } else if ( !((dt = &wst->desc_tbl.phigs_dt)->ws_category == PCAT_OUT
	|| dt->ws_category == PCAT_OUTIN || dt->ws_category == PCAT_MO) ) {
	*error_ind = ERR59;

    } else if ( dt->ws_category == PCAT_MO ) {
	*error_ind = ERR62;

    } else {
	*error_ind = 0;
	*tot_is_length = dt->out_dt.num_interior_styles;
	*tot_hs_length = dt->out_dt.num_hatch_styles;
	*tot_re_length = dt->out_dt.num_refeqs;
	*tot_sh_length = dt->out_dt.num_interior_shading_methods;
	facil->num_int_styles = MIN( is_length, 
			 dt->out_dt.num_interior_styles - is_start);
	facil->hatch_styles.num_ints = MIN( hs_length, 
			 dt->out_dt.num_hatch_styles - hs_start);
	facil->refl_eqns.num_ints = MIN( re_length, 
			 dt->out_dt.num_refeqs - re_start);
	facil->shad_meths.num_ints = MIN( sh_length, 
			 dt->out_dt.num_interior_shading_methods - sh_start);
	if ( facil->num_int_styles > 0) {
	    bcopy( (char*)&dt->out_dt.interior_styles[is_start],
		(char*)facil->int_styles,
		facil->num_int_styles * sizeof(Pint_style));
	}
	if ( facil->hatch_styles.num_ints > 0) {
	    bcopy( (char*)&dt->out_dt.hatch_styles[hs_start],
		(char*)facil->hatch_styles.ints, 
		facil->hatch_styles.num_ints * sizeof(Pint));
	}
	if ( facil->refl_eqns.num_ints > 0) {
	    bcopy( (char*)&dt->out_dt.refl_eqns[re_start],
		(char*)facil->refl_eqns.ints, 
		facil->refl_eqns.num_ints * sizeof(Pint));
	}
	if ( facil->shad_meths.num_ints > 0) {
	    bcopy( (char*)&dt->out_dt.polyline_shading_methods[sh_start],
		(char*)facil->shad_meths.ints, 
		facil->shad_meths.num_ints * sizeof(Pint));
	}
	facil->num_pred_inds = dt->out_dt.num_predefined_interior_indices;
    }
}


/* INQUIRE PREDEFINED INTERIOR REPRESENTATION */
void
pinq_pred_int_rep(ws_type, index, error_ind, bundle)
Pint		ws_type;		/* workstation type	*/
Pint		index;		/* predefined index	*/
Pint		*error_ind;	/* OUT error indicator	*/
Pint_bundle	*bundle;	/* OUT predefined interior rep	*/
{
    /* Error 51 and 62 are not generated by this function. */

    Wst				*wst = (Wst *)ws_type;
    Wst_phigs_dt		*dt;

    if ( !CB_ENTRY_CHECK( phg_cur_cph, 0, Pfn_INQUIRY)) {
	*error_ind = ERR2;

    } else if ( !phg_cb_wst_exists( wst )) {
	*error_ind = ERR52;

    } else if ( !((dt = &wst->desc_tbl.phigs_dt)->ws_category == PCAT_OUT
	|| dt->ws_category == PCAT_OUTIN || dt->ws_category == PCAT_MO) ) {
	*error_ind = ERR59;

    } else if ( index < 1 ) {
	*error_ind = ERR100;

    } else if ( index > dt->out_dt.num_predefined_interior_indices) {
	*error_ind = ERR102;

    } else {
	Pint_bundle_plus	*exb;

	*error_ind = 0;
	exb = &dt->out_dt.default_interior_bundle_table[index-1];
	bundle->style = exb->style;
	bundle->style_ind = exb->style_ind;
	if (exb->colr.type == PINDIRECT)
	    bundle->colr_ind = exb->colr.val.ind;
	/* TODO: else ... */
    }
}

/* INQUIRE PREDEFINED EXTENDED INTERIOR REPRESENTATION */
void
pinq_pred_int_rep_plus( ws_type, index, error_ind, rep )
    Pint		ws_type;		/* workstation type */
    Pint		index;		/* interior index */
    Pint		*error_ind;	/* OUT error indicator */
    Pint_bundle_plus	*rep;		/* OUT extended interior rep */
{
    /* Error 51 and 62 are not generated by this function. */

    Wst				*wst = (Wst *)ws_type;
    Wst_phigs_dt		*dt;

    if ( !CB_ENTRY_CHECK( phg_cur_cph, 0, Pfn_INQUIRY)) {
	*error_ind = ERR2;

    } else if ( !phg_cb_wst_exists( wst )) {
	*error_ind = ERR52;

    } else if ( !((dt = &wst->desc_tbl.phigs_dt)->ws_category == PCAT_OUT
	|| dt->ws_category == PCAT_OUTIN || dt->ws_category == PCAT_MO) ) {
	*error_ind = ERR59;

    } else if ( index < 1 ) {
	*error_ind = ERR100;

    } else if ( index > dt->out_dt.num_predefined_interior_indices) {
	*error_ind = ERR102;

    } else {
	*error_ind = 0;
	*rep = dt->out_dt.default_interior_bundle_table[index-1];
    }
}


/* INQUIRE EDGE FACILITIES */
void
pinq_edge_facs(ws_type, length, start, error_ind, facilities, total_length)
Pint		ws_type;		/* workstation type	*/
Pint		length;		/* length of application list	*/
Pint		start;		/* starting position	*/
Pint		*error_ind;	/* OUT error indicator	*/
Pedge_facs	*facilities;	/* OUT edge facilities	*/
Pint		*total_length;	/* OUT length of list in PHIGS	*/
{
    /* Error 51 is not generated by this function. */

    Wst				*wst = (Wst *)ws_type;
    Wst_phigs_dt		*dt;

    if ( !CB_ENTRY_CHECK( phg_cur_cph, 0, Pfn_INQUIRY)) {
	*error_ind = ERR2;

    } else if ( !phg_cb_wst_exists( wst )) {
	*error_ind = ERR52;

    } else if ( !((dt = &wst->desc_tbl.phigs_dt)->ws_category == PCAT_OUT
	|| dt->ws_category == PCAT_OUTIN || dt->ws_category == PCAT_MO) ) {
	*error_ind = ERR59;

    } else if ( dt->ws_category == PCAT_MO ) {
	*error_ind = ERR62;

    } else {
	*error_ind = 0;
	*total_length = dt->out_dt.num_edge_types;
	facilities->types.num_ints = 
	    MIN( length, dt->out_dt.num_edge_types - start);
	if ( facilities->types.num_ints > 0) {
	    bcopy( (char*)&dt->out_dt.edge_types[start],  
		(char*)facilities->types.ints, 
		facilities->types.num_ints * sizeof(Pint));
	}
	facilities->num_widths = dt->out_dt.num_edgewidths;
	facilities->nom_width = dt->out_dt.nominal_edgewidth;
	facilities->min_width = dt->out_dt.min_edgewidth;
	facilities->max_width = dt->out_dt.max_edgewidth;
	facilities->num_pred_inds = dt->out_dt.num_predefined_edge_indices;
    }
}


/* INQUIRE PREDEFINED EDGE REPRESENTATION */
void
pinq_pred_edge_rep(ws_type, index, error_ind, bundle)
Pint		ws_type;		/* workstation type	*/
Pint		index;		/* predefined index	*/
Pint		*error_ind;	/* OUT error indicator	*/
Pedge_bundle	*bundle;	/* OUT predefined edge rep	*/
{
    /* Error 51 and 62 are not generated by this function. */

    Wst				*wst = (Wst *)ws_type;
    Wst_phigs_dt		*dt;

    if ( !CB_ENTRY_CHECK( phg_cur_cph, 0, Pfn_INQUIRY)) {
	*error_ind = ERR2;

    } else if ( !phg_cb_wst_exists( wst )) {
	*error_ind = ERR52;

    } else if ( !((dt = &wst->desc_tbl.phigs_dt)->ws_category == PCAT_OUT
	|| dt->ws_category == PCAT_OUTIN || dt->ws_category == PCAT_MO) ) {
	*error_ind = ERR59;

    } else if ( index < 1 ) {
	*error_ind = ERR100;

    } else if ( index > dt->out_dt.num_predefined_edge_indices) {
	*error_ind = ERR102;

    } else {
	Pedge_bundle_plus  *eb = &dt->out_dt.default_edge_bundle_table[index-1];

	*error_ind = 0;
	bundle->flag = eb->flag;
	bundle->type = eb->type;
	bundle->width = eb->width;
	if (eb->colr.type == PINDIRECT)
	    bundle->colr_ind = eb->colr.val.ind;
	/* TODO: else ... */
    }
}


/* INQUIRE PREDEFINED EXTENDED EDGE REPRESENTATION */
void
pinq_pred_edge_rep_plus(ws_type, index, error_ind, bundle)
Pint		ws_type;		/* workstation type	*/
Pint		index;		/* predefined index	*/
Pint		*error_ind;	/* OUT error indicator	*/
Pedge_bundle_plus	*bundle;	/* OUT predefined edge rep	*/
{
    /* Error 51 and 62 are not generated by this function. */

    Wst				*wst = (Wst *)ws_type;
    Wst_phigs_dt		*dt;

    if ( !CB_ENTRY_CHECK( phg_cur_cph, 0, Pfn_INQUIRY)) {
	*error_ind = ERR2;

    } else if ( !phg_cb_wst_exists( wst )) {
	*error_ind = ERR52;

    } else if ( !((dt = &wst->desc_tbl.phigs_dt)->ws_category == PCAT_OUT
	|| dt->ws_category == PCAT_OUTIN || dt->ws_category == PCAT_MO) ) {
	*error_ind = ERR59;

    } else if ( index < 1 ) {
	*error_ind = ERR100;

    } else if ( index > dt->out_dt.num_predefined_edge_indices) {
	*error_ind = ERR102;

    } else {
	*error_ind = 0;
	*bundle = dt->out_dt.default_edge_bundle_table[index-1];
    }
}


/* INQUIRE PATTERN FACILITIES */
void
pinq_pat_facs(ws_type, error_ind, predefined)
Pint	ws_type;		/* workstation type	*/
Pint	*error_ind;	/* OUT error indicator	*/
Pint	*predefined;	/* OUT number of predefined pattern indices	*/
{
    /* Error 51 is not generated by this function. */

    Wst				*wst = (Wst *)ws_type;
    Wst_phigs_dt		*dt;

    if ( !CB_ENTRY_CHECK( phg_cur_cph, 0, Pfn_INQUIRY)) {
	*error_ind = ERR2;

    } else if ( !phg_cb_wst_exists( wst )) {
	*error_ind = ERR52;

    } else if ( !((dt = &wst->desc_tbl.phigs_dt)->ws_category == PCAT_OUT
	|| dt->ws_category == PCAT_OUTIN || dt->ws_category == PCAT_MO) ) {
	*error_ind = ERR59;

    } else if ( dt->ws_category == PCAT_MO ) {
	*error_ind = ERR62;

    } else {
	*error_ind = 0;
	*predefined = dt->out_dt.num_pattern_types;
    }
}


/* INQUIRE PREDEFINED PATTERN REPRESENTATION */
void
pinq_pred_pat_rep(ws_type, index, store, error_ind, rep)
Pint		ws_type;		/* workstation type	*/
Pint		index;		/* predefined index	*/
Pstore          store;         /* handle to Store object */
Pint		*error_ind;	/* OUT error indicator	*/
Ppat_rep	**rep;		/* OUT predefined pattern rep	*/
{
    /* Error 51 and 62 not generated by this function. */

    Wst				*wst = (Wst *)ws_type;
    Wst_phigs_dt		*dt;

    if ( !CB_ENTRY_CHECK( phg_cur_cph, 0, Pfn_INQUIRY)) {
	*error_ind = ERR2;

    } else if ( !phg_cb_wst_exists( wst )) {
	*error_ind = ERR52;

    } else if ( !((dt = &wst->desc_tbl.phigs_dt)->ws_category == PCAT_OUT
	|| dt->ws_category == PCAT_OUTIN || dt->ws_category == PCAT_MO) ) {
	*error_ind = ERR59;

    } else if ( index < 1 ) {
	*error_ind = ERR112;

    } else if ( index > dt->out_dt.num_pattern_types) {
	*error_ind = ERR102;

    } else {
	int	i, size, num_colrs;
	Pint_size	*dim = &dt->out_dt.pattern_types[index-1].dims;

	*error_ind = 0;
	size = (num_colrs = (dim->size_x * dim->size_y)) * sizeof(Pint);
	if ( CB_STORE_SPACE( ((_Pstore *)store), size, error_ind ) ) {
	    *rep = &((_Pstore *)store)->data.pat_rep;
	    (*rep)->colr_array = (Pint *)((_Pstore *)store)->buf;
	    (*rep)->dims = *dim;
	    if ( num_colrs > 0 ) {
		for ( i = 0; i < num_colrs; i++ )
		    (*rep)->colr_array[i] =
			dt->out_dt.pattern_types[index-1].colr_array[i].ind;
	    }
	}
    }
}


/* INQUIRE PREDEFINED EXTENDED PATTERN REPRESENTATION */
void
pinq_pred_pat_rep_plus(ws_type, index, store, error_ind, rep)
Pint		ws_type;		/* workstation type	*/
Pint		index;		/* predefined index	*/
Pstore		store; 		/* handle to Store object */
Pint		*error_ind;	/* OUT error indicator	*/
Ppat_rep_plus	**rep;	/* OUT predefined pattern rep	*/
{
    /* Error 51 and 62 are not generated by this function. */

    Wst				*wst = (Wst *)ws_type;
    Wst_phigs_dt		*dt;

    if ( !CB_ENTRY_CHECK( phg_cur_cph, 0, Pfn_INQUIRY)) {
	*error_ind = ERR2;

    } else if ( !phg_cb_wst_exists( wst )) {
	*error_ind = ERR52;

    } else if ( !((dt = &wst->desc_tbl.phigs_dt)->ws_category == PCAT_OUT
	|| dt->ws_category == PCAT_OUTIN || dt->ws_category == PCAT_MO) ) {
	*error_ind = ERR59;

    } else if ( index < 1 ) {
	*error_ind = ERR112;

    } else if ( index > dt->out_dt.num_pattern_types) {
	*error_ind = ERR102;

    } else {
	int	size;
	Pint_size	*dim = &dt->out_dt.pattern_types[index-1].dims;

	*error_ind = 0;
	size = (dim->size_x * dim->size_y) * sizeof(Pcoval);
	if ( CB_STORE_SPACE( ((_Pstore *)store), size, error_ind ) ) {
	    *rep = &((_Pstore *)store)->data.ext_pat_rep;
	    (*rep)->colr_array = (Pcoval *)((_Pstore *)store)->buf;
	    (*rep)->dims = *dim;
	    (*rep)->type = dt->out_dt.pattern_types[index-1].type;
	    if ( size > 0 )
		bcopy( (char*)dt->out_dt.pattern_types[index-1].colr_array,
		  (char *)(*rep)->colr_array, size );
	}
    }
}


/* INQUIRE COLOUR MODEL FACILITIES */
void
pinq_colr_model_facs(ws_type, length, start, error_ind, models, total_length, def)
Pint	ws_type;		/* workstation type	*/
Pint	length;		/* length of application list	*/
Pint	start;		/* starting position	*/
Pint	*error_ind;	/* OUT error indicator	*/
Pint_list *models;	/* OUT list of colour models	*/
Pint	*total_length;	/* OUT length of list in PHIGS	*/
Pint	*def;		/* OUT default colour model	*/
{
    /* Error 51 is not generated by this function. */

    Wst				*wst = (Wst *)ws_type;
    Wst_phigs_dt		*dt;

    if ( !CB_ENTRY_CHECK( phg_cur_cph, 0, Pfn_INQUIRY)) {
	*error_ind = ERR2;

    } else if ( !phg_cb_wst_exists( wst )) {
	*error_ind = ERR52;

    } else if ( !((dt = &wst->desc_tbl.phigs_dt)->ws_category == PCAT_OUT
	|| dt->ws_category == PCAT_OUTIN || dt->ws_category == PCAT_MO) ) {
	*error_ind = ERR59;

    } else if ( dt->ws_category == PCAT_MO ) {
	*error_ind = ERR62;

    } else {
	*error_ind = 0;
	*def = dt->out_dt.default_colour_model;
	models->num_ints = 0;
	if ( (*total_length = dt->out_dt.num_colour_models) > 0) {
	    if (start < 0 || start >= dt->out_dt.num_colour_models)
		*error_ind = ERR2201;
	    else if (length > 0) {
		models->num_ints = MIN( length,
				    dt->out_dt.num_colour_models - start);
		bcopy( (char*)&dt->out_dt.colour_models[start],
		    (char*)models->ints, 
		    models->num_ints * sizeof(Pint));
	    } else if (length < 0)
		*error_ind = ERRN153;
	}
    }
}


/* INQUIRE DIRECT COLOUR MODEL FACILITIES */
void
pinq_direct_colr_model_facs(ws_type, length, start, error_ind, 
			   total_length, models)
Pint	ws_type;		/* workstation type	*/
Pint	length;		/* length of application list	*/
Pint	start;		/* starting position	*/
Pint	*error_ind;	/* OUT error indicator	*/
Pint_list	*models;	/* OUT list of colour models	*/
Pint	*total_length;	/* OUT length of list in PHIGS	*/
{

    /* TODO: This function is a duplicate of pinqcolourmodelfacil */
    /* Error 51 is not generated by this function. */

    Wst				*wst = (Wst *)ws_type;
    Wst_phigs_dt		*dt;

    if ( !CB_ENTRY_CHECK( phg_cur_cph, 0, Pfn_INQUIRY)) {
	*error_ind = ERR2;

    } else if ( !phg_cb_wst_exists( wst )) {
	*error_ind = ERR52;

    } else if ( !((dt = &wst->desc_tbl.phigs_dt)->ws_category == PCAT_OUT
	|| dt->ws_category == PCAT_OUTIN || dt->ws_category == PCAT_MO) ) {
	*error_ind = ERR59;

    } else if ( dt->ws_category == PCAT_MO ) {
	*error_ind = ERR62;

    } else {
	*error_ind = 0;
	models->num_ints = 0;
	if ( (*total_length = dt->out_dt.num_colour_models) > 0) {
	    if (start < 0 || start >= dt->out_dt.num_colour_models)
		*error_ind = ERR2201;
	    else if (length > 0) {
		models->num_ints = 
		     MIN( length, dt->out_dt.num_colour_models - start);
		bcopy( (char*)&dt->out_dt.colour_models[start],
		    (char*)models->ints, 
		    models->num_ints * sizeof(Pint));
	    } else if (length < 0)
		*error_ind = ERRN153;
	}
    }
}


/* INQUIRE COLOUR FACILITIES */
void
pinq_colr_facs(ws_type, error_ind, facilities)
Pint 	ws_type;		/* workstation type	*/
Pint    	*error_ind;	/* OUT error indicator	*/
Pcolr_facs	*facilities;	/* OUT colour facilities	*/
{
    /* Error 51 and 62 are not generated by this function. */

    Wst				*wst = (Wst *)ws_type;
    Wst_phigs_dt		*dt;

    if ( !CB_ENTRY_CHECK( phg_cur_cph, 0, Pfn_INQUIRY)) {
	*error_ind = ERR2;

    } else if ( !phg_cb_wst_exists( wst )) {
	*error_ind = ERR52;

    } else if ( wst->bound_status != WST_BOUND) {
	*error_ind = ERR51;

    } else if ( !((dt = &wst->desc_tbl.phigs_dt)->ws_category == PCAT_OUT
	|| dt->ws_category == PCAT_OUTIN || dt->ws_category == PCAT_MO) ) {
	*error_ind = ERR59;

    } else {
	*error_ind = 0;
	facilities->num_colrs = dt->out_dt.num_colours;
	facilities->colr_avail = dt->out_dt.colour_availability;
	facilities->num_pred_inds = dt->out_dt.num_predefined_colours;
	facilities->prim_colrs[0].cieluv_x = dt->out_dt.chroma_info.xr;
	facilities->prim_colrs[0].cieluv_y = dt->out_dt.chroma_info.yr;
	facilities->prim_colrs[0].cieluv_y_lum = dt->out_dt.chroma_info.Yr;
	facilities->prim_colrs[1].cieluv_x = dt->out_dt.chroma_info.xg;
	facilities->prim_colrs[1].cieluv_y = dt->out_dt.chroma_info.yg;
	facilities->prim_colrs[1].cieluv_y_lum = dt->out_dt.chroma_info.Yg;
	facilities->prim_colrs[2].cieluv_x = dt->out_dt.chroma_info.xb;
	facilities->prim_colrs[2].cieluv_y = dt->out_dt.chroma_info.yb;
	facilities->prim_colrs[2].cieluv_y_lum = dt->out_dt.chroma_info.Yb;

    }
}


/* INQUIRE PREDEFINED COLOUR REPRESENTATION */
void
pinq_pred_colr_rep(ws_type, index, error_ind, bundle)
Pint		ws_type;		/* workstation type	*/
Pint		index;		/* predefined index	*/
Pint		*error_ind;	/* OUT error indicator	*/
Pcolr_rep	*bundle;	/* OUT predefined colour rep	*/
{
    /* Error 51 and 62 are not generated by this function. */

    Wst				*wst = (Wst *)ws_type;
    Wst_phigs_dt		*dt;

    if ( !CB_ENTRY_CHECK( phg_cur_cph, 0, Pfn_INQUIRY)) {
	*error_ind = ERR2;

    } else if ( !phg_cb_wst_exists( wst )) {
	*error_ind = ERR52;

    } else if ( !((dt = &wst->desc_tbl.phigs_dt)->ws_category == PCAT_OUT
	|| dt->ws_category == PCAT_OUTIN || dt->ws_category == PCAT_MO) ) {
	*error_ind = ERR59;

    } else if ( index < 0 ) {
	*error_ind = ERR113;

    } else if ( index >= dt->out_dt.num_predefined_colours) {
	*error_ind = ERR102;

    } else {
	*error_ind = 0;
	*bundle = dt->out_dt.default_colour_table[index];
    }
}


/* INQUIRE LIST OF AVAILABLE GDP 3 */
void
pinq_list_avail_gdp3(ws_type, length, start, error_ind, gdps, total_length)
Pint	ws_type;		/* workstation type	*/
Pint	length;		/* length of application list	*/
Pint	start;		/* starting position	*/
Pint	*error_ind;	/* OUT error indicator	*/
Pint_list	*gdps;		/* OUT list of GDPs	*/
Pint	*total_length;	/* OUT length of list in PHIGS	*/
{
    /* Error 51 and 62 are not generated by this function. */

    Wst				*wst = (Wst *)ws_type;
    Wst_phigs_dt		*dt;

    if ( !CB_ENTRY_CHECK( phg_cur_cph, 0, Pfn_INQUIRY)) {
	*error_ind = ERR2;

    } else if ( !phg_cb_wst_exists( wst )) {
	*error_ind = ERR52;

    } else if ( !((dt = &wst->desc_tbl.phigs_dt)->ws_category == PCAT_OUT
	|| dt->ws_category == PCAT_OUTIN || dt->ws_category == PCAT_MO) ) {
	*error_ind = ERR59;

    } else {
	*error_ind = 0;
	gdps->num_ints = 0;
	if ( (*total_length = dt->out_dt.num_gdp3) > 0) {
	    if (start < 0 || start >= dt->out_dt.num_gdp3)
		*error_ind = ERR2201;
	    else if (length > 0) {
		gdps->num_ints = MIN( length, dt->out_dt.num_gdp3 - start);
		bcopy( (char*)&dt->out_dt.gdp3_ids[start],
		    (char*)gdps->ints, 
		    gdps->num_ints * sizeof(Pint));
	    } else if (length < 0)
		*error_ind = ERRN153;
	}
    }
}


/* INQUIRE LIST OF AVAILABLE GDP */
void
pinq_list_avail_gdp(ws_type, length, start, error_ind, gdps, total_length)
Pint	ws_type;		/* workstation type	*/
Pint	length;		/* length of application list	*/
Pint	start;		/* starting position	*/
Pint	*error_ind;	/* OUT error indicator	*/
Pint_list	*gdps;		/* OUT list of GDPs	*/
Pint	*total_length;	/* OUT length of list in PHIGS	*/
{
    /* Error 51 and 62 are not generated by this function. */

    Wst				*wst = (Wst *)ws_type;
    Wst_phigs_dt		*dt;

    if ( !CB_ENTRY_CHECK( phg_cur_cph, 0, Pfn_INQUIRY)) {
	*error_ind = ERR2;

    } else if ( !phg_cb_wst_exists( wst )) {
	*error_ind = ERR52;

    } else if ( !((dt = &wst->desc_tbl.phigs_dt)->ws_category == PCAT_OUT
	|| dt->ws_category == PCAT_OUTIN || dt->ws_category == PCAT_MO) ) {
	*error_ind = ERR59;

    } else {
	*error_ind = 0;
	gdps->num_ints = 0;
	if ( (*total_length = dt->out_dt.num_gdp) > 0) {
	    if (start < 0 || start >= dt->out_dt.num_gdp)
		*error_ind = ERR2201;
	    else if (length > 0) {
		gdps->num_ints = MIN( length, dt->out_dt.num_gdp - start);
		bcopy( (char*)&dt->out_dt.gdp_ids[start],
		    (char*)gdps->ints, 
		    gdps->num_ints * sizeof(Pint));
	    } else if (length < 0)
		*error_ind = ERRN153;
	}
    }
}

static int
find_gdp_id( id,  n, list)
Pint	id;
Pint	n;
Pint	*list;
{
    register int i;
    for (i = 0 ; i < n ; i++)
	if (list[i] == id) return i;	/* 0 is a valid return value */
    return -1;
}

void
pinq_gdp3( ws_type, gdp, error_ind, num_attrs, attrs )
    Pint	ws_type;		/* workstation type	*/
    Pint	gdp;		/* GDP function number	*/
    Pint	*error_ind;	/* OUT error indicator	*/
    Pint	*num_attrs;	/* OUT number of attributes */
    Pattrs	attrs[5];	/* OUT list of attributes */
{
    /* Error 51 and 62 are not generated by this function. */

    Wst				*wst = (Wst *)ws_type;
    Wst_phigs_dt		*dt;
    int				ind;

    if ( !CB_ENTRY_CHECK( phg_cur_cph, 0, Pfn_INQUIRY)) {
	*error_ind = ERR2;

    } else if ( !phg_cb_wst_exists( wst )) {
	*error_ind = ERR52;

    } else if ( !((dt = &wst->desc_tbl.phigs_dt)->ws_category == PCAT_OUT
	|| dt->ws_category == PCAT_OUTIN || dt->ws_category == PCAT_MO) ) {
	*error_ind = ERR59;

    } else if ( (ind = find_gdp_id( gdp, dt->out_dt.num_gdp3,
	    dt->out_dt.gdp3_ids) ) < 0 ) {
	*error_ind = ERR64;

    } else {
	Wst_gdp_attrs		*gdp_attrs;

	*error_ind = 0;
	gdp_attrs = &dt->out_dt.gdp3_attrs[ind];
	*num_attrs = MIN(5,gdp_attrs->number);
	if ( gdp_attrs->number > 0 )
	    bcopy( (char*)gdp_attrs->attrs, (char*)attrs, 
		gdp_attrs->number * sizeof(Pattrs));
    }
}

void
pinq_gdp( ws_type, gdp, error_ind, num_attrs, attrs )
    Pint	ws_type;		/* workstation type	*/
    Pint	gdp;		/* GDP function number	*/
    Pint	*error_ind;	/* OUT error indicator	*/
    Pint	*num_attrs;	/* OUT number of attributes */
    Pattrs	attrs[5];		/* OUT list of attributes */
{
    /* Error 51 and 62 are not generated by this function. */

    Wst				*wst = (Wst *)ws_type;
    Wst_phigs_dt		*dt;
    int				ind;

    if ( !CB_ENTRY_CHECK( phg_cur_cph, 0, Pfn_INQUIRY)) {
	*error_ind = ERR2;

    } else if ( !phg_cb_wst_exists( wst )) {
	*error_ind = ERR52;

    } else if ( !((dt = &wst->desc_tbl.phigs_dt)->ws_category == PCAT_OUT
	|| dt->ws_category == PCAT_OUTIN || dt->ws_category == PCAT_MO) ) {
	*error_ind = ERR59;

    } else if ( (ind = find_gdp_id(gdp, dt->out_dt.num_gdp,
    				   dt->out_dt.gdp_ids)) < 0) {
	*error_ind = ERR64;

    } else {
	Wst_gdp_attrs		*gdp_attrs;

	*error_ind = 0;
	gdp_attrs = &dt->out_dt.gdp_attrs[ind];
	*num_attrs = MIN(5,gdp_attrs->number);
	if ( gdp_attrs->number > 0 )
	    bcopy( (char*)gdp_attrs->attrs, (char*)attrs, 
		gdp_attrs->number * sizeof(Pattrs));
    }
}

void
pinq_list_avail_gse(ws_type, length, start, error_ind, gses, total_length)
Pint	ws_type;		/* workstation type	*/
Pint	length;		/* length of application list	*/
Pint	start;		/* starting position	*/
Pint	*error_ind;	/* OUT error indicator	*/
Pint_list	*gses;		/* OUT list of GSEs	*/
Pint	*total_length;	/* OUT length of list in PHIGS	*/
{
    /* Error 51 is not generated by this function. */

    Wst				*wst = (Wst *)ws_type;
    Wst_phigs_dt		*dt;

    if ( !CB_ENTRY_CHECK( phg_cur_cph, 0, Pfn_INQUIRY)) {
	*error_ind = ERR2;

    } else if ( !phg_cb_wst_exists( wst )) {
	*error_ind = ERR52;

    } else if ( !((dt = &wst->desc_tbl.phigs_dt)->ws_category == PCAT_OUT
	|| dt->ws_category == PCAT_OUTIN || dt->ws_category == PCAT_MO) ) {
	*error_ind = ERR59;

    } else if ( dt->ws_category == PCAT_MO ) {
	*error_ind = ERR62;

    } else {
	*error_ind = 0;
	gses->num_ints = 0;
	if ( (*total_length = dt->out_dt.num_gse) > 0) {
	    if (start < 0 || start >= dt->out_dt.num_gse)
		*error_ind = ERR2201;
	    else if (length > 0) {
		gses->num_ints = MIN( length, dt->out_dt.num_gse - start);
		if ( gses->num_ints > 0)
		    bcopy( (char*)&dt->out_dt.gse_ids[start],
		        (char*)gses->ints, 
			gses->num_ints * sizeof(Pint));
	    } else if (length < 0)
		*error_ind = ERRN153;
	}
    }
}


/* INQUIRE WORKSTATION TABLE LENGTHS */
void
pinq_ws_st_table(ws_type, error_ind, lengths)
Pint		ws_type;		/* workstation type	*/
Pint		*error_ind;	/* OUT error indicator	*/
Pws_st_tables	*lengths;	/* OUT lengths of workstation tables	*/
{
    /* Error 51 and 62 are not generated by this function. */

    Wst				*wst = (Wst *)ws_type;
    Wst_phigs_dt		*dt;

    if ( !CB_ENTRY_CHECK( phg_cur_cph, 0, Pfn_INQUIRY)) {
	*error_ind = ERR2;

    } else if ( !phg_cb_wst_exists( wst )) {
	*error_ind = ERR52;

    } else if ( !((dt = &wst->desc_tbl.phigs_dt)->ws_category == PCAT_OUT
	|| dt->ws_category == PCAT_OUTIN || dt->ws_category == PCAT_MO) ) {
	*error_ind = ERR59;
    
    } else {
	*error_ind = 0;
	lengths->line_bundles = dt->out_dt.num_polyline_bundle_entries;
	lengths->mark_bundles = dt->out_dt.num_polymarker_bundle_entries;
	lengths->text_bundles = dt->out_dt.num_text_bundle_entries;
	lengths->int_bundles = dt->out_dt.num_interior_bundle_entries;
	lengths->edge_bundles = dt->out_dt.num_edge_bundle_entries;
	lengths->pat_reps = dt->out_dt.num_pattern_table_entries;
	lengths->colr_reps = dt->out_dt.num_colour_indices;
	lengths->view_reps = dt->num_view_indices;
    }
}


/* INQUIRE EXTENDED WORKSTATION TABLE LENGTHS */
void
pinq_ws_st_table_plus(ws_type, error_ind, lengths)
Pint		ws_type;		/* workstation type	*/
Pint		*error_ind;	/* OUT error indicator	*/
Pws_tables_plus	*lengths;	/* OUT lengths of workstation tables	*/
{
    /* Error 51 is not generated by this function. */

    Wst				*wst = (Wst *)ws_type;
    Wst_phigs_dt		*dt;

    if ( !CB_ENTRY_CHECK( phg_cur_cph, 0, Pfn_INQUIRY)) {
	*error_ind = ERR2;

    } else if ( !phg_cb_wst_exists( wst )) {
	*error_ind = ERR52;

    } else if ( !((dt = &wst->desc_tbl.phigs_dt)->ws_category == PCAT_OUT
	|| dt->ws_category == PCAT_OUTIN || dt->ws_category == PCAT_MO) ) {
	*error_ind = ERR59;

    } else {
	*error_ind = 0;
	lengths->line_bundles = dt->out_dt.num_polyline_bundle_entries;
	lengths->mark_bundles = dt->out_dt.num_polymarker_bundle_entries;
	lengths->text_bundles = dt->out_dt.num_text_bundle_entries;
	lengths->int_bundles = dt->out_dt.num_interior_bundle_entries;
	lengths->edge_bundles = dt->out_dt.num_edge_bundle_entries;
	lengths->pat_reps = dt->out_dt.num_pattern_table_entries;
	lengths->colr_reps = dt->out_dt.num_colour_indices;
	lengths->view_reps = dt->num_view_indices;
	lengths->dcue_rep = dt->out_dt.num_depth_cue_bundle_entries;
	lengths->light_src_rep = dt->out_dt.num_light_src_bundle_entries;
	/* TODO: */
	lengths->colr_map_rep = 1;
    }
}

void
pinq_dyns_structs(ws_type, error_ind, dynamics)
Pint		ws_type;		/* workstation type	*/
Pint		*error_ind;	/* OUT error indicator	*/
Pdyns_structs	*dynamics;	/* OUT structure dynamics	*/
{
    /* Error 51 and 62 are not generated by this function. */

    Wst				*wst = (Wst *)ws_type;
    Wst_phigs_dt		*dt;

    if ( !CB_ENTRY_CHECK( phg_cur_cph, 0, Pfn_INQUIRY)) {
	*error_ind = ERR2;

    } else if ( !phg_cb_wst_exists( wst )) {
	*error_ind = ERR52;

    } else if ( !((dt = &wst->desc_tbl.phigs_dt)->ws_category == PCAT_OUT
	|| dt->ws_category == PCAT_OUTIN || dt->ws_category == PCAT_MO) ) {
	*error_ind = ERR59;

    } else {
	*error_ind = 0;
	dynamics->content = dt->out_dt.struct_content_mod;
	dynamics->post = dt->out_dt.post;
	dynamics->unpost = dt->out_dt.unpost;
	dynamics->del = dt->out_dt.struct_delete;
	dynamics->ref = dt->out_dt.ref_mod;
    }
}

void
pinq_num_avail_in(ws_type, error_ind, numbers)
Pint 	ws_type;		/* workstation type	*/
Pint    	*error_ind;	/* OUT error indicator	*/
Pnum_in	*numbers;	/* OUT number of input devices	*/
{
    Wst				*wst = (Wst *)ws_type;
    Wst_phigs_dt		*dt;

    if ( !CB_ENTRY_CHECK( phg_cur_cph, 0, Pfn_INQUIRY)) {
	*error_ind = ERR2;

    } else if ( !phg_cb_wst_exists( wst )) {
	*error_ind = ERR52;

    } else if ( wst->bound_status != WST_BOUND) {
	*error_ind = ERR51;

    } else if ( !((dt = &wst->desc_tbl.phigs_dt)->ws_category == PCAT_IN
	|| dt->ws_category == PCAT_OUTIN) ) {
	*error_ind = ERR61;

    } else {
	*error_ind = 0;
	*numbers = dt->in_dt.num_devs;
    }
}

static void
copy_pet_list( num, list, store, error_ind, pets )
    Pint	num;
    Pint	*list;
    Pstore	store;
    Pint	*error_ind;
    Pint_list	**pets;	     /* Note: *pets must already be set upon entry */
{
    int         size = num * sizeof(Pint);
    
    (*pets)->num_ints = num;
    if ( num > 0 ) {
	if (CB_STORE_SPACE( ((_Pstore *)store), size, error_ind ))
	    bcopy( (char*)list, (char*)((_Pstore *)store)->buf, size );
    }
    (*pets)->ints = (Pint *)((_Pstore *)store)->buf;
}

void
pinq_def_loc_data3(ws_type, device, store, error_ind, loc_pos, pets, echo_vol, loc_data)
Pint		ws_type;	/* workstation type	             */
Pint		device;		/* logical input device number	     */
Pstore		store;		/* handle to Store object            */
Pint		*error_ind;	/* OUT error indicator	             */
Ppoint3         *loc_pos;       /* OUT default initial position      */
Pint_list       **pets;         /* OUT list of prompt and echo types */
Plimit3         *echo_vol;      /* OUT default echo volume           */
Ploc_data3      **loc_data;     /* OUT default data record           */
{
    /* Error 51 is not generated by this function. */

    Wst				*wst = (Wst *)ws_type;
    		Wst_phigs_dt	*dt;
    register	Wst_defloc	*defloc;

    if ( !CB_ENTRY_CHECK( phg_cur_cph, 0, Pfn_INQUIRY)) {
	*error_ind = ERR2;

    } else if ( !phg_cb_wst_exists( wst )) {
	*error_ind = ERR52;

    } else if ( !((dt = &wst->desc_tbl.phigs_dt)->ws_category == PCAT_IN
	|| dt->ws_category == PCAT_OUTIN) ) {
	*error_ind = ERR61;

    } else if ( device < 1 || device > dt->in_dt.num_devs.loc) {
	*error_ind = ERR250;

    } else {
	int	size;
	*error_ind = 0;
	defloc = &dt->in_dt.locators[device-1];
	*loc_pos = defloc->position;
	*echo_vol = defloc->e_volume;
	*loc_data = &((_Pstore *)store)->data.loc_data3.drec;
	**loc_data = defloc->record;
	*pets = &((_Pstore *)store)->data.loc_data3.pets;
	copy_pet_list( defloc->num_pets, defloc->pets, store, error_ind,
	    pets );
    }
}

void
pinq_def_loc_data(ws_type, device, store, error_ind, loc_pos, pets, echo_area, loc_data)
Pint	   ws_type;		/* workstation type	*/
Pint	   device;		/* logical input device number	*/
Pstore	   store;		/* handle to Store object       */
Pint	   *error_ind;      	/* OUT error indicator	*/
Ppoint     *loc_pos;            /* OUT default initial position  */
Pint_list  **pets;              /* OUT list of prompt and echo types */
Plimit     *echo_area;           /* OUT default echo volume       */
Ploc_data  **loc_data;          /* OUT default data record       */
{
    /* Error 51 is not generated by this function. */

    Wst				*wst = (Wst *)ws_type;
    Wst_phigs_dt		*dt;
    register Wst_defloc		*defloc;

    if ( !CB_ENTRY_CHECK( phg_cur_cph, 0, Pfn_INQUIRY)) {
	*error_ind = ERR2;

    } else if ( !phg_cb_wst_exists( wst )) {
	*error_ind = ERR52;

    } else if ( !((dt = &wst->desc_tbl.phigs_dt)->ws_category == PCAT_IN
	|| dt->ws_category == PCAT_OUTIN) ) {
	*error_ind = ERR61;

    } else if ( device < 1 || device > dt->in_dt.num_devs.loc) {
	*error_ind = ERR250;

    } else {
	int	size;
	*error_ind = 0;
	defloc = &(dt->in_dt.locators[device-1]);
	loc_pos->x = defloc->position.x;
	loc_pos->y = defloc->position.y;
	CB_ECHO_VOLUME_TO_AREA( defloc->e_volume, *echo_area );
	*loc_data = &((_Pstore *)store)->data.loc_data.drec;
	**loc_data = *(Ploc_data *)&defloc->record;
	*pets = &((_Pstore *)store)->data.loc_data3.pets;
	copy_pet_list( defloc->num_pets, defloc->pets, store, error_ind,
	    pets );
    }
}

void
pinq_def_stroke_data3( ws_type, device, store, error_ind, max_buf_size, pets, echo_volume, stroke_data )
Pint		ws_type;	/* workstation type	*/
Pint		device;		/* logical input device number	*/
Pstore          store;          /* handle to Store object       */
Pint            *error_ind;     /* OUT error indicator          */
Pint            *max_buf_size;  /* OUT max. input buffer size   */
Pint_list       **pets;         /* OUT list of prompt and echo types */
Plimit3         *echo_volume;   /* OUT default echo volume      */
Pstroke_data3   **stroke_data;  /* OUT default data record      */
{
    /* Error 51 is not generated by this function. */

    Wst				*wst = (Wst *)ws_type;
    Wst_phigs_dt		*dt;
    register Wst_defstroke	*defstroke;

    if ( !CB_ENTRY_CHECK( phg_cur_cph, 0, Pfn_INQUIRY)) {
	*error_ind = ERR2;

    } else if ( !phg_cb_wst_exists( wst )) {
	*error_ind = ERR52;

    } else if ( !((dt = &wst->desc_tbl.phigs_dt)->ws_category == PCAT_IN
	|| dt->ws_category == PCAT_OUTIN) ) {
	*error_ind = ERR61;

    } else if ( device < 1 || device > dt->in_dt.num_devs.stroke) {
	*error_ind = ERR250;

    } else {
	int	size;
	*error_ind = 0;
	defstroke = &dt->in_dt.strokes[device-1];
	*max_buf_size = defstroke->max_bufsize;
	*echo_volume = defstroke->e_volume;
	*stroke_data = &((_Pstore *)store)->data.stroke_data3.drec;
	**stroke_data = defstroke->record;
	*pets = &((_Pstore *)store)->data.loc_data3.pets;
	copy_pet_list( defstroke->num_pets, defstroke->pets, store, error_ind,
	    pets );
    }
}

void
pinq_def_stroke_data(ws_type, device, store, error_ind, max_buf_size, pets, echo_area, stroke_data)
Pint		ws_type;	/* workstation type	*/
Pint		device;		/* logical input device number	*/
Pstore          store;          /* handle to Store object       */
Pint            *error_ind;     /* OUT error indicator          */
Pint            *max_buf_size;  /* OUT max. input buffer size   */
Pint_list       **pets;         /* OUT list of prompt and echo types */
Plimit          *echo_area;     /* OUT default echo volume      */
Pstroke_data    **stroke_data;  /* OUT default data record      */
{
    /* Error 51 is not generated by this function. */

    Wst				*wst = (Wst *)ws_type;
    Wst_phigs_dt		*dt;
    register Wst_defstroke	*defstroke;

    if ( !CB_ENTRY_CHECK( phg_cur_cph, 0, Pfn_INQUIRY)) {
	*error_ind = ERR2;

    } else if ( !phg_cb_wst_exists( wst )) {
	*error_ind = ERR52;

    } else if ( !((dt = &wst->desc_tbl.phigs_dt)->ws_category == PCAT_IN
	|| dt->ws_category == PCAT_OUTIN) ) {
	*error_ind = ERR61;

    } else if ( device < 1 || device > dt->in_dt.num_devs.stroke) {
	*error_ind = ERR250;

    } else {
	int	size;
	*error_ind = 0;
	defstroke = &dt->in_dt.strokes[device-1];
	*max_buf_size = defstroke->max_bufsize;
	CB_ECHO_VOLUME_TO_AREA( defstroke->e_volume, *echo_area );
	*stroke_data = &((_Pstore *)store)->data.stroke_data.drec;
	(*stroke_data)->buffer_size = defstroke->record.buffer_size;
	(*stroke_data)->init_pos = defstroke->record.init_pos;
	(*stroke_data)->x_interval = defstroke->record.x_interval;
	(*stroke_data)->y_interval = defstroke->record.y_interval;
	(*stroke_data)->time_interval = defstroke->record.time_interval;
	*pets = &((_Pstore *)store)->data.loc_data3.pets;
	copy_pet_list( defstroke->num_pets, defstroke->pets, store, error_ind,
	    pets );
    }
}

void
pinq_def_val_data3(ws_type, device, store, error_ind, def_value, pets, echo_vol, val_data)
Pint		ws_type;	/* workstation type	*/
Pint		device;		/* logical input device number	*/
Pstore          store;          /* handle to Store object       */
Pint            *error_ind;     /* OUT error indicator          */
Pfloat          *def_value;     /* OUT default initial value    */
Pint_list       **pets;         /* OUT list of prompt and echo types */
Plimit3         *echo_vol;      /* OUT default echo volume      */
Pval_data3      **val_data;     /* OUT default data record      */
{
    /* Error 51 is not generated by this function. */

    Wst				*wst = (Wst *)ws_type;
    		Wst_phigs_dt	*dt;
    register	Wst_defval	*defval;

    if ( !CB_ENTRY_CHECK( phg_cur_cph, 0, Pfn_INQUIRY)) {
	*error_ind = ERR2;

    } else if ( !phg_cb_wst_exists( wst )) {
	*error_ind = ERR52;

    } else if ( !((dt = &wst->desc_tbl.phigs_dt)->ws_category == PCAT_IN
	|| dt->ws_category == PCAT_OUTIN) ) {
	*error_ind = ERR61;

    } else if ( device < 1 || device > dt->in_dt.num_devs.val) {
	*error_ind = ERR250;

    } else {
	int	size;
	*error_ind = 0;
	defval = &dt->in_dt.valuators[device-1];
	*def_value = defval->value;
	*echo_vol = defval->e_volume;
	*val_data = &((_Pstore *)store)->data.val_data3.drec;
	**val_data = defval->record;
	*pets = &((_Pstore *)store)->data.loc_data3.pets;
	copy_pet_list( defval->num_pets, defval->pets, store, error_ind,
	    pets );
    }
}

void
pinq_def_val_data(ws_type, device, store, error_ind, def_value, pets, echo_area, val_data)
Pint 		ws_type;	/* workstation type	*/
Pint    	device;		/* logical input device number	*/
Pstore          store;          /* handle to Store object       */
Pint            *error_ind;     /* OUT error indicator          */
Pfloat          *def_value;     /* OUT default initial value    */
Pint_list       **pets;         /* OUT list of prompt and echo types */
Plimit          *echo_area;     /* OUT default echo volume      */
Pval_data       **val_data;     /* OUT default data record      */
{
    /* Error 51 is not generated by this function. */

    Wst				*wst = (Wst *)ws_type;
    Wst_phigs_dt		*dt;
    register Wst_defval		*defval;

    if ( !CB_ENTRY_CHECK( phg_cur_cph, 0, Pfn_INQUIRY)) {
	*error_ind = ERR2;

    } else if ( !phg_cb_wst_exists( wst )) {
	*error_ind = ERR52;

    } else if ( !((dt = &wst->desc_tbl.phigs_dt)->ws_category == PCAT_IN
	|| dt->ws_category == PCAT_OUTIN) ) {
	*error_ind = ERR61;

    } else if ( device < 1 || device > dt->in_dt.num_devs.val) {
	*error_ind = ERR250;

    } else {
	int	size;
	*error_ind = 0;
	defval = &dt->in_dt.valuators[device-1];
	*def_value = defval->value;
	CB_ECHO_VOLUME_TO_AREA( defval->e_volume, *echo_area );
	*val_data = &((_Pstore *)store)->data.val_data3.drec;
	**val_data = *(Pval_data *)&defval->record;
	*pets = &((_Pstore *)store)->data.loc_data3.pets;
	copy_pet_list( defval->num_pets, defval->pets, store, error_ind,
	    pets );
    }
}

void
pinq_def_choice_data3(ws_type, device, store, error_ind, max_choices, pets, echo_vol, choice_data)
Pint		ws_type;	/* workstation type	*/
Pint		device;		/* logical input device number	*/
Pstore          store;          /* handle to Store object       */
Pint            *error_ind;     /* OUT error indicator          */
Pint            *max_choices;   /* OUT max. number of choices   */
Pint_list       **pets;         /* OUT list of prompt and echo types */
Plimit3         *echo_vol;      /* OUT default echo volume      */
Pchoice_data3   **choice_data;  /* OUT default data record      */
{
    /* Error 51 is not generated by this function. */

    Wst				*wst = (Wst *)ws_type;
    		Wst_phigs_dt	*dt;
    register	Wst_defchoice	*defchoice;

    if ( !CB_ENTRY_CHECK( phg_cur_cph, 0, Pfn_INQUIRY)) {
	*error_ind = ERR2;

    } else if ( !phg_cb_wst_exists( wst )) {
	*error_ind = ERR52;

    } else if ( !((dt = &wst->desc_tbl.phigs_dt)->ws_category == PCAT_IN
	|| dt->ws_category == PCAT_OUTIN) ) {
	*error_ind = ERR61;

    } else if ( device < 1 || device > dt->in_dt.num_devs.choice) {
	*error_ind = ERR250;

    } else {
	int	size;
	*error_ind = 0;
	defchoice = &dt->in_dt.choices[device-1];
	*max_choices = defchoice->choices;
	*echo_vol = defchoice->e_volume;
	*choice_data = &((_Pstore *)store)->data.choice_data3.drec;
	**choice_data = defchoice->record;
	*pets = &((_Pstore *)store)->data.loc_data3.pets;
	copy_pet_list( defchoice->num_pets, defchoice->pets, store, error_ind,
	    pets );
    }
}

void
pinq_def_choice_data(ws_type, device, store, error_ind, max_choices, pets, echo_area, choice_data)
Pint		ws_type;	/* workstation type	*/
Pint		device;		/* logical input device number	*/
Pstore          store;          /* handle to Store object       */
Pint            *error_ind;     /* OUT error indicator          */
Pint            *max_choices;   /* OUT max. number of choices   */
Pint_list       **pets;         /* OUT list of prompt and echo types */
Plimit          *echo_area;     /* OUT default echo volume      */
Pchoice_data    **choice_data;  /* OUT default data record      */
{
    /* Error 51 is not generated by this function. */

    Wst				*wst = (Wst *)ws_type;
    Wst_phigs_dt		*dt;
    register Wst_defchoice	*defchoice;

    if ( !CB_ENTRY_CHECK( phg_cur_cph, 0, Pfn_INQUIRY)) {
	*error_ind = ERR2;

    } else if ( !phg_cb_wst_exists( wst )) {
	*error_ind = ERR52;

    } else if ( !((dt = &wst->desc_tbl.phigs_dt)->ws_category == PCAT_IN
	|| dt->ws_category == PCAT_OUTIN) ) {
	*error_ind = ERR61;

    } else if ( device < 1 || device > dt->in_dt.num_devs.choice) {
	*error_ind = ERR250;

    } else {
	int	size;
	*error_ind = 0;
	defchoice = &dt->in_dt.choices[device-1];
	*max_choices = defchoice->choices;
	CB_ECHO_VOLUME_TO_AREA( defchoice->e_volume, *echo_area );
	*choice_data = &((_Pstore *)store)->data.choice_data.drec;
	**choice_data = *(Pchoice_data *)&defchoice->record;
	*pets = &((_Pstore *)store)->data.loc_data3.pets;
	copy_pet_list( defchoice->num_pets, defchoice->pets, store, error_ind,
	    pets );
    }
}

void
pinq_def_pick_data3(ws_type, device, store, error_ind, pets, echo_vol, pick_data)
Pint		ws_type;	/* workstation type	*/
Pint		device;		/* logical input device number	*/
Pstore          store;          /* handle to Store object       */
Pint            *error_ind;     /* OUT error indicator          */
Pint_list       **pets;         /* OUT list of prompt and echo types */
Plimit3         *echo_vol;      /* OUT default echo volume      */
Ppick_data3     **pick_data;    /* OUT default data record      */
{
    /* Error 51 is not generated by this function. */

    Wst				*wst = (Wst *)ws_type;
    Wst_phigs_dt		*dt;
    register Wst_defpick	*defpick;

    if ( !CB_ENTRY_CHECK( phg_cur_cph, 0, Pfn_INQUIRY)) {
	*error_ind = ERR2;

    } else if ( !phg_cb_wst_exists( wst )) {
	*error_ind = ERR52;

    } else if ( !((dt = &wst->desc_tbl.phigs_dt)->ws_category == PCAT_IN
	|| dt->ws_category == PCAT_OUTIN) ) {
	*error_ind = ERR61;

    } else if ( device < 1 || device > dt->in_dt.num_devs.pick ) {
	*error_ind = ERR250;

    } else {
	int	size;
	*error_ind = 0;
	defpick = &dt->in_dt.picks[device-1];
	*echo_vol = defpick->e_volume;
	*pick_data = &((_Pstore *)store)->data.pick_data3.drec;
	**pick_data = defpick->record;
	*pets = &((_Pstore *)store)->data.loc_data3.pets;
	copy_pet_list( defpick->num_pets, defpick->pets, store, error_ind,
	    pets );
    }
}

void
pinq_def_pick_data(ws_type, device, store, error_ind, pets, echo_area, pick_data)
Pint		ws_type;	/* workstation type	*/
Pint		device;		/* logical input device number	*/
Pstore          store;          /* handle to Store object       */
Pint            *error_ind;     /* OUT error indicator          */
Pint_list       **pets;         /* OUT list of prompt and echo types */
Plimit          *echo_area;     /* OUT default echo volume      */
Ppick_data      **pick_data;    /* OUT default data record      */
{
    /* Error 51 is not generated by this function. */

    Wst				*wst = (Wst *)ws_type;
    Wst_phigs_dt		*dt;
    register Wst_defpick	*defpick;

    if ( !CB_ENTRY_CHECK( phg_cur_cph, 0, Pfn_INQUIRY)) {
	*error_ind = ERR2;

    } else if ( !phg_cb_wst_exists( wst )) {
	*error_ind = ERR52;

    } else if ( !((dt = &wst->desc_tbl.phigs_dt)->ws_category == PCAT_IN
	|| dt->ws_category == PCAT_OUTIN) ) {
	*error_ind = ERR61;

    } else if ( device < 1 || device > dt->in_dt.num_devs.pick ) {
	*error_ind = ERR250;

    } else {
	int	size;
	*error_ind = 0;
	defpick = &dt->in_dt.picks[device-1];
	CB_ECHO_VOLUME_TO_AREA( defpick->e_volume, *echo_area );
	*pick_data = &((_Pstore *)store)->data.pick_data.drec;
	**pick_data = *(Ppick_data *)&defpick->record;
	*pets = &((_Pstore *)store)->data.loc_data3.pets;
	copy_pet_list( defpick->num_pets, defpick->pets, store, error_ind,
	    pets );
    }
}

void
pinq_def_string_data3(ws_type, device, store, error_ind, max_buf_size, pets, echo_vol, string_data)
Pint		ws_type;	/* workstation type	*/
Pint		device;		/* logical input device number	*/
Pstore          store;          /* handle to Store object       */
Pint            *error_ind;     /* OUT error indicator          */
Pint            *max_buf_size;  /* OUT max. input buffer size   */
Pint_list       **pets;         /* OUT list of prompt and echo types */
Plimit3         *echo_vol;      /* OUT default echo volume      */
Pstring_data3   **string_data;  /* OUT default data record      */
{
    /* Error 51 is not generated by this function. */

    Wst				*wst = (Wst *)ws_type;
    Wst_phigs_dt		*dt;
    register Wst_defstring	*defstring;

    if ( !CB_ENTRY_CHECK( phg_cur_cph, 0, Pfn_INQUIRY)) {
	*error_ind = ERR2;

    } else if ( !phg_cb_wst_exists( wst )) {
	*error_ind = ERR52;

    } else if ( !((dt = &wst->desc_tbl.phigs_dt)->ws_category == PCAT_IN
	|| dt->ws_category == PCAT_OUTIN) ) {
	*error_ind = ERR61;

    } else if ( device < 1 || device > dt->in_dt.num_devs.string) {
	*error_ind = ERR250;

    } else {
	int	size;
	*error_ind = 0;
	defstring = &dt->in_dt.strings[device-1];
	*max_buf_size = defstring->max_bufsize;
	*echo_vol = defstring->e_volume;
	*string_data = &((_Pstore *)store)->data.string_data3.drec;
	**string_data = defstring->record;
	*pets = &((_Pstore *)store)->data.loc_data3.pets;
	copy_pet_list( defstring->num_pets, defstring->pets, store, error_ind,
	    pets );
    }
}

void
pinq_def_string_data(ws_type, device, store, error_ind, max_buf_size, pets, echo_area, string_data)
Pint		ws_type;	/* workstation type	*/
Pint		device;		/* logical input device number	*/
Pstore          store;          /* handle to Store object       */
Pint            *error_ind;     /* OUT error indicator          */
Pint            *max_buf_size;  /* OUT max. input buffer size   */
Pint_list       **pets;         /* OUT list of prompt and echo types */
Plimit          *echo_area;     /* OUT default echo volume      */
Pstring_data    **string_data;  /* OUT default data record      */
{
    /* Error 51 is not generated by this function. */

    Wst				*wst = (Wst *)ws_type;
    Wst_phigs_dt		*dt;
    register Wst_defstring	*defstring;

    if ( !CB_ENTRY_CHECK( phg_cur_cph, 0, Pfn_INQUIRY)) {
	*error_ind = ERR2;

    } else if ( !phg_cb_wst_exists( wst )) {
	*error_ind = ERR52;

    } else if ( !((dt = &wst->desc_tbl.phigs_dt)->ws_category == PCAT_IN
	|| dt->ws_category == PCAT_OUTIN) ) {
	*error_ind = ERR61;

    } else if ( device < 1 || device > dt->in_dt.num_devs.string) {
	*error_ind = ERR250;

    } else {
	int	size;
	*error_ind = 0;
	defstring = &dt->in_dt.strings[device-1];
	*max_buf_size = defstring->max_bufsize;
	CB_ECHO_VOLUME_TO_AREA( defstring->e_volume, *echo_area );
	*string_data = &((_Pstore *)store)->data.string_data.drec;
	**string_data = *(Pstring_data *)&defstring->record;
	*pets = &((_Pstore *)store)->data.loc_data3.pets;
	copy_pet_list( defstring->num_pets, defstring->pets, store, 
	    error_ind, pets );
    } 
} 

#define COPY_LIST(dt,ilist,start,facil,olist) \
    bcopy((char*)&(dt)->out_dt.ilist[start],  \
	(char*)(facil)->olist.ints, \
	(facil)->olist.num_ints * sizeof(Pint))

void
pinq_curv_surf_facs( ws_type, cat_len, cat_st, sat_len, sat_st, tcat_len,
	tcat_st, psc_len, psc_st, error_ind, facil, tot_cat_len, 
	tot_sat_len, tot_tcat_len, tot_psc_len )
    Pint		ws_type;	/* workstation type */
    Pint		cat_len;	/* length of curve approx types list */
    Pint		cat_st;		/* starting position */
    Pint		sat_len;	/* length of surface approx types list*/
    Pint		sat_st;		/* starting position */
    Pint		tcat_len;	/* length of trim curve approx types
					   list*/
    Pint		tcat_st;	/* starting position */
    Pint                psc_len;        /* length of parametric surface
					    characteristics list */
    Pint                psc_st;         /* starting position */
    Pint		*error_ind;	/* OUT: error indidator */
    Pcurvsurf_facs	*facil;		/* OUT: curve and surface facilities */
    Pint		*tot_cat_len;	/* OUT: total length of curve approx
						types list */
    Pint		*tot_sat_len;	/* OUT: total length of surface approx
						types list */
    Pint		*tot_tcat_len;	/* OUT: total length of trim curve
						approx types list */
    Pint                *tot_psc_len;  /* OUT: total length of parametric
                                                surface characteristics list */
{
    /* Error 51 and 62 are not generated by this function */

    Wst				*wst = (Wst *)ws_type;
    Wst_phigs_dt		*dt;

    if ( !CB_ENTRY_CHECK(phg_cur_cph, 0, Pfn_INQUIRY)) {
	*error_ind = ERR2;

    } else if ( !phg_cb_wst_exists( wst  ) ) {
	*error_ind = ERR52;

    } else if ( !((dt = &wst->desc_tbl.phigs_dt)->ws_category == PCAT_OUT
	|| dt->ws_category == PCAT_OUTIN || dt->ws_category == PCAT_MO) ) {
	*error_ind = ERR59;

    } else {
	*error_ind = 0;
	*tot_cat_len = dt->out_dt.num_curve_approx_types;
	*tot_sat_len = dt->out_dt.num_surface_approx_types;
	*tot_tcat_len = dt->out_dt.num_trim_curve_approx_types;
	*tot_psc_len = dt->out_dt.num_para_surf_characs;

	facil->cat_types.num_ints =
	    MIN(cat_len, dt->out_dt.num_curve_approx_types - cat_st);
	facil->sat_types.num_ints =
	    MIN(sat_len, dt->out_dt.num_surface_approx_types - sat_st);
	facil->tcat_types.num_ints =
	    MIN(tcat_len, dt->out_dt.num_trim_curve_approx_types - tcat_st);
	facil->psc_types.num_ints =
	    MIN(psc_len, dt->out_dt.num_para_surf_characs - psc_st);

	if ( facil->cat_types.num_ints > 0 )
	    COPY_LIST(dt,curve_approx_types,cat_st,facil,cat_types);
	if ( facil->sat_types.num_ints > 0 )
	    COPY_LIST(dt,surface_approx_types,sat_st,facil,sat_types);
	if ( facil->tcat_types.num_ints > 0 )
	    COPY_LIST(dt,trim_curve_approx_types,tcat_st,facil,tcat_types);
	if ( facil->psc_types.num_ints > 0 )
	    COPY_LIST(dt,para_surf_characs,psc_st,facil,psc_types);

	facil->max_bsp_order = dt->out_dt.max_nurb_order;
	facil->max_tc_order = dt->out_dt.max_trim_curve_order;
    }
}
#undef COPY_LIST

/* INQUIRE DEPTH CUE FACILITIES */
void
pinq_dcue_facs(ws_type, error_ind, facilities)
Pint		ws_type;		/* workstation type	*/
Pint		*error_ind;	/* OUT error indicator	*/
Pint		*facilities;	/* OUT depth cue facilities	*/
{
    /* Error 51 is not generated by this function. */

    Wst				*wst = (Wst *)ws_type;
    Wst_phigs_dt		*dt;

    if ( !CB_ENTRY_CHECK( phg_cur_cph, 0, Pfn_INQUIRY)) {
	*error_ind = ERR2;

    } else if ( !phg_cb_wst_exists( wst )) {
	*error_ind = ERR52;

    } else if ( !((dt = &wst->desc_tbl.phigs_dt)->ws_category == PCAT_OUT
	|| dt->ws_category == PCAT_OUTIN || dt->ws_category == PCAT_MO) ) {
	*error_ind = ERR59;

    } else {
	*error_ind = 0;
	*facilities = dt->out_dt.num_predefined_depth_cue_indices;
    }
}


/* INQUIRE PREDEFINED DEPTH CUE REPRESENTATION */
void
pinq_pred_dcue_rep(ws_type, index, error_ind, bundle)
Pint		ws_type;		/* workstation type	*/
Pint		index;		/* predefined index	*/
Pint		*error_ind;	/* OUT error indicator	*/
Pdcue_bundle	*bundle;	/* OUT predefined depth cue rep	*/
{
    /* Error 51 is not generated by this function. */

    Wst				*wst = (Wst *)ws_type;
    Wst_phigs_dt		*dt;

    if ( !CB_ENTRY_CHECK( phg_cur_cph, 0, Pfn_INQUIRY)) {
	*error_ind = ERR2;

    } else if ( !phg_cb_wst_exists( wst )) {
	*error_ind = ERR52;

    } else if ( !((dt = &wst->desc_tbl.phigs_dt)->ws_category == PCAT_OUT
	|| dt->ws_category == PCAT_OUTIN || dt->ws_category == PCAT_MO) ) {
	*error_ind = ERR59;

    } else if ( dt->ws_category == PCAT_MO ) {
	*error_ind = ERR62;

    } else if ( index >= dt->out_dt.num_predefined_depth_cue_indices) {
	*error_ind = ERR102;

    } else if ( index < 0 ) {
	*error_ind = ERR119;

    } else {
	*error_ind = 0;
	bundle->mode = dt->out_dt.default_depth_cue_table[index].mode;
	bundle->ref_planes[0] = 
		dt->out_dt.default_depth_cue_table[index].ref_planes[0];
	bundle->ref_planes[1] = 
		dt->out_dt.default_depth_cue_table[index].ref_planes[1];
	bundle->scaling[0] = 
		dt->out_dt.default_depth_cue_table[index].scaling[0];
	bundle->scaling[1] = 
		dt->out_dt.default_depth_cue_table[index].scaling[1];
	bundle->colr = dt->out_dt.default_depth_cue_table[index].colr;
    }
}


/* INQUIRE LIGHT SOURCE FACILITIES */
void
pinq_light_src_facs(ws_type, length, start, error_ind, facilities, total_length)
Pint		ws_type;		/* workstation type	*/
Pint		length;		/* length of application list	*/
Pint		start;		/* starting position	*/
Pint		*error_ind;	/* OUT error indicator	*/
Plight_src_facs	*facilities;	/* OUT light source facilities	*/
Pint		*total_length;	/* OUT length of list in PHIGS	*/
{
    /* Error 51 is not generated by this function. */

    Wst				*wst = (Wst *)ws_type;
    Wst_phigs_dt		*dt;

    if ( !CB_ENTRY_CHECK( phg_cur_cph, 0, Pfn_INQUIRY)) {
	*error_ind = ERR2;

    } else if ( !phg_cb_wst_exists( wst )) {
	*error_ind = ERR52;

    } else if ( !((dt = &wst->desc_tbl.phigs_dt)->ws_category == PCAT_OUT
	|| dt->ws_category == PCAT_OUTIN || dt->ws_category == PCAT_MO) ) {
	*error_ind = ERR59;

    } else {
	*error_ind = 0;
	*total_length = dt->out_dt.num_light_src_types;
	facilities->types.num_ints =
	    MIN(length, dt->out_dt.num_light_src_types - start);
	if (facilities->types.num_ints > 0) {
	    bcopy((char *)&dt->out_dt.light_src_types[start],
		  (char *)facilities->types.ints,
		  facilities->types.num_ints * sizeof(Pint));
	}
	facilities->max = dt->out_dt.max_light_src;
	facilities->num_pred_inds = dt->out_dt.num_predefined_light_src_indices;
    }
}


/* INQUIRE PREDEFINED LIGHT SOURCE REPRESENTATION */
void
pinq_pred_light_src_rep(ws_type, index, error_ind, bundle)
Pint		ws_type;		/* workstation type	*/
Pint		index;		/* predefined index	*/
Pint		*error_ind;	/* OUT error indicator	*/
Plight_src_bundle	*bundle;	/* OUT predefined light source rep */
{
    /* Error 51 is not generated by this function. */

    Wst				*wst = (Wst *)ws_type;
    Wst_phigs_dt		*dt;

    if ( !CB_ENTRY_CHECK( phg_cur_cph, 0, Pfn_INQUIRY)) {
	*error_ind = ERR2;

    } else if ( !phg_cb_wst_exists( wst )) {
	*error_ind = ERR52;

    } else if ( !((dt = &wst->desc_tbl.phigs_dt)->ws_category == PCAT_OUT
	|| dt->ws_category == PCAT_OUTIN || dt->ws_category == PCAT_MO) ) {
	*error_ind = ERR59;

    } else if ( dt->ws_category == PCAT_MO ) {
	*error_ind = ERR62;

    } else if ( index < 1 ) {
	*error_ind = ERR129;

    } else if ( index > dt->out_dt.num_predefined_light_src_indices) {
	*error_ind = ERR102;

    } else {
	*error_ind = 0;
	*bundle = dt->out_dt.default_light_src_table[index-1];
    }
}

/* INQUIRE RENDERING COLOUR MODEL FACILITIES */
void
pinq_rendering_colr_model_facs( ws_type, length, start, error_ind, models,
	total_length )
    Pint	ws_type;		/* workstation type */
    Pint	length;		/* length of application list */
    Pint	start;		/* starting position */
    Pint	*error_ind;	/* OUT error indicator */
    Pint_list *models;		/* OUT list of rendering colour models */
    Pint	*total_length;	/* OUT length of list in PHIGS */
{
    Wst				*wst = (Wst *)ws_type;
    Wst_phigs_dt		*dt;

    if ( !CB_ENTRY_CHECK( phg_cur_cph, 0, Pfn_INQUIRY)) {
	*error_ind = ERR2;

    } else if ( !phg_cb_wst_exists( wst )) {
	*error_ind = ERR52;

    } else if ( !((dt = &wst->desc_tbl.phigs_dt)->ws_category == PCAT_OUT
	|| dt->ws_category == PCAT_OUTIN || dt->ws_category == PCAT_MO) ) {
	*error_ind = ERR59;

    } else if ( dt->ws_category == PCAT_MO ) {
	*error_ind = ERR62;

    } else {
	*error_ind = 0;
	models->num_ints = 0;
	if ( (*total_length = dt->out_dt.num_rendering_colour_models) > 0) {
	    if (start < 0 || start >= dt->out_dt.num_rendering_colour_models)
		*error_ind = ERR2201;
	    else if (length > 0) {
		models->num_ints = MIN( length,
		    dt->out_dt.num_rendering_colour_models - start);
		bcopy( (char*)&dt->out_dt.rendering_colour_models[start],
		    (char*)models->ints, 
		    models->num_ints * sizeof(Pint));
	    } else if (length < 0)
		*error_ind = ERRN153;
	}
    }
}


void
pinq_text_extent( ws_type, font, exp, sp, ht, path, hor, ver, str,
    error_ind, rect, offset)
    Pint	ws_type;		/* workstation type	*/
    Pint	font;		/* text font	*/
    Pfloat	exp;		/* char expansion factor	*/
    Pfloat	sp;		/* char spacing	*/
    Pfloat	ht;		/* char height	*/
    Ptext_path	path;		/* text path	*/
    Phor_text_align	hor;	/* horizontal alignment	*/
    Pvert_text_align	ver;	/* vertical alignment	*/
    char	*str;		/* text string	*/
    Pint	*error_ind;	/* OUT error indicator	*/
    Prect	*rect;		/* OUT extent rectangle	*/
    Ppoint	*offset;	/* OUT concatenation offset	*/
{
    Wst					*wst = (Wst *)ws_type;
    Phg_args				cp_args;
    Phg_ret				ret;
    register Phg_args_q_text_extent	*args = &cp_args.data.q_text_extent;
    register Wst_phigs_dt		*dt;
    int					charset_index;

    if ( !CB_ENTRY_CHECK( phg_cur_cph, 0, Pfn_INQUIRY)) {
	*error_ind = ERR2;

    } else if ( wst->bound_status != WST_BOUND) {
	*error_ind = ERR51;

    } else if ( !phg_cb_wst_exists( wst ) ) {
	*error_ind = ERR52;

    } else if ( !((dt = &wst->desc_tbl.phigs_dt)->ws_category == PCAT_OUT
	|| dt->ws_category == PCAT_OUTIN || dt->ws_category == PCAT_MO) ) {
	*error_ind = ERR59;

    } else if ( dt->ws_category == PCAT_MO ) {
	*error_ind = ERR62;

    /* check for valid text pairs using charset 1 */
    } else if ( (charset_index = phg_cb_charset_index(dt,PCS_ASCII)) == -1) {
	/* this should never happen, so the assure is always wrong. */
        assure(charset_index != -1);

    } else if ( !phg_cb_valid_text_pair( font, PPREC_STROKE,
		dt->out_dt.num_text_pairs[charset_index], 
		dt->out_dt.text_pairs[charset_index])) {
	*error_ind = ERR106;

    } else if ( !str || !str[0] ) {	/* don't waste time if no string. */
	*error_ind = 0;
	rect->p.x = rect->p.y = rect->q.x = rect->q.y = 0.0;
	offset->x = offset->y = 0.0;

    } else {
	args->wsid = wst->wsid;
	/* use charset 1 for all charsets since this function does not
	 * specify them */
	args->font = font;
	args->path = path;
	args->hor = hor;
	args->ver = ver;
	args->height = ht;
	args->char_expan = exp;
	args->spacing = sp;
	args->str = str;
	args->length = strlen( str ) + 1;
	CP_FUNC( phg_cur_cph, CP_FUNC_OP_INQ_TEXT_EXTENT, &cp_args, &ret);

	if ( ret.err) {
	    *error_ind = ret.err;
	} else {
	    *error_ind = 0;
	    *rect = ret.data.text_extent.rect;
	    *offset = ret.data.text_extent.offset;
	}
    }
}


/* INQUIRE DYNAMICS OF WORKSTATION PLUS */
void pinq_dyns_ws_attrs_plus( ws_type, err_ind, attr )
    Pint		ws_type;	/* IN */
    Pint		*err_ind;	/* OUT */
    Pdyns_ws_attrs_plus	*attr;		/* OUT */
{
    Wst				*wst = (Wst *)ws_type;
    Wst_phigs_dt		*dt;

    if ( !CB_ENTRY_CHECK( phg_cur_cph, 0, Pfn_INQUIRY)) {
	*err_ind = ERR2;

    } else if ( !phg_cb_wst_exists( wst)) {
	*err_ind = ERR52;

    } else if ( !((dt = &wst->desc_tbl.phigs_dt)->ws_category == PCAT_OUT
	|| dt->ws_category == PCAT_OUTIN || dt->ws_category == PCAT_MO) ) {
	*err_ind = ERR59;

    } else {
	*err_ind = 0;
	attr->light_src_rep = dt->out_dt.light_source_rep;
	attr->dcue_rep = dt->out_dt.dcue_rep;
	attr->colr_map_rep = dt->out_dt.colour_mapping_rep;
    }
}


/* INQUIRE COLOUR MAPPING FACILITIES */
void
pinq_colr_map_facs(ws_type, length, start, error_ind, facilities, total_length)
    Pint		ws_type;		/* workstation type	*/
    Pint		length;		/* length of application list	*/
    Pint		start;		/* starting position	*/
    Pint		*error_ind;	/* OUT error indicator	*/
    Pcolr_map_facs	*facilities;	/* OUT polyline facilities	*/
    Pint		*total_length;	/* OUT length of list in PHIGS	*/
{
    Wst				*wst = (Wst *)ws_type;
    Wst_phigs_dt		*dt;

    if ( !CB_ENTRY_CHECK( phg_cur_cph, 0, Pfn_INQUIRY)) {
	*error_ind = ERR2;

    } else if ( !phg_cb_wst_exists( wst )) {
	*error_ind = ERR52;

    } else if ( !((dt = &wst->desc_tbl.phigs_dt)->ws_category == PCAT_OUT
	|| dt->ws_category == PCAT_OUTIN || dt->ws_category == PCAT_MO) ) {
	*error_ind = ERR59;

    } else if ( dt->ws_category == PCAT_MO ) {
	*error_ind = ERR62;

    } else {
	*error_ind = 0;
	*total_length = dt->out_dt.num_colr_mapping_methods;
	facilities->meths.num_ints = 
	    MIN( length, dt->out_dt.num_colr_mapping_methods - start);
	if ( facilities->meths.num_ints > 0) {
	    bcopy( (char*)&dt->out_dt.colr_mapping_methods[start],
	        (char*)facilities->meths.ints, 
		facilities->meths.num_ints * sizeof(Pint));
	}
	facilities->num_pred_inds =
	    dt->out_dt.num_predefined_colr_mapping_indices;
    }
}

/* INQUIRE PREDEFINED COLOUR MAPPING REPRESENTATION */
void
pinq_pred_colr_map_rep( ws_type, index, store, error_ind, map_method, map_data)
    Pint		ws_type;	/* IN */
    Pint	 	index;		/* polyline index	*/
    Pstore		store;		/* store object */
    Pint		*error_ind;	/* OUT error indicator	*/
    Pint		*map_method;	/* OUT: mapping method */
    Pcolr_map_data	**map_data;	/* OUT: data record pointer */
{
    Wst			*wst = (Wst *)ws_type;
    Wst_phigs_dt	*dt;
    int			size = 0;
    Phg_colr_map_rep	*rep;

    if ( !CB_ENTRY_CHECK( phg_cur_cph, 0, Pfn_INQUIRY)) {
	*error_ind = ERR2;

    } else if ( !phg_cb_wst_exists( wst )) {
	*error_ind = ERR52;

    } else if ( !((dt = &wst->desc_tbl.phigs_dt)->ws_category == PCAT_OUT
	|| dt->ws_category == PCAT_OUTIN || dt->ws_category == PCAT_MO) ) {
	*error_ind = ERR59;

    } else if ( dt->ws_category == PCAT_MO ) {
	*error_ind = ERR62;

    } else if ( index >= dt->out_dt.num_predefined_colr_mapping_indices) {
	*error_ind = ERR102;

    } else if ( index < 0 ) {
	*error_ind = ERR121;

    } else {
	*error_ind = 0;
	rep = &dt->out_dt.default_colr_mapping_table[index];
	switch ( *map_method = rep->method ) {
	    case PCOLR_MAP_TRUE:
		/* no data record contents */
		break;

	    case PCOLR_MAP_PSEUDO:
		size = rep->rec.meth_r2.weights.num_floats * sizeof(Pfloat)
		    + rep->rec.meth_r2.colrs.num_colr_reps * sizeof(Pcolr_rep);
		if ( CB_STORE_SPACE( ((_Pstore *)store), size, error_ind ) ) {
		    *map_data = &((_Pstore *)store)->data.colr_map_rec;
		    (*map_data)->meth_r2 = rep->rec.meth_r2;
		    (*map_data)->meth_r2.weights.floats =
			(Pfloat *)((_Pstore *)store)->buf;
		    (*map_data)->meth_r2.colrs.colr_reps = (Pcolr_rep *)
			((*map_data)->meth_r2.weights.floats +
			    (*map_data)->meth_r2.weights.num_floats);
		    bcopy(
		      (char *)rep->rec.meth_r2.weights.floats,
					(char *)(*map_data)->meth_r2.weights.floats,
					(*map_data)->meth_r2.weights.num_floats *
					sizeof(Pfloat) );
		    bcopy(
		      (char *)rep->rec.meth_r2.colrs.colr_reps,
					(char *)(*map_data)->meth_r2.colrs.colr_reps,
					(*map_data)->meth_r2.colrs.num_colr_reps *
			    sizeof(Pcolr_rep) );
		}
		break;

		case PCOLR_MAP_PSEUDO_N: {
		    Pfloat_list_list	*colrs;
		    Pfloat		*buf;
		    register int	i;

		    colrs = &rep->rec.meth_r3.colr_lists;
		    size = colrs->num_lists * sizeof(Pfloat_list);
		    for ( i = 0; i < colrs->num_lists; i++ )
			size += colrs->lists[i].num_floats * sizeof(Pfloat);
		    if ( CB_STORE_SPACE( ((_Pstore *)store), size, error_ind ) ) {
			*map_data = &((_Pstore *)store)->data.colr_map_rec;
			(*map_data)->meth_r3 =
			    rep->rec.meth_r3;
			(*map_data)->meth_r3.colr_lists.lists =
			    (Pfloat_list *)((_Pstore *)store)->buf;
			buf = (Pfloat *)
			    ((*map_data)->meth_r3.colr_lists.lists
				+ colrs->num_lists);
			for ( i = 0; i < colrs->num_lists; i++ ) {
			  (*map_data)->meth_r3.colr_lists.lists[i].num_floats
				= colrs->lists[i].num_floats;
			    (*map_data)->meth_r3.colr_lists.lists[i].floats
				= buf;
			    bcopy( (char *)colrs->lists[i].floats, (char *)buf,
						colrs->lists[i].num_floats * sizeof(Pfloat) );
			    buf += colrs->lists[i].num_floats;
			}
		    }
		} break;
	}
    }
}

/* INQUIRE COLOUR MAPPING METHOD FACILITIES */
void
pinq_colr_map_method_facs(ws_type, map_method, error_ind, map_st)
    Pint		ws_type;	/* workstation type	*/
    Pint		map_method;	/* mapping method	*/
    Pint		*error_ind;	/* OUT error indicator	*/
    Pcolr_map_st	*map_st;	/* OUT mapping state	*/
{
    Wst		*wst = (Wst *)ws_type;
    Wst_phigs_dt		*dt;

    if ( !CB_ENTRY_CHECK( phg_cur_cph, 0, Pfn_INQUIRY)) {
	*error_ind = ERR2;

    } else if ( wst->bound_status != WST_BOUND ) {
	*error_ind = ERR51;

    } else if ( !phg_cb_wst_exists( wst)) {
	*error_ind = ERR52;
    
    } else if ( !((dt = &wst->desc_tbl.phigs_dt)->ws_category == PCAT_OUT
	|| dt->ws_category == PCAT_OUTIN || dt->ws_category == PCAT_MO) ) {
	*error_ind = ERR59;

    } else if ( dt->ws_category == PCAT_MO ) {
	*error_ind = ERR62;

    } else if ( !phg_cb_int_in_list( map_method,
        dt->out_dt.num_colr_mapping_methods,
        dt->out_dt.colr_mapping_methods ) ) {
        *error_ind = ERR126;

    } else {
	switch ( map_method ) {
            case PCOLR_MAP_TRUE:
	            map_st->int_data = 
		       wst->desc_tbl.phigs_dt.out_dt.num_true_colours;
	    break;
            case PCOLR_MAP_PSEUDO:
	            map_st->int_data = 
		       wst->desc_tbl.phigs_dt.out_dt.num_colours -
		       wst->desc_tbl.phigs_dt.out_dt.num_true_colours;
            break;
            case PCOLR_MAP_PSEUDO_N:
	        map_st->int_data = 0;
            break;
        }
    }
}
