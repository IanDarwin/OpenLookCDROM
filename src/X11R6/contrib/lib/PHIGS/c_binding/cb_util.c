/* $XConsortium: cb_util.c,v 5.3 94/04/17 20:40:57 hersh Exp $ */

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

/* Utility functions for the PHIGS C binding */

#include "phg.h"
#include "cp.h"
#include "cb_priv.h"


int
phg_cb_entry_check( cph, err, func)
    Cp_handle	cph;
    int		err;	/* error number to report if phigs not open */
    int		func;	/* function number to report, 0 if inquiry */
{
    int		status;

    if (CB_PHIGS_OPEN(cph)) {
	ERR_FLUSH( cph->erh);
	ERR_SET_CUR_FUNC( cph->erh, func);
	status = 1;
    } else {
	status = 0;
	/* report error if not an inquiry function */
	if ( func != Pfn_INQUIRY && err)
	    ERR_HANDLE( err, func, NULL); /* no error file if not open */
    }
    return status;
}


int
phg_cb_int_in_list( v, num, list)
    register Pint	v;
    register int	num;
    register Pint	*list;
{
    while ( num--) {
	if ( *list++ == v)
	    return 1;
    }
    return 0;
}


Wst_phigs_dt*
phg_cb_check_set_rep( cph, fnid, ws, index, colour)
    Cp_handle	cph;
    Pint	fnid;	/* function id */
    Pint	ws;	/* workstation identifier	*/
    Pint	index;	/* some bundle index	*/
    Pint	colour;	/* colour index */
{
    Psl_ws_info		*wsinfo;
    Wst_phigs_dt	*dt = NULL;

    if ( CB_ENTRY_CHECK( cph, ERR3, fnid)) {
        if ( PSL_WS_STATE( cph->psl) != PWS_ST_WSOP) {
	    ERR_REPORT( cph->erh, ERR3);

	} else if ( index < 1) {
	    ERR_REPORT( cph->erh, ERR100);

	} else if ( colour < 0) {
	    ERR_REPORT( cph->erh, ERR113);

	} else if ( !(wsinfo = phg_psl_get_ws_info( cph->psl, ws))) {
	    ERR_REPORT( cph->erh, ERR54);	/* ws not open */

	} else {
	    dt = &((Wst*)wsinfo->wstype)->desc_tbl.phigs_dt;
	    if ( !( dt->ws_category == PCAT_OUTIN || dt->ws_category == PCAT_OUT
		|| dt->ws_category == PCAT_MO)) {
		ERR_REPORT( cph->erh, ERR59);
		dt = NULL;
	    }
	}
    }
    return dt;
}

void
phg_cb_copy_hierarchy( ret_hier, store, error_ind, paths )
    Phg_ret_hierarchy       	*ret_hier;	/* returned hierarchy */
    _Pstore		       	*store;		/* OUT store handle */
    Pint			*error_ind;
    Pelem_ref_list_list		*paths;		/* OUT structure path list */
{
		int		size;
    register	Pelem_ref	*bufp, *retp;
    register	int		i;

    *error_ind = 0;
    size = ret_hier->counts.num_ints * sizeof(Pelem_ref_list)
	+ ret_hier->num_pairs * sizeof(Pelem_ref);
    paths->num_elem_ref_lists = ret_hier->counts.num_ints;
    if ( CB_STORE_SPACE( store, size, error_ind ) ) {
	paths->elem_ref_lists = (Pelem_ref_list *)store->buf;
	bufp = (Pelem_ref *)(paths->elem_ref_lists + paths->num_elem_ref_lists);
	retp = ret_hier->paths;
	for ( i = 0; i < paths->num_elem_ref_lists; i++ ) {
	    paths->elem_ref_lists[i].num_elem_refs = ret_hier->counts.ints[i];
	    paths->elem_ref_lists[i].elem_refs = bufp;
	    while (ret_hier->counts.ints[i]--)
		*bufp++ = *retp++;
	}
    }
}

void
phg_cb_update_DC_size( wsinfo )
    Psl_ws_info		*wsinfo;
{
    		Phg_args	cp_args;
    		Phg_ret		ret;
		Wst_xwin_dt	*xdt;
    register	Wst_phigs_dt	*dt;

    xdt = &((Wst*)wsinfo->wstype)->desc_tbl.xwin_dt;
    if ( xdt->dc_model == PHIGS_DC_LIMITS_ADJUST_TO_WINDOW ) {
	/* Update the DC limits. */
	cp_args.data.idata = wsinfo->wsid;
	CP_FUNC( phg_cur_cph, CP_FUNC_OP_INQ_WIN_INFO, &cp_args, &ret );
	dt = &((Wst*)wsinfo->wstype)->desc_tbl.phigs_dt;
	dt->dev_coords[0] = ret.data.win_info.display_size.size_dc.size_x;
	dt->dev_coords[1] = ret.data.win_info.display_size.size_dc.size_y;
	dt->dev_coords[2] = ret.data.win_info.display_size.size_dc.size_z;
	dt->dev_addrs_units[0] =
	    ret.data.win_info.display_size.size_raster.size_x;
	dt->dev_addrs_units[1] =
	    ret.data.win_info.display_size.size_raster.size_y;
	dt->dev_addrs_units[2] =
	    ret.data.win_info.display_size.size_raster.size_z;
    }
}

/* Holds a singly linked list of all stores created. */
static _Pstore	*store_list = (_Pstore *)NULL;

/* Currently the PHIGS DP C binding does not define when the store functions
 * may be called, so these functions don't check the system state.  Stores
 * can currently be created regardless of the PHIGS state, and there's no
 * attempt to delete them when PHIGS is closed; although the function to do
 * that is defined below (phg_destroy_all_stores).
 */

void
pcreate_store( err, store )
    Pint		*err;
    Pstore		*store;
{
    if ( !(*store = (Pstore)calloc( 1, sizeof(_Pstore) )) ) {
	*err = ERR900;
    } else {
	*err = 0;
	((_Pstore *)(*store))->next = store_list;
	store_list = ((_Pstore *)(*store));
    }
}

void
pdel_store( store )
    Pstore		store;
{
    _Pstore	**node;

    /* Find the store in the list then free it.  Do nothing if store not
     *  found.
     */
    for ( node = &store_list; *node; node = &(*node)->next ) {
	if ( *node == (_Pstore *)store ) {
	    (*node) = (*node)->next;	/* remove from list. */
	    if ( ((_Pstore *)store)->size > 0 )
		free( ((_Pstore *)store)->buf );
	    free( (char *)store );
	    break;
	}
    }
}

void
phg_destroy_all_stores()
{
    _Pstore	*node, *next;

    for ( node = store_list; node; node = next ) {
	next = node->next;
	if ( node->size > 0 )
	    free( node->buf );
	free( (char *)node );
    }
}

int
phg_resize_store( store, size, err )
    _Pstore	*store;
    int		size;
    Pint	*err;
{
    _Pstore	old_store;

    *err = 0;
    if ( store ) {
	old_store.buf = store->buf;	/* remember the old buffer */
	if ( size > 0 && !(store->buf = Malloc( size )) ) {
	    *err = ERR900;
	    store->buf = old_store.buf;	/* recover the old buffer */
	} else {
	    if ( store->size > 0 ) 
		free( old_store.buf );	/* free up the old space */
	    store->size = size;
	}
    }
    return ( *err ? 0 : 1 );	/* failure or success */
}

int
phg_colours_valid( count, colour_type, colours )
    register	int	count;
		int	colour_type;
    register	Pcoval	*colours;
{
    register	int	i;

    switch ( colour_type ) {
	case PINDIRECT:
	    for ( i = 0; i < count; i++, colours++ )
		if ( colours->ind < 0 )
		    return 0;
	    break;
	case PMODEL_RGB:
	    for ( i = 0; i < count; i++, colours++ )
		if ( colours->direct.rgb.red < 0.0
		  || colours->direct.rgb.red > 1.0
		  || colours->direct.rgb.green < 0.0
		  || colours->direct.rgb.green > 1.0
		  || colours->direct.rgb.blue < 0.0
		  || colours->direct.rgb.blue > 1.0 )
		    return 0;
	    break;
    }
    return 1;
}

int
phg_cb_echo_limits_valid( cph, funcid, wsid, ev, dt )
    Cp_handle			cph;
    Pint			funcid;
    Pint			wsid;
    register Plimit3		*ev;
    register Wst_phigs_dt	*dt;
{
    int		status = 0;
    Psl_ws_info	*wsinfo;

    wsinfo = phg_psl_get_ws_info( cph->psl, wsid );
    phg_cb_update_DC_size( wsinfo );
    switch ( funcid ) {
	case Pfn_init_loc3:
	case Pfn_init_pick3:
	case Pfn_init_stroke3:
	case Pfn_init_val3:
	case Pfn_init_choice3:
	case Pfn_init_string3:
	case Pfn_escape:
	    if ( !CB_ECHO_VOLUME_VALID( ev) ) {
		ERR_REPORT( cph->erh, ERR254);
	    } else if ( !CB_ECHO_VOLUME_IN_RANGE( ev, dt->dev_coords) ) {
		ERR_REPORT( cph->erh, ERR255);
	    } else {
		status = !0;
	    }
	    break;

	case Pfn_init_loc:
	case Pfn_init_pick:
	case Pfn_init_stroke:
	case Pfn_init_val:
	case Pfn_init_choice:
	case Pfn_init_string:
	    if ( !CB_ECHO_AREA_VALID( ev) ) {
		ERR_REPORT( cph->erh, ERR254);
	    } else if ( !CB_ECHO_AREA_IN_RANGE( ev, dt->dev_coords) ) {
		ERR_REPORT( cph->erh, ERR255);
	    } else {
		status = !0;
	    }
	    break;
    }

    return status;
}
