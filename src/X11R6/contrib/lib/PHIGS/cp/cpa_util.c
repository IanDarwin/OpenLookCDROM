/* $XConsortium: cpa_util.c,v 5.3 94/04/17 20:41:16 hersh Exp $ */

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

#include "phg.h"
#include "cp.h"
#include "cp_priv.h"
#include "PEX.h"
#include "PEXmacs.h"
#include "PEXfuncs.h"
#include "PEXproto.h"
#include "phigspex.h"


int
phg_cpa_build_struct_id_list( cph, css_srvr, sids, xids )
    Cp_handle		cph;
    Cpx_css_srvr	*css_srvr;
    int			**sids;	/* NULL if PHIGS struct ids not needed */
    XID			**xids; /* NULL if PEX struct ids not needed */
{
    int		count;

    register	Cpa_struct_data		*sp;
    register	int			*sid;
    register	XID			*xid;

    /* Count 'em first. */
    count = 0;
    CPA_FOR_ALL_STRUCTS( css_srvr, sp )
	++count;
    CPA_END_FOR_ALL_STRUCTS

    if ( count > 0 ) {
	if ( sids ) {
	    if ( (*sids = sid = (int *)Malloc( count * sizeof(int) )) ) {
		CPA_FOR_ALL_STRUCTS( css_srvr, sp )
		    *sid++ = sp->sid;
		CPA_END_FOR_ALL_STRUCTS
	    } else {
		ERR_BUF( cph->erh, ERR900 );
		goto end;
	    }
	}

	if ( xids ) {
	    if ( (*xids = xid = (XID *)Malloc( count * sizeof(XID) )) ) {
		CPA_FOR_ALL_STRUCTS( css_srvr, sp )
		    *xid++ = sp->xid;
		CPA_END_FOR_ALL_STRUCTS
	    } else {
		ERR_BUF( cph->erh, ERR900 );
		if ( sids ) {
		    free( (char *)*sids );
		    *sids = (int *)NULL;
		}
		goto end;
	    }
	}
    }

end:
    return count;
}


void
phg_cpa_free_structure_lists( cph, css_srvr )
    Cp_handle		cph;
    Cpx_css_srvr	*css_srvr;
{
    register	int			i;
    register	Cpa_struct_data		*sp, *next, **lists;

    /* Free the list elements. */
    lists = css_srvr->model.a.struct_lists;
    for ( i = 0; i < CPA_STRUCT_HASH_SIZE; i++ ) {
	for ( sp = lists[i]; sp; sp = next) {
	    next = sp->next;
	    free( (char *)sp );
	}
	lists[i] = (Cpa_struct_data *)NULL;
    }
}


void
phg_cpa_destroy_attached_structures( cph, css_srvr )

    Cp_handle		cph;
    Cpx_css_srvr	*css_srvr;
{
    XID			*xids;
    int			count;

    count = phg_cpa_build_struct_id_list( cph, css_srvr, (int**)NULL, &xids );
    if ( count > 0 && xids ) {
	(void)PEXDestroyStructures( css_srvr->display, xids, count );
	XFlush( css_srvr->display );
	free( (char *)xids );
    }
}


void
phg_cpa_link_struct( cph, css_srvr, sp )
    Cp_handle		cph;
    Cpx_css_srvr	*css_srvr;
    Cpa_struct_data	*sp;
{
    register Cpa_struct_data		**spp;

    spp = &css_srvr->model.a.struct_lists[_ABS(sp->sid) % CPA_STRUCT_HASH_SIZE];
    for ( ; *spp; spp = &(*spp)->next )
	;
    *spp = sp;
}


Cpa_struct_data*
phg_cpa_struct_exists( cph, css_srvr, id_type, id, op )
    Cp_handle		cph;
    Cpx_css_srvr	*css_srvr;
    int			id_type; /* CPX_BY_SID is most efficient */
    int			id;
    int			op;	/* check, add or remove */
{
    /* Returns pointer to structure info if struct exists or is created. */

    register Cpa_struct_data		**sp, **lists;

    /* Find it if it's there. */
    lists = css_srvr->model.a.struct_lists;
    switch ( id_type ) {
	case CPX_BY_SID:
	    sp = &lists[_ABS(id) % CPA_STRUCT_HASH_SIZE];
	    for ( ; *sp && (*sp)->sid != id; sp = &(*sp)->next )
		;
	    break;
	case CPX_BY_XID: {
	    register int i;
	    for ( i = 0; i >= 0 && i < CPA_STRUCT_HASH_SIZE; i++ )
		for ( sp = &lists[i]; *sp; sp = &(*sp)->next )
		    if ( (*sp)->xid == (XID)id ) {
			i = -2; /* to break out of outer for loop */
			break;
		    }
	    } break;
    }

    if ( *sp ) {	/* structure exists */
	if ( op == CPA_STRUCT_OP_REMOVE || op == CPA_STRUCT_OP_DESTROY ) {
	    Cpa_struct_data	*tmp;

	    /* Remove it from the list.  Don't free it unless requested
	     * because the caller may need to use it.  He's responsible
	     * for freeing it in that case.
	     */
	    tmp = *sp;
	    *sp = tmp->next;
	    if ( op == CPA_STRUCT_OP_DESTROY )
		free( (char *)tmp );
	    return tmp;
	}

    } else if ( op == CPA_STRUCT_OP_CREATE ) {
	if ( !(*sp = (Cpa_struct_data*) Malloc(sizeof(Cpa_struct_data))) ) {
	    ERR_BUF( cph->erh, ERR900 );
	} else {
	    if ( id_type == CPX_BY_SID ) {
		(*sp)->sid = (Pint)id;
		(*sp)->xid = 0;
	    } else if ( id_type == CPX_BY_XID ) {
		(*sp)->sid = 0;
		(*sp)->xid = (XID)id;
	    }
	    (*sp)->next = (Cpa_struct_data *)NULL;
	}
    }

    return *sp;
}

int
phg_cpa_check_struct_exists(cph, css_srvr, sid)
Cp_handle	 cph;
Cpx_css_srvr	*css_srvr;
Pint		 sid;
{
    if (phg_cpa_struct_exists(cph, css_srvr, CPX_BY_SID, sid,
					 CPA_STRUCT_OP_CHECK))
	return(TRUE);
    else
	return(FALSE);
}


Cpa_struct_data*
phg_cpa_open_struct_in_list( cph, css_srvr, id_type, count, ids )
    Cp_handle		cph;
    Cpx_css_srvr	*css_srvr;
    int			id_type; /* CPX_BY_SID is most efficient */
    register int	count;
    register int	*ids;
{
    /* If the open structure exists the supplied list is searched for the
     * open structure id.  A pointer to the internal struct data for it is
     * returned if it is.
     */

    register int		i;
    register Cpa_struct_data	*open_struct = (Cpa_struct_data *)NULL;
    register Cpa_struct_data	*rtn_struct = (Cpa_struct_data *)NULL;

    if ( open_struct = phg_cpa_struct_exists( cph, css_srvr, CPX_BY_SID,
	    cph->psl->open_struct, CPA_STRUCT_OP_CHECK ) ) {
	switch ( id_type ) {
	    case CPX_BY_SID:
		for ( i = 0; i < count; i++, ids++ )
		    if ( *ids == open_struct->sid ) {
                        rtn_struct = open_struct;
			break;
		    }
		break;
	    case CPX_BY_XID:
		for ( i = 0; i < count; i++, ids += sizeof(XID)/sizeof(*ids) )
		    if ( *((XID*)ids) == open_struct->xid ) {
                        rtn_struct = open_struct;
			break;
		    }
		break;
	}
    }

    return rtn_struct;
}


int
phg_cpa_struct_list_empty( css_srvr )
    Cpx_css_srvr	*css_srvr;
{
    register	int			i;
    register	Cpa_struct_data		**lists;

    lists = css_srvr->model.a.struct_lists;
    for ( i = 0; i < CPA_STRUCT_HASH_SIZE; i++, lists++ )
	if ( *lists )
	    return 0;
    return 1;
}
