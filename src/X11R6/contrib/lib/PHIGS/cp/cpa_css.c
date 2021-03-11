/* $XConsortium: cpa_css.c,v 5.8 94/04/17 20:41:15 mor Exp $ */

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
#include "ws.h"
#include "PEX.h"
#include "PEXmacs.h"
#include "PEXfuncs.h"
#include "PEXproto.h"
#include "phigspex.h"


/* CP/CSS functions for servers that support server side structure
 * storage and PHIGS workstations.
 */

void
phg_cpa_add_el( cph, cp_args, css_srvr )
    Cp_handle		cph;
    Phg_args		*cp_args;
    Cpx_css_srvr	*css_srvr;
{
    Cpa_struct_data	*stp;
    int			save_id;

    if ( stp = phg_cpa_struct_exists( cph, css_srvr, CPX_BY_SID,
	    cph->psl->open_struct, CPA_STRUCT_OP_CHECK ) ) {

	/* If EXECUTE STRUCTURE, map the structure id or create it. */
	if ( cp_args->data.add_el.el_type == PELEM_EXEC_STRUCT ) {
	    Cpa_struct_data		*ste;
	    pexExecuteStructure		*ed;

	    ed = (pexExecuteStructure*)cp_args->data.add_el.pex_oc.oc;
	    if ( !(ste = phg_cpa_struct_exists( cph, css_srvr, CPX_BY_SID,
		    (int)ed->id, CPA_STRUCT_OP_CHECK )) ) {
		if ( ste = phg_cpa_create_struct( cph, css_srvr, (Pint)ed->id,
			&cph->psl->edit_mode, 0 ) ) {
		    phg_cpa_link_struct( cph, css_srvr, ste );
		}
	    }
	    if ( ste ){
                save_id = (int)ed->id;
		ed->id = ste->xid;
	    }
	    else
		return;
	}

	if ( PEXStoreElements( css_srvr->display, stp->xid, (CARD32)1,
		(CARD32)cp_args->data.add_el.pex_oc.size,
		(char *)cp_args->data.add_el.pex_oc.oc ) ) {
	    CPA_FLUSH( css_srvr );
	}

	/* If EXECUTE STRUCTURE restore the structure id */
	if (cp_args->data.add_el.el_type == PELEM_EXEC_STRUCT ) {
	    pexExecuteStructure		*ed;

	    ed = (pexExecuteStructure*)cp_args->data.add_el.pex_oc.oc;
	    ed->id = (pexStructure)save_id;
	}
    }
}


void
phg_cpa_inq_el_type_size( cph, cp_args, ret, css_srvr )
    Cp_handle		cph;
    Phg_args		*cp_args;
    Phg_ret		*ret;
    Cpx_css_srvr	*css_srvr;
{
    pexElementInfo		*tnslst;
    CARD16			mode;
    Pint			sid;
    CARD32			eptr, numels, length;
    CARD16			whence;
    INT32			offset;
    Cpa_struct_data		*stp;
    char			*el_data;
    CARD32			numEls;
    Phg_ret_el_type_size	*ed;

    ret->err = 0;
    if ( cp_args->data.q_el_data.el_id == -1 ) {
	sid = cph->psl->open_struct;
	whence = PEXCurrent;
	offset = 0;
    } else {
	sid = cp_args->data.q_el_data.struct_id;
	whence = PEXBeginning;
	offset = cp_args->data.q_el_data.el_id;
    }

    if ( !(stp = phg_cpa_struct_exists( cph, css_srvr, CPX_BY_SID,
	    sid, CPA_STRUCT_OP_CHECK )) ) {
	ret->err = ERR201;
	return;
    }

    if ( cp_args->data.q_el_data.el_id >= 0 ) {
	if ( !PEXGetStructureInfo( css_srvr->display, stp->xid,
		&mode, &eptr, &numels, &length, (CARD16*)NULL )
		|| cp_args->data.q_el_data.el_id > numels ) {
	    ret->err = ERR202;
	    return;
	}
    }

    if( !PEXGetElementInfo( css_srvr->display, stp->xid,
	whence, offset, whence, offset, &tnslst, &numels ) ) {
	    ret->err = ERR900;
	    return;
    }

    ed = &ret->data.el_type_size;
    ed->size = 0;
    ed->type = phg_utx_pex_eltype_to_phigs( tnslst->elementType );
    el_data = NULL;
    /* Not all element types have a "size."  Only the ones with variable
     * sized data do.
     */
    /* Some types need the actual data to determine the size. */
    switch ( ed->type ) {
	case PELEM_FILL_AREA_SET3:
	case PELEM_FILL_AREA_SET:
	case PELEM_POLYLINE_SET3_DATA:
	case PELEM_FILL_AREA_SET3_DATA:
	case PELEM_TRI_STRIP3_DATA:
	case PELEM_QUAD_MESH3_DATA:
	case PELEM_SET_OF_FILL_AREA_SET3_DATA:
	case PELEM_CELL_ARRAY3_PLUS:
	case PELEM_NUNI_BSP_CURVE:
	case PELEM_NUNI_BSP_SURF:
	case PELEM_TEXT3:
	case PELEM_TEXT:
	case PELEM_ANNO_TEXT_REL3:
	case PELEM_ANNO_TEXT_REL:
	case PELEM_GDP3:
	case PELEM_GDP:
	case PELEM_PARA_SURF_CHARACS: 
	case PELEM_LIGHT_SRC_STATE:
	    if ( !PEXFetchElements( css_srvr->display, stp->xid,
		whence, offset, whence, offset, &el_data, &numEls )) {
		ret->err = ERR900;
		return;
	    }
	    break;
    }
    ed->size = phg_utx_compute_el_size( tnslst, (pexElementInfo *)el_data );
}


void
phg_cpa_inq_el_content( cph, cp_args, ret, css_srvr )
    Cp_handle		cph;
    Phg_args		*cp_args;
    Phg_ret		*ret;
    Cpx_css_srvr	*css_srvr;
{
    Pint		sid;
    char		*el_data;
    CARD16		whence;
    INT32		offset;
    CARD16		mode;
    CARD32		eptr, numels, length;

    register	Cpa_struct_data	*stp;

    ret->err = 0;
    if ( cp_args->data.q_el_data.el_id == -1 ) {
	sid = cph->psl->open_struct;
	whence = PEXCurrent;
	offset = 0;
    } else {
	sid = cp_args->data.q_el_data.struct_id;
	whence = PEXBeginning;
	offset = cp_args->data.q_el_data.el_id;
    }

    if ( !(stp = phg_cpa_struct_exists( cph, css_srvr, CPX_BY_SID,
	    sid, CPA_STRUCT_OP_CHECK )) ) {
	ret->err = ERR201;
	return;
    }

    if ( cp_args->data.q_el_data.el_id >= 0 ) {
	if ( !PEXGetStructureInfo( css_srvr->display, stp->xid,
		&mode, &eptr, &numels, &length, (CARD16*)NULL )
		|| cp_args->data.q_el_data.el_id > numels ) {
	    ret->err = ERR202;
	    return;
	}
    }

    if ( PEXFetchElements( css_srvr->display, stp->xid,
	    whence, offset, whence, offset, &el_data, &numels ) ) {
	ret->data.el_info.pex_oc.oc = (pexElementInfo *)el_data;
	ret->data.el_info.pex_oc.size =
	    ret->data.el_info.pex_oc.oc->length * sizeof(CARD32);
	ret->data.el_info.op = phg_utx_pex_eltype_to_phigs(
	    ret->data.el_info.pex_oc.oc->elementType );
	if ( ret->data.el_info.op == PELEM_EXEC_STRUCT ) {
	    /* Map the X resource id to the user's structure id. */
	    Cpa_struct_data	*ste;
	    if ( ste = phg_cpa_struct_exists( cph, css_srvr, CPX_BY_XID,
		(int)((pexExecuteStructure *)ret->data.el_info.pex_oc.oc)->id,
		    CPA_STRUCT_OP_CHECK ) )
		((pexExecuteStructure *)ret->data.el_info.pex_oc.oc)->id
		    = ste->sid;
	}
    } else
	ret->err = ERR900;
}


void
phg_cpa_open_struct( cph, cp_args, ret, css_srvr )
    register	Cp_handle		cph;
    		Phg_args		*cp_args;
    		Phg_ret			*ret;
    register	Cpx_css_srvr	*css_srvr;
{
    CARD16	mode;

    register	Cpa_struct_data		*stp;

    ret->err = 1;
    /* See if it exists, create it if it doesn't. */
    if ( !(stp = phg_cpa_struct_exists( cph, css_srvr, CPX_BY_SID,
	    cp_args->data.idata, CPA_STRUCT_OP_CHECK )) ) {
	if ( !CPA_ALLOC_STRUCT( stp ) ) {
	    ERR_BUF( cph->erh, ERR900 );
	} else {
	    stp->sid = cp_args->data.idata;
	    phg_cpa_link_struct( cph, css_srvr, stp );
	    stp->xid = CPA_ENCODE_STRUCT_ID(css_srvr, stp->sid);
	    (void)PEXCreateStructure( css_srvr->display, stp->xid );
	    /* See if the create worked. */
	    if ( !PEXGetStructureInfo( css_srvr->display, stp->xid,
		    &mode, (CARD32*)NULL, (CARD32*)NULL, (CARD32*)NULL,
		    (CARD16*)NULL ) ) {
		/* Structure wasn't created.  Clean up. */
		(void)phg_cpa_struct_exists( cph, css_srvr, CPX_BY_SID,
		    stp->sid, CPA_STRUCT_OP_DESTROY );
		ERR_BUF( cph->erh, ERRN202 );
		stp = (Cpa_struct_data *)NULL;
	    }
	}
    }

    if ( stp ) {
	(void)PEXSetElementPointer( css_srvr->display, stp->xid,
	    PEXEnd, (INT32)0 );
	(void)PEXSetEditingMode( css_srvr->display, stp->xid,
	    (CARD16)PEX_CONV_FROM_Peditmode(cph->psl->edit_mode) );
	ret->err = 0;
    }
}


void
phg_cpa_close_struct( cph, cp_args, css_srvr )
    Cp_handle		cph;
    Phg_args		*cp_args;
    Cpx_css_srvr	*css_srvr;
{
    CPA_FLUSH( css_srvr );
}


Cpa_struct_data*
phg_cpa_create_struct( cph, css_srvr, sid, edit_mode, element_ptr )
    Cp_handle		cph;
    Cpx_css_srvr	*css_srvr;
    Pint		sid;
    Pedit_mode		*edit_mode;	/* set it if non-NULL */
    Pint		element_ptr;	/* set it if >= 0 */
{
    Cpa_struct_data	*stp;
    CARD16		mode;

    if ( !CPA_ALLOC_STRUCT( stp ) ) {
	ERR_BUF( cph->erh, ERR900 );
    } else {
	stp->sid = sid;
	stp->xid = CPA_ENCODE_STRUCT_ID(css_srvr, stp->sid);
	(void)PEXCreateStructure( css_srvr->display, stp->xid );
	/* See if the create worked. */
	if ( !PEXGetStructureInfo( css_srvr->display, stp->xid,
		&mode, (CARD32*)NULL, (CARD32*)NULL, (CARD32*)NULL,
		(CARD16*)NULL ) ) {
	    /* Structure wasn't created.  Clean up. */
	    ERR_BUF( cph->erh, ERRN202 );
	    free( (char *)stp );
	} else {
	    if ( element_ptr >= 0 )
		(void)PEXSetElementPointer( css_srvr->display, stp->xid,
		    PEXBeginning, (INT32)element_ptr );
	    if ( edit_mode )
		(void)PEXSetEditingMode( css_srvr->display, stp->xid,
		    (CARD16)PEX_CONV_FROM_Peditmode(*edit_mode) );
	}
    }

    return stp;
}


void
phg_cpa_delete_all_structs( cph, cp_args, css_srvr )
    Cp_handle		cph;
    Phg_args		*cp_args;
    Cpx_css_srvr	*css_srvr;
{
    XID			*xids = (XID *)NULL;
    int			count;
    Cpa_struct_data	*stp;

    count = phg_cpa_build_struct_id_list( cph, css_srvr, (int**)NULL, &xids );
    if ( count > 0 && xids ) {
	(void)PEXDestroyStructures( css_srvr->display, xids, count );
	phg_cpa_free_structure_lists( cph, css_srvr );
	free( (char *)xids );

	if ( PSL_STRUCT_STATE(cph->psl) == PSTRUCT_ST_STOP ) {
	    if ( stp = CPA_CREATE_OPEN_STRUCT( cph, css_srvr ) )
		phg_cpa_link_struct( cph, css_srvr, stp );
	}
	CPA_FLUSH( css_srvr );
    }
}


void
phg_cpa_delete_struct( cph, cp_args, css_srvr )
    register	Cp_handle		cph;
    		Phg_args		*cp_args;
    register	Cpx_css_srvr	*css_srvr;
{
    Cpa_struct_data		*stp, *nstp = (Cpa_struct_data *)NULL;

    if ( stp = phg_cpa_struct_exists( cph, css_srvr, CPX_BY_SID,
	    cp_args->data.idata, CPA_STRUCT_OP_REMOVE ) ) {

	(void)PEXDestroyStructures( css_srvr->display, &stp->xid, 1 );
	free( (char *)stp );

	/* Have to re-open it if it's the open structure. */
	if ( PSL_STRUCT_STATE(cph->psl) == PSTRUCT_ST_STOP
		&& cp_args->data.idata == cph->psl->open_struct ) {
	    if ( nstp = CPA_CREATE_OPEN_STRUCT( cph, css_srvr ) )
		phg_cpa_link_struct( cph, css_srvr, nstp );
	}
	CPA_FLUSH( css_srvr );
    }
}


void
phg_cpa_delete_struct_net( cph, cp_args, css_srvr )
    register Cp_handle		cph;
    Phg_args			*cp_args;
    Cpx_css_srvr		*css_srvr;
{
    Cpa_struct_data		*stp;
    XID				*pexids = (XID *)NULL;
    int				count, reopen = 0;
    Phg_args_del_struct_net	*args = &cp_args->data.del_struct_net;

    register	int	i;

    if ( stp = phg_cpa_struct_exists( cph, css_srvr, CPX_BY_SID, args->id,
	    CPA_STRUCT_OP_CHECK ) ) {
	if ( !PEXGetStructuresInNetwork( css_srvr->display, stp->xid,
		args->flag == PFLAG_DEL ? PEXAll : PEXOrphans,
		&pexids, &count ) ) {
	    ERR_BUF( cph->erh, ERR900 );

	} else {
	    /* See if the open structure is involved. */
	    if ( PSL_STRUCT_STATE(cph->psl) == PSTRUCT_ST_STOP ) {
		if ( phg_cpa_open_struct_in_list( cph, css_srvr, CPX_BY_XID,
			count, (int *)pexids ) )
		    reopen = 1;
	    }

	    (void)PEXDestroyStructures( css_srvr->display, pexids, count );
	    /* Remove knowledge of them from the CP. */
	    for ( i = 0; i < count; i++ )
		(void)phg_cpa_struct_exists( cph, css_srvr, CPX_BY_XID,
		    (int)pexids[i], CPA_STRUCT_OP_DESTROY );

	    if ( reopen ) {
		if ( stp = CPA_CREATE_OPEN_STRUCT( cph, css_srvr ) )
		    phg_cpa_link_struct( cph, css_srvr, stp );
	    }
	    CPA_FLUSH( css_srvr );
	}
    }
}


void
phg_cpa_set_edit_mode( cph, cp_args, css_srvr )
    register Cp_handle		cph;
    Phg_args			*cp_args;
    Cpx_css_srvr		*css_srvr;
{
    Cpa_struct_data		*stp;

    if ( stp = phg_cpa_struct_exists( cph, css_srvr, CPX_BY_SID,
	    cph->psl->open_struct, CPA_STRUCT_OP_CHECK ) ) {
	(void)PEXSetEditingMode( css_srvr->display, stp->xid,
	    (CARD16)PEX_CONV_FROM_Peditmode((Pedit_mode)cp_args->data.idata ));
    }
}


void
phg_cpa_set_el_ptr( cph, cp_args, css_srvr )
    register Cp_handle		cph;
    Phg_args			*cp_args;
    Cpx_css_srvr		*css_srvr;
{
    Phg_args_set_el_ptr		*args = &cp_args->data.set_el_ptr;
    CARD16			base;
    INT32			offset;
    Cpa_struct_data		*stp;

    switch ( args->op ) {
	case PHG_ARGS_SETEP_ABS:
	    base = PEXBeginning;
	    offset = args->data;
	    break;
	case PHG_ARGS_SETEP_REL:
	    base = PEXCurrent;
	    offset = args->data;
	    break;
	case PHG_ARGS_SETEP_LABEL:
	    break;
    }

    if ( stp = phg_cpa_struct_exists( cph, css_srvr, CPX_BY_SID,
	    cph->psl->open_struct, CPA_STRUCT_OP_CHECK ) ) {
	if ( args->op == PHG_ARGS_SETEP_LABEL )
	    /* TODO: need to trap PEX error for nonexistent label. */
	    (void)PEXSetElementPointerAtLabel( css_srvr->display, stp->xid,
		(INT32)args->data, (INT32)0 );
	else
	    (void)PEXSetElementPointer( css_srvr->display, stp->xid, base, offset );
    }
}


void
phg_cpa_copy_all_els( cph, cp_args, css_srvr )
    register Cp_handle		cph;
    Phg_args			*cp_args;
    Cpx_css_srvr		*css_srvr;
{
    Cpa_struct_data		*src, *dest;

    if ( (dest = phg_cpa_struct_exists( cph, css_srvr, CPX_BY_SID,
	    cph->psl->open_struct, CPA_STRUCT_OP_CHECK ))
	&&
	 (src = phg_cpa_struct_exists( cph, css_srvr, CPX_BY_SID,
	    cp_args->data.idata, CPA_STRUCT_OP_CHECK )) ) {
	(void)PEXCopyElements( css_srvr->display,
	    src->xid, PEXBeginning, (INT32)0, PEXEnd, (INT32)0,
	    dest->xid, PEXCurrent, (INT32)0 );
	CPA_FLUSH( css_srvr );
    }
}


void
phg_cpa_delete_el( cph, cp_args, css_srvr )
    register Cp_handle		cph;
    Phg_args			*cp_args;
    Cpx_css_srvr		*css_srvr;
{
    Cpa_struct_data		*stp;
    Phg_args_del_el		*args = &cp_args->data.del_el;

    if ( stp = phg_cpa_struct_exists( cph, css_srvr, CPX_BY_SID,
	    args->op == PHG_ARGS_EMPTY_STRUCT ?
		args->data.struct_id : cph->psl->open_struct,
	    CPA_STRUCT_OP_CHECK ) ) {
	switch ( args->op ) {
	    case PHG_ARGS_DEL_CURRENT:
		(void)PEXDeleteElements( css_srvr->display, stp->xid,
		    PEXCurrent, (INT32)0, PEXCurrent, (INT32)0 );
		break;
	    case PHG_ARGS_DEL_RANGE:
		(void)PEXDeleteElements( css_srvr->display, stp->xid,
		    PEXBeginning, (INT32)args->data.ep_values.ep1,
		    PEXBeginning, (INT32)args->data.ep_values.ep2 );
		    break;
	    case PHG_ARGS_DEL_LABEL:
		/* TODO: Need to trap the "no such label" PEX error. */
		(void)PEXDeleteBetweenLabels( css_srvr->display, stp->xid,
		    (INT32)args->data.label_range.label1,
		    (INT32)args->data.label_range.label2 );
		    break;
	    case PHG_ARGS_EMPTY_STRUCT:
		(void)PEXDeleteElements( css_srvr->display, stp->xid,
		    PEXBeginning, (INT32)0, PEXEnd, (INT32)0 );
		    break;
	}
	CPA_FLUSH( css_srvr );

    } else if ( args->op == PHG_ARGS_EMPTY_STRUCT ) {
	/* Structure doesn't exist, create it. */
	if ( stp = phg_cpa_create_struct( cph, css_srvr, args->data.struct_id,
	    (Pedit_mode*)NULL, -1 ) ) {
	    phg_cpa_link_struct( cph, css_srvr, stp );
	}
    }
}


void
phg_cpa_inq_el_ptr( cph, cp_args, ret, css_srvr )
    Cp_handle		cph;
    Phg_args		*cp_args;	/* should be NULL */
    Phg_ret		*ret;
    Cpx_css_srvr	*css_srvr;
{
    Cpa_struct_data		*stp;
    CARD32			eptr;

    if ( stp = phg_cpa_struct_exists( cph, css_srvr, CPX_BY_SID,
	    cph->psl->open_struct, CPA_STRUCT_OP_CHECK ) ) {
	if ( PEXGetStructureInfo( css_srvr->display, stp->xid, (CARD16*)NULL,
		&eptr, (CARD32*)NULL, (CARD32*)NULL, (CARD16*)NULL) ) {
	    ret->data.idata = eptr;
	} else
	    ret->err = cph->erh->errno;
    }
}


void
phg_cpa_inq_struct_status( cph, cp_args, ret, css_srvr )
    Cp_handle		cph;
    Phg_args		*cp_args;
    Phg_ret		*ret;
    Cpx_css_srvr		*css_srvr;
{
    Cpa_struct_data		*stp;
    CARD32			numels;

    if ( stp = phg_cpa_struct_exists( cph, css_srvr, CPX_BY_SID,
	    cp_args->data.idata, CPA_STRUCT_OP_CHECK ) ) {
	if ( PEXGetStructureInfo( css_srvr->display, stp->xid, (CARD16*)NULL,
		(CARD32*)NULL, &numels, (CARD32*)NULL, (CARD16*)NULL) )
	    ret->data.idata = numels > 0 ?
		(Pint) PSTRUCT_STATUS_NOT_EMPTY : (Pint) PSTRUCT_STATUS_EMPTY;
	else
	    ret->err = cph->erh->errno;
    }
}


void
phg_cpa_inq_struct_ids( cph, cp_args, ret, css_srvr )
    Cp_handle		cph;
    Phg_args		*cp_args;	/* should be NULL */
    Phg_ret		*ret;
    Cpx_css_srvr	*css_srvr;
{
    int			*sids = (int *)NULL;
    int			count;
    Cp_a		*asrvr = &css_srvr->model.a;

    /* TODO: make this more efficient: don't copy the ids, use scratch
     * space for the original list.
     */
    count = phg_cpa_build_struct_id_list(cph, css_srvr, &sids, (XID**)NULL);
    if ( count > 0 ) {
	if ( !sids ) {
	    ret->err = ERR900;
	} else {
	    if ( PHG_SCRATCH_SPACE( &asrvr->scratch, count * sizeof(Pint)) ){
		bcopy( (char *)sids, (char *)asrvr->scratch.buf,
		    count * sizeof(Pint) );
		ret->data.int_list.num_ints = count;
		ret->data.int_list.ints = (Pint *)asrvr->scratch.buf;
	    } else
		ret->err = ERR900;
	    free( (char *)sids );
	}
    } else {
	ret->data.int_list.num_ints = 0;
	ret->data.int_list.ints = (Pint *)NULL;
    }
}


void
phg_cpa_el_search( cph, cp_args, ret, css_srvr )
    Cp_handle		cph;
    Phg_args		*cp_args;
    Phg_ret		*ret;
    Cpx_css_srvr	*css_srvr;
{
    Phg_args_el_search	*args = &cp_args->data.el_search;
    Cpa_struct_data	*stp;
    Cp_a		*asrvr = &css_srvr->model.a;
    int			j;
    CARD16		*incl, *excl;
    CARD16		search_status;
    CARD32		offset;

    register	int		i, count;
    register	CARD16		*list;
    register	Pelem_type	*elems;

    if ( !(stp = phg_cpa_struct_exists( cph, css_srvr, CPX_BY_SID,
	    args->struct_id, CPA_STRUCT_OP_CHECK )) )
	ret->err = ERR201;
    else {
	if ( (count = args->incl.num_elem_types + args->excl.num_elem_types) > 0
		&& !PHG_SCRATCH_SPACE(&asrvr->scratch, count * sizeof(*elems)) )
	    ret->err = ERR900;
	else {
	    incl = (CARD16 *)asrvr->scratch.buf;
	    excl = incl + args->incl.num_elem_types;
	    for ( j = 0; j < 2; j++ ) {
		list = (j == 0 ? incl : excl);
		elems = (Pelem_type *) (j == 0
		    ? args->incl.elem_types : args->excl.elem_types);
		count = (j == 0
		    ? args->incl.num_elem_types : args->excl.num_elem_types);
		for ( i = 0; i < count; i++, elems++, list++ )
		    PEX_CONV_FROM_Psrcheltype( *elems, *list )
	    }
	    if ( !PEXElementSearch( css_srvr->display, stp->xid,
		    (CARD16)PEXBeginning, (INT32)args->start_el,
		    (CARD32)PEX_CONV_FROM_Psrchdir(args->dir),
		    (CARD32)args->incl.num_elem_types,
		    (CARD32)args->excl.num_elem_types,
		    incl, excl, &search_status, &offset ) )
		ret->err = ERR900;	/* TODO: Use phg_pex_errno. */
	    else {
		ret->data.el_search.found_el = offset;
		ret->data.el_search.status =
		    PEX_CONV_TO_Psrchstatus(search_status);
	    }
	}
    }
}


void
phg_cpa_inq_wss_posted_to( cph, cp_args, ret, css_srvr )
    Cp_handle		cph;
    Phg_args		*cp_args;
    Phg_ret		*ret;
    Cpx_css_srvr	*css_srvr;
{
    /* Register variables not used since common case will be small lists.
     * Don't want to incur the register overhead in that case.
     */
    Cp_a		*asrvr = &css_srvr->model.a;
    CARD32		count;
    int			i;
    Cpa_struct_data	*stp;
    pexPhigsWks		*ids;
    Pint		*postings;
    Ws			*ws;

    if ( !(stp = phg_cpa_struct_exists( cph, css_srvr, CPX_BY_SID,
	    cp_args->data.idata, CPA_STRUCT_OP_CHECK )) )
	ret->err = ERR201;
    else if ( !PEXGetWksPostings( css_srvr->display, stp->xid, &count, &ids ) )
	ret->err = ERR900;	/* TODO: Use phg_pex_errno. */
    else {
	if ( !PHG_SCRATCH_SPACE( &asrvr->scratch, count * sizeof(Pint)) )
	    ret->err = ERR900;
	else {
	    ret->data.int_list.num_ints = count;
	    ret->data.int_list.ints = (Pint *)asrvr->scratch.buf;
	    postings = ret->data.int_list.ints;
	    for ( i = 0; i < count; i++, postings++, ids++ ) {
		if ( ws = phg_cpx_ws_exists( cph, CPX_BY_XID, css_srvr, (int)*ids ))
		    *postings = ws->id;
		else
		    /* Shouldn't happen, but do something just in case. */
		    *postings = *ids;
	    }
	}
    }
}


void
phg_cpa_inq_hierarchy( cph, cp_args, ret, css_srvr )
    Cp_handle		cph;
    Phg_args		*cp_args;
    Phg_ret		*ret;
    Cpx_css_srvr	*css_srvr;
{
    Cp_a			*asrvr = &css_srvr->model.a;
    Phg_args_q_hierarchy	*args = &cp_args->data.q_hierarchy;
    int				(*func)();
    CARD32			count, num_paths, size;
    caddr_t			path_data;

    register	int			i, j;
    register	Cpa_struct_data		*stp;
    register	Pint			*count_ptr;
    register	Pelem_ref		*ref_ptr;
    register	pexElementRef		*pex_ref;
    register	caddr_t			buf;


    ret->err = 0;
    func = args->dir == PHG_ARGS_HIER_ANCESTORS ?
	PEXGetAncestors : PEXGetDescendants;
    if ( !(stp = phg_cpa_struct_exists( cph, css_srvr, CPX_BY_SID,
	    args->struct_id, CPA_STRUCT_OP_CHECK )) )
	ret->err = ERR201;
    else if ( !(*func)( css_srvr->display, stp->xid,
	    (CARD16)PEX_CONV_FROM_Ppathorder(args->order), (CARD32)args->depth,
	    &num_paths, &path_data ) )
	ret->err = ERR900;	/* TODO: use phg_pex_errno. */
    else {
	/* See how much space is needed. */
	buf = path_data;
	ret->data.hierarchy.num_pairs = 0;
	size = num_paths * sizeof(Pint);
	for ( i = 0; i < num_paths; i++ ) {
	    count = *(CARD32 *)buf;
	    ret->data.hierarchy.num_pairs += count;
	    size += count * sizeof(Pelem_ref);
	    buf += sizeof(CARD32) + count * sizeof(pexElementRef);
	}
	if ( size > 0 
		&& !PHG_SCRATCH_SPACE( &asrvr->scratch, size ) )
	    ret->err = ERR900;
	else {
	    ret->data.hierarchy.counts.num_ints = num_paths;
	    ret->data.hierarchy.counts.ints = (Pint *)asrvr->scratch.buf;
	    ret->data.hierarchy.paths =
		(Pelem_ref *)(ret->data.hierarchy.counts.ints + num_paths);
	    count_ptr = ret->data.hierarchy.counts.ints;
	    ref_ptr = ret->data.hierarchy.paths;
	    buf = path_data;
	    for ( i = 0; i < num_paths; i++, count_ptr++ ) {
		*count_ptr = *(CARD32 *)buf;
		pex_ref = (pexElementRef *)(buf + sizeof(CARD32));
		for ( j = 0; j < *count_ptr; j++, pex_ref++, ref_ptr++ ) {
		    ref_ptr->elem_pos = pex_ref->offset;
		    if ( stp = phg_cpa_struct_exists( cph, css_srvr, CPX_BY_XID,
			    (int)pex_ref->structure, CPA_STRUCT_OP_CHECK ) )
			ref_ptr->struct_id = stp->sid;
		    else
			/* Shouldn't happen, but do something just in case. */
			ref_ptr->struct_id = pex_ref->structure;
		}
		buf += sizeof(CARD32) + *count_ptr * sizeof(*pex_ref);
	    }
	}
    }
}


void
phg_cpa_change_struct_id( cph, cp_args, css_srvr )
    Cp_handle		cph;
    Phg_args		*cp_args;
    Cpx_css_srvr	*css_srvr;
{
    Cpa_struct_data		*old, *new;
    Phg_args_change_struct	*args = &cp_args->data.change_struct;
    int				keep_old;
    CARD32			numels;
    CARD16			has_refs;

    old = phg_cpa_struct_exists( cph, css_srvr, CPX_BY_SID,
	args->orig_id, CPA_STRUCT_OP_CHECK );
    new = phg_cpa_struct_exists( cph, css_srvr, CPX_BY_SID,
	args->new_id, CPA_STRUCT_OP_CHECK );
    
    if ( old && new && old->sid == new->sid )
	return;	/* they're identical, do nothing */

    /* Clear or create the "new" structure. */
    if ( new )
	(void)PEXDeleteElements( css_srvr->display, new->xid,
	    PEXBeginning, (INT32)0, PEXEnd, (INT32)0 );
    else if ( new = phg_cpa_create_struct( cph, css_srvr,
	    args->new_id, (Pedit_mode *)NULL, -1 ) )
	phg_cpa_link_struct( cph, css_srvr, new );
    else
	return;	/* can't create the new structure */
    
    if ( old ) {
	/* Copy the elements from old to new. */
	(void)PEXCopyElements( css_srvr->display, old->xid,
	    PEXBeginning, (INT32)0, PEXEnd, (INT32)0,
	    new->xid, PEXBeginning, (INT32)0 );

	/* Determine the disposition of the old structure. */
	keep_old = 0;
	numels = !0;
	if ( args->posted )
	    keep_old = 1;
	else if ( PSL_STRUCT_STATE(cph->psl) == PSTRUCT_ST_STOP
		&& old->sid == cph->psl->open_struct )
	    keep_old = 1;
	else if ( PEXGetStructureInfo( css_srvr->display, old->xid,
		(CARD16*)NULL, (CARD32*)NULL, &numels, (CARD32*)NULL,
		(CARD16*)&has_refs ) && has_refs == True )
	    keep_old = 1;

	if ( keep_old ) {
	    if ( numels != 0 )
		(void)PEXDeleteElements( css_srvr->display, old->xid,
		    PEXBeginning, (INT32)0, PEXEnd, (INT32)0 );
	} else {
	    (void)PEXDestroyStructures( css_srvr->display, &old->xid, (int)1 );
	    (void)phg_cpa_struct_exists( cph, css_srvr, CPX_BY_SID,
		old->sid, CPA_STRUCT_OP_DESTROY );
	}
    }
    CPA_FLUSH( css_srvr );
}


static int change_err;

void
phg_cpa_change_struct_refs( cph, cp_args, css_srvr )
    Cp_handle		cph;
    Phg_args		*cp_args;
    Cpx_css_srvr	*css_srvr;
{
    Cpa_struct_data		*old, *new;
    Phg_args_change_struct	*args = &cp_args->data.change_struct;
    CARD16			has_refs;

    old = phg_cpa_struct_exists( cph, css_srvr, CPX_BY_SID,
	args->orig_id, CPA_STRUCT_OP_CHECK );
    new = phg_cpa_struct_exists( cph, css_srvr, CPX_BY_SID,
	args->new_id, CPA_STRUCT_OP_CHECK );
    
    if ( !old || (old && new && old->sid == new->sid) )
	return;	/* do nothing */

    if ( PEXGetStructureInfo( css_srvr->display, old->xid,
	    (CARD16*)NULL, (CARD32*)NULL, (CARD32*)NULL, (CARD32*)NULL,
	    (CARD16*)&has_refs ) && has_refs == True ) {
	if ( !new ) {
	    if ( new = phg_cpa_create_struct( cph, css_srvr,
		    args->new_id, (Pedit_mode *)NULL, -1 ) ) {
		phg_cpa_link_struct( cph, css_srvr, new );
	    } else {
		change_err = 1;
		return;	/* can't create the new structure */
	    }
	}
	(void)PEXChangeStructureRefs(  css_srvr->display,
	    old->xid, new->xid );
	CPA_FLUSH( css_srvr );
    }
}


void
phg_cpa_change_struct_idrefs( cph, cp_args, css_srvr )
    Cp_handle		cph;
    Phg_args		*cp_args;
    Cpx_css_srvr	*css_srvr;
{
    Ws			*ws;
    pexBitmask		mask[PEXMSGetWksInfo];
    CARD32              old_modes[MAX_NO_OPEN_WS];
    int			cur;

    /* First need to set workstation update states to avoid an intermediate
     * update when the reference is changed.  This can only work correctly
     * when the current update mode is Each (PHIGS ASAP), because turning
     * off updates when the mode is Easy will cause the reference change
     * update to be missed.  It's unnecessary to change the mode when the
     * current mode is None.
     */
    bzero((char *)mask, sizeof(mask));   
    PEX_BITSET(mask, PEXPWDisplayUpdate);
    cur = 0;
    CPX_FOR_ALL_WS(cph,ws) {
	if ( ws->css_srvr == css_srvr ) { /* only ones for this css_srvr */
	    (void)PEXGetWksInfo( ws->display, ws->rid, mask,
		(char *)&old_modes[cur] );
	    if ( old_modes[cur] == PEXVisualizeEach )
		(void)PEXSetDisplayUpdateMode( ws->display, ws->rid,
		    PEXVisualizeNone );
	    ++cur;
	}
    }

    change_err = 0;
    phg_cpa_change_struct_refs( cph, cp_args, css_srvr );
    if ( change_err == 0 ) {
	cp_args->data.change_struct.posted = 0;
	phg_cpa_change_struct_id( cph, cp_args, css_srvr );
    }

    /* Set workstation update states to what they were. */
    cur = 0;
    CPX_FOR_ALL_WS(cph,ws) {
	if ( ws->css_srvr == css_srvr ) {
	    if ( old_modes[cur] == PEXVisualizeEach )
		(void)PEXSetDisplayUpdateMode( ws->display, ws->rid,
		    (pexEnumTypeIndex) old_modes[cur] );
	    cur++;
	}
    }
    CPA_FLUSH( css_srvr );
}


void
phg_cpa_inc_spa_search( cph, cp_args, ret, css_srvr )
    Cp_handle		cph;
    Phg_args		*cp_args;
    Phg_ret		*ret;
    Cpx_css_srvr	*css_srvr;
{
    pexBitmask			mask;
    CARD8			*card8_p;
    CARD16			*card16_p;
    CARD32			length, *card32_p, ret_path_count;
    pexCoord3D			*pex_pt;
    PEXFLOAT			*pex_dist;
    pexElementRef		*ret_path;
    Cpa_struct_data		*stp;
    Cp_a			*asrvr = &css_srvr->model.a;
    Phg_args_inc_spa_search	*args = &cp_args->data.inc_spa_search;

    register int		i, count;
    register Pint		*names;
    register pexName		*pex_names;
    register pexElementRef	*pex_ref;
    register Pelem_ref	*el_ref;

    ret->err = 0;
    if ( !asrvr->iss_resources.search_context ) {
	/* First ISS occurrence.  Create the search context and filters. */
	asrvr->iss_resources.nrm_incl_filter = XAllocID( css_srvr->display );
	(void)PEXCreateNameSet( css_srvr->display,
	    asrvr->iss_resources.nrm_incl_filter );
	asrvr->iss_resources.nrm_excl_filter = XAllocID( css_srvr->display );
	(void)PEXCreateNameSet( css_srvr->display,
	    asrvr->iss_resources.nrm_excl_filter );
	asrvr->iss_resources.inv_incl_filter = XAllocID( css_srvr->display );
	(void)PEXCreateNameSet( css_srvr->display,
	    asrvr->iss_resources.inv_incl_filter );
	asrvr->iss_resources.inv_excl_filter = XAllocID( css_srvr->display );
	(void)PEXCreateNameSet( css_srvr->display,
	    asrvr->iss_resources.inv_excl_filter );

	mask = PEXSCNormalList | PEXSCInvertedList;
	length = 6 * sizeof(CARD32);
	if ( !PHG_SCRATCH_SPACE( &asrvr->scratch, length ) ) {
	    ret->err = ERR900;
	    return;
	}
	card32_p = (CARD32 *)asrvr->scratch.buf;
	*card32_p++ = 1;	/* length of normal filter list */
	*card32_p++ = asrvr->iss_resources.nrm_incl_filter;
	*card32_p++ = asrvr->iss_resources.nrm_excl_filter;
	*card32_p++ = 1;	/* length of inverted filter list */
	*card32_p++ = asrvr->iss_resources.inv_incl_filter;
	*card32_p++ = asrvr->iss_resources.inv_excl_filter;
	asrvr->iss_resources.search_context = XAllocID( css_srvr->display );
	(void)PEXCreateSearchContext( css_srvr->display,
	    asrvr->iss_resources.search_context,
	    mask, length, asrvr->scratch.buf );
    }

    /* Update the filters, silently clamping the length. */
    count = MIN(args->nrm_filt.incl_set.num_ints,
	cph->pdt.max_num_names_for_nameset);
    if ( !PHG_SCRATCH_SPACE( &asrvr->scratch, count * sizeof(pexName)) ) {
	ret->err = ERR900;
	return;
    }
    names = args->nrm_filt.incl_set.ints;
    pex_names = (pexName *)asrvr->scratch.buf;
    for ( i = 0; i < count; i++ )
	pex_names[i] = (pexName)names[i];
    (void)PEXChangeNameSet( css_srvr->display,
	asrvr->iss_resources.nrm_incl_filter,
	(CARD16)PEXNSReplace, (CARD32)count, pex_names );

    count = MIN(args->nrm_filt.excl_set.num_ints,
	cph->pdt.max_num_names_for_nameset);
    if ( !PHG_SCRATCH_SPACE( &asrvr->scratch, count * sizeof(pexName)) ) {
	ret->err = ERR900;
	return;
    }
    names = args->nrm_filt.excl_set.ints;
    pex_names = (pexName *)asrvr->scratch.buf;
    for ( i = 0; i < count; i++ )
	pex_names[i] = (pexName)names[i];
    (void)PEXChangeNameSet( css_srvr->display,
	asrvr->iss_resources.nrm_excl_filter,
	(CARD16)PEXNSReplace, (CARD32)count, pex_names );

    count = MIN(args->inv_filt.incl_set.num_ints,
	cph->pdt.max_num_names_for_nameset);
    if ( !PHG_SCRATCH_SPACE( &asrvr->scratch, count * sizeof(pexName)) ) {
	ret->err = ERR900;
	return;
    }
    names = args->inv_filt.incl_set.ints;
    pex_names = (pexName *)asrvr->scratch.buf;
    for ( i = 0; i < count; i++ )
	pex_names[i] = (pexName)names[i];
    (void)PEXChangeNameSet( css_srvr->display,
	asrvr->iss_resources.inv_incl_filter,
	(CARD16)PEXNSReplace, (CARD32)count, pex_names );

    count = MIN(args->inv_filt.excl_set.num_ints,
	cph->pdt.max_num_names_for_nameset);
    if ( !PHG_SCRATCH_SPACE( &asrvr->scratch, count * sizeof(pexName)) ) {
	ret->err = ERR900;
	return;
    }
    names = args->inv_filt.excl_set.ints;
    pex_names = (pexName *)asrvr->scratch.buf;
    for ( i = 0; i < count; i++ )
	pex_names[i] = (pexName)names[i];
    (void)PEXChangeNameSet( css_srvr->display,
	asrvr->iss_resources.inv_excl_filter,
	(CARD16)PEXNSReplace, (CARD32)count, pex_names );


    /* Update the search context. */
    mask = PEXSCPosition | PEXSCDistance | PEXSCCeiling | PEXSCModelClipFlag
	| PEXSCStartPath;
    length = sizeof(pexCoord3D) + sizeof(PEXFLOAT) + 3 * sizeof(CARD32);
    if ( args->start_path.num_elem_refs > 0 )
	length += args->start_path.num_elem_refs * sizeof(pexElementRef);
    if ( !PHG_SCRATCH_SPACE( &asrvr->scratch, length ) ) {
	ret->err = ERR900;
	return;
    }
    pex_pt = (pexCoord3D *)asrvr->scratch.buf;
    PEX_CONV_FROM_Ppoint3(&args->ref_pt, pex_pt)
    pex_dist = (PEXFLOAT *)(pex_pt + 1);
    *pex_dist = args->distance;
    card32_p = (CARD32 *)(pex_dist + 1);
    *card32_p = (CARD32)args->ceiling;
    card32_p++;
    *card32_p = (args->mclip_flag == PIND_CLIP) ?
	(CARD32) PEXClip : (CARD32) PEXNoClip;

    /* Convert the start path. */
    card32_p++;
    if ( args->start_path.num_elem_refs <= 0 )
	*card32_p = 0;
    else {
	*card32_p = args->start_path.num_elem_refs;
	pex_ref = (pexElementRef *)(card32_p + 1);
	el_ref = args->start_path.elem_refs;
	for ( i = 0; i < args->start_path.num_elem_refs; i++ ) {
	    if ( !(stp = phg_cpa_struct_exists( cph, css_srvr, CPX_BY_SID,
		    el_ref->struct_id, CPA_STRUCT_OP_CHECK )) ) {
		ret->err = ERR203;
		return;
	    }
	    pex_ref->structure = stp->xid;
	    pex_ref->offset = el_ref->elem_pos;
	    ++pex_ref; ++el_ref;
	}
    }

    PEXChangeSearchContext( css_srvr->display,
	asrvr->iss_resources.search_context,
	mask, length, asrvr->scratch.buf );

    /* Perform the search and retrieve the result. */
    if ( !PEXSearchNetwork( css_srvr->display,
	    asrvr->iss_resources.search_context,
	    &ret_path_count, &ret_path ) ) {
	ret->err = ERR900;	/* TODO: use phg_pex_errno */
	return;
    }

    /* Convert the returned path. */
    if ( ret_path_count <= 0 ) {
	ret->data.inc_spa_search.path.num_elem_refs = 0;
	ret->data.inc_spa_search.path.elem_refs = (Pelem_ref *)NULL;
    } else {
	/* Convert the structure ids to application ids. */
	length = ret_path_count * sizeof(Pelem_ref);
	if ( !PHG_SCRATCH_SPACE( &cph->scratch, length ) ) {
	    ret->err = ERR900;
	    return;
	}
	el_ref = (Pelem_ref *)cph->scratch.buf;
	pex_ref = ret_path;
	ret->data.inc_spa_search.path.num_elem_refs = ret_path_count;
	ret->data.inc_spa_search.path.elem_refs = el_ref;
	for ( i = 0; i < ret_path_count; i++, el_ref++, pex_ref++ ) {
	    el_ref->struct_id = CPA_DECODE_STRUCT_ID(css_srvr,
		pex_ref->structure);
	    el_ref->elem_pos = pex_ref->offset;
	}
    }
}

