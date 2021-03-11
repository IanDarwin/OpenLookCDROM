/* $XConsortium: cb_stru.c,v 5.5 94/04/17 20:40:55 mor Exp $ */

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

/* Miscellaneous Structure functions for the PHIGS C binding */

#include "phg.h"
#include "cp.h"
#include "cb_priv.h"

void
pset_indiv_asf( att_id, att_source)
    Paspect	att_id;		/* attribute identifier	*/
    Pasf	att_source;	/* attribute source	*/
{
    Phg_args		cp_args;
    Phg_el_data		ed;
    Phg_args_add_el	*args = &cp_args.data.add_el;

    if (CB_ENTRY_CHECK( phg_cur_cph, ERR5, Pfn_set_indiv_asf)) {
	if ( PSL_STRUCT_STATE( phg_cur_cph->psl) == PSTRUCT_ST_STOP) {
	    args->el_type = PELEM_INDIV_ASF;
	    ed.asf_info.attr_id = att_id;
	    ed.asf_info.asf = att_source;
	    if ( CB_BUILD_OC(args->el_type, &ed, &args->pex_oc) )
		CP_FUNC( phg_cur_cph, CP_FUNC_OP_ADD_EL, &cp_args, NULL);
	} else {
	    ERR_REPORT( phg_cur_cph->erh, ERR5);
	}
    }
}

void
pset_local_tran3(xform, compose_type)
Pmatrix3	xform;	/* transformation matrix	*/
Pcompose_type	compose_type;	/* composition type	*/
{
    Phg_args		cp_args;
    Phg_el_data		ed;
    Phg_args_add_el	*args = &cp_args.data.add_el;

    if (CB_ENTRY_CHECK(phg_cur_cph, ERR5, Pfn_set_local_tran3)) {
	if (PSL_STRUCT_STATE(phg_cur_cph->psl) != PSTRUCT_ST_STOP) {
	    ERR_REPORT(phg_cur_cph->erh, ERR5);
	}
	else {
	    args->el_type = PELEM_LOCAL_MODEL_TRAN3;
	    ed.local_xform3.comptype = compose_type;
	    ed.local_xform3.mat3 = (Pfloat *)xform;
	    if ( CB_BUILD_OC(args->el_type, &ed, &args->pex_oc) )
		CP_FUNC(phg_cur_cph, CP_FUNC_OP_ADD_EL, &cp_args, NULL);
	}
    }
}

void
pset_local_tran(xform, compose_type)
Pmatrix	xform;	/* transformation matrix	*/
Pcompose_type	compose_type;	/* composition type	*/
{
    Phg_args		cp_args;
    Phg_el_data		ed;
    Phg_args_add_el	*args = &cp_args.data.add_el;

    if (CB_ENTRY_CHECK(phg_cur_cph, ERR5, Pfn_set_local_tran)) {
	if (PSL_STRUCT_STATE(phg_cur_cph->psl) != PSTRUCT_ST_STOP) {
	    ERR_REPORT(phg_cur_cph->erh, ERR5);
	}
	else {
	    args->el_type = PELEM_LOCAL_MODEL_TRAN;
	    ed.local_xform.comptype = compose_type;
	    ed.local_xform.mat = (Pfloat *)xform;
	    if ( CB_BUILD_OC(args->el_type, &ed, &args->pex_oc) )
		CP_FUNC(phg_cur_cph, CP_FUNC_OP_ADD_EL, &cp_args, NULL);
	}
    }
}

void
pset_global_tran3(xform)
Pmatrix3	xform;	/* transformation matrix	*/
{
    Phg_args		cp_args;
    Phg_el_data		ed;
    Phg_args_add_el	*args = &cp_args.data.add_el;

    if (CB_ENTRY_CHECK(phg_cur_cph, ERR5, Pfn_set_global_tran3)) {
	if (PSL_STRUCT_STATE(phg_cur_cph->psl) != PSTRUCT_ST_STOP) {
	    ERR_REPORT(phg_cur_cph->erh, ERR5);
	}
	else {
	    args->el_type = PELEM_GLOBAL_MODEL_TRAN3;
	    ed.mat3 = (Pfloat *)xform;
	    if ( CB_BUILD_OC(args->el_type, &ed, &args->pex_oc) )
		CP_FUNC(phg_cur_cph, CP_FUNC_OP_ADD_EL, &cp_args, NULL);
	}
    }
}

void
pset_global_tran(xform)
Pmatrix	xform;	/* transformation matrix	*/
{
    Phg_args		cp_args;
    Phg_el_data		ed;
    Phg_args_add_el	*args = &cp_args.data.add_el;

    if (CB_ENTRY_CHECK(phg_cur_cph, ERR5, Pfn_set_global_tran)) {
	if (PSL_STRUCT_STATE(phg_cur_cph->psl) != PSTRUCT_ST_STOP) {
	    ERR_REPORT(phg_cur_cph->erh, ERR5);
	}
	else {
	    args->el_type = PELEM_GLOBAL_MODEL_TRAN;
	    ed.mat = (Pfloat *)xform;
	    if ( CB_BUILD_OC(args->el_type, &ed, &args->pex_oc) )
		CP_FUNC(phg_cur_cph, CP_FUNC_OP_ADD_EL, &cp_args, NULL);
	}
    }
}

void
pset_view_ind(index)
Pint	index;	/* view index	*/
{
    Phg_args		cp_args;
    Phg_el_data		ed;
    Phg_args_add_el	*args = &cp_args.data.add_el;

    if (CB_ENTRY_CHECK(phg_cur_cph, ERR5, Pfn_set_view_ind)) {
	if (PSL_STRUCT_STATE(phg_cur_cph->psl) != PSTRUCT_ST_STOP) {
	    ERR_REPORT(phg_cur_cph->erh, ERR5);
	}
	else if (index < 0) {
	    ERR_REPORT(phg_cur_cph->erh, ERR114);
	}
	else {
	    args->el_type = PELEM_VIEW_IND;
	    ed.idata = index;
	    if ( CB_BUILD_OC(args->el_type, &ed, &args->pex_oc) )
		CP_FUNC(phg_cur_cph, CP_FUNC_OP_ADD_EL, &cp_args, NULL);
	}
    }
}

void
pexec_struct(struct_id)
Pint	struct_id;	/* structure identifier	*/
{
    Phg_args		cp_args;
    Phg_el_data		ed;
    Phg_args_add_el	*args = &cp_args.data.add_el;

    if (CB_ENTRY_CHECK(phg_cur_cph, ERR5, Pfn_exec_struct)) {
	if (PSL_STRUCT_STATE(phg_cur_cph->psl) != PSTRUCT_ST_STOP) {
	    ERR_REPORT(phg_cur_cph->erh, ERR5);
	}
	else {
	    args->el_type = PELEM_EXEC_STRUCT;
	    ed.idata = struct_id;
	    if ( CB_BUILD_OC(args->el_type, &ed, &args->pex_oc) )
		CP_FUNC(phg_cur_cph, CP_FUNC_OP_ADD_EL, &cp_args, NULL);
	}
    }
}

void
plabel(label_id)
Pint	label_id;	/* label identifier	*/
{
    Phg_args		cp_args;
    Phg_el_data		ed;
    Phg_args_add_el	*args = &cp_args.data.add_el;

    if (CB_ENTRY_CHECK(phg_cur_cph, ERR5, Pfn_label)) {
	if (PSL_STRUCT_STATE(phg_cur_cph->psl) != PSTRUCT_ST_STOP) {
	    ERR_REPORT(phg_cur_cph->erh, ERR5);
	}
	else {
	    args->el_type = PELEM_LABEL;
	    ed.idata = label_id;
	    if ( CB_BUILD_OC(args->el_type, &ed, &args->pex_oc) )
		CP_FUNC(phg_cur_cph, CP_FUNC_OP_ADD_EL, &cp_args, NULL);
	}
    }
}

void
pappl_data(data)
Pdata	*data;	/* application data	*/
{
    Phg_args		cp_args;
    Phg_el_data		ed;
    Phg_args_add_el	*args = &cp_args.data.add_el;

    if (CB_ENTRY_CHECK(phg_cur_cph, ERR5, Pfn_appl_data)) {
	if (PSL_STRUCT_STATE(phg_cur_cph->psl) != PSTRUCT_ST_STOP) {
	    ERR_REPORT(phg_cur_cph->erh, ERR5);
	}
	else {
	    args->el_type = PELEM_APPL_DATA;
	    ed.appl_data = *data;
	    if ( CB_BUILD_OC(args->el_type, &ed, &args->pex_oc) )
		CP_FUNC(phg_cur_cph, CP_FUNC_OP_ADD_EL, &cp_args, NULL);
	}
    }
}

void
pset_pick_id( pick_id)
    Pint	pick_id;	/* pick identifier	*/
{
    Phg_args		cp_args;
    Phg_el_data		ed;
    Phg_args_add_el	*args = &cp_args.data.add_el;

    if (CB_ENTRY_CHECK(phg_cur_cph, ERR5, Pfn_set_pick_id)) {
	if (PSL_STRUCT_STATE(phg_cur_cph->psl) != PSTRUCT_ST_STOP) {
	    ERR_REPORT(phg_cur_cph->erh, ERR5);

	} else {
	    args->el_type = PELEM_PICK_ID;
	    ed.idata = pick_id;
	    if ( CB_BUILD_OC(args->el_type, &ed, &args->pex_oc) )
		CP_FUNC( phg_cur_cph, CP_FUNC_OP_ADD_EL, &cp_args, NULL);
	}
    }
}

void
pset_model_clip_vol3( op, half_spaces)
    Pint		op;			/* operator	*/
    Phalf_space_list3	*half_spaces;	/* list of half spaces	*/
{
    Phg_args		cp_args;
    Phg_el_data		ed;
    Phg_args_add_el	*args = &cp_args.data.add_el;

    if (CB_ENTRY_CHECK(phg_cur_cph, ERR5, Pfn_set_model_clip_vol3)) {
	if (PSL_STRUCT_STATE(phg_cur_cph->psl) != PSTRUCT_ST_STOP) {
	    ERR_REPORT(phg_cur_cph->erh, ERR5);

	} else {
	    args->el_type = PELEM_MODEL_CLIP_VOL3;
	    ed.mclip_vol3.op = op;
	    ed.mclip_vol3.hsplst = *half_spaces;
	    if ( CB_BUILD_OC(args->el_type, &ed, &args->pex_oc) )
		CP_FUNC( phg_cur_cph, CP_FUNC_OP_ADD_EL, &cp_args, NULL);
	}
    }
}

void
pset_model_clip_vol( op, half_spaces)
    Pint		op;			/* operator	*/
    Phalf_space_list	*half_spaces;	/* list of half spaces	*/
{
    Phg_args		cp_args;
    Phg_el_data		ed;
    Phg_args_add_el	*args = &cp_args.data.add_el;

    if (CB_ENTRY_CHECK(phg_cur_cph, ERR5, Pfn_set_model_clip_vol)) {
	if (PSL_STRUCT_STATE(phg_cur_cph->psl) != PSTRUCT_ST_STOP) {
	    ERR_REPORT(phg_cur_cph->erh, ERR5);

	} else {
	    args->el_type = PELEM_MODEL_CLIP_VOL;
	    ed.mclip_vol.op = op;
	    ed.mclip_vol.hsplst = *half_spaces;
	    if ( CB_BUILD_OC(args->el_type, &ed, &args->pex_oc) )
		CP_FUNC( phg_cur_cph, CP_FUNC_OP_ADD_EL, &cp_args, NULL);
	}
    }
}

void
pset_model_clip_ind( ind)
    Pclip_ind	ind;	/* clipping indicator	*/
{
    Phg_args		cp_args;
    Phg_el_data		ed;
    Phg_args_add_el	*args = &cp_args.data.add_el;

    if (CB_ENTRY_CHECK(phg_cur_cph, ERR5, Pfn_set_model_clip_ind)) {
	if (PSL_STRUCT_STATE(phg_cur_cph->psl) != PSTRUCT_ST_STOP) {
	    ERR_REPORT(phg_cur_cph->erh, ERR5);

	} else {
	    args->el_type = PELEM_MODEL_CLIP_IND;
	    ed.clip_ind = ind;
	    if ( CB_BUILD_OC(args->el_type, &ed, &args->pex_oc) )
		CP_FUNC( phg_cur_cph, CP_FUNC_OP_ADD_EL, &cp_args, NULL);
	}
    }
}

void
prestore_model_clip_vol()
{
    Phg_args		cp_args;
    Phg_el_data		ed;
    Phg_args_add_el	*args = &cp_args.data.add_el;

    if (CB_ENTRY_CHECK(phg_cur_cph, ERR5, Pfn_restore_model_clip_vol)) {
	if (PSL_STRUCT_STATE(phg_cur_cph->psl) != PSTRUCT_ST_STOP) {
	    ERR_REPORT(phg_cur_cph->erh, ERR5);

	} else {
	    args->el_type = PELEM_RESTORE_MODEL_CLIP_VOL;
	    if ( CB_BUILD_OC(args->el_type, &ed, &args->pex_oc) )
		CP_FUNC( phg_cur_cph, CP_FUNC_OP_ADD_EL, &cp_args, NULL);
	}
    }
}

void
pgdp3(point_list, gdp3_id, gdp_data)
    Ppoint_list3	*point_list;
    Pint		gdp3_id;	/* gdp function identifier	*/
    Pgdp_data3		*gdp_data;	/* data record pointer	*/
{
    Phg_args		cp_args;
    Phg_el_data		ed;
    Phg_args_add_el	*args = &cp_args.data.add_el;

    if (CB_ENTRY_CHECK(phg_cur_cph, ERR5, Pfn_gdp3)) {
	if (PSL_STRUCT_STATE(phg_cur_cph->psl) != PSTRUCT_ST_STOP) {
	    ERR_REPORT(phg_cur_cph->erh, ERR5);

	} else {
	    args->el_type = PELEM_GDP3;
	    ed.gdp3.id = gdp3_id;
	    ed.gdp3.pts = *point_list;
	    switch ( gdp3_id ) {
		default:
		    ed.gdp3.rec.unsupp.size = gdp_data->unsupp.size;
		    ed.gdp3.rec.unsupp.data = gdp_data->unsupp.data;
		    break;
	    }
	    if ( CB_BUILD_OC(args->el_type, &ed, &args->pex_oc) )
		CP_FUNC( phg_cur_cph, CP_FUNC_OP_ADD_EL, &cp_args, NULL);
	}
    }
}

void
pgdp(point_list, gdp_id, gdp_data)
    Ppoint_list	*point_list;	/* list of points */
    Pint	gdp_id;		/* gdp function identifier	*/
    Pgdp_data	*gdp_data;	/* data record pointer	*/
{
    Phg_args		cp_args;
    Phg_el_data		ed;
    Phg_args_add_el	*args = &cp_args.data.add_el;

    if (CB_ENTRY_CHECK(phg_cur_cph, ERR5, Pfn_gdp)) {
	if (PSL_STRUCT_STATE(phg_cur_cph->psl) != PSTRUCT_ST_STOP) {
	    ERR_REPORT(phg_cur_cph->erh, ERR5);

	} else {
	    args->el_type = PELEM_GDP;
	    ed.gdp.id = gdp_id;
	    ed.gdp.pts = *point_list;
	    switch ( gdp_id ) {
		default:
		    ed.gdp.rec.unsupp.size = gdp_data->unsupp.size;
		    ed.gdp.rec.unsupp.data = gdp_data->unsupp.data;
		    break;
	    }
	    if ( CB_BUILD_OC(args->el_type, &ed, &args->pex_oc) )
		CP_FUNC( phg_cur_cph, CP_FUNC_OP_ADD_EL, &cp_args, NULL);
	}
    }
}

void
pgse( id, gse )
    Pint	id;	/* gse identifier	*/
    Pgse_data	*gse;	/* gse data record	*/
{
    Phg_args            cp_args;
    Phg_el_data		ed;
    Phg_args_add_el	*args = &cp_args.data.add_el;

    if (CB_ENTRY_CHECK(phg_cur_cph, ERR5, Pfn_gse)) {
	if (PSL_STRUCT_STATE(phg_cur_cph->psl) != PSTRUCT_ST_STOP) {
	    ERR_REPORT(phg_cur_cph->erh, ERR5);
	} else {
	    args->el_type = PELEM_GSE;
	    ed.gse.id = id;
	    switch ( id ) {
		default:
		    ed.gse.rec.unsupp.size = gse->unsupp.size;
		    ed.gse.rec.unsupp.data = gse->unsupp.data;
		    break;
	    }
	    if ( CB_BUILD_OC(args->el_type, &ed, &args->pex_oc) )
		CP_FUNC( phg_cur_cph, CP_FUNC_OP_ADD_EL, &cp_args, NULL);
	}
    }
}

void
pset_rendering_colr_model( colour_model )
    Pint	colour_model;	/* colour model	*/
{
    Phg_args		cp_args;
    Phg_el_data		ed;
    Phg_args_add_el	*args = &cp_args.data.add_el;

    if (CB_ENTRY_CHECK(phg_cur_cph, ERR5, Pfn_set_rendering_colr_model)) {
	if (PSL_STRUCT_STATE(phg_cur_cph->psl) != PSTRUCT_ST_STOP) {
	    ERR_REPORT(phg_cur_cph->erh, ERR5);

	} else {
	    args->el_type = PELEM_RENDERING_COLR_MODEL;
	    ed.idata = colour_model;
	    if ( CB_BUILD_OC(args->el_type, &ed, &args->pex_oc) )
		CP_FUNC(phg_cur_cph, CP_FUNC_OP_ADD_EL, &cp_args, NULL);
	}
    }
}

void
pset_para_surf_characs( psc_type, data )
    Pint		psc_type;	/* type	*/
    Ppara_surf_characs	*data;		/* data record */
{
    Phg_args		cp_args;
    Phg_el_data		ed;
    Phg_args_add_el	*args = &cp_args.data.add_el;

    if (CB_ENTRY_CHECK(phg_cur_cph, ERR5, Pfn_set_para_surf_characs)) {
	if (PSL_STRUCT_STATE(phg_cur_cph->psl) != PSTRUCT_ST_STOP) {
	    ERR_REPORT(phg_cur_cph->erh, ERR5);

	} else {
	    args->el_type = PELEM_PARA_SURF_CHARACS;
	    ed.psc.type = psc_type;
	    ed.psc.data = *data;
	    if ( CB_BUILD_OC(args->el_type, &ed, &args->pex_oc) )
		CP_FUNC(phg_cur_cph, CP_FUNC_OP_ADD_EL, &cp_args, NULL);
	}
    }
}

/* Inquiry functions */

void
pinq_struct_st( struct_state)
    Pstruct_st	*struct_state;	/* OUT structure state	*/
{
    if ( CB_ENTRY_CHECK( phg_cur_cph, 0, Pfn_INQUIRY))
	 *struct_state = PSL_STRUCT_STATE( phg_cur_cph->psl);
    else
	 *struct_state = PSTRUCT_ST_STCL;
}

void
pinq_open_struct( error_ind, status, struct_id)
    Pint		*error_ind;	/* OUT error indicator	*/
    Popen_struct_status	*status;	/* OUT status of open structure	*/
    Pint		*struct_id;	/* OUT structure identifier	*/
{
    if ( !CB_ENTRY_CHECK( phg_cur_cph, 0, Pfn_INQUIRY)) {
	*error_ind = ERR2;

    } else {
	if ( PSL_STRUCT_STATE( phg_cur_cph->psl) != PSTRUCT_ST_STOP) {
	    *error_ind = 0;
	    *status = PSTRUCT_NONE;
	} else {
	    *error_ind = 0;
	    *status = PSTRUCT_OPEN;
	    *struct_id = phg_cur_cph->psl->open_struct;
	}
    }
}

void
pinq_struct_status( struct_id, error_ind, status)
    Pint		struct_id;	/* structure identifier	*/
    Pint		*error_ind;	/* OUT error indicator	*/
    Pstruct_status	*status;	/* OUT existence status	*/
{
    Phg_args				cp_args;
    Phg_ret				ret;

    if ( !CB_ENTRY_CHECK( phg_cur_cph, 0, Pfn_INQUIRY)) {
	*error_ind = ERR2;

    } else {
	cp_args.data.idata = struct_id;
	ret.err = 0;
	CP_FUNC( phg_cur_cph, CP_FUNC_OP_INQ_STRUCT_STATUS, &cp_args, &ret);
	if ( ret.err) {
	    *error_ind = ret.err;
	} else {
	    *error_ind = 0;
	    *status = (Pstruct_status)ret.data.idata;
	}
    }
}

void
pinq_cur_elem_type_size( error_ind, type, size)
    Pint	*error_ind;	/* OUT error indicator	*/
    Pelem_type	*type;		/* OUT element type	*/
    size_t	*size;		/* OUT element size	*/
{
    Phg_args				cp_args;
    register Phg_args_q_el_data		*args = &cp_args.data.q_el_data;
    Phg_ret	ret;

    if ( !CB_ENTRY_CHECK( phg_cur_cph, 0, Pfn_INQUIRY)) {
	*error_ind = ERR5;

    } else if ( PSL_STRUCT_STATE( phg_cur_cph->psl) != PSTRUCT_ST_STOP) {
	*error_ind = ERR5;

    } else {
	args->el_id = -1;	/* signal to use the current element */
	ret.err = 0;
	CP_FUNC( phg_cur_cph, CP_FUNC_OP_INQ_EL_TYPE_SIZE, &cp_args, &ret);
	if ( ret.err) {
	    *error_ind = ret.err;
	} else {
	    *error_ind = 0;
	    *type = ret.data.el_type_size.type;
	    *size = ret.data.el_type_size.size;
	}
    }
}

void
pinq_elem_type_size( struct_id, element, error_ind, type, size)
    Pint	struct_id;	/* structure identifier	*/
    Pint	element;	/* element number	*/
    Pint	*error_ind;	/* OUT error indicator	*/
    Pelem_type	*type;		/* OUT element type	*/
    size_t	*size;		/* OUT element size	*/
{
    Phg_args				cp_args;
    register Phg_args_q_el_data		*args = &cp_args.data.q_el_data;
    Phg_ret				ret;

    if ( !CB_ENTRY_CHECK( phg_cur_cph, 0, Pfn_INQUIRY)) {
	*error_ind = ERR2;

    } else if ( element < 0) {
	*error_ind = ERR202;

    } else {
	args->struct_id = struct_id;
	args->el_id = element;
	ret.err = 0;
	CP_FUNC( phg_cur_cph, CP_FUNC_OP_INQ_EL_TYPE_SIZE, &cp_args, &ret);
	if ( ret.err) {
	    *error_ind = ret.err;
	} else {
	    *error_ind = 0;
	    *type = ret.data.el_type_size.type;
	    *size = ret.data.el_type_size.size;
	}
    }
}


static void
inq_content( cp_args, store, ret )
    Phg_args		*cp_args;
    _Pstore		*store;
    Phg_ret		*ret;
{
    int			size;
    pexElementInfo	*oc;

    ret->err = 0;
    CP_FUNC(phg_cur_cph, CP_FUNC_OP_INQ_EL_CONTENT, cp_args, ret);
    if ( !ret->err ) {
	if ( ret->data.el_info.op != PELEM_NIL ) {
	    oc = ret->data.el_info.pex_oc.oc;
	    size = phg_utx_compute_el_size( oc, oc );
	    if ( CB_STORE_SPACE( store, size, &ret->err ) )
		phg_utx_el_data_from_pex( oc, store->buf,
		    &store->data.elem_data );
	}
    }
}


void
pinq_cur_elem_content( store, error_ind, data)
    Pstore      store;         /* handle to Store object       */
    Pint	*error_ind;	/* OUT error indicator	        */
    Pelem_data	**data;		/* OUT data record	*/
{
    Phg_args				cp_args;
    register Phg_args_q_el_data		*args = &cp_args.data.q_el_data;
    Phg_ret	ret;

    if ( !CB_ENTRY_CHECK( phg_cur_cph, 0, Pfn_INQUIRY)) {
	*error_ind = ERR5;

    } else if ( PSL_STRUCT_STATE( phg_cur_cph->psl) != PSTRUCT_ST_STOP) {
	*error_ind = ERR5;

    } else {
	args->el_id = -1;	/* signal to use the current element */
	inq_content( &cp_args, ((_Pstore *)store), &ret );
	*data = &((_Pstore *)store)->data.elem_data;
	*error_ind = ret.err;
    }
}

void
pinq_elem_content( struct_id, element, store, error_ind, data)
    Pint	struct_id;	/* structure identifier	*/
    Pint	element;	/* element number	*/
    Pstore      store;         /* handle to Store object */
    Pint	*error_ind;	/* OUT error indicator	*/
    Pelem_data	**data;		/* OUT data record	*/
{
    Phg_args				cp_args;
    register Phg_args_q_el_data		*args = &cp_args.data.q_el_data;
    Phg_ret	ret;

    if ( !CB_ENTRY_CHECK( phg_cur_cph, 0, Pfn_INQUIRY)) {
	*error_ind = ERR2;

    } else if ( element == 0) {
	*error_ind = 0;
	*data = (Pelem_data *) 0;

    } else if ( element < 0) {
	*error_ind = ERR202;

    } else {
	args->struct_id = struct_id;
	args->el_id = element;
	inq_content( &cp_args, ((_Pstore *)store), &ret );
	*data = &((_Pstore *)store)->data.elem_data;
	*error_ind = ret.err;
    }
}

void
pinq_struct_ids( length, start, error_ind, struct_ids, total_length)
    Pint	length;		/* length of appl. list	*/
    Pint	start;		/* starting position	*/
    Pint	*error_ind;	/* OUT error indicator	*/
    Pint_list	*struct_ids;	/* OUT list of structure ids	*/
    Pint	*total_length;	/* OUT length of list in PHIGS	*/
{
    Phg_args		cp_args;
    Phg_ret		ret;

    if ( !CB_ENTRY_CHECK( phg_cur_cph, 0, Pfn_INQUIRY)) {
	*error_ind = ERR2;

    } else {
	ret.err = 0;
	CP_FUNC( phg_cur_cph, CP_FUNC_OP_INQ_STRUCT_IDS, &cp_args, &ret);
	if ( ret.err) {
	    *error_ind = ret.err;
	} else {
	    *error_ind = 0;
	    struct_ids->num_ints = 0;
	    if ( (*total_length = ret.data.int_list.num_ints) > 0) {
		if (start < 0 || start >= ret.data.int_list.num_ints)
		    *error_ind = ERR2201;
		else if (length > 0) {
		    struct_ids->num_ints = MIN( length,
					    ret.data.int_list.num_ints - start);
		    bcopy( (char*) &ret.data.int_list.ints[start],
			    (char*) struct_ids->ints,
			    struct_ids->num_ints * sizeof(Pint));
		} else if (length < 0)
		    *error_ind = ERRN153;
	    }
	}
    }
}

void
pinq_paths_ances(struct_id, order, depth, store, error_ind, paths)
Pint            	struct_id;	/* structure identifier	*/
Ppath_order     	order;   	/* path order	*/
Pint            	depth;		/* path depth	*/
Pstore                  store;         /* handle to Store object */
Pint            	*error_ind;	/* OUT error indicator	*/
Pelem_ref_list_list	**paths;	/* OUT structure path list	*/
{
    Phg_args		cp_args;
    Phg_ret		ret;

    if ( !CB_ENTRY_CHECK( phg_cur_cph, 0, Pfn_INQUIRY)) {
	*error_ind = ERR2;

    } else if ( depth < 0) {
	*error_ind = ERR207;

    } else {
	cp_args.data.q_hierarchy.dir = PHG_ARGS_HIER_ANCESTORS;
	cp_args.data.q_hierarchy.struct_id = struct_id;
	cp_args.data.q_hierarchy.order = order;
	cp_args.data.q_hierarchy.depth = depth;
	ret.err = 0;
	*paths = &((_Pstore *)store)->data.struct_paths;
	(*paths)->num_elem_ref_lists = 0;
	CP_FUNC( phg_cur_cph, CP_FUNC_OP_INQ_HIERARCHY, &cp_args, &ret);
	if ( !(*error_ind = ret.err) ) {
	    phg_cb_copy_hierarchy( &ret.data.hierarchy, ((_Pstore *)store),
				   error_ind, *paths );
	}
    }
}

void
pinq_paths_descs(struct_id, order, depth, store, error_ind, paths)
Pint            	struct_id;	/* structure identifier	*/
Ppath_order     	order;   	/* path order	*/
Pint            	depth;		/* path depth	*/
Pstore                  store;         /* handle to Store object      */
Pint            	*error_ind;	/* OUT error indicator	*/
Pelem_ref_list_list	**paths;	/* OUT structure path list	*/
{
    Phg_args		cp_args;
    Phg_ret		ret;

    if ( !CB_ENTRY_CHECK( phg_cur_cph, 0, Pfn_INQUIRY)) {
	*error_ind = ERR2;

    } else if ( depth < 0) {
	*error_ind = ERR207;

    } else {
	cp_args.data.q_hierarchy.dir = PHG_ARGS_HIER_DESCENDANTS;
	cp_args.data.q_hierarchy.struct_id = struct_id;
	cp_args.data.q_hierarchy.order = order;
	cp_args.data.q_hierarchy.depth = depth;
	ret.err = 0;
	*paths = &((_Pstore *)store)->data.struct_paths;
	(*paths)->num_elem_ref_lists = 0;
	CP_FUNC( phg_cur_cph, CP_FUNC_OP_INQ_HIERARCHY, &cp_args, &ret);
	if ( !(*error_ind = ret.err) ) {
	    phg_cb_copy_hierarchy( &ret.data.hierarchy, ((_Pstore *)store),
				   error_ind, *paths );
	}
    }
}

void
pinq_wss_posted(struct_id, length, start, error_ind, ws, total_length)
Pint     	struct_id;	/* structure identifier	*/
Pint    	length;		/* length of application list	*/
Pint    	start;		/* starting position	*/
Pint    	*error_ind;	/* OUT error indicator	*/
Pint_list	*ws;		/* OUT list of workstations	*/
Pint    	*total_length;	/* OUT length of list in PHIGS	*/
{
    Phg_args		cp_args;
    Phg_ret		ret;

    if ( !CB_ENTRY_CHECK( phg_cur_cph, 0, Pfn_INQUIRY)) {
	*error_ind = ERR2;

    } else {
	cp_args.data.idata = struct_id;
	ret.err = 0;
	CP_FUNC( phg_cur_cph, CP_FUNC_OP_INQ_WSS_POSTED_TO, &cp_args, &ret);
	if ( ret.err) {
	    *error_ind = ret.err;
	} else {
	    *error_ind = 0;
	    ws->num_ints = 0;
	    if ( (*total_length = ret.data.int_list.num_ints) > 0)
		if (start < 0 || start >= ret.data.int_list.num_ints)
		    *error_ind = ERR2201;
		else if (length > 0) {
		    ws->num_ints = MIN( length, ret.data.int_list.num_ints - start);
		    bcopy( (char*) &ret.data.int_list.ints[start],
			    (char*) ws->ints,
			    ws->num_ints * sizeof(Pint));
		} else if (length < 0)
		    *error_ind = ERRN153;
	}
    }
}

void
pelem_search(struct_id, start_el, dir, incl, excl, error_ind, status, found_el)
Pint		struct_id;	/* structure identifier	*/
Pint		start_el;	/* starting element pointer	*/
Psearch_dir	dir;	/* search direction	*/
Pelem_type_list	*incl;	/* element incl. list	*/
Pelem_type_list	*excl;	/* element excl. list	*/
Pint		*error_ind;	/* OUT error indicator	*/
Psearch_status	*status;	/* OUT search status	*/
Pint		*found_el;	/* OUT found element pointer	*/
{
    Phg_args                     cp_args;
    register Phg_args_el_search	*args = &cp_args.data.el_search;
    Phg_ret			 ret;
    Phg_ret_el_search		*search;

    if ( ! CB_ENTRY_CHECK(phg_cur_cph, 0, Pfn_INQUIRY)) {
	*error_ind = ERR2;
    } else {
	args->struct_id = struct_id;
	args->start_el = start_el;
	args->dir = dir;
	args->incl = *incl;
	args->excl = *excl;
	CP_FUNC( phg_cur_cph, CP_FUNC_OP_EL_SEARCH, &cp_args, &ret);
	if ( ret.err )
	    *error_ind = ret.err;
	else {
	    *error_ind = 0;
	    search = &ret.data.el_search;
	    *status = search->status;
	    *found_el = search->found_el;
	}
    }
}

static void
incr_spa_search(ref, dist, sp, mclip_flag, ceil, norm, inv, len, st,
    error_ind, fp, tot_len)
Ppoint3	        	*ref;		/* search reference point	*/
Pfloat	        	dist;		/* search distance	*/
Pelem_ref_list		*sp;		/* starting path list	*/
Pclip_ind		mclip_flag;	/* model clip flag */
Pint	        	ceil;		/* search ceiling index	*/
Pfilter_list    	*norm;		/* normal filter list	*/
Pfilter_list    	*inv;		/* inverted filter list	*/
Pint	        	len;		/* length of application list	*/
Pint	        	st;		/* starting position	*/
Pint	        	*error_ind;	/* OUT error indicator	*/
Pelem_ref_list		*fp;		/* OUT found path	*/
Pint	        	*tot_len;	/* OUT length of list in PHIGS	*/
{
    Phg_args		cp_args;
    unsigned		num_names;
    Pint		num_incl, num_excl;
    Pint		*names = (Pint *)NULL;
    Phg_ret		ret;
    Pelem_ref_list	*path = &ret.data.inc_spa_search.path;

    register Phg_args_inc_spa_search	*args = &cp_args.data.inc_spa_search;
    register Pint			*inc_names, *exc_names;
    register int			i;

    if ( ! CB_ENTRY_CHECK(phg_cur_cph, 0, Pfn_INQUIRY)) {
	*error_ind = ERR2;

    } else if ( ceil < 1 || ceil > sp->num_elem_refs ) {
	*error_ind = ERR204;

    } else {
	args->ref_pt = *ref;
	args->distance = dist;
	args->start_path = *sp;
	args->mclip_flag = mclip_flag;
	args->ceiling = ceil;
	args->nrm_filt.incl_set.num_ints = 0;
	args->nrm_filt.excl_set.num_ints = 0;
	args->inv_filt.incl_set.num_ints = 0;
	args->inv_filt.excl_set.num_ints = 0;

	/* Silently clamp the number of filters to that supported. */
	num_incl = MIN(norm->num_filters,
	    phg_cur_cph->pdt.max_length_normal_iss_filter);
	num_excl = MIN(inv->num_filters,
	    phg_cur_cph->pdt.max_length_inverted_iss_filter);
	for ( i = 0; i < num_incl; i++ ) {
	    args->nrm_filt.incl_set.num_ints +=
		norm->filters[i].incl_set.num_ints;
	    args->nrm_filt.excl_set.num_ints +=
		norm->filters[i].excl_set.num_ints;
	}
	for ( i = 0; i < num_excl; i++ ) {
	    args->inv_filt.incl_set.num_ints +=
		inv->filters[i].incl_set.num_ints;
	    args->inv_filt.excl_set.num_ints +=
		inv->filters[i].excl_set.num_ints;
	}
	num_names = args->nrm_filt.incl_set.num_ints
	    + args->nrm_filt.excl_set.num_ints
	    + args->inv_filt.incl_set.num_ints
	    + args->inv_filt.excl_set.num_ints;
	if ( num_names == 0 ) {
	    /* Get rid of any empty lists. */
	    args->nrm_filt.incl_set.num_ints = 0;
	    args->nrm_filt.excl_set.num_ints = 0;
	    args->inv_filt.incl_set.num_ints = 0;
	    args->inv_filt.excl_set.num_ints = 0;
	} else {
	    /* Collapse each filter's list of names into one big inclusion
	     * and exclusion set per inverted and normal filter set.
	     */
	    if ( !(names = (Pint *) malloc((sizeof(Pint) * num_names))) ) {
		*error_ind = ERR900;
		return;
	    }

	    args->nrm_filt.incl_set.ints = names;
	    args->nrm_filt.excl_set.ints = args->nrm_filt.incl_set.ints
		+ args->nrm_filt.incl_set.num_ints;
	    args->inv_filt.incl_set.ints = args->nrm_filt.excl_set.ints
		+ args->nrm_filt.excl_set.num_ints;
	    args->inv_filt.excl_set.ints = args->inv_filt.incl_set.ints
		+ args->inv_filt.incl_set.num_ints;

	    inc_names = args->nrm_filt.incl_set.ints;
	    exc_names = args->nrm_filt.excl_set.ints;
	    for ( i = 0; i < num_incl; i++ ) {
		bcopy( (char *)norm->filters[i].incl_set.ints,
		    (char *)inc_names,
		    norm->filters[i].incl_set.num_ints * sizeof(Pint) );
		inc_names += norm->filters[i].incl_set.num_ints;
		bcopy( (char *)norm->filters[i].excl_set.ints,
		    (char *)exc_names,
		    norm->filters[i].excl_set.num_ints * sizeof(Pint) );
		exc_names += norm->filters[i].excl_set.num_ints;
	    }

	    inc_names = args->inv_filt.incl_set.ints;
	    exc_names = args->inv_filt.excl_set.ints;
	    for ( i = 0; i < num_excl; i++ ) {
		bcopy( (char *)inv->filters[i].incl_set.ints,
		    (char *)inc_names,
		    inv->filters[i].incl_set.num_ints * sizeof(Pint) );
		inc_names += inv->filters[i].incl_set.num_ints;
		bcopy( (char *)inv->filters[i].excl_set.ints,
		    (char *)exc_names,
		    inv->filters[i].excl_set.num_ints * sizeof(Pint) );
		exc_names += inv->filters[i].excl_set.num_ints;
	    }
	}

	CP_FUNC( phg_cur_cph, CP_FUNC_OP_INC_SPA_SEARCH, &cp_args, &ret );
	if ( names )
	    free( (char *)names );
	if ( ret.err )
	    *error_ind = ret.err;
	else {
	    *error_ind = 0;
	    *tot_len = path->num_elem_refs;
	    fp->num_elem_refs = 0;
	    if ( path->num_elem_refs > 0 ) {
		if ( st < 0 || st >= path->num_elem_refs )
		    *error_ind = ERR2201;
		else if ( len > 0 ) {
		    fp->num_elem_refs = MIN(len, path->num_elem_refs - st);
		    bcopy( (char *)&path->elem_refs[st], (char *)fp->elem_refs,
			(int)(fp->num_elem_refs * sizeof(Pelem_ref)) );
		} else if ( len < 0 )
		    *error_ind = ERRN153;
	    }
	}
    }
}

void
pincr_spa_search3(ref, dist, sp, mclip_flag, ceil, norm, inv, len, st,
    error_ind, fp, tot_len)
Ppoint3	        	*ref;		/* search reference point	*/
Pfloat	        	dist;		/* search distance	*/
Pelem_ref_list		*sp;		/* starting path list	*/
Pclip_ind		mclip_flag;	/* model clip flag */
Pint	        	ceil;		/* search ceiling index	*/
Pfilter_list    	*norm;		/* normal filter list	*/
Pfilter_list    	*inv;		/* inverted filter list	*/
Pint	        	len;		/* length of application list	*/
Pint	        	st;		/* starting position	*/
Pint	        	*error_ind;	/* OUT error indicator	*/
Pelem_ref_list		*fp;		/* OUT found path	*/
Pint	        	*tot_len;	/* OUT length of list in PHIGS	*/
{
    incr_spa_search( ref, dist, sp, mclip_flag, ceil, norm, inv, len,
	st, error_ind, fp, tot_len);
}

void
pincr_spa_search(ref, dist, sp, mclip_flag, ceil, norm, inv, len, st,
    error_ind, fp, tot_len)
Ppoint	        	*ref;		/* search reference point	*/
Pfloat	        	dist;		/* search distance	*/
Pelem_ref_list		*sp;		/* starting path list	*/
Pclip_ind		mclip_flag;	/* model clip flag */
Pint	        	ceil;		/* search ceiling index	*/
Pfilter_list    	*norm;		/* normal filter list	*/
Pfilter_list    	*inv;		/* inverted filter list	*/
Pint	        	len;		/* length of application list	*/
Pint	        	st;		/* starting position	*/
Pint	        	*error_ind;	/* OUT error indicator	*/
Pelem_ref_list		*fp;		/* OUT found path	*/
Pint	        	*tot_len;	/* OUT length of list in PHIGS	*/
{
    Ppoint3	ref_pt;

    ref_pt.x = ref->x; ref_pt.y = ref->y; ref_pt.z = 0.0;
    incr_spa_search( &ref_pt, dist, sp, mclip_flag, ceil, norm, inv, len,
	st, error_ind, fp, tot_len);
}

void
pset_colr_map_ind( index )
    Pint	index;	/* colour mapping index	*/
{
    Phg_args		cp_args;
    Phg_el_data		ed;
    Phg_args_add_el	*args = &cp_args.data.add_el;

    if (CB_ENTRY_CHECK(phg_cur_cph, ERR5, Pfn_set_colr_map_ind)) {
	if (PSL_STRUCT_STATE(phg_cur_cph->psl) != PSTRUCT_ST_STOP) {
	    ERR_REPORT(phg_cur_cph->erh, ERR5);
	}
	else if (index < 0) {
	    ERR_REPORT(phg_cur_cph->erh, ERR121);
	}
	else {
	    args->el_type = PELEM_COLR_MAP_IND;
	    ed.idata = index;
	    if ( CB_BUILD_OC(args->el_type, &ed, &args->pex_oc) )
		CP_FUNC(phg_cur_cph, CP_FUNC_OP_ADD_EL, &cp_args, NULL);
	}
    }
}

void
pset_hlhsr_id( id )
    Pint	id;	/* HLHSR identifier	*/
{
    Phg_args		cp_args;
    Phg_el_data		ed;
    Phg_args_add_el	*args = &cp_args.data.add_el;
    
    if (CB_ENTRY_CHECK(phg_cur_cph, ERR5, Pfn_set_hlhsr_id)) {
	if (PSL_STRUCT_STATE(phg_cur_cph->psl) != PSTRUCT_ST_STOP) {
	    ERR_REPORT(phg_cur_cph->erh, ERR5);

	} else {
	    args->el_type = PELEM_HLHSR_ID;
	    ed.idata = id;
	    if ( CB_BUILD_OC(args->el_type, &ed, &args->pex_oc) )
		CP_FUNC( phg_cur_cph, CP_FUNC_OP_ADD_EL, &cp_args, NULL);
	}
    }
}
