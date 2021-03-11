/* $XConsortium: cb_sedt.c,v 5.2 94/04/17 20:40:55 rws Exp $ */

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

/* Structure edit functions for the PHIGS C binding */

#include "phg.h"
#include "cp.h"
#include "cb_priv.h"

void
popen_struct( struct_id)
    Pint	struct_id;	/* structure identifier	*/
{
    Phg_args		cp_args;
    Phg_ret		ret;

    if (CB_ENTRY_CHECK( phg_cur_cph, ERR6, Pfn_open_struct)) {
	if ( PSL_STRUCT_STATE( phg_cur_cph->psl) == PSTRUCT_ST_STCL) {
	    cp_args.data.idata = struct_id;
	    CP_FUNC( phg_cur_cph, CP_FUNC_OP_OPEN_STRUCT, &cp_args, &ret);
	    if ( !ret.err)
		PSL_STRUCT_STATE( phg_cur_cph->psl) = PSTRUCT_ST_STOP;
	    ERR_FLUSH( phg_cur_cph->erh);
	} else {
	    ERR_REPORT( phg_cur_cph->erh, ERR6);
	}
    }
}

void
pclose_struct()
{
    Phg_args		cp_args;

    if (CB_ENTRY_CHECK( phg_cur_cph, ERR5, Pfn_close_struct)) {
	if ( PSL_STRUCT_STATE( phg_cur_cph->psl) == PSTRUCT_ST_STOP) {
	    CP_FUNC( phg_cur_cph, CP_FUNC_OP_CLOSE_STRUCT, &cp_args, NULL);
	    PSL_STRUCT_STATE( phg_cur_cph->psl) = PSTRUCT_ST_STCL;
	} else {
	    ERR_REPORT( phg_cur_cph->erh, ERR5);
	}
    }
}

void
pcopy_all_elems_struct( struct_id)
    Pint	struct_id;	/* structure identifier	*/
{
    Phg_args		cp_args;

    if ( CB_ENTRY_CHECK( phg_cur_cph, ERR5, Pfn_copy_all_elems_struct)) {

	if ( PSL_STRUCT_STATE( phg_cur_cph->psl) != PSTRUCT_ST_STOP) {
	    ERR_REPORT( phg_cur_cph->erh, ERR5);

	} else {
	    cp_args.data.idata = struct_id;
	    CP_FUNC( phg_cur_cph, CP_FUNC_OP_COPY_ALL_ELS, &cp_args, NULL);
	}
    }
}

void
pdel_elem()
{
    Phg_args		cp_args;

    if ( CB_ENTRY_CHECK( phg_cur_cph, ERR5, Pfn_del_elem)) {

	if ( PSL_STRUCT_STATE( phg_cur_cph->psl) != PSTRUCT_ST_STOP) {
	    ERR_REPORT( phg_cur_cph->erh, ERR5);

	} else {
	    cp_args.data.del_el.op = PHG_ARGS_DEL_CURRENT;
	    CP_FUNC( phg_cur_cph, CP_FUNC_OP_DELETE_EL, &cp_args, NULL);
	}
    }
}

void
pdel_elem_range(ep1_value, ep2_value)
Pint	ep1_value;	/* element pointer 1 value	*/
Pint	ep2_value;	/* element pointer 2 value	*/
{
    Phg_args			cp_args;
    register Phg_args_del_el	*args = &cp_args.data.del_el;

    if ( CB_ENTRY_CHECK( phg_cur_cph, ERR5, Pfn_del_elem_range)) {

	if ( PSL_STRUCT_STATE( phg_cur_cph->psl) != PSTRUCT_ST_STOP) {
	    ERR_REPORT( phg_cur_cph->erh, ERR5);

	} else {
	    args->op = PHG_ARGS_DEL_RANGE;
	    args->data.ep_values.ep1 = MAX( 0, ep1_value); /* filter < 0 */
	    args->data.ep_values.ep2 = ep2_value;
	    CP_FUNC( phg_cur_cph, CP_FUNC_OP_DELETE_EL, &cp_args, NULL);
	}
    }
}

void
pdel_elems_labels( label1_id, label2_id)
    Pint	label1_id;	/* label 1 identifier	*/
    Pint	label2_id;	/* label 2 identifier	*/
{
    Phg_args			cp_args;
    register Phg_args_del_el	*args = &cp_args.data.del_el;

    if ( CB_ENTRY_CHECK( phg_cur_cph, ERR5, Pfn_del_elems_labels)) {

	if ( PSL_STRUCT_STATE( phg_cur_cph->psl) != PSTRUCT_ST_STOP) {
	    ERR_REPORT( phg_cur_cph->erh, ERR5);

	} else {
	    args->op = PHG_ARGS_DEL_LABEL;
	    args->data.label_range.label1 = label1_id;
	    args->data.label_range.label2 = label2_id;
	    CP_FUNC( phg_cur_cph, CP_FUNC_OP_DELETE_EL, &cp_args, NULL);
	}
    }
}

void
pempty_struct( struct_id)
    Pint	struct_id;	/* structure id	*/
{
    Phg_args			cp_args;
    register Phg_args_del_el	*args = &cp_args.data.del_el;

    if ( CB_ENTRY_CHECK( phg_cur_cph, ERR2, Pfn_empty_struct)) {
	args->op = PHG_ARGS_EMPTY_STRUCT;
	args->data.struct_id = struct_id;
	CP_FUNC( phg_cur_cph, CP_FUNC_OP_DELETE_EL, &cp_args, NULL);
    }
}

void
pdel_struct( struct_id)
    Pint	struct_id;	/* structure identifier	*/
{
    Phg_args			cp_args;

    if ( CB_ENTRY_CHECK( phg_cur_cph, ERR2, Pfn_del_struct)) {
	cp_args.data.idata = struct_id;
	CP_FUNC( phg_cur_cph, CP_FUNC_OP_DELETE_STRUCT, &cp_args, NULL);
    }
}

void
pdel_struct_net( struct_id, ref_flag)
    Pint	struct_id;	/* structure identifier	*/
    Pref_flag	ref_flag;	/* reference handling flag	*/
{
    Phg_args				cp_args;
    register Phg_args_del_struct_net	*args = &cp_args.data.del_struct_net;

    if ( CB_ENTRY_CHECK( phg_cur_cph, ERR2, Pfn_del_struct_net)) {
	args->id = struct_id;
	args->flag = ref_flag;
	CP_FUNC( phg_cur_cph, CP_FUNC_OP_DELETE_STRUCT_NET, &cp_args, NULL);
    }
}

void
pdel_all_structs()
{
    Phg_args	cp_args;

    if ( CB_ENTRY_CHECK( phg_cur_cph, ERR2, Pfn_del_all_struct)) {
	CP_FUNC( phg_cur_cph, CP_FUNC_OP_DELETE_ALL_STRUCTS, &cp_args, NULL);
    }
}

void
pchange_struct_id( orig_struct_id, result_struct_id)
    Pint	orig_struct_id;		/* originial structure id	*/
    Pint	result_struct_id;	/* result structure id	*/
{
    Phg_args				cp_args;
    register Phg_args_change_struct	*args = &cp_args.data.change_struct;

    if ( CB_ENTRY_CHECK( phg_cur_cph, ERR2, Pfn_change_struct_id)) {
	args->orig_id = orig_struct_id;
	args->new_id = result_struct_id;
	CP_FUNC( phg_cur_cph, CP_FUNC_OP_CHANGE_STRUCT_ID, &cp_args, NULL);
    }
}

void
pchange_struct_refs( orig_struct_id, result_struct_id)
    Pint	orig_struct_id;		/* originial structure id	*/
    Pint	result_struct_id;	/* result structure id	*/
{
    Phg_args				cp_args;
    register Phg_args_change_struct	*args = &cp_args.data.change_struct;

    if ( CB_ENTRY_CHECK( phg_cur_cph, ERR2, Pfn_change_struct_refs)) {
	args->orig_id = orig_struct_id;
	args->new_id = result_struct_id;
	CP_FUNC( phg_cur_cph, CP_FUNC_OP_CHANGE_STRUCT_REFS, &cp_args, NULL);
    }
}

void
pchange_struct_id_refs( orig_struct_id, result_struct_id)
    Pint	orig_struct_id;		/* originial structure id	*/
    Pint	result_struct_id;	/* result structure id	*/
{
    Phg_args				cp_args;
    register Phg_args_change_struct	*args = &cp_args.data.change_struct;

    if ( CB_ENTRY_CHECK( phg_cur_cph, ERR2, Pfn_change_struct_id_refs)) {
	args->orig_id = orig_struct_id;
	args->new_id = result_struct_id;
	CP_FUNC( phg_cur_cph, CP_FUNC_OP_CHANGE_STRUCT_IDREFS, &cp_args, NULL);
    }
}

void
pset_elem_ptr( ep_value)
    Pint	ep_value;	/* element pointer value	*/
{
    Phg_args				cp_args;
    register Phg_args_set_el_ptr	*args = &cp_args.data.set_el_ptr;

    if (CB_ENTRY_CHECK( phg_cur_cph, ERR5, Pfn_set_elem_ptr)) {
	if ( PSL_STRUCT_STATE( phg_cur_cph->psl) == PSTRUCT_ST_STOP) {
	    args->op = PHG_ARGS_SETEP_ABS;
	    args->data = ep_value;
	    CP_FUNC( phg_cur_cph, CP_FUNC_OP_SET_EL_PTR, &cp_args, NULL);
	} else {
	    ERR_REPORT( phg_cur_cph->erh, ERR5);
	}
    }
}

void
poffset_elem_ptr( ep_offset)
    Pint	ep_offset;	/* element pointer offset	*/
{
    Phg_args				cp_args;
    register Phg_args_set_el_ptr	*args = &cp_args.data.set_el_ptr;

    if (CB_ENTRY_CHECK( phg_cur_cph, ERR5, Pfn_offset_elem_ptr)) {
	if ( PSL_STRUCT_STATE( phg_cur_cph->psl) == PSTRUCT_ST_STOP) {
	    args->op = PHG_ARGS_SETEP_REL;
	    args->data = ep_offset;
	    CP_FUNC( phg_cur_cph, CP_FUNC_OP_SET_EL_PTR, &cp_args, NULL);
	} else {
	    ERR_REPORT( phg_cur_cph->erh, ERR5);
	}
    }
}

void
pset_elem_ptr_label( label_id)
    Pint	label_id;	/* label identifier	*/
{
    Phg_args				cp_args;
    register Phg_args_set_el_ptr	*args = &cp_args.data.set_el_ptr;

    if (CB_ENTRY_CHECK( phg_cur_cph, ERR5, Pfn_set_elem_ptr_label)) {
	if ( PSL_STRUCT_STATE( phg_cur_cph->psl) == PSTRUCT_ST_STOP) {
	    args->op = PHG_ARGS_SETEP_LABEL;
	    args->data = label_id;
	    CP_FUNC( phg_cur_cph, CP_FUNC_OP_SET_EL_PTR, &cp_args, NULL);
	} else {
	    ERR_REPORT( phg_cur_cph->erh, ERR5);
	}
    }
}

void
pset_edit_mode( mode)
    Pedit_mode	mode;	/* edit mode	*/
{
    Phg_args	cp_args;

    if ( CB_ENTRY_CHECK( phg_cur_cph, ERR2, Pfn_set_edit_mode)) {
	cp_args.data.idata = (Pint)mode;
	CP_FUNC( phg_cur_cph, CP_FUNC_OP_SET_EDIT_MODE, &cp_args, NULL);
	phg_cur_cph->psl->edit_mode = mode;
    }
}


/* Inquiry functions */

void
pinq_elem_ptr( error_ind, ep_value)
    Pint	*error_ind;	/* OUT error indicator	*/
    Pint	*ep_value;	/* OUT element pointer value	*/
{
    Phg_args	cp_args;
    Phg_ret	ret;

    if ( !CB_ENTRY_CHECK( phg_cur_cph, 0, Pfn_INQUIRY)) {
	*error_ind = ERR5;

    } else if ( PSL_STRUCT_STATE( phg_cur_cph->psl) != PSTRUCT_ST_STOP) {
	*error_ind = ERR5;

    } else {
	ret.err = 0;
	CP_FUNC( phg_cur_cph, CP_FUNC_OP_INQ_EL_PTR, &cp_args, &ret);
	if ( ret.err) {
	    *error_ind = ret.err;
	} else {
	    *error_ind = 0;
	    *ep_value = ret.data.idata;
	}
    }
}

void
pinq_edit_mode( error_ind, edit_mode)
    Pint	*error_ind;	/* OUT error indicator	*/
    Pedit_mode	*edit_mode;	/* OUT edit mode	*/
{
    if ( !CB_ENTRY_CHECK( phg_cur_cph, 0, Pfn_INQUIRY)) {
	*error_ind = ERR2;

    } else {
	*error_ind = 0;
	*edit_mode = phg_cur_cph->psl->edit_mode;
    }
}
