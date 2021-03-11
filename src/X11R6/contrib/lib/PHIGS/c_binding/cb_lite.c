/* $XConsortium: cb_lite.c,v 5.4 94/04/17 20:40:50 mor Exp $ */

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


/* Lighting functions for the PHIGS C binding */

#include "phg.h"
#include "cp.h"
#include "cb_priv.h"

/* SET INTERIOR REFLECTANCE EQUATION */
void
pset_refl_eqn(equation)
Pint	equation;	/* reflectance equation */
{
    Phg_args		cp_args;
    Phg_el_data		ed;
    Phg_args_add_el	*args = &cp_args.data.add_el;

    if (CB_ENTRY_CHECK(phg_cur_cph, ERR5, Pfn_set_refl_eqn)) {
    	if (PSL_STRUCT_STATE(phg_cur_cph->psl) != PSTRUCT_ST_STOP) {
	    ERR_REPORT(phg_cur_cph->erh, ERR5);

	} else {
	    args->el_type = PELEM_INT_REFL_EQN;
	    ed.idata = equation;
	    if ( CB_BUILD_OC(args->el_type, &ed, &args->pex_oc) )
		CP_FUNC( phg_cur_cph, CP_FUNC_OP_ADD_EL, &cp_args, NULL);
	}
    }
}


/* SET BACK INTERIOR REFLECTANCE EQUATION */
void
pset_back_refl_eqn(equation)
Pint	equation;	/* back reflectance equation */
{
    Phg_args		cp_args;
    Phg_el_data		ed;
    Phg_args_add_el	*args = &cp_args.data.add_el;

    if (CB_ENTRY_CHECK(phg_cur_cph, ERR5, Pfn_set_back_refl_eqn)) {
    	if (PSL_STRUCT_STATE(phg_cur_cph->psl) != PSTRUCT_ST_STOP) {
	    ERR_REPORT(phg_cur_cph->erh, ERR5);

	} else {
	    args->el_type = PELEM_BACK_INT_REFL_EQN;
	    ed.idata = equation;
	    if ( CB_BUILD_OC(args->el_type, &ed, &args->pex_oc) )
		CP_FUNC( phg_cur_cph, CP_FUNC_OP_ADD_EL, &cp_args, NULL);
	}
    }
}


static int
list_indices_valid( activation, deactivation )
    Pint_list	*activation;
    Pint_list	*deactivation;
{
    register	int	i, j;
    register	Pint	*alst = activation->ints;
    register	Pint	*dlst = deactivation->ints;

    /* Check for invalid index. */
    for ( i = 0; i < activation->num_ints; i++, alst++ )
	if ( *alst < 1 )
	    return ERR133;
    for ( j = 0; j < deactivation->num_ints; j++, dlst++ )
	if ( *dlst < 1 )
	    return ERR133;

    /* Check for common index. */
    alst = activation->ints;
     for ( i = 0; i < activation->num_ints; i++, alst++ ) {
        dlst = deactivation->ints;
	for ( j = 0; j < deactivation->num_ints; j++, dlst++ )
	    if ( *alst == *dlst )
		return ERR135;
    }
		
    return 0;
}

/* SET LIGHT SOURCE STATE */
void
pset_light_src_state(activation, deactivation)
Pint_list	 *activation;	/* activation list */
Pint_list	 *deactivation;	/* deactivation list */
{
    int			err;
    Phg_args		cp_args;
    Phg_el_data		ed;
    Phg_args_add_el	*args = &cp_args.data.add_el;

    if (CB_ENTRY_CHECK(phg_cur_cph, ERR5, Pfn_set_light_src_state)) {
    	if (PSL_STRUCT_STATE(phg_cur_cph->psl) != PSTRUCT_ST_STOP) {
	    ERR_REPORT(phg_cur_cph->erh, ERR5);

	} else if ( err = list_indices_valid( activation, deactivation ) ) {
	    ERR_REPORT(phg_cur_cph->erh, err);

	} else {
	    args->el_type = PELEM_LIGHT_SRC_STATE;
	    ed.light_state.act_set = *activation;
	    ed.light_state.deact_set = *deactivation;
	    if ( CB_BUILD_OC(args->el_type, &ed, &args->pex_oc) )
		CP_FUNC( phg_cur_cph, CP_FUNC_OP_ADD_EL, &cp_args, NULL);
	}
    }
}

/* SET LIGHT SOURCE REPRESENTATION */
void
pset_light_src_rep(ws, index, rep)
Pint	        	ws;	/* workstation identifier */
Pint	        	index;	/* light table index */
Plight_src_bundle	*rep;	/* light source representation */	
{
    Phg_args			cp_args;
    register Phg_args_set_rep	*args = &cp_args.data.set_rep;
    register Wst_phigs_dt	*dt;
    Psl_ws_info			*wsinfo;
    

    if (CB_ENTRY_CHECK(phg_cur_cph, ERR3, Pfn_set_light_src_rep)) {
    	if (PSL_WS_STATE(phg_cur_cph->psl) != PWS_ST_WSOP) {
	    ERR_REPORT(phg_cur_cph->erh, ERR3);
	    return;

	} else if (!(wsinfo = phg_psl_get_ws_info( phg_cur_cph->psl, ws))) {
	    ERR_REPORT(phg_cur_cph->erh, ERR54);
	    return;
	}

	dt = &((Wst*)wsinfo->wstype)->desc_tbl.phigs_dt;
	if ( !( dt->ws_category == PCAT_OUTIN || dt->ws_category == PCAT_OUT
	    || dt->ws_category == PCAT_MO) ) {
	    ERR_REPORT(phg_cur_cph->erh, ERR59);

	} else if (index < 1) {
	    ERR_REPORT(phg_cur_cph->erh, ERR129);

	} else if (!CB_LIGHT_SRC_TYPE_SUPPORTED(rep->type)) {
	    ERR_REPORT(phg_cur_cph->erh, ERR131);

	} else if ( (rep->type == PLIGHT_SPOT) && ((rep->rec.spot.angle < 0)
	    || (rep->rec.spot.angle > M_PI)) ) {
	    ERR_REPORT(phg_cur_cph->erh, ERR132);

	} else {
	    args->wsid = ws;
	    args->type = PHG_ARGS_LIGHTSRCREP;
	    args->rep.index = index;
	    args->rep.bundl.lightsrcrep = *rep;
	    CP_FUNC( phg_cur_cph, CP_FUNC_OP_SET_REP, &cp_args, NULL);
	}
    }
}


/* INQUIRE LIGHT SOURCE REPRESENTATION */
void
pinq_light_src_rep( ws, index, type, error_ind, rep)
    Pint	 ws;		/* workstation identifier	*/
    Pint	 index;		/* light source index	*/
    Pinq_type	 type;		/* type of returned value	*/
    Pint	*error_ind;	/* OUT error indicator	*/
    Plight_src_bundle	*rep;		/* OUT depth cue representation	*/
{
    Phg_args		cp_args;
    Phg_ret		ret;
    Psl_ws_info		*wsinfo;
    Wst_phigs_dt	*dt;

    if ( !CB_ENTRY_CHECK( phg_cur_cph, 0, Pfn_INQUIRY)) {
	*error_ind = ERR3;

    } else if ( PSL_WS_STATE( phg_cur_cph->psl) != PWS_ST_WSOP) {
	*error_ind = ERR3;

    } else if ( !(wsinfo = phg_psl_get_ws_info( phg_cur_cph->psl, ws))) {
	*error_ind = ERR54;
    } else {
	*error_ind = 0;
	dt = &((Wst*)wsinfo->wstype)->desc_tbl.phigs_dt;
	if ( !(dt->ws_category == PCAT_OUT || 
	       dt->ws_category == PCAT_OUTIN ||
	       dt->ws_category == PCAT_MO) ) {
	    *error_ind = ERR59;

	} else if ( index < 1) {
	    *error_ind = ERR129;
	}

	if (!*error_ind) {
	    cp_args.data.q_rep.wsid = ws;
	    cp_args.data.q_rep.index = index;
	    cp_args.data.q_rep.type = type;
	    cp_args.data.q_rep.rep_type = PHG_ARGS_LIGHTSRCREP;
	    CP_FUNC( phg_cur_cph, CP_FUNC_OP_INQ_REPRESENTATION, &cp_args,
			&ret);
	    if ( ret.err) {
		*error_ind = ret.err;
	    } else {
		*rep = ret.data.rep.lightsrcrep;
	    }
	}
    }
}
