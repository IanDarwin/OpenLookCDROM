/* $XConsortium: cb_text.c,v 5.2 94/04/17 20:40:56 rws Exp $ */

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

/* Text functions for the PHIGS C binding */

/* 
 * ANNOTATION TEXT RELATIVE
 * ANNOTATION TEXT RELATIVE 3 
 * INQUIRE LIST OF TEXT INDICES 
 * SET ANNOTATION TEXT ALIGNMENT 
 * SET ANNOTATION TEXT CHARACTER HEIGHT 
 * SET ANNOTATION TEXT CHARACTER UP VECTOR 
 * SET ANNOTATION TEXT PATH 
 * SET ANNOTATION STYLE
 * SET CHARACTER EXPANSION FACTOR 
 * SET CHARACTER HEIGHT 
 * SET CHARACTER SPACING 
 * SET CHARACTER UP VECTOR 
 * SET TEXT ALIGNMENT 
 * SET TEXT COLOUR INDEX 
 * SET TEXT FONT 
 * SET TEXT INDEX 
 * SET TEXT PATH 
 * SET TEXT PRECISION 
 * TEXT 
 * TEXT 3 
 */


#include "phg.h"
#include "cp.h"
#include "cb_priv.h"




void
ptext3(text_pt, dir, text)
Ppoint3		*text_pt;	/* text point	*/
Pvec3   	dir[2];		/* directon vectors	*/
char		*text;		/* text string	*/
{
    Phg_args		cp_args;
    Phg_el_data		ed;
    Phg_args_add_el	*args = &cp_args.data.add_el;

    if (CB_ENTRY_CHECK(phg_cur_cph, ERR5, Pfn_text3)) {
	if (PSL_STRUCT_STATE(phg_cur_cph->psl) != PSTRUCT_ST_STOP) {
	    ERR_REPORT(phg_cur_cph->erh, ERR5);	
	}
	else {
	    args->el_type = PELEM_TEXT3;
	    ed.text3.pt      = *text_pt;
	    ed.text3.dir[0]  = dir[0];
	    ed.text3.dir[1]  = dir[1];
	    if ( (ed.text3.string  = text))
		ed.text3.length = strlen(text) + 1;
	    else
		ed.text3.length = 0;
	    if ( CB_BUILD_OC(args->el_type, &ed, &args->pex_oc) )
		CP_FUNC( phg_cur_cph, CP_FUNC_OP_ADD_EL, &cp_args, NULL);
	}
    }
}

void
ptext(text_pt, text)
Ppoint	*text_pt;	/* text point	*/
char	*text;		/* text string	*/
{
    Phg_args		cp_args;
    Phg_el_data		ed;
    Phg_args_add_el	*args = &cp_args.data.add_el;

    if (CB_ENTRY_CHECK(phg_cur_cph, ERR5, Pfn_text)) {
	if (PSL_STRUCT_STATE(phg_cur_cph->psl) != PSTRUCT_ST_STOP) {
	    ERR_REPORT(phg_cur_cph->erh, ERR5);
	}
	else {
	    args->el_type = PELEM_TEXT;
	    ed.text.pt = *text_pt;
	    if ( (ed.text.string  = text))
		ed.text.length = strlen(text) + 1;
	    else
		ed.text.length = 0;
	    if ( CB_BUILD_OC(args->el_type, &ed, &args->pex_oc) )
		CP_FUNC( phg_cur_cph, CP_FUNC_OP_ADD_EL, &cp_args, NULL);
	}
    }
}

void
pset_text_font(font)
Pint	font;	/* text font	*/
{
    Phg_args		cp_args;
    Phg_el_data		ed;
    Phg_args_add_el	*args = &cp_args.data.add_el;

    if (CB_ENTRY_CHECK(phg_cur_cph, ERR5, Pfn_set_text_font)) {
	if (PSL_STRUCT_STATE(phg_cur_cph->psl) != PSTRUCT_ST_STOP) {
	    ERR_REPORT(phg_cur_cph->erh, ERR5);
	}
	else {
	    args->el_type = PELEM_TEXT_FONT;
	    ed.idata = font;
	    if ( CB_BUILD_OC(args->el_type, &ed, &args->pex_oc) )
		CP_FUNC( phg_cur_cph, CP_FUNC_OP_ADD_EL, &cp_args, NULL);
	}
    }
}

void
pset_text_prec(precision)
Ptext_prec	precision;	/* text precision	*/
{
    Phg_args		cp_args;
    Phg_el_data		ed;
    Phg_args_add_el	*args = &cp_args.data.add_el;

    if (CB_ENTRY_CHECK(phg_cur_cph, ERR5, Pfn_set_text_prec)) {
	if (PSL_STRUCT_STATE(phg_cur_cph->psl) != PSTRUCT_ST_STOP) {
	    ERR_REPORT(phg_cur_cph->erh, ERR5);
	}
	else {
	    args->el_type = PELEM_TEXT_PREC;
	    ed.txprec = precision;
	    if ( CB_BUILD_OC(args->el_type, &ed, &args->pex_oc) )
		CP_FUNC( phg_cur_cph, CP_FUNC_OP_ADD_EL, &cp_args, NULL);
	}
    }
}

void
pset_char_expan(exp_factor)
Pfloat	exp_factor;	/* character expansion factor	*/
{
    Phg_args		cp_args;
    Phg_el_data		ed;
    Phg_args_add_el	*args = &cp_args.data.add_el;

    if (CB_ENTRY_CHECK(phg_cur_cph, ERR5, Pfn_set_char_expan)) {
	if (PSL_STRUCT_STATE(phg_cur_cph->psl) != PSTRUCT_ST_STOP) {
	    ERR_REPORT(phg_cur_cph->erh, ERR5);
	}
	else {
	    args->el_type = PELEM_CHAR_EXPAN;
	    ed.fdata = exp_factor;
	    if ( CB_BUILD_OC(args->el_type, &ed, &args->pex_oc) )
		CP_FUNC( phg_cur_cph, CP_FUNC_OP_ADD_EL, &cp_args, NULL);
	}
    }
}



void
pset_char_space(spacing)
Pfloat	spacing;	/* character spacing	*/
{
    Phg_args		cp_args;
    Phg_el_data		ed;
    Phg_args_add_el	*args = &cp_args.data.add_el;

    if (CB_ENTRY_CHECK(phg_cur_cph, ERR5, Pfn_set_char_space)) {
	if (PSL_STRUCT_STATE(phg_cur_cph->psl) != PSTRUCT_ST_STOP) {
	    ERR_REPORT(phg_cur_cph->erh, ERR5);
	}
	else {
	    args->el_type = PELEM_CHAR_SPACE;
	    ed.fdata = spacing;
	    if ( CB_BUILD_OC(args->el_type, &ed, &args->pex_oc) )
		CP_FUNC( phg_cur_cph, CP_FUNC_OP_ADD_EL, &cp_args, NULL);
	}
    }
}

void
pset_text_colr_ind(colour)
Pint	colour;	/* text colour index	*/
{
    Phg_args		cp_args;
    Phg_el_data		ed;
    Phg_args_add_el	*args = &cp_args.data.add_el;

    if (CB_ENTRY_CHECK(phg_cur_cph, ERR5, Pfn_set_text_colr_ind)) {
	if (PSL_STRUCT_STATE(phg_cur_cph->psl) != PSTRUCT_ST_STOP) {
	    ERR_REPORT(phg_cur_cph->erh, ERR5);
	}
	else if (colour < 0) {
	    ERR_REPORT(phg_cur_cph->erh, ERR113);
	}
	else {
	    args->el_type = PELEM_TEXT_COLR_IND;
	    ed.idata = colour;
	    if ( CB_BUILD_OC(args->el_type, &ed, &args->pex_oc) )
		CP_FUNC( phg_cur_cph, CP_FUNC_OP_ADD_EL, &cp_args, NULL);
	}
    }
}

void
pset_text_colr(colour)
Pgcolr	*colour;	/* text colour */
{
    Phg_args		cp_args;
    Phg_el_data		ed;
    Phg_args_add_el	*args = &cp_args.data.add_el;

    if (CB_ENTRY_CHECK(phg_cur_cph, ERR5, Pfn_set_text_colr)) {
	if (PSL_STRUCT_STATE(phg_cur_cph->psl) != PSTRUCT_ST_STOP) {
	    ERR_REPORT(phg_cur_cph->erh, ERR5);
	}
	else {
	    args->el_type = PELEM_TEXT_COLR;
	    ed.colour = *colour;
	    if ( CB_BUILD_OC(args->el_type, &ed, &args->pex_oc) )
		CP_FUNC( phg_cur_cph, CP_FUNC_OP_ADD_EL, &cp_args, NULL);
	}
    }
}

void
pset_char_ht(height)
Pfloat	height;	/* character height	*/
{
    Phg_args		cp_args;
    Phg_el_data		ed;
    Phg_args_add_el	*args = &cp_args.data.add_el;

    if (CB_ENTRY_CHECK(phg_cur_cph, ERR5, Pfn_char_ht)) {
	if (PSL_STRUCT_STATE(phg_cur_cph->psl) != PSTRUCT_ST_STOP) {
	    ERR_REPORT(phg_cur_cph->erh, ERR5);
	}
	else {
	    args->el_type = PELEM_CHAR_HT;
	    ed.fdata = height;
	    if ( CB_BUILD_OC(args->el_type, &ed, &args->pex_oc) )
		CP_FUNC( phg_cur_cph, CP_FUNC_OP_ADD_EL, &cp_args, NULL);
	}
    }
}



void
pset_char_up_vec(up_vect)
Pvec	*up_vect;	/* character up vector	*/
{
    Phg_args		cp_args;
    Phg_el_data		ed;
    Phg_args_add_el	*args = &cp_args.data.add_el;

    if (CB_ENTRY_CHECK(phg_cur_cph, ERR5, Pfn_char_up_vec)) {
	if (PSL_STRUCT_STATE(phg_cur_cph->psl) != PSTRUCT_ST_STOP) {
	    ERR_REPORT(phg_cur_cph->erh, ERR5);
	}
	else {
	    args->el_type = PELEM_CHAR_UP_VEC;
	    ed.char_up = *up_vect;
	    if ( CB_BUILD_OC(args->el_type, &ed, &args->pex_oc) )
		CP_FUNC( phg_cur_cph, CP_FUNC_OP_ADD_EL, &cp_args, NULL);
	}
    }
}

void
pset_text_path(path)
Ptext_path	path;	/* text path	*/
{
    Phg_args		cp_args;
    Phg_el_data		ed;
    Phg_args_add_el	*args = &cp_args.data.add_el;

    if (CB_ENTRY_CHECK(phg_cur_cph, ERR5, Pfn_set_text_path)) {
	if (PSL_STRUCT_STATE(phg_cur_cph->psl) != PSTRUCT_ST_STOP) {
	    ERR_REPORT(phg_cur_cph->erh, ERR5);
	}
	else {
	    args->el_type = PELEM_TEXT_PATH;
	    ed.txpath = path;
	    if ( CB_BUILD_OC(args->el_type, &ed, &args->pex_oc) )
		CP_FUNC( phg_cur_cph, CP_FUNC_OP_ADD_EL, &cp_args, NULL);
	}
    }
}

void
pset_text_align(text_align)
Ptext_align	*text_align;	/* text alignment	*/
{
    Phg_args		cp_args;
    Phg_el_data		ed;
    Phg_args_add_el	*args = &cp_args.data.add_el;

    if (CB_ENTRY_CHECK(phg_cur_cph, ERR5, Pfn_set_text_align)) {
	if (PSL_STRUCT_STATE(phg_cur_cph->psl) != PSTRUCT_ST_STOP) {
	    ERR_REPORT(phg_cur_cph->erh, ERR5);
	}
	else {
	    args->el_type = PELEM_TEXT_ALIGN;
	    ed.txalign = *text_align;
	    if ( CB_BUILD_OC(args->el_type, &ed, &args->pex_oc) )
		CP_FUNC( phg_cur_cph, CP_FUNC_OP_ADD_EL, &cp_args, NULL);
	}
    }
}


void
pset_text_ind(index)
Pint	index;	/* text index	*/
{   
    Phg_args		cp_args;
    Phg_el_data		ed;
    Phg_args_add_el	*args = &cp_args.data.add_el;

    if (CB_ENTRY_CHECK(phg_cur_cph, ERR5, Pfn_set_text_ind)) {
	if (PSL_STRUCT_STATE(phg_cur_cph->psl) != PSTRUCT_ST_STOP) {
	    ERR_REPORT(phg_cur_cph->erh, ERR5);
	}
	else if (index < 1) {
	    ERR_REPORT(phg_cur_cph->erh, ERR100);
	}
	else {
	    args->el_type = PELEM_TEXT_IND;
	    ed.idata = index;
	    if ( CB_BUILD_OC(args->el_type, &ed, &args->pex_oc) )
		CP_FUNC( phg_cur_cph, CP_FUNC_OP_ADD_EL, &cp_args, NULL);
	}
    }
}


int
phg_cb_valid_text_pair( font, prec, num, list)
    Pint		font;
    Ptext_prec		prec;
    register int	num;
    register Ptext_font_prec	*list;
{
    while ( num--) {
	if ( list->font == font && list->prec == prec)
	    return 1;
	else
	    ++list;
    }
    return 0;
}

void
panno_text_rel3( ref_pt, anno_offset, text )
    Ppoint3	*ref_pt;	/* reference point	*/
    Pvec3	*anno_offset;	/* annotation offset	*/
    char	*text;		/* annotation text string	*/
{
    Phg_args		cp_args;
    Phg_el_data		ed;
    Phg_args_add_el	*args = &cp_args.data.add_el;

    if (CB_ENTRY_CHECK(phg_cur_cph, ERR5, Pfn_anno_text_rel3)) {
	if (PSL_STRUCT_STATE(phg_cur_cph->psl) != PSTRUCT_ST_STOP) {
	    ERR_REPORT(phg_cur_cph->erh, ERR5);	

	} else {
	    args->el_type = PELEM_ANNO_TEXT_REL3;
	    ed.anno_text_rel3.ref_pt = *ref_pt;
	    ed.anno_text_rel3.offset.x = anno_offset->delta_x;
	    ed.anno_text_rel3.offset.y = anno_offset->delta_y;
	    ed.anno_text_rel3.offset.z = anno_offset->delta_z;
	    if ( (ed.anno_text_rel3.string  = text))
		ed.anno_text_rel3.length = strlen(text) + 1;
	    else
		ed.anno_text_rel3.length = 0;
	    if ( CB_BUILD_OC(args->el_type, &ed, &args->pex_oc) )
		CP_FUNC( phg_cur_cph, CP_FUNC_OP_ADD_EL, &cp_args, NULL);
	}
    }
}

void
panno_text_rel( ref_pt, anno_offset, text )
    Ppoint	*ref_pt;	/* reference point	*/
    Pvec	*anno_offset;	/* annotation offset	*/
    char	*text;		/* annotation text string	*/
{
    Phg_args		cp_args;
    Phg_el_data		ed;
    Phg_args_add_el	*args = &cp_args.data.add_el;

    if (CB_ENTRY_CHECK(phg_cur_cph, ERR5, Pfn_anno_text_rel)) {
	if (PSL_STRUCT_STATE(phg_cur_cph->psl) != PSTRUCT_ST_STOP) {
	    ERR_REPORT(phg_cur_cph->erh, ERR5);	

	} else {
	    args->el_type = PELEM_ANNO_TEXT_REL;
	    ed.anno_text_rel.ref_pt = *ref_pt;
	    ed.anno_text_rel.offset.x = anno_offset->delta_x;
	    ed.anno_text_rel.offset.y = anno_offset->delta_y;
	    if ( (ed.anno_text_rel.string  = text))
		ed.anno_text_rel.length = strlen(text) + 1;
	    else
		ed.anno_text_rel.length = 0;
	    if ( CB_BUILD_OC(args->el_type, &ed, &args->pex_oc) )
		CP_FUNC( phg_cur_cph, CP_FUNC_OP_ADD_EL, &cp_args, NULL);
	}
    }
}

void
pset_anno_char_ht(height)
Pfloat	height;	/* character height	*/
{
    Phg_args		cp_args;
    Phg_el_data		ed;
    Phg_args_add_el	*args = &cp_args.data.add_el;

    if (CB_ENTRY_CHECK(phg_cur_cph, ERR5, Pfn_set_anno_char_ht)) {
	if (PSL_STRUCT_STATE(phg_cur_cph->psl) != PSTRUCT_ST_STOP) {
	    ERR_REPORT(phg_cur_cph->erh, ERR5);
	}
	else {
	    args->el_type = PELEM_ANNO_CHAR_HT;
	    ed.fdata = height;
	    if ( CB_BUILD_OC(args->el_type, &ed, &args->pex_oc) )
		CP_FUNC( phg_cur_cph, CP_FUNC_OP_ADD_EL, &cp_args, NULL);
	}
    }
}

void
pset_anno_char_up_vec(up_vect)
Pvec	*up_vect;	/* character up vector	*/
{
    Phg_args		cp_args;
    Phg_el_data		ed;
    Phg_args_add_el	*args = &cp_args.data.add_el;

    if (CB_ENTRY_CHECK(phg_cur_cph, ERR5, Pfn_set_anno_char_up_vec)) {
	if (PSL_STRUCT_STATE(phg_cur_cph->psl) != PSTRUCT_ST_STOP) {
	    ERR_REPORT(phg_cur_cph->erh, ERR5);
	}
	else {
	    args->el_type = PELEM_ANNO_CHAR_UP_VEC;
	    ed.char_up = *up_vect;
	    if ( CB_BUILD_OC(args->el_type, &ed, &args->pex_oc) )
		CP_FUNC( phg_cur_cph, CP_FUNC_OP_ADD_EL, &cp_args, NULL);
	}
    }
}

void
pset_anno_path(path)
Ptext_path	path;	/* text path	*/
{
    Phg_args		cp_args;
    Phg_el_data		ed;
    Phg_args_add_el	*args = &cp_args.data.add_el;

    if (CB_ENTRY_CHECK(phg_cur_cph, ERR5, Pfn_set_anno_path)) {
	if (PSL_STRUCT_STATE(phg_cur_cph->psl) != PSTRUCT_ST_STOP) {
	    ERR_REPORT(phg_cur_cph->erh, ERR5);
	}
	else {
	    args->el_type = PELEM_ANNO_PATH;
	    ed.txpath = path;
	    if ( CB_BUILD_OC(args->el_type, &ed, &args->pex_oc) )
		CP_FUNC( phg_cur_cph, CP_FUNC_OP_ADD_EL, &cp_args, NULL);
	}
    }
}

void
pset_anno_align(text_align)
Ptext_align	*text_align;	/* text alignment	*/
{
    Phg_args		cp_args;
    Phg_el_data		ed;
    Phg_args_add_el	*args = &cp_args.data.add_el;

    if (CB_ENTRY_CHECK(phg_cur_cph, ERR5, Pfn_set_anno_align)) {
	if (PSL_STRUCT_STATE(phg_cur_cph->psl) != PSTRUCT_ST_STOP) {
	    ERR_REPORT(phg_cur_cph->erh, ERR5);
	}
	else {
	    args->el_type = PELEM_ANNO_ALIGN;
	    ed.txalign = *text_align;
	    if ( CB_BUILD_OC(args->el_type, &ed, &args->pex_oc) )
		CP_FUNC( phg_cur_cph, CP_FUNC_OP_ADD_EL, &cp_args, NULL);
	}
    }
}

void
pset_anno_style(style)
Pint	style;	/* annotation style	*/
{
    Phg_args		cp_args;
    Phg_el_data		ed;
    Phg_args_add_el	*args = &cp_args.data.add_el;

    if (CB_ENTRY_CHECK(phg_cur_cph, ERR5, Pfn_set_anno_style)) {
	if (PSL_STRUCT_STATE(phg_cur_cph->psl) != PSTRUCT_ST_STOP) {
	    ERR_REPORT(phg_cur_cph->erh, ERR5);
	}
	else {
	    args->el_type = PELEM_ANNO_STYLE;
	    ed.idata = style;
	    if ( CB_BUILD_OC(args->el_type, &ed, &args->pex_oc) )
		CP_FUNC( phg_cur_cph, CP_FUNC_OP_ADD_EL, &cp_args, NULL);
	}
    }
}
