/* $XConsortium: css_ini.c,v 5.2 94/04/17 20:42:40 rws Exp $ */

/***********************************************************

Copyright (c) 1989,1990, 1991  X Consortium

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

Copyright (c) 1989,1990, 1991 by Sun Microsystems, Inc.

                        All Rights Reserved

Permission to use, copy, modify, and distribute this software and its 
documentation for any purpose and without fee is hereby granted, 
provided that the above copyright notice appear in all copies and that
both that copyright notice and this permission notice appear in 
supporting documentation, and that the names of Sun Microsystems,
and the X Consortium, not be used in advertising or publicity 
pertaining to distribution of the software without specific, written 
prior permission.  

SUN MICROSYSTEMS DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, 
INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT 
SHALL SUN MICROSYSTEMS BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL 
DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,
WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION,
ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS
SOFTWARE.

******************************************************************/

/* Initialise css */

#include "phg.h"
#include "cp.h"
#include "ws.h"
#include "css.h"
#include "css_priv.h"

#define CSS_STAB_SIZE		1009
#define CHOICE_POPUP_STAB_SIZE	5

/*******************

    phg_css_init - initialise css data, tables, etc.

*******************/

Css_handle
phg_css_init(erh, ssh_type)
Err_handle	erh;
Css_ssh_type	ssh_type;
{
    register Css_handle	cssh;
    register Css_func	*fptr;
   
    if ( !(cssh = (Css_handle) calloc((unsigned)1,sizeof(Css_struct))) )
        return(NULL);					/* out of memory */
    fptr = cssh->el_funcs;
    fptr[(int)PELEM_NIL] = phg_css_no_data;
    fptr[(int)PELEM_POLYLINE3] = phg_css_pex_oc;
    fptr[(int)PELEM_POLYLINE] = phg_css_pex_oc;
    fptr[(int)PELEM_POLYMARKER3] = phg_css_pex_oc;
    fptr[(int)PELEM_POLYMARKER] = phg_css_pex_oc;
    fptr[(int)PELEM_TEXT3] = phg_css_pex_oc;
    fptr[(int)PELEM_TEXT] = phg_css_pex_oc;
    fptr[(int)PELEM_ANNO_TEXT_REL3] = phg_css_pex_oc;
    fptr[(int)PELEM_ANNO_TEXT_REL] = phg_css_pex_oc;
    fptr[(int)PELEM_FILL_AREA3] = phg_css_pex_oc;
    fptr[(int)PELEM_FILL_AREA] = phg_css_pex_oc;
    fptr[(int)PELEM_FILL_AREA_SET3] = phg_css_pex_oc;
    fptr[(int)PELEM_FILL_AREA_SET] = phg_css_pex_oc;
    fptr[(int)PELEM_CELL_ARRAY3] = phg_css_pex_oc;
    fptr[(int)PELEM_CELL_ARRAY] = phg_css_pex_oc;
    fptr[(int)PELEM_GDP3] = phg_css_pex_oc;
    fptr[(int)PELEM_GDP] = phg_css_pex_oc;
    fptr[(int)PELEM_LINE_IND] = phg_css_pex_oc;
    fptr[(int)PELEM_MARKER_IND] = phg_css_pex_oc;
    fptr[(int)PELEM_TEXT_IND] = phg_css_pex_oc;
    fptr[(int)PELEM_INT_IND] = phg_css_pex_oc;
    fptr[(int)PELEM_EDGE_IND] = phg_css_pex_oc;
    fptr[(int)PELEM_LINETYPE] = phg_css_pex_oc;
    fptr[(int)PELEM_LINEWIDTH] = phg_css_pex_oc;
    fptr[(int)PELEM_LINE_COLR_IND] = phg_css_pex_oc;
    fptr[(int)PELEM_MARKER_TYPE] = phg_css_pex_oc;
    fptr[(int)PELEM_MARKER_SIZE] = phg_css_pex_oc;
    fptr[(int)PELEM_MARKER_COLR_IND] = phg_css_pex_oc;
    fptr[(int)PELEM_TEXT_FONT] = phg_css_pex_oc;
    fptr[(int)PELEM_TEXT_PREC] = phg_css_pex_oc;
    fptr[(int)PELEM_CHAR_EXPAN] = phg_css_pex_oc;
    fptr[(int)PELEM_CHAR_SPACE] = phg_css_pex_oc; 
    fptr[(int)PELEM_TEXT_COLR_IND] = phg_css_pex_oc;
    fptr[(int)PELEM_CHAR_HT] = phg_css_pex_oc;
    fptr[(int)PELEM_CHAR_UP_VEC] = phg_css_pex_oc;
    fptr[(int)PELEM_TEXT_PATH] = phg_css_pex_oc;
    fptr[(int)PELEM_TEXT_ALIGN] = phg_css_pex_oc;
    fptr[(int)PELEM_ANNO_CHAR_HT] = phg_css_pex_oc;
    fptr[(int)PELEM_ANNO_CHAR_UP_VEC] = phg_css_pex_oc;
    fptr[(int)PELEM_ANNO_PATH] = phg_css_pex_oc;
    fptr[(int)PELEM_ANNO_ALIGN] = phg_css_pex_oc;
    fptr[(int)PELEM_ANNO_STYLE] = phg_css_pex_oc;
    fptr[(int)PELEM_INT_STYLE] = phg_css_pex_oc;
    fptr[(int)PELEM_BACK_INT_STYLE] = phg_css_pex_oc;
    fptr[(int)PELEM_INT_STYLE_IND] = phg_css_pex_oc;
    fptr[(int)PELEM_BACK_INT_STYLE_IND] = phg_css_pex_oc;
    fptr[(int)PELEM_INT_COLR_IND] = phg_css_pex_oc;
    fptr[(int)PELEM_EDGE_FLAG] = phg_css_pex_oc;
    fptr[(int)PELEM_EDGETYPE] = phg_css_pex_oc;
    fptr[(int)PELEM_EDGEWIDTH] = phg_css_pex_oc;
    fptr[(int)PELEM_EDGE_COLR_IND] = phg_css_pex_oc;
    fptr[(int)PELEM_PAT_SIZE] = phg_css_pex_oc;
    fptr[(int)PELEM_PAT_REF_POINT_VECS] = phg_css_pex_oc;
    fptr[(int)PELEM_PAT_REF_POINT] = phg_css_pex_oc;
    fptr[(int)PELEM_ADD_NAMES_SET] = phg_css_pex_oc;
    fptr[(int)PELEM_REMOVE_NAMES_SET] = phg_css_pex_oc;
    fptr[(int)PELEM_INDIV_ASF] = phg_css_pex_oc;
    fptr[(int)PELEM_HLHSR_ID] = phg_css_pex_oc;
    fptr[(int)PELEM_LOCAL_MODEL_TRAN3] = phg_css_pex_oc;
    fptr[(int)PELEM_LOCAL_MODEL_TRAN] = phg_css_pex_oc;
    fptr[(int)PELEM_GLOBAL_MODEL_TRAN3] = phg_css_pex_oc;
    fptr[(int)PELEM_GLOBAL_MODEL_TRAN] = phg_css_pex_oc;
    fptr[(int)PELEM_MODEL_CLIP_VOL3] = phg_css_pex_oc;
    fptr[(int)PELEM_MODEL_CLIP_VOL] = phg_css_pex_oc;
    fptr[(int)PELEM_MODEL_CLIP_IND] = phg_css_pex_oc;
    fptr[(int)PELEM_RESTORE_MODEL_CLIP_VOL] = phg_css_pex_oc;
    fptr[(int)PELEM_VIEW_IND] = phg_css_pex_oc;
    fptr[(int)PELEM_EXEC_STRUCT] = phg_css_struct_ref;
    fptr[(int)PELEM_LABEL] = phg_css_pex_oc;
    fptr[(int)PELEM_APPL_DATA] = phg_css_pex_oc;
    fptr[(int)PELEM_GSE] = phg_css_pex_oc;
    fptr[(int)PELEM_PICK_ID] = phg_css_pex_oc;

    fptr[(int)PELEM_POLYLINE_SET3_DATA] = phg_css_pex_oc;
    fptr[(int)PELEM_FILL_AREA_SET3_DATA] = phg_css_pex_oc;
    fptr[(int)PELEM_TRI_STRIP3_DATA] = phg_css_pex_oc;
    fptr[(int)PELEM_QUAD_MESH3_DATA] = phg_css_pex_oc;
    fptr[(int)PELEM_SET_OF_FILL_AREA_SET3_DATA] = phg_css_pex_oc;
    fptr[(int)PELEM_NUNI_BSP_CURVE] = phg_css_pex_oc;
    fptr[(int)PELEM_NUNI_BSP_SURF] = phg_css_pex_oc;
    fptr[(int)PELEM_CELL_ARRAY3_PLUS] = phg_css_pex_oc;
    fptr[(int)PELEM_TEXT_COLR] = phg_css_pex_oc;
    fptr[(int)PELEM_MARKER_COLR] = phg_css_pex_oc;
    fptr[(int)PELEM_EDGE_COLR] = phg_css_pex_oc;
    fptr[(int)PELEM_LINE_COLR] = phg_css_pex_oc;
    fptr[(int)PELEM_INT_COLR] = phg_css_pex_oc;
    fptr[(int)PELEM_BACK_INT_COLR] = phg_css_pex_oc;
    fptr[(int)PELEM_CURVE_APPROX_CRIT] = phg_css_pex_oc;
    fptr[(int)PELEM_LINE_SHAD_METH] = phg_css_pex_oc;
    fptr[(int)PELEM_REFL_PROPS] = phg_css_pex_oc;
    fptr[(int)PELEM_BACK_REFL_PROPS] = phg_css_pex_oc;
    fptr[(int)PELEM_INT_SHAD_METH] = phg_css_pex_oc;
    fptr[(int)PELEM_BACK_INT_SHAD_METH] = phg_css_pex_oc;
    fptr[(int)PELEM_INT_REFL_EQN] = phg_css_pex_oc;
    fptr[(int)PELEM_BACK_INT_REFL_EQN] = phg_css_pex_oc;
    fptr[(int)PELEM_SURF_APPROX_CRIT] = phg_css_pex_oc;
    fptr[(int)PELEM_PARA_SURF_CHARACS] = phg_css_pex_oc;
    fptr[(int)PELEM_FACE_DISTING_MODE] = phg_css_pex_oc;
    fptr[(int)PELEM_FACE_CULL_MODE] = phg_css_pex_oc;
    fptr[(int)PELEM_LIGHT_SRC_STATE] = phg_css_pex_oc;
    fptr[(int)PELEM_DCUE_IND] = phg_css_pex_oc;
    fptr[(int)PELEM_COLR_MAP_IND] = phg_css_pex_oc;
    fptr[(int)PELEM_RENDERING_COLR_MODEL] = phg_css_pex_oc;


    if ( !(cssh->stab = phg_css_stab_init(CSS_STAB_SIZE)) ) {
	free((char *)cssh);
	return(NULL);					/* out of memory */
    }
    cssh->open_struct = NULL;
    cssh->el_ptr = NULL;
    cssh->el_index = 0;
    cssh->edit_mode = PEDIT_INSERT;
    cssh->erh = erh;
    if ( !(cssh->ws_list = (Css_ws_list)
	    malloc((MAX_NO_OPEN_WS+1) * sizeof(Css_ws_on))) ) {
	phg_css_stab_free(cssh->stab);
	free((char *)cssh);
	return(NULL);					/* out of memory */
    }
    cssh->mem = NULL;		/* don't allocate space until it's needed */
    cssh->ssh_type = ssh_type;
    return(cssh);   
}

/*******************

    phg_css_destroy - free memory used by css

*******************/

void
phg_css_destroy(cssh)
    Css_handle	cssh;
{
    phg_css_delete_all_structs(cssh);
    phg_css_stab_free(cssh->stab);
    free((char *)cssh->ws_list);
    if (cssh->mem)
	free(cssh->mem);
    free((char *)cssh);
}
