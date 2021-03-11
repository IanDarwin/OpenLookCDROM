/* $XConsortium: css_el.c,v 5.4 94/04/17 20:42:39 rws Exp $ */

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

#include <X11/Xos.h>
#include "phg.h"
#include "css.h"
#include "css_priv.h"
#include "alloc.h"

static void css_rm_from_refer_sets();
static int css_add_to_refer_sets();

#define ELDATA_PTR(argdata) (((Css_eldata *)(argdata))->ptr)

/*******************

    phg_css_add_elem - Create a new element, insert (or replace) it in the
		       currently open structure, and call the appropriate
		       function (in el_funcs) to store the element data,
		       according to its type. If it is an execute structure
		       element, update the structure refer sets.
		       Return TRUE if successful, otherwise return FALSE
		       (malloc failure).

*******************/

int
phg_css_add_elem(cssh, args)
register Css_handle		cssh;
register Phg_args_add_el	*args;
{
    register El_handle elptr;

    if ( (cssh->edit_mode == PEDIT_INSERT) || (!cssh->el_index) ) {
	/* in replace mode, if current element is #0, insert before element #1*/
	CSS_CREATE_EL(cssh, elptr)
	CSS_INSERT_EL(cssh, elptr)
	elptr->eltype = args->el_type;
	if (!(*cssh->el_funcs[(int)args->el_type]) (cssh, elptr,
		(caddr_t)args->pex_oc.oc, CSS_EL_CREATE)){
	    ERR_BUF(cssh->erh, ERR901);
	    return(FALSE);				/* out of memory */
	}
    } else {
	/* replace mode, really replacing the current element */
	elptr = cssh->el_ptr;
	/* find out if new data is of the same class as the old */
	if (cssh->el_funcs[(int)args->el_type]
		!= cssh->el_funcs[(int)elptr->eltype]) {
	    /* 2 different classes of data, so free the old, create the new */
	    (void)(*cssh->el_funcs[(int)elptr->eltype])
		(cssh, elptr, (caddr_t)cssh->open_struct, CSS_EL_FREE);
	    elptr->eltype = args->el_type;
	    if (!(*cssh->el_funcs[(int)args->el_type])
		    (cssh, elptr, (caddr_t)args->pex_oc.oc, CSS_EL_CREATE)) {
		ERR_BUF(cssh->erh, ERR901);
		return(FALSE);				/* out of memory */
	    }
	} else {
	    /* same class of data, so just replace what is there now */
	    elptr->eltype = args->el_type;
	    if (!(*cssh->el_funcs[(int)args->el_type])
		    (cssh, elptr, (caddr_t)args->pex_oc.oc, CSS_EL_REPLACE)) {
		ERR_BUF(cssh->erh, ERR901);
		return(FALSE);				/* out of memory */
	    }
	}
    }
    return(TRUE);
}

/*******************

    phg_css_set_ep - Set the element pointer as indicated by opcode. Return
		     new value of element pointer unless a label was not
		     found; in that case return NULL.

*******************/

El_handle
phg_css_set_ep(cssh, opcode, data)
register Css_handle	cssh;
Phg_args_set_ep_op	opcode;
register Pint		data;
{
    register El_handle	elptr;
    register int	i;

    switch (opcode) {

      case PHG_ARGS_SETEP_ABS:
	cssh->el_ptr = cssh->open_struct->first_el;
	cssh->el_index = 0;
	/* fall through to SETEP_REL code */

      case PHG_ARGS_SETEP_REL:
	elptr = cssh->el_ptr;
	if (data >= 0) 
	    /* search forward */
	    while (data--) {
		if (elptr->next == cssh->open_struct->last_el)
		    break;
		elptr = elptr->next;
		cssh->el_index++;
	    }
	else 
	    /* search backwards */
	    while (data++ && elptr!=cssh->open_struct->first_el) {
		elptr = elptr->prev;
		cssh->el_index--;
	    }
	break;

      case PHG_ARGS_SETEP_LABEL:
	elptr = cssh->el_ptr->next;
	i = cssh->el_index + 1;
	while ( elptr != cssh->open_struct->last_el ) {
	    if ( elptr->eltype == PELEM_LABEL
		    && ((pexLabel*)elptr->eldata.ptr)->label == data )
		break;
	    elptr = elptr->next;
	    i++;
	}
	if (elptr == cssh->open_struct->last_el) {
	    ERR_BUF(cssh->erh, ERR205);
	    return(NULL);				/* label not found */
	}
	cssh->el_index = i;
        break;
    }
    return(cssh->el_ptr = elptr);
}

/*******************

    phg_css_el_delete_list - Return element pointers to the first and last of
                             a list of elements to be deleted, as indicated by
			     the parameters for deleting a range, between
			     labels, etc. The pointers will both point to the
			     same element for DEL_CURRENT. ep1 always points
			     to the element closest to the beginning of the
			     structure, so any routine processing the list
			     of elements delimited by ep1 and ep2 can do so
			     by following the "next" field of each successive
			     element.

*******************/

void
phg_css_el_delete_list(cssh, opcode, data, ep1, ep2)
register Css_handle	cssh;
Phg_args_del_el_op	opcode;
Phg_args_del_el_data	*data;
register El_handle	*ep1, *ep2;
{
    register El_handle	elptr;
    register int 	i1, i2;
    Struct_handle	structp;

    switch (opcode) {

      case PHG_ARGS_DEL_CURRENT:
	if (cssh->el_index)
	    *ep1 = *ep2 = cssh->el_ptr;
	else
	    *ep1 = NULL;
	break;

      case PHG_ARGS_DEL_RANGE:
	if (!cssh->open_struct->num_el) {
	    *ep1 = NULL;
	    return;		/* empty structure */
	}
	i1 = data->ep_values.ep1;
	/* make sure i1 <= i2 */
	if (i1 > data->ep_values.ep2) {
	    i2 = i1;
	    i1 = data->ep_values.ep2;
	} else
	    i2 = data->ep_values.ep2;
	if (i1<1 && i2<1) {
	    /* nothing to delete here, but set ep to element 0 */
	    *ep1 = NULL;
	    cssh->el_ptr = cssh->open_struct->first_el;
	    cssh->el_index = 0;
	    return;
	}
	/* make sure index values are in range */
	if (i1 < 1)
	    i1 = 1;
	else if (i1 > cssh->open_struct->num_el)
	    i1 = cssh->open_struct->num_el;
	if (i2 < 1)
	    i2 = 1;
	else if (i2 > cssh->open_struct->num_el)
	    i2 = cssh->open_struct->num_el;
	/* now get the pointer values */
	i2 = i2 - i1;
	*ep1 = cssh->open_struct->first_el;
	while (i1--)
	    *ep1 = (*ep1)->next;
	*ep2 = *ep1;
	while (i2--)
	    *ep2 = (*ep2)->next;
	break;

      case PHG_ARGS_DEL_LABEL:
	*ep1 = *ep2 = NULL;
        elptr = cssh->el_ptr->next;
        i1 = cssh->el_index + 1;
	while ( elptr != cssh->open_struct->last_el ) {
	    if ( elptr->eltype == PELEM_LABEL
		    && ((pexLabel*)elptr->eldata.ptr)->label ==
			data->label_range.label1 )
		break;
	    elptr = elptr->next;
	    i1++;
	}
        if (elptr == cssh->open_struct->last_el) {
            ERR_BUF(cssh->erh, ERR206);
            return;					/* label not found */
	}
	*ep1 = elptr;
	elptr = elptr->next;
	i2 = i1 + 1;
	while ( elptr != cssh->open_struct->last_el ) {
	    if ( elptr->eltype == PELEM_LABEL
		    && ((pexLabel*)elptr->eldata.ptr)->label ==
			data->label_range.label2 )
		break;
	    elptr = elptr->next;
	    i2++;
	}
        if (elptr == cssh->open_struct->last_el) {
            ERR_BUF(cssh->erh, ERR206);
            return;					/* label not found */
	}
	if (i2-1 == i1) {
	    /* nothing to delete here, but set ep to label 1 */
	    cssh->el_ptr = *ep1;
	    cssh->el_index = i1;
	    *ep1 = NULL;
	} else {
	    *ep1 = (*ep1)->next;
	    *ep2 = elptr->prev;
	}
	break;

      case PHG_ARGS_EMPTY_STRUCT:
	*ep1 = NULL;
	/* if structure doesn't exist, create an empty one */
	if ( !(structp = CSS_STRUCT_EXISTS(cssh, data->struct_id)) ) {
	    if ( !(structp = phg_css_create_struct(data->struct_id)) ) {
		ERR_BUF(cssh->erh, ERR901);
		return;					/* out of memory */
	    }
	    if ( !phg_css_stab_insert(cssh->stab, data->struct_id, structp) ) {
		ERR_BUF(cssh->erh, ERR901);
		return;					/* out of memory */
	    }
	} else {
	    if (structp->num_el) {
		*ep1 = structp->first_el->next;
		*ep2 = structp->last_el->prev;
	    }
	}
	break;
    }
}


/*******************

    phg_css_delete_el - Delete the list of elements delimited by ep1 and ep2.
			This routine assumes ep1 and ep2 were obtained from
			phg_css_el_delete_list and therefore doesn't worry
			about the possibility of ep1 pointing to element 0.
			Set the current element index and pointer appropriately.

*******************/

void
phg_css_delete_el(cssh, opcode, data, ep1, ep2)
register Css_handle	cssh;
Phg_args_del_el_op	opcode;
Phg_args_del_el_data	*data;
register El_handle	ep1, ep2;
{
    Struct_handle	structp;
    register El_handle	elptr;
    register int	i;

    if (opcode != PHG_ARGS_EMPTY_STRUCT) {
	/* set new current element pointer and index */
	cssh->el_ptr = ep1->prev;
	CSS_UPDATE_EL_INDEX(cssh)
	structp = cssh->open_struct;
    } else {
	/* only reset element ptr/index if this structure is the open one */
	if (cssh->open_struct && 
	    (data->struct_id == cssh->open_struct->struct_id) ) {
	    cssh->el_ptr = cssh->open_struct->first_el;
	    cssh->el_index = 0;
	    structp = cssh->open_struct;
	} else {
	    structp = CSS_STRUCT_EXISTS(cssh, data->struct_id);
	}
    }

    /* remove group of elements from structure */
    ep1->prev->next = ep2->next;
    ep2->next->prev = ep1->prev;
    /* free the space they use */
    elptr = ep1;
    i = 0;
    ep2 = ep2->next;
    do {
	(void) (*cssh->el_funcs[(int)elptr->eltype]) 
	    (cssh, elptr, (caddr_t)structp, CSS_EL_FREE);
	ep1 = elptr;
	elptr = elptr->next;
	free( (char *)ep1 );
	i++;
    } while (elptr != ep2);
    structp->num_el -= i;
}



/*******************

    phg_css_struct_ref - Handle element data for execute structure element
			 This includes creating non_existent structures
			 and updating ws_appear and refer_to information
			 as needed. Also includes translating between structure
			 pointer stored in css and structure id stored in
			 archive.
			 Return TRUE(1) if successful, FALSE(0) if malloc
			 failed; return -1 if successful but an empty
		 	 structure had to be created.

*******************/

int
phg_css_struct_ref(cssh, elptr, argdata, opcode)
    Css_handle		cssh;
    El_handle		elptr;
    caddr_t		argdata;
    Css_el_op		opcode;
{
    register Struct_handle	structp;
    int				structid;
    int				retval = TRUE;

    switch (opcode) {
      case CSS_EL_REPLACE:
	phg_css_rm_from_ws_appear(cssh, (Struct_handle)elptr->eldata.ptr,
	    cssh->open_struct->ws_appear_on, 1);
	css_rm_from_refer_sets(cssh->open_struct, elptr);
	/* fall through */

      case CSS_EL_CREATE:
      case CSS_EL_AR_TO_CSS:
	structid = opcode == CSS_EL_AR_TO_CSS ? 
	    ((Css_eldata *)argdata)->idata :
		((pexExecuteStructure *)argdata)->id;
	/* if executed structure doesn't exist, create an empty one */
	if ( !(structp = CSS_STRUCT_EXISTS(cssh, structid)) ) {
	    if ( !(structp = phg_css_create_struct(structid)) )
		return(FALSE);                               /* out of memory */
	    if ( !phg_css_stab_insert(cssh->stab, structid, structp) )
		return(FALSE);                               /* out of memory */
	    retval = -1;
	}
	elptr->eldata.ptr = (caddr_t)structp;
        if ( !css_add_to_refer_sets(cssh, structp) )
            return(FALSE);                              /* out of memory */
        if ( !phg_css_add_to_ws_appear(cssh, structp, 
		cssh->open_struct->ws_appear_on, 1))
            return(FALSE);                              /* out of memory */
	break;

      case CSS_EL_COPY:
	elptr->eldata.ptr = ((Css_eldata *)argdata)->ptr;
	structp = (Struct_handle)elptr->eldata.ptr;
        if ( !css_add_to_refer_sets(cssh, structp) )
            return(FALSE);                              /* out of memory */
        if ( !phg_css_add_to_ws_appear(cssh, structp, 
		cssh->open_struct->ws_appear_on, 1))
            return(FALSE);                              /* out of memory */
	break;

      case CSS_EL_CSS_TO_AR:
	elptr->eldata.idata = 
	    ((Struct_handle)(((Css_eldata *)argdata)->ptr))->struct_id;
	break;

      case CSS_EL_FREE:
	if (argdata && cssh->ssh_type!=SSH_AR) {
	    /* if no structp passed in argdata, or if ssh is an archive (archive
	     * maintains no ws_appear or refer_to lists), no need to do this
	     */
	    phg_css_rm_from_ws_appear(cssh, (Struct_handle)elptr->eldata.ptr,
		((Struct_handle)argdata)->ws_appear_on, 1);
	    css_rm_from_refer_sets((Struct_handle)argdata, elptr);
	}
	break;

      case CSS_EL_INQ_CONTENT: {
	    Phg_ret_q_content	*ret_data = (Phg_ret_q_content *)argdata;
	    pexExecuteStructure	*oc;

	    /* The OC isn't kept around in PEX format, so build it. */
	    CSS_MEM_BLOCK(cssh, sizeof(pexExecuteStructure), oc,
		pexExecuteStructure)
	    if ( !oc )
		return(FALSE);			/* out of memory */

	    oc->head.elementType = PEXOCExecuteStructure;
	    oc->head.length = sizeof(*oc) / sizeof(CARD32);
	    oc->id = ((Struct_handle)elptr->eldata.ptr)->struct_id;
	    ret_data->pex_oc.oc = (pexElementInfo *)oc;
	    ret_data->pex_oc.size = sizeof(*oc);
      } break;

      case CSS_EL_INQ_TYPE_SIZE: {
	Pint	*size = (Pint *)argdata;
	*size = 0;
      } break;
    }
    return(retval);
}


/*******************

    phg_css_pex_oc - Handle element data for a PEX output command.
		     Return TRUE if successful, FALSE if malloc failed.

*******************/

int
phg_css_pex_oc(cssh, elptr, argdata, opcode)
    Css_handle		cssh;
    El_handle		elptr;
    caddr_t		argdata;
    Css_el_op		opcode;
{
    switch (opcode) {
	case CSS_EL_CREATE:
	case CSS_EL_COPY:
	case CSS_EL_REPLACE: {
	    pexElementInfo	*argp;

	    argp = (opcode == CSS_EL_COPY) ? 
		(pexElementInfo *)ELDATA_PTR(argdata)
		    : (pexElementInfo *)argdata;
	    if (opcode == CSS_EL_REPLACE)
		free((char *)elptr->eldata.ptr);
	    if ( !(elptr->eldata.ptr = (caddr_t)
		    malloc((unsigned)(argp->length * sizeof(CARD32)))) ) {
		return(FALSE);				/* out of memory */
	    }
	    bcopy( (char *)argp, (char *)elptr->eldata.ptr,
		(int)(argp->length * sizeof(CARD32)) );
	} break;

	case CSS_EL_FREE:
	    free( (char *)elptr->eldata.ptr );
	    break;

	case CSS_EL_INQ_CONTENT: {
	    Phg_ret_q_content	*ret_data = (Phg_ret_q_content *)argdata;

	    ret_data->pex_oc.oc = (pexElementInfo *)elptr->eldata.ptr;
	    ret_data->pex_oc.size =
		ret_data->pex_oc.oc->length * sizeof(CARD32);
	} break;

	case CSS_EL_INQ_TYPE_SIZE: {
	    Pint		*size = (Pint *)argdata;

	    *size = phg_utx_compute_el_size(
		(pexElementInfo *)elptr->eldata.ptr,
		(pexElementInfo *)elptr->eldata.ptr );
	} break;
    }
    return(TRUE);
}

/*******************

    phg_css_no_data - Dummy for element types with no associated data.
		      Only PELEM_NIL now.
		      Return TRUE.

*******************/

int
phg_css_no_data(cssh, elptr, argdata, opcode)
Css_handle	cssh;
El_handle	elptr;
caddr_t		*argdata;
Css_el_op	opcode;
{
    switch (opcode) {
	case CSS_EL_INQ_CONTENT: {
	    Phg_ret_q_content	*ret_data = (Phg_ret_q_content *)argdata;

	    ret_data->pex_oc.oc = (pexElementInfo *)NULL;
	    ret_data->pex_oc.size = 0;
	} break;

	case CSS_EL_INQ_TYPE_SIZE: {
	    Pint		*size = (Pint *)argdata;

	    *size = 0;
	} break;
    }
    return(TRUE);
}


/* 
 * Structure Sets - a set has a key and data associated with that key. For
 * the structure sets the data fields are counts, pointers to other sets or 
 * NULL. Counting is used to know when a structure is not referenced by any
 * structure.
 * 
 * Notation: { ( , ) } is a set of ordered pairs; {( , {( , )})} is a set
 * whose data field is another set.
 * 
 * 	refer_to_me	- set of structures that refer to me
 * 			{(struct_ptr, count)}
 * 
 * 	i_refer_to	- structures this structure refers to; associated 
 * 			  with each referenced structure is a set of element
 * 			  ptrs for the EXECUTE_STRUCTURE elements.
 * 			{(struct_ptr, {(el_ptr, NULL)})}
 * 
 */

/*******************

    css_add_to_refer_sets - Add the executed structure (named in current
			    structure element) to the currently open
			    structure's i_refer_to set, and add the
			    currently open structure to the executed
			    structure's refer_to_me set.
			    Return TRUE if successful, FALSE if malloc
			    failed.

*******************/

static int
css_add_to_refer_sets(cssh, exec_struct)
register Css_handle	cssh;
register Struct_handle	exec_struct;		/* structure being executed */
{   
    register Struct_handle	open_struct = cssh->open_struct;
    Css_set_ptr			el_set;		/* set of element pointers  */
    int				count;		/* how many times a structure 
					 	 * has been referenced */

    /* see if open structure has previously referenced exec_struct, if not
     * then create a set for exec_struct before adding the elptr to the
     * EXECUTE STRUCTURE element.
     */
    if (!phg_css_set_element_of(open_struct->i_refer_to,
	    (caddr_t)exec_struct, (caddr_t *)&el_set)){
	if ( !(el_set = phg_css_set_create(SET_DATA_SET)) )
	    return(FALSE);                               /* out of memory */
	if ( !phg_css_set_add(open_struct->i_refer_to, (caddr_t)exec_struct,
		(caddr_t)el_set) )
	    return(FALSE);                               /* out of memory */
    }
    if ( !phg_css_set_add(el_set, (caddr_t)cssh->el_ptr, (caddr_t)NULL) )
	return(FALSE);	                               /* out of memory */

    /* add open struct to exec_struct's refer_to_me set */
    if( !phg_css_set_element_of(exec_struct->refer_to_me,
	    (caddr_t)open_struct, (caddr_t *)&count) )
	count = 0;    
    count++;
    if ( !phg_css_set_add(exec_struct->refer_to_me, (caddr_t)open_struct,
	    (caddr_t)count) )
	return(FALSE);	                               /* out of memory */

    return(TRUE);
}

/*******************

    css_rm_from_refer_sets - Remove the executed structure (named in current 
			     structure element) from the currently open
			     structure's i_refer_to set, and remove the
			     currently open structure from the executed
			     structure's refer_to_me set.

*******************/

static void
css_rm_from_refer_sets(edit_struct, elptr)
register Struct_handle	edit_struct;		/* structure being edited */
register El_handle	elptr;			/* element being deleted */
{   
    register Struct_handle	exec_struct =	/* structure being executed */ 
	(Struct_handle)elptr->eldata.ptr;
    Css_set_ptr			el_set;		/* set of element pointers  */
    int				count;		/* how many times a structure 
					 	 * has been referenced */

    /* remove element ptr from set of references to exec_struct, and remove
     * the set of references if there was only the one ptr in it
     */
    if (phg_css_set_element_of(edit_struct->i_refer_to,
	    (caddr_t)exec_struct, (caddr_t *)&el_set)) {
	(void) phg_css_set_remove(el_set, (caddr_t)elptr);
	if (!el_set->num_elements) {
	    (void) phg_css_set_remove(edit_struct->i_refer_to,
		(caddr_t)exec_struct);
	    phg_css_set_free(el_set);
	}
    }
    /* if executed struct is no longer referred to by the open structure,
     * remove edit_struct from the set, otherwise just decrement the count
     */
    if (phg_css_set_element_of(exec_struct->refer_to_me,
	    (caddr_t)edit_struct, (caddr_t *)&count)) {
	if (--count <= 0)
	    (void) phg_css_set_remove(exec_struct->refer_to_me,
		(caddr_t)edit_struct);
	else
	    (void)phg_css_set_add(exec_struct->refer_to_me,
		(caddr_t)edit_struct, (caddr_t)count);
    }
}


/*******************

    phg_css_el_search - Perform "element search" forward or backward
			for an element in the inclusion set, but not
			in the exclusion set.

*******************/

phg_css_el_search( cssh, struct_id, start_el, dir, incl, excl, ret)
Css_handle	 cssh;
Pint		 struct_id;
Pint		 start_el;
Psearch_dir	 dir;	/* search direction	*/
Pelem_type_list	*incl;	/* element incl. list	*/
Pelem_type_list	*excl;	/* element excl. list	*/
Phg_ret		*ret;
{
    Struct_handle	structp;
    register El_handle	elptr;
    register Pint	elem_no;
    char		type_array [(int)PELEM_NUM_EL_TYPES];

    if ( !(structp = CSS_STRUCT_EXISTS(cssh, struct_id)) ) {
	ret->err = ERR201;
	return;
    } else {
	ret->err = 0;
	ret->data.el_search.status = PSEARCH_STATUS_FAILURE;
	ret->data.el_search.found_el = 0;
    }

    /* Find start_el element, clamping to NIL elements at ends of structure */
    if (start_el <= 0) {
	start_el = 0;
	elptr = structp->first_el;	/* HEAD NIL element */

    } else if (start_el >= structp->num_el) {/* as far as we ever go */
	    start_el = structp->num_el;
	    elptr = structp->last_el->prev;	/* last non-NIL element */

    } else {				/* count forward for start_el */
	register El_handle	next;

	assure( (1 <= start_el) && (start_el <= structp->num_el) );

	elptr = structp->first_el;
	next = elptr->next;
	elem_no = 0;
	while (elem_no < start_el) {
	    elptr = next;
	    assure(elptr)
	    next = elptr->next;
	    assure(next)
	    elem_no++;
	}
	assure(elem_no == start_el)			/* ready to search */
    }
    /* elptr points to element numbered start_el in struct_id */

    /* Load the type_array (of include/exclude info), ready to search */
    {
	register char		*type_ptr;
	register Pelem_type	 sel_type;
#   define count	elem_no		/* re-use the integer register */

	if (incl->num_elem_types > 0 && incl->elem_types[0] == PELEM_ALL)
    incl_all_true:
	    for (type_ptr = type_array;
		    type_ptr < &type_array[(int)PELEM_NUM_EL_TYPES]; type_ptr++)
		*type_ptr = TRUE; 		/* turn them all on */
	else {
	    for (type_ptr = type_array;
		    type_ptr < &type_array[(int)PELEM_NUM_EL_TYPES]; type_ptr++)
		*type_ptr = FALSE; 		/* turn them all off */
	    for (count = incl->num_elem_types; count--; )
		if ( (sel_type = *incl->elem_types++) == PELEM_ALL)
		    goto incl_all_true;
		else
		    type_array[(int)sel_type] = TRUE;
	}

	for (count = excl->num_elem_types; count--; )
	    if ( (sel_type = *excl->elem_types++) == PELEM_ALL) {
		assure(ret->data.el_search.status == PSEARCH_STATUS_FAILURE)
		return;
	    } else
		type_array[(int)sel_type] = FALSE;
#   undef  count		/* re-use the integer register */
    }

    /* elptr points to element numbered start_el in struct_id */
    if (dir == PDIR_FORWARD) {
	for (elem_no = start_el; elem_no <= structp->num_el;
		elptr = elptr->next, elem_no++) {
	    if( type_array[(int)elptr->eltype] ) {
		ret->data.el_search.found_el = elem_no;
		ret->data.el_search.status = PSEARCH_STATUS_SUCCESS;
		return;
	    }
	}
    }
    else {	/* (dir == PDIR_BACKWARD) */
	for (elem_no = start_el; elptr; elptr = elptr->prev, elem_no--) {
	    if( type_array[(int)elptr->eltype] ) {
		ret->data.el_search.found_el = elem_no;
		ret->data.el_search.status = PSEARCH_STATUS_SUCCESS;
		return;
	    }
	}
    }
}
