/* $XConsortium: css_str.c,v 5.4 94/04/17 20:42:46 rws Exp $ */

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

#include "phg.h"
#include "cp.h"
#include "ws.h"
#include "css.h"
#include "css_priv.h"
#include "alloc.h"

static void css_struct_free();
static int css_get_dlist();
static int css_get_other_refs();
static int css_get_network();
static int css_change_ref_structp();

#define CSS_ADD_NEW_STRUCT(cssh, structid, structp) \
    if ( !((structp) = phg_css_create_struct((structid))) ) { \
	ERR_BUF((cssh)->erh, ERR901); \
	return(NULL);                           /* out of memory */ \
    } \
    if ( !(phg_css_stab_insert((cssh)->stab, (structid), (structp))) ) { \
	ERR_BUF((cssh)->erh, ERR901); \
	return(NULL);                           /* out of memory */ \
    }
	
/*******************

    phg_css_open_struct - Open the structure with the given id. If it doesn't
			  exist, create it and insert it in the structure
			  table. Set the element pointer/index to the end of
			  structure and return a pointer to the structure.
			  Return NULL if malloc failed.

*******************/

Struct_handle
phg_css_open_struct(cssh, structid)
register Css_handle	cssh;
register Pint		structid;
{
    if ( !(cssh->open_struct = CSS_STRUCT_EXISTS(cssh, structid)) ) {
	/* no such structure, so create an empty one */
	CSS_ADD_NEW_STRUCT(cssh, structid, cssh->open_struct)
    }
    cssh->el_ptr = cssh->open_struct->last_el->prev;
    cssh->el_index = cssh->open_struct->num_el;
    return(cssh->open_struct);
}

/*******************

    phg_css_close_struct - close the currently open structure, return a
			   pointer to it.

*******************/

Struct_handle
phg_css_close_struct(cssh)
register Css_handle cssh;
{
    register Struct_handle structp = cssh->open_struct;

    cssh->open_struct = NULL;
    return(structp);
}

/*******************

    phg_css_copy_struct - Copy given structure into currently open structure.

*******************/

int
phg_css_copy_struct(cssh, structp)
register Css_handle	cssh;
register Struct_handle	structp;
{
    register El_handle	elptr, elnew;
    register int	i, skip_copies;
    /* save and use the number of elements, in case copy is from open
     * structure into open structure (if so, num_el will change while copying)
     */
    register int	 n = structp->num_el;

    /* if the copy is from the open structure into the open structure AND 
     * the element index is not at the beginning or the end of the structure,
     * save the number of newly copied elements to skip over when they
     * are reached while stepping through the structure
     */
    if ( CSS_STRUCT_IS_OPEN(cssh, structp) && 
	 cssh->el_index>0 && cssh->el_index<structp->num_el)
	skip_copies = cssh->el_index;
    else
	skip_copies = 0;
    elptr = structp->first_el->next;
    for (i = 1; i <= n; i++) {
	CSS_CREATE_EL(cssh, elnew)
	CSS_INSERT_EL(cssh, elnew)
	if ( ! (*cssh->el_funcs[(int)elptr->eltype]) 
	       (cssh, elnew, (caddr_t)&elptr->eldata, CSS_EL_COPY) ) {
	    ERR_BUF(cssh->erh, ERR901);
	    return(FALSE);				/* out of memory */
	}
	elnew->eltype = elptr->eltype;
	if (i == skip_copies) {
	    /* have to skip "skip_copies" elements now to get to the rest of
	     * the structure, because we have reached the ones we just inserted
	     */
	    while (skip_copies--)
		elptr = elptr->next;
	}
	elptr = elptr->next;
    }
    return(TRUE);
}

/*******************

    phg_css_delete_struct - Delete specified structure, free space allocated
			    for it, and remove all references to it.
			    If a structure was open, re-open it (empty) after
			    deleting it.

*******************/

void
phg_css_delete_struct(cssh, delstruct)
Css_handle	cssh;
Struct_handle	delstruct;
{
    register Css_set_element	*refstruct, *el;
    register Struct_handle	rstructp;
    register El_handle		elptr;
    Css_set_ptr			el_set;
    Pint			delstruct_id;

    if ( CSS_STRUCT_IS_OPEN(cssh, delstruct) )
	delstruct_id = delstruct->struct_id;
    /* remove all references to delstruct */
    refstruct = delstruct->refer_to_me->elements->next;
    while (refstruct) {
	rstructp = (Struct_handle)refstruct->key;
	(void) phg_css_set_element_of(rstructp->i_refer_to,
	    (caddr_t)delstruct, (caddr_t *)&el_set);
	/* remove exec struct elements from each struct referencing delstruct */
	el = el_set->elements->next;
	while (el) {
	    elptr = (El_handle)el->key;
	    if (CSS_STRUCT_IS_OPEN(cssh, rstructp) && elptr==cssh->el_ptr)
		cssh->el_ptr = cssh->el_ptr->prev;
	    elptr->prev->next = elptr->next;
	    elptr->next->prev = elptr->prev;
	    free((char *)elptr);
	    el = el->next;
	}
	rstructp->num_el -= el_set->num_elements;
	phg_css_set_free(el_set);
	(void) phg_css_set_remove( rstructp->i_refer_to, (caddr_t)delstruct);
	/* fix element index if this is the current structure */
	if ( CSS_STRUCT_IS_OPEN(cssh, rstructp) )
	    CSS_UPDATE_EL_INDEX(cssh)
	refstruct = refstruct->next;
    }

    /* empty the structure and delete it from the structure table */
    CSS_EMPTY_STRUCT(cssh, delstruct->struct_id)
    (void) phg_css_stab_delete(cssh->stab, delstruct->struct_id);
    css_struct_free(cssh, delstruct);
    /* re-open it if it was open before */
    if ( CSS_STRUCT_IS_OPEN(cssh, delstruct) )
	(void) phg_css_open_struct(cssh, delstruct_id);
}

/*******************

    phg_css_delete_net - Delete structure network indicated by structp;
			 if refflag == PFLAG_KEEP and a structure in the network
			 is referenced from outside the network, don't
			 delete it.
			 Uses css_get_dlist recursively for each level of
			 "i_refer_to" structures, to get list of structures
			 which are ok to delete, if refflag == PFLAG_KEEP.

*******************/

void
phg_css_delete_net(cssh, structp, refflag)
register Css_handle	cssh;
register Struct_handle	structp;
Pref_flag		refflag;
{
    Css_set_ptr			network, dlist;
    register Css_set_element	*delstruct;
    register Css_ws_list         ws_list;

    if ( !(network = phg_css_set_create(SET_DATA_SET)) ) {
	ERR_BUF(cssh->erh, ERR900);			/* out of memory */
	return;
    }
    /* first get the structure network (all you need if refflag == PFLAG_DELETE) */
    if ( !css_get_network(cssh, structp, &network) ) {
	phg_css_set_free(network);
	ERR_BUF(cssh->erh, ERR900);			/* out of memory */
	return;
    }
    if (refflag == PFLAG_KEEP) {
	if ( !(dlist = phg_css_set_create(SET_DATA_SET)) ) {
	    phg_css_set_free(network);
	    ERR_BUF(cssh->erh, ERR900);			/* out of memory */
	    return;
	}
	/* now build list of structures from network that are ok to delete */
	if ( !css_get_dlist(cssh, structp, network, structp, dlist) ) {
	    phg_css_set_free(network);
	    phg_css_set_free(dlist);
	    ERR_BUF(cssh->erh, ERR900);			/* out of memory */
	    return;
	}
	delstruct = dlist->elements->next;
    } else
	delstruct = network->elements->next;
    phg_css_delete_struct(cssh, structp);
    while (delstruct) {
	if (ws_list = CSS_GET_WS_ON( (Struct_handle)delstruct->key ))
	    for (;ws_list->wsh; ws_list++)
		(*ws_list->wsh->unpost)( ws_list->wsh,
		    (Struct_handle)delstruct->key );
	phg_css_delete_struct(cssh, (Struct_handle)delstruct->key);
	delstruct = delstruct->next;
    }
    phg_css_set_free(network);
    if (refflag == PFLAG_KEEP)
	phg_css_set_free(dlist);
}

static int
css_get_dlist(cssh, parent, network, root, dlist)
Css_handle		cssh;
register Struct_handle	parent;
register Css_set_ptr	network;
Struct_handle		root;
Css_set_ptr		dlist;
{
    register Css_set_element	*exec, *ref;
    Css_set_ptr			refer_to_me;
    caddr_t			data;
    register char		ok_to_delete = TRUE;

    exec = parent->i_refer_to->elements->next;
    while (exec) {
	if (((Struct_handle)exec->key)->refer_to_me->num_elements > 1) {
	    /* construct a list of references excluding the immediate parent
	     * and the network root, then use it to see if anyone references
	     * this struct from outside the network
	     */
	    if ( !(refer_to_me = phg_css_set_create(SET_DATA_SET)) )
		return(FALSE);				/* out of memory */
	    ref = ((Struct_handle)exec->key)->refer_to_me->elements->next;
	    while (ref) {
		if ((Struct_handle)ref->key!=parent && 
			(Struct_handle)ref->key!=root) {
		    if ( !phg_css_set_add(refer_to_me, ref->key,
			    (caddr_t)NULL) ) {
			phg_css_set_free(refer_to_me);
			return(FALSE);			/* out of memory */
		    }
		    if ( !css_get_other_refs((Struct_handle)ref->key, 
			    refer_to_me, root) ) {
			phg_css_set_free(refer_to_me);
			return(FALSE);			/* out of memory */
		    }
		}
		ref = ref->next;
	    }
	    ref = refer_to_me->elements->next;
	    while (ref) {
		if ( !phg_css_set_element_of(network, ref->key, &data) ) {
		    ok_to_delete = FALSE;
		    break;
		}
		ref = ref->next;
	    }
	    phg_css_set_free(refer_to_me);
	}
	if (ok_to_delete) {
	    if ( !css_get_dlist(cssh, 
		    (Struct_handle)exec->key, network, root, dlist) )
		return(FALSE);				/* out of memory */
	    if ( !phg_css_set_add(dlist, exec->key, (caddr_t)NULL) )
		return(FALSE);				/* out of memory */
	}
	else
	    /* no need to look at this struct's descendants */
	    ok_to_delete = TRUE;
	exec = exec->next;
    }
    return(TRUE);
}

static int
css_get_other_refs(structp, refer_set, root)
register Struct_handle	structp;
register Css_set_ptr	refer_set;
register Struct_handle	root;
{
    register Css_set_element	*ref;

    ref = structp->refer_to_me->elements->next;
    while (ref) {
	if ((Struct_handle)ref->key != root) {
	    if ( !phg_css_set_add(refer_set, ref->key, (caddr_t)NULL) )
		return(FALSE);				/* out of memory */
	    if ( !css_get_other_refs((Struct_handle)ref->key, refer_set, root) )
		return(FALSE);				/* out of memory */
	}
	ref = ref->next;
    }
    return(TRUE);
}

#ifdef	DEBUG
static
css_print_structlist(header, structlist)
char		*header;
Css_set_ptr	structlist;
{
    register Css_set_element	*el;

    fprintf(stderr, "%s: ", header);
    el = structlist->elements->next;
    while (el) {
	fprintf(stderr, "%d ", ((Struct_handle)el->key)->struct_id);
	el = el->next;
    }
    fprintf(stderr, "\n");
}
#endif	/* DEBUG */

/*******************

    css_get_network - Recursively construct a list (stored as a Css_set)
		      of the structure pointers in the network 
		      identified by structp (i.e., all descendants).
		      Assumes structlist points to a valid (presumably empty)
		      Css_set; it is up to the caller to free the set created
		      for the list when it is through with it.
		      Returns TRUE if successful, FALSE if malloc
		      failed while building the set.
		      N.B. - this list does not include the network root.

*******************/

static int
css_get_network(cssh, structp, structlist)
register Css_handle 	cssh;
register Struct_handle 	structp;
register Css_set_ptr	*structlist;
{
    register Css_set_element	*el;

    el = structp->i_refer_to->elements->next;
    while (el) {
	if ( !phg_css_set_add(*structlist, el->key, (caddr_t)NULL) ) {
	    phg_css_set_free(*structlist);
	    return(FALSE);				/* out of memory */
	}
	if ( !css_get_network(cssh, (Struct_handle)el->key, structlist))
	    return(FALSE);				/* out of memory */
	el = el->next;
    }
    return(TRUE);
}

/*******************

    phg_css_delete_all_structs - Delete all structures, free allocated space
				 for the structures, and in the structure
				 table. If a structure was open, re-open it
				 after deleting everything.

*******************/

void
phg_css_delete_all_structs(cssh)
register Css_handle cssh;
{
    register Css_hash_block	**stab_row, *block, *prevbl;
    register int		n;
    Pint			open_structid;

    if (cssh->open_struct)
	open_structid = cssh->open_struct->struct_id;
    n = cssh->stab->nstructs;
    stab_row = cssh->stab->table;
    while (n) {
	block = (*stab_row)->next;
	while (block) {
	    css_struct_free(cssh, block->struct_ptr);
	    prevbl = block;
	    block = block->next;
	    free((char *)prevbl);
	    n--;
	}
	/* reset row pointer */
	(*stab_row)->next = NULL;
	if (n)
	    /* in case we are at the last row of the table */
	    stab_row++;
    }
    cssh->stab->nstructs = 0;
    if (cssh->open_struct)
	(void) phg_css_open_struct(cssh, open_structid);
}

/*******************

    phg_css_change_struct_id - Change structure id.
			       Return a list of affected workstations (the
			       combined ws_appear_on lists of the structures
			       orig and new).

*******************/

Css_ws_list
phg_css_change_struct_id(cssh, ids, orig, new, orig_posted_somewhere)
Css_handle		cssh;
Phg_args_change_struct	*ids;
register Struct_handle	orig;
Struct_handle		new;
int			orig_posted_somewhere;
{
    register Struct_handle	structp = NULL;
    register Struct_handle	execp;
    register Css_set_element	*ref, *structel;
    Css_set_ptr			el_set;
    Css_ws_list			wsnext;

    cssh->ws_list->wsh = NULL;
    if (!phg_css_join_ws_list(cssh, orig, new, &cssh->ws_list, CSS_WS_APPEAR))
	return(NULL);					/* out of memory */
    if (orig) {
	/* remove from old location in structure table */
	(void) phg_css_stab_delete(cssh->stab, ids->orig_id);
	if ( orig_posted_somewhere || orig->refer_to_me->num_elements
		|| orig->ws_posted_to ) {
	    /* orig struct is referenced or posted, so create an empty one */
	    CSS_ADD_NEW_STRUCT(cssh, ids->orig_id, structp)
	}
	if (orig->refer_to_me->num_elements || orig->ws_posted_to) {
	    /* fix up ws_posted_to and ws_appear_on lists */
	    if ( !phg_css_copy_ws_lists(cssh, orig, structp, CSS_WS_BOTH) )
		return(NULL);				/* out of memory */
	    if (wsnext = orig->ws_posted_to) {
		/* tell ws about changed structure pointer for orig */
		while (wsnext->wsh) {
		    (*wsnext->wsh->change_posting)( wsnext->wsh,
			orig, structp);
		    wsnext++;
		}
		free((char *)orig->ws_posted_to);
		orig->ws_posted_to = NULL;
	    }
	    phg_css_rm_from_ws_appear(cssh, orig, structp->ws_appear_on, 1);
	    structp->refer_to_me = orig->refer_to_me;
	    ref = structp->refer_to_me->elements->next;
	    while (ref) {
		/* change the struct ptrs in the ref lists & struct elements */
		execp = (Struct_handle)ref->key;
		(void) phg_css_set_element_of(execp->i_refer_to,
		    (caddr_t)orig, (caddr_t*)&el_set);
		(void) phg_css_set_remove(execp->i_refer_to, (caddr_t)orig);
		(void) phg_css_set_add(execp->i_refer_to, (caddr_t)structp,
		    (caddr_t)el_set);
		structel = el_set->elements->next;
		while (structel) {
		    ((El_handle)structel->key)->eldata.ptr = (char *)structp;
		    structel = structel->next;
		}
		ref = ref->next;
	    }
	    if ( !(orig->refer_to_me = phg_css_set_create(SET_DATA_SET)) ) {
		ERR_BUF(cssh->erh, ERR901);
		return(NULL);				/* out of memory */
	    }
	}
	if (new) {
	    /* if new is referenced or posted, copy data before destroying new*/
	    if ( !phg_css_copy_ws_lists(cssh, new, orig, CSS_WS_POST) )
		return(NULL);				/* out of memory */
	    if (!phg_css_add_to_ws_appear(cssh, orig, new->ws_appear_on, 1)) {
		ERR_BUF(cssh->erh, ERR901);
		return(NULL);				/* out of memory */
	    }
	    if (wsnext = new->ws_posted_to)
		/* tell ws about changed structure pointer for new */
		while (wsnext->wsh) {
		    (*wsnext->wsh->change_posting)(wsnext->wsh, new, orig);
		    wsnext++;
		}
	    if ( !css_change_ref_structp(new, orig) ) {
		ERR_BUF(cssh->erh, ERR901);
		return(NULL);				/* out of memory */
	    }
	    /* empty new and delete */
	    CSS_EMPTY_STRUCT(cssh, ids->new_id)
	    (void) phg_css_stab_delete(cssh->stab, ids->new_id);
	    css_struct_free(cssh, new);
	    if ( structp && !(structp->refer_to_me->num_elements || 
			      structp->ws_posted_to) )
		/* only refs to structp were by new, which no longer exists */
		phg_css_delete_struct(cssh, structp);
	}
	/* now move to new location in struct table */
	orig->struct_id = ids->new_id;
	if ( !(phg_css_stab_insert(cssh->stab, ids->new_id, orig)) ) {
	    ERR_BUF(cssh->erh, ERR901);
	    return(NULL);				/* out of memory */
	}
	/* if original struct was open, "re-create" it empty by opening it */
	if ( CSS_STRUCT_IS_OPEN(cssh, orig) )
	    (void) phg_css_open_struct(cssh, ids->orig_id);
	/* if the "new" structure was open, re-open it with the correct ptr */
	else if (new && CSS_STRUCT_IS_OPEN(cssh, new) )
	    (void) phg_css_open_struct(cssh, ids->new_id);
    } else {
	/* original structure did not exist, so either empty out the new one
	 * if it exists, or create an empty one
	 */
	if (new)
	    CSS_EMPTY_STRUCT(cssh, new->struct_id)
	else {
	    CSS_ADD_NEW_STRUCT(cssh, ids->new_id, structp)
	}
    }
    return(cssh->ws_list->wsh ? cssh->ws_list : NULL);
}

/*******************

    phg_css_change_struct_refs - Change structure references.
			         Return a list of affected workstations (the
			         combined ws_appear_on lists of all structures
				 that refer to orig).

*******************/

Css_ws_list
phg_css_change_struct_refs(cssh, ids, orig, new)
Css_handle		cssh;
Phg_args_change_struct	*ids;
register Struct_handle	orig;
register Struct_handle	new;
{
    register Struct_handle	structp;
    register Css_ws_list	wsnext;
    Css_ws_list			wssave = NULL;
    int				was_posted;

    cssh->ws_list->wsh = NULL;
    if (orig && orig->refer_to_me->num_elements) {
	/* nothing to do if no references to orig */
	if (!new)
	    CSS_ADD_NEW_STRUCT(cssh, ids->new_id, new)
	if ( !phg_css_join_ws_list(cssh, orig, (Struct_handle)NULL,
		&cssh->ws_list, CSS_WS_APPEAR) )
	    return(NULL);				/* out of memory */
	/* add orig's posted_to and appear_on lists to new, then delete */
	if (wsnext = orig->ws_posted_to) {
	    while (wsnext->wsh) {
		if ( !phg_css_ws_posted(new, wsnext->wsh) ) {
		    if ( !phg_css_post(cssh, new->struct_id, wsnext->wsh, 
				&was_posted) )
			return(NULL);			/* out of memory */
		    structp = new;
		} else
		    structp = NULL;
		(*wsnext->wsh->change_posting)(wsnext->wsh, orig, structp);
		(void) phg_css_unpost(cssh, orig->struct_id, wsnext->wsh);
		/* don't need this, because unpost removes ws_list entry */
		/* wsnext++; */
	    }
	}
	/* save current orig->ws_appear_on list for rm_from_ws_appear */
	if ( !phg_css_join_ws_list(cssh, orig, (Struct_handle)NULL,
		&wssave, CSS_WS_APPEAR) )
	    return(NULL);				/* out of memory */
	if (!phg_css_add_to_ws_appear(cssh, new, orig->ws_appear_on, 1)) {
	    ERR_BUF(cssh->erh, ERR901);
	    return(NULL);				/* out of memory */
	}
	if (wssave) {
	    phg_css_rm_from_ws_appear(cssh, orig, wssave, 1);
	    free((char *)wssave);
	}
	if ( !css_change_ref_structp(orig, new) ) {
	    ERR_BUF(cssh->erh, ERR901);
	    return(NULL);				/* out of memory */
	}
    }
    return(cssh->ws_list->wsh ? cssh->ws_list : NULL);
}

/*******************

    phg_css_change_struct_idrefs - Change structure id and references.
			           Return a list of affected workstations (the
			           ws_appear_on list of the structure new).

*******************/

Css_ws_list
phg_css_change_struct_idrefs(cssh, ids, orig, new)
Css_handle		cssh;
Phg_args_change_struct	*ids;
register Struct_handle	orig;
register Struct_handle	new;
{
    Struct_handle		structp;
    register Css_ws_list	wsnext;

    cssh->ws_list->wsh = NULL;
    if (!phg_css_join_ws_list(cssh, (Struct_handle)NULL, new,
	    &cssh->ws_list, CSS_WS_APPEAR))
	return(NULL);					/* out of memory */
    if (orig) {
	/* remove from old location in structure table */
	(void) phg_css_stab_delete(cssh->stab, ids->orig_id);
	if (new) {
	    /* merge the two posted_to and appear_on lists */
	    if (!phg_css_join_ws_list(cssh, (Struct_handle)NULL, new,
		    &orig->ws_posted_to, CSS_WS_POST))
		return(NULL);				/* out of memory */
            if (!phg_css_add_to_ws_appear(cssh, orig, new->ws_appear_on, 1)) {
                ERR_BUF(cssh->erh, ERR901);
                return(NULL);                           /* out of memory */
            }
	    /* tell ws about changed structure pointer for new */
	    if (wsnext = new->ws_posted_to)
		while (wsnext->wsh) {
		    (*wsnext->wsh->change_posting)(wsnext->wsh, new, orig);
		    wsnext++;
		}
	    /* if there are references to new, copy them before destroying it */
	    if ( !css_change_ref_structp(new, orig) ) {
		ERR_BUF(cssh->erh, ERR901);
		return(NULL);                           /* out of memory */
	    }
	    /* empty the structure and delete it from the structure table */
	    CSS_EMPTY_STRUCT(cssh, ids->new_id)
	    (void) phg_css_stab_delete(cssh->stab, ids->new_id);
	    css_struct_free(cssh, new);
	}
	/* now move to new location in struct table */
	orig->struct_id = ids->new_id;
	if ( !(phg_css_stab_insert(cssh->stab, ids->new_id, orig)) ) {
	    ERR_BUF(cssh->erh, ERR901);
	    return(NULL);				/* out of memory */
	}
	/* if original struct was open, "re-create" it empty by opening it */
	if ( CSS_STRUCT_IS_OPEN(cssh, orig) )
	    (void) phg_css_open_struct(cssh, ids->orig_id);
	/* if the "new" structure was open, re-open it with the correct ptr */
	else if ( CSS_STRUCT_IS_OPEN(cssh, new) )
	    (void) phg_css_open_struct(cssh, ids->new_id);
    } else {
	/* original structure did not exist, so either empty out the new one
	 * if it exists, or create an empty one
	 */
	if (new)
	    CSS_EMPTY_STRUCT(cssh, new->struct_id)
	else {
	    CSS_ADD_NEW_STRUCT(cssh, ids->new_id, structp)
	}
    }
    return(cssh->ws_list->wsh ? cssh->ws_list : NULL);
}

/*******************

    css_change_ref_structp - Change the structure pointers in the i_refer_to
			     lists and exec struct elements of all structures
			     referencing oldref to reference newref, and
			     change newref's refer_to_me list accordingly.
			     Empty oldref's refer_to_me list.
			     Return TRUE if successful, FALSE if malloc
			     failed.

*******************/

static int
css_change_ref_structp(oldref, newref)
register Struct_handle oldref, newref;
{
    register Struct_handle	structp;
    Css_set_ptr			old_el, new_el;
    register Css_set_element	*structel, *ref;
    int				count;

    ref = oldref->refer_to_me->elements->next;
    while (ref) {
	structp = (Struct_handle)ref->key;
	if ( !phg_css_set_element_of(structp->i_refer_to, (caddr_t)newref,
		(caddr_t*)&new_el) ) {
	    /* struct doesn't also reference newref, so create a new set */
	    if ( !(new_el = phg_css_set_create(SET_DATA_SET)) )
		return(FALSE);				/* out of memory */
	    if ( !phg_css_set_add(structp->i_refer_to, (caddr_t)newref,
		    (caddr_t)new_el) )
		return(FALSE);				/* out of memory */
	}
	(void) phg_css_set_element_of(structp->i_refer_to, (caddr_t)oldref,
	    (caddr_t*)&old_el);
	(void) phg_css_set_remove(structp->i_refer_to, (caddr_t)oldref);
	structel = old_el->elements->next;
	while (structel) {
	    /* change the struct ptrs in the ref lists and struct elements */
	    (void) phg_css_set_add(new_el, structel->key, (caddr_t)NULL);
	    ((El_handle)structel->key)->eldata.ptr = (char *)newref;
	    structel = structel->next;
	}
	/* have to increment ref->data, not replace */
	if ( !phg_css_set_element_of(newref->refer_to_me,
		(caddr_t)ref->key, (caddr_t*)&count) )
	    count = 0;
	if ( !phg_css_set_add(newref->refer_to_me, ref->key,
	      (caddr_t)(count + old_el->num_elements)) )
	    return(FALSE);				/* out of memory */
	free((char *)old_el);
	ref = ref->next;
    }
    phg_css_set_empty(oldref->refer_to_me);
    return(TRUE);
}

/* 
 * Structure Format:
 *  A structure contains a list of elements and sets of information about
 * where the structure appears. The elements are in a doubly linked list
 * with marker nodes that denote the beginning and end of the list:
 * 
 * 		    start node:		      end node:
 *      first_el-> |-------------|<--|  ---->|-------------|<--last_el
 * 		   | op: PELEM_NIL |   |  |    | op: PELEM_NIL |
 * 		   | prev:       |   ---+----|-prev:       |
 * 		   | next:-------|------|    | next:       |
 * 		   |-------------|	     |-------------|
 * 
 *  "first_el" and "last_el" are fields in the ``Css_ssl'' struct.
 * 
 * New elements are placed between the start node and the end node.
 * 
 */

/*******************

    phg_css_create_struct - Create a structure with the given id, initialise
			    the fields. 
			    Return a pointer to the structure if successful,
			    otherwise return NULL (malloc failure).

*******************/

Struct_handle
phg_css_create_struct(id)
Pint id;
{
    register Struct_handle	s;
    register Css_set_ptr	set;
    register El_handle		el;
    ALLOC_DECLARE(5);
    
    if ( !ALLOCATED(s = (Struct_handle) malloc(sizeof(Css_ssl))) )
        return(NULL);					/* out of memory */
    s->ws_posted_to = NULL;
    s->ws_appear_on = NULL;

    if ( !(set = phg_css_set_create(SET_DATA_SET)) ) {
	ALLOC_FREE;
	return(NULL);					/* out of memory */
    }
    s->refer_to_me = set;
    if ( !(set = phg_css_set_create(SET_DATA_SET_OF_SETS)) ) {
	phg_css_set_free(s->refer_to_me);
	ALLOC_FREE;
	return(NULL);					/* out of memory */
    }
    s->i_refer_to = set;
    
    /* set up 1st and last dummy elements that delimit list */
    if ( !ALLOCATED(el = (El_handle) malloc(sizeof(Css_structel))) ) {
	phg_css_set_free(s->refer_to_me);
	phg_css_set_recursive_free(s->i_refer_to);
	ALLOC_FREE;
	return(NULL);					/* out of memory */
    }
    el->eltype = PELEM_NIL;
    el->prev = NULL;
    s->first_el = el;
    if ( !ALLOCATED(el = (El_handle) malloc(sizeof(Css_structel))) ) {
	phg_css_set_free(s->refer_to_me);
	phg_css_set_recursive_free(s->i_refer_to);
	ALLOC_FREE;
	return(NULL);					/* out of memory */
    }
    el->eltype = PELEM_NIL;
    el->next = NULL;
    s->first_el->next = el;
    el->prev = s->first_el;
    s->last_el = el;

    s->struct_id = id;
    s->num_el = 0;

    return(s);
}

/*******************

    css_struct_free - free all data used by this structure

*******************/

static void
css_struct_free(cssh, structp)
register Css_handle cssh;
register Struct_handle structp;
{
    register El_handle elptr, elfree;

    elptr = structp->first_el;
    while (elptr) {
	(void) (*cssh->el_funcs[(int)elptr->eltype]) 
	    (cssh, elptr, NULL, CSS_EL_FREE);
	elfree = elptr;
	elptr = elptr->next;
	free((char *)elfree);
    }
    phg_css_set_free(structp->refer_to_me);
    phg_css_set_recursive_free(structp->i_refer_to);
    if (structp->ws_posted_to)
	free((char *)structp->ws_posted_to);
    if (structp->ws_appear_on)
	free((char *)structp->ws_appear_on);
    free((char *)structp);
}
