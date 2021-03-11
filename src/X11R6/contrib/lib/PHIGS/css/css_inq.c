/* $XConsortium: css_inq.c,v 5.6 94/04/17 20:42:41 eswu Exp $ */

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

static int css_inq_sort();
static int css_inq_descendants();
static int css_inq_ancestors();
static int css_add_to_allpaths();
static int css_path_unique();

/*******************

    phg_css_inq_el_content - Return the contents of the specified element in
			     the specified structure if elnum >= 0; otherwise 
			     return the contents of the current element in the
			     open structure.

*******************/

void
phg_css_inq_el_content(cssh, structid, elnum, ret)
Css_handle	cssh;
Pint		structid;
register Pint	elnum;
Phg_ret		*ret;
{
    Struct_handle		structp;
    register El_handle		elptr;

    if (elnum >= 0) {
	if ( !(structp = CSS_STRUCT_EXISTS(cssh, structid)) ) {
	    ret->err = ERR201;
	    return;				 /* structure does not exist */
	}
	if (elnum > structp->num_el) {
	    ret->err = ERR202;
	    return;				 /* element does not exist */
	}
	elptr = structp->first_el;
	while (elnum--)
	    elptr = elptr->next;
    } else 
	/* inquire current element content */
	elptr = cssh->el_ptr;

    ret->data.el_info.op = elptr->eltype;
    if ( !((*cssh->el_funcs[(int)elptr->eltype]) 
	    (cssh, elptr, (caddr_t)&ret->data.el_info, CSS_EL_INQ_CONTENT)) )
	ret->err = ERR900;			/* out of memory */
}

/*******************

    phg_css_inq_el_type_size - Return the type and size of the specified 
			       element in the specified structure if elnum >= 0;
			       otherwise return the type and size of the current
			       element in the open structure.

*******************/

void
phg_css_inq_el_type_size(cssh, structid, elnum, ret)
Css_handle	cssh;
Pint		structid;
register Pint	elnum;
Phg_ret		*ret;
{
    Struct_handle		structp;
    register El_handle		elptr;

    if (elnum >= 0) {
	if ( !(structp = CSS_STRUCT_EXISTS(cssh, structid)) ) {
	    ret->err = ERR201;
	    return;				 /* structure does not exist */
	}
	if (elnum > structp->num_el) {
	    ret->err = ERR202;
	    return;				 /* element does not exist */
	}
	elptr = structp->first_el;
	while (elnum--)
	    elptr = elptr->next;
    } else 
	/* inquire current element content */
	elptr = cssh->el_ptr;

    (void) (*cssh->el_funcs[(int)elptr->eltype]) 
	 (cssh, elptr, (caddr_t)&ret->data.el_type_size.size,
	     CSS_EL_INQ_TYPE_SIZE);
    ret->data.el_type_size.type = elptr->eltype;
}

/*******************

    phg_css_inq_struct_status - Return structure status as one of:
				existing/empty,
				existing/non-empty,
				non-existent.

*******************/

void
phg_css_inq_struct_status(cssh, structid, ret)
Css_handle	cssh;
Pint		structid;
Phg_ret		*ret;
{
    Struct_handle structp;

    if ( structp = CSS_STRUCT_EXISTS(cssh, structid) )
	ret->data.idata = (Pint) (structp->num_el ? 
				   PSTRUCT_STATUS_NOT_EMPTY : PSTRUCT_STATUS_EMPTY);
    else
	ret->data.idata = (Pint) PSTRUCT_STATUS_NON_EXISTENT;
}

/*******************

    phg_css_inq_struct_ids - Return a list of structure ids in use.

*******************/

void
phg_css_inq_struct_ids(cssh, ret)
register Css_handle	cssh;
Phg_ret			*ret;
{
    register Css_hash_block	**stab_row, *block;
    register int		n;
    register int		*ids;

    ret->data.int_list.num_ints = n = cssh->stab->nstructs;
    CSS_MEM_BLOCK(cssh, n*sizeof(Pint), ids, Pint)
    if (!ids) {
	ret->err = ERR900;
	return;						/* out of memory */
    }
    ret->data.int_list.ints = ids;
    stab_row = cssh->stab->table;
    while (n) {
	block = (*stab_row)->next;
	while (block) {
	    *ids++ = block->struct_id;
	    block = block->next;
	    n--;
	}
	if (n)
	    /* in case we are at the last row of the table */
	    stab_row++;
    }
}

#ifdef ARCHIVE
/*******************

    phg_css_inq_conf - Return a list of conflicting structure ids in css and
		       specified archive.
		       This routine just constructs lists of approriate
		       structure ids as directed by its arguments, and then
		       calls phg_css_get_conf to do the work of comparing the
		       lists.

*******************/

void
phg_css_inq_conf(cssh, arh, opcode, structid, netsrc, ret)
register Css_handle	cssh;
Ar_handle		arh;
Phg_args_conf_op	opcode;
Pint			structid;
Pstructnetsrc		netsrc;
Phg_ret			*ret;
{

    Phg_ret		cssids, arids;
    Struct_handle	structp;

    switch (opcode) {

      case PHG_ARGS_CONF_ALL:
	cssids.err = 0;
	phg_css_inq_struct_ids(cssh, &cssids);
	if (cssids.err) {
	    ret->err = cssids.err;
	    return;					/* out of memory */
	}
	arids.err = 0;
	phg_css_inq_struct_ids(arh->ssh, &arids);
	if (arids.err) {
	    ret->err = arids.err;
	    return;					/* out of memory */
	}
	break;

      case PHG_ARGS_CONF_NET:
	if (netsrc == PNET_CSS) {
	    if ( !(structp = CSS_STRUCT_EXISTS(cssh, structid)) ) {
		ret->err = ERR201;		/* non-existent structure */
		return;
	    }
	    if ( !phg_ar_get_network(cssh, structp, &cssids.data.int_list) ) {
		ret->err = ERR900;
		return;					/* out of memory */
	    }
	    arids.err = 0;
	    phg_css_inq_struct_ids(arh->ssh, &arids);
	    if (arids.err) {
		ret->err = arids.err;
		return;					/* out of memory */
	    }
	} else {	/* netsrc == PNET_ARCHIVE */
	    if ( !(structp = CSS_STRUCT_EXISTS(arh->ssh, structid)) ) {
		ret->err = ERR201;		/* non-existent structure */
		return;
	    }
	    if ( !phg_ar_get_network(arh->ssh, structp, &arids.data.int_list)) {
		ret->err = ERR900;
		return;					/* out of memory */
	    }
	    cssids.err = 0;
	    phg_css_inq_struct_ids(cssh, &cssids);
	    if (cssids.err) {
		ret->err = cssids.err;
		return;					/* out of memory */
	    }
	}
	break;
    }
    (void) phg_css_get_conf(&cssids.data.int_list, &arids.data.int_list, 
		            &ret->data.int_list);
}
#endif /* ARCHIVE */

/*******************

    phg_css_get_conf - Compare the two lists of structure ids to determine
		       whether there are any conflicts. If conflist is non-NULL,
		       return the list of conflicting structures (empty if
		       there are none); if conflist is NULL, return (a value of
		       1) as soon as one conflict is found, return 0 if no 
		       conflicts are found.

*******************/

int
phg_css_get_conf(csslist, arlist, conflist)
Pint_list	*csslist, *arlist, *conflist;
{
    register int	*arptr, *cssptr;
    register int	*cssend, *arend;
    register int	numconf = 0;

    cssptr = csslist->ints;
    arptr = arlist->ints;
    if ( !(csslist->num_ints && arlist->num_ints) ) {
	if (conflist)
	    conflist->num_ints = 0;
	return(0);			/* empty list, so no conflicts */
    }
    qsort((char *)cssptr, csslist->num_ints, sizeof(int), css_inq_sort);
    qsort((char *)arptr, arlist->num_ints, sizeof(int), css_inq_sort);
    /* use the same memory block as the Pintlst for the cssh; the loop
     * below won't overwrite anything that hasn't already been compared 
     */
    if (conflist)
	conflist->ints = cssptr;

    /* now loop through the two lists; loop inner while < the current outer
     * loop element, then see if equal or greater than */
    cssend = cssptr + csslist->num_ints;
    arend = arptr + arlist->num_ints;
    while (cssptr < cssend) {
	while (*arptr<*cssptr && ++arptr!=arend) ;
	if (arptr == arend)
	    break;
	if (*arptr == *cssptr)
	    if (conflist)
		conflist->ints[numconf++] = *arptr;
	    else
		return(1);
	cssptr++;
    }
    if (conflist)
	conflist->num_ints = numconf;
    return(numconf);
}

static int
#ifdef __STDC__
css_inq_sort(n1_param, n2_param)
register const void *n1_param, *n2_param;
{
    register const int *n1 = (int *) n1_param;
    register const int *n2 = (int *) n2_param;
#else
css_inq_sort(n1, n2)
register int *n1, *n2;
{
#endif
    if (*n1 < *n2 )
       	return(-1);
    else if (*n1 == *n2)
    	return(0);
    else /* if (n1 > n2) */
    	return(1);
}

/*******************

    phg_css_inq_hierarchy - Return hierarchy (descendants or ancestors)
			    of specified structure in css.

*******************/

/* size of memory block increments for curpath.elem_refs and counts.ints */
#define TMPMEM_BLOCKSIZE	20			/* 20 elements */

#define CHECK_TMPMEM_BLOCKSIZE(blockptr, blocktype, els_used) \
    if ( (els_used) && !( (els_used)%TMPMEM_BLOCKSIZE) ) { \
	/* get more space */ \
	(blockptr) = (blocktype *) realloc((char *)(blockptr), \
	    (unsigned)((els_used + TMPMEM_BLOCKSIZE) * sizeof(blocktype))); \
	if (!(blockptr)) \
	    return(FALSE);			/* out of memory */ \
    }


void
phg_css_inq_hierarchy(ssh, dir, structid, order, depth, ret)
register Css_handle	ssh;
Phg_args_hierarchy_dir	dir;
Pint			structid;
Ppath_order		order;
Pint			depth;
register Phg_ret	*ret;
{
    Struct_handle	structp;
    Pelem_ref_list	allpaths, curpath;
    Pint_list		counts;
    int			retval = TRUE;
    ALLOC_DECLARE(5);

    if ( !(structp = CSS_STRUCT_EXISTS(ssh, structid)) ) {
	ret->err = ERR201;			/* non-existent structure */
	return;
    }
    /* take the whole thing, use realloc later if it's not big enough */
    CSS_MEM_BLOCK(ssh, ssh->mem_size, allpaths.elem_refs, Pelem_ref)
    if (!allpaths.elem_refs) {
	ret->err = ERR900;
	return;						/* out of memory */
    }
    /* start out with room for a current path depth of TMPMEM_BLOCKSIZE and a 
     * total of TMPMEM_BLOCKSIZE paths
     */
    curpath.elem_refs = (Pelem_ref *)
	malloc((unsigned)(TMPMEM_BLOCKSIZE * sizeof(Pelem_ref)));
    if (!ALLOCATED(curpath.elem_refs)) {
	ret->err = ERR900;
	return;						/* out of memory */
    }
    counts.ints = (Pint *)
	malloc((unsigned)(TMPMEM_BLOCKSIZE * sizeof(Pint)));
    if (!ALLOCATED(counts.ints)) {
	ALLOC_FREE;
	ret->err = ERR900;
	return;						/* out of memory */
    }
    /* Realloc isn't called above this point, so ALLOC_FREE is fine. */

    allpaths.num_elem_refs = curpath.num_elem_refs = counts.num_ints = 0;
    if (dir == PHG_ARGS_HIER_DESCENDANTS)
	retval = css_inq_descendants(ssh, structid, &allpaths, &curpath,
	                             &counts, order, depth);
    else if (structp->refer_to_me->num_elements)
	retval = css_inq_ancestors(ssh, structp, &allpaths, &curpath, 
				   &counts, order, depth);
    if (!retval) {
	ret->err = ERR900;				/* out of memory */
	goto free_and_return;
    }
    ret->data.hierarchy.paths = allpaths.elem_refs;
    ret->data.hierarchy.num_pairs = allpaths.num_elem_refs;
    ret->data.hierarchy.counts.num_ints = counts.num_ints;
    /* make sure ssh->mem has enough space before copying counts array */
    if (allpaths.num_elem_refs*sizeof(Pelem_ref) + counts.num_ints*sizeof(Pint) >
	    ssh->mem_size) {
	int increm = counts.num_ints*sizeof(Pint);

	ssh->mem = realloc(ssh->mem, (unsigned)(ssh->mem_size+increm));
	if (ssh->mem) {
	    ssh->mem_size += increm;
	    allpaths.elem_refs = (Pelem_ref *)ssh->mem;
	} else {
	    ret->err = ERR900;				/* out of memory */
	    goto free_and_return;	
	}
    }
    ret->data.hierarchy.counts.ints = 
	(Pint *)(allpaths.elem_refs + allpaths.num_elem_refs);
    /* copy counts.ints data to ssh->mem area because cp won't free it */
    while (counts.num_ints--)
	ret->data.hierarchy.counts.ints[counts.num_ints] = 
	    counts.ints[counts.num_ints]; 
free_and_return:
    /* Can't use ALLOC_FREE because realloc may have been called, and may have
     * freed the address malloc'd above, and saved by ALLOCATED.
     */
    free((char*)curpath.elem_refs);
    free((char*)counts.ints);
}

static int
css_inq_descendants(ssh, structid, allpaths, curpath, counts, order, depth)
register Css_handle 	ssh;
register Pint	 	structid;
Pelem_ref_list		*allpaths;
register Pelem_ref_list	*curpath;
Pint_list 		*counts;
Ppath_order		order;
Pint			depth;
{
    register Struct_handle	structp;
    register El_handle		elptr;
    /*register*/ int		execid, elnum;
    char 			leafnode = TRUE;
    int				retval;

    /* if structid doesn't exist, it must be a leaf node in an archive,
     * so skip the structure element processing, but be sure to add 
     * (structid, 0) to curpath
     */
    if ( (structp=CSS_STRUCT_EXISTS(ssh, structid)) &&
	 (order==PORDER_BOTTOM_FIRST || !depth || curpath->num_elem_refs!=depth) ) {
	elptr = structp->first_el->next;
	elnum = 1;
	while (elptr != structp->last_el) {
	    if (elptr->eltype == PELEM_EXEC_STRUCT) {
		execid = (ssh->ssh_type == SSH_AR) ? elptr->eldata.idata :
		    ((Struct_handle)elptr->eldata.ptr)->struct_id;
		CHECK_TMPMEM_BLOCKSIZE(curpath->elem_refs, Pelem_ref,
		    curpath->num_elem_refs)
		curpath->elem_refs[curpath->num_elem_refs].struct_id = structid;
		curpath->elem_refs[curpath->num_elem_refs++].elem_pos = elnum;
		if ( !css_inq_descendants(ssh, execid, allpaths, curpath,
			counts, order, depth) )
		    return(FALSE);			/* out of memory */
		leafnode = FALSE;
		curpath->num_elem_refs--;
	    }
	    elptr = elptr->next;
	    elnum++;
	    assure(elnum <= structp->num_el + 1)
	    if ( !(elnum <= structp->num_el + 1) )
		break;
	}
    }
    if (leafnode && curpath->num_elem_refs) {
	CHECK_TMPMEM_BLOCKSIZE(curpath->elem_refs, Pelem_ref,
	    curpath->num_elem_refs)
	curpath->elem_refs[curpath->num_elem_refs].struct_id = structid;
	curpath->elem_refs[curpath->num_elem_refs++].elem_pos = 0;
        if (order==PORDER_BOTTOM_FIRST && depth && curpath->num_elem_refs>depth &&
            !css_path_unique(allpaths, curpath, counts, order, depth) )
            /* if path is bottom first and has to be truncated to depth, don't
             * add it to allpaths unless it is unique
             */
            retval = TRUE;
	else retval = 
	    css_add_to_allpaths(ssh, allpaths, curpath, counts, order, depth);
	curpath->num_elem_refs--;
    } else
	retval = TRUE;
    return(retval);
}

static int
css_inq_ancestors(ssh, structp, allpaths, curpath, counts, order, depth)
Css_handle	 		ssh;
Struct_handle	 		structp;
Pelem_ref_list		*allpaths;
register Pelem_ref_list	*curpath;
Pint_list 		*counts;
Ppath_order		order;
Pint			depth;
{
    register Struct_handle	parentp;
    register Css_set_element	*el;
    register El_handle		elptr;
    register int		nrefs, elnum;

    if (!curpath->num_elem_refs) {
	/* start out with the (structid, 0) entry */
	CHECK_TMPMEM_BLOCKSIZE(curpath->elem_refs, Pelem_ref,
	    curpath->num_elem_refs)
	curpath->elem_refs[curpath->num_elem_refs].struct_id = structp->struct_id;
	curpath->elem_refs[curpath->num_elem_refs++].elem_pos = 0;
    }
    if (!structp->refer_to_me->num_elements || 
	 (order==PORDER_BOTTOM_FIRST && depth && curpath->num_elem_refs==depth) ) {
	/* either found the root, or have gone far enough */

	if (order==PORDER_TOP_FIRST && depth && curpath->num_elem_refs>depth &&
	    !css_path_unique(allpaths, curpath, counts, order, depth) )
	    /* if path is top first and has to be truncated to depth, don't
	     * add it to allpaths unless it is unique
	     */
	    return(TRUE);
	return(
	    css_add_to_allpaths(ssh, allpaths, curpath, counts, order, depth) );
    }
    el = structp->refer_to_me->elements->next;
    while (el) {
	parentp = (Struct_handle)el->key;
	elptr = parentp->first_el->next;
	elnum = 1;
	nrefs = (int)el->data;
	while (nrefs--) {
	    if (ssh->ssh_type != SSH_AR)
		while ( !(elptr->eltype==PELEM_EXEC_STRUCT &&
			(Struct_handle)elptr->eldata.ptr==structp) ) {
		    elptr = elptr->next;
		    elnum++;
		}
	    else	/* archive has structids, not pointers */
		while ( !(elptr->eltype==PELEM_EXEC_STRUCT &&
			elptr->eldata.idata==structp->struct_id) ) {
		    elptr = elptr->next;
		    elnum++;
		}
	    CHECK_TMPMEM_BLOCKSIZE(curpath->elem_refs, Pelem_ref,
		curpath->num_elem_refs)
	    curpath->elem_refs[curpath->num_elem_refs].struct_id
		= parentp->struct_id;
	    curpath->elem_refs[curpath->num_elem_refs++].elem_pos = elnum;
	    if ( !css_inq_ancestors(ssh, parentp, allpaths, curpath,
		    counts, order, depth) )
		return(FALSE);			/* out of memory */
	    curpath->num_elem_refs--;
	    elptr = elptr->next;
	    elnum++;
	}
	el = el->next;
    }
    return(TRUE);
}

static int
css_add_to_allpaths(ssh, allpaths, curpath, counts, order, depth)
register Css_handle 	ssh;
register Pelem_ref_list	*allpaths, *curpath;
register Pint_list	*counts;
Ppath_order		order;
register Pint		depth;
{
    register int i;

    /* decide how much of curpath to copy, see if allpaths has enough space */
    if (!depth || curpath->num_elem_refs < depth)
	depth = curpath->num_elem_refs;
    if ((allpaths->num_elem_refs+depth)*sizeof(Pelem_ref) > ssh->mem_size) {
	ssh->mem = realloc(ssh->mem,
	    (unsigned)(ssh->mem_size+CSS_MEM_BLOCKSIZE));
	if (ssh->mem) {
	    ssh->mem_size += CSS_MEM_BLOCKSIZE;
	    allpaths->elem_refs = (Pelem_ref *)ssh->mem;
	} else
	    return(FALSE);				/* out of memory */
    }
    CHECK_TMPMEM_BLOCKSIZE(counts->ints, Pint, counts->num_ints)
    counts->ints[counts->num_ints++] = depth;
    if (curpath->elem_refs->elem_pos) {
	/* descendants, so increment */
	i = (order == PORDER_TOP_FIRST) ? 0 : curpath->num_elem_refs - depth;
	while (depth--)
	    allpaths->elem_refs[allpaths->num_elem_refs++] = curpath->elem_refs[i++];
    } else {
	/* ancestors (constructed backwards), so decrement */
	i = (order == PORDER_TOP_FIRST) ? curpath->num_elem_refs - 1 : depth - 1;
	while (depth--)
	    allpaths->elem_refs[allpaths->num_elem_refs++] = curpath->elem_refs[i--];
    }
    return(TRUE);
}

static int
css_path_unique(allpaths, newpath, counts, order, depth)
register Pelem_ref_list	*allpaths, *newpath;
register Pint_list	*counts;
Ppath_order		order;
register Pint		depth;
{
    register int	i, j, k, n;
    int			start, unique;

    /* compare newpath with each path in allpath to see whether it is unique */
    k = 0;
    if (newpath->elem_refs->elem_pos) {
	/* descendants, so increment while stepping through newpath->elem_refs */
	start = (order == PORDER_TOP_FIRST) ? 0 : newpath->num_elem_refs - depth;
	for (i = 0; i < counts->num_ints; i++) {
	    if (counts->ints[i] < depth) {
		/* no point in comparing these 2 paths */
		k += counts->ints[i];
		continue;
	    }
	    j = start;
	    n = depth;
	    unique = FALSE;
	    while (n--) {
		if (newpath->elem_refs[j].struct_id!=
			allpaths->elem_refs[k].struct_id ||
		    newpath->elem_refs[j].elem_pos!=
			allpaths->elem_refs[k].elem_pos) {
		    unique = TRUE;
		    k += n + 1;
		    break;
		}
		j++;
		k++;
	    }
	    if (!unique)
		return(FALSE); 
	}
    } else {
	/* ancestors (constructed backwards), so decrement */
	start = (order == PORDER_TOP_FIRST) ? newpath->num_elem_refs - 1 : depth - 1;
	for (i = 0; i < counts->num_ints; i++) {
	    if (counts->ints[i] < depth) {
		/* no point in comparing these 2 paths */
		k += counts->ints[i];
		continue;
	    }
	    j = start;
	    n = depth;
	    unique = FALSE;
	    while (n--) {
		if (newpath->elem_refs[j].struct_id
			!=allpaths->elem_refs[k].struct_id ||
		    newpath->elem_refs[j].elem_pos
			!=allpaths->elem_refs[k].elem_pos) {
		    unique = TRUE;
		    k += n + 1;
		    break;
		}
		j--;
		k++;
	    }
	    if (!unique)
		return(FALSE); 
	}
    }
    return(TRUE);
}

/*******************

    phg_css_inq_ws_posted - Return the list of workstations the given
			    structure is posted to.

*******************/

void
phg_css_inq_ws_posted(cssh, structid, ret)
register Css_handle	cssh;
Pint			structid;
Phg_ret			*ret;
{
    register Struct_handle	structp;
    register Css_ws_list	wsptr;

    CSS_MEM_BLOCK(cssh, MAX_NO_OPEN_WS*sizeof(Pint),
	          ret->data.int_list.ints, Pint)
    if (!ret->data.int_list.ints) {
	ret->err = ERR900;
	return;						/* out of memory */
    }
    if ( !(structp = CSS_STRUCT_EXISTS(cssh, structid)) ) {
	ret->err = ERR201;
	return;				 /* structure does not exist */
    }
    ret->data.int_list.num_ints = 0;
    if (wsptr = structp->ws_posted_to)
	while (wsptr->wsh)
	    ret->data.int_list.ints[ret->data.int_list.num_ints++] = 
		(wsptr++)->wsh->id;
}
