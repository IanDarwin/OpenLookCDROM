/* $XConsortium: ar_hier.c,v 5.4 94/04/17 20:40:38 hersh Exp $ */

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

#include <sys/types.h>
#include "phg.h"
#include "cp.h"
#include "ws.h"
#include "ar.h"
#include "PEXprotost.h"
#include "PEXproto.h"
#include "PEX.h"

static int
path_unique(allpaths, newpath, counts, order, depth)
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
		if (newpath->elem_refs[j].struct_id!=allpaths->elem_refs[k].struct_id ||
		    newpath->elem_refs[j].elem_pos!=allpaths->elem_refs[k].elem_pos) {
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
		if (newpath->elem_refs[j].struct_id!=allpaths->elem_refs[k].struct_id ||
		    newpath->elem_refs[j].elem_pos!=allpaths->elem_refs[k].elem_pos) {
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

static int
add_to_allpaths(cph, allpaths, curpath, counts, order, depth)
register Cp_handle 	cph;
register Pelem_ref_list	*allpaths, *curpath;
register Pint_list	*counts;
Ppath_order		order;
register Pint		depth;
{
    register int i;

    /* decide how much of curpath to copy, see if allpaths has enough space */
    if (!depth || curpath->num_elem_refs < depth)
	depth = curpath->num_elem_refs;
    if ((allpaths->num_elem_refs+depth)*sizeof(Pelem_ref) > cph->scratch.size) {
	cph->scratch.buf = realloc(cph->scratch.buf,cph->scratch.size + 1024);
	if (cph->scratch.buf) {
	    cph->scratch.size += 1024;
	    allpaths->elem_refs = (Pelem_ref *)cph->scratch.buf;
	} else
	    return(FALSE);				/* out of memory */
    }
    PHG_AR_CHECK_TMPMEM_BLOCKSIZE(counts->ints, Pint, counts->num_ints)
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

int
phg_ar_inq_descendants(cph, arh, structid, allpaths, 
		       curpath, counts, order, depth)
Cp_handle		cph;
Ar_handle		arh;		       
register Pint	 	structid;
Pelem_ref_list		*allpaths;
register Pelem_ref_list	*curpath;
Pint_list 		*counts;
Ppath_order		order;
Pint			depth;
{
    Phg_ar_index_entry		*entry;
    int				 execid, elnum;
    char 			 leafnode = TRUE;
    int				 retval;
    pexElementInfo		*ptr;
    caddr_t			 buffer;
    unsigned int		 buffer_size;

    buffer_size = 256;
    if (!(buffer = (caddr_t)malloc(buffer_size)))
	return(FALSE);

    /* if structid doesn't exist, it must be a leaf node in an archive,
     * so skip the structure element processing, but be sure to add 
     * (structid, 0) to curpath
     */
    if ( (entry = phg_ar_get_entry_from_archive(arh, structid)) &&
	 (order==PORDER_BOTTOM_FIRST || !depth || curpath->num_elem_refs!=depth) ) {
	
	/* grow buffer if necessary */
	if (entry->length > buffer_size) {
	    free(buffer);
	    buffer_size = entry->length;
	    if (!(buffer = (caddr_t)malloc(buffer_size)))
		return(FALSE);
	}

	if (phg_ar_read_struct_from_archive(arh, entry, buffer)) {
	    free(buffer);
	    return(FALSE);
	}
	
	ptr = (pexElementInfo *)buffer;
	for (elnum = 1; elnum <= entry->nelts; elnum++) {
	    if (ptr->elementType == PEXOCExecuteStructure) {
		execid = ((pexExecuteStructure *)(ptr))->id;
		PHG_AR_CHECK_TMPMEM_BLOCKSIZE(curpath->elem_refs, Pelem_ref,
					      curpath->num_elem_refs)
		curpath->elem_refs[curpath->num_elem_refs].struct_id = structid;
		curpath->elem_refs[curpath->num_elem_refs++].elem_pos = elnum;
		if ( !phg_ar_inq_descendants(cph, arh, execid, allpaths,
					curpath, counts, order, depth) )
		    return(FALSE);			/* out of memory */
		leafnode = FALSE;
		curpath->num_elem_refs--;
	    }
	    ptr += ptr->length * sizeof(CARD32)/sizeof(*ptr);
	}
    }
    if (leafnode && curpath->num_elem_refs) {
	PHG_AR_CHECK_TMPMEM_BLOCKSIZE(curpath->elem_refs, Pelem_ref, curpath->num_elem_refs)
	curpath->elem_refs[curpath->num_elem_refs].struct_id = structid;
	curpath->elem_refs[curpath->num_elem_refs++].elem_pos = 0;
        if (order==PORDER_BOTTOM_FIRST && depth && curpath->num_elem_refs>depth &&
            !path_unique(allpaths, curpath, counts, order, depth) )
            /* if path is bottom first and has to be truncated to depth, don't
             * add it to allpaths unless it is unique
             */
            retval = TRUE;
	else retval = 
	    add_to_allpaths(cph, allpaths, curpath, counts, order, depth);
	curpath->num_elem_refs--;
    } else
	retval = TRUE;
	
    free(buffer);
    return(retval);
}


int
phg_ar_inq_ancestors(cph, arh, structid, allpaths, curpath, 
					counts, order, depth)
Cp_handle		cph;
Ar_handle		arh;		       
Pint			structid;
Pelem_ref_list		*allpaths;
register Pelem_ref_list	*curpath;
Pint_list 		*counts;
Ppath_order		order;
Pint			depth;
{

    Phg_ar_index_entry		*entry;
    Pelem_ref_list		 parents;
    int				 ers_size, elnum, i;
    caddr_t			 buffer;
    unsigned int		 buffer_size;
    pexElementInfo		*ptr;
    
    ers_size = 10;
    parents.num_elem_refs = 0;
    if (!(parents.elem_refs = (Pelem_ref *)
			malloc((unsigned)(ers_size * sizeof(Pelem_ref)))))
				return(FALSE);

    buffer_size = 256;
    if (!(buffer = (caddr_t)malloc(buffer_size))) {
			free((char *)parents.elem_refs);
			return(FALSE);
    }

    if (!curpath->num_elem_refs) {
	/* start out with the (structid, 0) entry */
	PHG_AR_CHECK_TMPMEM_BLOCKSIZE(curpath->elem_refs, Pelem_ref, curpath->num_elem_refs)
	curpath->elem_refs[curpath->num_elem_refs].struct_id = structid;
	curpath->elem_refs[curpath->num_elem_refs++].elem_pos = 0;
    }
    
    /** First, create a list of all of the structures which refer to this
     ** structure, and at what element they do it **/

    /** TODO:  This is a very inefficient way to do this.  Basically, we
     ** are traversing the entire archive file for each and every structure
     ** in the list.  Need to come up with a better method.  Possibly
     ** construct a call graph on the zeroth recursion into this function **/

    PHG_AR_FOR_ALL_TOC_ENTRIES(arh, entry)
    
	/* grow buffer if necessary */
	if (entry->length > buffer_size) {
	    free((char *)buffer);
	    buffer_size = entry->length;
	    if (!(buffer = (caddr_t)malloc(buffer_size))) {
		free((char *)parents.elem_refs);
		return(FALSE);
	    }
	}
	
	if (phg_ar_read_struct_from_archive(arh, entry, buffer)) {
	    free((char *)buffer);
	    free((char *)parents.elem_refs);
	    return(FALSE);
	}
	
	ptr = (pexElementInfo *)buffer;
	for (elnum = 1; elnum <= entry->nelts; elnum++) {
	    if (    ptr->elementType == PEXOCExecuteStructure  &&
		    ((pexExecuteStructure *)(ptr))->id == structid) {
		if (parents.num_elem_refs > ers_size) {
		    ers_size += 10;
		    parents.elem_refs = (Pelem_ref *)realloc((char *)parents.elem_refs, 
							(int)(ers_size * sizeof(Pelem_ref)));
		    if (!parents.elem_refs) {
			free((char *)buffer);
			return(FALSE);
		    }
		}
		parents.elem_refs[parents.num_elem_refs].struct_id = entry->str;
		parents.elem_refs[parents.num_elem_refs++].elem_pos = elnum;
	    }
	    ptr += ptr->length * sizeof(CARD32)/sizeof(*ptr);
	}
	
    PHG_AR_END_FOR_ALL_TOC_ENTRIES
    
    
    if ( 
	  /* Found the root */
	  ((parents.num_elem_refs == 0) && (curpath->num_elem_refs > 1))
    
			||

	  /* Haven't found the root, but we've gone far enough */
	  ((parents.num_elem_refs != 0) && (order == PORDER_BOTTOM_FIRST) && 
				depth && (curpath->num_elem_refs == depth))
	   
			) {
	int retval;
	
	if (order == PORDER_TOP_FIRST && depth && curpath->num_elem_refs > depth &&
	    !path_unique(allpaths, curpath, counts, order, depth) )
	    /* if path is top first and has to be truncated to depth, don't
	     * add it to allpaths unless it is unique
	     */
	    goto free_and_return;

	retval = add_to_allpaths(cph, allpaths, curpath, counts, order, depth);
	free((char *)buffer);
	free((char *)parents.elem_refs);
	return(retval);
    }
    
    for (i = 0; i < parents.num_elem_refs; i++) {
	PHG_AR_CHECK_TMPMEM_BLOCKSIZE(curpath->elem_refs, Pelem_ref, curpath->num_elem_refs);
	curpath->elem_refs[curpath->num_elem_refs].struct_id = parents.elem_refs[i].struct_id;
	curpath->elem_refs[curpath->num_elem_refs++].elem_pos = parents.elem_refs[i].elem_pos;
	if ( !phg_ar_inq_ancestors(cph, arh, parents.elem_refs[i].struct_id,
		    allpaths, curpath, counts, order, depth) )
		return(FALSE);			/* out of memory */
	curpath->num_elem_refs--;
    }
    

free_and_return:
    free((char *)buffer);
    free((char *)parents.elem_refs);

    return(TRUE);
    
}

