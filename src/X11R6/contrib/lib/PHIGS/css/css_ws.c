/* $XConsortium: css_ws.c,v 5.2 94/04/17 20:42:46 rws Exp $ */

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
#include "css.h"
#include "css_priv.h"

#define ADD_TO_WS_LIST(cssh, wslist, wshdl, n, wspost) \
  { register Css_ws_list wsptr; \
    \
    if (!(wslist)) { \
	if ( !((wslist) = (Css_ws_list) \
		malloc((MAX_NO_OPEN_WS+1) * sizeof(Css_ws_on))) ) { \
	    ERR_BUF((cssh)->erh, ERR901); \
	    return(0);					/* out of memory */ \
	} \
	(wslist)->wsh = NULL; \
    } \
    wsptr = (wslist); \
    while (wsptr->wsh && wsptr->wsh!=(wshdl)) \
	wsptr++; \
    if (wsptr->wsh) \
	/* don't increment the count if this is the ws_posted_to list */ \
	if ((wspost)) \
	    (wspost) = FALSE; \
	else \
	    wsptr->count += (n); \
    else { \
	/* this ws not already in the list, so add it */ \
	wsptr->wsh = (wshdl); \
	wsptr->count = (n); \
	(wsptr+1)->wsh = NULL; \
    } \
  }

#define RM_FROM_WS_LIST(wslist, wshdl, n) \
  { register Css_ws_list wsptr; \
    \
    if (wslist) { \
	wsptr = (wslist); \
	while (wsptr->wsh && wsptr->wsh!=(wshdl)) \
	    wsptr++; \
	if (wsptr->wsh) { \
	    if (n) \
		wsptr->count -= (n); \
	    else \
		wsptr->count = 0; \
	    if ( !wsptr->count ) \
		while (wsptr->wsh) { \
		    wsptr->wsh = (wsptr+1)->wsh; \
		    wsptr->count = (wsptr+1)->count; \
		    wsptr++; \
		} \
	    if (!wslist->wsh) { \
		free((char *)(wslist)); \
		wslist = NULL; \
	    } \
	} \
    } \
  }

/*******************

    phg_css_post - "Post" the given structure by adding wsh to its list of
		   workstations it is posted to and appears on. Also add wsh
		   to list of workstations appearing on for all descendants
		   of the structure.

*******************/

Struct_handle
phg_css_post(cssh, structid, wsh, was_posted)
Css_handle		cssh;
Pint			structid;
register Ws_handle	wsh;
int			*was_posted;
{
    register Struct_handle 	structp;
    Css_ws_on			addlist[2];
    char 			postflag;

    *was_posted = FALSE;
    if ( !(structp = CSS_STRUCT_EXISTS(cssh, structid)) ) {
	/* create an empty one */
	if ( !(structp = phg_css_create_struct(structid)) ) {
	    ERR_BUF(cssh->erh, ERR901);
	    return(NULL);				/* out of memory */
	}
	if ( !phg_css_stab_insert(cssh->stab, structid, structp) ) {
	    ERR_BUF(cssh->erh, ERR901);
	    return(NULL);				/* out of memory */
	}
    }
    postflag = TRUE;
    ADD_TO_WS_LIST(cssh, structp->ws_posted_to, wsh, 1, postflag)
    /* if postflag has been negated, struct is already posted - nothing to do */
    if (postflag) {
	addlist[0].wsh = wsh;
	addlist[0].count = 1;
	addlist[1].wsh = NULL;
	if ( !phg_css_add_to_ws_appear(cssh, structp, addlist, 1) )
	    return(NULL);				/* out of memory */
    } else
	*was_posted = TRUE;

    return(structp);
}

/*******************

    phg_css_unpost - "Unpost" the given structure by removing wsh from its list
		     of workstations it is posted to and appears on. Also remove
		     from list of workstations appearing on for all descendants
		     of the structure.

*******************/

Struct_handle
phg_css_unpost(cssh, structid, wsh)
Css_handle		cssh;
Pint			structid;
register Ws_handle	wsh;
{
    register Struct_handle 	structp;
    Css_ws_on			rmlist[2];

    if ( !(structp = CSS_STRUCT_EXISTS(cssh, structid)) )
	return(NULL);
    RM_FROM_WS_LIST(structp->ws_posted_to, wsh, 1)

    rmlist[0].wsh = wsh;
    rmlist[0].count = 1;
    rmlist[1].wsh = NULL;
    phg_css_rm_from_ws_appear(cssh, structp, rmlist, 1);
    return(structp);
}

/*******************

    phg_css_unpost_all - "Unpost" all structures posted to and appearing on
			 the given workstation, by removing wsh from all
			 posted to and appearing on lists.

*******************/

void
phg_css_unpost_all(cssh, wsh)
Css_handle		cssh;
register Ws_handle	wsh;
{
    register Css_hash_block	**stab_row, *block;
    register int		n;

    n = cssh->stab->nstructs;
    stab_row = cssh->stab->table;
    while (n) {
	block = (*stab_row)->next;
	while (block) {
	    RM_FROM_WS_LIST(block->struct_ptr->ws_posted_to, wsh, 1)
	    /* 0 tells RM_FROM_WS_LIST to zero the count */
	    RM_FROM_WS_LIST(block->struct_ptr->ws_appear_on, wsh, 0)
	    block = block->next;
	    n--;
	}
	if (n)
	    /* in case we are at the last row of the table */
	    stab_row++;
    }
}

/*******************

    phg_css_add_to_ws_appear - Add the given list of workstations to the
			       executed structure and all of its descendants.

*******************/

int
phg_css_add_to_ws_appear(cssh, execp, addlist, nexec)
Css_handle		cssh;
register Struct_handle 	execp;
Css_ws_list		addlist;
int			nexec;
{
    register Css_ws_list	wsnext;
    register Css_set_element	*el;
    char 			postflag = FALSE;

    if ( !(wsnext = addlist) )
	return(TRUE);					/* nothing to do */
    /* combine the 2 ws_appear_on lists for execp and its descendants */
    while (wsnext->wsh) {
	ADD_TO_WS_LIST(cssh, execp->ws_appear_on, wsnext->wsh, 
	    wsnext->count*nexec, postflag)
	wsnext++;
    }
    el = execp->i_refer_to->elements->next;
    while (el) {
	if ( !phg_css_add_to_ws_appear(cssh, (Struct_handle)el->key, 
		addlist, ((Css_set_ptr)el->data)->num_elements*nexec) )
	    return(FALSE);				/* out of memory */
	el = el->next;
    }
    return(TRUE);
}

/*******************

    phg_css_rm_from_ws_appear - Remove the given list of workstations from
				the executed structure and all of its
				descendants.

*******************/

void
phg_css_rm_from_ws_appear(cssh, execp, rmlist, nexec)
Css_handle		cssh;
register Struct_handle 	execp;
Css_ws_list		rmlist;
int			nexec;
{
    register Css_ws_list	wsnext;
    register Css_set_element	*el;

    if ( !(wsnext = rmlist) )
	return;						/* nothing to do */
    while (wsnext->wsh) {
	RM_FROM_WS_LIST( execp->ws_appear_on, wsnext->wsh, wsnext->count*nexec)
	wsnext++;
    }
    el = execp->i_refer_to->elements->next;
    while (el) {
	phg_css_rm_from_ws_appear(cssh, (Struct_handle)el->key, 
	    rmlist, ((Css_set_ptr)el->data)->num_elements*nexec);
	el = el->next;
    }
}

/*******************

    phg_css_join_ws_list - Combine the lists of workstations that the
			   structures s1 and s2 appear on or are posted to
			   (according to the list_op argument), returning the
			   result in newlist. If newlist is NULL, space will
			   be allocated for a new list, otherwise s1 and s2's
			   lists will be added in to newlist.

*******************/

int
phg_css_join_ws_list(cssh, s1, s2, newlist, op)
Css_handle		cssh;
register Struct_handle 	s1, s2;
register Css_ws_list	*newlist;
Css_ws_list_op		op;
{
    register Css_ws_list 	wsnext;
    char 			postflag = FALSE;

    if (s1 && (wsnext = 
	    (op==CSS_WS_APPEAR ? s1->ws_appear_on : s1->ws_posted_to)) ) {
	while (wsnext->wsh) {
	    ADD_TO_WS_LIST(cssh, *newlist, wsnext->wsh, wsnext->count, postflag)
	    wsnext++;
	}
    }
    if (s2 && (wsnext = 
	    (op==CSS_WS_APPEAR ? s2->ws_appear_on : s2->ws_posted_to)) ) {
	while (wsnext->wsh) {
	    ADD_TO_WS_LIST(cssh, *newlist, wsnext->wsh, wsnext->count, postflag)
	    wsnext++;
	}
    }
    return(TRUE);
}

/*******************

    phg_css_copy_ws_lists - Copy the ws_posted to and ws_appear_on lists
			    from structure "from" to structure "to",
			    allocating space for the list(s) if necessary.
			    If op is not CSS_WS_BOTH, only copy the list
			    specified.

*******************/

int
phg_css_copy_ws_lists(cssh, from, to, op)
Css_handle		cssh;
register Struct_handle	from, to;
Css_ws_list_op		op;
{
    register int i;
   
    if ( (op==CSS_WS_POST || op==CSS_WS_BOTH) && from->ws_posted_to) {
	if (!to->ws_posted_to)
	    if ( !(to->ws_posted_to = (Css_ws_list)
		    malloc((MAX_NO_OPEN_WS+1) * sizeof(Css_ws_on))) ) {
		ERR_BUF((cssh)->erh, ERR901);
		return(FALSE);				/* out of memory */
	    }
	i = 0;
	while (from->ws_posted_to[i].wsh) {
	    to->ws_posted_to[i].wsh = from->ws_posted_to[i].wsh;
	    to->ws_posted_to[i].count = from->ws_posted_to[i].count;
	    i++;
	}
	to->ws_posted_to[i].wsh = NULL;
    }

    if ( (op==CSS_WS_APPEAR || op==CSS_WS_BOTH) && from->ws_appear_on) {
	if (!to->ws_appear_on)
	    if ( !(to->ws_appear_on = (Css_ws_list)
		    malloc((MAX_NO_OPEN_WS+1) * sizeof(Css_ws_on))) ) {
		ERR_BUF((cssh)->erh, ERR901);
		return(FALSE);				/* out of memory */
	    }
	i = 0;
	while (from->ws_appear_on[i].wsh) {
	    to->ws_appear_on[i].wsh = from->ws_appear_on[i].wsh;
	    to->ws_appear_on[i].count = from->ws_appear_on[i].count;
	    i++;
	}
	to->ws_appear_on[i].wsh = NULL;
    }
    return(TRUE);
}

/*******************

    phg_css_ws_posted - is structp posted to wsh?

*******************/

int
phg_css_ws_posted(structp, wsh)
Struct_handle	structp;
Ws_handle	wsh;
{
    register Css_ws_list wsptr;
   
    if ( !(wsptr = structp->ws_posted_to) )
	return(FALSE);
    while (wsptr->wsh && wsptr->wsh!=wsh)
	wsptr++;
    return((int)wsptr->wsh);
}

/*******************

    phg_css_ws_appearances - how many times does structp appear on wsh?

*******************/

int
phg_css_ws_appearances(structp, wsh)
Struct_handle		structp;
register Ws_handle	wsh;
{
    register Css_ws_list wsptr;
   
    if ( !(wsptr = structp->ws_appear_on) )
	return(0);
    while (wsptr->wsh && wsptr->wsh!=wsh)
	wsptr++;
    return(wsptr->wsh ? wsptr->count : 0);
}
