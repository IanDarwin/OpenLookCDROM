/* $XConsortium: css.h,v 5.3 94/04/17 20:41:45 rws Exp $ */

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

#ifndef PHG_CSS_H_INCLUDED
#define PHG_CSS_H_INCLUDED

#include <sys/types.h>

/* structure element data */
typedef union {
    Pint	idata;
    caddr_t	ptr;
} Css_eldata;

typedef struct _set_element {
    caddr_t			key;
    caddr_t			data;
    struct _set_element		*next;
} Css_set_element;

typedef struct {
    int			data_type;	/* type of data associated w/ key */
    int			num_elements;	/* number of elements in the set */
    Css_set_element	*elements;	/* elements of this set */
    Css_set_element	*last_element;	/* last element in 'elements' */
} Css_set;

typedef Css_set	*Css_set_ptr;

#define SET_DATA_SET		(0x0)
#define SET_DATA_SET_OF_SETS	(0x1)

/* structure element */
typedef struct _Css_structel {
    Pelem_type			eltype;		/* element type */
    Css_eldata			eldata;		/* element data */
    struct _Css_structel	*prev;		/* backward link */
    struct _Css_structel	*next;		/* forward link */
} Css_structel;

typedef struct _Css_ws_on {
    Ws_handle	wsh;
    Pint	count;
} Css_ws_on;

typedef struct _Css_ws_on *Css_ws_list;

/* structure state list */
typedef struct _Css_ssl {
    Pint		struct_id;		/* structure id */
    Css_ws_list		ws_posted_to;		/* list of WS posted to */
    Css_ws_list		ws_appear_on;		/* list of WS I appear on */
    Css_set_ptr		refer_to_me;	/* set of structs which refer to me */
    Css_set_ptr		i_refer_to;		/* set of structs I refer to */
    Pint		num_el; 		/* number of elements */
    El_handle		first_el;		/* first structure element */
    El_handle		last_el;		/* last structure element */
} Css_ssl;

typedef int		(*Css_func)();

/* because list of types starts at 0 */
#define NUM_EL_TYPES	PELEM_NUM_EL_TYPES

typedef struct _hash_block {
    Pint		struct_id;	/* structure id we're looking for */
    Struct_handle	struct_ptr;	/* pointer to it */
    struct _hash_block	*next;		/* next block */
} Css_hash_block;

typedef struct {
    Css_hash_block	**table;	/* the table */
    int			size;		/* size of the table */
    int			nstructs;	/* number of structures in table */
} Css_struct_tab;

typedef enum {					/* type of structure store */
    SSH_CSS,					/* central structure store */
    SSH_AR					/* archive structure store */
} Css_ssh_type;

typedef struct _Css_struct {
    Css_func		el_funcs[NUM_EL_TYPES];	/* func vectors for el data */
    Css_struct_tab	*stab;			/* structure table */
    Struct_handle	open_struct;		/* pointer to open structure */
    El_handle		el_ptr;			/* current element pointer*/
    Pint		el_index;		/* current element index */
    Pedit_mode		edit_mode;
    Err_handle		erh;
    caddr_t		mem;			/* mem block for general use */
    int			mem_size;		/* size of memory block */
    Css_ws_list		ws_list;		/* list of ws handles */
    Css_ssh_type	ssh_type;		/* type of structure store */
} Css_struct;

extern Css_handle
    phg_css_init();

extern El_handle
    phg_css_set_ep();

extern Struct_handle
    phg_css_close_struct(),
    phg_css_open_struct(),
    phg_css_post(),
    phg_css_unpost(),
    phg_css_stab_search();

extern Css_ws_list
    phg_css_ar_retrieve(),
    phg_css_change_struct_id(),
    phg_css_change_struct_idrefs(),
    phg_css_change_struct_refs(),
    phg_css_get_ws_on();

extern void
    phg_css_archive(),
    phg_css_delete_all_structs(),
    phg_css_delete_el(),
    phg_css_delete_net(),
    phg_css_delete_struct(),
    phg_css_destroy(),
    phg_css_el_delete_list(),
    phg_css_inq_conf(),
    phg_css_inq_el_content(),
    phg_css_inq_el_type_size(),
    phg_css_inq_hierarchy(),
    phg_css_inq_struct_status(),
    phg_css_inq_struct_ids(),
    phg_css_inq_ws_posted(),
    phg_css_unpost_all();

extern int
    phg_css_add_elem(),
    phg_css_copy_struct(),
    phg_css_ws_appearances(),
    phg_css_ws_posted();

#define CSS_CUR_STRUCT_ID(cssh)	((cssh)->open_struct->struct_id)
#define CSS_CUR_STRUCTP(cssh)	((cssh)->open_struct)
#define CSS_CUR_ELP(cssh)	((cssh)->el_ptr)
#define CSS_INQ_EL_INDEX(cssh)	((cssh)->el_index)
#define CSS_EDIT_MODE(cssh)	((cssh)->edit_mode)
#define CSS_SET_EDIT_MODE(cssh, mode)	((cssh)->edit_mode = (mode))
#define CSS_STRUCT_EXISTS(cssh, structid) \
    phg_css_stab_search( (cssh)->stab, (structid) )
#define CSS_GET_WS_ON(structp)	((structp) ? (structp)->ws_appear_on : NULL)
#define CSS_STRUCT_HAS_DESCENDANTS(structp) \
    ((structp)->i_refer_to->num_elements)

/* caveat emptor - this macro doesn't test for NULLs.  It therefore returns
 * TRUE incorrectly if there is NOT an open structure and the structp argument
 * is NULL.  The caller generally knows one or both of these conditions
 * is false, so there's little point in having the macro check it again .
 */
#define CSS_STRUCT_IS_OPEN(cssh, structp) ((cssh)->open_struct==(structp))

/* counts backwards to get the sequential element index for the given elptr */
#define CSS_GET_EL_INDEX(elptr, elindex)	\
  { register El_handle _elptr = elptr;		\
						\
    (elindex) = 0;				\
    while (_elptr->prev) {			\
	(elindex)++;				\
	_elptr = _elptr->prev;			\
    }						\
  }

/* use element index to find the appropriate element pointer */
#define CSS_GET_EL_PTR(structp, elindex, elptr)		\
  { register int _elindex = elindex;			\
							\
    if (_elindex<=0 || _elindex>(structp)->num_el)	\
	(elptr) = NULL;					\
    else {						\
	(elptr) = (structp)->first_el;			\
	while (_elindex--)				\
	    (elptr) = (elptr)->next;			\
    }							\
  }

#endif
