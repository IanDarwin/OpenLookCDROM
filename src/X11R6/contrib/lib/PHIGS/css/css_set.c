/* $XConsortium: css_set.c,v 5.2 94/04/17 20:42:42 rws Exp $ */

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

/*******************

    phg_css_set_create - Create a set of integers (or pointers) or strings.
			 Returns a handle to a set or NULL.

*******************/

Css_set *
phg_css_set_create(data_type)
int	data_type;
{   
    register Css_set		*new_set;
    register Css_set_element	*new_element;
    
    if ( !(new_set = (Css_set *)malloc(sizeof(Css_set))) )
        return(NULL);
    if( !(new_element = (Css_set_element *)malloc(sizeof(Css_set_element))) ) {
	free((char *)new_set);
    	return(NULL);
    }
    new_element->next = NULL;
    new_set->elements = new_element;
    new_set->last_element = NULL;
    new_set->num_elements = 0;
    new_set->data_type = data_type;
    return(new_set);
}

/*******************

    phg_css_set_add - Add the key and data to the set. If key is pointer to a
		      string, the string is copied.
		      Returns true if the data and key were added to the set
		      (if the key is already in the set, true is also
		      returned - only need one occurrence of it, but the data
		      value is updated).
		      False means malloc failed.

*******************/

int
phg_css_set_add(s, key, data)
register Css_set	*s;
caddr_t			key;
caddr_t 		data;
{
    register Css_set_element	*el, *parent;
    register Css_set_element	*new_element;
    
    /* if key is bigger than the last element, add it at the end,
     * else look through the list.
     */
    if (s->last_element && s->last_element->key <= key) {
	if (s->last_element->key == key) {
	    /* found the key, so update the data associated with it */
	    s->last_element->data = data;
	    return(TRUE);
	} else {
	    parent = s->last_element;
	    el = NULL;
	}
    } else {
	parent = s->elements;
	el = parent->next;
	while (el && el->key < key) {
	    parent = el;
	    el = el->next;
	}
	if (el && el->key == key) {
	    /* found the key, so update the data associated with it */
	    el->data = data;
	    return(TRUE);
	}
    }
    /* else add new element */
    if ( !(new_element = (Css_set_element *)malloc(sizeof(Css_set_element))) )
    	return(FALSE);
    new_element->key = key;
    new_element->data = data;
    new_element->next = el;
    parent->next = new_element;
    if (el == NULL)
	s->last_element = new_element;
    s->num_elements++;
    return(TRUE); 					/* inserted key */
}

/*******************

    phg_css_set_remove - Remove the element from the set.
		         Return true if it is deleted; false otherwise.

*******************/

int
phg_css_set_remove(s, key)
register Css_set	*s;
caddr_t			key;
{
    register Css_set_element	*el, *parent;
    
    parent = s->elements;
    el = parent->next;
    while (el && el->key < key) {
	parent = el;
	el = el->next;
    }
    if (el && el->key==key) {
	/* found the key, so delete it */
	parent->next = el->next;
	if (el == s->last_element)
		s->last_element = parent;
	free((caddr_t)el);
	s->num_elements--;
	return(TRUE);
    }
    return(FALSE);				/* key not found */
}

/*******************

    phg_css_set_element_of - Returns true if data is element of the set;
                             false if it isn't.

*******************/

int
phg_css_set_element_of(s, key, data)
register Css_set			*s;
register caddr_t			key;
caddr_t 				*data;
{
    register Css_set_element	*el;
    
    /* check end of list first */
    el = s->last_element;
    if (el) {
	if (el->key == key) {
	    /* found the key, so return the data */
	    *data = el->data;
	    return (TRUE);
	} else if (el->key < key) {
	    return(FALSE);			/* key not found */
	}
    }
    el = s->elements->next;
    while (el && el->key < key)
	el = el->next;
    if (el && el->key == key) {
	/* found the key, so return the data */
	*data = el->data;
	return(TRUE);
    }
    return(FALSE);				/* key not found */
}

/*******************

    phg_css_set_empty - Empty out the set, but don't destroy it.

*******************/

void
phg_css_set_empty(s)
register Css_set *s;
{   
    register Css_set_element	*el;
    register Css_set_element	*el_to_free;
    
    el = s->elements->next;
    
    while (el) {
	el_to_free = el;
	el = el->next;
	free((caddr_t)el_to_free);
    }
    s->elements->next = NULL;
    s->last_element = NULL;
    s->num_elements = 0;
}

/*******************

    phg_css_set_free - Free the elements of the set and the set itself.

*******************/

void
phg_css_set_free(s)
register Css_set *s;
{   
    register Css_set_element	*el;
    register Css_set_element	*el_to_free;
    
    el = s->elements;
    
    while (el) {
	el_to_free = el;
	el = el->next;
	free((caddr_t)el_to_free);
    }
    free((char *)s);
}

/*******************

    phg_css_set_recursive_free - Frees a set of sets.

*******************/

void
phg_css_set_recursive_free(s)
Css_set	*s;
{   
    register Css_set_element	*el;
    
    if (s->data_type == SET_DATA_SET_OF_SETS) {
	el = s->elements->next;	
	while (el) {	
	    phg_css_set_recursive_free((Css_set *)el->data);
	    el = el->next;
	}
    }
    phg_css_set_free(s);
}
