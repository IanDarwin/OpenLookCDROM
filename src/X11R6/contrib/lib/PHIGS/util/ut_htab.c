/* $XConsortium: ut_htab.c,v 5.2 94/04/17 20:42:20 rws Exp $ */

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

/* Hash table code. */

#include <stdio.h>
#include <sys/types.h>


typedef struct _Htab_entry {
    int			key;
    caddr_t		cdata;	/* client data */
    struct _Htab_entry	*next;
} Htab_entry;

typedef struct _Htab {
    int		num_entries;	/* in table */
    int		hash_size;
    Htab_entry	**tbl;		/* *tbl[num_entries] */
    int		search_used;	/* non-zero if first/next search ever used*/
    int		cur_list;	/* for first and next searches only */
    Htab_entry	*next_entry;	/* for first and next searches only */
} Htab;

#ifndef _ABS
#define _ABS(x) ((x) < 0 ? -(x) : (x))
#endif

static Htab_entry**
get_entry( htab, key )
    Htab	*htab;
    int		key;
{
    Htab_entry	**entry_p;

    /* Returns either a pointer to the pointer to the entry (not a pointer
     * to the entry itself) or a pointer to the place the pointer to the
     * entry should exist if it were to be added.
     */
    entry_p = &htab->tbl[_ABS(key) % htab->hash_size];
    for ( ; *entry_p && (*entry_p)->key != key; entry_p = &(*entry_p)->next )
	;
    return entry_p;
}


/* Public functions. */
Htab*
phg_ut_htab_create( hash_size )
    int		hash_size;
{
    Htab	*htab;

    /* Get a block of memory for the Htab structure and the table. */
    if ( hash_size <= 0 )
	hash_size = 1;	/* makes it an essentially linear table. */

    if ( htab = (Htab *)calloc( 1, sizeof(Htab)
	    + sizeof(Htab_entry *)* (unsigned)hash_size ) ) {
	htab->tbl = (Htab_entry **)(htab + 1);
	/* Initialize the table. */
	htab->num_entries = 0;
	htab->hash_size = hash_size;
    } else
	htab = (Htab *)NULL;

    return htab;
}

void
phg_ut_htab_destroy( htab, destroy_entry )
    Htab	*htab;
    void	(*destroy_entry)();
{
    register int	i;
    register Htab_entry	*entry, *next;

    for ( i = 0; i < htab->hash_size; i++ ) {
	for ( entry = htab->tbl[i]; entry; entry = next ) {
	    next = entry->next;
	    if ( destroy_entry )
		(*destroy_entry)( entry->key, entry->cdata );
	    free( (char *)entry );
	}
    }
    free( (char *)htab );
}

caddr_t
phg_ut_htab_delete_entry( htab, key )
    Htab	*htab;
    int		key;
{
    caddr_t	cdata;
    Htab_entry	**entry_p, *tmp;

    /* If the entry exists, free it return the client's data. */
    if ( *(entry_p = get_entry( htab, key )) ) {
	cdata = (*entry_p)->cdata;
	tmp = *entry_p;
	*entry_p = tmp->next;
	/* Update the first/next search field. */
	if ( tmp == htab->next_entry )
	    htab->next_entry = tmp->next;
	free( (char *)tmp );
	--htab->num_entries;
    } else
	cdata = (caddr_t)NULL;

    return cdata;
}

int
phg_ut_htab_add_entry( htab, key, cdata )
    Htab	*htab;
    int		key;
    caddr_t	cdata;
{
    Htab_entry	**entry_p;

    /* If found, just replace the client data. */
    if ( *(entry_p = get_entry( htab, key )) ) {
	(*entry_p)->cdata = cdata;

    } else {
	/* Create the entry and link it into the table. */
	if ( *entry_p = (Htab_entry *)malloc( sizeof(Htab_entry) ) ) {
	    (*entry_p)->key = key;
	    (*entry_p)->cdata = cdata;
	    (*entry_p)->next = (Htab_entry *)NULL;
	    ++htab->num_entries;

	    /* Update the first/next search fields if the "next_entry" is
	     * the end of the list this entry was added to.
	     */
	    if ( htab->search_used && !htab->next_entry
		    && ( _ABS(key) % htab->hash_size == htab->cur_list ) )
		htab->next_entry = *entry_p;
	}
    }

    return (*entry_p ? 1 : 0);
}

int
phg_ut_htab_get_entry( htab, key, cdata_p )
    Htab	*htab;
    int		key;
    caddr_t	*cdata_p;
{
    Htab_entry	**entry_p;

    if ( *(entry_p = get_entry( htab, key )) )
	if ( cdata_p )
	    *cdata_p = (*entry_p)->cdata;

    return (*entry_p ? 1 : 0);
}

void
phg_ut_htab_change_data( htab, key, cdata )
    Htab	*htab;
    int		key;
    caddr_t	cdata;
{
    Htab_entry	**entry_p;

    if ( *(entry_p = get_entry( htab, key )) )
	(*entry_p)->cdata = cdata;
}

int
phg_ut_htab_num_entries( htab )
    Htab	*htab;
{
    return htab->num_entries;
}

int
phg_ut_htab_first_entry( htab, key, cdata )
    Htab	*htab;
    int		*key;
    caddr_t	*cdata;
{
    htab->search_used = 1;	/* this is never reset to 0 */
    for ( htab->cur_list = 0; htab->cur_list < htab->hash_size;
	    htab->cur_list++ ) {
	if ( htab->tbl[htab->cur_list] ) {
	    htab->next_entry = htab->tbl[htab->cur_list]->next;
	    if ( cdata ) *cdata = htab->tbl[htab->cur_list]->cdata;
	    if ( key ) *key = htab->tbl[htab->cur_list]->key;
	    return 1;
	}
    }

    return 0;
}

int
phg_ut_htab_next_entry( htab, key, cdata )
    Htab	*htab;
    int		*key;
    caddr_t	*cdata;
{
    int		status = 0;

    if ( htab->next_entry ) {
	if ( cdata ) *cdata = htab->next_entry->cdata;
	if ( key ) *key = htab->next_entry->key;
	htab->next_entry = htab->next_entry->next;
	status = 1;

    } else {
	++htab->cur_list;
	for ( ; htab->cur_list < htab->hash_size; htab->cur_list++ ) {
	    if ( htab->tbl[htab->cur_list] ) {
		htab->next_entry = htab->tbl[htab->cur_list]->next;
		if ( cdata) *cdata = htab->tbl[htab->cur_list]->cdata;
		if ( key ) *key = htab->tbl[htab->cur_list]->key;
		status = 1;
		break;
	    }
	}

    }

    return status;
}
