/* $XConsortium: css_stb.c,v 5.2 94/04/17 20:42:43 rws Exp $ */

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
#include "alloc.h"

/*************************************************************************\
* 									  *
*   This file implements a hash table; collisions of the hash value are   *
*   handled by separate chaining.                                         *
* 									  *
\*************************************************************************/

#define ABS(X)  (((X) >= 0) ? (X) : -(X))

/*******************

    phg_css_stab_init - Creates and initialises the hash table; returns a
			handle to the table or NULL.

*******************/

Css_struct_tab *
phg_css_stab_init(table_size)
register int table_size;			/* size of hash table */
{
    register Css_hash_block 	**block, *endblocks;
    register int		i;
    register Css_struct_tab	*new_table;	/* the new hash table */
    ALLOC_DECLARE(5);

    new_table = (Css_struct_tab *) malloc(sizeof(Css_struct_tab));
    if (!ALLOCATED(new_table)) {
	ALLOC_FREE;
        return(NULL);
    }

    new_table->table = (Css_hash_block **)
	malloc((unsigned)(sizeof(Css_hash_block *) * table_size));
    if (!ALLOCATED(new_table->table)) {
	ALLOC_FREE;
        return(NULL);
    }

    /*
     * allocate hash_blocks used to flag end of chained lists of colliding
     * structure ids in one big chunk, assign pointers into this chunk
     */
    block = new_table->table;
    endblocks = (Css_hash_block *)
	malloc((unsigned) (sizeof(Css_hash_block) * table_size));
    if (!ALLOCATED(endblocks)) {
	ALLOC_FREE;
        return(NULL);
    }

    for (i = 0; i < table_size; i++) {
	*block = endblocks++;
	(*block)->next = (Css_hash_block *) NULL;
	(*block)->struct_id = -1;
	block++;
    }
    new_table->size = table_size;
    new_table->nstructs = 0;
    return(new_table);
}

/*******************

    phg_css_stab_search - Returns a pointer to struct_id if it is in the table,
			  otherwise returns NULL.

*******************/

Struct_handle
phg_css_stab_search(stab, struct_id)
Css_struct_tab	*stab;		/* the structure table */
register Pint	struct_id;
{
    register int		hash_value;
    register Css_hash_block 	*block;

    hash_value = struct_id % stab->size;
    hash_value = ABS(hash_value);
    block = *(stab->table + hash_value);
    
    block = block->next;		/* skip over the unused block */
    while ((block != (Css_hash_block *) NULL) &&
	   (block->struct_id < struct_id) )
	block = block->next;

    if (block == (Css_hash_block *) NULL)
        return(NULL);

    if (block->struct_id == struct_id)
        return(block->struct_ptr);
    else
        return(NULL);
}

/*******************

    phg_css_stab_insert - Inserts the structure id and pointer to its data in
			  the hash table. 
			  Returns -1 if the structure id is already in table
				   0 if malloc failed
				   1 if successful

*******************/

int
phg_css_stab_insert(stab, struct_id, struct_ptr)
Css_struct_tab	*stab;		/* the structure table */
register int	struct_id;
Struct_handle	struct_ptr;
{
    register int		hash_value;
    register Css_hash_block 	*block;
    register Css_hash_block 	*parent;
    register Css_hash_block	*new_block;
		
    hash_value = struct_id % stab->size;
    hash_value = ABS(hash_value);
    parent = *(stab->table + hash_value);

    block = parent->next;		/* skip over the unused block */
    while ((block != (Css_hash_block *) NULL) &&
	   (block->struct_id < struct_id) ) {
	parent = block;
	block = block->next;
    }

    if ((block != (Css_hash_block *) NULL) && (block->struct_id == struct_id)) {
        return(-1);
    }
    else {
	new_block = (Css_hash_block *) malloc(sizeof(Css_hash_block));
	if (new_block == (Css_hash_block *) NULL)
	    return(0);
	new_block->struct_id = struct_id;
	new_block->struct_ptr = struct_ptr;
	new_block->next = parent->next;
	parent->next = new_block;
	stab->nstructs++;
        return(1);
    }
}

/*******************

    phg_css_stab_delete - Delete the structure id from the hash table; leaves
			  it up to the caller to deallocate structure data.
			  Returns true if successful, false if structure id
			  not found.

*******************/

int
phg_css_stab_delete(stab, struct_id)
Css_struct_tab	*stab;		/* the structure table */
register int	struct_id;
{
    register int		hash_value;
    register Css_hash_block 	*block;
    register Css_hash_block 	*parent;
		
    hash_value = struct_id % stab->size;
    hash_value = ABS(hash_value);
    parent = *(stab->table + hash_value);

    block = parent->next;		/* skip over the unused block */
    while ((block != (Css_hash_block *) NULL) &&
	   (block->struct_id < struct_id) ) {
	parent = block;
	block = block->next;
    }

    if ((block != (Css_hash_block *) NULL) && (block->struct_id == struct_id)) {
	/* block and parent->next point to the same place */
	parent->next = parent->next->next;
	free((char *)block);
	stab->nstructs--;
        return(TRUE);
    }
    else
        return(FALSE);
}

/*******************

    phg_css_stab_free - Frees memory used by the hash table; caller should
			free memory used for structure data.

			Note that this means that all chained lists of hash
			blocks should already have been freed before this 
			routine is called.

*******************/

void
phg_css_stab_free(stab)
Css_struct_tab	*stab;
{
    free((char *) *stab->table);
    free((char *) stab->table);
    free((char *) stab);
}
