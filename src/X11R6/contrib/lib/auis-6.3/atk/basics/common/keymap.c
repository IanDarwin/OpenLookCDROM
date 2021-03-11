/* ********************************************************************** *\
 *         Copyright IBM Corporation 1988,1991 - All Rights Reserved      *
 *        For full copyright information see:'andrew/config/COPYRITE'     *
\* ********************************************************************** */

/*
	$Disclaimer: 
 * Permission to use, copy, modify, and distribute this software and its 
 * documentation for any purpose is hereby granted without fee, 
 * provided that the above copyright notice appear in all copies and that 
 * both that copyright notice, this permission notice, and the following 
 * disclaimer appear in supporting documentation, and that the names of 
 * IBM, Carnegie Mellon University, and other copyright holders, not be 
 * used in advertising or publicity pertaining to distribution of the software 
 * without specific, written prior permission.
 * 
 * IBM, CARNEGIE MELLON UNIVERSITY, AND THE OTHER COPYRIGHT HOLDERS 
 * DISCLAIM ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING 
 * ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS.  IN NO EVENT 
 * SHALL IBM, CARNEGIE MELLON UNIVERSITY, OR ANY OTHER COPYRIGHT HOLDER 
 * BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY 
 * DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, 
 * WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS 
 * ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE 
 * OF THIS SOFTWARE.
 *  $
*/

#ifndef NORCSID
#define NORCSID
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/basics/common/RCS/keymap.c,v 2.13 1993/10/29 21:45:02 rr2b Exp $";
#endif


 

/* keymap.c -- A class that provides mappings of keys to procedures.
December, 1986 */

#include <class.h>
#include <keymap.eh>
#include <proctbl.ih>

#define KEYMASK	(keymap_MAXKEYS-1)

struct keymap_fulltable *NewFullTable()
{

    int i;
    struct keymap_fulltable *newTable = (struct keymap_fulltable *)
                                 malloc(sizeof(struct keymap_fulltable));

    for (i = 0; i < keymap_MAXKEYS; ++i) {
        newTable->types[i] = keymap_Empty;
	newTable->objects[i] = NULL;
	newTable->rocks[i]=0;
    }
    return newTable;
}

static void DoInitialize(self, sparsep)
    struct keymap *self;
    boolean sparsep;
{

    self->sparsep = sparsep;
    if (sparsep) {
        self->table.sparse = (struct keymap_sparsetable *)
                             malloc(sizeof(struct keymap_sparsetable));
        self->table.sparse->numValid = 0;
    }
    else
        self->table.full = NewFullTable();
}

boolean keymap__InitializeObject(classID, self)
    struct classheader *classID;
    struct keymap *self;
{
    DoInitialize(self, TRUE);
    return TRUE;
}

void keymap__FinalizeObject(classID, self)
struct classheader *classID;
struct keymap *self;
{
    int i;
    if(self->sparsep) {
	for (i = 0; i < self->table.sparse->numValid; ++i) {
	    if(self->table.sparse->types[i]==keymap_Keymap) keymap_Destroy((struct keymap *)self->table.sparse->objects[i]);
	}
	if(self->table.sparse) free(self->table.sparse);
    } else {
	for (i = 0; i < keymap_MAXKEYS; ++i) {
	    if(self->table.full->types[i] == keymap_Keymap)
		keymap_Destroy((struct keymap *)self->table.full->objects[i]);
	}
	if(self->table.full) free(self->table.full);
    }
}

static boolean bindKey(self,keys,obj,rock,type)
    struct keymap *self;
    unsigned char *keys;
    struct basicobject *obj;
    long rock;
    enum keymap_Types type;
{
    enum keymap_Types e;
    register unsigned char *p;
    struct keymap *km1, *km2;
    register unsigned char c;

    for (p = keys, km1 = self; p[1] != 0; ++p) {
	c = (*p == 128)? 0 : *p; /*  allows nulls in string to be represented by 128 */
	e = keymap_Lookup(km1, c, &km2,NULL );
	if (e == keymap_Empty) {
	    e = keymap_Keymap;
	    km2 = keymap_New();
	    keymap_InsertObject(km1, c, km2, rock, keymap_Keymap);
	}
	else if (e != keymap_Keymap)
	    return FALSE;
	km1 = km2;
    }
    c = (*p == 128)? 0 : *p; /*  allows nulls in string to be represented by 128  */
	
    keymap_InsertObject(km1,c,obj,rock,type);

    return TRUE;
}

boolean keymap__BindToKey(self, keys, pe, rock)
    struct keymap *self;
    char *keys;
    struct proctable_Entry *pe;
    long rock;
{

    if (keys == NULL || *keys == 0)
	return FALSE;
    bindKey(self,keys,(struct basicobject *) pe,rock,keymap_Proc);
    return TRUE;
}

void keymap__RemoveBinding(self,keys)
struct keymap *self;
char *keys;
{
    bindKey(self,keys,NULL,NULL,keymap_Empty);
}

static ExpandTable(self)
    struct keymap *self;
{

/* Don't even try to expand a full table... */
    if (self->sparsep) {

        int i;
        struct keymap_fulltable *newTable;

        newTable = NewFullTable();
        for (i = 0; i < self->table.sparse->numValid; ++i) {

            int key = self->table.sparse->keys[i];

            newTable->types[key] = self->table.sparse->types[i];
            newTable->objects[key] = self->table.sparse->objects[i];
	    newTable->rocks[key]=self->table.sparse->rocks[i];
	}
        free(self->table.sparse);
        self->table.full = newTable;
        self->sparsep = FALSE;
    }
}

void keymap__InsertObject(self, slot, object, rock, type)
    struct keymap *self;
    long slot;
    struct basicobject *object;
    long rock;
    enum keymap_Types type;
{

    slot &= KEYMASK;
    if (self->sparsep) {

        int i, numValid = self->table.sparse->numValid;

        for (i = 0; i < numValid && self->table.sparse->keys[i] != slot; ++i)
            ;
        if (i != numValid || (numValid < keymap_SPARSESIZE && ++self->table.sparse->numValid)) {
            self->table.sparse->keys[i] = slot & KEYMASK;
            self->table.sparse->types[i] = type;
	    self->table.sparse->objects[i] = object;
	    self->table.sparse->rocks[i]=rock;
            return;
        }
        else
            ExpandTable(self);
    }

    self->table.full->types[slot & KEYMASK] = type;
    self->table.full->objects[slot & KEYMASK] = object;
    self->table.full->rocks[slot&KEYMASK]=rock;
}

enum keymap_Types keymap__Lookup(self, key, object,rockP)
    struct keymap *self;
    char key;
    struct basicobject **object;
    long *rockP;
{
    key &= KEYMASK;

    if (self->sparsep) {

        int i;

        for (i = 0; (i < self->table.sparse->numValid) && (self->table.sparse->keys[i] != key); i++)
            ;
        if (i < self->table.sparse->numValid) {
            if (object != NULL)
                *object = self->table.sparse->objects[i];
	    if(rockP!=NULL)
		*rockP=self->table.sparse->rocks[i];
            return self->table.sparse->types[i];
        }
        else
            return keymap_Empty;
    }

    if (object != NULL)
        *object = self->table.full->objects[(unsigned char)key];
    if(rockP!=NULL)
	*rockP=self->table.full->rocks[(unsigned char)key];
    return self->table.full->types[(unsigned char)key];
}
