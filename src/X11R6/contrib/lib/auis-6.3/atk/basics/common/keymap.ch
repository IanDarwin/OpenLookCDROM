/* ********************************************************************** *\
 *         Copyright IBM Corporation 1988,1991 - All Rights Reserved      *
 *        For full copyright information see:'andrew/config/COPYRITE'     *
\* ********************************************************************** */

/*
	$Disclaimer: 
*Permission to use, copy, modify, and distribute this software and its 
*documentation for any purpose is hereby granted without fee, 
*provided that the above copyright notice appear in all copies and that 
*both that copyright notice, this permission notice, and the following 
*disclaimer appear in supporting documentation, and that the names of 
*IBM, Carnegie Mellon University, and other copyright holders, not be 
*used in advertising or publicity pertaining to distribution of the software 
*without specific, written prior permission.
*
*IBM, CARNEGIE MELLON UNIVERSITY, AND THE OTHER COPYRIGHT HOLDERS 
*DISCLAIM ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING 
*ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS.  IN NO EVENT 
*SHALL IBM, CARNEGIE MELLON UNIVERSITY, OR ANY OTHER COPYRIGHT HOLDER 
*BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY 
*DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, 
*WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS 
*ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE 
*OF THIS SOFTWARE.
* $
*/


 

/* keymap.ch -- A class that provides mappings of keys to procedures.
December, 1986 */

/* Number of entries in a keymap.  Should be power of two. */
#define keymap_MAXKEYS	256
/* Number of entries in a sparse map. Should be a multiple of 4 for good
 * structure packing. */
#define keymap_SPARSESIZE 16

/* These are the values that the return values from Lookup can take on. */

enum keymap_Types  {
    keymap_Empty,	/* no binding for this key */
    keymap_Proc,	/* binding is a proctable structure */
    keymap_Keymap	/* binding is another keymap */
};

struct keymap_sparsetable {
    int numValid;
    char keys[keymap_SPARSESIZE];
    enum keymap_Types types[keymap_SPARSESIZE];
    struct basicobject *objects[keymap_SPARSESIZE];
    long rocks[keymap_SPARSESIZE];
};    

struct keymap_fulltable {
    enum keymap_Types types[keymap_MAXKEYS];
    struct basicobject *objects[keymap_MAXKEYS];
    long rocks[keymap_MAXKEYS];
};

class keymap {
classprocedures:
    InitializeObject(struct keymap *self) returns boolean;
    FinalizeObject(struct keymap *self);
methods:
    BindToKey(char *keys, struct proctable_Entry *pe, long rock)	returns boolean;
    RemoveBinding(char *keys);
    InsertObject(long slot, struct basicobject *object, long rock, enum keymap_Types type);
    /* Lookup stuffs an object in the char * and returns an int that describes its type. */
    Lookup(char key, struct basicobject **object, long *rockP) returns enum keymap_Types;
data:
    union {
        struct keymap_sparsetable *sparse;
        struct keymap_fulltable *full;
    } table;
    boolean sparsep;
};
