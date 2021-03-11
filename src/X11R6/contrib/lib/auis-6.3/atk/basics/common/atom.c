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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/basics/common/RCS/atom.c,v 2.13 1992/12/15 21:27:21 rr2b R6tape $";
#endif


 


#include <andrewos.h> /* strings.h */
#include <stdio.h>
#include <atom.eh>


#define Log2HashTableSize   9
#define HashTableSize       (1 << Log2HashTableSize)

struct alist
{
    struct atom *atom;
    struct alist *next;
};

static struct alist *hashTable[HashTableSize];

/*
 * Statics
 */

static int Hash(word)
register unsigned char *word;
{
    register unsigned int total = 0;

    /* Pretty good hash function */

    while (*word)
        total = (total >> 1) + (total << 1) + *word++;

    return total & (HashTableSize - 1);
}

static struct atom *CreateAtom(name, index)
register char *name;
register int index;
{
    register struct atom *a;
    register struct alist *l;

    a = atom_New();
    a->name = (char*) malloc(strlen(name) + 1);
    (void) strcpy(a->name, name);

    l = (struct alist *) malloc(sizeof (struct alist));
    l->atom = a;
    l->next = hashTable[index];

    hashTable[index] = l;
    return a;
}

/*
 * Class procedures
 */

struct atom *atom__Intern(classID, name)
struct classheader *classID;
register char *name;
{
    register int index;
    register struct alist *a;

    index = Hash((unsigned char *)name);

    for (a = hashTable[index]; a != NULL; a = a->next)
        if (0 == strcmp(name, atom_Name(a->atom)))
            return a->atom;

    return CreateAtom(name, index);
}


struct atom *atom__InternRock(classID, rock)
struct classheader *classID;
long rock;
{
    char temp[16];

    sprintf(temp, "g0x%lx", rock); 
    return atom_Intern(temp);
}

boolean atom__InitializeClass(classID)
struct classheader *classID;
{
    register struct alist **ap = hashTable;
    register int i = HashTableSize;

    while (i--)
        *ap++ = NULL;

    return TRUE;
}

boolean atom__InitializeObject(classID, self)
struct classheader *classID;
struct atom *self;
{
    self->name = NULL;
    return TRUE;
}

void atom__FinalizeObject(classID, self)
struct classheader *classID;
struct atom *self;
{
	fprintf(stderr, "Illegal Destruction of an Atom\n");
}
