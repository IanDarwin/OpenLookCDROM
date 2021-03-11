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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/basics/common/RCS/atomlist.c,v 2.11 1993/05/04 01:08:09 susan Exp $";
#endif


 


#include <andrewos.h> /* strings.h */
#include <class.h>
#include <atomlist.eh>
#include <atom.ih>

#ifndef _IBMR2
extern char *malloc();
#endif /* _IBMR2 */

/*
 * Class Procedures
 */

boolean atomlist__InitializeObject(classID, self)
struct classheader *classID;
struct atomlist *self;
{
  self->atoms = NULL;
  return TRUE;
}

void atomlist__FinalizeObject(classID, self)
struct classheader *classID;
register struct atomlist *self;
{
    register struct atoms *next;

    while (self->atoms != NULL) {
	next = self->atoms->next;
	free(self->atoms);
	self->atoms = next;
    }
}


struct atomlist *atomlist__Copy(classID, oldlist)
struct classheader *classID;
struct atomlist *oldlist;
{
    struct atomlist *newlist = atomlist_New();
    register struct atoms *atoms;

    for (atoms = oldlist->atoms; atoms != NULL; atoms = atoms->next)
        atomlist_Append(newlist, atoms->atom);

    return newlist;
}


struct atomlist *atomlist__StringToAtomlist(classID, string)
struct classheader *classID;
char *string;
{
    char *atomstart;
    char *atomend;
    struct atom *atom;
    struct atomlist *newlist = atomlist_New();

    if (string != NULL)  {
	for (atomstart = atomend = string; atomend != NULL; atomstart =  1 + atomend)  {
	    atomend = index(atomstart,'.');
	    if (atomend != NULL) *atomend = '\0';
            atom = atom_Intern(atomstart);
	    if (atomend != NULL) *atomend = '.';
            atomlist_Append(newlist, atom);
	}
    }
    return newlist;
}

/*
 * Methods
 */

struct atom *atomlist__Last(self)
struct atomlist *self;
{
    register struct atoms *atoms;

    for (atoms = self->atoms; atoms != NULL && atoms->next != NULL; atoms = atoms->next)
        ;

    return atoms == NULL ? NULL : atoms->atom;
}


short atomlist__Memberp(self, key)
struct atomlist *self;
struct atom *key;
{
    register struct atoms *atoms;

    for (atoms = self->atoms; atoms != NULL && atoms->atom != key; atoms = atoms->next)
        ;

    return (atoms != NULL);
}

void atomlist__DropFirst(self)
struct atomlist *self;
{
    struct atoms *oldfirst = self->atoms;

    if (oldfirst != NULL) {
	self->atoms = self->atoms->next;
	free(oldfirst);
    }
}

void atomlist__Prepend(self, atom)
struct atomlist *self;
struct atom *atom;
{
    register struct atoms *atoms;

    atoms = (struct atoms *) malloc(sizeof(struct atoms));

    atoms->next = self->atoms;
    atoms->atom = atom;
    self->atoms = atoms;
}

void atomlist__Append(self, atom)
struct atomlist *self;
struct atom *atom;
{
    register struct atoms *new;
    register struct atoms **last;

    new = (struct atoms *) malloc(sizeof(struct atoms));
    new->atom = atom;
    new->next = NULL;

    for (last = &(self->atoms); *last != NULL; last = &((*last)->next))
        ;

    *last = new;
}


void atomlist__JoinToEnd(self, otherlist)
struct atomlist *self;
struct atomlist *otherlist;
{
    register struct atoms *otherAtoms;
    register struct atoms **last;

    for (last = &(self->atoms); *last != NULL; last = &((*last)->next))
        ;

    for (otherAtoms = atomlist_TraversalStart(otherlist); otherAtoms != NULL; otherAtoms = atomlist_TraversalNext(otherlist, otherAtoms)) {
        *last = (struct atoms *) malloc(sizeof(struct atoms));
        (*last)->atom = atomlist_TraversalAtom(otherlist, otherAtoms);
	(*last)->next = NULL;
	last = &((*last)->next);
    }
}

void atomlist__JoinToBeginning(self, otherlist)
struct atomlist *self;
struct atomlist *otherlist;
{
    register struct atoms *otherAtoms;
    register struct atoms **last;
    register struct atoms *temp;

    last = &(self->atoms);

    for (otherAtoms = atomlist_TraversalStart(otherlist); otherAtoms != NULL; otherAtoms = atomlist_TraversalNext(otherlist, otherAtoms)) {
        temp = (struct atoms *) malloc(sizeof(struct atoms));
        temp->atom = atomlist_TraversalAtom(otherlist, otherAtoms);
	temp->next = *last;
	*last = temp;
	last = &(temp->next);
    }
}

void atomlist__Cut(self, mark)
register struct atomlist *self;
struct atoms *mark;
{
    register struct atoms *next;

    while (self->atoms != mark && self->atoms != NULL)  {
	next = self->atoms->next;
	free(self->atoms);
	self->atoms = next;
    }
}
