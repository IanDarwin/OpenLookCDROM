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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/basics/common/RCS/keystate.c,v 2.10 1993/10/29 21:45:02 rr2b Exp $";
#endif


 

/* keystate.ch -- A class that keep track of partially evaluated sequences of keystrokes.
December, 1986 */

#include <class.h>
#include <proctbl.ih>
#include <keymap.ih>
#include <keystate.eh>


void keystate__SetObject(self, object)
    struct keystate *self;
    struct basicobject *object;
{

    self->object = object;
}

struct keystate *keystate__AddBefore(self, ks)
    struct keystate *self;
    struct keystate *ks;
{
    self->next = ks;
    return self;
}

struct keystate *keystate__AddAfter(self, ks)
    struct keystate *self;
    struct keystate *ks;
{
    register struct keystate *kp;

    self->next = NULL;
    if (ks == NULL)
	return self;
    for (kp = ks; kp->next != NULL; kp = kp->next)
	;
    kp->next = self;
    return ks;
}

void InitState(self)
    struct keystate *self;
{
    struct keystate *ks;

    for (ks = self; ks != NULL; ks = ks->next)
	ks->curMap = ks->orgMap;
}

void keystate__Reset(self)
    struct keystate *self;
{
    InitState(self);
}

enum keystate_ApplyKeyValues keystate__ApplyKey(self, key, ppe, rockP, pobject)
    struct keystate *self;
    char key;
    struct proctable_Entry **ppe;
    long *rockP;
    struct basicobject **pobject;
{
    struct keystate *ks;
    struct proctable_Entry *pe;
    long rock;
    boolean allBad = TRUE;	/* true while all keymaps looked at have no binding for this key */
    boolean foundProc = FALSE;	/* true if we execute something */
    boolean foundMap = FALSE;	/* true if we advance one keymap */
    enum keymap_Types code;

    for (ks = self; ks != NULL; ks = ks->next) {
        if (ks->function != NULL)
            code = (*ks->function)(ks->functionData, key, &pe, &rock);
        else {
            if (ks->curMap == NULL)
                continue;
            code = keymap_Lookup(ks->curMap, key, &pe, &rock);
        }
	if (code == keymap_Empty)  {
	    ks->curMap = NULL;
	    continue;
	}
	/* Found something, either a keymap or a proc. */
	allBad = FALSE;
	if (code == keymap_Keymap) {
	    foundMap = TRUE;
	    ks->curMap = (struct keymap *)pe;
	}
	else { 	/* code == keymap_Proc */
	    if (foundMap) {
		/* Sorry, an earlier keymap takes precedence. */
		ks->curMap = NULL;
		continue;
	    }
	    foundProc = TRUE;
	    if (ppe != NULL)
		*ppe = pe;
	    if (pobject != NULL)
		*pobject = ks->object;
            if (rockP != NULL)
                *rockP = rock;
	    break;
	}
    }
    if (foundProc) {
	keystate_Reset(self);
	if(pe == NULL)
	    return keystate_NoBinding;
	else
	    return keystate_ProcFound;
    }
    if (allBad) {
	keystate_Reset(self);
	return keystate_NoBinding;
    }
    return keystate_Pending;
}

/* Apply the procedure in a proctable entry to the object, or to an object of the correct type as found in the keystate chain. */
enum keystate_DoProcValues keystate__DoProc(self, pe, rock, object)
    struct keystate *self;
    struct proctable_Entry *pe;
    long rock;
    struct basicobject *object;
{
    struct keystate *ks;

    proctable_ForceLoaded(pe);
    if (!proctable_Defined(pe)) {
	keystate_Reset(self);
	return keystate_NoProc;
    }
    if (!class_IsType(object, pe->type)) {
	/* Attempt to find object of correct type. */
	for (ks = self; ks != NULL; ks = ks->next)
	    if (class_IsType(ks->object, pe->type))
		break;
	if (ks == NULL) {
	    keystate_Reset(self);
	    return keystate_TypeMismatch;
	}
	object = ks->object;
    }
    (*pe->proc)(object, rock);
    return keystate_ProcCalled;
}

void keystate__FreeChain(self)
    register struct keystate *self;
{
    register struct keystate *ks, *kp;

    for (ks = self; ks != NULL; ks = kp) {
	kp = ks->next;
	keystate_Destroy(ks);
    }
}

/* Class methods begin here. */


/* Basic class routines.  We have our own allocation routines for speed. */

static struct keystate *freeKS = NULL;

boolean keystate__InitializeObject(classID, self)
    struct classheader *classID;
    struct keystate *self;
{
    self->next = NULL;
    self->orgMap = NULL;
    self->curMap = NULL;
    self->object = NULL;
    self->function = NULL;
    return TRUE;
}

struct keystate *keystate__Create(classID, object, keymap)
    struct classheader *classID;
    struct basicobject *object;
    struct keymap *keymap;
{
    register struct keystate *keystate;
    
    keystate = keystate_New();
    keystate->orgMap = keymap;
    keystate->curMap = keymap;
    keystate->object = object;
    return keystate;
}

void keystate__SetOverride(self, function, functionData)
    struct keystate *self;
    enum keymap_Types (*function)();
    long functionData;
{

    self->function = function;
    self->functionData = functionData;
}

void keystate__GetOverride(self, function, functionData)
    struct keystate *self;
    enum keymap_Types (**function)();
    long *functionData;
{

    *function = self->function;
    *functionData = self->functionData;
}

struct keystate *keystate__Allocate(classID)
    struct classheader *classID;
{
    struct keystate *ks;

    if (freeKS == NULL)
	ks = (struct keystate *) malloc(sizeof (struct keystate));
    else {
	ks = freeKS;
	freeKS = freeKS->next;
    }
    return ks;
}

void keystate__Deallocate(classID, ks)
    struct classheader *classID;
    struct keystate *ks;
{
    ks->next = freeKS;
    freeKS = ks;
}
