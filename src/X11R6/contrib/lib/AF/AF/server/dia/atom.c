/***********************************************************
Copyright 1987 by Digital Equipment Corporation, Maynard, Massachusetts,
and the Massachusetts Institute of Technology, Cambridge, Massachusetts.

                        All Rights Reserved

Permission to use, copy, modify, and distribute this software and its 
documentation for any purpose and without fee is hereby granted, 
provided that the above copyright notice appear in all copies and that
both that copyright notice and this permission notice appear in 
supporting documentation, and that the names of Digital or MIT not be
used in advertising or publicity pertaining to distribution of the
software without specific, written prior permission.  


******************************************************************/
/*
 * Copyright 1993 by Digital Equipment Corporation, Maynard, Massachusetts.
 * 
 * Permission to use, copy, modify, distribute, and sell this software and its 
 * documentation for any purpose is hereby granted without fee, provided that 
 * the above copyright notice appear in all copies and that both that 
 * copyright notice and this permission notice appear in supporting 
 * documentation, and that the name of Digital not be used in advertising or 
 * publicity pertaining to distribution of the software without specific, 
 * written prior permission.  Digital makes no representations about the 
 * suitability of this software for any purpose.  It is provided "as is" 
 * without express or implied warranty.
 * 
 * DIGITAL DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING 
 * ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL 
 * DIGITAL BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY 
 * DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN 
 * AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF 
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 */

#include <string.h>
#include "audio.h"
#include "Aatom.h"
#include "misc.h"
#include "resource.h"

#define InitialTableSize 100

typedef struct _Node {
    struct _Node   *left,   *right;
    AAtom a;
    unsigned int fingerPrint;
    char   *string;
} NodeRec, *NodePtr;

static AAtom lastAtom = ANone;
static NodePtr atomRoot = (NodePtr)NULL;
static unsigned long tableLength;
static NodePtr *nodeTable;

AAtom 
MakeAtom(string, len, makeit)
    char *string;
    unsigned len;
    ABool makeit;
{
    register    NodePtr * np;
    unsigned i;
    int     comp;
    register unsigned int   fp = 0;

    np = &atomRoot;
    for (i = 0; i < (len+1)/2; i++)
    {
	fp = fp * 27 + string[i];
	fp = fp * 27 + string[len - 1 - i];
    }
    while (*np != (NodePtr) NULL)
    {
	if (fp < (*np)->fingerPrint)
	    np = &((*np)->left);
	else if (fp > (*np)->fingerPrint)
	    np = &((*np)->right);
	else
	{			       /* now start testing the strings */
	    comp = strncmp(string, (*np)->string, (int)len);
	    if ((comp < 0) || ((comp == 0) && (len < strlen((*np)->string))))
		np = &((*np)->left);
	    else if (comp > 0)
		np = &((*np)->right);
	    else
		return(*np)->a;
	    }
    }
    if (makeit)
    {
	register NodePtr nd;

	nd = (NodePtr) xalloc(sizeof(NodeRec));
	if (!nd)
	    return BAD_RESOURCE;
	if (lastAtom < AA_LAST_PREDEFINED)
	{
	    nd->string = string;
	}
	else
	{
	    nd->string = (char *) xalloc(len + 1);
	    if (!nd->string) {
		xfree(nd);
		return BAD_RESOURCE;
	    }
	    strncpy(nd->string, string, (int)len);
	    nd->string[len] = 0;
	}
	if ((lastAtom + 1) >= tableLength) {
	    NodePtr *table;

	    table = (NodePtr *) xrealloc(nodeTable,
					 tableLength * (2 * sizeof(NodePtr)));
	    if (!table) {
		if (nd->string != string)
		    xfree(nd->string);
		xfree(nd);
		return BAD_RESOURCE;
	    }
	    tableLength <<= 1;
	    nodeTable = table;
	}
	*np = nd;
	nd->left = nd->right = (NodePtr) NULL;
	nd->fingerPrint = fp;
	nd->a = (++lastAtom);
	*(nodeTable+lastAtom) = nd;
	return nd->a;
    }
    else
	return ANone;
}

ValidAtom(atom)
    AAtom atom;
{
    return (atom != ANone) && (atom <= lastAtom);
}

char *
NameForAtom(atom)
    AAtom atom;
{
    NodePtr node;
    if (atom > lastAtom) return 0;
    if ((node = nodeTable[atom]) == (NodePtr)NULL) return 0;
    return node->string;
}

AtomError()
{
    FatalError("initializing atoms");
}

void
FreeAtom(patom)
    NodePtr patom;
{
    if(patom->left)
	FreeAtom(patom->left);
    if(patom->right)
	FreeAtom(patom->right);
    if (patom->a > AA_LAST_PREDEFINED)
	xfree(patom->string);
    xfree(patom);
}
    
void
FreeAllAtoms()
{
    if(atomRoot == (NodePtr)NULL)
	return;
    FreeAtom(atomRoot);
    atomRoot = (NodePtr)NULL;
    xfree(nodeTable);
    nodeTable = (NodePtr *)NULL;
    lastAtom = ANone;
}

extern MakePredeclaredAtoms();

void
InitAtoms()
{
    FreeAllAtoms();
    tableLength = InitialTableSize;
    nodeTable = (NodePtr *)xalloc(InitialTableSize*sizeof(NodePtr));
    if (!nodeTable)
	AtomError();
    nodeTable[ANone] = (NodePtr)NULL;
    MakePredeclaredAtoms();
    if (lastAtom != AA_LAST_PREDEFINED)
	AtomError ();
}

