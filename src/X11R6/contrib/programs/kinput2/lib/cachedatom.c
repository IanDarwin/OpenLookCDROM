#ifndef lint
static char *rcsid = "$Id: cachedatom.c,v 1.9 1994/05/17 04:48:01 ishisone Rel $";
#endif
/*
 * Copyright (c) 1991  Software Research Associates, Inc.
 *
 * Permission to use, copy, modify, and distribute this software and its
 * documentation for any purpose and without fee is hereby granted, provided
 * that the above copyright notice appear in all copies and that both that
 * copyright notice and this permission notice appear in supporting
 * documentation, and that the name of Software Research Associates not be
 * used in advertising or publicity pertaining to distribution of the
 * software without specific, written prior permission.  Software Research
 * Associates makes no representations about the suitability of this software
 * for any purpose.  It is provided "as is" without express or implied
 * warranty.
 *
 * Author:  Makoto Ishisone, Software Research Associates, Inc., Japan
 *
 *  neither R5 Xlib nor Xmu doesn't do what I want, so I wrote one myself.
 */

#include <X11/Intrinsic.h>
#include "CachedAtom.h"

#define DEBUG_VAR debug_cachedatom
#include "DebugPrint.h"

typedef struct _atomrec_ {
    Atom atom;				/* atom number */
    Display *dpy;
    String name;			/* for obtaining atom name */
    struct _atomrec_ *nextOfName;	/* next element of atom name list */
    struct _atomrec_ *nextOfNumber;	/* next element of atom number list */
} CachedAtomRec;

typedef struct _cAtomNameRec_ {
    String name;			/* atom name */
    CachedAtomRec atomrec;		/* caution: not a pointer! */
    struct _cAtomNameRec_ *next;	/* next entry of the same hash value */
} CachedAtomNameRec;

#define NAMEHASHSIZE	128
#define NUMBERHASHSIZE	128

static CachedAtomNameRec *nameHash[NAMEHASHSIZE];
					/* for searching by name */
static CachedAtomRec *numberHash[NUMBERHASHSIZE];
					/* for searching by number */


/*
 * iternal functions
 */

static int
nameHashFunc(s)
String s;
{
    register String sp = s;
    register int c, sum;

    sum = 0;
    while ((c = *sp++) != '\0') sum += c;

    return (sum ^ (sp - s)) % NAMEHASHSIZE;
}

#define numberHashFunc(atom)	((int)((atom) % NUMBERHASHSIZE))

static CachedAtomRec *
newAtomRec(dpy, atom, arp, nrp)
Display *dpy;
Atom atom;
CachedAtomRec *arp;	/* NULL (create new rec) or &nrp->atomrec */
CachedAtomNameRec *nrp;
{
    int hashvalue;

    if (arp == NULL) arp = XtNew(CachedAtomRec);
    arp->atom = atom;
    arp->dpy = dpy;
    arp->name = nrp->name;

    if (arp == &(nrp->atomrec)) {
	arp->nextOfName = NULL;
    } else {
	arp->nextOfName = nrp->atomrec.nextOfName;
	nrp->atomrec.nextOfName = arp;
    }

    /* insert it in numberHash */
    hashvalue = numberHashFunc(atom);
    arp->nextOfNumber = numberHash[hashvalue];
    numberHash[numberHashFunc(atom)] = arp;

    return arp;
}

static CachedAtomNameRec *
newNameRec(name, hashvalue)
String name;
int hashvalue;
{
    CachedAtomNameRec *nrp = XtNew(CachedAtomNameRec);

    nrp->name = XtNewString(name);
    nrp->next = nameHash[hashvalue];
    nameHash[hashvalue] = nrp;
    return nrp;
}
    
/*
 * public functions
 */

Atom
CachedInternAtom(dpy, name, exists)
Display *dpy;
char *name;
Bool exists;
{
    int hashvalue = nameHashFunc(name);
    CachedAtomNameRec *nrp = nameHash[hashvalue];
    Atom atom;

    TRACE(("CachedInternAtom(name:%s)...  ", name));
    while (nrp != NULL) {
	if (!strcmp(nrp->name, name)) {
	    CachedAtomRec *arp = &(nrp->atomrec);

	    do {
		if (arp->dpy == dpy) {
		    TRACE(("found in the cache (%ld)\n", arp->atom));
		    return arp->atom;
		}
		arp = arp->nextOfName;
	    } while (arp != NULL);

	    TRACE(("not in the cache\n"));
	    if ((atom = XInternAtom(dpy, name, exists)) == None) return None;
	    (void)newAtomRec(dpy, atom, (CachedAtomRec *)NULL, nrp);
	    return atom;
	}
	nrp = nrp->next;
    }

    TRACE(("not in the cache\n"));
    if ((atom = XInternAtom(dpy, name, exists)) == None) return None;
    nrp = newNameRec(name, hashvalue);
    (void)newAtomRec(dpy, atom, &(nrp->atomrec), nrp);

    return atom;
}

String
CachedGetAtomName(dpy, atom)
Display *dpy;
Atom atom;
{
    int hashvalue = numberHashFunc(atom);
    CachedAtomRec *arp = numberHash[hashvalue];
    CachedAtomNameRec *nrp;
    String name;

    TRACE(("CachedGetAtomName(atom=%ld)...  ", atom));
    while (arp != NULL) {
	if (arp->atom == atom && arp->dpy == dpy) {
	    TRACE(("found in the cache (%s)\n", arp->name));
	    return arp->name;
	}
	arp = arp->nextOfNumber;
    }
    TRACE(("not in the cache\n"));
    if ((name = XGetAtomName(dpy, atom)) == NULL) return NULL;
    hashvalue = nameHashFunc(name);
    nrp = newNameRec(name, hashvalue);
    XFree(name);
    (void)newAtomRec(dpy, atom, &(nrp->atomrec), nrp);
    return nrp->name;
}

#ifdef DEBUG
void
dumpAtomCache()
{
    int i;

    for (i = 0; i < NAMEHASHSIZE; i++) {
	CachedAtomNameRec *nrp = nameHash[i];
	int n = 0;

	while (nrp != NULL) {
	    n++;
	    nrp = nrp->next;
	}
	printf("nameHash[%3d]: %d\n", i, n);
    }
    for (i = 0; i < NUMBERHASHSIZE; i++) {
	CachedAtomRec *arp = numberHash[i];
	int n = 0;

	while (arp != NULL) {
	    n++;
	    arp = arp->nextOfNumber;
	}
	printf("numberHash[%3d]: %d\n", i, n);
    }
}
#endif
