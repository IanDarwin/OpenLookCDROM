#ifndef lint
static char *rcsid = "$Id: cachedfont.c,v 1.13 1994/05/17 10:51:54 ishisone Rel $";
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
 */

#include <X11/Intrinsic.h>
#include <X11/Xatom.h>
#include <X11/Xmu/CharSet.h>
#include "CachedAtom.h"
#include "CachedFont.h"
#include "AsyncErr.h"

#define DEBUG_VAR debug_cachedfont
#include "DebugPrint.h"

typedef struct _fontrec_ {
    Display *dpy;
    String *names;		/* full name or aliases... (in lower case) */
    Cardinal num_names;
    Atom fontprop;		/* 'FONT' property value */
    XFontStruct *font;
    int refcnt;
    struct _fontrec_ *next;
} FontRec;

static FontRec	*FRP = NULL;

static FontRec *
LookupByAtom(dpy, atom)
Display *dpy;
Atom atom;
{
    register FontRec *frp;

    for (frp = FRP; frp != NULL; frp = frp->next) {
	if (frp->dpy == dpy && frp->fontprop == atom) return frp;
    }
    return NULL;
}

static FontRec *
LookupByName(dpy, name)
Display *dpy;
String name;
{
    register FontRec *frp;
    register String *names;
    register Cardinal i;

    for (frp = FRP; frp != NULL; frp = frp->next) {
	if (frp->dpy != dpy) continue;

	for (i = frp->num_names, names = frp->names; i > 0; i--, names++) {
	    if (!strcmp(*names, name)) return frp;
	}
    }
    return NULL;
}

static void
AddName(frp, name)
register FontRec *frp;
String name;
{
    String dup = XtNewString(name);

    if (frp->num_names++ == 0) {
	frp->names = (String *)XtMalloc(sizeof(String));
    } else {
	frp->names = (String *)XtRealloc((char *)frp->names,
					 sizeof(String) * frp->num_names);
    }
    frp->names[frp->num_names - 1] = dup;
}

static void
LoadFont(frp)
register FontRec *frp;
{
    if (frp->font->fid == None) {
	frp->font->fid = XLoadFont(frp->dpy, frp->names[0]);
	frp->refcnt = 0;	/* reset */
    }
}

static XFontStruct *
AddFont(dpy, name)
Display *dpy;
char *name;
{
    FontRec *frp;
    XFontStruct *font;
    Atom fontprop;

    font = XLoadQueryFont(dpy, name);
    if (font == NULL) return NULL;

    if (!XGetFontProperty(font, XA_FONT, (unsigned long *)&fontprop)) {
	/* well, make it by myself */
	DPRINT(("AddFont(): %s doesn't have FONT property\n", name));
	fontprop = CachedInternAtom(dpy, name, False);
    }

    if ((frp = LookupByAtom(dpy, fontprop)) != NULL) {
	/* already loaded. use it */
	TRACE(("\tfound in the cache (alias name?)\n"));
	AddName(frp, name);
	frp->refcnt++;
	if (frp->font->fid == None) {
	    frp->font->fid = font->fid;
	    XFreeFontInfo((char **)NULL, font, 1);
	} else {
	    XFreeFont(dpy, font);
	}
	return frp->font;
    }

    TRACE(("\tnot found in the cache\n"));
    frp = XtNew(FontRec);
    frp->dpy = dpy;
    frp->num_names = 0;
    AddName(frp, name);
    frp->fontprop = fontprop;
    frp->font = font;
    frp->refcnt = 1;
    frp->next = FRP;
    FRP = frp;

    return frp->font;
}


/*
 *	Public functions
 */

XFontStruct *
CachedLoadQueryFontByName(dpy, name)
Display *dpy;
String name;
{
    FontRec *frp;
    XFontStruct *font;
    char buf[256];
    char *loweredname = buf;
    int len = strlen(name) + 1;

    if (len > sizeof(buf)) loweredname = XtMalloc(len);

    XmuCopyISOLatin1Lowered(loweredname, name);

    TRACE(("CachedLoadQueryFontByName(name=%s)\n", name));
    if ((frp = LookupByName(dpy, loweredname)) != NULL) {
	TRACE(("\tfound in the cache\n"));
	LoadFont(frp);
	frp->refcnt++;
	if (loweredname != buf) XtFree(loweredname);
	return frp->font;
    }

    font = AddFont(dpy, loweredname);
    if (loweredname != buf) XtFree(loweredname);
    return font;
}

XFontStruct *
CachedLoadQueryFontByProp(dpy, atom)
Display *dpy;
Atom atom;
{
    FontRec *frp;
    XFontStruct *font;
    String name;
    XAEHandle h;

    TRACE(("CachedLoadQueryFontByProp(atom=%ld)\n", atom));
    if ((frp = LookupByAtom(dpy, atom)) != NULL) {
	TRACE(("\tfound in the cache\n"));
	LoadFont(frp);
	frp->refcnt++;
	return frp->font;
    }

    /* get fontname */
    /* make it safe... */
    h = XAESetIgnoreErrors(dpy);
    name = CachedGetAtomName(dpy, atom);
    XAEUnset(h);

    TRACE(("\tnot found. got font name: %s\n", name ? name : "<null>"));

    if (name == NULL) {
	font = NULL;
    } else {
	char buf[256];
	char *loweredname = buf;
	int len = strlen(name) + 1;

	if (len > sizeof(buf)) loweredname = XtMalloc(len);
	XmuCopyISOLatin1Lowered(loweredname, name);
	font = AddFont(dpy, loweredname);
	if (loweredname != buf) XtFree(loweredname);
    }
    return font;
}

/* ARGSUSED */
XFontStruct *
CachedLoadFontByFontStruct(dpy, font)
Display *dpy;
XFontStruct *font;
{
    register FontRec *frp;

    TRACE(("CachedLoadFontByFontStruct(fid=%08lx)\n", font->fid));
    for (frp = FRP; frp != NULL; frp = frp->next) {
	if (frp->font == font) {
	    if (frp->refcnt++ == 0) {
		/* load it */
		TRACE(("\trefcnt == 0. loading %s again...\n", frp->names[0]));
		frp->font->fid = XLoadFont(frp->dpy, frp->names[0]);
	    }
	    return font;
	}
    }

    /* not found */
    TRACE(("\tfont not found in the cache\n"));
    return NULL;
}

/* ARGSUSED */
void
CachedFreeFont(dpy, font)
Display *dpy;
XFontStruct *font;
{
    register FontRec *frp;

    TRACE(("CachedFreeFont(fid=%08lx)\n", font->fid));
    for (frp = FRP; frp != NULL; frp = frp->next) {
	if (frp->font == font) {
	    if (--frp->refcnt == 0) {
		/* unload it, but not free its structure for later use */
		TRACE(("\trefcnt == 0. unloading...\n"));
		XUnloadFont(frp->dpy, frp->font->fid);
		frp->font->fid = None;
	    }
	    return;
	}
    }

    /* not found. free anyway... */
    TRACE(("\tfont not found in the cache. free anyway...\n"));
    XFreeFont(dpy, font);
}
