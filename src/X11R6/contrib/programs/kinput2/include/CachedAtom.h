/* $Id: CachedAtom.h,v 1.4 1991/09/16 05:12:16 ishisone Rel $ */
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

#ifndef _CachedAtom_h
#define _CachedAtom_h

/*
 * Atom CachedInternAtom(Display *dpy, String name, Bool only_if_exists)
 * String CachedGetAtomName(Display *dpy, Atom atom)
 *	それぞれ XInternAtom(), XGetAtomName() とほぼ同じ。
 *	キャッシングを行なうところが違う。(R5 の XInternAtom() は
 *	キャッシュするが XGetAtomName() はしない)
 *	また、CachedGetAtomName() が返す文字列は共有されているので
 *	free() してはならない。
 */

extern Atom CachedInternAtom(
#if NeedFunctionPrototypes
	Display *	/* dpy */,
	String		/* name */,
	Bool		/* only_if_exists */
#endif
);

extern String CachedGetAtomName(
#if NeedFunctionPrototypes
	Display *	/* dpy */,
	Atom		/* atom */
#endif
);

#endif
