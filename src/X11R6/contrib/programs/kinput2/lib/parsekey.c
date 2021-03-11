#ifndef lint
static char *rcsid = "$Id: parsekey.c,v 1.1 1994/05/16 05:42:30 ishisone Rel $";
#endif
/*- 
 * Copyright (c) 1991, 1994  Software Research Associates, Inc.
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

#include <X11/Xos.h>
#include <X11/IntrinsicP.h>
#include "ParseKey.h"

/*- parseModifiers: parse modifier list -*/
static void
parseModifiers(s, modp, chkmodp)
String s;
long *modp;		/* RETURN: modifiers which must be set */
long *chkmodp;		/* RETURN: modifiers to be checked */
{
    String p;
    int i;
    int c;
    static struct _moddesc_ {
	String modname;		/* modifier name */
	long modmask;		/* modifier mask */
    } mods[] = {
	{ "Shift",	ShiftMask },
	{ "Lock",	LockMask },
	{ "Ctrl",	ControlMask },
	{ "Meta",	Mod1Mask },
	{ "Alt",	Mod1Mask },
	{ "Mod1",	Mod1Mask },
	{ "Mod2",	Mod2Mask },
	{ "Mod3",	Mod3Mask },
	{ "Mod4",	Mod4Mask },
	{ "c",		ControlMask },
	{ "s",		ShiftMask },
	{ "l",		LockMask },
	{ "m",		Mod1Mask },
	{ "a",		Mod1Mask },
    };

    *modp = *chkmodp = 0L;

#define SKIPBLANK \
    while ((c = *s) == ' ' || c == '\t' || c == '\n') s++; \
    if (c == '\0') return;
#define SEARCHBLANK \
    p = s; while ((c = *p) != '\0' && c != ' ' && c != '\t' && c != '\n') p++;

    while (*s != '\0') {
	int tilde = 0;

	SKIPBLANK;
	if (c == '~') {
	    tilde = 1;
	    s++;
	    SKIPBLANK;
	}
	SEARCHBLANK;
	*p = '\0';
	for (i = 0; i < XtNumber(mods); i++) {
	    if (!strcmp(s, mods[i].modname)) {
		*chkmodp |= mods[i].modmask;
		if (!tilde) *modp |= mods[i].modmask;
		break;
	    }
	}
	if (c == '\0') break;
	s = p + 1;
    }
#undef SKIPBLANK
#undef SEARCHBLANK
}

/*- mystrstr: not-so-good implementaion of ANSI strstr() -*/
static char *
mystrstr(s1, s2)
char *s1;
char *s2;
{
    register char *p, *q;

    while (*(p = s1++) != '\0') {
	q = s2;
	do {
	    if (*q == '\0') return s1 - 1;
	} while (*p++ == *q++);
    }
    return NULL;
}


/*
 * Public function
 */

int
ParseKeyEvent(s, keysymp, modp, chkmodp)
String s;		/* IN: keyevent description */
KeySym *keysymp;	/* OUT: keysym */
long *modp;		/* OUT: mask of modifiers which must be pressed */
long *chkmodp;		/* OUT: mask of modifiers of interest */
{
    String key;
    String p;
    KeySym keysym;

    /*
     * keyevent description (stored in  argument 's') must be of the
     * following format (subset of Xt translation table syntax):
     *		modifier-list<Key>keysym
     * modifier-list is a combination of:
     *		Ctrl, Shift, Lock, Meta, Alt, Mod1, Mod2, Mod3, Mod4, Mod5
     * if '~' is prepended before a modifier, it means the modifier key should
     * not be pressed.
     */

    /* find "<Key>" */
    if ((p = mystrstr(s, "<Key>")) != NULL) {
	key = p + 5;	/* p + strlen("<Key>") */
    } else if ((p = mystrstr(s, "<KeyPress>")) != NULL) {
	key = p + 10;	/* p + strlen("<KeyPress>") */
    } else if ((p = mystrstr(s, "<KeyDown>")) != NULL) {
	key = p + 9;	/* p + strlen("<KeyDown>") */
    } else {
	return 0;
    }
    *p = '\0';
    while (*key == ' ' || *key == '\t') key++;
    p = key;
    while (*p != '\0' && *p != ' ' && *p != '\t') p++;
    *p = '\0';

    /* get modifier mask */
    parseModifiers(s, modp, chkmodp);

    /* get keycode */
    if ((keysym = XStringToKeysym(key)) == NoSymbol) return 0;

    *keysymp = keysym;

    return 1;
}
