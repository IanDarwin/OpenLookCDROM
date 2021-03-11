#ifndef lint
static char *rcsid = "$Id: fontset.c,v 1.9 1994/06/02 04:59:23 ishisone Rel $";
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
 *  a FontSet handler for kinput2.
 */

#include <X11/Intrinsic.h>
#include <X11/Xatom.h>
#include <X11/Xmu/CharSet.h>
#include "CachedAtom.h"
#include "CachedFont.h"
#include "KIFontSet.h"

#define DEBUG_VAR debug_fontset
#include "DebugPrint.h"

typedef struct {
    String name;
    String xlfdname;
} FSFontName;

static char noname[] = "no";

static int match();
static Cardinal parseFontSet();
static Boolean tryCharSet();
static XFontStruct *csSuppliedMatchFont();
static XFontStruct *exactMatchFont();
static XFontStruct *csReplacedMatchFont();
static String getXLFDName();


/*- match: returns 1 if the specified string matches the pattern -*/
static int
match(s, p)
register char *s;	/* string */
register char *p;	/* pattern */
{
    register int tmp;

    while ((tmp = *p++) != '\0') {
	switch (tmp) {
	case '?':
	    if (*s++ == '\0') return 0;
	    continue;
	case '*':
	    while ((tmp = match(s, p)) == 0) {
		if (*s++ == '\0') return -1;
	    }
	    return tmp;
	default:
	    if (*s++ != tmp) return 0;
	    continue;
	}
    }
    return (*s == '\0');
}

/*- countCommas: count number of commas in the string -*/
static Cardinal
countCommas(s)
String s;
{
    int n = 0;

    while (*s != '\0') {
	if (*s++ == ',') n++;
    }
    return n;
}

/*- parseFontSet: separate each font in a font name list -*/
static Cardinal
parseFontSet(spec, basenames)
String spec;		/* IN */
FSFontName *basenames;	/* OUT */
{
    register char *p, *q;
    Cardinal nnames;
    int c;

    nnames = 0;
    p = spec;
    for (;;) {
	/* skip leading blanks */
	while ((c = *p) != '\0' && (c == ' ' || c == '\t' || c == '\n')) p++;
	if (c == '\0') break;

	basenames[nnames++].name = p;

	/* find comma or NUL char */
	for (q = p; (c = *q) != '\0' && c != ','; q++) ;

	/* omit trailing blanks */
	p = q - 1;
	while ((c = *p) == ' ' || c == '\t' || c == '\n') *p-- = '\0';

	if (*q == '\0') break;
	*q = '\0';
	p = q + 1;
    }

#ifdef DEBUG
    if (DDEBUG_CONDITION(10)) {
	if (nnames == 0) {
	    printf("\tparse error\n");
	} else {
	    int i;
	    printf("\t%d elements\n", nnames);
	    for (i = 0; i < nnames; i++) printf("\t\t%s\n", basenames[i].name);
	}
    }
#endif

    return nnames;
}

/*- tryCharSet: apply specified function to the fontnames for getting appropriate font for given character set -*/
static Boolean
tryCharSet(dpy, cset, extfont, namelist, numlist, func)
Display *dpy;
KICharSet *cset;
KICharSetFont *extfont;
FSFontName *namelist;
Cardinal numlist;
XFontStruct *(*func)();
{
    KICharSetSpec *specs;
    Cardinal i;
    XFontStruct *font;

    for (i = 0, specs = cset->specs; i < cset->num_specs; i++, specs++) {
	font = (*func)(dpy, specs->pattern, namelist, numlist);
	if (font != NULL) {
	    extfont->charset = specs->pattern;
	    extfont->cldata = specs->cldata;
	    extfont->font = font;
	    return True;
	}
    }
    return False;
}

static XFontStruct *
exactMatchFont(dpy, csetstr, namelist, numlist)
Display *dpy;
String csetstr;
FSFontName *namelist;
Cardinal numlist;
{
    XFontStruct *font;

    while (numlist-- > 0) {
	String name = (namelist++)->name;
	String p;
	int namelen;
	int hyphen;
	
	if (*name != '-' && *name != '+') continue;	/* not an XLFD name */
	
	namelen = strlen(name);
	for (p = name + namelen - 1, hyphen = 0; p > name; p--) {
	    if (*p == '-') hyphen++;
	    if (hyphen == 2) goto found;
	}
	continue;	/* doesn't contain charset part */

    found:
	p++;		/* now p points the charset part of the fontname */
	if (match(p, csetstr) > 0 &&
	    (font = CachedLoadQueryFontByName(dpy, name)) != NULL) {
	    TRACE(("\tmatched in exactMatchFont()\n"));
	    TRACE(("\t\tcset=%s, font=%s\n", csetstr, name));
	    return font;
	}
    }
    return NULL;
}

static XFontStruct *
csSuppliedMatchFont(dpy, csetstr, namelist, numlist)
Display *dpy;
String csetstr;
FSFontName *namelist;
Cardinal numlist;
{
    XFontStruct *font;

    while (numlist-- > 0) {
	String name = (namelist++)->name;
	char namebuf[512];

	if (*name != '-' && *name != '+') continue;	/* not an XLFD name */

	(void)strcpy(namebuf, name);
	(void)strcat(namebuf, "-");
	(void)strcat(namebuf, csetstr);

	if ((font = CachedLoadQueryFontByName(dpy, namebuf)) != NULL) {
	    TRACE(("\tmatched in csSuppliedMatchFont()\n"));
	    TRACE(("\t\tcset=%s, font=%s\n", csetstr, namebuf));
	    return font;
	}
    }
    return NULL;
}

static XFontStruct *
csReplacedMatchFont(dpy, csetstr, namelist, numlist)
Display *dpy;
String csetstr;
FSFontName *namelist;
Cardinal numlist;
{
    XFontStruct *font;

    while (numlist-- > 0) {
	String name = namelist->name;
	char namebuf[512];
	String p;
	int namelen;
	int hyphen;
	
	if (*name != '-' && *name != '+') {	/* non XLFD name */
	    if (namelist->xlfdname == NULL ||
		(namelist->xlfdname == noname &&
		(namelist->xlfdname = getXLFDName(dpy, name)) == NULL)) {
		/* this font doesn't have XLFD name */
		namelist++;
		continue;
	    }
	    name = namelist->xlfdname;
	}
	namelist++;

	(void)strcpy(namebuf, name);
	namelen = strlen(namebuf);

	/* find charset part of the font name */
	for (p = namebuf + namelen - 1, hyphen = 0; p > namebuf; p--) {
	    if (*p == '-') hyphen++;
	    if (hyphen == 2) goto found;
	}
	continue;	/* doesn't contain charset part */

    found:
	p++;		/* now p points the charset part of the fontname */

	/* replace charset part */
	(void)strcpy(p, csetstr);

	if ((font = CachedLoadQueryFontByName(dpy, namebuf)) != NULL) {
	    TRACE(("\tmatched in csReplacedMatchFont()\n"));
	    TRACE(("\t\tcset=%s, font=%s\n", csetstr, namebuf));
	    return font;
	}
    }
    return NULL;
}

/*- getXLFDName: obtain XLFD font name from a non XLFD font name -*/
static String
getXLFDName(dpy, nonxlfdname)
Display *dpy;
String nonxlfdname;
{
    XFontStruct *font;
    Atom fontprop;
    String name;

    TRACE(("getXLFDName(%s)\n", nonxlfdname));
    font = CachedLoadQueryFontByName(dpy, nonxlfdname);
    if (font == NULL) {
	DPRINT(("getXLFDName(%s):CachedLoadQueryFontByName() failed\n",
		nonxlfdname));
	return NULL;
    }
    if (!XGetFontProperty(font, XA_FONT, (unsigned long *)&fontprop)) {
	/* can't get 'FONT' property. so XLFD name cannot be obtained */
	DPRINT(("getXLFDName(%s): can't get FONT property\n",
		nonxlfdname));
	CachedFreeFont(dpy, font);
	return NULL;
    }

    CachedFreeFont(dpy, font);
    name = CachedGetAtomName(dpy, fontprop);
    TRACE(("\tgot %s\n", name));
    return (*name == '-' || *name == '+') ? name : NULL;
}


/*
 * public functions
 */

int
ExtractFontsFromFontSet(dpy, fontset, charsets, exfonts, numcsets)
Display *dpy;
String fontset;
KICharSet *charsets;
KICharSetFont *exfonts;
Cardinal numcsets;
{
    Cardinal nfonts;
    String fsp;
    char fsbuf[1024];
    FSFontName *fnp;
    FSFontName fnbuf[20];
    int nfound;
    int i;

    TRACE(("ExtractFontsFromFontSet(fontset=%s)\n", fontset));
    for (i = 0; i < numcsets; i++) {
	exfonts[i].font = NULL;
	exfonts[i].charset = NULL;
	exfonts[i].cldata = NULL;
    }

    if (strlen(fontset) >= sizeof(fsbuf)) {
	fsp = XtMalloc((unsigned int)(strlen(fontset) + 1));
    } else {
	fsp = fsbuf;
    }
    XmuCopyISOLatin1Lowered(fsp, fontset);

    if ((nfonts = countCommas(fsp) + 1) >= XtNumber(fnbuf)) {
	fnp = (FSFontName *)XtMalloc(nfonts * sizeof(FSFontName));
    } else {
	fnp = fnbuf;
    }
    for (i = 0; i < nfonts; i++) fnp[i].xlfdname = noname;

    if ((nfonts = parseFontSet(fsp, fnp)) == 0) {
	if (fsp != fsbuf) XtFree(fsp);
	if (fnp != fnbuf) XtFree((char *)fnp);
	return 0;
    }

    nfound = 0;
    for (i = 0; i < numcsets; i++) {
	KICharSet *cp = charsets + i;
	KICharSetFont *fp = exfonts + i;

	TRACE(("\tfor charset #%d\n", i));
	if (tryCharSet(dpy, cp, fp, fnp, nfonts, exactMatchFont) ||
	    tryCharSet(dpy, cp, fp, fnp, nfonts, csSuppliedMatchFont) ||
	    tryCharSet(dpy, cp, fp, fnp, nfonts, csReplacedMatchFont)) {
	    nfound++;
	}
    }
    if (fsp != fsbuf) XtFree(fsp);
    if (fnp != fnbuf) XtFree((char *)fnp);

    return nfound;
}
