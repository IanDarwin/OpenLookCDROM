#ifndef lint
static char *rcsid = "$Id: fontbank.c,v 1.4 1994/05/17 10:52:15 ishisone Rel $";
#endif
/*
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
#include <X11/Intrinsic.h>
#include "CachedFont.h"
#include "KIFontSet.h"
#include "FontBank.h"

#define DEBUG_VAR debug_fontbank
#include "DebugPrint.h"

#define FC_HASH_SIZE	8

typedef struct _font_cache_ {
    int sum;
    char *name;
    XFontStruct **fonts;
    int num_fonts;
    struct _font_cache_ *next;
} FontCache;

typedef struct _fb_info_ {
    char *language;
    KICharSet *charsets;
    int num_charsets;
} FBInfo;

typedef struct _fb_rec_ {
    Display *dpy;
    struct _fb_rec_ *next;
    FBInfo *info;
    int reference_count;
    FontCache *hash[FC_HASH_SIZE];
} FBRec;
    
static FontBank fontBankList = NULL;

/*
 * Language information:
 * The FBInfo structure holds information about the character sets
 * required for a specific language.  Currently, this infomation
 * is provided only for Japanese.
 */

static KICharSetSpec asciiCharSets[] = {
    { "iso8859-1", NULL },		/* preferable */
    { "jisx0201.1976-0", NULL },	/* alternative */
    { "iso8859-*", NULL },		/* alternative */
};
static KICharSetSpec kanaCharSets[] = {
    { "jisx0201.1976-0", NULL },
};
static KICharSetSpec kanjiCharSets[] = {
    { "jisx0208.1983-0", NULL },
    { "jisx0208.1983-1", NULL },
    { "jisx0208.1990-0", NULL },
    { "jisx0208.1990-1", NULL },
    { "jisx0208.1976-0", NULL },
    { "jisx0208.1976-1", NULL },
};
static KICharSet jpCharSets[3] = {
    { asciiCharSets, XtNumber(asciiCharSets) },
    { kanaCharSets, XtNumber(kanaCharSets) },
    { kanjiCharSets, XtNumber(kanjiCharSets) },
};

static FBInfo fontBankInfo[] = {
    { "ja_JP", jpCharSets, XtNumber(jpCharSets) },
    { NULL }
};

static int
getsum(s)
char *s;
{
    unsigned char *p = (unsigned char *)s;
    int sum = 0;

    while (*p != '\0') sum += *p++;
    return sum;
}

static FBInfo *
getInfo(lang)
char *lang;
{
    FBInfo *fip;

    for (fip = fontBankInfo; fip->language != NULL; fip++) {
	if (!strcmp(fip->language, lang)) return fip;
    }
    DPRINT(("fonbank: language %s not supported\n", lang));
    return NULL;
}

static XFontStruct **
lookupCacheFonts(bank, fontset, num_fontsp)
FontBank bank;
char *fontset;
int *num_fontsp;
{
    FontCache *fc;
    int i;
    int sum;

    sum = getsum(fontset);
    fc = bank->hash[sum % FC_HASH_SIZE];

    /* lookup cache */
    while (fc != NULL) {
	if (fc->sum == sum && !strcmp(fc->name, fontset)) {
	    /* found */
	    *num_fontsp = fc->num_fonts;
	    for (i = 0; i < *num_fontsp; i++) {
		(void)CachedLoadFontByFontStruct(bank->dpy, fc->fonts[i]);
	    }
	    return fc->fonts;
	}
	fc = fc->next;
    }
    *num_fontsp = 0;
    return NULL;
}

static void
cacheFonts(bank, fontset, fonts, num_fonts)
FontBank bank;
char *fontset;
XFontStruct **fonts;
int num_fonts;
{
    FontCache *fc;
    int sum;

    fc = XtNew(FontCache);
    fontset = XtNewString(fontset);
    sum = getsum(fontset);

    fc->sum = sum;
    fc->name = fontset;
    fc->fonts = fonts;
    fc->num_fonts = num_fonts;
    fc->next = bank->hash[sum % FC_HASH_SIZE];
    bank->hash[sum % FC_HASH_SIZE] = fc;
}

static XFontStruct **
extractFonts(dpy, fontset, charsets, ncharsets, nfontsp)
Display *dpy;
char *fontset;
KICharSet *charsets;
int ncharsets;
int *nfontsp;
{
    KICharSetFont *kifonts;
    KICharSetFont buf[10];
    XFontStruct **fonts, **fp;

    if (ncharsets > 10) {
	kifonts = (KICharSetFont *)XtMalloc(ncharsets * sizeof(KICharSetFont));
    } else {
	kifonts = buf;
    }
    *nfontsp = ExtractFontsFromFontSet(dpy, fontset, charsets,
				       kifonts, ncharsets);
    fonts = NULL;
    if (*nfontsp > 0) {
	int i;

	fonts = (XFontStruct **)XtMalloc(*nfontsp * sizeof(XFontStruct *));
	for (i = 0, fp = fonts; i < ncharsets; i++) {
	    if (kifonts[i].font != NULL) *fp++ = kifonts[i].font;
	}
    }
    if (kifonts != buf) XtFree((char *)kifonts);
    return fonts;
}

static void
freeCache(bank)
FontBank bank;
{
    FontCache *fc;
    int i;

    for (i = 0; i < FC_HASH_SIZE; i++) {
	fc = bank->hash[i];
	while (fc != NULL) {
	    FontCache *next = fc->next;

	    XtFree(fc->name);
	    XtFree((char *)fc->fonts);
	    XtFree((char *)fc);
	    fc = next;
	}
    }
}


/*
 * Public functions
 */

FontBank
FontBankCreate(dpy, language)
Display *dpy;
char *language;
{
    FontBank fb;
    FBInfo *info;
    int i;

    TRACE(("FontBankCreate(language:%s)\n", language));

    if ((info = getInfo(language)) == NULL) return NULL;

    for (fb = fontBankList; fb != NULL; fb = fb->next) {
	if (fb->dpy == dpy && fb->info == info) {
	    TRACE(("\tfontbank for %s already exists\n", language));
	    fb->reference_count++;
	    return fb;
	}
    }

    TRACE(("\tcreate fontbank for %s...\n", language));
    fb = XtNew(FBRec);
    fb->dpy = dpy;
    fb->info = info;
    fb->reference_count = 1;
    for (i = 0; i < FC_HASH_SIZE; i++) {
	fb->hash[i] = NULL;
    }
    fb->next = fontBankList;
    fontBankList = fb;
    return fb;
}

void
FontBankDestroy(bank)
FontBank bank;
{
    TRACE(("FontBankDestroy()\n"));

    if (--(bank->reference_count) <= 0) {
	FontBank fb, fb0;

	TRACE(("\tfreeing fontbank...\n"));
	fb = fontBankList;
	fb0 = NULL;
	while (fb != NULL) {
	    if (fb == bank) {
		if (fb0 == NULL) {
		    fontBankList = fb->next;
		} else {
		    fb0->next = fb->next;
		}
		freeCache(fb);
		XtFree((char *)fb);
		return;
	    }
	    fb0 = fb;
	    fb = fb->next;
	}
    }
}

XFontStruct **
FontBankGet(bank, fontset, num_fontsp)
FontBank bank;
char *fontset;
int *num_fontsp;
{
    XFontStruct **fpp;

    TRACE(("FontBankGet(fontset:%s)\n", fontset));

    if ((fpp = lookupCacheFonts(bank, fontset, num_fontsp)) != NULL) {
	TRACE(("\tfontset found in fontbank (numfonts=%d)\n",
	       *num_fontsp));
	return fpp;
    }

    fpp = extractFonts(bank->dpy, fontset, bank->info->charsets,
		       bank->info->num_charsets, num_fontsp);

    /* enter cache */
    TRACE(("\tcaching fontset (numfonts=%d)\n", *num_fontsp));
    cacheFonts(bank, fontset, fpp, *num_fontsp);
    return fpp;
}

void
FontBankFreeFonts(bank, fonts, num_fonts)
FontBank bank;
XFontStruct **fonts;
int num_fonts;
{
    int i;

    TRACE(("FontBankFreeFonts()\n"));

    for (i = 0; i < num_fonts; i++) {
	CachedFreeFont(bank->dpy, fonts[i]);
    }
}
