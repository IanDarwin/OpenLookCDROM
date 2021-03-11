#ifndef lint
static char *rcsid = "$Id: WcharDisp.c,v 1.22 1993/09/08 02:43:27 ishisone Rel $";
#endif
/*
 * Copyright (c) 1990  Software Research Associates, Inc.
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

#include <X11/IntrinsicP.h>
#include <X11/StringDefs.h>
#include <X11/Xmu/CharSet.h>
#include "CachedAtom.h"
#include "WcharDispP.h"
#include "LocalAlloc.h"

#define DEBUG_VAR debug_wcharDisplay
#include "DebugPrint.h"

/*
 * R5 servers implicitly changes font properties when an alias name is
 * used.  The character set properties (CHARSET_REGISTRY and
 * CHARSET_ENCODING) are taken from the XLFD fontname.  So if the
 * fontname is represented in lower-case letters, for example:
 *    -jis-fixed-medium-r-normal--16-110-100-100-c-160-jisx0208.1983-0
 *       (this example is taken from R5 fonts/misc/fonts.alias file)
 * then, the value of CHARSET_REGISTRY becomes "jisx0208.1983",
 * instead of the registered charset name "JISX0208.1983".
 * The following flag forces kinput2 to accept these invalid lower
 * case charset names as well.
 */
#define ALLOW_LOWERCASE_CHARSET_NAME

static FontMapping defaultMapping = { { False, False, False, False } };

static XtResource resources[] = {
#define offset(field) XtOffset(WcharDisplayObject, wcharDisplay.field)
    { XtNfontG0, XtCFont, XtRFontStruct, sizeof (XFontStruct *),
	offset(defaultfonts[0]), XtRString, XtDefaultFont },
    { XtNfontG1, XtCFont, XtRFontStruct, sizeof (XFontStruct *),
	offset(defaultfonts[1]), XtRImmediate, (XtPointer)NULL },
    { XtNfontG2, XtCFont, XtRFontStruct, sizeof (XFontStruct *),
	offset(defaultfonts[2]), XtRImmediate, (XtPointer)NULL },
    { XtNfontG3, XtCFont, XtRFontStruct, sizeof (XFontStruct *),
	offset(defaultfonts[3]), XtRImmediate, (XtPointer)NULL },
    { XtNfontMapping, XtCFontMapping, XtRFontMapping, sizeof (FontMapping),
	offset(defaultmapping), XtRFontMapping, (XtPointer)&defaultMapping },
#undef offset
};

static WDCharSet defCharSet[] = {
    { "ISO8859-1",		G0LCharSet },
    { "JISX0201.1976-0",	G0LCharSet },	/* alternative */
#ifdef ALLOW_LOWERCASE_CHARSET_NAME
    { "iso8859-1",		G0LCharSet },
    { "jisx0201.1976-0",	G0LCharSet },	/* alternative */
#endif
};

static void ClassInitialize();
static void StringToFontMapping();

static void Initialize();
static void Destroy();
static Boolean SetValues();

static void GetAtoms();
static void GetGC();
static void ChangeFont();

static int StringWidth();
static int LineHeight();
static void DrawString();
static int MaxChar();
static void SetFonts();

static int countControlChars();
static void expandControlChars();
static int charWidth();

WcharDisplayClassRec wcharDisplayClassRec = {
  { /* object fields */
    /* superclass		*/	(WidgetClass)&convDisplayClassRec,
    /* class_name		*/	"WcharDisplay",
    /* widget_size		*/	sizeof(WcharDisplayRec),
    /* class_initialize		*/	ClassInitialize,
    /* class_part_initialize	*/	NULL,
    /* class_inited		*/	FALSE,
    /* initialize		*/	Initialize,
    /* initialize_hook		*/	NULL,
    /* obj1			*/	NULL,
    /* obj2			*/	NULL,
    /* obj3			*/	0,
    /* resources		*/	resources,
    /* num_resources		*/	XtNumber(resources),
    /* xrm_class		*/	NULLQUARK,
    /* obj4			*/	FALSE,
    /* obj5			*/	FALSE,
    /* obj6			*/	FALSE,
    /* obj7			*/	FALSE,
    /* destroy			*/	Destroy,
    /* obj8			*/	NULL,
    /* obj9			*/	NULL,
    /* set_values		*/	SetValues,
    /* set_values_hook		*/	NULL,
    /* obj10			*/	NULL,
    /* get_values_hook		*/	NULL,
    /* obj11			*/	NULL,
    /* version			*/	XtVersion,
    /* callback_private		*/	NULL,
    /* obj12			*/	NULL,
    /* obj13			*/	NULL,
    /* obj14			*/	NULL,
    /* extension		*/	NULL
  },
  { /* convDisplay fields */
    /* StringWidth		*/	StringWidth,
    /* LineHeight		*/	LineHeight,
    /* DrawString		*/	DrawString,
    /* MaxChar			*/	MaxChar,
    /* DrawCursor		*/	XtInheritDrawCursor,
    /* GetCursorBounds		*/	XtInheritGetCursorBounds,
    /* SetFonts			*/	SetFonts,
  },
  { /* wcharDisplay fields */
    /* charset_specs		*/	defCharSet,
    /* num_specs		*/	XtNumber(defCharSet),
  }
};

WidgetClass wcharDisplayObjectClass = (WidgetClass)&wcharDisplayClassRec;

/* ARGSUSED */
static void
ClassInitialize()
{
    /* add String -> FontMapping converter */
    XtAddConverter(XtRString, XtRFontMapping, StringToFontMapping,
		   (XtConvertArgList)NULL, (Cardinal)0);
}

/* ARGSUSED */
static void
StringToFontMapping(args, num_args, from, to)
XrmValue *args;
Cardinal *num_args;
XrmValue *from;
XrmValue *to;
{
    char *s = (char *)from->addr;
    char buf[128];
    static FontMapping fm;
    int c;
    int i;

    for (i = 0; i < 4; i++) fm.grmapping[i] = False;
    to->size = sizeof(FontMapping);
    to->addr = (caddr_t)&fm;

    if (strlen(s) + 1 > sizeof(buf)) return;

    XmuCopyISOLatin1Lowered(buf, s);
    s = buf;
    for (i = 0; i < 4; i++) {
	while ((c = *s) != '\0' && (c == ' ' || c == '\t' || c == '\n')) s++;
	if (c == '\0') break;
	if (c == '/' || c == ',') {
	    s++;
	    continue;
	}
	if (!strncmp(s, "gl", 2)) {
	    fm.grmapping[i] = False;
	} else if (!strncmp(s, "gr", 2)) {
	    fm.grmapping[i] = True;
	} else {
	    XtStringConversionWarning(s, XtRFontMapping);
	}
	s += 2;
	while ((c = *s) != '\0' && (c == ' ' || c == '\t' || c == '\n')) s++;
	if (c == '\0') break;
	if (c == '/' || c == ',') s++;
    }
}

/* ARGSUSED */
static void
Initialize(req, new, args, num_args)
Widget req;
Widget new;
ArgList args;
Cardinal *num_args;
{
    WcharDisplayObjectClass class = (WcharDisplayObjectClass)XtClass(new);
    WcharDisplayObject obj = (WcharDisplayObject)new;
    int i;
    static char stipple_bits[] = {
	0x55, 0x55, 0x00, 0x00, 0xaa, 0xaa, 0x00, 0x00, 0x55, 0x55, 0x00, 0x00,
	0xaa, 0xaa, 0x00, 0x00, 0x55, 0x55, 0x00, 0x00, 0xaa, 0xaa, 0x00, 0x00,
	0x55, 0x55, 0x00, 0x00, 0xaa, 0xaa, 0x00, 0x00
    };

    /* create a Stipple Bitmap (for drawing CurrentSubSegment) */
    obj->wcharDisplay.stipple =
      XCreateBitmapFromData(XtDisplayOfObject((Widget)obj),
			    RootWindowOfScreen(XtScreenOfObject((Widget)obj)),
			    stipple_bits, 16, 16);

    for (i = 0; i < 4; i++) {
	obj->wcharDisplay.fonts[i] = obj->wcharDisplay.defaultfonts[i];
	obj->wcharDisplay.grmapping[i] =
		obj->wcharDisplay.defaultmapping.grmapping[i];
    }

    obj->wcharDisplay.num_specs = class->wcharDisplay_class.num_specs;
    obj->wcharDisplay.charset_specs = class->wcharDisplay_class.charset_specs;

    GetAtoms(obj);
    GetGC(obj);
}

static void
Destroy(w)
Widget w;
{
    WcharDisplayObject obj = (WcharDisplayObject)w;

    XtFree((char *)obj->wcharDisplay.fontspecs);
    XtWSReleaseGCSet(w, obj->wcharDisplay.gcset_normal);
    XtWSReleaseGCSet(w, obj->wcharDisplay.gcset_rev);
    XFreePixmap(XtDisplayOfObject((Widget)obj), obj->wcharDisplay.stipple);
}

/* ARGSUSED */
static Boolean
SetValues(cur, req, wid, args, num_args)
Widget cur;
Widget req;
Widget wid;
ArgList args;
Cardinal *num_args;
{
    WcharDisplayObject old = (WcharDisplayObject)cur;
    WcharDisplayObject new = (WcharDisplayObject)wid;
    Boolean redraw = False;
    int i;

#define wd wcharDisplay 
    for (i = 0; i < 4; i++) {
	if ((new->wd.defaultfonts[i] != old->wd.defaultfonts[i] ||
	     new->wd.defaultmapping.grmapping[i] !=
			new->wd.defaultmapping.grmapping[i]) &&
	    new->wd.fonts[i] == old->wd.defaultfonts[i]) {
	    redraw = True;
	    break;
	}
    }
    if (redraw ||
	new->convDisplay.foreground != old->convDisplay.foreground ||
	new->convDisplay.background != old->convDisplay.background) {
	XtWSReleaseGCSet(wid, new->wcharDisplay.gcset_normal);
	XtWSReleaseGCSet(wid, new->wcharDisplay.gcset_rev);
	GetGC(new);
	redraw = True;
    }
#undef wd

    return redraw;
}

static void
GetAtoms(obj)
WcharDisplayObject obj;
{
    Display *dpy = XtDisplayOfObject((Widget)obj);
    WDCharSet *csp;
    FontSpec *fsp;
    Cardinal nspecs;
    char buf[128];
    char *p, *q;
    int i;
    String params[1];
    Cardinal num_params;

    if ((nspecs = obj->wcharDisplay.num_specs) == 0) {
	params[0] = XtClass((Widget)obj)->core_class.class_name;
	num_params = 1;
	XtAppErrorMsg(XtWidgetToApplicationContext((Widget)obj),
		      "noEntry", "charSpec", "WidgetError",
		      "%s: has no character set spec.",
		      params, &num_params);
    }

    csp = obj->wcharDisplay.charset_specs;
    fsp = (FontSpec *)XtMalloc(sizeof(FontSpec) * obj->wcharDisplay.num_specs);
    obj->wcharDisplay.fontspecs = fsp;

    for (i = 0; i < nspecs; i++, csp++, fsp++) {
	p = csp->charset;
	q = buf;
	while (*p != '\0' && *p != '-') *q++ = *p++;
	if (*p++ == '\0' || *p == '\0') {
	    params[0] = XtClass((Widget)obj)->core_class.class_name;
	    num_params = 1;
	    XtAppErrorMsg(XtWidgetToApplicationContext((Widget)obj),
			  "invalidSpec", "charSetSpec", "WidgetError",
			  "%s: has invalid character set spec.",
			  params, &num_params);
	}
	*q = '\0';
	fsp->registry = CachedInternAtom(dpy, buf, False);
	fsp->encoding = CachedInternAtom(dpy, p, False);
    }
}

static void
GetGC(obj)
WcharDisplayObject obj;
{
    XtGCMask mask = GCFont|GCForeground|GCBackground;
    XGCValues values;
    int ascent, descent;
    Boolean *map = obj->wcharDisplay.grmapping;

    values.function = GXcopy;
    values.foreground = obj->convDisplay.foreground;
    values.background = obj->convDisplay.background;
    mask = GCFunction|GCForeground|GCBackground;
    obj->wcharDisplay.gc_normal = XtGetGC((Widget)obj, mask, &values);

    values.foreground = obj->convDisplay.background;
    values.fill_style = FillStippled;
    values.stipple = obj->wcharDisplay.stipple;
    mask = GCFunction|GCForeground|GCFillStyle|GCStipple;
    obj->wcharDisplay.gc_stipple = XtGetGC((Widget)obj, mask, &values);

    mask = GCFont|GCFunction|GCForeground|GCBackground;
    values.function = GXcopy;
    values.foreground = obj->convDisplay.foreground;
    values.background = obj->convDisplay.background;
    obj->wcharDisplay.gcset_normal = XtWSGetGCSet((Widget)obj, mask, &values,
						  obj->wcharDisplay.fonts[0],
						  obj->wcharDisplay.fonts[1],
						  obj->wcharDisplay.fonts[2],
						  obj->wcharDisplay.fonts[3]);
    values.foreground = obj->convDisplay.background;
    values.background = obj->convDisplay.foreground;
    obj->wcharDisplay.gcset_rev = XtWSGetGCSet((Widget)obj, mask, &values,
					       obj->wcharDisplay.fonts[0],
					       obj->wcharDisplay.fonts[1],
					       obj->wcharDisplay.fonts[2],
					       obj->wcharDisplay.fonts[3]);

    /* set font mapping */
    XWSSetMapping(obj->wcharDisplay.gcset_normal,
		  map[0], map[1], map[2], map[3]);
    XWSSetMapping(obj->wcharDisplay.gcset_rev,
		  map[0], map[1], map[2], map[3]);

    XWSFontHeight(obj->wcharDisplay.gcset_normal, NULL, 0, &ascent, &descent);
    obj->wcharDisplay.ascent = ascent;
    obj->wcharDisplay.fontheight = ascent + descent;
}

static void
ChangeFont(obj, fonts, mapping)
WcharDisplayObject obj;
XFontStruct **fonts;
Boolean *mapping;
{
    Boolean newgc = False;
    int i;

    for (i = 0; i < 4; i++) {
	if (fonts[i] != obj->wcharDisplay.fonts[i] ||
	    mapping[i] != obj->wcharDisplay.grmapping[i]) {
	    obj->wcharDisplay.fonts[i] = fonts[i];
	    obj->wcharDisplay.grmapping[i] = mapping[i];
	    newgc = True;
	}
    }
    if (newgc) {
	XtWSReleaseGCSet((Widget)obj, obj->wcharDisplay.gcset_normal);
	XtWSReleaseGCSet((Widget)obj, obj->wcharDisplay.gcset_rev);
	GetGC(obj);
    }
}

static int
StringWidth(w, seg, start, end)
Widget w;
ICString *seg;
int start;
int end;
{
    WcharDisplayObject obj = (WcharDisplayObject)w;
    wchar *wstr;
    int len = seg->nchars;
    int nctl;
    int width;

    if (end < 0 || len < end) end = len;
    if (start >= end || start >= len) return 0;

    wstr = (wchar *)seg->data + start;
    len = end - start;

    /*
     * searching for control characters -- if found, convert them
     * into '^?' format for readability.
     */
    if ((nctl = countControlChars(wstr, len)) == 0) {
	/* no control characters */
	width = XWSTextWidth(obj->wcharDisplay.gcset_normal, wstr, len);
    } else {
	wchar *s = (wchar *)LOCAL_ALLOC(sizeof(wchar) * (len + nctl));

	expandControlChars(wstr, len, s);
	width = XWSTextWidth(obj->wcharDisplay.gcset_normal, s, len + nctl);
	LOCAL_FREE(s);
    }
    return width;
}

static int
LineHeight(w, ascentp)
Widget w;
Position *ascentp;
{
    WcharDisplayObject obj = (WcharDisplayObject)w;

    if (ascentp != NULL) *ascentp = obj->wcharDisplay.ascent;
    return obj->wcharDisplay.fontheight;
}

static void
DrawString(w, canvas, seg, start, end, x, y)
Widget w;
Widget canvas;
ICString *seg;
int start;
int end;
int x;
int y;
{
    WcharDisplayObject obj = (WcharDisplayObject)w;
    wchar *wstr;
    XWSGC gcset;
    int len = seg->nchars;
    int nctl;
    Display *dpy = XtDisplay(canvas);
    Window win = XtWindow(canvas);
    int width;
    int attr;

    if (end < 0 || len < end) end = len;
    if (start >= end || start >= len) return;

    wstr = (wchar *)seg->data + start;
    len = end - start;

#define STIPPLED	1
#define UNDERLINED	2

    if (seg->attr == ICAttrNormalString) {
	gcset = obj->wcharDisplay.gcset_normal;
	attr = 0;
    } else if (seg->attr & ICAttrConverted) {
	/* converted segment */
	if (seg->attr & ICAttrCurrentSegment) {
	    gcset = obj->wcharDisplay.gcset_rev;
	    attr = 0;
	} else if (seg->attr & ICAttrCurrentSubSegment) {
	    gcset = obj->wcharDisplay.gcset_rev;
	    attr = STIPPLED;
	} else {
	    gcset = obj->wcharDisplay.gcset_normal;
	    attr = 0;
	}
    } else {	/* ICAttrNotConverted */
	gcset = obj->wcharDisplay.gcset_normal;
	attr = UNDERLINED;
    }

    if ((nctl = countControlChars(wstr, len)) == 0) {
	width = XWSDrawImageString(dpy, win, gcset,
				   x, y + obj->wcharDisplay.ascent,
				   wstr, len);
    } else {
	wchar *s = (wchar *)LOCAL_ALLOC((len + nctl) * sizeof(wchar));
	expandControlChars(wstr, len, s);
	width = XWSDrawImageString(dpy, win, gcset,
				   x, y + obj->wcharDisplay.ascent,
				   s, len + nctl);
	LOCAL_FREE(s);
    }

    if (attr == UNDERLINED) {
	int uloffset = 1;

	if (obj->wcharDisplay.ascent >= obj->wcharDisplay.fontheight) {
	    /* font descent is 0  -- not likely to happen */
	    uloffset = -1;
	}
	XDrawLine(dpy, win, obj->wcharDisplay.gc_normal,
		  x, y + obj->wcharDisplay.ascent + uloffset,
		  x + width - 1, y + obj->wcharDisplay.ascent + uloffset);
    } else if (attr == STIPPLED) {
	XFillRectangle(dpy, win, obj->wcharDisplay.gc_stipple, x, y,
		       (unsigned int)width,
		       (unsigned int)obj->wcharDisplay.fontheight);
    }
}

static int
MaxChar(w, seg, start, width)
Widget w;
ICString *seg;
int start;
int width;
{
    WcharDisplayObject obj = (WcharDisplayObject)w;
    XWSGC gcset = obj->wcharDisplay.gcset_normal;
    wchar *sp = (wchar *)seg->data + start;
    wchar *ep = (wchar *)seg->data + seg->nchars;
    int cwidth;
    int chars;

    chars = 0;
    while (sp < ep) {
	cwidth = charWidth(*sp++, gcset);
	if (width < cwidth) break;
	chars++;
	if ((width -= cwidth) == 0) break;
    }
    return chars;
}

static void
SetFonts(w, fonts, num_fonts)
Widget w;
XFontStruct  **fonts;
Cardinal num_fonts;
{
    WcharDisplayObject obj = (WcharDisplayObject)w;
    WDCharSet *csp = obj->wcharDisplay.charset_specs;
    FontSpec *fsp = obj->wcharDisplay.fontspecs;
    Cardinal nspecs = obj->wcharDisplay.num_specs;
    Cardinal i, j;
    XFontStruct *pickedfonts[4];
    Boolean mapping[4];
    static int csetmask[4] = {
	G0LCharSet|G0RCharSet,
	G1LCharSet|G1RCharSet,
	G2LCharSet|G2RCharSet,
	G3LCharSet|G3RCharSet,
    };

    if (num_fonts == 0) {
	ChangeFont(obj, obj->wcharDisplay.defaultfonts,
		   obj->wcharDisplay.defaultmapping.grmapping);
	return;
    }

    for (i = 0; i < 4; i++) pickedfonts[i] = NULL;

    (void)_CDPickupFonts(w, fsp, nspecs, fonts, num_fonts);

#define GRMAP	(G0RCharSet|G1RCharSet|G2RCharSet|G3RCharSet)
    for (j = 0; j < nspecs; j++, fsp++, csp++) {
	if (fsp->font == NULL) continue;
	for (i = 0; i < 4; i++) {
	    if (pickedfonts[i] == NULL && (csp->flag & csetmask[i])) {
		pickedfonts[i] = fsp->font;
		mapping[i] = (csp->flag & csetmask[i] & GRMAP) ? True : False;
	    }
	}
    }
#undef GRMAP
    for (i = 0; i < 4; i++) {
	if (pickedfonts[i] == NULL) {
	    pickedfonts[i] = obj->wcharDisplay.defaultfonts[i];
	    mapping[i] = obj->wcharDisplay.defaultmapping.grmapping[i];
	}
    }

    ChangeFont(obj, pickedfonts, mapping);
}

/* countControlChars -- count number of control characters in a string */
static int
countControlChars(wstr, len)
register wchar *wstr;
int len;
{
    register wchar *end = wstr + len;
    register int n = 0;

    while (wstr < end) {
	if (*wstr < 0x20 || *wstr == 0x7f) n++;
	wstr++;
    }
    return n;  
}

/* expandControlChars -- convert control characters into '^?' format */
static void
expandControlChars(org, orglen, res)
wchar *org;
int orglen;
wchar *res;
{
    wchar *end;

    for (end = org + orglen; org < end; org++) {
	if (*org < 0x20 || *org == 0x7f) {
	    *res++ = '^';
	    *res++ = *org ^ 0x40;
	} else {
	    *res++ = *org;
	}
    }
}

#define WITHIN_RANGE_2D(row, col, fs) \
    ((fs)->min_byte1 <= (row) && (row) <= (fs)->max_byte1 && \
     (fs)->min_char_or_byte2 <= (col) && (col) <= (fs)->max_char_or_byte2)

#define WITHIN_RANGE(c, fs) \
    ((fs)->min_char_or_byte2 <= (c) && (c) <= (fs)->max_char_or_byte2)

#define CHAR_INFO_2D(row, col, fs) \
    ((fs)->per_char + ((row) - (fs)->min_byte1) * \
     ((fs)->max_char_or_byte2 - (fs)->min_char_or_byte2 + 1) + \
     ((col) - (fs)->min_char_or_byte2))

#define CHAR_INFO(c, fs) \
    ((fs)->per_char + ((c) - (fs)->min_char_or_byte2))

#define CHAR_EXIST(csp) \
    ((csp)->width != 0 || ((csp)->rbearing != 0) || ((csp)->lbearing != 0))

static int
defaultCharWidth(font)
XFontStruct *font;
{
    int defchar = font->default_char;

    if (font->min_byte1 || font->max_byte1) {
	int row = defchar >> 8;
	int col = defchar & 0xff;
	if (WITHIN_RANGE_2D(row, col, font)) {
	    if (font->per_char == NULL) {
		return font->min_bounds.width;
	    } else {
		XCharStruct *csp = CHAR_INFO_2D(row, col, font);
		return CHAR_EXIST(csp) ? csp->width : 0;
	    }
	} else {
	    return 0;
	}
    } else {
	if (WITHIN_RANGE(defchar, font)) {
	    if (font->per_char == NULL) {
		return font->min_bounds.width;
	    } else {
		XCharStruct *csp = CHAR_INFO(defchar, font);
		return CHAR_EXIST(csp) ? csp->width : 0;
	    }
	} else {
	    return 0;
	}
    }
}

/* charWidth -- returns width of the specified character */
static int
charWidth(c, gcset)
register int c;
XWSGC gcset;
{
    register XFontStruct *font;
    int width;
    int gset;
    int nonPrinting = (c < 0x20 || c == 0x7f);    

    if (nonPrinting) c ^= 0x40;

    switch (c & 0x8080) {
    case 0x0000: gset = 0; break;
    case 0x8080: gset = 1; break;
    case 0x0080: gset = 2; break;
    case 0x8000: gset = 3; break;
    }

    if ((font = gcset->fe[gset].font) == NULL) return 0;

    if (gcset->fe[gset].flag & GRMAPPING) {
	c |= 0x8080;
    } else {
	c &= 0x7f7f;
    }

    if (gcset->fe[gset].flag & TWOB) {
	register int row = (c >> 8) & 0xff;
	register int col = c & 0xff;
	if (WITHIN_RANGE_2D(row, col, font)) {
	    if (font->per_char == NULL) {
		width = font->min_bounds.width;
	    } else {
		XCharStruct *csp = CHAR_INFO_2D(row, col, font);

		width = CHAR_EXIST(csp) ? csp->width : defaultCharWidth(font);
	    }
	} else {
	    width = defaultCharWidth(font);
	}
    } else {
	c &= 0xff;
	if (WITHIN_RANGE(c, font)) {
	    if (font->per_char == NULL) {
		width = font->min_bounds.width;
	    } else {
		XCharStruct *csp = CHAR_INFO(c, font);

		width = CHAR_EXIST(csp) ? csp->width : defaultCharWidth(font);
	    }
	} else {
	    width = defaultCharWidth(font);
	}
    }
    if (nonPrinting) width += charWidth('^', gcset);

    return width;
}


/*
 * jpWcharDisplay definition
 *
 *	character set assignment for Japanese wchar:
 *	    G0: ascii (ISO8859/1 left-half)
 *	    G1: kanji (JISX0208)
 *	    G2: half-width kana (JISX0201 right-half)
 *	    G3: unused
 */

static FontMapping jpDefaultMapping = { { False, False, True, False } };

#define JPFONT_ASCII	"-Misc-Fixed-Medium-R-*--14-*-*-*-C-*-ISO8859-1"
#define JPFONT_KANJI	"-Misc-Fixed-Medium-R-*--14-*-*-*-C-*-JISX0208.1983-0"
#define JPFONT_KANA	"-Misc-Fixed-Medium-R-*--14-*-*-*-C-*-JISX0201.1976-0"

static XtResource jpresources[] = {
    /* only override superclass's default */
#define offset(field) XtOffset(JpWcharDisplayObject, wcharDisplay.field)
    { XtNfont, XtCFont, XtRFontStruct, sizeof (XFontStruct *),
	offset(defaultfonts[0]), XtRString, JPFONT_ASCII },
    { XtNkanjiFont, XtCKanjiFont, XtRFontStruct, sizeof (XFontStruct *),
	offset(defaultfonts[1]), XtRString, JPFONT_KANJI },
    { XtNkanaFont, XtCKanaFont, XtRFontStruct, sizeof (XFontStruct *),
	offset(defaultfonts[2]), XtRString, JPFONT_KANA },
    { XtNfontG3, "Not.used", XtRFontStruct, sizeof (XFontStruct *),
	offset(defaultfonts[3]), XtRImmediate, (XtPointer)NULL },
    { XtNfontMapping, XtCFontMapping, XtRFontMapping, sizeof (FontMapping),
	offset(defaultmapping), XtRFontMapping, (XtPointer)&jpDefaultMapping },
#undef offset
};

static WDCharSet jpCharSet[] = {
    { "ISO8859-1",		G0LCharSet },		/* my preference */
    { "JISX0201.1976-0",	G0LCharSet | G2RCharSet },
    { "JISX0208.1990-0",	G1LCharSet },
    { "JISX0208.1983-0",	G1LCharSet },
    { "JISX0208.1978-0",	G1LCharSet },
    { "JISX0208.1983-1",	G1RCharSet },
    { "JISX0208.1978-1",	G1RCharSet },
#ifdef ALLOW_LOWERCASE_CHARSET_NAME
    { "iso8859-1",		G0LCharSet },		/* my preference */
    { "jisx0201.1976-0",	G0LCharSet | G2RCharSet },
    { "jisx0208.1990-0",	G1LCharSet },
    { "jisx0208.1983-0",	G1LCharSet },
    { "jisx0208.1978-0",	G1LCharSet },
    { "jisx0208.1983-1",	G1RCharSet },
    { "jisx0208.1978-1",	G1RCharSet },
#endif
};

JpWcharDisplayClassRec jpWcharDisplayClassRec = {
  { /* object fields */
    /* superclass		*/	(WidgetClass)&wcharDisplayClassRec,
    /* class_name		*/	"JpWcharDisplay",
    /* widget_size		*/	sizeof(JpWcharDisplayRec),
    /* class_initialize		*/	NULL,
    /* class_part_initialize	*/	NULL,
    /* class_inited		*/	FALSE,
    /* initialize		*/	NULL,
    /* initialize_hook		*/	NULL,
    /* obj1			*/	NULL,
    /* obj2			*/	NULL,
    /* obj3			*/	0,
    /* resources		*/	jpresources,
    /* num_resources		*/	XtNumber(jpresources),
    /* xrm_class		*/	NULLQUARK,
    /* obj4			*/	FALSE,
    /* obj5			*/	FALSE,
    /* obj6			*/	FALSE,
    /* obj7			*/	FALSE,
    /* destroy			*/	NULL,
    /* obj8			*/	NULL,
    /* obj9			*/	NULL,
    /* set_values		*/	NULL,
    /* set_values_hook		*/	NULL,
    /* obj10			*/	NULL,
    /* get_values_hook		*/	NULL,
    /* obj11			*/	NULL,
    /* version			*/	XtVersion,
    /* callback_private		*/	NULL,
    /* obj12			*/	NULL,
    /* obj13			*/	NULL,
    /* obj14			*/	NULL,
    /* extension		*/	NULL
  },
  { /* convDisplay fields */
    /* StringWidth		*/	XtInheritStringWidth,
    /* LineHeight		*/	XtInheritLineHeight,
    /* DrawString		*/	XtInheritDrawString,
    /* MaxChar			*/	XtInheritMaxChar,
    /* DrawCursor		*/	XtInheritDrawCursor,
    /* GetCursorBounds		*/	XtInheritGetCursorBounds,
    /* SetFonts			*/	XtInheritSetFonts,
  },
  { /* wcharDisplay fields */
    /* charset_specs		*/	jpCharSet,
    /* num_specs		*/	XtNumber(jpCharSet),
  },
  { /* jpWcharDisplay fields */
    /* empty			*/	0,
  },
};

WidgetClass jpWcharDisplayObjectClass = (WidgetClass)&jpWcharDisplayClassRec;
