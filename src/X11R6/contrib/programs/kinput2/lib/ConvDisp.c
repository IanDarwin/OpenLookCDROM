#ifndef lint
static char *rcsid = "$Id: ConvDisp.c,v 1.19 1992/08/05 01:51:35 ishisone Rel $";
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
#include <X11/Xmu/Atoms.h>
#include <X11/Xmu/Converters.h>
#include "CachedAtom.h"
#include "ConvDispP.h"

#define DEBUG_VAR debug_ConvDisplay
#include "DebugPrint.h"

static XtResource resources[] = {
#define offset(field) XtOffset(ConvDisplayObject, convDisplay.field)
    { XtNforeground, XtCForeground, XtRPixel, sizeof (Pixel),
	offset(foreground), XtRString, XtDefaultForeground },
    { XtNbackground, XtCBackground, XtRPixel, sizeof (Pixel),
	offset(background), XtRString, XtDefaultBackground },
    { XtNcursorBitmap, XtCCursorBitmap, XtRBitmap, sizeof (Pixmap),
	offset(cursor), XtRImmediate, None },
    { XtNhotX, XtCHotX, XtRPosition, sizeof (Position),
	offset(hotx), XtRString, "3" },
    { XtNhotY, XtCHotY, XtRPosition, sizeof (Position),
	offset(hoty), XtRString, "2" },
#undef offset
};

static void ClassInitialize();
static void ClassPartInitialize();

static void Initialize();
static void Destroy();
static Boolean SetValues();

static Pixmap DefaultCursor();
static void GetGC();
static void ComputeBounds();

static int StringWidth();
static int LineHeight();
static void DrawString();
static int MaxChar();
static void DrawCursor();
static void GetCursorBounds();
static void SetFonts();

ConvDisplayClassRec convDisplayClassRec = {
  { /* object fields */
    /* superclass		*/	(WidgetClass) &objectClassRec,
    /* class_name		*/	"ConvDisplay",
    /* widget_size		*/	sizeof(ConvDisplayRec),
    /* class_initialize		*/	ClassInitialize,
    /* class_part_initialize	*/	ClassPartInitialize,
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
    /* DrawCursor		*/	DrawCursor,
    /* GetCursorBounds		*/	GetCursorBounds,
    /* SetFonts			*/	SetFonts,
  }
};

WidgetClass convDisplayObjectClass = (WidgetClass)&convDisplayClassRec;

static void
ClassInitialize()
{
    static XtConvertArgRec screenConvertArg[] = {
        { XtBaseOffset, (caddr_t) XtOffset(Widget, core.screen),
	   sizeof(Screen *) }
    };

    /* add string->bitmap converter (for insert-cursor) */
    XtAddConverter("String", "Bitmap", XmuCvtStringToBitmap,
                   screenConvertArg, XtNumber(screenConvertArg));
}

static void
ClassPartInitialize(cl)
WidgetClass cl;
{
    ConvDisplayObjectClass class = (ConvDisplayObjectClass)cl;
    ConvDisplayObjectClass super = (ConvDisplayObjectClass)class->object_class.superclass;
    ConvDisplayClassPart *classpart = &(class->convDisplay_class);
    ConvDisplayClassPart *superpart = &(super->convDisplay_class);

    if (classpart->StringWidth == XtInheritStringWidth)
	classpart->StringWidth = superpart->StringWidth;
    if (classpart->LineHeight == XtInheritLineHeight)
	classpart->LineHeight = superpart->LineHeight;
    if (classpart->DrawString == XtInheritDrawString)
	classpart->DrawString = superpart->DrawString;
    if (classpart->MaxChar == XtInheritMaxChar)
	classpart->MaxChar = superpart->MaxChar;
    if (classpart->DrawCursor == XtInheritDrawCursor)
	classpart->DrawCursor = superpart->DrawCursor;
    if (classpart->GetCursorBounds == XtInheritGetCursorBounds)
	classpart->GetCursorBounds = superpart->GetCursorBounds;
    if (classpart->SetFonts == XtInheritSetFonts)
	classpart->SetFonts = superpart->SetFonts;
}

/* ARGSUSED */
static void
Initialize(req, new, args, num_args)
Widget req;
Widget new;
ArgList args;
Cardinal *num_args;
{
    ConvDisplayObject obj = (ConvDisplayObject)new;

    if (obj->convDisplay.cursor == None) {
	obj->convDisplay.cursor = DefaultCursor(obj);
	obj->convDisplay.cursorcreated = True;
    } else {
	obj->convDisplay.cursorcreated = False;
    }
    obj->convDisplay.cursorvisible = False;
    GetGC(obj);
    ComputeBounds(obj);
}

static void
Destroy(w)
Widget w;
{
    ConvDisplayObject obj = (ConvDisplayObject)w;

    XtReleaseGC(w, obj->convDisplay.cursorgc);
    if (obj->convDisplay.cursorcreated == True) {
	XFreePixmap(XtDisplayOfObject(w), obj->convDisplay.cursor);
    }
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
    ConvDisplayObject new = (ConvDisplayObject)wid;
    ConvDisplayObject old = (ConvDisplayObject)cur;

    if (new->convDisplay.foreground != old->convDisplay.foreground ||
	new->convDisplay.background != old->convDisplay.background) {
	XtReleaseGC(wid, old->convDisplay.cursorgc);
	GetGC(new);
    }

    if (new->convDisplay.cursor != old->convDisplay.cursor ||
	new->convDisplay.hotx != old->convDisplay.hotx ||
	new->convDisplay.hoty != old->convDisplay.hoty) {
	if (new->convDisplay.cursor != old->convDisplay.cursor &&
	    old->convDisplay.cursorcreated) {
	    XFreePixmap(XtDisplayOfObject(wid), old->convDisplay.cursor);
	    new->convDisplay.cursorcreated = False;
	}
	ComputeBounds(new);
    }

    return False;
}

static Pixmap
DefaultCursor(obj)
ConvDisplayObject obj;
{
    static char default_bits[] = { 0x0c, 0x0c, 0x1e, 0x3f, 0x33 };
    return XCreateBitmapFromData(XtDisplayOfObject((Widget)obj),
				 RootWindowOfScreen(XtScreenOfObject((Widget)obj)),
				 default_bits,
				 6, 5);
}

static void
GetGC(obj)
ConvDisplayObject obj;
{
    XtGCMask mask = GCFunction|GCForeground|GCBackground;
    XGCValues values;

    values.function = GXxor;
    values.foreground = obj->convDisplay.foreground ^ obj->convDisplay.background;
    values.background = 0;
    obj->convDisplay.cursorgc = XtGetGC((Widget)obj, mask, &values);
}

static void
ComputeBounds(obj)
ConvDisplayObject obj;
{
    unsigned int width, height;
    Window junkroot;
    int junkx, junky;
    unsigned int junkbw, junkdepth;

    if (obj->convDisplay.cursor == None ||
	!XGetGeometry(XtDisplayOfObject((Widget)obj), obj->convDisplay.cursor,
		      &junkroot, &junkx, &junky, &width, &height,
		      &junkbw, &junkdepth)) {
	obj->convDisplay.cursor = None;
	width = height = 0;
    }
    obj->convDisplay.cursorbounds.width = width;
    obj->convDisplay.cursorbounds.height = height;
    obj->convDisplay.cursorbounds.x = -obj->convDisplay.hotx;
    obj->convDisplay.cursorbounds.y = -obj->convDisplay.hoty;
}

/* ARGSUSED */
static int
StringWidth(w, str, start, end)
Widget w;
ICString *str;
int start;
int end;
{
    XtAppError(XtWidgetToApplicationContext(w),
	       "ConvDisplay Object: StringWidth function isn't defined.");
    return 0;	/* for lint */
}

/* ARGSUSED */
static int
LineHeight(w, ascentp)
Widget w;
Position *ascentp;
{
    XtAppError(XtWidgetToApplicationContext(w),
	       "ConvDisplay Object: LineHeight function isn't defined.");
    return 0;	/* for lint */
}

/* ARGSUSED */
static void
DrawString(w, canvas, str, start, end, x, y)
Widget w;
Widget canvas;
ICString *str;
int start;
int end;
int x;
int y;
{
    XtAppError(XtWidgetToApplicationContext(w),
	       "ConvDisplay Object: DrawString function isn't defined.");
}

/* ARGSUSED */
static int
MaxChar(w, str, start, width)
Widget w;
ICString *str;
int start;
int width;
{
    XtAppError(XtWidgetToApplicationContext(w),
	       "ConvDisplay Object: MaxChar function isn't defined.");
    return 0;	/* for lint */
}

static void
DrawCursor(w, canvas, x, y, on)
Widget w;
Widget canvas;
int x;
int y;
int on;
{
    ConvDisplayObject obj = (ConvDisplayObject)w;

    if (!XtIsRealized(canvas) ||
	obj->convDisplay.cursor == None ||
	(obj->convDisplay.cursorvisible && on) ||
	(!obj->convDisplay.cursorvisible && !on)) {
	obj->convDisplay.cursorvisible = (on != 0);
	return;
    }

    XCopyPlane(XtDisplay(canvas),
	       obj->convDisplay.cursor, XtWindow(canvas),
	       obj->convDisplay.cursorgc,
	       0, 0,
	       obj->convDisplay.cursorbounds.width,
	       obj->convDisplay.cursorbounds.height,
	       x + obj->convDisplay.cursorbounds.x,
	       y + obj->convDisplay.cursorbounds.y,
	       1L);
    obj->convDisplay.cursorvisible = (on != 0);
}

static void
GetCursorBounds(w, bounds)
Widget w;
XRectangle *bounds;
{
    ConvDisplayObject obj = (ConvDisplayObject)w;

     bounds->x = obj->convDisplay.cursorbounds.x;
     bounds->y = obj->convDisplay.cursorbounds.y;
     bounds->width = obj->convDisplay.cursorbounds.width;
     bounds->height = obj->convDisplay.cursorbounds.height;
}

/* ARGSUSED */
static void
SetFonts(w, fonts, num_fonts)
Widget w;
XFontStruct **fonts;
Cardinal num_fonts;
{
    XtAppError(XtWidgetToApplicationContext(w),
	       "ConvDisplay Object: SetFonts function isn't defined.");
}


/*
 * public functions
 */

int
CDStringWidth(w, str, start, end)
Widget w;
ICString *str;
int start;
int end;
{
    ConvDisplayObjectClass class = (ConvDisplayObjectClass)w->core.widget_class;

    XtCheckSubclass(w, convDisplayObjectClass, "CDStringWidth()");
    return (*class->convDisplay_class.StringWidth)(w, str, start, end);
}

int
CDLineHeight(w, ascent)
Widget w;
Position *ascent;
{
    ConvDisplayObjectClass class = (ConvDisplayObjectClass)w->core.widget_class;

    XtCheckSubclass(w, convDisplayObjectClass, "CDLineHeight()");
    return (*class->convDisplay_class.LineHeight)(w, ascent);
}

void
CDDrawString(w, canvas, str, start, end, x, y)
Widget w;
Widget canvas;
ICString *str;
int start;
int end;
int x;
int y;
{
    ConvDisplayObjectClass class = (ConvDisplayObjectClass)w->core.widget_class;

    XtCheckSubclass(w, convDisplayObjectClass, "CDDrawString()");
    (*class->convDisplay_class.DrawString)(w, canvas, str, start, end, x, y);
}

int
CDMaxChar(w, str, start, width)
Widget w;
ICString *str;
int start;
int width;
{
    ConvDisplayObjectClass class = (ConvDisplayObjectClass)w->core.widget_class;

    XtCheckSubclass(w, convDisplayObjectClass, "CDMaxChar()");
    return (*class->convDisplay_class.MaxChar)(w, str, start, width);
}

void
CDDrawCursor(w, canvas, x, y, on)
Widget w;
Widget canvas;
int x;
int y;
int on;
{
    ConvDisplayObjectClass class = (ConvDisplayObjectClass)w->core.widget_class;

    XtCheckSubclass(w, convDisplayObjectClass, "CDDrawCursor()");
    (*class->convDisplay_class.DrawCursor)(w, canvas, x, y, on);
}

void
CDGetCursorBounds(w, bounds)
Widget w;
XRectangle *bounds;
{
    ConvDisplayObjectClass class = (ConvDisplayObjectClass)w->core.widget_class;

    XtCheckSubclass(w, convDisplayObjectClass, "CDGetCursorBounds()");
    (*class->convDisplay_class.GetCursorBounds)(w, bounds);
}

void
CDSetFonts(w, fonts, num_fonts)
Widget w;
XFontStruct **fonts;
Cardinal num_fonts;
{
    ConvDisplayObjectClass class = (ConvDisplayObjectClass)w->core.widget_class;

    XtCheckSubclass(w, convDisplayObjectClass, "CDSetFonts()");
    (*class->convDisplay_class.SetFonts)(w, fonts, num_fonts);
}


void
CDSetBlockCursor(w, shape)
Widget w;
XRectangle *shape;
{
    ConvDisplayObject obj = (ConvDisplayObject)w;
    Display *dpy = XtDisplayOfObject((Widget)obj);
    Pixmap block;
    GC tmpgc;
    XGCValues values;

    XtCheckSubclass(w, convDisplayObjectClass, "CDMakeBlockCursor()");

    block = XCreatePixmap(dpy,
			  RootWindowOfScreen(XtScreenOfObject((Widget)obj)),
			  shape->width, shape->height, 1);

    values.function = GXset;
    tmpgc = XtGetGC(w, GCFunction, &values);
    XFillRectangle(dpy, block, tmpgc, 0, 0, shape->width, shape->height);
    XtReleaseGC(w, tmpgc);

    XtVaSetValues(w,
		  XtNcursorBitmap, block,
		  XtNhotX, -shape->x,
		  XtNhotY, -shape->y,
		  NULL);
}


/*
 * semi-public function (for subclass use)
 */

int
_CDPickupFonts(widget, fontspecs, num_specs, fonts, num_fonts)
Widget widget;
FontSpec *fontspecs;
Cardinal num_specs;
XFontStruct **fonts;
Cardinal num_fonts;
{
    Display *dpy = XtDisplayOfObject(widget);
    Atom cs_reg = CachedInternAtom(dpy, "CHARSET_REGISTRY", False);
    Atom cs_enc = CachedInternAtom(dpy, "CHARSET_ENCODING", False);
    Atom atom;
    Cardinal i, j;
    FontSpec *fsp;
    int npick;

    DPRINT(("_CDPickupFonts()\n"));

    /* pickup fonts */
    npick = 0;
    for (i = 0, fsp = fontspecs; i < num_specs; i++, fsp++) {
	DPRINT(("\tlooking for a font..."));

	fsp->font = NULL;
	for (j = 0; j < num_fonts; j++) {
	    if (fonts[j] != NULL &&
		XGetFontProperty(fonts[j], cs_reg, (unsigned long *)&atom) &&
		atom == fsp->registry &&
		XGetFontProperty(fonts[j], cs_enc, (unsigned long *)&atom) &&
		atom == fsp->encoding) {
		DPRINT((" found"));
		fsp->font = fonts[j];
		npick++;
		break;
	    }
	}
	DPRINT(("\n"));
    }

    return npick;
}
