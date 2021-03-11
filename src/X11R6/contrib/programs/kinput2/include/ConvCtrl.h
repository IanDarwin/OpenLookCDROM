/* $Id: ConvCtrl.h,v 1.18 1994/06/01 10:04:08 ishisone Rel $ */
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

#ifndef _ConversionControl_h
#define _ConversionControl_h

/* ConversionControl widget public header file */

#include <X11/Shell.h>

#define XtNinputObject "inputObject"
#define XtCInputObject "InputObject"
#define XtNinputObjectClass "inputObjectClass"
#define XtCInputObjectClass "InputObjectClass"
#define XtNdisplayObjectClass "displayObjectClass"
#define XtCDisplayObjectClass "DisplayObjectClass"
#define XtNselectionWidgetClass "selectionWidgetClass"
#define XtCSelectionWidgetClass "SelectionWidgetClass"
#define XtNauxWidgetClass "auxWidgetClass"
#define XtCAuxWidgetClass "AuxWidgetClass"

#define XtNclientWindow "clientWindow"
#define XtNfocusWindow "focusWindow"

#define XtNcursor "cursor"

#define XtNeventSelectMethod "eventSelectMethod"
#define XtCEventSelectMethod "EventSelectMethod"

#define XtNtextEncoding "textEncoding"
#define XtCTextEncoding "TextEncoding"

#define XtNtextCallback "textCallback"
#define XtNnewTextCallback "newTextCallback"
#define XtNendCallback "endCallback"
#define XtNunusedEventCallback "unusedEventCallback"

#define XtNsendbackKeyPress "sendbackKeyPress"
#define XtCSendbackEvent "SendbackEvent"

typedef enum {
    ESMethodNone,	/* do nothing */
    ESMethodInputOnly,	/* creates a Input-Only window on clientwindow
			 * and select event on it
			 */
    ESMethodSelectFocus	/* selects event on fucuswindow */
} EventSelectMethod;

typedef struct {
    Atom	encoding;	/* text encoding e.g. COMPOUND_TEXT */
    int		format;		/* text format (8/16/32) */
    int		length;		/* text length */
    XtPointer	text;		/* text data */
} CCTextCallbackArg;

typedef struct _ConversionControlClassRec*	ConversionControlWidgetClass;
typedef struct _ConversionControlRec*		ConversionControlWidget;

extern WidgetClass conversionControlWidgetClass;


/*
 *	public functions (and their argument type)
 */

#define CASpotLocation		0x1L
#define CAFocusWindow		0x2L
#define CAClientArea		0x4L
#define CAStatusArea		0x8L
#define CAColormap		0x10L
#define CAColor			0x20L
#define CABackgroundPixmap	0x40L
#define CALineSpacing		0x80L
#define CAFonts			0x100L
#define CACursor		0x200L

typedef struct {
    Position	spotx, spoty;
    Window	focuswindow;
    XRectangle	clientarea;
    XRectangle	statusarea;
    Colormap	colormap;
    Pixel	foreground, background;
    Pixmap	background_pixmap;
    Dimension	linespacing;
    XFontStruct	**fonts;	/* an array of pointers to (XFontStruct),
				 * NOT a pointer to an array of (XFontStruct)
				 */
    Cardinal	num_fonts;
    Cursor	cursor;
} ConversionAttributes;

extern void CControlStartConversion(
#if NeedFunctionPrototypes
	Widget			/* w */,
	Window			/* clientwindow */,
	unsigned long		/* valuemask */,
	ConversionAttributes *	/* value */
#endif
);

extern void CControlChangeAttributes(
#if NeedFunctionPrototypes
	Widget			/* w */,
	unsigned long		/* valuemask */,
	ConversionAttributes *	/* value */
#endif
);

extern void CControlChangeFocus(
#if NeedFunctionPrototypes
	Widget			/* w */,
	int			/* set (non-zero: set, zero: unset) */
#endif
);

extern void CControlEndConversion(
#if NeedFunctionPrototypes
	Widget			/* w */
#endif
);

#endif
