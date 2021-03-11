/* $Id: ConvMgr.h,v 1.5 1991/04/18 16:59:00 ishisone Rel $ */
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

#ifndef _ConversionManager_h
#define _ConversionManager_h

typedef struct _ConversionManagerClassRec*	ConversionManagerWidgetClass;
typedef struct _ConversionManagerRec*		ConversionManagerWidget;

extern WidgetClass conversionManagerWidgetClass;

/*
 * (semi)public functions
 *
 *	these functions are for the protocol widgets
 */

/*
 * void CMGetConverter()
 *	returns a widget of the specified class that is on the
 *	same screen as the client window.
 *	first it searches its internal cache and if unused one found,
 *	take and returns it. otherwise make a new one and returns it.
 */
extern Widget CMGetConverter(
#if NeedFunctionPrototypes
	Widget		/* w */,
	Window		/* client */,
	WidgetClass	/* converterclass */,
	WidgetClass	/* inputobjclass */,
	WidgetClass	/* displayobjclass */
#endif
);

/*
 * void CMPrepareConverter()
 *	make a new converter widget of the specified class on the
 *	specified screen, add it to the internal cache for later use.
 */
extern void CMPrepareConverter(
#if NeedFunctionPrototypes
	Widget		/* w */,
	Screen *	/* screen */,
	WidgetClass	/* converterclass */,
	WidgetClass	/* inputobjclass */,
	WidgetClass	/* displayobjclass */
#endif
);

/*
 * void CMReleaseConverter()
 *	mark specified converter widget as unused, and enter it
 *	to the cache.
 */
extern void CMReleaseConverter(
#if NeedFunctionPrototypes
	Widget	/* w */,
	Widget	/* converter */
#endif
);

#endif
