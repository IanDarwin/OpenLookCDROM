/* $XConsortium: List.c,v 1.34 91/09/27 18:35:07 converse Exp $ */

/*
 * Copyright 1989 Massachusetts Institute of Technology
 *
 * Permission to use, copy, modify, distribute, and sell this software and its
 * documentation for any purpose is hereby granted without fee, provided that
 * the above copyright notice appear in all copies and that both that
 * copyright notice and this permission notice appear in supporting
 * documentation, and that the name of M.I.T. not be used in advertising or
 * publicity pertaining to distribution of the software without specific,
 * written prior permission.  M.I.T. makes no representations about the
 * suitability of this software for any purpose.  It is provided "as is"
 * without express or implied warranty.
 *
 * M.I.T. DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING ALL
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL M.I.T.
 * BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION
 * OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN 
 * CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *
 */

/*
 * Copyright (c) 1992 The Regents of the University of Texas System.
 * Copyright (c) 1993 The Regents of the University of Texas System.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted provided
 * that the above copyright notice and this paragraph are duplicated in all
 * such forms and that any documentation, advertising materials,  and other
 * materials  related to such  distribution  and use  acknowledge  that the
 * software  was  developed  by the  University of Texas.  The  name of the
 * University may not be  used to endorse or promote  products derived from
 * this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR IMPLIED
 * WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED WARRANTIES OF
 * MERCHANTIBILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#ifndef lint
static char copyright[] =
"@(#) Copyright (c) 1993 The Regents of the University of Texas System.\n\
 All rights reserved.\n";
static char rcsid[] =
"$Id: List.c,v 1.2 1994/03/21 21:30:28 jones Exp $\n";
static char rcspath[] =
"$Source: /mintaka/u0/xx/ss/jones/jones.old/mftp.src/moxftp-2.0/RCS/List.c,v $\n";
#endif

/* $Log: List.c,v $
 * Revision 1.2  1994/03/21  21:30:28  jones
 * Use XmREVISION instead of XmUPDATE_LEVEL.
 *
 * Revision 1.1  1994/03/14  18:55:53  jones
 * Initial revision
 *
 */

/*
 * List.c - List widget
 *
 * This is the List widget, it is useful to display a list, without the
 * overhead of having a widget for each item in the list.  It allows 
 * the user to select an item in a list and notifies the application through
 * a callback function.
 *
 *	Created: 	8/13/88
 *	By:		Chris D. Peterson
 *                      MIT X Consortium
 */

/* 
 * The version of List.c is a derivative work of the MIT X Consortium
 * List widget created by Chris D. Peterson.  It is not stand alone.
 * It was desinged to live in an Athena Viewport or a Motif Srcolled 
 * window.
 * 
 * 	Modified:	5/20/92
 *	By:		William L. Jones
 *			UT SYSTEM CHPC
 */

#include <stdio.h>
#include <ctype.h>

#include <X11/IntrinsicP.h>
#include <X11/StringDefs.h>
#include <X11/Xatom.h>

#if defined(MOTIF)
#include <Xm/XmP.h>
#include <Xm/Xm.h>
#if XmREVISION==2
#include <Xm/PrimitiveP.h>
#endif
#endif

#if defined(XAW3D)
#include <X11/Xaw3d/XawInit.h>
#include <X11/Xaw3d/ThreeDP.h>
#else
#if defined(XAW)
#include <X11/Xaw/XawInit.h>
#endif
#endif

#if !defined(DEC_STUFF)&&!defined(HP_STUFF)
#include <X11/Xmu/Drawing.h>
#endif

#include "ListP.h"

/* 
 * Default Translation table.
 */

static char defaultTranslations[] =  
#if defined(MOTIF)
  "<Btn1Down>:   		  Set()\n\
   <Btn1Motion>:		  Set(M)\n\
   <Btn1Up>:     	  	  Notify()\n\
   <Key>osfHelp: 		  PrimitiveHelp()\n\
   Ctrl <Key>osfBeginLine:	  Listop(Top)\n\
   Ctrl ~Shift <Key>osfBeginLine: Listop(Start)\n\
   Ctrl <Key>osfEndLine:          Listop(Bottom)\n\
   <Key>osfEndLine:   	          Listop(End)\n\
   ~Ctrl <Key>osfPageDown:	  Listop(NextPage)\n\
   ~Ctrl <Key>osfPageUp:          Listop(PrevPage)\n\
  ~Ctrl ~Shift <Key>osfLeft:      Listop(Left)\n\
  ~Ctrl ~Shift <Key>osfRight:     Listop(Right)\n\
  ~Ctrl ~Shift <Key>osfUp:        Listop(Up)\n\
  ~Ctrl ~Shift <Key>osfDown:      Listop(Down)\n\
  ~Shift  ~Meta ~Alt <Key>space:  Listop(Select)\n\
  Shift ~Meta ~Alt <Key>Tab:      PrimitivePrevTabGroup()\n\
  ~Meta ~Alt <Key>Tab:            PrimitiveNextTabGroup()\n\
  <Enter>:			  ListEnter()\n\
  <Leave>:			  ListLeave()\n\
  <FocusIn>:			  ListFocusIn()\n\
  <FocusOut>:			  ListFocusOut()";
#else
#if defined(OPENWINDOW)
  "<Btn1Down>:   		  Set()\n\
   <Btn1Motion>: 		  Set(M)\n\
   <Btn1Up>:     		  Notify()\n\
   <Enter>:			  ListEnter()\n\
   <Leave>:			  ListLeave()\n\
   <FocusIn>:			  ListFocusIn()\n\
   <FocusOut>:			  ListFocusOut()";
#else
  "<Btn1Down>:   Set()\n\
   <Btn1Motion>: Set(M)\n\
   <Btn1Up>:     Notify()";
#endif
#endif
   


/****************************************************************
 *
 * Full class record constant
 *
 ****************************************************************/

/* Private Data */

#define offset(field) XtOffset(MyListWidget, field)

static XtResource resources[] = {
    {XtNforeground, XtCForeground, XtRPixel, sizeof(Pixel),
	offset(list.foreground), XtRString, XtDefaultForeground},
#if defined(XAW)
    {XtNcursor, XtCCursor, XtRCursor, sizeof(Cursor),
       offset(simple.cursor), XtRString, "left_ptr"},
#endif
#if defined(XAW3D)
    {XtNshadowWidth, XtCShadowWidth, XtRDimension, sizeof(Dimension),
        offset(threeD.shadow_width), XtRImmediate, (XtPointer) 4},
#endif
#if defined(XAW)||defined(OPENWINDOW)
    {XtNfont,  XtCFont, XtRFontStruct, sizeof(XFontStruct *),
	offset(list.font),XtRString, XtDefaultFont},
    {XtNfontbold,  XtCFont, XtRFontStruct, sizeof(XFontStruct *),
	offset(list.fontbold),XtRString, NULL},
#else
    {XmNfontList, XmCFontList, XmRFontList, sizeof(XmFontList),
        offset(list.font),  XmRImmediate, (caddr_t) NULL},
    {XmNfontListbold, XmCFontList, XmRFontList, sizeof(XmFontList),
        offset(list.fontbold),  XmRImmediate, (caddr_t) NULL},
#endif
    {XtNlist, XtCList, XtRPointer, sizeof(char **),
       offset(list.list), XtRString, NULL},
    {XtNdefaultColumns, XtCColumns, XtRInt,  sizeof(int),
	offset(list.default_cols), XtRImmediate, (XtPointer)2},
    {XtNlongest, XtCLongest, XtRInt,  sizeof(int),
	offset(list.longest), XtRImmediate, (XtPointer)0},
    {XtNnumberStrings, XtCNumberStrings, XtRInt,  sizeof(int),
	offset(list.nitems), XtRImmediate, (XtPointer)0},
    {XtNpasteBuffer, XtCBoolean, XtRBoolean,  sizeof(Boolean),
	offset(list.paste), XtRImmediate, (XtPointer) False},
    {XtNforceColumns, XtCColumns, XtRBoolean,  sizeof(Boolean),
	offset(list.force_cols), XtRImmediate, (XtPointer) False},
    {XtNverticalList, XtCBoolean, XtRBoolean,  sizeof(Boolean),
	offset(list.vertical_cols), XtRImmediate, (XtPointer) False},
    {XtNinternalWidth, XtCWidth, XtRDimension,  sizeof(Dimension),
	offset(list.internal_width), XtRImmediate, (XtPointer)4},
    {XtNinternalHeight, XtCHeight, XtRDimension, sizeof(Dimension),
	offset(list.internal_height), XtRImmediate, (XtPointer)2},
    {XtNcolumnSpacing, XtCSpacing, XtRDimension,  sizeof(Dimension),
	offset(list.column_space), XtRImmediate, (XtPointer)6},
    {XtNrowSpacing, XtCSpacing, XtRDimension,  sizeof(Dimension),
	offset(list.row_space), XtRImmediate, (XtPointer)2},
    {XtNcallback, XtCCallback, XtRCallback, sizeof(XtPointer),
        offset(list.callback), XtRCallback, NULL},
    {XtNscrollcallback, XtCScrollCallback, XtRCallback, sizeof(XtPointer),
        offset(list.scallback), XtRCallback, NULL},
    {XtNmulitselect, XtCMulitselect, XtRBoolean,  sizeof(Boolean),
        offset(list.mulitselect),  XtRImmediate, (XtPointer)TRUE},
    {XtNuseboldfont ,XtCBoolean, XtRBoolean,  sizeof(Boolean),
        offset(list.bold),  XtRImmediate, (XtPointer)FALSE},
};
#undef offset

static void Initialize();
static void ChangeSize();
static void Resize();
static void Redisplay();
static void Exposex();
static void Destroy();
static void NOOP();
#if defined(OPENWINDOW)
static Boolean accept_focus();
static Widget  register_focus();
#endif
#if defined(MOTIF)
static void ClassInitialize();
#endif
static Boolean Layout();
static XtGeometryResult PreferredGeom();
static Boolean SetValues();
static void Notify(), Set(), Unset(), Listop();
int  do_scroll();

#if defined(MOTIF)||defined(OPENWINDOW)
static void ListEnter(), ListLeave(), ListFocusIn(), ListFocusOut();
#endif

static XtActionsRec actions[] = {
      {"Notify",         Notify},
      {"Set",            Set},
      {"Unset",          Unset},
      {"Listop", 	 Listop},
#if defined(MOTIF)||defined(OPENWINDOW)
      {"ListEnter",	 ListEnter},
      {"ListLeave",	 ListLeave},
      {"ListFocusIn",    ListFocusIn},
      {"ListFocusOut",   ListFocusOut},
#endif
};

static void Realize();

MyListClassRec MylistClassRec = {
  {
/* core_class fields */	
#if defined(MOTIF)
#define superclassx		(&xmPrimitiveClassRec)
#else
#if defined(XAW)
#if defined(XAW3D)
#define superclassx             (&threeDClassRec)
#else
#define superclassx             (&simpleClassRec)
#endif
#else
#if defined(OPENWINDOW)
#define superclassx             (&primitiveClassRec)
#endif
#endif
#endif
    /* superclass	  	*/	(WidgetClass) superclassx,
    /* class_name	  	*/	"MyList",
    /* widget_size	  	*/	sizeof(MyListRec),
#if defined(MOTIF)
    /* class_initialize   	*/	ClassInitialize,
#else
#if defined(XAW)
    /* class_initialize   	*/	XawInitializeWidgetSet,
#else
    /* class_initialize   	*/	NULL,
#endif
#endif
    /* class_part_initialize	*/	NULL,
    /* class_inited       	*/	FALSE,
    /* initialize	  	*/	Initialize,
    /* initialize_hook		*/	NULL,
    /* realize		  	*/	Realize,
    /* actions		  	*/	actions,
    /* num_actions	  	*/	XtNumber(actions),
    /* resources	  	*/	resources,
    /* num_resources	  	*/	XtNumber(resources),
    /* xrm_class	  	*/	NULLQUARK,
    /* compress_motion	  	*/	TRUE,
#define CE (XtExposeCompressMultiple|XtExposeGraphicsExpose)
    /* compress_exposure  	*/	CE,
    /* compress_enterleave	*/	TRUE,
    /* visible_interest	  	*/	FALSE,
    /* destroy		  	*/	Destroy,
    /* resize		  	*/	Resize,
    /* expose		  	*/	Redisplay,
    /* set_values	  	*/	SetValues,
    /* set_values_hook		*/	NULL,
    /* set_values_almost	*/	XtInheritSetValuesAlmost,
    /* get_values_hook		*/	NULL,
#if defined(OPENWINDOW)
    /* accept_focus	 	*/	accept_focus,
#else
    /* accept_focus	 	*/	NULL,
#endif
    /* version			*/	XtVersion,
    /* callback_private   	*/	NULL,
    /* tm_table		   	*/	defaultTranslations,
    /* query_geometry		*/      PreferredGeom,
    /* disp acceletor	        */	NULL,
    /* extension	        */	NULL,
  },
#if defined(XAW)
/* Simple class fields initialization */
  {
    /* change_sensitive		*/	XtInheritChangeSensitive
  },
#if defined(XAW3D)
  {
    /* shadowdraw       	*/      XtInheritXaw3dShadowDraw
  }
#endif
#endif
#if defined(MOTIF)
  {
#if defined(XmREVISION)
#if XmREVISION>=2&&defined(XmInheritBorderHighlight)
      XmInheritBorderHighlight,         /* border_highlight   */
      XmInheritBorderUnhighlight,       /* border_unhighlight */
#else
      _XmHighlightBorder,               /* border_highlight   */
      _XmUnhighlightBorder,             /* border_unhighlight */
#endif
#else
      _XmHighlightBorder,               /* border_highlight   */
      _XmUnhighlightBorder,             /* border_unhighlight */
#endif
      NULL,
      NULL,
      NULL,
      NULL,
      NULL,
  }
#endif
#if defined(OPENWINDOW)
  {
      NULL,
      XtInheritHighlightHandler,
      XtInheritTraversalHandler,
      register_focus,
      XtInheritActivateFunc,
      NULL,
      0,
      OlVersion,
      NULL,
      NULL,
      NULL,
  }
#endif
};

WidgetClass MylistWidgetClass = (WidgetClass)&MylistClassRec;

/****************************************************************
 *
 * Private Procedures
 *
 ****************************************************************/

#if defined(MOTIF)
static void ClassInitialize(w)
    Widget w;
{
}
#endif

static void NOOP()
{
}

#if defined(MOTIF)
static void
ListFocusIn(w, event, params, num_params)
Widget w;
XEvent * event;
String * params;
Cardinal *num_params;
{
#if XmREVISION==1
     _XmPrimitiveFocusIn((XmPrimitiveWidget)w, (XEvent*)event);
#endif
#if XmREVISION==2
     _XmPrimitiveFocusIn((XmPrimitiveWidget)w, (XEvent*)event, NULL, 0);
#endif
}

static void
ListFocusOut(w, event, params, num_params)
Widget w;
XEvent * event;
String * params;
Cardinal *num_params;
{
#if XmREVISION==1
    _XmPrimitiveFocusOut((XmPrimitiveWidget)w, event);
#endif
#if XmREVISION==2
    _XmPrimitiveFocusOut((XmPrimitiveWidget)w, event, NULL, 0);
#endif
}

static void
ListEnter(w, event, params, num_params)
Widget w;
XEvent * event;
String * params;
Cardinal *num_params;
{
     XmProcessTraversal(w, XmTRAVERSE_CURRENT);
#if XmREVISION==1
    _XmPrimitiveEnter((XmPrimitiveWidget)w, (XEvent*)event);
#endif
#if XmREVISION==2
    _XmPrimitiveEnter((XmPrimitiveWidget)w, (XEvent*)event, NULL, 0);
#endif
}

static void
ListLeave(w, event, params, num_params)
Widget w;
XEvent * event;
String * params;
Cardinal *num_params;
{
#if XmREVISION==1
     _XmPrimitiveLeave((XmPrimitiveWidget)w, (XEvent*)event);
#endif
#if XmREVISION==2
     _XmPrimitiveLeave((XmPrimitiveWidget)w, (XEvent*)event, NULL, 0);
#endif
}

#if defined(DEC_STUFF)||defined(HP_STUFF)
typedef struct _PixmapCache {
    Screen *screen;
    Pixmap pixmap;
    Pixel foreground, background;
    unsigned int depth;
    int ref_count;
    struct _PixmapCache *next;
  } CacheEntry;

static CacheEntry *pixmapCache = NULL;

static Pixmap XmuCreateStippledPixmap(screen, fore, back, depth)
    Screen *screen;
    Pixel fore, back;
    unsigned int depth;
/*
 *	Creates a stippled pixmap of specified depth
 *	caches these so that multiple requests share the pixmap
 */
{
    register Display *display = DisplayOfScreen(screen);
    CacheEntry *cachePtr;
    Pixmap stippled_pixmap;
    static unsigned char pixmap_bits[] = {
	0x02, 0x01,
    };

/*
 *	Creates a stippled pixmap of depth DefaultDepth(screen)
 *	caches these so that multiple requests share the pixmap
 */

#define pixmap_width 2
#define pixmap_height 2

    /* see if we already have a pixmap suitable for this screen */
    for (cachePtr = pixmapCache; cachePtr; cachePtr = cachePtr->next) {
	if (cachePtr->screen == screen && cachePtr->foreground == fore &&
	    cachePtr->background == back && cachePtr->depth == depth)
	    return( cachePtr->ref_count++, cachePtr->pixmap );
    }

    stippled_pixmap = XCreatePixmapFromBitmapData (display,
			RootWindowOfScreen(screen), (char *)pixmap_bits, 
			pixmap_width, pixmap_height, fore, back, depth);

    /* and insert it at the head of the cache */
    cachePtr = XtNew(CacheEntry);
    cachePtr->screen = screen;
    cachePtr->foreground = fore;
    cachePtr->background = back;
    cachePtr->depth = depth;
    cachePtr->pixmap = stippled_pixmap;
    cachePtr->ref_count = 1;
    cachePtr->next = pixmapCache;
    pixmapCache = cachePtr;

    return( stippled_pixmap );
}

static void XmuReleaseStippledPixmap(screen, pixmap)
    Screen *screen;
    Pixmap pixmap;
{
    register Display *display = DisplayOfScreen(screen);
    CacheEntry *cachePtr, **prevP;
    for (prevP = &pixmapCache, cachePtr = pixmapCache; cachePtr;) {
	if (cachePtr->screen == screen && cachePtr->pixmap == pixmap) {
	    if (--cachePtr->ref_count == 0) {
		XFreePixmap( display, pixmap );
		*prevP = cachePtr->next;
		XtFree( (char*)cachePtr );
		break;
	    }
	}
	prevP = &cachePtr->next;
	cachePtr = *prevP;
    }
}
#endif
#endif

#if defined(OPENWINDOW)
static void
ListFocusIn(w, event, params, num_params)
Widget w;
XEvent * event;
String * params;
Cardinal *num_params;
{
/*
    _OlSetCurrentFocusWidget(w, OL_IN);
*/
}

static void
ListFocusOut(w, event, params, num_params)
Widget w;
XEvent * event;
String * params;
Cardinal *num_params;
{
/*
    _OlSetCurrentFocusWidget(w, OL_OUT);
*/
}

static void
ListEnter(w, event, params, num_params)
Widget w;
XEvent * event;
String * params;
Cardinal *num_params;
{
/*
    _OlSetCurrentFocusWidget(w, OL_IN);
*/
    /*
     * I know.  A gross hack. 
     */
    XSetInputFocus(XtDisplay(w), XtWindow(w), RevertToNone, CurrentTime);
}

static void
ListLeave(w, event, params, num_params)
Widget w;
XEvent * event;
String * params;
Cardinal *num_params;
{
/*
    _OlSetCurrentFocusWidget(w, OL_OUT);
*/
}
#endif

#if defined(OPENWINDOW)
static Boolean
accept_focus(w, timestamp)
Widget w;
Time   *timestamp;
{
#if defined(XX)
#if defined(OL_VERSION)&&OL_VERSION==3
    XSetInputFocus(XtDisplay(w), XtWindow(w), RevertToNone, *timestamp);
#endif
#endif
    return TRUE;
}

static Widget
register_focus(w)
Widget w;
{
    return w;
}
#endif


static void GetGCs(w)
Widget w;
{
    XGCValues	    values;
    XtGCMask        valueMask;
    MyListWidget lw = (MyListWidget) w;    


    valueMask = GCForeground | GCFont | GCGraphicsExposures | GCFunction; 
    values.foreground	= lw->list.foreground;
    values.font		= lw->list.fs->fid;
    values.function     = GXcopy;
    values.graphics_exposures = TRUE;
    lw->list.normgc = XtGetGC(w, valueMask, &values);

    values.foreground	= lw->core.background_pixel;
    lw->list.revgc = XtGetGC(w, valueMask, &values);

    valueMask = GCForeground | GCFont | GCGraphicsExposures | GCFunction; 
    values.foreground	= lw->list.foreground;
    values.font		= lw->list.fsbold->fid;
    values.function     = GXcopy;
    values.graphics_exposures = TRUE;
    lw->list.boldgc = XtGetGC(w, valueMask, &values);
    values.foreground	= lw->core.background_pixel;
    lw->list.revboldgc = XtGetGC(w, valueMask, &values);


    values.tile       = XmuCreateStippledPixmap(XtScreen(w), 
						lw->list.foreground,
						lw->core.background_pixel,
						lw->core.depth);
    values.fill_style = FillTiled;

    valueMask |= GCTile | GCFillStyle;
    lw->list.graygc = XtGetGC(w, valueMask, &values);
}

static void
Realize( w, valueMask, attributes )
Widget w;
Mask *valueMask;
XSetWindowAttributes *attributes;
{
  MyListWidget lw = (MyListWidget) w;    
  int i;
  
  (*MylistClassRec.core_class.superclass->core_class.realize)
    (w, valueMask, attributes);

}

/*	Function Name: ResetList
 *	Description: Resets the new list when important things change.
 *	Arguments: w - the widget.
 *                 changex, changey - allow the height or width to change?
 *	Returns: none.
 */

static int sl[1];
static void
ResetList(w, changex, changey)
Widget w;
Boolean changex, changey;
{
    MyListWidget lw = (MyListWidget) w;
    Dimension width = w->core.width;
    Dimension height = w->core.height;
    register int i, len;

/*
 * If list is NULL then the list will just be the name of the widget.
 */

    if (lw->list.list == NULL) {
      lw->list.list = &(lw->core.name);
      lw->list.nitems = 1;
      sl[0] = 0;
      lw->list.selected = sl;
      sl[0] = 0;
    }

    if (lw->list.nitems == 0)	    /* Get number of items. */
        for ( ; lw->list.list[lw->list.nitems] != NULL ; lw->list.nitems++);
    
    if (lw->list.longest == 0) { /* Get column width. */
        for ( i = 0 ; i < lw->list.nitems; i++) {
	    len = XTextWidth(lw->list.fs, lw->list.list[i],
			     strlen(lw->list.list[i]));
	    if (len > lw->list.longest) {
	        lw->list.longest = len;
	        lw->list.longest_str = strlen(lw->list.list[i]);
	    }
	}
	lw->list.longest += XTextWidth(lw->list.fs, "  ", 2);
	lw->list.longest_str += 2;
    } 

    lw->list.col_width = lw->list.longest + lw->list.column_space + 
	                 2*PAD; 

    width = w->core.width;
    height = w->core.height;
    if(Layout(w, FALSE, FALSE, &width, &height));
}

/*	Function Name: ChangeSize.
 *	Description: Laysout the widget.
 *	Arguments: w - the widget to try change the size of.
 *	Returns: none.
 */

static void
ChangeSize(w, width, height)
Widget w;
Dimension width, height;
{
    XtWidgetGeometry request, reply;

    request.request_mode = CWWidth | CWHeight;
    request.width = width;
    request.height = height;
    
    switch ( XtMakeGeometryRequest(w, &request, &reply) ) {
    case XtGeometryYes:
    case XtGeometryNo:
        break;
    case XtGeometryAlmost:
	Layout(w, (request.height != reply.height),
	          (request.width != reply.width),
	       &(reply.width), &(reply.height));
	request = reply;
	switch (XtMakeGeometryRequest(w, &request, &reply) ) {
	case XtGeometryYes:
	case XtGeometryNo:
	    break;
	case XtGeometryAlmost:
	    request = reply;
	    if (Layout(w, FALSE, FALSE,
		       &(request.width), &(request.height))) {
	      char buf[BUFSIZ];
	      sprintf(buf, "List Widget: %s %s",
		      "Size Changed when it shouldn't have",
		      "when computing layout");
	      XtAppWarning(XtWidgetToApplicationContext(w), buf);
	    }
	    request.request_mode = CWWidth | CWHeight;
	    XtMakeGeometryRequest(w, &request, &reply);
	    break;
	default:
	  XtAppWarning(XtWidgetToApplicationContext(w),
		       "List Widget: Unknown geometry return.");
	  break;
	}
	break;
    default:
	XtAppWarning(XtWidgetToApplicationContext(w),
		     "List Widget: Unknown geometry return.");
	break;
    }
}

/*	Function Name: Initialize
 *	Description: Function that initilizes the widget instance.
 *	Arguments: junk - NOT USED.
 *                 new  - the new widget.
 *	Returns: none
 */


/* ARGSUSED */
static void 
Initialize(junk, new)
Widget junk, new;
{
    MyListWidget lw = (MyListWidget) new;
 
    lw->list.top = 0;
    lw->list.yoffset = 0;
    lw->list.xoffset = 0;
#if defined(XAW3D)
    lw->list.redisplay = 0;
#endif
#if defined(MOTIF)
    if (lw->list.font == NULL) {
	lw->list.font = _XmGetDefaultFontList ((Widget)lw,XmTEXT_FONTLIST);
        lw->list.font = XmFontListCopy(lw->list.font);
    }
    _XmFontListGetDefaultFont(lw->list.font, &lw->list.fs);
    if (!lw->list.fs)  {
	XtAppWarning(XtWidgetToApplicationContext(new), "No font list");
        return;
    }
    if (lw->list.fontbold == NULL) {
	lw->list.fontbold = _XmGetDefaultFontList ((Widget)lw, XmTEXT_FONTLIST);
        lw->list.fontbold = XmFontListCopy(lw->list.font);
    }
    _XmFontListGetDefaultFont(lw->list.fontbold, &lw->list.fsbold);
    if (!lw->list.fsbold)  {
	XtAppWarning(XtWidgetToApplicationContext(new), "No bold font list");
        return;
    }
#else
    lw->list.fs = lw->list.font;
    if (lw->list.fontbold == NULL || !lw->list.bold) {
	lw->list.fsbold = lw->list.font;
	lw->list.fs     = lw->list.font;
    } else {
	lw->list.fsbold = lw->list.fontbold;
	lw->list.fs     = lw->list.font;
    }
#endif

/* 
 * Initialize all private resources.
 */

    GetGCs(new);

    /* Set row height. */
    lw->list.row_height = lw->list.fs->max_bounds.ascent
			+ lw->list.fs->max_bounds.descent
			+ lw->list.row_space + 2*PAD;

    ResetList(new, (new->core.width == 0), (new->core.height == 0));

    lw->list.highlight = lw->list.is_highlighted = NO_HIGHLIGHT;

} /* Initialize */

/*	Function Name: CvtToItem
 *	Description: Converts Xcoord to item number of item containing that
 *                   point.
 *	Arguments: w - the list widget.
 *                 xloc, yloc - x location, and y location.
 *	Returns: the item number.
 */

static int
CvtToItem(w, xloc, yloc, item)
Widget w;
int xloc, yloc;
int *item;
{
    int one, another;
    MyListWidget lw = (MyListWidget) w;
    int ret_val = OKAY;

    if (lw->list.vertical_cols) {
        one  =  xloc - lw->list.clip_right_margin - PAD;
	if (one >= 0) one = lw->list.nrows*(one/lw->list.col_width);
	else          one = 0;
        another = yloc -  lw->list.clip_top_margin - lw->list.yoffset - PAD;
	if (another >= 0) another = another/lw->list.row_height;
	else              another = 0;
	 /* If out of range, return minimum possible value. */
	if (another >= lw->list.nrows) {
	    another = lw->list.nrows - 1;
	    ret_val = OUT_OF_RANGE;
	}
    }
    else {
        one = yloc -  lw->list.clip_top_margin - lw->list.yoffset - PAD;
	if (one >= 0) one = lw->list.ncols*(one/lw->list.row_height);
	else          one = 0;
	/* If in right margin handle things right. */
        another = xloc - lw->list.clip_right_margin - PAD;
 	if (another >= 0) another = another/lw->list.col_width;
	else		  another = 0;  
	if (another >= lw->list.ncols) {
	    another = lw->list.ncols - 1; 
	    ret_val = OUT_OF_RANGE;
	}
    }  
    if ((xloc < 0) || (yloc < 0))
        ret_val = OUT_OF_RANGE;
    if (one < 0) one = 0;
    if (another < 0) another = 0;
    *item = one + another;
    if (*item >= lw->list.nitems) return(OUT_OF_RANGE);
    return(ret_val);
}

/*	Function Name: FindCornerItems.
 *	Description: Find the corners of the rectangle in item space.
 *	Arguments: w - the list widget.
 *                 event - the event structure that has the rectangle it it.
 *                 ul_ret, lr_ret - the corners ** RETURNED **.
 *	Returns: none.
 */

static void
FindCornerItems(w, event, ul_ret, lr_ret)
Widget w;
XEvent * event;
int *ul_ret, *lr_ret;
{
    int xloc, yloc;
    int n;

    if (event->type == Expose) {
        xloc = event->xexpose.x;
        yloc = event->xexpose.y;
        n = CvtToItem(w, xloc, yloc, ul_ret);
        xloc += event->xexpose.width;
        yloc += event->xexpose.height;
        CvtToItem(w, xloc, yloc, lr_ret);
    } else if (event->type == GraphicsExpose) {
        xloc = event->xgraphicsexpose.x;
        yloc = event->xgraphicsexpose.y;
        CvtToItem(w, xloc, yloc, ul_ret);
        xloc += event->xgraphicsexpose.width;
        yloc += event->xgraphicsexpose.height;
        CvtToItem(w, xloc, yloc, lr_ret);
    } else {
	*lr_ret = 0;
	*ul_ret = 0;
    }
    
}

/*	Function Name: ItemInRectangle
 *	Description: returns TRUE if the item passed is in the given rectangle.
 *	Arguments: w - the list widget.
 *                 ul, lr - corners of the rectangle in item space.
 *                 item - item to check.
 *	Returns: TRUE if the item passed is in the given rectangle.
 */

static Boolean
ItemInRectangle(w, ul, lr, item)
Widget w;
int ul, lr, item;
{
    MyListWidget lw = (MyListWidget) w;
    register int mod_item;
    int things;
    
    if (item < ul || item > lr) 
        return(FALSE);
    if (lw->list.vertical_cols)
        things = lw->list.nrows;
    else
        things = lw->list.ncols;

/*s lag */
    if (things == 0)  return(FALSE);
/*s lag */

    mod_item = item % things;
    if ( (mod_item >= ul % things) && (mod_item <= lr % things ) )
        return(TRUE);
    return(FALSE);
}

/*	Function Name: HighlightBackground
 *	Description: paints the color of the background for the given item.
 *	Arguments: w - the widget.
 *                 x, y - ul corner of the area item occupies.
 *                 item - the item we are dealing with.
 *                 gc - the gc that is used to paint this rectangle
 *	Returns: 
 */

static void
HighlightBackground(w, x, y, item, gch, gcs)
Widget w;
int x, y, item;
GC gch;
GC gcs;
{
    MyListWidget lw = (MyListWidget) w;
    int hl_x, hl_y, width, height;


    hl_x = x - lw->list.column_space/2;
    width = lw->list.longest - lw->list.fs->max_bounds.width*lw->list.xoffset;
    hl_y = y - lw->list.row_space/2;
    height = lw->list.row_height + lw->list.row_space - 2*PAD;

    XFillRectangle(XtDisplay(w), XtWindow(w), gch, hl_x, hl_y, width, height);
    if(!gcs) return;
    hl_x = hl_x - PAD;
    hl_y = hl_y - PAD;

    width = width + PAD*2 - 1;
    if (width < 0) return;
    height = height + PAD*2 - 1;
    XDrawRectangle(XtDisplay(w), XtWindow(w), gcs, hl_x, hl_y, width, height);
}

/*	Function Name: PaintItemName
 *	Description: paints the name of the item in the appropriate location.
 *	Arguments: w - the list widget.
 *                 item - the item to draw.
 *	Returns: none.
 *
 *      NOTE: no action taken on an unrealized widget.
 */

static void
PaintItemName(w, item)
Widget w;
int item;
{
    char * str;
    int  bottom;
    GC gc;
    GC gch;
    GC gcs;
    int x, y, str_y;
    MyListWidget lw = (MyListWidget) w;
    MyListClassRec *lwc = (MyListClassRec *) XtClass(w);
    XFontStruct     *fs;

    if (!XtIsRealized(w)) return; /* Just in case... */

    if (lw->list.selected[item]&LIST_BOLD) 
        fs = lw->list.fsbold;
    else 
        fs = lw->list.fs;
   
    if (lw->list.vertical_cols) {
	x = lw->list.col_width * (item / lw->list.nrows)
	  + lw->list.clip_right_margin + PAD;
        y = lw->list.row_height * (item % lw->list.nrows)
	  + lw->list.clip_top_margin + PAD + lw->list.yoffset;
    }
    else {
        x = lw->list.col_width * (item % lw->list.ncols)
	  + lw->list.clip_right_margin + PAD;
        y = lw->list.row_height * (item / lw->list.ncols)
	  + lw->list.clip_top_margin + PAD + lw->list.yoffset;
    }

    str_y = y + fs->max_bounds.ascent;

    gch = lw->list.revgc;
    gcs = NULL;
    if (!XtIsSensitive(w)) {
	gc = lw->list.graygc;
    } else {
	if (lw->list.selected[item]&LIST_BOLD) {
	    gc = lw->list.boldgc;
	} else  {	
	    gc = lw->list.normgc;
	}
    }
   
    if (lw->list.selected[item]&LIST_SELECT) {
	if (lw->list.selected[item]&LIST_BOLD) {
	    gc  = lw->list.revboldgc;
	    gch = lw->list.boldgc;
	} else {
 	    gc = lw->list.revgc;
	    gch = lw->list.normgc;
	}
    }

    if (item == lw->list.is_highlighted) {
        if (item == lw->list.highlight) {
	    if (lw->list.selected[item]&LIST_BOLD) {
 	        gcs = lw->list.boldgc;
	     } else {
 	        gcs = lw->list.normgc;
	     }
        } else {
	    if (lw->list.selected[item]&LIST_BOLD) {
 	        gcs = lw->list.revboldgc;
	    } else {
	        gcs = lw->list.revgc;
	    }
            lw->list.is_highlighted = NO_HIGHLIGHT;
        }
    } else {
	if(item == lw->list.highlight) {
	    if (lw->list.selected[item]&LIST_BOLD) {
 	        gcs = lw->list.boldgc;
	    } else {
 	        gcs = lw->list.normgc;
	    }
            lw->list.is_highlighted = item;
	}
    }
	 
    bottom = (int)lw->list.clip_height/(int)lw->list.row_height;
    bottom = bottom * lw->list.row_height;
    bottom = bottom + lw->list.clip_top_margin + 2*PAD; 
    if ( y     < 0  )                             return;
    if ((y + lw->list.row_height + PAD) > bottom) return;

    str =  lw->list.list[item];	/* draw it */
    HighlightBackground(w, x, y, item, gch, gcs);
    if (lw->list.xoffset < (int)strlen(str)) {
	str = &str[lw->list.xoffset];
        XDrawString(XtDisplay(w), XtWindow(w), gc, x, str_y, str, strlen(str));
    } 
#if defined(XAW3D)
    /*
     * Hack to redraw boarder.
     */
    {
	Region region;
        XPoint points[5];
	int width;
        int height;

	width = lw->list.longest - 
		lw->list.fs->max_bounds.width*lw->list.xoffset;
	height = lw->list.row_height + lw->list.row_space - 2*PAD;
	if (((int)(x - lw->list.column_space/2 + width) < 
	     (int)lw->list.clip_width) ||
	    lw->list.redisplay) return;

        points[0].x = x - lw->list.column_space/2;
	points[0].y = y - lw->list.row_space/2;
	 
        points[1].x = points[0].x + width;
	points[1].y = points[0].y;

        points[2].x = points[0].x + width;
	points[2].y = points[0].y + height;
   
	points[3].x = points[0].x;
	points[4].y = points[0].y + height;;

        points[5].x = points[0].x ;
	points[5].y = points[0].y;

        region = XPolygonRegion(points, 4, WindingRule);
        (*lwc->threeD_class.shadowdraw)(w, NULL, region, FALSE);
        XDestroyRegion(region);
    }
#endif
}
    
/*	Function Name: Redisplay
 *	Description: Repaints the widget window on expose events.
 *	Arguments: w - the list widget.
 *                 event - the expose event for this repaint.
 *                 junk - NOT USED.
 *	Returns: 
 */

/* ARGSUSED */
static void 
Redisplay(w, event, region)
Widget w;
XEvent *event;
Region region;
{
    int item;			/* an item to work with. */
    int ul_item, lr_item;       /* corners of items we need to paint. */
    MyListWidget lw = (MyListWidget) w;
    MyListClassRec *lwc = (MyListClassRec *) XtClass(w);
    Widget parent;


    if (event == NULL) {	/* repaint all. */
        ul_item = 0;
	lr_item = lw->list.nrows * lw->list.ncols - 1;
#if defined(XAW3D)
        XClearArea(XtDisplay(lw), XtWindow(lw),
   		   lw->list.clip_right_margin - PAD,
    	           lw->list.clip_top_margin - PAD,
                   lw->list.clip_width + 2*PAD, /* most be off by one */
                   lw->list.clip_height + 2*PAD,
		   FALSE);
#else
	XClearWindow(XtDisplay(w), XtWindow(w));
#endif
    } else {
        FindCornerItems(w, event, &ul_item, &lr_item);
    }
#if defined(XAW3D)
    lw->list.redisplay = 1;
#endif
    for (item = ul_item; (item <= lr_item && item < lw->list.nitems) ; item++)
      if (ItemInRectangle(w, ul_item, lr_item, item))  PaintItemName(w, item);

#if defined(MOTIF)
    if (lw->primitive.highlighted)
         _XmHighlightBorder((Widget)lw);
    else 
         _XmUnhighlightBorder((Widget)lw);
#endif
#if defined(XAW3D)
    (*lwc->threeD_class.shadowdraw)(w, event, region, FALSE);
    lw->list.redisplay = 0;
#endif
}

/*	Function Name: PreferredGeom
 *	Description: This tells the parent what size we would like to be
 *                   given certain constraints.
 *	Arguments: w - the widget.
 *                 intended - what the parent intends to do with us.
 *                 requested - what we want to happen.
 *	Returns: none.
 */

static XtGeometryResult 
PreferredGeom(w, intended, requested)
Widget w;
XtWidgetGeometry *intended, *requested;
{
    Dimension new_width, new_height;
    Boolean change, width_req, height_req;
    
    width_req = intended->request_mode & CWWidth;
    height_req = intended->request_mode & CWHeight;

    if (width_req)
      new_width = intended->width;
    else
      new_width = w->core.width;

    if (height_req)
      new_height = intended->height;
    else
      new_height = w->core.height;

    requested->request_mode = 0;
    
/*
 * We only care about our height and width.
 */
    if ( !width_req && !height_req)
       return(XtGeometryYes);
    
    change = Layout(w, !width_req, !height_req, &new_width, &new_height);

    requested->request_mode |= CWWidth;
    requested->width = new_width;
    requested->request_mode |= CWHeight;
    requested->height = new_height;

    if (change)
        return(XtGeometryAlmost);
    return(XtGeometryYes);
}

/*	Function Name: Resize
 *	Description: resizes the widget, by changing the number of rows and
 *                   columns.
 *	Arguments: w - the widget.
 *	Returns: none.
 */

void
MyResize_list(w)
Widget w;
{

  return;
}

static void
Resize(w)
Widget w;
{
  Dimension width, height;
  MyListWidget lw = (MyListWidget) w;
  int  top, left, right;

  width = w->core.width;
  height = w->core.height;

  ClipListGC(w, &width, &height);
  if (Layout(w, FALSE, FALSE, &width, &height))
    XtAppWarning(XtWidgetToApplicationContext(w),
	    "List Widget: Size changed when it shouldn't have when resising.");
  if (!do_scroll(w, SCROLL_SET,  SCROLL_TYPE_V,  0)) {
      SetPos(lw, lw->list.top, 1);
  }
}

/*	Function Name: Layout
 *	Description: lays out the item in the list.
 *	Arguments: w - the widget.
 *                 xfree, yfree - TRUE if we are free to resize the widget in
 *                                this direction.
 *                 width, height - the is the current width and height that 
 *                                 we are going to layout the list widget to,
 *                                 depending on xfree and yfree of course.
 *                               
 *	Returns: TRUE if width or height have been changed.
 */

static Boolean
Layout(w, xfree, yfree, width, height)
Widget w;
Boolean xfree, yfree;
Dimension *width, *height;
{
    MyListWidget lw = (MyListWidget) w;
    Boolean change = FALSE;

/* 
 * If force columns is set then always use number of columns specified
 * by default_cols.
 */

    if (lw->list.force_cols) {
        lw->list.ncols = lw->list.default_cols;
	if (lw->list.ncols <= 0) lw->list.ncols = 1;
	/* 12/3 = 4 and 10/3 = 4, but 9/3 = 3 */
	lw->list.nrows = ( ( lw->list.nitems - 1) / lw->list.ncols) + 1 ;
	if (xfree && lw->core.width != *width) {
	    change = TRUE;
	}
        if (yfree && lw->core.height != *height) {
	    change = TRUE;
	}
	return(change);
    }

    lw->list.ncols = ( (int)(*width - 2 * lw->list.clip_right_margin)
                            / (int)lw->list.col_width);
    if (lw->list.ncols <= 0) lw->list.ncols = 1;
    lw->list.nrows = ( ( lw->list.nitems - 1) / lw->list.ncols) + 1 ;
    if (xfree && lw->core.width != *width) {
 	change = TRUE;
    }
    if (yfree && lw->core.height != *height) {
        change = TRUE;
    }
    return(change);
}

/*	Function Name: Listop
 *	Description: Porcess keybord commands
 *	Arguments: w - the widget that the notify occured in.
 *                 event - event that caused this notification.
 *                 params, num_params - Up, Down, Right, Left, Select 
 *	Returns: none.
 */

/* ARGSUSED */
static void
Listop(w, event, params, num_params)
Widget w;
XEvent * event;
String * params;
Cardinal *num_params;
{
  int item = -1;
  MyListWidget lw = (MyListWidget) w;
  int add, x,y, page, pageit;
  int item_len;
  MyXawListReturnStruct ret_value_string;
  Widget parent;
  int  old_yoffset;
  int  top;
  int  o = 0;
  int  os = 0;
  int  s = 0;

  
  if (!XtIsRealized(w)) return; /* Just in case... */
  pageit  = 0;

  if( *num_params != 1) return;
  if (lw->list.nitems <= 0) return;
  if (strcmp(params[0], "Top") == 0) {
	item = 0;
  }
  if (strcmp(params[0], "Bottom") == 0) item = lw->list.nitems;
  if (item < 0) {
    item =  lw->list.is_highlighted;
    if (lw->list.is_highlighted == NO_HIGHLIGHT) item = 0;
  }
  if (lw->list.vertical_cols) {
        add  =  1;
  } else {
        add  =  lw->list.ncols;
  }
  if (lw->list.is_highlighted != NO_HIGHLIGHT) {
	o = lw->list.is_highlighted%add;
	os++;
  }

  parent = XtParent(w);
  page = (((int)lw->list.clip_height+2*PAD)/(int)lw->list.row_height)*add;

  if (strcmp(params[0], "Select") == 0) {
    if (lw->list.selected[item]&LIST_SELECT) 
	lw->list.selected[item] &=  ~LIST_SELECT;
    else 	                             
	lw->list.selected[item] |= LIST_SELECT;
  } else if (strcmp(params[0], "Start") == 0 ) {
        item = (item/add)*add;
  } else if (strcmp(params[0], "Bottom") == 0 ) {
        item = (item/add)*add + o; 	
  } else if (strcmp(params[0], "End") == 0) {
        item = (item/add)*add +add - 1;
  } else if (strcmp(params[0], "Right") == 0) {
	item = item + 1;
  } else if (strcmp(params[0], "Left") == 0) {
	item = item - 1;
  } else if (strcmp(params[0], "Up") == 0) {
	if ((item - add) >= 0) item = item - add;
  } else if (strcmp(params[0], "PrevPage") == 0) {
        pageit  = 1;
	item = (item/page)*page;
	item = item - page;
	if (item < 0) item = 0;
	item = (item/add)*add;
	item = item + o;
  } else if (strcmp(params[0], "Down") == 0) {
        if ((item + add) < lw->list.nitems) item = item + add;
  } else if (strcmp(params[0], "NextPage") == 0) {
        pageit  = 1;
	item = item + page;
	item = (item/add)*add;
	item = item + o;
        if (item >= lw->list.nitems) {
		if (((lw->list.nitems-1)%add) < o) {
			item = lw->list.nitems - 1 - add ; 
		} else {
			item = lw->list.nitems - 1; 
		}
	}
	item = (item/add)*add;
	item = item + o;
  }

  if (item < 0) item = 0;
  if (item >= lw->list.nitems) item = lw->list.nitems - 1; 
  MyXawListHighlight(w, item);
  item_len = strlen(lw->list.list[item]);

  if ( lw->list.paste )       /* if XtNpasteBuffer set then paste it. */
             XStoreBytes(XtDisplay(w), lw->list.list[item], item_len);

  /*
   * Notify cursor position.
   */

  ret_value_string.string = lw->list.list[item];
  ret_value_string.list_index = item;
  ret_value_string.type = TYPE_LISTOP;

  XtCallCallbacks( w, XtNcallback, (XtPointer) &ret_value_string);

  old_yoffset = visable_y(lw, item, pageit, &top);

  if ( (-old_yoffset) == lw->list.yoffset) return;

  do_scroll(w, SCROLL_MOVE, SCROLL_TYPE_V,  old_yoffset/lw->list.row_height);
}

/*	Function Name: Notify
 *	Description: Notifies the user that a button has been pressed, and
 *                   calles the callback, if the XtNpasteBuffer resource
 *                   is true then the name of the item is also put in the
 *                   X cut buffer ( buf (0) ).
 *	Arguments: w - the widget that the notify occured in.
 *                 event - event that caused this notification.
 *                 params, num_params - not used.
 *	Returns: none.
 */

/* ARGSUSED */
static void
Notify(w, event, params, num_params)
Widget w;
XEvent * event;
String * params;
Cardinal *num_params;
{
    MyListWidget lw = ( MyListWidget ) w;
    int item, item_len;
    MyXawListReturnStruct ret_value;

/* 
 * Find item and if out of range then unhighlight and return. 
 * 
 * If the current item is unhighlighted then the user has aborted the
 * notify, so unhighlight and return.
 */

    if ( ((CvtToItem(w, event->xbutton.x, event->xbutton.y, &item))
	  == OUT_OF_RANGE) || (lw->list.highlight != item) ) {
        MyXawListUnhighlight(w);
        ret_value.string = NULL;
        ret_value.list_index = XAW_LIST_NONE;
        ret_value.type = TYPE_NOTIFY;
        XtCallCallbacks( w, XtNcallback, (XtPointer) &ret_value);
	return;
    }

    item_len = strlen(lw->list.list[item]);

    if ( lw->list.paste )	/* if XtNpasteBuffer set then paste it. */
        XStoreBytes(XtDisplay(w), lw->list.list[item], item_len);

/* 
 * Call Callback function.
 */

    ret_value.string = lw->list.list[item];
    ret_value.list_index = item;
    ret_value.type = TYPE_NOTIFY;
    
    XtCallCallbacks( w, XtNcallback, (XtPointer) &ret_value);
}

/*	Function Name: Unset
 *	Description: unhighlights the current element.
 *	Arguments: w - the widget that the event occured in.
 *                 event - not used.
 *                 params, num_params - not used.
 *	Returns: none.
 */

/* ARGSUSED */
static void
Unset(w, event, params, num_params)
Widget w;
XEvent * event;
String * params;
Cardinal *num_params;
{
  MyXawListUnhighlight(w);
}

/*	Function Name: Set
 *	Description: Highlights the current element.
 *	Arguments: w - the widget that the event occured in.
 *                 event - event that caused this notification.
 *                 params, num_params - not used.
 *	Returns: none.
 */

/* ARGSUSED */
static void
Set(w, event, params, num_params)
Widget w;
XEvent * event;
String * params;
Cardinal *num_params;
{
  int item;
  MyListWidget lw = (MyListWidget) w;
  int  select = 1;

  if ( (CvtToItem(w, event->xbutton.x, event->xbutton.y, &item))
      == OUT_OF_RANGE)  {
    MyXawListUnhighlight(w);		        /* Unhighlight current item. */
    return;
  }

  if (*num_params >= 1) {
	if (strcmp(params[0], "M") == 0) {
		if ( lw->list.mulitselect == FALSE) return;
  	    	if ( lw->list.is_highlighted == item)  return;
	} else if (strcmp(params[0], "2") == 0) {
		select = 1;
	} else if (strcmp(params[0], "1") == 0) {
		select = 1;
	} else { 
	 	select = 0;
	}
   }

   if (select) {
        if (lw->list.selected[item]&LIST_SELECT) 
		lw->list.selected[item] &= ~LIST_SELECT;
        else 	 
                lw->list.selected[item] |= LIST_SELECT;
   }
   MyXawListHighlight(w, item);                /* highlighted then do it. */
}

/*
 * Set specified arguments into widget
 */

static Boolean 
SetValues(current, request, new)
Widget current, request, new;
{
    MyListWidget cl = (MyListWidget) current;
    MyListWidget rl = (MyListWidget) request;
    MyListWidget nl = (MyListWidget) new;
    Boolean redraw = FALSE;
#if defined(MOTIF)
    XFontStruct     *fs;

    if ((cl->list.row_space != rl->list.row_space) ||
	(cl->list.font != rl->list.font) ||
	(cl->list.fontbold != rl->list.fontbold))  {
	if (cl->list.font != rl->list.font) {
	    XmFontListFree(cl->list.font);
	    nl->list.font = XmFontListCopy(nl->list.font);
            _XmFontListGetDefaultFont(nl->list.font, &fs);
	    nl->list.fs = fs;
	}
	if (cl->list.fontbold != rl->list.fontbold) {
	    XmFontListFree(cl->list.fontbold);
	    nl->list.fontbold = XmFontListCopy(nl->list.fontbold);
            _XmFontListGetDefaultFont(nl->list.fontbold, &fs);
	    nl->list.fsbold = fs;
	}
        nl->list.row_height = fs->max_bounds.ascent
                            + fs->max_bounds.descent
			    + nl->list.row_space;
        redraw = TRUE;
    }
#endif

    if ((cl->list.foreground != rl->list.foreground) ||
	(cl->core.background_pixel != rl->core.background_pixel) ||
	(cl->list.fontbold != rl->list.fontbold) ||
	(cl->list.bold != rl->list.bold) ||
	(cl->list.font != rl->list.font)) {
	XGCValues values;

#if !defined(MOTIF)
	if (cl->list.bold || !cl->list.bold) {
	    cl->list.fsbold = rl->list.font;
	    cl->list.fs = rl->list.font;
	} else {
	    cl->list.fsbold = rl->list.fontbold;
	    cl->list.fs = rl->list.font;
	}
#endif
	XGetGCValues(XtDisplay(current), cl->list.graygc, GCTile, &values);
	XmuReleaseStippledPixmap(XtScreen(current), values.tile);
	XtReleaseGC(current, cl->list.graygc);
	XtReleaseGC(current, cl->list.revgc);
	XtReleaseGC(current, cl->list.normgc);
	XtReleaseGC(current, cl->list.boldgc);
	XtReleaseGC(current, cl->list.revboldgc);
        GetGCs(new);
        redraw = TRUE;
    }

    /* Reset row height. */

    if ((cl->list.row_space != rl->list.row_space) ||
	(cl->list.font != rl->list.font))  {
        nl->list.row_height = nl->list.fs->max_bounds.ascent
	                    + nl->list.fs->max_bounds.descent
			    + nl->list.row_space;
   }

    
    if ((cl->core.width != rl->core.width)                     ||
	(cl->core.height != rl->core.height)                   ||
	(cl->list.internal_width != rl->list.internal_width)   ||
	(cl->list.internal_height != rl->list.internal_height) ||
	(cl->list.column_space != rl->list.column_space)       ||
	(cl->list.row_space != rl->list.row_space)             ||
	(cl->list.default_cols != rl->list.default_cols)       ||
	(  (cl->list.force_cols != rl->list.force_cols) &&
	   (rl->list.force_cols != rl->list.ncols) )           ||
	(cl->list.vertical_cols != rl->list.vertical_cols)     ||
	(cl->list.longest != rl->list.longest)                 ||
	(cl->list.nitems != rl->list.nitems)                   ||
	(cl->list.font != rl->list.font)                       ||
	(cl->list.list != rl->list.list)                        ) {

      ResetList(new, TRUE, TRUE);
      redraw = TRUE;
    }

    if (cl->list.list != rl->list.list)
	nl->list.is_highlighted = nl->list.highlight = NO_HIGHLIGHT;

    if ((cl->core.sensitive != rl->core.sensitive) ||
	(cl->core.ancestor_sensitive != rl->core.ancestor_sensitive)) {
    	nl->list.highlight = NO_HIGHLIGHT;
	redraw = TRUE;
    }
    
    if (!XtIsRealized(current))
      return(FALSE);
      
    return(redraw);
}

static void Destroy(w)
    Widget w;
{
    MyListWidget lw = (MyListWidget) w;
    XGCValues values;
    
    XGetGCValues(XtDisplay(w), lw->list.graygc, GCTile, &values);
    XmuReleaseStippledPixmap(XtScreen(w), values.tile);
    XtReleaseGC(w, lw->list.graygc);
    XtReleaseGC(w, lw->list.revgc);
    XtReleaseGC(w, lw->list.normgc);
}

/* Exported Functions */
MyXawListSetPos(w)
Widget w;
{
  MyXawListScrollReturnStruct ret_value ;
  MyListWidget lw = (MyListWidget) w;
  Widget parent;
  int item;
  int pos;

  if (!XtIsRealized(w)) return; /* Just in case... */
  if (lw->list.highlight == NO_HIGHLIGHT) return;

  item = lw->list.highlight;
  parent = XtParent(w);

  if (lw->list.vertical_cols) {
     pos = item % lw->list.nrows;
  } else {
     pos = item / lw->list.ncols;
  }

  do_scroll(w, SCROLL_MOVE, SCROLL_TYPE_V,  pos);
}


SetPos(w, item, top)
Widget w;
int item;
int top;
{
  int pos;
  MyListWidget lw = (MyListWidget) w;
  int xx = 0;;

  if (item == 0 && lw->list.yoffset == 0) return;
  pos = visable_y(lw, item, top, &xx);
  if ( (-pos) == lw->list.yoffset) return;
  do_scroll(w, SCROLL_MOVE, SCROLL_TYPE_V,  pos/lw->list.row_height);
}

/*	Function Name: XawListChange.
 *	Description: Changes the list being used and shown.
 *	Arguments: w - the list widget.
 *                 list - the new list.
 *                 nitems - the number of items in the list.
 *                 longest - the length (in Pixels) of the longest element
 *                           in the list.
 *                 resize - if TRUE the the list widget will
 *                          try to resize itself.
 *	Returns: none.
 *      NOTE:      If nitems of longest are <= 0 then they will be calculated.
 *                 If nitems is <= 0 then the list needs to be NULL terminated.
 */

void
#if NeedFunctionPrototypes
MyXawListChange(Widget w, char ** list, int nitems, int longest,
#if NeedWidePrototypes
	      int resize_it, 
#else
	      Boolean resize_it, 
#endif
int *select_list, int pos, int high)
#else
MyXawListChange(w, list, nitems, longest, resize_it, select_list, pos, high)
Widget w;
char ** list;
int nitems, longest;
Boolean resize_it;
int *select_list;
int pos;
int high;
#endif
{
    MyListWidget lw = (MyListWidget) w;
    int i;
    Dimension width, height;
    int no_resize;

    lw->list.list = list;
    lw->list.selected = select_list;
    if (nitems <= 0) nitems = 0;
    lw->list.nitems = nitems;
    if (longest <= 0) longest = 0;
    lw->list.longest = longest;
    lw->list.is_highlighted = lw->list.highlight = NO_HIGHLIGHT;
    if (high >= 0) {
	 lw->list.highlight = high;
    }
    lw->list.yoffset       = 0;
    lw->list.xoffset       = 0;
    ResetList(w, FALSE, FALSE);
    i = 0;
    if (pos >= 0 && pos < lw->list.nitems) i = pos;
    lw->list.top = i;
    if (!do_scroll(w, SCROLL_SET,  SCROLL_TYPE_V,  0)) {
         SetPos(lw, lw->list.top, 1);
         if (XtIsRealized(w)) Redisplay(w, NULL, NULL);
    }
}

/*	Function Name: MyXawListUnhighlight
 *	Description: unlights the current highlighted element.
 *	Arguments: w - the widget.
 *	Returns: none.
 */

void
#if NeedFunctionPrototypes
MyXawListUnhighlight(Widget w)
#else
MyXawListUnhighlight(w)
Widget w;
#endif
{
    MyListWidget lw = ( MyListWidget ) w;

    lw->list.highlight = NO_HIGHLIGHT;
    if (lw->list.is_highlighted != NO_HIGHLIGHT)
        PaintItemName(w, lw->list.is_highlighted); /* unhighlight this one. */
}

/*	Function Name: XawListHighlight
 *	Description: Highlights the given item.
 *	Arguments: w - the list widget.
 *                 item - the item to hightlight.
 *	Returns: none.
 */

void
#if NeedFunctionPrototypes
MyXawListHighlight(Widget w, int item)
#else
MyXawListHighlight(w, item)
Widget w;
int item;
#endif
{
    MyListWidget lw = ( MyListWidget ) w;
    
    if (XtIsSensitive(w)) {
        lw->list.highlight = item;
        if (lw->list.is_highlighted != NO_HIGHLIGHT)
            PaintItemName(w, lw->list.is_highlighted);  /* Unhighlight. */
	PaintItemName(w, item); /* HIGHLIGHT this one. */ 
    }
}

/*	Function Name: XawListShowCurrent
 *	Description: returns the currently highlighted object.
 *	Arguments: w - the list widget.
 *	Returns: the info about the currently highlighted object.
 */

MyXawListReturnStruct *
#if NeedFunctionPrototypes
MyXawListShowCurrent(Widget w)
#else
MyXawListShowCurrent(w)
Widget w;
#endif
{
    MyListWidget lw = ( MyListWidget ) w;
    MyXawListReturnStruct * ret_val;

    ret_val = (MyXawListReturnStruct *) XtMalloc (sizeof (MyXawListReturnStruct));
    
    ret_val->list_index = lw->list.highlight;
    if (ret_val->list_index == XAW_LIST_NONE)
      ret_val->string = "";
    else
      ret_val->string = lw->list.list[ ret_val->list_index ];

    return(ret_val);
}

set_list_yoffset(w, offset)
Widget w;
int offset;
{
    MyListWidget lw = ( MyListWidget ) w;
    int xyzzy;
    int ht = 0;
    int max_height;
    int max_item;
    int old_yoffset;
    XEvent event;
    int highlight;
    int top;
    int left;
    int right;

    offset = -offset * lw->list.row_height;
    if (lw->list.yoffset == offset) return;
    old_yoffset = lw->list.yoffset;

    if (lw->list.vertical_cols) {
        max_height = lw->list.row_height * (lw->list.nitems % lw->list.nrows) +
	 		 lw->list.clip_top_margin + PAD;
    } else {
        max_height = lw->list.row_height * (lw->list.nitems / lw->list.ncols);
        if (max_height == 0) max_height = lw->list.row_height;
        max_height +=lw->list.clip_top_margin + PAD;
    }
    if ( (-offset) > max_height) return;
    highlight = lw->list.highlight;
    lw->list.highlight = NO_HIGHLIGHT;
    if (lw->list.is_highlighted != NO_HIGHLIGHT)
        PaintItemName(w, lw->list.is_highlighted);  
    lw->list.yoffset = offset;

    xyzzy = lw->list.yoffset - old_yoffset;
    if (xyzzy < 0) xyzzy = - xyzzy;
    if (xyzzy > (int)lw->core.height/2) {
	xyzzy = 0;
    } else {
	xyzzy = 1;
    }
    if (xyzzy && lw->list.yoffset > old_yoffset) {
        XCopyArea(XtDisplay(lw), XtWindow(lw), XtWindow(lw), lw->list.normgc,
                 lw->list.clip_right_margin - PAD, 
	         lw->list.clip_top_margin - PAD  , 

		 lw->list.clip_width + 2*PAD,  
		 lw->list.clip_height + 2*PAD -
			   (lw->list.yoffset - old_yoffset), 

                 lw->list.clip_right_margin - PAD , 
		 lw->list.clip_top_margin  - PAD  +
			 (lw->list.yoffset - old_yoffset));
        event.type           = Expose;
        event.xexpose.x      = lw->list.clip_right_margin - PAD;
        event.xexpose.y      = lw->list.clip_top_margin   - PAD;
        event.xexpose.width  = lw->list.clip_width + 2*PAD;
    	event.xexpose.height = lw->list.clip_top_margin + 2*PAD +
			       lw->list.yoffset - old_yoffset;
        XClearArea(XtDisplay(lw), XtWindow(lw),
		   event.xexpose.x, event.xexpose.y,
		   event.xexpose.width,  event.xexpose.height, FALSE);
 	Redisplay(w, &event, NULL);
    } else if (xyzzy){
        XCopyArea(XtDisplay(lw), XtWindow(lw), XtWindow(lw), lw->list.normgc,
                  lw->list.clip_right_margin + PAD , 
		  lw->list.clip_top_margin  + PAD + 
				(old_yoffset - lw->list.yoffset), 

		  lw->list.clip_width + 2*PAD, 
		  lw->list.clip_height + 2*PAD - 
				(old_yoffset - lw->list.yoffset), 

		  lw->list.clip_right_margin + PAD,  
		  lw->list.clip_top_margin + PAD );

        event.type           = Expose;
        event.xexpose.x      =  lw->list.clip_right_margin - PAD;
        event.xexpose.y      =  lw->list.clip_top_margin   - PAD +
				lw->list.clip_height - 
			        (old_yoffset-lw->list.yoffset); 
        event.xexpose.width  = lw->list.clip_width + 2*PAD;
    	event.xexpose.height = old_yoffset - lw->list.yoffset + 2*PAD;
        XClearArea(XtDisplay(lw), XtWindow(lw),
		   event.xexpose.x, event.xexpose.y,
		   event.xexpose.width,  event.xexpose.height, FALSE);
 	Redisplay(w, &event, NULL);
    } else {
 	Redisplay(w, NULL, NULL);
    }
    lw->list.highlight = highlight;
    if (lw->list.highlight != NO_HIGHLIGHT) 
	PaintItemName(w, lw->list.highlight);  
    lw->list.top =  GetCurrentPos(w);
}

set_list_xoffset(w, offset)
Widget w;
int offset;
{
    MyListWidget lw = ( MyListWidget ) w;

    if (lw->list.xoffset == offset) return;
    lw->list.xoffset = offset;
    Redisplay(w, NULL, NULL);
}

int
do_scroll(w, action, type, value)
Widget w;
int    action;
int    type;
int    value;
{
    MyListWidget lw = ( MyListWidget ) w;
    MyXawListScrollReturnStruct ret_value;
    int max_height;
    int max_itemy;
    int max_itemx;
    XFontStruct     *fs;

    if (lw->list.vertical_cols) {
        max_itemy   = lw->list.nitems % lw->list.nrows;
    } else {
        max_itemy   = (lw->list.nitems + lw->list.ncols - 1) / lw->list.ncols;
    }

    ret_value.reason = action;
    ret_value.type =  type;
    ret_value.resize = 0;
    ret_value.value = value;

    ret_value.page_y = (int)lw->list.clip_height/(int)lw->list.row_height;
    ret_value.page_x = (int)lw->list.clip_width/
		       (int)lw->list.fs->max_bounds.width;
    ret_value.max_y  = max_itemy;
    ret_value.max_x  = lw->list.longest_str;
    if (ret_value.page_x >= ret_value.max_x || lw->list.ncols != 1) 
	ret_value.page_x = 0;
    ret_value.max_x += 5;
    if (ret_value.page_y >= max_itemy) {
	ret_value.page_y = 0;
    }
    if (type == SCROLL_TYPE_V && 
	value >= (ret_value.max_y - ret_value.page_y)) {
	ret_value.value = ret_value.max_y - ret_value.page_y;
    } else if (type == SCROLL_TYPE_H &&
	value >= (ret_value.max_x - ret_value.page_x)) {
	ret_value.value = ret_value.max_x - ret_value.page_x;
    }

    XtCallCallbacks( w, XtNscrollcallback, (XtPointer) &ret_value);
    return  ret_value.resize;
}


ClipListGC(widget, width, height)
Widget widget;
Dimension *width, *height;
{
    MyListWidget lw = ( MyListWidget ) widget;
    Position x, y, w,h;
    XRectangle rect;
    int bottom;


#if defined(XAW)
    x = lw->core.border_width;
    y = lw->core.border_width;
#else
    x = 0;
    y = 0;
#endif
#if defined(MOTIF)
    x += lw->primitive.shadow_thickness;
    y += lw->primitive.shadow_thickness;
#endif
#if defined(XAW3D)
    if (lw->threeD.shadow_width > 0) {
        x += lw->threeD.shadow_width;
        y += lw->threeD.shadow_width;
    }
#endif
    w = lw->core.width  - (2*x);
    h = lw->core.height - (2*y);
    h = (h-2*PAD)/lw->list.row_height;
    h = h*lw->list.row_height;
    h += 2*PAD;
    if (x == (lw->list.clip_right_margin - PAD) &&
	y == (lw->list.clip_top_margin   - PAD) &&
        w == (lw->list.clip_width        + PAD*2) &&
	h == (lw->list.clip_height       + PAD*2)) return; 

    lw->list.clip_right_margin  = x + PAD;
    lw->list.clip_top_margin    = y + PAD;
    lw->list.clip_width         = w - PAD*2;
    lw->list.clip_height        = h - PAD*2;
    if (width != NULL) *width = w;
    if (height != NULL) *height = h;

#if defined(DID_NOT_WORK)
    rect.x = 0 ;
    rect.y = 0 ;
    rect.width = w;
    rect.height = h;
    
    if (!XtIsRealized((Widget)lw)) return; /* Just in case... */

    XSetClipRectangles(XtDisplay((Widget)lw), lw->list.normgc, x, y, &rect, 1, 
		       Unsorted);
    XSetClipRectangles(XtDisplay((Widget)lw), lw->list.graygc, x, y, &rect, 1, 
		       Unsorted);
    XSetClipRectangles(XtDisplay((Widget)lw), lw->list.revgc, x, y, &rect, 1, 
		       Unsorted);
#endif
}

visable_y(lw, item, pageit, top)
MyListWidget lw;
int          item; 
int	     pageit;
int	     *top;
{ 
  int  x,y;
  int  bottom;
  int  old_yoffset;
  int  add;

  *top = GetCurrentPos(lw);

  if (lw->list.vertical_cols) {
     x = lw->list.col_width * (item / lw->list.nrows)
         + lw->list.clip_right_margin + PAD;
     y = lw->list.row_height * (item % lw->list.nrows)
         + lw->list.clip_top_margin + PAD + lw->list.yoffset;
     add = 1;
  } else {
     x = lw->list.col_width * (item % lw->list.ncols)
         + lw->list.clip_right_margin + PAD;
     y = lw->list.row_height * (item / lw->list.ncols)
         + lw->list.clip_top_margin + PAD + lw->list.yoffset;
     add = lw->list.ncols;
  }

  bottom = lw->list.clip_top_margin + lw->list.clip_height + PAD;

  old_yoffset = lw->list.yoffset;
  {
	int y1, y2;
        y1 = y;
        y2 = y + lw->list.row_height;
        if (!pageit && (0 <= y1) && (y2 < bottom)) return  (-old_yoffset);
        if (y1 < 0) {
	    while (y1 < 0) {
        	old_yoffset += lw->list.row_height; 
		y1 	    += lw->list.row_height;
		*top	    -= add;
	    }
        } else {
	   while (y2 > bottom) {
        	old_yoffset -= lw->list.row_height;
		y2 	    -= lw->list.row_height;
		y1          -= lw->list.row_height;
		*top	    += add;
	   }
        }
        while (pageit && (y1 - lw->list.row_height) > 0) {
        	old_yoffset -= lw->list.row_height;
		y1 	    -= lw->list.row_height;
		*top	    += add;
	}
  }
  return (-old_yoffset);
}

GetCurrentPos(w)
Widget w;
{
    MyListWidget lw = ( MyListWidget ) w;
    int item;
    int n;

    if (!XtIsRealized(w)) return 0; /* Just in case... */
    n = CvtToItem(w, 
		  lw->list.clip_right_margin + PAD , 
		  lw->list.clip_top_margin + PAD, 
		  &item);
    if (n == OUT_OF_RANGE) return 0;
    return item;
}

GetBottomPos(w)
Widget w;
{
    MyListWidget lw = ( MyListWidget ) w;
    int item;
    int n;

    if (!XtIsRealized(w)) return lw->list.nitems; /* Just in case... */
    n = CvtToItem(w, lw->list.clip_right_margin + 
		     lw->list.clip_width + 2*PAD , 
		     lw->list.clip_top_margin + lw->list.clip_height + 2*PAD,
		     &item);
    if (n == OUT_OF_RANGE) return lw->list.nitems;;
    return item;
}
