/* -*-C-*-
********************************************************************************
*
* File:         w_libXm.c
* RCS:          $Header: /users/npm/src/winterp/src-server/RCS/w_libXm.c,v 2.7 1994/06/06 15:40:57 npm Exp $
* Description:  Random XLISP Primitives and Methods for the Motif toolkit.
* Author:       Niels Mayer
* Created:      Fri Nov 24 00:36:30 1989
* Modified:     Sun Jun  5 14:48:14 1994 (Niels Mayer) npm@indeed
* Language:     C
* Package:      N/A
* Status:       X11r6 contrib release
*
* Copyright (C) 1994, Enterprise Integration Technologies Corp. and Niels Mayer.
* WINTERP 1.15-1.99, Copyright (c) 1993, Niels P. Mayer.
* WINTERP 1.0-1.14, Copyright (c) 1989-1992 Hewlett-Packard Co. and Niels Mayer.
* 
* Permission to use, copy, modify, distribute, and sell this software and its
* documentation for any purpose is hereby granted without fee, provided that
* the above copyright notice appear in all copies and that both that
* copyright notice and this permission notice appear in supporting
* documentation, and that the name of Enterprise Integration Technologies,
* Hewlett-Packard Company, or Niels Mayer not be used in advertising or
* publicity pertaining to distribution of the software without specific,
* written prior permission. Enterprise Integration Technologies, Hewlett-Packard
* Company, and Niels Mayer makes no representations about the suitability of
* this software for any purpose.  It is provided "as is" without express or
* implied warranty.
* 
* ENTERPRISE INTEGRATION TECHNOLOGIES, HEWLETT-PACKARD COMPANY AND NIELS MAYER
* DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING ALL IMPLIED
* WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL ENTERPRISE
* INTEGRATION TECHNOLOGIES, HEWLETT-PACKARD COMPANY OR NIELS MAYER BE LIABLE
* FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER
* RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF
* CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN
* CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
*
********************************************************************************
*/
static char rcs_identity[] = "@(#)$Header: /users/npm/src/winterp/src-server/RCS/w_libXm.c,v 2.7 1994/06/06 15:40:57 npm Exp $";

/*
*------------------------------------------------------------------------------
* See ./winterp/COPYRIGHT for information on contacting the authors.
* Please e-mail comments, modifications, questions, improvements and
* bugfixes to the WINTERP mailing list winterp@netcom.com. Please send 
* mailing list subscribe/unsubscribe notices to winterp-request@netcom.com .
* Post XLISP-specific questions/information to the USENET newsgroup
* comp.lang.lisp.x.
*------------------------------------------------------------------------------
*/

#include <stdio.h>
#include <Xm/Xm.h>
#include <X11/cursorfont.h>	/* defines XC_num_glyphs */
#include "winterp.h"

#ifdef WINTERP_MOTIF_11
/*
 * <limits.h> defines machine dependent limits on sizes of numbers, if your
 * machine doesn't have this, then your compiler doesn't conform to standards
 * XPG2, XPG3, POSIX.1, FIPS 151-1 and you should complain to the manufacturer.
 * 
 * If for some reason your system isn't standards-conforming, you may work
 * around this problem by using the following definitions (assuming 32 bit machine):
 * 
 * #define INT_MAX 2147483647
 */
#include <limits.h>
#endif				/* WINTERP_MOTIF_11 */

#ifdef WINTERP_XTANGO_WIDGET
extern void micro_sleep();	/* def'd in xtango/xtangotrans.c */
#else /* !defined(WINTERP_XTANGO_WIDGET) */
/* Xtango not included in WINTERP, so define our own micro_sleep()
   for use by :UPDATE_DISPLAY right here... */
#include <sys/types.h>
#include <sys/time.h>
static void micro_sleep(usec)
     int usec;
{
  struct timeval t;
  t.tv_sec = usec >> 20;
  t.tv_usec = usec & 0xfffff;
  if (t.tv_usec >= 1000000) {
    ++t.tv_sec;
    t.tv_usec -= 1000000;
  }
  select(0, 0, 0, 0, &t);
}
#endif /* WINTERP_XTANGO_WIDGET */

/*****************************************************************************
 * (send <widget> :UPDATE_DISPLAY [<sleep-microseconds>])
 *
 * (see XmUpdateDisplay(3X))
 * This function is useful for refreshing winterp's
 * Xwindows displays while inside a callback. Since the X display won't get
 * refreshed until you hit the XtNextEvent/XtDispatchEvent loop you cannot
 * normally change the display inside a callback. Thus, this function is
 * expecally useful for forcing a status message to be seen before a long
 * computation occurs within a callback, timeout, or workproc.
 *
 * The optional FIXNUM argument <sleep-microseconds> specifies the number of
 * microseconds for the WINTERP process to sleep after updating the
 * display. This will ensure that the updated display remains visible for a
 * given amount of time prior to any subsequent updates. The functionality
 * offered by this optional argument is a special WINTERP extension; the
 * standard OSF/Motif XmUpdateDisplay() call doesn't allow for this.
 ****************************************************************************/
LVAL Widget_Class_Method_UPDATE_DISPLAY()
{
  LVAL self;
  Widget widget_id;
  int usec_to_sleep = 0;

  widget_id = Wcls_Get_WIDGETOBJ_Argument_Returning_Validated_WidgetID(&self);

  /* get optional [<sleep-microseconds>] */
  if (moreargs()) {
    usec_to_sleep = (int) getfixnum(xlgafixnum());
  }

  xllastarg();

  XmUpdateDisplay(widget_id);

  if (usec_to_sleep > 0)
    micro_sleep(usec_to_sleep);

  return (self);
}


/******************************************************************************
 * (send <widget> :ADD_TAB_GROUP)
 * ==> returns <widget>
 *
 * This method adds <widget> to the list of tab groups associated with a 
 * particular widget hierarchy.
 *
 * void XmAddTabGroup(tabGroup)
 *      Widget tabGroup;
 ******************************************************************************/
LVAL Widget_Class_Method_ADD_TAB_GROUP()
{
  LVAL self;
  Widget widget_id;

  widget_id = Wcls_Get_WIDGETOBJ_Argument_Returning_Validated_WidgetID(&self);
  xllastarg();

  XmAddTabGroup(widget_id);
  return (self);
}


/******************************************************************************
 * (send <widget> :REMOVE_TAB_GROUP)
 * ==> returns <widget>
 *
 * This method removes <widget> from the list of tab groups associated with a 
 * particular widget hierarchy.
 *
 * void XmRemoveTabGroup (tabGroup)
 *      Widget tabGroup;
 ******************************************************************************/
LVAL Widget_Class_Method_REMOVE_TAB_GROUP()
{
  LVAL self;
  Widget widget_id;

  widget_id = Wcls_Get_WIDGETOBJ_Argument_Returning_Validated_WidgetID(&self);
  xllastarg();

  XmRemoveTabGroup(widget_id);
  return (self);
}

/******************************************************************************
 * (send <widget> :IS_PRIMITIVE)
 * ==> returns T if <widget> is a motif primitive widget, else NIL.
 *
 * #define XmIsPrimitive(w)	XtIsSubclass(w, xmPrimitiveWidgetClass)
 ******************************************************************************/
LVAL Widget_Class_Method_IS_PRIMITIVE()
{
  extern LVAL true;
  LVAL self;
  Widget widget_id;

  widget_id = Wcls_Get_WIDGETOBJ_Argument_Returning_Validated_WidgetID(&self);
  xllastarg();

  return (XmIsPrimitive(widget_id) ? true : NIL);
}


/******************************************************************************
 * (send <widget> :IS_GADGET)
 * ==> returns T if <widget> is a motif gadget, else NIL.
 *
 * #define XmIsGadget(w)	XtIsSubclass(w, xmGadgetClass)
 ******************************************************************************/
LVAL Widget_Class_Method_IS_GADGET()
{
  extern LVAL true;
  LVAL self;
  Widget widget_id;

  widget_id = Wcls_Get_WIDGETOBJ_Argument_Returning_Validated_WidgetID(&self);
  xllastarg();

  return (XmIsGadget(widget_id) ? true : NIL);
}


/******************************************************************************
 * (send <widget> :IS_MANAGER)
 * ==> returns T if <widget> is a motif manager widget, else NIL.
 *
 * #define XmIsManager(w)	XtIsSubclass(w, xmManagerWidgetClass)
 ******************************************************************************/
LVAL Widget_Class_Method_IS_MANAGER()
{
  extern LVAL true;
  LVAL self;
  Widget widget_id;

  widget_id = Wcls_Get_WIDGETOBJ_Argument_Returning_Validated_WidgetID(&self);
  xllastarg();

  return (XmIsManager(widget_id) ? true : NIL);
}


#ifdef WINTERP_MOTIF_11
/******************************************************************************
 * (XM_GET_COLORS <background-pixel>)
 * returns an array <background> <foreground> <top_shadow> <bot_shadow> <select>
 * pixels.
******************************************************************************/
LVAL Wxm_Prim_XM_GET_COLORS()
{
  extern Screen* screen;	/* global in winterp.c */
  extern Colormap colormap;	/* global in winterp.c */
  Pixel foreground, top_shadow, bottom_shadow, select;
  LVAL result, background = xlga_pixel();
  xllastarg();

  XmGetColors(screen, colormap, get_xpixel(background), &foreground, &top_shadow, &bottom_shadow, &select);

  xlsave1(result);
  result = newvector((unsigned) 5);
  setelement(result, 0, background);
  setelement(result, 1, cv_pixel(foreground));
  setelement(result, 2, cv_pixel(top_shadow));
  setelement(result, 3, cv_pixel(bottom_shadow));
  setelement(result, 4, cv_pixel(select));

  xlpop();
  return (result);
}
#endif


#ifdef WINTERP_MOTIF_11
/******************************************************************************
 * (XM_SET_FONT_UNITS <horiz-val> <vert-val>)
 * 	==> Returns NIL.
 *
 * <horiz-val> and <vert-val> are nonngegative FIXNUMS
 *
 * Note: when WINTERP allows for multiple displays, this function will
 * end up changing arg sequences to accomodate a <display> arg.
 *
 *-----------------------------------------------------------------------------
 *           void XmSetFontUnits (display, h_value, v_value)
 *                Display   * display;
 *                int       h_value;
 *                int       v_value;
 ******************************************************************************/
LVAL Wxm_Prim_XM_SET_FONT_UNITS()
{
  extern Display* display;	/* winterp.c */
  LVAL lval_h_value = xlgafixnum();
  LVAL lval_v_value = xlgafixnum();
  long h_value = (long) getfixnum(lval_h_value);
  long v_value = (long) getfixnum(lval_v_value);
  xllastarg();

  if ((h_value < 0L) || (h_value > (long) INT_MAX))
    xlerror("Font unit specification out of range", lval_h_value);
  if ((v_value < 0L) || (v_value > (long) INT_MAX))
    xlerror("Font unit specification out of range", lval_v_value);

  XmSetFontUnits(display, (int) h_value, (int) v_value);

  return (NIL);
}
#endif				/* WINTERP_MOTIF_11 */


#ifdef WINTERP_MOTIF_11
/******************************************************************************
 * (XM_TRACKING_LOCATE <widget> <fontcursor-FIXNUM> [<confine-to-p>]
 *	==> Returns a WIDGETOBJ or NIL if the window of the buttonpress is
 *	    not a widget.
 *
 * "XmTrackingLocate provides a modal interface for selection of
 * a component.  It is intended to support context help.  The
 * function grabs the pointer and returns the widget in which a
 * button press occurs." (taken from XmTrackingLocate.3X man-page).
 * 
 * <widget> is the widget "to use as the basis of the modal interaction
 *
 * <fontcursor-FIXNUM> specifies the shape of the cursor for this interaction.
 * the value of this FIXNUM specifies a font whose name/value is defined in
 * /usr/include/X11/cursorfont.h.
 *
 * If the last arg supplied, <confine-to-p>, is non-NIL, then the interaction
 * will be confined to <widget>. If this arg is not supplied, <confine-to-p>
 * is treated as NIL.
 *----------------------------------------------------------------------------
 * Widget XmTrackingLocate (widget, cursor, confine_to)
 *      Widget    widget;
 *      Cursor    cursor;
 *      Boolean   confine_to;
 ******************************************************************************/
LVAL Wxm_Prim_XM_TRACKING_LOCATE()
{
  LVAL lval_widget;
  Widget widget_id;
  Cursor cursor;
  LVAL lval_fontcursor_shape;
  long fontcursor_shape;
  Boolean confineto_p;

  /* get <widget> */
  widget_id = Wcls_Get_WIDGETOBJ_Argument_Returning_Validated_WidgetID(&lval_widget);

  /* get <fontcursor-FIXNUM> */
  lval_fontcursor_shape = xlgafixnum();
  
  /* get optional [<confine-to-p>] */
  if (moreargs())
    confineto_p = ((nextarg() != NIL) ? TRUE : FALSE);
  else
    confineto_p = FALSE;
    
  xllastarg();

  /* convert <fontcursor-FIXNUM> to a cursor */
  fontcursor_shape = (long) getfixnum(lval_fontcursor_shape);
  if ((fontcursor_shape & 1L) != 0) /* <fontcursor-FIXNUM> can't be an odd number ... */
    xlerror("Font-Cursor fixnum must be an even number", lval_fontcursor_shape);
  if ((fontcursor_shape < 0L) || (fontcursor_shape > (long) (XC_num_glyphs - 2)))
    xlerror("Font-Cursor fixnum out of range", lval_fontcursor_shape);
  cursor = XCreateFontCursor(XtDisplayOfObject(widget_id),
			     (unsigned int) fontcursor_shape);
  lval_widget =
    Wcls_WidgetID_To_WIDGETOBJ(XmTrackingLocate(widget_id, cursor, confineto_p));

  XFreeCursor(XtDisplay(widget_id), cursor);

  return (lval_widget);
}
#endif				/* WINTERP_MOTIF_11 */


#ifdef WINTERP_MOTIF_11
/******************************************************************************
 * (XM_CONVERT_UNITS <widget> <orientation> <from_unit_type> <from_value> <to_unit_type>)
 * 	==> Returns the converted value, a FIXNUM.
 *
 * "XmConvertUnits - a function that converts a value in one unit type to
 *  another unit type." (quoted from XmConvertUnits.3X manual page).
 *
 * <widget> is a WIDGETOBJ -- "Specifies the widget for which the data is to be
 * converted."
 *
 * <orientation> is a keyword symbol, either :HORIZONTAL or :VERTICAL
 * "Specifies whether the converter uses the horizontal or vertical screen
 *  resolution when performing the conversions."
 *
 * <from_unit_type> and <to_unit_type> are keyword symbols, either
 * :PIXELS, :100TH_MILLIMETERS, :1000TH_INCHES, :100TH_POINTS, :100TH_FONT_UNITS
 *
 * <from_value> is a FIXNUM.
 *
 * Note that functions XmCvtFromHorizontalPixels(), XmCvtFromVerticalPixels(),
 * XmCvtToHorizontalPixels(), XmCvtToVerticalPixels() have not been interfaced
 * since their functionality can be achieved through XmConvertUnits().
 *
 *----------------------------------------------------------------------------
 * int XmConvertUnits (widget, orientation, from_unit_type,
 *                     from_value, to_unit_type)
 *	Widget    widget;
 *	int       orientation;
 *	int       from_unit_type;
 *	int       from_value;
 *	int       to_unit_type;
 ******************************************************************************/
static LVAL k_VERTICAL, k_HORIZONTAL, k_PIXELS, k_100TH_MILLIMETERS,
  k_1000TH_INCHES, k_100TH_POINTS, k_100TH_FONT_UNITS; /* init'd in Wxm_Init() */
LVAL Wxm_Prim_XM_CONVERT_UNITS()
{
  LVAL lval_widget;
  Widget widget_id;
  LVAL lval_orientation, lval_from_unit_type, lval_to_unit_type;
  int orientation, from_unit_type, to_unit_type, from_value;

  /* get <widget> */
  widget_id = Wcls_Get_WIDGETOBJ_Argument_Returning_Validated_WidgetID(&lval_widget);

  /* get  <orientation> */
  lval_orientation = xlgetarg();
  if (lval_orientation == k_VERTICAL)
    orientation = XmVERTICAL;
  else if (lval_orientation == k_HORIZONTAL)
    orientation = XmHORIZONTAL;
  else
    xlerror("Invalid orientation value, expected :VERTICAL or :HORIZONTAL",
	    lval_orientation);

  /* get <from_unit_type> */
  lval_from_unit_type = xlgetarg();
  if (lval_from_unit_type == k_PIXELS)
    from_unit_type = XmPIXELS;
  else if (lval_from_unit_type == k_100TH_MILLIMETERS)
    from_unit_type = Xm100TH_MILLIMETERS;
  else if (lval_from_unit_type == k_1000TH_INCHES)
    from_unit_type = Xm1000TH_INCHES;
  else if (lval_from_unit_type == k_100TH_POINTS)
    from_unit_type = Xm100TH_POINTS;
  else if (lval_from_unit_type == k_100TH_FONT_UNITS)
    from_unit_type = Xm100TH_FONT_UNITS;
  else 
    xlerror("Invalid <from_unit_type> keyword, expected one of [:PIXELS, :100TH_MILLIMETERS, :1000TH_INCHES, :100TH_POINTS, :100TH_FONT_UNITS].",
	    lval_from_unit_type);

  /* get <from_value> */
  from_value = (long) getfixnum(xlgafixnum());

  /* get <to_unit_type> */
  lval_to_unit_type = xlgetarg();
  if (lval_to_unit_type == k_PIXELS)
    to_unit_type = XmPIXELS;
  else if (lval_to_unit_type == k_100TH_MILLIMETERS)
    to_unit_type = Xm100TH_MILLIMETERS;
  else if (lval_to_unit_type == k_1000TH_INCHES)
    to_unit_type = Xm1000TH_INCHES;
  else if (lval_to_unit_type == k_100TH_POINTS)
    to_unit_type = Xm100TH_POINTS;
  else if (lval_to_unit_type == k_100TH_FONT_UNITS)
    to_unit_type = Xm100TH_FONT_UNITS;
  else 
    xlerror("Invalid <to_unit_type> keyword, expected one of [:PIXELS, :100TH_MILLIMETERS, :1000TH_INCHES, :100TH_POINTS, :100TH_FONT_UNITS].",
	    lval_to_unit_type);

  xllastarg();

  return (cvfixnum((FIXTYPE) XmConvertUnits(widget_id,
					    orientation,
					    from_unit_type,
					    from_value,
					    to_unit_type)));
}
#endif				/* WINTERP_MOTIF_11 */


#ifdef WINTERP_MOTIF_11
/******************************************************************************
 * (send <widget> :PROCESS_TRAVERSAL <direction>)
 *	==> "Returns T if the setting succeeded.  Returns NIL if the
 *           keyboard focus policy is not :EXPLICIT, if there are no
 *           traversable items, or if the call to the routine has invalid
 *           parameters."
 *
 * "a function that determines which component receives keyboard events
 *  when a widget has the focus." (from XmProcessTraversal.3X manual page)
 *
 * <direction> is a keyword symbol, one of the following:
 * :TRAVERSE_CURRENT
 *	"Finds the hierarchy and the tab group that contain widget.
 * 	If this tab group is not the active tab group, makes it the
 * 	active tab group.  If widget is an item in the active tab
 * 	group, makes it the active item.  If widget is the active
 * 	tab group, makes the first traversable item in the tab group
 * 	the active item."
 * :TRAVERSE_DOWN
 *     "Finds the hierarchy that contains widget.  Finds the active
 * 	item in the active tab group and makes the item below it the
 * 	active item.  If there is no item below, wraps."
 * :TRAVERSE_HOME
 * 	"Finds the hierarchy that contains widget.  Finds the active item
 * 	in the active tab group and makes the first traversable item in
 * 	the tab group the active item."
 * :TRAVERSE_LEFT
 *      "Finds the hierarchy that contains widget.  Finds the active item
 * 	in the active tab group and makes the item to the left the active
 * 	item.  If there is no item to the left, wraps."
 * :TRAVERSE_NEXT
 *      "Finds the hierarchy that contains widget.  Finds the active item
 *      in the active tab group and makes the next item the active item."
 * :TRAVERSE_NEXT_TAB_GROUP
 *      "Finds the hierarchy that contains widget.  Finds the active tab
 *      group (if any) and makes the next tab group the active tab group
 *	in the hierarchy."
 * :TRAVERSE_PREV
 *      "Finds the hierarchy that contains widget.  Finds the active item
 *      in the active tab group and makes the previous item the active item."
 * :TRAVERSE_PREV_TAB_GROUP
 *      "Finds the hierarchy that contains widget.  Finds the active tab
 *      group (if any) and makes the previous tab group the active tab
 *      group in the hierarchy.
 * :TRAVERSE_RIGHT
 *     "Finds the hierarchy that contains widget.  Finds the active item
 *     in the active tab group and makes the item to the right the active
 *     item.  If there is no item to the right, wraps."
 * :TRAVERSE_UP
 *     "Finds the hierarchy that contains widget. Finds the active item
 *	in the active tab group and makes the item above it the active item.
 *      If there is no item above, wraps."
 *----------------------------------------------------------------------------
 *          Boolean XmProcessTraversal (widget, direction)
 *               Widget    widget;
 *               int       direction;
 ******************************************************************************/
static LVAL k_TRAVERSE_CURRENT, k_TRAVERSE_NEXT, k_TRAVERSE_PREV, k_TRAVERSE_HOME,
  k_TRAVERSE_NEXT_TAB_GROUP, k_TRAVERSE_PREV_TAB_GROUP, k_TRAVERSE_UP, k_TRAVERSE_DOWN,
  k_TRAVERSE_LEFT, k_TRAVERSE_RIGHT; /* init'd in Wxm_Init() */
LVAL Widget_Class_Method_PROCESS_TRAVERSAL()
{
  extern LVAL true;
  LVAL self;
  Widget widget_id;
  int direction;
  LVAL lval_direction;

  /* get <widget> */
  widget_id = Wcls_Get_WIDGETOBJ_Argument_Returning_Validated_WidgetID(&self);

  /* get <direction> */
  lval_direction = xlgetarg();
  if (lval_direction == k_TRAVERSE_CURRENT)
    direction = XmTRAVERSE_CURRENT;
  else if (lval_direction == k_TRAVERSE_NEXT)
    direction = XmTRAVERSE_NEXT;
  else if (lval_direction == k_TRAVERSE_PREV)
    direction = XmTRAVERSE_PREV;
  else if (lval_direction == k_TRAVERSE_HOME)
    direction = XmTRAVERSE_HOME;
  else if (lval_direction == k_TRAVERSE_NEXT_TAB_GROUP)
    direction = XmTRAVERSE_NEXT_TAB_GROUP;
  else if (lval_direction == k_TRAVERSE_PREV_TAB_GROUP)
    direction = XmTRAVERSE_PREV_TAB_GROUP;
  else if (lval_direction == k_TRAVERSE_UP)
    direction = XmTRAVERSE_UP;
  else if (lval_direction == k_TRAVERSE_DOWN)
    direction = XmTRAVERSE_DOWN;
  else if (lval_direction == k_TRAVERSE_LEFT)
    direction = XmTRAVERSE_LEFT;
  else if (lval_direction == k_TRAVERSE_RIGHT)
    direction = XmTRAVERSE_RIGHT;
  else 
    xlerror("Invalid traversal direction keyword, expected one of [:TRAVERSE_CURRENT, :TRAVERSE_NEXT, :TRAVERSE_PREV, :TRAVERSE_HOME, :TRAVERSE_NEXT_TAB_GROUP, :TRAVERSE_PREV_TAB_GROUP, :TRAVERSE_UP, :TRAVERSE_DOWN, :TRAVERSE_LEFT, :TRAVERSE_RIGHT].",
	    lval_direction);

  xllastarg();

  return (XmProcessTraversal(widget_id, direction) ? true : NIL);
}
#endif				/* WINTERP_MOTIF_11 */


/******************************************************************************
 * (XM_SET_MENU_CURSOR <fontcursor>)
 *	==> Returns NIL.
 *
 *  "XmSetMenuCursor programmatically modifies the menu cursor for a client;
 *  after the cursor has been created by the client, this function registers
 *  the cursor with the menu system.  After calling this function, the
 *  specified cursor is displayed whenever this client displays a Motif
 *  menu on the indicated display.  The client can then specify different
 *  cursors on different displays." (from XmSetMenuCursor.3X manual page).
 *
 * <fontcursor>> specifies the shape of the cursor for this interaction.
 * The value of this FIXNUM specifies a font whose name/value is defined in
 * /usr/include/X11/cursorfont.h.
 *
 * Note: when WINTERP allows for multiple displays, this function will
 * end up changing arg sequences to accomodate a <display> arg.
 *----------------------------------------------------------------------------
 *           void XmSetMenuCursor (display, cursorId)
 *                Display   * display;
 *                Cursor    cursorId;
 ******************************************************************************/
static Cursor menu_cursor = (Cursor) NULL;
LVAL Wxm_Prim_XM_SET_MENU_CURSOR()
{
  extern Display* display;	/* from winterp.c -- this'll have to change when WINTERP gets updated for multi-display. */
  LVAL lval_fontcursor_shape;
  long fontcursor_shape;

  /* get <fontcursor> */
  lval_fontcursor_shape = xlgafixnum();
  
  xllastarg();

  /* convert <fontcursor> to a cursor */
  fontcursor_shape = (long) getfixnum(lval_fontcursor_shape);
  if ((fontcursor_shape & 1L) != 0) /* <fontcursor-FIXNUM> can't be an odd number ... */
    xlerror("Font-Cursor fixnum must be an even number", lval_fontcursor_shape);
  if ((fontcursor_shape < 0L) || (fontcursor_shape > (long) (XC_num_glyphs - 2)))
    xlerror("Font-Cursor fixnum out of range", lval_fontcursor_shape);

  /*
   * Typically, one would want to call XFreeCursor() after creating a Cursor
   * instance with XCreateFontCursor(), however, since the cursor is long-lasting
   * it is not clear when you can really free it. Here, I've decided that the
   * only safe thing to do is free the menu_cursor set previously by this 
   * routine. It isn't clear that anything inside Motif actually takes care
   * of freeing the cursor passed in, nor is it clear that setting the new cursor
   * will free up the previously defined cursor (e.g. the default cursor)...
   */
  if (menu_cursor != (Cursor) NULL) /* menu cursor set here previously */
    XFreeCursor(display, menu_cursor); /* cleanup from XCreateFontCursor() */

  menu_cursor = XCreateFontCursor(display, (unsigned int) fontcursor_shape);

  XmSetMenuCursor(display, menu_cursor);

  return (NIL);
}


#ifdef WINTERP_MOTIF_12  /* this was introduced in Motif 1.1, but we punt on adding this until Motif 1.2 because XmGetDestination() missing in HPUX 8.0 UEDK Motif 1.1, however, it's there in standard OSFMotif 1.1 */
/******************************************************************************
 * (XM_GET_DESTINATION)
 * 	==> Returns a WIDGETOBJ, "the widget to be used as the current
 *	    destination for quick paste and certain clipboard operations."
 *	    Returns NIL if "there is no current destination".
 *
 * See XmGetDestination.3X manual page for details.
 *-----------------------------------------------------------------------------
 *           Widget XmGetDestination (display)
 *                Display   *display;
 ******************************************************************************/
LVAL Wxm_Prim_XM_GET_DESTINATION()
{
  extern Display* display;	/* winterp.c */
  xllastarg();
  return (Wcls_WidgetID_To_WIDGETOBJ(XmGetDestination(display)));
}
#endif /* WINTERP_MOTIF_12 */


#ifdef WINTERP_MOTIF_12
/******************************************************************************
 * (send <widget> :CHANGE_COLOR <bg-color>)
 *	==> Returns <widget>.
 *
 *	<widget> must be a widget; an XLISP error gets signalled if a gadget 
 *      is used.
 *
 *	<bg-color> is a STRING or PIXEL value representing the desired color
 *	for <widget>'s background. If a STRING value is given, it will be
 *	converted to a pixel value based on color names given in
 *	/usr/lib/X11/rgb.txt; alternately, one may specify the color
 *	specification in hexadecimal as "#RRGGBB"...
 *
 * Given a new background color <bg-color>, method :CHANGE_COLOR recalculates
 * the foreground, select, and shadow colors based on the new background color
 * and sets the corresponding resources for the  widget. The new colors are
 * based on Motif's default color calculation procedures.
 *-----------------------------------------------------------------------------
 * void XmChangeColor (widget, background)
 *      Widget    widget;
 *      Pixel     background;
 ******************************************************************************/
LVAL Widget_Class_Method_CHANGE_COLOR()
{
  extern Display* display;	/* global in winterp.c */
  extern Colormap colormap;	/* global in winterp.c */
  XColor screenColor;
  LVAL   self, bg_lval;
  Widget widget_id;

  widget_id = Wcls_Get_WIDGETOBJ_Argument_Returning_Validated_WidgetID(&self);
  Wxt_Error_If_Gadget(widget_id, self); /* Motif 1.2 core-dumps when XmChangeColor() called on gadget! */
  bg_lval = xlgetarg();
  xllastarg();

  if (pixel_p(bg_lval))
    XmChangeColor(widget_id, get_xpixel(bg_lval));
  else if (stringp(bg_lval)) {
    if (!XParseColor(display, colormap, getstring(bg_lval), &screenColor))
      xlerror("XParseColor() couldn't parse color specification.", bg_lval);
    if (!XAllocColor(display, colormap, &screenColor))
      xlerror("XAllocColor() couldn't allocate specified color.", bg_lval);
    XmChangeColor(widget_id, screenColor.pixel);
  }
  else
    xlbadtype(bg_lval);

  return (self);
}
#endif /* WINTERP_MOTIF_12 */


/******************************************************************************
 *
 ******************************************************************************/
Wxm_Init()
{
#ifdef WINTERP_MOTIF_11
  /* these are used by Wxm_Prim_XM_CONVERT_UNITS(), which is Motif 1.1 only */
  k_VERTICAL		= xlenter(":VERTICAL");
  k_HORIZONTAL		= xlenter(":HORIZONTAL");
  k_PIXELS		= xlenter(":PIXELS");
  k_100TH_MILLIMETERS	= xlenter(":100TH_MILLIMETERS");
  k_1000TH_INCHES	= xlenter(":1000TH_INCHES");
  k_100TH_POINTS	= xlenter(":100TH_POINTS");
  k_100TH_FONT_UNITS	= xlenter(":100TH_FONT_UNITS");

  /* these are used by Widget_Class_Method_PROCESS_TRAVERSAL(), which is Motif 1.1 only */
  k_TRAVERSE_CURRENT	= xlenter(":TRAVERSE_CURRENT");
  k_TRAVERSE_NEXT	= xlenter(":TRAVERSE_NEXT");
  k_TRAVERSE_PREV	= xlenter(":TRAVERSE_PREV");
  k_TRAVERSE_HOME	= xlenter(":TRAVERSE_HOME");
  k_TRAVERSE_NEXT_TAB_GROUP = xlenter(":TRAVERSE_NEXT_TAB_GROUP");
  k_TRAVERSE_PREV_TAB_GROUP = xlenter(":TRAVERSE_PREV_TAB_GROUP");
  k_TRAVERSE_UP		= xlenter(":TRAVERSE_UP");
  k_TRAVERSE_DOWN	= xlenter(":TRAVERSE_DOWN");
  k_TRAVERSE_LEFT	= xlenter(":TRAVERSE_LEFT");
  k_TRAVERSE_RIGHT	= xlenter(":TRAVERSE_RIGHT");
#endif				/* WINTERP_MOTIF_11 */
}
