/* -*-C-*-
********************************************************************************
*
* File:         w_utils.c
* RCS:          $Header: /users/npm/src/winterp/src-server/RCS/w_utils.c,v 2.9 1994/06/06 15:40:52 npm Exp $
* Description:  Various X Functionality
* Author:       Niels Mayer
* Created:      Fri Sep 29 01:24:38 1989
* Modified:     Sun Jun  5 14:54:15 1994 (Niels Mayer) npm@indeed
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
static char rcs_identity[] = "@(#)$Header: /users/npm/src/winterp/src-server/RCS/w_utils.c,v 2.9 1994/06/06 15:40:52 npm Exp $";

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
#include <X11/cursorfont.h>	/* defines XC_crosshair */
#include "winterp.h"


/******************************************************************************
 * (GET_EVENT_COORDS <xevent>)
 * 	--> Returns (<x-fixnum> . <y-fixnum>) or signals error
 * <xevent> is an XEvent* as returned via symbol CALLBACK_XEVENT
 * (bound via :ADD_CALLBACK or :SET_CALLBACK method), ACTION_XEVENT (bound in
 * in the Lisp(...) ActionProc from a translation table), and EVHANDLER_XEVENT
 * (bound via :ADD_EVHANDLER or :SET_EVHANDLER method).
******************************************************************************/
LVAL Wut_Prim_GET_EVENT_COORDS()
{
  int mousex, mousey;
  XEvent* event;
  LVAL lval_x, lval_y, lval_result;
  LVAL lval_event = xlga_xevent(); /* get <xevent> */
  xllastarg();

  /* protect some pointers */
  xlstkcheck(3);
  xlsave(lval_x);
  xlsave(lval_y);
  xlsave(lval_result);

  event = get_xevent(lval_event);
  switch (event->type) {	/* better safe than sorry -- don't know what kind of XEvent will get passed... */
  case KeyPress:
  case KeyRelease:
    mousex = event->xkey.x;
    mousey = event->xkey.y;
    break;
  case ButtonPress:
  case ButtonRelease:
    mousex = event->xbutton.x;
    mousey = event->xbutton.y;
    break;
  case MotionNotify:
    mousex = event->xmotion.x;
    mousey = event->xmotion.y;
    break;
  case EnterNotify:
  case LeaveNotify:
    mousex = event->xcrossing.x;
    mousey = event->xcrossing.y;
    break;
  default:
    xlerror("GET_EVENT_COORDS requires KeyPress, KeyRelease, ButtonPress, ButtonRelease, MotionNotify, EnterNotify, or LeaveNotify event",
	    lval_event);
    break;
  }

  lval_x = cvfixnum((FIXTYPE) mousex);
  lval_y = cvfixnum((FIXTYPE) mousey);
  lval_result = cons(lval_x, lval_y);

  /* restore the stack */
  xlpopn(3);

  return (lval_result);
}


/******************************************************************************
 ** (GET_MOUSE_LOCATION)
 ** 
 ** [ NEW ]:  it returns a dotted pair ... (root_x . root_y)
 **
 ** Primitive written by Richard Hess, Consilium, uunet!cimshop!rhess.
 ** Fixes applied by Niels Mayer...
 ******************************************************************************/
LVAL Wut_Prim_GET_MOUSE_LOCATION()
{
  extern Display* display;	/* global in winterp.c */
  extern Window   root_win;	/* global in winterp.c */
  LVAL            lval_result, lval_x, lval_y;
  int             x_rtn, y_rtn;
  Window          junk1, junk2;
  int             junk3, junk4;
  unsigned int    junk5;
  
  /* protect some pointers -- added by NPM */
  xlstkcheck(3);
  xlsave(lval_x);
  xlsave(lval_y);
  xlsave(lval_result);

  if (!XQueryPointer(display, root_win, &junk1, &junk2,
		     &x_rtn, &y_rtn, &junk3, &junk4, &junk5))
    xlfail("XQueryPointer() failed...");
  lval_x = cvfixnum((FIXTYPE) x_rtn);
  lval_y = cvfixnum((FIXTYPE) y_rtn);
  lval_result = cons(lval_x, lval_y);

  /* restore the stack */
  xlpopn(3);

  return (lval_result);
}


/******************************************************************************
 * (X_ALLOC_COLOR <color>)
 * where <color> is a string, either a colorname from /usr/lib/X11/rgb.txt
 * or a hexadecimal color specification "#RRGGBB".
 * it returns a Pixel-value for the color.
******************************************************************************/
LVAL Wut_Prim_X_ALLOC_COLOR()
{
  extern Display* display;	/* global in winterp.c */
  extern Colormap colormap;	/* global in winterp.c */
  XColor        screenColor;
  LVAL          str_color;

  str_color = xlgastring();
  xllastarg();
  
  if (!XParseColor(display, colormap, getstring(str_color), &screenColor))
    xlerror("XParseColor() couldn't parse color specification.", str_color);
  if (!XAllocColor(display, colormap, &screenColor))
    xlerror("XAllocColor() couldn't allocate specified color.", str_color);
  return (cv_pixel(screenColor.pixel));
}


/******************************************************************************
 * (X_STORE_COLOR <pixel> <color>)    [nicer would be (send <pixel> :store <color>)]
 * where <color> is a string, either a colorname from /usr/lib/X11/rgb.txt
 * or a hexadecimal color specification "#RRGGBB".
 * it returns a Pixel-value for the color.
******************************************************************************/
LVAL Wut_Prim_X_STORE_COLOR()
{
  extern Display* display;	/* global in winterp.c */
  extern Colormap colormap;	/* global in winterp.c */
  XColor        screenColor;
  LVAL          str_color;
  LVAL		lval_pixel;

  lval_pixel = xlga_pixel();
  str_color = xlgastring();
  xllastarg();
  
  screenColor.pixel = get_xpixel(lval_pixel);
  if (!XParseColor(display, colormap, getstring(str_color), &screenColor))
    xlerror("XParseColor() couldn't parse color specification.", str_color);
  if (!XStoreColor(display, colormap, &screenColor))
    xlerror("XStoreColor() couldn't allocate specified color.", str_color);
  return (lval_pixel);
}


/******************************************************************************
 * (X_ALLOC_N_COLOR_CELLS_NO_PLANES <num-cells>)
 * returns an array of <num-cells> <pixel-objects> see Oliver Jones, p. 278
******************************************************************************/
LVAL Wut_Prim_X_ALLOC_N_COLOR_CELLS_NO_PLANES()
{
  extern Display* display;	/* global in winterp.c */
  extern Colormap colormap;	/* global in winterp.c */
  Pixel*        pixels;
  int		i, num_cells;
  LVAL		result;

  num_cells = (int) getfixnum(xlgafixnum());
  xllastarg();
  if (num_cells <= 0)
    return (NIL);
  
  pixels = (Pixel*) XtMalloc((unsigned) (num_cells * sizeof(Pixel)));
  XAllocColorCells(display, colormap, FALSE, NULL, 0, pixels, num_cells);

  xlsave1(result);
  result = newvector((unsigned) num_cells);
  for (i = 0; i < num_cells; i++)
    setelement(result, i, cv_pixel(pixels[i]));
  xlpop();
  XtFree((char*) pixels);
  return (result);
}


/******************************************************************************
 * (X_BELL [<volume>])
 * 	--> RETURNS NIL
 *
 * <volume> is an optional FIXNUM argument ranging from -100 to 100. If the
 * <volume> arg is not givem, it defaults to '0'. Note that for most cases
 * '0' is a reasonable value, as it will allow the user to set up the X Window
 * system to operate silently by setting the base bell volume negative via
 * the 'xset' unix command.
******************************************************************************/
LVAL Wut_Prim_X_BELL()
{
  extern Display* display;	/* global in winterp.c */
  int percent = 0;		/* volume percentage [-100 100] */

  if (moreargs())
    percent = Get_Int_Argument(-100L, 100L);
  xllastarg();

  XBell(display, percent);

  return (NIL);
}


/******************************************************************************
 * (GET_MOUSED_WIDGET)
 * evaluating this function will change the cursor to a crossbar, indicating
 * that the user is to 'click' the mouse to designate an object on the screen.
 * If the user clicks on a visual item within WINTERP, this fucntion will
 * return the WIDGETOBJ associated with the visual item. 
 ******************************************************************************/
LVAL Wut_Prim_GET_MOUSED_WIDGET()
{
  extern Display* display;	/* global in winterp.c */
  extern Window   root_win;	/* global in winterp.c */
#ifdef _NO_PROTO		/* <Xm/Xm.h> or Makefile says we have ANSI C Fn. Prototypes */
  extern XmGadget _XmInputInGadget(); /* from XmP.h, def'd in Xm/GadetUtils.c */
#else  /* !defined(_NO_PROTO) ==> ANSI... */
  extern XmGadget _XmInputInGadget(Widget cw, register int x, register int y); /* from XmP.h, def'd in Xm/GadetUtils.c */
#endif /* _NO_PROTO */
  Cursor	  cursor = XCreateFontCursor(display, XC_crosshair);
  Window          parent_win, cur_win, child_win;
  int             win_x, win_y;
  Widget          widget_id, gadget_id;
  XEvent	  event;
  Bool            xtc_ok;
  LVAL		  lval_widget;

  xllastarg();

  /* NOTE: winterp 1.13 used to use 'XtLastTimestampProcessed(display)' in place of
   * 'CurrentTime' below, but that occasionally caused XGrabPointer() to return
   * the result 'GrabInvalidTime'. When I used 'CurrentTime' I no longer got
   * such errors.
   *
   * Note that I never saw these errors on HPUX7.0/X11r4 server, but did see
   * the error when running against an HPUX8.0/X11r4 server, and also saw the
   * problem running on an Apollo Domain X server. 
   *
   * The problem would only occur if WINTERP hadn't processed any X events for
   * a while (e.g. all windows obscured and/or iconified) and 'GET_MOUSED_WIDGET'
   * was sent to WINTERP over the eval-server. I guess what was happening is that
   * 'XtLastTimestampProcessed()' was returning the last time stamp corresponding
   * to the last X event processed by WINTERP. In the case of the failure mode
   * mentioned above, I guess the time stamp was ancient...
   *
   * Note that WINTERP < 1.13 used 'CurrentTime' -- I switched to XtLastTimestampProcessed()
   * after reading the following bogosity in the Asente&Swick's "X Window System Toolkit":
   *	"In most circumstances where a programmer would be tempted to pass the
   *	symbolic constant CurrentyTime to a preocedure, it is more correct and
   *	therefore preferable to use XtLastTimestampProcessed(). This will make the
   *	user less likely to encounter difficulties when running the application across a
   *	network with delays" (p.436).
   *
   * -- NPM 7/7/92.
   * PS: If this ends up needing to be changed, be sure to update
   *     xtango/xtangoxt.c:{TANGOinput_image(),TANGOinput_coord()}
   *     since that uses similar code employing XGrabPointer(...CurrentTime...)
   */

   /*
    * Remove any spurious button presses from the event queue before beginning
    * grab. This can prevent problems in the following sequence of occurances:
    * (1) Time consuming operation occurs during which an extra button press
    * is enqueued by the user (by accident).
    * (2) :input_coord/:input_widget/get_moused_widget is called, pointer is grabbed,
    *     cursor is turned to x-hair.
    * (3) Something inexplicably bad happens, which hangs XWindowEvent()
    *     looking for an event that doesn't come...
    * The code below is used in TANGOinput_image(), TANGOinput_coord() and
    * Wut_Prim_GET_MOUSED_WIDGET(). Be sure to update those if you change this...
    */
   XSync(display, 0);
   while (XCheckMaskEvent(display,
			  ButtonPressMask | ButtonReleaseMask |
			  ButtonMotionMask | PointerMotionMask |
			  KeyPressMask,
			  &event)) { /* discard above listed event-types */ }

  /* Confine mouse pointer to the root window -- get widget from visible shell window */
  if (GrabSuccess != XGrabPointer(display, root_win, False,
				  ButtonPressMask|ButtonReleaseMask,
				  GrabModeAsync, GrabModeAsync, root_win, cursor,
				  CurrentTime))
    {
      XFreeCursor(display, cursor); /* cleanup from XCreateFontCursor() */
      xlfail("GET_MOUSED_WIDGET -- couldn't grab pointer (XGrabPointer() failed).");
    }
  
  XWindowEvent(display, root_win, ButtonPressMask, &event); /* remove the buttonpress from the queue */
  XWindowEvent(display, root_win, ButtonReleaseMask, &event); /* get the buttonrelease event */
  XUngrabPointer(display, event.xbutton.time);
  XFlush(display);
  XFreeCursor(display, cursor); /* cleanup from XCreateFontCursor() */

  if (!event.xbutton.subwindow) {
    xlfail("GET_MOUSED_WIDGET aborted -- you clicked on the root window.");
  }

  parent_win = event.xbutton.window; /* ASSERT event.xbutton.window == root_win, due to using XWindowEvent(root_win) */
  win_x      = event.xbutton.x;
  win_y      = event.xbutton.y;
  cur_win    = event.xbutton.subwindow;
  while ((xtc_ok = XTranslateCoordinates(display,
					 parent_win, cur_win,
					 win_x, win_y, /* give the x,y coords of event in parent_w */
					 &win_x, &win_y, /* return the x,y coords relative to cur_win */
					 &child_win)) /* returns child window of cur_win if that contains coords, else nil */
	 && child_win) {
#ifdef WINTERP_DEBUG_1
	 fprintf(stderr, "parent_win=0x%lx, cur_win=0x%lx, child_win=0x%lx\n", (unsigned long) parent_win, (unsigned long) cur_win, (unsigned long) child_win);
#endif /* WINTERP_DEBUG_1 */
	 parent_win = cur_win;
	 cur_win    = child_win;
       }

#ifdef WINTERP_DEBUG_1
  fprintf(stderr, "	Smallest window containing userclick is 0x%lx\n", (unsigned long) cur_win);
#endif /* WINTERP_DEBUG_1 */

  if (!xtc_ok)
    xlfail("Bug in GET_MOUSED_WIDGET -- XTranslateCoordinates() failed.");

  if (!(widget_id = XtWindowToWidget(display, cur_win)))
    xlfail("GET_MOUSED_WIDGET -- Couldn't find widget associated with window.\n	(Is the selected widget/window inside a different application?).\n");

  /* if the widget is a composite it may be managing a gadget -- attempt to retrieve it by looking up x,y coords in manager */
  if (XtIsComposite(widget_id) &&
      (gadget_id = (Widget) _XmInputInGadget(widget_id, win_x, win_y)))
    lval_widget = Wcls_WidgetID_To_WIDGETOBJ(gadget_id); /* then return the WIDGETOBJ assoc'd with gadget */
  else
    lval_widget = Wcls_WidgetID_To_WIDGETOBJ(widget_id); /* otherwise, we return the WIDGETOBJ assoc'd with smallest window */

  return (lval_widget);
}


static Boolean exposed_p;

#ifdef WINTERP_MOTIF_11		/* change is really due to X11r4... */
static void Wut_Expose_Event_Handler_Proc(widget_id, client_data, event, continue_to_dispatch)
     Widget    widget_id;
     XtPointer client_data;
     XEvent*   event;
     Boolean*  continue_to_dispatch; /* currently not supported... */
#else /* MOTIF 1.0 and X11r3 */
static void Wut_Expose_Event_Handler_Proc(widget_id, client_data, event)
     Widget    widget_id;
     XtPointer client_data;
     XEvent*   event;
#endif /* WINTERP_MOTIF_11 */
{
  XmUpdateDisplay(widget_id);
  exposed_p = TRUE;
}


/******************************************************************************
 * void Wut_Wait_Till_Expose();
 ******************************************************************************/
void Wut_Wait_Till_Expose(widget_id)
     Widget widget_id;
{
  extern XtAppContext app_context; /* winterp.c */

  /* reset the global to indicate that exposure hasn't happened */
  exposed_p = FALSE;

  /* attach expose handler which sets exposed_p when fired */
  XtAddEventHandler(widget_id, ExposureMask, FALSE, 
		    Wut_Expose_Event_Handler_Proc, NULL);

#ifdef WINTERP_DEBUG_1
  /* loop until expose occured */
  {
    XEvent              event;
    do {
      XtAppNextEvent(app_context, &event);
      fprintf(stdout,"wait_till_expose: type=%d\n", event.type);
      fflush(stdout);
      XtDispatchEvent(&event);
    } while (exposed_p == FALSE);
  }
#else /* !defined(WINTERP_DEBUG_1) */
  /* loop until expose occured */
  while (exposed_p == FALSE)	/* exposed_p set in Wut_Expose_Event_Handler_Proc */
    XtAppProcessEvent(app_context, XtIMXEvent);	/* don't process timeouts or alternate input sources... */
#endif /* WINTERP_DEBUG_1 */
  
  /* remove expose handler */
  XtRemoveEventHandler(widget_id, ExposureMask, FALSE,
		       Wut_Expose_Event_Handler_Proc, NULL);
}
