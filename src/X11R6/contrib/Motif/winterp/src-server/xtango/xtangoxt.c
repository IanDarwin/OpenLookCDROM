/* -*-C-*-
*******************************************************************************
*
* File:         xtangoxt.c
* RCS:          $Header: /disk3/npm/src/winterp/src-server/xtango/RCS/xtangoxt.c,v 2.8 1994/06/09 01:31:12 npm Exp $
* Description:  XTANGO ANIMATION PACKAGE (xt)
* Author:       John T. Stasko, Doug Hayes, Niels Mayer
* Created:      1990
* Modified:     Sun Jun  5 05:23:48 1994 (Niels Mayer) npm@indeed
* Language:     C
* Package:      N/A
* Status:	X11r6 contrib release
*
* Xtango 1.52 Copyright 1990-1994 Georgia Institute of Technology
* 			     (by John T. Stasko and Doug Hayes).
* WINTERP 2.0 Copyright (C) 1994, Enterprise Integration Technologies Corp. and Niels Mayer.
* WINTERP 1.15-1.99, Copyright (c) 1993, Niels P. Mayer.
* WINTERP 1.0-1.14, Copyright (c) 1989-1992 Hewlett-Packard Co. and Niels Mayer.
* 
* This version of Xtango 1.52 (varargs version) represents a subset of
* the Xtango distribution that has been modified specifically for use with
* WINTERP. Non-WINTERP uses of Xtango should use the complete, standard
* version of Xtango, which is available under separate copyright via
* anonymous ftp from par.cc.gatech.edu:pub/xtangovarargs.tar.Z and
* par.cc.gatech.edu:pub/xtango.tar.Z.
* 
* Permission to use, copy, modify, distribute, and sell this software and its
* documentation for any purpose is hereby granted without fee, provided that
* the above copyright notice appear in all copies and that both that
* copyright notice and this permission notice appear in supporting
* documentation, and that the name of Georgia Institute of Technology, 
* John T. Stasko, Doug Hayes, Enterprise Integration Technologies, 
* Hewlett-Packard Company, or Niels Mayer not be used in advertising or
* publicity pertaining to distribution of the software without specific,
* written prior permission. Georgia Institute of Technology, John T. Stasko,
* Doug Hayes, Enterprise Integration Technologies, Hewlett-Packard Company,
* and Niels Mayer makes no representations about the suitability of this 
* software for any purpose.  It is provided "as is" without express or
* implied warranty.
* 
* GEORGIA INSTITUTE OF TECHNOLOGY, JOHN T. STASKO, DOUG HAYES, ENTERPRISE
* INTEGRATION TECHNOLOGIES, HEWLETT-PACKARD COMPANY AND NIELS MAYER
* DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING ALL IMPLIED
* WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL GEORGIA
* INSTITUTE OF TECHNOLOGY, JOHN T. STASKO, DOUG HAYES, ENTERPRISE INTEGRATION
* TECHNOLOGIES, HEWLETT-PACKARD COMPANY OR NIELS MAYER BE LIABLE
* FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER
* RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF
* CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN
* CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
*
*******************************************************************************
*/
static char rcs_identity[] = "@(#)$Header: /disk3/npm/src/winterp/src-server/xtango/RCS/xtangoxt.c,v 2.8 1994/06/09 01:31:12 npm Exp $";

/* Modification Date  Description			      */
/* -----------------  --------------------------------------- */
/* 90/11/10 JDH       Baselined source.			      */
/*							      */
/**************************************************************/
/* NOTE:   These are Xt functions and callbacks which do NOT  */
/*	   depend on the underlying widget set.		      */
/**************************************************************/

/**************************************************************/
/*****************	 include files       ******************/ 
/**************************************************************/

#include "xtangolocal.h"

#include <X11/cursorfont.h>

/**************************************************************/
/*****************	GLOBAL variables     ******************/
/**************************************************************/

/**************************************************************/
/*****************	LOCAL variables      ******************/
/**************************************************************/

/**************************************************************/
/*****************      LOCAL functions      ******************/
/**************************************************************/

/**************************************************************/
/*****************       entry points        ******************/
/**************************************************************/

#ifndef WINTERP			/* NPM: removed these because names are not distinct-enough */
void pan_callback(), refresh_callback(), resize_callback();
void select_point(), quit_callback(),    zoom_callback();
void TANGOend();
#endif /* !defined(WINTERP) */

int  TANGOinput_coord();
int  TANGOinput_image();

/**************************************************************/
/* TANGO_check_X_events -- Handle all pending X events.       */
/*							      */
/* RETURNS:  None.					      */
/**************************************************************/
#ifdef WINTERP
/*
 * NPM: Special version of TANGO_check_X_events() for WINTERP,
 * since Motif can't handle WINTERP/XLISP longjmp'ing out of
 * a nested event loop during an XLISP error. Problems also
 * arise because events that should be handled in WINTERP's
 * main event loop might get handled here. Creating new widgets
 * or destroying existing widgets inside a nested event loop
 * seems to cause core dumps. So instead, we only handle
 * a few kinds of events "manually" here, rather than dealing
 * with all the problems caused by generalized event handling,
 * composed with the problems of running the xlisp evaluator
 * and/or dealing with catching error returns from it.
 *
 * The code below deals with the following cases:
 * - if KEY_PRESS_MASK is set in method :SET_ANIMATION_EVENT_PROCESSING
 *   then, when the user enters ^C (control C) in the widget running the
 *   animation, the animation will stop with an xlabort().
 * - if BUTTON_PRESS_MASK is set in method :SET_ANIMATION_EVENT_PROCESSING
 *   then, when the user clicks any mouse button in the widget running the
 *   animation, the animation will stop with an xlabort().
 * - if EXPOSURE_MASK is set in method :SET_ANIMATION_EVENT_PROCESSING
 *   then the window running the current animation will refresh on exposure.
 */
#include <X11/keysym.h>		/* define XK_Cancel */
void
TANGO_check_X_events()
{
  extern void xlabort();	/* xlisp/xldbug.c */
  extern void Xtango_Restore_Context();	/* ../t_utils.c */
  XEvent event;
  KeySym sym;

  if ((TANGO__data->anim_event_mask != NoEventMask) &&
      XCheckWindowEvent(TANGO__data->display, TANGO__data->display_window,
			TANGO__data->anim_event_mask, &event)) {
    switch (event.type) {
    case KeyPress:
      sym = XLookupKeysym((XKeyEvent*) &event, 0);
      if ((sym == ((KeySym) XK_Cancel)) | /* abort animation on Cancel key ("Stop" on HP keyboard) */
	  (sym == ((KeySym) XK_Escape)) | /* abort animation when escape key pressed ("Esc" on HP Keyboard) */
	  (sym == ((KeySym) XK_Break))){ /* abort animation when Break key pressed ("Break" on HP Keyboard) */
	Xtango_Restore_Context(); /* must do this, since Xtango_Save_Set_Context_From_WidgetID() or Xtango_Save_Set_Context() called prior to the Xtango function that called TANGO_check_X_events() */
	xlabort("Xtango animation aborted by typing Stop/Esc/Break into animation window.");
      }
      /* ELSE issue warning and reinsert the extracted key event back into event queue */
      else
	COMPLAIN("WARNING: TANGO_check_X_events() ignored XKeyEvent (Keysym 0x%lx).\n", (unsigned long) sym);
      break;
    case ButtonPress:
      XWindowEvent(TANGO__data->display, TANGO__data->display_window, /* discard the buttonrelease event */
		   ButtonReleaseMask, &event);
      Xtango_Restore_Context();	/* must do this, since Xtango_Save_Set_Context_From_WidgetID() or Xtango_Save_Set_Context() called prior to the Xtango function that called TANGO_check_X_events() */
      xlabort("Xtango animation aborted by clicking mouse-button in an animation window.");
      break;
    case Expose:		/* ExposureMask */
      /* NPM: process exposure events only on the window performing animation -- otherwise
	 the unrefreshed areas end up looking pretty ugly since only the damage area will get
	 refreshed in the process of doing an animation */
      if (TANGO__data && TANGO__data->pixmap) /* Avoid resize when widget initially displayed -- *before* TANGO__data->pixmap has been created. */
	TANGO_refresh();
      break;
    default:			/* this case shouldn't occur, may indicate an error somewhere if it does ... */
      COMPLAIN("WARNING: TANGO_check_X_events() put back XAnyEvent (event.type %ld).\n", (long) event.type);
      XPutBackEvent(TANGO__data->display, &event); /* NOTE EVENT QUEUE FROBBING -- HOPE THIS DON'T INTRODUCE EVENT ORDERING BUGS... */
      break;
    }
  }
}
#else /* !defined(WINTERP) */
void
TANGO_check_X_events()
{
   XEvent xev;

   while (XtPending() || TANGO__data->paused) {
      XtNextEvent(&xev);
      XtDispatchEvent(&xev);
      } 
}
#endif /* WINTERP */


#ifndef WINTERP
/**************************************************************/
/* TANGOend -- Force user to select 'quit' button to exit.    */
/*	       (ie. allow all X events to be processed)       */
/*							      */
/* RETURNS:  None.					      */
/**************************************************************/
void
TANGOend()
{
   XEvent xev;

   XFlush(TANGO__data->display);
   while (1) {				/* User must press 'quit' button */
      XtNextEvent(&xev);
      XtDispatchEvent(&xev);
      }
}
#endif /* !defined(WINTERP) */


/**************************************************************/
/* TANGOinput_coord -- Allow the user to select an arbitrary  */
/*		       window location.			      */
/*							      */
/* RETURNS:  TRUE(1) since a valid coordinate is always	      */
/*	     returned.					      */
/**************************************************************/
int
TANGOinput_coord(x,y)
   WIN_COORD *x,*y;
{
   XEvent event;
#ifdef WINTERP
    extern void xlfail();	/* ../xlisp/xldbug.c */
    extern void Xtango_Restore_Context(); /* ../t_utils.c */
    Cursor cursor = XCreateFontCursor(TANGO__data->display, XC_crosshair);
#else /* !defined(WINTERP) */
   int	  xpt, ypt;
#endif /* WINTERP */

   if (!TANGO__data) TANGOinit();

   DEBUG("TANGOinput_coord(0x%lx, 0x%lx)\n", (unsigned long) x, (unsigned long) y);

#ifdef WINTERP
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
   XSync(TANGO__data->display, 0);
   while (XCheckMaskEvent(TANGO__data->display,
			  ButtonPressMask | ButtonReleaseMask |
			  ButtonMotionMask | PointerMotionMask |
			  KeyPressMask,
			  &event)) { /* discard above listed event-types */ }

   /* Confine mouse pointer to main (TANGO__data->easel) window */
   /* NOTE: See comment in ../w_utils.c:Wut_Prim_GET_MOUSED_WIDGET()
      about problems w/ using XtLastTimestampProcessed() and reasons
      for using CurrentTime instead */
   if (GrabSuccess != XGrabPointer(TANGO__data->display, TANGO__data->display_window, False,
				   ButtonPressMask | ButtonReleaseMask,
				   GrabModeAsync, GrabModeAsync, TANGO__data->display_window,
				   cursor, CurrentTime)) {
     XFreeCursor(TANGO__data->display, cursor); /* cleanup from XCreateFontCursor() */
     Xtango_Restore_Context();	/* restore before returning -- assumes prior call to Xtango_Save_Set_Context_From_WidgetID() or Xtango_Save_Set_Context() */
     xlfail("TANGOinput_coord -- couldn't grab pointer (XGrabPointer() failed).");
   }
   XWindowEvent(TANGO__data->display, TANGO__data->display_window, /* remove the buttonpress from the queue */
		ButtonPressMask, &event);
   XWindowEvent(TANGO__data->display, TANGO__data->display_window, /* get the buttonrelease event */
		ButtonReleaseMask, &event);
   XUngrabPointer(TANGO__data->display, event.xbutton.time);
   XFlush(TANGO__data->display);
   XFreeCursor(TANGO__data->display, cursor); /* cleanup from XCreateFontCursor() */

   *x = ( ((WIN_COORD) (event.xbutton.x - TANGO__data->x_window_offset))
	 / TANGO__data->x_WIN_COORD_to_int) + TANGO__data->lx;

   *y = ( ((WIN_COORD) (event.xbutton.y - TANGO__data->y_window_offset))
	 / TANGO__data->y_WIN_COORD_to_int) + TANGO__data->ty;

#else /* !defined(WINTERP) */

   /* Wait for button press event handler to change "getMouse" */
   for (TANGO__data->getMouse = TRUE; TANGO__data->getMouse; ) {
      XtNextEvent(&event);
      XtDispatchEvent(&event);
      }

   *x = (double)TANGO__data->mousex / TANGO__data->x_WIN_COORD_to_int
	                                 + (double)TANGO__data->lx;

   *y = (double)TANGO__data->mousey / TANGO__data->y_WIN_COORD_to_int
	                                 + (double)TANGO__data->ty;

#endif /* WINTERP */

   return(1);
}



/**************************************************************/
/* TANGOinput_image -- Allow the user to select a position in */
/*		       the window, and return the uppermost   */
/*		       image object whose bounding box	      */
/*		       contains that position.		      */
/*							      */
/* RETURNS:  TRUE(1) or FALSE(0) based on whether or not an   */
/*	     image was found.				      */
/**************************************************************/
int
TANGOinput_image(im)
   TANGO_IMAGE *im;
{
#ifdef WINTERP
   extern void xlfail();	/* ../xlisp/xldbug.c */
   extern void Xtango_Restore_Context(); /* ../t_utils.c */
   Cursor cursor = XCreateFontCursor(TANGO__data->display, XC_crosshair);
#else /* !defined(WINTERP) */
   int	       xpt,ypt;
#endif /* WINTERP */
   WIN_COORD   x,y,lx,by,rx,ty;
   IMAGE_PTR   i;
   TANGO_IMAGE image;
   XEvent      event;

   if (!TANGO__data) TANGOinit();

   DEBUG("TANGOinput_image(0x%lx)\n", (unsigned long) *im);

#ifdef WINTERP
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
   XSync(TANGO__data->display, 0);
   while (XCheckMaskEvent(TANGO__data->display,
			  ButtonPressMask | ButtonReleaseMask |
			  ButtonMotionMask | PointerMotionMask |
			  KeyPressMask,
			  &event)) { /* discard above listed event-types */ }

   /* Confine mouse pointer to main (TANGO__data->easel) window */
   /* NOTE: See comment in ../w_utils.c:Wut_Prim_GET_MOUSED_WIDGET()
      about problems w/ using XtLastTimestampProcessed() and reasons
      for using CurrentTime instead */
   if (GrabSuccess != XGrabPointer(TANGO__data->display, TANGO__data->display_window, False,
				   ButtonPressMask | ButtonReleaseMask,
				   GrabModeAsync, GrabModeAsync, TANGO__data->display_window,
				   cursor, CurrentTime)) {
     XFreeCursor(TANGO__data->display, cursor); /* cleanup from XCreateFontCursor() */
     Xtango_Restore_Context();	/* restore before returning -- assumes prior call to Xtango_Save_Set_Context_From_WidgetID() or Xtango_Save_Set_Context() */
     xlfail("TANGOinput_image -- couldn't grab pointer (XGrabPointer() failed).");
   }
   XWindowEvent(TANGO__data->display, TANGO__data->display_window, /* remove the buttonpress from the queue */
		ButtonPressMask, &event);
   XWindowEvent(TANGO__data->display, TANGO__data->display_window, /* get the buttonrelease event */
		ButtonReleaseMask, &event); 
   XUngrabPointer(TANGO__data->display, event.xbutton.time);
   XFlush(TANGO__data->display);
   XFreeCursor(TANGO__data->display, cursor); /* cleanup from XCreateFontCursor() */

   x = ( ((WIN_COORD) (event.xbutton.x - TANGO__data->x_window_offset))
	/ TANGO__data->x_WIN_COORD_to_int) + TANGO__data->lx;

   y = ( ((WIN_COORD) (event.xbutton.y - TANGO__data->y_window_offset))
	/ TANGO__data->y_WIN_COORD_to_int) + TANGO__data->ty;

#else  /* !defined(WINTERP) */

   /* Wait for button press event handler to change "getMouse" */
   for (TANGO__data->getMouse = TRUE; TANGO__data->getMouse; ) {
      XtNextEvent(&event);
      XtDispatchEvent(&event);
      }

   x = (double)TANGO__data->mousex / TANGO__data->x_WIN_COORD_to_int
                                         + (double)TANGO__data->lx;

   y = (double)TANGO__data->mousey / TANGO__data->y_WIN_COORD_to_int
                                         + (double)TANGO__data->ty;

#endif /* WINTERP */

   for (i = TANGO__data->confighead; i; i = i->nexti) {
      image = i->image;

      /* only want to check visible images */
      if (image->visible) {
	 TANGO_bounding_box(image,&lx,&by,&rx,&ty);

#define ERR_FACTOR 0.015
/* this is useful for horiz and vert lines */
	 if ((lx-ERR_FACTOR <= x) && (x <= rx+ERR_FACTOR) && 
             (ty-ERR_FACTOR <= y) && (y <= by+ERR_FACTOR))
	    break;
	 }
      }

   if (i) {
      *im = i->image;
      return(1);
      }

   *im = NULL;
   return(0);
}



#ifdef WINTERP
/**************************************************************/
/* TANGOget_event_image -- return the uppermost image obj     */
/*		       whose bounding box contains the x,y    */
/*		       position from <event> argument.        */
/*							      */
/* RETURNS:  TRUE(1) or FALSE(0) based on whether or not an   */
/*	     image was found.				      */
/**************************************************************/
int
TANGOget_event_image(im, event)
     TANGO_IMAGE *im;
     XEvent *event;
{
  WIN_COORD   x,y,lx,by,rx,ty;
  IMAGE_PTR   i;
  TANGO_IMAGE image;
  int mousex, mousey;

  if (!TANGO__data) TANGOinit();

  DEBUG("TANGOget_event_image(0x%lx, 0x%lx)\n", (unsigned long) *im, (unsigned long) event);

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
    *im = NULL;
    return (0);			/* RETURN failure */
  }

   x = ( ((WIN_COORD) (mousex - TANGO__data->x_window_offset))
	/ TANGO__data->x_WIN_COORD_to_int) + TANGO__data->lx;

   y = ( ((WIN_COORD) (mousey - TANGO__data->y_window_offset))
	/ TANGO__data->y_WIN_COORD_to_int) + TANGO__data->ty;

  for (i = TANGO__data->confighead; i; i = i->nexti) {
    image = i->image;

    /* only want to check visible images */
    if (image->visible) {
      TANGO_bounding_box(image,&lx,&by,&rx,&ty);

      /* this is useful for horiz and vert lines */
      if ((lx-ERR_FACTOR <= x) && (x <= rx+ERR_FACTOR) && 
	  (ty-ERR_FACTOR <= y) && (y <= by+ERR_FACTOR))
	break;
    }
  }

  if (i) {
    *im = i->image;
    return(1);			/* RETURN success */
  }

  *im = NULL;
  return(0);			/* RETURN failure */
}



/**************************************************************/
/* TANGOget_event_coord -- return the tango coordinates of    */
/*		       position from <event> argument.        */
/*							      */
/* RETURNS:  TRUE(1) or FALSE(0) based on whether or not an   */
/*	     image was found.				      */
/**************************************************************/
int
TANGOget_event_coord(x, y, event)
     WIN_COORD *x;
     WIN_COORD *y;
     XEvent *event;
{
  int mousex, mousey;

  if (!TANGO__data) TANGOinit();

  DEBUG("TANGOget_event_coord(0x%lx, 0x%lx, 0x%lx)\n", (unsigned long) *x, (unsigned long) *y, (unsigned long) event);

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
    *x = 0.0;
    *y = 0.0;
    return (0);			/* RETURN failure */
  }

  *x = ( ((WIN_COORD) (mousex - TANGO__data->x_window_offset))
	/ TANGO__data->x_WIN_COORD_to_int) + TANGO__data->lx;

  *y = ( ((WIN_COORD) (mousey - TANGO__data->y_window_offset))
	/ TANGO__data->y_WIN_COORD_to_int) + TANGO__data->ty;

  return(1);			/* RETURN success */
}
#endif /* WINTERP */



#ifndef WINTERP			/* NPM: removed these because names are not distinct-enough */
/**************************************************************/
/* pan_callback -- Pan easel in specified direction.	      */
/*							      */
/* RETURNS:  None.					      */
/**************************************************************/
void
pan_callback(w, direction, call_data)
   Widget  w;
   int     direction;
   caddr_t call_data;
{
   double panfactor = 0.2,change;

   switch (direction) {
      case 0:	change = (TANGO__data->rx - TANGO__data->lx) * panfactor;
                TANGO__data->lx -= change;
         	TANGO__data->rx -= change;
         	break;
      case 1:	change = (TANGO__data->rx - TANGO__data->lx) * panfactor;
                TANGO__data->lx += change;
		TANGO__data->rx += change;
		break;
      case 2:	change = (TANGO__data->by - TANGO__data->ty) * panfactor;
                TANGO__data->ty -= change;
 		TANGO__data->by -= change;
		break;
      case 3:	change = (TANGO__data->by - TANGO__data->ty) * panfactor;
                TANGO__data->by += change;
		TANGO__data->ty += change;
		break;
      default: printf("???\n");
      }

   TANGO__data->x_WIN_COORD_to_int = ((WIN_COORD) TANGO__data->width) /
       (TANGO__data->rx - TANGO__data->lx);
   TANGO__data->y_WIN_COORD_to_int = ((WIN_COORD) TANGO__data->height) /
       (TANGO__data->by - TANGO__data->ty);

   TANGO_refresh();
}
#endif /* !defined(WINTERP) */



#ifndef WINTERP			/* NPM: removed these because names are not distinct-enough */
/**************************************************************/
/* refresh_callback -- Redraw all images onscreen.	      */
/*							      */
/* RETURNS:  None.					      */
/**************************************************************/
void
refresh_callback(w, client_data, call_data)
   Widget  w;
   caddr_t client_data, call_data;
{
   if (TANGO__data->pixmap) /* Avoid resize when widget initially displayed */
			/* -- *before* TANGO__data->pixmap has been created. */
      TANGO_refresh();
}
#endif /* !defined(WINTERP) */



#ifndef WINTERP			/* NPM: removed these because names are not distinct-enough */
/**************************************************************/
/* resize_callback -- Resize widgets.			      */
/*							      */
/* RETURNS:  None.					      */
/**************************************************************/
void
resize_callback(w, client_data, call_data)
   Widget  w;
   caddr_t client_data, call_data;
{
   if (TANGO__data->pixmap) {/* Avoid resize when widget initially displayed */
			/* -- *before* TANGO__data->pixmap has been created. */
      TANGO_anim_setup(w);
      TANGO_refresh();
      }
}
#endif /* !defined(WINTERP) */



#ifndef WINTERP			/* NPM: removed these because names are not distinct-enough */
/**************************************************************/
/* quit_callback -- Exit TANGO.				      */
/*							      */
/* RETURNS:  None.					      */
/**************************************************************/
void
quit_callback(w, client_data, call_data)
   Widget  w;
   caddr_t client_data, call_data;
{
   exit(0);
}
#endif /* !defined(WINTERP) */



#ifndef WINTERP			/* NPM: removed these because names are not distinct-enough */
/**************************************************************/
/* zoom_callback -- Zoom in/out.			      */
/*							      */
/* RETURNS:  None.					      */
/**************************************************************/
void
zoom_callback(w, direction, call_data)
   Widget  w;
   int     direction;
   caddr_t call_data;
{
#define  ZOOMFACTOR  0.8
   WIN_COORD change;

   switch (direction) {
      case 0:
         change = (TANGO__data->rx - TANGO__data->lx) * (1.0-ZOOMFACTOR);
         TANGO__data->rx -= change/2.0;
         TANGO__data->lx += change/2.0;
         change = (TANGO__data->by - TANGO__data->ty) * (1.0-ZOOMFACTOR);
         TANGO__data->by -= change/2.0;
         TANGO__data->ty += change/2.0;
         break;
      case 1:
         change = (TANGO__data->rx - TANGO__data->lx) * (1.0/ZOOMFACTOR - 1.0);
         TANGO__data->rx += change/2.0;
         TANGO__data->lx -= change/2.0;
         change = (TANGO__data->by - TANGO__data->ty) * (1.0/ZOOMFACTOR - 1.0);
         TANGO__data->by += change/2.0;
         TANGO__data->ty -= change/2.0;
         break;
      default:
         printf("???\n");
   }

   TANGO__data->x_WIN_COORD_to_int = ((WIN_COORD) TANGO__data->width) /
       (TANGO__data->rx - TANGO__data->lx);
   TANGO__data->y_WIN_COORD_to_int = ((WIN_COORD) TANGO__data->height) /
       (TANGO__data->by - TANGO__data->ty);

   TANGO_refresh();
}
#endif /* !defined(WINTERP) */



/**************************************************************/
/* TANGOset_coord - allows the user to modify the window      */
/*   viewing coords.			                      */
/*							      */
/* RETURNS:  1 if valid coords entered, 0 otherwise.	      */
/**************************************************************/
int
TANGOset_coord(lx, by, rx, ty)
   double lx, by, rx, ty;
{
   if (lx >= rx) {
      printf("rx (%f) > lx (%f) passed to TANGOset_coord: illegal\n",
	     rx,lx);
      return(0);
      }
   if (by <= ty) {
      printf("by (%f) < ty (%f) passed to TANGOset_coord: illegal\n",
	     by,ty);
      return(0);
      }
   TANGO__data->lx = lx;
   TANGO__data->rx = rx;
   TANGO__data->by = by;
   TANGO__data->ty = ty;

   TANGO__data->x_WIN_COORD_to_int = ((WIN_COORD) TANGO__data->width) /
       (TANGO__data->rx - TANGO__data->lx);
   TANGO__data->y_WIN_COORD_to_int = ((WIN_COORD) TANGO__data->height) /
       (TANGO__data->by - TANGO__data->ty);

   TANGO_refresh();
   return(1);
}





/**************************************************************/
/* TANGOinq_coord - returns current values of window coords   */
/*							      */
/* RETURNS:  None.	                                      */
/**************************************************************/
void
TANGOinq_coord(lx, by, rx, ty)
   double *lx, *by, *rx, *ty;
{
   *lx = TANGO__data->lx;
   *rx = TANGO__data->rx;
   *by = TANGO__data->by;
   *ty = TANGO__data->ty;
}



#ifndef WINTERP			/* NPM: removed these because names are not distinct-enough */
/**************************************************************/
/* select_point -- (callback) Save user-selected coordinates. */
/*							      */
/* RETURNS:  None.					      */
/**************************************************************/
void
select_point(w, notused, event)
   Widget w;
   void   *notused;
   XEvent *event;
{
   TANGO__data->getMouse = FALSE;
   TANGO__data->mousex = event->xbutton.x;
   TANGO__data->mousey = event->xbutton.y;
}
#endif /* !defined(WINTERP) */

/**************************************************************/
/***************       end of xtangoxt.c       ****************/
/**************************************************************/
