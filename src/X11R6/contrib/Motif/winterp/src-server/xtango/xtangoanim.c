/* -*-C-*-
*******************************************************************************
*
* File:         xtangoanim.c
* RCS:          $Header: /disk3/npm/src/winterp/src-server/xtango/RCS/xtangoanim.c,v 2.9 1994/06/09 01:23:49 npm Exp $
* Description:  XTANGO ANIMATION PACKAGE (anim)
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
static char rcs_identity[] = "@(#)$Header: /disk3/npm/src/winterp/src-server/xtango/RCS/xtangoanim.c,v 2.9 1994/06/09 01:23:49 npm Exp $";

/* Modification Date  Description			      */
/* -----------------  --------------------------------------- */
/* 90/10/26 JDH	      Baselined source.                       */
/*							      */
/**************************************************************/

/**************************************************************/
/*****************	 include files       ******************/ 
/**************************************************************/

#include "xtangolocal.h"

#include <X11/StringDefs.h>

#ifdef WINTERP			/* NPM: Motif-specific includes req'd for mods to TANGO_anim_setup() for drawing into XmDrawingArea or XmDrawnButton */
#include <Xm/DrawingA.h>
#include <Xm/DrawnB.h>
#endif /* WINTERP */

/**************************************************************/
/*****************	LOCAL data types     ******************/
/**************************************************************/

/**************************************************************/
/*****************	GLOBAL variables     ******************/
/**************************************************************/

/**************************************************************/
/*****************	LOCAL variables      ******************/
/**************************************************************/

/**************************************************************/
/*****************      LOCAL functions      ******************/
/**************************************************************/

/******************************************************************/
/* TANGO_anim_setup -- Create Pixmaps to hold animation frame(s). */
/*		       Whenever the frame dimensions are changed  */
/*		       (eg. by resizing the window) call this     */
/*		       function to create new Pixmaps and update  */
/*		       global TANGO__data information.		  */
/*								  */
/* RETURNS:  NULL.                                         	  */
/******************************************************************/
void
TANGO_anim_setup(widgetid)
   Widget widgetid;
{
   int	     old;
#ifdef WINTERP /* NPM: addition allowing XmDrawnButton to be used as drawing area -- a feature of WINTERP's TANGO:WIDGET_CLASS */
   Dimension total_border_width, total_border_height;
   Arg	     wargs[10];
   Dimension w			= 0;
   Dimension h			= 0;
   Dimension highlight_width	= 0;
   Dimension shadow_width	= 0;
   Dimension margin_width	= 0;
   Dimension margin_height	= 0;
   Dimension margin_bottom	= 0;
   Dimension margin_top		= 0;
   Dimension margin_left	= 0;
   Dimension margin_right	= 0;
#else /* !defined(WINTERP) */
   Arg	     wargs[2];
   Dimension w ,h;
#endif /* WINTERP */
   Display  *_display;

   _display = TANGO__data->display;

	/* Each frame gets copied here for viewing */
   TANGO__data->display_window = XtWindow(widgetid);

   /* Create offscreen frame(s)...Pixmap(s) */

   if (TANGO__data->pixmap) XFreePixmap(_display, TANGO__data->pixmap);	
                       /* Resizing old frame? */

#ifdef WINTERP /* NPM: addition allowing XmDrawnButton to be used as drawing area -- a feature of WINTERP's TANGO:WIDGET_CLASS */
   if (XtClass(widgetid) == xmDrawingAreaWidgetClass) {
     XtSetArg(wargs[0], XmNwidth, &w);
     XtSetArg(wargs[1], XmNheight, &h);
     XtGetValues(widgetid, wargs, 2);
     if (w <= 5) w = 10;	/* set a minimum width */
     if (h <= 5) h = 10;	/* set a minimum height */
     TANGO__data->width  = (int) w; /* Update global data */
     TANGO__data->height = (int) h;
     TANGO__data->x_window_offset = TANGO__data->y_window_offset = 0;
   }
   else {			/* xmDrawnButtonWidgetClass */
     XtSetArg(wargs[0], XmNwidth,		&w);
     XtSetArg(wargs[1], XmNheight,		&h);
     XtSetArg(wargs[2], XmNhighlightThickness,	&highlight_width);
     XtSetArg(wargs[3], XmNshadowThickness,	&shadow_width);
     XtSetArg(wargs[4], XmNmarginWidth,		&margin_width);
     XtSetArg(wargs[5], XmNmarginHeight,	&margin_height);
     XtSetArg(wargs[6], XmNmarginBottom,	&margin_bottom);
     XtSetArg(wargs[7], XmNmarginTop,		&margin_top);
     XtSetArg(wargs[8], XmNmarginLeft,		&margin_left);
     XtSetArg(wargs[9], XmNmarginRight,		&margin_right);
     XtGetValues(widgetid, wargs, 10);

     total_border_width  = highlight_width + shadow_width + margin_width;
     total_border_height = highlight_width + shadow_width + margin_height;

     w = w - (2*total_border_width  + margin_left + margin_right);
     h = h - (2*total_border_height + margin_top  + margin_bottom);
     if (w <= 5) w = 10;	/* set a minimum width */
     if (h <= 5) h = 10;	/* set a minimum height */
     TANGO__data->width  = (int) w; /* Update global data */
     TANGO__data->height = (int) h;
     TANGO__data->x_window_offset = (int) (total_border_width  + margin_left);
     TANGO__data->y_window_offset = (int) (total_border_height + margin_top);
   }
#else /* !defined(WINTERP) */
   XtSetArg(wargs[0], XtNwidth, &w);
   XtSetArg(wargs[1], XtNheight, &h);
   XtGetValues(TANGO__data->easel, wargs, 2);

   TANGO__data->width = w;	/* Update global data */
   TANGO__data->height = h;
#endif /* WINTERP */

   TANGO__data->x_WIN_COORD_to_int = ((WIN_COORD) w) 
                                       / (TANGO__data->rx - TANGO__data->lx);
   TANGO__data->y_WIN_COORD_to_int = ((WIN_COORD) h) 
                                       / (TANGO__data->by - TANGO__data->ty);

#ifdef WINTERP
   /* In WINTERP, Pixmap created by XCreatePixmap() call below is freed
      via wc_Xtango.c:Xtango_Widget_Destroy_Callback() when a TANGO_WIDGET
      instance is destroyed  */
#endif /* WINTERP */
   TANGO__data->pixmap = XCreatePixmap(_display, 
                             DefaultRootWindow(_display), 
                             (unsigned int) w, (unsigned int) h,
  	         	     DefaultDepth(_display, DefaultScreen(_display)));
   
   /* Pixmaps always start in an unknown state...clear it */

   old = TANGO_color(TANGO__data->bgcolor);
#ifdef WINTERP
   /* NPM: unset the clip region (set by XSetClipRectangles()) that might have
      been set by previous calls to TANGO_damage_clear_screen() (in TANGOtrans_perform())
      or TANGO_clear_screen() (in TANGO_refresh()) -- Fixed in xtango 1.52 */
#endif /* WINTERP */
   XSetClipMask(TANGO__data->display, TANGO_gc(), None);
   XFillRectangle(_display, TANGO__data->pixmap, TANGO_gc(), 0, 0, 
                      (unsigned int) w, (unsigned int) h);
   TANGO_color(old);
}



/*******************************************************************/
/* TANGO_anim_next_damage_frame -- Display next animation frame by */
/*			    PixBlting offscreen Pixmap to onscreen */
/*			    window.				   */
/*								   */
/* RETURNS:  NULL                                       	   */
/*******************************************************************/
void
TANGO_anim_next_damage_frame()
{
#ifdef WINTERP
#if 0 /* COMMENTED OUT -- debug print statements ...*/
  printf("damage_x=%d, damage_y=%d, damage_width=%u, damage_height=%u\n",
	 TANGO__data->damage_x, TANGO__data->damage_y, TANGO__data->damage_width, TANGO__data->damage_height);
  printf("lx=%f, by=%f, rx=%f, ty=%f\n",
	 TANGO__data->lx, TANGO__data->by, TANGO__data->rx, TANGO__data->ty);
  printf("damlx=%f, damby=%f, damrx=%f, damty=%f\n",
	 TANGO__data->damlx, TANGO__data->damby, TANGO__data->damrx, TANGO__data->damty);
#endif /* COMMENTED OUT */
  /*
   * NPM: if we're in a drawn button we end up drawing to the onscreen-window
   * which is sized larger than the offscreen pixmap -- the size difference
   * accomodates the shadows bordering the onscreen-window. The origin of the
   * offscreen pixmap is XCopyArea()'d into the window at destination location
   * <x_window_offset,y_window_offset>. The size and location drawn is determined
   * by the size/location of the damage area set in TANGO_clear_screen() or
   * TANGO_damage_clear_screen(), which are called from TANGO_refresh() or
   * TANGOtrans_perform(), respectively.
   * 
   * Because the offscreen pixmap may have to be copied with an offset to the
   * onscreen-window we must reset the clip mask to account for the offset of
   * the onscreen-window (via XSetClipOrigin()). Alternately we can just clear
   * the clip mask (via XSetClipMask(TANGO__data->display, TANGO_gc(), None)) and tell
   * XCopyArea() to copy the rectangle corresponding to the damaged region
   * (damage_x, damage_y, damage_height, damage_width) to the onscreen-window
   * (damage_x+x_window_offset, damage_y+y_window_offset, damage_height, damage_width).
   * Since I can twiddle the params to XCopyArea() to only copy the required part
   * of the offscreen pixmap to the onscreen-window region, I opt not to set 
   * a special clip region for the visible onscreen-window.
   * 
   * The clip region specified by TANGO__data->damage_x, TANGO__data->damage_y,
   * TANGO__data->damage_width, and TANGO__data->damage_height, is set via
   * XSetClipRectangles() in TANGO_damage_clear_screen() (in TANGOtrans_perform()) 
   * or TANGO_clear_screen() (in TANGO_refresh()).
   */
  if ((TANGO__data->x_window_offset != 0) || (TANGO__data->y_window_offset != 0)) {
    XSetClipMask(TANGO__data->display, TANGO_gc(), None);
    XCopyArea(TANGO__data->display, TANGO__data->pixmap,
	      TANGO__data->display_window, TANGO_gc(),
	      TANGO__data->damage_x, TANGO__data->damage_y, /* XCopyArea() src_x, src_y */
	      TANGO__data->damage_width, TANGO__data->damage_height, /* XCopyArea() width, height */
	      TANGO__data->damage_x + TANGO__data->x_window_offset, /* XCopyArea() dest_x */
	      TANGO__data->damage_y + TANGO__data->y_window_offset); /* XCopyArea() dest_y */
  }
  else {
    /* NPM: When 'TANGO__data->display_window' is a normal drawing area ... */
    XCopyArea(TANGO__data->display, TANGO__data->pixmap,
	      TANGO__data->display_window, TANGO_gc(),
	      TANGO__data->damage_x, TANGO__data->damage_y,
	      TANGO__data->damage_width, TANGO__data->damage_height,
	      TANGO__data->damage_x, TANGO__data->damage_y);
  }
#else /* !defined(WINTERP) */
   XCopyArea(TANGO__data->display, TANGO__data->pixmap, 
             TANGO__data->display_window, TANGO_gc(),  
	     TANGO__data->damage_x, TANGO__data->damage_y,
	     TANGO__data->damage_width, TANGO__data->damage_height,
	     TANGO__data->damage_x, TANGO__data->damage_y);
#endif /* WINTERP */
}

/**************************************************************/
/*****************    end of xtangoanim.c    ******************/
/**************************************************************/
