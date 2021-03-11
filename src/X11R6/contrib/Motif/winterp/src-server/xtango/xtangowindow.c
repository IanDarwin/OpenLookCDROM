/* -*-C-*-
*******************************************************************************
*
* File:         xtangowindow.c
* RCS:          $Header: /disk3/npm/src/winterp/src-server/xtango/RCS/xtangowindow.c,v 2.8 1994/06/09 01:30:51 npm Exp $
* Description:  XTANGO ANIMATION PACKAGE (window)
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
static char rcs_identity[] = "@(#)$Header: /disk3/npm/src/winterp/src-server/xtango/RCS/xtangowindow.c,v 2.8 1994/06/09 01:30:51 npm Exp $";

/* Modification Date  Description			      */
/* -----------------  --------------------------------------- */
/* 90/10/26 JDH	      Baselined source.                       */
/*							      */
/**************************************************************/

/**************************************************************/
/*****************	 include files       ******************/ 
/**************************************************************/

#include "xtangolocal.h"

/**************************************************************/
/*****************	LOCAL data types     ******************/
/**************************************************************/

/**************************************************************/
/*****************	LOCAL variables      ******************/
/**************************************************************/

/**************************************************************/
/*****************      LOCAL functions      ******************/
/**************************************************************/

void animate_begin();


/**************************************************************/
/* TANGOset_bgcolor -- Sets the background color of the       */
/*                        XTango window                       */
/* 							      */
/* RETURNS:  None.                                            */
/**************************************************************/
void
TANGOset_bgcolor(name)
   char *name;
{
   TANGO_COLOR col;

   col = TANGOload_color(name);
   TANGO__data->bgcolor = col;
   TANGO_refresh();
}
   

#ifndef WINTERP			/* prevent linker error w/ unresolved ref to xtangomotif.c:TANGO_layout_window() */
/**************************************************************/
/* TANGO_setup_windows -- Open X display, decide whether      */
/*			  color or mono screen, and init all  */
/*			  X stuff.			      */
/* 							      */
/* RETURNS:  None.					      */
/**************************************************************/
void
TANGO_setup_windows()
{
   Visual     *vis;
   int        lx,by,rx,ty;

   TANGO__data = (ANIMATION_PTR) malloc( sizeof( struct ANIMATION) );

   TANGO__data->display = XOpenDisplay("");
   if (!TANGO__data->display) {
      COMPLAIN("ERROR: TANGO_setup_windows: unable to open X display\n");
      exit(1);
      }

   vis = DefaultVisual(TANGO__data->display, 
                       DefaultScreen(TANGO__data->display));
   TANGO__data->color_screen = (vis->class != GrayScale && 
                                vis->class != StaticGray);

   TANGO__data->pixmap = NULL;  /* needed to short-circuit initial attempt */
                                /* at refreshing window                    */
   TANGO__data->debug = 0;

   TANGO_initGC();

   TANGO_layout_window();

   animate_begin();
}
#endif /* !defined(WINTERP) */



/**************************************************************/
/* TANGO_flush -- Send any queued X events to the server.     */
/* 							      */
/* RETURNS:  None.					      */
/**************************************************************/
void
TANGO_flush()
{
   XFlush(TANGO__data->display);
}



#ifndef WINTERP			/* NPM: Not used in xtango 1.52 */
/**************************************************************/
/* TANGO_batch_mode -- Explicitly flush or hold X events      */
/*		       (well, almost...)		      */
/* 							      */
/* RETURNS:  None.					      */
/**************************************************************/
void
TANGO_batch_mode(fg)
   int fg;
{
   static int batch_level = 0;

   if (fg)
      ++batch_level;
   else if (batch_level > 0) {
      --batch_level;
      if (batch_level == 0)
	 XFlush(TANGO__data->display);
      }
}
#endif /* !defined(WINTERP) */



/**************************************************************/
/* animate_begin -- This routine is used to set up the TANGO  */
/*		    animation window and package.  The	      */
/*		    routine receives the animation window,    */
/*		    its parent window, plus the user-         */
/*		    specified, real-valued window boundary    */
/*		    coords.  The routine initializes all the  */
/*		    fields of the main, GLOBAL animation data */
/*		    structure, TANGO__data.		      */
/* 							      */
/* RETURNS:  None.					      */
/**************************************************************/
void
animate_begin()
{
   TANGO__data->bgcolor = TANGO_COLOR_WHITE;
   TANGO__data->width = 0;	      /* Width & height init'd in anim_setup */
   TANGO__data->height = 0;	      /*    (user may resize window)         */
   TANGO__data->damlx = TANGO__data->rx = 1.0;
   TANGO__data->damrx = TANGO__data->lx = 0.0;
   TANGO__data->damty = TANGO__data->by = 1.0;
   TANGO__data->damby = TANGO__data->ty = 0.0;
   TANGO__data->mono_fillstyle = 0;    /* Patterns represent colors */
   TANGO__data->delay = 0;

#ifdef WINTERP
   TANGO__data->anim_event_mask = ExposureMask|KeyPressMask; /* see TANGO_check_X_events() */
#else /* !defined(WINTERP) */
   /* Start paused--don't lose initial x stuff */
   /*    (before window/widgets are displayed) */
   TANGO__data->paused = 1;	
#endif /* WINTERP */

   TANGO__data->motionblur = 0;
   TANGO__data->confighead = NULL;
   TANGO__data->configtail = NULL;

   TANGO_anim_setup(TANGO__data->easel);

   /* Wait until widgets are displayed */
   TANGO_flush();
   TANGO_check_X_events();
}

/**************************************************************/
/*****************   end of xtangowindow.c   ******************/
/**************************************************************/
