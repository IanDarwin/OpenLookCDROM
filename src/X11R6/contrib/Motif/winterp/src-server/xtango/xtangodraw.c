/* -*-C-*-
*******************************************************************************
*
* File:         xtangodraw.c
* RCS:          $Header: /disk3/npm/src/winterp/src-server/xtango/RCS/xtangodraw.c,v 2.7 1994/06/09 01:25:28 npm Exp $
* Description:  XTANGO ANIMATION PACKAGE (draw)
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
static char rcs_identity[] = "@(#)$Header: /disk3/npm/src/winterp/src-server/xtango/RCS/xtangodraw.c,v 2.7 1994/06/09 01:25:28 npm Exp $";

/* Modification Date  Description			      */
/* -----------------  --------------------------------------- */
/* 90/10/26 JDH	      Baselined source.                       */
/*							      */
/**************************************************************/

/**********************************************************/
/* All functions draw to the current offscreen Pixmap in  */
/* TANGO__data->pixmap and use the current setting in the */
/* single graphics context stored locally in xtangogc.c.  */
/**********************************************************/

/**************************************************************/
/*****************	 include files       ******************/ 
/**************************************************************/

#include "xtangolocal.h"

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

/**************************************************************/
/* TANGO_Line -- Draw line from (x1,y1) to (x2,y2).	      */
/*							      */
/* RETURNS:  None.					      */
/**************************************************************/
void
TANGO_Line(x1, y1, x2, y2)
   int x1,y1,x2,y2;
{
   XSetFillStyle(TANGO__data->display, TANGO_gc(), FillSolid);
   XDrawLine(TANGO__data->display, TANGO__data->pixmap, 
             TANGO_gc(), x1, y1, x2, y2);
}



/**************************************************************/
/* TANGO_Rectangle -- Draw rectangle from (lx,ty) to (rx,by). */
/*		      (top left corner & bottom right corner) */
/*							      */
/* RETURNS:  None.					      */
/**************************************************************/
void
TANGO_Rectangle(lx, ty, rx, by)
   int lx, ty, rx, by;
{
   int width  = rx - lx - 1;
   int height = by - ty - 1;
   XSetFillStyle(TANGO__data->display, TANGO_gc(), FillSolid);
   XDrawRectangle(TANGO__data->display, TANGO__data->pixmap, TANGO_gc(), 
                  lx, ty, 
                  (unsigned int) ((width >= 0) ? width : 0),
                  (unsigned int) ((height >= 0) ? height : 0));
}



/**************************************************************/
/* TANGO_Fill_Rectangle -- Draw rectangle from (lx,ty) to     */
/*			   (rx,by) using current fill style.  */
/*		           (top left corner & bottom right    */
/*			    corner)			      */
/*							      */
/* RETURNS:  None.					      */
/**************************************************************/
void
TANGO_Fill_Rectangle(lx, ty, rx, by)
   int lx, ty, rx, by;
{
   XFillRectangle(TANGO__data->display, TANGO__data->pixmap, 
                  TANGO_gc(), lx, ty,  
                  (unsigned int) (rx-lx), (unsigned int) (by-ty));
}



/**************************************************************/
/* TANGO_Ellipse -- Draw ellipse centered at (cx,cy) with     */
/*		    radii radx and rady.		      */
/*							      */
/* RETURNS:  None.					      */
/**************************************************************/
void
TANGO_Ellipse(cx, cy, radx, rady)
   int cx, cy, radx, rady;
{
   XSetFillStyle(TANGO__data->display, TANGO_gc(), FillSolid);
   XDrawArc(TANGO__data->display, TANGO__data->pixmap, TANGO_gc(),
	    cx-radx, cy-rady, 
            (unsigned int) radx<<1, (unsigned int) rady<<1, 0, 360*64);
}
 


/**************************************************************/
/* TANGO_Fill_Ellipse -- Draw filled ellipse centered at      */
/*			 (cx,cy) with radii radx and rady     */
/*			 using current fill style.	      */
/*							      */
/* RETURNS:  None.					      */
/**************************************************************/
void
TANGO_Fill_Ellipse(cx, cy, radx, rady)
   int cx, cy, radx, rady;
{
   XFillArc(TANGO__data->display, TANGO__data->pixmap, TANGO_gc(),
	    cx-radx, cy-rady, 
            (unsigned int) radx<<1, (unsigned int) rady<<1, 0, 360*64);
}
 


/**************************************************************/
/* TANGO_Polyline -- Draw polyline with numpts vertices.      */
/*							      */
/* Used with permission from Steven P. Reiss -- Brown Univ.   */
/*		    ashdraw.c: draw_pointset()		      */
/*							      */
/* RETURNS:  None.					      */
/**************************************************************/
void
TANGO_Polyline(numpts, x, y)
   int		numpts;
   register int *x, *y;
{
   XPoint       *npts;
   register int i;

   if (numpts <= 0) return;
   npts = (XPoint *) malloc(sizeof(XPoint)*(numpts+1));

   for (i=0; i<numpts; i++) {
      npts[i].x = (short) x[i];
      npts[i].y = (short) y[i];
      }

   XSetFillStyle(TANGO__data->display, TANGO_gc(), FillSolid);
   XDrawLines(TANGO__data->display, TANGO__data->pixmap, TANGO_gc(),
	       npts, numpts, CoordModeOrigin);

   free(npts);
}



/**************************************************************/
/* TANGO_Polygon -- Draw filled polygon with numpts vertices  */
/*		    using current fill style.		      */
/*							      */
/* RETURNS:  None.					      */
/**************************************************************/
void
TANGO_Polygon(numpts, x, y)
   int numpts, x[], y[];
{
   XPoint pts[10];	/* Max 10 points */
   int    i;

   for (i=0; i<numpts; i++) {
      pts[i].x = (short) x[i];
      pts[i].y = (short) y[i];
      }

   XFillPolygon(TANGO__data->display, TANGO__data->pixmap, TANGO_gc(),
	        pts, numpts, Nonconvex, CoordModeOrigin);
}



/**************************************************************/
/* TANGO_Text -- Draw solid text.			      */
/*							      */
/* RETURNS:  None.					      */
/**************************************************************/
void
TANGO_Text(x, y, msg)
   int  x, y;
   char *msg;
{
   /* On mono machines only solid black or solid white is supported */
   XSetFillStyle(TANGO__data->display, TANGO_gc(), FillSolid);

   XDrawString(TANGO__data->display, TANGO__data->pixmap, TANGO_gc(),
	       x, y, msg, strlen(msg));
}

/**************************************************************/
/*****************     end of xtangodraw.c   ******************/
/**************************************************************/
