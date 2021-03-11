/*
 * $RCSfile: HitView.c,v $
 *
 * (c) Copyright 1992-1994 Adobe Systems Incorporated.
 * All rights reserved.
 * 
 * Permission to use, copy, modify, distribute, and sublicense this software
 * and its documentation for any purpose and without fee is hereby granted,
 * provided that the above copyright notices appear in all copies and that
 * both those copyright notices and this permission notice appear in
 * supporting documentation and that the name of Adobe Systems Incorporated
 * not be used in advertising or publicity pertaining to distribution of the
 * software without specific, written prior permission.  No trademark license
 * to use the Adobe trademarks is hereby granted.  If the Adobe trademark
 * "Display PostScript"(tm) is used to describe this software, its
 * functionality or for any other purpose, such use shall be limited to a
 * statement that this software works in conjunction with the Display
 * PostScript system.  Proper trademark attribution to reflect Adobe's
 * ownership of the trademark shall be given whenever any such reference to
 * the Display PostScript system is made.
 * 
 * ADOBE MAKES NO REPRESENTATIONS ABOUT THE SUITABILITY OF THE SOFTWARE FOR
 * ANY PURPOSE.  IT IS PROVIDED "AS IS" WITHOUT EXPRESS OR IMPLIED WARRANTY.
 * ADOBE DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING ALL
 * IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 * NON- INFRINGEMENT OF THIRD PARTY RIGHTS.  IN NO EVENT SHALL ADOBE BE LIABLE
 * TO YOU OR ANY OTHER PARTY FOR ANY SPECIAL, INDIRECT, OR CONSEQUENTIAL
 * DAMAGES OR ANY DAMAGES WHATSOEVER WHETHER IN AN ACTION OF CONTRACT,
 * NEGLIGENCE, STRICT LIABILITY OR ANY OTHER ACTION ARISING OUT OF OR IN
 * CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.  ADOBE WILL NOT
 * PROVIDE ANY TRAINING OR OTHER SUPPORT FOR THE SOFTWARE.
 * 
 * Adobe, PostScript, and Display PostScript are trademarks of Adobe Systems
 * Incorporated which may be registered in certain jurisdictions
 * 
 * Author:  Adobe Systems Incorporated
 */

/***************************************************************
**
**	INCLUDES
**
***************************************************************/

#include "Hit.h"

/***************************************************************
**
** FUNCTION:    setOrigin
**
** DESCRIPTION: Sets the origin to where it should by using translation and
**		updates the saved transformation matrices.  Re-initializes
**		the matrix and sets it to avoid cumulative roundoff error
**
** PARAMETERS:  None
**
** RETURN:      None.
**
***************************************************************/

void setOrigin()
{
    Point pt;
    XPoint xpt;

    xpt.x = AppData.originX;
    xpt.y = AppData.originY;
    convertToOrigDPS(&xpt, &pt);
    PSWSetMatrixAndGetTransform(pt.x, pt.y, AppData.scale,
				AppData.originX, AppData.originY,
				AppData.ctm, AppData.invctm,
				&AppData.xOffset, &AppData.yOffset);
} /* setOrigin() */

/***************************************************************
**
** FUNCTION:    scrollProc 
**
** DESCRIPTION: Callback routine for hitting scroll bars
**
** PARAMETERS:  w           callback widget ID
**              clientData  callback client data
**              callData    callback Motif data structure
**
** RETURN:      None.
**
***************************************************************/

/* ARGSUSED */

void scrollProc (w, clientData, callData)
    Widget w;
    XtPointer clientData, callData;
{
    int		x, y;
    int		deltaX, deltaY;
    Widget	draw = AppData.drawingArea;

    /*
    ** Get the new values of the scroll bars
    */
    XtVaGetValues(AppData.hScroll, XmNvalue, &x, NULL);
    XtVaGetValues(AppData.vScroll, XmNvalue, &y, NULL);

    /*
    ** Calculate the delta in the scrolling
    */
    deltaX = x - AppData.scrollX;
    deltaY = y - AppData.scrollY;

    /*
    ** If we are not already scrolling, and there is some scrolling to do,
    ** then scroll the pixmap and copy the pixmap to the window
    */
    if (deltaX == 0 && deltaY == 0) return;

    if (!AppData.scrolling) {
	doScroll(deltaX, deltaY);
	XCopyArea(XtDisplay(draw), AppData.original,
		  XtWindow(draw), AppData.gc, 0, 0,
		  AppData.drawingWidth, AppData.drawingHeight, 0, 0);
    }

    /*
    ** Set the scrolling flag; future scrolls will not update the window
    ** until the current one is done
    */
    AppData.scrolling = True;
} /* end scrollProc() */

/***************************************************************
**
** FUNCTION:    setViewClip
**
** DESCRIPTION: Set the view clip for scroll updating
**
** PARAMETERS:  deltaX	X displacement
**		deltaY	Y displacement
**
** RETURN:      None.
**
***************************************************************/

static void setViewClip(deltaX, deltaY)
    int deltaX, deltaY;
{
    float r[8];
    XPoint xll, xur;
    Point ll, ur;

    /*
    ** If one of the deltas is 0, then the area to update is just a
    ** single rectangle.
    */
    if (deltaX == 0 || deltaY == 0) {
	if (deltaX == 0) {
	    /*
	    ** Just a single horizontal rectangle
	    */
	    xll.x = 0;
	    xur.x = AppData.drawingWidth;
	    if (deltaY > 0) {
		xll.y = AppData.drawingHeight;
		xur.y = AppData.drawingHeight - deltaY;
	    } else {
		xll.y = -deltaY;
		xur.y = 0;
	    }

	} else if (deltaY == 0) {
	    /*
	    ** Just a single vertical rectangle
	    */
	    xll.y = AppData.drawingHeight;
	    xur.y = 0;
	    if (deltaX > 0) {
		xll.x = AppData.drawingWidth - deltaX;
		xur.x = AppData.drawingWidth;
	    } else {
		xll.x = 0;
		xur.x = -deltaX;
	    }
	}
	/*
	** Convert the rectangle into PS coordinates, and set the view clip
	*/
	convertToDPS(&xll, &ll);
	convertToDPS(&xur, &ur);
	PSrectviewclip(ll.x, ll.y, ur.x - ll.x, ur.y - ll.y);

    } else {
	/*
	** Scrolling in both areas, so there are two rectangles.  It's
	** easiest to do if we let them overlap; fortunately that is
	** legal!  First do the horizontal rectangle.
	*/
	xll.x = 0;
	xur.x = AppData.drawingWidth;
	if (deltaY > 0) {
	    xll.y = AppData.drawingHeight;
	    xur.y = AppData.drawingHeight - deltaY;
	} else {
	    xll.y = -deltaY;
	    xur.y = 0;
	}
	convertToDPS(&xll, &ll);
	convertToDPS(&xur, &ur);

	/*
	** Store in point array
	*/
	r[0] = ll.x;
	r[1] = ll.y;
	r[2] = ur.x - ll.x;
	r[3] = ur.y - ll.y;

	/*
	** Now do vertical rectangle and store in point array
	*/
	xll.y = AppData.drawingHeight;
	xur.y = 0;
	if (deltaX > 0) {
	    xll.x = AppData.drawingWidth - deltaX;
	    xur.x = AppData.drawingWidth;
	} else {
	    xll.x = 0;
	    xur.x = -deltaX;
	}
	convertToDPS(&xll, &ll);
	convertToDPS(&xur, &ur);
	r[4] = ll.x;
	r[5] = ll.y;
	r[6] = ur.x - ll.x;
	r[7] = ur.y - ll.y;

	/*
	** Set the view clip to the two rectangles
	*/
	PSWSetRectViewClip(r, 8);
    }
}

/***************************************************************
**
** FUNCTION:    doScroll
**
** DESCRIPTION: Scroll the drawing area by some delta
**
** PARAMETERS:  deltaX	X displacement
**		deltaY	Y displacement
**
** RETURN:      None.
**
***************************************************************/

void doScroll(deltaX, deltaY)
    int deltaX, deltaY;
{
    /*
    ** Set the PS origin in the X window to the new settings of 
    ** the scrollbars
    */
    AppData.originX -= deltaX;
    AppData.originY -= deltaY;

    /*
    ** Update the graphics states for the two buffers to reflect new origin
    */
    (void) XDPSSetContextGState(AppData.dpsCtxt, AppData.compGState);
    setOrigin();
    (void) XDPSUpdateContextGState(AppData.dpsCtxt, AppData.compGState);

    (void) XDPSSetContextGState(AppData.dpsCtxt, AppData.origGState);
    setOrigin();
    (void) XDPSUpdateContextGState(AppData.dpsCtxt, AppData.origGState);

    /*
    ** Update the stored position of the scroll bars
    */
    AppData.scrollX += deltaX;
    AppData.scrollY += deltaY;

    /*
    ** Either redraw the whole thing or copy and fill in
    */

    if (AppData.copyAll) drawSelf();
    else {
	/*
	** Copy visible area to new location
	*/
	XCopyArea(XtDisplay(AppData.drawingArea), AppData.original,
		  AppData.original, AppData.gc,
		  deltaX, deltaY, AppData.drawingWidth,
		  AppData.drawingHeight, 0, 0);
	/*
	** Set view clip to fix-up area and redisplay
	*/
	setViewClip(deltaX, deltaY);
	drawSelf();
	PSinitviewclip();
    }
}

/***************************************************************
**
** FUNCTION:    graphicExpose
**
** DESCRIPTION: Callback routine that handles GraphicsExpose and
**		NoExpose events.  We get one for each copy into the window
**		from the pixmap, so they should always be NoExpose events.
**		When we get one, call scrollProc to see if we need to
**		scroll any more.  Not handling further scroll callbacks
**		until the event arrives makes scrolling much more responsive.
**
** PARAMETERS:	w		window widget
**		clientData	clientdata
**		event		event information
**		goOn		continue to dispatch
**
** RETURN:      None.
**
***************************************************************/

/* ARGSUSED */

void graphicExpose (w, clientData, event, goOn)
    Widget 	w;
    XtPointer clientData;
    XEvent	*event;
    Boolean	*goOn;
{
    if (AppData.scrolling) {
	AppData.scrolling = False;
	scrollProc(w, NULL, NULL);
    }
} /* end graphicExpose() */

/***************************************************************
**
** FUNCTION:    positionDrawingArea 
**
** DESCRIPTION: Routine to position the origin so that the
**		point (ix, iy) on the image, in PS coordinates,
**		corresponds to the point (vx, vy) on the visible 
**		window, in X coords.
**
** PARAMETERS:
**	ix	x coord of point on DPS context (PS coordinates)
**	iy	y coord of point on DPS context (PS coordinates)
**	vx	x coors of point on drawing area (X coordinates)
**	vy	y coors of point on drawing area (X coordinates)
**
** RETURN:      None.
**
***************************************************************/

void positionDrawingArea (ix, iy, vx, vy)
    float	ix;
    float	iy;
    int		vx;
    int		vy;
{
    int		xoff, yoff;
    int 	hSize, vSize;
    float	scrollX, scrollY;

    AppData.desktop = FALSE;

    /* Convert ix, iy into X units */

    ix *= ((float) AppData.scaledWidth) / PAGE_WIDTH;
    iy *= ((float) AppData.scaledHeight) / PAGE_HEIGHT;

    if ((int) AppData.drawingWidth >= AppData.scaledWidth) {
	/*
	** The scaled width is narrower than the view window, so
	** center the picture and set scroll bar to be unscrollable
	*/
	xoff = ((int) AppData.drawingWidth - AppData.scaledWidth) / 2.0;
	scrollX = 0;
	hSize = AppData.scaledWidth;
	AppData.desktop = TRUE;
    } else {
	/*
	** The scaled width is larger than the view window, so
	** turn on the scroll bar, and set up its maximum and
	** slider size.  Do this by subtracting the view offset from the
	** image offset.
	*/
	scrollX = ix - vx;
	scrollX = MAX(scrollX, 0);
	scrollX = MIN(scrollX,
		      AppData.scaledWidth - (int) AppData.drawingWidth);
	hSize = AppData.drawingWidth;
	xoff = -(int) (scrollX + 0.5);
    }

    /*
    ** Now do the same thing for the height.  We want to compute the offset
    ** relative to the lower left corner, but X coordinates are relative
    ** to the upper left, so the drawing height must be added in.  Also, since
    ** the coordinates go in the other direction, the view offset must be
    ** added, not subtracted.
    */
    if ((int) AppData.drawingHeight >= AppData.scaledHeight) {
	yoff = ((int) AppData.drawingHeight - AppData.scaledHeight) / 2.0;
	scrollY = AppData.scaledHeight - (int) AppData.drawingHeight;
	vSize = AppData.scaledHeight;
	vy = AppData.drawingHeight / 2;
	AppData.desktop = TRUE;
    } else {
	scrollY = iy + vy - (int) AppData.drawingHeight;
	scrollY = MAX(scrollY, 0);
	scrollY = MIN(scrollY,
		      AppData.scaledHeight - (int) AppData.drawingHeight);
	vSize = AppData.drawingHeight;
	yoff = -(int) (scrollY + 0.5);
    }

    /*
    ** Update the scrollbars
    */
    AppData.scrollX = (int) (scrollX + 0.5); 
    AppData.scrollY = (int) (AppData.scaledHeight -
			     (int) AppData.drawingHeight - scrollY + 0.5);

    XtVaSetValues(AppData.hScroll, XmNmaximum, AppData.scaledWidth,
		  XmNvalue, AppData.scrollX, XmNsliderSize, hSize, NULL);
    XtVaSetValues(AppData.vScroll, XmNmaximum, AppData.scaledHeight,
		  XmNvalue, AppData.scrollY, XmNsliderSize, vSize, NULL); 
    
    /*
    ** Store the new origin and update the gstates to use this origin
    */
    AppData.originX = xoff;
    AppData.originY = AppData.drawingHeight - yoff;

    (void) XDPSSetContextGState(AppData.dpsCtxt, AppData.compGState);
    setOrigin();
    (void) XDPSUpdateContextGState(AppData.dpsCtxt, AppData.compGState);

    (void) XDPSSetContextGState(AppData.dpsCtxt, AppData.origGState);
    setOrigin();
    (void) XDPSUpdateContextGState(AppData.dpsCtxt, AppData.origGState);
} /* end positionDrawingArea() */

/***************************************************************
**
** FUNCTION:    scaleDrawingArea 
**
** DESCRIPTION: Recomputes the scaled drawing size and updates the font
**
** PARAMETERS:  None
**
** RETURN:      None.
**
***************************************************************/

void scaleDrawingArea ()
{
    AppData.scale = (float) AppData.magnify / 100.0;
    
    /*
    ** Compute the new dimensions that would be needed to hold the entire
    ** page at the new scale factor
    */
    AppData.scaledWidth = PAGE_WIDTH * AppData.origXScale * AppData.scale;
    AppData.scaledHeight = PAGE_HEIGHT * AppData.origYScale * AppData.scale;

    /*
    ** Update the saved gstates to make the current font be inversely
    ** scaled.  This keeps the control points from becoming huge or
    ** tiny.
    */
    (void) XDPSSetContextGState(AppData.dpsCtxt, AppData.compGState);
    PSselectfont(FontName, CtlPtSize / AppData.scale);
    (void) XDPSUpdateContextGState(AppData.dpsCtxt, AppData.compGState);

    (void) XDPSSetContextGState(AppData.dpsCtxt, AppData.origGState);
    PSselectfont(FontName, CtlPtSize / AppData.scale);
    (void) XDPSUpdateContextGState(AppData.dpsCtxt, AppData.origGState);
} /* end scaleDrawingArea() */
