/*
 * $RCSfile: TextView.c,v $
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

#include "Text.h"

static void doScroll();

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
    ** then offset the origin and copy the area
    */
    if (deltaX == 0 && deltaY == 0) return;

    if (!AppData.scrolling) {
	doScroll(deltaX, deltaY);
	XCopyArea(XtDisplay(draw), AppData.buf,
		  XtWindow(draw), AppData.gc, 0, 0,
		  AppData.drawingWidth, AppData.drawingHeight, 0, 0);
    }

    /*
    ** Count the scrolling operation
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

static void doScroll(deltaX, deltaY)
    int deltaX, deltaY;
{
    /*
    ** Set the PS origin in the X window to the new settings of 
    ** the scrollbars
    */
    AppData.originX -= deltaX;
    AppData.originY -= deltaY;

    /*
    ** Update the graphics state for the buffer to reflect new origin
    */
    setOrigin();

    /*
    ** Update the stored position of the scroll bars
    */
    AppData.scrollX += deltaX;
    AppData.scrollY += deltaY;

    /*
    ** Copy visible area to new location
    */
    XCopyArea(XtDisplay(AppData.drawingArea), AppData.buf,
	      AppData.buf, AppData.gc,
	      deltaX, deltaY, AppData.drawingWidth,
	      AppData.drawingHeight, 0, 0);

    /*	
    ** Set view clip to fix-up area and redisplay
    */
    setViewClip(deltaX, deltaY);
    drawSelf();
    PSinitviewclip();
}

/***************************************************************
**
** FUNCTION:    graphicExpose
**
** DESCRIPTION: Callback routine that handles GraphicsExpose and
**		NoExpose events.  We get one for each copy into the window
**		from the pixmap, so they should always be NoExpose events.
**		Use this to check if we need to scroll any more.  Not
**		handling further scroll callbacks until the event arrives
**		makes scrolling much more responsive.
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

    /* Convert ix, iy into X units */

    ix *= ((float) AppData.scaledWidth) / PAGE_WIDTH;
    iy *= ((float) AppData.scaledHeight) / PAGE_HEIGHT;

    if (AppData.drawingWidth >= AppData.scaledWidth) {
	/*
	** The scaled width is narrower than the view window, so
	** center the picture and set scroll bar to be unscrollable
	*/
	xoff = ((int) AppData.drawingWidth - (int) AppData.scaledWidth) / 2.0;
	scrollX = 0;
	hSize = AppData.scaledWidth;
    } else {
	/*
	** The scaled width is larger than the view window, so
	** turn on the scroll bar, and set up its maximum and
	** slider size.  Do this by converting the image offset into X
	** coordinates and subtracting the view offset
	*/
	scrollX = ix - vx;
	scrollX = MAX(scrollX, 0);
	scrollX = MIN(scrollX,
		      (int) AppData.scaledWidth - (int) AppData.drawingWidth);
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
    if (AppData.drawingHeight >= AppData.scaledHeight) {
	yoff = ((int) AppData.drawingHeight - (int) AppData.scaledHeight) /
		2.0;
	scrollY = (int) AppData.scaledHeight - (int) AppData.drawingHeight;
	vSize = AppData.scaledHeight;
	vy = AppData.drawingHeight / 2;
    } else {
	scrollY = iy + vy - (int) AppData.drawingHeight;
	scrollY = MAX(scrollY, 0);
	scrollY = MIN(scrollY,
		     (int) AppData.scaledHeight - (int) AppData.drawingHeight);
	vSize = AppData.drawingHeight;
	yoff = -(int) (scrollY + 0.5);
    }

    /*
    ** Update the scrollbars
    */
    AppData.scrollX = scrollX; 
    AppData.scrollY = (int) AppData.scaledHeight -
	    (int) AppData.drawingHeight - scrollY;

    XtVaSetValues(AppData.hScroll, XmNmaximum, (int) AppData.scaledWidth,
		  XmNvalue, AppData.scrollX, XmNsliderSize, hSize, NULL);
    XtVaSetValues(AppData.vScroll, XmNmaximum, (int) AppData.scaledHeight,
		  XmNvalue, AppData.scrollY, XmNsliderSize, vSize, NULL); 
    
    /*
    ** Set the PS origin in the X window to reflect the new 
    ** context location, and save the new offsets
    */
    AppData.originX = xoff;
    AppData.originY = (int) AppData.drawingHeight - yoff;
    setOrigin();

    PSinitclip();
    PSinitviewclip();
} /* end positionDrawingArea() */

/***************************************************************
**
** FUNCTION:    scaleDrawingArea 
**
** DESCRIPTION: Routine to scale the coordinate system and
**		and position it so that the point (ix, iy) on the image,
**		in PS coordinates, corresponds to the point (vx, vy)
**		on the visible window, in X coords.
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

void scaleDrawingArea (ix, iy, vx, vy)
    float ix;
    float iy;
    int	vx;
    int	vy;
{
    float	delta;
    XPoint	xpoint;
    Point	point;
    float	width, height;
    
    /*
    ** Change the scale of the DPS context by the new/old factor
    */
    delta = (((float) AppData.magnify) / 100.0) / AppData.scale;
    AppData.scale = ((float) AppData.magnify) / 100.0;

    PSscale(delta, delta);
    
    /*
    ** Compute the user space height and width of the window
    */
    PSWGetTransform(AppData.ctm, AppData.invctm,
		    &AppData.xOffset, &AppData.yOffset);

    xpoint.x = AppData.drawingWidth;
    xpoint.y = 0;
    convertToDPS(&xpoint, &point);
    width = point.x;
    height = point.y;

    xpoint.x = 0;
    xpoint.y = AppData.drawingHeight;
    convertToDPS(&xpoint, &point);
    width -= point.x;
    height -= point.y;

    /*
    ** Compute how large a window would be required to show the whole page
    */
    AppData.scaledWidth = PAGE_WIDTH * (int) AppData.drawingWidth / width;
    AppData.scaledHeight = PAGE_HEIGHT * (int) AppData.drawingHeight / height;

    /* 
    ** Position the drawing area at the same relative position.
    */
    positionDrawingArea(ix, iy, vx, vy);
} /* end scaleDrawingArea() */
