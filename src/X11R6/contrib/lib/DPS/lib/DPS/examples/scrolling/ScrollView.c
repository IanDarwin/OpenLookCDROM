/*
 * $RCSfile: ScrollView.c,v $
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
*****************************************************************/

#include "Scroll.h"
#include <X11/Xproto.h>

static void doScroll();

/***************************************************************
**
** FUNCTION:    setOrigin
**
** DESCRIPTION: Sets the origin of the context to the point that
**		corresponds to (AppData.originX, AppData.originY)
**		by translating user space.  To avoid cumulative
**		translation round-off error, always reinitialize the
**		coordinate system and translate and scale from there
**
** PARAMETERS:  None
**
** RETURN:      None.
**
***************************************************************/

static void setOrigin()
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

    /*
    ** Get the new values of the scroll bars
    */
    XtVaGetValues(AppData.selfHScroll, XmNvalue, &x, NULL);
    XtVaGetValues(AppData.selfVScroll, XmNvalue, &y, NULL);

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

    if (!AppData.scrolling) doScroll(deltaX, deltaY);

    /*
    ** Set the scrolling flag
    */
    AppData.scrolling = True;
} /* end scrollProc() */

/***************************************************************
**
** FUNCTION:    findViewClip
**
** DESCRIPTION: Create a clip list for scrolling updating
**
** PARAMETERS:  deltaX		X displacement
**		deltaY		Y displacement
**
** RETURN:      clipList	List of clipping rectangles
**		clipLen		Number of rectangles in clip
**
***************************************************************/

static void findViewClip(deltaX, deltaY, clipList, clipLen)
    int deltaX, deltaY;
    int **clipList;
    int *clipLen;
{
    static int r[8];
    int count;
    
    /*
    ** If one of the deltas is 0, then the area to update is just a
    ** single rectangle.
    */
    if (deltaX == 0 || deltaY == 0) {
	if (deltaX == 0) {
	    /*
	    ** Just a single horizontal rectangle
	    */
	    r[0] = 0;
	    r[2] = AppData.drawingWidth;
	    if (deltaY > 0) {
		r[1] = AppData.drawingHeight;
		r[3] = deltaY;
	    } else {
		r[1] = -deltaY;
		r[3] = -deltaY;
	    }

	} else if (deltaY == 0) {
	    /*
	    ** Just a single vertical rectangle
	    */
	    r[1] = AppData.drawingHeight;
	    r[3] = AppData.drawingHeight;
	    if (deltaX > 0) {
		r[0] = AppData.drawingWidth - deltaX;
		r[2] = deltaX;
	    } else {
		r[0] = 0;
		r[2] = -deltaX;
	    }
	}
	count = 1;

    } else {
	/*
	** Scrolling in both areas, so there are two rectangles.  It's
	** easiest to do if we let them overlap; fortunately that is
	** legal!  First do the horizontal rectangle.
	*/
	r[0] = 0;
	r[2] = AppData.drawingWidth;
	if (deltaY > 0) {
	    r[1] = AppData.drawingHeight;
	    r[3] = deltaY;
	} else {
	    r[1] = -deltaY;
	    r[3] = -deltaY;
	}

	/*
	** Now do vertical rectangle 
	*/
	r[5] = AppData.drawingHeight;
	r[7] = AppData.drawingHeight;
	if (deltaX > 0) {
	    r[4] = AppData.drawingWidth - deltaX;
	    r[6] = deltaX;
	} else {
	    r[4] = 0;
	    r[6] = -deltaX;
	}
	count = 2;
    }	
    *clipList = r;
    *clipLen = count;
} /* end findViewClip() */

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
    Drawable d;
    Widget	draw = AppData.currentDraw;
    int		*clipList;
    int		clipLen;

    /*
    ** Set the origin in the X window to the new settings of 
    ** the scrollbars
    */
    AppData.originX -= deltaX;
    AppData.originY -= deltaY;

    /*
    ** Update the stored position of the scroll bars
    */
    AppData.scrollX += deltaX;
    AppData.scrollY += deltaY;

    AppData.lastXdelta = deltaX;
    AppData.lastYdelta = deltaY;

    /*
    ** The drawable we are shifting is either the buffer or the window
    */
    if (AppData.scrollStrategy == scroll_self_buffer &&
	!AppData.showDrawing) d = AppData.buf;
    else d = XtWindow(draw);

    /*
    ** Copy visible area to new location
    */
    XCopyArea(XtDisplay(draw), d, d, AppData.gc,
	      deltaX, deltaY, AppData.drawingWidth,
	      AppData.drawingHeight, 0, 0);

    /*
    ** Adjust the origin to account for the scrolling.  If we're not buffering
    ** there's nothing more to do.  The GraphicsExpose events will cause
    ** the newly-exposed areas to be filled in.
    */
    setOrigin();
    if (AppData.scrollStrategy == scroll_self_redraw) return;

    /*	
    ** Set view clip to fix-up area and redisplay
    */
    findViewClip(deltaX, deltaY, &clipList, &clipLen);
    drawSelf(clipList, clipLen);

    /*
    ** Done drawing.  If not watching progress, copy the scrolled image
    ** into the window
    */
    if (!AppData.showDrawing) {
	XCopyArea(XtDisplay(draw), AppData.buf,
		  XtWindow(draw), AppData.gc, 0, 0,
		  AppData.drawingWidth, AppData.drawingHeight, 0, 0);
    }
} /* end doScroll() */

/***************************************************************
**
** FUNCTION:    graphicsExpose
**
** DESCRIPTION: Callback routine that handles GraphicsExpose and
**		NoExpose events.  If using buffered drawing, we get a
**		NoExpose event for each copy.  If not buffered, we get
**		GraphicsExpose events to indicate where we need to redraw
**		In either case, call scrollProc again to see if the scrollbars
**		have moved more since the scroll we're handling.  Not
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

void graphicsExpose (w, clientData, event, goOn)
    Widget 	w;
    XtPointer clientData;
    XEvent	*event;
    Boolean	*goOn;
{
    static int *bboxList = NULL;
    static int bboxLen = 0, bboxCount = 0;
    XExposeEvent *e = &event->xexpose;

    if (!AppData.scrolling) return;

    switch (event->type) {
	case GraphicsExpose:
	    /*
	    ** GraphicsExpose occur when unbuffered or watching progress
	    */
	    if (AppData.scrollStrategy != scroll_self_redraw) {
		/*
		** Watching progress; copy from buffer to be sure
		*/
		XCopyArea(XtDisplay(w), AppData.buf, XtWindow(w), AppData.gc,
			  e->x, e->y, e->width, e->height, e->x, e->y);
	    } else {
		addExposureToBBox(&bboxList, &bboxLen, &bboxCount, e);

		/*
		** If no more GraphicsExpose events, set view clip and redraw
		*/
		if (e->count == 0) {
		    drawSelf(bboxList, bboxCount/4);
		    bboxCount = 0;
		}
		if (e->count == 0) {
		    AppData.scrolling = False;
		    AppData.lastXdelta = AppData.lastYdelta = 0;
		    scrollProc(w, NULL, NULL);
		}
	    }
	    break;

	case NoExpose:
	    /*
	    ** NoExpose occur in many cases but are only relevant when
	    ** buffered and scrolling
	    */
	    if (AppData.scrollStrategy == scroll_self_buffer &&
		AppData.scrolling) {
		AppData.scrolling = False;
		AppData.lastXdelta = AppData.lastYdelta = 0;
		scrollProc(w, NULL, NULL);
	    }
	    break;
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

    if ((int) AppData.drawingWidth >= AppData.scaledWidth) {
	/*
	** The scaled width is narrower than the view window, so
	** center the picture and set scroll bar to be unscrollable
	*/
	xoff = ((int) AppData.drawingWidth - AppData.scaledWidth) / 2.0;
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

    if (AppData.scrollStrategy & SCROLL_AUTO) {
	if (xoff <= 0) xoff = 0;
	else hSize = AppData.drawingWidth;

	if (yoff <= 0) yoff = AppData.scaledHeight;
	else {
	    yoff = (int) AppData.drawingHeight - yoff;
	    vSize = AppData.drawingHeight;
	    AppData.scrollY = 0;
	}

	XmScrollBarSetValues(AppData.autoHScroll, AppData.scrollX,
			     hSize, 0, 0, True);
	XmScrollBarSetValues(AppData.autoVScroll, AppData.scrollY,
			     vSize, 0, 0, True);
    } else {
	yoff = AppData.drawingHeight - yoff;

	XtVaSetValues(AppData.selfHScroll,
		      XmNmaximum, (int) AppData.scaledWidth,
		      XmNvalue, AppData.scrollX, XmNsliderSize, hSize, NULL);
	XtVaSetValues(AppData.selfVScroll,
		      XmNmaximum, (int) AppData.scaledHeight,
		      XmNvalue, AppData.scrollY, XmNsliderSize, vSize, NULL);
    }
    
    /*
    ** Set the origin in the X window to reflect the new location
    */
    AppData.originX = xoff;
    AppData.originY = yoff;

    setOrigin();
} /* end positionDrawingArea() */

/***************************************************************
**
** FUNCTION:	setWindowSize
**
** DESCRIPTION:	Set the window size to the specified dimensions
**		If they are smaller than the current clip window,
**		use the size of the clip window instead
**
** PARAMETERS:  width, height	Window dimensions
**
** RETURN:      None.
**
***************************************************************/

void setWindowSize(width, height)
    Dimension width, height;
{
    XtVaGetValues(XtParent(AppData.currentDraw),
		  XtNwidth, &AppData.drawingWidth,
		  XtNheight, &AppData.drawingHeight, NULL);

    if (width < AppData.drawingWidth) width = AppData.drawingWidth;
    if (height < AppData.drawingHeight) height = AppData.drawingHeight;

    XtVaSetValues(AppData.currentDraw, XtNwidth, width,
		  XtNheight, height, NULL);
} /* setWindowSize() */

/***************************************************************
**
** FUNCTION:    getwindowSize
**
** DESCRIPTION: Updates the window size fields in the AppData structure
**
** PARAMETERS:  None
**
** RETURN:      None.
**
***************************************************************/

static void getWindowSize()
{
    XtVaGetValues(AppData.currentDraw, XtNwidth, &AppData.drawingWidth,
		  XtNheight, &AppData.drawingHeight, NULL);
} /* end getWindowSize() */

/***************************************************************
**
** FUNCTION:    scaleDrawingArea 
**
** DESCRIPTION: Recomputes the scaled drawing size.  If using auto
**		scrolling, resize the drawing area to this size
**
** PARAMETERS:  None
**
** RETURN:      None.
**
***************************************************************/

void scaleDrawingArea ()
{
    AppData.scale = (float) AppData.magnify / 100.0;
    
    AppData.scaledWidth = PAGE_WIDTH * AppData.origXScale * AppData.scale;
    AppData.scaledHeight = PAGE_HEIGHT * AppData.origYScale * AppData.scale;

    if (AppData.scrollStrategy & SCROLL_AUTO) {
	setWindowSize(AppData.scaledWidth, AppData.scaledHeight);
    }
} /* end scaleDrawingArea() */

/***************************************************************
**
** FUNCTION:    pixmapHandler
**
** DESCRIPTION: New error handler to detect pixmap allocation failure
**
** PARAMETERS:  dpy	Current display
**		error	Error event
**
** RETURN:      0
**
***************************************************************/

static Boolean pixmapError;
static int (*oldHandler)();

static int pixmapHandler(dpy, error)
    Display *dpy;
    XErrorEvent *error;
{
    if (error->error_code == BadAlloc &&
	error->request_code == X_CreatePixmap) {
	pixmapError = True;
	return 0;
    } else return (*oldHandler) (dpy, error);
} /* pixmapHandler() */

/***************************************************************
**
** FUNCTION:    allocPixmap
**
** DESCRIPTION: Allocate a pixmap of the desired size, but if
**		it would exceed the memory limit or if the allocation
**		fails, return None
**
** PARAMETERS:  dpy	The display
**		win	A window on the screen
**		w, h	The pixmap dimensions
**		d	Pixmap depth
**
** RETURN:      The pixmap, or None
**
***************************************************************/

static Pixmap allocPixmap(dpy, win, w, h, d)
    Display *dpy;
    Window win;
    unsigned int w, h, d;
{
    Pixmap p;
    unsigned int dBytes;

    dBytes = (d + 7) / 8;		/* Convert into bytes */
    if (w * h * dBytes > AppData.pixmapMaxSize) return None;

    XSync(dpy, False);
    oldHandler = XSetErrorHandler(pixmapHandler);
    pixmapError = False;
    p = XCreatePixmap(dpy, win, w, h, d);
    XSync(dpy, False);
    (void) XSetErrorHandler(oldHandler);
    if (pixmapError) return None;
    else return p;
} /* end allocPixmap() */

/***************************************************************
**
** FUNCTION:    initializePixmap
**
** DESCRIPTION: Free the old pixmap, if any, and allocate a new one
**
** PARAMETERS:  width, height	Size of pixmap
**
** RETURN:      None.
**
***************************************************************/

static void initializePixmap(width, height)
    int width, height;
{
    int depth;

    /*
    ** Free the old pixmap if it exists
    */
    if (AppData.buf != None) {
	XFreePixmap(XtDisplay(AppData.time), AppData.buf);
    }
    AppData.pixmapWidth = width;
    AppData.pixmapHeight = height;

    /*
    ** If new size is 0, return
    */
    if (width == 0) {
	AppData.buf = None;
	return;
    }

    /*
    ** New size must be at least as large as the drawing area
    */
    if (width < (int) AppData.drawingWidth) {
	width = AppData.pixmapWidth = AppData.drawingWidth;
    }
    if (height < (int) AppData.drawingHeight) {
	height = AppData.pixmapHeight = AppData.drawingHeight;
    }

    /*
    ** Find the drawing area depth and allocate the pixmap
    */
    XtVaGetValues(AppData.currentDraw, XtNdepth, &depth, NULL);
    AppData.buf = allocPixmap(XtDisplay(AppData.time),
			      XtWindow(AppData.currentDraw),
			      width, height, depth);
} /* end initializePixmap() */

/***************************************************************
**
** FUNCTION:    initializeContext
**
** DESCRIPTION: Set the context to use a drawable, and set up the
**		X offset and the coordinate system for drawing
**
** PARAMETERS:  drawable	New window or pixmap
**		x, y		X offset
**
** RETURN:      None.
**
***************************************************************/

static void initializeContext(drawable, x, y)
    Drawable drawable;
    int x, y;
{
    PSWSetContextDrawable(drawable, x, y);
    AppData.xOffset = x;
    AppData.yOffset = y;
} /* end initializeContext() */

/***************************************************************
**
** FUNCTION:    setupAndDrawAuto
**
** DESCRIPTION: Setup the context and redraw in auto-scrolling
**		mode.  Keep the ix, iy position in the drawing
**		at vx, vy in the window
**
** PARAMETERS:  center		Whether to center the image
**		ix, iy		Position in the drawing
**		vx, vy		Position in the window
**
** RETURN:      None.
**
***************************************************************/

static void setupAndDrawAuto(center, ix, iy, vx, vy)
    Boolean center;
    float ix, iy;
    int vx, vy;
{
    int area[4];
    Widget w = AppData.currentDraw;

    /*
    ** If the current drawing area is not the auto drawing area,
    ** unmanage the self-scrolling window and manage the auto-scrolling
    ** one.  Set the size of the auto-scrolled window to the current
    ** scaled width
    */
    if (AppData.currentDraw != AppData.autoDrawingArea) {
	XtUnmanageChild(AppData.selfScrolling);
	XtManageChild(AppData.autoScrolling);
	w = AppData.currentDraw = AppData.autoDrawingArea;
	setWindowSize(AppData.scaledWidth, AppData.scaledHeight);
    }

    if (center) {
	ix = PAGE_WIDTH / 2.0;
	iy = PAGE_HEIGHT / 2.0;
	vx = AppData.drawingWidth / 2;
	vy = AppData.drawingHeight / 2;
    } 

    if (AppData.scrollStrategy != scroll_auto_redraw) {
	/*
	** Create a backing pixmap.  If this fails, convert to
	** unbuffered drawing
	*/
	initializePixmap(AppData.scaledWidth, AppData.scaledHeight);
	if (AppData.buf == None) {
	    putUpInfoDialog(AppData.noAutoPixmapMessage);
	    if (AppData.scrollStrategy == scroll_background) {
		XSetWindowBackground(XtDisplay(w), XtWindow(w),
				     WhitePixelOfScreen(XtScreen(w)));
	    }
	    AppData.scrollStrategy = scroll_auto_redraw;
	    XtVaSetValues(AppData.currentStrategy, XmNset, False, NULL);
	    XtVaSetValues(AppData.autoRedraw, XmNset, True, NULL);
	    XtSetSensitive(AppData.watchFrame, False);
	}
    }

    if (AppData.scrollStrategy != scroll_auto_redraw) {
	/*
	** Set the context to the pixmap and the origin to the center.
	** The centered origin minimizes problems with limited imaging
	** area.
	*/
	initializeContext(AppData.buf, AppData.scaledWidth/2,
			  AppData.scaledHeight/2);
	positionDrawingArea(ix, iy, vx, vy);
	if (AppData.showDrawing) XClearWindow(XtDisplay(w), XtWindow(w));
	
	/*
	** Set the clipping area to the whole page and redraw
	*/
	area[0] = 0;
	area[1] = AppData.pixmapHeight;
	area[2] = AppData.pixmapWidth;
	area[3] = AppData.pixmapHeight;
	drawSelf(area, 1);

    } else {
	/*
	** Unbuffered drawing, so free pixmap and set window as destination
	*/
	initializePixmap(0, 0);
	initializeContext(XtWindow(w), AppData.scaledWidth/2,
			  AppData.scaledHeight/2);
	positionDrawingArea(ix, iy, vx, vy);
    }

    /*
    ** Now get the image to appear in the window
    */
    if (AppData.scrollStrategy == scroll_background) {
	/*
	** Using window background, so install as background and clear
	*/
	XSetWindowBackgroundPixmap(XtDisplay(w), XtWindow(w), AppData.buf);
	XClearWindow(XtDisplay(w), XtWindow(w));

    } else if (AppData.scrollStrategy == scroll_auto_buffer) {
	/*
	** Using buffered drawing.  If we weren't watching the drawing,
	** clear the window.  The expose events will cause the buffer
	** to be copied into the window
	*/
	if (!AppData.showDrawing) flushAndClear(w);

    } else {
	/*
	** Unbuffered drawing; clear window and let expose events
	** drive the actual drawing
	*/
	flushAndClear(w);
    }
} /* end setupAndDrawAuto() */

/***************************************************************
**
** FUNCTION:    setupAndDrawSelf
**
** DESCRIPTION: Setup the context and redraw in self-scrolling
**		mode.  Keep the ix, iy position in the drawing
**		at vx, vy in the window
**
** PARAMETERS:  center		Whether to center the image
**		ix, iy		Position in the drawing
**		vx, vy		Position in the window
**
** RETURN:      None.
**
***************************************************************/

static void setupAndDrawSelf(center, ix, iy, vx, vy)
    Boolean center;
    float ix, iy;
    int vx, vy;
{
    int area[4];
    Widget w = AppData.currentDraw;

    /*
    ** If the current drawing area is not the self drawing area,
    ** unmanage the auto-scrolling window and manage the self-scrolling
    ** one.  Get the current size of the drawing area
    */
    if (AppData.currentDraw != AppData.selfDrawingArea) {
	XtUnmanageChild(AppData.autoScrolling);
	XtManageChild(AppData.selfScrolling);
	w = AppData.currentDraw = AppData.selfDrawingArea;
	getWindowSize();
    }

    if (center) {
	ix = PAGE_WIDTH / 2.0;
	iy = PAGE_HEIGHT / 2.0;
	vx = AppData.drawingWidth / 2;
	vy = AppData.drawingHeight / 2;
    } 

    if (AppData.scrollStrategy == scroll_self_buffer) {
	/*
	** Create a backing pixmap.  If this fails, convert to
	** unbuffered drawing
	*/
	initializePixmap(AppData.drawingWidth, AppData.drawingHeight);
	if (AppData.buf == None) {
	    putUpInfoDialog(AppData.noSelfPixmapMessage);
	    AppData.scrollStrategy = scroll_self_redraw;
	    XtVaSetValues(AppData.currentStrategy, XmNset, False, NULL);
	    XtVaSetValues(AppData.selfRedraw, XmNset, True, NULL);
	    AppData.currentStrategy = AppData.selfRedraw;
	    XtSetSensitive(AppData.watchFrame, False);
	}
    }

    if (AppData.scrollStrategy == scroll_self_buffer) {
	/*
	** Set the context to the pixmap
	*/
	initializeContext(AppData.buf, 0, AppData.drawingHeight);
	positionDrawingArea(ix, iy, vx, vy);
	if (AppData.showDrawing) XClearWindow(XtDisplay(w), XtWindow(w));

	/*
	** Set the clipping area to the size of the window and redraw
	*/
	area[0] = 0;
	area[1] = AppData.drawingHeight;
	area[2] = AppData.drawingWidth;
	area[3] = AppData.drawingHeight;

	drawSelf(area, 1);
	/*
	** If we weren't watching the drawing, clear the window.
	** The expose events will cause the bufferto be copied into the window
	*/
	if (!AppData.showDrawing) flushAndClear(w);

    } else {
	/*
	** Unbuffered drawing, so free pixmap and set window as destination
	*/
	initializePixmap(0, 0);
	initializeContext(XtWindow(w), 0, AppData.drawingHeight);
	positionDrawingArea(ix, iy, vx, vy);
	/*
	** Clear window and let expose events drive the actual drawing
	*/
	flushAndClear(w);
    }
} /* end setupAndDrawSelf() */

/***************************************************************
**
** FUNCTION:    setupAndDrawUnmoving
**
** DESCRIPTION: Setup the buffers and redraw, keeping everything
**		positioned as it currently is
**
** PARAMETERS:  None
**
** RETURN:      None.
**
***************************************************************/

void setupAndDrawUnmoving()
{
    XPoint xpt;
    Point pt;
    Position x, y;

    xpt.x = 0;
    xpt.y = 0;
    if (AppData.currentDraw == AppData.autoDrawingArea) {
	XtVaGetValues(AppData.autoDrawingArea, XtNx, &x, XtNy, &y, NULL);
	xpt.x -= x;
	xpt.y -= y;
    }
    convertToDPS(&xpt, &pt);

    setupAndDraw(False, pt.x, pt.y, 0, 0);
} /* end setupAndDrawUnmoving() */

/***************************************************************
**
** FUNCTION:    setupAndDraw
**
** DESCRIPTION: Set everything up for drawing, and redraw.  Either
**		center the drawing or keep (ix, iy) in the image at
**		(vx, vy) in the window
**
** PARAMETERS:  center		Whether to center the image
**		ix, iy		Fixed point in image
**		vx, vy		Fixed point in window
**
** RETURN:      None.
**
***************************************************************/

void setupAndDraw(center, ix, iy, vx, vy)
    Boolean center;
    float ix, iy;
    int vx, vy;
{
    if (AppData.scrollStrategy & SCROLL_AUTO) {
	setupAndDrawAuto(center, ix, iy, vx, vy);
    } else setupAndDrawSelf(center, ix, iy, vx, vy);
} /* end setupAndDraw() */
