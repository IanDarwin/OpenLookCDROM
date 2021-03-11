/*
 * $RCSfile: ImportXDPS.c,v $
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

#include "Import.h"

/***************************************************************
**
**	DATA DECLARATIONS
**
***************************************************************/

static char FontName[] = "ControlPointsFont";
static char ControlString[] = "aaaaaaaa";

static float CtlPtSize = 4.0;		/* size of the control points */

/***************************************************************
**
** FUNCTION:	drawSelf
**
** DESCRIPTION:	Draws the picture by copying elements to the original
**		pixmap.  If start is non-NULL, it means that everything
**		below start is ok and doesn't need to be copied
**
** PARAMETERS:	start	Everything below this element is already ok
**
** RETURN:	None
**
***************************************************************/

void drawSelf(start)
    Element *start;
{
    Element *e;
    Display *dpy = XtDisplay(AppData.drawingArea);
    int x, y;

    /*
    ** Set rendering to original pixmap for drawing background (and
    ** possibly boxes
    */
    XDPSSetContextGState(AppData.dpsCtxt, AppData.origGState);
    if (start == NULL) PSWDesktop(0.0, 0.0, PAGE_WIDTH, PAGE_HEIGHT);
    DPSWaitContext(AppData.dpsCtxt);

    for (e = AppData.lastElement; e != NULL; e = e->prev) {
	/*
	** See if we're before the start element
	*/
	if (e == start) start = NULL;
	if (start != NULL) continue;
	/*
	** The moveElement is the element being moved; it should
	** temporarily not be in the picture
	*/
	if (e == AppData.moveElement) continue;

	/*
	** If we're using boxes, or if there is no image for this
	** element, draw a box.  Wait after drawing if we're not using
	** boxes so that later elements appear correctly stacked
	*/
	if (AppData.useBoxes || e->image == None) {	
	    PSWDrawBox(e->origBBox.ll.x, e->origBBox.ll.y,
		       e->origBBox.ur.x, e->origBBox.ur.y,
		       e->tx, e->ty, e->sx, e->sy, e->rotation);
	    if (!AppData.useBoxes) DPSWaitContext(AppData.dpsCtxt);
	/*
	** If not using boxes, copy image through a mask
	*/
	} else {
	    x = e->xBBox.x + AppData.originX;
	    y = e->xBBox.y - AppData.scaledHeight + AppData.originY;
	    XSetClipOrigin(dpy, AppData.gc, x, y);
	    XSetClipMask(dpy, AppData.gc, e->mask);
	    XCopyArea(dpy, e->image, AppData.original, AppData.gc,
		      0, 0, e->xBBox.width, e->xBBox.height, x, y);
	}

	if (AppData.useBoxes) DPSWaitContext(AppData.dpsCtxt);
	/*
	** If showing buffers, refresh original buffer window
	*/
	if (AppData.showBuffer) {
	    XSetClipMask(dpy, AppData.gc, None);
	    XCopyArea(XtDisplay(AppData.drawingArea), AppData.original,
		      XtWindow(AppData.bufOrig), AppData.gc,
		      0, 0, AppData.drawingWidth,
		      AppData.drawingHeight, 0, 0);
	}
    }
    /*
    ** Reset clip mask
    */
    XSetClipMask(dpy, AppData.gc, None);
} /* end drawSelf() */

/***************************************************************
**
** FUNCTION:	drawSelfAndUpdate
**
** DESCRIPTION:	Draw the picture, then copy the original pixmap into
**		the drawing area window
**
** PARAMETERS:	start	Everything below this element is already ok
**
** RETURN:	None
**
***************************************************************/

void drawSelfAndUpdate(start)
    Element *start;
{
    drawSelf(start);
    XCopyArea(XtDisplay(AppData.drawingArea), AppData.original,
	      XtWindow(AppData.drawingArea), AppData.gc,
	      0, 0, AppData.drawingWidth, AppData.drawingHeight, 0, 0);
    if (AppData.selected) drawSelectionMarks();
} /* end drawSelfAndUpdate() */

/*************************************************************
**
** FUNCTION:	convertPoint
**
** DESCRIPTION:	Convert an X point into user space, possibly relative
**		to previous point
**
** PARAMETERS:	x, y		Point in X coordinates
**		pfx		Where to store user space coordinates
**		relative	If true, make relative to previous point
**
** RETURN:	None
**
*************************************************************/

static void convertPoint(x, y, pfx, relative)
    int x, y;
    float *pfx;
    Boolean relative;
{
    XPoint xpt;
    Point pt;
    static Point lastPt;

    /*
    ** Convert to user space and store
    */
    xpt.x = x + AppData.originX;
    xpt.y = y - AppData.scaledHeight + AppData.originY;
    convertToDPS(&xpt, &pt);
    *pfx = pt.x;
    *(pfx+1) = pt.y;

    /*
    ** If relative, adjust by previous point
    */
    if (relative) {
	*pfx -= lastPt.x;
	*(pfx+1) -= lastPt.y;
    }
    lastPt = pt;
} /* end convertPoint() */

/*************************************************************
**
** FUNCTION:	drawSelectionMarks
**
** DESCRIPTION:	Draw marks around the selected object
**
** PARAMETERS:	None
**
** RETURN:	None
**
*************************************************************/

void drawSelectionMarks()
{
    float pts[18];
    Element *e = AppData.selected;
    XPoint xpt;
    Point pt;
    XRect *r = &e->sizeBox;

    if (e == NULL) return;

    /*
    ** Set gstate for drawing into the window
    */
    XDPSSetContextGState(AppData.dpsCtxt, AppData.winGState);
    PSgsave();

    /*
    ** Easiest way to do this is to rotate the coordinate system so that
    ** we don't need to rotate the points.  First translate to the origin
    ** of the picture (== the origin of the sizeBox)
    */
    xpt.x = r->x + AppData.originX;
    xpt.y = r->y - AppData.scaledHeight + AppData.originY;
    convertToDPS(&xpt, &pt);
    PStranslate(pt.x, pt.y);
    if (e->rotation != 0.0) PSrotate(e->rotation);

    /*
    ** Convert X points to user space coordinates.  Subtract the translation
    ** from the initial point since we've already translated there, and make
    ** the other points relative
    */
    convertPoint(r->x, r->y, &pts[0], False);
    pts[0] -= pt.x;
    pts[1] -= pt.y;
    convertPoint(r->x, r->y + r->height/2, &pts[2], True);
    convertPoint(r->x, r->y + r->height, &pts[4], True);
    convertPoint(r->x + r->width/2, r->y + r->height, &pts[6], True);
    convertPoint(r->x + r->width, r->y + r->height, &pts[8], True);
    convertPoint(r->x + r->width, r->y + r->height/2, &pts[10], True);
    convertPoint(r->x + r->width, r->y, &pts[12], True);
    convertPoint(r->x + r->width/2, r->y, &pts[14], True);
    pts[16] = pts[17] = 0;
    /*
    ** Draw the points
    */
    PSWDrawControlPoints(pts[0], pts[1], &pts[2], 16,
			 ControlString, 8);
    PSgrestore();
} /* end drawSelectionMarks() */

/*************************************************************
**
** FUNCTION:	computeRotatePoint
**
** DESCRIPTION:	Rotate an X coordinate
**
** PARAMETERS:	x, y		Point to rotate
**		ox, oy		Center of rotation
**		rot		Rotation (in radians)
**		pt		Returns rotated point
**
** RETURN:	None
**
*************************************************************/

static void computeRotatePoint(x, ox, y, oy, rot, pt)
    int x, y, ox, oy;
    float rot;
    XPoint *pt;
{
    float r, theta;
    float dx, dy;

    /*
    ** Compute offsets from center of rotation
    */
    dx = x - ox;
    dy = y - oy;

    /*
    ** Rotate point by angle
    */
    if (dx == 0 && dy == 0) {
	pt->x = 0;
	pt->y = 0;
    } else {
	r = sqrt(dx * dx + dy * dy);
	theta = atan2(dy, dx) + rot;
	pt->x = r * cos(theta) + 0.5;
	pt->y = r * sin(theta) + 0.5;
    }

    /*
    ** Add center of rotation back to get point
    */
    pt->x += ox;
    pt->y += oy;
} /* end computeRotatePoint() */

/*************************************************************
**
** FUNCTION:	drawRotatedRectangle
**
** DESCRIPTION:	Draw a rotated rectangle using X functions
**
** PARAMETERS:	rect		Rectangle to draw
**		rotation	Amount to rotate (in degrees)
**		ox, oy		Center of rotation
**
** RETURN:	None
**
*************************************************************/

static void drawRotatedRectangle(rect, rotation, ox, oy)
    XRect *rect;
    float rotation;
    int ox, oy;
{
    XPoint xpt[5];
    float r = -DTOR(rotation);

    /*
    ** Compute the four rotated points and draw lines
    */
    computeRotatePoint(rect->x, ox, rect->y, oy, r, xpt);
    computeRotatePoint(rect->x + rect->width, ox, rect->y, oy, r, xpt + 1);
    computeRotatePoint(rect->x + rect->width, ox, rect->y + rect->height, oy,
		       r, xpt + 2);
    computeRotatePoint(rect->x, ox, rect->y + rect->height, oy, r, xpt + 3);
    xpt[4] = xpt[0];
    XDrawLines(XtDisplay(AppData.drawingArea), AppData.composite,
	       AppData.blackgc, xpt, 5, CoordModeOrigin);
} /* end drawRotatedRectangle() */

/*************************************************************
**
** FUNCTION:	computeRotatedBounds
**
** DESCRIPTION:	Compute the bounding box of a rotated rectangle
**
** PARAMETERS:	rect		Rectangle to rotate
**		rotation	Amount to rotate (in degrees)
**		ox, oy		Center of rotation
**
** RETURN:	bounds		Returns bounding box
**
*************************************************************/

static void computeRotatedBounds(rect, rotation, ox, oy, bounds)
    XRect *rect;
    float rotation;
    int ox, oy;
    XRect *bounds;
{
    XPoint xpt[4];
    float r = -DTOR(rotation);
    int minx, miny, maxx, maxy;
    int i;

    /*
    ** Compute the four rotated points
    */
    computeRotatePoint(rect->x, ox, rect->y, oy, r, xpt);
    computeRotatePoint(rect->x + rect->width, ox, rect->y, oy, r, xpt + 1);
    computeRotatePoint(rect->x + rect->width, ox, rect->y + rect->height, oy,
		       r, xpt + 2);
    computeRotatePoint(rect->x, ox, rect->y + rect->height, oy, r, xpt + 3);

    /*
    ** Find the minimum and maximum points
    */
    minx = maxx = xpt[0].x;
    miny = maxy = xpt[0].y;

    for (i = 1; i < 4; i++) {
	if (xpt[i].x < minx) minx = xpt[i].x;
	else if (xpt[i].x > maxx) maxx = xpt[i].x;
	if (xpt[i].y < miny) miny = xpt[i].y;
	else if (xpt[i].y > maxy) maxy = xpt[i].y;
    }
    bounds->x = minx;
    bounds->y = miny;
    bounds->width = maxx - minx + 1;
    bounds->height = maxy - miny + 1;
} /* end computeRotatedBounds() */

/***************************************************************
**
** FUNCTION:	copyOrigToComposite
**
** DESCRIPTION:	Copies the original buffer to the composite one
**		and updates the buffer window
**
** PARAMETERS:	None
**
** RETURN:	None
**
***************************************************************/

static void copyOrigToComposite()
{
    Display *dpy = XtDisplay(AppData.drawingArea);

    XCopyArea(dpy, AppData.original, AppData.composite, AppData.gc,
	      0, 0, AppData.drawingWidth, AppData.drawingHeight, 
	      0, 0);

    if (AppData.showBuffer) {
	XCopyArea(dpy, AppData.composite, XtWindow(AppData.bufComp),
		  AppData.gc, 0, 0, AppData.drawingWidth,
		  AppData.drawingHeight, 0, 0);
    }
} /* end copyOrigToComposite() */

/***************************************************************
**
** FUNCTION:	checkScrolling
**
** DESCRIPTION:	Checks if the point is outside the window, and scrolls
**		if so.
**
** PARAMETERS:	x	Mouse x coordinate
**		y	Mouse y coordinate
**		initPt	Initial point
**
** RETURN:	whether scrolling occured
**
***************************************************************/

static Boolean checkScrolling(x, y, initPt)
    int x, y;
    XPoint *initPt;
{
    int deltaX = 0, deltaY = 0;

    /*
    ** If inside the window, no scrolling needed
    */
    if (x >= 0 && y >= 0 && x <= (int) AppData.drawingWidth &&
	y <= (int) AppData.drawingHeight) return False;

    /*
    ** If x is outside to left, scroll to left...but not beyond left edge
    */
    if (x < 0) {
	deltaX = x;
	if (deltaX < -AppData.scrollX) deltaX = -AppData.scrollX;

    } else if (x > (int) AppData.drawingWidth) {
	/*
	** Ditto for outside to right
	*/
	if (AppData.scaledWidth > (int) AppData.drawingWidth) {
	    if (AppData.scrollX + x > AppData.scaledWidth) {
		deltaX = AppData.scaledWidth -
			(AppData.scrollX + AppData.drawingWidth);
	    } else deltaX = x - AppData.drawingWidth;

	}
    }

    /*
    ** Now do the same thing for y
    */
    if (y < 0) {
	deltaY = y;
	if (deltaY < -AppData.scrollY) deltaY = -AppData.scrollY;

    } else if (y > (int) AppData.drawingHeight) {
	if (AppData.scaledHeight > (int) AppData.drawingHeight) {
	    if (AppData.scrollY + y > AppData.scaledHeight) {
		deltaY = AppData.scaledHeight -
			(AppData.scrollY + AppData.drawingHeight);
	    } else deltaY = y - AppData.drawingHeight;
	}
    }

    /*
    ** We may have hit an edge and not need to scroll after all
    */
    if (deltaX == 0 && deltaY == 0) return False;

    /*
    ** Do the scroll
    */
    doScroll(deltaX, deltaY);

    /*
    ** Reflect new scrolled values in scrollbars
    */
    XtVaSetValues(AppData.hScroll, XmNvalue, AppData.scrollX, NULL);
    XtVaSetValues(AppData.vScroll, XmNvalue, AppData.scrollY, NULL);

    initPt->x -= deltaX;
    initPt->y -= deltaY;
    return True;
} /* end checkScrolling() */

/***************************************************************
**
** FUNCTION:	getNextMouseEvent
**
** DESCRIPTION:	Returns the next button release or motion event
**
** PARAMETERS:	dpy	Display
**		win	Window
**
** RETURN:	event	Event
**
***************************************************************/

static void getNextMouseEvent(dpy, win, event)
    Display *dpy;
    Window win;
    XEvent *event;
{
    XWindowEvent(dpy, win, ButtonMotionMask | ButtonReleaseMask, event);

    /*
    ** Do motion compression by skipping over more motion events
    */
    while (event->type != ButtonRelease) {
	if (!XCheckWindowEvent(dpy, win,
			       ButtonMotionMask | ButtonReleaseMask,
			       event)) break;
    }
} /* end getNextMouseEvent() */

/***************************************************************
**
** FUNCTION:	mergeXRects
**
** DESCRIPTION:	Adds the second X rectangle to the first
**
** PARAMETERS:	orig		Original rectangle
**		add		Rect being added
**
** RETURN:	None
**
***************************************************************/

static void mergeXRects(orig, add)
    XRect	*orig, *add;
{
    if (add->x < orig->x) {
	orig->width += orig->x - add->x;
	orig->x = add->x;
    }
    if (add->y < orig->y) {
	orig->height += orig->y - add->y;
	orig->y = add->y;
    }
    if (add->x + add->width > orig->x + orig->width) {
	orig->width = add->x + add->width - orig->x;
    }
    if (add->y + add->height > orig->y + orig->height) {
	orig->height = add->y + add->height - orig->y;
    }
} /* end mergeXRects() */

/***************************************************************
**
** FUNCTION:	computeMergeBBox
**
** DESCRIPTION:	Compute bounding box that must be updated
**
** PARAMETERS:	rect		Pointer to rectangle
**		oldBBox		Previous curve bounding box (updated)
**
** RETURN:	copy		Rectangle to copy
**
***************************************************************/

static void computeMergeBBox(rect, oldBBox, copy)
    XRect *rect;
    XRect *oldBBox, *copy;
{
    XRect currentBBox;

    currentBBox = *rect;
    currentBBox.width++;
    currentBBox.height++;

    mergeXRects(oldBBox, &currentBBox);

    *copy = *oldBBox;
    *oldBBox = currentBBox;
} /* end computeMergeBBox() */

/***************************************************************
**
** FUNCTION:	doSweep
**
** DESCRIPTION:	Handle a mouse movement during rectangle sweep
**
** PARAMETERS:	initPt		Where the mouse started
**		curPt		Where the mouse currently is
**		first		Whether this is the first time
**		scrolled	Whether we scrolled this time through
**		ratio		width/height ratio of rectangle
**		b		bounding box of rectangle
**
** RETURN:	rect		Rectangle swept out
**
***************************************************************/

static void doSweep(initPt, curPt, first, scrolled, ratio, b, rect)
    XPoint *initPt, *curPt;
    Boolean *first, scrolled;
    float ratio;
    BBox *b;
    XRect *rect;
{
    static XRect oldBBox;
    XRect copy;
    Display	*dpy = XtDisplay(AppData.drawingArea);
    Window 	win = XtWindow(AppData.drawingArea);
    float newratio;

    /*
    ** Construct a rectangle from initial point to current point
    */
    rect->x = initPt->x;
    rect->y = initPt->y;
    rect->width = curPt->x - initPt->x;
    rect->height = curPt->y - initPt->y;

    /*
    ** If the first time through, set up the bounding box for the initial
    ** drawing (normal size)
    */
    if (*first) {
	oldBBox.width = (b->ur.x - b->ll.x) * AppData.scaledWidth / PAGE_WIDTH;
	oldBBox.height = (b->ur.y - b->ll.y) * AppData.scaledHeight /
		PAGE_HEIGHT;
	oldBBox.x = initPt->x;
	oldBBox.y = initPt->y - oldBBox.height;
	*first = False;
    }

    /*
    ** If swept area is 0 width or height, use normal size
    */
    if (rect->width == 0 || rect->height == 0) {
	rect->width = (b->ur.x - b->ll.x) * AppData.scaledWidth / PAGE_WIDTH;
	rect->height = (b->ur.y - b->ll.y) * AppData.scaledHeight /
		PAGE_HEIGHT;
	rect->y -= rect->height;
    } else {
	/*
	** Adjust box to make it match the ratio of the picture
	*/
	newratio = ABS((float) rect->width) / ABS((float) rect->height);

	if (newratio > ratio) {
	    /*
	    ** Too wide, make narrower
	    */
	    rect->width = (int) (ratio * (float) ABS(rect->height) + 0.5) *
		    SIGN(rect->width);
	} else if (newratio < ratio) {
	    /*
	    ** Too tall, make shorter
	    */
	    rect->height = (int) ((float) ABS(rect->width) / ratio + 0.5) *
		    SIGN(rect->height);
	}
    }

    /*
    ** Make into a proper X rectangle
    */
    if (rect->width < 0) {
	rect->x += rect->width;
	rect->width = -rect->width;
    }

    if (rect->height < 0) {
	rect->y += rect->height;
	rect->height = -rect->height;
    }

    /*
    ** Compute area that must be updated.  If we scrolled, make it
    ** everything, but we still have to compute the bbox so we have
    ** an oldBBox for next time around
    */
    computeMergeBBox(rect, &oldBBox, &copy);
    if (scrolled) {
	copy.x = copy.y = 0;
	copy.width = AppData.drawingWidth;
	copy.height = AppData.drawingHeight;
    }

    /*
    ** Copy original into composite
    */
    XCopyArea(dpy, AppData.original, AppData.composite, AppData.gc,
	      copy.x, copy.y, copy.width, copy.height, copy.x, copy.y);

    /*
    ** Draw the rectangle to the composite pixmap and copy to window
    */
    XDrawRectangle(XtDisplay(AppData.drawingArea), AppData.composite,
		   AppData.blackgc, rect->x, rect->y,
		   rect->width, rect->height);
    XCopyArea(dpy, AppData.composite, win, AppData.gc, 
	      copy.x, copy.y, copy.width, copy.height, copy.x, copy.y);

    if (AppData.showBuffer) {	
	XCopyArea(dpy, AppData.composite, XtWindow(AppData.bufComp),
		  AppData.gc, copy.x, copy.y, copy.width, copy.height,
		  copy.x, copy.y);
    }
} /* end doSweep() */

/***************************************************************
**
** FUNCTION:	doSweepLoop
**
** DESCRIPTION:	Event handling loop for sweeping a rectangle
**
** PARAMETERS:	initPt		Initial mouse down point
**		ratio		width/height ration of rectangle
**		b		bounding box
**
** RETURN:	rect		Rectangle swept out
**
***************************************************************/

static void doSweepLoop(initPt, ratio, b, rect)
    XPoint *initPt;
    float ratio;
    BBox *b;
    XRect *rect;
{
    XEvent	event;
    XPoint	xpoint, lastPt;
    Boolean	first = True;
    Display	*dpy = XtDisplay(AppData.drawingArea);
    Window 	win = XtWindow(AppData.drawingArea);
    Boolean 	scrolled;

    lastPt = *initPt;

    do {
	/*
	** Wait for a motion or button release event
	*/
	getNextMouseEvent(dpy, win, &event);

	/*
	** See if user moved outside of window, and scroll drawing if so
	*/
	scrolled = checkScrolling(event.xbutton.x, event.xbutton.y, initPt);

	/*
	** Store new coordinates in xpoint
	*/
	xpoint.x = event.xbutton.x;
	xpoint.y = event.xbutton.y;
	
	/*
	** If the mouse moved, update
	*/
	if (xpoint.x - lastPt.x || xpoint.y - lastPt.y) {
	    doSweep(initPt, &xpoint, &first, scrolled, ratio, b, rect);
	}
	lastPt = xpoint;
    } while (event.type != ButtonRelease);
} /* end doSweepLoop() */

/***************************************************************
**
** FUNCTION:	sweepRectangle
**
** DESCRIPTION:	Sweep out a rectangle with the mouse
**
** PARAMETERS:	pt	X coordinates of button press
**		b	bounding box of rectangle, for ratio
**		
** RETURN:	rect	returned X rectangle
**
***************************************************************/

void sweepRectangle(pt, b, rect)
    XPoint	*pt;
    BBox	*b;
    XRect 	*rect;
{
    float ratio;

    /*
    ** Copy the original to the composite pixmap
    */
    copyOrigToComposite();

    /*
    ** Compute the initial ratio and the starting rectangle
    */
    ratio = (b->ur.x - b->ll.x) / (b->ur.y - b->ll.y);
    rect->width = (b->ur.x - b->ll.x) * AppData.scaledWidth / PAGE_WIDTH;
    rect->height = (b->ur.y - b->ll.y) * AppData.scaledHeight / PAGE_HEIGHT;
    rect->x = pt->x;
    rect->y = pt->y - rect->height;;

    /*
    ** Draw the starting rectangle to the composite pixmap and copy to window
    */
    XDrawRectangle(XtDisplay(AppData.drawingArea), AppData.composite,
		   AppData.blackgc, rect->x, rect->y,
		   rect->width, rect->height);
    XCopyArea(XtDisplay(AppData.drawingArea), AppData.composite,
	      XtWindow(AppData.drawingArea), AppData.gc,
	      rect->x, rect->y, rect->width+1, rect->height+1,
	      rect->x, rect->y);

    /*
    ** Call sweep event dispatching loop
    */
    doSweepLoop(pt, ratio, b, rect);

    /*
    ** Copy original into window
    */
    XCopyArea(XtDisplay(AppData.drawingArea), AppData.original,
	      XtWindow(AppData.drawingArea), AppData.gc,
	      rect->x, rect->y, rect->width+1, rect->height+1,
	      rect->x, rect->y);
} /* end sweepRectangle() */

/*************************************************************
**
** FUNCTION:	computeMoveBBox
**
** DESCRIPTION:	Compute the bounding box that must be updated
**
** PARAMETERS:	rect		Pointer to rectangle
**		oldBBox		Previous curve bounding box (updated)
**
** RETURN:	copy		Rectangle to copy
**
*************************************************************/

static void computeMoveBBox(rect, oldBBox, copy)
    XRect *rect;
    XRect *oldBBox, *copy;
{
    XRect currentBBox;

    currentBBox = *rect;
    currentBBox.x += AppData.originX;
    currentBBox.y -= AppData.scaledHeight - AppData.originY;

    mergeXRects(oldBBox, &currentBBox);

    *copy = *oldBBox;
    copy->x -= 2;
    copy->y -= 2;
    copy->width += 4;
    copy->height += 4;
    *oldBBox = currentBBox;
} /* end computeMoveBBox() */

/*************************************************************
**
** FUNCTION:	doMove
**
** DESCRIPTION:	Handle a mouse movement during move
**
** PARAMETERS:	initPt		Where the mouse started
**		curPt		Where the mouse currently is
**		first		Whether this is the first time
**		scrolled	Whether we scrolled this time through
**		e		Element being moved
**
** RETURN:	None
**
*************************************************************/

static void doMove(initPt, curPt, first, scrolled, e)
    XPoint *initPt, *curPt;
    Boolean *first, scrolled;
    Element *e;
{
    static XRect oldBBox;
    XRect copy, rect;
    Display	*dpy = XtDisplay(AppData.drawingArea);
    Window 	win = XtWindow(AppData.drawingArea);
    int x, y;
    XPoint xpt;
    Point pt;

    /*
    ** If first time, compute old bounding box.
    */
    if (*first) {
	oldBBox = e->xBBox;
	oldBBox.x += AppData.originX;
	oldBBox.y -= AppData.scaledHeight - AppData.originY;
	*first = False;
    }

    /*
    ** Find area that will be 
    */
    rect.x = e->xBBox.x + curPt->x - initPt->x;
    rect.y = e->xBBox.y + curPt->y - initPt->y;
    rect.width = e->xBBox.width;
    rect.height = e->xBBox.height;

    /*
    ** Compute area that must be updated.  If we scrolled, make it
    ** everything, but we still have to compute the bbox so we have
    ** an oldBBox for next time around
    */
    computeMoveBBox(&rect, &oldBBox, &copy);
    if (scrolled) {
	copy.x = copy.y = 0;
	copy.width = AppData.drawingWidth;
	copy.height = AppData.drawingHeight;
    }

    /*
    ** Copy original into composite
    */
    XCopyArea(dpy, AppData.original, AppData.composite, AppData.gc,
	      copy.x, copy.y, copy.width, copy.height, copy.x, copy.y);

    /*
    ** Copy picture to the composite pixmap
    */
    if (!AppData.useBoxes && e->image != None) {
	x = rect.x + AppData.originX;
	y = rect.y - AppData.scaledHeight + AppData.originY;
	XSetClipOrigin(dpy, AppData.gc, x, y);
	XSetClipMask(dpy, AppData.gc, e->mask);
	XCopyArea(dpy, e->image, AppData.composite, AppData.gc,
		  0, 0, e->xBBox.width, e->xBBox.height, x, y);
	XSetClipMask(dpy, AppData.gc, None);
    /*
    ** If drawing a box, translate to new locale
    */
    } else {
	xpt.x = e->sizeBox.x + AppData.originX + curPt->x - initPt->x;
	xpt.y = e->sizeBox.y - AppData.scaledHeight + AppData.originY +
		curPt->y - initPt->y;
	convertToDPS(&xpt, &pt);
	PSWDrawBox(e->origBBox.ll.x, e->origBBox.ll.y,
		   e->origBBox.ur.x, e->origBBox.ur.y,
		   pt.x, pt.y, e->sx, e->sy, e->rotation);
	DPSWaitContext(AppData.dpsCtxt);
    }

    /*
    ** Copy to the window
    */
    XCopyArea(dpy, AppData.composite, win, AppData.gc, 
	      copy.x, copy.y, copy.width, copy.height, copy.x, copy.y);

    if (AppData.showBuffer) {	
	XCopyArea(dpy, AppData.composite, XtWindow(AppData.bufComp),
		  AppData.gc, copy.x, copy.y, copy.width, copy.height,
		  copy.x, copy.y);
    }
} /* end doMove() */

/*************************************************************
**
** FUNCTION:	doMoveLoop
**
** DESCRIPTION:	Event handling loop for moving an element
**
** PARAMETERS:	initPt		Initial mouse down point
**		e		Element being moved
**
** RETURN:	None
**
*************************************************************/

static void doMoveLoop(initPt, e)
    XPoint *initPt;
    Element *e;
{
    XEvent	event;
    XPoint	xpoint, lastPt;
    Boolean	first = True;
    Display	*dpy = XtDisplay(AppData.drawingArea);
    Window 	win = XtWindow(AppData.drawingArea);
    Boolean 	scrolled;

    lastPt = *initPt;

    do {
	/*
	** Wait for a motion or button release event
	*/
	getNextMouseEvent(dpy, win, &event);

	/*
	** See if user moved outside of window, and scroll drawing if so
	*/
	scrolled = checkScrolling(event.xbutton.x, event.xbutton.y, initPt);

	/*
	** Store new coordinates in xpoint
	*/
	xpoint.x = event.xbutton.x;
	xpoint.y = event.xbutton.y;
	
	/*
	** If the mouse moved, update
	*/
	if (xpoint.x - lastPt.x || xpoint.y - lastPt.y) {
	    doMove(initPt, &xpoint, &first, scrolled, e);
	}
	lastPt = xpoint;
    } while (event.type != ButtonRelease);

    /*
    ** Update bounding boxes to reflect new location
    */
    e->xBBox.x += xpoint.x - initPt->x;
    e->xBBox.y += xpoint.y - initPt->y;
    e->sizeBox.x += xpoint.x - initPt->x;
    e->sizeBox.y += xpoint.y - initPt->y;
} /* end doMoveLoop() */

/*************************************************************
**
** FUNCTION:	moveElement
**
** DESCRIPTION:	Move an element in the picture
**
** PARAMETERS:	xpt		Initial mouse down point
**		e		Element being moved
**
** RETURN:	None
**
*************************************************************/

void moveElement(xpt, e)
    XPoint *xpt;
    Element *e;
{
    int x, y;
    Display *dpy = XtDisplay(AppData.drawingArea);
    Point pt;

    /*
    ** Redraw original, without the moving element
    */
    AppData.moveElement = e;
    drawSelf(NULL);
    
    /*
    ** Copy the original to the composite pixmap
    */
    copyOrigToComposite();

    /*
    ** Draw the moving object or box to the composite pixmap
    */
    x = e->xBBox.x + AppData.originX;
    y = e->xBBox.y - AppData.scaledHeight + AppData.originY;
    if (!AppData.useBoxes && e->image != None) {
	XSetClipOrigin(dpy, AppData.gc, x, y);
	XSetClipMask(dpy, AppData.gc, e->mask);
	XCopyArea(dpy, e->image, AppData.composite, AppData.gc,
		  0, 0, e->xBBox.width, e->xBBox.height, x, y);
	XSetClipMask(dpy, AppData.gc, None);
    } else {
	XDPSSetContextGState(AppData.dpsCtxt, AppData.compGState);
	PSWDrawBox(e->origBBox.ll.x, e->origBBox.ll.y,
		   e->origBBox.ur.x, e->origBBox.ur.y,
		   e->tx, e->ty, e->sx, e->sy, e->rotation);
	DPSWaitContext(AppData.dpsCtxt);
    }

    /*
    ** Copy pixmap to window
    */
    XCopyArea(dpy, AppData.composite,
	      XtWindow(AppData.drawingArea), AppData.gc,
	      x, y, e->xBBox.width, e->xBBox.height, x, y);
    if (AppData.showBuffer) {	
	XCopyArea(dpy, AppData.composite, XtWindow(AppData.bufComp),
		  AppData.gc, 0, 0, AppData.drawingWidth,
		  AppData.drawingHeight, 0, 0);
    }

    /*
    ** Handle mouse events during move
    */
    doMoveLoop(xpt, e);

    /*
    ** Compute new translation and redraw picture
    */
    xpt->x = e->sizeBox.x + AppData.originX;
    xpt->y = e->sizeBox.y - AppData.scaledHeight + AppData.originY;
    convertToDPS(xpt, &pt);
    e->tx = pt.x;
    e->ty = pt.y;

    AppData.moveElement = NULL;
    drawSelfAndUpdate(e);
} /* end moveElement() */

/***************************************************************
**
** FUNCTION:	doScale
**
** DESCRIPTION:	Handle a mouse movement during rectangle scale
**
** PARAMETERS:	initPt		Where the mouse started
**		curPt		Where the mouse currently is
**		first		Whether this is the first time
**		scrolled	Whether we scrolled this time through
**		e		Element being scaled
**
** RETURN:	rect		Current scale rectangle
**
***************************************************************/

static void doScale(initPt, curPt, first, scrolled, rect, e)
    XPoint *initPt, *curPt;
    Boolean *first, scrolled;
    XRect *rect;
    Element *e;
{
    static XRect oldBBox;
    XRect copy, bounds;
    Display	*dpy = XtDisplay(AppData.drawingArea);
    Window 	win = XtWindow(AppData.drawingArea);
    int origX, origY, height, width;
    float r, theta;

    /*
    ** Compute origin of scale
    */
    origX = e->sizeBox.x + AppData.originX;	
    origY = e->sizeBox.y - AppData.scaledHeight + AppData.originY;

    if (*first) {
	oldBBox = e->xBBox;
	initPt->x -= origX;
	initPt->y -= origY;
	/*
	** Rotate the initial point by the element rotation to obtain
	** the offsets perpendicular to the element axes
	*/
	if (e->rotation != 0.0) {
	    r = sqrt((float) (initPt->x * initPt->x + initPt->y * initPt->y));
	    if (initPt->x == 0 && initPt->y == 0) theta = DTOR(e->rotation);
	    else theta = atan2((float) initPt->y, (float) initPt->x) +
		    DTOR(e->rotation);
	    initPt->x = r * cos(theta);
	    initPt->y = r * sin(theta);
	}
	*first = False;
    }

    curPt->x -= origX;
    curPt->y -= origY;
    /*
    ** Rotate current point by the element rotation to obtain
    ** the offsets perpendicular to the element axes
    */
    if (e->rotation != 0.0) {
	r = sqrt((float) (curPt->x * curPt->x + curPt->y * curPt->y));
	if (curPt->x == 0 && curPt->y == 0) theta = DTOR(e->rotation);
	else theta = atan2((float) curPt->y, (float) curPt->x) +
		DTOR(e->rotation);
	curPt->x = r * cos(theta);
	curPt->y = r * sin(theta);
    }

    /*
    ** Scale width and height.  Make sure they're not 0
    */
    if (initPt->x != 0) {
	width = e->sizeBox.width * (float) curPt->x / (float) initPt->x;
    } else width = e->sizeBox.width;
    if (width == 0) width = 1;

    if (initPt->y != 0) {
	height = e->sizeBox.height * (float) curPt->y / (float) initPt->y;
    } else height = e->sizeBox.height;
    if (height == 0) height = -1;

    /*
    ** Define rectangle for new size and find its bounds
    */
    rect->x = origX;
    rect->y = origY;
    rect->width = width;
    rect->height = height;
    computeRotatedBounds(rect, e->rotation, rect->x, rect->y, &bounds);

    /*
    ** Compute area that must be updated.  If we scrolled, make it
    ** everything, but we still have to compute the bbox so we have
    ** an oldBBox for next time around
    */
    computeMergeBBox(&bounds, &oldBBox, &copy);
    if (scrolled) {
	copy.x = copy.y = 0;
	copy.width = AppData.drawingWidth;
	copy.height = AppData.drawingHeight;
    }

    /*
    ** Copy original into composite
    */
    XCopyArea(dpy, AppData.original, AppData.composite, AppData.gc,
	      copy.x, copy.y, copy.width, copy.height, copy.x, copy.y);

    /*
    ** Draw the rotated box to the composite pixmap and copy to window
    */
    drawRotatedRectangle(rect, e->rotation, rect->x, rect->y);
    XCopyArea(dpy, AppData.composite, win, AppData.gc, 
	      copy.x, copy.y, copy.width, copy.height, copy.x, copy.y);

    if (AppData.showBuffer) {	
	XCopyArea(dpy, AppData.composite, XtWindow(AppData.bufComp),
		  AppData.gc, copy.x, copy.y, copy.width, copy.height,
		  copy.x, copy.y);
    }
} /* end doScale() */

/***************************************************************
**
** FUNCTION:	doScaleLoop
**
** DESCRIPTION:	Event handling loop for scaling a rectangle
**
** PARAMETERS:	initPt		Initial mouse down point
**		rect		Current scale rectangle
**		e		Element being scaled
**
** RETURN:	None
**
***************************************************************/

static void doScaleLoop(initPt, rect, e)
    XPoint *initPt;
    XRect *rect;
    Element *e;
{
    XEvent	event;
    XPoint	xpoint, lastPt;
    Boolean	first = True;
    Display	*dpy = XtDisplay(AppData.drawingArea);
    Window 	win = XtWindow(AppData.drawingArea);
    Boolean 	scrolled;

    lastPt = *initPt;

    do {
	/*
	** Wait for a motion or button release event
	*/
	getNextMouseEvent(dpy, win, &event);

	/*
	** See if user moved outside of window, and scroll drawing if so
	*/
	scrolled = checkScrolling(event.xbutton.x, event.xbutton.y, initPt);

	/*
	** Store new coordinates in xpoint
	*/
	xpoint.x = event.xbutton.x;
	xpoint.y = event.xbutton.y;
	
	/*
	** If the mouse moved, update
	*/
	if (xpoint.x - lastPt.x || xpoint.y - lastPt.y) {
	    doScale(initPt, &xpoint, &first, scrolled, rect, e);
	}
	lastPt = xpoint;
    } while (event.type != ButtonRelease);
} /* end doScaleLoop() */

/*************************************************************
**
** FUNCTION:	scaleElement
**
** DESCRIPTION:	Scale an element in the picture
**
** PARAMETERS:	xpt		Initial mouse down point
**		e		Element being moved
**
** RETURN:	None
**
*************************************************************/

void scaleElement(xpt, e)
    XPoint *xpt;
    Element *e;
{
    Display *dpy = XtDisplay(AppData.drawingArea);
    XRect rect, copy;

    /*
    ** Copy the original to the composite pixmap
    */
    copyOrigToComposite();

    /*
    ** Compute the original rectangle and bounding box
    */
    rect = e->sizeBox;
    rect.x += AppData.originX;
    rect.y -= AppData.scaledHeight - AppData.originY;
    copy = e->xBBox;
    copy.x += AppData.originX;
    copy.y -= AppData.scaledHeight - AppData.originY;

    /*
    ** Draw the bounding box to the composite pixmap and copy to window
    */
    drawRotatedRectangle(&rect, e->rotation, rect.x, rect.y);
    XCopyArea(dpy, AppData.composite,
	      XtWindow(AppData.drawingArea), AppData.gc,
	      copy.x, copy.y, copy.width+1, copy.height+1,
	      copy.x, copy.y);

    if (AppData.showBuffer) {	
	XCopyArea(dpy, AppData.composite, XtWindow(AppData.bufComp),
		  AppData.gc, 0, 0, AppData.drawingWidth,
		  AppData.drawingHeight, 0, 0);
    }

    /*
    ** Handle mouse events during scale
    */
    doScaleLoop(xpt, &rect, e);

    /*
    ** Update scale factors and redraw the picture
    */
    e->sx *= ((float) rect.width / (float) e->sizeBox.width);
    e->sy *= ((float) rect.height / (float) e->sizeBox.height);
    updateElement(e);
    drawSelfAndUpdate(NULL);
    XDefineCursor(XtDisplay(AppData.drawingArea),
		  XtWindow(AppData.drawingArea), None); 
} /* end scaleElement() */

/***************************************************************
**
** FUNCTION:	doRotate
**
** DESCRIPTION:	Handle a mouse movement during rectangle rotate
**
** PARAMETERS:	initPt		Where the mouse started
**		curPt		Where the mouse currently is
**		first		Whether this is the first time
**		scrolled	Whether we scrolled this time through
**		e		Element being scaled
**
** RETURN:	rot		Rotation
**
***************************************************************/

static void doRotate(initPt, curPt, first, scrolled, rot, e)
    XPoint *initPt, *curPt;
    Boolean *first, scrolled;
    float *rot;
    Element *e;
{
    static XRect oldBBox;
    XRect copy;
    Display	*dpy = XtDisplay(AppData.drawingArea);
    Window 	win = XtWindow(AppData.drawingArea);
    static float theta1;
    float theta2;
    XRect rect, bounds;

    /*
    ** Define the rectangle we'll be rotating
    */
    rect = e->sizeBox;
    rect.x += AppData.originX;	
    rect.y -= AppData.scaledHeight - AppData.originY;

    /*
    ** If first time, compute original bounding box and find rotation
    ** of original point from the horizontal
    */
    if (*first) {
	oldBBox = e->xBBox;
	oldBBox.x += AppData.originX;	
	oldBBox.y -= AppData.scaledHeight - AppData.originY;
	if (initPt->x == rect.x && initPt->y == rect.y) initPt->x++;
	theta1 = atan2((float) (initPt->y - rect.y),
		       (float) (initPt->x - rect.x));
	*first = False;
    }

    /*
    ** Compute the rotation of the current point from the horizontal and
    ** subtract the initial point rotation to find the real rotation
    */
    if (curPt->x == rect.x && curPt->y == rect.y) {
	*rot = 0;
    } else {
	theta2 = atan2((float) (curPt->y - rect.y),
		       (float) curPt->x - rect.x);
	*rot = RTOD(theta1 - theta2);
    }

    computeRotatedBounds(&rect, e->rotation + *rot, rect.x, rect.y, &bounds);
    /*
    ** Compute area that must be updated.  If we scrolled, make it
    ** everything, but we still have to compute the bbox so we have
    ** an oldBBox for next time around
    */
    computeMergeBBox(&bounds, &oldBBox, &copy);
    if (scrolled) {
	copy.x = copy.y = 0;
	copy.width = AppData.drawingWidth;
	copy.height = AppData.drawingHeight;
    }

    /*
    ** Copy original into composite
    */
    XCopyArea(dpy, AppData.original, AppData.composite, AppData.gc,
	      copy.x, copy.y, copy.width, copy.height, copy.x, copy.y);

    /*
    ** Draw the new box to the composite pixmap and copy to window
    */
    drawRotatedRectangle(&rect, e->rotation + *rot, rect.x, rect.y);
    XCopyArea(dpy, AppData.composite, win, AppData.gc, 
	      copy.x, copy.y, copy.width, copy.height, copy.x, copy.y);

    if (AppData.showBuffer) {	
	XCopyArea(dpy, AppData.composite, XtWindow(AppData.bufComp),
		  AppData.gc, copy.x, copy.y, copy.width, copy.height,
		  copy.x, copy.y);
    }
} /* end doRotate() */

/***************************************************************
**
** FUNCTION:	doRotateLoop
**
** DESCRIPTION:	Event handling loop for rotating a rectangle
**
** PARAMETERS:	initPt		Initial mouse down point
**		e		Element being scaled
**
** RETURN:	rot		Rotation
**
***************************************************************/

static void doRotateLoop(initPt, rot, e)
    XPoint *initPt;
    float *rot;
    Element *e;
{
    XEvent	event;
    XPoint	xpoint, lastPt;
    Boolean	first = True;
    Display	*dpy = XtDisplay(AppData.drawingArea);
    Window 	win = XtWindow(AppData.drawingArea);
    Boolean 	scrolled;

    lastPt = *initPt;

    do {
	/*
	** Wait for a motion or button release event
	*/
	getNextMouseEvent(dpy, win, &event);

	/*
	** See if user moved outside of window, and scroll drawing if so
	*/
	scrolled = checkScrolling(event.xbutton.x, event.xbutton.y, initPt);

	/*
	** Store new coordinates in xpoint
	*/
	xpoint.x = event.xbutton.x;
	xpoint.y = event.xbutton.y;
	
	/*
	** If the mouse moved, update
	*/
	if (xpoint.x - lastPt.x || xpoint.y - lastPt.y) {
	    doRotate(initPt, &xpoint, &first, scrolled, rot, e);
	}
	lastPt = xpoint;
    } while (event.type != ButtonRelease);
} /* end doRotateLoop() */

/*************************************************************
**
** FUNCTION:	rotateElement
**
** DESCRIPTION:	Rotate an element in the picture
**
** PARAMETERS:	xpt		Initial mouse down point
**		e		Element being moved
**
** RETURN:	None
**
*************************************************************/

void rotateElement(xpt, e)
    XPoint *xpt;
    Element *e;
{
    Display *dpy = XtDisplay(AppData.drawingArea);
    float rot;
    XRect rect, copy;

    /*
    ** Copy the original to the composite pixmap
    */
    copyOrigToComposite();

    /*
    ** Compute original rectangle and bounding box
    */
    rect = e->sizeBox;
    rect.x += AppData.originX;	
    rect.y -= AppData.scaledHeight - AppData.originY;
    copy = e->xBBox;
    copy.x += AppData.originX;
    copy.y -= AppData.scaledHeight - AppData.originY;

    /*
    ** Draw the bounding box to the composite pixmap and copy to window
    */
    drawRotatedRectangle(&rect, e->rotation, rect.x, rect.y);
    XCopyArea(dpy, AppData.composite,
	      XtWindow(AppData.drawingArea), AppData.gc,
	      copy.x, copy.y, copy.width+1, copy.height+1,
	      copy.x, copy.y);

    if (AppData.showBuffer) {	
	XCopyArea(dpy, AppData.composite, XtWindow(AppData.bufComp),
		  AppData.gc, 0, 0, AppData.drawingWidth,
		  AppData.drawingHeight, 0, 0);
    }

    /*
    ** Handle mouse events during rotate
    */
    doRotateLoop(xpt, &rot, e);

    /*
    ** Update rotation and redraw the picture
    */
    e->rotation += rot;
    updateElement(e);
    drawSelfAndUpdate(NULL);
    XDefineCursor(XtDisplay(AppData.drawingArea),
		  XtWindow(AppData.drawingArea), None); 
} /* end rotateElement() */

/***************************************************************
**
** FUNCTION:	initBuffers
**
** DESCRIPTION:	Creates the buffers and the gstates that refer to them
**
** PARAMETERS:	None
**
** RETURN:	None
**
***************************************************************/

static void initBuffers()
{	
    Display *dpy = XtDisplay(AppData.drawingArea);
    Window win = XtWindow(AppData.drawingArea);
    int depth;

    XtVaGetValues(AppData.drawingArea, XtNdepth, &depth, NULL);

    /*
    ** Create pixmap buffers
    */
    AppData.original = XCreatePixmap(dpy, win,
		AppData.drawingWidth, AppData.drawingHeight, depth);
    AppData.composite = XCreatePixmap(dpy, win,
		AppData.drawingWidth, AppData.drawingHeight, depth);

    /*
    ** Clear pixmaps
    */
    XFillRectangle(dpy, AppData.original, AppData.gc, 0, 0,
		   AppData.drawingWidth, AppData.drawingHeight);
    XFillRectangle(dpy, AppData.composite, AppData.gc, 0, 0,
		   AppData.drawingWidth, AppData.drawingHeight);

    /*
    ** Create gstates
    */
    (void) XDPSSetContextDrawable(AppData.dpsCtxt,
                                  AppData.original, AppData.drawingHeight);
    (void) XDPSCaptureContextGState(AppData.dpsCtxt, &AppData.origGState);
    (void) XDPSSetContextDrawable(AppData.dpsCtxt,
                                  AppData.composite, AppData.drawingHeight);
    (void) XDPSCaptureContextGState(AppData.dpsCtxt, &AppData.compGState);
} /* end initBuffers() */

/***************************************************************
**
** FUNCTION:	initDPSContext
**
** DESCRIPTION:	Handle post-Realize initialization: 
**
** PARAMETERS:	None
**
** RETURN:	None
**
***************************************************************/

void initDPSContext()
{
    Display *dpy = XtDisplay(AppData.drawingArea);
    XPoint xpt1, xpt2;
    Point pt;
    int i;

    /*
    ** Get height and width of drawing window
    */
    XtVaGetValues(AppData.drawingArea, XtNheight, &AppData.drawingHeight,
		  XtNwidth, &AppData.drawingWidth, NULL);

    /*
    ** Create the DPSContext in which rendering will occur
    */
    AppData.dpsCtxt = XDPSGetSharedContext(dpy);
    (void) XDPSSetEventDelivery(dpy, dps_event_pass_through);

    if (AppData.dpsCtxt == NULL) {
	printf("Couldn't create a Display PostScript context.\n");
	exit(1);
    }

    if (XDPSSetContextDrawable(AppData.dpsCtxt, XtWindow(AppData.drawingArea),
                               AppData.drawingHeight) != dps_status_success) {
        printf ("Couldn't set Display PostScript context drawable.\n");
        exit (1);
    }

    XDPSChainTextContext (AppData.dpsCtxt, AppData.trace);

    /*
    ** Set the default DPSContext
    */
    DPSSetContext(AppData.dpsCtxt);

    /*
    ** Create context for EPS imaging
    */
    AppData.imageCtxt =
	    XDPSCreateSimpleContext(XtDisplay(AppData.drawingArea),
				    None, None, 0, 0, DPSDefaultTextBackstop,
				    DPSDefaultErrorProc, NULL);
    if (AppData.imageCtxt == NULL) {
	printf("Couldn't create Display PostScript imaging context.\n");
	exit(1);
    }
    XDPSRegisterContext(AppData.imageCtxt, False);

    XDPSChainTextContext (AppData.imageCtxt, AppData.trace);
    PSWDefineExecFunction(AppData.imageCtxt);

    /*
    ** Create the control point font
    */
    PSWDefineFont(FontName);
    PSselectfont(FontName, CtlPtSize);

    (void) XDPSCaptureContextGState(AppData.dpsCtxt, &AppData.winGState);

    /*
    ** Initialize the buffers -- must be last initialization;
    ** leaves the right gstate
    */
    initBuffers();

    /*
    ** Get the transformation matrices
    */
    PSWGetTransform(AppData.ctm, AppData.invctm,
		    &AppData.xOffset, &AppData.yOffset);
    for (i = 0; i < 6; i++) AppData.origInvctm[i] = AppData.invctm[i];

    /*
    ** Compute how large a page would be needed to draw the whole thing
    */
    pt.x = 0;
    pt.y = 0;
    convertToX(&xpt1, &pt);
    pt.x = PAGE_WIDTH;
    pt.y = PAGE_HEIGHT;
    convertToX(&xpt2, &pt);

    AppData.scaledWidth = xpt2.x - xpt1.x;
    AppData.scaledHeight = xpt1.y - xpt2.y;

    /*
    ** Position the drawing area so the center is in the center of the window
    */
    positionDrawingArea(PAGE_WIDTH / 2, PAGE_HEIGHT / 2,
			AppData.drawingWidth / 2, AppData.drawingHeight / 2);

} /* end initDPSContext() */

/***************************************************************
**
** FUNCTION:	convertToX
**
** DESCRIPTION:	Convert user space to X coordinates. 
**
** PARAMETERS:	pXPt 	points to the target XPoint struct;
**		pUPt	points to the target Point struct;
**
** RETURN:	None
**
***************************************************************/

void convertToX (pXPt, pUPt)
    XPoint	*pXPt;
    Point	*pUPt;
{
    pXPt->x = AppData.ctm[A_COEFF] * pUPt->x + AppData.ctm[C_COEFF] * pUPt->y +
	    AppData.ctm[TX_CONS] + AppData.xOffset;
    pXPt->y = AppData.ctm[B_COEFF] * pUPt->x + AppData.ctm[D_COEFF] * pUPt->y +
	    AppData.ctm[TY_CONS] + AppData.yOffset;
} /* end convertToX() */   

/***************************************************************
**
** FUNCTION:	convertToDPS
**
** DESCRIPTION:	Convert X coordinates to user space
**
** PARAMETERS:	pXPt	points to the target XPoint struct;
**		pUPt	points to the target Point struct;
**
** RETURN:	None
**
***************************************************************/
void convertToDPS(pXPt, pUPt)
    XPoint	*pXPt;
    Point	*pUPt;
{
    int ix, iy;

    ix = pXPt->x - AppData.xOffset;
    iy = pXPt->y - AppData.yOffset;

    pUPt->x = AppData.invctm[A_COEFF] * ix + AppData.invctm[C_COEFF] * iy +
	    AppData.invctm[TX_CONS];
    pUPt->y = AppData.invctm[B_COEFF] * ix + AppData.invctm[D_COEFF] * iy +
	    AppData.invctm[TY_CONS];
} /* end convertToDPS() */   

/***************************************************************
**
** FUNCTION:	convertToOrigDPS
**
** DESCRIPTION:	Convert X coordinates to user space using the original
**		transformation matrix
**
** PARAMETERS:	pXPt	points to the target XPoint struct;
**		pUPt	points to the target Point struct;
**
** RETURN:	None
**
***************************************************************/
void convertToOrigDPS(pXPt, pUPt)
    XPoint	*pXPt;
    Point	*pUPt;
{
    int ix, iy;

    ix = pXPt->x - AppData.xOffset;
    iy = pXPt->y - AppData.yOffset;

    pUPt->x = AppData.origInvctm[A_COEFF] * ix +
	    AppData.origInvctm[C_COEFF] * iy + AppData.origInvctm[TX_CONS];
    pUPt->y = AppData.origInvctm[B_COEFF] * ix +
	    AppData.origInvctm[D_COEFF] * iy + AppData.origInvctm[TY_CONS];
} /* end convertToOrigDPS() */

