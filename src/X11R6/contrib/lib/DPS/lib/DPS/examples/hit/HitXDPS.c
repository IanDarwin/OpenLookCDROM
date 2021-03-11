/*
 * $RCSfile: HitXDPS.c,v $
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
**	DATA DECLARATIONS
**
***************************************************************/

static void mergeBoxes(), bboxOfObject(), insetBox();

static int Grid;		/* User path index for grid */

static UserPath Curve;		/* User path for the bezier curve */

char FontName[] = "ControlPointsFont";
static char ControlString[] = "abba";

float CtlPtSize = 4.0;		/* size of the control points */

/***************************************************************
**
** FUNCTION:	drawControlPoints
**
** DESCRIPTION:	Displays the control points for the curve passed in
**
** PARAMETERS:	object		curve being drawn
**
** RETURN:	None
**
***************************************************************/

static void drawControlPoints(object)
    UserPath	*object;
{
    int	  i, j;
    float pts[10];

    /*
    ** Set the position of the first control point
    */
    i = 0;
    pts[i++] = object->pts[0];
    pts[i++] = object->pts[1];

    /* 
    ** Compute the positions of other points relative to the previous point 
    */
    for (j = 2; i < PTS_BEZIER * 2;  j++, i++)
	    pts[i] = object->pts[j] - object->pts[j - 2];
    pts[i++] = 0;
    pts[i++] = 0;

    PSWDrawControlPoints(pts[0], pts[1], &pts[2], 8, ControlString);
} /* end drawControlPoints() */

/***************************************************************
**
** FUNCTION:	drawControlLines
**
** DESCRIPTION: Draws the control lines for the upath
**
** PARAMETERS:	object	upath of curve to draw
**
** RETURN:	None
**
***************************************************************/

static void drawControlLines(object)
    UserPath	*object;
{
    DPSUserPathOp ops[4];

    /* 
    ** Set the operations to draw the two lines.  We can use
    ** the same coordinates and bounding box
    */ 
    ops[0] = ops[2] = dps_moveto;
    ops[1] = ops[3] = dps_lineto;

    PSDoUserPath((DPSPointer) object->pts, 8, dps_float,
		 ops, 4, (DPSPointer) object->bbox, dps_ustroke);
} /* end drawControlLines() */

/***************************************************************
**
** FUNCTION:	drawObject
**
** DESCRIPTION:	Draws the object and, if selected, the controls
**
** PARAMETERS:	object	upath of curve to draw
**
** RETURN:	None
**
***************************************************************/

static void drawObject(object)
    UserPath	*object;
{
    PSDoUserPath((DPSPointer) object->pts, object->numPts, dps_float,
		 object->ops, object->numOps, (DPSPointer) object->bbox,
		 dps_ustroke);
    if (AppData.selected) {
	drawControlPoints(object);
	drawControlLines(object);
    }
} /* end drawObject() */

/***************************************************************
**
** FUNCTION:	drawSelf
**
** DESCRIPTION:	Draws the existing curve as defined in the upath
**		Curve.  Also draws the grid if it is turned on.
**		Does not return until drawing is complete.
**
** PARAMETERS:	None
**
** RETURN:	None
**
***************************************************************/

void drawSelf()
{
    XDPSChainTextContext(AppData.dpsCtxt, AppData.drawTrace);

    PSWClearWindow(PAGE_WIDTH, PAGE_HEIGHT);

    /*
    ** Draw the background desktop, if needed
    */
    if (AppData.desktop) PSWDesktop(0.0, 0.0, PAGE_WIDTH,
				    PAGE_HEIGHT);

    /*
    ** Draw the grid if necessary
    */
    if (AppData.gridOn) PSWDrawGrid(COLORGRID, WIDTHGRID, Grid);

    /*
    ** Draw the curve and control points if selected
    */
    drawObject(&Curve);

    DPSWaitContext(AppData.dpsCtxt);
    if (AppData.drawTrace) XDPSChainTextContext(AppData.dpsCtxt, False);

    /*
    ** If showing buffers, refresh original buffer window
    */
    if (AppData.showBuffer) {
	XCopyArea(XtDisplay(AppData.drawingArea), AppData.original,
		  XtWindow(AppData.bufOrig), AppData.gc,
		  0, 0, AppData.drawingWidth,
		  AppData.drawingHeight, 0, 0);
    }
} /* end drawSelf() */

/***************************************************************
**
** FUNCTION:	drawSelfAndUpdate
**
** DESCRIPTION:	Draw the curve, then copy the original pixmap into
**		the drawing area window
**
** PARAMETERS:	None
**
** RETURN:	None
**
***************************************************************/

void drawSelfAndUpdate()
{
    drawSelf();
    XCopyArea(XtDisplay(AppData.drawingArea), AppData.original,
	      XtWindow(AppData.drawingArea), AppData.gc,
	      0, 0, AppData.drawingWidth, AppData.drawingHeight, 0, 0);
}

/***************************************************************
**
** FUNCTION:	copyCurve
**
** DESCRIPTION:	This function creates a copy of the existing
**		curve.  Since the operands are the same for
**		drawing the curve the original operand array is
**		used.
**
** PARAMETERS:	object	upath in which to copy curve
**
** RETURN:	None
**
***************************************************************/

static void copyCurve(object)
    UserPath	*object;
{
    register int i;

    /*
    ** Use the original operand array
    */
    object->ops = Curve.ops;
    object->numOps = Curve.numOps;
	
    /*
    ** Allocate space for the points on the object
    */
    object->pts = (float *) XtCalloc(PTS_CURVE_BUFFER, sizeof(float));

    /*
    ** Copy the points and bbox from the existing object
    */
    for (i = 0; i < Curve.numPts; i++) object->pts[i] = Curve.pts[i];
    object->numPts = Curve.numPts;

    for (i = 0; i < 4; i++) object->bbox[i] = Curve.bbox[i];
} /* end copyCurve() */

/***************************************************************
**
** FUNCTION:	changePoint
**
** DESCRIPTION:	Change the point number passed in by the amount 
**		passed in in pt.  Recalculate the bounds because 
**		one of the bounding points could have been the 
**		changed point.
**
** PARAMETERS:	ptNum	index of control point
**		object	upath to change
**		pt	point indicating change in x & y
**
** RETURN:	None
**
***************************************************************/

static void changePoint(ptNum, object, pt)
    int		ptNum;
    UserPath	*object;
    Point	*pt;
{
    int		i;
	
    /*
    ** Relocate the point
    */
    object->pts[ptNum*2] += pt->x;
    object->pts[ptNum*2 + 1] += pt->y;

    /*
    ** Recalculate the bounds
    */
    LLX(object) = LLY(object) = 9999;
    URX(object) = URY(object) = -9999;

    for (i = 0; i < PTS_BEZIER * 2; i += 2) {
	LLX(object) = MIN(LLX(object), object->pts[i]);
	LLY(object) = MIN(LLY(object), object->pts[i+1]);
	URX(object) = MAX(URX(object), object->pts[i]);
	URY(object) = MAX(URY(object), object->pts[i+1]);
    }
} /* end changePoint() */

/***************************************************************
**
** FUNCTION:	setPoint
**
** DESCRIPTION:	This function determines which points need to be
**		changed when one of the control points is changed.
**		ptNum is the changing control point. pt holds the 
**		relative change in each coordinate.
**
** PARAMETERS:	ptNum	index of changing control point
**		object	upath to change
**		pt	point indicating change in x & y
**
** RETURN:	None
**
***************************************************************/

static void setPoint(ptNum, object, pt)
    int		ptNum;
    UserPath	*object;
    Point	*pt;
{	
    changePoint(ptNum, object, pt);
	
    /* 
    ** If this is the first or last point, the adjacent control
    ** point must change by the same amount
    */
    if (ptNum == 0) changePoint(1, object, pt);
    else if (ptNum == 3) changePoint(2, object, pt);
} /* end setPoint() */

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
**
** RETURN:	whether scrolling occured
**
***************************************************************/

static Boolean checkScrolling(x, y)
    int x, y;
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
    ** Restore original buffer gstate and do the scroll
    */
    (void) XDPSSetContextGState(AppData.dpsCtxt, AppData.origGState);
    doScroll(deltaX, deltaY);

    /*
    ** Reflect new scrolled values in scrollbars
    */
    XtVaSetValues(AppData.hScroll, XmNvalue, AppData.scrollX, NULL);
    XtVaSetValues(AppData.vScroll, XmNvalue, AppData.scrollY, NULL);

    /*
    ** Restore composite gstate for drawing
    */
    (void) XDPSSetContextGState(AppData.dpsCtxt, AppData.compGState);

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
** FUNCTION:	computeReshapeBBox
**
** DESCRIPTION:	Compute bounding box that must be updated
**
** PARAMETERS:	object		Pointer to changing object
**		first		Whether this is the first call (updated)
**		oldBBox		Previous curve bounding box (updated)
**
** RETURN:	xll, xur	X bounds of area to update
**
***************************************************************/

static void computeReshapeBBox(object, first, oldBBox, xll, xur)
    UserPath *object;
    Boolean *first;
    BBox *oldBBox;
    XPoint *xll, *xur;
{
    BBox currentBBox;

    if (AppData.copyAll) {
	xll->x = xur->y = 0;
	xll->y = AppData.drawingHeight;
	xur->x = AppData.drawingWidth;
	return;
    }

    /*
    ** The changed area is the union of the bounding box of the 
    ** new curve and the previous curve
    */
    bboxOfObject(object, &currentBBox);
		
    if (!*first) {
	mergeBoxes(oldBBox, &currentBBox);
    } else {
	*oldBBox = currentBBox;
	*first = False;
    }

    /*
    ** Compute the changed area.
    */
    convertToX(xll, &(oldBBox->ll));
    convertToX(xur, &(oldBBox->ur));

    *oldBBox = currentBBox;
} /* end computeReshapeBBox() */

/***************************************************************
**
** FUNCTION:	doReshape
**
** DESCRIPTION:	Handle a mouse movement during reshape
**
** PARAMETERS:	object  	Changing object
**		delta		Change in mouse location original
**		first		Whether this is the first time
**		scrolled	Whether we scrolled this time through
**		ptNum		Which point is changing
**
** RETURN:	None
**
***************************************************************/

static void doReshape(object, delta, first, scrolled, ptNum)
    UserPath *object;
    Point *delta;
    Boolean *first, scrolled;
    int ptNum;
{
    static BBox	oldBBox;
    XPoint	xll, xur;
    Display	*dpy = XtDisplay(AppData.drawingArea);
    Window 	win = XtWindow(AppData.drawingArea);

    /*
    ** Change the point locations of the curve
    */
    setPoint(ptNum, object, delta);

    /*
    ** Compute area that must be updated.  If we scrolled, make it
    ** everything, but we still have to compute the bbox so we have
    ** an oldBBox for next time around
    */
    computeReshapeBBox(object, first, &oldBBox, &xll, &xur);
    if (scrolled) {
	xll.x = xur.y = 0;
	xll.y = AppData.drawingHeight;
	xur.x = AppData.drawingWidth;
    }

    /*
    ** Copy original into composite
    */
    XCopyArea(dpy, AppData.original, AppData.composite, AppData.gc,
	      xll.x, xur.y, xur.x - xll.x, xll.y - xur.y, xll.x, xur.y);

    /*
    ** Draw the new curve to the composite pixmap
    */
    drawObject(object);

    /* 
    ** Synchronize so the drawing is complete and copy into the window
    */
    DPSWaitContext(AppData.dpsCtxt);
    XCopyArea(dpy, AppData.composite, win, AppData.gc, xll.x, xur.y,
	      xur.x - xll.x, xll.y - xur.y, xll.x, xur.y);

    if (AppData.showBuffer) {	
	XCopyArea(dpy, AppData.composite, XtWindow(AppData.bufComp),
		  AppData.gc, xll.x, xur.y, xur.x - xll.x, xll.y - xur.y,
		  xll.x, xur.y);
    }
} /* end doReshape() */

/***************************************************************
**
** FUNCTION:	doReshapeLoop
**
** DESCRIPTION:	Event handling loop for Bezier reshaping
**
** PARAMETERS:	object		Pointer to changing object
**		initPt		Initial mouse down point
**		ptNum		Which point is changing
**
** RETURN:	None
**
***************************************************************/

static void doReshapeLoop(object, initPt, ptNum)
    UserPath *object;
    Point *initPt;
    int ptNum;
{
    Point	point, delta, lastPt;
    XEvent	event;
    XPoint	xpoint;
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
	scrolled = checkScrolling(event.xbutton.x, event.xbutton.y);

	/*
	** Store new coordinates in xpoint and point
	*/
	xpoint.x = event.xbutton.x;
	xpoint.y = event.xbutton.y;
	convertToDPS(&xpoint, &point);
	
	/*
	** Compute the movement of the mouse
	*/
	delta.x = point.x - lastPt.x;
	delta.y = point.y - lastPt.y;

	/*
	** If the mouse moved, update
	*/
	if (delta.x || delta.y) {
	    doReshape(object, &delta, &first, scrolled, ptNum);
	}
	lastPt = point;
    } while (event.type != ButtonRelease);
} /* end doReshapeLoop() */

/***************************************************************
**
** FUNCTION:	reshapeObject
**
** DESCRIPTION:	Redraws the graphic when one of the control
**		points changes
**
** PARAMETERS:	pt	X coordinates of button press
**		ptNum	index of control point used
**
** RETURN:	None
**
***************************************************************/

void reshapeObject(pt, ptNum)
    XPoint	*pt;
    int		ptNum;
{
    Point	initPt;
    UserPath	object;
    register int i;

    XDPSChainTextContext(AppData.dpsCtxt, AppData.drawTrace);
	
    /* 
    ** Since all our drawing is to the composite pixmap, switch to that
    ** pixmap's gstate
    */
    (void) XDPSSetContextGState(AppData.dpsCtxt, AppData.compGState);

    /*
    ** Convert the mouse point to PS coordinates
    */
    convertToDPS(pt, &initPt);

    /*
    ** Initialize the object to the existing object
    */
    copyCurve(&object);

    /*
    ** Copy the original to the composite pixmap
    */
    copyOrigToComposite();

    /*
    ** Call reshape event dispatching loop
    */
    doReshapeLoop(&object, &initPt, ptNum);

    /*
    ** Done, so update stored curve from new one
    */
    for (i = 0; i < Curve.numPts; i++) Curve.pts[i] = object.pts[i];
    for (i = 0; i < 4; i++) Curve.bbox[i] = object.bbox[i];
    XtFree((char *) object.pts);

    /*
    ** Restore drawing to original pixmap
    */
    (void) XDPSSetContextGState(AppData.dpsCtxt, AppData.origGState);
    drawSelfAndUpdate();

    if (AppData.drawTrace) XDPSChainTextContext(AppData.dpsCtxt, False);
} /* end reshapeObject() */

/***************************************************************
**
** FUNCTION:	moveAll
**
** DESCRIPTION:	Moves the graphic object using the relative point
**		change.
**
** PARAMETERS:	pt	relative point change	
**
** RETURN:	None
**
***************************************************************/

static void moveAll(pt)
    Point	*pt;
{
    int	i;
    register UserPath *c = &Curve;

    for (i = 0; i < PTS_BEZIER * 2;  i += 2) {
	Curve.pts[i] += pt->x;
	Curve.pts[i + 1] += pt->y;	
    }

    LLX(c) += pt->x;
    LLY(c) += pt->y;
    URX(c) += pt->x;
    URY(c) += pt->y;
} /* end moveAll() */

/***************************************************************
**
** FUNCTION:	computeMoveBBox
**
** DESCRIPTION:	Compute bounding box that must be updated
**
** PARAMETERS:	trans		Current translation
**		first		Whether this is the first call (updated)
**		oldBBox		Previous curve bounding box (updated)
**
** RETURN:	xll, xur	X bounds of area to update
**
***************************************************************/

static void computeMoveBBox(trans, first, oldBBox, xll, xur)
    Point *trans;
    Boolean *first;
    BBox *oldBBox;
    XPoint *xll, *xur;
{
    BBox currentBBox;

    if (AppData.copyAll) {
	xll->x = xur->y = 0;
	xll->y = AppData.drawingHeight;
	xur->x = AppData.drawingWidth;
	return;
    }

    /*
    ** The changed area is the union of the bounding box of the 
    ** translated curve and the previous curve
    */
    bboxOfObject(&Curve, &currentBBox);

    currentBBox.ll.x += trans->x;
    currentBBox.ur.x += trans->x;
    currentBBox.ll.y += trans->y;
    currentBBox.ur.y += trans->y;

    if (!*first) {
	mergeBoxes(oldBBox, &currentBBox);
    } else {
	*oldBBox = currentBBox;
	*first = False;
    }

    /*
    ** Compute the changed area.
    */
    convertToX(xll, &(oldBBox->ll));
    convertToX(xur, &(oldBBox->ur));

    *oldBBox = currentBBox;
} /* end computeMoveBBox() */

/***************************************************************
**
** FUNCTION:	doMove
**
** DESCRIPTION:	Handle a mouse movement during move
**
** PARAMETERS:	trans		Current translation
**		first		Whether this is the first call
**		scrolled	Whether we scrolled this time through
**
** RETURN:	None
**
***************************************************************/

static void doMove(trans, first, scrolled)
    Point *trans;
    Boolean *first, scrolled;
{
    static BBox	oldBBox;
    XPoint 	xll, xur;
    Display 	*dpy = XtDisplay(AppData.drawingArea);
    Window 	win = XtWindow(AppData.drawingArea);

    /*
    ** Compute area that must be updated.  If we scrolled, make it
    ** everything, but we still have to compute the bbox so we have
    ** an oldBBox for next time around
    */
    computeMoveBBox(trans, first, &oldBBox, &xll, &xur);
    if (scrolled) {
	xll.x = xur.y = 0;
	xll.y = AppData.drawingHeight;
	xur.x = AppData.drawingWidth;
    }

    /*
    ** Copy original into composite
    */
    XCopyArea(dpy, AppData.original, AppData.composite, AppData.gc,
	      xll.x, xur.y, xur.x - xll.x, xll.y - xur.y, xll.x, xur.y);

    /*
    ** Draw the translated curve to the composite pixmap
    */
    PSgsave();
    PStranslate(trans->x, trans->y);
    drawObject(&Curve);
    PSgrestore();

    /* 
    ** Synchronize so the drawing is complete before copying to the window 
    */
    DPSWaitContext(AppData.dpsCtxt);
    XCopyArea(dpy, AppData.composite, win, AppData.gc, xll.x, xur.y,
	      xur.x - xll.x, xll.y - xur.y, xll.x, xur.y);

    if (AppData.showBuffer) {
	XCopyArea(dpy, AppData.composite, XtWindow(AppData.bufComp),
		  AppData.gc, xll.x, xur.y, xur.x - xll.x, xll.y - xur.y,
		  xll.x, xur.y);
    }
} /* end doMove() */

/***************************************************************
**
** FUNCTION:	doMoveLoop
**
** DESCRIPTION:	Event handling loop for Bezier moving
**
** PARAMETERS:	initPt		Original mouse point
**
** RETURN:	trans		Returns final translation
**
***************************************************************/

static void doMoveLoop(initPt, trans)
    Point *initPt, *trans;
{
    XPoint	xpoint;
    Point	point, delta, lastPt;
    XEvent	event;
    Display 	*dpy = XtDisplay(AppData.drawingArea);
    Window 	win = XtWindow(AppData.drawingArea);
    Boolean 	scrolled, first = True;

    lastPt = *initPt;

    do {
	/*
	** Wait for a motion or button release event
	*/
	getNextMouseEvent(dpy, win, &event);

	/*
	** See if user moved outside of window, and scroll drawing if so
	*/
	scrolled = checkScrolling(event.xbutton.x, event.xbutton.y);

	/*
	** Store new coordinates in xpoint and point
	*/
	xpoint.x = event.xbutton.x;
	xpoint.y = event.xbutton.y;
	convertToDPS(&xpoint, &point);

	/*
	** Compute the movement of the mouse and total change
	*/
	delta.x = point.x - lastPt.x;
	delta.y = point.y - lastPt.y;

	trans->x = point.x - initPt->x;
	trans->y = point.y - initPt->y;
	
	/*
	** If the mouse moved, update
	*/
	if (delta.x || delta.y) doMove(trans, &first, scrolled);
	lastPt = point;
    } while (event.type != ButtonRelease);
} /* end doMoveLoop() */

/***************************************************************
**
** FUNCTION:	moveObject
**
** DESCRIPTION:	Moves the graphic object.  Essentially the same
**		algorithm is used for moving the object as in
**		reshapeObject() except translation is used to
**		move the position of the object when it is redrawn.
**
** PARAMETERS:	xpoint	position of cursor at start of move	
**
** RETURN:	None
**
***************************************************************/

void moveObject(xpt)
    XPoint	*xpt;
{
    Point	initPt, trans;

    XDPSChainTextContext(AppData.dpsCtxt, AppData.drawTrace);
	
    /* 
    ** Since all our drawing is to the composite pixmap, switch to that
    ** pixmap's gstate
    */
    (void) XDPSSetContextGState(AppData.dpsCtxt, AppData.compGState);

    /*
    ** Convert the mouse point to PS coordinates
    */
    convertToDPS(xpt, &initPt);

    /*
    ** Copy the original to the composite pixmap
    */
    copyOrigToComposite();

    doMoveLoop(&initPt, &trans);

    /*
    ** Done, so move the curve to the new location
    */
    moveAll(&trans);

    /*
    ** Restore drawing to original pixmap
    */
    (void) XDPSSetContextGState(AppData.dpsCtxt, AppData.origGState);
    drawSelfAndUpdate();

    if (AppData.drawTrace) XDPSChainTextContext(AppData.dpsCtxt, False);
} /* end moveObject() */

/***************************************************************
**
** FUNCTION:	hitControl
**
** DESCRIPTION:	Check for a hit on the control points.
**
** PARAMETERS:	xpoint		X coordinates of hit point 
**		ptNum		returned index of control point hit
**
** RETURN:	True if control point hit 
**		False if no hit
**
***************************************************************/

Boolean	hitControl(xpoint, ptNum)
    XPoint	*xpoint;
    int		*ptNum;
{
    int		i;
    Point	center;
    float	hitSize;
    float 	dx, dy;

    /*
    ** Convert mouse point into PS coordinates
    */
    convertToDPS(xpoint, &center);

    /*
    ** Check for a control point hit. Just see if the distance
    ** between the mouse point and the control point is less than
    ** the size of the control point or the mouse sensitivity
    */
    hitSize = MAX(CtlPtSize / AppData.scale,
		  AppData.hitSize / 2.0 / AppData.scale);

    for (i = 0; i < PTS_BEZIER * 2; i += 2) {
	dx = center.x - Curve.pts[i];
	dy = center.y - Curve.pts[i+1];
	if (sqrt(dx*dx + dy*dy) < hitSize) {
	    *ptNum = i/2;
	    return True;
	}
    }
    return False;
} /* end hitControl() */

/***************************************************************
**
** FUNCTION:	hitObject
**
** DESCRIPTION:	Check for a hit on the curve. This uses 
**		the inustroke operator to check for an 
**		intersection with the path of the Bezier.
**
** PARAMETERS:	xpoint	X coordinates of hit point 
**
** RETURN:	True if curve hit
**		False if no hit
**
***************************************************************/

Boolean	hitObject(xpoint)
    XPoint	*xpoint;
{
    Point	center;
    BBox	bounds;
    Boolean	hit = True;
    float	hitSize;
    
    XDPSChainTextContext(AppData.dpsCtxt, AppData.hitTrace);

    /*
    ** Convert mouse point into PS coordinates
    */
    convertToDPS(xpoint, &center);

    /*
    ** Compute the hit size in scaled user space
    */
    hitSize = AppData.hitSize / 2.0 / AppData.scale;

    /*
    ** Get the bounding box of the curve and check if the point is
    ** inside the bounding box.  The bounding box is expanded by the
    ** hit size
    */
    bboxOfObject(&Curve, &bounds);
    
    if (center.x < bounds.ll.x - hitSize || center.x > bounds.ur.x + hitSize ||
	center.y < bounds.ll.y - hitSize || center.y > bounds.ur.y + hitSize)
	    hit = False;

    if (hit) {
	/*
	** We're in the bounding box, so check against the curve
	*/
	hit = PSHitUserPath(center.x, center.y, hitSize,
			    (DPSPointer) Curve.pts, Curve.numPts, dps_float,
			    Curve.ops, Curve.numOps, (DPSPointer) Curve.bbox,
			    dps_inustroke);
    }

    if (AppData.hitTrace) XDPSChainTextContext(AppData.dpsCtxt, False);

    return hit;
} /* end hitObject() */

/***************************************************************
**
** FUNCTION:	createCurve
**
** DESCRIPTION:	Generates the initial curve.
**
** PARAMETERS:	None	
**
** RETURN:	None
**
***************************************************************/

static void createCurve()
{
    int 	i;
    XPoint	xPt;
    Point	pt;
    register UserPath *c = &Curve;

    PSerasepage();

    /*
    ** Allocate the curve point and operator arrays
    */
    Curve.pts = (float *) XtCalloc(PTS_CURVE_BUFFER, sizeof(float));
    Curve.ops = (DPSUserPathOp *) XtCalloc(OPS_CURVE_BUFFER, 
					   sizeof(DPSUserPathOp));

    /*
    ** Initialize the bounding box
    */
    LLX(c) = LLY(c) = 9999;
    URX(c) = URY(c) = -9999;

    for (i = 0; i < PTS_BEZIER * 2; i += 2) {
	/*
	** Randomly select the points for the curve
	*/
	xPt.x = rand() % ((int) AppData.drawingWidth - 8) + 4;
	xPt.y = rand() % ((int) AppData.drawingHeight - 8) + 4;
	convertToDPS(&xPt, &pt);
	c->pts[i] = pt.x;
	c->pts[i+1] = pt.y;
		
	/*
	** Update the bounding box
	*/
	LLX(c) = MIN(LLX(c), c->pts[i]);
	LLY(c) = MIN(LLY(c), c->pts[i+1]);
	URX(c) = MAX(URX(c), c->pts[i]);
	URY(c) = MAX(URY(c), c->pts[i+1]);
    }
    c->numPts = PTS_BEZIER * 2;

    /*
    ** Add the path construction operators
    */
    c->ops[0] = (DPSUserPathOp) dps_moveto;
    c->ops[1] = (DPSUserPathOp) dps_curveto;
    c->numOps = 2;

    /*
    ** Draw the curve in the buffer
    */
    drawSelf();
} /* end createCurve() */

/***************************************************************
**
** FUNCTION:	drawSensitivityCircle
**
** DESCRIPTION:	Draw the hit detection point.
**
** PARAMETERS:	None
**
** RETURN:	None
**
***************************************************************/

void drawSensitivityCircle()
{
    DPSPointer pushCookie;

    /*
    ** Temporarily set the context drawable to the mouse window
    */
    (void) XDPSPushContextGState(AppData.dpsCtxt, AppData.mouseGState,
				 &pushCookie);
    
    /*
    ** Clear the window and draw the new size
    */
    XClearWindow(XtDisplay(AppData.mouseArea), XtWindow(AppData.mouseArea));
    PSarc(0.0, 0.0, AppData.hitSize / 2.0, 0.0, 360.0);
    PSfill();

    /*
    ** Restore the previous graphics state
    */
    (void) XDPSPopContextGState(pushCookie);
} /* end drawSensitivityCircle() */

/***************************************************************
**
** FUNCTION:	createGrid
**
** DESCRIPTION:	Creates the grid.  A cached user path is created
**		and stored as a user object in the server. The 
**		grid does not change so it can be stored in the 
**		server instead of being resent each time. 
**
** PARAMETERS:	None
**
** RETURN:	None
**
***************************************************************/

static void createGrid()
{
    int			i, j, numPts, numOps;
    float		pt;
    DPSUserPathOp	*ops;
    float		*pts;
    float		bbox[4];

    /*
    ** Compute the size of the point and operator arrays and allocate them
    */
    numOps = 2 * ceil(PAGE_WIDTH / SIZEGRID +
		      PAGE_HEIGHT / SIZEGRID) + 2;
    numPts = numOps * 2;
    pts = (float *) XtCalloc(numPts, sizeof(float));
    ops = (DPSUserPathOp *) XtCalloc(numOps, sizeof(DPSUserPathOp));

    /*
    ** Make the path be cached
    */
    i = j = 0;
    ops[j++] = dps_ucache;

    /*
    ** The bounding box is the whole window
    */
    bbox[0] = 0;
    bbox[1] = 0;
    bbox[2] = PAGE_WIDTH;
    bbox[3] = PAGE_HEIGHT;

    /*
    ** Add vertical lines to path
    */
    for (pt = 0; pt < PAGE_WIDTH; pt += SIZEGRID) {
	pts[i++] = pt;
	pts[i++] = 0;
	ops[j++] = dps_moveto;
	
	pts[i++] = pt;
	pts[i++] = PAGE_HEIGHT;
	ops[j++] = dps_lineto;
    }

    /*
    ** Add horizontal lines to path
    */
    for (pt = 0; pt < PAGE_HEIGHT; pt += SIZEGRID) {
	pts[i++] = 0;
	pts[i++] = pt;
	ops[j++] = dps_moveto;
	
	pts[i++] = PAGE_WIDTH;
	pts[i++] = pt;
	ops[j++] = dps_lineto;
    }
    
    /* 
    ** Store the grid as a user object first by placing it 
    ** on the stack and then define it 
    */
    PSDoUserPath((DPSPointer) pts, i, dps_float, ops, j,
		 (DPSPointer) bbox, dps_send);
    Grid = PSDefineAsUserObj();

    XtFree((char *) pts);
    XtFree((char *) ops);
} /* end createGrid() */

/***************************************************************
**
** FUNCTION:	insetBox
**
** DESCRIPTION:	Decreases a rectangle by the deltas
**
** PARAMETERS:	pBox	pointer to bbox
**		deltaX 	change in X
**		deltaY	change in Y
**
** RETURN:	None (the bbox dimensions are updated).
**
***************************************************************/

static void insetBox (pBox, deltaX, deltaY)
    BBox	*pBox;
    float	deltaX;
    float	deltaY;
{
    pBox->ll.x += deltaX;
    pBox->ll.y += deltaY;
    pBox->ur.x -= deltaX;
    pBox->ur.y -= deltaY;
}

/***************************************************************
**
** FUNCTION:	mergeBoxes
**
** DESCRIPTION:	Adds the second bbox to the first
**
** PARAMETERS:	orig		Original bbox
**		add		Box being added
**
** RETURN:	None
**
***************************************************************/

static void mergeBoxes (orig, add)
    BBox	*orig, *add;
{
    if (add->ll.x < orig->ll.x) orig->ll.x = add->ll.x;
    if (add->ll.y < orig->ll.y) orig->ll.y = add->ll.y;
    if (add->ur.x > orig->ur.x) orig->ur.x = add->ur.x;
    if (add->ur.y > orig->ur.y) orig->ur.y = add->ur.y;
} /* end mergeBoxes() */

/***************************************************************
**
** FUNCTION:	bboxOfObject
**
** DESCRIPTION:	Return the bbox of a user path, expanded by the
**		control point (knob) size
**
** PARAMETERS:	object		User path
**		bbox		New bbox
**
** RETURN:	None
**
***************************************************************/

static void bboxOfObject(object, bbox)
    UserPath *object;
    BBox *bbox;
{
    float knobsize = (CtlPtSize+2) / AppData.scale;

    bbox->ll.x = LLX(object);
    bbox->ll.y = LLY(object);
    bbox->ur.x = URX(object);
    bbox->ur.y = URY(object);
    insetBox(bbox, -knobsize, -knobsize);
} /* end bboxOfObject() */

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
    ** Define pixmap gstates
    */
    (void) XDPSSetContextDrawable(AppData.dpsCtxt,
				  AppData.composite, AppData.drawingHeight);
    (void) XDPSCaptureContextGState(AppData.dpsCtxt, &AppData.compGState);

    (void) XDPSSetContextDrawable(AppData.dpsCtxt,
				  AppData.original, AppData.drawingHeight);
    (void) XDPSCaptureContextGState(AppData.dpsCtxt, &AppData.origGState);
} /* end initBuffers() */

/***************************************************************
**
** FUNCTION:	initMouseWindow
**
** DESCRIPTION:	Create the gstate to use to draw into the
**		mouse sensitivity window
**
** PARAMETERS:	None
**
** RETURN:	None
**
***************************************************************/

static void initMouseWindow()
{
    Dimension width, height;

    XtVaGetValues(AppData.mouseArea, XtNwidth, &width,
		  XtNheight, &height, NULL);

    (void) XDPSSetContextDrawable(AppData.dpsCtxt, XtWindow(AppData.mouseArea),
				  height);

    /*
    ** Make the origin be in the center
    */
    PSsetXoffset(width/2, height/2);
    PSsetgray(0.0);
    PSinitclip();
    PSinitviewclip();
    (void) XDPSCaptureContextGState(AppData.dpsCtxt, &AppData.mouseGState);
} /* end initMouseWindow() */

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
    XPoint xpt;
    Point pt1, pt2;
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

    /*
    ** Set the default DPSContext
    */
    DPSSetContext(AppData.dpsCtxt);

    /*
    ** Create the control point font and make it the current font
    */
    PSWDefineFont(FontName);
    PSselectfont(FontName, CtlPtSize / AppData.scale);
    PSsetlinewidth(LINEWIDTH);

    /*
    ** Initialize the window used to show the hit point size
    */
    initMouseWindow();

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

    xpt.x = 0;
    xpt.y = 100;
    convertToDPS(&xpt, &pt1);
    xpt.x = 100;
    xpt.y = 0;
    convertToDPS(&xpt, &pt2);

    AppData.origXScale = ABS(100.0 / (pt2.x - pt1.x));
    AppData.origYScale = ABS(100.0 / (pt2.y - pt1.y));

    AppData.scaledWidth = PAGE_WIDTH * AppData.origXScale * AppData.scale;
    AppData.scaledHeight = PAGE_HEIGHT * AppData.origYScale * AppData.scale;

    /*
    ** Position the drawing area so the center is in the center of the window
    */
    positionDrawingArea(PAGE_WIDTH / 2, PAGE_HEIGHT / 2,
			AppData.drawingWidth / 2, AppData.drawingHeight / 2);

    /*
    ** Create the grid and the curve
    */
    createGrid();
    createCurve();
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

