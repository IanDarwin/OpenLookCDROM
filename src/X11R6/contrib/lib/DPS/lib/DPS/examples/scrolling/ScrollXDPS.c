/*
 * $RCSfile: ScrollXDPS.c,v $
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

#include "Scroll.h"

/***************************************************************
**
** FUNCTION:	initDPSContext
**
** DESCRIPTION:	Initializes the main window's DPS Context;
**		the main window must have been initialized. 
**
** PARAMETERS:	None.
**
** RETURN:	None.
**
***************************************************************/

void initDPSContext()
{
    Display *dpy = XtDisplay(AppData.time);
    XPoint xpt;
    Point pt1, pt2;
    Pixmap p;
    int depth;
    int i;

   /*
    ** Create the DPSContext in which rendering will occur
    */
    AppData.dpsCtxt = XDPSGetSharedContext(dpy);
    (void) XDPSSetEventDelivery(dpy, dps_event_pass_through);

    if (AppData.dpsCtxt == NULL) {
	printf("Couldn't create a Display PostScript context.\n");
	exit(1);
    }

    XDPSChainTextContext (AppData.dpsCtxt, AppData.trace);

    /*
    ** Set the default DPSContext
    */
    DPSSetContext(AppData.dpsCtxt);

    XtVaGetValues(AppData.autoDrawingArea, XtNdepth, &depth, NULL);
    p = XCreatePixmap(dpy,
		      RootWindowOfScreen(XtScreen(AppData.autoDrawingArea)),
		      1, 1, depth);

    XDPSSetContextDrawable(AppData.dpsCtxt, p, 1);

    PSWGetTransform(AppData.ctm, AppData.invctm,
		    &AppData.xOffset, &AppData.yOffset);
    for (i = 0; i < 6; i++) AppData.origInvctm[i] = AppData.invctm[i];

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

    XFreePixmap(dpy, p);
} /* end initDPSContext() */

/***************************************************************
**
** FUNCTION:	pathRectIntersects
**
** DESCRIPTION:	Checks whether the bounding box of the path, offset
**		by the translation, intersects the clip rectangle
**
** PARAMETERS:	pathBBox	The path bounding box
**		clipRect	The clip rectangle
**		tx, ty		Translation applied to path
**
** RETURN:	Whether intersection occurs
**
***************************************************************/

static Boolean pathRectIntersects(pathBBox, clipRect, tx, ty)
    float *pathBBox;
    float *clipRect;
    float tx, ty;
{
    float urx, ury;

    /*
    ** Clip rectangles are x/y/w/h, not llx/lly/urx/ury
    */
    urx = clipRect[0] + clipRect[2];
    ury = clipRect[1] + clipRect[3];

    return pathBBox[0]+tx <= urx && pathBBox[2]+tx >= clipRect[0] &&
	     pathBBox[1]+ty <= ury && pathBBox[3]+ty >= clipRect[1];
} /* end pathRectIntersects() */

/***************************************************************
**
** FUNCTION:	pathPathIntersects
**
** DESCRIPTION:	Checks whether the two path bboxes intersect
**
** PARAMETERS:	b1	First bbox
**		b2	Second bbox
**
** RETURN:	Wether intersection occurs
**
***************************************************************/

static Boolean pathPathIntersects(b1, b2)
    float *b1, *b2;
{
    return b1[0] <= b2[2] && b1[2] >= b2[0] &&
	    b1[1] <= b2[3] && b1[3] >= b2[1];
} /* end pathPathIntersects() */

/***************************************************************
**
** Saved and wire-frame graphics parameters
**
***************************************************************/

static GraphicParams currentParams;
static Boolean firstPath;

static GraphicParams wireParams = {
    PATH_TYPE_STROKE,
    COLOR_MODEL_GRAY,
    0.0,		/* gray */
    0.0, 0.0, 0.0,	/* reg, green, blue */
    0,			/* linewidth */
    10,			/* miter limit */
    0,			/* line join */
    0			/* line cap */
};

/***************************************************************
**
** FUNCTION:	checkGraphicsParameters
**
** DESCRIPTION:	Checks whether any of the graphics parameters differ from
**		the saved ones.
**
** PARAMETERS:	g	New graphics parameters
**
** RETURN:	Whether there are differences
**
***************************************************************/

static Boolean checkGraphicsParams(g)
    register GraphicParams *g;
{
#define NE(param) (g->param != currentParams.param)

    if (firstPath) return False;

    if (NE(path_type) || NE(color_type) || NE(linewidth) ||
	NE(miterlimit) || NE(linejoin) || NE(linecap)) return True;

    if (g->color_type == COLOR_MODEL_GRAY) {
	if (NE(gray)) return True;
    } else {
	if (NE(red) || NE(green) || NE(blue)) return True;
    }
    return False;
#undef NE
} /* end checkGraphicsParams() */

/***************************************************************
**
** FUNCTION:	setGraphicsParams
**
** DESCRIPTION:	Sets the graphics parameters, either completely or
**		selectively.  Updates saved graphics parameters
**
** PARAMETERS:	g	New graphics parameters
**
** RETURN:	None.
**
***************************************************************/

static void setGraphicsParams(g)
    register GraphicParams *g;
{
#define NE(param) (g->param != currentParams.param)
#define SET(param) (currentParams.param = g->param)

    if (firstPath || !AppData.optimizeChanges) {
	firstPath = False;
	currentParams = *g;
	PSWSetGraphicsParams(g->color_type, g->gray, g->red, g->green,
			     g->blue, g->linewidth, g->miterlimit,
		 	     g->linejoin, g->linecap);
    } else {
	SET(path_type);
	if (NE(color_type)) {
	    if (g->color_type == COLOR_MODEL_GRAY) {
		PSsetgray(g->gray);
		SET(gray);
	    } else {
		PSsetrgbcolor(g->red, g->green, g->blue);
		SET(red); SET(green); SET(blue);
	    }
	    SET(color_type);
	} else {
	    if (g->color_type == COLOR_MODEL_GRAY) {
		if (NE(gray)) {
		    PSsetgray(g->gray);
		    SET(gray);
		}
	    } else if (NE(red) || NE(green) || NE(blue)) {
		PSsetrgbcolor(g->red, g->green, g->blue);
		SET(red); SET(green); SET(blue);
	    }
	}
	if (NE(linewidth)) {
	    PSsetlinewidth(g->linewidth);
	    SET(linewidth);
	}
	if (NE(miterlimit)) {
	    PSsetmiterlimit(g->miterlimit);
	    SET(miterlimit);
	}
	if (NE(linejoin)) {
	    PSsetlinejoin(g->linejoin);
	    SET(linejoin);
	}
	if (NE(linecap)) {
	    PSsetlinecap(g->linecap);
	    SET(linecap);
	}
    }
#undef NE
#undef SET
} /* end setGraphicsParams() */

/***************************************************************
**
** FUNCTION:	drawUsingPathOps
**
** DESCRIPTION:	Draws a user path using Level 1 operators
**
** PARAMETERS:	p	The user path
**		action	The user path operator
**
** RETURN:	None.
**
***************************************************************/

static void drawUsingPathOps(p, action)
    UserPath *p;
    unsigned char action;
{
    register float *f = p->pts;
    float f0, f1, f2, f3, f4, f5;
    int i;

    PSnewpath();
    for (i = 0; i < p->num_ops - 1; i++) {
	switch (p->ops[i+1]) {
	    /*
	    ** No arcs are left in distilled files
	    */
            case dps_moveto:
	        f0 = *f++;
		f1 = *f++;
		PSmoveto(f0, f1);
		break;
            case dps_rmoveto:
	        f0 = *f++;
		f1 = *f++;
		PSrmoveto(f0, f1);
		break;
            case dps_lineto:
	        f0 = *f++;
		f1 = *f++;
		PSlineto(f0, f1);
		break;
            case dps_rlineto:
	        f0 = *f++;
		f1 = *f++;
		PSrlineto(f0, f1);
		break;
            case dps_curveto:
	        f0 = *f++;
		f1 = *f++;
		f2 = *f++;
		f3 = *f++;
		f4 = *f++;
		f5 = *f++;
		PScurveto(f0, f1, f2, f3, f4, f5);
		break;
            case dps_rcurveto:
	        f0 = *f++;
		f1 = *f++;
		f2 = *f++;
		f3 = *f++;
		f4 = *f++;
		f5 = *f++;
		PScurveto(f0, f1, f2, f3, f4, f5);
		break;
	    case dps_closepath:
		PSclosepath();
		break;
	}
    }

    switch (action) {
	case PATH_TYPE_FILL:
	    PSfill();
	    break;
	case PATH_TYPE_STROKE:
	    PSstroke();
	    break;
	case PATH_TYPE_CLIP:
	    PSclip();
	    break;
    }
} /* end drawUsingPathOps() */

/***************************************************************
**
** FUNCTION:	flushSavedUserPath
**
** DESCRIPTION:	Executes the saved user path, and then resets the
**		saved path to be empty
**
** PARAMETERS:	None.
**
** RETURN:	None.
**
***************************************************************/

static UserPath savePath;
static DPSUserPathAction saveAction;

static void flushSavedUserPath()
{
    UserPath *p = &savePath;

    if (p->num_ops == 0) return;

    PSDoUserPath((DPSPointer) p->pts, p->num_pts, dps_float,
		 p->ops, p->num_ops, (DPSPointer) p->bbox, saveAction);
    if (saveAction == dps_uappend) PSclip();

    savePath.num_pts = 0;
    savePath.num_ops = 0;
    savePath.bbox[0] = savePath.bbox[1] = 99999;
    savePath.bbox[2] = savePath.bbox[3] = -99999;
} /* end flushSavedUserPath() */

/***************************************************************
**
** FUNCTION:	addToUserPath
**
** DESCRIPTION:	Adds the path to the saved user path.  If it would
**		overflow the saved path, flush the saved path first
**
** PARAMETERS:	pts	Points of path
**		num_pts Number of points
**		ops	Path construction operators
**		num_ops	Number of operators
**		bbox	Path bbox
**		action	Path action
**
** RETURN:	None.
**
***************************************************************/

static void addToUserPath(pts, num_pts, ops, num_ops, bbox, action)
    float *pts;
    int num_pts;
    DPSUserPathOp *ops;
    int num_ops;
    float *bbox;
    DPSUserPathAction action;
{
    int i;

    /*
    ** Allocate the saved path if the first time through
    */
    if (savePath.pts == NULL) {
	savePath.pts = (float *) XtMalloc(PTS_UPATH_BUFFER * sizeof(float));
	savePath.ops = (DPSUserPathOp *)
		XtMalloc(OPS_UPATH_BUFFER * sizeof(DPSUserPathOp));
	savePath.bbox[0] = savePath.bbox[1] = 99999;
	savePath.bbox[2] = savePath.bbox[3] = -99999;
    }

    /*
    ** If this would overflow the buffers, flush the saved path
    */
    if (num_pts + savePath.num_pts > PTS_UPATH_BUFFER ||
	num_ops + savePath.num_ops > OPS_UPATH_BUFFER) flushSavedUserPath();

    /*
    ** If not a stroke operation, old and new paths must not intersect
    */
    if (action != dps_ustroke && savePath.num_ops > 0) {
	if (pathPathIntersects(bbox, savePath.bbox)) {
	    flushSavedUserPath();
	}
    }

    /*
    ** If this is not the first path in the saved path, skip the ucache
    */
    if (savePath.num_ops > 0 && ops[0] == dps_ucache) {
	ops++;
	num_ops--;
    }

    /*
    ** Copy points and operators to saved path
    */
    for (i = 0; i < num_pts; i++) savePath.pts[savePath.num_pts+i] = pts[i];
    for (i = 0; i < num_ops; i++) savePath.ops[savePath.num_ops+i] = ops[i];
    savePath.num_pts += num_pts;
    savePath.num_ops += num_ops;
    saveAction = action;

    /*
    ** Update saved path bbox
    */
    if (bbox[0] < savePath.bbox[0]) savePath.bbox[0] = bbox[0];
    if (bbox[1] < savePath.bbox[1]) savePath.bbox[1] = bbox[1];
    if (bbox[2] > savePath.bbox[2]) savePath.bbox[2] = bbox[2];
    if (bbox[3] > savePath.bbox[3]) savePath.bbox[3] = bbox[3];
} /* end addToUserPath() */

/***************************************************************
**
** FUNCTION:	drawPath
**
** DESCRIPTION:	Draws a user path
**
** PARAMETERS:	p	User path
**		action	Path type
**
** RETURN:	None.
**
***************************************************************/

static void drawPath(p, action)
    UserPath *p;
    unsigned char action;
{
    DPSUserPathAction act;
    int i;

    /*
    ** If not using user paths, draw with Level 1 operators
    */
    if (AppData.drawStrategy == draw_paths) {
	drawUsingPathOps(p, action);
	return;
    }

    /*
    ** Convert action to user path operator
    */
    switch (action) {
	case PATH_TYPE_FILL:
	    act = dps_ufill;
	    break;
	case PATH_TYPE_STROKE:
	    act = dps_ustroke;
	    break;
	case PATH_TYPE_CLIP:
	    act = dps_uappend;
	    PSnewpath();
	    break;
    }

    /*
    ** If not caching, skip the ucache operator
    */
    if (AppData.drawStrategy == draw_cache) i = 0;
    else i = 1;

    /*
    ** Either add the points to the saved path or draw the path
    */
    if (AppData.consolidate) {
	addToUserPath(p->pts, p->num_pts, p->ops + i, p->num_ops - i,
		      p->bbox, act);
    } else {
	PSDoUserPath((DPSPointer) p->pts, p->num_pts, dps_float,
		     p->ops + i, p->num_ops - i, (DPSPointer) p->bbox, act);
    
	if (action == PATH_TYPE_CLIP) PSclip();
    }
} /* drawPath() */

/***************************************************************
**
** FUNCTION:	drawPicture
**
** DESCRIPTION:	Draws the saved picture
**
** PARAMETERS:	clipList	Clipping rectangles
**		clipLen		Number of clipping rectangles
**
** RETURN:	None.
**
***************************************************************/

static void drawPicture(clipList, clipLen)
    float *clipList;
    int clipLen;
{
    Page *p = &AppData.picture;
    float width, height;
    float tx, ty;
    Graphic *g;
    GraphicParams *gp;
    int i;

    PSWDrawFrame(0, 0, (int) PAGE_WIDTH, (int) PAGE_HEIGHT);

    /*
    ** Compute dimensions of saved picture and translate it to center
    ** it in the page
    */
    width = p->bounds.ur.x - p->bounds.ll.x;
    if (width < 0) width = 0;
    height = p->bounds.ur.y - p->bounds.ll.y;
    if (height < 0) height = 0;

    tx = (PAGE_WIDTH - width) / 2 - p->bounds.ll.x;
    ty = (PAGE_HEIGHT - height) / 2 - p->bounds.ll.y;
    PStranslate(tx, ty);
    PSinitclip();

    /*
    ** If not doing clipping in the client, ignore clipping rectangles
    */
    if (!AppData.clientClipping) clipLen = 0;

    firstPath = True;
    for (g = p->qHead; g != NULL; g = g->next) {
	/*
	** If reinitialize clip, flush saved paths first
	*/
	if (g->parms.path_type == PATH_TYPE_INITCLIP) {
	    flushSavedUserPath();
	    PSinitclip();
	    continue;
	}

	/*
	** If clipping, check to see if path intersects any clipping rectangle
	*/
	if (clipLen != 0 && clipList != NULL) {
	    for (i = 0; i < clipLen; i++) {
		if (pathRectIntersects(g->path.bbox, clipList+(i*4), tx, ty))
			goto DRAW_IT;
	    }
	    /*
	    ** If path is completely outside viewing area, and it's a clip
	    ** path, flush until we come to an initclip
	    */
	    if (g->parms.path_type == PATH_TYPE_CLIP) {
		while (g->next != NULL &&
		       g->next->parms.path_type != PATH_TYPE_INITCLIP) {
		    g = g->next;
		}
	    }
	    continue;
	}

DRAW_IT:
	/*
	** If drawing wire frame, use the wire frame parameters
	*/
	if (AppData.wireFrame) gp = &wireParams;
	else gp = &g->parms;
	    
	/*
	** If consolidating and using user paths, check if any graphics
	** parameters have changed.  Flush saved path if so
	*/
	if (AppData.consolidate && AppData.drawStrategy != draw_paths &&
	    checkGraphicsParams(gp)) flushSavedUserPath();

	/*
	** Update graphics parameters and draw the path
	*/
	setGraphicsParams(gp);
	drawPath(&g->path, gp->path_type);
    }

    /*
    ** Draw the last buffered path
    */
    if (AppData.consolidate) flushSavedUserPath();

    PStranslate(-tx, -ty);
} /* end drawPicture() */

/***************************************************************
**
** FUNCTION:    markStartTime
**
** DESCRIPTION: routine to set the start time of the DPS drawing method
**
** PARAMETERS:	startTime	pointer where current time is stored
**
** RETURN:      None.
**
***************************************************************/

static void markStartTime(startTime)
    struct timeval *startTime;
{
    struct timezone timeZone;

    gettimeofday (startTime, &timeZone);
} /* end markStartTime() */

/***************************************************************
**
** FUNCTION:    getElapsedTime
**
** DESCRIPTION: Returns milliseconds since startTime
**
** PARAMETERS:	startTime	pointer to start time
**
** RETURN:	elapsed time since the start in milliseconds
**
***************************************************************/

static long getElapsedTime (startTime)
    struct timeval *startTime;
{
    struct timezone timeZone;
    struct timeval finishTime;
    long elapsedSeconds, elapsedMicroseconds;

    gettimeofday (&finishTime, &timeZone);
    elapsedSeconds = finishTime.tv_sec - startTime->tv_sec;
    elapsedMicroseconds = finishTime.tv_usec - startTime->tv_usec;

    return ((long)(elapsedSeconds * 1000 + (elapsedMicroseconds/1000)));
} /* end getElapsedtime() */

/***************************************************************
**
** FUNCTION:	convertClipsToUserSpace
**
** DESCRIPTION:	Converts X clip list to user space
**
** PARAMETERS:	xClipList	Current clipping rectangles (in X coords)
**		dpsClipCount	Number of rectangles in dpsClipList
**		xClipCount	Number of rectangles in xClipList
**
** RETURN:	dpsClipList	Clipping rectangles in user space
**
***************************************************************/

static void convertClipsToUserSpace(xClipList, dpsClipList,
				    xClipLen, dpsClipLen)
    int *xClipList;
    float **dpsClipList;
    int xClipLen;
    int *dpsClipLen;
{
    Point pt;
    XPoint xpt;
    register int i;

    if (*dpsClipLen < xClipLen) {
	*dpsClipList = (float *) XtRealloc((XtPointer) *dpsClipList,
					   xClipLen * 4 * sizeof(float));
	*dpsClipLen = xClipLen;
    }

    for (i = 0; i < xClipLen; i++) {
	xpt.x = xClipList[i*4];
	xpt.y = xClipList[i*4+1];
	convertToDPS(&xpt, &pt);
	(*dpsClipList)[i*4] = pt.x;
	(*dpsClipList)[i*4+1] = pt.y;

	xpt.x = xClipList[i*4] + xClipList[i*4+2];
	xpt.y = xClipList[i*4+1] - xClipList[i*4+3];
	convertToDPS(&xpt, &pt);
	(*dpsClipList)[i*4+2] = pt.x - (*dpsClipList)[i*4];
	(*dpsClipList)[i*4+3] = pt.y - (*dpsClipList)[i*4+1];
    }
} /* end convertClipsToUserSpace() */

/***************************************************************
**
** FUNCTION:	checkWatchEvent
**
** DESCRIPTION:	Auxilliary routine for update buffer; checks whether
**		the passed event is a GraphicsExpose or NoExpose event
**		on the drawable passed as an arg.
**
** PARAMETERS:	dpy	Display pointer
**		e	X Event pointer
**		arg	Client data (drawable to check)
**
** RETURN:	Whether event matches.
**
***************************************************************/

/* ARGSUSED */

static Bool checkWatchEvent(dpy, e, arg)
    Display *dpy;
    XEvent *e;
    char *arg;
{
    return (e->xany.window == (Window) arg &&
	    (e->type == GraphicsExpose || e->type == NoExpose));
}

/***************************************************************
**
** FUNCTION:	updateBuffer
**
** DESCRIPTION:	Updates the backing pixmap after drawing with
**		"watch progress"
**
** PARAMETERS:	clipList	Current clipping rectangles (in X coords)
**		clipLen		Number of rectangles in clipList
**
** RETURN:	None.
**
***************************************************************/
static void updateBuffer(clipList, clipLen)
    int *clipList;
    int clipLen;
{
    Drawable d = AppData.buf;
    Display *dpy = XtDisplay(AppData.currentDraw);
    static int *bboxList = NULL;
    static int bboxLen = 0, bboxCount = 0;
    static float *dpsClipList = NULL;
    static int clipCount = 0;
    int i;
    XEvent e;
    
    if (AppData.lastXdelta != 0 || AppData.lastYdelta != 0) {
	/*
	** Copy visible area to new location
	*/
	XCopyArea(dpy, d, d, AppData.gc, AppData.lastXdelta,
		  AppData.lastYdelta, AppData.drawingWidth,
		  AppData.drawingHeight, 0, 0);
    }

    DPSWaitContext(AppData.dpsCtxt);

    /*
    ** That did a sync...get rid of all GraphicsExpose and NoExpose events
    ** for the pixmap
    */
    while (XCheckIfEvent(dpy, &e, checkWatchEvent, (char *) AppData.buf)) {}
    
    for (i = 0; i < clipLen; i++) {
	XCopyArea(dpy, XtWindow(AppData.currentDraw), d,
		  AppData.gc, clipList[i*4], clipList[i*4+1] - clipList[i*4+3],
		  clipList[i*4+2], clipList[i*4+3],
		  clipList[i*4], clipList[i*4+1] - clipList[i*4+3]);
    }

    while (clipLen > 0) {
	XIfEvent(dpy, &e, checkWatchEvent, (char *) AppData.buf);
	if (e.type == GraphicsExpose) {
	    addExposureToBBox(&bboxList, &bboxLen, &bboxCount,
			      (XExposeEvent *) &e);
	    if (e.xexpose.count == 0) clipLen--;
	} else clipLen--;
    }

    if (bboxCount > 0) {
	convertClipsToUserSpace(bboxList, &dpsClipList,
				bboxCount/4, &clipCount);
	PSWSetRectViewClip(dpsClipList, bboxCount);
	drawPicture(dpsClipList, bboxCount / 4);
	bboxCount = 0;
    }
} /* end updateBuffer() */

/***************************************************************
**
** FUNCTION:	drawSelf
**
** DESCRIPTION:	Times and draws the current picture
**
** PARAMETERS:	clipList	Current clipping rectangles (in X coords)
**		clipLen		Number of rectangles in clipList
**
** RETURN:	None.
**
***************************************************************/

void drawSelf(xClipList, clipLen)
    int *xClipList;
    int clipLen;
{
    struct timeval t;
    static float *dpsClipList = NULL;
    static int clipCount = 0;

    convertClipsToUserSpace(xClipList, &dpsClipList, clipLen, &clipCount);
    PSWSetRectViewClip(dpsClipList, clipLen * 4);

    setWaitCursor();
    DPSWaitContext(AppData.dpsCtxt);
    markStartTime(&t);

    if (AppData.showDrawing && !(AppData.scrollStrategy  & SCROLL_REDRAW)) {
	PSgsave();
	PSWSetMatrixAndOffset(XtWindow(AppData.currentDraw),
			      AppData.ctm, AppData.xOffset, AppData.yOffset);
	drawPicture(dpsClipList, clipLen);
	PSgrestore();
	updateBuffer(xClipList, clipLen);
    } else drawPicture(dpsClipList, clipLen);
    PSinitviewclip();

    DPSWaitContext(AppData.dpsCtxt);
    showTime(getElapsedTime(&t));
    clearWaitCursor();
} /* end drawSelf() */

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
