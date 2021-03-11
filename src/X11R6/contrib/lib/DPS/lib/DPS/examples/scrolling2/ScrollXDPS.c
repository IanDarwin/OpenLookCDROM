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
#include <DPS/DPSScrollW.h>
#include <Xm/TextF.h>

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
    (void) XDPSSetEventDelivery(XtDisplay(AppData.time),
				dps_event_pass_through);

    XtVaGetValues(AppData.scroller, XtNcontext, &AppData.dpsCtxt, NULL);

    if (AppData.dpsCtxt == NULL) {
	printf("Couldn't create a Display PostScript context.\n");
	exit(1);
    }

    XDPSChainTextContext (AppData.dpsCtxt, AppData.trace);

    /*
    ** Set the default DPSContext
    */
    DPSSetContext(AppData.dpsCtxt);
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
**		start		Whether this is the first call
**		incremental	Whether to do incremental drawing
**
** RETURN:	Whether drawing is complete
**
***************************************************************/

static Boolean drawPicture(clipList, clipLen, start, incremental)
    float *clipList;
    int clipLen;
    Boolean start, incremental;
{
    Page *p = &AppData.picture;
    float width, height;
    static float tx, ty;
    static Graphic *g;
    GraphicParams *gp;
    int i;
    int count;

    if (start) {
	PSWClearPage();
	if (!AppData.openDrawing) return True;

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

	firstPath = True;
	g = p->qHead;
    }

    /*
    ** If not doing clipping in the client, ignore clipping rectangles
    */
    if (!AppData.clientClipping) clipLen = 0;

    count = 0;

    for (/* */; g != NULL; g = g->next) {
	if (incremental && count >= AppData.chunkSize) break;

	/*
	** If reinitialize clip, flush saved paths first
	*/
	if (g->parms.path_type == PATH_TYPE_INITCLIP) {
	    flushSavedUserPath();
	    PSinitclip();
	    count++;
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
	count++;
    }

    /*
    ** Draw the last buffered path
    */
    if (AppData.consolidate || incremental) flushSavedUserPath();

    return g == NULL;

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
**		latestTime	most recent time
**		elapsedTime	total time since last reset
**
** RETURN:	none
**
***************************************************************/

static void getElapsedTime (startTime, latestTime, elapsedTime)
    struct timeval *startTime;
    unsigned long *latestTime, *elapsedTime;
{
    struct timezone timeZone;
    struct timeval finishTime;
    long seconds, microseconds;
    static struct timeval saveStart;

    if (AppData.resetTime) {
	saveStart = *startTime;
	AppData.resetTime = False;
    }

    gettimeofday (&finishTime, &timeZone);
    seconds = finishTime.tv_sec - saveStart.tv_sec;
    microseconds = finishTime.tv_usec - saveStart.tv_usec;

    *elapsedTime = (long)(seconds * 1000 + (microseconds/1000));

    seconds = finishTime.tv_sec - startTime->tv_sec;
    microseconds = finishTime.tv_usec - startTime->tv_usec;

    *latestTime = (long)(seconds * 1000 + (microseconds/1000));
} /* end getElapsedTime() */

/***************************************************************
**
** FUNCTION:	drawSelf
**
** DESCRIPTION:	Times and draws the current picture
**
** PARAMETERS:	clipList	Current clipping rectangles
**		clipLen		Number of rectangles in clipList
**		start		Whether this is the first call
**		incremental	Whether to do incremental drawing
**
** RETURN:	Whether drawing is complete
**
***************************************************************/

Boolean drawSelf(clipList, clipLen, start, incremental)
    float *clipList;
    int clipLen;
    Boolean start, incremental;
{
    static struct timeval t;
    char *value;
    unsigned long elapsed, total;

    if (start) {
	value = XmTextFieldGetString(AppData.incrementText);
	AppData.chunkSize = atoi(value);
	if (AppData.chunkSize <= 0) AppData.chunkSize = 1000000;
	XtFree(value);

	setWaitCursor();
	DPSWaitContext(AppData.dpsCtxt);
	markStartTime(&t);
    }

    if (drawPicture(clipList, clipLen, start, incremental)) {
	DPSWaitContext(AppData.dpsCtxt);
	getElapsedTime(&t, &elapsed, &total);
	showTime(elapsed, total);
	clearWaitCursor();
	return True;
    } else return False;
} /* end drawSelf() */

/***************************************************************
**
** FUNCTION:	abortDrawing
**
** DESCRIPTION:	Clean up when drawing is aborted
**
** PARAMETERS:	None
**
** RETURN:	None
**
***************************************************************/

void abortDrawing()
{
	clearWaitCursor();
}
