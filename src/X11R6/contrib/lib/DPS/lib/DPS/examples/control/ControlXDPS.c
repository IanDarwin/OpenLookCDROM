/*
 * $RCSfile: ControlXDPS.c,v $
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
** INCLUDE FILES
**
***************************************************************/

#include "Control.h"
#include "ControlWraps.h"

/***************************************************************
**
** DATA DECLARATIONS
**
***************************************************************/

/*
** Font name for drawing with show and xyshow operators
*/
static char fontname[] = "ControlPointsFont";

/*
** The first entry in each of these arrays is the size
*/

/*
** Points and operators for drawing filled rectangles
** using large user paths and cached user paths
*/
static float ptsRectFill[] = {8, -2, -2, 0, 4, 4, 0, 0, -4};
static DPSUserPathOp opsRectFill[] = {5, dps_rmoveto, dps_rlineto, dps_rlineto,
					 dps_rlineto, dps_closepath};

/*
** Points and operators for drawing open rectangles
** using large user paths and cached user paths
*/
static float ptsRectOpen[] = {8, -2, -2, 0, 4, 4, 0, 0, -4};
static DPSUserPathOp opsRectOpen[] = {5, dps_rmoveto, dps_rlineto, dps_rlineto,
				         dps_rlineto, dps_closepath};

/*
** Points and operators for drawing X's
** using large user paths and cached user paths
*/
static float ptsX[] = {8, -2, -2, 4, 4, 0, -4, -4, 4};
static DPSUserPathOp opsX[] = {4, dps_rmoveto, dps_rlineto, dps_rmoveto,
				  dps_rlineto};

/*
** Points and operators for drawing crosses
** using large user paths and cached user paths
*/
static float ptsCross[] = {8, 0, 2, 0, -4, -2, 2, 4, 0};
static DPSUserPathOp opsCross[] = {4, dps_rmoveto, dps_rlineto, dps_rmoveto,
				      dps_rlineto};

/*
** Array of XY coordinate points at which all control points
** are drawn
*/
static float    XYPoints   [MAX_ARRAY];

/*
** Arrays to store points and operators for drawing
** using large user paths, cached user paths, and XYSHOW
*/
static DPSUserPathOp OpsBuffer   [MAX_UPATHOPS];
static char	     ShowBuffer  [MAX_UPATHOPS];
static float         XYBuffer    [MAX_UPATHPTS];
static float         BBox        [4];

/*
** The size we will use;
*/

static float figureSize = FIGURESIZE;

/*
** Variables for transformation between DPS/X and
** X Window System coordinates
*/
static float Ctm[6], Invctm[6];      /* transformation matrices */
static int XOffset, YOffset;       /* coordinate system offsets */

/***************************************************************
**
** FUNCTION:    erasePage 
**
** DESCRIPTION: Erase the current DPS drawing page.
**
** PARAMETERS:  None.
**
** RETURN:      None.
**
***************************************************************/
void erasePage ()
{
    /*
    ** Erase the DPS context page
    */
    PSerasepage ();
} /* end erasePage () */

/***************************************************************
**
** FUNCTION:    initArray
**
** DESCRIPTION: Builds the arrays of points to be drawn.
**
** PARAMETERS:  None.
**
** RETURN:      None.
**
***************************************************************/
void initArray()
{
    int i;

    /*
    ** initialize the random number generator
    */
    srand(time((time_t) NULL));

    /*
    ** Fill the array of XY coordinate points with random values
    ** adjusted to fit within the window boundaries
    */
    for (i = 0; i < 2 * MAX_POINTS; i += 2) {
        XYPoints[i] = (float) ((long) rand()
            % (((long) (AppData.width - figureSize)) * 10)
            + figureSize / 2 * 10) * 0.1;

        XYPoints[i + 1] = (float) ((long) rand()
            % (((long) (AppData.height - figureSize)) * 10)
            + figureSize / 2 * 10) * 0.1;
    }
    /*
    ** Initialize one extra XY coordinate for the relative
    ** computations done in cached user paths
    */
    XYPoints[i]   = 0;
    XYPoints[i+1] = 0;

    /*
    ** Initialize index and number of points
    */
    AppData.index     = 0;
    AppData.numPoints = 5;
    setRectFill();
} /* end initArray () */

/***************************************************************
**
** FUNCTION:    setRectFill
**
** DESCRIPTION: Set the control parameters for a filled rectangle.
**
** PARAMETERS:  None.
**
** RETURN:      None.
**
***************************************************************/
void setRectFill ()
{
    AppData.fontchar     = 'a';
    AppData.basicProc    = "BRF";
    AppData.userPtsArray = ptsRectFill;
    AppData.userOpsArray = opsRectFill;
    AppData.userOp       = dps_ufill;
    AppData.rectOp       = "rectfill";
} /* end setRectFill () */

/***************************************************************
**
** FUNCTION:    setRectOpen
**
** DESCRIPTION: Set the control parameters for an open rectangle.
**
** PARAMETERS:  None.
**
** RETURN:      None.
**
***************************************************************/
void setRectOpen ()
{
    AppData.fontchar     = 'b';
    AppData.basicProc    = "BRS";
    AppData.userPtsArray = ptsRectOpen;
    AppData.userOpsArray = opsRectOpen;
    AppData.userOp       = dps_ustroke;
    AppData.rectOp       = "rectstroke";

} /* end setRectOpen () */

/***************************************************************
**
** FUNCTION:    setX
**
** DESCRIPTION: Set the control parameters for an 'X'.
**
** PARAMETERS:  None.
**
** RETURN:      None.
**
***************************************************************/
void setX ()
{
    AppData.fontchar     = 'c';
    AppData.basicProc    = "BX";
    AppData.userPtsArray = ptsX;
    AppData.userOpsArray = opsX;
    AppData.userOp       = dps_ustroke;
    AppData.rectOp       = "pop";

} /* end setX () */

/***************************************************************
**
** FUNCTION:    setCross
**
** DESCRIPTION: Set the control parameters for a cross ('+').
**
** PARAMETERS:  None.
**
** RETURN:      None.
**
***************************************************************/
void setCross ()
{
    AppData.fontchar     = 'd';
    AppData.basicProc    = "BC";
    AppData.userPtsArray = ptsCross;
    AppData.userOpsArray = opsCross;
    AppData.userOp       = dps_ustroke;
    AppData.rectOp       = "pop";

} /* end setCross () */

/***************************************************************
**
** FUNCTION:    adjust
**
** DESCRIPTION: Adjusts X and Y to be in the center of a pixel in
**		device space
**
** PARAMETERS:  X	pointer to x value
**		Y	pointer to y value
**
** RETURN:      None.
**
***************************************************************/
static void adjust(X, Y)
    register float *X, *Y;
{
    float x, y;

    /* Convert to device space */

    x = floor(Ctm[A_COEFF] * *X + Ctm[C_COEFF] * *Y + Ctm[TX_CONS]);
    y = floor(Ctm[B_COEFF] * *X + Ctm[D_COEFF] * *Y + Ctm[TY_CONS]);

    /* Stroke adjust */

    x += 0.5;
    y += 0.5;

    /* Convert back */

    *X = Invctm[A_COEFF] * x + Invctm[C_COEFF] * y + Invctm[TX_CONS];
    *Y = Invctm[B_COEFF] * x + Invctm[D_COEFF] * y +Invctm[TY_CONS];
} /* end adjust () */

/***************************************************************
**
** FUNCTION:    adjustFigureSize
**
** DESCRIPTION: Adjusts figure size so that points do not fall
**		directly on pixel boundaries.  This avoids round-off
**		error
**
** PARAMETERS:  None
**
** RETURN:      None.
**
***************************************************************/
static void adjustFigureSize()
{
    int intX, intY;
    double floatX, floatY;
    float x, y;

    /*
    ** Convert (0.5, 0.5) into user space
    */

    x = Invctm[A_COEFF] * .5 + Invctm[C_COEFF] * .5 + Invctm[TX_CONS];
    y = Invctm[B_COEFF] * .5 + Invctm[D_COEFF] * .5 + Invctm[TY_CONS];

    /*
    ** Move to a corner
    */

    x -= figureSize/2;
    y -= figureSize/2;

    /*
    ** Convert to device space
    */

    floatX = Ctm[A_COEFF] * x + Ctm[C_COEFF] * y + Ctm[TX_CONS];
    floatY = Ctm[B_COEFF] * x + Ctm[D_COEFF] * y + Ctm[TY_CONS];

    /*
    ** Round to nearest integer
    */

    intX = floor(floatX + 0.5);
    intY = floor(floatY + 0.5);

    /*
    ** If we are too close to a pixel boundary, make figure larger and
    ** adjust coordinates in user path descriptions.
    */

    if (ABS(floatX - (double) intX) < 0.0005 ||
	ABS(floatY - (double) intY) < 0.0005) {
	figureSize += 0.001;

	ptsRectFill[4] = ptsRectFill[5] = figureSize;
	ptsRectFill[8] = -figureSize;
	ptsRectFill[1] = ptsRectFill[2] = -figureSize/2;

	ptsRectOpen[4] = ptsRectOpen[5] = figureSize;
	ptsRectOpen[8] = -figureSize;
	ptsRectOpen[1] = ptsRectOpen[2] = -figureSize/2;

	ptsX[3] = ptsX[4] = ptsX[8] = figureSize;
	ptsX[6] = ptsX[7] = -figureSize;
	ptsX[1] = ptsX[2] = -figureSize/2;

	ptsCross[7] = figureSize;
	ptsCross[4] = -figureSize;
	ptsCross[2] = ptsCross[6] = figureSize/2;
	ptsCross[5] = -figureSize/2;
    }
}

/***************************************************************
**
** FUNCTION:    drawBasic
**
** DESCRIPTION: Draw the control points using basic drawing.
**              Each point is rendered with a separate wrap call.
**
** PARAMETERS:  None.
**
** RETURN:      None.
**
***************************************************************/
void drawBasic ()
{
    int i;
    float x, y;

    for (i = AppData.index; i < AppData.index + (AppData.numPoints * 2);
	 i += 2) {
        /*
        ** Render the control point using basic drawing
        */
	x = XYPoints[i];
	y = XYPoints[i + 1];
	if (AppData.devIndependent) adjust(&x, &y);
        PSWBasic(x, y, AppData.basicProc);
    }
} /* end drawBasic () */

/***************************************************************
**
** FUNCTION:    drawUserCache
**
** DESCRIPTION: Draw the control points using cached user paths.
**              The user path for the control point is cached.
**              Each point is rendered with a separate wrap call.
**
** PARAMETERS:  None.
**
** RETURN:      None.
**
***************************************************************/
void drawUserCache ()
{
    int i, i_op = 0, i_pt;

    /*
    ** Set the initial operator to cache the user path
    */
    OpsBuffer[i_op++] = dps_ucache; 
    
    /*
    ** Set up the bounding box points
    */
    BBox[0] = -figureSize/2;
    BBox[1] = -figureSize/2;
    BBox[2] = figureSize/2;
    BBox[3] = figureSize/2;

    /*
    ** Start the user path out at the origin
    */
    XYBuffer[0] = 0;
    XYBuffer[1] = 0;
    OpsBuffer[i_op++] = dps_moveto;
    if (AppData.devIndependent) adjust(XYBuffer, XYBuffer+1);

    /*
    ** Copy the user path points for rendering the control point
    ** into the XY coordinate point array
    */
    i_pt = 2;
    for (i = 1;  i <= AppData.userPtsArray[0]; i++)
        XYBuffer[i_pt++] = AppData.userPtsArray[i];

    /*
    ** Copy the operators for rendering the control point into
    ** the operator array
    */
    for (i = 1;  i <= AppData.userOpsArray[0]; i++)
        OpsBuffer[i_op++] = AppData.userOpsArray[i];

    /*
    ** Save the graphics state for the translations that follow
    */
    PSgsave();

    /*
    ** Translate the user space to the first point
    ** (relative to the origin)
    */
    PStranslate(XYPoints[AppData.index], XYPoints[AppData.index + 1]);

    for (i = AppData.index; i < AppData.index + (AppData.numPoints * 2);
	 i += 2) {
        /*
        ** Render the control point using the cached user path
        */
        PSDoUserPath((DPSPointer) XYBuffer, i_pt, dps_float, OpsBuffer, i_op,
            (DPSPointer) BBox, AppData.userOp);

        /*
        ** Translate the user space to the next point
        ** (relative to the previous point)
        */
        PStranslate(XYPoints[i + 2] - XYPoints[i],
            XYPoints[i + 3] - XYPoints[i + 1]);
    }
    /*
    ** Restore the graphics state
    */
    PSgrestore();
} /* end drawUserCache () */

/***************************************************************
**
** FUNCTION:    drawUserPath
**
** DESCRIPTION: Draw the control points using large user paths.
**              Each point, DPS operator, and user path is added
**              to an XY coordinate array and an operator array.
**              The points are rendered in groups based on the
**              array limits.
**
** PARAMETERS:  None.
**
** RETURN:      None.
**
***************************************************************/
void drawUserPath ()
{
    int i, i_op, i_pt, j;

    /*
    ** Set up the bounding box points
    */
    BBox[0] = 0;
    BBox[1] = 0;
    BBox[2] = AppData.width;
    BBox[3] = AppData.height;

    i = 0; i_pt = 0; i_op = 0;

    while (i < AppData.numPoints * 2) {
        /*
        ** Send the array to the server if the array limit has
        ** been reached
        */
        if ((i_pt + AppData.userPtsArray[0] + 2 > MAX_UPATHPTS)
	    ||  (i_op + (int) AppData.userOpsArray[0] + 1 > MAX_UPATHOPS)) {
            /*
            ** Render the control points using the large user path
            */
            PSDoUserPath((DPSPointer) XYBuffer, i_pt, dps_float, OpsBuffer,
                i_op, (DPSPointer) BBox, AppData.userOp);

            i_pt = 0; i_op = 0;
        }

        /*
        ** Set the next XY coordinate point and operator
        */
        XYBuffer[i_pt++] = XYPoints[AppData.index + i++];
        XYBuffer[i_pt++] = XYPoints[AppData.index + i++];
	if (AppData.devIndependent) adjust (XYBuffer+i_pt-2, XYBuffer+i_pt-1);
        OpsBuffer[i_op++] = dps_moveto;

        /*
        ** Copy the user path points for rendering the control point
        ** into the XY coordinate point array
        */
        for ( j = 1; j <= AppData.userPtsArray[0]; j++, i_pt++)
            XYBuffer[i_pt] = AppData.userPtsArray[j];

        /*
        ** Copy the operators for rendering the control point into
        ** the operator array
        */
        for ( j = 1; j <= (int) AppData.userOpsArray[0]; j++, i_op++)
            OpsBuffer[i_op] = AppData.userOpsArray[j];
    } 
    /*
    ** Render the last control point using the large user path
    */
    PSDoUserPath((DPSPointer) XYBuffer, i_pt, dps_float, OpsBuffer,
        i_op, (DPSPointer) BBox, AppData.userOp);
} /* end drawUserPath () */

/***************************************************************
**
** FUNCTION:    drawRectOp
**
** DESCRIPTION: Draw the control points using rect operators.
**              Each point is added to an array of rectangular
**              parameters.  The points are rendered in groups
**              based on the array limit.
**
** PARAMETERS:  None.
**
** RETURN:      None.
**
***************************************************************/
void drawRectOp ()
{
    int i, j;

    for (i = AppData.index, j = 0; i < AppData.index + (AppData.numPoints * 2);
	 i += 2, j += 4) {
        /*
        ** Draw the rectangles if the array limit has been reached
        */
        if (j + 3 > MAX_RECTPTS) {
            /*
            ** Render the control points using the rect operators
            */
            PSWRectDraw(XYBuffer, j, AppData.rectOp);
            j = 0;
        }
        /*
        ** Set up XY coordinate points and size of the next
        ** rectangle to be drawn
        */
        XYBuffer[j] = XYPoints[i];
        XYBuffer[j + 1] = XYPoints[i + 1];
	if (AppData.devIndependent) adjust (XYBuffer+j, XYBuffer+j+1);
        XYBuffer[j] -= figureSize/2;
        XYBuffer[j + 1] -= figureSize/2;
        XYBuffer[j + 2] = figureSize;
        XYBuffer[j + 3] = figureSize;
    }
    /*
    ** Render the last control points using the rect operators
    */
    PSWRectDraw(XYBuffer, j, AppData.rectOp);
} /* end drawRectOp () */

/***************************************************************
**
** FUNCTION:    drawImage
**
** DESCRIPTION: Draw the control points for the composite
**              drawing approach.  The control point is rendered
**              once to a bit-clearing pixmap and a second time to a
**              bit-setting pixmap.
**
** PARAMETERS:  None.
**
** RETURN:      None.
**
***************************************************************/
void drawImage ()
{
    float x, y, x1, y1;
    Display *dpy = XtDisplay(AppData.drawingArea);
    DPSPointer pushCookie;

    XFillRectangle(dpy, AppData.bitClearPixmap, AppData.bitClearGC,
		   0, 0, AppData.pixmapWidth, AppData.pixmapHeight);

    XFillRectangle(dpy, AppData.bitSetPixmap, AppData.bitSetGC,
		   0, 0, AppData.pixmapWidth, AppData.pixmapHeight);

    /* Convert the center of the pixmap into a coordinate */

    x1 = ((float) AppData.pixmapWidth) / 2;
    y1 = -((float) AppData.pixmapHeight) / 2;

    x = Invctm[A_COEFF] * x1 + Invctm[C_COEFF] * y1 + Invctm[TX_CONS];
    y = Invctm[B_COEFF] * x1 + Invctm[D_COEFF] * y1 + Invctm[TY_CONS];

    if (AppData.devIndependent) adjust(&x, &y);

    (void) XDPSPushContextGState(AppData.dpsCtxt, AppData.bitSetGState,
				 &pushCookie);

    PSWBasic(x, y, AppData.basicProc);

    (void) XDPSSetContextGState(AppData.dpsCtxt, AppData.bitClearGState);

    PSWBasic(x, y, AppData.basicProc);

    XDPSPopContextGState(pushCookie);

} /* end drawImage () */

/***************************************************************
**
** FUNCTION:    drawPixmaps
**
** DESCRIPTION: Draw the control points using the composite
**              drawing approach.  The control point is rendered
**              once to a bit-clearing pixmap and a second time to a
**              bit-setting pixmap.  The clearing pixmap is
**              logically ANDed and the setting pixmap
**              is logically ORed to the display window.
**
** PARAMETERS:  None.
**
** RETURN:      None.
**
***************************************************************/
void drawPixmaps ()
{
    float   ux, uy;
    int     x, y;
    int     i;
    Display *dpy = XtDisplay(AppData.drawingArea);

    /*
    ** Render the control point to the bit-clearing and -setting pixmaps.
    */
    drawImage();

    /*
    ** Wait to make sure the DPS image rendering has completed
    ** before any Xlib calls using the images
    */
    DPSWaitContext(AppData.dpsCtxt);

    for (i = AppData.index; i < AppData.index + (AppData.numPoints * 2);
	 i += 2)
    {
        /*
        ** Convert the DPS user space XY coordinates to X Window
        ** System XY coordinates
        */
        ux = XYPoints [i];
        uy = XYPoints [i + 1];

        x  = Ctm[A_COEFF] * ux + Ctm[C_COEFF] * uy +
		Ctm[TX_CONS] + XOffset - AppData.pixmapWidth/2;
        y  = Ctm[B_COEFF] * ux + Ctm[D_COEFF] * uy +
		Ctm[TY_CONS] + YOffset - AppData.pixmapHeight/2;

        /*
        ** AND the bit-clearing pixmap into the display window
        */
        XCopyArea (dpy, AppData.bitClearPixmap,
            XtWindow (AppData.drawingArea), AppData.andGC,
            (int) 0, (int) 0, AppData.pixmapWidth, AppData.pixmapHeight, x, y);

        /*
        ** OR the bit-setting pixmap into the display window
        */
        XCopyArea (dpy, AppData.bitSetPixmap,
            XtWindow (AppData.drawingArea), AppData.orGC,
            (int) 0, (int) 0, AppData.pixmapWidth, AppData.pixmapHeight, x, y);
    }
} /* end drawPixmaps () */

/***************************************************************
**
** FUNCTION:    drawShow
**
** DESCRIPTION: Draw the control points using the show operator.
**              Each point is rendered with a separate wrap call.
**
** PARAMETERS:  None.
**
** RETURN:      None.
**
***************************************************************/
void drawShow ()
{
    char fontchar[2];
    int i;

    /*
    ** Set up the control point as a single character string
    */
    fontchar[0] = AppData.fontchar;
    fontchar[1] = 0; 

    /*
    ** Select and scale the font
    */
    PSselectfont(fontname, figureSize);

    for (i = AppData.index; i < AppData.index + (AppData.numPoints * 2);
	 i += 2) {
        /*
        ** Render the control point using the show operator
        */
        PSWShow(XYPoints[i], XYPoints[i+1], fontchar);
    }
} /* end drawShow () */

/***************************************************************
**
** FUNCTION:    drawXYShow
**
** DESCRIPTION: Draw the control points using the xyshow operator.
**              Each point is added to an array of XY coordinate
**              points.  The points are all rendered with one wrap
**              call.
**
** PARAMETERS:  None.
**
** RETURN:      None.
**
***************************************************************/
void drawXYShow ()
{
    int i, j;

    /*
    ** Set up the control points as a repeated character string
    */
    for (i = 0; i < AppData.numPoints; i++)
        ShowBuffer[i] = AppData.fontchar;
    ShowBuffer[i] = 0;

    /*
    ** Select and scale the font
    */
    PSselectfont(fontname, figureSize);

    for (i = AppData.index + 2, j = 0;
	 i < AppData.index + (AppData.numPoints * 2); i++, j++) {
        /*
        ** Calculate the points as delta values from the
        ** previous point
        */
        XYBuffer[j] = XYPoints[i] - XYPoints[i - 2];
    }
    XYBuffer[j++] = 0;
    XYBuffer[j++] = 0;

    /*
    ** Render the control points using the xyshow operator
    */
    PSWXYShow(XYPoints[AppData.index],
	      XYPoints[AppData.index + 1], ShowBuffer,
	      XYBuffer, j);
} /* end drawXYShow () */

/***************************************************************
**
** FUNCTION:    initPixmaps
**
** DESCRIPTION: Create bit-clearing and -setting pixmaps and GCs for the
**		control point.
**
** PARAMETERS:  None.
**
** RETURN:      None.
**
***************************************************************/
void initPixmaps()
{
    XGCValues xgc;
    float   ux, uy;
    int depth;
    Display *dpy = XtDisplay(AppData.drawingArea);

    /*
    ** Compute width and height for the composite control points
    ** (in pixels).  Add one for the line width.
    */
    ux = figureSize+1;
    uy = AppData.height - figureSize - 1;
    AppData.pixmapWidth =
	    ceil(Ctm[A_COEFF] * ux + Ctm[C_COEFF] * uy +
		 Ctm[TX_CONS] + XOffset) + 1;
    AppData.pixmapHeight =
	    ceil(Ctm[B_COEFF] * ux + Ctm[D_COEFF] * uy +
		 Ctm[TY_CONS] + YOffset) + 1;

    XtVaGetValues(AppData.drawingArea, XtNdepth, &depth, NULL);

    /*
    ** Create two pixmaps into which the composite control point
    ** will be drawn.  The first is for bit-clearing; the second is
    ** for bit-setting
    */
    AppData.bitClearPixmap =
	    XCreatePixmap (dpy, XtWindow (AppData.drawingArea),
			   AppData.pixmapWidth, AppData.pixmapHeight, depth);

    AppData.bitSetPixmap =
	    XCreatePixmap (dpy, XtWindow (AppData.drawingArea),
			   AppData.pixmapWidth, AppData.pixmapHeight, depth);

    /*
    ** Create the GC for the bit-clearing pixmap with the foreground and
    ** background pixel values defined to be all bits on
    */
    xgc.foreground = ~0;
    xgc.background = ~0;
    AppData.bitClearGC  = XCreateGC (dpy, AppData.bitClearPixmap,
				     GCForeground | GCBackground, &xgc);

    /*
    ** Create the GC for the bit-setting pixmap with the
    ** foreground and background pixel values defined to be all
    ** bits off
    */
    xgc.foreground = 0;
    xgc.background = 0;
    AppData.bitSetGC  = XCreateGC (dpy, AppData.bitSetPixmap,
				   GCForeground | GCBackground, &xgc);

    /*
    ** Create the ANDing and ORing GCs
    */
    xgc.function = GXand;
    AppData.andGC = XCreateGC (dpy, AppData.bitClearPixmap, GCFunction, &xgc);

    xgc.function = GXor;
    AppData.orGC = XCreateGC (dpy, AppData.bitSetPixmap, GCFunction, &xgc);

    /*
    ** Set the DPS context window to the bit-clearing pixmap
    */
    PSgsave();
    (void) XDPSSetContextDrawable(AppData.dpsCtxt, AppData.bitClearPixmap,
				  AppData.pixmapHeight);

    (void) XDPSCaptureContextGState(AppData.dpsCtxt, &AppData.bitSetGState);

    /*
    ** Set the DPS context window to the bit-setting pixmap
    */
    (void) XDPSSetContextDrawable(AppData.dpsCtxt, AppData.bitSetPixmap,
				    AppData.pixmapHeight);
        
    (void) XDPSCaptureContextGState(AppData.dpsCtxt, &AppData.bitClearGState);

    /*
    ** Reset the DPS context window to the display window
    */
    PSgrestore();
} /* end initPixmaps () */

/***************************************************************
**
** FUNCTION:    initDPSContext
**
** DESCRIPTION: Post-Realization initialization of DPSContext and such.
**
** PARAMETERS:  shell		Application shell for program
**
** RETURN:      None.
**
***************************************************************/

void initDPSContext (shell)
    Widget shell;
{
    Display *dpy = XtDisplay(shell);
    Dimension height;
    Dimension width;
    int x, y;

    /*
    ** Get height and width of drawing window
    */
    XtVaGetValues(AppData.drawingArea, XtNheight, &height,
		  XtNwidth, &width, NULL);

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
                               height) != dps_status_success)
    {
        printf ("Couldn't set Display PostScript context drawable.\n");
        exit (1);
    }
    XDPSChainTextContext (AppData.dpsCtxt, AppData.trace);

    /*
    ** Set the default DPSContext
    */
    DPSSetContext(AppData.dpsCtxt);

    /*
    ** Compute the DPS user space height and width of the window
    */
    PSWGetTransform(Ctm, Invctm, &XOffset, &YOffset);

    x = (int) width;
    y = 0;
    x -= XOffset;
    y -= YOffset;
    AppData.width = Invctm[A_COEFF] * x + Invctm[C_COEFF] * y + Invctm[TX_CONS];
    AppData.height= Invctm[B_COEFF] * x + Invctm[D_COEFF] * y + Invctm[TY_CONS];

    /*
    ** Adjust figure size to avoid having points fall into "pixel cracks"
    */
    adjustFigureSize();

    /*
    ** Set up PSW definitions
    */
    PSWDefineControlPoints (figureSize);
    PSWDefineFont (fontname);
    initArray (); 

    /*
    ** Create bit-clearing and -setting pixmaps
    */
    initPixmaps ();
} /* end initDPSContext () */
