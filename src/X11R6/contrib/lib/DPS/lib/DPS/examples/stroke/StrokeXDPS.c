/*
 * $RCSfile: StrokeXDPS.c,v $
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

#include "Stroke.h"

static void MarkTime(), ReturnTime();


/***************************************************************
**
** FUNCTION:    getTransform
**
** DESCRIPTION: Get X coordinate system height and width and
**              convert them to DPS coordinate system values.
**
** PARAMETERS:  None.
**
** RETURN:      None.
**
***************************************************************/
static void getTransform()
{
    Dimension width;
    float ctm[6], invctm[6];
    int xOffset, yOffset, x, y;

    /*
    ** Get the transform matrices
    */
    PSWGetTransform(ctm, invctm, &xOffset, &yOffset);

    /*
    ** Get the width of the draw area in pixels
    */
    XtVaGetValues(AppData.window1, XtNwidth, &width, NULL);

    /*
    ** Compute the height and width of the draw area in DPS units
    */
    x = (int) width;
    y = 0;
    x -= xOffset;
    y -= yOffset;
    AppData.width = invctm[A_COEFF] * x + invctm[C_COEFF] * y +
	    invctm[TX_CONS];
    AppData.height= invctm[B_COEFF] * x + invctm[D_COEFF] * y +
	    invctm[TY_CONS];

} /* end getTransform () */

/***************************************************************
**
** FUNCTION:    makeHorizLines
**
** DESCRIPTION: Draws the horizontal lines.
**
** PARAMETERS:  None.
**
** RETURN:      None.
**
***************************************************************/
void makeHorizLines()
{
    float ypos, linespacing;

    /*
    ** Compute the spacing of the horizontal lines
    */
    linespacing = AppData.height / NUMLINESHORIZ;

    /*
    ** Build the path for the horizontal lines
    */
    for (ypos = 0.0; ypos <= AppData.height; ypos += linespacing)
        PSWMakeLine(0.0, ypos, AppData.width, ypos);

    /*
    ** Stroke the horizontal lines
    */
    PSstroke();

} /* end makeHorizLines () */



/***************************************************************
**
** FUNCTION:    makeVertLines
**
** DESCRIPTION: Draws the vertical lines.
**
** PARAMETERS:  None.
**
** RETURN:      None.
**
***************************************************************/
void makeVertLines()
{
    float xpos, linespacing;

    /*
    ** Compute the spacing of the vertical lines
    */
    linespacing = AppData.width / NUMLINESVERT;

    /*
    ** Build the path for the vertical lines
    */
    for (xpos = 0.0; xpos <= AppData.width; xpos += linespacing)
        PSWMakeLine(xpos, 0.0, xpos, AppData.height);

    /*
    ** Stroke the vertical lines
    */
    PSstroke();

} /* end makeVertLines () */


/***************************************************************
**
** FUNCTION:    makeDiagLines
**
** DESCRIPTION: Draws the diagonal lines.
**
** PARAMETERS:  None.
**
** RETURN:      None.
**
***************************************************************/
void makeDiagLines()
{
    float angle, length;

    /*
    ** Compute the length of the diagonal
    */
    length = (float) sqrt(AppData.width * AppData.width
        + AppData.height * AppData.height);

    /*
    ** Save the graphics state to allow for translation and
    ** rotation
    */
    PSgsave();

    for (angle = 0.0; angle <= 90.0; angle += DIAGDEGS)
    {
        /*
        ** Add the line from the origin at the current angle
        ** to the current path
        */
        PSWMakeLine(0.0, 0.0, length, 0.0);

        /*
        ** Rotate the context for the next line
        */
        PSrotate(DIAGDEGS);
    }

    /*
    ** Stroke the diagonal lines
    */
    PSstroke();

    /*
    ** Restore the graphics state
    */
    PSgrestore();
        

} /* end makeDiagLines () */ 

/***************************************************************
**
** FUNCTION:    makeArcs
**
** DESCRIPTION: Draws the arcs.
**
** PARAMETERS:  None.
**
** RETURN:      None.
**
***************************************************************/
void makeArcs()
{
    float radius, maxradius, spacing;
    
    /*
    ** Compute the maximum radius arc for the draw area
    */
    maxradius = (float) sqrt(AppData.width * AppData.width
        + AppData.height * AppData.height);

    /*
    ** Compute the spacing of the arcs
    */
    spacing = maxradius/NUMARCS;

    /*
    ** Build the path for the arcs
    */
    for (radius = spacing; radius <= maxradius; radius += spacing)
        PSWMakeArc(0.0, 0.0, radius, 0.0, 90.0);

    /*
    ** Stroke the arcs
    */
    PSstroke();

} /* end makeArcs () */ 

/***************************************************************
**
** FUNCTION:    doDraw
**
** DESCRIPTION: Do the drawing and update the timing
**
** PARAMETERS:  timingText: index of widget to update with the timing
**
** RETURN:      None.
**
***************************************************************/
static void doDraw(timingText)
    int timingText;
{
    long elapsedTime;

    /*
    ** Set the line width based on user interface value
    */
    PSsetlinewidth(AppData.lineWidth);

    /*
    ** Erase the page
    */
    PSerasepage();

    /*
    ** Mark the start time for rendering
    */
    DPSWaitContext (AppData.dpsCtxt);
    MarkTime ();

    if (AppData.horizontalT) makeHorizLines();
    if (AppData.verticalT) makeVertLines();
    if (AppData.diagonalT) makeDiagLines();
    if (AppData.arcT) makeArcs();

    /*
    ** Obtain the elapsed time of the rendering
    */
    DPSWaitContext (AppData.dpsCtxt);
    ReturnTime (&elapsedTime);

    /*
    ** Set the elapsed time into the display window
    */
    setTimingValue (timingText, elapsedTime);
} /* end doDraw () */

/***************************************************************
**
** FUNCTION:    drawIt
**
** DESCRIPTION: Called when draw pushbutton is activated.  Sets
**              the drawable, strokeadjustment value, and linewidth
**              before drawing the various lines.  StrokeAdjustment
**              is drawn first.  Gets the time in milleseconds for
**              each operation.
**
** PARAMETERS:  None.
**
** RETURN:      None.
**
***************************************************************/
void drawIt()
{
    /*
    ** Draw first in non-stroke adjust window
    */
    XDPSSetContextGState(AppData.dpsCtxt, AppData.gs0);
    doDraw(cTimingText0);

    /*
    ** Then draw in stroke adjust window
    */
    XDPSSetContextGState(AppData.dpsCtxt, AppData.gs1);
    doDraw(cTimingText1);
} /* end drawIt () */

/***************************************************************
**
** FUNCTION:    initDPSContext
**
** DESCRIPTION: Post-Realization initialization of DPSContext and such.
**
** PARAMETERS:  None.
**
** RETURN:      None.
**
***************************************************************/

void initDPSContext (dpy)
    Display *dpy;
{
    Dimension height;

    /*
    ** Create the DPSContext in which rendering will occur
    */
    AppData.dpsCtxt = XDPSGetSharedContext(dpy);
    (void) XDPSSetEventDelivery(dpy, dps_event_pass_through);

    if (AppData.dpsCtxt == NULL)
    {
        printf("Couldn't create a Display PostScript context.\n");
        exit(-1);
    }

    /*
    ** Enable chaining if initially used
    */
    XDPSChainTextContext (AppData.dpsCtxt, AppData.traceToggle);

    /*
    ** Set the default DPSContext
    */
    DPSSetContext(AppData.dpsCtxt);

    /*
    ** Set up PSW definitions
    */
    PSWDefs();

    /*
    ** Set up a gstate for stroke adjusted window
    */

    XtVaGetValues(AppData.window1, XtNheight, &height, NULL);

    XDPSSetContextDrawable(AppData.dpsCtxt, XtWindow(AppData.window1), height);
    PSsetstrokeadjust(True);
    XDPSCaptureContextGState(AppData.dpsCtxt, &AppData.gs1);

    /*
    ** Set up a gstate for un-stroke adjusted window
    */

    XtVaGetValues(AppData.window0, XtNheight, &height, NULL);

    XDPSSetContextDrawable(AppData.dpsCtxt, XtWindow(AppData.window0), height);
    PSsetstrokeadjust(False);
    XDPSCaptureContextGState(AppData.dpsCtxt, &AppData.gs0);

    /*
    ** Get DPS transform
    */
    getTransform();

} /* end initDPSContexts () */

static struct timeval StartTime;

/***************************************************************
**
** FUNCTION:    MarkTime
**
** DESCRIPTION: Mark the start of a timing interval.
**
** PARAMETERS:  None.
**
** RETURN:      None.
**
***************************************************************/
static void MarkTime()
{
  struct timezone timeZone;

  gettimeofday (&StartTime, &timeZone);
}

/***************************************************************
**
** FUNCTION:    ReturnTime
**
** DESCRIPTION: Return the elapsed time in milliseconds since MarkTime was called.
**
** PARAMETERS:  
**              elapsedTime: time in milliseconds
**
** RETURN:      None.
**
***************************************************************/
static void ReturnTime (elapsedTime)
     long *elapsedTime;
{
  struct timeval endTime;
  struct timezone timeZone;

  gettimeofday (&endTime, &timeZone);

  *elapsedTime = (endTime.tv_sec - StartTime.tv_sec) * 1000 +
	  (endTime.tv_usec - StartTime.tv_usec)/1000;
}

