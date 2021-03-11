/*
 * $RCSfile: DialXDPS.c,v $
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

#include "Dial.h"
#include "DialWraps.h"

/***************************************************************
**
** DATA DECLARATIONS
**
***************************************************************/

/*
** These definitions are for the names of the user path arrays
** in the server.  A user object could be used as well.
*/
static char *upath1  = "upath1";
static char *upath10 = "upath10";
static char *upath45 = "upath45";
static char *upath90 = "upath90";

/***************************************************************
**
** FUNCTION:    setupTrigUserPath
**
** DESCRIPTION: Calculate the points for the angled lines at
**              equally spaced angles for the user path and
**              define them for the DPS server.
**
** PARAMETERS:  startlen    starting length from circle center
**              endlen      ending length from circle center
**              deg         delta angle in degrees
**              upathname   user path name
**
** RETURN:      None.
**
***************************************************************/
static void setupTrigUserPath(startlen, endlen, deg, upathname)
    float startlen;
    float endlen;
    float deg;
    char *upathname;
{
    int i, j, angle;
    float x, y;
    float       pts[MAX_PTS];
    char        ops[MAX_OPS];
    float bbox[4];

    /*
    ** Set the definition of the bounding box as the rectangular draw
    ** area defined for the dial
    */
    bbox[0] = 0.0;
    bbox[1] = 0.0;
    bbox[2] = AppData.width; 
    bbox[3] = AppData.height; 

    /*
    ** Set the circle center coordinate points
    */
    x = AppData.width / 2;
    y = AppData.height / 2;

    /*
    ** Initialize the array indices
    */
    i = 0; j = 0;

    for (angle = 0; angle < 360; angle += deg)
    {
        /*
        ** Calculate the coordinate points for the start of the angled
        ** line using standard trigometric calculations.  Place the
        ** DPS 'moveto' operator into the operator array.
        */
        pts[i++] = x + (float) cos (angle * RADIANS) * startlen;
        pts[i++] = y + (float) sin (angle * RADIANS) * startlen;
        ops[j++] = dps_moveto;

        /*
        ** Calculate the coordinate points for the end of the angled
        ** line using standard trigometric calculations.  Place the
        ** DPS 'lineto' operator into the operator array.
        */
        pts[i++] = x + (float) cos (angle * RADIANS) * endlen;
        pts[i++] = y + (float) sin (angle * RADIANS) * endlen;
        ops[j++] = dps_lineto;

    } /* end for */

    /*
    ** Place the user path name on the DPS stack
    */
    PSWPlaceName(upathname);

    /*
    ** Define the user path in the server
    */
    PSDoUserPath ((DPSPointer) pts, i,
        dps_float, ops, j, (DPSPointer) bbox, dps_def);

} /* end setupTrigUserPath () */

/***************************************************************
**
** FUNCTION:    setupUserPaths
**
** DESCRIPTION: Perform the initial setup for defining the user
**              path for each of the degree marks (1, 10, 45, 90).
**
** PARAMETERS:  None.
**
** RETURN:      None.
**
***************************************************************/
static void setupUserPaths()
{
    /*
    ** Define a user path for each of the degree marks (1, 10, 45, 90)
    */
    setupTrigUserPath (AppData.radius * LENGTH1,  AppData.radius,
        (float) DEGREE1, upath1);
    setupTrigUserPath (AppData.radius * LENGTH10, AppData.radius,
        (float) DEGREE10, upath10);
    setupTrigUserPath (AppData.radius * LENGTH45, AppData.radius,
        (float) DEGREE45, upath45);
    setupTrigUserPath (AppData.radius * LENGTH90, AppData.radius,
        (float) DEGREE90, upath90);

} /* end setupUserPaths () */

/***************************************************************
**
** FUNCTION:    drawRotateLines
**
** DESCRIPTION: Draw the angled lines at equally spaced angles
**              using rotated line with wraps.
**
** PARAMETERS:  color       gray scale color value
**              width       line width
**              startlen    starting length from circle center
**              endlen      ending length from circle center
**              degree      delta angle in degrees
**
** RETURN:      None.
**
***************************************************************/
static void drawRotateLines (color, width, startlen, endlen, degree)
     float color;
     float width;
     float startlen;
     float endlen;
     float degree;
{
    int angle;

    for (angle = 0; angle < 360; angle += degree)
        /*
        ** Rotate user space and add line to current path
        */
        PSWRotate_MakeLine (degree, startlen, (float) 0.0, endlen, (float) 0.0);

    /*
    ** Stroke all the lines in the current path
    */
    PSWStrokePath (color, width);

} /* end drawRotateLines () */

/***************************************************************
**
** FUNCTION:    drawRotate
**
** DESCRIPTION: Perform setup for all the line segments to be
**              drawn using rotated lines with wraps.
**
** PARAMETERS:  None.
**
** RETURN:      None.
**
***************************************************************/
void drawRotate()
{
    /*
    ** Save the graphics state to allow for translation and
    ** rotation
    */
    PSgsave ();

    /*
    ** Translate the context to the dial circle center
    */
    PStranslate (AppData.width / 2, AppData.height / 2);

    /*
    ** Draw mark on 1 degree increments
    */
    if (AppData.one)
        drawRotateLines ((float) COLOR1, (float) WIDTH1,
			 AppData.radius * LENGTH1, AppData.radius,
			 (float) DEGREE1);

    /*
    ** Draw mark on 10 degree increments
    */
    if (AppData.ten)
        drawRotateLines ((float) COLOR10,(float) WIDTH10,
			 AppData.radius * LENGTH10, AppData.radius,
			 (float) DEGREE10);

    /*
    ** Draw mark on 45 degree increments
    */
    if (AppData.fortyFive)
        drawRotateLines ((float) COLOR45,(float) WIDTH45,
			 AppData.radius * LENGTH45, AppData.radius,
			 (float) DEGREE45);

    /*
    ** Draw mark on 90 degree increments
    */
    if (AppData.ninety)
        drawRotateLines ((float) COLOR90,(float) WIDTH90,
			 AppData.radius * LENGTH90, AppData.radius,
			 (float) DEGREE90);

    /*
    ** Restore the graphics state
    */
    PSgrestore ();

} /* end drawRotate () */

/***************************************************************
**
** FUNCTION:    drawTrigLines
**
** DESCRIPTION: Draw the angled lines at equally spaced angles
**              using trigometrically calculated coordinates.
**
** PARAMETERS:  color       gray scale color value
**              width       line width
**              startlen    starting length from circle center
**              endlen      ending length from circle center
**              degree      delta angle in degrees
**
** RETURN:      None.
**
***************************************************************/
void drawTrigLines (color, width, startlen, endlen, degree)
    float color;
    float width;
    float startlen;
    float endlen;
    float degree;
{
    int angle;
    float x, y;

    x = AppData.width/2;
    y = AppData.height/2;

    for (angle = 0; angle < 360; angle += degree)
        /*
        ** Calculate the coordinate points for the ends of the angled
        ** line using standard trigometric calculations and add the
        ** line to the current path
        */
        PSWMakeLine (
            x + (float) cos (angle * RADIANS) * startlen,
            y + (float) sin (angle * RADIANS) * startlen,
            x + (float) cos (angle * RADIANS) * endlen,
            y + (float) sin (angle * RADIANS) * endlen);
    /*
    ** Stroke all the lines in the current path
    */
    PSWStrokePath (color, width);

} /* end drawTrigLines () */

/***************************************************************
**
** FUNCTION:    drawTrig
**
** DESCRIPTION: Perform setup for all the line segments to be
**              drawn using trigometrically calculated coordinates.
**
** PARAMETERS:  None.
**
** RETURN:      None.
**
***************************************************************/
void drawTrig()
{
    /*
    ** Draw mark on 1 degree increments
    */
    if (AppData.one)
        drawTrigLines ((float) COLOR1, (float) WIDTH1,
            AppData.radius * LENGTH1, AppData.radius, DEGREE1);

    /*
    ** Draw mark on 10 degree increments
    */
    if (AppData.ten)
        drawTrigLines ((float) COLOR10, (float) WIDTH10,
            AppData.radius * LENGTH10, AppData.radius, (float) DEGREE10);

    /*
    ** Draw mark on 45 degree increments
    */
    if (AppData.fortyFive)
        drawTrigLines ((float) COLOR45, (float) WIDTH45,
            AppData.radius * LENGTH45, AppData.radius, (float) DEGREE45);

    /*
    ** Draw mark on 90 degree increments
    */
    if (AppData.ninety)
        drawTrigLines ((float) COLOR90, (float) WIDTH90,
            AppData.radius * LENGTH90, AppData.radius, (float) DEGREE90);
} /* end drawTrig () */

/***************************************************************
**
** FUNCTION:    drawTrigUserPathLines
**
** DESCRIPTION: Draw the angled lines at equally spaced angles
**              using trigometrically calculated coordinates and
**              defining a user path for PSDoUserPath ().
**
** PARAMETERS:  color       gray scale color value
**              width       line width
**              startlen    starting length from circle center
**              endlen      ending length from circle center
**              degree      delta angle in degrees
**
** RETURN:      None.
**
***************************************************************/
void drawTrigUserPathLines (color, width, startlen, endlen, degree)
    float color;
    float width;
    float startlen;
    float endlen;
    float degree;
{
    int i, j, angle;
    float x, y;
    float       pts[MAX_PTS];
    char        ops[MAX_OPS];
    float bbox[4];

    /*
    ** Set the definition of the bounding box as the rectangular draw
    ** area defined for the dial
    */
    bbox[0] = 0.0;
    bbox[1] = 0.0;
    bbox[2] = AppData.width;
    bbox[3] = AppData.height;

    /*
    ** Set the circle center coordinate points
    */
    x = AppData.width / 2;
    y = AppData.height / 2;

    /*
    ** Initialize the array indices
    */
    i = 0; j = 0;

    for (angle = 0; angle < 360; angle += degree)
    {
        /*
        ** Calculate the coordinate points for the start of the angled
        ** line using standard trigometric calculations.  Place the
        ** DPS 'moveto' operator into the operator array.
        */
        pts[i++] = x + (float) cos (angle * RADIANS) * startlen;
        pts[i++] = y + (float) sin (angle * RADIANS) * startlen;
        ops[j++] = dps_moveto;

        /*
        ** Calculate the coordinate points for the end of the angled
        ** line using standard trigometric calculations.  Place the
        ** DPS 'lineto' operator into the operator array.
        */
        pts[i++] = x + (float) cos (angle * RADIANS) * endlen;
        pts[i++] = y + (float) sin (angle * RADIANS) * endlen;
        ops[j++] = dps_lineto;

    } /* end for */

    /*
    ** Set the draw color and line width
    */
    PSWSetColorWidth (color, width);

    /*
    ** Stroke all the lines in the user path
    */
    PSDoUserPath ((DPSPointer) pts, i,
        dps_float, ops, j, (DPSPointer) bbox, dps_ustroke);

} /* end drawTrigUserPathLines () */

/***************************************************************
**
** FUNCTION:    drawTrigUserPaths
**
** DESCRIPTION: Perform setup for all the line segments to be
**              drawn using trigometrically calculated coordinates
**              and defining a user path for PSDoUserPath ().
**
** PARAMETERS:  None.
**
** RETURN:      None.
**
***************************************************************/
void drawTrigUserPaths ()
{
    /*
    ** Draw mark on 1 degree increments
    */
    if (AppData.one)
        drawTrigUserPathLines ((float) COLOR1, (float) WIDTH1,
            AppData.radius * LENGTH1, AppData.radius, (float) DEGREE1);

    /*
    ** Draw mark on 10 degree increments
    */
    if (AppData.ten)
        drawTrigUserPathLines ((float) COLOR10, (float) WIDTH10,
            AppData.radius * LENGTH10, AppData.radius, (float) DEGREE10);

    /*
    ** Draw mark on 45 degree increments
    */
    if (AppData.fortyFive)
        drawTrigUserPathLines ((float) COLOR45, (float) WIDTH45,
            AppData.radius * LENGTH45, AppData.radius, (float) DEGREE45);

    /*
    ** Draw mark on 90 degree increments
    */
    if (AppData.ninety)
        drawTrigUserPathLines ((float) COLOR90, (float) WIDTH90,
            AppData.radius * LENGTH90, AppData.radius, (float) DEGREE90);
} /* end drawTrigUserPaths () */

/***************************************************************
**
** FUNCTION:    drawTrigUserPathsServer
**
** DESCRIPTION: Draw the angled lines at equally spaced angles
**              using trigometrically calculated coordinates and
**              a previously defined user path for PSDoUserPath ().
**
** PARAMETERS:  None.
**
** RETURN:      None.
**
***************************************************************/
void drawTrigUserPathsServer ()
{
    /*
    ** Draw mark on 1 degree increments
    */
    if (AppData.one)
        PSWDrawUserPath ((float) COLOR1, (float) WIDTH1, upath1);

    /*
    ** Draw mark on 10 degree increments
    */
    if (AppData.ten)
        PSWDrawUserPath ((float) COLOR10, (float) WIDTH10, upath10);

    /*
    ** Draw mark on 45 degree increments
    */
    if (AppData.fortyFive)
        PSWDrawUserPath ((float) COLOR45, (float) WIDTH45, upath45);

    /*
    ** Draw mark on 90 degree increments
    */
    if (AppData.ninety)
        PSWDrawUserPath ((float) COLOR90, (float) WIDTH90, upath90);
} /* end drawTrigUserPathsServer () */

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

void initDPSContext ()
{
    Display *dpy = XtDisplay(AppData.drawArea);
    Dimension height, width;
    float ctm[6], invctm[6];
    int xOffset, yOffset, x, y;

    /*
    ** Get height of drawing window
    */
    XtVaGetValues (AppData.drawArea, XtNheight, &height,
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

    if (XDPSSetContextDrawable(AppData.dpsCtxt, XtWindow(AppData.drawArea),
                               height) != dps_status_success)
    {
        printf ("Couldn't set Display PostScript context drawable.\n");
        exit (1);
    }
    XDPSChainTextContext (AppData.dpsCtxt, AppData.trace);

    /*
    ** Set the default DPSContext
    */
    DPSSetContext (AppData.dpsCtxt);

    /*
    ** Set up PSW definitions
    */
    PSWDefs ();

    /*
    ** Get the transform matrices
    */
    PSWGetTransform(AppData.dpsCtxt, ctm, invctm, &xOffset, &yOffset);

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

    /*
    ** Compute the maximum dimension (radius) circle that will fit in the
    ** rectangular draw area defined for the dial
    */
    if (AppData.width > AppData.height) {
        AppData.radius = AppData.height / 2 - WIDCIRCBRD / 2 - CIRCFF;
    } else AppData.radius = AppData.width / 2 - WIDCIRCBRD / 2 - CIRCFF;

    /*
    ** Set up user paths in server
    */
    setupUserPaths ();

} /* end initDPSContexts () */

/***************************************************************
**
** FUNCTION:    drawDialBackground
**
** DESCRIPTION: Erase the page and draw the dial background.
**
** PARAMETERS:  None.
**
** RETURN:      None.
**
***************************************************************/
void drawDialBackground ()
{
    PSerasepage ();
    PSarc(AppData.width / 2, AppData.height / 2, AppData.radius,
	  0.0, 360.0);
    PSWFillPath ((float)CLRCIRC);
}

/***************************************************************
**
** FUNCTION:    drawDialBorder
**
** DESCRIPTION: Draw the border around the dial after rendering the ticks.
**
** PARAMETERS:  None.
**
** RETURN:      None.
**
***************************************************************/
void drawDialBorder()
{
    PSarc(AppData.width / 2, AppData.height / 2, AppData.radius,
	  0.0, 360.0);
    PSWStrokePath ((float) CLRCIRCBRD, (float) WIDCIRCBRD);
}

