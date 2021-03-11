/*
 * $RCSfile: LineXDPS.c,v $
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

#include "Line.h"
#include "LineWraps.h"

/***************************************************************
**
** FUNCTION:	erasePage 
**
** DESCRIPTION:	Erase the current DPS drawing page.
**
** PARAMETERS:	None.
**
** RETURN:		None.
**
***************************************************************/
void erasePage ()
{
	float viewRect [4];

	/*
	** Clear the screen
	*/
	viewRect [0] = 0.0;
	viewRect [1] = 0.0;
	viewRect [2] = AppData.width;
	viewRect [3] = AppData.height;
	PSWEraseViewBind (BGCOLOR, BGSTRCOLOR, BGSTRWIDTH, viewRect);
} /* end erasePage () */

/***************************************************************
**
** FUNCTION:	makeLines
**
** DESCRIPTION:	Builds the arrays of end points for the lines to be drawn.
**
** PARAMETERS:	numLines	number of lines to draw
**
** RETURN:	None.
**
***************************************************************/
void makeLines(numLines)
	int numLines;
{
	register int i;

	for (i = 0; i < numLines; i++)
	{
		AppData.X[i]  = rand() % ((int) AppData.width  - 4) + 2;
		AppData.Y[i]  = rand() % ((int) AppData.height - 4) + 2;
		AppData.X1[i] = rand() % ((int) AppData.width  - 4) + 2;
		AppData.Y1[i] = rand() % ((int) AppData.height - 4) + 2;
	} /* for */
} /* end makeLines () */

/***************************************************************
**
** FUNCTION:    makeColorWidth
**
** DESCRIPTION: Builds arrays of either constant or random widths and
**              colors for each line
**
** PARAMETERS:	numLines - number of lines to draw
**
** RETURN:	None.
**
***************************************************************/

void makeColorWidth(numLines)
	int numLines;
{
	Boolean cwState;
	int temp;
	register int i;
	float lineColor, lineWidth;

        cwState = XmToggleButtonGetState (AppData.colorWidthButton);
        if (cwState) {
                XmScaleGetValue (AppData.colorScale, &temp);
                lineColor = ((float)temp)/1000.0;
                XmScaleGetValue (AppData.widthScale, &temp);
                lineWidth = ((float)temp)/100.0;
		for (i = 0; i < numLines; i++) {
			AppData.C[i] = lineColor;
			AppData.W[i] = lineWidth;
		}
        } else {
		for (i = 0; i < numLines; i++) {
			AppData.C[i] = (rand() % 1000) * 0.001;
			AppData.W[i] = (rand() % MAXWIDTH * 10) * .1;
		}
	}
} /* end makeColorWidth */

/***************************************************************
**
** FUNCTION:	drawDPSPrintf
**
** DESCRIPTION:	Draws the line using DPSPrintf
**
** PARAMETERS:	nlines		# of lines to draw
**
** RETURN:	None.
**
***************************************************************/
void drawDPSPrintf (nlines)
    int nlines;
{
    int i;

    /*
    ** Clear the screen
    */
    DPSPrintf(AppData.dpsCtxt, "%g setgray\n", BGCOLOR);
    DPSPrintf(AppData.dpsCtxt, "%g %g %g %g rectfill\n",
	      0.0, 0.0, AppData.width, AppData.height);
    DPSPrintf(AppData.dpsCtxt, "%g setgray\n", BGSTRCOLOR);
    DPSPrintf(AppData.dpsCtxt, "%g setlinewidth\n", BGSTRWIDTH);
    DPSPrintf(AppData.dpsCtxt, "%g %g %g %g rectstroke\n",
	      0.0, 0.0, AppData.width, AppData.height);

    /*
    ** Draw the lines
    */
    for ( i = 0; i < nlines; i++) {
	DPSPrintf(AppData.dpsCtxt, "%g setlinewidth\n", AppData.W[i]);
	DPSPrintf(AppData.dpsCtxt, "%g setgray\n", AppData.C[i]);
	DPSPrintf(AppData.dpsCtxt, "%g %g moveto\n",
		  AppData.X[i], AppData.Y[i]);
	DPSPrintf(AppData.dpsCtxt, "%g %g lineto\n",
		  AppData.X1[i], AppData.Y1[i]);
	DPSPrintf(AppData.dpsCtxt, "stroke\n");
    }
} /* end drawDPSPrintf () */

/***************************************************************
**
** FUNCTION:	drawSingleOps
**
** DESCRIPTION:	Draws the line using single operators.
**
** PARAMETERS:	nlines		# of lines to draw
**
** RETURN:	None.
**
***************************************************************/
void drawSingleOps (nlines)
    int nlines;
{
    int i;

    /*
    ** Clear the screen
    */
    PSsetgray (BGCOLOR);
    PSrectfill (0.0, 0.0, AppData.width, AppData.height);
    PSsetgray (BGSTRCOLOR);
    PSsetlinewidth (BGSTRWIDTH);
    PSrectstroke (0.0, 0.0, AppData.width, AppData.height);

    /*
    ** Draw the lines
    */
    for ( i = 0; i < nlines; i++) {
	PSsetlinewidth(AppData.W[i]);
	PSsetgray(AppData.C[i]);
	PSmoveto(AppData.X[i], AppData.Y[i]);
	PSlineto(AppData.X1[i], AppData.Y1[i]);
	PSstroke();
    }
} /* end drawSingleOps () */

/***************************************************************
**
** FUNCTION:	drawSimpleWraps
**
** DESCRIPTION:	Draws the lines using simple wraps.
**
** PARAMETERS:	nlines		# of lines to draw
**
** RETURN:	None.
**
***************************************************************/
void drawSimpleWraps (nlines)
    int nlines;
{
    int i;
    float viewRect [4];

    /*
    ** Clear the screen
    */
    viewRect [0] = 0.0;
    viewRect [1] = 0.0;
    viewRect [2] = AppData.width;
    viewRect [3] = AppData.height;
    PSWEraseView (BGCOLOR, BGSTRCOLOR, BGSTRWIDTH, viewRect);

    /*
    ** Draw the lines
    */
    for ( i = 0; i < nlines; i++) {
	PSWDrawLine(AppData.W[i], AppData.C[i], AppData.X[i], AppData.Y[i],
		    AppData.X1[i], AppData.Y1[i]);
    }
} /* end drawSimpleWraps () */

/***************************************************************
**
** FUNCTION:	drawWrapsBind
**
** DESCRIPTION:	Draws the lines usings wraps with bindings.
**
** PARAMETERS:	nlines		# of lines to draw
**
** RETURN:	None.
**
***************************************************************/
void drawWrapsBind (nlines)
    int nlines;
{
    int i;
    float viewRect [4];

    /*
    ** Clear the screen
    */
    viewRect [0] = 0.0;
    viewRect [1] = 0.0;
    viewRect [2] = AppData.width;
    viewRect [3] = AppData.height;
    PSWEraseViewBind (BGCOLOR, BGSTRCOLOR, BGSTRWIDTH, viewRect);

    /*
    ** Draw the lines
    */
    for ( i = 0; i < nlines; i++) {
	PSWDrawLineBind(AppData.W[i], AppData.C[i], AppData.X[i], AppData.Y[i],
			AppData.X1[i], AppData.Y1[i]);
    }
} /* end drawWrapsBind () */

/***************************************************************
**
** FUNCTION:	drawWrapsRepeat
**
** DESCRIPTION:	Draws the lines using wraps with repetition.
**
** PARAMETERS:	nlines		# of lines to draw
**
** RETURN:	None.
**
***************************************************************/
void drawWrapsRepeat (nlines)
    int nlines;
{
    float viewRect [4];

    /*
    ** Clear the screen
    */
    viewRect [0] = 0.0;
    viewRect [1] = 0.0;
    viewRect [2] = AppData.width;
    viewRect [3] = AppData.height;
    PSWEraseViewBind (BGCOLOR, BGSTRCOLOR, BGSTRWIDTH, viewRect);

    /*
    ** Draw the lines
    */
    PSWDrawLineRepeatBind(AppData.W, AppData.C, AppData.X, AppData.Y,
			  AppData.X1, AppData.Y1, nlines);
} /* end drawWrapsRepeat () */

/***************************************************************
**
** FUNCTION:	drawOptimizedStroke
**
** DESCRIPTION:	Draws the lines using wraps with optimized stroking.
**
** PARAMETERS:	nlines		# of lines to draw
**
** RETURN:	None.
**
***************************************************************/
void drawOptimizedStroke (nlines)
    int nlines;
{
    int i;
    float viewRect [4];

    /*
    ** Clear the screen
    */
    viewRect [0] = 0.0;
    viewRect [1] = 0.0;
    viewRect [2] = AppData.width;
    viewRect [3] = AppData.height;
    PSWEraseViewBind (BGCOLOR, BGSTRCOLOR, BGSTRWIDTH, viewRect);

    /*
    ** Draw the lines
    */
    for (i = 0; i < nlines; i++) {
	PSWMakeLineBind(AppData.X[i], AppData.Y[i],
			AppData.X1[i], AppData.Y1[i]);

	if ((i == nlines - 1)
	    || (AppData.C[i] != AppData.C[i + 1])
	    || (AppData.W[i] != AppData.W[i + 1]))

		PSWStrokeLineBind(AppData.C[i], AppData.W[i]);

    } /* for */
} /* end drawOptimizedStroke () */

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
    float ctm[6], invctm[6];
    int xOffset, yOffset, x, y;

    /*
    ** Get height and width of drawing window
    */
    XtVaGetValues( AppData.drawingArea, XtNheight, &height,
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
    ** Set up PSW definitions
    */
    PSWDefs();

    /*
    ** Compute the DPS user space height and width of the window
    */
    PSWGetTransform(AppData.dpsCtxt, ctm, invctm, &xOffset, &yOffset);

    x = (int) width;
    y = 0;
    x -= xOffset;
    y -= yOffset;
    AppData.width = invctm[A_COEFF] * x + invctm[C_COEFF] * y + invctm[TX_CONS];
    AppData.height= invctm[B_COEFF] * x + invctm[D_COEFF] * y + invctm[TY_CONS];

} /* end initDPSContext () */

