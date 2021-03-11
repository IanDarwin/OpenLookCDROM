/*
 * $RCSfile: Line.h,v $
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

#ifndef _LINE_H
#define _LINE_H

/***************************************************************
**
** INCLUDE FILES
**
***************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <sys/time.h>

#include <X11/Intrinsic.h>
#include <Xm/Text.h>
#include <Xm/MwmUtil.h>
#include <Mrm/MrmAppl.h>        /* Motif Toolkit and MRM */

#include <DPS/dpsXclient.h>
#include <DPS/dpsXshare.h>

/***************************************************************
**
** CONSTANT DEFINITIONS
**
***************************************************************/

/*
** These numbers are matched with corresponding numbers in line.uil
*/

#define cErrorBox		0
#define cMainDrawArea		1
#define cTimingText0		2
#define cTimingText1		3
#define cTimingText2		4
#define cTimingText3		5
#define cTimingText4		6
#define cTimingText5		7
#define cTraceToggle		8
#define cTotalText		9
#define cColorScale		10
#define cWidthScale		11
#define cColorWidthButton	12
#define cScaleForm		13

#define MAXARRAY 1000
#define MAXWIDTH 5

#define A_COEFF 0
#define B_COEFF 1
#define C_COEFF 2
#define D_COEFF 3
#define TX_CONS 4
#define TY_CONS 5

#define	BGCOLOR		0.8
#define BGSTRCOLOR	0.0
#define BGSTRWIDTH	4.0

#if !defined(SVR4) && !defined(SYSV)
/* On BSD-based systems rand is really poor, so use random instead */
#define rand random
#define srand srandom
#endif /* !SVR4 && !SYSV */

/***************************************************************
**
** TYPE DECLARATIONS
**
***************************************************************/

typedef struct
{
	Widget		drawingArea;	/* drawing area widget ID */
	Widget		timing0;	/* labels for displaying timings */
	Widget		timing1;
	Widget		timing2;
	Widget		timing3;
	Widget		timing4;
	Widget		timing5;
	Widget		totalText;	/* number of lines to draw widget */
	Widget		colorWidthButton;
	Widget		colorScale;
	Widget		widthScale;
	Widget		scaleForm;
	DPSContext	dpsCtxt;	/* drawing DPS context		  */
	float		height;		/* DPS height of the X Window */
	float		width;		/* DPS width of the X Window  */
	Boolean		trace;		/* PostScript text context flag */
	Boolean		numberChanged;
	Cardinal	numberOfLines;
	XtAppContext 	appContext;

	/*
	** arrays of start and end points, width, and color
	** for the lines to be drawn
	*/
	float	X[MAXARRAY];	/* X coordinate of start point*/
	float	Y[MAXARRAY];	/* Y coordinate of start point*/
	float	X1[MAXARRAY];	/* X coordinate of end point  */
	float	Y1[MAXARRAY];	/* Y coordinate of end point  */
	float	C[MAXARRAY];	/* gray scale color value */
	float	W[MAXARRAY];	/* line width */
} AppDataType, *AppDataTypePtr;

/***************************************************************
**
** FUNCTION DELCARATIONS
**
***************************************************************/

extern void erasePage(), makeLines(), makeColorWidth(), initDPSContext();

extern void drawDPSPrintf(), drawSingleOps(), drawSimpleWraps(),
	drawWrapsBind(), drawWrapsRepeat(), drawOptimizedStroke();

/***************************************************************
**
** EXTERNAL DATA DECLARATIONS
**
***************************************************************/

/*
** Global pointers to the application name and data block
*/
extern AppDataType  AppData;

#endif /* _LINE_H -- Add nothing below this line */
