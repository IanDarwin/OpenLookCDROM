/*
 * $RCSfile: Hit.h,v $
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

#ifndef _HIT__H
#define _HIT__H

/***************************************************************
**
**	INCLUDES
**
***************************************************************/

#include <stdio.h>
#include <math.h>
#include <time.h>

#include <X11/Intrinsic.h>
#include <Xm/Text.h>
#include <Xm/DialogS.h>
#include <Xm/MwmUtil.h>
#include <Mrm/MrmAppl.h>        /* Motif Toolkit and MRM */

#include <DPS/dpsXclient.h>
#include <DPS/dpsXshare.h>
#include <DPS/dpsXuserpath.h>

#include "HitWraps.h"

/***************************************************************
**
**	CONSTANT DEFINITIONS
**
***************************************************************/

/*
** These numbers are matched with corresponding numbers in hit.uil
*/

#define cMainDrawArea       1
#define cMouseDrawArea      2
#define cBufferDrawArea0    3
#define cBufferDrawArea1    4
#define cDrawingToggle      5
#define cHitDetToggle       6
#define cZoomingToggle      7
#define	cMainHorzSBar	    8
#define	cMainVertSBar	    9

#define PTS_PER_INCH 72

#define PAGE_WIDTH	(PTS_PER_INCH * 8.5)
#define PAGE_HEIGHT	(PTS_PER_INCH * 11.0)

#define	PTS_BEZIER		4
#define	OPS_BEZIER		2
#define	PTS_CURVE_BUFFER	(PTS_BEZIER * 2)
#define	OPS_CURVE_BUFFER	3

#define	LINEWIDTH		0.15

#define COLORGRID	0.75
#define WIDTHGRID	2.0
#define SIZEGRID	36

#define	LLX(obj) ((obj)->bbox[0])
#define	LLY(obj) ((obj)->bbox[1])
#define	URX(obj) ((obj)->bbox[2])
#define	URY(obj) ((obj)->bbox[3])

#define MIN(x, y)	((x) < (y) ? (x) : (y))
#define MAX(x, y)	((x) > (y) ? (x) : (y))
#define ABS(a)		((a) < 0 ? -(a) : (a))

#define A_COEFF	0
#define B_COEFF	1
#define C_COEFF	2
#define D_COEFF	3
#define TX_CONS	4
#define TY_CONS	5

#if !defined(SVR4) && !defined(SYSV)
/* On BSD-based systems rand is really poor, so use random instead */
#define rand random
#define srand srandom
#endif /* !SVR4 && !SYSV */

/***************************************************************
**
**	TYPEDEF DECLARATIONS
**
***************************************************************/

typedef struct {
	float 		*pts;
	DPSUserPathOp	*ops;
	float		bbox[4];
	int		numPts;
	int		numOps;
} UserPath;
	
typedef struct {
        float           x; 
        float           y;
} Point;

typedef struct {
        Point       ll;
        Point       ur;
} BBox;

typedef struct {
	GC		gc;		/* our graphics context */
	DPSContext	dpsCtxt;	/* DPS context */
	Widget		drawingArea;	/* main drawing area */
	Widget		bufOrig;	/* original buffer show window */
	Widget		bufComp;	/* composite buffer show window */
	Widget		bufferBox;	/* dialog box showing buffers */
	Widget		hScroll;	/* horizontal scrollbar */
	Widget		vScroll;	/* vertical scrollbar */
	Widget		mouseArea;	/* hit point display area */
	DPSGState	origGState;	/* gstate for original buffer */
	DPSGState	compGState;	/* gstate for composite buffer */
	DPSGState	mouseGState;	/* gstate for mouse display area */
	int		xOffset;	/* x offset of device space origin */
	int		yOffset;	/* y offset of device space origin */
	float		ctm[6];		/* current transformation matrix */
	float		invctm[6];	/* inverse ctm */
	float		origInvctm[6];	/* original inverse ctm */
	int		originX;	/* where the origin is */
	int 		originY;	/*  (relative to X offset */
	float		origXScale;
	float		origYScale;
	Boolean		drawTrace;	/* drawing trace flag */
	Boolean		hitTrace;	/* hit detection trace flag */
	Boolean		zoomTrace;	/* zoom trace flag */
	Boolean		showBuffer;	/* display buffers flag */
	Boolean		gridOn;		/* display grid flag */
	Boolean		copyAll;	/* copy whole pixmaps or bboxes */
	Boolean		desktop;	/* page size <= view window */
	Boolean		selected;	/* curve selected flag */
	Boolean		zooming;	/* zooming in progress flag */
	Boolean		scrolling;	/* scrolling in progress flag */
	float		hitSize;	/* hit detect size in points */
	int		magnify;	/* magnification percent */
	float		scale;		/* magnification scale */
	Dimension	drawingHeight;	/* height of view window */
	Dimension	drawingWidth;	/* width of view window */
	int		scaledWidth;	/* width of scaled page */
	int		scaledHeight;	/* height of scaled page */
	int		scrollX;	/* X location of scroll bar */
	int		scrollY;	/* Y location of scroll bar */
	Pixmap		original;	/* original curve pixmap */
	Pixmap		composite;	/* composite curve pixmap */
} AppDataType, *AppDataTypePtr;

extern void initDPSContext();
extern void graphicExpose();
extern void scrollProc();
extern void doScroll();
extern void convertToX();
extern void convertToDPS();
extern void convertToOrigDPS();
extern void positionDrawingArea();
extern void scaleDrawingArea();
extern void drawSelf();
extern void drawSelfAndUpdate();
extern void reshapeObject();
extern void moveObject();
extern void drawSensitivityCircle();
extern void setOrigin();

extern Boolean hitControl();
extern Boolean hitObject();

/***************************************************************
**
**	EXTERNAL DATA DECLARATIONS
**
***************************************************************/

/*
** Global pointers to the application name and data block
*/
extern	AppDataType     AppData;

extern	float	CtlPtSize;
extern	char	FontName[];

#endif /* _HIT_H -- Add nothing below this line */
