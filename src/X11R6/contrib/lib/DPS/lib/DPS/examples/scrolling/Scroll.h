/*
 * $RCSfile: Scroll.h,v $
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

#ifndef _SCROLL_H
#define _SCROLL_H

/***************************************************************
**
** 			INCLUDE FILES
**
***************************************************************/

#include <stdio.h>
#include <X11/Xos.h>
#include <math.h>
#include <varargs.h>
#include <sys/stat.h>

#include <X11/Intrinsic.h>
#include <Xm/Text.h>
#include <Xm/DialogS.h>
#include <Xm/MwmUtil.h>
#include <Xm/MessageB.h>
#include <Mrm/MrmAppl.h>        /* Motif Toolkit and MRM */

#include <DPS/dpsXclient.h>
#include <DPS/dpsXshare.h>
#include <DPS/dpsXuserpath.h>

#include "ParserDefs.h"
#include "ScrollWraps.h"

/***************************************************************
**
** 			CONSTANT DEFINITIONS
**
***************************************************************/

/* Define the maximum error msg size */

#define MAX_STRING_LEN	256

/*
** These numbers are matched with corresponding numbers in Scrolling.uil
*/

#define cAutoDrawArea 0
#define cSelfDrawArea 1
#define cTraceToggle 2
#define cSelfHsb 3
#define cSelfVsb 4
#define cAutoScrollWin 5
#define cTimingText 6
#define cAutoRedrawToggle 7
#define cSelfRedrawToggle 8
#define cBackgroundToggle 9
#define cWatchFrame 10

#define SCROLL_AUTO 1
#define SCROLL_REDRAW 2
#define SCROLL_BACKGROUND 4

typedef enum {
    scroll_background = SCROLL_AUTO | SCROLL_BACKGROUND,
    scroll_auto_buffer = SCROLL_AUTO, 
    scroll_auto_redraw = SCROLL_AUTO | SCROLL_REDRAW,
    scroll_self_buffer = 0,
    scroll_self_redraw = SCROLL_REDRAW
} ScrollStrategy;

typedef enum {
    action_load, action_distill
} Action;

typedef enum {
    draw_paths, draw_userpaths, draw_cache
} DrawStrategy;

/* Define miscellaneous constants for application */
#define PTS_PER_INCH		72
#define PAGE_WIDTH		(PTS_PER_INCH * 8.5)
#define PAGE_HEIGHT		(PTS_PER_INCH * 11.0)

#define A_COEFF 0
#define B_COEFF 1
#define C_COEFF 2
#define D_COEFF 3
#define TX_CONS 4
#define TY_CONS 5

/***************************************************************
**
** MACRO DEFINITIONS
**
***************************************************************/

#ifdef MIN
#undef MIN
#endif
#define MIN(a,b)		((a) < (b) ? (a) : (b))

#ifdef MAX
#undef MAX
#endif
#define MAX(a,b)		((a) > (b) ? (a) : (b))

#ifdef ABS
#undef ABS
#endif
#define ABS(a)			((a) < 0 ? -(a) : (a))

#if !defined(SVR4) && !defined(SYSV)
#define memmove(d,s,l) bcopy(s,d,l)
#endif /* !SVR4 && !SYSV */

/***************************************************************
**
** 			TYPE DECLARATIONS
**
***************************************************************/

typedef struct {
	GC		gc;
	Widget		autoDrawingArea;	/* drawing area widget ID */
	Widget		autoScrolling;
	Widget		selfDrawingArea;
	Widget		selfScrolling;
	Widget		time;
	Widget		mainWindow;
	Widget		autoHScroll;
	Widget		autoVScroll;
	Widget		selfHScroll;
	Widget		selfVScroll;
	Widget		currentStrategy;
	Widget 		autoRedraw;
	Widget		selfRedraw;
	Widget		currentDraw;
	Widget		watchFrame;
	Widget		fileDialog;
	XmString	noAutoPixmapMessage;
	XmString	noSelfPixmapMessage;
	XmString	noInputFileMessage;
	XmString	noOutputFileMessage;
	XmString	noDistillFileMessage;
	XmString	noMemoryMessage;
	XmString	badReadMessage;
	XmString	badWriteMessage;
	XmString	badFileMessage;
	XmString	parserErrorMessage;
	XmString	noDistillContextMessage;
	XmString	distillErrorMessage;
	XmString	distillCompleteMessage;
	int		pixmapMaxSize;
	DPSContext	dpsCtxt;
	DPSGState	gState;
	Boolean		trace;
	Pixmap		buf;		/* Window buffer */
	Boolean		desktop;
	Boolean		zooming;
	Boolean		scrolling;	/* TRUE if scroll redraw pending */
	ScrollStrategy	scrollStrategy;
	DrawStrategy	drawStrategy;
	Boolean		showDrawing;
	Boolean		clientClipping;
	Boolean		optimizeChanges;
	Boolean		wireFrame;
	Boolean		consolidate;
	int		magnify;	/* magnification factor */
	float		scale;		/* magnification scale */
	Cursor		waitCursor;	/* cursor while waiting */
	Page		picture;
	Dimension	drawingWidth;
	Dimension	drawingHeight;
	int		scaledWidth;
	int		scaledHeight;
	int		pixmapWidth;
	int		pixmapHeight;
	int		scrollX;	/* X scrollbar value */
	int		scrollY;	/* Y scrollbar value */
	int		lastXdelta;	/* last X scroll amount */
	int		lastYdelta;	/* last Y scroll amount */
	int		halfX;		/* halftone phase X */
	int		halfY;		/* halftone phase Y */
	unsigned long	serial;		/* serial # for scroll syncing */
	int		xOffset;	/* x offset of device space origin */
	int		yOffset;	/* y offset of device space origin */
	float		ctm[6];		/* current transformation matrix */
	float		invctm[6];	/* inverse ctm */
	float		origInvctm[6];	/* original inverse ctm */
	float		origXScale;
	float		origYScale;
	int		originX;	/* where the origin is */
	int		originY;	/*    (relative to X offset) */
} AppDataType, *AppDataTypePtr;

extern AppDataType AppData;

extern void scrollProc();
extern void graphicsExpose();
extern void initDPSContext();
extern void drawSelf();
extern void setupAndDraw();
extern void setupAndDrawUnmoving();
extern void convertToDPS();
extern void convertToX();
extern void convertToOrigDPS();
extern void scaleDrawingArea();
extern void positionDrawingArea();
extern void addExposureToBBox();
extern void putUpInfoDialog();
extern Boolean parseFile();
extern void distillFile();
extern void setWaitCursor();
extern void clearWaitCursor();
extern void flushAndClear();
extern void showTime();
extern void setWindowSize();

#endif /* _SCROLL_H -- Add nothing below this line! */
