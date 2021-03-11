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
#include <stdlib.h>
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

#define cBackgroundToggle 0
#define cBigPixmapToggle 1
#define cWatchProgressToggle 2
#define cMinimalDrawingToggle 3
#define cIncrementalDrawingToggle 4
#define cTraceToggle 5
#define cIncrementSelector 6
#define cAbortDrawingToggle 7
#define cTimingText 8
#define cTotalTimingText 9

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

/***************************************************************
**
** MACRO DEFINITIONS
**
***************************************************************/

#if !defined(SVR4) && !defined(SYSV)
#define memmove(d,s,l) bcopy(s,d,l)
#endif /* !SVR4 && !SYSV */

/***************************************************************
**
** 			TYPE DECLARATIONS
**
***************************************************************/

typedef struct {
	Widget		scroller;
	Widget		drawingArea;
	Widget		time;
	Widget		totalTime;
	Widget		mainWindow;
	Widget		fileDialog;
	Widget		incrementText;
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
	DPSContext	dpsCtxt;
	Boolean		openDrawing;
	Boolean		trace;
	Boolean		zooming;
	Boolean		resetTime;
	Boolean		usePixmap;
	Boolean		bigPixmap;
	Boolean		watchProgress;
	Boolean		minimalDrawing;
	Boolean		incrementalDrawing;
	DrawStrategy	drawStrategy;
	Boolean		clientClipping;
	Boolean		optimizeChanges;
	Boolean		wireFrame;
	Boolean		consolidate;
	Boolean		abortPartialDrawing;
	int		chunkSize;
	float		newScale;	/* new scale factor */
	float		scale;		/* magnification scale */
	Cursor		waitCursor;	/* cursor while waiting */
	Page		picture;
} AppDataType, *AppDataTypePtr;

extern AppDataType AppData;

extern void initDPSContext();
extern Boolean drawSelf();
extern void putUpInfoDialog();
extern Boolean parseFile();
extern void distillFile();
extern void setWaitCursor();
extern void clearWaitCursor();
extern void showTime();
extern void setWindowSize();
extern void abortDrawing();

#endif /* _SCROLL_H -- Add nothing below this line! */
