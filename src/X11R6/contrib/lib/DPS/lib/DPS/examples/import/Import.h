/*
 * $RCSfile: Import.h,v $
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

#ifndef _IMPORT__H
#define _IMPORT__H

/***************************************************************
**
**	INCLUDES
**
***************************************************************/

#include <stdio.h>
#include <sys/time.h>
#define _XOPEN_SOURCE	/* Get definition of M_PI */
#include <math.h>

#include <X11/Intrinsic.h>
#include <Xm/Text.h>
#include <Xm/DialogS.h>
#include <Xm/FileSB.h>
#include <Xm/MwmUtil.h>
#include <Mrm/MrmAppl.h>        /* Motif Toolkit and MRM */

#include <DPS/dpsXclient.h>
#include <DPS/dpsXshare.h>
#include <DPS/dpsXuserpath.h>

#include "ImportWraps.h"

/***************************************************************
**
**	CONSTANT DEFINITIONS
**
***************************************************************/

/*
** These numbers are matched with corresponding numbers in hit.uil
*/

#define cMainDrawArea       1
#define cBufferDrawArea0    2
#define cBufferDrawArea1    3
#define cTraceToggle        4
#define	cMainHorzSBar	    5
#define	cMainVertSBar	    6

#define PTS_PER_INCH 72

#define PAGE_WIDTH	(PTS_PER_INCH * 8.5)
#define PAGE_HEIGHT	(PTS_PER_INCH * 11.0)

#define MIN(x, y)	((x) < (y) ? (x) : (y))
#define MAX(x, y)	((x) > (y) ? (x) : (y))
#define ABS(a)		((a) < 0 ? -(a) : (a))
#define SIGN(a)		((a) < 0 ? -1 : 1)
#define DTOR(angle)	((angle) * M_PI / 180.0)
#define RTOD(angle)	((angle) * 180.0 / M_PI)

#define A_COEFF	0
#define B_COEFF	1
#define C_COEFF	2
#define D_COEFF	3
#define TX_CONS	4
#define TY_CONS	5

/***************************************************************
**
**	TYPEDEF DECLARATIONS
**
***************************************************************/

typedef struct {
    float 	x; 
    float 	y;
} Point;

typedef struct {
    Point       ll;
    Point       ur;
} BBox;

/*
** XRect is like XRectangle, but it has signed width and height fields
*/

typedef struct {
    int x, y, width, height;
} XRect;

typedef struct _Resource {
    String		name;
    String		version, revision;	/* For procsets only */
    Boolean		included;
    struct _Resource	*next;
} Resource;

typedef struct _ResourceType {
    String		name;
    Resource		*list;
    struct _ResourceType *next;
} ResourceType;

typedef struct _Element {
    char		*filename;	/* Name of EPS file */
    unsigned long	length;		/* Length of file in bytes */
    FILE		*f;		/* File pointer, if open */
    BBox		origBBox;	/* Bounding box from file */
    float		tx, ty;		/* Translation of element in picture */
    float		sx, sy;		/* Scale of element in picture */
    float		rotation;	/* Rotation of element in picture */
    Pixmap	 	image;		/* Rendered image pixmap */
    Pixmap		mask;		/* Rendered mask pixmap */
    XRect		xBBox;		/* Bounding box, aligned with axes */
    XRect		sizeBox;	/* Bounding box in unrotated space */
    ResourceType	*resources;	/* List of resources used by file */
    struct _Element 	*next, *prev;
} Element;

typedef struct {
	GC		gc;		/* our graphics context */
	GC		blackgc;	/* gc for drawing black */
	GC		bitmapgc;	/* gc for drawing to bitmaps */
	DPSContext	dpsCtxt;	/* DPS context */
	DPSContext	imageCtxt;	/* imaging context */
	Widget		drawingArea;	/* main drawing area */
	Widget		bufOrig;	/* original buffer show window */
	Widget		bufComp;	/* composite buffer show window */
	Widget		bufferBox;	/* dialog box showing buffers */
	Widget		hScroll;	/* horizontal scrollbar */
	Widget		vScroll;	/* vertical scrollbar */
	Widget		fileDialog;	/* file selection dialog */
	DPSGState	origGState;	/* gstate for original pixmap */
	DPSGState	compGState;	/* gstate for composite pixmap */
	DPSGState	winGState;	/* gstate for window */
	int		depth;		/* depth of drawing area */
	int		xOffset;	/* x offset of device space origin */
	int		yOffset;	/* y offset of device space origin */
	float		ctm[6];		/* current transformation matrix */
	float		invctm[6];	/* inverse ctm */
	float		origInvctm[6];	/* original inverse ctm */
	int		originX;	/* where the origin is */
	int 		originY;	/*  (relative to X offset */
	Boolean		trace;		/* trace flag */
	Boolean		showBuffer;	/* display buffers flag */
	Boolean		useBoxes;	/* use boxes instead of pictures */
	Boolean		scrolling;	/* scrolling in progress flag */
	Boolean		includePreview;	/* include preview in save file */
	Boolean		deepPreview;	/* make previews 8-bits deep */
	Boolean		rotating;	/* in the midst of rotating */
	Boolean		scaling;	/* in the midst of scaling */
	Dimension	drawingHeight;	/* height of view window */
	Dimension	drawingWidth;	/* width of view window */
	int		scaledWidth;	/* width of scaled page */
	int		scaledHeight;	/* height of scaled page */
	int		scrollX;	/* X location of scroll bar */
	int		scrollY;	/* Y location of scroll bar */
	Pixmap		original;	/* original curve pixmap */
	Pixmap		composite;	/* composite curve pixmap */
	Cursor		crosshairCursor;/* crosshair cursor */
	Cursor		busyCursor;	/* busy cursor */
	long		pixmapMaxSize;	/* maximum size for pixmaps */
	Element		*elements;	/* first (highest) element) */
	Element		*lastElement;	/* last (lowest) element */
	Element		*selected;	/* selected element */
	Element		*adding;	/* element currently being added */
	Element		*pendingCut;	/* cut, but might still be requested */
	Element		*copiedElement;	/* element that was last copied */
	Element		*moveElement;	/* element being moved */
} AppDataType, *AppDataTypePtr;

extern void initDPSContext();
extern void graphicExpose();
extern void scrollProc();
extern void doScroll();
extern void convertToX();
extern void convertToDPS();
extern void convertToOrigDPS();
extern void positionDrawingArea();
extern void drawSelf();
extern void drawSelfAndUpdate();
extern void setOrigin();
extern void selectElement();
extern void unselect();
extern void freeElement();
extern void sweepRectangle();
extern void addElement();
extern void freeResourceList();
extern Boolean parseFileHeader();
extern Boolean imageFile();
extern char *convertToEPS();
extern void pasteEPS();
extern void writePictureToFile();
extern void setEPSIPixmapParameters();
extern void setEPSIBitmapParameters();
extern Pixmap allocPixmap();
extern void drawSelectionMarks();
extern void computeBBox();
extern void updateElement();
extern void renderElement();
extern void moveElement();
extern void scaleElement();
extern void rotateElement();
extern Boolean pointInElement();

/***************************************************************
**
**	EXTERNAL DATA DECLARATIONS
**
***************************************************************/

/*
** Global pointers to the application name and data block
*/
extern	AppDataType     AppData;

#endif /* _IMPORT_H -- Add nothing below this line */
