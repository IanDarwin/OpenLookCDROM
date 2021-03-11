/*
 * $RCSfile: Text.h,v $
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

#ifndef _TEXT_H
#define _TEXT_H

/***************************************************************
**
** INCLUDE FILES
**
***************************************************************/

#include <stdio.h>
#include <math.h>
#include <sys/time.h>

#include <X11/Intrinsic.h>
#include <Xm/Text.h>
#include <Xm/DialogS.h>
#include <Xm/MwmUtil.h>
#include <Xm/MessageB.h>
#include <Mrm/MrmAppl.h>        /* Motif Toolkit and MRM */

#include <DPS/dpsXclient.h>
#include <DPS/dpsXshare.h>

#include "TextWraps.h"
#include "parseAFM.h"

/***************************************************************
**
** CONSTANT DEFINITIONS
**
***************************************************************/

/*
** These numbers are matched with corresponding numbers in text.uil
*/

#define cOptionBox		0
#define cErrorBox		1
#define cMainDrawArea		2
#define cTextDrawArea		3
#define cTimingText0		4
#define cTimingText1		5
#define cTimingText2		6
#define cStatusText0		7
#define cStatusText1		8
#define cStatusText2		9
#define cStatusText3		10
#define cStatusText4		11
#define cStatusText5		12
#define cStatusText6		13
#define cCompButton0		14
#define cCompButton1		15
#define cTraceToggle		16
#define cHsb			17
#define cVsb			18

#define NUM_SIZES		3	/* number of font sizes in text application */

#define PTS_PER_INCH 72

#define PAGE_WIDTH	(PTS_PER_INCH * 8.5)
#define PAGE_HEIGHT	(PTS_PER_INCH * 10.0)

#define FONT_BASE		"Times-Roman"
#define FONT_OUTLINE		"Times-Roman-Outline"
#define FONT_SCREEN		"Times-Roman-Screen"
#define FONT_PRINTER		"Times_Roman-Printer"
#define FONT_OUTLINE_UNCACHED	"Times-Roman-Outline-Uncached"
#define FONT_SCREEN_UNCACHED	"Times-Roman-Screen-Uncached"
#define FONT_PRINTER_UNCACHED	"Times_Roman-Printer-Uncached"

#define KERNING			1
#define TRACKING		2

#define CACHE			1
#define OUTLINE			2

#define COMPARE_KERNS		1
#define COMPARE_WIDTHS		2
#define COMPARE_BITMAPS		3

#define	MAX_XSHOW		128

/*
** The following are "attributes," or "events," that can happen at a
** certain character on the line of text.  The field "attr" in the
** showstruct structure is an array of these attributes, whoose indexes
** correspond to the "text" field.
*/

/*
** showany attributes:
*/
#define SA_NOATTR		0	/* no attribute for this character */

#define SA_PRKERN		1	/* Pair kern */
#define SA_ABSMOV		2	/* Absolute move */
#define SA_SPACEADJ		4	/* Space adjustment value change */
#define SA_TRACKADJ		8	/* Track kern value change */

#define  TRACKVAL		2.0	/* tracking value */

#define  NUM_LINES		19	/* Number of lines in text.c */
#define  LINE_LENGTH		330	/* Length for calculating full justification */
#define  LINE_LENGTH_TR		462	/* Length for calculating full justification */
					/* with tracking */
#define  MAX_XSHOW		128

/*
** These defines are the indices into the transformation matrix,
** which is an array of six floats.  They are used in functions
** which convert coordinate points between X space and DPS space.
** These defines, the functions which use them, and the pswraps
** function PSWGetTransform are all copied from the X supplement
** to the Adobe _Client_Library_Reference_Manual_ (section 5.3).
*/
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

/***************************************************************
**
** TYPE DECLARATIONS
**
***************************************************************/

typedef struct {
	int	code;
	float	dx;
} KernPair;

typedef struct {
	int	numOfChars;		/* number of chars described	*/
	int	numOfPairs;		/* number of kern pairs listed	*/
	int	maxChar;		/* maximum character code 	*/
	int	*kernIndex;		/* array of kern pair indices	*/
	int	*numKernPairs;		/* array of number of kern pairs*/
	KernPair *kernPairs;		/* array of kern pair structs	*/
	float	*widths;		/* array of character widths	*/
	float	*bitmapWidths[NUM_SIZES]; /* array of bitmap char widths */
} FontMetrics;

typedef struct {
    int chars;
    int kerns;
    int time;
} Timing;

typedef struct {
    float x, y;
} Point;

typedef struct {        /* rectangle */
    float top;
    float left;
    float bottom;
    float right;
} Rect;

typedef struct showstruct
{
    char *text;		/* characters to be shown (null terminated) */
    unsigned int *attr;	/* array of attributes equal in length to *text */

    float *prkern;	/* amount of pair kern (array) */
    Point *absmov;	/* coordinates for absolute movetos (array) */
    float *spaceadj;	/* amount to adjust space character (array) */
    float *trackkern;	/* track kern adjustment amount (array) */

    int	textlen;	/* number of characters in text and attr */
    int	prkernlen;	/* number of entries filled in prkern array */
    int	absmovlen;	/* number of absolute movetos / 2 coords*/
    int spaceadjlen;	/* number of entries filled in space adjust array */
    int trackkernlen;	/* number of entries filled in track kerns array */
    
    /*
    ** These values are unimportant to showany(), but are used in the
    ** building of the structure by the routines in buildshow.c.  They allow
    ** dynamic re-sizing of the arrays, but may be removed if the buildshow
    ** routines are not used to create the showstruct data structure
    */

    int	textmax;	/* max length of the text and attr */
    int	prkernmax;	/* max length allocated for prkern array */
    int	absmovmax;	/* max length allocated for absmov array */
    int	spaceadjmax;	/* max length allocated for space adjust array */
    int	trackkernmax;	/* max length allocated for track kerns array */
} ShowStruct;

typedef enum {
    show_xshow, show_rmshow, show_varshow
} ShowType;

typedef struct
{
    Widget	drawingArea;	/* drawing area widget ID */
    Widget	optionBox;
    Widget	time;
    Widget 	numChars;
    Widget	kernPairs;
    Widget	cacheStatus[7];
    Widget	hScroll;
    Widget	vScroll;
    GC		gc;		/* X graphic context */
    DPSContext	dpsCtxt;	/* DPS context */
    Pixmap	buf;		/* Window buffer */
    Cursor 	waitCursor;	/* cursor while waiting for text to render */
    int	      	fontNum;	/* font number from array */
    float	fontSize;	/* font size */
    int		magnify;	/* magnification factor */
    float	scale;		/* magnification scale */
    Boolean	scrolling;	/* TRUE if scroll redraw pending */
    Boolean	zooming;	/* True if zoom in progress */
    Boolean	justify;	/* justify the text */
    Boolean	trace;
    int		spacing;	/* Kerning and/or tracking */
    ShowType	show;		/* show manner */
    int		issues;		/* font issues */
    int		comp;		/* comparisons */
    int		screen;		/* Use screen widths */
    Dimension	scaledHeight;	/* scaled page width in X units	*/
    Dimension	scaledWidth;	/* scaled page height in X units */
    Dimension	drawingWidth;	/* width of visible area */
    Dimension	drawingHeight;	/* height of visible area */
    int		scrollX;	/* X scrollbar value */
    int		scrollY;	/* Y scrollbar value */
    int		xOffset;	/* x offset of device space origin */
    int		yOffset;	/* y offset of device space origin */
    int		originX;	/* where the origin is */
    int		originY;	/*  (relative to X offset */
    float	ctm[6];		/* current transformation matrix */
    float	invctm[6];	/* inverse ctm */
    float	origInvctm[6];	/* original inverse ctm */
    int		size,		/* Font cache parameters */
                lower,
                upper;
    ShowStruct	s;		/* structure used by showany () */
    float	*charspace;	/* pointer to char spacing array */
    Timing	timingInfo;	/* Timing information */
    FontMetrics	metrics;	/* font metrics structure */
} AppDataType, *AppDataTypePtr;

/***************************************************************
**
** FUNCTION DELCARATIONS
**
***************************************************************/

extern void graphicExpose();
extern void scaleDrawingArea();
extern void positionDrawingArea();
extern void scrollProc();
extern void convertToX();
extern void convertToDPS();
extern void convertToOrigDPS();

extern void initDPSContext();
extern void drawSelf();
extern void drawSelfAndUpdate();
extern void eraseFields();
extern void displayFields();

void ResetShowStruct ( /* ShowStruct *show */ );
void AllocShowStruct ( /* ShowStruct *show */ );
void FreeShowStruct ( /* ShowStruct *show */ );
void AddString ( /* ShowStruct *show, char *string */ );
void AddMoveto ( /* ShowStruct *show, int index, float x, float y */ );
void AddPairKern ( /* ShowStruct *show, int index, float value */ );
void AddTracking ( /* ShowStruct *show, int index, float value */ );
void AddSpaceAdj ( /* ShowStruct *show, int index, float value */ );
void ShowAny ( /* ShowStruct *show */ );

/***************************************************************
**
** EXTERNAL DATA DECLARATIONS
**
***************************************************************/

/*
** Global pointers to the application name and data block
*/
extern AppDataType  AppData;

extern char 		*textstrings[ ];
extern float		textxy[ ][2];

extern float		FontSizes [NUM_SIZES];

#endif /* _DIAL_H -- Add nothing below this line */
