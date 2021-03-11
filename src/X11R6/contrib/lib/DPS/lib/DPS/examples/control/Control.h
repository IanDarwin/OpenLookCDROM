/*
 * $RCSfile: Control.h,v $
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

#ifndef _CONTROL_H
#define _CONTROL_H

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
#include <Xm/MwmUtil.h>
#include <Mrm/MrmAppl.h>        /* Motif Toolkit and MRM */

#include <DPS/dpsXclient.h>
#include <DPS/dpsXshare.h>
#include <DPS/dpsXuserpath.h>

/***************************************************************
**
** CONSTANT DEFINITIONS
**
***************************************************************/

/*
** These numbers are matched with corresponding numbers in line.uil
*/

#define cMainDrawArea       1
#define cTraceToggle        2
#define cTimingText0        3
#define cTimingText1        4
#define cTimingText2        5
#define cTimingText3        6
#define cTimingText4        7
#define cTimingText5        8
#define cTimingText6        9
#define cButton3	    10

#define A_COEFF 0
#define B_COEFF 1
#define C_COEFF 2
#define D_COEFF 3
#define TX_CONS 4
#define TY_CONS 5

#define MAX_POINTS      1000
#define MAX_ARRAY       (2 * MAX_POINTS + 2)
#define MAX_UPATHOPS    (MAX_POINTS + 1)
#define MAX_UPATHPTS    (2 * MAX_UPATHOPS)
#define MAX_RECTPTS     525

#define FIGURESIZE      4	/* Size of control point, in points */

#define ABS(x) ((x) >= 0 ? (x) : -(x))

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

typedef struct {
    Widget      drawingArea; 	/* drawing area widget ID     */
    Widget	timing0;	/* timing output widget       */
    Widget	timing1;
    Widget	timing2;
    Widget	timing3;
    Widget	timing4;
    Widget	timing5;
    Widget	timing6;
    Widget	button3;	/* Rectangle operators button */
    DPSContext  dpsCtxt;        /* drawing DPS context        */
    float       height;         /* DPS height of the X Window */
    float       width;          /* DPS width of the X Window  */
    int         numPoints;      /* number of points to draw   */
    int         index;          /* index of points to draw    */
    Pixmap      bitSetPixmap;   /* bit-setting pixmap ID      */
    GC          bitSetGC;       /* bit-setting GC             */
    DPSGState	bitSetGState;	/* gstate for setting pixmap  */
    Pixmap      bitClearPixmap; /* bit-clearing pixmap ID     */
    GC          bitClearGC;     /* bit-clearing GC	      */
    DPSGState	bitClearGState;	/* gstate for clearing pixmap */
    GC		andGC;		/* GC for ANDing	      */
    GC		orGC;		/* GC for ORing		      */
    XtAppContext appContext;	/* Xt application context     */
    unsigned    pixmapHeight;   /* pixmap height	      */
    unsigned    pixmapWidth;    /* pixmap width		      */
    char        fontchar;       /* font character to draw     */
    char        *basicProc;     /* basic method wrap procedure*/
    float       *userPtsArray;  /* user paths points array    */
    DPSUserPathOp *userOpsArray;/* user paths operators array */
    DPSUserPathAction userOp;   /* user paths DPS operator    */
    char        *rectOp;        /* rectangular DPS operator   */
    Boolean	devIndependent;	/* state of device indep. box */
    Boolean	trace;		/* trace toggle state	      */
} AppDataType, *AppDataTypePtr;

/***************************************************************
**
** FUNCTION DELCARATIONS
**
***************************************************************/

void        initDPSContext          ();
void	    drawBasic		    ();
void	    drawUserCache	    ();
void	    drawUserPath	    ();
void	    drawRectOp		    ();
void	    drawPixmaps		    ();
void	    drawShow		    ();
void	    drawXYShow		    ();
void	    erasePage		    ();
void	    setRectFill		    ();
void	    setRectOpen		    ();
void	    setX		    ();
void	    setCross		    ();

/***************************************************************
**
** EXTERNAL DATA DECLARATIONS
**
***************************************************************/

/*
** Global pointers to the application name and data block
*/
extern AppDataType  AppData;


#endif /* _CONTROL_H -- Add nothing below this line */
