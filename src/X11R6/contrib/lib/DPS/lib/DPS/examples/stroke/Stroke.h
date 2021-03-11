/*
 * $RCSfile: Stroke.h,v $
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


#ifndef _STROKE_H
#define _STROKE_H

/***************************************************************
**
** INCLUDE FILES
**
***************************************************************/

#include <stdio.h>
#include <math.h>
#include <sys/time.h>

#include <X11/Intrinsic.h>
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
** These numbers are matched with corresponding numbers in Stroke.uil
*/

#define cMainDrawArea0  1
#define cMainDrawArea1  2
#define cTimingText0    3
#define cTimingText1    4
#define cTraceToggle    5
#define cWidthScale     6
#define cTypeButton0    7
#define cTypeButton1    8
#define cTypeButton2    9
#define cTypeButton3    10

#define LAST_LIST_WIDGET    11
#define MAX_WIDGETS (LAST_LIST_WIDGET + 1)

#define A_COEFF 0                   /* Constants for X to DPS   */
#define B_COEFF 1                   /* coordinate translations. */
#define C_COEFF 2
#define D_COEFF 3
#define TX_CONS 4
#define TY_CONS 5

#define NUMLINESHORIZ   22.0    /* Constants for number  */
#define NUMLINESVERT    21.0    /* of lines to be drawn. */
#define NUMARCS         10.0
#define DIAGDEGS        10.0

/***************************************************************
**
** TYPE DECLARATIONS
**
***************************************************************/

typedef struct
{
    Boolean     traceToggle;  	/* PostScript trace text      */
    Widget      window0;        /* drawing area widget ID     */
    Widget      window1;        /* drawing area widget ID     */
    DPSContext  dpsCtxt;        /* drawing DPS context        */
    DPSGState	gs0;		/* Window 0 gstate	      */
    DPSGState	gs1;		/* Window 1 gstate	      */
    float       width;          /* width of area in PS units  */
    float       height;         /* height of area in PS units */
    Boolean     horizontalT;    /* horizontal toggle state    */
    Boolean     verticalT;      /* vertical toggle state      */
    Boolean     diagonalT;      /* diagonal toggle state      */
    Boolean     arcT;           /* arc toggle state           */
    float       lineWidth;      /* line width scale value     */
} AppDataType, *AppDataTypePtr;

/***************************************************************
**
** FUNCTION DELCARATIONS
**
***************************************************************/

extern void setTimingValue(), makeHorizLines(),
	    makeVertLines(), makeArcs(), makeDiagLines(),
	    initDPScontext(), drawIt(), initDPSContext();

/***************************************************************
**
** EXTERNAL DATA DECLARATIONS
**
***************************************************************/

/*
** Global pointers to the application name and data block
*/
extern AppDataType  AppData;

#endif /* _STROKE_H -- Add nothing below this line */
