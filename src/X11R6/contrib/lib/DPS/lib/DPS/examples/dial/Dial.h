/*
 * $RCSfile: Dial.h,v $
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


#ifndef _DIAL_H
#define _DIAL_H

/***************************************************************
**
** INCLUDE FILES
**
***************************************************************/

#include <stdio.h>
#include <sys/time.h>
#define _XOPEN_SOURCE	/* Get definition of M_PI */
#include <math.h>

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
** These numbers are matched with corresponding numbers in dial.uil
*/

#define cMainDrawArea       0
#define cTimingText0        1
#define cTimingText1        2
#define cTimingText2        3
#define cTimingText3        4
#define cTraceToggle        5

#define DRAW_EXPOSE           5

#define A_COEFF 0
#define B_COEFF 1
#define C_COEFF 2
#define D_COEFF 3
#define TX_CONS 4
#define TY_CONS 5

#define RADIANS (M_PI/180.0)

#define WIDCIRCBRD 5.0		/* Width of circle border */
#define CLRCIRC 0.3		/* Color of circle */
#define CLRCIRCBRD 0.0		/* Color of circle border */
#define CIRCFF 5		/* Border between circle and frame */

#define WIDTH1 0.5		/* Width of 1-degree lines */
#define COLOR1 1.0		/* Color of 1-degree lines */
#define LENGTH1 (10.0/11.0)	/* Distance 1-degree lines start from center */
#define DEGREE1 1.0		/* Spacing between 1-degree lines */

#define WIDTH10 1.5		/* Same for 10-degree lines */
#define COLOR10 1.0
#define LENGTH10 (6.0/7.0)       
#define DEGREE10 10.0    

#define WIDTH45 2.5     	/* Same for 45-degree lines */
#define COLOR45 0.0
#define LENGTH45 (0.75)  
#define DEGREE45 45.0    

#define WIDTH90 3.5     	/* Same for 90-degree lines */
#define COLOR90 0.0
#define LENGTH90 (0.7)   
#define DEGREE90 90.0    

#define MAX_PTS 1500
#define MAX_OPS 750

/***************************************************************
**
** TYPE DECLARATIONS
**
***************************************************************/

typedef struct
{
    Boolean 	trace;   	 /* Trace on/off variable */
    Widget      drawArea;        /* drawing area widget ID     */
    Widget	timing0;
    Widget	timing1;
    Widget	timing2;
    Widget	timing3;
    DPSContext  dpsCtxt;        /* drawing DPS context        */
    XtAppContext appContext; 
    Boolean	one;            /* draw one degree increments */
    Boolean	ten;            /* draw ten degree increments */
    Boolean	fortyFive;      /* draw 45 degree increments  */
    Boolean	ninety;         /* draw 90 degree increments  */
    float       width;          /* drawing area width         */
    float       height;         /* drawing area height        */
    float       radius;         /* circle dimension (radius)  */
} AppDataType, *AppDataTypePtr;

/***************************************************************
**
** FUNCTION DELCARATIONS
**
***************************************************************/

void        drawDialBackground();
void        drawDialBorder();

void        drawRotate              ();
void	    drawTrig		    ();
void        drawTrigUserPaths       ();
void        drawTrigUserPathsServer ();
void        initDPSContext          ();

/***************************************************************
**
** EXTERNAL DATA DECLARATIONS
**
***************************************************************/

/*
** Global pointers to the application name and data block
*/
extern AppDataType  AppData;

#endif /* _DIAL_H -- Add nothing below this line */
