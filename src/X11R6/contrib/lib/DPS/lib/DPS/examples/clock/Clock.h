/*
 * $RCSfile: Clock.h,v $
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


#ifndef _CLOCK_H
#define _CLOCK_H

/***************************************************************
**
** INCLUDE FILES
**
***************************************************************/

#include <stdio.h>
#include <time.h>
#include <sys/time.h>
#define _XOPEN_SOURCE	/* Get definition of M_PI */
#include <math.h>

#include <Xm/Text.h>
#include <Xm/DialogS.h>
#include <Xm/MwmUtil.h>
#include <Xm/MessageB.h>
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
#define cTimingBox          3
#define cTimingText         4
#define cTimingButton       5
#define cOptionButton0      6
#define cOptionButton1      7
#define cOptionButton2      8
#define cOptionButton3      9

#define LAST_LIST_WIDGET    9
#define MAX_WIDGETS (LAST_LIST_WIDGET + 1)

#define A_COEFF 0                   /* Constants for X to DPS   */
#define B_COEFF 1                   /* coordinate translations. */
#define C_COEFF 2
#define D_COEFF 3
#define TX_CONS 4
#define TY_CONS 5

/*
** Colors and sizes for various components
*/

#define XDPS_BLACK      0.0
#define XDPS_DKGRAY     0.3
#define XDPS_MDGRAY	0.5
#define XDPS_LTGRAY     0.7
#define XDPS_WHITE      1.0

#define CLOCKSIZE	150.0	/* Clock goes from -150 to 150 in x and y */

#define CLRPAGE         XDPS_DKGRAY
#define CLRCIRC         XDPS_MDGRAY

#define SIZEDASHES      0.90

#define WIDMIN          3.5 
#define CLRMIN          XDPS_WHITE  
#define LENMIN          (19.0/20.0)
#define DEGMIN          (-6.0)

#define WIDHOUR         6.0 
#define CLRHOUR         XDPS_WHITE  
#define LENHOUR         (14.0/15.0)
#define DEGHOUR         (-30.0)

#define ALARM_INTERVAL  ((DEGHOUR / 60) / 2)
#define ALARM_REPEATS   8
#define ALARM_LEVEL     50  /* -100 = off, 100 = loudest */

#define CLRHANDS        XDPS_WHITE
#define CLRSECOND       XDPS_LTGRAY
#define CLRSHADOW       0.20
#define CLRALARMTOP     0.8
#define CLRALARMBOT     XDPS_DKGRAY

#define LNWIDSECOND     1.0
#define LNWIDHANDS      3.0

#define OFFSETHANDSX    1.0
#define OFFSETHANDSY    (-1.0)
#define OFFSETSHADX     2.0
#define OFFSETSHADY     (-2.0)

#define MAX_PTS         300
#define MAX_OPS         150

#define HITSETTING      8.0 	/* Hit sensitivity */

#define RADIAN          (M_PI/180)

#define ALARM           0
#define HOUR            1
#define MINUTE          2
#define SHADOW          3
#define SECOND          4

/***************************************************************
**
** MACRO DEFINITIONS
**
***************************************************************/

#define ABS(a)      (((a) < 0) ? -(a) : (a))

/***************************************************************
**
** TYPE DECLARATIONS
**
***************************************************************/

typedef struct
{
    Boolean     trace;		/* state of the trace toggle button */
    Boolean     pixmapBackground; /* using pixmap flag	      */
    Boolean     doubleBuffering;/* double buffering flag      */
    Boolean     graphicStates;  /* graphic states flag        */
    Boolean     serverPaths;    /* user paths in server flag  */
    Boolean     alarmOn;        /* alarm turned on flag       */
    Pixmap      facePixmap;	/* pixmap for clock face      */
    Pixmap      clockPixmap;	/* pixmap for complete clock  */
    Dimension   Xwidth;		/* drawing area width	      */
    Dimension   Xheight;	/* drawing area height        */
    Widget      widget;         /* drawing area widget ID     */
    Widget	timingBox;	/* timing box widget ID	      */
    Widget	timingText;	/* timing text widget ID      */
    Widget	timingButton;	/* timing button widget ID    */
    Widget	traceToggle;	/* trace toggle widget ID     */
    XtAppContext appContext;	/* application context	      */
    DPSContext  dpsCtxt;        /* drawing DPS context        */
    Boolean	mapped;         /* window mapped flag         */
    Boolean	redrewAlready;	/* supress extra redraw	      */
    int         numIterations;  /* times drawn this method    */
    int		lastTimes[5];	/* last 5 elapsed times	      */
    long        milliSecs;      /* milliseconds part of time  */
} AppDataType, *AppDataTypePtr;

/***************************************************************
**
** FUNCTION DELCARATIONS
**
***************************************************************/

void        drawClockTime           ();
void        setAlarm                ();
int         isHit                   ();
int         checkAlarm              ();
void	    handleWindowResize	    ();
void	    setBufferRendering	    ();
void	    setGStates		    ();
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

#endif /* _CLOCK_H -- Add nothing below this line */
