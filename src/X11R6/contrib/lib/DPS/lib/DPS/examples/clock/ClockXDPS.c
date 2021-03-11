/*
 * $RCSfile: ClockXDPS.c,v $
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

/***************************************************************
**
** INCLUDE FILES
**
***************************************************************/

#include "Clock.h"
#include "ClockWraps.h"

/***************************************************************
**
** DATA DECLARATIONS
**
***************************************************************/

/*
* Start time for measurement
*/
static struct timeval StartTime;

static void markTime(), returnTime();

/*
** These are the userpath operands and operators for the clock hands.
** They are sent and stored in the server.
*/
static float ptsHour[] = {
                          -4.5, 0,               /* dps_moveto */
			  0, 100,                /* dps_rlineto */
			  0, 100, 4.5, 180, 0,   /* dps_arcn */
			  0, -100,               /* dps_rlineto */
                                                 /* dps_closepath */
			  0, 0,                  /* dps_moveto */
			  0, 0, 10, 360, 0       /* dps_arcn */
                                                 /* dps_closepath */
			  };
static float bboxHour[] = {-10, -10, 10, 150};

static char opsHour[] = {
                          dps_moveto, dps_rlineto, dps_arcn, dps_rlineto,
			  dps_closepath, dps_moveto, dps_arcn, dps_closepath
			  };

static float ptsMin[] = {
                          -4.5, 0,              /* dps_moveto */
                          0, 132,               /* dps_rlineto */
			  0, 132, 4.5, 180, 0,  /* dps_arcn */
			  0, -132,              /* dps_rlineto */
                                                /* dps_closepath */
			  0, 0,                 /* dps_moveto */
			  0, 0, 10, 360, 0      /* dps_arcn */
                                                /* dps_closepath */
			  };
static float bboxMin[] = {-10, -10, 10, 145};


static char opsMin[] = {
                        dps_moveto, dps_rlineto, dps_arcn, dps_rlineto,
			dps_closepath, dps_moveto, dps_arcn, dps_closepath
                        };

static float ptsSec[] = {
			 -1.5, 0,               /* dps_moveto */
			 0, 145,                /* dps_rlineto */
			 3, 0,                  /* dps_rlineto */
			 0, -145,               /* dps_rlineto */
			 4, 0,                  /* dps_rlineto */
                         0, -20,                /* dps_rlineto */
			 0, -20, 5.5, 360, 180, /* dps_arcn */
			 0, 20,                 /* dps_rlineto */
			 4, 0,                  /* dps_rlineto */
			                        /* dps_closepath */
			 0, 0,                  /* dps_moveto */
                         0, 0, 10, 360, 0       /* dps_arcn */
                                                /* dsp_closepath */
			 };
static float bboxSec[] = {-10, -30, 10, 170};

static char opsSec[] = {
                        dps_moveto, dps_rlineto, dps_rlineto,
                        dps_rlineto, dps_rlineto, dps_rlineto, dps_arcn,
			dps_rlineto, dps_rlineto, dps_closepath, dps_moveto,
			dps_arcn, dps_closepath
			};

static float ptsAlarmTop[] = {
                              -1.0, 100,           /* dps_moveto */
                              0, 5,                /* dps_rlineto */
                              0, 105, 1.0, 180, 0, /* dps_arcn */
			      0, -5                /* dps_rlineto */
                                                   /* dps_closepath */
			      };
static float bboxAlarmTop[] = {-5, 70, 5, 120};

static char opsAlarmTop[] = {
                             dps_moveto, dps_rlineto,
                             dps_arcn, dps_rlineto, dps_closepath};

static float ptsAlarmBot[] = {
                              -1.0, 0,             /* dps_moveto */
                              0, 100,              /* dps_rlineto */
                              2.0, 0,              /* dps_rlineto */
                              0, -100              /* dps_rlineto */
                                                   /* dps_closepath */
			      };
static float bboxAlarmBot[] = {-5, -2, 5, 120};

static char opsAlarmBot[] = {
                             dps_moveto, dps_rlineto,
                             dps_rlineto, dps_rlineto, dps_closepath
			     };


/*
** Graphics state user object identifiers
*/
static DPSGState
           gstateWindow = 0,
	   gstateBuffer = 0,
           gstateHour = 0,
           gstateMin = 0,
           gstateSec = 0,
           gstateShad = 0;

/*
** Clock hands userpaths user object identifiers
*/
static int
          upathHour = 0, 
          upathMin = 0,
          upathSec = 0,
          upathAlarmTop = 0,
          upathAlarmBot = 0;

/*
** Angle in degrees of clock hands
*/
static float
          angleHour,
          angleMin,
          angleSec,
          angleAlarm = 0;

/*
** Variables for transformation between DPS/X and
** X Window System coordinates
*/
static float Ctm[6], Invctm[6];      /* transformation matrices */
static int XOffset, YOffset;       /* coordinate system offsets */

/***************************************************************
**
** FUNCTION:    drawUpathLines
**
** DESCRIPTION: Use PSDoUserPath to draw the ticks of the clock
**              indicating hours or minutes.
**
** PARAMETERS:  clr         gray scale color value
**              wid         line width
**              startlen    starting length from circle center
**              endlen      ending length from circle center
**              deg         delta angle in degrees
**
** RETURN:      None.
**
***************************************************************/
static void drawUpathLines (clr, wid, startlen, endlen, deg)
    float   clr, wid, startlen, endlen, deg;
{
    int     i , j;
    float   angle;
    float   bbox[4];
    float   pts[MAX_PTS];
    char    ops[MAX_OPS];
    
    /*
    ** Initialize the loop control variables
    */
    deg = ABS (deg * RADIAN);
    i = 0; j = 0;

    for (angle = 0; angle < 2 * M_PI; angle += deg)
    {
        /*
        ** Calculate the coordinate points for the start of the angled
        ** line using standard trigometric calculations.  Place the
        ** DPS 'moveto' operator into the operator array.
        */
        pts[i++] = (floor) (cos (angle) * startlen);
        pts[i++] = (floor) (sin (angle) * startlen);
        ops[j++] = dps_moveto;
        
        /*
        ** Calculate the coordinate points for the end of the angled
        ** line using standard trigometric calculations.  Place the
        ** DPS 'lineto' operator into the operator array.
        */
        pts[i++] = (floor) (cos (angle) * endlen);
        pts[i++] = (floor) (sin (angle) * endlen);
        ops[j++] = dps_lineto;
    }

    /*
    ** Set the draw color and line width
    */
    PSsetgray (clr);
    PSsetlinewidth (wid);

    /*
    ** Define the bounding box
    */
    bbox[0] = -CLOCKSIZE;
    bbox[1] = -CLOCKSIZE;
    bbox[2] = CLOCKSIZE;
    bbox[3] = CLOCKSIZE;

    /*
    ** Stroke all the lines in the userpath
    */
    PSDoUserPath ((DPSPointer) pts, i, dps_float, ops, j,
        (DPSPointer) bbox, dps_ustroke);

} /* end drawUpathLines () */


/***************************************************************
**
** FUNCTION:    drawFace
**
** DESCRIPTION: Draws the clock face background
**
** PARAMETERS:  None.
**
** RETURN:      None.
**
***************************************************************/
static void drawFace ()
{
    float radius;

    /* Clear the page; add a little bit of slop */

    PSWErasePage (CLRPAGE, -CLOCKSIZE - 2, -CLOCKSIZE - 2,
		  2*CLOCKSIZE + 4, 2*CLOCKSIZE + 4);

    radius = CLOCKSIZE * SIZEDASHES;

    /*
    ** Draw the filled circle that makes up the clock face
    */
    PSWDrawCircle (radius, CLRCIRC, True);
        
    /*
    ** Set the line cap parameter to round cap
    */
    PSsetlinecap (1);

    /*
    ** Draw the minute and hour tick marks around the face
    */
    drawUpathLines (CLRMIN,  WIDMIN,  radius * LENMIN,  radius, DEGMIN);
    drawUpathLines (CLRHOUR, WIDHOUR, radius * LENHOUR, radius, DEGHOUR);
}

/***************************************************************
**
** FUNCTION:    defineUPaths
**
** DESCRIPTION: Define the userpaths of the hands as user objects.
**
** PARAMETERS:  None.
**
** RETURN:      None.
**
***************************************************************/
static void defineUPaths ()
{
    /*
    ** Setup hour hand userpath object
    */
    PSDoUserPath ((DPSPointer) ptsHour, XtNumber(ptsHour),
		  dps_float, opsHour, XtNumber(opsHour),
		  (DPSPointer) bboxHour, dps_send);
    upathHour = PSDefineAsUserObj();

    /*
    ** Setup minute hand userpath object
    */
    PSDoUserPath ((DPSPointer) ptsMin, XtNumber(ptsMin),
		  dps_float, opsMin, XtNumber(opsMin),
		  (DPSPointer) bboxMin, dps_send);
    upathMin = PSDefineAsUserObj();

    /*
    ** Setup second hand userpath object
    */
    PSDoUserPath ((DPSPointer) ptsSec, XtNumber(ptsSec),
		  dps_float, opsSec, XtNumber(opsSec),
		  (DPSPointer) bboxSec, dps_send);
    upathSec = PSDefineAsUserObj();

    /*
    ** Setup top of alarm hand userpath object
    */
    PSDoUserPath ((DPSPointer) ptsAlarmTop, XtNumber(ptsAlarmTop),
		  dps_float, opsAlarmTop, XtNumber(opsAlarmTop),
		  (DPSPointer) bboxAlarmTop, dps_send);
    upathAlarmTop = PSDefineAsUserObj();

    /*
    ** Setup bottom of alarm hand userpath object
    */
    PSDoUserPath ((DPSPointer) ptsAlarmBot, XtNumber(ptsAlarmBot),
		  dps_float, opsAlarmBot, XtNumber(opsAlarmBot),
		  (DPSPointer) bboxAlarmBot, dps_send);
    upathAlarmBot = PSDefineAsUserObj();

} /* end defineUPaths () */


/***************************************************************
**
** FUNCTION:    definegstate
**
** DESCRIPTION: Define a new graphics state with a coordinate
**              system translation, and new color and line
**              width.  The graphics state will retain the
**              current scaling from the current graphics state.
**
** PARAMETERS:  gstate      graphics state user object identifier
**              offsetX     coordinate translation X offset
**              offsetY     coordinate translation Y offset
**              color       drawing color
**              linewidth   stroking line width
**
** RETURN:      gstate      graphics state user object identifier
**
***************************************************************/
static DPSGState definegstate (gstate, offsetX, offsetY, color, linewidth)
    DPSGState     gstate;
    float   offsetX, offsetY, color, linewidth;
{
    /*
    ** Save the graphics state to allow for definition of a new
    ** graphics state
    */
    PSgsave ();

    /*
    ** Set the coordinate system translation, color, and line width
    */
    PSWSetGraphicsParams (offsetX, offsetY, color, linewidth);

    /*
    ** If this is the first time, capture a new gstate.  If not,
    ** update the old gstate to the new values
    */

    if (gstate == 0) XDPSCaptureContextGState(AppData.dpsCtxt, &gstate);
    else XDPSUpdateContextGState(AppData.dpsCtxt, gstate);

    /*
    ** Restore the graphics state
    */
    PSgrestore ();

    /*
    ** Return the graphics state user object identifier
    */
    return gstate;

} /* end definegstate () */

/***************************************************************
**
** FUNCTION:    defineGStates
**
** DESCRIPTION: Redefine the gsates because the CTM has changed
**              as the result of the resizing the draw area.
**
** PARAMETERS:  None.
**
** RETURN:      None.
**

***************************************************************/
static void defineGStates ()
{
    /*
    ** Define the hour hand graphics state user object
    */
    gstateHour = definegstate (gstateHour, 0.0, 0.0,
			       CLRHANDS - 0.2, LNWIDHANDS);

    /*
    ** Define the minute hand graphics state user object
    */
    gstateMin = definegstate (gstateMin, OFFSETHANDSX, OFFSETHANDSY,
			      CLRHANDS - 0.2, LNWIDHANDS);

    /*
    ** Define the second hand graphics state user object
    */
    gstateSec = definegstate (gstateSec, 2 * OFFSETHANDSX, 2 * OFFSETHANDSY,
			      CLRSECOND, LNWIDSECOND);

    /*
    ** Define the second hand shadow graphics state user object
    */
    gstateShad = definegstate (gstateShad, 2 * OFFSETHANDSX + OFFSETSHADX,
			       2 * OFFSETHANDSY + OFFSETSHADY,
			       CLRSHADOW, LNWIDSECOND);

} /* end defineGStates () */

/***************************************************************
**
** FUNCTION:    setGStates
**
** DESCRIPTION: Update gstates to reflect turning graphics states
**		on or off.  If now on, define new gstates.  If now
**		off, free old gstates and set either the buffer or
**		window gstate for future rendering
**
** PARAMETERS:  None.
**
** RETURN:      None.
**
***************************************************************/
void setGStates ()
{
    if (AppData.graphicStates) defineGStates();
    else {
	XDPSFreeContextGState(AppData.dpsCtxt, gstateHour);
	XDPSFreeContextGState(AppData.dpsCtxt, gstateMin);
	XDPSFreeContextGState(AppData.dpsCtxt, gstateSec);
	XDPSFreeContextGState(AppData.dpsCtxt, gstateShad);
	gstateHour = gstateMin = gstateSec = gstateShad = 0;

	if (AppData.doubleBuffering) {
	    XDPSSetContextGState(AppData.dpsCtxt, gstateBuffer);
	} else XDPSSetContextGState(AppData.dpsCtxt, gstateWindow);
    }
} /* end restoreInitialGState () */

/***************************************************************
**
** FUNCTION:    drawClockHand
**
** DESCRIPTION: Draws the clock hands using the userpath either
**              as stored in the server or by sending them each
**              time. The decision is based on user selection in
**              the control panel in the timing window.
**
** PARAMETERS:  hand    number representing the hand to draw
**
** RETURN:      None.
**
***************************************************************/
static void drawClockHand (hand)
    int hand;
{
    /*
    ** If userpaths in server was selected, the requested hand
    ** is drawn using the stored userpath
    */
    if (AppData.serverPaths)
    {
        switch (hand)
        {
            case ALARM:
                PSsetgray (CLRALARMTOP);
                PSWUpathFill (upathAlarmTop);
                PSsetgray (CLRALARMBOT);
                PSWUpathFill (upathAlarmBot);
                break;

            case HOUR:
                PSWUpathStrokeFill (upathHour);
                break;          

            case MINUTE:
                PSWUpathStrokeFill (upathMin);
                break;          

            case SHADOW:
                PSWUpathFill (upathSec);
                break;          

            case SECOND:
                PSWUpathFill (upathSec);
                PSWDrawCircle (10.0, CLRSECOND - 0.2, False);
                break;      
        }
    }
    /*
    ** If userpaths in server was not selected, the requested hand
    ** is drawn by sending the userpath to the server with PSDoUserPath()
    */
    else
    {
        switch (hand)
        {

            case ALARM:

                PSsetgray (CLRALARMTOP);
                PSDoUserPath ((DPSPointer) ptsAlarmTop, XtNumber(ptsAlarmTop),
                    dps_float, opsAlarmTop, XtNumber(opsAlarmTop),
                    (DPSPointer) bboxAlarmTop, dps_ufill);
                PSsetgray (CLRALARMBOT);
                PSDoUserPath ((DPSPointer) ptsAlarmBot, XtNumber(ptsAlarmBot),
                    dps_float, opsAlarmBot, XtNumber(opsAlarmBot),
                    (DPSPointer) bboxAlarmBot, dps_ufill);
                break;

            case HOUR:

                PSDoUserPath ((DPSPointer) ptsHour, XtNumber(ptsHour),
                    dps_float, opsHour, XtNumber(opsHour),
		    (DPSPointer) bboxHour, dps_ustroke);
                PSsetgray (CLRHANDS);
                PSDoUserPath ((DPSPointer) ptsHour, XtNumber(ptsHour),
                    dps_float, opsHour, XtNumber(opsHour),
                    (DPSPointer) bboxHour, dps_ufill);
                break;

            case MINUTE:

                PSDoUserPath ((DPSPointer) ptsMin, XtNumber(ptsMin),
                    dps_float, opsMin, XtNumber(opsMin),
                    (DPSPointer) bboxMin, dps_ustroke);
                PSsetgray (CLRHANDS);
                PSDoUserPath ((DPSPointer) ptsMin, XtNumber(ptsMin),
                    dps_float, opsMin, XtNumber(opsMin),
                    (DPSPointer) bboxMin, dps_ufill);
                break;

            case SHADOW:

                PSDoUserPath ((DPSPointer) ptsSec, XtNumber(ptsSec),
                    dps_float, opsSec, XtNumber(opsSec),
                    (DPSPointer) bboxSec, dps_ufill);
                break;      

            case SECOND:

                PSDoUserPath ((DPSPointer) ptsSec, XtNumber(ptsSec),
                    dps_float, opsSec, XtNumber(opsSec),
                    (DPSPointer) bboxSec, dps_ufill);
                PSWDrawCircle (10.0, CLRSECOND - 0.2, False);
                break;      
        }
    }

} /* end drawClockHand () */

/***************************************************************
**
** FUNCTION:    drawHands
**
** DESCRIPTION: Sets up the graphics state and draws the clock
**              hands.  Draws either with or without the gstate
**              saved in the server as a user object.  Decision
**              is based on user selection in the control panel
**              in the timing window.  A slight performance
**              advantage is gained with gstates but they use
**              up an appreciable amount of memory so they
**              should be used judiciously.
**
** PARAMETERS:  None.
**
** RETURN:      None.
**
***************************************************************/
void drawHands ()
{
    /*
    ** If the graphics state drawing was selected, install the graphics
    ** state user object for the appropriate hand and draw the hand
    */
    if (AppData.graphicStates)
    {
	PSgsave();
        /*
        ** Alarm hand
        */
        PSWInstallGstate (gstateHour, angleAlarm);
        drawClockHand (ALARM);

        /*
        ** Hour hand
        */
        PSWInstallGstate (gstateHour, angleHour);
        drawClockHand (HOUR);

        /*
        ** Minute hand
        */
        PSWInstallGstate (gstateMin, angleMin);
        drawClockHand (MINUTE);

        /*
        ** Second hand shadow
        */
        PSWInstallGstate (gstateShad, angleSec);
        drawClockHand (SHADOW);

        /*
        ** Second hand
        */
        PSWInstallGstate (gstateSec, angleSec);
        drawClockHand (SECOND);

	PSgrestore();
    }

    /*
    ** If the graphics state drawing was not selected, for each hand:
    ** save the current graphics state, set the color and line width,
    ** translate the origin, rotate the context, draw the hand, and
    ** restore the original graphics state for the next hand
    */
    else
    {
        /*
        ** Alarm hand 
        */
	PSWPushDrawingParams(0.0, 0.0, 0.0, 0.0, angleAlarm);
        drawClockHand (ALARM);
	PSgrestore();

        /*
        ** Hour hand
        */
	PSWPushDrawingParams(0.0, 0.0, CLRHANDS - 0.2,
				      LNWIDHANDS, angleHour);
        drawClockHand (HOUR);
	PSgrestore();
        
        /*
        ** Minute hand
        */
	PSWPushDrawingParams(OFFSETHANDSX, OFFSETHANDSY,
			     CLRHANDS - 0.2, LNWIDHANDS, angleMin);
        drawClockHand (MINUTE);
	PSgrestore();

        /*
        ** Second hand shadow
        */
	PSWPushDrawingParams(2*OFFSETHANDSX + OFFSETSHADX,
                             2*OFFSETHANDSY + OFFSETSHADY,
			     CLRSHADOW, LNWIDSECOND, angleSec);
        drawClockHand (SHADOW);
	PSgrestore();

        /*
        ** Second hand
        */
	PSWPushDrawingParams(2 * OFFSETHANDSX, 2 * OFFSETHANDSY,
			     CLRSECOND, LNWIDSECOND, angleSec);
        drawClockHand (SECOND);
        PSgrestore ();
    }

} /* end drawHands */

/***************************************************************
**
** FUNCTION:    convertTimeToHandAngles
**
** DESCRIPTION: Get the current system time and calculate the
**              angles of the hands.
**
** PARAMETERS:  timeOfDay	current time
**
** RETURN:      None.
**
***************************************************************/
static void convertTimeToHandAngles (timeOfDay)
    struct timeval      *timeOfDay;
{
    struct tm           *ltime;

    /*
    ** Get the current system time and convert to local time
    */
    ltime = localtime ((time_t *) &timeOfDay->tv_sec);

    /*
    ** Calculate the angles of the hour, minute, and second hands
    */
    angleHour = ((ltime->tm_hour % 12) + ltime->tm_min/60.0) *
	    DEGHOUR;
    angleMin = (ltime->tm_min + ltime->tm_sec/60.0) * DEGMIN;
    angleSec = ltime->tm_sec * DEGMIN;

    /*
    ** Save the millisecond value for later timer scheduling
    */
    AppData.milliSecs = timeOfDay->tv_usec / 1000;

} /* end convertTimeToHandAngles () */

/***************************************************************
**
** FUNCTION:    drawClockTime
**
** DESCRIPTION: Invoked each time the clock is drawn.  The clock
**              face pixmap is copied into the clock image
**              pixmap.  The current system time is obtained to
**              determine the angles of the hands.  The hands
**              are then drawn into the clock image pixmap.  The
**              draw area window is cleared which copies the
**              newly drawn clock image pixmap into the window.
**
** PARAMETERS:  None.
**
** RETURN:      None.
**
***************************************************************/
void drawClockTime ()
{
    long elapsedTime;
    static int depth;
    static GC gc = NULL;
    Display *dpy = XtDisplay(AppData.widget);
    Window win = XtWindow(AppData.widget);
    int total, i;

    /*
    ** Create a GC for copying, and find depth of drawing area
    */

    if (gc == NULL) {
	gc = XCreateGC(dpy, win, 0, NULL);
	XtVaGetValues(AppData.widget, XtNdepth, &depth, NULL);
    }

    /*
    ** If double buffering, create pixmap.
    ** Install as current drawable and capture a gstate
    ** If currently using gstates for hands, redefine them
    */

    if (AppData.doubleBuffering && AppData.clockPixmap == None) {
	AppData.clockPixmap = XCreatePixmap (dpy, win, AppData.Xwidth,
						 AppData.Xheight, depth);
	PSWSetDrawable(AppData.clockPixmap,
		       AppData.Xwidth/2, AppData.Xheight/2);
	XDPSCaptureContextGState(AppData.dpsCtxt, &gstateBuffer);
	if (AppData.graphicStates) defineGStates();
    }

    /*
    ** If using a pixmap background, create pixmap and draw face into
    ** it.  The DPSWaitContext for timing, after the if statement,
    ** guarantees synchronization.
    */

    if (AppData.pixmapBackground && AppData.facePixmap == None) {
	  AppData.facePixmap =
		  XCreatePixmap (dpy, win,
				 AppData.Xwidth, AppData.Xheight,
				 depth);
	  /* Temporarily render into pixmap */
	  PSgsave();
	  PSWSetDrawable(AppData.facePixmap,
			 AppData.Xwidth/2, AppData.Xheight/2);
          drawFace ();
	  PSgrestore();
    }

    /*
    ** Start timing and convert time into angles
    */

    DPSWaitContext (AppData.dpsCtxt);
    markTime ();

    convertTimeToHandAngles(&StartTime);

    /*
    ** If double buffering, draw into clock pixmap.  Either copy face
    ** pixmap or redraw the face, then draw the hands, then copy into window.
    ** Assume current context is set to render into the clock pixmap.
    ** Must call DPSWaitContext to ensure drawing is done before copying
    */

    if (AppData.doubleBuffering) {
	if (AppData.pixmapBackground) {
	    XCopyArea (dpy, AppData.facePixmap, AppData.clockPixmap,
		       gc, 0, 0, AppData.Xwidth, AppData.Xheight, 0, 0);
	} else { /* Do not use a predrawn face pixmap */
	    drawFace ();
	}

	drawHands ();
	DPSWaitContext (AppData.dpsCtxt);
	XCopyArea (dpy, AppData.clockPixmap, win, gc,
		   0, 0, AppData.Xwidth, AppData.Xheight, 0, 0);

    } else { 
	/*
	** If no double buffering, draw directly into window
	*/
	if (AppData.pixmapBackground) {
	    XCopyArea (dpy, AppData.facePixmap, win, gc,
		       0, 0, AppData.Xwidth, AppData.Xheight, 0, 0);
	} else {
	    drawFace ();
	}
	
	drawHands ();
    }

    /*
    ** Wait for completion, then get elapsed time
    */
    DPSWaitContext (AppData.dpsCtxt);
    returnTime (&elapsedTime);

    /*
    ** Increment time counters and set elapsed time into the display window
    ** Keep the last 5 times in the lastTimes array, and use their average
    ** as the displayed time.
    */

    for (i = 4; i > 0; i--) AppData.lastTimes[i] = AppData.lastTimes[i-1];
    AppData.lastTimes[0] = elapsedTime;
    if (AppData.numIterations < 5) AppData.numIterations++;

    total = 0;
    for (i = 0; i < AppData.numIterations; i++) total += AppData.lastTimes[i];

    setTimingValue (total / AppData.numIterations);

    /*
    ** Increment the millisecond value for later timer scheduling
    */
    AppData.milliSecs += elapsedTime;

    if (AppData.trace)
	    XmToggleButtonSetState (AppData.traceToggle, False, True);
}

/***************************************************************
**
** FUNCTION:    isHit
**
** DESCRIPTION: Check for hit detection.  No boundary check is
**              made because the alarm hand can reside in pretty
**              much the whole draw area.
**
** PARAMETERS:  x, y    X Window System coordinates of mouse
**
** RETURN:      hit     hit flag indicating mouse hit alarm hand
**
***************************************************************/
int isHit (x, y)
    int x, y;
{
    Bool hit;
    float ptX, ptY;
    int xoffset, yoffset;
    float ctm[6], invctm[6];

    /*
    ** Rotate coordinate system underneath the mouse point, and get
    ** new transformation matrices
    */

    PSgsave();
    PSrotate(angleAlarm);
    PSWGetTransform(ctm, invctm, &xoffset, &yoffset);

    /*
    ** Convert mouse point into user space
    */
    x -= xoffset;
    y -= yoffset;
    ptX = invctm[A_COEFF] * x + invctm[C_COEFF] * y + invctm[TX_CONS];
    ptY = invctm[B_COEFF] * x + invctm[D_COEFF] * y + invctm[TY_CONS];

    /*
    ** See if the point hits either the alarm top or bottom
    */

    hit = PSHitUserPath(ptX, ptY, HITSETTING,
                        (DPSPointer) ptsAlarmTop, XtNumber(ptsAlarmTop),
			dps_float, opsAlarmTop, XtNumber(opsAlarmTop), 
			(DPSPointer) bboxAlarmTop, dps_inufill);
    if (!hit) {
        hit = PSHitUserPath(ptX, ptY, HITSETTING,
                            (DPSPointer) ptsAlarmBot, XtNumber(ptsAlarmBot),
			    dps_float, opsAlarmBot, XtNumber(opsAlarmBot),
			    (DPSPointer) bboxAlarmBot, dps_inufill);
    }
    PSgrestore();

    /*
    ** Return the hit flag
    */
    return hit;

} /* end isHit () */

/***************************************************************
**
** FUNCTION:    checkAlarm
**
** DESCRIPTION: Check for alarm time to ring.  If it has rung in
**              in this interval, it must clear the interval or
**              be manually moved before it can ring again.
**
** PARAMETERS:  None.
**
** RETURN:      ring    flag indicating it is time to ring alarm
**
***************************************************************/
int checkAlarm ()
{
    int ring = FALSE;
    static int hasRung = FALSE;

    /*
    ** Determine if hour hand is within the alarm hand interval
    */
    if (angleHour <= angleAlarm - ALARM_INTERVAL &&
	angleHour >= angleAlarm + ALARM_INTERVAL) {
        if  (! hasRung) {
            /*
            ** Set the ring flag
            */
            hasRung = TRUE;
            ring    = TRUE;
        }
    } else {
        /*
        ** Clear the has rung flag
        */
        hasRung = FALSE;
    }
    
    /*
    ** Return the ring flag
    */
    return ring;

} /* end checkAlarm () */

/***************************************************************
**
** FUNCTION:    setAlarm
**
** DESCRIPTION: Enter a modal loop to redraw the alarm hand per
**              mouse drag event.
**
** PARAMETERS:  None.
**
** RETURN:      None.
**
***************************************************************/
void setAlarm ()
{
    XEvent  event;
    int     tracking = TRUE;
    int     x, y;
    float   ptX, ptY;
    Display *dpy = XtDisplay(AppData.widget);

    /*
    ** Wait for the first button motion or release event
    */
    XMaskEvent (dpy, ButtonMotionMask | ButtonReleaseMask, &event);
        
    /*
    ** Loop collecting button motion events until a button release event
    */
    while (tracking) {
        /*
        ** Consolidate events currently on the queue
        */
        while (XCheckMaskEvent(dpy, ButtonMotionMask | ButtonReleaseMask,
			       &event)) {
            if (event.type == ButtonRelease) {
                tracking = FALSE;
                break;
            }
        } /* end while loop */

        /*
        ** Compute the x,y coordinates of the button release  in DPS units
        */
        x = event.xbutton.x - XOffset;
        y = event.xbutton.y - YOffset;
        ptX = Invctm[A_COEFF] * x + Invctm[C_COEFF] * y + Invctm[TX_CONS];
        ptY = Invctm[B_COEFF] * x + Invctm[D_COEFF] * y + Invctm[TY_CONS];

        /*
        ** Compute the new alarm hand angle (-360 to 0 degrees)
        */
        angleAlarm = atan2 ((double) ptY, (double) ptX) / RADIAN  -  90.0;
        if (angleAlarm > 0) angleAlarm -= 360;

        /*
        ** Draw the clock with the new alarm hand angle and current time
        */
        drawClockTime ();

    } /* end while loop */
} /* end setAlarm () */

/***************************************************************
**
** FUNCTION:    initDPSContext
**
** DESCRIPTION: Post-Realization initialization of DPSContext and such.
**
** PARAMETERS:  None.
**
** RETURN:      None.
**
***************************************************************/
void initDPSContext ()
{
    Dimension height, width;
    Display *dpy = XtDisplay(AppData.widget);
    XSetWindowAttributes attr;

    /*
    ** Get height of drawing window
    */
    XtVaGetValues (AppData.widget, XtNheight, &height, XtNwidth, &width, NULL);

    /*
    ** Create the DPSContext in which rendering will occur
    */
    AppData.dpsCtxt = XDPSGetSharedContext(dpy);
    (void) XDPSSetEventDelivery(dpy, dps_event_pass_through);

    if (AppData.dpsCtxt == NULL) {
	printf("Couldn't create a Display PostScript context.\n");
	exit(1);
    }

    if (XDPSSetContextDrawable(AppData.dpsCtxt, XtWindow(AppData.widget),
                               height) != dps_status_success)
    {
        printf ("Couldn't set Display PostScript context drawable.\n");
        exit (1);
    }

    /*
    ** Set the default DPSContext
    */
    DPSSetContext (AppData.dpsCtxt);

    XDPSChainTextContext (AppData.dpsCtxt, AppData.trace);

    /*
    ** Save the initial graphics state
    */
    XDPSCaptureContextGState(AppData.dpsCtxt, &gstateWindow);

    /*
    ** Define the initial Window geometry
    */
    PSWGetTransform (Ctm, Invctm, &XOffset, &YOffset); 
    handleWindowResize(AppData.widget, width, height);

    /*
    ** Keep DPS origin in center of window
    */

    attr.bit_gravity = CenterGravity;
    XChangeWindowAttributes(dpy, XtWindow(AppData.widget),
			    CWBitGravity, &attr);

    /*
    ** Define the userpaths and gstates for the drawing of the hands
    */
    defineUPaths ();
} /* end initDPSContext () */

/***************************************************************
**
** FUNCTION:    markTime
**
** DESCRIPTION: Mark the start time for an operation.
**
** PARAMETERS:  None.
**
** RETURN:      None.
**
***************************************************************/
static void markTime ()
{
    gettimeofday (&StartTime, (struct timezone *)NULL);
}

/***************************************************************
**
** FUNCTION:    returnTime
**
** DESCRIPTION: Return the elapsed time since markTime was called
**              in milliseconds
**
** PARAMETERS:	elapsedTime	returned time in milliseconds
**
** RETURN:      None.
**
***************************************************************/
static void returnTime (elapsedTime)
    long *elapsedTime;
{
    struct timeval endTime;

    gettimeofday (&endTime, (struct timezone *)NULL);
    *elapsedTime = (endTime.tv_sec - StartTime.tv_sec) * 1000 +
	    (endTime.tv_usec - StartTime.tv_usec)/1000;
}

/***************************************************************
**
** FUNCTION:    handleWindowResize
**
** DESCRIPTION: Rescales coordinate system to reflect new window size
**
** PARAMETERS:	w		Widget that was resized
**		newWidth	New widget width
**		newHeight	New widget height
**
** RETURN:      None.
**
***************************************************************/

void handleWindowResize (w, newWidth, newHeight)
    Widget w;
    int newWidth, newHeight;
{
    float newDPSHeight, newDPSWidth;
    int x, y;
    float xScale, yScale;

    if (AppData.doubleBuffering) {
	XDPSSetContextGState(AppData.dpsCtxt, gstateWindow);
    }

    PSWSetOffset(newWidth/2, newHeight/2);

    /*
    ** Compute coordinate of upper right corner in user space
    */
    x = newWidth - newWidth/2;
    y = 0 - newHeight/2;

    newDPSWidth = Invctm[A_COEFF] * x + Invctm[C_COEFF] * y + Invctm[TX_CONS];
    newDPSHeight = Invctm[B_COEFF] * x + Invctm[D_COEFF] * y + Invctm[TY_CONS];

    xScale = newDPSWidth / CLOCKSIZE;
    yScale = newDPSHeight / CLOCKSIZE;

    PSscale (xScale, yScale);
    PSinitclip();
    PSinitviewclip();

    PSWGetTransform (Ctm, Invctm, &XOffset, &YOffset);

    AppData.Xwidth = newWidth;
    AppData.Xheight = newHeight;

    if (AppData.clockPixmap) {
	XFreePixmap (XtDisplay(w), AppData.clockPixmap);
	XDPSFreeContextGState(AppData.dpsCtxt, gstateBuffer);
    }
    if (AppData.facePixmap) XFreePixmap (XtDisplay(w), AppData.facePixmap);
    AppData.clockPixmap = AppData.facePixmap = None;

    XDPSUpdateContextGState(AppData.dpsCtxt, gstateWindow);

    if (AppData.graphicStates && !AppData.doubleBuffering) defineGStates();
} /* end resizeWindow () */

/***************************************************************
**
** FUNCTION:    setBufferRendering
**
** DESCRIPTION: Enable or disable rendering to offscreen buffer
**
** PARAMETERS:	None
**
** RETURN:      None.
**
***************************************************************/
void setBufferRendering()
{
    if (AppData.doubleBuffering) {
	if (AppData.clockPixmap != None) {
	    XDPSSetContextGState(AppData.dpsCtxt, gstateBuffer);
	}
    } else XDPSSetContextGState(AppData.dpsCtxt, gstateWindow);
    if (AppData.graphicStates) defineGStates();
}
