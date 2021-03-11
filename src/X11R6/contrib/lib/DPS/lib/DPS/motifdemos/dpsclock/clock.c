/* clock.c - Creates/Manages a drawing area with a clock in it

Derived from Xaw/Clock.c in 1991 by Adobe Systems Incorporated.
   
Copyright 1987, 1988 by Digital Equipment Corporation, Maynard, Massachusetts,
and the Massachusetts Institute of Technology, Cambridge, Massachusetts.

                        All Rights Reserved

Permission to use, copy, modify, and distribute this software and its
documentation for any purpose and without fee is hereby granted,
provided that the above copyright notice appear in all copies and that
both that copyright notice and this permission notice appear in
supporting documentation, and that the names of Digital or MIT not be
used in advertising or publicity pertaining to distribution of the
software without specific, written prior permission.

DIGITAL DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING
ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL
DIGITAL BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR
ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,
WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION,
ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS
SOFTWARE.

*/

/*
 * (c) Copyright 1990-1994 Adobe Systems Incorporated.
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

#include <X11/StringDefs.h>
#include <X11/cursorfont.h>
#include <X11/Intrinsic.h>
#include <X11/Xutil.h>
#include <Xm/Xm.h>
#include <Xm/DrawingA.h>
#include <X11/Xos.h>
#include <X11/Xlib.h>
#include <X11/StringDefs.h>
#include <X11/IntrinsicP.h>
#include <DPS/dpsXclient.h>
#include "clockint.h"
#include <stdio.h>
#include <time.h>

/*	===== BEGIN	CONSTANTS =====		*/
#define SECOND_HAND_TIME	30
/*	===== END	CONSTANTS =====		*/


/*	===== BEGIN	PRIVATE PROCS =====	*/
static void Resize(w, data, callData)
  Widget w;
  ClockData *data;
  caddr_t callData;
  {
  Display *dpy;
  Arg wargs[2];
  Dimension width, height;

  if (!XtIsRealized(w)) return;
  XSync(XtDisplay(w), False);
  if (!data->initialized) RealizeClock(w);

  XtSetArg(wargs[0], XtNheight, &height);
  XtSetArg(wargs[1], XtNwidth, &width);
  XtGetValues(w, wargs, 2);

  PSWResizeClock(data->ctxt, width, height, data->params.padding);
  }

static void ClockTic(data, id)
  ClockData *data;
  XtIntervalId *id;
  {
  Widget w = data->w;
  struct tm *localtime();
  struct tm tm;
  time_t time_value;
  char timeString[28];
  char *timePtr = timeString;
  register Display *dpy = XtDisplay(w);

  if (id || !data->intervalID)
    data->intervalID = XtAddTimeOut(data->params.update * 1000,
	    (XtTimerCallbackProc) ClockTic, (caddr_t)data);

  (void)time (&time_value);
  tm = *localtime(&time_value);

  /* Beep on the half hour; double-beep on the hour.	*/
  if (data->params.chime == TRUE)
    {
    if (data->beeped && (tm.tm_min != 30) && (tm.tm_min != 0))
      data->beeped = FALSE;

    if (((tm.tm_min == 30) || (tm.tm_min == 0)) && (!data->beeped))
      {
      data->beeped = TRUE;
      XBell(dpy, 50);
      if (tm.tm_min == 0)
	XBell(dpy, 50);
      }
    }

  if (data->params.analog == FALSE)
    {
    int i, len, prevLen;
    char timeString[26];	/* Specification of asctime */

    timePtr = asctime(&tm);
    len = strlen(timePtr);

    if (timePtr[len - 1] == '\n')
      timePtr[--len] = '\0';

    /* Move year before time components */

    strncpy(timeString, &timePtr[0], 10);
    strncpy(&timeString[10], &timePtr[19], 5);
    strncpy(&timeString[15], &timePtr[10],9);
    timeString[24] = '\0';
    timePtr = timeString;

    prevLen = strlen(data->prevTimeString);

    i = 0;
    while ((i<len) && (i<prevLen) && (data->prevTimeString[i]==timePtr[i]))
      i++;

    strcpy(data->prevTimeString + i, timePtr + i);

    PSWUpdateDigitalClock(data->ctxt, timePtr, i);
    }
  else
    {
    /*
     * The second (or minute) hand is sec (or min) sixtieths around the clock
     * face. The hour hand is (hour + min/60) twelfths of the way around the
     * clock-face.  The derivation is left as an excercise for the reader.
     */

    /*
     * 12 hour clock.
     */
    if (tm.tm_hour > 12)
      tm.tm_hour -= 12;

    if (!id ||
        tm.tm_min != data->otm.tm_min || tm.tm_hour != data->otm.tm_hour)
      {
      PSWDrawHands(
        data->ctxt,
        (((double)tm.tm_hour) + (((double)tm.tm_min) / 60.0)) * 30.0,
     	((double)tm.tm_min) * 6.0);
      }

    if (data->showSecondHand == TRUE)
      PSWDrawSecondHand(data->ctxt, ((double) tm.tm_sec) * 6.0);
    data->otm = tm;
    }

  DPSWaitContext(data->ctxt);
  }


static void Redisplay(w, data, clientData)
  Widget w;
  ClockData *data;
  XmDrawingAreaCallbackStruct *clientData;
  {
  if (!data->initialized)
    {
    RealizeClock(w);
    DPSinitviewclip(data->ctxt);
    Resize(w, data, clientData);
    }
  if (data->params.analog)
    {

    /* We can be smart about exposure events as long as the second
       hand isn't showing.  Otherwise, we just redraw the whole
       clock on the last event. */

	if (data->showSecondHand)
		{
	    if (clientData->event->xexpose.count != 0)
			return;
		}
    else 
		{
		extern void NewViewClip();
		DPSinitviewclip(data->ctxt);
		NewViewClip(data->ctxt, &clientData->event->xexpose);
		}
	PSWRedrawClockFace(data->ctxt);
	DPSWaitContext(data->ctxt);
    }
  else
    {
    data->prevTimeString[0] = '\0';
    }

  ClockTic((caddr_t)data, (XtIntervalId)0);
  DPSinitviewclip(data->ctxt);
  }
/*	===== END	PRIVATE PROCS =====	*/


/*	===== BEGIN	PUBLIC PROCS =====	*/
Widget CreateClock(parent, params)
  Widget parent;
  ClockParams *params;
  {
  Arg wargs[3];
  ClockData *data;

  data = (ClockData *)malloc(sizeof(ClockData));
  data->initialized = 0;
  data->params = *params;

  XtSetArg(wargs[0], XmNuserData, data);
  XtSetArg(wargs[1], XtNwidth, 300);
  XtSetArg(wargs[2], XtNheight, 200);
  data->w = XtCreateManagedWidget(
    "clockCanvas", xmDrawingAreaWidgetClass, parent, wargs, 3);
  XtAddCallback(data->w, XmNexposeCallback, (XtCallbackProc) Redisplay, data);
  XtAddCallback(data->w, XmNresizeCallback, (XtCallbackProc) Resize, data);

  if (data->params.update <= 0)
    data->params.update = 60;	/* make invalid updates use a default */

  data->showSecondHand = (data->params.update <= SECOND_HAND_TIME);
  data->intervalID = 0;

  return(data->w);
  }

void RealizeClock(w)
  Widget w;
  {
  Dimension width, height;
  Arg wargs[3];
  ClockData *data;
  Display *dpy = XtDisplay(w);

  XtSetArg(wargs[0], XtNheight, &height);
  XtSetArg(wargs[1], XtNwidth, &width);
  XtSetArg(wargs[2], XmNuserData, &data);
  XtGetValues(w, wargs, 3);

  /* set bit gravity */
  {
  Window win = XtWindow(w);
  unsigned long mask = CWBitGravity;
  XSetWindowAttributes attr;

  attr.bit_gravity = ForgetGravity;
  XChangeWindowAttributes(dpy, win, mask, &attr);
  XSync(dpy, False);
  }

  data->gc = XCreateGC(dpy, RootWindow(dpy, DefaultScreen(dpy)), 0, NULL);
  data->ctxt = XDPSCreateSimpleContext(
    dpy, XtWindow(w), data->gc, 0, height,
    DPSDefaultTextBackstop, DPSDefaultErrorProc, NULL);

  if (data->ctxt == NULL)
    {
    fprintf(stderr, "\ndpsclock: DPS is not available.\n");
    fprintf(stderr,
      "You need an X server with the DPS extension, or a DPS NX agent.\n");
    exit(1);
    }

  PSWInitClock1(data->ctxt, data->params.font, width, height);
  PSWInitClock2(data->ctxt);	/* VAX Compiler limit */
  data->initialized = 1;
  }
/*	===== END	PUBLIC PROCS =====	*/
