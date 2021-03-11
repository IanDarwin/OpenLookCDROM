/*
 * LineChart Display Widget
 *
 * Copyright 1989 PCS Computer Systeme GmbH, West Germany
 *
 * Permission to use, copy, modify, distribute, and sell this software and its
 * documentation for any purpose is hereby granted without fee, provided that
 * the above copyright notice appear in all copies and that both that
 * copyright notice and this permission notice appear in supporting
 * documentation, and that the name of PCS not be used in advertising or
 * publicity pertaining to distribution of the software without specific,
 * written prior permission.  PCS makes no representations about the
 * suitability of this software for any purpose.  It is provided "as is"
 * without express or implied warranty.
 *
 * PCS DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING ALL
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL PCS
 * BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION
 * OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN 
 * CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *
 * Original Author:  Emanuel Jay Berkenbilt, MIT Project Athena
 * Author:           Thomas A. Baghli, PCS Computer Systeme GmbH, West Germany
 *                   tom@meepmeep.pcs.com
 */


#include <stdio.h>

#include <X11/IntrinsicP.h>
#include <X11/StringDefs.h>
#include <X11/Xmu/Misc.h>
#include <X11/Xlib.h>

#include "LineChP.h"

extern char *calloc(), *malloc();

static void HandleKey();

static char defaultTranslations[] =
    "<KeyDown>:    HandleKey()    ";

static XtActionsRec actionsTable[] = {
     { "HandleKey",    HandleKey },
     { NULL,           NULL      },
};

static Dimension defDimension = 200;
static int defInterval = 10*1000;
static int defStepSize = 1;
static int defMinValue = 0;
static int defMaxValue = 10;

#define offset(field) XtOffset(LineChartWidget, lineChart.field)
#define goffset(field) XtOffset(Widget, core.field)

static XtResource resources[] = {
    { XtNwidth, XtCWidth, XtRDimension, sizeof(Dimension),
	goffset(width), XtRDimension, (caddr_t) &defDimension },
    { XtNheight, XtCHeight, XtRDimension, sizeof(Dimension),
	goffset(height), XtRDimension, (caddr_t) &defDimension },
    { XtNbackground, XtCBackground, XtRPixel, sizeof(Pixel),
	goffset(background_pixel), XtRString, "White" },
    { XtNforeground, XtCForeground, XtRPixel, sizeof(Pixel),
        offset(fgpixel), XtRString, "Black" },
    { XtNhighlight, XtCForeground, XtRPixel, sizeof(Pixel),
        offset(hipixel), XtRString, "Black" },
    { XtNfont, XtCFont, XtRFontStruct, sizeof(XFontStruct *),
        offset(font), XtRString, XtDefaultFont },
    { XtNreverseVideo, XtCReverseVideo, XtRBoolean, sizeof (Boolean),
	offset(reverse_video), XtRString, "FALSE" },
    { XtNstepSize, XtCStepSize, XtRInt, sizeof(int),
       offset(step_size), XtRInt, (caddr_t)&defStepSize },
    { XtNupdate, XtCInterval, XtRInt, sizeof(int),
       offset(update), XtRInt, (caddr_t)&defInterval },
    { XtNgetValue, XtCCallback, XtRCallback, sizeof(caddr_t),
        offset(get_value), XtRImmediate, (caddr_t) NULL},
    { XtNlabel, XtCLabel, XtRString, sizeof(String),
	offset(label), XtRString, NULL},
    { XtNminValue, XtCValue, XtRInt, sizeof(int),
	offset(min_value), XtRInt, (caddr_t) &defMinValue},
    { XtNmaxValue, XtCValue, XtRInt, sizeof(int),
	offset(max_value), XtRInt, (caddr_t) &defMaxValue}
    
};

#undef offset
#undef goffset

static void ClassInitialize();
static void Initialize(), Realize(), Redisplay(), Destroy();
static Boolean SetValues();

LineChartClassRec lineChartClassRec = {
    { /* core fields */
    /* superclass		*/	&widgetClassRec,
    /* class_name		*/	"LineChart",
    /* size			*/	sizeof(LineChartRec),
    /* class_initialize		*/	ClassInitialize,
    /* class_part_initialize	*/	NULL,
    /* class_inited		*/	FALSE,
    /* initialize		*/	Initialize,
    /* initialize_hook		*/	NULL,
    /* realize			*/	Realize,
    /* actions			*/	actionsTable,
    /* num_actions		*/	XtNumber(actionsTable),
    /* resources		*/	resources,
    /* num_resources		*/	XtNumber(resources),
    /* xrm_class		*/	NULL,
    /* compress_motion		*/	TRUE,
    /* compress_exposure	*/	TRUE,
    /* compress_enterleave	*/	TRUE,
    /* visible_interest		*/	FALSE,
    /* destroy			*/	Destroy,
    /* resize			*/	NULL,
    /* expose			*/	Redisplay,
    /* set_values		*/	SetValues,
    /* set_values_hook		*/	NULL,
    /* set_values_almost	*/	NULL,
    /* get_values_hook		*/	NULL,
    /* accept_focus		*/	NULL,
    /* version			*/	XtVersion,
    /* callback_private		*/	NULL,
    /* tm_table			*/	defaultTranslations,
    /* query_geometry		*/	XtInheritQueryGeometry,
    /* display_accelerator	*/	XtInheritDisplayAccelerator,
    /* extension		*/	NULL
    }
};

WidgetClass lineChartWidgetClass = (WidgetClass) &lineChartClassRec;

static void draw_background(), draw_it(caddr_t client_data, XtIntervalId *id);
static void graph();
static void refresh_graph(), shift_graph();
static void adjust_timeout();

static void
ClassInitialize()
{
    /* DO NOTHING */
}

/* ARGSUSED */
static void
Initialize(request, new)
    Widget request, new;
{
    LineChartWidget pw = (LineChartWidget) new;
    XtGCMask valuemask;
    XGCValues myXGCV;
    int min_size;
    Display *dpy = XtDisplay(pw);
    int screen = DefaultScreen(XtDisplay(pw));

    /* The maximum number of values that can be saved - this
     * is the number of horizontal pixels in the screen.
     */

    int NUM_VALS = DisplayWidth(dpy, screen);

    if (pw->lineChart.reverse_video) {
	Pixel fg = pw->lineChart.fgpixel;
	Pixel bg = pw->core.background_pixel;

	if (pw->core.border_pixel == fg)
	     pw->core.border_pixel = bg;
	if (pw->lineChart.hipixel == pw->lineChart.fgpixel)
	     pw->lineChart.hipixel = bg;
	pw->lineChart.fgpixel = bg;
	pw->core.background_pixel = fg;
    }

    valuemask = GCForeground | GCBackground;
    myXGCV.foreground = pw->lineChart.fgpixel;
    if (pw->lineChart.font) {
	 myXGCV.font = pw->lineChart.font->fid;
	 valuemask |= GCFont;
    }
    myXGCV.background = pw->core.background_pixel;
    pw->lineChart.fgGC = XtGetGC(new, valuemask, &myXGCV);
    myXGCV.foreground = pw->lineChart.hipixel;
    pw->lineChart.hiGC = XtGetGC(new, valuemask, &myXGCV);

    if (pw->lineChart.font == NULL)
	pw->lineChart.font = XQueryFont(XtDisplay(pw),
	    XGContextFromGC(DefaultGCOfScreen(XtScreen(pw))));

    pw->lineChart.font_height = pw->lineChart.font->max_bounds.ascent
	 + pw->lineChart.font->max_bounds.descent;

    if (!VALID_UPDATE(pw->lineChart.update))
	 pw->lineChart.update = defInterval;
    pw->lineChart.orig_update = pw->lineChart.update;
    pw->lineChart.interval_id =
	     XtAppAddTimeOut(XtWidgetToApplicationContext( (Widget) pw),
		pw->lineChart.update,
		(XtTimerCallbackProc)draw_it, 
		(XtPointer) pw);

    pw->lineChart.lines_per_label = LINES_PER_LABEL;

    /* Allocate memory for saved_values.  This has to be done after
     * the display is opened because before that there is no way of
     * finding out the width of the screen.
     */
    if ((pw->lineChart.saved_values =
	 (int *) calloc(NUM_VALS, sizeof(int))) == NULL) {
	    perror("LineChart: Failure getting memory for saved_values.");
	    exit(1);
    }

    /* Calculate the minimum width and height of the window
     * so that the text and graphs fit reasonably well.
     */

    min_size = 2 * label_width(pw);
    if (request->core.width < min_size)
	 pw->core.width = min_size;

    pw->core.height = HOST_HEIGHT + 2 * HOST_HEIGHT
	 + TOPSTUFF + BOTTOMSTUFF + 2 * INNER_BORDER;
    pw->lineChart.g_width = 0;
    pw->lineChart.left_pix = 0;
    pw->lineChart.cur_value = 0;
}

static void
Realize(w, valueMask, attrs)
    Widget w;
    XtValueMask *valueMask;
    XSetWindowAttributes *attrs;
{
    XtCreateWindow(w, (unsigned)InputOutput, (Visual *)CopyFromParent,
	 *valueMask, attrs);
}

/* ARGSUSED */
static void
Redisplay(w, event, region)
    Widget w;
    XEvent *event;
    Region region;
{
    LineChartWidget pw = (LineChartWidget) w;

    if (XtIsRealized((Widget) pw)) {
	XClearWindow(XtDisplay(pw), XtWindow(pw));
	draw_background(pw);
	if ((pw->lineChart.cur_value + 1) * pw->lineChart.step_size
	    > pw->lineChart.g_width)
	    shift_graph(pw);
	if (pw->lineChart.g_width > 0)
	    refresh_graph(pw);
    }
}

static void
Destroy(w)
    Widget w;
{
    LineChartWidget pw = (LineChartWidget) w;

    if (pw->lineChart.interval_id)
	XtRemoveTimeOut(pw->lineChart.interval_id);
    XtDestroyGC(pw->lineChart.fgGC);
    XtDestroyGC(pw->lineChart.hiGC);
}

/* ARGSUSED */
static Boolean
SetValues(current, request, new)
    Widget current, request, new;
{
    LineChartWidget oldpw = (LineChartWidget) current;
    LineChartWidget pw = (LineChartWidget) new;

    if (pw->lineChart.update != oldpw->lineChart.update) {
	XtRemoveTimeOut(oldpw->lineChart.interval_id);
	pw->lineChart.interval_id =
	     XtAppAddTimeOut(XtWidgetToApplicationContext( (Widget) pw),
			     pw->lineChart.update,
			     (XtTimerCallbackProc)draw_it, (XtPointer) pw);
    }
    return(FALSE);
}


/* ARGSUSED */
static void
draw_it(client_data, id)
caddr_t client_data;
XtIntervalId *id;		/* unused */
{
    LineChartWidget pw = (LineChartWidget) client_data;
    int value;

    if (pw->lineChart.update > 0)
	pw->lineChart.interval_id =
	     XtAppAddTimeOut(XtWidgetToApplicationContext( (Widget) pw),
			     pw->lineChart.update,
			     (XtTimerCallbackProc)draw_it, (XtPointer) pw);

    if (pw->lineChart.get_value == NULL)
      return;

    XtCallCallbacks( (Widget) pw, XtNgetValue, (caddr_t) &value);
    graph(pw, value);
}


/****************************************************************************

   These routines take an array of integers and graph them by drawing
   parallel line graphs inside of a single window.  Each graph is called
   a "graph."  The entire set of graphs is called a "graph set."

   This program keeps track of a few kinds of values.  There are
   1.  Raw values: These are the values that are passed to this
       section of the program from main_event_loop() in xLineChart.c.  
       They are simply a list of numbers as far as window.c is concerned.
       Before they can be used by X, they need to be scaled.  See the
       comment on the SCALE macro below for more information.
   2.  Pixel values: These values represent actual numbers of pixels.
   3.  Saved values: These correspond to values on the graph.  If
       the stepsize of the graph were always one, there would be no 
       difference between these and pixel values, but since the step 
       size can be set by the user of the program from the commandline,
       there must be a distinction.  Since it is important to know 
       which values are Pixel values and which ones are Saved values, 
       the comments on the data structure and variables will tell 
       which type of values each variable or field is.  A lot of subtle
       bugs (especially in refresh) can occur if the values are mixed up.

*****************************************************************************/
 
/* Convenient macros to do text stuff in the window. */
#define XTW(s) XTextWidth(pw->lineChart.font, s, strlen(s))
#define XDS(s) XDrawString(XtDisplay(pw), XtWindow(pw), \
			   pw->lineChart.hiGC, x, y, s, strlen(s))

static void
draw_background(pw)
    LineChartWidget pw;
/* This routine draws the background for a window - that is, the hostname,
 * the dividers, and the labels including maximum and minimum values.
 */
{
    int g_height;
    int x, x1, x2;
    int y, y1, y2;
    char numstring[20];
  
    /* Calculate the size of a single graph based on the size of the window. */

    pw->lineChart.g_width = pw->core.width - label_width(pw);
    if (pw->lineChart.g_width < 0)
	 pw->lineChart.g_width = 0;
    g_height = pw->core.height - TOPSTUFF - BOTTOMSTUFF;

    pw->lineChart.left_pix = label_width(pw);

    x1 = pw->lineChart.left_pix;
    y1 = TOPSTUFF;
    x2 = x1 + pw->lineChart.g_width;
    y2 = y1;
    XDrawLine(XtDisplay(pw), XtWindow(pw), pw->lineChart.fgGC,
	      x1, y1, x2, y2);

    y1 = g_height + TOPSTUFF;
    y2 = y1;
    XDrawLine(XtDisplay(pw), XtWindow(pw), pw->lineChart.fgGC,
	      x1, y1, x2, y2);


    /* Draw label. */

    /* Set min_pix, max_pix, and zero values based on the
     * graph number, remembering to add in the TOPSTUFF and
     * GRAPH_BORDER - see comments in #defines section.
     */

    pw->lineChart.min_pix = g_height + TOPSTUFF - GRAPH_BORDER;
    pw->lineChart.max_pix = TOPSTUFF + GRAPH_BORDER;
    pw->lineChart.zero = pw->lineChart.min_pix
      + (SCALE(pw, pw->lineChart.min_value));

    /* Note that since vertical pixel values increase as
     * you move down the screen, max_pix < min_pix.
     *
     * Note "%d " rather than "%d".  This to leave a space
     * after the number before the beginning of the graph.
     */

    (void) sprintf(numstring, "%d ", pw->lineChart.min_value);
    x = x1 - XTW(numstring);
    y = pw->lineChart.min_pix;
    XDS(numstring);

    (void) sprintf(numstring, "%d ", pw->lineChart.max_value);
    x = x1 - XTW(numstring);
    y = pw->lineChart.max_pix + pw->lineChart.font_height;
    XDS(numstring);

    x = INNER_BORDER;
    /* Set the y value for the first string so that the members
     * of the label array will be centered in the graph area.
     */
    y = pw->lineChart.max_pix + DRAW_GRAPH_HEIGHT(pw) / 2
      - (pw->lineChart.lines_per_label - 2)
	* (pw->lineChart.font_height / 2 + FONT_PAD);
    XDS(pw->lineChart.label);
    y = y + pw->lineChart.font_height + 2 * FONT_PAD;
    XFlush(XtDisplay(pw));
}

/* This routine calculates the the width in pixels of the label section of 
 * the graph set.  It does this by taking the width of the longest string
 * in the label array and adding to it the width of a five-digit number
 * and a space (as represented by "99999 " below).
 */

int
label_width(pw)
LineChartWidget pw;
{
    return(XTW(pw->lineChart.label) + XTW("99999 ") + INNER_BORDER + FONT_PAD);
}

/* This routine takes care of shifting the graph to the left when the
 * information gets drawn off the right end.  It is also called from
 * redisplay_window if the current_value is larger than will fit on the
 * graph.  It should be called whenever this occurs.  This routine 
 * moves the current_value to the center of the graph and takes care
 * of moving all saved information so that the graph will be updated 
 * properly.  In addition, this routine sees that the max_value field
 * for each graph is as high as it needs to be.
 */

static void
shift_graph(pw)
LineChartWidget pw;
{
    register int i,t;
    int num_vals_shift;
    int max;
    int min;

    if (pw->lineChart.g_width == 0) {
	/* This means that the window has not been drawn yet. */
	return;
    }

    num_vals_shift = (pw->lineChart.g_width / 2) / pw->lineChart.step_size;

    for (i = 0; i < num_vals_shift; i++)
	 pw->lineChart.saved_values[i] =
	      pw->lineChart.saved_values
		[pw->lineChart.cur_value - num_vals_shift + i];
    max = min = 0;
    for (i = 0; i < num_vals_shift; i++) {
	    t = pw->lineChart.saved_values[i];
	    max = (max > t) ? max : t;
	    min = (min < t) ? min : t;
    }
    if (max == min) {
	    max = pw->lineChart.max_value;
    }
    pw->lineChart.max_value = max;
    if (pw->lineChart.max_value == min) {
	    min -= 1;
    }
    pw->lineChart.min_value = min;
    pw->lineChart.cur_value = num_vals_shift;
  
    XClearArea(XtDisplay(pw), XtWindow(pw), 0, 0,
	       pw->lineChart.g_width, pw->core.height, True);
}

/* This routine takes care of all the scaling of values.  This routine takes
 * the graph number and the raw value and sets the values of x and y to the
 * actual point on the window corresponding to those values.
 */

void
val_to_pixels(pw, which_val, x, y)
LineChartWidget pw;
int which_val;
int *x, *y;
{
    register int tmp = (pw->lineChart.zero -
			SCALE(pw,pw->lineChart.saved_values[which_val]));
    *x = pw->lineChart.left_pix + pw->lineChart.step_size * which_val;
    *y = Max(tmp, pw->lineChart.max_pix);

    /* If the choice of Max and Min look odd, just remember that pixel values
     * go up as actual values go down; i.e., min_pix is greater than max_pix.
     */
}

/* This routine adds the values that are given to it to the graph. */

static void
graph(pw, value)
    LineChartWidget pw;
    int value;
{
    int x1, y1, x2, y2;
    Display *dpy = XtDisplay(pw);
    int screen = DefaultScreen(XtDisplay(pw));

    /* The maximum number of values that can be saved - this
     * is the number of horizontal pixels in the screen.
     */
    int NUM_VALS = DisplayWidth(dpy, screen);

    if (pw->lineChart.g_width == 0) {
	 /* This means that the window has not been drawn yet. */
	 return;
    }

    pw->lineChart.saved_values[pw->lineChart.cur_value] = value;

    val_to_pixels(pw, pw->lineChart.cur_value, &x2, &y2);

    if (pw->lineChart.cur_value == 0)	{
	    /* If this is the leftmost point, then just plot it. */
	    XDrawPoint(XtDisplay(pw), XtWindow(pw), pw->lineChart.fgGC,
		       x2, y2);
    } else {
	    /* If this is not the left most point, then draw a line
	     * connecting this point with the one to its left.
	     */
	    val_to_pixels(pw, (pw->lineChart.cur_value - 1) % NUM_VALS,
			  &x1, &y1);
	    XDrawLine(XtDisplay(pw), XtWindow(pw), pw->lineChart.fgGC,
		      x1, y1, x2, y2);
    }

    /* Check to see whether the graph needs to be shifted. */

    if ((pw->lineChart.cur_value + 1) * pw->lineChart.step_size
	> pw->lineChart.g_width) {
	    shift_graph(pw);
    }

    /* Update the current value.  This needs to be done even when the
     * graph is shifted, as all shift_graph does is move the pointers.
     */
    pw->lineChart.cur_value++;
}

/* This routine redraws the graph from the values stored in
 * pw->lineChart.saved_values[][].
 */

static void
refresh_graph(pw)
LineChartWidget pw;
{
    int save_cur_value;

    save_cur_value = pw->lineChart.cur_value;

    for (pw->lineChart.cur_value = 0;
	 pw->lineChart.cur_value <= save_cur_value;) {
	    graph(pw, pw->lineChart.saved_values[pw->lineChart.cur_value]);
    }
}

/*ARGSUSED*/
static void
HandleKey(w, event, params, num_params)
    Widget w;
    XEvent *event;
    String *params;
    Cardinal *num_params;
/* This routine interprets the key that was pressed on top of the window. 
 * It returns TRUE if Quit has been selected, signaling that the program
 * is done.
 */
{
#define STRBUFSIZE 64
    char strbuf[STRBUFSIZE];
    KeySym keycode;
    LineChartWidget pw = (LineChartWidget) w;

    (void) XLookupString( (XKeyEvent *) event, strbuf, STRBUFSIZE,
			 &keycode, NULL);

    switch (keycode) {
    case 'Q':
    case 'q':
	exit(0);
	break;
    case 'R':
    case 'r':
	/* This routine resets the graph moving the current value
	 * pointer to zero so that all saved values are ignored.
	 *
	 * Clear the window, signaling an exposure event.
	 */
	XClearArea(XtDisplay(w), XtWindow(w), 0, 0,
	     pw->core.width, pw->core.height, True);
	pw->lineChart.cur_value = 0;

	if (pw->lineChart.interval_id)
	    XtRemoveTimeOut(pw->lineChart.interval_id);
	pw->lineChart.update = pw->lineChart.orig_update;
	pw->lineChart.interval_id =
	     XtAppAddTimeOut(XtWidgetToApplicationContext( (Widget) pw),
			     pw->lineChart.update,
			     (XtTimerCallbackProc)draw_it, (XtPointer) pw);
	break;
    case 's':
	/* Maybe the following adjustments should be calls to SetValues? */
	adjust_timeout(pw, SMALL_ADD_TIME);
	break;
    case 'S':
	adjust_timeout(pw, LARGE_ADD_TIME);
	break;
    case 'f':
	adjust_timeout(pw, SMALL_SUBTRACT_TIME);
	break;
    case 'F':
	adjust_timeout(pw, LARGE_SUBTRACT_TIME);
	break;
    case '?':
	puts("Q/q - Quit");
	puts("R/r - Reset graph and timer");
	puts("s - Decrease update interval (slower) by a small amount");
	puts("S - Decrease update interval (slower) by a large amount");
	puts("f - Increase update interval (faster) by a small amount");
	puts("F - Increase update interval (faster) by a large amount");
	puts("? - Help");
	break;
    default:
	break;
    }
}

static void
adjust_timeout(pw, delta)
    LineChartWidget pw;
    int delta;
{
    if (pw->lineChart.interval_id)
	XtRemoveTimeOut(pw->lineChart.interval_id);

    while (!VALID_UPDATE(pw->lineChart.update + delta))
	delta /= 2;
    pw->lineChart.update += delta;

    pw->lineChart.interval_id =
	     XtAppAddTimeOut(XtWidgetToApplicationContext( (Widget) pw),
			     pw->lineChart.update,
			     (XtTimerCallbackProc)draw_it, (XtPointer) pw);
}
