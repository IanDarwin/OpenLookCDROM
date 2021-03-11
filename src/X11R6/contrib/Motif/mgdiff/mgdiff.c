#ifndef lint
static char rcsid[] = "mgdiff.c,v 2.0 1994/05/19 02:01:11 dan Exp";
#endif

#ifndef lint
static char copyright[] = "Copyright (c) 1994, Daniel Williams";
#endif

/*
 * Copyright (c) 1994    Daniel Williams
 * 
 * The X Consortium, and any party obtaining a copy of these files from
 * the X Consortium, directly or indirectly, is granted, free of charge,
 * a full and unrestricted irrevocable, world-wide, paid up,
 * royalty-free, nonexclusive right and license to deal in this software
 * and documentation files (the "Software"), including without limitation
 * the rights to use, copy, modify, merge, publish, distribute,
 * sublicense, and/or sell copies of the Software, and to permit persons
 * who receive copies from any such party to do so.  This license
 * includes without limitation a license to do the foregoing actions
 * under any patents of the party supplying this software to the X
 * Consortium.  The following conditions apply:
 * 
 * The above copyright notice and this permission notice shall be
 * included in all copies or substantial portions of the Software.
 * 
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
 * EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
 * MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
 * IN NO EVENT SHALL DANIEL WILLIAMS OR SYSTEMS & SCIENTIFIC SOFTWARE BE
 * LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
 * OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
 * WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

#include <X11/Xos.h>

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <errno.h>
#include <sys/stat.h>

#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
#include <Xm/MainW.h>
#include <Xm/Form.h>
#include <Xm/Frame.h>
#include <Xm/DrawingA.h>
#include <Xm/MessageB.h>
#include <Xm/ScrollBar.h>
#include <Xm/PushB.h>
#include <Xm/ToggleB.h>
#include <Xm/PanedW.h>
#include <Xm/Label.h>
#include <Xm/TextF.h>
#include <Xm/AtomMgr.h>
#include <Xm/Protocols.h>

#include "mgdiff.h"
#include "externs.h"
#include "patchlevel.h"
#include "mgdiff.xbm"

static void Visible (Widget widget, XtPointer closure, XEvent *event, Boolean *continue_to_dispatch);
static Region RegionFromRect (int x, int y, int w, int h);
static void resize_cb (Widget w, XtPointer closure, XtPointer call_data);
static void drawit (Widget w, XtPointer closure, XtPointer call_data);
static void file_cb (Widget w, XtPointer closure, XtPointer call_data);
static void view_cb (Widget w, XtPointer closure, XtPointer call_data);
static void select_cb (Widget w, XtPointer closure, XtPointer call_data);
static void options_cb (Widget w, XtPointer closure, XtPointer call_data);
static void helpmenu_cb (Widget w, XtPointer closure, XtPointer call_data);
static void resize_em (WidgetList children);
static void set_pixmaps (WidgetList children);
static void create_gcs (void);
static void configure_rowcol (Widget widget, XEvent *event, String *params, Cardinal *num_params);
static void update_pixmaps (void);
static void do_nothing (Widget widget, XEvent *event, String *params, Cardinal *num_params);
static void add_actions (XtAppContext app);
static int  x_error_handler (Display *dpy, XErrorEvent *event);
static void xt_error_handler (String message);
static void xt_warning_handler (String message);
static void redraw_partial_vert (Widget w);
static void redraw_partial_horz (Widget w);
static void sbh_moved (Widget w, XtPointer closure, XtPointer call_data);
static void fake_adjust_label (Widget w);
static void update_line_numbers (int l, int r);
static void sb_moved (Widget w, XtPointer closure, XtPointer call_data);
static void redraw_full (Widget w);
static void redraw_partial (Widget w, Dimension ypos, Dimension height);
static void update_screenstate (int reason, int topline);
static void redraw_both_vert (void);
static void redraw_both_horz (void);
static void next_diff (Widget w, XtPointer closure, XtPointer call_data);
static void next_diff_unselected (void);
static void prev_diff (Widget w, XtPointer closure, XtPointer call_data);
static void drag_diff (Widget w, XtPointer closure, XtPointer call_data);
static void show_version (Widget parent);
static void update_overall (void);
static void refresh (void);
static void toggle_saveas_sensitive (Boolean sensitive);
static void exit_cb (Widget w, XtPointer closure, XtPointer call_data);
static void Select (Widget widget, XEvent *event, String *params, Cardinal *num_params);
static void Scroll (Widget widget, XEvent *event, String *params, Cardinal *num_params);
static int  ilog10 (int lines);
static void handle_diff_errors (DiffInfo *d);
static void select_all (Side side);
static void unselect_all (void);
static Boolean all_selected (void);
static Dimension get_preferred_width (Widget w);
static char *basename (char *name);

#define APP_DEFAULTS_VERSION 1

/* 
 * treat failure to find the resources from the application defaults
 * file as a unrecoverable error; specify just enough fallback
 * resources to allow the user to exit the program. 
 */
static String fallbacks[] = {
    "*menubar.button_0.XmString: File",
    "*file_menu*button_4.XmString: Exit",
    NULL
};

static XrmOptionDescRec option_table[] = {
    {"-quit", "quitIfSame", XrmoptionNoArg, "true"},
    {"-args", "diffArgs", XrmoptionSepArg, NULL},
    {"-file", "filename", XrmoptionSepArg, NULL},
    {"-debug", "debug", XrmoptionNoArg, "true"}
};

static int lines_of_context;

static Pixmap bitmap;

static struct screenstate {
    Block *b;
    int topline;
    int leftcol;
    int sindex, findex[2];
} oldss, newss;

static DiffInfo *di;
static char *diffcmd;
static char *diffargs;

static int font_height = 10;
static int font_width;
static int font_descent;
static int font_mono;
static int font_widestline;

static GC gc;

GC gcfore[5];
static GC gcback[5];

static XtAppContext app;
Widget toplevel;
static Widget file_menu;
static Widget textl, textr, sb;
static Widget form1, form2, frame1, frame2, form3, form4;
static Widget fnamel, fnamer;
static Widget frame3, frame4;
static Widget sbl, dam, sbr, sbh;
static Widget form21, frame31, linenuml;
static Widget form22, frame41, linenumr;

static char *str_fnamel, *str_fnamer;
static char *str_snamel, *str_snamer;
static char *tempfname;
static char *user_filename;
char *progname;

static int linenum_columns;

static XtTranslations text_trans;

static Boolean overview_flag;
static Boolean horz_scrollbar;
static Boolean drag_scroll;
static int app_defaults_version;
static Boolean no_files_flag;
static Boolean quit_flag;
static Boolean debug_flag;

/* 
 * these track the visibility status of the two DrawingArea widgets 
 * into which the text is drawn.  If it's fully obscured, I don't 
 * bother doing any drawing.  If it's unobscured, I use a fast 
 * method of redrawing.  If it's partially obscured, I use a slow (but 
 * sure) method of redrawing.  This turns out to be faster (on my server,
 * anyway) and less complex than turning on and correctly processing
 * GraphicsExpose events.
 */
static int statel = VisibilityFullyObscured;
static int stater = VisibilityFullyObscured;

/* ARGSUSED1 */
static void Visible (Widget widget, XtPointer closure, XEvent *event, Boolean *continue_to_dispatch)
{
    XVisibilityEvent *e = (XVisibilityEvent *) event;

    if (widget == textl)
	statel = e->state;
    else if (widget == textr)
	stater = e->state;
    else			/* CONSTCOND */
	assert (False);
}

/* 
 * if the labels that display the filenames are large enough, center 
 * the filename; if not, show the end of it (likely to be the most 
 * interesting part)
 */
/* ARGSUSED1 */
static void adjust_label (Widget widget, XtPointer closure, XEvent *event, Boolean *continue_to_dispatch)
{
    if (event->type == Expose) {
	XtRemoveEventHandler (widget, ExposureMask, False, adjust_label, NULL);
	fake_adjust_label (widget);
    }
    else if (event->type == ConfigureNotify) {
	Dimension margin_width, string_width;
	unsigned char alignment;
	XmString label_string;
	XmFontList font_list;

	XtVaGetValues (widget,
		       XmNmarginWidth, &margin_width,
		       XmNalignment, &alignment,
		       XmNlabelString, &label_string,
		       XmNfontList, &font_list,
		       NULL);

	string_width = XmStringWidth (font_list, label_string);
	XmStringFree (label_string);

	if ((int) (string_width + 2 * margin_width) > event->xconfigure.width) {
	    if (alignment != XmALIGNMENT_END)
		XtVaSetValues (widget, XmNalignment, XmALIGNMENT_END, NULL);
	}
	else if (alignment != XmALIGNMENT_CENTER)
	    XtVaSetValues (widget, XmNalignment, XmALIGNMENT_CENTER, NULL);
    }
}

/* 
 * fake up a call to the above event handler to adjust the filename labels
 */
static void fake_adjust_label (Widget w)
{
    Dimension width;
    XConfigureEvent e;

    e.type = ConfigureNotify;
    XtVaGetValues (w, XmNwidth, &width, NULL);
    e.width = width;
    adjust_label (w, NULL, (XEvent *) &e, NULL);
}

/* 
 * given the position and size of a rectangle create a
 * Region structure
 */
static Region RegionFromRect (int x, int y, int w, int h)
{
    XPoint points[4];

    points[0].x = (short) x;
    points[0].y = (short) y;
    points[1].x = (short) x;
    points[1].y = (short) y + h;
    points[2].x = (short) x + w;
    points[2].y = (short) y + h;
    points[3].x = (short) x + w;
    points[3].y = (short) y;
    return (XPolygonRegion (points, 4, EvenOddRule));
}

/* 
 * handle the resizeCallbacks from the drawing areas that are used
 * to render the text panels by adjusting the scrollbars
 */
/* ARGSUSED1 */
static void resize_cb (Widget w, XtPointer closure, XtPointer call_data)
{
    Dimension width, height;
    int lines, columns, value;
    XmScrollBarCallbackStruct cbs;
    static int been_here;

    /* 
     * if graphics contexts not created yet, create them
     */
    if (!been_here) {
	XGCValues gc_values;
	XFontStruct *xfs;

	static XtResource resources[] = {
	{XtNfont, XtCFont, XtRFont, sizeof (Font),
	     XtOffset (XGCValues *, font), XtRString, "7x13bold"}};

	XtGetApplicationResources (toplevel, &gc_values, resources, XtNumber (resources), NULL, 0);

	xfs = XQueryFont (XtDisplay (w), gc_values.font);
	font_descent = xfs->descent;
	font_height = xfs->ascent + xfs->descent;
	font_width = xfs->max_bounds.width;
	font_mono = ((xfs->per_char == NULL) || (xfs->min_bounds.width == xfs->max_bounds.width));
	font_widestline = XTextWidth (xfs, di->longline, strlen (di->longline));
	XFreeFontInfo (NULL, xfs, 1);

	XtVaGetValues (w, XmNbackground, &gc_values.background, NULL);
	gc_values.graphics_exposures = False;
	gc_values.foreground = None;

	gc = XtGetGC (w, GCForeground|GCBackground|GCGraphicsExposures, &gc_values);
	been_here = 1;
    }

    XtVaGetValues (w,
		   XmNwidth, &width,
		   XmNheight, &height,
		   NULL);

    lines = (int) height / font_height;
    if (font_mono) {
	int maximum, minimum;

	XtVaGetValues (sbh, XmNmaximum, &maximum, XmNminimum, &minimum, NULL);
	columns = min ((int) width / font_width, maximum - minimum);
	XtVaSetValues (sbh,
		       XmNsliderSize, columns,
		       XmNpageIncrement, columns,
		       NULL);
    }
    else {
	XtVaSetValues (sbh,
		       XmNmaximum, font_widestline,
		       XmNincrement, font_width,
		       XmNsliderSize, width,
		       XmNpageIncrement, width,
		       NULL);
    }

    if (di->lines <= lines) {
	XtVaSetValues (sb,
		       XmNmaximum, di->lines,
		       XmNvalue, 0,
		       XmNsliderSize, di->lines,
		       XmNpageIncrement, di->lines,
		       NULL);
	newss.topline = 0;
    }
    else {
	int maximum;

	XtVaGetValues (sb,
		       XmNmaximum, &maximum,
		       XmNvalue, &value,
		       NULL);
	if ((maximum - lines) < value) {
	    XtVaSetValues (sb,
			   XmNvalue, (maximum - lines),
			   XmNsliderSize, lines,
			   XmNpageIncrement, lines,
			   NULL);
	}
	else {
	    XtVaSetValues (sb,
			   XmNsliderSize, lines,
			   XmNpageIncrement, lines,
			   NULL);
	}
    }

    XtVaGetValues (sb, XmNvalue, &value, NULL);
    cbs.reason = XmCR_DRAG;
    cbs.value = value;
    sb_moved (sb, NULL, (XtPointer) &cbs);
}

/* 
 * this is the main drawing function, called as the exposeCallback of
 * each of the text drawing areas.
 */
/* ARGSUSED1 */
static void drawit (Widget w, XtPointer closure, XtPointer call_data)
{
    XmDrawingAreaCallbackStruct *cbs = (XmDrawingAreaCallbackStruct *) call_data;
    Dimension ypos;
    int stemp, itemp, rect_height;
    Dimension width, height;
    Region region;
    Block *b;
    GC fore, back;
    int columns;

    XtVaGetValues (w, XmNwidth, &width, XmNheight, &height, NULL);
    XtAddExposureToRegion (cbs->event, region = XCreateRegion ());

    columns = (int) width / font_width + 1;

    itemp = newss.sindex;
    ypos = 0;
    for (b = newss.b; b != NULL; b = b->next) {
	int j;
	Chunk *ths, *oth;

	if (w == textl) {
	    ths = &b->arr[LEFT];
	    oth = &b->arr[RIGHT];
	    if (b->selected == LEFT) {
		fore = gcfore[4];
		back = gcback[4];
	    }
	    else {
		fore = gcfore[ths->type];
		back = gcback[ths->type];
	    }
	}
	else if (w == textr) {
	    ths = &b->arr[RIGHT];
	    oth = &b->arr[LEFT];
	    if (b->selected == RIGHT) {
		fore = gcfore[4];
		back = gcback[4];
	    }
	    else {
		fore = gcfore[ths->type];
		back = gcback[ths->type];
	    }
	}
	else			/* CONSTCOND */
	    assert (False);

	if ((rect_height = font_height * (b->ssize - itemp)) > (int) height)
	    rect_height = height;

	if (XRectInRegion (region, 0, ypos, width, rect_height) != RectangleOut) {
	    XRectangle rect;
	    Region r1, r2;

	    r1 = RegionFromRect (0, ypos, width, rect_height);
	    XIntersectRegion (region, r1, r2 = XCreateRegion ());
	    XClipBox (r2, &rect);

	    XFillRectangles (XtDisplay (w), XtWindow (w), back, &rect, 1);
	    XDestroyRegion (r1);
	    XDestroyRegion (r2);
	}

	stemp = ypos;
	assert (stemp >= 0);
	for (j = itemp; j < ths->fsize; j++) {
	    stemp += font_height;
	    assert (stemp >= 0);
	    if (ths->text != NULL) {
		if (XRectInRegion (region, 0, stemp - font_height, width, font_height) != RectangleOut) {
		    if (font_mono) {
			XDrawString (XtDisplay (w), XtWindow (w),
				     fore,
				     0, stemp - font_descent,
				     &ths->text[j][newss.leftcol],
				     min (ths->tlen[j] - newss.leftcol, columns));
		    }
		    else {
			XDrawString (XtDisplay (w), XtWindow (w),
				     fore,
				     -newss.leftcol, stemp - font_descent,
				     ths->text[j], ths->tlen[j]);
		    }
		}
	    }
	    else if (oth->text != NULL) {
		if (XRectInRegion (region, 0, stemp - font_height, width, font_height) != RectangleOut) {
		    if (font_mono) {
			XDrawString (XtDisplay (w), XtWindow (w),
				     fore,
				     0, stemp - font_descent,
				     &oth->text[j][newss.leftcol],
				     min (oth->tlen[j] - newss.leftcol, columns));
		    }
		    else {
			XDrawString (XtDisplay (w), XtWindow (w),
				     fore,
				     -newss.leftcol, stemp - font_descent,
				     oth->text[j], oth->tlen[j]);
		    }
		}
	    }
	    else		/* CONSTCOND */
		assert (False);
	}
	ypos += rect_height;

	itemp = 0;
	if (ypos > height) {
	    XDestroyRegion (region);
	    return;
	}
    }

    /* 
     * if we get here, it means that we're at the end of the display 
     * and have to clear the last fractional line (or more, if the displayed
     * text is shorter than the height of the drawing area.)
     */
    XClearArea (XtDisplay (w), XtWindow (w), 0, ypos, width, height - ypos, 0);
    XDestroyRegion (region);
}

/* 
 * called by the entries in the 'File' menu
 */
/* ARGSUSED */
static void file_cb (Widget w, XtPointer closure, XtPointer call_data)
{
    switch ((int) closure) {
    case 0:			/* open */
	toggle_open_sensitive (False);
	set_cursor (toplevel);
	open_both_files (toplevel, str_fnamel, str_fnamer);
	break;
    case 1:			/* open left */
	toggle_open_sensitive (False);
	set_cursor (toplevel);
	open_left_file (toplevel, str_fnamel);
	break;
    case 2:			/* open right */
	toggle_open_sensitive (False);
	set_cursor (toplevel);
	open_right_file (toplevel, str_fnamer);
	break;
    case 3:			/* save as */
	if (all_selected ()) {
	    set_cursor (toplevel);
	    save_file (toplevel, di->first, str_fnamel);
	}
	else {
	    werror (toplevel, "Save Error", "Save", "there are unselected text blocks");
	}
	break;
    case 4:			/* exit */
	exit_cb (w, NULL, NULL);
	break;
    default:
	assert (False);
	break;
    }
}

/* 
 * called by the entries in the 'View' menu
 */
/* ARGSUSED */
static void view_cb (Widget w, XtPointer closure, XtPointer call_data)
{
    switch ((int) closure) {
    case 0:			/* previous */
	prev_diff (NULL, NULL, NULL);
	break;
    case 1:			/* next */
	next_diff (NULL, NULL, NULL);
	break;
    case 2:			/* next unselected */
	next_diff_unselected ();
	break;
    default:
	assert (False);
	break;
    }
}

/* 
 * called by the entries in the 'Select' menu
 */
/* ARGSUSED */
static void select_cb (Widget w, XtPointer closure, XtPointer call_data)
{
    switch ((int) closure) {
    case 0:			/* left */
	select_all (LEFT);
	break;
    case 1:			/* right */
	select_all (RIGHT);
	break;
    case 2:			/* unselect */
	unselect_all ();
	break;
    default:
	assert (False);
	break;
    }
}

/* 
 * called by the entries in the 'Options' menu
 */
/* ARGSUSED */
static void options_cb (Widget w, XtPointer closure, XtPointer call_data)
{
    switch ((int) closure) {
    case 0:			/* toggle overview area */
	overview_flag = !overview_flag;
	if (overview_flag) {
	    XtManageChild (frame1);
	    XtVaSetValues (frame2,
			   XmNrightAttachment, XmATTACH_WIDGET,
			   XmNrightWidget, frame1,
			   NULL);
	}
	else {
	    XtVaSetValues (frame2, XmNrightAttachment, XmATTACH_FORM, NULL);
	    XtUnmanageChild (frame1);
	}
	break;
    case 1:			/* toggle horizontal scrollbar */
	horz_scrollbar = !horz_scrollbar;
	if (horz_scrollbar) {
	    XtManageChild (sbh);
	    XtVaSetValues (form2,
			   XmNbottomAttachment, XmATTACH_WIDGET,
			   XmNbottomWidget, sbh,
			   NULL);
	}
	else {
	    XtVaSetValues (form2, XmNbottomAttachment, XmATTACH_FORM, NULL);
	    XtUnmanageChild (sbh);
	}
	break;
    case 2:			/* toggle drag scrolling */
	drag_scroll = !drag_scroll;
	if (drag_scroll) {
	    XtAddCallback (sbl, XmNdragCallback, drag_diff, NULL);
	    XtAddCallback (sbr, XmNdragCallback, drag_diff, NULL);
	    XtAddCallback (sbh, XmNdragCallback, sbh_moved, NULL);
	    XtAddCallback (sb, XmNdragCallback, sb_moved, NULL);
	}
	else {
	    XtRemoveCallback (sbl, XmNdragCallback, drag_diff, NULL);
	    XtRemoveCallback (sbr, XmNdragCallback, drag_diff, NULL);
	    XtRemoveCallback (sbh, XmNdragCallback, sbh_moved, NULL);
	    XtRemoveCallback (sb, XmNdragCallback, sb_moved, NULL);
	}
	break;
    default:
	assert (False);
	break;
    }
}

/* 
 * called by the entries in the 'Help' menu
 */
/* ARGSUSED */
static void helpmenu_cb (Widget w, XtPointer closure, XtPointer call_data)
{
    switch ((int) closure) {
    case 0:			/* version */
	show_version (toplevel);
	break;
    case 1:			/* manual page */
	show_manual_page (toplevel);
	break;
    case 2:			/* legend */
	show_legend (toplevel);
	break;
    default:
	assert (False);
	break;
    }
}

/* 
 * set sizes of scrollbars in overview area
 */
static void resize_em (WidgetList children)
{
    Dimension height, left, right;

    XtVaGetValues (children[1], XmNheight, &height, NULL);

    left = (int) height * di->flines[LEFT] / max (di->flines[LEFT], di->flines[RIGHT]);
    right = (int) height * di->flines[RIGHT] / max (di->flines[LEFT], di->flines[RIGHT]);
	
    XtVaSetValues (children[0], XmNheight, left, NULL);
    XtVaSetValues (children[2], XmNheight, right, NULL);
}

/* 
 * the program uses two scrollbars and a drawing area to create the 
 * overview display; they are drawn on by drawing into some pixmaps 
 * and then setting the backgroundPixmap of those widgets to those
 * pixmaps.
 */
static void set_pixmaps (WidgetList children)
{
    Pixmap p[3];
    Dimension width[3], height[3];
    int depth[3];
    Block *b;
    int yfpos[2];
    int i;
    static int been_here;
    static GC dagcf, dagcb;

    if (!been_here) {
	XGCValues gc_values;

	XtVaGetValues (children[1],
		       XmNforeground, &gc_values.foreground,
		       XmNbackground, &gc_values.background,
		       NULL);
	dagcf = XtGetGC (children[1], GCForeground|GCBackground, &gc_values);
	XtVaGetValues (children[1],
		       XmNforeground, &gc_values.background,
		       XmNbackground, &gc_values.foreground,
		       NULL);
	dagcb = XtGetGC (children[1], GCForeground|GCBackground, &gc_values);
	been_here = 1;
    }

    for (i = 0; i < 3; i++) {
	XtVaGetValues (children[i],
		       XmNwidth, &width[i],
		       XmNheight, &height[i],
		       XmNdepth, &depth[i],
		       XmNbackgroundPixmap, &p[i],
		       NULL);
	if (p[i] != XmUNSPECIFIED_PIXMAP)
	    XFreePixmap (XtDisplay (children[i]), p[i]);
	p[i] = XCreatePixmap (XtDisplay (children[i]), XtWindow (children[i]),
			   width[i], height[i], depth[i]);
    }

    for (i = 0; i < 5; i++) {
	XSetClipMask (XtDisplay (toplevel), gcfore[i], None);
	XSetClipMask (XtDisplay (toplevel), gcback[i], None);
    }

    XFillRectangle (XtDisplay (children[1]), p[1], dagcb, 0, 0, width[1], height[1]);

    yfpos[LEFT] = yfpos[RIGHT] = 0;
    for (b = di->first; b != NULL; b = b->next) {
	int y, h;
	int y1, y2, y3, y4;
	int back;

	y1 = y = (int) height[0] * yfpos[LEFT] / max (di->flines[LEFT], 1);
	yfpos[LEFT] += b->arr[LEFT].fsize;
	h = ((int) height[0] * yfpos[LEFT] / max (di->flines[LEFT], 1)) - y;
	y3 = ((h == 0) ? y1 : (y1 + h - 1));
	back = (b->selected == LEFT) ? 4 : b->arr[LEFT].type;
	XFillRectangle (XtDisplay (children[0]), p[0], gcback[back],
			0, y, width[0], h);

	y2 = y = (int) height[2] * yfpos[RIGHT] / max (di->flines[RIGHT], 1);
	yfpos[RIGHT] += b->arr[RIGHT].fsize;
	h = ((int) height[2] * yfpos[RIGHT] / max (di->flines[RIGHT], 1)) - y;
	y4 = ((h == 0) ? y2 : (y2 + h - 1));
	back = (b->selected == RIGHT) ? 4 : b->arr[RIGHT].type;
	XFillRectangle (XtDisplay (children[2]), p[2], gcback[back],
			0, y, width[2], h);

	if (b->arr[LEFT].type != SAME) {
	    XDrawLine (XtDisplay (children[1]), p[1], dagcf, 0, y1, width[1] - 1, y2);
	    XDrawLine (XtDisplay (children[1]), p[1], dagcf, 0, y3, width[1] - 1, y4);
	}
    }

    XDrawLine (XtDisplay (children[1]), p[1], dagcf, 0, 0, 0, height[1]);
    XDrawLine (XtDisplay (children[1]), p[1], dagcf, width[1] - 1, 0, width[1] - 1, height[1]);

    for (i = 0; i < 3; i++)
	XtVaSetValues (children[i], XmNbackgroundPixmap, p[i], NULL);
}

/* 
 * create a bunch of the GCs that are needed
 */
static void create_gcs (void)
{
    static XtResource resources[][4] = {
    {{XtNfont, XtCFont, XtRFont, sizeof (Font),
	  XtOffset (XGCValues *, font), XtRString, "7x13bold"},
     {"diffForeground", "DiffForeground", XtRPixel, sizeof (Pixel),
	  XtOffset (XGCValues *, foreground), XtRString, "black"},
     {"diffBackground", "DiffBackground", XtRPixel, sizeof (Pixel),
	  XtOffset (XGCValues *, background), XtRString, "yellow"}},

    {{XtNfont, XtCFont, XtRFont, sizeof (Font),
	  XtOffset (XGCValues *, font), XtRString, "7x13bold"},
     {"sameForeground", "SameForeground", XtRPixel, sizeof (Pixel),
	  XtOffset (XGCValues *, foreground), XtRString, "black"},
     {"sameBackground", "SameBackground", XtRPixel, sizeof (Pixel),
	  XtOffset (XGCValues *, background), XtRString, "grey"}},

    {{XtNfont, XtCFont, XtRFont, sizeof (Font),
	  XtOffset (XGCValues *, font), XtRString, "7x13bold"},
     {"insertForeground", "InsertForeground", XtRPixel, sizeof (Pixel),
	  XtOffset (XGCValues *, foreground), XtRString, "black"},
     {"insertBackground", "InsertBackground", XtRPixel, sizeof (Pixel),
	  XtOffset (XGCValues *, background), XtRString, "orange"}},

    {{XtNfont, XtCFont, XtRFont, sizeof (Font),
	  XtOffset (XGCValues *, font), XtRString, "7x13bold"},
     {"blankForeground", "BlankForeground", XtRPixel, sizeof (Pixel),
	  XtOffset (XGCValues *, foreground), XtRString, "black"},
     {"blankBackground", "BlankBackground", XtRPixel, sizeof (Pixel),
	  XtOffset (XGCValues *, background), XtRString, "grey66"}},

    {{XtNfont, XtCFont, XtRFont, sizeof (Font),
	  XtOffset (XGCValues *, font), XtRString, "7x13bold"},
     {"selectForeground", "SelectForeground", XtRPixel, sizeof (Pixel),
	  XtOffset (XGCValues *, foreground), XtRString, "black"},
     {"selectBackground", "SelectBackground", XtRPixel, sizeof (Pixel),
	  XtOffset (XGCValues *, background), XtRString, "light slate blue"}}};

    static int been_here;

    if (!been_here) {
	int i;

	for (i = 0; i < 5; i++) {
	    XGCValues gc_values;

	    XtGetApplicationResources (toplevel, &gc_values, resources[i], XtNumber (resources[i]), NULL, 0);
	    gcfore[i] = XtGetGC (toplevel, GCForeground|GCBackground|GCFont, &gc_values);
	    gc_values.foreground = gc_values.background;
	    gcback[i] = XtGetGC (toplevel, GCForeground|GCBackground|GCFont, &gc_values);
	}
	been_here = 1;
    }
}

/* 
 * this action routine is called when the overview area has been resized
 */
/* ARGSUSED1 */
static void configure_rowcol (Widget widget, XEvent *event, String *params, Cardinal *num_params)
{
    WidgetList children;

    create_gcs ();
    XtVaGetValues (widget, XmNchildren, &children, NULL);
    resize_em (children);
    set_pixmaps (children);
}

/* 
 * this function is called when the overview area information has
 * changed and needs to be updated
 */
static void update_pixmaps (void)
{
    WidgetList children;

    XtVaGetValues (form3, XmNchildren, &children, NULL);
    set_pixmaps (children);
}

/* ARGSUSED */
static void do_nothing (Widget widget, XEvent *event, String *params, Cardinal *num_params)
{
}

/* 
 * add actions and change the translation tables of several widgets
 */
static void add_actions (XtAppContext app)
{
    XtActionsRec action[2];
    XtTranslations temp;
    /* 
     * remove much of the input behavior of the scrollbar widgets 
     * used in the overview display
     */
    static char *foo1 = "\
	~Shift Ctrl ~Meta ~Alt <Btn1Down>: DoNothing() \n\
        ~Shift Ctrl ~Meta ~Alt <Btn1Up>: DoNothing() \n\
        ~Shift ~Ctrl ~Meta ~Alt <Btn1Down>: DoNothing() \n\
        ~Shift ~Ctrl ~Meta ~Alt <Btn1Up>: DoNothing() \n\
        ~Shift ~Ctrl ~Meta ~Alt Button1<PtrMoved>: DoNothing() \n\
	<Key>osfBeginLine: DoNothing() \n\
	<Key>osfEndLine: DoNothing() \n\
	<Key>osfPageUp: DoNothing() \n\
	<Key>osfPageDown: DoNothing() \n\
	~Shift ~Ctrl <Key>osfUp: DoNothing() \n\
	~Shift ~Ctrl <Key>osfDown: DoNothing() \n\
	~Shift ~Ctrl <Key>osfLeft: DoNothing() \n\
	~Shift ~Ctrl <Key>osfRight: DoNothing()";
    /* 
     * catch changes in the size of the overview display
     */
    static char *foo2 = "\
	<Configure>: configure_rowcol() \n\
	<Map>: configure_rowcol()";
    /* 
     * add selecting and scrolling actions to the drawing areas
     * that are used to render text
     */
    static char *foo3 = "\
	<Btn1Down>: Select() \n\
	~Ctrl <Key>osfPageDown: Scroll(PageDown) \n\
	~Ctrl <Key>osfPageUp: Scroll(PageUp) \n\
	<Key>osfLeft: Scroll(Left) \n\
	<Key>osfRight: Scroll(Right) \n\
	<Key>osfDown: Scroll(Down) \n\
	<Key>osfUp: Scroll(Up) \n\
	Ctrl <Key>osfBeginLine: Scroll(BeginData) \n\
	<Key>osfBeginLine: Scroll(BeginLine) \n\
	Ctrl <Key>osfEndLine: Scroll(EndData) \n\
	<Key>osfEndLine: Scroll(EndLine) \n\
	Ctrl <Key>osfPageUp: Scroll(PageLeft) \n\
	Ctrl <Key>osfPageDown: Scroll(PageRight) \n\
	<Key>osfBeginData: Scroll(Home) \n\
	<Key>osfEndData: Scroll(End)";
    /* 
     * these last two are not always in the XKeysymDB as they should be,
     * giving rise to runtime warnings from Xt
     */

    action[0].string = "DoNothing";
    action[0].proc = do_nothing;
    XtAppAddActions (app, action, 1);
    temp = XtParseTranslationTable (foo1);
    XtOverrideTranslations (sbl, temp);
    XtOverrideTranslations (sbr, temp);

    action[0].string = "configure_rowcol";
    action[0].proc = configure_rowcol;
    XtAppAddActions (app, action, 1);
    temp = XtParseTranslationTable (foo2);
    XtOverrideTranslations (form3, temp);

    action[0].string = "Scroll";
    action[0].proc = Scroll;
    action[1].string = "Select";
    action[1].proc = Select;
    XtAppAddActions (app, action, 2);
    text_trans = XtParseTranslationTable (foo3);
}

/* 
 * cleanup temporary file at program exit
 */
static void cleanup_at_exit (void)
{
    if (tempfname != NULL)
	(void) unlink (tempfname);
}

/* 
 * provide our own error handlers that print out and then
 * abort, causing a core dump for debugging purposes.  this
 * only happens when the '-debug' flag is used.
 */
static int x_error_handler (Display *dpy, XErrorEvent *event)
{
    char etext[BUFSIZ];

    XGetErrorText (dpy, event->error_code, etext, BUFSIZ);
    (void) fprintf (stderr, "X Error: %s\n", etext);
    abort ();
    /* NOTREACHED */
}

static void xt_error_handler (String message)
{
    (void) fprintf (stderr, "Xt Error: %s\n", message);
    abort ();
    /* NOTREACHED */
}

/* 
 * filter out Xt warnings that are expected and are not the
 * result of real problems
 */
static void xt_warning_handler (String message)
{
    static char *matchtable[] = {
	"linenuml",		/* Traversal_on must always be true. */
	"linenumr",		/* Traversal_on must always be true. */
	"osfBeginData",		/* sometimes unknown as a keysym */
	"osfEndData",		/* sometimes unknown as a keysym */
	"specified slider size",
	"scrollbar page increment"
    };
    int i;

    for (i = 0; i < XtNumber (matchtable); i++)
	if (strstr (message, matchtable[i]))
	    return;

    (void) fprintf (stderr, "Xt Warning: %s\n", message);
}

int main (int argc, char *argv[])
{
    Widget mainw, menubar, widget;
    int minimum_width, minimum_height;
    int depth;			/* depth of top level shell */

    static XtResource resources[] = {
    {"diffCommand", "DiffCommand", XtRString, sizeof (String), 0, XtRString, "diff"},
    {"diffArgs", "DiffArgs", XtRString, sizeof (String), 0, XtRString, ""},
    {"dragScroll", "DragScroll", XtRBoolean, sizeof (Boolean), 0, XtRString, "true"},
    {"horzScrollbar", "HorzScrollbar", XtRBoolean, sizeof (Boolean), 0, XtRString, "true"},
    {"linesOfContext", "LinesOfContext", XtRInt, sizeof (int), 0, XtRString, "3"},
    {"overview", "Overview", XtRBoolean, sizeof (Boolean), 0, XtRString, "true"},
    {"appDefaultsVersion", "AppDefaultsVersion", XtRInt, sizeof (int), 0, XtRString, "0"},
    {"quitIfSame", "QuitIfSame", XtRBoolean, sizeof (Boolean), 0, XtRString, "false"},
    {"debug", "Debug", XtRBoolean, sizeof (Boolean), 0, XtRString, "false"},
    {"filename", "Filename", XtRString, sizeof (String), 0, XtRString, ""}};
    
    progname = basename (argv[0]);

    toplevel = XtVaAppInitialize (&app, "Mgdiff", option_table, XtNumber (option_table),
#if X11R5
				  &argc,
#else
				  (unsigned int *) &argc,
#endif
				  argv, fallbacks, NULL);

    XtVaGetValues (toplevel, XmNdepth, &depth, NULL);
    if (depth == 1)
	XtAppError (app, "monochrome displays are not supported");

    add_editres (toplevel);

    XmAddWMProtocolCallback (toplevel,
			     XmInternAtom (XtDisplay (toplevel), "WM_DELETE_WINDOW", False),
			     exit_cb,
			     NULL);

    bitmap = XCreatePixmapFromBitmapData (XtDisplay (toplevel),
					  RootWindowOfScreen (XtScreen (toplevel)),
					  (char *) mgdiff_bits,
					  mgdiff_width, mgdiff_height,
					  BlackPixelOfScreen (XtScreen (toplevel)),
					  WhitePixelOfScreen (XtScreen (toplevel)),
					  1);
    XtVaSetValues (toplevel, XmNiconPixmap, bitmap, NULL);

    XtGetApplicationResources (toplevel, &diffcmd, &resources[0], 1, NULL, 0);
    XtGetApplicationResources (toplevel, &diffargs, &resources[1], 1, NULL, 0);
    XtGetApplicationResources (toplevel, &drag_scroll, &resources[2], 1, NULL, 0);
    XtGetApplicationResources (toplevel, &horz_scrollbar, &resources[3], 1, NULL, 0);
    XtGetApplicationResources (toplevel, &lines_of_context, &resources[4], 1, NULL, 0);
    if (lines_of_context < 0) {
        lines_of_context = 0;	/* print out warning */
	XtAppWarning (app, "Illegal value specified for \"linesOfContext\" resource");
    }
    XtGetApplicationResources (toplevel, &overview_flag, &resources[5], 1, NULL, 0);
    XtGetApplicationResources (toplevel, &app_defaults_version, &resources[6], 1, NULL, 0);
    XtGetApplicationResources (toplevel, &quit_flag, &resources[7], 1, NULL, 0);
    XtGetApplicationResources (toplevel, &debug_flag, &resources[8], 1, NULL, 0);
    XtGetApplicationResources (toplevel, &user_filename, &resources[9], 1, NULL, 0);

    if (app_defaults_version == 0) {
	static String array[] = {
	    "The application defaults file for mgdiff (named \"Mgdiff\")",
	    "was not found and so the program will not work correctly.",
	    "Exit the program and see the RESOURCES section of the",
	    "X(1) manual page for information on how to install it."
	};
	werror_long (toplevel, "No Application Defaults", array, sizeof (array) / sizeof (array[0]));
    }
    else if (app_defaults_version != APP_DEFAULTS_VERSION) {
	static String array[] = {
	    "The application defaults file for mgdiff (named \"Mgdiff\") is",
	    "the wrong version and so the program may not work correctly.",
	};
	werror_long (toplevel, "Wrong Application Defaults", array, sizeof (array) / sizeof (array[0]));
    }

    if (debug_flag) {
	XSetErrorHandler (x_error_handler);
	XtAppSetErrorHandler (app, xt_error_handler);
    }
    else
	XtAppSetWarningHandler (app, xt_warning_handler);

    (void) atexit (cleanup_at_exit);

    switch (argc) {

	/* 
	 * no filenames on command line; start up empty
	 */
    case 1:
	no_files_flag = True;
	di = blank_diff_info ();
	str_snamel = strdup ("(no file)");
	str_snamer = strdup ("(no file)");
	break;

	/* 
	 * two filenames on command line; process them
	 */
    case 3:
	if (strcmp (argv[1], "-") == 0) {
	    tempfname = tempnam (NULL, "mgdif");
	    str_fnamel = strdup (tempfname);
	    str_snamel = strdup (user_filename);
	    if (!copy_to_file (stdin, tempfname)) {
		(void) fprintf (stderr, "Error copying stdin to temp file \"%s\"\n", tempfname);
		exit (2);
	    }
	}
	else if (!file_tests (toplevel, argv[1])) {
	    no_files_flag = True;
	    di = blank_diff_info ();
	    str_snamel = strdup ("(no file)");
	    str_snamer = strdup ("(no file)");
	    break;
	}
	else {
	    str_snamel = strdup (argv[1]);
	    str_fnamel = strdup (argv[1]);
	}

	if (strcmp (argv[2], "-") == 0) {
	    tempfname = tempnam (NULL, "mgdif");
	    str_fnamer = strdup (tempfname);
	    str_snamer = strdup (user_filename);
	    if (!copy_to_file (stdin, tempfname)) {
		(void) fprintf (stderr, "Error copying stdin to temp file \"%s\"\n", tempfname);
		exit (2);
	    }
	}
	else if (!file_tests (toplevel, argv[2])) {
	    no_files_flag = True;
	    di = blank_diff_info ();
	    str_snamel = strdup ("(no file)");
	    str_snamer = strdup ("(no file)");
	    break;
	}
	else {
	    str_snamer = strdup (argv[2]);
	    str_fnamer = strdup (argv[2]);
	}

	di = build_diff_info (diffcmd, diffargs, str_fnamel, str_fnamer);
	handle_diff_errors (di);

	if ((di->status == 0) && quit_flag) {
	    exit (0);
	}
	else if (di->status == 2) {
	    str_fnamel = str_fnamer = NULL;
	    str_snamel = strdup ("(no file)");
	    str_snamer = strdup ("(no file)");
	}
	break;

	/* 
	 * anything else is a usage error
	 */
    default:
	(void) fprintf (stderr, "Usage: %s [-toolkitoption ...] [-quit] [-file name] [-args diffargs] [file1 file2]\n", progname);
	exit (2);
	break;
    }

    newss.b = di->first;
    newss.topline = newss.sindex = newss.findex[LEFT] = newss.findex[RIGHT] = 0;

    mainw = XtVaCreateManagedWidget ("mainw", xmMainWindowWidgetClass,
				     toplevel,
				     NULL);

    /* 
     * create menu bar
     */
    menubar = XmVaCreateSimpleMenuBar (mainw, "menubar",
				       XmVaCASCADEBUTTON, NULL, NULL,
				       XmVaCASCADEBUTTON, NULL, NULL,
				       XmVaCASCADEBUTTON, NULL, NULL,
				       XmVaCASCADEBUTTON, NULL, NULL,
				       XmVaCASCADEBUTTON, NULL, NULL,
				       NULL);

    /* 
     * make the minimum permissible width of the window equal to the 
     * minimum width of the menubar
     */
    minimum_width = (int) get_preferred_width (menubar);
    minimum_height = (minimum_width * 3) / 4;

    file_menu = XmVaCreateSimplePulldownMenu (menubar, "file_menu", 0, file_cb,
				  XmVaPUSHBUTTON, NULL, NULL, NULL, NULL,
				  XmVaPUSHBUTTON, NULL, NULL, NULL, NULL,
				  XmVaPUSHBUTTON, NULL, NULL, NULL, NULL,
				  XmVaPUSHBUTTON, NULL, NULL, NULL, NULL,
				  XmVaSEPARATOR,
				  XmVaPUSHBUTTON, NULL, NULL, NULL, NULL,
				  NULL);
    if (no_files_flag || (di->status == 2)) {
	toggle_openlr_sensitive (False);
	toggle_saveas_sensitive (False);
    }
    XmVaCreateSimplePulldownMenu (menubar, "view_menu", 1, view_cb,
				  XmVaPUSHBUTTON, NULL, NULL, NULL, NULL,
				  XmVaPUSHBUTTON, NULL, NULL, NULL, NULL,
				  XmVaPUSHBUTTON, NULL, NULL, NULL, NULL,
				  NULL);
    XmVaCreateSimplePulldownMenu (menubar, "select_menu", 2, select_cb,
				  XmVaPUSHBUTTON, NULL, NULL, NULL, NULL,
				  XmVaPUSHBUTTON, NULL, NULL, NULL, NULL,
				  XmVaSEPARATOR,
				  XmVaPUSHBUTTON, NULL, NULL, NULL, NULL,
				  NULL);
    widget = XmVaCreateSimplePulldownMenu (menubar, "options_menu", 3, options_cb,
					   XmVaTOGGLEBUTTON, NULL, NULL, NULL, NULL,
					   XmVaTOGGLEBUTTON, NULL, NULL, NULL, NULL,
					   XmVaTOGGLEBUTTON, NULL, NULL, NULL, NULL,
					   NULL);
    XmVaCreateSimplePulldownMenu (menubar, "help_menu", 4, helpmenu_cb,
				  XmVaPUSHBUTTON, NULL, NULL, NULL, NULL,
				  XmVaPUSHBUTTON, NULL, NULL, NULL, NULL,
				  XmVaPUSHBUTTON, NULL, NULL, NULL, NULL,
				  NULL);

    XtVaSetValues (menubar,
		   XmNmenuHelpWidget, XtNameToWidget (menubar, "button_4"),
		   NULL);

    XmToggleButtonSetState (XtNameToWidget (widget, "button_0"), overview_flag, False);
    XmToggleButtonSetState (XtNameToWidget (widget, "button_1"), horz_scrollbar, False);
    XmToggleButtonSetState (XtNameToWidget (widget, "button_2"), drag_scroll, False);

    XtManageChild (menubar);

    form1 = XtVaCreateManagedWidget ("form1", xmFormWidgetClass, mainw,
				     XmNhorizontalSpacing, 1,
				     XmNverticalSpacing, 1,
				     NULL);
    XtVaSetValues (mainw,
		   XmNmenuBar, menubar,
		   XmNworkWindow, form1,
		   NULL);


    frame1 = XtVaCreateWidget ("frame1", xmFrameWidgetClass, form1,
				      XmNleftAttachment, XmATTACH_NONE,
				      XmNrightAttachment, XmATTACH_FORM,
				      XmNbottomAttachment, XmATTACH_FORM,
				      XmNtopAttachment, XmATTACH_FORM,
				      NULL);
    if (overview_flag)
	XtManageChild (frame1);

    form3 = XtVaCreateManagedWidget ("form3", xmFormWidgetClass, frame1,
				     XmNhorizontalSpacing, 0,
				     NULL);

    sbl = XtVaCreateManagedWidget ("sbl", xmScrollBarWidgetClass, form3,
				   XmNmaximum, max (1, di->flines[LEFT]),
				   XmNpageIncrement, 1,
				   XmNleftAttachment, XmATTACH_FORM,
				   XmNrightAttachment, XmATTACH_WIDGET,
				   XmNbottomAttachment, XmATTACH_NONE,
				   XmNtopAttachment, XmATTACH_FORM,
				   XmNshowArrows, False,
				   XmNsliderSize, 1,
				   XmNshadowThickness, 0,
				   NULL);
    XtAddCallback (sbl, XmNpageIncrementCallback, next_diff, NULL);
    XtAddCallback (sbl, XmNpageDecrementCallback, prev_diff, NULL);
    XtAddCallback (sbl, XmNvalueChangedCallback, drag_diff, NULL);
    if (drag_scroll)
	XtAddCallback (sbl, XmNdragCallback, drag_diff, NULL);

    dam = XtVaCreateManagedWidget ("dam", xmDrawingAreaWidgetClass, form3,
				   XmNtraversalOn, False,
				   XmNleftAttachment, XmATTACH_NONE,
				   XmNrightAttachment, XmATTACH_WIDGET,
				   XmNbottomAttachment, XmATTACH_FORM,
				   XmNtopAttachment, XmATTACH_FORM,
				   NULL);
    sbr = XtVaCreateManagedWidget ("sbr", xmScrollBarWidgetClass, form3,
				   XmNmaximum, max (1, di->flines[RIGHT]),
				   XmNpageIncrement, 1,
				   XmNleftAttachment, XmATTACH_NONE,
				   XmNrightAttachment, XmATTACH_FORM,
				   XmNbottomAttachment, XmATTACH_NONE,
				   XmNtopAttachment, XmATTACH_FORM,
				   XmNshowArrows, False,
				   XmNsliderSize, 1,
				   XmNshadowThickness, 0,
				   NULL);
    add_actions (app);

    XtAddCallback (sbr, XmNpageIncrementCallback, next_diff, NULL);
    XtAddCallback (sbr, XmNpageDecrementCallback, prev_diff, NULL);
    XtAddCallback (sbr, XmNvalueChangedCallback, drag_diff, NULL);
    if (drag_scroll)
	XtAddCallback (sbr, XmNdragCallback, drag_diff, NULL);

    frame2 = XtVaCreateManagedWidget ("frame2", xmFrameWidgetClass, form1,
				      XmNleftAttachment, XmATTACH_FORM,
				      XmNbottomAttachment, XmATTACH_FORM,
				      XmNtopAttachment, XmATTACH_FORM,
				      NULL);
    form4 = XtVaCreateManagedWidget ("form4", xmFormWidgetClass, frame2, NULL);
    form2 = XtVaCreateManagedWidget ("form2", xmFormWidgetClass, form4,
				     XmNhorizontalSpacing, 1,
				     XmNverticalSpacing, 1,
				     XmNleftAttachment, XmATTACH_FORM,
				     XmNrightAttachment, XmATTACH_FORM,
				     XmNtopAttachment, XmATTACH_FORM,
				     NULL);

    sbh = XtVaCreateWidget ("sbh", xmScrollBarWidgetClass, form4,
				   XmNmaximum, di->maxcols,
				   XmNsliderSize, 1,
				   XmNorientation, XmHORIZONTAL,
				   XmNleftAttachment, XmATTACH_FORM,
				   XmNrightAttachment, XmATTACH_FORM,
				   XmNbottomAttachment, XmATTACH_FORM,
				   XmNtopAttachment, XmATTACH_NONE,
				   XmNheight, 20,
				   NULL);
    XtAddCallback (sbh, XmNvalueChangedCallback, sbh_moved, NULL);
    if (drag_scroll)
	XtAddCallback (sbh, XmNdragCallback, sbh_moved, NULL);
    if (horz_scrollbar)
	XtManageChild (sbh);

    sb = XtVaCreateManagedWidget ("sb", xmScrollBarWidgetClass, form2,
				  XmNmaximum, di->lines,
				  XmNleftAttachment, XmATTACH_POSITION,
				  XmNleftOffset, -10, /* -1/2 of width */
				  XmNrightAttachment, XmATTACH_NONE,
				  XmNbottomAttachment, XmATTACH_FORM,
				  XmNtopAttachment, XmATTACH_FORM,
				  XmNleftPosition, 50,
				  XmNwidth, 20,
				  NULL);
    if (drag_scroll)
	XtAddCallback (sb, XmNdragCallback, sb_moved, NULL);
    XtAddCallback (sb, XmNvalueChangedCallback, sb_moved, NULL);
    XtAddCallback (sb, XmNincrementCallback, sb_moved, NULL);
    XtAddCallback (sb, XmNdecrementCallback, sb_moved, NULL);
    XtAddCallback (sb, XmNpageIncrementCallback, sb_moved, NULL);
    XtAddCallback (sb, XmNpageDecrementCallback, sb_moved, NULL);
    XtAddCallback (sb, XmNtoTopCallback, sb_moved, NULL);
    XtAddCallback (sb, XmNtoBottomCallback, sb_moved, NULL);

    form21 = XtVaCreateManagedWidget ("form21", xmFormWidgetClass, form2,
				      XmNleftAttachment, XmATTACH_FORM,
				      XmNrightAttachment, XmATTACH_WIDGET,
				      XmNbottomAttachment, XmATTACH_NONE,
				      XmNtopAttachment, XmATTACH_FORM,
				      NULL);

    frame3 = XtVaCreateManagedWidget ("frame3", xmFrameWidgetClass, form21,
				      XmNleftAttachment, XmATTACH_FORM,
				      XmNrightAttachment, XmATTACH_WIDGET,
				      XmNbottomAttachment, XmATTACH_FORM,
				      XmNtopAttachment, XmATTACH_FORM,
				      NULL);
    fnamel = XtVaCreateManagedWidget ("fnamel", xmLabelWidgetClass, frame3,
				      XmNlabelString, XmStringCreateSimple (str_snamel),
				      NULL);

    linenum_columns = ilog10 (max (di->flines[LEFT], di->flines[RIGHT]));

    frame31 = XtVaCreateManagedWidget ("frame31", xmFrameWidgetClass, form21,
				       XmNleftAttachment, XmATTACH_NONE,
				       XmNrightAttachment, XmATTACH_FORM,
				       XmNbottomAttachment, XmATTACH_FORM,
				       XmNtopAttachment, XmATTACH_FORM,
				       NULL);
    linenuml = XtVaCreateManagedWidget ("linenuml", xmTextFieldWidgetClass, frame31,
					XmNcolumns, linenum_columns,
					XmNsensitive, True,
					XmNtraversalOn, False,
					XmNeditable, False,
					XmNhighlightThickness, 0,
					XmNmarginHeight, 2,
					XmNshadowThickness, 0,
					XmNcursorPositionVisible, False,
					NULL);
    XtVaSetValues (frame3, XmNrightWidget, frame31, NULL);


    form22 = XtVaCreateManagedWidget ("form22", xmFormWidgetClass, form2,
				      XmNleftAttachment, XmATTACH_WIDGET,
				      XmNrightAttachment, XmATTACH_FORM,
				      XmNbottomAttachment, XmATTACH_NONE,
				      XmNtopAttachment, XmATTACH_FORM,
				      NULL);

    frame4 = XtVaCreateManagedWidget ("frame4", xmFrameWidgetClass, form22,
				      XmNleftAttachment, XmATTACH_FORM,
				      XmNrightAttachment, XmATTACH_WIDGET,
				      XmNbottomAttachment, XmATTACH_FORM,
				      XmNtopAttachment, XmATTACH_FORM,
				      NULL);
    fnamer = XtVaCreateManagedWidget ("fnamer", xmLabelWidgetClass, frame4,
				      XmNlabelString, XmStringCreateSimple (str_snamer),
				      NULL);

    frame41 = XtVaCreateManagedWidget ("frame41", xmFrameWidgetClass, form22,
				       XmNleftAttachment, XmATTACH_NONE,
				       XmNrightAttachment, XmATTACH_FORM,
				       XmNbottomAttachment, XmATTACH_FORM,
				       XmNtopAttachment, XmATTACH_FORM,
				       NULL);
    linenumr = XtVaCreateManagedWidget ("linenumr", xmTextFieldWidgetClass, frame41,
					XmNcolumns, linenum_columns,
					XmNsensitive, True,
					XmNeditable, False,
					XmNtraversalOn, False,
					XmNhighlightThickness, 0,
					XmNmarginHeight, 2,
					XmNshadowThickness, 0,
					XmNcursorPositionVisible, False,
					NULL);
    XtVaSetValues (frame4, XmNrightWidget, frame41, NULL);

    update_line_numbers (1, 1);

    textl = XtVaCreateManagedWidget ("text", xmDrawingAreaWidgetClass, form2,
				     XmNtraversalOn, False,
				     XmNleftAttachment, XmATTACH_FORM,
				     XmNrightAttachment, XmATTACH_WIDGET,
				     XmNbottomAttachment, XmATTACH_FORM,
				     XmNtopAttachment, XmATTACH_WIDGET,
				     NULL);
    XtOverrideTranslations (textl, text_trans);
    XtAddCallback (textl, XmNexposeCallback, drawit, NULL);

    textr = XtVaCreateManagedWidget ("text", xmDrawingAreaWidgetClass, form2,
				     XmNtraversalOn, False,
				     XmNleftAttachment, XmATTACH_WIDGET,
				     XmNrightAttachment, XmATTACH_FORM,
				     XmNbottomAttachment, XmATTACH_FORM,
				     XmNtopAttachment, XmATTACH_WIDGET,
				     NULL);
    XtOverrideTranslations (textr, text_trans);
    XtAddCallback (textr, XmNexposeCallback, drawit, NULL);
    XtAddCallback (textr, XmNresizeCallback, resize_cb, NULL);

    XtVaSetValues (sbl, XmNrightWidget, dam, NULL);
    XtVaSetValues (dam, XmNrightWidget, sbr, NULL);
    XtVaSetValues (form21, XmNrightWidget, sb, NULL);
    if (overview_flag)
	XtVaSetValues (frame2,
		       XmNrightAttachment, XmATTACH_WIDGET,
		       XmNrightWidget, frame1,
		       NULL);
    else
	XtVaSetValues (frame2,
		       XmNrightAttachment, XmATTACH_FORM,
		       NULL);
    XtVaSetValues (form22, XmNleftWidget, sb, NULL);
    XtVaSetValues (textl, XmNtopWidget, form21, XmNrightWidget, sb, NULL);
    XtVaSetValues (textr, XmNtopWidget, form22, XmNleftWidget, sb, NULL);
    if (horz_scrollbar)
	XtVaSetValues (form2,
		       XmNbottomAttachment, XmATTACH_WIDGET,
		       XmNbottomWidget, sbh,
		       NULL);
    else
	XtVaSetValues (form2, XmNbottomAttachment, XmATTACH_FORM, NULL);

    XtRealizeWidget (toplevel);

    XtVaSetValues (toplevel, XmNminWidth, minimum_width, XmNminHeight, minimum_height, NULL);

    XtAddEventHandler (textl, VisibilityChangeMask, False, Visible, NULL);
    XtAddEventHandler (textr, VisibilityChangeMask, False, Visible, NULL);

    XtAddEventHandler (fnamel, StructureNotifyMask, False, adjust_label, NULL);
    XtAddEventHandler (fnamer, StructureNotifyMask, False, adjust_label, NULL);
    XtAddEventHandler (fnamel, ExposureMask, False, adjust_label, NULL);
    XtAddEventHandler (fnamer, ExposureMask, False, adjust_label, NULL);

    XtAppMainLoop (app);
    /* NOTREACHED */
}

static void redraw_partial_vert (Widget w)
{
    Dimension xsrc, ysrc, xdest, ydest;
    Dimension width, height;
    XExposeEvent e;
    XmDrawingAreaCallbackStruct dacbs;

    XtVaGetValues (w, XmNwidth, &width, XmNheight, &height, NULL);

    dacbs.event = (XEvent *) &e;
    e.type = Expose;
    e.send_event = False;
    e.display = XtDisplay (w);
    e.window = XtWindow (w);
    e.count = 0;
    e.x = 0;
    e.width = width;

    xsrc = xdest = 0;
   
    if (newss.topline < oldss.topline) { /* scrolling down */
	ysrc = 0;
	ydest = (oldss.topline - newss.topline) * font_height;
	e.y = 0;
	if (ydest < height) {
	    XCopyArea (XtDisplay (w), XtWindow (w), 
		       XtWindow (w), gc,
		       xsrc, ysrc, width, height - ysrc,
		       xdest, ydest);
	    e.height = ydest;
	}
	else {
	    e.height = height;
	}
    }
    else {			/* scrolling up */
	ydest = 0;
	ysrc = (newss.topline - oldss.topline) * font_height;
	if (ysrc < height) {
	    XCopyArea (XtDisplay (w), XtWindow (w), 
		       XtWindow (w), gc,
		       xsrc, ysrc, width, height - ysrc,
		       xdest, ydest);
	    e.y = height - ysrc;
	    e.height = ysrc;
	}
	else {
	    e.y = 0;
	    e.height = height;
	}
    }

    XtCallCallbacks (w, XmNexposeCallback, &dacbs);
}

static void redraw_partial_horz (Widget w)
{
    Dimension xsrc, ysrc, xdest, ydest;
    Dimension width, height;
    XExposeEvent e;
    XmDrawingAreaCallbackStruct dacbs;

    XtVaGetValues (w, XmNwidth, &width, XmNheight, &height, NULL);

    dacbs.event = (XEvent *) &e;
    e.type = Expose;
    e.send_event = False;
    e.display = XtDisplay (w);
    e.window = XtWindow (w);
    e.count = 0;
    e.y = 0;
    e.height = height;

    ysrc = ydest = 0;
   
    if (newss.leftcol < oldss.leftcol) { /* scrolling right */
	xsrc = 0;
	xdest = (oldss.leftcol - newss.leftcol) * (font_mono ? font_width : 1);
	e.x = 0;
	if (xdest < width) {
	    XCopyArea (XtDisplay (w), XtWindow (w), 
		       XtWindow (w), gc,
		       xsrc, ysrc, width - xsrc, height,
		       xdest, ydest);
	    e.width = xdest;
	}
	else {
	    e.width = width;
	}
    }
    else {			/* scrolling left */
	xdest = 0;
	xsrc = (newss.leftcol - oldss.leftcol) *  (font_mono ? font_width : 1);
	if (xsrc < width) {
	    XCopyArea (XtDisplay (w), XtWindow (w), 
		       XtWindow (w), gc,
		       xsrc, ysrc, width - xsrc, height,
		       xdest, ydest);
	    e.x = width - xsrc;
	    e.width = xsrc;
	}
	else {
	    e.x = 0;
	    e.width = width;
	}
    }

    XtCallCallbacks (w, XmNexposeCallback, &dacbs);
}

/* ARGSUSED */
static void sbh_moved (Widget w, XtPointer closure, XtPointer call_data)
{
    XmScrollBarCallbackStruct *cbs = (XmScrollBarCallbackStruct *) call_data;

    if (cbs->value != newss.leftcol) {
	oldss = newss;
	newss.leftcol = cbs->value;
	redraw_both_horz ();
    }
}

/* 
 * the XmTextFieldSetString calls may leak memory according to Purify
 */
static void update_line_numbers (int l, int r)
{
    char buffer[16];

    (void) sprintf (buffer, "%*d", linenum_columns, l);
    XmTextFieldSetString (linenuml, buffer);
    (void) sprintf (buffer, "%*d", linenum_columns, r);
    XmTextFieldSetString (linenumr, buffer);
}

/* ARGSUSED */
static void sb_moved (Widget w, XtPointer closure, XtPointer call_data)
{
    XmScrollBarCallbackStruct *cbs = (XmScrollBarCallbackStruct *) call_data;

    switch (cbs->reason) {
    case XmCR_INCREMENT:
    case XmCR_DECREMENT:
    case XmCR_PAGE_INCREMENT:
    case XmCR_PAGE_DECREMENT:
    case XmCR_TO_TOP:
    case XmCR_TO_BOTTOM:
	update_screenstate (cbs->reason, cbs->value);
	break;
    case XmCR_VALUE_CHANGED:
	if (cbs->value < newss.topline) 
	    update_screenstate (XmCR_PAGE_DECREMENT, cbs->value);
	else if (cbs->value > newss.topline) 
	    update_screenstate (XmCR_PAGE_INCREMENT, cbs->value);
	else {
	    update_line_numbers (newss.findex[LEFT] + 1, newss.findex[RIGHT] + 1);
	    return;
	}
	break;
    case XmCR_DRAG:
	if (cbs->value < newss.topline) 
	    update_screenstate (XmCR_PAGE_DECREMENT, cbs->value);
	else if (cbs->value > newss.topline) 
	    update_screenstate (XmCR_PAGE_INCREMENT, cbs->value);
	else
	    return;
	break;
    default:			/* CONSTCOND */
	assert (False);
	break;
    }
    update_overall ();

    update_line_numbers (newss.findex[LEFT] + 1, newss.findex[RIGHT] + 1);

    redraw_both_vert ();
}

/* 
 * redraw an entire DrawingArea widget by synthesizing an Expose event 
 * and invoking the widget's expose callbacks
 */
static void redraw_full (Widget w)
{
    Dimension width, height;
    XExposeEvent e;
    XmDrawingAreaCallbackStruct dacbs;

    XtVaGetValues (w, XmNwidth, &width, XmNheight, &height, NULL);
    dacbs.event = (XEvent *) &e;
    e.type = Expose;
    e.send_event = False;
    e.display = XtDisplay (w);
    e.count = e.x = e.y = 0;
    e.width = width;
    e.height = height;
    e.window = XtWindow (w);
    XtCallCallbacks (w, XmNexposeCallback, &dacbs);
}

/* 
 * redraw part of a DrawingArea widget by synthesizing an Expose event 
 * and invoking the widget's expose callbacks
 */
static void redraw_partial (Widget w, Dimension ypos, Dimension height)
{
    Dimension width;
    XExposeEvent e;
    XmDrawingAreaCallbackStruct dacbs;

    XtVaGetValues (w, XmNwidth, &width, NULL);
    dacbs.event = (XEvent *) &e;
    e.type = Expose;
    e.send_event = False;
    e.display = XtDisplay (w);
    e.count = e.x = 0;
    e.y = ypos;
    e.width = width;
    e.height = height;
    e.window = XtWindow (w);
    XtCallCallbacks (w, XmNexposeCallback, &dacbs);
}

static void update_screenstate (int reason, int topline)
{
    Block *b;

    if (topline == newss.topline)
	return;

    switch (reason) {
    case XmCR_INCREMENT:
	oldss = newss;
	newss.topline += 1;
	if (newss.sindex >= (newss.b->ssize - 1)) {
	    newss.b = newss.b->next;
	    newss.sindex = 0;
	}
	else
	    newss.sindex += 1;
	break;
    case XmCR_DECREMENT:
	oldss = newss;
	newss.topline -= 1;
	if (newss.sindex == 0) {
	    newss.b = newss.b->prev;
	    newss.sindex = newss.b->ssize - 1;
	}
	else
	    newss.sindex -= 1;
	break;
    case XmCR_PAGE_INCREMENT:
	oldss = newss;
	newss.topline = topline;
	for (b = newss.b; b->next != NULL; b = b->next) {
	    if ((newss.topline >= b->sline) &&
		(newss.topline < (b->sline + b->ssize)))
		break;
	}
	newss.b = b;
	newss.sindex = newss.topline - newss.b->sline;
	break;
    case XmCR_PAGE_DECREMENT:
	oldss = newss;
	newss.topline = topline;
	for (b = newss.b; b->prev != NULL; b = b->prev) {
	    if ((newss.topline >= b->sline) &&
		(newss.topline < (b->sline + b->ssize)))
		break;
	}
	newss.b = b;
	newss.sindex = newss.topline - newss.b->sline;
	break;
    case XmCR_TO_TOP:
	oldss = newss;
	newss.b = di->first;
	newss.topline = 0;
	newss.sindex = 0;
	break;
    case XmCR_TO_BOTTOM:
	oldss = newss;
	newss.topline = topline;
	for (b = di->last; b->prev != NULL; b = b->prev) {
	    if ((newss.topline >= b->sline) &&
		(newss.topline < (b->sline + b->ssize)))
		break;
	}
	newss.b = b;
	newss.sindex = newss.topline - newss.b->sline;
	break;
    default:
	assert (False);
	break;
    }
}

static void redraw_both_vert (void)
{
    switch (statel) {
    case VisibilityUnobscured:
	redraw_partial_vert (textl);
	break;
    case VisibilityPartiallyObscured:
	redraw_full (textl);
	break;
    case VisibilityFullyObscured:
	break;
    default:
	assert (False);
	break;
    }

    switch (stater) {
    case VisibilityUnobscured:
	redraw_partial_vert (textr);
	break;
    case VisibilityPartiallyObscured:
	redraw_full (textr);
	break;
    case VisibilityFullyObscured:
	break;
    default:
	assert (False);
	break;
    }
}

static void redraw_both_horz (void)
{
    switch (statel) {
    case VisibilityUnobscured:
	redraw_partial_horz (textl);
	break;
    case VisibilityPartiallyObscured:
	redraw_full (textl);
	break;
    case VisibilityFullyObscured:
	break;
    default:
	assert (False);
	break;
    }

    switch (stater) {
    case VisibilityUnobscured:
	redraw_partial_horz (textr);
	break;
    case VisibilityPartiallyObscured:
	redraw_full (textr);
	break;
    case VisibilityFullyObscured:
	break;
    }
}

/* ARGSUSED */
static void next_diff (Widget w, XtPointer closure, XtPointer call_data)
{
    Block *b;

    for (b = newss.b->next; b != NULL; b = b->next)
	if ((b->arr[LEFT].type != SAME) && ((newss.topline + lines_of_context) < b->sline)) {
	    int value, maximum, slidersize;
	    XmScrollBarCallbackStruct newcbs;

	    value = (b->sline >= lines_of_context) ? (b->sline - lines_of_context) : b->sline;
	    XtVaGetValues (sb, XmNmaximum, &maximum, XmNsliderSize, &slidersize, NULL);
	    if (value > (maximum - slidersize))
		value = maximum - slidersize;

	    if ((w == sbl) || (w == sbr)) {
		int side = (w == sbl) ? LEFT : RIGHT;

		if (newcbs.value > (maximum - slidersize)) {
		    newcbs.value = maximum - slidersize;
		    XtVaSetValues (w, XmNvalue, newcbs.value - b->sline + b->arr[side].fline, NULL);
		    return;
		}
	    }

	    newcbs.reason = XmCR_VALUE_CHANGED;
	    newcbs.event = NULL;
	    newcbs.value = value;
	    XtVaSetValues (sb, XmNvalue, value, NULL);
	    XtCallCallbacks (sb, XmNvalueChangedCallback, &newcbs);
	    return;
	}
}

static void next_diff_unselected (void)
{
    Block *b;

    for (b = newss.b->next; b != NULL; b = b->next)
	if ((b->selected == NEITHER) && (b->arr[LEFT].type != SAME) && ((newss.topline + lines_of_context) < b->sline)) {
	    int value, maximum, slidersize;
	    XmScrollBarCallbackStruct newcbs;

	    value = (b->sline >= lines_of_context) ? (b->sline - lines_of_context) : b->sline;
	    XtVaGetValues (sb, XmNmaximum, &maximum, XmNsliderSize, &slidersize, NULL);
	    if (value > (maximum - slidersize))
		value = maximum - slidersize;
	    newcbs.reason = XmCR_VALUE_CHANGED;
	    newcbs.event = NULL;
	    newcbs.value = value;
	    XtVaSetValues (sb, XmNvalue, value, NULL);
	    XtCallCallbacks (sb, XmNvalueChangedCallback, &newcbs);
	    return;
	}
}

/* ARGSUSED */
static void prev_diff (Widget w, XtPointer closure, XtPointer call_data)
{
    Block *b;

    for (b = newss.b->prev; b != NULL; b = b->prev)
	if ((b->arr[LEFT].type != SAME) && ((newss.topline + lines_of_context) >= b->sline)) {
	    int value;
	    XmScrollBarCallbackStruct newcbs;

	    value = (b->sline >= lines_of_context) ? (b->sline - lines_of_context) : 0;
	    newcbs.reason = XmCR_VALUE_CHANGED;
	    newcbs.event = NULL;
	    newcbs.value = value;
	    XtVaSetValues (sb, XmNvalue, value, NULL);
	    XtCallCallbacks (sb, XmNvalueChangedCallback, &newcbs);
	    return;
	}
}

/* ARGSUSED */
static void drag_diff (Widget w, XtPointer closure, XtPointer call_data)
{
    XmScrollBarCallbackStruct *cbs = (XmScrollBarCallbackStruct *) call_data;
    Block *b;
    XmScrollBarCallbackStruct newcbs;
    int side = (w == sbl) ? LEFT : RIGHT;
    int maximum, slidersize;

    if (cbs->value < newss.findex[side]) { /* towards the start */
	for (b = newss.b; b != NULL; b = b->prev) {
	    if (cbs->value < (b->arr[side].fline + b->arr[side].fsize)) {
		newcbs.reason = XmCR_VALUE_CHANGED;
		newcbs.event = NULL;
		newcbs.value = b->sline + (cbs->value - b->arr[side].fline);
		XtVaSetValues (sb, XmNvalue, newcbs.value, NULL);
		XtCallCallbacks (sb, XmNvalueChangedCallback, &newcbs);
		return;
	    }
	}
    }
    else {			/* towards the end */
	for (b = newss.b; b != NULL; b = b->next) {
	    if (cbs->value < (b->arr[side].fline + b->arr[side].fsize)) {
		newcbs.reason = XmCR_VALUE_CHANGED;
		newcbs.event = NULL;
		newcbs.value = b->sline + (cbs->value - b->arr[side].fline);

		XtVaGetValues (sb, XmNmaximum, &maximum, XmNsliderSize, &slidersize, NULL);
		if (newcbs.value > (maximum - slidersize)) {
		    newcbs.value = maximum - slidersize;
		    XtVaSetValues (w, XmNvalue, newcbs.value - b->sline + b->arr[side].fline, NULL);
		    return;
		}
		XtVaSetValues (sb, XmNvalue, newcbs.value, NULL);
		XtCallCallbacks (sb, XmNvalueChangedCallback, &newcbs);
		return;
	    }
	}
    }
}

static void show_version (Widget parent)
{
    static Widget dialog;
    char buffer[512];

    if (dialog == NULL) {
	Pixel fg, bg;
	Display *dpy = XtDisplay (parent);
	Pixmap bitmap;

	dialog = XmCreateInformationDialog (parent, "version", NULL, 0);
	XtUnmanageChild (XmMessageBoxGetChild (dialog, XmDIALOG_CANCEL_BUTTON));
	XtUnmanageChild (XmMessageBoxGetChild (dialog, XmDIALOG_HELP_BUTTON));
	XtVaGetValues (dialog, XmNforeground, &fg, XmNbackground, &bg, NULL);

	bitmap = XCreatePixmapFromBitmapData (dpy, XtWindow (parent),
					      (char *) mgdiff_bits,
					      mgdiff_width, mgdiff_height,
					      fg, bg,
					      DefaultDepth (dpy, DefaultScreen (dpy)));
	(void) sprintf (buffer, "mgdiff\n\nA graphical difference browser\n\nAuthor: Dan Williams (dan@sass.com)\nVersion: %s PL%s", VERSION, PATCHLEVEL);

	XtVaSetValues (dialog,
		       XmNautoUnmanage, True,
		       XmNsymbolPixmap, bitmap,
		       XtVaTypedArg, XmNmessageString, XmRString, buffer, strlen (buffer)+1,
		       NULL);
    }
    XtManageChild (dialog);
}

static void update_overall (void)
{
    int value;

    value = newss.b->arr[LEFT].fline;
    if (newss.sindex < newss.b->arr[LEFT].fsize)
	value += newss.sindex;
    else
	value += newss.b->arr[LEFT].fsize;
    if (value < di->flines[LEFT]) 
	XtVaSetValues (sbl, XmNvalue, value, NULL);
    newss.findex[LEFT] = value;

    value = newss.b->arr[RIGHT].fline;
    if (newss.sindex < newss.b->arr[RIGHT].fsize)
	value += newss.sindex;
    else
	value += newss.b->arr[RIGHT].fsize;
    if (value < di->flines[RIGHT]) 
	XtVaSetValues (sbr, XmNvalue, value, NULL);
    newss.findex[RIGHT] = value;
}

void process_both_files (char *file1, char *name1, char *file2, char *name2)
{
    DiffInfo *newdi;

    str_fnamel = strdup (file1);
    str_snamel = strdup (name1);
    str_fnamer = strdup (file2);
    str_snamer = strdup (name2);

    set_cursor (toplevel);
    newdi = build_diff_info (diffcmd, diffargs, str_fnamel, str_fnamer);
    /* 
     * can't put this before build_diff_info (where it would get to 
     * reuse the freed memory) because build_diff_info processes 
     * exposure events which need the information.
     */
    free_diff_info (di);
    di = newdi;
    if (di->status != 2) {
	no_files_flag = False;
	toggle_openlr_sensitive (True);
	toggle_saveas_sensitive (True);
    }
    else {
	no_files_flag = True;
	toggle_openlr_sensitive (False);
	toggle_saveas_sensitive (False);
	free (str_fnamel);
	free (str_snamel);
	free (str_fnamer);
	free (str_snamer);
	str_snamel = strdup ("(no file)");
	str_snamer = strdup ("(no file)");
	str_fnamel = str_fnamer = NULL;
    }
    refresh ();
    fake_adjust_label (fnamel);
    fake_adjust_label (fnamer);
    reset_cursor (toplevel);

    handle_diff_errors (di);
}

void process_left_file (char *file1, char *name1)
{
    DiffInfo *newdi;

    str_fnamel = strdup (file1);
    str_snamel = strdup (name1);

    set_cursor (toplevel);
    newdi = build_diff_info (diffcmd, diffargs, str_fnamel, str_fnamer);
    /* 
     * can't put this before build_diff_info (where it would get to 
     * reuse the freed memory) because build_diff_info processes 
     * exposure events which need the information.
     */
    free_diff_info (di);
    di = newdi;
    if (di->status == 2) {
	no_files_flag = True;
	toggle_openlr_sensitive (False);
	toggle_saveas_sensitive (False);
	free (str_fnamel);
	free (str_snamel);
	free (str_fnamer);
	free (str_snamer);
	str_snamel = strdup ("(no file)");
	str_snamer = strdup ("(no file)");
	str_fnamel = str_fnamer = NULL;
    }
    refresh ();
    fake_adjust_label (fnamel);
    reset_cursor (toplevel);

    handle_diff_errors (di);
}

void process_right_file (char *file2, char *name2)
{
    DiffInfo *newdi;

    str_fnamer = strdup (file2);
    str_snamer = strdup (name2);

    set_cursor (toplevel);
    newdi = build_diff_info (diffcmd, diffargs, str_fnamel, str_fnamer);
    /* 
     * can't put this before build_diff_info (where it would get to 
     * reuse the freed memory) because build_diff_info processes 
     * exposure events which need the information.
     */
    free_diff_info (di);
    di = newdi;
    if (di->status == 2) {
	no_files_flag = True;
	toggle_openlr_sensitive (False);
	toggle_saveas_sensitive (False);
	free (str_fnamel);
	free (str_snamel);
	free (str_fnamer);
	free (str_snamer);
	str_snamel = strdup ("(no file)");
	str_snamer = strdup ("(no file)");
	str_fnamel = str_fnamer = NULL;
    }
    refresh ();
    fake_adjust_label (fnamer);
    reset_cursor (toplevel);

    handle_diff_errors (di);
}

static void refresh (void)
{
    newss.b = di->first;
    newss.topline = newss.sindex = newss.findex[LEFT] = newss.findex[RIGHT] = 0;

    XtVaSetValues (sbl, XmNmaximum, di->flines[LEFT], XmNpageIncrement, 1,
		   XmNvalue, 0,
		   NULL);
    XtVaSetValues (sbr, XmNmaximum, di->flines[RIGHT], XmNpageIncrement, 1,
		   XmNvalue, 0,
		   NULL);
    XtVaSetValues (sbh, XmNsliderSize, 1, XmNmaximum, di->maxcols,
		   XmNvalue, 0,
		   NULL);
    XtVaSetValues (sb,  XmNsliderSize, 1,
		   XmNmaximum, di->lines,
		   XmNvalue, 0,
		   NULL);
    XtVaSetValues (fnamel, XtVaTypedArg, XmNlabelString, XmRString,
		   str_snamel, strlen (str_snamel) + 1, NULL);
    XtVaSetValues (fnamer, XtVaTypedArg, XmNlabelString, XmRString,
		   str_snamer, strlen (str_snamer) + 1, NULL);

    linenum_columns = ilog10 (max (di->flines[LEFT], di->flines[RIGHT]));
    XtVaSetValues (linenuml, XmNcolumns, linenum_columns, NULL);
    XtVaSetValues (linenumr, XmNcolumns, linenum_columns, NULL);

    update_line_numbers (newss.findex[LEFT] + 1, newss.findex[RIGHT] + 1);
    resize_cb (textl, NULL, NULL);
    resize_cb (textr, NULL, NULL);
    redraw_full (textl);
    redraw_full (textr);
    configure_rowcol (form3, NULL, NULL, NULL);
}

void toggle_openlr_sensitive (Boolean sensitive)
{
    if (!no_files_flag || !sensitive) {
	XtSetSensitive (XtNameToWidget (file_menu, "button_1"), sensitive);
	XtSetSensitive (XtNameToWidget (file_menu, "button_2"), sensitive);
    }
}

void toggle_open_sensitive (Boolean sensitive)
{
    XtSetSensitive (XtNameToWidget (file_menu, "button_0"), sensitive);
    toggle_openlr_sensitive (sensitive);
}

static void toggle_saveas_sensitive (Boolean sensitive)
{
    XtSetSensitive (XtNameToWidget (file_menu, "button_3"), sensitive);
}

/* 
 * return the status of the diff command as the status of this program
 */
/* ARGSUSED */
static void exit_cb (Widget w, XtPointer closure, XtPointer call_data)
{
    exit ((di != NULL) ? di->status : 2);
}

/* ARGSUSED2 */
static void Select (Widget widget, XEvent *event, String *params, Cardinal *num_params)
{
    if (event->xany.type == ButtonPress) {
	Block *b;
	Dimension ypos, height;
	int itemp, rect_height;

	XtVaGetValues (widget, XmNheight, &height, NULL);

	itemp = newss.sindex;
	ypos = 0;
	for (b = newss.b; b != NULL; b = b->next) {
	    if ((rect_height = font_height * (b->ssize - itemp)) > (int) height)
		rect_height = height;

	    if ((event->xbutton.y >= (unsigned int) ypos) &&
		(event->xbutton.y < (unsigned int) (ypos + rect_height)) &&
		(b->arr[LEFT].type != SAME)) {
		switch (b->selected) {
		case LEFT:
		    if (widget == textl) {
			b->selected = NEITHER;
			redraw_partial (textl, ypos, rect_height);
			redraw_partial (textr, ypos, rect_height);
			update_pixmaps ();
		    }
		    else if (widget == textr) {
			b->selected = RIGHT;
			redraw_partial (textl, ypos, rect_height);
			redraw_partial (textr, ypos, rect_height);
			update_pixmaps ();
		    }
		    else
			assert (False);
		    break;
		case RIGHT:
		    if (widget == textl) {
			b->selected = LEFT;
			redraw_partial (textl, ypos, rect_height);
			redraw_partial (textr, ypos, rect_height);
			update_pixmaps ();
		    }
		    else if (widget == textr) {
			b->selected = NEITHER;
			redraw_partial (textl, ypos, rect_height);
			redraw_partial (textr, ypos, rect_height);
			update_pixmaps ();
		    }
		    else
			assert (False);
		    break;
		case NEITHER:
		    b->selected = (widget == textl) ? LEFT : RIGHT;
		    redraw_partial (textl, ypos, rect_height);
		    redraw_partial (textr, ypos, rect_height);
		    update_pixmaps ();
		    break;
		default:
		    assert (False);
		    break;
		}
		return;
	    }

	    ypos += rect_height;

	    itemp = 0;
	    if (ypos > height)
		return;
	}
    }
}

/* 
 * action routine for the text drawing areas: implements the Scrolled 
 * Region recommendations of Section 2.3.4 of the OSF/Motif Style Guide
 */
static void Scroll (Widget widget, XEvent *event, String *params, Cardinal *num_params)
{
    static char *zero = "0";
    static char *one = "1";

    if (*num_params == 1) {
	if (strcmp (params[0], "PageDown") == 0) 
	    XtCallActionProc (sb, "PageDownOrRight", event, &zero, (Cardinal) 1);
	else if (strcmp (params[0], "PageUp") == 0) 
	    XtCallActionProc (sb, "PageUpOrLeft", event, &zero, (Cardinal) 1);
	else if (strcmp (params[0], "Left") == 0) 
	    XtCallActionProc (sbh, "IncrementUpOrLeft", event, &one, (Cardinal) 1);
	else if (strcmp (params[0], "Right") == 0) 
	    XtCallActionProc (sbh, "IncrementDownOrRight", event, &one, (Cardinal) 1);
	else if (strcmp (params[0], "Down") == 0) 
	    XtCallActionProc (sb, "IncrementDownOrRight", event, &zero, (Cardinal) 1);
	else if (strcmp (params[0], "Up") == 0) 
	    XtCallActionProc (sb, "IncrementUpOrLeft", event, &zero, (Cardinal) 1);
	else if (strcmp (params[0], "PageRight") == 0) 
	    XtCallActionProc (sbh, "PageDownOrRight", event, &one, (Cardinal) 1);
	else if (strcmp (params[0], "PageLeft") == 0) 
	    XtCallActionProc (sbh, "PageUpOrLeft", event, &one, (Cardinal) 1);
	else if (strcmp (params[0], "BeginLine") == 0) 
	    XtCallActionProc (sbh, "TopOrBottom", event, NULL, (Cardinal) 0);
	else if (strcmp (params[0], "EndLine") == 0) 
	    XtCallActionProc (sbh, "TopOrBottom", event, NULL, (Cardinal) 0);
	else if (strcmp (params[0], "BeginData") == 0) 
	    XtCallActionProc (sb, "TopOrBottom", event, NULL, (Cardinal) 0);
	else if (strcmp (params[0], "EndData") == 0) 
	    XtCallActionProc (sb, "TopOrBottom", event, NULL, (Cardinal) 0);
	else {
	    char buffer[1024];
	    
	    (void) sprintf (buffer, "Illegal argument to action proc Scroll (\"%s\")", params[0]);
	    XtAppWarning (XtWidgetToApplicationContext (widget), buffer);
	}
    }
    else {
	char buffer[1024];
	    
	(void) sprintf (buffer, "Illegal number of arguments to action proc Scroll (\"%d\")", *num_params);
	XtAppWarning (XtWidgetToApplicationContext (widget), buffer);
    }
}

/* 
 * return the integer base 10 logarithm of the input, also useful as 
 * the number of decimal digits needed to represent the value
 */
static int ilog10 (int lines)
{
    int i;

    for (i = 1; lines >= 10; i++)
	lines /= 10;
    return (i);
}

static void handle_diff_errors (DiffInfo *d)
{
    switch (d->errors) {
    case 0: break;
    case 1: werror_long (toplevel, "Diff Error", d->etext, d->errors); break;
    default: werror_long (toplevel, "Diff Errors", d->etext, d->errors); break;
    }
}

static void select_all (Side side)
{
    Block *b;

    for (b = di->first; b != NULL; b = b->next)
	if (b->arr[LEFT].type != SAME)
	    b->selected = side;
    redraw_full (textl);
    redraw_full (textr);
    update_pixmaps ();
}

static void unselect_all (void)
{
    Block *b;

    for (b = di->first; b != NULL; b = b->next)
	if (b->arr[LEFT].type != SAME)
	    b->selected = NEITHER;
    redraw_full (textl);
    redraw_full (textr);
    update_pixmaps ();
}

/* 
 * return True if all blocks have been selected, False otherwise
 */
static Boolean all_selected (void)
{
    Block *b;

    for (b = di->first; b != NULL; b = b->next)
	if ((b->arr[LEFT].type != SAME) && (b->selected == NEITHER))
	    return (False);

    return (True);
}

static Dimension get_preferred_width (Widget w)
{
    XtWidgetGeometry size;

    size.request_mode = CWWidth;
    (void) XtQueryGeometry (w, NULL, &size);
    return (size.width);
}

/* 
 * delete any prefix ending in '/' and return a copy
 */
static char *basename (char *path)
{
    if (path) {
	char *p;

	/*
	 * find the end of the string
	 */
	for (p = path; *p != '\0'; p++)
	    ;

	/* 
	 * search backwards for a '/' or the beginning of the string
	 */
	for (; ((*p != '/') && (p > path)); p--)
	    ;

	/* 
	 * return a copy of the piece
	 */
	return (strdup ((*p == '/') ? ++p : p));
    }
    else
	return (NULL);
}
