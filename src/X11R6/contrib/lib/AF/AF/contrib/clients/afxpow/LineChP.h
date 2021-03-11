/*
 * LineChart chart display widget
 *
 * Copyright 1989, PCS Computer Systeme GmbH, West Germany
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

#ifndef _LineChartP_h
#define _LineChartP_h

#include "LineChart.h"
#include <X11/CoreP.h>

#define LINES_PER_LABEL 2

/* For setting and adjusting the timer. */

#define HUNDRED 100

/* These are passed to adjust_timeout() and represent
 * numbers of seconds.
 */
#define LARGE_ADD_TIME HUNDRED
#define LARGE_SUBTRACT_TIME -LARGE_ADD_TIME
#define SMALL_ADD_TIME LARGE_ADD_TIME/10
#define SMALL_SUBTRACT_TIME -SMALL_ADD_TIME
#define MAX_TIME 10 * HUNDRED /* five minutes */


#define VALID_UPDATE(x) ((x <= MAX_TIME) && (x > 25))

/* This is a padding along the inner border of the
 * window inside of which nothing will be drawn.
 */

#define INNER_BORDER ((int) (.3 * (float) pw->lineChart.font_height))

/* This is a similar border but it appears around each graph. */

#define GRAPH_BORDER ((int) (.2 * (float) pw->lineChart.font_height))

/* This appears around each label. */

#define FONT_PAD ((int) (.1 * (float) pw->lineChart.font_height))

/* height in pixels of the name of the host and the padding around it */

#define HOST_HEIGHT (FONT_PAD * 2 + pw->lineChart.font_height)

/* height in pixels of a single graph when the window is at its smallest */

#define MIN_GRAPH_HEIGHT ((pw->lineChart.lines_per_label + 1) \
    * (pw->lineChart.font_height + 2 * FONT_PAD))

/* default height of the graph */

#define GRAPH_HEIGHT ((pw->lineChart.lines_per_label * 2) \
    * pw->lineChart.font_height)

/* height of the drawable area inside of a graph */

#define DRAW_GRAPH_HEIGHT(pw) (pw->lineChart.min_pix - pw->lineChart.max_pix)

/* Amount of space on the top and bottom of the
 * window that will not be used by the graphs
 */

#define TOPSTUFF (INNER_BORDER + (int) pw->core.border_width + HOST_HEIGHT)
#define BOTTOMSTUFF (INNER_BORDER + (int) pw->core.border_width)

/* An expression that will scale a pixel value for graph number i.
 */

#define SCALE(pw,n) (((n) * DRAW_GRAPH_HEIGHT(pw)) \
     / (((pw)->lineChart.max_value == (pw)->lineChart.min_value) ? 1 : \
	((pw)->lineChart.max_value - (pw)->lineChart.min_value)))

/* New fields for the lineChart widget instance record */
typedef struct {
    String label;		/* Label for graph. */
    GC fgGC;			/* graphics context for fgpixel */
    GC hiGC;			/* graphics context for hipixel */
    Pixel fgpixel;		/* color index for graph */
    Pixel hipixel;		/* color index for text */
    int step_size;              /* pixel (value) draw size */
    int update;			/* update frequency */
    XtIntervalId interval_id;   /* timeout interval id */
    XFontStruct *font;		/* font for text */
    unsigned int font_height;	/* height of font */
    Boolean reverse_video;	/* display in reverse video */
    XtCallbackList get_value;	/* proc to call to get value. */
    int lines_per_label;
    int min_value;  /* min and max are raw values used to calculate scale. */
    int max_value;  /* See the comment at the top for a description of */
		    /* what is meant by "raw values." */
    int min_pix;    /* y pixel values for the bottom and top of the */
    int max_pix;    /* area in which the graph is actually drawn. */
    int zero;       /* y pixel value for zero on the graph */
    int g_width;    /* width of drawable graph in pixels */
    int left_pix;   /* x pixel values for left of graph drawing area */
    int cur_value;  /* saved value -index to the current item in saved_values*/
    int orig_update;/* Original update_value. */
    int *saved_values; /* This is an array int[value_num] of
		     * raw values.  It is used for refreshing the graph.
		     * There is one saved value for each step on the 
		     * graph.  Each saved value corresponds to 
		     * x_params.step_size pixel values.
		     */
} LineChartPart;

/* Full instance record declaration */
typedef struct _LineChartRec {
    CorePart core;
    LineChartPart lineChart;
} LineChartRec;

/* New fields for the LineChart widget class record */
typedef struct {
    int dummy;
} LineChartClassPart;

/* Full class record declaration */
typedef struct _LineChartClassRec {
    CoreClassPart core_class;
    LineChartClassPart lineChart_class;
} LineChartClassRec;

/* Class pointer */
extern LineChartClassRec lineChartClassRec;

#endif
