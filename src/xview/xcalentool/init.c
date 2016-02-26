/*
 * $Id: init.c,v 2.3 1994/08/29 17:32:54 billr Exp $
 */
/*
 * calentool - day/week/month/year-at-a-glance calendar for XView/Open Look
 * 
 * Original suntool source Copyright (C) 1987, Sun Microsystems, Inc.
 * 	All Rights Reserved
 * Permission is hereby granted to use and modify this program in source
 * or binary form as long as it is not sold for profit and this copyright
 * notice remains intact.
 * Original author: Philip Heller, Sun Microsystems, Inc.
 * 
 * All additional software, enhancements and modifications are
 * Copyright 1988, 1989, 1991, 1994 by Tektronix, Inc. - All Rights Reserved.
 * 
 * Permission to use, copy, modify, and distribute this software and its
 * documentation for any purpose is hereby granted without fee, provided that
 * the above copyright notice appear in all copies and that both that
 * copyright notice and this permission notice appear in supporting
 * documentation, and that the name of Tektronix, Inc. not be used in
 * advertising or publicity pertaining to distribution of the software
 * without specific, written prior permission.
 * 
 * TEKTRONIX INCORPORATED MAKES NO REPRESENTATIONS ABOUT THE
 * SUITABILITY OF THIS SOFTWARE FOR ANY PURPOSE.  IT IS PROVIDED "AS IS"
 * WITHOUT EXPRESS OR IMPLIED WARRANTY.  TEKTRONIX INCORPORATED
 * DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING ALL IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.  IN NO
 * EVENT SHALL TEKTRONIX INCORPORATED BE LIABLE FOR ANY SPECIAL,
 * INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM
 * LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE
 * OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
 * PERFORMANCE OF THIS SOFTWARE.
 * 
 * Author: Bill Randle, Tektronix, Inc. <billr@saab.cna.tek.com>
 */ 

/***************************************************
 *						   *
 *   Initialization routines, for memory pixrects  *
 *						   *
 ***************************************************/


#include <stdio.h>
#include <X11/Xlib.h>
#include <xview/xview.h>
#include <xview/defaults.h>
#include <xview/svrimage.h>
#include <xview/font.h>
#include <sys/file.h>
#include <sys/stat.h>
#include <pwd.h>
#include "ct.h"
#include "xv_ct.h"


extern int dayslot_width, dayslot_height;
extern int weekslot_width, weekslot_height;
extern int ybox_width, ybox_height;
extern Frame frame;
extern Frame prompt_frame;
extern int otherfile, one_based, read_only;
extern char *othername, *mailto;
extern Xv_font font;
extern XWindowAttributes attr;
Pixmap daybox_pr, timeslot_pr, gray_pr;
Pixmap weekslot_pr, weekarrow_pr, smallarrow_pr;
Pixmap arrowshaft_pr, arrowhead_pr;
Pixmap weekarrowshaft_pr, weekarrowhead_pr;
Pixmap gr_arrowshaft_pr, gr_arrowhead_pr;
Pixmap gr_weekarrowshaft_pr, gr_weekarrowhead_pr;
Pixmap triangle_pr;
Pixmap tri_up_pr;
Pixmap a_ydaybox_pr, ydaybox_pr, ymonthbox_pr;
Pixmap daybox_td_pr, weekslot_td_pr;
Pixmap ydaybox_td_pr, timeslot_td_pr;
Pixmap leftarrow, rightarrow;
Pixmap morebutton;
Pixmap savePix;
char t_title[160];
char apts_pathname[160], tmpapts_pathname[160];
char apts_dir[128], lib_dir[128];
static char dots[] = {1, 1};
GC gcp, gcps, gcp1;
Drawable rootwin;

#include <X11/bitmaps/left_ptr>

#ifdef __STDC__
void init_daybox_pr (void);
void init_timeslot_pr (void);
void init_weekslot_pr (void);
void init_weekarrow_pr (void);
void init_arrow_prs (void);
void init_triangle_pr (void);
void init_ydaybox_pr (void);
void init_ymonthbox_pr (void);
void init_morebutton_pr (void);
#else
void init_daybox_pr ();
void init_timeslot_pr ();
void init_weekslot_pr ();
void init_weekarrow_pr ();
void init_arrow_prs ();
void init_triangle_pr ();
void init_ydaybox_pr ();
void init_ymonthbox_pr ();
void init_morebutton_pr ();
#endif

void
init_pixrects()
{
	Pixmap pattern;

	rootwin = (Drawable)xv_get(frame, XV_XID);
	savePix = XCreatePixmap(mydisplay, rootwin, 16, 16, attr.depth);
	pattern = XCreatePixmap(mydisplay, rootwin, 64, 2, 1);
	/* generic gc for 1 bit deep pixmap */
	gcp1 = XCreateGC(mydisplay, pattern,
		GCBackground|GCForeground|GCFunction|GCFont,
		&gc_val);
	init_daybox_pr();
	init_timeslot_pr();
	init_weekslot_pr();
        init_weekarrow_pr();
	init_arrow_prs();
	init_triangle_pr();
	init_ydaybox_pr();
	init_ymonthbox_pr();
	init_morebutton_pr();
	XFreeGC(mydisplay, gcp);
	XFreeGC(mydisplay, gcps);
	XFreeGC(mydisplay, gcp1);
	XFreePixmap(mydisplay, pattern);
}

static XPoint boxin[5] = { {2,2}, {61,2}, {61,61}, {2,61}, {2,2} };
static XPoint boxout[5] = { {0,0}, {63,0}, {63,63}, {0,63}, {0,0} };

/* Concentric squares for drawing days of month calendar. */
void
init_daybox_pr()
{
	Pixmap pattern;
	int i;

	daybox_pr = XCreatePixmap(mydisplay, rootwin,
			64, 64, attr.depth);
	pattern = XCreatePixmap(mydisplay, rootwin, 64, 2, 1);

	/* create some gc's for making the Pixmaps */
	/* they will be destroyed when finished */
	gcp = XCreateGC(mydisplay, daybox_pr,
		GCBackground|GCForeground|GCFunction|GCFont,
		&gc_val);
	gcps = XCreateGC(mydisplay, daybox_pr,
		GCBackground|GCForeground|GCFunction|GCFont|GCFillStyle,
		&gc_val);
	/* fill pixmap with background color */
	XSetForeground(mydisplay, gcp, backgr);
	XFillRectangle(mydisplay, daybox_pr, gcp,
		0, 0,
		64, 64);

	XSetForeground(mydisplay, gcp, foregr);
        XDrawLines(mydisplay, daybox_pr, gcp, boxin, 5, CoordModeOrigin);
        XDrawLines(mydisplay, daybox_pr, gcp, boxout, 5, CoordModeOrigin);

	XSetFunction(mydisplay, gcp1, GXclear);
	XFillRectangle(mydisplay, pattern, gcp1,
		0, 0,
		64, 2);

	XSetFunction(mydisplay, gcp1, GXset);
	for (i=3; i<61; i+=8)
		XDrawPoint(mydisplay, pattern, gcp1, i, 0);
	for (i=7; i<61; i+=8)
		XDrawPoint(mydisplay, pattern, gcp1, i, 1);

	daybox_td_pr = XCreatePixmap(mydisplay, rootwin,
			64, 64, attr.depth);
	XCopyArea(mydisplay, daybox_pr, daybox_td_pr, gcp, 0, 0,
		64, 64,
		0, 0);

	XSetStipple(mydisplay, gcps, pattern);
	XSetFunction(mydisplay, gcps, GXcopy);
	XSetTSOrigin(mydisplay, gcps, 3, 3);
	XFillRectangle(mydisplay, daybox_td_pr, gcps,
		3, 3,
		58, 58);
	XFreePixmap(mydisplay, pattern);

}

static XPoint ts_box[] = { {0,0}, {0,0}, {0,0}, {0,0}, {0,0} };

/* Box for 30-minute slot in picture of day. */
void
init_timeslot_pr()
{
	int i;
	Pixmap pattern;

	ts_box[1].x = ts_box[2].x = dayslot_width-1;
	ts_box[2].y = ts_box[3].y = dayslot_height-1;

	pattern = XCreatePixmap(mydisplay, rootwin,
			dayslot_width, 2, 1);
	timeslot_pr = XCreatePixmap(mydisplay, rootwin,
			dayslot_width,
			dayslot_height,
			attr.depth);
	timeslot_td_pr = XCreatePixmap(mydisplay, rootwin,
			dayslot_width,
			dayslot_height,
			attr.depth);

	XSetFunction(mydisplay, gcp1, GXclear);
	XFillRectangle(mydisplay, pattern, gcp1,
		0, 0,
		dayslot_width, 2);

	XSetFunction(mydisplay, gcp1, GXset);
	for (i=1; i<dayslot_width; i+=8)
		XDrawPoint(mydisplay, pattern, gcp1, i, 0);
	for (i=5; i<dayslot_width; i+=8)
		XDrawPoint(mydisplay, pattern, gcp1, i, 1);

	XSetForeground(mydisplay, gcp, backgr);
	XFillRectangle(mydisplay, timeslot_pr, gcp,
		0, 0,
		dayslot_width, dayslot_height);
	XSetForeground(mydisplay, gcp, foregr);
	XSetLineAttributes(mydisplay, gcp, 0,
		LineDoubleDash, CapButt, JoinMiter);
	XSetDashes(mydisplay, gcp, 0, dots, 2);
        XDrawLines(mydisplay, timeslot_pr, gcp, ts_box, 5, CoordModeOrigin);

	XCopyArea(mydisplay, timeslot_pr, timeslot_td_pr, gcp, 0, 0,
		dayslot_width, dayslot_height,
		0, 0);
	XSetLineAttributes(mydisplay, gcp, 0,
		LineSolid, CapButt, JoinMiter);

	XSetStipple(mydisplay, gcps, pattern);
	XSetFunction(mydisplay, gcps, GXcopy);
	XSetTSOrigin(mydisplay, gcps, 0, 1);
	XFillRectangle(mydisplay, timeslot_td_pr, gcps,
		0, 1,
		dayslot_width-2, dayslot_height-3);
	XFreePixmap(mydisplay, pattern);
}

static XPoint ws_box[] = { {0,0}, {0,0}, {0,0}, {0,0}, {0,0} };

/* Box for 30-minute slot in picture of week. */
void
init_weekslot_pr()
{
	int i;
	Pixmap pattern;

	ws_box[1].x = ws_box[2].x = weekslot_width-1;
	ws_box[2].y = ws_box[3].y = weekslot_height-1;

	pattern = XCreatePixmap(mydisplay, rootwin,
			weekslot_width,
			2,
			1);

	weekslot_pr = XCreatePixmap(mydisplay, rootwin,
			weekslot_width,
			weekslot_height,
			attr.depth);

	weekslot_td_pr = XCreatePixmap(mydisplay, rootwin,
			weekslot_width,
			weekslot_height,
			attr.depth);

	XSetFunction(mydisplay, gcp1, GXclear);
	XFillRectangle(mydisplay, pattern, gcp1,
		0, 0,
		weekslot_width, 2);

	XSetFunction(mydisplay, gcp1, GXset);
	for (i=1; i<weekslot_width; i+=8)
		XDrawPoint(mydisplay, pattern, gcp1, i, 0);
	for (i=5; i<weekslot_width; i+=8)
		XDrawPoint(mydisplay, pattern, gcp1, i, 1);

	XSetForeground(mydisplay, gcp, backgr);
	XFillRectangle(mydisplay, weekslot_pr, gcp,
		0, 0,
		weekslot_width, weekslot_height);
	XSetForeground(mydisplay, gcp, foregr);
	XSetLineAttributes(mydisplay, gcp, 0,
		LineDoubleDash, CapButt, JoinMiter);
	XSetDashes(mydisplay, gcp, 0, dots, 2);
	XDrawLines(mydisplay, weekslot_pr, gcp, ws_box, 5, CoordModeOrigin);

	XSetForeground(mydisplay, gcp, backgr);
	XFillRectangle(mydisplay, weekslot_td_pr, gcp,
		0, 0,
		weekslot_width, weekslot_height);
	XSetForeground(mydisplay, gcp, foregr);
        XDrawLines(mydisplay, weekslot_td_pr, gcp, ws_box, 5, CoordModeOrigin);
	XSetLineAttributes(mydisplay, gcp, 0,
		LineSolid, CapButt, JoinMiter);

	XSetStipple(mydisplay, gcps, pattern);
	XSetFunction(mydisplay, gcps, GXcopy);
	XSetTSOrigin(mydisplay, gcps, 0, 1);
	XFillRectangle(mydisplay, weekslot_td_pr, gcps,
		0, 1,
		weekslot_width-1, weekslot_height-3);
	XFreePixmap(mydisplay, pattern);
}

static XPoint inside[6] = { {2,2}, {26,2}, {38,14}, {26,26}, {2,26}, {2,2} };
static XPoint outside[6] = { {0,0}, {27,0}, {41,14}, {27,28}, {0,28}, {0,0} };
static XPoint smallinside[6] = { {3,3}, {25,3}, {37,13}, {25,25}, {3,25}, {3,3} };

/* Arrow pointing to a week in picture of month. */
void
init_weekarrow_pr()
{
	weekarrow_pr = XCreatePixmap(mydisplay, rootwin,
			42, 29,
			attr.depth);
	XSetForeground(mydisplay, gcp, backgr);
	XFillRectangle(mydisplay, weekarrow_pr, gcp,
		0, 0,
		42, 29);

	XSetForeground(mydisplay, gcp, foregr);
        XDrawLines(mydisplay, weekarrow_pr, gcp, outside, 6, CoordModeOrigin);
        XDrawLines(mydisplay, weekarrow_pr, gcp, inside, 6, CoordModeOrigin);

	smallarrow_pr = XCreatePixmap(mydisplay, rootwin,
			42, 29,
			attr.depth);
	XSetForeground(mydisplay, gcp, backgr);
	XFillRectangle(mydisplay, smallarrow_pr, gcp,
		0, 0,
		42, 29);

	XSetForeground(mydisplay, gcp, foregr);
	XFillPolygon(mydisplay, smallarrow_pr, gcp, smallinside, 6, Convex, CoordModeOrigin);
}

static XPoint d_arr[] = { {0,0}, {0,0}, {7,0}, {8,0}, {15,0},
	{15,0}, {8,0}, {7,0}, {0,0}, };
static XPoint w_arr[] = { {0,0}, {0,0}, {6,0}, {7,0}, {13,0},
	{13,0}, {7,0}, {6,0}, {0,0}, };
static XPoint l_arr[] = { {2,6}, {6,2}, {6,5}, {16,5}, {16,7}, {6,7},
	{6,10}, {2,6} };
static XPoint r_arr[] = { {2,5}, {12,5}, {12,2}, {16,6}, {12,10},
	{12,7}, {2,7}, {2,5} };
static XPoint arr_box[] = { {0,0}, {18,0}, {18,13}, {0,13}, {0,0} };

void
init_arrow_prs()
{
	int i;
	Pixmap pattern;

	d_arr[0].y = d_arr[5].y = d_arr[8].y = dayslot_height - 9;
	d_arr[1].y = d_arr[4].y = dayslot_height - 10;
	d_arr[2].y = d_arr[3].y = dayslot_height - 3;
	d_arr[6].y = d_arr[7].y = dayslot_height - 2;
	w_arr[0].y = w_arr[5].y = w_arr[8].y = weekslot_height - 9;
	w_arr[1].y = w_arr[4].y = weekslot_height - 10;
	w_arr[2].y = w_arr[3].y = weekslot_height - 3;
	w_arr[6].y = w_arr[7].y = weekslot_height - 2;

	arrowshaft_pr = XCreatePixmap(mydisplay, rootwin,
			16,
			dayslot_height,
			attr.depth);
	arrowhead_pr = XCreatePixmap(mydisplay, rootwin,
			16,
			dayslot_height,
			attr.depth);
	weekarrowshaft_pr = XCreatePixmap(mydisplay, rootwin,
			14,
			weekslot_height,
			attr.depth);
	weekarrowhead_pr = XCreatePixmap(mydisplay, rootwin,
			14,
			weekslot_height,
			attr.depth);
	gr_arrowshaft_pr = XCreatePixmap(mydisplay, rootwin,
			16,
			dayslot_height,
			attr.depth);
	gr_arrowhead_pr = XCreatePixmap(mydisplay, rootwin,
			16,
			dayslot_height,
			attr.depth);
	gr_weekarrowshaft_pr = XCreatePixmap(mydisplay, rootwin,
			14,
			weekslot_height,
			attr.depth);
	gr_weekarrowhead_pr = XCreatePixmap(mydisplay, rootwin,
			14,
			weekslot_height,
			attr.depth);

	XSetForeground(mydisplay, gcp, backgr);
	XFillRectangle(mydisplay, arrowshaft_pr, gcp, 0, 0,
		 16, dayslot_height);
	XSetForeground(mydisplay, gcp, foregr);
	XFillRectangle(mydisplay, arrowshaft_pr, gcp, 3, 0, 2, dayslot_height);
	XFillRectangle(mydisplay, arrowshaft_pr, gcp, 11, 0, 2, dayslot_height);

	XSetForeground(mydisplay, gcp, backgr);
	XFillRectangle(mydisplay, arrowhead_pr, gcp, 0, 0,
		 16, dayslot_height);
	XSetForeground(mydisplay, gcp, foregr);
	XFillRectangle(mydisplay, arrowhead_pr, gcp, 3, 0,
		 2, dayslot_height-5);
	XFillRectangle(mydisplay, arrowhead_pr, gcp, 11, 0,
		 2, dayslot_height-5);
	XDrawLines(mydisplay, arrowhead_pr, gcp, d_arr, 9, CoordModeOrigin);

	XSetForeground(mydisplay, gcp, backgr);
	XFillRectangle(mydisplay, weekarrowshaft_pr, gcp, 0, 0,
		 14, weekslot_height);
	XSetForeground(mydisplay, gcp, foregr);
	XFillRectangle(mydisplay, weekarrowshaft_pr, gcp, 3, 0, 2, weekslot_height);
	XFillRectangle(mydisplay, weekarrowshaft_pr, gcp, 9, 0, 2, weekslot_height);

	XSetForeground(mydisplay, gcp, backgr);
	XFillRectangle(mydisplay, weekarrowhead_pr, gcp, 0, 0,
		 14, weekslot_height);
	XSetForeground(mydisplay, gcp, foregr);
	XFillRectangle(mydisplay, weekarrowhead_pr, gcp, 3, 0, 2, weekslot_height-5);
	XFillRectangle(mydisplay, weekarrowhead_pr, gcp, 9, 0, 2, weekslot_height-5);
	XDrawLines(mydisplay, weekarrowhead_pr, gcp, w_arr, 9, CoordModeOrigin);

	pattern = XCreatePixmap(mydisplay, rootwin,
			2,
			dayslot_height,
			1);
	XSetFunction(mydisplay, gcp1, GXset);
	XFillRectangle(mydisplay, pattern, gcp1, 0, 0, 2, dayslot_height-1);
	XSetFunction(mydisplay, gcp1, GXclear);
	for (i=0; i<dayslot_height; i+=4) {
		XDrawPoint(mydisplay, pattern, gcp1, 0, i);
		XDrawPoint(mydisplay, pattern, gcp1, 1, i);
	}
	XSetForeground(mydisplay, gcp, backgr);
	XFillRectangle(mydisplay, gr_arrowshaft_pr, gcp, 0, 0,
		 16, dayslot_height);
	XSetStipple(mydisplay, gcps, pattern);
	XSetTSOrigin(mydisplay, gcps, 3, 0);
	XSetFunction(mydisplay, gcps, GXcopy);
	XFillRectangle(mydisplay, gr_arrowshaft_pr, gcps, 3, 0, 2, dayslot_height);
	XSetTSOrigin(mydisplay, gcps, 11, 0);
	XFillRectangle(mydisplay, gr_arrowshaft_pr, gcps, 11, 0, 2, dayslot_height);
	XFillRectangle(mydisplay, gr_weekarrowshaft_pr, gcp, 0, 0,
		 14, weekslot_height);
	XSetStipple(mydisplay, gcps, pattern);
	XSetTSOrigin(mydisplay, gcps, 3, 0);
	XFillRectangle(mydisplay, gr_weekarrowshaft_pr, gcps, 3, 0, 2, weekslot_height);
	XSetTSOrigin(mydisplay, gcps, 9, 0);
	XFillRectangle(mydisplay, gr_weekarrowshaft_pr, gcps, 9, 0, 2, weekslot_height);
	XFillRectangle(mydisplay, gr_arrowhead_pr, gcp, 0, 0,
		 16, dayslot_height);
	XFillRectangle(mydisplay, gr_arrowhead_pr, gcps, 3, 0, 2, dayslot_height-5);
	XFillRectangle(mydisplay, gr_arrowhead_pr, gcps, 11, 0, 2, dayslot_height-5);
	XSetForeground(mydisplay, gcp, foregr);
	XDrawLines(mydisplay, gr_arrowhead_pr, gcp, d_arr, 9, CoordModeOrigin);
	XSetForeground(mydisplay, gcp, backgr);
	XDrawPoint(mydisplay, gr_arrowhead_pr, gcp, 2, dayslot_height-8);
	XDrawPoint(mydisplay, gr_arrowhead_pr, gcp, 2, dayslot_height-7);
	XDrawPoint(mydisplay, gr_arrowhead_pr, gcp, 5, dayslot_height-4);
	XDrawPoint(mydisplay, gr_arrowhead_pr, gcp, 5, dayslot_height-5);
	XDrawPoint(mydisplay, gr_arrowhead_pr, gcp, 10, dayslot_height-5);
	XDrawPoint(mydisplay, gr_arrowhead_pr, gcp, 10, dayslot_height-4);
	XDrawPoint(mydisplay, gr_arrowhead_pr, gcp, 13, dayslot_height-8);
	XDrawPoint(mydisplay, gr_arrowhead_pr, gcp, 13, dayslot_height-7);
	XFillRectangle(mydisplay, gr_weekarrowhead_pr, gcp, 0, 0,
		 14, weekslot_height);
	XFillRectangle(mydisplay, gr_weekarrowhead_pr, gcps, 3, 0, 2, weekslot_height-5);
	XFillRectangle(mydisplay, gr_weekarrowhead_pr, gcps, 9, 0, 2, weekslot_height-5);
	XSetForeground(mydisplay, gcp, foregr);
	XDrawLines(mydisplay, gr_weekarrowhead_pr, gcp, w_arr, 9, CoordModeOrigin);
	XSetForeground(mydisplay, gcp, backgr);
	XDrawPoint(mydisplay, gr_weekarrowhead_pr, gcp, 2, weekslot_height-7);
	XDrawPoint(mydisplay, gr_weekarrowhead_pr, gcp, 2, weekslot_height-6);
	XDrawPoint(mydisplay, gr_weekarrowhead_pr, gcp, 5, weekslot_height-4);
	XDrawPoint(mydisplay, gr_weekarrowhead_pr, gcp, 5, weekslot_height-3);
	XDrawPoint(mydisplay, gr_weekarrowhead_pr, gcp, 8, weekslot_height-4);
	XDrawPoint(mydisplay, gr_weekarrowhead_pr, gcp, 8, weekslot_height-3);
	XDrawPoint(mydisplay, gr_weekarrowhead_pr, gcp, 11, weekslot_height-7);
	XDrawPoint(mydisplay, gr_weekarrowhead_pr, gcp, 11, weekslot_height-6);
	XFreePixmap(mydisplay, pattern);

	leftarrow = XCreatePixmap(mydisplay, rootwin,
			19,
			14,
			attr.depth);
	XSetForeground(mydisplay, gcp, backgr);
	XFillRectangle(mydisplay, leftarrow, gcp, 0, 0,
		 19, 14);
	XSetForeground(mydisplay, gcp, foregr);
	XFillPolygon(mydisplay, leftarrow, gcp, l_arr, 8, Convex, CoordModeOrigin);
	XDrawLines(mydisplay, leftarrow, gcp, arr_box, 5, CoordModeOrigin);

	rightarrow = XCreatePixmap(mydisplay, rootwin,
			19,
			14,
			attr.depth);
	XSetForeground(mydisplay, gcp, backgr);
	XFillRectangle(mydisplay, rightarrow, gcp, 0, 0,
		 19, 14);
	XSetForeground(mydisplay, gcp, foregr);
	XFillPolygon(mydisplay, rightarrow, gcp, r_arr, 8, Convex, CoordModeOrigin);
	XDrawLines(mydisplay, rightarrow, gcp, arr_box, 5, CoordModeOrigin);
}


static XPoint triangle[4] = { {0,0}, {15,0}, {15,15}, {0,0} };

void
init_triangle_pr()
{
	triangle_pr = XCreatePixmap(mydisplay, rootwin,
			16,
			16,
			attr.depth);
	XSetForeground(mydisplay, gcp, backgr);
	XFillRectangle(mydisplay, triangle_pr, gcp, 0, 0,
		 16, 16);
	XSetForeground(mydisplay, gcp, foregr);
	XFillPolygon(mydisplay, triangle_pr, gcp, triangle, 4, Convex, CoordModeOrigin);

	tri_up_pr = XCreateBitmapFromData(mydisplay, rootwin,
		left_ptr_bits,
		16, 16);
#if 0
		WhitePixel(mydisplay, screen),
		BlackPixel(mydisplay, screen),
		attr.depth);
#endif
}

static XPoint ybox[] = { {0,0}, {0,0}, {0,0}, {0,0}, {0,0} };

/* squares for drawing days of month on the year calendar */
void
init_ydaybox_pr()
{
	Pixmap pattern;
	int i;

	ybox[1].x = ybox[2].x = ybox_width -1;
	ybox[2].y = ybox[3].y = ybox_height - 1;
 	ydaybox_pr = XCreatePixmap(mydisplay, rootwin,
			ybox_width,
			ybox_height,
			attr.depth);
 	a_ydaybox_pr = XCreatePixmap(mydisplay, rootwin,
			ybox_width,
			ybox_height,
			attr.depth);
	XFillRectangle(mydisplay, a_ydaybox_pr, gcp, 0, 0,
		 ybox_width, ybox_height);
	XSetForeground(mydisplay, gcp, backgr);
	XFillRectangle(mydisplay, ydaybox_pr, gcp, 0, 0,
		 ybox_width, ybox_height);
	XDrawLines(mydisplay, a_ydaybox_pr, gcp, ybox, 5, CoordModeOrigin);

	XSetForeground(mydisplay, gcp, foregr);
	XDrawLines(mydisplay, ydaybox_pr, gcp, ybox, 5, CoordModeOrigin);

 	pattern = XCreatePixmap(mydisplay, rootwin,
			ybox_width,
			2,
			1);
	XSetFunction(mydisplay, gcp1, GXclear);
	XFillRectangle(mydisplay, pattern, gcp1, 0, 0,
		 ybox_width, 2);
	XSetFunction(mydisplay, gcp1, GXset);
	for (i=1; i<ybox_width; i+=4)
		XDrawPoint(mydisplay, pattern, gcp1, i, 0);
	for (i=3; i<ybox_width; i+=4)
		XDrawPoint(mydisplay, pattern, gcp1, i, 1);

  	ydaybox_td_pr = XCreatePixmap(mydisplay, rootwin,
			ybox_width,
			ybox_height,
			attr.depth);
	XSetForeground(mydisplay, gcp, backgr);
	XFillRectangle(mydisplay, ydaybox_td_pr, gcp, 0, 0,
		 ybox_width, ybox_height);

	XSetForeground(mydisplay, gcp, foregr);
	XDrawLines(mydisplay, ydaybox_td_pr, gcp, ybox, 5, CoordModeOrigin);

	XSetFunction(mydisplay, gcps, GXcopy);
	XSetStipple(mydisplay, gcps, pattern);
	XSetTSOrigin(mydisplay, gcps, 1, 1);
	XFillRectangle(mydisplay, ydaybox_td_pr, gcps,
		1, 1,
		ybox_width-2, ybox_height-2);

	XFreePixmap(mydisplay, pattern);
}

static XPoint ymbox[] = { {0,0}, {0,0}, {0,0}, {0,0}, {0,0} };

/* label box for each month on the year calendar */
void
init_ymonthbox_pr()
{
	int ymbox_width;

	ymbox_width = 7 * ybox_width;
	ymbox[1].x = ymbox[2].x = ymbox_width -1;
	ymbox[2].y = ymbox[3].y = ybox_height - 1;
  	ymonthbox_pr = XCreatePixmap(mydisplay, rootwin,
			ymbox_width,
			ybox_height,
			attr.depth);
	XSetForeground(mydisplay, gcp, backgr);
	XFillRectangle(mydisplay, ymonthbox_pr, gcp, 0, 0,
		 ymbox_width, ybox_height);

	XSetForeground(mydisplay, gcp, foregr);
	XDrawLines(mydisplay, ymonthbox_pr, gcp, ymbox, 5, CoordModeOrigin);
}

/* create "More" button for day and week pages */
void
init_morebutton_pr()
{
	xfont = (XFontStruct *)xv_get(font, FONT_INFO);
	XSetFont(mydisplay, gcp, xfont->fid);
	m_width = XTextWidth(xfont, " More ", 6);
	m_height = xv_get(font, FONT_DEFAULT_CHAR_HEIGHT)+6;
	morebutton = XCreatePixmap(mydisplay, rootwin,
		m_width,
		m_height,
		attr.depth);
	
	XSetForeground(mydisplay, gcp, backgr);
	XFillRectangle(mydisplay, morebutton, gcp, 0, 0,
		 m_width, m_height);
	XSetForeground(mydisplay, gcp, foregr);
	XDrawLine(mydisplay, morebutton, gcp, 0, 0, m_width-1, 0);
	XDrawLine(mydisplay, morebutton, gcp, m_width-1, 0,
		m_width-1, m_height-1);
	XDrawLine(mydisplay, morebutton, gcp, m_width-1, m_height-1,
		0, m_height-1);
	XDrawLine(mydisplay, morebutton, gcp, 0, m_height-1, 0, 0);
	XDrawString(mydisplay, morebutton, gcp, 0, m_height-6, " More ", 6);
}
