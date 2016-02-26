/*
 * $Id: event.c,v 2.3 1994/08/29 17:32:54 billr Exp $
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
/********************************************************
 *							*
 *    Main driver and month and year event routines	*
 *    for main subwindow  				*
 *							*
 ********************************************************/


#include <stdio.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <xview/xview.h>
#include <xview/panel.h>
#include <xview/canvas.h>
#undef ROUNDUP  /* to avoid multiply declared define */
#include <xview/seln.h>
#include <xview/font.h>
#include <xview/cursor.h>
#include <sys/file.h>
#include "ct.h"
#include "xv_ct.h"
#include "event.h"

int do_icon = 0;
XPoint icon_tbox[] = { {0,64}, {63,64}, {63,76}, {0,76}, {0,64} };

extern Frame frame, attr_frame;
extern Frame fframe, sframe, mframe, fileframe;
extern struct tm olddate, closedate;
extern int update_interval, show_time;
extern char timestr[];
extern int monday_first, hour24;
extern Xv_Font sfont;
extern Seln_client s_client;
extern int locked;
extern XPoint ilabel;
extern int icon_in_use;
extern char datestr_day[];
extern char *smonthnames[];
extern XWMHints *myWMHints;
extern Window icon_window;
extern Canvas canvas;
Notify_value myframe_interposer();

#ifdef __STDC__
extern int day_inputevent (Xv_Window window, Event *event);
extern void week_inputevent (Xv_window window, Event *event);
extern void mframe_done (Frame frame);
extern void sframe_done (Frame frame);
#else
extern int day_inputevent ();
extern void week_inputevent ();
extern void mframe_done ();
extern void sframe_done ();
#endif

#ifdef __STDC__
void month_inputevent (Xv_window window, Event *event);
void year_inputevent (Xv_window window, Event *event);
void update_icon_time (void);
void format_icon_time (void);
#else
void month_inputevent ();
void year_inputevent ();
void update_icon_time ();
void format_icon_time ();
#endif

void
mainsw_inputevent(window, event)
Xv_window window;
Event *event;
{
	if (locked)	/* no updates allowed */
		return;

	switch (mainsw_state) {
		case DISPLAYING_DAY:
			xv_set(canvas, XV_HELP_DATA, "calentool:DayDisplay", 0);
			if (event_action(event) == ACTION_COPY && event_is_up(event)) {
				if (seln_acquire(s_client, SELN_PRIMARY) != SELN_PRIMARY) {
					err_rpt("Can't acquire selection primary", NON_FATAL);
					return;
				}
				if (seln_acquire(s_client, SELN_SHELF) != SELN_SHELF) {
					err_rpt("Can't acquire selection shelf", NON_FATAL);
					return;
				}
				seln_report_event(s_client, event);
			}
			day_inputevent(window, event);
			break;
		case DISPLAYING_WEEK:
			xv_set(canvas, XV_HELP_DATA, "calentool:WeekDisplay", 0);
			week_inputevent(window, event);
			break;
		case DISPLAYING_MONTH:
			xv_set(canvas, XV_HELP_DATA, "calentool:MonthDisplay", 0);
			month_inputevent(window, event);
			break;
		case DISPLAYING_YEAR:
			xv_set(canvas, XV_HELP_DATA, "calentool:YearDisplay", 0);
			year_inputevent(window, event);
			break;
	}
}

void
month_inputevent(window, event)
Xv_window window;
Event *event;
{
        int i, x, y, week_index;

	if ((event_id(event) == KEY_TOP(6) || event_action(event) == ACTION_GO_PAGE_BACKWARD)
	    && event_is_up(event)) {
		lastmonth();
		return;
	} else if ((event_id(event) == KEY_TOP(7) || event_action(event) == ACTION_GO_PAGE_FORWARD)
	    && event_is_up(event)) {
		nextmonth();
		return;
	}

	/* translate coordinates to pixwin space */
        x = event_x(event);
        y = event_y(event);          
        if (event_id(event) != MS_LEFT)
                return;

        if (event_is_up(event))  {   /* Button up. */
		fix_current_day();
                if (selected_type == DAY) {
                        mainsw_state = DISPLAYING_DAY;
                        xv_set(window, WIN_CURSOR, day_cursor, 0);
			draw_day();
                }
                else if (selected_type == WEEK) {
			mainsw_state = DISPLAYING_WEEK;
			xv_set(window, WIN_CURSOR, week_cursor, 0);
			draw_week();
		}
		return;
	}

	/* Button down. */
        selected_type = NONE;
	drawable = (Drawable)xv_get(window, XV_XID);
        for (i=0; i<monthlength(current.tm_mon); i++) {   /* In a day? */
                if ((x >= boxlims[i].lowx) &&
                    (x <= boxlims[i].highx) &&
                    (y >= boxlims[i].lowy) &&
                    (y <= boxlims[i].highy)) {

				current.tm_mday = i + 1;
                                selected_type = DAY;
				XSetFunction(mydisplay, gcc, GXinvert);
				XFillRectangle(mydisplay, drawable, gcc,
					boxlims[i].lowx+3,
					boxlims[i].lowy+3,
					58,
					58);
                                return;
                }              
        }                      
	XSetFunction(mydisplay, gcc, GXor);
        for (i=0; i<6; i++) {              /* No.  In a week? */
                if (week_arrows[i].active == 0)
                        return;
                if ((x >= week_arrows[i].left) &&
                    (x <= week_arrows[i].right) &&
                    (y >= week_arrows[i].top) &&
                    (y <= week_arrows[i].bottom))  {
			week_index = i;
			current.tm_mday = -current.tm_wday + 1 + (7 * week_index);
			if (monday_first) {
				current.tm_mday++;
				if (current.tm_wday == SUN)
					current.tm_mday -= 7;
			}
                        selected_type = WEEK;
			XCopyPlane(mydisplay, smallarrow_pr, drawable, gcc,
				0, 0,
				42, 29,
				week_arrows[week_index].left,
				week_arrows[week_index].top, 1L);
                        return;
                }              
        }                      
}                               

void
year_inputevent(window, event)
Xv_window window;
Event *event;
{
	int x, y, i;

	if ((event_id(event) == KEY_TOP(6) || event_action(event) == ACTION_GO_PAGE_BACKWARD)
	    && event_is_up(event)) {
		lastyear();
		return;
	} else if ((event_id(event) == KEY_TOP(7) || event_action(event) == ACTION_GO_PAGE_FORWARD)
	    && event_is_up(event)) {
		nextyear();
		return;
	}

	/* translate coordinates to pixwin space */
        x = event_x(event);
        y = event_y(event);          
        if (event_id(event) != MS_LEFT)
                return;
        if (event_is_up(event))  {       /* Button up. */
		if (selected_type == MONTH) {
			mainsw_state = DISPLAYING_MONTH;
			xv_set(window, WIN_CURSOR, month_cursor, 0);
			draw_month();
		}
		return;
	}

	/* Button down. */
	selected_type = NONE;
        for (i=0; i<12; i++) {                   /* In a month? */
                if ((x >= mboxlims[i].lowx) &&
                    (x <= mboxlims[i].highx) &&
                    (y >= mboxlims[i].lowy) &&
                    (y <= mboxlims[i].highy)) {
			selected_type = MONTH;
			current.tm_mday = 1;
			current.tm_mon = i;
			drawable = (Drawable)xv_get(window, XV_XID);
			XSetFunction(mydisplay, gcc, GXinvert);
			XFillRectangle(mydisplay, drawable, gcc,
				mboxlims[i].lowx,
				mboxlims[i].lowy,
				7*ybox_width,
				ybox_height-1);
			break;
		}
	}
}

void
dismiss_event_proc(window, event)
Xv_window window;
Event *event;
{
	if (event_action(event) == ACTION_DISMISS)
		xv_set(window, FRAME_CMD_PUSHPIN_IN, FALSE,
			XV_SHOW, FALSE, 0);
}

Notify_value
check_close(client, event, arg, when)
Notify_client client;
Event *event;
Notify_arg arg;
Notify_event_type when;
{
	/* check for L7 key and close frame if found */
	if (event_action(event) == ACTION_CLOSE)
		return (myframe_interposer(client, event, arg, when));
	else
		return (notify_next_event_func(client, (Notify_event)event, arg, when));
}

void
close_frame()
{
	/* save some information as we close */
	if (mainsw_state == DISPLAYING_DAY && day_is_open)
		close_day();
	/* if frame not closed yet, close it now (for the canvas) */
	if (!(int)xv_get(frame, FRAME_CLOSED))
		xv_set(frame, FRAME_CLOSED, TRUE, 0);
	olddate = current;
	get_today();
	closedate = today;
	if (fframe) {
		/* kill off future appt popup */
		xv_destroy_safe(fframe);
		fframe = 0;
	}
#ifndef NO_SUN_MOON
	/* kill sun/moon data frames */
	if (mframe)
		mframe_done(0);
	if (sframe)
		sframe_done(0);
#endif
	do_icon = 1;
}

void
set_icon()
{
	GC icon_gc;
	XGCValues icon_gc_values;

	/* create GC for the icon */
	icon_gc_values.foreground = foregr;
	icon_gc_values.background = backgr;
	icon_gc_values.font = ((XFontStruct *)xv_get(sfont, FONT_INFO))->fid;
	icon_gc_values.function = GXcopy;
	icon_gc = XCreateGC(mydisplay, iconPixmap,
		GCBackground|GCForeground|GCFunction|GCFont, &icon_gc_values);

	/* choose the icon to set */
	switch (icon_in_use) {
		case -1:
		case STD_ICON:
			XCopyArea(mydisplay, icon,
				iconPixmap, icon_gc, 0, 0, 64, 64, 0, 0);
			break;
		case REV_ICON:
			XCopyArea(mydisplay, rev_icon,
				iconPixmap, icon_gc, 0, 0, 64, 64, 0, 0);
			break;
		case NA_ICON:
			XCopyArea(mydisplay, na_icon,
				iconPixmap, icon_gc, 0, 0, 64, 64, 0, 0);
			break;
	}

	/* add in the date */
	sprintf(datestr_day, "%d", today.tm_mday);

	if (icon_in_use == REV_ICON) {
		/* switch fg and bg colors */
		XSetForeground(mydisplay, icon_gc, backgr);
		XSetBackground(mydisplay, icon_gc, foregr);
	}

	XDrawString(mydisplay, iconPixmap, icon_gc, 13, 54, datestr_day,
		strlen(datestr_day));
	XDrawString(mydisplay, iconPixmap, icon_gc, 39, 54,
		smonthnames[today.tm_mon], strlen(smonthnames[today.tm_mon]));
	XCopyArea(mydisplay, iconPixmap, icon_window, gc, 0, 0, 64, 64, 0, 0);
	XFreeGC(mydisplay, icon_gc);

	if (show_time)
		update_icon_time();
	
	/* Due to some interaction between XView3 and twm/fvwm, the
	   value for the icon_window WM hint is altered. To remedy this,
	   we always reset the hints before the icon is re-displayed.
	   (from Damian Chu <dac@doc.imperial.ac.uk>).
	*/
	XSetWMHints(mydisplay, xv_get(frame, XV_XID), myWMHints);
}

/* update the time field of the current icon */
void
update_icon_time()
{
	format_icon_time();

	/* set the space to the background color */
        XSetFunction(mydisplay, gc, GXcopy);
	XSetForeground(mydisplay, gc, backgr);
        XFillRectangle(mydisplay, iconPixmap, gc, 0, 64, 64, 14);

	/* draw a box around the time appendage */
	XSetFunction(mydisplay, gc, GXcopy);
	XSetForeground(mydisplay, gc, foregr);
	XDrawLines(mydisplay, iconPixmap, gc, icon_tbox, 5,
			CoordModeOrigin);

	XDrawString(mydisplay, iconPixmap, gc, ilabel.x, ilabel.y,
		timestr, strlen(timestr));
	if (xv_get(frame, FRAME_CLOSED) == TRUE) {
		XCopyArea(mydisplay, iconPixmap, icon_window, gc, 0, 0, 64,
			(show_time ? 77 : 64), 0, 0);
	}
}

void
format_icon_time()
{
	if (update_interval >= 60)
		/* display hh:mm */
		sprintf(timestr, " %2d:%02d", today.tm_hour, today.tm_min);
	else
		/* display hh:mm:ss */
		sprintf(timestr, " %2d:%02d:%02d", today.tm_hour, today.tm_min, today.tm_sec);
	if (!hour24) {
		/* display am/pm for 12-hour time */
		if (today.tm_hour > 12) {
			strcat(timestr, "pm");
			timestr[1] = ((today.tm_hour - 12) / 10) + '0';
			timestr[2] = ((today.tm_hour - 12) % 10) + '0';
		} else if (today.tm_hour == 12) {
			strcat(timestr, "pm");
		} else {
			strcat(timestr, "am");
		}
		if (timestr[1] == '0')
			timestr[1] = ' ';
	}
}
