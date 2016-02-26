/*
 * $Id: notify.c,v 2.3 1994/08/29 17:34:39 billr Exp $
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
/**********************************************************
 *							  *
 *	 These are the notify routines which are invoked  *
 * by events in the control panel subwindow,and the	  *
 * various popup window panels.				  *
 *							  *
 **********************************************************/



#include <stdio.h>
#include <xview/xview.h>
#include <xview/canvas.h>
#include <xview/cursor.h>
#include <xview/defaults.h>
#include <xview/notice.h>
#include <xview/openmenu.h>
#include <xview/panel.h>
#include <xview/font.h>
#include "ct.h"
#include "xv_ct.h"

struct _print_menu_client_data {
	Panel_item item;
	Event *event;
	} print_menu_client_data;

extern int monday_first;
extern int mainsw_state, nr_weekdays, day_is_open;
extern int dayslot_width, nr_weekdays;
extern struct tm today, current;
extern Xv_Cursor month_cursor, week_cursor, day_cursor, year_cursor;
extern Xv_Font font;
extern Frame frame;
extern Panel panel;
extern Canvas canvas;
extern Menu next_menu, previous_menu, year_menu, month_menu;
extern Menu day_menu, week_menu, current_menu;
extern Menu done_menu;
extern Frame fframe;
extern Frame attr_frame;
extern Panel_item repeat_pi, remind_pi, daysel_pi;
extern Panel_item everyx_pi, whichwk_pi, marked_pi;
extern Panel_item setdate_pi;
extern Panel_item runl_pi, advw_pi;
#ifndef NO_SUN_MOON
extern Frame mframe, sframe;
extern Panel_item moonbutton_pi, sunbutton_pi;
#endif
extern Frame fileframe;
extern Panel_item filename_pi, file_ro_pi, file_ac_pi;
#ifndef NO_PRINTER
extern Panel_item prcmd_pi, prfile_pi, prfname_pi;
extern int print_to_file;
extern Frame prframe;
extern Menu print_menu;
extern char psfile[];
#endif
extern Frame prompt_frame, date_frame;
extern struct appt_entry future[];
extern struct dayslot *slots;
extern int attr_bi;  /* index into currently active day slot */
extern int new_entry;
extern struct tm olddate;
extern int otherfile, read_only;
extern char *othername, apts_pathname[], orig_apts_pathname[];
extern int orig_ro;
extern char printer[];
extern int show_future;
extern int show_time;
extern int update_interval;
extern struct appt_entry shelf_appt;
extern int day_first;
extern int do_icon;
extern void close_frame();
extern Notify_value leave();

int i;


Xv_opaque done_menu_proc(menu, menu_item)
Menu menu;
Menu_item menu_item;
{
	int value;

	/* fetch the value of the item selected */
	value = (int) xv_get(menu_item, MENU_VALUE);
	if (value == 1)
		close_frame();
	else
		(void)leave((Notify_client)0, 0, (Notify_signal_mode)0);
	return XV_OK;
}

int
weekbutton_notify(item, event)
Panel_item item;
Event *event;
{
	if (event_id(event) == MS_LEFT && event_is_up(event)) {
		/* current week */
		xv_set(week_menu, XV_KEY_DATA, MENU_KEY, 1, 0);
		if (mainsw_state != DISPLAYING_WEEK) {
			if (mainsw_state == DISPLAYING_DAY)
				close_day();
			mainsw_state = DISPLAYING_WEEK;
			xv_set(xv_get(canvas, CANVAS_NTH_PAINT_WINDOW, 0),
				WIN_CURSOR, week_cursor, 0);
		}
		draw_week();
	} else
		/* event not handled */
		xv_set(week_menu, XV_KEY_DATA, MENU_KEY, 0, 0);
	return XV_OK;
}

Menu
week_menu_gen(menu, oper)
Menu menu;
Menu_generate oper;
{
	int i, w, wkday, mday;
	Menu_item an_item;
	struct tm Save;

	if (oper == MENU_DISPLAY) {
		if (mainsw_state > DISPLAYING_MONTH) {
			/* make all entries inactive */
			for (i=1; i<=6; i++) {
				an_item = xv_get(menu, MENU_NTH_ITEM, i);
				xv_set(an_item, MENU_INACTIVE, TRUE, 0);
			}
		} else {
			/* make all entries active */
			for (i=1; i<=6; i++) {
				an_item = xv_get(menu, MENU_NTH_ITEM, i);
				xv_set(an_item, MENU_INACTIVE, FALSE, 0);
			}
		}
		/* get current week in month */
		Save = current;
		wkday = current.tm_wday;
		mday = current.tm_mday;
		w = current.tm_mday / 7 + 1;
		if (monday_first && wkday == SUN) {
			/*
			 * On monday_first calendars, Sunday is the end
			 * of the previous week, so if we are looking at
			 * Sunday month 1 then fake it into thinking
			 * this is the last day of the previous month
			 */
			if (--w == 0) {
				current.tm_mday--;
				fix_current_day();
				mday = current.tm_mday;
				w = current.tm_mday / 7 + 1;
			}
		}
		/* get day of week of first day of the month */
		current.tm_mday = 1;
		fix_current_day();
		if (mday % 7 > (7 - current.tm_wday))
			w++;
		xv_set(menu, MENU_DEFAULT, w, 0);
		current = Save;
	}
	return menu;
}

Xv_opaque week_menu_proc(menu, menu_item)
Menu menu;
Menu_item menu_item;
{
	int value;

	if ((int)xv_get(menu, XV_KEY_DATA, MENU_KEY)) {
		/* already handled by button notify proc */
		/* clear event handled flag */
		xv_set(menu, XV_KEY_DATA, MENU_KEY, 0, 0);
		return XV_OK;
	}

	/* fetch the value of the item selected */
	value = (int) xv_get(menu_item, MENU_VALUE);
	if (value > 0) {
		/* process it */
		current.tm_mday = (value - 1) * 7 + 1;
		if (current.tm_mday > monthlength(current.tm_mon))
			current.tm_mday = monthlength(current.tm_mon);
	}
	if (mainsw_state != DISPLAYING_WEEK) {
		if (mainsw_state == DISPLAYING_DAY)
			close_day();
		mainsw_state = DISPLAYING_WEEK;
		xv_set(xv_get(canvas, CANVAS_NTH_PAINT_WINDOW, 0),
			WIN_CURSOR, week_cursor, 0);
	}
	draw_week();
	return XV_OK;
}

void
lastmonth()
{

	current.tm_mon -= 1;
	current.tm_mday = 1;
	draw_month();
}

void
nextmonth()
{ 
	current.tm_mon += 1;
	current.tm_mday = 1;
        draw_month();
} 
 
Menu
month_menu_gen(menu, oper)
Menu menu;
Menu_generate oper;
{
	if (oper == MENU_DISPLAY) {
		xv_set(menu, MENU_DEFAULT, current.tm_mon + 1, 0);
	}
	return menu;
}

Xv_opaque month_menu_proc(menu, menu_item)
Menu menu;
Menu_item menu_item;
{
	int value;

	/* fetch the value of the item selected from the menu_item */
	value = (int) xv_get(menu_item, MENU_VALUE);

	/* process it */
	if (value > 0) {
		current.tm_mon = value - 1;
	}
	if (mainsw_state != DISPLAYING_MONTH) {
		if (mainsw_state == DISPLAYING_DAY)
			close_day();
		mainsw_state = DISPLAYING_MONTH;
		xv_set(xv_get(canvas, CANVAS_NTH_PAINT_WINDOW, 0),
			WIN_CURSOR, month_cursor, 0);
	}
	draw_month();
	return XV_OK;
}

void
lastyear()
{
	current.tm_mday = 1;
	current.tm_mon = JAN;
	current.tm_year -= 1;
	draw_year();
}

void
nextyear()
{
	current.tm_mday = 1;
	current.tm_mon = JAN;
	current.tm_year += 1;
	draw_year();
}

Menu
year_menu_gen(menu, oper)
Menu menu;
Menu_generate oper;
{
	Menu_item item;
	static char str[5];
	static int special = 0;

	/*
	 * XView bug? : this routine gets called with oper==MENU_DISPLAY twice
	 * when the Year button is selected (rather than menu'ed). Thus the
	 * check for 'special' below to prevent adding an extra menu entry due
	 * to the extra time thru this routine.
	 */
	if (oper == MENU_DISPLAY && !special) {
		if (current.tm_year < START_YEAR || current.tm_year >= START_YEAR+NR_YEARS) {
			/* create new default entry for current year */
			sprintf(str, "%4d", current.tm_year+1900);
			item = (Menu_item)xv_create(XV_NULL, MENUITEM, MENU_STRING,
					str, MENU_VALUE, -1,
					NULL);
			xv_set(menu, MENU_INSERT, 0, item, NULL);
			xv_set(menu, MENU_DEFAULT, 1, NULL);
			special = 1;
		} else {
			xv_set(menu, MENU_DEFAULT, (current.tm_year-START_YEAR+1), 0);
			special = 0;
		}
	} else if ((oper == MENU_DISPLAY_DONE || oper == MENU_NOTIFY_DONE) && special) {
		/* remove special entry created earlier */
		xv_set(menu, MENU_REMOVE, 1, NULL);
		special = 0;
	}
	return menu;
}

Xv_opaque year_menu_proc(menu, menu_item)
Menu menu;
Menu_item menu_item;
{
	int value;

	/* obtain the value of the selected item */
	value = (int) xv_get(menu_item, MENU_VALUE);

	/* process the selection */
	if (value > 0) {
		current.tm_year = START_YEAR + value - 1;
	}
	if (mainsw_state != DISPLAYING_YEAR) {
		if (mainsw_state == DISPLAYING_DAY) 
			close_day(); 
		mainsw_state = DISPLAYING_YEAR; 
		xv_set(xv_get(canvas, CANVAS_NTH_PAINT_WINDOW, 0),
			WIN_CURSOR, year_cursor, 0);
	}
	draw_year(); 
	return XV_OK;
}


void
lastweek()
{
	if (mainsw_state == DISPLAYING_DAY) {
		close_day();
		current.tm_mday -= 7;
		draw_day();
	} else if (mainsw_state == DISPLAYING_WEEK) {
		current.tm_mday -= 7;
		draw_week();
	}
}


void
nextweek()
{
        if (mainsw_state == DISPLAYING_DAY) {
                close_day();
		current.tm_mday += 7;
                draw_day();
        } else if (mainsw_state == DISPLAYING_WEEK) {
		current.tm_mday += 7;
                draw_week();
	}
}



void
yesterday()
{
	if (mainsw_state != DISPLAYING_DAY)
		return;
	close_day();
	current.tm_mday--;
	draw_day();
}


int
todaybutton_notify(item, event)
Panel_item item;
Event *event;
{
	if (event_id(event) == MS_LEFT && event_is_up(event)) {
		if (mainsw_state == DISPLAYING_DAY)
			close_day();
		else {
			mainsw_state = DISPLAYING_DAY;
			xv_set(xv_get(canvas, CANVAS_NTH_PAINT_WINDOW, 0),
				WIN_CURSOR, day_cursor, 0);
		}
		get_today();
		current = today;
		draw_day();
	}
	return XV_OK;
}



void
tomorrow()
{ 
        if (mainsw_state != DISPLAYING_DAY)
                return;
        close_day(); 
	current.tm_mday++;
        draw_day(); 
} 
 
Menu
current_menu_gen(menu, oper)
Menu menu;
Menu_generate oper;
{
	if (oper == MENU_DISPLAY) {
		xv_set(menu, MENU_DEFAULT, mainsw_state, 0);
	}
	return menu;
}

Xv_opaque current_menu_proc(menu, menu_item)
Menu menu;
Menu_item menu_item;
{
	int value;
	char date[9];
	struct tm Save;

	/* extract the value of the menu item selected */
	value = (int) xv_get(menu_item, MENU_VALUE);

	/* process the selection */
	if (mainsw_state == DISPLAYING_DAY)
		close_day();
	get_today();
	Save = current;
	current = today;
	switch (value) {
		case 1:	/* current day */
			mainsw_state = DISPLAYING_DAY;
			xv_set(xv_get(canvas, CANVAS_NTH_PAINT_WINDOW, 0),
				WIN_CURSOR, day_cursor, 0);
			draw_day();
			break;

		case 2:	/* current week */
			mainsw_state = DISPLAYING_WEEK;
			xv_set(xv_get(canvas, CANVAS_NTH_PAINT_WINDOW, 0),
				WIN_CURSOR, week_cursor, 0);
			draw_week();
			break;

		case 3:	/* current month */
			mainsw_state = DISPLAYING_MONTH;
			xv_set(xv_get(canvas, CANVAS_NTH_PAINT_WINDOW, 0),
				WIN_CURSOR, month_cursor, 0);
			draw_month();
			break;

		case 4:	/* current year */
			mainsw_state = DISPLAYING_YEAR;
			xv_set(xv_get(canvas, CANVAS_NTH_PAINT_WINDOW, 0),
				WIN_CURSOR, year_cursor, 0);
			draw_year();
			break;

		case 5:	/* change date */
			switch (day_first) {
			    case 1:
				sprintf(date, "%d/%d/%02d", Save.tm_mday, Save.tm_mon+1, Save.tm_year);
				break;
			    case 2:
				sprintf(date, "%02d-%02d%02d", Save.tm_year, Save.tm_mon+1, Save.tm_mday);
				break;
			    case 0:
			    default:
				sprintf(date, "%d/%d/%02d", Save.tm_mon+1, Save.tm_mday, Save.tm_year);
				break;
			}
			if (!date_frame)
				create_date_frame();
			xv_set(setdate_pi, PANEL_VALUE, date, 0);
			xv_set(date_frame,
				FRAME_CMD_PUSHPIN_IN, TRUE,
				XV_SHOW, TRUE,
				0);
			break;
	}
	return XV_OK;
}

/*
 * notifier for set date frame "Apply" button
 */
void
dtdone_proc(item, event)
Panel_item item;
Event *event;
{
	if (event_id(event) == MS_LEFT) {
		if (!parse_date((char *) xv_get(setdate_pi, PANEL_VALUE), FALSE)) {
			mainsw_state = DISPLAYING_DAY;
			xv_set(xv_get(canvas, CANVAS_NTH_PAINT_WINDOW, 0),
				WIN_CURSOR, day_cursor, 0);
			draw_day();
		}
		xv_set(date_frame,
			FRAME_CMD_PUSHPIN_IN, FALSE,
			XV_SHOW, FALSE,
			0);
	}
}

/*
 * notifier for set date frame "Reset" button
 */
void
dtreset_proc(item, event)
Panel_item item;
Event *event;
{
	char date[9];

	switch (day_first) {
	    case 1:
		sprintf(date, "%d/%d/%02d", current.tm_mday, current.tm_mon+1, current.tm_year);
		break;
	    case 2:
		sprintf(date, "%02d-%02d%02d", current.tm_year, current.tm_mon+1, current.tm_mday);
		break;
	    case 0:
	    default:
		sprintf(date, "%d/%d/%02d", current.tm_mon+1, current.tm_mday, current.tm_year);
		break;
	}
	xv_set(setdate_pi, PANEL_VALUE, date, 0);
}

/* "done" from subframe menu of change date frame */
void
dtframe_done(frame)
Frame frame;
{
	xv_set(date_frame, FRAME_CMD_PUSHPIN_IN, FALSE, XV_SHOW, FALSE, 0);
}

int
daybutton_notify(item, event)
Panel_item item;
Event *event;
{
	if (event_id(event) == MS_LEFT && event_is_up(event)) {
		/* we handled the event */
		xv_set(day_menu, XV_KEY_DATA, MENU_KEY, 1, 0);
		mainsw_state = DISPLAYING_DAY;
		draw_day();
		xv_set(xv_get(canvas, CANVAS_NTH_PAINT_WINDOW, 0),
			WIN_CURSOR, day_cursor, 0);
	} else
		/* event not handled */
		xv_set(day_menu, XV_KEY_DATA, MENU_KEY, 0, 0);
	return XV_OK;
}

Menu
day_menu_gen(menu, oper)
Menu menu;
Menu_generate oper;
{
	int i;
	Menu_item an_item;

	if (oper == MENU_DISPLAY) {
		if (mainsw_state > DISPLAYING_WEEK) {
			/* make all entries inactive */
			for (i=1; i<=7; i++) {
				an_item = xv_get(menu, MENU_NTH_ITEM, i);
				xv_set(an_item, MENU_INACTIVE, TRUE, 0);
			}
		} else {
			/* make all entries active */
			for (i=1; i<=7; i++) {
				an_item = xv_get(menu, MENU_NTH_ITEM, i);
				xv_set(an_item, MENU_INACTIVE, FALSE, 0);
			}
		}
		xv_set(menu, MENU_DEFAULT, current.tm_wday+1, NULL);
	}
	return menu;
}

Xv_opaque day_menu_proc(menu, menu_item)
Menu menu;
Menu_item menu_item;
{
	int value;

	if ((int)xv_get(menu, XV_KEY_DATA, MENU_KEY)) {
		/* already handled by button notify proc */
		/* clear event handled flag */
		xv_set(menu, XV_KEY_DATA, MENU_KEY, 0, 0);
		return XV_OK;
	}

	/* fetch the value of the item selected */
	value = (int) xv_get(menu_item, MENU_VALUE);

	/* process the selection */
	/* find selected day in this week */
	if (monday_first) {
		++value;
		if (current.tm_wday == 0)
			current.tm_wday = 7;
	}
	if (--value > current.tm_wday)
		current.tm_mday += value - current.tm_wday;
	else
		current.tm_mday -= current.tm_wday - value;
	mainsw_state = DISPLAYING_DAY;
	draw_day();
	xv_set(xv_get(canvas, CANVAS_NTH_PAINT_WINDOW, 0),
		WIN_CURSOR, day_cursor, 0);
	return XV_OK;
}

void
first_wkday()
{
	/*
	 * Set day to first displayable day of the week selected.
	 * If we have a 7-day week display, then it will always
	 * be the first day of the month. If we have a 5 or 6 day
	 * display, the first day may need to be adjusted to the
	 * following monday.
	 */
	if (nr_weekdays == 7)
		/* it's ok as is */
		return;
	fix_current_day();	/* update wkday, etc. */
	if (current.tm_wday == SUN)
		current.tm_mday++;
	else if (current.tm_wday > nr_weekdays)
		current.tm_mday += 7 - current.tm_wday + 1;
}

Xv_opaque next_menu_proc(menu, menu_item)
Menu menu;
Menu_item menu_item;
{
	int value;

	/* fetch the value of the selection */
	value = (int) xv_get(menu_item, MENU_VALUE);

	/* process the selection */
	value--;
	switch (mainsw_state) {
		case DISPLAYING_DAY:
			switch (value) {
				case 0:	/* day */
					tomorrow();
					break;
				case 1: /* week */
					close_day();
					current.tm_mday += 7;
					draw_day();
					break;
				case 2:	/* month */
					close_day();
					current.tm_mon++;
				/* make sure day ends up in proper month */
					if (current.tm_mday == monthlength(
						current.tm_mon-1))
						/* last day of month */
						current.tm_mday = monthlength(
							current.tm_mon%12);
					else if (current.tm_mday > 
						monthlength(current.tm_mon%12))
						current.tm_mday = monthlength(
							current.tm_mon%12);
					draw_day();
					break;
				case 3: /* year */
					close_day();
					current.tm_year++;
					draw_day();
					break;
			}
			break;
		case DISPLAYING_WEEK:
			switch (value) {
				case 1:	/* week */
					nextweek();
					break;
				case 2:	/* month */
					current.tm_mon++;
					draw_week();
					break;
				case 3:	/* year */
					current.tm_year++;
					draw_week();
					break;
			}
			break;
		case DISPLAYING_MONTH:
			switch (value) {
				case 2: /* month */
					nextmonth();
					break;
				case 3:	/* year */
					current.tm_year++;
					draw_month();
					break;
			}
			break;
		case DISPLAYING_YEAR:
			if (value == 3)
				nextyear();
			break;
	}
	return XV_OK;
}

/* common menu gen proc for next and previous menus */
Menu
next_prev_menu_gen(menu, oper)
Menu menu;
Menu_generate oper;
{
	Menu_item an_item;

	if (oper == MENU_DISPLAY) {
		i = 0;
		while (++i < mainsw_state) {
			an_item = xv_get(menu, MENU_NTH_ITEM, i);
			xv_set(an_item, MENU_INACTIVE, TRUE, 0);
		}
		for (i=mainsw_state; i<=DISPLAYING_YEAR; i++) {
			an_item = xv_get(menu, MENU_NTH_ITEM, i);
			xv_set(an_item, MENU_INACTIVE, FALSE, 0);
		}
		xv_set(menu, MENU_DEFAULT, mainsw_state, 0);
	}
	return menu;
}

Xv_opaque previous_menu_proc(menu, menu_item)
Menu menu;
Menu_item menu_item;
{
	int value;

	/* fetch the selected value from the menu_item */
	value = (int) xv_get(menu_item, MENU_VALUE);

	/* process the selection */
	if (value == 0)
		return XV_OK;	/* no selection made */
	value--;
	switch (mainsw_state) {
		case DISPLAYING_DAY:
			switch (value) {
				case 0:	/* day */
					yesterday();
					break;
				case 1: /* week */
					close_day();
					current.tm_mday -= 7;
					draw_day();
					break;
				case 2:	/* month */
					close_day();
					current.tm_mon--;
				/* make sure day ends up in proper month */
					if (current.tm_mday == monthlength(current.tm_mon+1))
						/* last day of month */
						current.tm_mday = monthlength((current.tm_mon+12)%12);
					else if (current.tm_mday > monthlength((current.tm_mon+12)%12))
						current.tm_mday = monthlength((current.tm_mon+12)%12);
					draw_day();
					break;
				case 3: /* year */
					close_day();
					current.tm_year--;
					draw_day();
					break;
			}
			break;
		case DISPLAYING_WEEK:
			switch (value) {
				case 1:	/* week */
					lastweek();
					break;
				case 2:	/* month */
					current.tm_mon--;
					draw_week();
					break;
				case 3:	/* year */
					current.tm_year--;
					draw_week();
					break;
			}
			break;
		case DISPLAYING_MONTH:
			switch (value) {
				case 2: /* month */
					lastmonth();
					break;
				case 3:	/* year */
					current.tm_year--;
					draw_month();
					break;
			}
			break;
		case DISPLAYING_YEAR:
			if (value == 3)
				lastyear();
			break;
	}
	return XV_OK;
}

#ifndef NO_SUN_MOON
int
moonbutton_notify(item, event) 
Panel_item item; 
Event *event; 
{ 
	if (event_id(event) == MS_LEFT && event_is_up(event)) {
		/* display popup frame with moon data */
		moon_data_frame();
		xv_set(moonbutton_pi, XV_SHOW, FALSE, 0);
	}
	return XV_OK;
}

int
sunbutton_notify(item, event) 
Panel_item item; 
Event *event; 
{ 
	if (event_id(event) == MS_LEFT && event_is_up(event)) {
		/* display popup frame with moon data */
		sun_data_frame();
		xv_set(sunbutton_pi, XV_SHOW, FALSE, 0);
	}
	return XV_OK;
}
#endif

Notify_value
myframe_interposer(client, event, arg, type)
Notify_client client;
Event *event;
Notify_arg arg;
Notify_event_type type;
{
	static int start_up = 1;
	int closed_initial, closed_current;
	Notify_value value;
	void repaint_canvas();

	/* get initial state */
	closed_initial = (int)xv_get(frame, FRAME_CLOSED);

	/* let the frame do its thing */
	value = notify_next_event_func(client, (Notify_event)event, arg, type);

	/* get new state */
	closed_current = (int)xv_get(frame, FRAME_CLOSED);
	if (start_up) {
		/* first time thru */
		start_up = 0;
		if (closed_initial) {
			/* starting up iconic */
			olddate = current;
			set_icon();
		}
	} else if (closed_current != closed_initial) {
		/* it changed state - either opened or closed */
		if (closed_current) {
			/* frame just closed */
			close_frame();
		} else {
			/* frame just opened */
			repaint_canvas(canvas, NULL, NULL, NULL, NULL);
		}
	} else if (closed_initial && closed_current && do_icon) {
		do_icon = 0;
		check_calendar();	/* get proper icon */
		set_icon();
	}


	return(NOTIFY_DONE);
}

/*
 * notifier for "Done" button in the popup future appt frame
 */
void
fdone_proc(item, event)
Panel_item item;
Event *event;
{
	if (!xv_get(fframe, FRAME_CMD_PUSHPIN_IN))
		xv_set(fframe, XV_SHOW, FALSE, 0);
	show_future = 0;
}

/*
 * notifier for "Keep" button in the popup future appt frame
 */
void
fkeep_proc(item, event)
Panel_item item;
Event *event;
{
	if (!xv_get(fframe, FRAME_CMD_PUSHPIN_IN))
		xv_set(fframe, XV_SHOW, FALSE, 0);
}

/*
 * Notifier for future appts. We get here when the user
 * selects one of the displayed messages. When this happens,
 * the day display for the selected future appt is displayed.
 */
int
fappt_notify(item, event)
Panel_item item;
Event *event;
{
	int value;

	if (event_id(event) == MS_LEFT) {
		value = (int)xv_get(item, XV_KEY_DATA, PANEL_ITEM_KEY);
		/* set current date to match the selected appt */
		current.tm_year = future[value].year;
		current.tm_mon = future[value].month;
		current.tm_mday = future[value].day;
		fix_current_day();
		xv_destroy_safe(fframe);
		fframe = 0;

		/* draw new day page */
		draw_day();
	}
	return XV_OK;
}

/*
 * Notify routine for everyx panel item in the attributes
 * popup window. In this routine, we only care about the state
 * of the "Selected Week" choice, which determines which panel
 * item is displayed.
 */
int
everyx_notify(item, event)
Panel_item item;
Event *event;
{
	int value;

	value = (int) xv_get(everyx_pi, PANEL_VALUE);
	if (value & 0x4) {
		if (value & 0x3) {
			/* not allowed for ALL_DAYS or EVERY_MON_FRI */
			value &= ~0x4;
			xv_set(everyx_pi, PANEL_VALUE, value, 0);
		} else {
			xv_set(repeat_pi, XV_SHOW, FALSE, 0);
			xv_set(whichwk_pi, XV_SHOW, TRUE, 0);
		}
	} else {
		xv_set(whichwk_pi, XV_SHOW, FALSE, 0);
		xv_set(repeat_pi, XV_SHOW, TRUE, 0);
	}
	return XV_OK;
}

/*
 * Notify routine for the appointment attributes popup window.
 * Since each panel item does not have its own notify routine,
 * we check the current state of everything when the user
 * selects the accept button and set the slot flags appropriately.
 */
void
attr_accept(item, event)
Panel_item item;
Event *event;
{
	int value, flag = 0, repeat = 0;
	int oflag;
	struct appt_entry *apt = slots[attr_bi].cur_appt;

	if (event_id(event) != MS_LEFT)
		return;  /* ignore everything else */
	
	oflag = apt->flags;

	/* get the everyx value (every day, week, month, year) */
	value = (int) xv_get(everyx_pi, PANEL_VALUE);
	/* value is bitmap of selected choices */
	/* EVERY_MON_FRI and ALL_DAYS are mutually exclusive */
	if (value & 0x1)
		flag |= EVERY_MON_FRI;
	else if (value & 0x2)
		flag |= ALL_DAYS;
	else if (oflag & (EVERY_MON_FRI|ALL_DAYS))
		apt->day = current.tm_mday;
	if (value & 0x4)
		flag |= Setday(current.tm_wday);
	else if (oflag & EVERY_SOMEDAY)
		apt->day = current.tm_mday;
	if (value & 0x8)
		flag |= ALL_MONTHS;
	else if (oflag & ALL_MONTHS)
		apt->month = current.tm_mon;
	if (value & 0x10)
		flag |= ALL_YEARS;
	else if (oflag & ALL_YEARS)
		apt->year = current.tm_year;
	
	if (value & 0x4) {
		/* repeat at week intervals selected by which week pi */
		value = (int) xv_get(whichwk_pi, PANEL_VALUE);
		if (value == 0 || value == 0x40)
			/* ALL selected or no selection */
			value = ALL_WEEKS;
		flag |= REPEAT;
		repeat = value;
	} else {
		/* get repeat interval */
		value = xv_get(repeat_pi, PANEL_VALUE);
		if (value > 0) {
			flag |= REPEAT;
			repeat = value;
		}
	}

	/* get lookahead value */
	value = xv_get(remind_pi, PANEL_VALUE);
	if (value > 0) {
		flag |= LOOKAHEAD;
		if (apt->lookahead != value) {
			new_entry = 1;
			apt->lookahead = value;
		}
	}

	/* get runlength value */
	value = xv_get(runl_pi, PANEL_VALUE);
	if (value > 0) {
		flag |= RUN;
		if (apt->runlength != value) {
			new_entry = 1;
			apt->runlength = value;
		}
	}

 	/* get advance warning value */
 	value = xv_get(advw_pi, PANEL_VALUE);
 	if (apt->warn != value) {
 		new_entry = 1;
 		apt->warn = value;
 	}

	if (oflag & A_NOTE) {
		flag |= A_NOTE;
		/* marked indicator */
		value = (int) xv_get(marked_pi, PANEL_VALUE);
		if (value == 0)
			flag |= MARKED;  /* don't show in month/yr display */
	}
	/* shouldn't really be in this routine if the appt
	 * was read only, however, this is still here for potential
	 * future use.
	 */
	if (oflag & READONLY)
		flag |= READONLY;

	if (apt->repeat != repeat || oflag != flag)
		new_entry = 1;	/* something changed */

	/* set the slot info */
	apt->repeat = repeat;
	apt->flags = flag;

	if (!xv_get(attr_frame, FRAME_CMD_PUSHPIN_IN))
		xv_set(attr_frame, XV_SHOW, FALSE, 0);
}

/*
 * abort the attribute setting process, leaving the current
 * appointment unmodified.
 */
void
attr_abort(item, event)
Panel_item item;
Event *event;
{
	if (event_id(event) == MS_LEFT) {
		set_attr();
		if (!xv_get(attr_frame, FRAME_CMD_PUSHPIN_IN))
			xv_set(attr_frame, XV_SHOW, FALSE, 0);
	}
}

#ifndef NO_SUN_MOON
/*
 * notifier for "Done" button in the popup sun data frame
 */
void
sdone_proc(item, event)
Panel_item item;
Event *event;
{
	xv_set(sframe,
		FRAME_CMD_PUSHPIN_IN, FALSE,
		XV_SHOW, FALSE,
		0);
	xv_set(sunbutton_pi, XV_SHOW, TRUE, 0);
}

/*
 * notifier for "Done" button in the popup moon data frame
 */
void
mdone_proc(item, event)
Panel_item item;
Event *event;
{
	xv_set(mframe,
		FRAME_CMD_PUSHPIN_IN, FALSE,
		XV_SHOW, FALSE,
		0);
	xv_set(moonbutton_pi, XV_SHOW, TRUE, 0);
}

/* "done" from subframe menu */
void
sframe_done(frame)
Frame frame;
{
	xv_set(sframe,
		FRAME_CMD_PUSHPIN_IN, FALSE,
		XV_SHOW, FALSE,
		0);
	xv_set(sunbutton_pi, XV_SHOW, TRUE, 0);
}

/* "done" from subframe menu */
void
mframe_done(frame)
Frame frame;
{
	xv_set(mframe,
		FRAME_CMD_PUSHPIN_IN, FALSE,
		XV_SHOW, FALSE,
		0);
	xv_set(moonbutton_pi, XV_SHOW, TRUE, 0);
}
#endif	/* NO_SUN_MOON */

/*
 * notifier for file button in main control panel
 */
int
filebutton_notify(item, event)
Panel_item item;
Event *event;
{
	if (event_id(event) == MS_LEFT) {
		if (!fileframe)
			create_file_frame();
		xv_set(filename_pi, PANEL_VALUE, apts_pathname, 0);
		xv_set(file_ro_pi, PANEL_VALUE, (read_only ? 0 : 1), 0);
		xv_set(fileframe, XV_SHOW, TRUE, 0);
	}
	return XV_OK;
}

/*
 * notifier for "Apply" button in the popup file frame
 */
void
file_accept(item, event)
Panel_item item;
Event *event;
{
	char save_name[160];
	int save_ro;
	struct tm save_date;

	if (event_id(event) == MS_LEFT) {
		/* cleanup existing appts file and open new one */
		save_date = current;
		strcpy(save_name, apts_pathname);
		save_ro = read_only;
		cleanup();
		othername = (char *)xv_get(filename_pi, PANEL_VALUE);
		otherfile = 1;
		read_only = (int) xv_get(file_ro_pi, PANEL_VALUE) == 0 ? 1 : 0;
		if (do_files(TRUE)) {
			/* error in opening new file - restore old */
			othername = save_name;
			read_only = save_ro;
			if (do_files(TRUE))
				/* can't restore original */
				err_rpt("can't restore appts file", FATAL);
		} else {
			/* read_only may have been changed by do_files() */
			xv_set(file_ro_pi, PANEL_VALUE, (read_only ? 0 : 1));
			current = save_date;
			switch(mainsw_state) {
				case DISPLAYING_DAY:
					draw_day();
					break;
				case DISPLAYING_WEEK:
					draw_week();
					break;
				case DISPLAYING_MONTH:
					draw_month();
					break;
				case DISPLAYING_YEAR:
					draw_year();
					break;
			}

			/* if we got here because the accept button was pushed,
			   hide the frame
			 */
			if (item == file_ac_pi)
				xv_set(fileframe, FRAME_CMD_PUSHPIN_IN, FALSE, 0);
		}
	}
	if (!xv_get(fileframe, FRAME_CMD_PUSHPIN_IN))
		xv_set(fileframe, XV_SHOW, FALSE, 0);
}

/*
 * notifier for "Reset" button in the popup file frame
 */
void
file_reset(item, event)
Panel_item item;
Event *event;
{
	if (event_id(event) == MS_LEFT)
		xv_set(filename_pi, PANEL_VALUE, apts_pathname, 0);
}

/*
 * notifier for "Save" button in the popup file frame
 */
void
file_save(item, event)
Panel_item item;
Event *event;
{
	if (event_id(event) == MS_LEFT) {
		othername = (char *)xv_get(filename_pi, PANEL_VALUE);
		if (!strcmp(othername, apts_pathname)) {
			/* no filename change */
			if (mainsw_state == DISPLAYING_DAY && day_is_open)
				close_day();
		}
	}
}

/*
 * notifier for "Original" button in the popup file frame
 */
void
file_orig(item, event)
Panel_item item;
Event *event;
{
	if (event_id(event) == MS_LEFT) {
		xv_set(filename_pi, PANEL_VALUE, orig_apts_pathname, 0);
		xv_set(file_ro_pi, PANEL_VALUE, (orig_ro ? 0 : 1), 0);
		/* force an Accept button event */
		file_accept(item, event);
	}
}

/*
 * "Done" from subframe menu of the file selection popup frame
 */
void
fileframe_done(frame)
Frame frame;
{
	xv_set(fileframe, XV_SHOW, FALSE, 0);
}

#ifndef NO_PRINTER
Menu
print_menu_gen(menu, oper)
Menu menu;
Menu_generate oper;
{
	int value;
#ifdef RASTER_ONLY
	Menu_item an_item;
#endif

	if (oper == MENU_DISPLAY) {
#ifdef RASTER_ONLY
		an_item = xv_get(print_menu, MENU_NTH_ITEM, PR_POSTSCRIPT);
		xv_set(an_item, MENU_INACTIVE, TRUE, 0);
#endif
		/* make previous printer type selection the default */
		value = (int)xv_get(menu, XV_KEY_DATA, MENU_KEY);
		xv_set(menu, MENU_DEFAULT, value, 0);
	}
	return menu;
}

Xv_opaque print_menu_proc(menu, menu_item)
Menu menu;
Menu_item menu_item;
{
	int value;

	/* get the value of the selected menu item */
	value = (int) xv_get(menu_item, MENU_VALUE);

	/* process the selection */
	if (value == 3) {
		/* change printer */
		if (!prframe)
			create_print_frame();
		xv_set(prframe, XV_SHOW, TRUE, 0);
	} else {
		/* save last printer type selection */
		xv_set(menu, XV_KEY_DATA, MENU_KEY, value, 0);
		print_calendar(value+1);
	}
	return XV_OK;
}

void
prdone_proc(item, event)
Panel_item item;
Event *event;
{
	char *newstr;

	if (event_id(event) == MS_LEFT && event_is_up(event)) {
		print_to_file = (int) xv_get(prfile_pi, PANEL_VALUE);
		if (!print_to_file) {
			/* print to real printer */
			newstr = (char *) xv_get(prcmd_pi, PANEL_VALUE);
			if (strcmp(printer, newstr)) {
				/* the string changed */
				strcpy(printer, newstr);
				if (notice_prompt(prframe, NULL,
					NOTICE_MESSAGE_STRINGS,
						"Printer Setting Changed -",
						"     Change defaults?",
						0,
					NOTICE_BUTTON_YES, "Change Defaults",
					NOTICE_BUTTON_NO,  "Ignore Change  ",
					0) == NOTICE_YES)
					    defaults_set_string("Calentool.printer.name", printer);
			}
		} else {
			/* print to a file */
			newstr = (char *) xv_get(prfname_pi, PANEL_VALUE);
			if (strcmp(psfile, newstr)) {
				/* the string changed */
				strcpy(psfile, newstr);
				if (notice_prompt(prframe, NULL,
					NOTICE_MESSAGE_STRINGS,
						"Filename Changed -",
						"     Change defaults?",
						0,
					NOTICE_BUTTON_YES, "Change Defaults",
					NOTICE_BUTTON_NO,  "Ignore Change  ",
					0) == NOTICE_YES)
					    defaults_set_string("Calentool.printer.filename", psfile);
			}
		}
		if (!xv_get(prframe, FRAME_CMD_PUSHPIN_IN))
			xv_set(prframe, XV_SHOW, FALSE, 0);
	}
}

void
prreset_proc(item, event)
Panel_item item;
Event *event;
{
	if (event_id(event) == MS_LEFT && event_is_up(event)) {
		xv_set(prfile_pi, PANEL_VALUE, print_to_file, 0);
		xv_set(prcmd_pi, PANEL_VALUE, printer, 0);
		xv_set(prfname_pi, PANEL_VALUE, psfile, 0);
		if (print_to_file) {
			xv_set(prcmd_pi, XV_SHOW, FALSE, 0);
			xv_set(prfname_pi, XV_SHOW, TRUE, 0);
		} else {
			xv_set(prfname_pi, XV_SHOW, FALSE, 0);
			xv_set(prcmd_pi, XV_SHOW, TRUE, 0);
		}
		if (!xv_get(prframe, FRAME_CMD_PUSHPIN_IN))
			xv_set(prframe, XV_SHOW, FALSE, 0);
	}
}

void
prframe_done(frame)
Frame frame;
{
	/* mark as no change */
	xv_set(prframe, XV_SHOW, FALSE, 0);
}

void
prchoice_proc(item, value, event)
Panel_item item;
int value;
Event *event;
{
	if (event_id(event) == MS_LEFT && event_is_up(event)) {
		if (value) {
			xv_set(prcmd_pi, XV_SHOW, FALSE, 0);
			xv_set(prfname_pi, XV_SHOW, TRUE, 0);
		} else {
			xv_set(prfname_pi, XV_SHOW, FALSE, 0);
			xv_set(prcmd_pi, XV_SHOW, TRUE, 0);
		}
	}
}
#endif	/* NO_PRINTER */
