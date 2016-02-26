/*
 * $Id: mpaint.c,v 2.3 1994/08/19 19:53:08 billr Exp $
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
 *	Artistic routines that draw in the main    *
 * subwindow for the month display.		   *
 *						   *
 ***************************************************/

#include <X11/Xlib.h>
#include <xview/xview.h>
#include <xview/canvas.h>
#include <xview/font.h>
#include <ctype.h>
#include <stdio.h>
#include "ct.h"
#include "xv_ct.h"
#include "paint.h"
#ifndef NO_HOLIDAYS
extern struct appt_entry a_appts[], c_appts[];
extern struct appt_entry i_appts[], j_appts[];
extern struct appt_entry s_appts[];
#endif
XPoint tri_pts[4] = { {0,0}, {16,0}, {0,16}, {-16,-16} };

/*
 * Routine to draw month calendar.
 */

void
draw_month()
{
	int start_dow, i, j, k, x, y, n_days;
	int days_in_week;
	int arrow_index, last_top;
	char c[4], title[20];
	int left_border, top_border;
	Rect *rect;
	int busy_today[31];
	FILE *apts;
	int read_stat;
	struct tm Save;
	struct appt_entry appt;
	int runl;

	lock_cursors();
	/* destory future appts popup, if it exists */
	if (fframe) {
		xv_destroy(fframe);
		fframe = 0;
	}
	fix_current_day();
	Save = current;
	current.tm_mday = 1;
	fix_current_day();
	working(TRUE);
	start_dow = current.tm_wday;
	n_days = monthlength(current.tm_mon);
	
        rect = (Rect *) xv_get(cpwindow, WIN_RECT);
	/* Erase the window */
	drawable = (Drawable)xv_get(cpwindow, XV_XID);
	XClearArea(mydisplay, drawable, 0, 0, 0, 0, FALSE);
        left_border = (rect->r_width - 7*64)/2 + 32;
        top_border = (rect->r_height - 6*64) / 2;
         
	sprintf(title, "%s, %d",
		monthnames[current.tm_mon], 1900 + current.tm_year);
	xfont = (XFontStruct *)xv_get(bigfont, FONT_INFO);
	XSetFont(mydisplay, gcc, xfont->fid);
	XSetFunction(mydisplay, gcc, GXcopy);
	XDrawString(mydisplay, drawable, gcc,
		(rect->r_width - xv_get(bigfont, FONT_DEFAULT_CHAR_WIDTH) *
		strlen(title))/2, top_border/2 + 7,
		title, strlen(title));

	sun_moon_buttons(FALSE);
	print_button(TRUE);

	for (i=0; i<31; i++)		/* Which days have appointments? */
		busy_today[i] = 0;
	if ((apts = fopen(apts_pathname, "r")) == NULL)
		err_rpt("can't open appointments file", FATAL);
	First = current;
	current.tm_mday = monthlength(current.tm_mon);
	fix_current_day();
	Last = current;
	working(FALSE);
	while ((read_stat = get_aentry(apts, &appt, FALSE, 1, First.tm_mon+1)) != EOF) {
		if (read_stat)
			continue;	/* read error (ignore) */
		if (appt.flags & A_COMMENT)
			continue;
		if ((appt.flags & MARKED_NOTE) == MARKED_NOTE)
			continue;
		current.tm_year = appt.year;
		current.tm_mon = appt.month;
		current.tm_mday = appt.day;
		if (appt.flags & ALL_YEARS)
			current.tm_year = First.tm_year;
		if (appt.flags & ALL_MONTHS)
			current.tm_mon = First.tm_mon;
		if (appt.flags & EVERY_SOMEDAY) {
			if ((current.tm_mon == First.tm_mon) && (current.tm_year == First.tm_year)) {
				/* find first occurance of this day this month */
				current.tm_mday = First.tm_mday;
				find_date(&appt);
				if (appt.flags & RUN)
					runl = appt.runlength;
				else
					runl = 1;
			} else if (appt.flags & RUN) {
				runl = appt.runlength;
				find_date(&appt);
				while (ymd_compare(current, First) < 0 && --runl) {
					current.tm_mday += 7;
					find_date(&appt);
				}
				if (ymd_compare(current, First) < 0) {
					/* ran out of runlength */
					continue;
				}
			} else
				continue;
			while (ymd_compare(current, Last) <= 0 && runl) {
				if (chk_week(appt.repeat, current.tm_mday)) {
					if (runl)
						busy_today[current.tm_mday-1]++;
					if (appt.flags & RUN)
						--runl;
				}
				current.tm_mday += 7;
				fix_current_day();
			}
		} else if ((appt.flags & REPEAT) && !(appt.flags & ALL_DAYS)
		    && !(appt.flags & EVERY_MON_FRI)) {
			if (appt.flags & EVERY_SOMEDAY)
				continue;
			if (appt.flags & RUN)
				runl = appt.runlength;
			else
				runl = 1;
			while (ymd_compare(current, First) < 0 && runl) {
				if (appt.flags & RUN)
					--runl;
				if (runl) {
					current.tm_mday += appt.repeat;
					fix_current_day();
				}
			}
			while (ymd_compare(current, Last) <= 0 && runl) {
				if (runl) {
					busy_today[current.tm_mday-1]++;
					current.tm_mday += appt.repeat;
					fix_current_day();
					if (appt.flags & RUN)
						--runl;
				}
			}
		} else if (current.tm_year == First.tm_year 
			&& current.tm_mon == First.tm_mon) {
			if (appt.flags & ALL_DAYS)
				for (i=0; i<monthlength(First.tm_mon); i++)
					busy_today[i]++;
			else if (appt.flags & EVERY_MON_FRI)
				for (i=0,j=First.tm_wday; i<monthlength(First.tm_mon); i++,j++) {
					if (j > SAT)
						j = SUN;
					else if (j >= MON && j <= FRI)
						busy_today[i]++;
				}
			else if (appt.flags & DELETED)
				busy_today[appt.day-1]--;
			else
				busy_today[appt.day-1]++;
		}
				
	}
	fclose(apts);
	current = First;
	fix_current_day();
#ifndef NO_HOLIDAYS
	/*
	 * now that we've gone thru the appointments file,
	 * check to see if the user has selected any holiday
	 * options and add them in.
	 */
	for (i=0; i<monthlength(First.tm_mon); i++) {
		working(TRUE);
		if (holiday_a == 1) {
			j = a_dates(holiday_a);
			for (k=0; k<j; k++)
				if (ymd2_compare(&current, &a_appts[k]) == 0)
					busy_today[i]++;
		}
		if (holiday_c == 1) {
			j = c_dates(holiday_c);
			for (k=0; k<j; k++)
				if (ymd2_compare(&current, &c_appts[k]) == 0)
					busy_today[i]++;
		}
		working(FALSE);
		if (holiday_i == 1) {
			j = i_dates(holiday_i);
			for (k=0; k<j; k++)
				if (ymd2_compare(&current, &i_appts[k]) == 0)
					busy_today[i]++;
		}
		working(TRUE);
		if (holiday_j == 1) {
			j = j_dates(holiday_j);
			for (k=0; k<j; k++)
				if (ymd2_compare(&current, &j_appts[k]) == 0)
					busy_today[i]++;
		}
		if (holiday_s == 1) {
			j = s_dates(holiday_s);
			for (k=0; k<j; k++)
				if (ymd2_compare(&current, &s_appts[k]) == 0)
					busy_today[i]++;
		}
		current.tm_mday++;
		working(FALSE);
	}
	current = First;
	fix_current_day();
#endif

	y = top_border;				/* Draw all day boxes. */
	if (monday_first) {
		if (start_dow == SUN) {
			x = 64*6 + left_border;
			days_in_week = 6;
		} else {
			x = 64*(start_dow - 1) + left_border;
			days_in_week = start_dow - 1 ;
		}
	} else {
		x = 64*start_dow + left_border;
		days_in_week = start_dow;
	}
        c[0] = ' ';
        c[1] = '1';
        c[2] = '\0';
        for (i=0; i<n_days; i++){
		if (!ymd_compare(current, today))
			/* gray box */
			drawable2 = daybox_td_pr;
		else
			drawable2 = daybox_pr;
		XCopyArea(mydisplay, drawable2, drawable, gcc, 0, 0,
			64, 64,
			x, y);
		if (busy_today[i] > 0) {
			tri_pts[0].x = x+45;
			tri_pts[0].y = y+2;
			XFillPolygon(mydisplay, drawable, gcc,
				tri_pts,
				4, Convex, CoordModePrevious);
		}
                boxlims[i].lowx = x;
                boxlims[i].lowy = y;
                boxlims[i].highx = x + 63;
                boxlims[i].highy = y + 63;
		XDrawString(mydisplay, drawable, gcc,
			x+8, y+25,
			c, strlen(c));
                days_in_week++;
		current.tm_mday++;
                if (days_in_week == 7){
                        days_in_week = 0;
                        x = left_border;
                        y += 64;
                }
		else
                        x += 64;
                if (c[1] != '9')
                        c[1]++;
                else {
                        c[1] = '0';
                        if (c[0] == ' ')
                                c[0] = '1';
                        else
                                c[0]++;
                }
        }
        x = left_border + 27;
        y = top_border - 16;
	c[1] = '\0';
	if (monday_first) {
		for (i=1; i<7; i++) {		/* Mon ... Sat Sun */
			c[0] = daynames[i][0];
			XDrawString(mydisplay, drawable, gcc,
				x, y,
				c, 1);
			x += 64;
		}
		c[0] = daynames[0][0];
		XDrawString(mydisplay, drawable, gcc,
			x, y,
			c, 1);
	} else {
		for (i=0; i<7; i++) {		/* Sun Mon ... Sat */
			c[0] = daynames[i][0];
			XDrawString(mydisplay, drawable, gcc,
				x, y,
				c, 1);
			x += 64;
		}
        }

        /* Draw the "week arrows" */
        arrow_index = 0;
        last_top = -1;
	xfont = (XFontStruct *)xv_get(font, FONT_INFO);
	XSetFont(mydisplay, gcc, xfont->fid);

	current = First;
        for (i=0; i<n_days; i++)
                if (boxlims[i].lowy > last_top) {
                        last_top = boxlims[i].lowy;
                        week_arrows[arrow_index].active = 1;
                        week_arrows[arrow_index].left = left_border - 64;
                        week_arrows[arrow_index].top = last_top + 12;
                        week_arrows[arrow_index].right =
                          week_arrows[arrow_index].left + 43;
                        week_arrows[arrow_index].bottom =
                          week_arrows[arrow_index].top + 28;
			XCopyArea(mydisplay, weekarrow_pr, drawable, gcc, 0, 0,
				42, 29,
				left_border-64, last_top + 12);

			/*  Week numbers  */
			sprintf(c, "%2d", week_number());
			XDrawString(mydisplay, drawable, gcc,
				left_border-54, last_top+32, 
				c, strlen(c));
			current.tm_mday += 7;
			fix_current_day();

                        arrow_index++;
                }
	current = Save;
	unlock_cursors();
}
