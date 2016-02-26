/*
 * $Id: ypaint.c,v 2.3 1994/08/29 17:35:04 billr Exp $
 */
/*
 * calentool - day/week/month/year-at-a-glance calendar for XView/Open Look
 * 
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
 *	Artistic routines that draw in the main	   *
 * subwindow for the year display.		   *
 *						   *
 ***************************************************/

#include <X11/Xlib.h>
#include <xview/xview.h>
#include <xview/canvas.h>
#include <xview/font.h>
#include <ctype.h>
#include <stdio.h>
#include "xv_ct.h"
#include "ct.h"
#include "paint.h"
#ifndef NO_HOLIDAYS
extern struct appt_entry a_appts[], c_appts[];
extern struct appt_entry i_appts[], j_appts[];
extern struct appt_entry s_appts[];
#endif

/*
 * draw calendar for a whole year.
 */
void
draw_year()
{
	int monthnr, daynr, boxnr, i, j, k;
	int row, col, x, y;
	int busy_today[367], startbox, nrdays, yrday, extra_days;
	char title[5], c[3], buf[100];
	Rect *rect;
	struct tm Save;
	struct appt_entry appt;
	int read_stat;
	FILE *apts;
	int runl;

	lock_cursors();
	/* destory future appts popup, if it exists */
	if (fframe) {
		xv_destroy_safe(fframe);
		fframe = 0;
	}
	fix_current_day();

	working(TRUE);
	rect = (Rect *) xv_get(cpwindow, WIN_RECT);
	/* Erase the window */
	drawable = (Drawable)xv_get(cpwindow, XV_XID);
	XClearArea(mydisplay, drawable, 0, 0, 0, 0, FALSE);
	startx = (rect->r_width - 3*8*ybox_width)/2 + xv_get(font, FONT_DEFAULT_CHAR_WIDTH)+1;
	starty = rect->r_height - 4*8*ybox_height + ybox_height/2;
	sun_moon_buttons(FALSE);
	print_button(TRUE);

	/* Which days have appointments? */
	for (i=0; i<dysize(current.tm_year + 1900); i++)
		busy_today[i] = 0;
	Save = current;
	current.tm_mon = JAN;
	current.tm_mday = 1;
	fix_current_day();
	First = current;
	current.tm_mon = DEC;
	current.tm_mday = monthlength(DEC);
	fix_current_day();
	Last = current;
	if ((apts = fopen(apts_pathname, "r")) == NULL)
		err_rpt("can't open appointments file", FATAL);

	working(FALSE);
	while ((read_stat = get_aentry(apts, &appt, FALSE, 1, 0)) != EOF) {
		if (read_stat)
			continue;	/* read error */
		if (appt.flags & A_COMMENT)
			continue;
		if ((appt.flags & MARKED_NOTE) == MARKED_NOTE)
			continue;
		current.tm_year = appt.year;
		current.tm_mon = appt.month;
		current.tm_mday = appt.day;
		if (appt.flags & ALL_YEARS)
			current.tm_year = First.tm_year;
		if (current.tm_year == First.tm_year) {
			if (appt.flags & ALL_MONTHS) {
				for (current.tm_mon = JAN; 
					current.tm_mon <= DEC; 
					current.tm_mon++) {
					if ((appt.flags & ALL_DAYS)
					    || (appt.flags & EVERY_MON_FRI)) {
						for (current.tm_mday = 1; current.tm_mday <=
							monthlength(current.tm_mon); current.tm_mday++) {
							fix_current_day();
							if (current.tm_wday >= MON && current.tm_wday <=FRI)
								busy_today[current.tm_yday]++;
							else if (appt.flags & ALL_DAYS)
								/* pick up Sat and Sun */
								busy_today[current.tm_yday]++;
						}
					} else if (appt.flags & EVERY_SOMEDAY) {
						i = Pickday(appt.flags);
						for (current.tm_mday = 1; current.tm_mday <=
							monthlength(current.tm_mon); current.tm_mday++) {
							fix_current_day();
							if (current.tm_wday == i)
								if (chk_week(appt.repeat, current.tm_mday))
									busy_today[current.tm_yday]++;
						}
					} else {
						fix_current_day();
						busy_today[current.tm_yday]++;
					}
				}
			} else if ((appt.flags & ALL_DAYS)
			    || (appt.flags & EVERY_MON_FRI)) {
				for (current.tm_mday = 1; current.tm_mday <=
				    monthlength(current.tm_mon); current.tm_mday++) {
					fix_current_day();
					if (current.tm_wday >= MON && current.tm_wday <=FRI)
						busy_today[current.tm_yday]++;
					else if (appt.flags & ALL_DAYS)
						/* pick up Sat and Sun */
						busy_today[current.tm_yday]++;
				}
			} else if (appt.flags & REPEAT) {
				if (appt.flags & EVERY_SOMEDAY) {
					current.tm_mday = 1;
					find_date(&appt);
					if (appt.flags & RUN)
						runl = appt.runlength;
					else
						runl = 1;
					while (ymd_compare(current, Last) <= 0 && runl) {
						if (chk_week(appt.repeat, current.tm_mday)) {
							if (runl)
								busy_today[current.tm_yday]++;
							if (appt.flags & RUN)
								--runl;
						}
						current.tm_mday += 7;
						fix_current_day();
						if (current.tm_mon != appt.month && !(appt.flags & RUN))
							break;
					}
				} else {
					fix_current_day();
					if (appt.flags & RUN)
						runl = appt.runlength;
					else
						runl = 1;
					while (ymd_compare(current, Last) <= 0 && runl) {
						if (runl) {
							busy_today[current.tm_yday]++;
							current.tm_mday += appt.repeat;
							fix_current_day();
							if (appt.flags & RUN)
								--runl;
						}
					}
				}
			} else {
				fix_current_day();
				if (appt.flags & DELETED)
					busy_today[current.tm_yday]--;
				else
					busy_today[current.tm_yday]++;
			}
		} else if (appt.flags & RUN) {
			/* find 1st appt in this year */
			runl = appt.runlength;
			if (appt.flags & EVERY_SOMEDAY)
				find_date(&appt);
			while (ymd_compare(current, First) < 0 && runl) {
				if (appt.flags & RUN)
					--runl;
				if (runl) {
						if (appt.flags & EVERY_SOMEDAY) {
							current.tm_mday += 7;
							find_date(&appt);
						} else {
							current.tm_mday += appt.repeat;
							fix_current_day();
						}
				}
			}
			while (ymd_compare(current, Last) <= 0 && runl) {
				if (runl) {
					busy_today[current.tm_yday]++;
					if (appt.flags & RUN)
						--runl;
					if (appt.flags & EVERY_SOMEDAY) {
						current.tm_mday += 7;
						find_date(&appt);
					} else {
						current.tm_mday += appt.repeat;
						fix_current_day();
					}
				}
			}
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
	for (i=0; i<dysize(current.tm_year + 1900); i++) {
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
		fix_current_day();
		working(FALSE);
	}
	current = First;
	fix_current_day();
#endif	/* NO_HOLIDAYS */

	/* display year title */
	sprintf(title, "%d", 1900 + First.tm_year);
	xfont = (XFontStruct *)xv_get(bigfont, FONT_INFO);
	XSetFont(mydisplay, gcc, xfont->fid);
	XSetFunction(mydisplay, gcc, GXcopy);
	XDrawString(mydisplay, drawable, gcc,
		(rect->r_width - xv_get(bigfont, FONT_DEFAULT_CHAR_WIDTH) *
		strlen(title))/2, starty - xv_get(bigfont, FONT_DEFAULT_CHAR_HEIGHT),
		title, strlen(title));
	xfont = (XFontStruct *)xv_get(font, FONT_INFO);
	XSetFont(mydisplay, gcc, xfont->fid);

	/* display day names */
	if (monday_first) {
#ifdef FRENCH
		strcpy(buf, "Lu Ma Me Je Ve Sa Di    ");
		strcat(buf, "Lu Ma Me Je Ve Sa Di    ");
		strcat(buf, "Lu Ma Me Je Ve Sa Di");
#else
# ifdef SWEDISH
		strcpy(buf, "Ma Ti On To Fr LO SO    ");
		strcat(buf, "Ma Ti On To Fr LO SO    ");
		strcat(buf, "Ma Ti On To Fr LO SO");
# else
		strcpy(buf, "Mo Tu We Th Fr Sa Su    ");
		strcat(buf, "Mo Tu We Th Fr Sa Su    ");
		strcat(buf, "Mo Tu We Th Fr Sa Su");
# endif /* SWEDISH */
#endif /* FRENCH */
	} else {
#ifdef FRENCH
		strcpy(buf, "Di Lu Ma Me Je Ve Sa    ");
		strcat(buf, "Di Lu Ma Me Je Ve Sa    ");
		strcat(buf, "Di Lu Ma Me Je Ve Sa");
#else
# ifdef SWEDISH
		strcpy(buf, "SO Ma Ti On To Fr LO SO    ");
		strcat(buf, "SO Ma Ti On To Fr LO SO    ");
		strcat(buf, "SO Ma Ti On To Fr LO SO");
# else
		strcpy(buf, "Su Mo Tu We Th Fr Sa    ");
		strcat(buf, "Su Mo Tu We Th Fr Sa    ");
		strcat(buf, "Su Mo Tu We Th Fr Sa");
# endif /* SWEDISH */
#endif /* FRENCH */
	}
	XDrawString(mydisplay, drawable, gcc,
		startx+4, starty-2,
		buf, strlen(buf));

	/* draw months */
	monthnr = 0;
	yrday = 0;
	extra_days = 0;
	daynr = 1;
	nrdays = 0;
	if (monday_first) {
		if (First.tm_wday == SUN)
			startbox = 6;
		else
			startbox = First.tm_wday - 1;
	} else
		startbox = First.tm_wday;
	for (row=0; row<4; row++) {
		for (col=0; col<3; col++) {
			x = startx + 8*ybox_width*col;
			y = starty + 8*ybox_height*row;
			mboxlims[monthnr].lowx = x;
			mboxlims[monthnr].lowy = y;
			mboxlims[monthnr].highx = x + 7*ybox_width;
			mboxlims[monthnr].highy = y + 7*ybox_height;
			/* draw month box and label */
			XCopyArea(mydisplay, ymonthbox_pr, drawable, gcc,
				0, 0, 7*ybox_width, ybox_height,
				x, y);
			XDrawString(mydisplay, drawable, gcc,
				x+8, y+xv_get(font, FONT_DEFAULT_CHAR_HEIGHT),
				monthnames[row*3 + col],
				strlen(monthnames[row*3 + col]));
			if (!extra_days) {
				nrdays = monthlength(monthnr);
				daynr = 1;
			}
			boxnr = 0;
			for (j=0; j<6; j++) {
				y += ybox_height;
				/* foreach week in the month */
				for (k=0; k<7; k++) {
					/* foreach day in the week */
					/* draw day boxes */
					XCopyArea(mydisplay, ydaybox_pr, drawable, gcc,
						0, 0, ybox_width, ybox_height,
						x, y);
					/* label day boxes */
					if (boxnr >= startbox && daynr <= nrdays) {
						if (ymd_compare(today, current) == 0) {
							/* gray box */
							XCopyArea(mydisplay, ydaybox_td_pr, drawable, gcc,
								0, 0, ybox_width, ybox_height,
								x, y);
						}
						c[0] = (daynr<10 ? ' ' : daynr/10 + '0');
						c[1] = daynr%10 + '0';
						c[2] = '\0';
						/* if appointment exists, reverse video box */
						if (busy_today[yrday] > 0 && ymd_compare(today, current) != 0) {
							/* use inverted color box */
							XCopyArea(mydisplay, a_ydaybox_pr, drawable, gcc,
								0, 0, ybox_width, ybox_height,
								x, y);
							/* reverse fg and bg colors to get inverted text */
							XSetForeground(mydisplay, gcc, backgr);
							XSetBackground(mydisplay, gcc, foregr);
							XDrawString(mydisplay, drawable, gcc,
								x+4, y+14,
								c, strlen(c));
							XSetForeground(mydisplay, gcc, foregr);
							XSetBackground(mydisplay, gcc, backgr);
						} else
							XDrawString(mydisplay, drawable, gcc,
								x+4, y+14,
								c, strlen(c));

						if (daynr == nrdays)
							startbox = (k + 1) % 7;
						++daynr;
						++yrday;
						current.tm_mday++;
						fix_current_day();
					}
					x += ybox_width;
					++boxnr;
				}
				x -= 7*ybox_width;
			}
			++monthnr;
		}
	}
	current = Save;
	fix_current_day();
	unlock_cursors();
}

