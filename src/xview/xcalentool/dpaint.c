/*
 * $Id: dpaint.c,v 2.3 1994/08/19 20:36:24 billr Exp $
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
 * subwindow for the day display.		   *
 *						   *
 ***************************************************/

#include <ctype.h>
#include <sys/time.h>
#include <stdio.h>
#ifndef NOTOOL
#include <X11/Xlib.h>
#include <xview/xview.h>
#include <xview/canvas.h>
#include <xview/cursor.h>
#include <xview/font.h>
#include "xv_ct.h"
#endif
#include "ct.h"
#include "paint.h"
#ifndef NOTOOL
#include "riseset.h"
#define J1970   2440587.5 /* VAX clock Epoch 1970 Jan 1 (0h UT) */

#ifndef NO_SUN_MOON
extern Frame mframe, sframe;
extern Canvas mcanvas, scanvas;
extern Panel_item mdate_pi, sdate_pi;
extern GC sgc;
GC gcm;
#endif  /* NO_SUN_MOON */
extern Server_image leftarrow, rightarrow;
extern Server_image arrowshaft_pr, arrowhead_pr;
extern Server_image gr_arrowshaft_pr, gr_arrowhead_pr;
extern Xv_Cursor day_cursor;
extern char riseset_buf[][64];
extern int old_slot;
#endif  /* NOTOOL */
extern int day_message_size;
extern int show_future;
extern int new_entry;
extern int findex;		/* index into struct future array */
extern struct appt_entry future[];
extern unsigned int m_width, m_height, g_width, g_height;
extern int stipple_day;

#ifdef __STDC__
void draw_day1 (void);
void draw_future_appts (void);
void write_sun_data (void);
void write_moon_data (void);
void draw_day_outline (void);
void draw_day_appts (void);
void rewrite_string (int bi, int justify);
void draw_arrowhead (int i, int offset, int gray);
void draw_arrowshaft (int i, int offset, int gray);
void more_check (int slotno);
void write_times (void);
#else
void draw_day1 ();
void draw_future_appts ();
void write_sun_data ();
void write_moon_data ();
void draw_day_outline ();
void draw_day_appts ();
void rewrite_string ();
void draw_arrowhead ();
void draw_arrowshaft ();
void more_check ();
void write_times ();
#endif

#ifndef NOTOOL
/*
 * This one draws the current selected day in the
 * main subwindow.
 */

void
draw_day()
{
	draw_day1();
	XSync(mydisplay, 0);
	draw_future_appts();
#ifndef NO_SUN_MOON
	if (sframe)
		write_sun_data();
	if (mframe)
		write_moon_data();
#endif
}

/*
 * Draw main day page without future appts or Sun/Moon data
 */
void
draw_day1()
{
	int i;

	lock_cursors();
	fix_current_day();
	working(TRUE);
	drawable = (Drawable)xv_get(cpwindow, XV_XID);
	draw_day_outline();
	i = get_day_appts();
	working(FALSE);
	if (i) {
		draw_day_appts();
	}
	day_is_open = TRUE;
	unlock_cursors();
	xv_set(xv_get(canvas, CANVAS_NTH_PAINT_WINDOW, 0),
		WIN_CURSOR, day_cursor, 0);
}

/*
 * Utility for draw_day ... draws the outline of a day.
 */
void
draw_day_outline()
{
	char timestring[8], daystring[31], buf[64];
	int x, y, starty, i;
	Rect *rect;

	/* First erase the window. */
	rect = (Rect *) xv_get(cpwindow, WIN_RECT);
	XClearArea(mydisplay, drawable, 0, 0, 0, 0, FALSE);
	old_slot = -1;	/* text cursor no longer displayed */

	/* Calculate coords of top-left corner of big box. */
	x = (rect->r_width - dayslot_width) / 2;
	starty = y = (rect->r_height - (n_slots * dayslot_height)) / 2;

	/* Format daystring to say, for example, */
	if (day_first)
		/* Tuesday, 13 March 1990 */
		sprintf(daystring, "%s %d %s %d",
			daynames[current.tm_wday], current.tm_mday,
			monthnames[current.tm_mon], 1900 + current.tm_year);
	else
		/* Tuesday, March 13, 1990 */
		sprintf(daystring, "%s %s %d, %d",
			daynames[current.tm_wday], monthnames[current.tm_mon],
			current.tm_mday, 1900 + current.tm_year);
	xfont = (XFontStruct *)xv_get(bigfont, FONT_INFO);
	XSetFont(mydisplay, gcc, xfont->fid);
	XSetForeground(mydisplay, gcc, foregr);
	XSetBackground(mydisplay, gcc, backgr);
	XSetFunction(mydisplay, gcc, GXcopy);
	XDrawString(mydisplay, drawable, gcc,
		(rect->r_width - xv_get(bigfont, FONT_DEFAULT_CHAR_WIDTH) *
		strlen(daystring))/2, starty/2 + 7,
		daystring, strlen(daystring));

	xfont = (XFontStruct *)xv_get(font, FONT_INFO);
	XSetFont(mydisplay, gcc, xfont->fid);
	(void)XGetGeometry(mydisplay, leftarrow, &m_root, &m_x, &m_y,
		&g_width, &g_height, &m_border, &m_depth);
	if (!ymd_compare(current, today))
		drawable2 = timeslot_td_pr;
	else
		drawable2 = timeslot_pr;
	for (i=0; i<n_slots; i++) {	/* Init and draw each 30 minute slot. */
		slots[i].slot_pos.top = y;
		slots[i].slot_pos.left = x;
		slots[i].slot_pos.bottom = y + dayslot_height;
		slots[i].slot_pos.right = x + dayslot_width;
		slots[i].moreb_pos.top = y;
		slots[i].moreb_pos.left = rect->r_width - 8 - m_width;
		slots[i].moreb_pos.bottom = y + m_height;
		slots[i].moreb_pos.right = rect->r_width - 8;
		slots[i].larrow_pos.top = slots[i].slot_pos.top+(dayslot_height-g_height)/2;
		slots[i].larrow_pos.left = slots[i].slot_pos.right + 8;
		slots[i].larrow_pos.bottom = slots[i].larrow_pos.top + g_height;
		slots[i].larrow_pos.right = slots[i].larrow_pos.left + g_width;
		slots[i].rarrow_pos.top = slots[i].larrow_pos.top;
		slots[i].rarrow_pos.left = slots[i].larrow_pos.right + 8;
		slots[i].rarrow_pos.bottom = slots[i].larrow_pos.bottom;
		slots[i].rarrow_pos.right = slots[i].rarrow_pos.left + g_width;
		if (i < n_tslots) {
			/* display time */
			if (hour24)
				sprintf(timestring, "%2d:%s",
					start_hour+(i/2),
					i%2 == 0 ? "00" : "30");
			else
				sprintf(timestring, "%2d:%s%s",
					(start_hour+(i/2))%12 == 0 ? 12 : (start_hour+(i/2))%12,
					i%2 == 0 ? "00" : "30", (start_hour+(i/2) < 12 ? "am" : "pm"));
		} else if (i == n_tslots) {
			sprintf(timestring, "Notes");
		} else {
			sprintf(timestring, "     ");
		}
		XDrawString(mydisplay, drawable, gcc,
			x-9*xv_get(font, FONT_DEFAULT_CHAR_WIDTH),
			y+xv_get(font, FONT_DEFAULT_CHAR_HEIGHT),
			timestring,
			strlen(timestring));
		XCopyArea(mydisplay, drawable2, drawable, gcc, 0, 0,
			dayslot_width, dayslot_height,
			x, y);
		y += dayslot_height;
	}

	XDrawLine(mydisplay, drawable, gcc,
		x, starty,
		x+dayslot_width-1, starty);
	XDrawLine(mydisplay, drawable, gcc,
		x, y,
		x+dayslot_width-1, y);
	y += (dayslot_height - 1) * 2;
	sprintf(buf,
		"Day of year: %d  --  %d days remaining.      Week number: %d",
		day_of_year((double)current.tm_mday, current.tm_mon+1, current.tm_year+1900),
		days_remaining_in_year((double)current.tm_mday, current.tm_mon+1, current.tm_year+1900),
		week_number());
	XDrawString(mydisplay, drawable, gcc,
		x, y, buf, strlen(buf));
	sun_moon_buttons(TRUE);
	print_button(TRUE);
}
#endif  /* NOTOOL */

#ifndef NOTOOL
/* draw in todays appointments */
void
draw_day_appts()
{
	int slotno, narrows, i;
	int offset;

	drawable = (Drawable)xv_get(cpwindow, XV_XID);
	if (!ymd_compare(current, today) && stipple_day)
		drawable2 = timeslot_td_pr;
	else
		drawable2 = timeslot_pr;
	XSetFunction(mydisplay, gcc, GXcopy);
	/* first clear all the slots */
	for (slotno=0; slotno<n_slots; slotno++) {
		slots[slotno].arrow_pos = 0;
		XCopyArea(mydisplay, drawable2, drawable, gcc, 0, 0,
			dayslot_width, dayslot_height,
			slots[slotno].slot_pos.left,
			slots[slotno].slot_pos.top);
	}

	/* draw in current info */
	for (slotno=0; slotno<n_slots; slotno++) {
		if (slots[slotno].active) {
			rewrite_string(slotno, JUSTIFY_LEFT);
			if ((narrows = slots[slotno].cur_appt->arrows) > 0) {
				/* find first free position for arrow */
				offset = 0;
				while (slots[slotno].arrow_pos & 1<<offset)
					offset++;
				slots[slotno].arrow_pos |= 1<<offset;
				i = slotno + narrows;
				draw_arrowhead(i, offset, FALSE);
				while (--narrows > 0)
					draw_arrowshaft(--i, offset, FALSE);
			}
			more_check(slotno);
		}
	}
}

/* Blacks out day-slot and then re-writes string. */
void
rewrite_string(bi, justify)
int bi, justify;  
{
	char slot_str[MAX_STRLEN];
	char *ptr;
	int strl, *iptr;

	XSetFunction(mydisplay, gcc, GXcopy);
	strl = strlen(slots[bi].cur_appt->str);
	iptr = &slots[bi].cur_appt->sindex;
	if (strl < day_message_size) {
		*iptr = 0; /* just in case */
		strcpy(slot_str, slots[bi].cur_appt->str);
		/* erase any previously existing scroll arrows */
		XClearArea(mydisplay, drawable,
			slots[bi].larrow_pos.left,
			slots[bi].larrow_pos.top,
			slots[bi].rarrow_pos.right-slots[bi].larrow_pos.left,
			slots[bi].larrow_pos.bottom-slots[bi].larrow_pos.top, FALSE);
	} else {
		if (justify == JUSTIFY_RIGHT) {
			/* show trailing part */
			ptr = &slots[bi].cur_appt->str[strl - day_message_size + 1];
			*iptr = strl - day_message_size + 1;
			strcpy(slot_str, ptr);
		} else {
			/* show leading or indexed part */
			if (justify == JUSTIFY_LEFT)
				*iptr = 0;
			if (*iptr > (strl - day_message_size + 1))
				*iptr = strl - day_message_size + 1;
			if (strlen(&slots[bi].cur_appt->str[*iptr]) >= day_message_size-1) {
				strncpy(slot_str, &slots[bi].cur_appt->str[*iptr], day_message_size-1);
				slot_str[day_message_size-1] = '\0';
			} else
				strcpy(slot_str, &slots[bi].cur_appt->str[*iptr]);
		}
		/* display scroll arrows */
		drawable = (Drawable)xv_get(cpwindow, XV_XID);
		(void)XGetGeometry(mydisplay, leftarrow, &m_root, &m_x, &m_y,
			&g_width, &g_height, &m_border, &m_depth);
		XCopyArea(mydisplay, leftarrow, drawable, gcc, 0, 0,
			g_width, g_height,
			slots[bi].larrow_pos.left,
			slots[bi].larrow_pos.top);
		XCopyArea(mydisplay, rightarrow, drawable, gcc, 0, 0,
			g_width, g_height,
			slots[bi].rarrow_pos.left,
			slots[bi].rarrow_pos.top);
	}

	XFillRectangle(mydisplay, drawable, gcc,
		slots[bi].slot_pos.left+2,
		slots[bi].slot_pos.top+2,
		dayslot_width-4,
		dayslot_height-4);
	/* reverse fg and bg colors to get inverted text */
	XSetForeground(mydisplay, gcc, backgr);
	XSetBackground(mydisplay, gcc, foregr);
	XDrawString(mydisplay, drawable, gcc,
		slots[bi].slot_pos.left+5,
		slots[bi].slot_pos.top+xv_get(font, FONT_DEFAULT_CHAR_HEIGHT),
		slot_str,
		strlen(slot_str));
	XSetForeground(mydisplay, gcc, foregr);
	XSetBackground(mydisplay, gcc, backgr);
}

/* display "more" button if necessary */
void
more_check(slotno)
int slotno;
{
	int i, offset;
	int narrows;
	struct appt_entry *aptr;

	/* clear any previous button that may be there */
	XClearArea(mydisplay, drawable,
		slots[slotno].moreb_pos.left,
		slots[slotno].moreb_pos.top,
		m_width,
		m_height, FALSE);
	/* button displayed when more than 1 reference
	 * and at least one real appt for this slot.
	 */
	if (slots[slotno].active > 1) {
		for (aptr=slots[slotno].first; aptr; aptr=aptr->next) {
			if (aptr == slots[slotno].cur_appt)
				continue;  /* already did this one */
			if (chk_deleted(&slots[slotno], aptr))
				continue;  /* ignore deleted appts */
			if ((narrows = aptr->arrows) > 0) {
				/* find first free position for arrow */
				offset = 0;
				while (slots[slotno].arrow_pos & 1<<offset)
					offset++;
				slots[slotno].arrow_pos |= 1<<offset;
				i = slotno + narrows;
				draw_arrowhead(i, offset, TRUE);
				while (--narrows > 0)
					draw_arrowshaft(--i, offset, TRUE);
			}
		}
		/* display more button to right of slot */
		XSetFunction(mydisplay, gcc, GXcopy);
		XCopyArea(mydisplay, morebutton, drawable, gcc, 0, 0,
			m_width, m_height,
			slots[slotno].moreb_pos.left,
			slots[slotno].moreb_pos.top);
	}
}

void
draw_arrowshaft(i, offset, gray)
int i;
int offset;
int gray;
{
	/* mark this position as used */
	slots[i].arrow_pos |= 1<<offset;
	/* translate to screen coordinates */
	offset = (offset + 1) * 40;
	XSetFunction(mydisplay, gcc, GXcopy);
	if (gray)
		XCopyArea(mydisplay, gr_arrowshaft_pr, drawable, gcc,
			0, 0,
			16, dayslot_height,
			slots[i].slot_pos.left+1+offset,
			slots[i].slot_pos.top);
	else
		XCopyArea(mydisplay, arrowshaft_pr, drawable, gcc,
			0, 0,
			16, dayslot_height,
			slots[i].slot_pos.left+1+offset,
			slots[i].slot_pos.top);
}



void
draw_arrowhead(i, offset, gray)
int i;
int offset;
int gray;
{
	/* mark this position as used */
	slots[i].arrow_pos |= 1<<offset;
	/* translate to screen coordinates */
	offset = (offset + 1) * 40;
	XSetFunction(mydisplay, gcc, GXcopy);
	if (gray)
		XCopyArea(mydisplay, gr_arrowhead_pr, drawable, gcc,
			0, 0,
			16, dayslot_height-1,
			slots[i].slot_pos.left+1+offset,
			slots[i].slot_pos.top);
	else
		XCopyArea(mydisplay, arrowhead_pr, drawable, gcc,
			0, 0,
			16, dayslot_height-1,
			slots[i].slot_pos.left+1+offset,
			slots[i].slot_pos.top);
}

/*
 * Routine to create popup window with future appts shown in it
 */
void
draw_future_appts()
{
	if (show_future && findex && (ymd_compare(current, today) == 0)) {
		create_future_popup();
	} else {
		/* nothing to show */
		/* destroy future appts popup, if it exists */
		if (fframe) {
			xv_destroy(fframe);
			fframe = 0;
		}
	}
}
#endif  /* NOTOOL */

#ifndef NO_SUN_MOON
/*
 * write sun data to the popup canvas
 */
void
write_sun_data()
{
	int	x, y, height;
	char	buf[64];
	struct	timeval tp;
	double	jdays, secs, offset;
	double	julian_day();
	
	/* first erase the window. */
	drawable2 = (Drawable)xv_get(canvas_paint_window(scanvas), XV_XID);
	XClearArea(mydisplay, drawable2, 0, 0, 0, 0, FALSE);

	x = xv_get(font, FONT_DEFAULT_CHAR_WIDTH);
	height = y = xv_get(font, FONT_DEFAULT_CHAR_HEIGHT);

#ifdef SVR4
	gettimeofday(&tp);
#else
	gettimeofday(&tp, NULL);
#endif
	if (ymd_compare(current, today) == 0) {
		/* use current time */
		write_times();
		xv_set(sdate_pi, PANEL_LABEL_STRING, riseset_buf[B_DMY], 0);
		y += 11 * height;
	} else {
		/* convert today's date to approx. seconds from 1-1-1970 */
		jdays = julian_day((double)today.tm_mday, today.tm_mon+1, today.tm_year+1900) - J1970;
		/* seconds from 00:00 GMT to now */
		offset = tp.tv_sec - (jdays * 24. * 3600.);
		/* convert this date to approx. seconds from 1-1-1970 */
		jdays = julian_day((double)current.tm_mday, current.tm_mon+1, current.tm_year+1900) - J1970;
		/* seconds to the same time on selected day */
		secs = (jdays * 24.0 * 3600.) + offset;
		riseset((long)secs);
		xv_set(sdate_pi, PANEL_LABEL_STRING, riseset_buf[B_DMY], 0);
		XDrawString(mydisplay, drawable2, sgc,
			x, y,
			riseset_buf[B_JLD],
			strlen(riseset_buf[B_JLD]));
		y += height;
	}
	sprintf(buf, "Sun Rise    (today): %s   Total Hours of", riseset_buf[B_SRD]);
	XDrawString(mydisplay, drawable2, sgc,
		x, y,
		buf, strlen(buf));
	y += height;
	
	sprintf(buf, "Sun Set     (today): %s   Sunlight %5s ", riseset_buf[B_SSD],
		riseset_buf[B_LOD]);
	XDrawString(mydisplay, drawable2, sgc,
		x, y,
		buf, strlen(buf));
	y += height;
	sprintf(buf, "Sun Rise (tomorrow): %s", riseset_buf[B_SRT]);
	XDrawString(mydisplay, drawable2, sgc,
		x, y,
		buf, strlen(buf));
	y += height;
	sprintf(buf, "Sun Set  (tomorrow): %s", riseset_buf[B_SST]);
	XDrawString(mydisplay, drawable2, sgc,
		x, y,
		buf, strlen(buf));
}

/*
 * write sun time data to the popup canvas
 */
void
write_times()
{
	int	x, y, height;
	struct	timeval tp;

	/* only update these if displaying today's page */
	if (ymd_compare(current, today) != 0)
		return;

	x = xv_get(font, FONT_DEFAULT_CHAR_WIDTH);
	y = height = xv_get(font, FONT_DEFAULT_CHAR_HEIGHT);
	drawable2 = (Drawable)xv_get(canvas_paint_window(scanvas), XV_XID);

#ifdef SVR4
	gettimeofday(&tp);
#else
	gettimeofday(&tp, NULL);
#endif
	riseset(tp.tv_sec);
	XDrawImageString(mydisplay, drawable2, sgc,
		x, y,
		riseset_buf[B_GMT], strlen(riseset_buf[B_GMT]));
	y += height;
	XDrawImageString(mydisplay, drawable2, sgc,
		x, y,
		riseset_buf[B_TDT], strlen(riseset_buf[B_TDT]));
	y += height;
	XDrawImageString(mydisplay, drawable2, sgc,
		x, y,
		riseset_buf[B_LCT], strlen(riseset_buf[B_LCT]));
	y += height;
	XDrawImageString(mydisplay, drawable2, sgc,
		x, y,
		riseset_buf[B_LMT], strlen(riseset_buf[B_LMT]));
	y += height;
	XDrawImageString(mydisplay, drawable2, sgc,
		x, y,
		riseset_buf[B_GST], strlen(riseset_buf[B_GST]));
	y += height;
	XDrawImageString(mydisplay, drawable2, sgc,
		x, y,
		riseset_buf[B_LST], strlen(riseset_buf[B_LST]));
	y += height;
	XDrawImageString(mydisplay, drawable2, sgc,
		x, y,
		riseset_buf[B_JLD], strlen(riseset_buf[B_JLD]));
	y += height;
	XDrawImageString(mydisplay, drawable2, sgc,
		x, y,
		riseset_buf[B_LHA], strlen(riseset_buf[B_LHA]));
	y += height;
	XDrawImageString(mydisplay, drawable2, sgc,
		x, y,
		riseset_buf[B_SDE], strlen(riseset_buf[B_SDE]));
	y += height;
	XDrawImageString(mydisplay, drawable2, sgc,
		x, y,
		riseset_buf[B_SAZ], strlen(riseset_buf[B_SAZ]));
	y += height;
	XDrawImageString(mydisplay, drawable2, sgc,
		x, y,
		riseset_buf[B_SEL], strlen(riseset_buf[B_SEL]));
}

/*
 * write moon data to the popup canvas
 */
void
write_moon_data()
{
	int	x, y, height;
	char	buf[64];
	struct	timeval tp;
	double	jdays, secs, offset;
	double	julian_day();

	/* first erase the window. */
	drawable2 = (Drawable)xv_get(canvas_paint_window(mcanvas), XV_XID);
	gcm = XCreateGC(mydisplay, drawable,
		GCBackground|GCForeground|GCFunction|GCFont,
		&gc_val);

	XClearArea(mydisplay, drawable2, 0, 0, 0, 0, FALSE);

	x = xv_get(font, FONT_DEFAULT_CHAR_WIDTH);
	y = height = xv_get(font, FONT_DEFAULT_CHAR_HEIGHT);

#ifdef SVR4
	gettimeofday(&tp);
#else
	gettimeofday(&tp, NULL);
#endif
	if (ymd_compare(current, today) == 0) {
		/* use current time */
		riseset(tp.tv_sec);
		moon_data(tp.tv_sec);
	} else {
		/* convert today's date to approx. seconds from 1-1-1970 */
		jdays = julian_day((double)today.tm_mday, today.tm_mon+1, today.tm_year+1900) - J1970;
		/* seconds from 00:00 GMT to now */
		offset = tp.tv_sec - (jdays * 24. * 3600.);
		/* convert this date to seconds from 1-1-1970 */
		jdays = julian_day((double)current.tm_mday, current.tm_mon+1, current.tm_year+1900) - J1970;
		/* seconds to the same time on selected day */
		secs = (jdays * 24.0 * 3600.) + offset;
		riseset((long)secs);
		moon_data((long)secs);
	}
	xv_set(mdate_pi, PANEL_LABEL_STRING, riseset_buf[B_DMY], 0);
	y = 6 * height;
	sprintf(buf, "Moon Rise    (today): %s", riseset_buf[B_MRD]);
	XDrawString(mydisplay, drawable2, gcm,
		x, y,
		buf, strlen(buf));
	y += height;
	sprintf(buf, "Moon Set     (today): %s", riseset_buf[B_MSD]);
	XDrawString(mydisplay, drawable2, gcm,
		x, y,
		buf, strlen(buf));
	y += height;
	sprintf(buf, "Moon Rise (tomorrow): %s", riseset_buf[B_MRT]);
	XDrawString(mydisplay, drawable2, gcm,
		x, y,
		buf, strlen(buf));
	y += height;
	sprintf(buf, "Moon Set  (tomorrow): %s", riseset_buf[B_MST]);
	XDrawString(mydisplay, drawable2, gcm,
		x, y,
		buf, strlen(buf));
	XFreeGC(mydisplay, gcm);
}

#endif	/* NO_SUN_MOON */
