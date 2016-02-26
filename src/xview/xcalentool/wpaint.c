/*
 * $Id: wpaint.c,v 2.3 1994/08/29 18:55:51 billr Exp $
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
 *	Artistic routines that draw in the main	   *
 * subwindow for the week display.		   *
 *						   *
 ***************************************************/

#include <xview/xview.h>
#include <xview/canvas.h>
#include <xview/font.h>
#include <ctype.h>
#include <stdio.h>
#include <X11/Xlib.h>
#include "ct.h"
#include "xv_ct.h"
#include "paint.h"

extern int week_message_size;

#ifdef __STDC__
void get_week_appts (void);
void paint_week_outline (void);
void paint_week_trim (void);
void draw_week_appts (void);
void free_week_appts (void);
void write_week_str (int day, int bi);
void draw_weekarrowhead (int day, int bi, int offset, int gray);
void draw_weekarrowshaft (int day, int bi, int offset, int gray);
void wmore_check (int day, int bi);
#else
void get_week_appts ();
void paint_week_outline ();
void paint_week_trim ();
void draw_week_appts ();
void free_week_appts ();
void write_week_str ();
void draw_weekarrowhead ();
void draw_weekarrowshaft ();
void wmore_check ();
#endif

/*
 * Routine to draw "Week-at-a-Glance".
 */

void
draw_week()
{
	struct tm Save;

	lock_cursors();
	/* destory future appts popup, if it exists */
	if (fframe) {
		xv_destroy_safe(fframe);
		fframe = 0;
	}
	fix_current_day();
	Save = current;
	current.tm_mday -= current.tm_wday; /* Sunday of this week */
	if (monday_first)  {
		if (current.tm_wday == SUN)
			current.tm_mday -= 7;
		if (nr_weekdays == 7)
			current.tm_mday++; /* start on Monday */
	}
	fix_current_day();
	if (nr_weekdays < 7) {
		current.tm_mday++;
		fix_current_day();
	}
	working(TRUE);
	get_week_appts();
	working(FALSE);
	drawable = (Drawable)xv_get(cpwindow, XV_XID);
	paint_week_outline();
	paint_week_trim();
	working(TRUE);
	draw_week_appts();
	free_week_appts();
	current = Save;
	(void)get_day_appts();
	working(FALSE);
	unlock_cursors();
}


/* Paint the outline for "Week-at-a-Glance". */
void
paint_week_outline()
{
	Rect *rect;
	int x, y, i, j;
 
	rect = (Rect *) xv_get(cpwindow, WIN_RECT);
	XClearArea(mydisplay, drawable, 0, 0, 0, 0, FALSE);
	startx = (rect->r_width - nr_weekdays*weekslot_width) / 2;
	starty = 10 + (rect->r_height - (n_slots*(weekslot_height+1))) / 2;
	y = starty;  /* avoid compiler warning */
 
	XSetFunction(mydisplay, gccs, GXcopy);
	XSetFunction(mydisplay, gcc, GXcopy);
	First = current;
	for (i=0; i<nr_weekdays; i++) {
		x = startx + i*weekslot_width;
		y = starty;
		week_boxes[i].wday_pos.left = x;
		week_boxes[i].wday_pos.top = y;
		week_boxes[i].wday_pos.right = x + weekslot_width;
		week_boxes[i].wday_pos.bottom = starty + n_slots*weekslot_height;
		week_boxes[i].moreb_pos.left = x + (weekslot_width - m_width) / 2;
		week_boxes[i].moreb_pos.top = week_boxes[i].wday_pos.bottom +
			2 * xv_get(font, FONT_DEFAULT_CHAR_HEIGHT);
		week_boxes[i].moreb_pos.right = week_boxes[i].moreb_pos.left + m_width;
		week_boxes[i].moreb_pos.bottom = week_boxes[i].moreb_pos.top + m_height;
		if (!ymd_compare(current, today))
			drawable2 = weekslot_td_pr;
		else
			drawable2 = weekslot_pr;
		for (j=0; j<n_slots; j++) {
			XCopyArea(mydisplay, drawable2, drawable, gcc,
				0, 0, weekslot_width, weekslot_height,
				x, y);
			y += weekslot_height;
		}
		current.tm_mday++;
		fix_current_day();
	}
	XDrawLine(mydisplay, drawable, gcc,
		startx, starty,
		startx+nr_weekdays*weekslot_width, starty);
	XDrawLine(mydisplay, drawable, gcc,
		startx, y-1,
		startx+nr_weekdays*weekslot_width, y-1);
	current = First;
	sun_moon_buttons(FALSE);
	print_button(TRUE);
}


void
paint_week_trim()
{
	int i, x, y, rightx;
	int cwidth;
	char c[8];
	
	cwidth = xv_get(font, FONT_DEFAULT_CHAR_WIDTH);
	First = current;
	for (i=0; i<nr_weekdays; i++) {
		x = startx + i*weekslot_width + (weekslot_width -
			2*(cwidth+2))/2;
		if (monday_first && i == 6)
			sprintf(c, "%3.3s", daynames[SUN]);
		else
			sprintf(c, "%3.3s", daynames[First.tm_wday + i]);
		XDrawString(mydisplay, drawable, gcc,
			x, starty-5,
			c, strlen(c));
	}
	
	y = starty + weekslot_height - 4;
	rightx = startx + nr_weekdays*weekslot_width + 10;
	for (i=0; i<n_slots; i++) {
		if (i < n_tslots) {
			if (hour24)
				sprintf(c, "%2d:%s",
					start_hour+(i/2),
					i%2 == 0 ? "00" : "30");
			else
				sprintf(c, "%2d:%s%s",
					(start_hour+(i/2))%12 == 0 ? 12 : (start_hour+(i/2))%12,
					i%2 == 0 ? "00" : "30", (start_hour+(i/2) < 12 ? "am" : "pm"));
		} else if (i == n_tslots) {
			sprintf(c, (hour24 ? "Notes" : " Notes"));
		} else {
			sprintf(c, "    ");
		}
		if (hour24) {
			XDrawString(mydisplay, drawable, gcc,
				startx-7*cwidth, y, c, strlen(c));
			XDrawString(mydisplay, drawable, gcc,
				rightx, y, c, strlen(c));
		} else {
			XDrawString(mydisplay, drawable, gcc,
				startx-8*cwidth, y, c, strlen(c));
			XDrawString(mydisplay, drawable, gcc,
				rightx-cwidth, y, c, strlen(c));
		}
		y += weekslot_height;
	}

	x = startx + (weekslot_width - 7*(cwidth +2))/2 + cwidth + 7;

	sprintf(c, "%d", 1900 + current.tm_year);
	XDrawString(mydisplay, drawable, gcc,
		startx-3*cwidth, y+weekslot_height, c, strlen(c));

	sprintf(c, "Week: %d", week_number());
	XDrawString(mydisplay, drawable, gcc,
		startx+nr_weekdays*weekslot_width-2*cwidth, y+weekslot_height,
		c, strlen(c));

	/* display week dates (month, day) */
	for (i=0; i<nr_weekdays; i++) {
		if (day_first)
			sprintf(c, "%2d %3.3s",
				current.tm_mday, monthnames[current.tm_mon]);
		else
			sprintf(c, "%3.3s %2d",
				monthnames[current.tm_mon], current.tm_mday);
		XDrawString(mydisplay, drawable, gcc, x, y, c, strlen(c));
		x += weekslot_width;
		current.tm_mday++;
		fix_current_day();
	}
	current = First;
	fix_current_day();
}	


void
get_week_appts()
{
	int i, j, save_read;
	struct tm Current;

	save_read = read_only;
	read_only = 1;
	Current = current;
	for (i=0; i<nr_weekdays; i++) {
		get_day_appts();	/* fills in slots[] array */
		for (j=0; j<n_slots; j++)
			week_boxes[i].weekslots[j] = slots[j];
		current.tm_mday++;
		fix_current_day();
	}
	read_only = save_read;
	current = Current;
	fix_current_day();
}			

/* draw in week appointments */
void
draw_week_appts()
{
	int index, slotno, offset, i;
	int narrows;
	struct dayslot *slptr;

	for (index=0; index<nr_weekdays; index++) {
		/* clear all arrow position information */
		for (slotno=0; slotno<n_slots; slotno++)
			week_boxes[index].weekslots[slotno].arrow_pos = 0;
		week_boxes[index].more = 0;
		for (slotno=0; slotno<n_slots; slotno++) {
			slptr = &week_boxes[index].weekslots[slotno];
			if (slptr->active) {
				x_coord = week_boxes[index].wday_pos.left;
				y_coord = week_boxes[index].wday_pos.top +
				  slotno*weekslot_height;
				write_week_str(index, slotno);
				if ((narrows = slptr->cur_appt->arrows) > 0) {
					/* find first free position for arrow */
					offset = 0;
					while (slptr->arrow_pos & 1<<offset)
						offset++;
					slptr->arrow_pos |= 1<<offset;
					i = slotno + narrows;
					week_boxes[index].weekslots[i].arrow_pos |= 1<<offset;
					draw_weekarrowhead(index, i, offset, FALSE);
					while (--narrows > 0) {
						week_boxes[index].weekslots[--i].arrow_pos
						  |= 1<<offset;
						draw_weekarrowshaft(index, i, offset, FALSE);
					}
				}
				wmore_check(index, slotno);
			}
		}
	}
}

void
draw_weekarrowshaft(day, bi, offset, gray)
int day, bi;
int offset, gray;
{
	int x, y;
	int pixoffset;

	/* mark this position as used */
	week_boxes[day].weekslots[bi].arrow_pos |= 1<<offset;
	pixoffset = (offset + 1) * 16;
	if (pixoffset > weekslot_width - 16)
		pixoffset = weekslot_width - 16;
	y = week_boxes[day].wday_pos.top + bi*weekslot_height;
	x = week_boxes[day].wday_pos.left;
	XSetFunction(mydisplay, gcc, GXcopy);
	if (gray)
		XCopyArea(mydisplay, gr_weekarrowshaft_pr, drawable, gcc,
			0, 0,
			14, weekslot_height,
			x+1+pixoffset, y);
	else
		XCopyArea(mydisplay, weekarrowshaft_pr, drawable, gcc,
			0, 0,
			14, weekslot_height,
			x+1+pixoffset, y);
}

void
draw_weekarrowhead(day, bi, offset, gray)
int day, bi;
int offset, gray;
{
	int x, y;
	int pixoffset;

	/* mark this position as used */
	week_boxes[day].weekslots[bi].arrow_pos |= 1<<offset;
	pixoffset = (offset + 1) * 16;
	if (pixoffset > weekslot_width - 16)
		pixoffset = weekslot_width - 16;
	y = week_boxes[day].wday_pos.top + bi*weekslot_height;
	x = week_boxes[day].wday_pos.left;
	XSetFunction(mydisplay, gcc, GXcopy);
	if (gray)
		XCopyArea(mydisplay, gr_weekarrowhead_pr, drawable, gcc,
			0, 0,
			14, weekslot_height-1,
			x+1+pixoffset, y);
	else
		XCopyArea(mydisplay, weekarrowhead_pr, drawable, gcc,
			0, 0,
			14, weekslot_height-1,
			x+1+pixoffset, y);
}

void
write_week_str(day, bi)
int day;
int bi;
{
	char slot_str[MAX_STRLEN];
	int strl;

	strl = strlen(week_boxes[day].weekslots[bi].cur_appt->str);
	if (strl <= week_message_size-1)
		strcpy(slot_str, week_boxes[day].weekslots[bi].cur_appt->str);
	else {
		/* show leading part */
		
		strncpy(slot_str, week_boxes[day].weekslots[bi].cur_appt->str, week_message_size-1);
		slot_str[week_message_size+1] = '\0';
	}
	XSetFunction(mydisplay, gcc, GXcopy);
	XFillRectangle(mydisplay, drawable, gcc,
		x_coord+2, y_coord+2,
		weekslot_width-4, weekslot_height-4);
	/* reverse fg and bg colors to get inverted text */
	XSetForeground(mydisplay, gcc, backgr);
	XSetBackground(mydisplay, gcc, foregr);
	XDrawString(mydisplay, drawable, gcc,
		x_coord + 4,
		y_coord + xv_get(font, FONT_DEFAULT_CHAR_HEIGHT),
		slot_str, strlen(slot_str));
	XSetForeground(mydisplay, gcc, foregr);
	XSetBackground(mydisplay, gcc, backgr);
}

/* free memory alloc'd for appts */
void
free_week_appts()
{
	int index, slotno;
	struct appt_entry *aptr, *optr;

	for (index=0; index<nr_weekdays; index++) {
		for (slotno=0; slotno<n_slots; slotno++) {
			if (week_boxes[index].weekslots[slotno].first)
				for (aptr=week_boxes[index].weekslots[slotno].first; aptr; ) {
					optr = aptr;
					aptr = aptr->next;
					free(optr);
				}
		}
	}
}

/* display "more" button if necessary */
void
wmore_check(day, bi)
int day, bi;
{
	int i, narrows, offset;
	struct appt_entry *aptr;
	struct dayslot *slptr;

	slptr = &week_boxes[day].weekslots[bi];
	if (slptr->active > 1) {
		for (aptr=slptr->first; aptr; aptr=aptr->next) {
			if (aptr == slptr->cur_appt)
				continue;  /* already did this one */
			if (chk_deleted(slptr, aptr))
				continue;
			if ((narrows = aptr->arrows) > 0) {
				/* find first free position for arrow */
				offset = 0;
				while (slptr->arrow_pos & 1<<offset)
					offset++;
				slptr->arrow_pos |= 1<<offset;
				i = bi + narrows;
				week_boxes[day].weekslots[i].arrow_pos |= 1<<offset;
				draw_weekarrowhead(day, i, offset, TRUE);
				while (--narrows > 0) {
					week_boxes[day].weekslots[--i].arrow_pos
					  |= 1<<offset;
					draw_weekarrowshaft(day, i, offset, TRUE);
				}
			}
		}
		week_boxes[day].more = 1;
		/* display more button at bottom of slot */
		XSetFunction(mydisplay, gcc, GXcopy);
		XCopyArea(mydisplay, morebutton, drawable, gcc,
			0, 0, m_width, m_height,
			week_boxes[day].moreb_pos.left,
			week_boxes[day].moreb_pos.top);
	}
}
