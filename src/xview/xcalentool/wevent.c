/*
 * $Id: wevent.c,v 2.3 1994/08/19 19:53:08 billr Exp $
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
 *	Week event routines for main subwindow.		*
 *							*
 ********************************************************/


#include <stdio.h>
#include <X11/Xlib.h>
#include <xview/xview.h>
#include <xview/canvas.h>
#include <xview/panel.h>
#include <xview/cursor.h>
#include <xview/font.h>
#include <xview/notice.h>
#include "ct.h"
#include "xv_ct.h"
#include "event.h"

extern Frame prompt_frame;
extern int monday_first;
extern int n_slots;

void
week_inputevent(window, event) 
Xv_window window;
Event *event;
{ 
	int i = -1;
	int x, y;
	static int day_chosen_from_week;
	extern void fix_current_day();

	if ((event_id(event) == KEY_TOP(6) || event_action(event) == ACTION_GO_PAGE_BACKWARD)
	    && event_is_up(event)) {
		lastweek();
		return;
	} else if ((event_id(event) == KEY_TOP(7) || event_action(event) == ACTION_GO_PAGE_FORWARD)
	    && event_is_up(event)) {
		nextweek();
		return;
	}

	/* translate coordinates to pixwin space */
	if (event_id(event) != MS_LEFT)	       /* Ignore kbd events. */
		return;

	if (event_is_down(event)) {		  /* Button down. */
		day_chosen_from_week = -1;
		for (i=0; i<nr_weekdays; i++) {
			x = event_x(event);
			y = event_y(event);
			if (x >= week_boxes[i].wday_pos.left && x <= week_boxes[i].wday_pos.right &&
			    y >= week_boxes[i].wday_pos.top && y <= week_boxes[i].wday_pos.bottom) {
				day_chosen_from_week = i;
				drawable = (Drawable)xv_get(window, XV_XID);
				XSetFunction(mydisplay, gcc, GXinvert);
				XFillRectangle(mydisplay, drawable, gcc,
					week_boxes[i].wday_pos.left+1,
					week_boxes[i].wday_pos.top+1,
					weekslot_width-2, (weekslot_height)*n_slots-2);
				return;
			}
			/* is cursor inside a "more" button ? */
			if (x>=week_boxes[i].moreb_pos.left && x<=week_boxes[i].moreb_pos.right &&
			    y>=week_boxes[i].moreb_pos.top && y<=week_boxes[i].moreb_pos.bottom) {
				if (week_boxes[i].more) {
					/* "more" button is active */
					day_chosen_from_week = i+10;
					return;
				}
			}
		}
		return;			/* Mouse wasn't in any square. */
	} else {			/* Button up. */
		if (day_chosen_from_week == -1)
			return;
		if (day_chosen_from_week >= 10) {
			/* more button selected */
			/* print info message */
			(void)notice_prompt(window, NULL,
				NOTICE_MESSAGE_STRINGS,
					"Select this day to view",
					"additional appointments.",
					0,
				NOTICE_BUTTON_YES, "Ok",
				0);
			return;
		}
		current.tm_mday -= current.tm_wday;
		if (monday_first) {
			if (current.tm_wday == SUN)
				current.tm_mday -= 7;
			if (nr_weekdays == 7)
				current.tm_mday++;
		}
		current.tm_mday += day_chosen_from_week;
		fix_current_day();
		if (nr_weekdays < 7) {
			current.tm_mday++;
			fix_current_day();
		}
		mainsw_state = DISPLAYING_DAY;
		xv_set(window, WIN_CURSOR, day_cursor, 0);
		draw_day();
	}
}
