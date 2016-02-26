/*
 * $Id: paint.h,v 2.3 1994/08/19 19:53:08 billr Exp $
 */
#ifndef PAINT_H
#define PAINT_H

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

#ifndef NOTOOL
#include <xview/panel.h>

extern Xv_Font font, bigfont;
extern Canvas canvas;
extern Xv_window cpwindow;
extern Pixwin *main_pixwin;
extern Pixmap timeslot_pr, daybox_pr, weekarrow_pr,  weekslot_pr;
extern Pixmap weekarrowshaft_pr, weekarrowhead_pr;
extern Pixmap gr_weekarrowshaft_pr, gr_weekarrowhead_pr;
extern Pixmap triangle_pr;
extern Pixmap a_ydaybox_pr, ydaybox_pr, ymonthbox_pr;
extern Pixmap timeslot_td_pr, daybox_td_pr, weekslot_td_pr, ydaybox_td_pr;
extern Pixmap morebutton;
extern struct weekrect week_boxes[];
extern int x_coord, y_coord, startx, starty;
extern int mainsw_state;
extern int dayslot_height, weekslot_height, weekslot_width;
extern int ybox_height, ybox_width;
extern struct rect_limits boxlims[];
extern struct rect_limits mboxlims[];
extern struct week_arrow week_arrows[];
extern Frame fframe;
extern int hour24, monday_first, day_first;
#endif  /* NOTOOL */
extern char apts_pathname[], tmpapts_pathname[];
extern int read_only, day_is_open, version2;
extern char *progname;
extern char *daynames[], *monthnames[];
extern int dayslot_width, nr_weekdays, max_strlen, n_tslots, n_slots;
extern int start_hour;
extern struct tm current, today, First, Last;
extern struct dayslot *slots;
#ifndef NO_HOLIDAYS
extern int holiday_a, holiday_c, holiday_i, holiday_j, holiday_s;
#endif

#endif
