/*
 * $Id: devent.c,v 2.3 1994/08/29 23:30:37 billr Exp $
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
 *	Day event routines for main subwindow.		*
 *							*
 ********************************************************/


#include <stdio.h>
#include <X11/Xlib.h>
#include <xview/xview.h>
#include <xview/canvas.h>
#include <xview/panel.h>
#include <xview/openmenu.h>
#include <xview/notice.h>
#undef ROUNDUP  /* to avoid multiply declared define */
#include <xview/seln.h>
#include <xview/font.h>
#include <xview/cursor.h>
#include <sys/time.h>
#include "xv_ct.h"
#include "ct.h"
#include "event.h"

Menu day_sel_menu;
extern Frame attr_frame;
extern Canvas canvas;
extern Xv_window cpwindow;
extern Panel_item everyx_pi, repeat_pi, remind_pi;
extern Panel_item whichwk_pi, marked_pi, advw_pi;
extern Panel_item runl_pi;
extern Pixmap tri_up_pr, ydaybox_pr;
extern Pixmap leftarrow, rightarrow;
extern Pixmap morebutton;
extern Pixmap savePix;
extern int n_tslots, n_slots, start_hour;
extern Seln_client s_client;
extern Xv_server server;
int attr_bi;
struct appt_entry shelf_appt = {0};
int old_slot = -1;	/* for text cursor location */
int box_index, found_flag;
char sel_text[MAX_STRLEN];
static char *get_shelf();
extern int chk_deleted();
void day_sel_menu_done_proc(), del_done();

#ifndef min
#define min(a,b)	   ((a)<(b)?(a):(b))
#endif

#ifdef NEEDS_EXTRA_PROTOS
#ifdef __STDC__
extern int select (int, fd_set *, fd_set *, fd_set *, struct timeval *);
#else
extern int select ();
#endif
#endif

#ifdef __STDC__
void text_cursor (int slotno);
void do_left_arrow (int bi);
void do_right_arrow (int bi);
void make_box_active (int bi);
void next_appt (int bi, int dpyflag);
void deactivate_lower_arrows (int bi, int dpyflag);
void paste_only (void);
void modify_appt (int bi);
void fill_appt (int bi);
void cut_delete (int bi);
void set_attr (void);
#else
void text_cursor ();
void do_left_arrow ();
void do_right_arrow ();
void make_box_active ();
void next_appt ();
void deactivate_lower_arrows ();
void paste_only ();
void modify_appt ();
void fill_appt ();
void cut_delete ();
void set_attr ();
#endif

int
day_inputevent(window, event)
Xv_Window window;
Event *event;
{
	Menu_item an_item;
	int x, y;
	int i, j, strl;
	struct appt_entry *aptr;
	static int start_arrow_box = -1, prev_box = 0;
	static int expecting = 0;
	char *paste_str;

	if (event_id(event) == WIN_NO_EXPOSE)
		/* optimization, since we get a lot of these events */
		return(0);
	/*** DEBUG ***/
	/**
	fprintf(stderr, "devent = %d, action = %d\n", event_id(event), event_action(event));
	**/
	if (event_id(event) == LOC_WINEXIT && old_slot >= 0) {
		/* erase text cursor */
		text_cursor(old_slot);
		old_slot = -1;
		return(0);
	}
	drawable = (Drawable)xv_get(cpwindow, XV_XID);
	if ((event_id(event) == KEY_TOP(6) || event_action(event) == ACTION_GO_PAGE_BACKWARD)
	    && event_is_up(event)) {
		yesterday();
	} else if ((event_id(event) == KEY_TOP(7) || event_action(event) == ACTION_GO_PAGE_FORWARD)
	    && event_is_up(event)) {
		tomorrow();
	}
	x = event_x(event);
	y = event_y(event);
	found_flag = 0;		/* See if cursor is in a box. */

	for (box_index=0; box_index<n_slots; box_index++) {
		/* is cursor inside a slot ? */
		if (x>=slots[box_index].slot_pos.left && x<=slots[box_index].slot_pos.right &&
		    y>=slots[box_index].slot_pos.top && y<=slots[box_index].slot_pos.bottom) {
			found_flag = FOUND_SLOT;
			break;
		}
		/* is cursor inside a "more" button ? */
		if (x>=slots[box_index].moreb_pos.left && x<=slots[box_index].moreb_pos.right &&
		    y>=slots[box_index].moreb_pos.top && y<=slots[box_index].moreb_pos.bottom) {
			if (slots[box_index].active > 1) {
				/* "more" button is active */
				found_flag = FOUND_MORE;
				break;
			}
		}
		/* is cursor inside a "leftarrow" button ? */
		if (x>=slots[box_index].larrow_pos.left && x<=slots[box_index].larrow_pos.right &&
		    y>=slots[box_index].larrow_pos.top && y<=slots[box_index].larrow_pos.bottom) {
			found_flag = FOUND_LARROW;
			break;
		}
		/* is cursor inside a "rightarrow" button ? */
		if (x>=slots[box_index].rarrow_pos.left && x<=slots[box_index].rarrow_pos.right &&
		    y>=slots[box_index].rarrow_pos.top && y<=slots[box_index].rarrow_pos.bottom) {
			found_flag = FOUND_RARROW;
			break;
		}
	}
	if (old_slot >= 0) {
		/* erase text cursor at old location */
		text_cursor(old_slot);
		old_slot = -1;
	}

	if (!found_flag && !expecting)
		return(0);		/* Not in a box => ignore. */

	if (found_flag == FOUND_SLOT && box_index != old_slot) {
		/* in a different slot than we were before */
		if (slots[box_index].active)
			/* display cursor at new location */
			text_cursor(box_index);
	}
	if (found_flag == FOUND_SLOT && event_action(event) == ACTION_PASTE
	    && event_is_up(event)) {
		/*
		 * Process a "Paste" ("Get") event by pasting the text
		 * from the SHELF. Note that this is different from
		 * pasting an appointment.
		 */
		if (!slots[box_index].active || read_only)
			return(0);
		new_entry = 1;	/* flag for file updating */
		strl = strlen(slots[box_index].cur_appt->str);
		paste_str = get_shelf();
		if (paste_str == NULL) {
			text_cursor(box_index);
			return(0);
		}
		strncpy(slots[box_index].cur_appt->str + strl, paste_str,
		    min(strlen(paste_str),MAX_STRLEN - strl ));
		slots[box_index].cur_appt->str[min(strlen(paste_str) + strl, MAX_STRLEN)] = '\0';
		rewrite_string(box_index, JUSTIFY_LEFT);
		/* display cursor at new location */
		text_cursor(box_index);
	} else if (event_action(event) == ACTION_COPY && event_is_up(event)) {
		/* put string for current appt on the shelf */
		if (found_flag == FOUND_SLOT && slots[box_index].active)
			/* we're in an active slot */
			strcpy(sel_text, slots[box_index].cur_appt->str);
		else
			sel_text[0] = '\0';
	} else if (found_flag == FOUND_SLOT && event_is_iso(event) && event_is_up(event)) {
		/* Process a kbd event. */
		if (!slots[box_index].active)
			return(0);
		if (event_id(event) == CTRL_R) {
			rewrite_string(box_index, JUSTIFY_LEFT);
			return(0);
		}
		if (slots[box_index].cur_appt->flags & READONLY)
			return(0);
		text_cursor(box_index); /* erase old cursor */
		new_entry = 1;	/* flag for file updating */
		strl = strlen(slots[box_index].cur_appt->str);
		if (event_id(event) == CTRL_U) {
			slots[box_index].cur_appt->str[0] = '\0';
			rewrite_string(box_index, JUSTIFY_LEFT);
		} else if (event_id(event) == CTRL_W) {
			while (strl > 0 && slots[box_index].cur_appt->str[strl-1] != ' ') {
				slots[box_index].cur_appt->str[strl-1] = '\0';
				strl--;
			}
			rewrite_string(box_index, JUSTIFY_RIGHT);
		} else if (event_id(event) == DEL || event_id(event) == BACKSPACE) {
			if (strl > 0) {
				slots[box_index].cur_appt->str[strl-1] = '\0';
				rewrite_string(box_index, JUSTIFY_RIGHT);
			}
		} else if (event_id(event) >= (int)' ' && strl < MAX_STRLEN-2) {
			if (event_is_string(event)) {
				/* key has been mapped to generate a string */
				if (strl+strlen(event_string(event)) < MAX_STRLEN-1) {
					strcat(&slots[box_index].cur_appt->str[strl-1], (char *)event_string(event));
					strl += strlen(event_string(event));
				} else {
					err_rpt("appointment entry string too long", NON_FATAL);
				}
			} else {
				/* single character key mapping */
				slots[box_index].cur_appt->str[strl] = (char)event_id(event);
				slots[box_index].cur_appt->str[strl+1] = '\0';
			}
			rewrite_string(box_index, JUSTIFY_RIGHT);
		}
		/* display cursor at new location */
		text_cursor(box_index);
	} else if (found_flag == FOUND_SLOT && (event_action(event) == ACTION_ERASE_CHAR_BACKWARD || event_action(event) == ACTION_ERASE_WORD_BACKWARD) && event_is_up(event)) {
		if (!slots[box_index].active)
			return(0);
		if (slots[box_index].cur_appt->flags & READONLY)
			return(0);
		text_cursor(box_index); /* erase old cursor */
		new_entry = 1;	/* flag for file updating */
		strl = strlen(slots[box_index].cur_appt->str);
		if (event_action(event) == ACTION_ERASE_WORD_BACKWARD) {
			while (strl > 0 && slots[box_index].cur_appt->str[strl-1] != ' ') {
				slots[box_index].cur_appt->str[strl-1] = '\0';
				strl--;
			}
			rewrite_string(box_index, JUSTIFY_RIGHT);
		} else if (event_action(event) == ACTION_ERASE_CHAR_BACKWARD) {
			if (strl > 0) {
				slots[box_index].cur_appt->str[strl-1] = '\0';
				rewrite_string(box_index, JUSTIFY_RIGHT);
			}
		}
		/* display cursor at new location */
		text_cursor(box_index);
	} else if (event_action(event) == ACTION_SELECT && event_is_down(event)) {
		/* LB down event */
		switch (found_flag) {
			case FOUND_SLOT:
				break;
			case FOUND_MORE:
				/* reverse video "more" button */
				XSetFunction(mydisplay, gcc, GXinvert);
				XFillRectangle(mydisplay, drawable, gcc,
					slots[box_index].moreb_pos.left,
					slots[box_index].moreb_pos.top,
					m_width,
					m_height);
				expecting = box_index + (FOUND_MORE<<8);
				break;
			case FOUND_LARROW:
				do_left_arrow(box_index);
				break;
			case FOUND_RARROW:
				do_right_arrow(box_index);
				break;
		}
	} else if (event_action(event) == ACTION_SELECT && event_is_up(event)) {
		/* Process an LB up click. */
		i = expecting>>8;
		if (expecting && found_flag != i) {
			/* return button to normal video */
			if (i == FOUND_MORE) {
				/* "more" button */
				i = expecting & 0xff;
				XSetFunction(mydisplay, gcc, GXinvert);
				XFillRectangle(mydisplay, drawable, gcc,
					slots[box_index].moreb_pos.left,
					slots[box_index].moreb_pos.top,
					m_width,
					m_height);
			}
		} else {
			switch (found_flag) {
				case FOUND_SLOT:
					if (!read_only) {
						make_box_active(box_index);
						new_entry = 1;
					}
					break;
				case FOUND_MORE:
					next_appt(box_index, TRUE);
					/* normal video "more" button */
					XSetFunction(mydisplay, gcc, GXcopy);
					XCopyArea(mydisplay, morebutton, drawable, gcc, 0, 0,
						m_width, m_height,
						slots[box_index].moreb_pos.left,
						slots[box_index].moreb_pos.top);
					break;
				case FOUND_LARROW:
					break;
				case FOUND_RARROW:
					break;
			}
		}
		expecting = 0;
	} else if (found_flag == FOUND_SLOT && event_action(event) == ACTION_ADJUST) {
		/* Process a MB click. */
		if (event_is_down(event)) {
			/* try to start dragging from here */
			if (!slots[box_index].active || box_index >= n_tslots) {
				/* not allowed in notes slots, either */
				start_arrow_box = -1;
				return(0);
			}
			if ((slots[box_index].cur_appt->flags & READONLY) || read_only) {
				start_arrow_box = -1;
				return(0);
			}
			if (slots[box_index].cur_appt->arrows > 0) {
				/* remove old arrows and adjust counts */
				deactivate_lower_arrows(box_index, TRUE);
				j = slots[box_index].cur_appt->arrows;
				while (j > 0)
					slots[box_index+(j--)].count--;
				slots[box_index].cur_appt->arrows = 0;
			}
			prev_box = start_arrow_box = box_index;
		} else {
			if (box_index >= n_tslots) {
				/* mouse currently in notes section */
				if (start_arrow_box == -1)
					/* started in notes section, too */
					return(0);
				else
					/* truncate at start of notes section */
					box_index = n_tslots - 1;
			}
			/* end of dragging => end of arrow */
			if (box_index > start_arrow_box && start_arrow_box != -1) {
				int left = (dayslot_width-2)/2 - 8;

				i = start_arrow_box;
				slots[i].cur_appt->arrows = box_index - start_arrow_box;
				XSetFunction(mydisplay, gcc, GXxor);
				while (++i < box_index) {
					slots[i].count++;
					/*
					 * erase arrow shaft on boxes - it will be
					 * replaced by a real arrowshaft during redraw
					 */
					XCopyArea(mydisplay, arrowshaft_pr, drawable, gcc,
						0, 0,
						16, dayslot_height-2,
						slots[i].slot_pos.left+left,
						slots[i].slot_pos.top+1);
				}
				slots[i].count++;
				/*
				 * erase arrow shaft on last box - it will be
				 * replaced by an arrowhead during redraw
				 */
				XCopyArea(mydisplay, arrowshaft_pr, drawable, gcc,
					0, 0,
					16, dayslot_height-2,
					slots[i].slot_pos.left+left,
					slots[i].slot_pos.top+1);
			}
			start_arrow_box = -1;
			new_entry = 1;
			draw_day_appts();
		}
	} else if (found_flag == FOUND_SLOT && event_id(event) == LOC_DRAG) {
		/* mouse dragging - is it the middle button ? */
		if (action_adjust_is_down(event) && start_arrow_box >= 0) {
			int left = (dayslot_width-2)/2 - 8;

			if (box_index >= n_tslots)
				/* don't flow into notes section */
				box_index = n_tslots - 1;
			/*
			 * xor arrow shaft thru current slot so
			 * we can see where we're dragging
			 */
			if (box_index > prev_box) {
				while (++prev_box <= box_index) {
					XCopyArea(mydisplay, arrowshaft_pr, drawable, gcc,
						0, 0,
						16, dayslot_height-2,
						slots[prev_box].slot_pos.left+left,
						slots[prev_box].slot_pos.top+1);
				}
				prev_box = box_index;
			} else if (box_index < prev_box && box_index >= start_arrow_box) {
				/* going backwards - cleanup as we go */
				while (prev_box > box_index) {
					XCopyArea(mydisplay, arrowshaft_pr, drawable, gcc,
						0, 0,
						16, dayslot_height-2,
						slots[prev_box].slot_pos.left+left,
						slots[prev_box].slot_pos.top+1);
					--prev_box;
				}
			}
		}
	} else if (found_flag == FOUND_SLOT && event_action(event) == ACTION_MENU && event_is_down(event)) {
		/* Process a RB click. */
		/*
		 * display popup menu of choices, but first disable
		 * certain entries if this is a readonly appointment
		 * or an empty slot.
		 */
		/* MENU_NTH_ITEM appears to include the title in the count,
		 * so the xv_get's add one to item number to get the right one.
		 */
		/* undelete - almost always inactive */
		an_item = xv_get(day_sel_menu, MENU_NTH_ITEM, MUNDELETE+1);
		xv_set(an_item, MENU_INACTIVE, TRUE, 0);
		if (read_only) {
			/* everything is inactive */
			paste_only();
			/* paste */
			an_item = xv_get(day_sel_menu, MENU_NTH_ITEM, MPASTE+1);
			xv_set(an_item, MENU_INACTIVE, TRUE, 0);
		} else if (!slots[box_index].first) {
			/* empty slot. only paste active */
			paste_only();
		} else if (slots[box_index].cur_appt->flags & READONLY) {
			/* readonly => paste and copy only */
			paste_only();
			/* copy */
			an_item = xv_get(day_sel_menu, MENU_NTH_ITEM, MCOPY+1);
			xv_set(an_item, MENU_INACTIVE, FALSE, 0);
		} else {
			/* delete */
			an_item = xv_get(day_sel_menu, MENU_NTH_ITEM, MDELETE+1);
			xv_set(an_item, MENU_INACTIVE, FALSE, 0);
			/* cut */
			an_item = xv_get(day_sel_menu, MENU_NTH_ITEM, MCUT+1);
			xv_set(an_item, MENU_INACTIVE, FALSE, 0);
			/* copy */
			an_item = xv_get(day_sel_menu, MENU_NTH_ITEM, MCOPY+1);
			xv_set(an_item, MENU_INACTIVE, FALSE, 0);
			/* modify */
			an_item = xv_get(day_sel_menu, MENU_NTH_ITEM, MMODIFY+1);
			xv_set(an_item, MENU_INACTIVE, FALSE, 0);
			for (aptr=slots[box_index].first; aptr; aptr=aptr->next)
				if (aptr->flags & DELETED) {
					if (!slots[box_index].active)
						/* only paste and undelete */
						paste_only();
					/* undelete */
					an_item = xv_get(day_sel_menu, MENU_NTH_ITEM, MUNDELETE+1);
					xv_set(an_item, MENU_INACTIVE, FALSE, 0);
					break;
				}
		}
		xv_set(day_sel_menu, XV_KEY_DATA, MENU_KEY, box_index, 0);
		menu_show(day_sel_menu, window, event, 0);
	} else if (found_flag == FOUND_SLOT && event_action(event) == ACTION_PROPS && event_is_down(event)) {
		if (slots[box_index].first && !read_only)
			modify_appt(box_index);
	}
		
	return(1);
}


/* make "paste" the only active menu entry */
void
paste_only()
{
	Menu_item an_item;

	/* delete */
	an_item = xv_get(day_sel_menu, MENU_NTH_ITEM, MDELETE+1);
	xv_set(an_item, MENU_INACTIVE, TRUE, 0);
	/* cut */
	an_item = xv_get(day_sel_menu, MENU_NTH_ITEM, MCUT+1);
	xv_set(an_item, MENU_INACTIVE, TRUE, 0);
	/* copy */
	an_item = xv_get(day_sel_menu, MENU_NTH_ITEM, MCOPY+1);
	xv_set(an_item, MENU_INACTIVE, TRUE, 0);
	/* modify */
	an_item = xv_get(day_sel_menu, MENU_NTH_ITEM, MMODIFY+1);
	xv_set(an_item, MENU_INACTIVE, TRUE, 0);
}

/* draw (or erase) text cursor in a day slot */
void
text_cursor(slotno)
int slotno;
{
#if 0
	int	strl, x;
	static int erase = 0;
#endif

	if (slots[slotno].cur_appt == NULL)
		return;
#if 0
	/*
	 * This is commented out for now because it is too slow. A more
	 * efficient method needs to be found. Originally, it used a GXxor
	 * function, but you never know what color the marker would appear in.
	 */
	strl = strlen(&slots[slotno].cur_appt->str[slots[slotno].cur_appt->sindex]);
	if (strl <= (day_message_size-1)) {
		x = slots[slotno].slot_pos.left + strl * xv_get(font, FONT_DEFAULT_CHAR_WIDTH);
		drawable = (Drawable)xv_get(cpwindow, XV_XID);
		XSetFunction(mydisplay, gcc, GXcopy);
		if (erase) {
			/* restore saved area */
			XCopyArea(mydisplay, savePix, drawable, gcc,
				0, 0,
				16, 16,
				x,
				slots[slotno].slot_pos.bottom-8);
			erase = 0;
		} else {
			/* save current area before drawing on it */
			XCopyArea(mydisplay, drawable, savePix, gcc,
				x,
				slots[slotno].slot_pos.bottom-8,
				16, 16,
				0, 0);
			XSetClipMask(mydisplay, gcc, tri_up_pr);
			XSetClipOrigin(mydisplay, gcc, x,
				slots[slotno].slot_pos.bottom-8);
			XCopyArea(mydisplay, ydaybox_pr, drawable, gcc,
				0, 0,
				16, 16,
				x,
				slots[slotno].slot_pos.bottom-8);
			XSetClipMask(mydisplay, gcc, None);
			erase = 1;
		}
	}
#endif
	old_slot = slotno;
}

/* make slot active */
void
make_box_active(bi)
int bi;
{
	add_to_slot(bi, NULL, TRUE);
	fill_appt(bi);
	rewrite_string(bi, JUSTIFY_LEFT);
	text_cursor(bi);
}

/* activate a hidden appt and make it visible */
int
activate_slot(bi, dpyflag)
int bi;
int dpyflag;
{
	if (slots[bi].active <= 0)
		/* nothing to activate */
		return(0);

	if (dpyflag)
		draw_day_appts();	/* redraw display */

	return(1);
}

/* clears a day slot */
void
deactivate_slot(bi, dpyflag)
int bi, dpyflag;
{
	if (!dpyflag)
		return;
	/* erase text cursor at old location */
	if (old_slot >= 0) {
		text_cursor(old_slot);
		old_slot = -1;
	}
	/* erase displayed slot */
	drawable = (Drawable)xv_get(cpwindow, XV_XID);
	if (!ymd_compare(current, today)) {
		XSetFunction(mydisplay, gcc, GXcopy);
		XCopyArea(mydisplay, timeslot_td_pr, drawable, gcc, 0, 0,
			dayslot_width, dayslot_height,
			slots[bi].slot_pos.left, slots[bi].slot_pos.top);
	} else {
		XClearArea(mydisplay, drawable,
			slots[bi].slot_pos.left+1,
			slots[bi].slot_pos.top+1,
			dayslot_width-2,
			dayslot_height-2,
			FALSE);
	}
	if (slots[bi].cur_appt->arrows > 0)
		deactivate_lower_arrows(bi, dpyflag);
}

/* clear any displayed arrowshafts and arrowheads */
void
deactivate_lower_arrows(bi, dpyflag)
int bi, dpyflag;
{
	int narrows, offset;

	if (!dpyflag)
		return;
	narrows = slots[bi].cur_appt->arrows;
	offset = (slots[bi].count - slots[bi].active + 1) * 40;
	drawable = (Drawable)xv_get(cpwindow, XV_XID);
	XSetFunction(mydisplay, gcc, GXcopy);
	while (narrows-- > 0) {
		bi++;
		if (slots[bi].active)
			continue;
		/* erase displayed arrowshaft or arrowhead */
		if (!ymd_compare(current, today)) {
			XCopyArea(mydisplay, timeslot_td_pr, drawable, gcc, 0, 0,
				16, dayslot_height-2,
				slots[bi].slot_pos.left+1+offset,
				slots[bi].slot_pos.top+1);
		} else {
			XClearArea(mydisplay, drawable,
				slots[bi].slot_pos.left+1+offset,
				slots[bi].slot_pos.top+1,
				16,
				dayslot_height-2,
				FALSE);
		}
	}
}

/* fill in appt struct with current info */
void
fill_appt(bi)
int bi;
{
	slots[bi].cur_appt->year = current.tm_year;
	slots[bi].cur_appt->month = current.tm_mon;
	slots[bi].cur_appt->day = current.tm_mday;
	slots[bi].cur_appt->arrows = 0;
	slots[bi].cur_appt->flags = slots[bi].cur_appt->repeat = 0;
	slots[bi].cur_appt->lookahead = slots[bi].cur_appt->sindex = 0;
	slots[bi].cur_appt->runlength = 0;
	slots[bi].cur_appt->warn = 10;
	if (bi >= n_tslots) {
		/* notes section */
		slots[bi].cur_appt->hour = 99;
		slots[bi].cur_appt->minute = 0;
		slots[bi].cur_appt->flags = A_NOTE;
	} else {
		/* regular appt */
		slots[bi].cur_appt->hour = bi/2 + start_hour;
		slots[bi].cur_appt->minute = (bi % 2) * 30;
	}
	slots[bi].cur_appt->str[0] = '\0';
}

/*
 * Display delete popup window to let user choose delete mode for
 * recurring appts (delete this one only or delete all), otherwise,
 * just wipe it out with no options.
 */
void
delete_appt(bi)
int bi;
{
	int value;

	if (slots[bi].cur_appt->flags & READONLY) {
		err_rpt("Can't delete a read-only appt", NON_FATAL);
		return;
	}
	if (Repeating(slots[bi].cur_appt->flags)) {
		attr_bi = bi;	/* set global index for notify func */

		value = notice_prompt(cpwindow, NULL,
			NOTICE_MESSAGE_STRINGS,
			"This is a recurring appointment",
			"Do you wish to:", NULL,
			NOTICE_BUTTON, "Delete this occurrence	    ", 0,
			NOTICE_BUTTON, "Delete all occurrences	    ", 1,
			NOTICE_BUTTON, "Abort ", 2,
			NULL);
		
		if (value != 2)
			del_done(value, FALSE);
	} else {
		cut_delete(bi);
		new_entry = 1;
	}
}

void
cut_appt(bi)
int bi;
{
	int value;

	/* cut (delete) current entry, saving the info on the "shelf" */
	if (slots[bi].cur_appt->flags & READONLY) {
		err_rpt("Can't cut a read-only appt", NON_FATAL);
		return;
	}
	shelf_appt = *slots[bi].cur_appt;
	if (Repeating(slots[bi].cur_appt->flags)) {
		attr_bi = bi;	/* set global index for notify func */

		value = notice_prompt(cpwindow, NULL,
			NOTICE_MESSAGE_STRINGS,
			"This is a recurring appointment",
			"Do you wish to:", NULL,
			NOTICE_BUTTON, "Move this occurrence	", 0,
			NOTICE_BUTTON, "Move all occurrences	", 1,
			NOTICE_BUTTON, "Abort ", 2,
			NULL);
		if (value != 2)
			del_done(value, TRUE);
	} else {
		cut_delete(bi);
		new_entry = 1;
	}
}

/*
 * Called from the delete/cut notice prompt
 */
void
del_done(value, saveit)
int value;
int saveit;
{
	struct appt_entry tmp;

	if (value == 0) {
		/* don't show it today */
		/* create duplicate entry with delete flag set */
		tmp = *slots[attr_bi].cur_appt;
		tmp.flags &= ~(ALL_YEARS|ALL_MONTHS|ALL_DAYS|EVERY_SOMEDAY|REPEAT|EVERY_MON_FRI|RUN);
		tmp.flags |= DELETED;
		tmp.year = current.tm_year;
		tmp.month = current.tm_mon;
		tmp.day = current.tm_mday;
		add_to_slot(attr_bi, &tmp, TRUE);
		/* in the case of "cut", modify the shelf appt */
		if (saveit) {
			shelf_appt.flags &= ~(ALL_YEARS|ALL_MONTHS|ALL_DAYS|EVERY_SOMEDAY|REPEAT|EVERY_MON_FRI|RUN);
			shelf_appt.year = current.tm_year;
			shelf_appt.month = current.tm_mon;
			shelf_appt.day = current.tm_mday;
		}
	} else {
		/* completely kill appt */
		cut_delete(attr_bi);
	}
	new_entry = 1;
}

void
cut_delete(bi)
int bi;
{
	int j;
	struct appt_entry *aptr, *cptr, *optr;

	cptr = slots[bi].cur_appt;
	optr = cptr; /* to keep gcc quiet */
	slots[bi].count--;
	slots[bi].active--;
	deactivate_slot(bi, TRUE);
	if ( (j = cptr->arrows) > 0) {
		/* adjust counts */
		while (j > 0)
			slots[bi+(j--)].count--;
	}
	if (slots[bi].cur_appt == slots[bi].first) {
		/* displaying first entry in list */
		/* see if there's any more */
		if (slots[bi].first->next)
			slots[bi].first = slots[bi].first->next;
		else {
			/* last one */
			slots[bi].first = NULL;
			slots[bi].active = 0;
		}
		slots[bi].cur_appt = slots[bi].first;
	} else {
		/* not first, so find previous one to this */
		for (aptr=slots[bi].first; slots[bi].cur_appt!=aptr; optr=aptr,aptr=aptr->next)
			;
		slots[bi].cur_appt = optr->next = aptr->next;
		if (!optr->next)
			slots[bi].cur_appt = slots[bi].first;
	}
	free(cptr);
	(void)activate_slot(bi, TRUE);	/* show any hidden appts */
}

void
copy_appt(bi)
int bi;
{
	/* copy current entry, saving the info on the "shelf" */
	shelf_appt = *slots[bi].cur_appt;
}

void
paste_appt(bi)
int bi;
{
	/* insert the saved entry (if any) */
	if (shelf_appt.str[0] == '\0') {
		err_rpt("nothing to paste", NON_FATAL);
		return;
	}
	shelf_appt.year = current.tm_year;
	shelf_appt.month = current.tm_mon;
	shelf_appt.day = current.tm_mday;
	if (shelf_appt.flags & EVERY_SOMEDAY) {
		/* change repeating appt to this day */
		shelf_appt.flags &= ~EVERY_SOMEDAY;
		shelf_appt.flags |= Setday(current.tm_wday);
	}
	if (bi >= n_tslots) {
		/* notes section */
		shelf_appt.hour = 99;
		shelf_appt.minute = 0;
		/* just in case converting from time to note */
		shelf_appt.flags |= A_NOTE;
	} else {
		/* regular appt */
		shelf_appt.hour = bi/2 + start_hour;
		shelf_appt.minute = (bi % 2) * 30;
		/* just in case converting from note to time */
		shelf_appt.flags &= ~MARKED_NOTE;
	}
	add_to_slot(bi, &shelf_appt, TRUE);
	new_entry = 1;
}

/*
 * Display attributes popup window to let user modify
 * various appointment options (such as repeat interval,
 * etc.)
 */
void
modify_appt(bi)
int bi;
{
	Rect *canvas_r;
	int top, left, width, height;

	if (slots[bi].cur_appt->flags & READONLY) {
		err_rpt("Can't modify a read-only appt", NON_FATAL);
		return;
	}
	if (!attr_frame)
		create_attr_frame();
	attr_bi = bi;	/* set global index for notify func */
	set_attr();	/* set panel item current values */

	/* get x,y position of canvas window on the screen so we
	 * can center this one in it.
	 */
	canvas_r = (Rect *) xv_get(cpwindow, WIN_RECT);
	width = (int) xv_get(attr_frame, XV_WIDTH);
	height = (int) xv_get(attr_frame, XV_HEIGHT);
	left =	canvas_r->r_left + (canvas_r->r_width - width) / 2;
	top =  canvas_r->r_top + (canvas_r->r_height - height) / 2;
	xv_set(attr_frame, WIN_X, left, WIN_Y, top, 0);
	xv_set(attr_frame, WIN_SHOW, TRUE, 0);
}

/* undelete a recurring appointment for this day */
/* we only get here if a deleted appt exists */
void
undelete_appt(bi)
int bi;
{
	struct appt_entry *aptr, *optr;

	/* search list to find deleted entry */
	for (optr=aptr=slots[bi].first; aptr; optr=aptr,aptr=aptr->next)
		if (aptr->flags & DELETED)
			break;
	if (aptr == slots[bi].first)
		slots[bi].first = aptr->next;
	else
		optr->next = aptr->next;
	slots[bi].count++;
	slots[bi].active++;
	if (slots[bi].active == 1) {
		slots[bi].cur_appt = slots[bi].first;
		(void)activate_slot(bi, TRUE);
	}
	free(aptr);
	new_entry = 1;
}

void
set_attr()
{
	int everyx_val = 0, whichwk_val = 0;
	struct appt_entry *apt = slots[attr_bi].cur_appt;

	xv_set(repeat_pi, PANEL_VALUE, 0, NULL); /* set default */
	xv_set(remind_pi, PANEL_VALUE, 0, NULL); /* set default */
	xv_set(runl_pi, PANEL_VALUE, 0, NULL);	/* set default */
	if (apt->flags & EVERY_MON_FRI)
		everyx_val |= 0x1;
	else if (apt->flags & ALL_DAYS)
		everyx_val |= 0x2;
	if (apt->flags & ALL_MONTHS)
		everyx_val |= 0x8;
	if (apt->flags & ALL_YEARS)
		everyx_val |= 0x10;
	if (apt->flags & EVERY_SOMEDAY) {
		everyx_val |= 0x4;
		if (apt->repeat == ALL_WEEKS)
			whichwk_val = 0x40;
		else
			whichwk_val = apt->repeat;
		xv_set(repeat_pi, XV_SHOW, FALSE, 0);
		xv_set(whichwk_pi, XV_SHOW, TRUE, 0);
	} else {
		if (apt->repeat) {
			xv_set(repeat_pi, PANEL_VALUE, apt->repeat, NULL);
		}
		xv_set(whichwk_pi, XV_SHOW, FALSE, 0);
		xv_set(repeat_pi, XV_SHOW, TRUE, 0);
	}
	xv_set(everyx_pi, PANEL_VALUE, everyx_val, NULL);
	xv_set(whichwk_pi, PANEL_VALUE, whichwk_val, NULL);
	if (apt->flags & LOOKAHEAD) {
		xv_set(remind_pi, PANEL_VALUE, apt->lookahead, NULL);
	}
	if (apt->flags & RUN) {
		xv_set(runl_pi, PANEL_VALUE, apt->runlength, NULL);
	}
	xv_set(marked_pi, PANEL_VALUE, (apt->flags & MARKED ? 0 : 1), NULL);
	xv_set(advw_pi, PANEL_VALUE, apt->warn, NULL);
	if (apt->flags & A_NOTE)
		xv_set(marked_pi, XV_SHOW, TRUE, 0);
	else
		xv_set(marked_pi, XV_SHOW, FALSE, 0);
}

/* "more" button selected. Display next appt in rotation. */
void
next_appt(bi, dpyflag)
int bi;
int dpyflag;
{
	static int loopcnt = 0;

	if (slots[bi].active)
		deactivate_slot(bi, dpyflag);

	if (slots[bi].cur_appt->next == NULL) {
		/* end of the chain */
		slots[bi].cur_appt = slots[bi].first;
		if (loopcnt) {
			/* infinite loop detected */
			loopcnt = 0;
			return;
		} else
			++loopcnt;
	} else
		/* activate next in chain */
		slots[bi].cur_appt = slots[bi].cur_appt->next;
	/* make sure it is not a deleted one */
	if (chk_deleted(&slots[bi], slots[bi].cur_appt))
		next_appt(bi, dpyflag); /* try next in chain */
	else if (!activate_slot(bi, dpyflag))
		next_appt(bi, dpyflag); /* try next in chain */
	loopcnt = 0;
}

/* left scroll arrow selected */
void
do_left_arrow(bi)
int bi;
{
	struct timeval timeout;
	Event event;
	int ertn;

	if (!slots[bi].active || strlen(slots[bi].cur_appt->str) < day_message_size)
		return;
	timeout.tv_sec = 0L;
	timeout.tv_usec = 100000L;	/* 1/10 sec */
	drawable = (Drawable)xv_get(cpwindow, XV_XID);
	/* reverse video the arrow */
	XSetFunction(mydisplay, gcc, GXinvert);
	XFillRectangle(mydisplay, drawable, gcc,
		slots[bi].larrow_pos.left,
		slots[bi].larrow_pos.top,
		19,
		14);
	while (TRUE) {
		if (slots[bi].cur_appt->sindex < strlen(slots[bi].cur_appt->str)) {
			++slots[bi].cur_appt->sindex;
			rewrite_string(bi, JUSTIFY_INDEX);
			/* reverse video the arrow (rewrite changed it) */
			XFillRectangle(mydisplay, drawable, gcc,
				slots[bi].larrow_pos.left,
				slots[bi].larrow_pos.top,
				19,
				14);
		}
		/* do this garbage to handle a repeat function */
		XFlush(mydisplay);
		(void)select(ConnectionNumber(mydisplay), NULL, NULL, NULL, &timeout);
		ertn = window_read_event(cpwindow, &event);
		if (ertn != -1 && event_is_up(&event))
			break;
	}
	/* put arrow back to normal */
	XSetFunction(mydisplay, gcc, GXcopy);
	XCopyArea(mydisplay, leftarrow, drawable, gcc, 0, 0,
		19, 14,
		slots[bi].larrow_pos.left,
		slots[bi].larrow_pos.top);
}

/* right scroll arrow selected */
void
do_right_arrow(bi)
int bi;
{
	struct timeval timeout;
	Event event;
	int ertn;

	if (!slots[bi].active || strlen(slots[bi].cur_appt->str) < day_message_size)
		return;
	timeout.tv_sec = 0L;
	timeout.tv_usec = 100000L;	/* 1/10 sec */
	drawable = (Drawable)xv_get(cpwindow, XV_XID);
	/* reverse video the arrow */
	XSetFunction(mydisplay, gcc, GXinvert);
	XFillRectangle(mydisplay, drawable, gcc,
		slots[bi].rarrow_pos.left,
		slots[bi].rarrow_pos.top,
		19,
		14);
	while (TRUE) {
		if (slots[bi].cur_appt->sindex > 0) {
			--slots[bi].cur_appt->sindex;
			rewrite_string(bi, JUSTIFY_INDEX);
			/* reverse video the arrow (rewrite changed it) */
			XSetFunction(mydisplay, gcc, GXinvert);
			XFillRectangle(mydisplay, drawable, gcc,
				slots[bi].rarrow_pos.left,
				slots[bi].rarrow_pos.top,
				19,
				14);
		}
		/* do this garbage to handle a repeat function */
		XFlush(mydisplay);
		(void)select(ConnectionNumber(mydisplay), NULL, NULL, NULL, &timeout);
		ertn = window_read_event(cpwindow, &event);
		if (ertn != -1 && event_is_up(&event))
			break;
	}
	/* put arrow back to normal */
	XSetFunction(mydisplay, gcc, GXcopy);
	XCopyArea(mydisplay, rightarrow, drawable, gcc, 0, 0,
		19, 14,
		slots[bi].rarrow_pos.left,
		slots[bi].rarrow_pos.top);
}

/*
 * get_shelf - get text from selection service shelf for copy
 * operation. From Mark Feblowitz <mdf0%shemesh@gte.COM>.
 */
static
char *
get_shelf()
{
	Seln_holder	holder;
	Seln_request	*buffer;

	holder = selection_inquire(server, SELN_SHELF);
	/* do we have the shelf? */
	if (!seln_holder_same_client(&holder, s_client)) {
		buffer = selection_ask(server, &holder, SELN_REQ_CONTENTS_ASCII, 0, 0);
		(void) strncpy(sel_text, buffer->data + sizeof(SELN_REQ_CONTENTS_ASCII),
			MAX_STRLEN-1);
		sel_text[MAX_STRLEN-1] = '\0';
		if (strlen(sel_text) == 0)
			/* empty string is no sel. */
			return(NULL);
		sel_text[MAX_STRLEN-1] = '\0';
	}
	return(sel_text);
}

/*
 * respond to function keys the selection service thinks
 * are important
 */
void
sel_func_key_proc(client_data, args)
char *client_data;
Seln_function_buffer *args;
{
	Seln_holder *holder;
	Seln_response resp;

	if ((resp = selection_figure_response(server, args, &holder)) == SELN_SHELVE) {
		/* put string for current appt on the shelf */
		if (found_flag == FOUND_SLOT && slots[box_index].active)
			/* we're in an active slot */
			strcpy(sel_text, slots[box_index].cur_appt->str);
		else
			sel_text[0] = '\0';
	}
}

/*
 * called by selection svc library when someone requests our shelf
 * text. Abridged from the seln_demo() in the SunView manual.
 */
Seln_result
sel_reply_proc(item, context, length)
Seln_attribute item;
Seln_replier_data *context;
int length;
{
	char *destp, *seln = NULL;
	int size;

	if (context->rank == SELN_SHELF)
		seln = sel_text;
	
	switch (item) {
		case SELN_REQ_CONTENTS_ASCII:
			/* send the contents of the selection buffer */
			if (seln == NULL)
				return(SELN_DIDNT_HAVE);
			context->context = seln;
			size = strlen(seln);
			destp = (char *)context->response_pointer;
			/* allow for padding */
			(void) strncpy(destp, seln, length-8);
			destp += size;
			/* pad to long word */
			while ((int)destp % 4 != 0)
				*destp++ = '\0';
			context->response_pointer = (char **)destp;
			*context->response_pointer++ = 0;
			break;
		
		case SELN_REQ_YIELD:
			*context->response_pointer++ = (char *)SELN_SUCCESS;
			break;
		
		case SELN_REQ_BYTESIZE:
			if (seln == NULL)
				return(SELN_DIDNT_HAVE);
			*context->response_pointer++ = (char *)strlen(seln);
			break;
			
		case SELN_REQ_END_REQUEST:
			break;
		
		default:
			return(SELN_UNRECOGNIZED);
	}

	return(SELN_SUCCESS);
}

void day_sel_menu_done_proc(menu, menu_item)
Menu menu;
Menu_item menu_item;
{
	int bi, value;

	value = xv_get(menu_item, MENU_VALUE);
	bi = (int)xv_get(day_sel_menu, XV_KEY_DATA, MENU_KEY);
	if (value > 0) {
		switch (value) {
			case MDELETE:
				delete_appt(bi);
				break;
			case MCUT:
				cut_appt(bi);
				break;
			case MCOPY:
				copy_appt(bi);
				break;
			case MPASTE:
				paste_appt(bi);
				break;
			case MMODIFY:
				modify_appt(bi);
				break;
			case MUNDELETE:
				undelete_appt(bi);
				break;
		}
		if (new_entry) {
			close_day();
			draw_day();	/* redraw display */
		}
	}
}
