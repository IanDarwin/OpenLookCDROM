/*
 * $Id: control.c,v 4.1 1992/10/06 11:25:51 jipping Exp $
 *
 *   Control.c ==> Routines that focus on window system interaction.
 *
 *   In reality, although is has been reduced for the 5.0 release, it 
 *   is WAY TOO LARGE.  But it contains all window reaction 
 *   routines/callbacks.  
 *
 * ----------------------------------------------------------------------
 * Copyright (c) 1993 by Mike Jipping and Hope College
 *
 * Permission is granted to copy and distribute this file in modified or
 * unmodified form, for noncommercial use, provided (a) this copyright notice
 * is preserved, (b) no attempt is made to restrict redistribution of this
 * file, and (c) this file is not distributed as part of any collection whose
 * redistribution is restricted by a compilation copyright.
 * ----------------------------------------------------------------------
 *
 * Revision History:
 *
 * $Log: control.c,v $
 * Revision 4.1  1992/10/06  11:25:51  jipping
 * BUG FIXES AFTER BETA TEST:  confirmation upon delete, rebuilding of
 * entry menu after category delete, fixed cut/paste of recurring entries
 *
 * Revision 4.0  1992/09/15  11:30:02  jipping
 * Release 4.0 beta.  MANY MANY changes -- they include:
 *    * changes to edit menu: addition of modify and delete category,
 *      changes to the way items are selected to be editted (select
 *      first, then choose menu item)
 *    * accomodation of deadline stuff in edit and create callbacks.
 *    * changes/bugfixes to copying recurring entries through the
 *      recurrence window (using PANEL_CLIENT_DATA, etc)
 *
 * Revision 3.1  1992/07/30  14:16:22  jipping
 * Fixed a bug with deleting the category being displayed.  Added a
 * notice when a category is created with a duplicate name.
 *
 * Revision 3.0  1992/07/27  18:30:59  jipping
 * Release 3.0 changes:
 * corrected pinning behavior (removed calls to make FRAME_CMD_PUSHPIN_IN
 * FALSE)
 * * corrected many list management bugs
 * * added callbacks to list all categories
 * * added traps for the "Quit" from window menu
 * * added callback for "Done" button (close_tdlist)
 *
 * Revision 2.6  1992/07/16  13:30:24  jipping
 * Enabled the editing and deletion of categories through
 * modifications to edit_it and switch_category
 *
 * Revision 2.5  1992/07/14  12:32:04  jipping
 * Eliminated callback for the text item in entry window.
 *
 * Revision 2.4  1992/07/13  17:03:34  jipping
 * More cleanup.  Nicer code in several spots.
 *
 * Revision 2.3  1992/07/13  15:52:22  jipping
 * Added a notify proc for the base window to allow resizing of todo
 * list when window is resized.
 *
 * Revision 2.2  1992/07/13  13:44:27  jipping
 * Fixed a 50-character display limit bug,
 *
 * Revision 2.0.1.1  1992/07/10  17:47:18  jipping
 * Attempted (without success) to squelch the bug where the edit window
 * does not disappear with first Return on entry text.
 *
 * Revision 2.0  1992/07/10  16:43:11  jipping
 * Initial Release.
 *
 *
 */

#include "globaldefs.h"

int file_status_ok();

void edit_it();
struct entry_list *entry_search();
extern Panel tdl_list;
extern FILE *yyin;

int editing=FALSE, copying=FALSE, cutting=FALSE;
	
/*
 *  go_forward -- move ahead the required number of days, depending on 
 *                whether the control or shift keys are pressed. 
 *
 *  Called from the "right arrow" button.
 */

void go_forward(item, event)
Panel_item item; 
Event      *event;
{
	if (event_shift_is_down(event)) {  /* move ahead a month */
		curr_month = (curr_month+1) % 12;
      if (curr_month == 0) curr_month = 12;
		if (curr_month == 1) curr_year++;

	} else if (event_ctrl_is_down(event)) {  /* move ahead a week */
		curr_day += 7;
		if (curr_day > daysinmonth(curr_month, curr_year)) {
			curr_day = curr_day - daysinmonth(curr_month, curr_year);
			curr_month = (curr_month+1) % 12;
			if (curr_month == 0) curr_month = 12;
			if (curr_month == 1) curr_year++;
		}

	} else if (event_meta_is_down(event)) {  /* move ahead a year */
		curr_year++;

	} else {      /* only move ahead a day */
		if ((curr_month == 12) & (curr_day == 31)) {
			curr_year ++;
			curr_day = curr_month = 1;
		} else if (curr_day < daysinmonth(curr_month, curr_year)) {
			curr_day ++;
		} else {
			curr_month ++;
			curr_day = 1;
		}
	}

	print_calendar(curr_month, curr_year);
	reset_date_message();
	display_list(curr_month, curr_day, curr_year);
}

/*
 *  go_backward -- move ahead the required number of days, depending on 
 *                 whether the control or shift keys are pressed. 
 *
 *  Called from the "left arrow" button.
 */

void go_backward(item, event)
Panel_item item; 
Event      *event;
{
	if (event_shift_is_down(event)) {  /* month */
		curr_month = (curr_month-1) % 12;
      if (curr_month == 0) {
			curr_month = 12;
			curr_year --;
		}
	} else if (event_ctrl_is_down(event)) {  /* week */
		curr_day -= 7;
		if (curr_day <= 0) {
			curr_month = (curr_month-1) % 12;
			if (curr_month == 0) {
				curr_month = 12;
				curr_year --;
			}
			curr_day = curr_day + daysinmonth(curr_month, curr_year);
		}
	} else if (event_meta_is_down(event)) {  /* year */
		curr_year--;
	} else {  /* daily */
		if ((curr_month == 1) & (curr_day == 1)) {
			curr_year --;
			curr_day = 31;
			curr_month = 12;
		} else if (curr_day > 1) {
			curr_day --;
		} else {
			curr_month --;
			curr_day = daysinmonth(curr_month, curr_year);
		}
	}
	print_calendar(curr_month, curr_year);
	reset_date_message();
	display_list(curr_month, curr_day, curr_year);
}

/*
 * go_today -- Go to the current (real) date.
 *
 * Connected to the "Today" button.
 */
void go_today(item, event)
Panel_item item; 
Event      *event;
{
   curr_month = today.tm_mon+1;
   curr_day = today.tm_mday;
   curr_year = today.tm_year+1900;
	
	print_calendar(curr_month, curr_year);
	reset_date_message();
	display_list(curr_month, curr_day, curr_year);
}

/*
 * **********************************************************************
 * Utility routine...copying the contents of the file denoted by "str"
 * into the file denoted by the descriptor "fd".
 */

void copyfile2(str, fd)
char *str;
FILE *fd;
{
	FILE    *spfp;
	int     t;
	
	if((spfp=fopen(str,"r")) == NULL) {
		fprintf(stderr,"Unable to open file %s\n",str);
		return;
	}
	while ((t=getc(spfp)) != EOF ) {
		putc(t, fd);
	}
	fclose(spfp);
}

/*
 * **********************************************************************
 * Closes the create/edit window.  Called from the "Cancel" button.
 */

void close_create(item, event)
Panel_item item; 
Event      *event;
{
	xv_set(entry_text, PANEL_VALUE, "", 0);
	xv_set(entry_editor->entry_priority, PANEL_VALUE, default_priority, 0);
	xv_set(entry_category, PANEL_INACTIVE, FALSE, 0);
	xv_set(entry_category_name, PANEL_INACTIVE, FALSE, 0);
	xv_set(entry_frame, XV_SHOW, FALSE, 0);
	display_list(curr_month, curr_day, curr_year);
}

/*
 * **********************************************************************
 * Closes the create/edit window and creates the item whose description
 * is given.  Called from the "Done" button.
 */
void create_it(item, event)
Panel_item item; 
Event      *event;
{
	char text[LINESIZ];
	struct category_rec *cr, *currcr;
	int value, priority;

	strcpy(text, (char *)xv_get(entry_text, PANEL_VALUE));
	priority = xv_get(entry_editor->entry_priority, PANEL_VALUE);
	if ((priority < 1) || (priority > 9)) {
		notice_prompt(entry_frame, NULL,
						  NOTICE_MESSAGE_STRINGS,
						     "The priority is an unacceptable value.",
						  0,
						  NOTICE_BUTTON, "Ok", 1,
						  0);
		return;
	}

	cr =	(struct category_rec *)xv_get(entry_category, PANEL_CLIENT_DATA);
	if ((int)xv_get(freq, PANEL_CLIENT_DATA)) {
		strcpy(cr->rl_tail->text, text);
		cr->rl_tail->priority = priority;
		rl_tail->deadline = (struct deadline_rec *)
			xv_get(deadline_deadline_frame->deadline_done, PANEL_CLIENT_DATA);
	} else {
		entry_head = cr->entry_head;
		entry_tail = cr->entry_tail;
		add_to(cr, curr_month, curr_day, curr_year, text, priority);
		cr = (struct category_rec *)xv_get(categories, PANEL_CLIENT_DATA);
		entry_head = cr->entry_head;
		entry_tail = cr->entry_tail;
	}
	display_list(curr_month, curr_day, curr_year);

	xv_set(entry_text, PANEL_VALUE, "", 0);
	xv_set(entry_frame, XV_SHOW, FALSE, 0);
	changed = TRUE;
	refresh_db(FALSE);
}

/*
 * **********************************************************************
 * Called from the "Create..." button, this routine clears the entry
 * window, then opens it.
 */
Menu_item create_item(item, op)
Menu_item	item;
Menu_generate	op;
{
	int choice, choices;
	struct category_rec *cr;
	char date[12];

	if (op == MENU_NOTIFY) {
		xv_set(entry_category,
				 PANEL_CLIENT_DATA, xv_get(categories, PANEL_CLIENT_DATA),
				 0);
		xv_set(entry_category_name,
				 PANEL_LABEL_STRING, xv_get(category_name, PANEL_LABEL_STRING),
				 0);
		xv_set(entry_editor->entry_priority, PANEL_VALUE, default_priority, 0);
		xv_set(freq, PANEL_CLIENT_DATA, FALSE, 0);
		xv_set(deadline_deadline_frame->deadline_done, PANEL_CLIENT_DATA, NULL, 0);
		xv_set(entry_done, PANEL_NOTIFY_PROC, create_it, 0);
		xv_set(entry_frame,
				 XV_SHOW, TRUE,
				 0);
	}
	return(item);
}

/*
 * **********************************************************************
 * Called from the "Done" button in the create/edit window (the edit
 * setup changes the NOTIFY procedure).  Simply replaces the information
 * in the appropriate fields.
 */
void edit_it(item, event)
Panel_item item; 
Event      *event;
{
	char text[LINESIZ];
	int si, datecode, priority;
	struct day_entry *de, *tde, pde, *trl, *prl;
	struct entry_list *el;
	struct category_rec *cr;

	strcpy(text, (char *)xv_get(entry_text, PANEL_VALUE));

	priority = xv_get(entry_editor->entry_priority, PANEL_VALUE);
	if ((priority < 0) || (priority > 9)) {
		notice_prompt(entry_frame, NULL,
						  NOTICE_MESSAGE_STRINGS,
						  "The priority is an unacceptable value.",
						  0,
						  NOTICE_BUTTON, "Ok", 1,
						  0);
		return;
	}
	si = xv_get(entry_done, PANEL_CLIENT_DATA);
	de = (struct day_entry *)xv_get(todo, PANEL_LIST_CLIENT_DATA, si);
		
	if ((int)xv_get(freq, PANEL_CLIENT_DATA)) {
		strcpy(rl_tail->text, text);
		rl_tail->priority = priority;
		rl_tail->deadline = (struct deadline_rec *)
			xv_get(deadline_deadline_frame->deadline_done, PANEL_CLIENT_DATA);
		if (de->prev != NULL) {
			if (de->next != NULL) de->next->prev = de->prev;
			de->prev->next = de->next;
			free(de);
		} else {
			if (de->next != NULL) de->next->prev = NULL;
			datecode = (curr_year-1990)*10000 + curr_month*100 + curr_day;
			el = entry_search(datecode, FALSE, NULL);
			if (de->recurring_entry) {
				el->rl_first = de->next;
				if (el->rl_last == de) el->rl_last = NULL;
			} else {
				el->first = de->next;
				if (el->last == de) el->last = NULL;
			}
		}
	} else {
		strcpy(de->text, text);
		de->priority = priority;
		de->deadline = (struct deadline_rec *)
			xv_get(deadline_deadline_frame->deadline_done, PANEL_CLIENT_DATA);
		if (de->recurring_entry) {  /* i.e., reverting to non-recurring */
			datecode = (curr_year-1990)*10000 + curr_month*100 + curr_day;
			el = entry_search(datecode, FALSE, NULL);
			
			/* remove from RL list */
			prl = NULL;
			for (trl = el->rl_first; trl != NULL; prl = trl,trl = trl->next) 
				if (trl == de) break;
			if (prl == NULL) {
				el->rl_first = de->next;
			} else {
				prl->next = de->next;
			}
			if (de->next != NULL) de->next->prev = de->prev;
			if (el->rl_last == de) el->rl_last = NULL;
			
			/* add to regular day entry list */
			if (el->last == NULL) {
				el->first = el->last = de;
				de->prev = de->next = NULL;
			} else {
				el->last->next = de;
				de->prev = el->last;
				de->next = NULL;
				el->last = de;
			}
			de->recurring_entry = FALSE;
		}
	}

	xv_set(entry_editor->entry_priority, PANEL_VALUE, default_priority, 0);
	xv_set(entry_category, PANEL_INACTIVE, FALSE, 0);
	xv_set(entry_category_name, PANEL_INACTIVE, FALSE, 0);
	xv_set(entry_text, PANEL_VALUE, "", 0);
	xv_set(entry_frame,
			 XV_SHOW, FALSE,
			 0);

	display_list(curr_month, curr_day, curr_year);
	changed = TRUE;
	refresh_db(FALSE);
}

/*
 * **********************************************************************
 * Menu callback for the "Modify" menu selection.  Looks for a selected
 * entry, or flags an error.
 */
Menu_item
edit(item, op)
	Menu_item	item;
	Menu_generate	op;
{
	int selected, nrows;
	struct day_entry *de;

	switch (op) {
	case MENU_DISPLAY:
		break;

	case MENU_DISPLAY_DONE:
		break;

	case MENU_NOTIFY:
		nrows = xv_get(todo, PANEL_LIST_NROWS);
		for (selected = 0; selected < nrows; selected ++) 
			if (xv_get(todo, PANEL_LIST_SELECTED, selected)) break;
		if (selected < nrows) {
			xv_set(todo, PANEL_LIST_SELECTED, selected, FALSE, 0);
			de = (struct day_entry *)
				xv_get(todo, PANEL_LIST_CLIENT_DATA, selected);
			edit_selection(de);
		} else {
         notice_prompt(tdlist, NULL,
							  NOTICE_MESSAGE_STRINGS,
							     "Please select an item to edit first",
							     0,
							  NOTICE_BUTTON, "Ok", 1,
							  0);
 		}
		break;

	case MENU_NOTIFY_DONE:
		break;
	}
	return item;
}

/*
 * **********************************************************************
 * Menu callback for the "Copy" menu selection.  Looks for a selected
 * entry, or flags an error.
 */
Menu_item
copy(item, op)
	Menu_item	item;
	Menu_generate	op;
{
	int selected, nrows;
	struct day_entry *de;

	switch (op) {
	case MENU_DISPLAY:
		break;

	case MENU_DISPLAY_DONE:
		break;

	case MENU_NOTIFY:
		nrows = xv_get(todo, PANEL_LIST_NROWS);
		for (selected = 0; selected < nrows; selected ++) 
			if (xv_get(todo, PANEL_LIST_SELECTED, selected)) break;
		if (selected < nrows) {
			xv_set(todo, PANEL_LIST_SELECTED, selected, FALSE, 0);
			de = (struct day_entry *)
				xv_get(todo, PANEL_LIST_CLIENT_DATA, selected);
			copy_selection(de);
			display_list(curr_month, curr_day, curr_year);
		} else {
         notice_prompt(tdlist, NULL,
							  NOTICE_MESSAGE_STRINGS,
							     "Please select an item to copy first",
							     0,
							  NOTICE_BUTTON, "Ok", 1,
							  0);
 		}
		break;

	case MENU_NOTIFY_DONE:
		break;
	}
	return item;
}

/*
 * **********************************************************************
 * Menu callback for the "Cut" menu selection.  Looks for a selected
 * entry, or flags an error.  Copies that entry to the cut buffer and
 * deletes it from the list.
 */
Menu_item
cut(item, op)
	Menu_item	item;
	Menu_generate	op;
{
	int selected, nrows;
	struct day_entry *de;

	switch (op) {
	case MENU_DISPLAY:
		break;

	case MENU_DISPLAY_DONE:
		break;

	case MENU_NOTIFY:
		nrows = xv_get(todo, PANEL_LIST_NROWS);
		for (selected = 0; selected < nrows; selected ++) 
			if (xv_get(todo, PANEL_LIST_SELECTED, selected)) break;
		if (selected < nrows) {
			xv_set(todo, PANEL_LIST_SELECTED, selected, FALSE, 0);
			de = (struct day_entry *)
				xv_get(todo, PANEL_LIST_CLIENT_DATA, selected);
			cut_selection(de);
		} else {
         notice_prompt(tdlist, NULL,
							  NOTICE_MESSAGE_STRINGS,
							     "Please select an item to cut first",
							     0,
							  NOTICE_BUTTON, "Ok", 1,
							  0);
 		}
		break;

	case MENU_NOTIFY_DONE:
		break;
	}
	return item;
}

/*
 * **********************************************************************
 * Menu callback for the "Paste" menu selection.  Expects a full cut
 * buffer and flags an error if that is not present.  Copies the cut
 * buffer contents to the list.
 */
Menu_item
paste(item, op)
	Menu_item	item;
	Menu_generate	op;
{
	switch (op) {
	case MENU_DISPLAY:
		break;

	case MENU_DISPLAY_DONE:
		break;

	case MENU_NOTIFY:
		paste_selection();
		break;

	case MENU_NOTIFY_DONE:
		break;
	}
	return item;
}

/*
 * **********************************************************************
 * This routine moves the priority for a selected item up or down 
 * a notch.
 */
Menu_item move_priority(item, op)
Menu_item	item;
Menu_generate	op;
{
	int selected, nrows;
	struct day_entry *de;

	switch (op) {
	case MENU_DISPLAY:
		break;

	case MENU_DISPLAY_DONE:
		break;

	case MENU_NOTIFY:
		nrows = xv_get(todo, PANEL_LIST_NROWS);
		for (selected = 0; selected < nrows; selected ++) 
			if (xv_get(todo, PANEL_LIST_SELECTED, selected)) break;
		if (selected < nrows) {
			xv_set(todo, PANEL_LIST_SELECTED, selected, FALSE, 0);
			de = (struct day_entry *)
				xv_get(todo, PANEL_LIST_CLIENT_DATA, selected);
			if (EQUAL((char *)xv_get(item, MENU_STRING), "inc priority")) {
				if (EQUAL(priority_listing, "ascending")) {
					if (de->priority > 1) de->priority--;
				} else {
					if (de->priority < 9) de->priority++;
				}
			} else {
				if (EQUAL(priority_listing, "ascending")) {
					if (de->priority < 9) de->priority++;
				} else {
					if (de->priority > 1) de->priority--;
				}
			}
			changed = TRUE;
			display_list(curr_month, curr_day, curr_year);
		} else {
         notice_prompt(tdlist, NULL,
							  NOTICE_MESSAGE_STRINGS,
							     "Please select an item first",
							     0,
							  NOTICE_BUTTON, "Ok", 1,
							  0);
 		}
		break;

	case MENU_NOTIFY_DONE:
		break;
	}
	return item;
}


/*
 * **********************************************************************
 * This routine is called when it's midnight.  It performs propagation
 * of list items and refreshes the data file.
 */
Notify_value midnight()
{
   struct timeval tv;
	struct category_rec *cr;

   gettimeofday(&tv, 0);
   tm = localtime(&tv.tv_sec);
   today = *tm;
   curr_month = today.tm_mon+1;
   curr_day = today.tm_mday;
   curr_year = today.tm_year+1900;

	propagate();

	cr = (struct category_rec *)xv_get(categories, PANEL_CLIENT_DATA);
	entry_head = cr->entry_head;
	entry_tail = cr->entry_tail;
	rl_head = cr->rl_head;
	rl_tail = cr->rl_tail;
	go_today((Panel_item)NULL, (Event *)NULL);

	return NOTIFY_DONE;
}

/*
 * **********************************************************************
 * Debugging routine to perform a fake propagate.  This will only be 
 * included if the "DEBUG" define is given.
 */
#ifdef DEBUG
Menu_item
fake_propagate(item, op)
	Menu_item	item;
	Menu_generate	op;
{
	int selected, nrows;
	struct day_entry *de;
   struct timeval tv;
	struct category_rec *cr;

	switch (op) {
	case MENU_DISPLAY:
		break;

	case MENU_DISPLAY_DONE:
		break;

	case MENU_NOTIFY:
		gettimeofday(&tv, 0);
		tm = localtime(&tv.tv_sec);
		today = *tm;
		curr_month = today.tm_mon+1;
		curr_day = today.tm_mday = curr_day+1;
		curr_year = today.tm_year+1900;
		
		propagate();
		
		cr = (struct category_rec *)xv_get(categories, PANEL_CLIENT_DATA);
		entry_head = cr->entry_head;
		entry_tail = cr->entry_tail;
		rl_head = cr->rl_head;
		rl_tail = cr->rl_tail;
		go_today(item, NULL);
		
		break;

	case MENU_NOTIFY_DONE:
		break;
	}
	return item;
}

#endif
/*
 * **********************************************************************
 * This routine is called hourly.  It wakes up, refreshes the data base,
 * and called the midght routine if necessary.
 */	
Notify_value hourly()
{
   struct timeval tv;

   gettimeofday(&tv, 0);
   tm = localtime(&tv.tv_sec);
	if (tm->tm_hour == 0) midnight();

	return NOTIFY_DONE;
}

/*
 * **********************************************************************
 * This routine is called for events on the base window.  It is mostly
 * used for resizing the window and the inner panels to match.
 */
void tdl_events(window, event)
Xv_Window       window;
Event *event;
{
	int height, width;
	int y;
	int rows, rowsize;
	
	switch(event_id(event)) {
	   case WIN_RESIZE:
		   height = xv_get(tdl_list, XV_HEIGHT);
			width = xv_get(tdl_list, XV_WIDTH);
			y = xv_get(todo, PANEL_ITEM_Y);
			rowsize = xv_get(todo, PANEL_LIST_ROW_HEIGHT);
			rows = (height - y - 20) / rowsize;
			if (rows <= 0)
				rows = 1;
			xv_set(todo, 
					 PANEL_LIST_DISPLAY_ROWS, rows,
					 PANEL_LIST_WIDTH, width - 30, 
					 NULL);
			break;
		default:
			break;
	}
}

/*
 * **********************************************************************
 * This routine is called when events happen on the TODO list itself.
 * The main ones that are handled are the CUT, COPY, and PASTE keys and
 * the up and down arrow. 
 */
void list_events(window, event)
Panel window;
Event *event;
{
	int selected, nrows;
	struct day_entry *de;

	if (!event_is_up(event)) return;

	switch (event_id(event)) {
	   case KEY_LEFT(10):
	   case KEY_LEFT(6):
	   case KEY_RIGHT(8):
      case KEY_RIGHT(14):
	      nrows = xv_get(todo, PANEL_LIST_NROWS);
			for (selected = 0; selected < nrows; selected ++) 
				if (xv_get(todo, PANEL_LIST_SELECTED, selected)) break;
			if (selected < nrows) {
				xv_set(todo, PANEL_LIST_SELECTED, selected, FALSE, 0);
				de = (struct day_entry *)
					xv_get(todo, PANEL_LIST_CLIENT_DATA, selected);
				switch (event_id(event)) {
				   case KEY_LEFT(10):
					   cut_selection(de);
						break;
	            case KEY_LEFT(6):
					   copy_selection(de);
						break;
	            case KEY_RIGHT(8):
						if (EQUAL(priority_listing, "ascending")) {
							if (de->priority > 1) de->priority--;
						} else {
							if (de->priority < 9) de->priority++;
						}
						break;
               case KEY_RIGHT(14):
						if (EQUAL(priority_listing, "ascending")) {
							if (de->priority < 9) de->priority++;
						} else {
							if (de->priority > 1) de->priority--;
						}
						break;
				}	
				changed = TRUE;
				display_list(curr_month, curr_day, curr_year);
			} else {
				notice_prompt(tdlist, NULL,
								  NOTICE_MESSAGE_STRINGS,
							        "Please select an item first",
							     0,
								  NOTICE_BUTTON, "Ok", 1,
								  0);
			}
			break;

	   case KEY_LEFT(8):
			paste_selection();
			break;

	}
}

/*
 * **********************************************************************
 * This routine is called as a trap for the window destroy events...so we
 * can write the database before we really quit.
 */
Notify_value my_destroy_func(client, status)
Notify_client   client;
Destroy_status  status;
{
	if (status == DESTROY_SAVE_YOURSELF) {
		/* save state. Not a death */
		return NOTIFY_DONE;
	}

	if (changed) refresh_db(log_level == LOG_AT_QUIT);
   xv_destroy_safe(tdlist);

	switch(status) {
	   case DESTROY_CHECKING:
		   break;
	   case DESTROY_CLEANUP:
		   return notify_next_destroy_func(client, status);
	   case DESTROY_PROCESS_DEATH:
		   exit(1);
	}
	return NOTIFY_DONE;
}

/*
 * **********************************************************************
 * Close the todo list to an iconic state.  Refresh the db if need be.
 */
void close_tdlist(item, event)
Panel_item item; 
Event      *event;
{
	if (changed) {
		refresh_db(FALSE);
	} else {
		file_status_ok();
	}
	xv_set(tdlist, FRAME_CLOSED, TRUE, 0);
}

/*
 * **********************************************************************
 * Terminate the program.
 */
void quit(item, event)
Panel_item item; 
Event      *event;
{
	my_destroy_func((Notify_client)NULL, DESTROY_PROCESS_DEATH);
	notify_stop();
}

/*
 * **********************************************************************
 * This routine is called when XVTDL receives a SIGHUP signal -- 
 * telling it to reread the database.
 */
Notify_value my_signal_handler(client, sig, when)
Notify_client   client;
int             sig;
Notify_signal_mode when;
{
	freeup();

	yyin = fopen(fname, "r");
	yyparse();
	propagate();
	fclose(yyin);

	if (category_head == NULL)
		category_head = (struct category_rec *)new_category("Every Day", NULL, FALSE);

   xv_set(categories,
          PANEL_CLIENT_DATA, category_head,
          PANEL_VALUE, 0,
          0);

	entry_head = category_head->entry_head;
	entry_tail = category_head->entry_tail;
	rl_head = category_head->rl_head;
	rl_tail = category_head->rl_tail;
	display_list(curr_month, curr_day, curr_year);

	return NOTIFY_DONE;
}

/*
 * **********************************************************************
 * This routine forces a possible updating of the database when the
 * tool is iconified.  Same effect as if the Done button were hit.
 */
Notify_value my_event_interposer(frame, event, arg, type)
Frame frame;
Event *event;
Notify_arg arg;
Notify_event_type type;
{
	if (event_action(event) == ACTION_CLOSE) {
		if (changed) refresh_db(FALSE);
	} else if (event_action(event) == ACTION_OPEN) {
		file_status_ok();
	}
		
	return notify_next_event_func(frame, event, arg, type);
}
