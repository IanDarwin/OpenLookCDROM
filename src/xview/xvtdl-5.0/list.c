/*
 * $Id: list.c,v 4.2 1992/10/06 11:30:33 jipping Exp $
 * **********************************************************************
 *
 * List.c ==> List manipulation routines.
 *            If it changes the current todo list widget somehow, it's 
 *            here!
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
 * $Log: list.c,v $
 * Revision 4.2  1992/10/06  11:30:33  jipping
 * Fixes after beta test: list management (mostly categories), changes to
 * log and deadline calls to include category (for expanded messages).
 *
 * Revision 4.1  1992/09/15  13:26:32  jipping
 * Removed references to cursors (no longer needed).
 *
 * Revision 4.0  1992/09/15  11:15:56  jipping
 * Release 4.0 beta.
 * Changes include:
 *    * accomodations to include deadlines and deadline checking
 *    * MANY list manipulation bugfixes
 *    * #ifdefs to make list.c work with "tdl"
 *
 * Revision 3.2  1992/07/30  14:17:12  jipping
 * Eliminated flashing list by hiding (XV_SHOW = FALSE) list when it
 * is under construction.
 *  -- added stuff to print_all
 *
 * Revision 3.1  1992/07/29  14:12:07  jipping
 * Fixed the list flashing problem by hiding the list while updating, then
 * showing the completed list (XV_SHOW).
 *
 * Revision 3.0  1992/07/27  18:36:51  jipping
 * Release 3.0 includes:
 * * added "display_all" to display all list items
 * * corrected cut buffer manipulation
 * * fixed several list management bugs
 * * added file backup to "refresh_db"
 * * added logging capability
 *
 * Revision 2.4  1992/07/14  16:42:37  jipping
 * Fixed incorrect list management in cut_selection (->next not being
 * taken care of).
 *
 * Revision 2.3  1992/07/14  13:01:36  jipping
 * Eliminated placing a callback on the text field in edit window.
 *
 * Revision 2.2  1992/07/13  14:25:22  jipping
 * Cleaned up warnings in compilation.
 *
 * Revision 2.1  1992/07/10  19:47:09  jipping
 * *** empty log message ***
 *
 * Revision 2.0  1992/07/10  15:58:04  jipping
 * Initial release.
 *
 *
 */
#include "globaldefs.h"

extern void edit_it();
extern int check_deadline();

struct category_rec *new_category();
void refresh_db(), propagate();

struct entry_list *entry_head, *entry_tail;
struct category_rec *category_head;
struct day_entry *cut_buffer=NULL;
struct recurrence_list *cut_rl_buffer=NULL;
static int selected_item;

/*
 * **********************************************************************
 * entry_search is a utility routine that searches for the list of
 * entries for a particular datecode in the current category.
 *
 * If "create" is TRUE, the entry list structure is created under 
 * the specified category if it is not found to exist.
 */
struct entry_list *entry_search(code, create, category)
int code, create;
struct category_rec *category;
{
	struct entry_list *el, *tmpel;
	struct category_rec *cr;

	for (el = entry_head; el != NULL; el = el->next) {
		if (el->day_code == code) {return el;}
	}

	if (! create) return NULL;
	
   /*** If is does not exist, create it! ***/

	tmpel = NEW(struct entry_list);
	tmpel->day_code = code;
	tmpel->first = tmpel->last = NULL;
	tmpel->next = NULL;
	tmpel->rl_first = tmpel->rl_last = NULL;
#ifdef TDL
	cr = category;
#else
	if (category == NULL) {
		cr = (struct category_rec *)xv_get(categories, PANEL_CLIENT_DATA);
	} else {
		cr = category;
	}
#endif
	if (cr == NULL) {
		cr = new_category("Every Day", NULL, FALSE);
	}
	if (cr->entry_head == NULL) {
		cr->entry_head = entry_head = tmpel;
	} else {
		cr->entry_tail->next = tmpel;
	}
	cr->entry_tail = entry_tail = tmpel;

	if ( (cr == category) || (category == NULL) ) {
		entry_head = cr->entry_head;
		entry_tail = cr->entry_tail;
		rl_head = cr->rl_head;
		rl_tail = cr->rl_tail;
	}

	return tmpel;
}

/*
 * **********************************************************************
 * A utility routine that returns the day entry in the entry list
 * parameter that matches the text in the recurrence entry.
 */
struct day_entry *entry_rl_search(el, rl)
struct entry_list *el;
struct recurrence_list *rl;
{
	struct day_entry *trl;
	
	for (trl = el->rl_first; trl != NULL; trl = trl->next) {
		if (EQUAL(trl->text, rl->text)) return trl;
	}
	return NULL;
}

/*
 * **********************************************************************
 * entry_text_search is a utility routine that searches the list of
 * entries for a particular datecode in the category given for the entry
 * with the matching name.
 */
int entry_text_search(text, code, cr)
char *text;
int code;
struct category_rec *cr;
{
	struct entry_list *el, *tmpel;
   struct day_entry *de, *rl;

	el = entry_search(code, FALSE, cr);

   if (el != NULL) {
      for (de = el->first; de != NULL; de = de->next) {
			if (EQUAL(text, de->text)) return 1;
		}
		for (rl = el->rl_first; rl != NULL; rl = rl->next) {
			if (EQUAL(text, rl->text)) return 1;
		}
   }
	return 0;
}

/*
 * **********************************************************************
 * This routine adds a todo list item ("text") with the specified
 * priority to the given category with the specified month, day, and
 * year. 
 */
add_to (category, month, day, year, text, priority)
struct category_rec *category;
int month, day, year, priority;
char *text;
{
	int datecode;
	struct day_entry *de, *tmpde;
	struct entry_list *el;

	datecode = (year-1990)*10000 + month*100 + day;
	el = entry_search(datecode, TRUE, category);
	de = el->last;

	tmpde = NEW(struct day_entry);
#ifndef TDL
	tmpde->deadline = (struct deadline_rec *)
		xv_get(deadline_deadline_frame->deadline_done, PANEL_CLIENT_DATA);
#else 
	tmpde->deadline = NULL;
#endif
	tmpde->next = NULL;
	strcpy(tmpde->text, text);
	tmpde->priority = priority;
	tmpde->recurring_entry = FALSE;
	tmpde->starting_day_code = datecode;
	tmpde->checked = FALSE;
	if (de == NULL) {
		el->first = el->last = tmpde;
		tmpde->prev = NULL;
	} else {
		de->next = el->last = tmpde;
		tmpde->prev = de;
	}
}

#ifndef TDL

/*
 * **********************************************************************
 * When a list item is chosen for editing, this routine is called with 
 * the day_entry structure of the list item.  This sets up the edit
 * window and (possibly) the recurrence editor window.
 */
void edit_selection(de)
struct day_entry *de;
{
	char selection_text[LINESIZ];
	int datecode;
	struct entry_list *el;
	struct recurrence_list *rl;
	struct day_entry *tmprl, *rl2;
	int choice, choices;
	struct category_rec *cr;

   /** Setup the category in the edit window. ***/
	cr = (struct category_rec *)xv_get(categories, PANEL_CLIENT_DATA);
	xv_set(entry_category, PANEL_CLIENT_DATA, cr, 0);
	xv_set(entry_category_name, PANEL_LABEL_STRING, cr->name, 0);

   /*** Now set up the rest of the window. ***/
	strcpy(selection_text,
			 (char *)xv_get(todo, PANEL_LIST_STRING, selected_item));
	xv_set(entry_text, PANEL_VALUE, selection_text, 0);
	xv_set(entry_category, PANEL_INACTIVE, TRUE, 0);
	xv_set(entry_category_name, PANEL_INACTIVE, TRUE, 0);
	xv_set(recurring, PANEL_INACTIVE, FALSE, 0);
	xv_set(entry_editor->entry_priority, PANEL_INACTIVE, FALSE, 0);
	xv_set(entry_editor->entry_priority, PANEL_VALUE, de->priority, 0);
	xv_set(entry_done,
			 PANEL_NOTIFY_PROC, edit_it,
			 PANEL_CLIENT_DATA, selected_item,
			 0);

	/*
    * Set up the deadline pointer
    */
	xv_set(deadline_deadline_frame->deadline_done,
			 PANEL_CLIENT_DATA, de->deadline,
			 0);

   /* 
    * If the entry is recurring, set up the recurrence window
    */	
	if (de->recurring_entry) {
		xv_set(freq, PANEL_CLIENT_DATA, TRUE, 0);
		datecode = (curr_year-1990)*10000 + curr_month*100 + curr_day;
		for (rl = rl_head; rl != NULL; rl = rl->next) {
			if (datecode_matches(datecode, rl)) {
				if (EQUAL(de->text, rl->text)) {
					if (rl->daily) {
						xv_set(freq, PANEL_VALUE, 1, 0);
						xv_set(recurring_cancel, 
								 PANEL_VALUE_X,      560,
								 PANEL_VALUE_Y,      40,
								 0);
						xv_set(recurring_done, 
								 PANEL_VALUE_X,      630,
								 PANEL_VALUE_Y,      40,
								 0);
						xv_set(recurring_panel, XV_HEIGHT, 60, 0);
						xv_set(recurring_frame, XV_HEIGHT, 60, 0);
					} else if ( (rl->weekly) | (rl->biweekly) ) {
						if (rl->weekly) {
							xv_set(freq, PANEL_VALUE, 2, 0);
						} else {
							xv_set(freq, PANEL_VALUE, 3, 0);
						}
						xv_set(weeks,
								 XV_SHOW, TRUE,
								 PANEL_VALUE, rl->number_of_weeks,
								 0);
						xv_set(day_of_week, 
								 XV_SHOW, TRUE,
								 PANEL_VALUE,    rl->dow,
								 0);
						xv_set(recurring_cancel, 
								 PANEL_VALUE_X,      560,
								 PANEL_VALUE_Y,      100,
								 0);
						xv_set(recurring_done, 
								 PANEL_VALUE_X,      630,
								 PANEL_VALUE_Y,      100,
								 0);
						xv_set(recurring_panel, XV_HEIGHT, 130, 0);
						xv_set(recurring_frame, XV_HEIGHT, 130, 0);
					} else if ( rl->monthly) {
						xv_set(freq, PANEL_VALUE, 4, 0);
						xv_set(week_of_month,
								 XV_SHOW, TRUE,
								 PANEL_VALUE, rl->week_number,
								 0);
						xv_set(day_of_week, 
								 XV_SHOW, TRUE,
								 PANEL_VALUE,    rl->dow,
								 0);
						xv_set(day_of_month, 
								 XV_SHOW, TRUE,
								 PANEL_VALUE,    rl->dom,
								 0);
						xv_set(months,
								 XV_SHOW, TRUE,
								 PANEL_VALUE,    rl->number_of_months,
								 0);
						xv_set(recurring_cancel, 
								 PANEL_VALUE_X,      560,
								 PANEL_VALUE_Y,      130,
								 0);
						xv_set(recurring_done, 
								 PANEL_VALUE_X,      630,
								 PANEL_VALUE_Y,      130,
								 0);
						xv_set(recurring_panel, XV_HEIGHT, 160, 0);
						xv_set(recurring_frame, XV_HEIGHT, 160, 0);
					} else if ( rl->yearly ) {
						xv_set(freq, PANEL_VALUE, 5, 0);
						xv_set(recurring_cancel, 
								 PANEL_VALUE_X,      560,
								 PANEL_VALUE_Y,      40,
								 0);
						xv_set(recurring_done, 
								 PANEL_VALUE_X,      630,
								 PANEL_VALUE_Y,      40,
								 0);
						xv_set(recurring_panel, XV_HEIGHT, 60, 0);
						xv_set(recurring_frame, XV_HEIGHT, 60, 0);
					}
					break;
				}
			}
		}
	} else {
      /***  Entry is not recurring...  ***/
		xv_set(freq, PANEL_CLIENT_DATA, FALSE, 0);
		xv_set(freq, PANEL_VALUE, 0, 0);
		xv_set(day_of_week, XV_SHOW, FALSE, 0);
		xv_set(weeks, XV_SHOW, FALSE, 0);
		xv_set(week_of_month, XV_SHOW, FALSE, 0);
		xv_set(day_of_month, XV_SHOW, FALSE, 0);
		xv_set(months, XV_SHOW, FALSE, 0);
	}
			
	xv_set(entry_frame,
			 XV_SHOW, TRUE,
			 0);
}

/*
 * **********************************************************************
 * Copying the selection is easy...copy it to a cut buffer variable.
 */
void copy_selection (de)
struct day_entry *de;
{
	int response, datecode;
	struct recurrence_list *rl;
	
	if (de->recurring_entry) {
		response = notice_prompt(tdlist, NULL,
										 NOTICE_MESSAGE_STRINGS,
										    "This is a recurring entry",
										    "Do you want to copy just today's entry",
										    "or each recurring entry?",
										 0,
										 NOTICE_BUTTON, "Today's only", 0,
										 NOTICE_BUTTON, "Each recurring", 1,
										 NOTICE_BUTTON, "Cancel", 2,
										 0);
		
		if (response == 2) return;

		if (response == 0) {
			de->recurring_entry = 0;
		} else {
			datecode =
				(today.tm_year-90)*10000 + (today.tm_mon+1)*100 + today.tm_mday;
			for (rl = rl_head; rl != NULL; rl = rl->next) {
				if (datecode_matches(datecode, rl)) {
					if (EQUAL(de->text, rl->text)) {
						if (cut_rl_buffer == NULL) cut_rl_buffer = NEW(struct recurrence_list);
						cut_rl_buffer->starting_day_code = rl->starting_day_code;
						strcpy(cut_rl_buffer->text, rl->text);
						cut_rl_buffer->priority = rl->priority;
						cut_rl_buffer->daily = rl->daily;
						cut_rl_buffer->weekly = rl->weekly;
						cut_rl_buffer->biweekly = rl->biweekly;
						cut_rl_buffer->monthly = rl->monthly;
						cut_rl_buffer->yearly = rl->yearly;
						cut_rl_buffer->dow = rl->dow;
						cut_rl_buffer->dom = rl->dom;
						cut_rl_buffer->week_number = rl->week_number;
						cut_rl_buffer->number_of_weeks = rl->number_of_weeks;
						cut_rl_buffer->number_of_months = rl->number_of_months;
						if (rl->deadline == NULL) {
							cut_rl_buffer->deadline = NULL;
						} else {
							if (cut_rl_buffer->deadline == NULL)
								cut_rl_buffer->deadline = NEW(struct deadline_rec);
							cut_rl_buffer->deadline->datecode = rl->deadline->datecode;
							cut_rl_buffer->deadline->actions = rl->deadline->actions;
							cut_rl_buffer->deadline->delete_time = rl->deadline->delete_time;
							cut_rl_buffer->deadline->delete_units = rl->deadline->delete_units;
							cut_rl_buffer->deadline->priority_up_units = rl->deadline->priority_up_units;
							cut_rl_buffer->deadline->priority_down_units = rl->deadline->priority_down_units;
							strcpy(cut_rl_buffer->deadline->mail_on, rl->deadline->mail_on);
							strcpy(cut_rl_buffer->deadline->mail_after, rl->deadline->mail_after);
							cut_rl_buffer->deadline->move_time = rl->deadline->move_time;
							cut_rl_buffer->deadline->move_units = rl->deadline->move_units;
						}
					}
				}
			}
		}
	} else {
		cut_rl_buffer = NULL;
	}
	
	if (cut_buffer == NULL) cut_buffer = NEW(struct day_entry);

	cut_buffer->recurring_entry = de->recurring_entry;
	cut_buffer->next = de->next;
	cut_buffer->starting_day_code = de->starting_day_code;
	strcpy(cut_buffer->text, de->text);
	cut_buffer->priority = de->priority;
	cut_buffer->checked = de->checked;
	if (de->deadline == NULL) {
		cut_buffer->deadline = NULL;
	} else {
		if (cut_buffer->deadline == NULL)
			cut_buffer->deadline = NEW(struct deadline_rec);
		cut_buffer->deadline->datecode = de->deadline->datecode;
		cut_buffer->deadline->actions = de->deadline->actions;
		cut_buffer->deadline->delete_time = de->deadline->delete_time;
		cut_buffer->deadline->delete_units = de->deadline->delete_units;
		cut_buffer->deadline->priority_up_units = de->deadline->priority_up_units;
		cut_buffer->deadline->priority_down_units = de->deadline->priority_down_units;
		strcpy(cut_buffer->deadline->mail_on, de->deadline->mail_on);
		strcpy(cut_buffer->deadline->mail_after, de->deadline->mail_after);
		cut_buffer->deadline->move_time = de->deadline->move_time;
		cut_buffer->deadline->move_units = de->deadline->move_units;
	}
}

/*
 * **********************************************************************
 * This routine cuts the selected entry from the todo list.  This is
 * done by locating the day_entry in a list...and freeing it up.
 */
void cut_selection (de)
struct day_entry *de;
{
	struct entry_list *el;
	struct recurrence_list *rl, *rlp;
	struct category_rec *cr;
	int response, datecode;

	if (de->recurring_entry) {
		response = notice_prompt(tdlist, NULL,
										 NOTICE_MESSAGE_STRINGS,
										    "This is a recurring entry",
										    "Do you want to cut just today's entry",
										    "or each recurring entry?",
										 0,
										 NOTICE_BUTTON, "Today's only", 0,
										 NOTICE_BUTTON, "Each recurring", 1,
										 NOTICE_BUTTON, "Cancel", 2,
										 0);
		
		if (response == 2) return;

		if (response == 0) {
			de->recurring_entry = 0;
		} else {
			rlp = NULL;
			datecode = (curr_year-1990)*10000 + curr_month*100 + curr_day;
			for (rl = rl_head; rl != NULL; rlp = rl, rl = rl->next) {
				if (datecode_matches(datecode, rl)) {
					if (EQUAL(de->text, rl->text)) {
						if (cut_rl_buffer == NULL) cut_rl_buffer = NEW(struct recurrence_list);
						cut_rl_buffer->starting_day_code = rl->starting_day_code;
						strcpy(cut_rl_buffer->text, rl->text);
						cut_rl_buffer->priority = rl->priority;
						cut_rl_buffer->daily = rl->daily;
						cut_rl_buffer->weekly = rl->weekly;
						cut_rl_buffer->biweekly = rl->biweekly;
						cut_rl_buffer->monthly = rl->monthly;
						cut_rl_buffer->yearly = rl->yearly;
						cut_rl_buffer->dow = rl->dow;
						cut_rl_buffer->dom = rl->dom;
						cut_rl_buffer->week_number = rl->week_number;
						cut_rl_buffer->number_of_weeks = rl->number_of_weeks;
						cut_rl_buffer->number_of_months = rl->number_of_months;
						if (rl->deadline == NULL) {
							cut_rl_buffer->deadline = NULL;
						} else {
							if (cut_rl_buffer->deadline == NULL)
								cut_rl_buffer->deadline = NEW(struct deadline_rec);
							cut_rl_buffer->deadline->datecode = rl->deadline->datecode;
							cut_rl_buffer->deadline->actions = rl->deadline->actions;
							cut_rl_buffer->deadline->delete_time = rl->deadline->delete_time;
							cut_rl_buffer->deadline->delete_units = rl->deadline->delete_units;
							cut_rl_buffer->deadline->priority_up_units = rl->deadline->priority_up_units;
							cut_rl_buffer->deadline->priority_down_units = rl->deadline->priority_down_units;
							strcpy(cut_rl_buffer->deadline->mail_on, rl->deadline->mail_on);
							strcpy(cut_rl_buffer->deadline->mail_after, rl->deadline->mail_after);
							cut_rl_buffer->deadline->move_time = rl->deadline->move_time;
							cut_rl_buffer->deadline->move_units = rl->deadline->move_units;
						}
						break;
					}
				}
			}
		}
	} else {
		cut_rl_buffer = NULL;
	}

	if (cut_buffer == NULL)	cut_buffer = NEW(struct day_entry);

	strcpy(cut_buffer->text, de->text);
	cut_buffer->checked = de->checked;
	cut_buffer->recurring_entry = de->recurring_entry;
	cut_buffer->priority = de->priority;
	cut_buffer->starting_day_code = de->starting_day_code;
	if (de->deadline == NULL) {
		cut_buffer->deadline = NULL;
	} else {
		if (cut_buffer->deadline == NULL)
			cut_buffer->deadline = NEW(struct deadline_rec);
		cut_buffer->deadline->datecode = de->deadline->datecode;
		cut_buffer->deadline->actions = de->deadline->actions;
		cut_buffer->deadline->delete_time = de->deadline->delete_time;
		cut_buffer->deadline->delete_units = de->deadline->delete_units;
		cut_buffer->deadline->priority_up_units = de->deadline->priority_up_units;
		cut_buffer->deadline->priority_down_units = de->deadline->priority_down_units;
		strcpy(cut_buffer->deadline->mail_on, de->deadline->mail_on);
		strcpy(cut_buffer->deadline->mail_after, de->deadline->mail_after);
		cut_buffer->deadline->move_time = de->deadline->move_time;
		cut_buffer->deadline->move_units = de->deadline->move_units;
	}

	datecode = (curr_year-1990)*10000 + curr_month*100 + curr_day;
	el = entry_search(datecode, FALSE, NULL);
	if (de->prev != NULL) {  /* in the middle of the list */
		if (de->next != NULL) de->next->prev = de->prev;
		de->prev->next = de->next;
	} else {   /* at the head of the list. */
		if (de->next != NULL) de->next->prev = NULL;
		if (de->recurring_entry) {
			el->rl_first = de->next;
		} else {
			el->first = de->next;
		}
	}
	if (de->recurring_entry) {
		if (el->rl_last == de) el->rl_last = de->prev;
	} else {
		if (el->last == de) el->last = de->prev;
	}

	/*** We also need to remove it from a recurring list ***/
	if (de->recurring_entry) {
		cr = (struct category_rec *)xv_get(categories, PANEL_CLIENT_DATA);
		if (rlp == NULL) {
			cr->rl_head = rl->next;
		} else {
			rlp->next = rl->next;
		}
		if (cr->rl_tail == rl) {
			cr->rl_tail = rlp;
		}
		free(rl);
		rl_head = cr->rl_head;
		rl_tail = cr->rl_tail;
	} 

	if (de->deadline != NULL) free(de->deadline);
	free(de);

	display_list(curr_month, curr_day, curr_year);
	changed = TRUE;
	refresh_db(FALSE);
}

/*
 * **********************************************************************
 * This routine is called to paste the cut buffer into the current list.
 */
void paste_selection ()
{
	int datecode, value;
	struct category_rec *cr;
	struct day_entry *de, *tmpde;
	struct entry_list *el;

   /*
    *  If there is nothing to paste...
    */
	if (cut_buffer == NULL) {
		notice_prompt(tdlist, NULL,
						  NOTICE_MESSAGE_STRINGS,
						     "There is nothing in the cut buffer.",
						  0,
						  NOTICE_BUTTON, "Ok", 1,
						  0);
		return;
	}

   /*
    *  Go through the hassle of a list addition
    */
	cr = (struct category_rec *)xv_get(categories, PANEL_CLIENT_DATA);
	datecode = (curr_year-1990)*10000 + curr_month*100 + curr_day;
	el = entry_search(datecode, TRUE, cr);
	de = el->last;

	if (cut_buffer->recurring_entry) {
		if (cr->rl_head == NULL) {
			cr->rl_head = cr->rl_tail = cut_rl_buffer;
		} else {
			cr->rl_tail->next = cut_rl_buffer;
			cr->rl_tail = cut_rl_buffer;
		}
		cut_rl_buffer->next = NULL;
	} else {
		tmpde = NEW(struct day_entry);
		tmpde->next = NULL;
		strcpy(tmpde->text, cut_buffer->text);
		tmpde->priority = cut_buffer->priority;
		tmpde->checked = cut_buffer->checked;
		tmpde->starting_day_code = datecode;
		tmpde->recurring_entry = cut_buffer->recurring_entry;
		if (cut_buffer->deadline == NULL) {
			tmpde->deadline = NULL;
		} else {
			tmpde->deadline = NEW(struct deadline_rec);
			tmpde->deadline->datecode = cut_buffer->deadline->datecode;
			tmpde->deadline->actions = cut_buffer->deadline->actions;
			tmpde->deadline->delete_time = cut_buffer->deadline->delete_time;
			tmpde->deadline->delete_units = cut_buffer->deadline->delete_units;
			tmpde->deadline->priority_up_units = cut_buffer->deadline->priority_up_units;
			tmpde->deadline->priority_down_units = cut_buffer->deadline->priority_down_units;
			strcpy(tmpde->deadline->mail_on, cut_buffer->deadline->mail_on);
			strcpy(tmpde->deadline->mail_after, cut_buffer->deadline->mail_after);
			tmpde->deadline->move_time = cut_buffer->deadline->move_time;
			tmpde->deadline->move_units = cut_buffer->deadline->move_units;
		}
		if (de == NULL) {
			el->first = el->last = tmpde;
			tmpde->prev = NULL;
		} else {
			de->next = el->last = tmpde;
			tmpde->prev = de;
		}
	}

	display_list(curr_month, curr_day, curr_year);
	changed = TRUE;
	refresh_db(FALSE);
}

/*
 * **********************************************************************
 * double_click --check for double click
 * Stolen from Mike SUllivans "ftptool"
 */
int double_click(last_sel, then, this_sel, now)
struct day_entry *last_sel;
struct timeval	*then;
struct day_entry *this_sel;
struct timeval	*now;
{
	struct timeval	delta;

	if (this_sel != last_sel) return 0;

	delta.tv_sec = now->tv_sec - then->tv_sec;
	if ((delta.tv_usec = now->tv_usec - then->tv_usec) < 0)
	{
		delta.tv_usec += 1000000;
		delta.tv_sec -= 1;
	}

	/*
	 * Compare delta against multiclick timeout.
	 */
	return (delta.tv_sec*10 + delta.tv_usec/100000) <= multiclick_timeout;
}

/*
 * **********************************************************************
 * This routine is called for every selection on the todo list itself.
 * This sets up the "selected" variable for other (editing) routines.
 */
int tdl_notify_proc(item, string, de, op, event)
Panel_item       item;
char             *string;
struct day_entry *de;
Panel_list_op    op;
Event            *event;
{
	int selected, nrows;
	static struct day_entry *last_sel;
	struct category_rec *cr;
	static struct timeval	then = {0, 0};
	static struct timeval	now = {0, 0};

	switch (op) {
	case PANEL_LIST_OP_SELECT:
		nrows = xv_get(todo, PANEL_LIST_NROWS);
		for (selected = 0; selected < nrows; selected ++) 
			if (xv_get(todo, PANEL_LIST_SELECTED, selected)) break;
		selected_item = selected;

	case PANEL_LIST_OP_DESELECT:
		now = event_time(event);
		if (double_click(last_sel, &then, de, &now)) {
			de->checked = ! de->checked;
			if (de->checked) {
				xv_set(todo, PANEL_LIST_GLYPH, selected_item, checked_on, 0);
			} else {
				xv_set(todo, PANEL_LIST_GLYPH, selected_item, checks[de->priority], 0);
				propagate();
				display_list(curr_month, curr_day, curr_year);
			}
			if (log_level == LOG_AT_CHECKED) {
				cr = (struct category_rec *)xv_get(categories, PANEL_CLIENT_DATA);
				log_entry(de,cr);
			}
			changed = TRUE;
		}
		last_sel = de;
		then = now;
		break;

	case PANEL_LIST_OP_VALIDATE:
		return XV_ERROR;

	case PANEL_LIST_OP_DELETE:
		break;
	}

	return XV_OK;
}

#endif

/*
 * **********************************************************************
 * This is the routine that propagates list items from a specific
 * category.  Basically, if an item exists on a previous day -- either
 * recurring or non-recurring -- and it is not checked off, it is moved
 * to the list for the current day.  
 *
 * Also, other entries are checked for possible deadline actions (e.g.,
 * a "negative" deadline to warn of an upcoming item).
 */
void propagate_category (cr)
struct category_rec *cr;
{
	int datecode, tmpdc;
	struct entry_list *el, *todayel;
	struct category_rec *actualcr;
	struct recurrence_list *rl;
	struct day_entry *de, *todayde, *tmpde, *pde, *currde;

	datecode = (today.tm_year-90)*10000 + (today.tm_mon+1)*100 + today.tm_mday;

	entry_head = cr->entry_head;
	entry_tail = cr->entry_tail;
	todayel = entry_search(datecode, TRUE, cr);
	
	/*** First, for the non-recurring entries ***/
	el = cr->entry_head;
	for (el = cr->entry_head; el != NULL; el = el->next) {
		if (el->day_code < datecode) {
			pde = NULL;
			currde = NULL;
			de = el->first; 
			el->first = NULL;
			while (de != NULL) {
				if (! de->checked ) {
					if (check_deadline(datecode, de, cr)) {
						tmpde = NEW(struct day_entry);
						tmpde->next = NULL;
						strcpy(tmpde->text, de->text);
						tmpde->priority = de->priority;
						tmpde->deadline = de->deadline;
						tmpde->starting_day_code = de->starting_day_code;
						tmpde->checked = FALSE;
						tmpde->recurring_entry = FALSE;
						todayde = todayel->last;
						if (todayde == NULL) {
							todayel->first = todayel->last = tmpde;
							tmpde->prev = NULL;
						} else {
							todayde->next = todayel->last = tmpde;
							tmpde->prev = todayde;
						}
					}

				} else if (EQUAL(on_propagation, "delete")) {
#ifndef TDL
					if (log_level == LOG_AT_QUIT) log_entry(de,cr);
#endif
				}
				pde = de;
				de = de->next;
				if (pde->checked && EQUAL(on_propagation, "retain")) {
					if (currde == NULL) {
						currde = pde;
						el->first = pde;
						currde->prev = NULL;
						currde->next = NULL;
					} else {
						currde->next = pde;
						pde->prev = currde;
						currde = pde;
						currde->next = NULL;
					}
				} else {
					free(pde);
				}
			}
			el->last = currde;
			
			/*** Now, for the recurring entries. ***/
			currde = NULL;
			de = el->rl_first; 
			el->rl_first = NULL;
			while (de != NULL) {
				if (! de->checked ) {
					tmpdc = de->starting_day_code;
					de->starting_day_code = yestercode(datecode);
					if (check_deadline(datecode, de, cr)) {
						tmpde = NEW(struct day_entry);
						tmpde->next = NULL;
						strcpy(tmpde->text, de->text);
						tmpde->priority = de->priority;
						tmpde->deadline = de->deadline;
						tmpde->starting_day_code = yestercode(datecode);
						tmpde->checked = FALSE;
						tmpde->recurring_entry = FALSE;
						todayde = todayel->last;
						if (todayde == NULL) {
							todayel->first = todayel->last = tmpde;
							tmpde->prev = NULL;
						} else {
							todayde->next = todayel->last = tmpde;
							tmpde->prev = todayde;
						}
					}
					de->starting_day_code = tmpdc;
				} else {
#ifndef TDL
					if (log_level == LOG_AT_QUIT) log_entry(de,cr);
#endif
				}
				pde = de;
				de = de->next;
				if ((pde->checked) && (EQUAL(on_propagation, "retain"))) {
					if (currde == NULL) {
						currde = pde;
						el->first = pde;
						currde->prev = NULL;
						currde->next = NULL;
					} else {
						currde->next = pde;
						pde->prev = currde;
						currde = pde;
						currde->next = NULL;
					}
				} else {
					free(pde);
				}
			}
			el->rl_last = currde;
		} else {
			/*** If it's not in the past, just check the deadline ***
			 *** to possibly activate an action.                  ***/
			for (de = el->first, pde=NULL; de != NULL; pde=de, de = de->next) {
				if (! check_deadline(datecode, de, cr)) {
					if (pde == NULL) {
						el->first = de->next;
					} else {
						pde->next = de->next;
					}
				}
			}
		}
	}

	/*
	 *  Finally, check the rest of the recurring entries for deadline
	 *  actions.
	 */
	tmpde = NEW(struct day_entry);
	for (rl = cr->rl_head; rl != NULL; rl = rl->next) {
		if (rl->starting_day_code > datecode) {
			strcpy(tmpde->text, rl->text);
			tmpde->priority = rl->priority;
			tmpde->deadline = rl->deadline;
			tmpde->starting_day_code = rl->starting_day_code;
			check_deadline(datecode, tmpde, cr);
		}
	}

   /* RECURSE! */
	if (cr->subcats != NULL) {
		for (cr=cr->subcats; cr != NULL; cr=cr->next)
			propagate_category(cr);
	}
}

/*
 * **********************************************************************
 * This is the routine that drives propagatation of list items.  This
 * is done for all categories via recursion in "propagate_category",
 * which does the dirty work of actual propagation.
 */
void propagate ()
{
	struct category_rec *cr;

	cr = category_head;
	while (cr != NULL) {
		propagate_category(cr);
		cr = cr->next;
	}
	changed = TRUE;
}
