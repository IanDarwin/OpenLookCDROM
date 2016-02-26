/*
 * $Id: recur.c,v 4.1 1992/10/06 11:34:15 jipping Exp $
 * **********************************************************************
 *
 * Recur.c --> routines for manipulating recurrence properties of todo
 *             list items.
 *
 * ----------------------------------------------------------------------
 * Copyright (c) 1992 by Mike Jipping and Hope College
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
 * $Log: recur.c,v $
 * Revision 4.1  1992/10/06  11:34:15  jipping
 * Fixes after beta test:  Fixed this to make a value of 0 mean "forever"
 *
 * Revision 4.0  1992/09/15  11:35:04  jipping
 * Release 4.0 beta.  Changes include:
 *   * reworking of "datecode_matches" to accomodate more flexible
 *     recurring interface.
 *   * addition of week of month and day of month considerations
 *     in recurring routines.
 *   * additions of #ifdefs to accomodate "tdl"
 *
 * Revision 3.0  1992/07/27  18:43:46  jipping
 * Release 3.0.  Fixed a bug that did not reassign head and tail vars
 * on list manipulation to the original categories.
 *
 * Revision 2.2  1992/07/16  13:38:02  jipping
 * Tweaked list management.
 *
 * Revision 2.1  1992/07/13  14:29:02  jipping
 * Cleaned up code to avoid compilation warnings.
 *
 * Revision 2.0  1992/07/06  15:55:28  jipping
 * Initial release.
 *
 *
 */

#include "globaldefs.h"

struct recurrence_list *rl_head, *rl_tail;

/*
 ***********************************************************************
 *  Utility routine that is used as a timestamp routine/
 */
int days_since_1990(month, day, year)
{
	int days=0;
	int count;

	for (count=1990; count<year; count++) days += leapyear(count)?366:365;
	for (count=1; count<month; count++)   days += daysinmonth(count, year);
	days += day;

	return(days);
}

int dc_month (datecode)
int datecode;
{
	return (datecode-(datecode/10000)*10000)/100;
}

int dc_day (datecode)
int datecode;
{
	return datecode-(datecode/10000)*10000-(datecode-(datecode/10000)*10000)/100*100;
}

int dc_year (datecode)
int datecode;
{
	return datecode/10000+1990;
}

/*
 * **********************************************************************
 * This routine is given a datecode and a recurrence specification and
 * returns a boolean value depicting whether that datecode matches 
 * the recurrence spec.  This is determined by starting date and 
 * recurrence property.
 */
int datecode_matches(datecode,rl)
int datecode;
struct recurrence_list *rl;
{
	int curr_week, week_difference, dow, dow2, wom, wks1, wks2, ms;

   /*
    *  If the spec is DAILY -- it's always true!
    */
	if (rl->daily) return TRUE;

   /*
    * In order to check the rest, we must compute a few things...
	 * namely, current day of week, current weeks-since-1990, the
    * weeks-since-1990 for the starting date of the spec, and
    * the difference between the week computation.
    */
	dow = zeller(dc_month(datecode), dc_day(datecode), dc_year(datecode));
	wks1 = days_since_1990(dc_month(datecode), dc_day(datecode), dc_year(datecode))/7;
	wks2 = days_since_1990(dc_month(rl->starting_day_code),
								  dc_day(rl->starting_day_code),
								  dc_year(rl->starting_day_code))/7;
	week_difference = wks1 - wks2;

	/*
    *  Check the rest of the possible recurrence properties.
    */
	if (rl->weekly &&
		 ((week_difference < rl->number_of_weeks) || (rl->number_of_weeks == 0)) &&
		 (week_difference >= 0) &&
		 (BIT_IS_SET(rl->dow, dow)))
		 return TRUE;

	if (rl->biweekly && (week_difference < rl->number_of_weeks) &&
		 (week_difference >= 0) &&
		 (BIT_IS_SET(rl->dow, dow)))
		 if ( (float)(week_difference/2) == ((float)week_difference/2.0) )
			 return TRUE;

	if (rl->monthly) {
		ms = (dc_month(datecode) - dc_month(rl->starting_day_code))
			      + 12 * (dc_year(datecode) - dc_year(rl->starting_day_code))
					+ 1;
		if ((ms > 0) & ((ms <= rl->number_of_months) || (rl->number_of_months == 0))) {
			if (rl->week_number == 0) {
				if (dc_day(datecode) == rl->dom)
					return TRUE;
			} else {
				if (BIT_IS_SET(rl->dow, dow)) {
					wom = (dc_day(datecode)-1)/7;
					if (BIT_IS_SET(rl->week_number, wom))
						return TRUE;
				}
			}
		}
			
	}
		 
	if (rl->yearly && (rl->starting_day_code % 10000 == datecode % 10000)) return TRUE;

	/* if nothing matches.... */
	return FALSE;
}

#ifndef TDL

/*
 * **********************************************************************
 * Start up the recurring editor on a new entry.  Called from the 
 * "Recurring..." button
 */
	
void create_recurring(item, event)
Panel_item item; 
Event      *event;
{
	xv_set(item, PANEL_NOTIFY_STATUS, XV_ERROR, 0);
	xv_set(freq, PANEL_CLIENT_DATA, FALSE, 0);
	xv_set(recurring_frame, XV_SHOW, TRUE, 0);
}

/*
 * **********************************************************************
 * Close up the recurring editor.  Called from the "cancel" button.
 */
void cancel_recurring(item, event)
Panel_item item; 
Event      *event;
{
	xv_set(recurring_frame, XV_SHOW, FALSE, 0);
	xv_set(entry_frame, XV_SHOW, TRUE, 0);
}

/*
 * **********************************************************************
 * Callback notify function for the "Recurring..." button.  Basically,
 * we set up the window widgets (for creating or editing).
 */
void choose_recurring()
{
	int fre;

	fre = xv_get(freq, PANEL_VALUE);
	xv_set(day_of_week, XV_SHOW, FALSE, 0);
	xv_set(weeks, XV_SHOW, FALSE, 0);
	xv_set(week_of_month, XV_SHOW, FALSE, 0);
	xv_set(day_of_month, XV_SHOW, FALSE, 0);
	xv_set(months, XV_SHOW, FALSE, 0);
	
	switch (fre) {
   	case 0:  /* daily -- no more action needed */
	   case 1:
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
		   break;

		case 2:  /* weekly -- need dow and number of weeks */
		case 3:  /* biweekly -- need dow and number of weeks */
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
			xv_set(day_of_week, XV_SHOW, TRUE, 0);
			xv_set(weeks, XV_SHOW, TRUE, 0);
			break;

		case 4:  /* monthly -- need week of month, dom, dow, and # months */
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
			xv_set(day_of_week, XV_SHOW, TRUE, 0);
			xv_set(week_of_month, XV_SHOW, TRUE, 0);
			xv_set(day_of_month, XV_SHOW, TRUE, 0);
			xv_set(months, XV_SHOW, TRUE, 0);
			break;

		case 5:  /* yearly -- no more action needed */
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
			break;
	}
}

/*
 * **********************************************************************
 * Callback notify function for the "Done" button in the recurrence
 * editor window.
 *
 * Here, we setup -- or destroy -- a recurrence record.  When we return,
 * the PANEL_CLIENT_DATA on the frequency item is set to TRUE if we have
 * left specifying a recurrence property or to FALSE if we have gone 
 * from a recurrence property to nothing.
 */
void close_recurring(item, event)
Panel_item item; 
Event      *event;
{
	int frequency, dow, d, day, diff, diff2;
	struct recurrence_list *tmprl;
	struct recurrence_list *rl, *rlp;
	int datecode, value;
	struct category_rec *cr;

	/*
    *  Start by finding out the category we are specifying in...
    */
	cr = (struct category_rec *)xv_get(entry_category, PANEL_CLIENT_DATA);

	/*
    * We might be EDITING a recurrence property.  Find it if we can.
    */
	tmprl = NULL;
	rlp = NULL;
	datecode = (curr_year-1990)*10000 + curr_month*100 + curr_day;
	for (rl = cr->rl_head; rl != NULL; rlp = rl, rl = rl->next) {
		if (datecode_matches(datecode, rl)) {
			if (EQUAL((char *)xv_get(entry_text, PANEL_VALUE), rl->text)) {
				tmprl = rl;
				break;
			}
		}
	}

	/* 
	 *  Now, start to set up the recurrence property, if we are creating
    *  or editing.
    */
	frequency = xv_get(freq, PANEL_VALUE);
	if (frequency > 0) {

		/** Create a recurrence structure if we need to **/
		if (tmprl == NULL) {
			tmprl = NEW(struct recurrence_list);
			if (cr->rl_head == NULL) {
				cr->rl_head = cr->rl_tail = tmprl;
			} else {
				cr->rl_tail->next = tmprl;
				cr->rl_tail = tmprl;
			}
		}

		/*
       *  If the category menu for the editing window is different 
       *  than that for the main window, temporarily reset the proper
       *  variables.
       */
		if (cr == (struct category_rec *)xv_get(categories, PANEL_CLIENT_DATA)) {
			rl_head = cr->rl_head;
			rl_tail = cr->rl_tail;
		}
		
		/*
       *  AND NOW...fill in the blanks of the recurrence structure
       */
		tmprl->starting_day_code = (curr_year-1990)*10000 + curr_month*100 + curr_day;
		d = xv_get(day_of_week, PANEL_VALUE);
      if ((frequency != 1) & (frequency != 5) & (d != 0)) {
			dow = zeller(curr_month, curr_day, curr_year);
			diff = 999;
			for (day=0; day < 7; day++) {
				if (BIT_IS_SET(d, day)) {
					diff2 = day - dow;
					if (diff2 < 0) diff2 += 7;
					if (diff > diff2) diff = diff2;
				}
			}
			tmprl->starting_day_code += diff;
		}
		tmprl->daily = tmprl->weekly = tmprl->biweekly = tmprl->monthly = tmprl->yearly = FALSE;
		tmprl->dow = -1;
		tmprl->week_number =
			tmprl->number_of_weeks =
				tmprl->number_of_months =
					tmprl->dom = 0;
		tmprl->next = NULL;
		
		switch (frequency) {
	      case 1:    /* daily */
			   tmprl->daily = TRUE;
		      break;

		   case 2:  /* weekly -- need dow and number of weeks */
				tmprl->weekly = TRUE;
				tmprl->number_of_weeks = xv_get(weeks, PANEL_VALUE);
				tmprl->dow = xv_get(day_of_week, PANEL_VALUE);
				break;

		   case 3:  /* biweekly */
				tmprl->biweekly = TRUE;
				tmprl->number_of_weeks = xv_get(weeks, PANEL_VALUE);
				tmprl->dow = xv_get(day_of_week, PANEL_VALUE);
				break;

			case 4:  /* monthly -- need week of month and dow */
				tmprl->monthly = TRUE;
				tmprl->week_number = xv_get(week_of_month, PANEL_VALUE);
				if (xv_get(day_of_month, PANEL_INACTIVE) == TRUE) {
					tmprl->dow = xv_get(day_of_week, PANEL_VALUE);
					tmprl->dom = 0;
				} else {
					tmprl->dow = 0;
					tmprl->dom = xv_get(day_of_month, PANEL_VALUE);
				}
				tmprl->number_of_months = xv_get(months, PANEL_VALUE);
				break;

			case 5:  /* yearly -- no more action needed */
				tmprl->yearly = TRUE;
				break;
		}
		xv_set(freq, PANEL_CLIENT_DATA, TRUE, 0);

	} else {
      /*
       * We are switching from recurring to non-recurring.
       * Reset PANEL_CLIENT_DATA and destroy the recurrence structure
       * if we need to.
       */
		xv_set(freq, PANEL_CLIENT_DATA, FALSE, 0);
		if (tmprl != NULL) {  /* get rid of the RL (we went to NONE) */
			if (rlp == NULL) {
				cr->rl_head = tmprl->next;
				if (cr->rl_tail == tmprl) cr->rl_tail = NULL;
			} else {
				rlp->next = tmprl->next;
				if (cr->rl_tail == tmprl) cr->rl_tail = rlp;
			}
			free(tmprl);
			if (cr == (struct category_rec *)xv_get(categories, PANEL_CLIENT_DATA)) {
				rl_head = cr->rl_head;
				rl_tail = cr->rl_tail;
			}
		}
	}

	/** Done!  Close the window **/
	xv_set(recurring_frame, XV_SHOW, FALSE, 0);
	xv_set(entry_frame, XV_SHOW, TRUE, 0);
}

/*
 * **********************************************************************
 * Callback routine for the week of month item -- when selected, the 
 * day of month item is inactivated.  When there is no selection, the
 * day of month item is activated.
 */
void choose_wom (item, value, event)
Panel_item item;
int value;
Event *event;
{
	if (value == 0) {
		xv_set(day_of_month, PANEL_INACTIVE, FALSE, 0);
	} else {
		xv_set(day_of_month, PANEL_INACTIVE, TRUE, 0);
	}
}

#endif
