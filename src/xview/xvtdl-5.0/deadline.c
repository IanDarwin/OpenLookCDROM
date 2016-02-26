/*
 * $Id: deadline.c,v 1.2 1992/10/06 11:28:47 jipping Exp $
 * **********************************************************************
 *
 *  Deadline.c ==> Routines that implement deadlines
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
 * $Log: deadline.c,v $
 * Revision 1.2  1992/10/06  11:28:47  jipping
 * Bug fixes after beta test: error checking on deadline spec, expanding of
 * message to mail to user, replacing hard coded mail program with user
 * defined program (from Makefile)
 *
 * Revision 1.1  1992/09/15  12:06:58  jipping
 * Initial revision
 *
 */
#include "globaldefs.h"

extern int dc_month(), dc_day(), dc_year();

#ifdef TDL
extern int do_deadline;
#endif

void	deadline_done_proc();

struct deadline_rec *save_dr;

#ifndef TDL
/*
 * Global object definitions.
 */
deadline_deadline_frame_objects	*deadline_deadline_frame;

/*
 * **********************************************************************
 * A dummy stub to call the GUIDE-generated routines to initialize the
 * deadline window.
 */
void initialize_deadline()
{
	deadline_deadline_frame =
		deadline_deadline_frame_objects_initialize(NULL, tdlist);
}

/*
 * **********************************************************************
 * Start up the deadline editor on a new entry.  Called from the 
 * "Deadline..." button.  This will set the deadline for an edited by
 * detecting if there is a deadline structure connected via
 * PANEL_CLIENT_DATA to the "Done" button.  
 */
void create_deadline(item, event)
Panel_item item; 
Event      *event;
{
	struct deadline_rec *dr;
	char date[12], unit;
	int acts, m, d, y, units;

	/*
    *  Start off by initializing the window to a default state.
    */
	xv_set(deadline_deadline_frame->deadline_delete_time,
			 PANEL_INACTIVE, TRUE,
			 PANEL_VALUE, 0,
			 0);
	xv_set(deadline_deadline_frame->deadline_delete_units,
			 PANEL_INACTIVE, TRUE,
			 PANEL_VALUE, 0,
			 0);
	xv_set(deadline_deadline_frame->deadline_up_increment,
			 PANEL_INACTIVE, TRUE,
			 PANEL_VALUE, 0,
			 0);
	xv_set(deadline_deadline_frame->deadline_down_increment, 
			 PANEL_INACTIVE, TRUE,
			 PANEL_VALUE, 0,
			 0);
	xv_set(deadline_deadline_frame->deadline_on_mail_to,
			 PANEL_INACTIVE, TRUE,
			 PANEL_VALUE, "",
			 0);
	xv_set(deadline_deadline_frame->deadline_after_mail_to, 
			 PANEL_INACTIVE, TRUE,
			 PANEL_VALUE, "",
			 0);
	xv_set(deadline_deadline_frame->deadline_move_after,
			 PANEL_INACTIVE, TRUE,
			 PANEL_VALUE, 0,
			 0);
	xv_set(deadline_deadline_frame->deadline_move_units,
			 PANEL_INACTIVE, TRUE,
			 PANEL_VALUE, 0,
			 0);
	xv_set(deadline_deadline_frame->deadline_actions,
			 PANEL_VALUE,  0,
			 0);
	
	/*** Does a deadline item exist already? ***/
	dr = (struct deadline_rec *)
		xv_get(deadline_deadline_frame->deadline_done, PANEL_CLIENT_DATA);

	/*
    * Set the deadline up according to the properties -- because no
    * deadline is previously existing.
    */
	if (dr == NULL) {
		save_dr = NULL;
		xv_set(deadline_deadline_frame->deadline_date,
				 PANEL_VALUE, "",
				 0);
		xv_set(deadline_deadline_frame->deadline_actions,
				 PANEL_VALUE, default_deadline.actions,
				 0);
		xv_set(deadline_deadline_frame->deadline_delete_time,
				 PANEL_VALUE, default_deadline.delete_time,
				 0);
		xv_set(deadline_deadline_frame->deadline_delete_units,
				 PANEL_VALUE, default_deadline.delete_units,
				 0);
		if (BIT_IS_SET(default_deadline.actions, 0)) {
			xv_set(deadline_deadline_frame->deadline_delete_time,
					 PANEL_INACTIVE,  FALSE,
					 0);
			xv_set(deadline_deadline_frame->deadline_delete_units,
					 PANEL_INACTIVE,  FALSE,
					 0);
		}

		xv_set(deadline_deadline_frame->deadline_up_increment,
				 PANEL_VALUE, default_deadline.priority_up_units,
				 0);
		if (BIT_IS_SET(default_deadline.actions, 1)) {
			xv_set(deadline_deadline_frame->deadline_up_increment,
					 PANEL_INACTIVE,  FALSE,
					 0);
		}
		xv_set(deadline_deadline_frame->deadline_down_increment,
				 PANEL_VALUE, default_deadline.priority_down_units,
				 0);
		if (BIT_IS_SET(default_deadline.actions, 2)) {
			xv_set(deadline_deadline_frame->deadline_down_increment,
					 PANEL_INACTIVE,  FALSE,
					 0);
		}
		xv_set(deadline_deadline_frame->deadline_on_mail_to,
				 PANEL_VALUE, default_deadline.mail_on,
				 0);
		if (BIT_IS_SET(default_deadline.actions, 3)) {
			xv_set(deadline_deadline_frame->deadline_on_mail_to,
					 PANEL_INACTIVE,  FALSE,
					 0);
		}
		xv_set(deadline_deadline_frame->deadline_after_mail_to,
				 PANEL_VALUE, default_deadline.mail_after,
				 0);
		if (BIT_IS_SET(default_deadline.actions, 4)) {
			xv_set(deadline_deadline_frame->deadline_after_mail_to,
					 PANEL_INACTIVE,  FALSE,
					 0);
		}
		xv_set(deadline_deadline_frame->deadline_move_after,
				 PANEL_VALUE, default_deadline.move_time,
				 0);
		xv_set(deadline_deadline_frame->deadline_move_units,
				 PANEL_VALUE, default_deadline.move_units,
				 0);
		if (BIT_IS_SET(default_deadline.actions, 5)) {
			xv_set(deadline_deadline_frame->deadline_move_after,
					 PANEL_INACTIVE,  FALSE,
					 0);
			xv_set(deadline_deadline_frame->deadline_move_units,
					 PANEL_INACTIVE,  FALSE,
					 0);
		}
	} else {
      /*
       *  We have a pre-existing deadline.  Save the deadline (in case of
       *  a cancel) and set up the window according to the existing 
       *  deadline.
       */
		save_dr = NEW(struct deadline_rec);
		save_dr->datecode = dr->datecode;
		save_dr->relative = dr->relative;
		save_dr->actions = dr->actions;
		save_dr->delete_time = dr->delete_time;
		save_dr->delete_units = dr->delete_units;
		save_dr->priority_up_units = dr->priority_up_units;
		save_dr->priority_down_units = dr->priority_down_units;
		strcpy(save_dr->mail_on, dr->mail_on);
		strcpy(save_dr->mail_after, dr->mail_after);
		save_dr->move_time = dr->move_time;
		save_dr->move_units = dr->move_units;

		if (dr->relative) {
			units = dr->datecode - (dr->datecode / 10) * 10;
			switch (units) {
		      case 0: unit = 'd'; break;
				case 1: unit = 'w'; break;
				case 2: unit = 'm'; break;
				case 3: unit = 'y'; break;
			}
			sprintf(date, "%d%c", dr->datecode/10, unit);
		} else {
			sprintf(date, "%d/%d/%d",
					  dc_month(dr->datecode), dc_day(dr->datecode), dc_year(dr->datecode)-1900);
		}
		xv_set(deadline_deadline_frame->deadline_date, PANEL_VALUE, date, 0);
		xv_set(deadline_deadline_frame->deadline_actions, PANEL_VALUE, dr->actions, 0);
		if (BIT_IS_SET(dr->actions, 0)) {
			xv_set(deadline_deadline_frame->deadline_delete_time,
					 PANEL_INACTIVE,  FALSE,
					 PANEL_VALUE, dr->delete_time,
					 0);
			xv_set(deadline_deadline_frame->deadline_delete_units,
					 PANEL_INACTIVE,  FALSE,
					 PANEL_VALUE, dr->delete_units,
					 0);
		}
		if (BIT_IS_SET(dr->actions, 1)) {
			xv_set(deadline_deadline_frame->deadline_up_increment,
					 PANEL_INACTIVE,  FALSE,
					 PANEL_VALUE, dr->priority_up_units,
					 0);
		}
		if (BIT_IS_SET(dr->actions, 2)) {
			xv_set(deadline_deadline_frame->deadline_down_increment,
					 PANEL_INACTIVE,  FALSE,
					 PANEL_VALUE, dr->priority_down_units,
					 0);
		}
		if (BIT_IS_SET(dr->actions, 3)) {
			xv_set(deadline_deadline_frame->deadline_on_mail_to,
					 PANEL_INACTIVE,  FALSE,
					 PANEL_VALUE, dr->mail_on,
					 0);
		}
		if (BIT_IS_SET(dr->actions, 4)) {
			xv_set(deadline_deadline_frame->deadline_after_mail_to,
					 PANEL_INACTIVE,  FALSE,
					 PANEL_VALUE, dr->mail_after,
					 0);
		}
		if (BIT_IS_SET(dr->actions, 5)) {
			xv_set(deadline_deadline_frame->deadline_move_after,
					 PANEL_INACTIVE,  FALSE,
					 PANEL_VALUE, dr->move_time,
					 0);
			xv_set(deadline_deadline_frame->deadline_move_units,
					 PANEL_INACTIVE,  FALSE,
					 PANEL_VALUE, dr->move_units,
					 0);
		}
	}

	xv_set(item, PANEL_NOTIFY_STATUS, XV_ERROR, 0);
	xv_set(deadline_deadline_frame->deadline_frame, XV_SHOW, TRUE, 0);
}

/*
 * **********************************************************************
 * Callback routine for the deadline actions.  Basically, activates or
 * deactivates items depending on the action settings.
 */
void
change_deadline_actions(item, value, event)
	Panel_item	item;
	unsigned int	value;
	Event		*event;
{
	deadline_deadline_frame_objects *ip
		= (deadline_deadline_frame_objects *) xv_get(item, XV_KEY_DATA, INSTANCE);
	short	i;
	
	for (i = 0; i < 6; i++) {
		if (value & 01) {
			switch (i) {
			   case 0:
				   xv_set(ip->deadline_delete_time, PANEL_INACTIVE, FALSE, 0);
				   xv_set(ip->deadline_delete_units, PANEL_INACTIVE, FALSE, 0);
					break;

			   case 1:
				   xv_set(ip->deadline_up_increment, PANEL_INACTIVE, FALSE, 0);
					break;

			   case 2:
				   xv_set(ip->deadline_down_increment, PANEL_INACTIVE, FALSE, 0);
					break;

			   case 3:
				   xv_set(ip->deadline_on_mail_to, PANEL_INACTIVE, FALSE, 0);
					break;

			   case 4:
				   xv_set(ip->deadline_after_mail_to, PANEL_INACTIVE, FALSE, 0);
					break;

			   case 5:
				   xv_set(ip->deadline_move_after, PANEL_INACTIVE, FALSE, 0);
				   xv_set(ip->deadline_move_units, PANEL_INACTIVE, FALSE, 0);
					break;
			}
		} else {
			switch (i) {
			   case 0:
				   xv_set(ip->deadline_delete_time, PANEL_INACTIVE, TRUE, 0);
				   xv_set(ip->deadline_delete_units, PANEL_INACTIVE, TRUE, 0);
					break;

			   case 1:
				   xv_set(ip->deadline_up_increment, PANEL_INACTIVE, TRUE, 0);
					break;

			   case 2:
				   xv_set(ip->deadline_down_increment, PANEL_INACTIVE, TRUE, 0);
					break;

			   case 3:
				   xv_set(ip->deadline_on_mail_to, PANEL_INACTIVE, TRUE, 0);
					break;

			   case 4:
				   xv_set(ip->deadline_after_mail_to, PANEL_INACTIVE, TRUE, 0);
					break;

			   case 5:
				   xv_set(ip->deadline_move_after, PANEL_INACTIVE, TRUE, 0);
				   xv_set(ip->deadline_move_units, PANEL_INACTIVE, TRUE, 0);
					break;
			}
		}
		value >>= 1;
	}
}

/*
 * **********************************************************************
 * Callback routine for the Cancel button.  Reinstates the saved deadline
 * and closes the window.
 */
void
deadline_cancel_proc(item, event)
	Panel_item	item;
	Event		*event;
{
	deadline_deadline_frame_objects *ip 
		= (deadline_deadline_frame_objects *) xv_get(item, XV_KEY_DATA, INSTANCE);
	
	xv_set(ip->deadline_done, PANEL_CLIENT_DATA, save_dr, 0);
	xv_set(ip->deadline_frame, XV_SHOW, FALSE, 0);
	xv_set(entry_frame, XV_SHOW, TRUE, 0);
}

/*
 * **********************************************************************
 * Callback routine for the Done button.  Sets up a deadline_rec
 * structure and attaches that structure to the Done button (via
 * PANEL_CLIENT_DATA).
 */
void deadline_done_proc(item, event)
Panel_item	item;
Event		*event;
{
	deadline_deadline_frame_objects *ip
		= (deadline_deadline_frame_objects *) xv_get(item, XV_KEY_DATA, INSTANCE);
	struct deadline_rec *dr;
	char date[12];
	int acts, m, d, y, units;

	/*** DO we need a NEW rec? ***/
	if ((dr = (struct deadline_rec *)
		  xv_get(ip->deadline_done, PANEL_CLIENT_DATA)) == NULL) {
		dr = NEW(struct deadline_rec);
	}
	
	/*** Set the datecode ***/
	strcpy(date, (char *)xv_get(ip->deadline_date, PANEL_VALUE));
	if (strlen(date) == 0) {
		notice_prompt(entry_frame, NULL,
						  NOTICE_MESSAGE_STRINGS,
						     "The deadline date is empty.",
						  0,
						  NOTICE_BUTTON, "Ok", 1,
						  0);
		xv_set(entry_frame, XV_SHOW, TRUE, 0);
		return;
	}
	if (strchr(date, '/') != 0) {
		sscanf(date, "%d/%d/%d", &m, &d, &y);
		dr->datecode = (y-90)*10000 + m*100 + d;
		dr->relative = FALSE;
	} else {
		units = date[strlen(date)-1];
		date[strlen(date)-1] = '\0';
		dr->datecode = atoi(date)*10;
		switch (units) {
		   case 'd': break;
			case 'w': dr->datecode += 1; break;
			case 'm': dr->datecode += 2; break;
			case 'y': dr->datecode += 3; break;
			default:
				notice_prompt(entry_frame, NULL,
								  NOTICE_MESSAGE_STRINGS,
								     "The relative deadline must only use",
								     "units specified by \"d\", \"w\", \"m\", or \"y\".",
								     "The deadline is unchanged.",
								  0,
								  NOTICE_BUTTON, "Ok", 1,
								  0);
				xv_set(entry_frame, XV_SHOW, TRUE, 0);
				return;
		}
		dr->relative = TRUE;
	}

   /*** Now set up the action information ***/
	dr->actions = xv_get(ip->deadline_actions, PANEL_VALUE);
	dr->delete_time =
		dr->delete_units =
			dr->priority_up_units =
				dr->priority_down_units =
					dr->move_time =
						dr->move_units = 0;
	dr->mail_on[0] = '\0';
	dr->mail_after[0] = '\0';

	if (BIT_IS_SET(dr->actions, 0)) {
		dr->delete_time = xv_get(ip->deadline_delete_time, PANEL_VALUE);
		dr->delete_units = xv_get(ip->deadline_delete_units, PANEL_VALUE);
	}
	if (BIT_IS_SET(dr->actions, 1)) {
		dr->priority_up_units = xv_get(ip->deadline_up_increment, PANEL_VALUE);
	}
	if (BIT_IS_SET(dr->actions, 2)) {
		dr->priority_down_units = xv_get(ip->deadline_down_increment, PANEL_VALUE);
	}
	if (BIT_IS_SET(dr->actions, 3)) {
		strcpy(dr->mail_on,
				 (char *)xv_get(ip->deadline_on_mail_to, PANEL_VALUE));
	}
	if (BIT_IS_SET(dr->actions, 4)) {
		strcpy(dr->mail_after,
				 (char *)xv_get(ip->deadline_after_mail_to, PANEL_VALUE));
	}
	if (BIT_IS_SET(dr->actions, 5)) {
		dr->move_time = xv_get(ip->deadline_move_after, PANEL_VALUE);
		dr->move_units = xv_get(ip->deadline_move_units, PANEL_VALUE);
	}
	
	xv_set(ip->deadline_done, PANEL_CLIENT_DATA, dr, 0);

	xv_set(ip->deadline_frame, XV_SHOW, FALSE, 0);
	xv_set(entry_frame, XV_SHOW, TRUE, 0);
}

#endif

/*
 * **********************************************************************
 * Big time deadline checker and action activator.  Ultimately returns a
 * boolean value that indicates whether the item connected with this
 * deadline should be retained in the todo list.
 *
 * Note that the "delete" action is done last.  This is because, if we
 * are to delete this entry, we need to process actions first.
 */
int check_deadline(datecode, de, cr)
int datecode;
struct day_entry *de;
struct category_rec *cr;
{
	char *temp_file;
	FILE *tmp;
	char mailcmd[120];
	struct passwd *pwd;
	int deadcode, units;
	int diff, month, days, years, m, d, y;

#ifdef TDL
	if (! do_deadline) return TRUE;
#endif

	/*** Is there a deadline to do? ***/
	if (de->deadline == NULL) return TRUE;

	/*
    * First compute the deadline datecode, if the datecode is relative.
    */
	if (de->deadline->relative) {
		units = de->deadline->datecode - (de->deadline->datecode / 10) * 10;
		deadcode = de->deadline->datecode/10;
		switch (units) {
		   case 0:
		   case 1:
			   if (units == 0) {
					days = deadcode;
				} else {
					days = deadcode * 7;
				}
				m = dc_month(de->starting_day_code);
				d = dc_day(de->starting_day_code);
				y = dc_year(de->starting_day_code);
				
				while (days != 0) {
					if (d+days > daysinmonth(m,y)) {
						m++;
						if (m > 12) {m = 1; y++;}
						days -= daysinmonth(m,y);
					} else {
						d += days;
						days = 0;
					}
				}
				deadcode = (y-1990)*10000 + m*100 + d;
				break;
			
		   case 2:
			   years = (deadcode + dc_month(de->deadline->datecode)) / 12;
				month = (deadcode + dc_month(de->deadline->datecode)) % 12;
				deadcode = 
					(dc_year(de->starting_day_code)-1990+years)*10000 +
						month*100 + dc_day(de->starting_day_code);
				break;

		   case 3:
			   deadcode = de->starting_day_code + deadcode*10000;
				break;
		}
	} else {
		deadcode = de->deadline->datecode;
	}

   /*** Should we continue?  Is the deadline past. ***/
	if (deadcode > datecode) return TRUE;

	/*
    * There is only one case where we do something if ON the deadline.
    * Send mail.
    */
	if (datecode == deadcode) {
		if (BIT_IS_SET(de->deadline->actions, 3)) {
			if ( (temp_file = (char *)tempnam(NULL, "xvtdl")) == NULL) {
				fprintf(stderr, "Unable to create temporary file name\n");
				return TRUE;
			}
			if ( (tmp = fopen(temp_file, "w")) == NULL) {
				fprintf(stderr, "Unable to open temp file %s\n", temp_file);
				free(temp_file);
				return TRUE;
			}
			pwd = getpwuid(getuid());

			fprintf(tmp, "Today is the deadline for the todo list item\n");
			fprintf(tmp, "      \"%s\"\n", de->text);
			fprintf(tmp, "      Category: %s\n", cr->name);
			fprintf(tmp, "      Priority: %d\n", de->priority);
			fclose(tmp);

			sprintf(mailcmd, "%s -s 'TDL Item Deadline is Now' %s <%s",
					           MAILPGM, pwd->pw_name, temp_file);
			system(mailcmd);
			unlink(temp_file);
		}
		return TRUE;
	}

   /*** Increase priority? ***/
	if (BIT_IS_SET(de->deadline->actions, 1)) {
		if (EQUAL(priority_listing, "ascending")) {
			de->priority -= de->deadline->priority_down_units;
			if (de->priority < 1) de->priority = 1;
		} else {
			de->priority += de->deadline->priority_up_units;
			if (de->priority > 9) de->priority = 9;
		}
	}

   /*** Decrease priority? ***/
	if (BIT_IS_SET(de->deadline->actions, 2)) {
		if (EQUAL(priority_listing, "ascending")) {
			de->priority += de->deadline->priority_up_units;
			if (de->priority > 9) de->priority = 9;
		} else {
			de->priority -= de->deadline->priority_down_units;
			if (de->priority < 1) de->priority = 1;
		}
	}

   /*** Send mail after a deadline passed? ***/
	if (BIT_IS_SET(de->deadline->actions, 4)) {
		if ( (temp_file = (char *)tempnam(NULL, "xvtdl")) == NULL) {
			fprintf(stderr, "Unable to create temporary file name\n");
			return TRUE;
		}
		if ( (tmp = fopen(temp_file, "w")) == NULL) {
			fprintf(stderr, "Unable to open temp file %s\n", temp_file);
			free(temp_file);
			return TRUE;
		}
		pwd = getpwuid(getuid());
		
		fprintf(tmp, "The deadline for the todo list item\n");
		fprintf(tmp, "     \"%s\"\n", de->text);
		fprintf(tmp, "     Category: %s\n", cr->name);
		fprintf(tmp, "     Priority: %d\n", de->priority);
		fprintf(tmp, "has past.  It was %1d/%1d/%1d.",
				       dc_month(deadcode),
				       dc_day(deadcode),
				       dc_year(deadcode)-1900);
		fclose(tmp);
		
		sprintf(mailcmd, "%s -s 'TDL Item Deadline has PAST!' %s <%s",
				  MAILPGM, pwd->pw_name, temp_file);
		system(mailcmd);
		unlink(temp_file);
	}

   /*** Move a deadline? ***/
   /*
	 * This is complicated.  It is split into moving a deadline
	 * *relatively* and moving *absolutely*.  Moving relatively is -- for
	 * ease of coding -- done in terms of days (e.g., 3w moved 1 week will
	 * be rewritten as 28 days).
	 *
	 * Both settings are done in terms of "days since 1990", then
	 * recoverted back to the desired relative or absolute form.
	 */
	if (BIT_IS_SET(de->deadline->actions, 5)) {
		if (de->deadline->relative) {
			days = days_since_1990(dc_month(deadcode),
										  dc_day(deadcode),
										  dc_year(deadcode)) -
					 days_since_1990(dc_month(de->starting_day_code),
										  dc_day(de->starting_day_code),
										  dc_year(de->starting_day_code));
			d = 0;
			switch (de->deadline->move_units) {
		      case 0:
		      case 1:
			      if (de->deadline->move_units == 0) {
						d = de->deadline->move_time;
					} else {
						d = de->deadline->move_time * 7;
					}
					break;
					
				case 2:
					month = dc_month(deadcode);
					y = dc_year(deadcode);
					for (m=1; m <= de->deadline->move_time; m++) {
						d += daysinmonth(month,y);
						month++;
						if (month > 12) {
							month = 1; 
							y++;
						}
					}
					break;

				case 3:
					for (y=1; y<=de->deadline->move_time; y++)
						d += leapyear(dc_year(deadcode)+y)?366:365;
					break;
			}
			days += d;
			de->deadline->datecode = days*10;
		} else {
			switch (de->deadline->move_units) {
		      case 0:
		      case 1:
			      if (de->deadline->move_units == 0) {
						days = de->deadline->move_time;
					} else {
						days = de->deadline->move_time * 7;
					}
					m = dc_month(deadcode);
					d = dc_day(deadcode);
					y = dc_year(deadcode);
					
					while (days != 0) {
						if (d+days > daysinmonth(m,y)) {
							m++;
							if (m > 12) {m = 1; y++;}
							days -= daysinmonth(m,y);
						} else {
							d += days;
							days = 0;
						}
					}
					de->deadline->datecode = (y-1990)*10000 + m*100 + d;
					break;
					
				case 2:
					years = (de->deadline->move_time + dc_month(deadcode)) / 12;
					month = (de->deadline->move_time + dc_month(deadcode)) % 12;
					de->deadline->datecode =
						(dc_year(deadcode)-1990+years)*10000 +
							month*100 + dc_day(deadcode);
					break;
					
				case 3:
					de->deadline->datecode += de->deadline->move_time*10000;
					break;
				}
		}
	}

	/*** Finally, do we delete this entry after all??? ***/
	if (BIT_IS_SET(de->deadline->actions, 0)) { /* must come after "5" */
		diff = datecode - deadcode;
		switch (de->deadline->delete_units) {
		   case 0:
			   days = days_since_1990(dc_month(datecode),
											  dc_day(datecode),
											  dc_year(datecode)) -
						 days_since_1990(dc_month(deadcode),
											  dc_day(deadcode),
											  dc_year(deadcode));
				if (days >= de->deadline->delete_time) return FALSE;
				break;
			  
		   case 1:
			   days = days_since_1990(dc_month(datecode),
											  dc_day(datecode),
											  dc_year(datecode)) -
						 days_since_1990(dc_month(deadcode),
											  dc_day(deadcode),
											  dc_year(deadcode));
				if (days/7 >= de->deadline->delete_time) return FALSE;
				break;
			  
		   case 2:
			   if ((diff/10000*12 + (diff-(diff/10000))/100) >= de->deadline->delete_time) {
					return FALSE;
				}
				break;
			
		   case 3:
			   if (diff/10000 >= de->deadline->delete_time) {
					return FALSE;
				}
				break;
		}
	}
	return TRUE;
}
