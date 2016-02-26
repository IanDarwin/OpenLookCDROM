/*
 * $Id: common.c,v 2.3 1994/08/29 17:32:54 billr Exp $
 */
/*
 * common.c
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

#include <stdio.h>
#include <ctype.h>
#include <sys/time.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/file.h>
#ifndef CALENCHECK
# include <X11/Xlib.h>
# include <xview/xview.h>
# include <xview/font.h>
# include <xview/notice.h>
# include "xv_ct.h"
# include <pwd.h>
#endif
#ifndef NO_DEFAULTS
# include <xview/defaults.h>
#endif
#include "ct.h"

struct tm save_day;
struct appt_entry future[MAX_FUTURE_ENTRIES];
int findex = 0;         /* index into struct future array */;
int day_is_open = FALSE;	/* indicates when slot info is current */
char apts_pathname[160], tmpapts_pathname[160];
char apts_dir[128], lib_dir[128];
char t_title[160];

extern struct tm current, today, First;
extern struct dayslot *slots;
extern char apts_pathname[], tmpapts_pathname[];
extern char *progname;
extern char *othername;
extern int n_slots, n_tslots, read_only, new_entry;
extern int otherfile, one_based, version2;
extern int show_future, start_hour;
extern int mainsw_state;

#ifndef CALENCHECK
int icon_in_use = -1;
extern struct tm olddate, closedate;
extern Frame frame, prompt_frame;
extern char datestr_day[];
extern int beep, beep_open;
extern int appt_check_limit;
extern Xv_Font sfont;
extern char *smonthnames[];
extern char *mailto;
extern int locked;
extern char *version();
#endif

#ifndef NO_HOLIDAYS
extern int holiday_a, holiday_c, holiday_i, holiday_j, holiday_s;

extern int a_dates(), c_dates(), i_dates(), j_dates(), s_dates();
extern struct appt_entry a_appts[], c_appts[];
extern struct appt_entry i_appts[], j_appts[];
extern struct appt_entry s_appts[];
#endif

#ifdef __STDC__
int chk_week (int repeat, int curday);
void find_date (struct appt_entry *appt);
void orphan_check (void);
int chk_deleted (struct dayslot *slptr, struct appt_entry *aptr);
#else
int chk_week ();
void find_date ();
void orphan_check ();
int chk_deleted ();
#endif

/*
 * Add an appointment entry pointed to by aptr to the day slot
 * specified by slotno. This routine is also used by paste()
 * when copying an entry off the save shelf. If dpyflag is true,
 * then any deactivated slots are cleared on the display (used by
 * paste). Also used to add a deleted entry for a specific day.
 */
void add_to_slot(slotno, aptr, dpyflag)
int slotno;
struct appt_entry *aptr;
int dpyflag;
{
	struct appt_entry *nappt, *optr;
	int n_arrows, found = 0, i;

	if ((nappt = (struct appt_entry *)malloc(sizeof(struct appt_entry))) == NULL)
		err_rpt("out of memory", FATAL);
	if (aptr == NULL) {
		/* fill in some needed fields */
		nappt->arrows = nappt->flags = 0;
		nappt->sindex = 0;
		nappt->str[0] = '\0';
	} else
		*nappt = *aptr;
	nappt->next = NULL;
	/* add appt to list of appts for this slot */
	if (slots[slotno].first == NULL) {
		slots[slotno].first = nappt;
		slots[slotno].cur_appt = nappt;
	} else {
		/* search for end of list */
		for (optr=slots[slotno].first;optr->next;optr=optr->next)
			;
		optr->next = nappt;
	}
	/* make sure it doesn't extend too far and truncate if neccessary */
	if (slotno >= n_tslots)
		nappt->arrows = 0;	/* force notes to have no arrows */
	if ((slotno + nappt->arrows) >= n_tslots)
		nappt->arrows = n_tslots - slotno - 1;	/* truncate */
	n_arrows = nappt->arrows;
	if (nappt->flags & DELETED) {
		/* look for matching non-deleted appt in list */
		for (optr=slots[slotno].first;optr && !found;optr=optr->next)
			if (!strcmp(nappt->str, optr->str)
			    && !(optr->flags & DELETED)
			    && Repeating(optr->flags)) {
				found = 1;
				break;
			}
		if (found && slots[slotno].cur_appt == optr) {
			/* the deleted appt is the current one */
			/* if it's active, undisplay it and display
			 * next one in list (if any)
			 */
			if (slots[slotno].active) {
				if (slots[slotno].active > 1)
					/* there's another one here */
					next_appt(slotno, dpyflag);
				else
					deactivate_slot(slotno, dpyflag);
				/* adjust reference counts */
				slots[slotno].count--;
				slots[slotno].active--;
				i = 0;
				while (++i <= n_arrows)
					--slots[slotno+i].count;
			} else {
				/* currently inactive */
				/* set current to next one in the list */
				if (optr->next)
					slots[slotno].cur_appt = optr->next;
				else
					slots[slotno].cur_appt = slots[slotno].first;
				/* adjust the counts */
				slots[slotno].active--;
				while (n_arrows >= 0)
					slots[slotno+(n_arrows--)].count--;
			}
		} else {
			/* just adjust the counts */
			slots[slotno].active--;
			while (n_arrows >= 0)
				slots[slotno+(n_arrows--)].count--;
		}
	} else {
		/* look for matching deleted appt in list */
		if (Repeating(nappt->flags)) {
			for (optr=slots[slotno].first;optr && !found;optr=optr->next)
				if (!strcmp(nappt->str, optr->str) && optr->flags & DELETED) {
					found = 1;
					break;
				}
			if (found) {
				/* just adjust reference counts and return */
				slots[slotno].active++;
				while (n_arrows >= 0)
					slots[slotno+(n_arrows--)].count++;
				return;
			}
		}
		/*
		 * Make sure there are no overlaps with the appt we
		 * are adding. If there are, hide the overlapping appt.
		 */
		if (slots[slotno].active)
			deactivate_slot(slotno, dpyflag);
		/* set current one to the new one */
		slots[slotno].cur_appt = nappt;
		/* now go back and put in the info for the appt we're inserting */
		slots[slotno].count++;
		slots[slotno].active++;
		if (n_arrows > 0) {
			slots[slotno+n_arrows].count++;
			while (--n_arrows > 0)
				slots[slotno+n_arrows].count++;
		}
	}
	if (dpyflag)
		draw_day_appts();	/* redraw display */
}

/* add a note to the current day */
void add_note(appt)
struct appt_entry *appt;
{
	int	slotno, found = 0;
	struct appt_entry *optr;

	/* This used to just find a free slot and add the note
	 * to it. However, with deleted notes we need to find
	 * the matching slotno (if it exists) to make sure that
	 * the deleted and non-deleted notes end up in the same
	 * slot number (so they won't be displayed).
	 */
	if (appt->flags & DELETED) {
		/* look for matching non-deleted note */
		for (slotno=n_tslots; slotno<n_slots && !found; slotno++) {
			if (!slots[slotno].active)
				break;	/* no more notes */
			for (optr=slots[slotno].first;optr;optr=optr->next) {
				if (!strcmp(appt->str, optr->str)
				    && !(optr->flags & DELETED)
				    && Repeating(optr->flags)) {
					found = 1;
					break;
				}
			}
		}
	} else {
		/* look for free slot and/or matching deleted note */
		for (slotno=n_tslots; slotno<n_slots && !found; slotno++) {
			if (!slots[slotno].active)
				break;	/* no more notes */
			for (optr=slots[slotno].first;optr;optr=optr->next)
				if (!strcmp(appt->str, optr->str)
				    && (optr->flags & DELETED)
				    && Repeating(appt->flags)) {
					found = 1;
					break;
				}
		}
	}
	if (found)
		--slotno;  /* for loop incremented slotno */
	if (slotno == n_slots) {
		/* overflow of notes field, so
		 * add to last note field list
		 */
		slotno = n_slots - 1;
	}
	add_to_slot(slotno, appt, FALSE);
}

/*
 * Fills in appointments for the day.  
 * The ".tmp.aptsXXXXX" file is filled out
 * with all the lines from the ".appointments" file 
 * which do not pertain to the current day.
 */
int
get_day_appts()
{
	FILE *apts, *temp_apts = 0;
	int slotno, i;
	int read_stat, some_appt = 0;
	int runl;
	struct appt_entry appt;
#ifndef NO_HOLIDAYS
	int j, k;
	char buf[MAX_STRLEN], *sptr;
#endif

	if (day_is_open)
		close_day();
	if ((apts = fopen(apts_pathname, "r")) == NULL)
		err_rpt("can't open appointments file", FATAL);

	if (!read_only)
	        if ((temp_apts = fopen(tmpapts_pathname, "w")) == NULL)
			err_rpt("can't open temp file for writing", FATAL);

	for (i=0; i<n_slots; i++) {	/* init each slot */
		slots[i].count = 0;
		slots[i].cur_appt = NULL;
		slots[i].first = NULL;
		slots[i].active = 0;
	}
	First = current;
	findex = 0;

#ifndef NO_HOLIDAYS
	/*
	 * First check to see if the user has selected any holiday
	 * options and add them in.
	 */
	working(TRUE);
	if (holiday_a) {
		j = a_dates(holiday_a);
		for (k=0; k<j; k++)
			if (ymd2_compare(&current, &a_appts[k]) == 0) {
				some_appt |= (holiday_a == 1 ? SOME_MKNOTES : SOME_NOTES);
				add_note(&a_appts[k]);
			}
	}
	working(FALSE);
	if (holiday_c) {
		j = c_dates(holiday_c);
		for (k=0; k<j; k++)
			if (ymd2_compare(&current, &c_appts[k]) == 0) {
				some_appt |= (holiday_c == 1 ? SOME_MKNOTES : SOME_NOTES);
				add_note(&c_appts[k]);
			}
	}
	working(TRUE);
	if (holiday_i) {
		j = i_dates(holiday_i);
		for (k=0; k<j; k++)
			if (ymd2_compare(&current, &i_appts[k]) == 0) {
				some_appt |= (holiday_i == 1 ? SOME_MKNOTES : SOME_NOTES);
				/* look for \n */
				if ((sptr = index(i_appts[k].str, '\n')) != NULL) {
					appt = i_appts[k];
					/* two notes in one */
					strcpy(buf, appt.str);
					*sptr = '\0';
					add_note(&appt);
					/* now second half of string in the next note */
					strcpy(appt.str, &buf[(int)(sptr-appt.str)+1]);
					add_note(&appt);
				} else
					add_note(&i_appts[k]);
			}
	}
	working(FALSE);
	if (holiday_j) {
		j = j_dates(holiday_j);
		for (k=0; k<j; k++)
			if (ymd2_compare(&current, &j_appts[k]) == 0) {
				some_appt |= (holiday_j == 1 ? SOME_MKNOTES : SOME_NOTES);
				add_note(&j_appts[k]);
			}
	}
	working(TRUE);
	if (holiday_s) {
		j = s_dates(holiday_s);
		for (k=0; k<j; k++)
			if (ymd2_compare(&current, &s_appts[k]) == 0) {
				some_appt |= (holiday_s == 1 ? SOME_MKNOTES : SOME_NOTES);
				add_note(&s_appts[k]);
			}
	}
	working(FALSE);
#endif
			
	/*
	 * now go thru the appointments file
	 */
	while ((read_stat=get_aentry(apts, &appt, FALSE, FALSE, First.tm_mon+1)) != EOF) {
		if (read_stat)
			continue;	/* read error (ignore) */
		if (appt.flags & A_COMMENT) {
			if (put_aentry(temp_apts, &appt)) {
				/* write error */
				break;
			}
			continue;
		}
		current.tm_year = appt.year;
		current.tm_mon = appt.month;
		current.tm_mday = appt.day;
		if (appt.flags & ALL_YEARS)
			current.tm_year = First.tm_year;
		if (appt.flags & ALL_MONTHS)
			current.tm_mon = First.tm_mon;
		if (appt.flags & ALL_DAYS)
			current.tm_mday = First.tm_mday;
		else if (appt.flags & EVERY_MON_FRI) {
			if (First.tm_wday >= MON && First.tm_wday <= FRI)
				current.tm_mday = First.tm_mday;
			else
				current.tm_mday = 0;
		} else if (appt.flags & EVERY_SOMEDAY) {
			if ((Pickday(appt.flags) == First.tm_wday)
			    && (chk_week(appt.repeat, First.tm_mday))) {
				if (appt.flags & RUN) {
					runl = appt.runlength;
					find_date(&appt);
					while (ymd_compare(current, First) < 0 && --runl) {
						current.tm_mday += 7;
						find_date(&appt);
					}
				} else
					current.tm_mday = First.tm_mday;
			} else
				current.tm_mday = 0;
		} else if (appt.flags & REPEAT) {
			if (appt.flags & RUN)
				runl = appt.runlength;
			else
				runl = 1;
			while (ymd_compare(current, First) < 0 && runl) {
				if (appt.flags & RUN)
					--runl;
				if (runl) {
					current.tm_mday += appt.repeat;
					fix_current_day();
				}
			}
		}
		if (ymd_compare(current, First) == 0) {
			/* if it's for this day, fill in slot info */
			if (appt.flags & A_NOTE) {
				/* notes section */
				add_note(&appt);
				if (appt.flags & MARKED)
					/* marked note */
					some_appt |= SOME_MKNOTES;
				else
					/* regular note */
					some_appt |= SOME_NOTES;
			} else {
				/* regular appointment */
				slotno = (appt.hour-start_hour) * 2 + appt.minute / 30;
				if (slotno < 0)
					slotno = 0;
				if (slotno >= n_tslots)
					slotno = n_tslots - 1;
				/* add this appt to the list of appts for the slot */
				/* and update all the reference counts */
				add_to_slot(slotno, &appt, FALSE);
				some_appt |= SOME_APPTS;
			}
		} else if (appt.flags & LOOKAHEAD) {
			/* This lookahead appt was not for today, so
			 * put it in the temp file.
			 */
			if (put_aentry(temp_apts, &appt)) {
				/* write error */
				break;
			}
			if (appt.flags & EVERY_SOMEDAY) {
				/* find next occurance of this appt */
				/* starting from the current day */
				current.tm_mday = First.tm_mday;
				fix_current_day();
				find_date(&appt); /* may modify current */
			}
			if (ymd_compare(current, First) > 0) {
				/* this appt is happening in
				 * the future, so remind us of it if
				 * it is within the lookahead window.
				 */
				save_day = current;
				current.tm_mday -= appt.lookahead;
				fix_current_day();
				if (ymd_compare(current, First) <=0) {
					/* save this one for the future popup window */
					if (findex > MAX_FUTURE_ENTRIES-1) {
						err_rpt("Too many future reminders", NON_FATAL);
						continue;
					}
					future[findex] = appt;
					/* fix up ymd */
					future[findex].year = save_day.tm_year;
					future[findex].month = save_day.tm_mon;
					future[findex].day = save_day.tm_mday;
					++findex;
					some_appt |= SOME_FUTURES;
				}
			}
                } else { 	/* line is not for today */
			/* copy it to temp file */
#ifdef DEBUG
			fprintf(stderr, "get_day_appts: calling put_aentry(), appt=%x\n", &appt);
#endif
			if (put_aentry(temp_apts, &appt)) {
				/* write error */
				break;
			}
		}
        }
	if (!read_only) {
		if (ferror(temp_apts))
			err_rpt("write on temp file failed", FATAL);
        	fclose(temp_apts);        
	}
        fclose(apts);             
	current = First;
	fix_current_day();
	orphan_check();

	return(some_appt);
}


/* check for match on weekly re-ocurring appts */
int chk_week(repeat, curday)
int repeat, curday;
{
	int weeknr = 0;

	if ((repeat & ALL_WEEKS) == ALL_WEEKS)
		return(1);	/* every week */
	if ((repeat & LAST_WEEK) && ((curday+7) > monthlength(current.tm_mon)))
		return(1);	/* last week in month */

	while (curday > 7) {
		/* find which week this day is in */
		curday -= 7;
		weeknr++;
	}
	if (repeat & (0x1<<weeknr))
		return(1);
	
	return(0);	/* no match */
}


/*
 * get date of next occurrence of a weekly repeated appt
 * (it may bridge into next week, month or year)
 */
void find_date(appt)
struct appt_entry *appt;
{
	struct tm save;

	fix_current_day();
	save = current;
	/* set current to match dow of repeated appt */
	if (appt->flags & EVERY_MON_FRI) {
		if (current.tm_wday == SUN)
			current.tm_mday++;
		else if (current.tm_wday == SAT)
			current.tm_mday += 2;
	} else 
		current.tm_mday += Pickday(appt->flags) - current.tm_wday;
	fix_current_day();
	if (ymd_compare(current, save) < 0) {
		/* already happened, so start looking next week */
		current.tm_mday += 7;
		fix_current_day();
	}
	/* search for first matching week */
	while (!chk_week(appt->repeat, current.tm_mday)) {
		current.tm_mday += 7;
		fix_current_day();
	}
	/* now check to make sure this is legal, i.e. there
	 * were no month or year restrictions
	 */
	if (!(appt->flags & RUN) && ((!(appt->flags & ALL_YEARS) && current.tm_year != save.tm_year)
	   || (!(appt->flags & ALL_MONTHS) && current.tm_mon != save.tm_mon)))
		/* invalid date, due to month or year wrap */
		current = save;
}

/*
 * orphan_check() - check each slot for orphan appointments. Orphans
 * are a deleted recurring appointment where it is deleted on a
 * specific date and the original appointment no longer exists.
 */
void orphan_check()
{
	int i, n_arrows;
	struct appt_entry *aptr, *optr;

	for (i=0; i<n_slots; i++) {
		while (slots[i].first != NULL) {
			optr = slots[i].first;
			if (chk_deleted(&slots[i], optr) == -1) {
				/* just adjust the counts */
				n_arrows = optr->arrows;
				while (n_arrows >= 0)
					slots[i+(n_arrows--)].count++;
				slots[i].active++;
				if (optr->next) {
					if (slots[i].cur_appt == slots[i].first)
						slots[i].cur_appt = slots[i].first->next;
					slots[i].first = slots[i].first->next;
				} else {
					/* last one */
					if (slots[i].cur_appt == slots[i].first)
						slots[i].cur_appt = NULL;
					slots[i].first = NULL;
					slots[i].active = 0;
				}
				free(optr);
				new_entry = 1;
			} else {
				while ((aptr = optr->next)) {
					if (chk_deleted(&slots[i], aptr) == -1) {
						/* just adjust the counts */
						n_arrows = aptr->arrows;
						while (n_arrows >= 0)
							slots[i+(n_arrows--)].count++;
						slots[i].active++;
						optr->next = aptr->next;
						if (slots[i].cur_appt == aptr)
							slots[i].cur_appt = aptr->next;
						free(aptr);
						new_entry = 1;
					} else {
						optr = aptr;
					}
				}
				break;
			}
		}
		if (chk_deleted(&slots[i], slots[i].cur_appt)) {
			next_appt(i, FALSE);
			if (slots[i].cur_appt == NULL)
				/* only deleted appts, so reset to first */
				slots[i].cur_appt = slots[i].first;
		}
	}
}

/* check to see if appt is deleted */
int
chk_deleted(slptr, aptr)
struct dayslot *slptr;
struct appt_entry *aptr;
{
	struct appt_entry *optr;

	if (slptr->first == NULL || aptr == NULL)
		return(0);
	if (aptr->flags & DELETED) {
		/* run through the list and look for a matching
		 * repeating non-deleted entry. If we don't find one,
		 * this appt is a deleted orphan.
		 */
		for (optr=slptr->first; optr; optr=optr->next)
			if (!(optr->flags & DELETED) && Repeating(optr->flags))
				/* now see if the current one matches */
				if (!strcmp(optr->str, aptr->str))
					return(1);
		return(-1);  /* orphan */
	}
	if (Repeating(aptr->flags)) {
		/* run through the list to see if there are any deleted */
		for (optr=slptr->first; optr; optr=optr->next)
			if (optr->flags & DELETED) {
				/* now see if the current one matches */
				if (!strcmp(optr->str, aptr->str))
					return(1);
			}
	}
	
	return(0);
}

/*
 * When timer has expired check to see if we are close to an
 * appointment. If so, switch to the other icon so we have a
 * visual indication and beep the console (if enabled).
 */
void check_calendar()
{
	int appt_pending = 0; 	/* no appointments pending */
	int slotno = 0; 	/* start with first timeslot */
	int smin, tmin;
	int sno, save_ro = 1;
	static int echoed_sno = -1;
	static int new_day = 0;
	FILE *console;
	struct appt_entry *aptr;
	struct tm Saveday;
	char *getenv();
#ifndef CALENCHECK
	char msgfile[128];
	int some_appts = 0;	/* no appointments today */
	int read_file = 0;	/* reread appts file when true */
	FILE *msgf;
#endif

#ifndef CALENCHECK
	if (locked)	/* can't change yet */
		return;
	lock_cursors();
	locked++;	/* make it unique to us */
	strcpy(msgfile, getenv("HOME"));
	strcat(msgfile, "/.msgfile");
#endif

	sno = echoed_sno;	/* assume no console echo */
	get_today();
	Saveday = current;
	/*
	 * Check to see if we're not displaying today.
	 * (Or in the case of "calencheck" if the appointments
	 * file has been modified.) If so, we need to update our
	 * slot information.
	 */
#ifndef CALENCHECK
	if ((int)xv_get(frame, FRAME_CLOSED) && ymd_compare(closedate, today) != 0)
		new_day++;
#endif
	if (ymd_compare(current, today) != 0 || new_day || !day_is_open) {
		if (day_is_open)
			close_day();
		current = today;
#ifdef CALENCHECK
		new_day++;
		(void)get_day_appts();
		day_is_open = TRUE;
#else
		save_ro = read_only;
		read_only = 1;	/* force read only mode */
		err2console(TRUE);
		(void)get_day_appts();
		err2console(FALSE);
		read_file++;
#endif
		read_only = save_ro;
		if (new_day)
			sno = echoed_sno = -1;
	} else {
		/* slot info is current */
#ifndef CALENCHECK
		unlock_cursors();
#endif
	}

	slotno = (today.tm_hour - start_hour)*2 + today.tm_min/30;
	if (slotno < 0)
		slotno = 0;
	/* our current time (minutes past midnight) */
	tmin = today.tm_hour * 60 + today.tm_min;
	if (slots[slotno].count > 0 && slotno > 0 && slotno < n_tslots)
		/* something going on now */
		appt_pending++;
	while (slotno < n_tslots) {
		if (slots[slotno].count > 0) {
			/* convert slotno back to time difference */
			smin = start_hour * 60 + slotno * 30 - tmin;
			if (smin < 0)
				smin = 0;
			if (slots[slotno].active) {
				/* get all valid appts at this time */
				for (aptr=slots[slotno].first; aptr;
				    aptr=aptr->next)
					if (!chk_deleted(&slots[slotno], aptr)
					    && aptr->warn
					    && (smin <= aptr->warn)) {
						sno = slotno;
						appt_pending++;
						break;
					}
				if (sno > echoed_sno)
					break;
			}
		}
		slotno++;
	}
	if (!appt_pending) {
#ifndef CALENCHECK
		/*
		 * Is there anything happening today (optionally
		 * including memos)?
		 * Don't care about things that happened before now
		 * so start looking at <slotno>, which is set to
		 * reflect the current hour (or 0 if before start_hour).
		 */
		slotno = (today.tm_hour - start_hour)*2 + today.tm_min/30;
		if (slotno < 0)
			slotno = 0;
		/*
		 * appt_check_limit is typically either "n_tslots"
		 * or "n_slots" depending on whether we include the
		 * notes section when indicating that we still have
		 * appts today.
		 */
		while (slotno < appt_check_limit)
			if (slots[slotno++].count) {
				some_appts++;
				break;
			}
		/* maybe change the icon */
		if ((some_appts && (icon_in_use != STD_ICON)) ||
		   (!some_appts && (icon_in_use != NA_ICON))) {
			icon_in_use = some_appts ? STD_ICON : NA_ICON;
			set_icon();
		}
		/* clean out the ~/.msgfile file */
		if (beep && ((msgf = fopen(msgfile, "w")) != NULL)) {
			fprintf(msgf, "I'm out running around.");
			fclose(msgf);
		}
#endif
	} else {
 		/* notify the user via the console (once) ... */
#ifdef CALENCHECK
		if (sno > echoed_sno) {
			echoed_sno = sno;
			/* get all valid appts at this time */
			for (aptr=slots[sno].first; aptr; aptr=aptr->next)
				if (!chk_deleted(&slots[sno], aptr)) {
					if (getenv("WINDOW_PARENT") != NULL
					    && (console = fopen("/dev/console", "w")) != NULL) {
						fprintf(console, "<< %s >> %s\n",
						    progname, aptr->str);
						fclose(console);
					} else {
						fprintf(stderr, "\007\007<< %s >> %s\n",
						    progname, aptr->str);
					}
				}
		}
#else
		if ((beep || beep_open) && sno > echoed_sno) {
			echoed_sno = sno;
			window_bell(frame);
			if (beep_open) {
				/* open on today */
				olddate = Saveday = today;
				mainsw_state = DISPLAYING_DAY;
				xv_set(frame, FRAME_CLOSED, FALSE, 0);
				draw_day();
			}
			if (beep)
				if ((console = fopen("/dev/console", "w")) != NULL) {
					/* get all valid appts at this time */
					for (aptr=slots[sno].first; aptr;
					    aptr=aptr->next)
						if (!chk_deleted(&slots[sno], aptr))
							fprintf(console, "<< %s >> %s\n", progname, aptr->str);
					fclose(console);
				}
			/*
			 * also put a copy in ~/.msgfile, in case
			 * xnlock(1) is running
			 */
			if ((msgf = fopen(msgfile, "w")) != NULL) {
				fprintf(msgf, "%s", slots[sno].cur_appt->str);
				fclose(msgf);
			}
		}
 		/* ... and change the icon */
		if (icon_in_use != REV_ICON) {
			icon_in_use = REV_ICON;
			set_icon();
		}
#endif
	}
#ifdef CALENCHECK
	if (new_day) {
		Saveday = today;
		new_day = 0;
	}
#else
	if (new_day && (int)xv_get(frame, FRAME_CLOSED)) {
		/* update times so that it opens on today */
		closedate = olddate = Saveday = today;
		read_file++;
		/* update date field of the icons */
		set_icon();
		new_day = 0;
		show_future = 1;	/* show future appts again */
	}
#endif
#ifndef CALENCHECK
	if (read_file) {
		current = Saveday;
		err2console(TRUE);
		(void)get_day_appts();
		err2console(FALSE);
		day_is_open = TRUE;
	}
	if (locked == 2)
		unlock_cursors();
#endif
}

int do_files(window_prompt)
int window_prompt;
{
	char *slash, *default_ptr, *envptr;
	char buff[128];
	int to_slash, fd, errflag;
#ifndef CALENCHECK
	int numask;
	struct passwd *pw;
	struct stat statbuf;
#endif
	FILE *appts;

	/* the tmp file */
	sprintf(tmpapts_pathname, "/tmp/appts%d", (int)getpid());
	if (otherfile) {
		strcpy(apts_pathname, othername);
		if ((slash = rindex(apts_pathname, '/')) != NULL) {
			to_slash = slash - apts_pathname;
			strncpy(apts_dir, apts_pathname, to_slash);
			apts_dir[to_slash] = '\0';
		} else {
			strcpy(apts_dir, ".");
		}
	} else {
#ifndef NO_DEFAULTS
		sprintf(buff, "%s.appts", progname);
		if ((default_ptr = defaults_get_string(buff,
		    "Calentool.appts", NULL)) != NULL) {
			if ((slash = rindex(default_ptr, '/')) != NULL) {
				to_slash = slash - default_ptr;
				strncpy(apts_dir, default_ptr, to_slash);
				apts_dir[to_slash] = '\0';
			} else {
				strcpy(apts_dir, ".");
			}
		} else
#endif
		if ((envptr = getenv("CALENTOOL_DIR")) != NULL) {   
			strcpy(apts_dir, envptr);
		} else if ((envptr = getenv("XCALENTOOL_DIR")) != NULL) {   
			strcpy(apts_dir, envptr);
#ifndef CALENCHECK
		} else if (mailto) {
			if ((pw = getpwnam(mailto)) == NULL)
				/* no entry */
				exit(1);
			envptr = pw->pw_dir; /* home directory */
			strcpy(apts_dir, envptr);
#endif
		} else if ((envptr = getenv("HOME")) != NULL) {   
			strcpy(apts_dir, envptr);
		} else {   
			apts_dir[0] = '\0';
		}
		if (*apts_dir) {
			/* prepend directory on pathnames */
			sprintf(apts_pathname, "%s/.appointments", apts_dir);
		} else {
			/* use current directory */
			strcpy(apts_pathname, ".appointments");
		}
	}
	
	/* directory for date/event data files */
#ifndef NO_DEFAULTS
	sprintf(buff, "%s.libdir", progname);
	if ((default_ptr = defaults_get_string(buff,
	    "Calentool.libdir", NULL)) != NULL)
		strcpy(lib_dir, default_ptr);
	else
#endif
		strcpy(lib_dir, DATELIB_DIR);

	errflag = 0;
	if (access(apts_pathname, R_OK) == -1) {
#ifndef CALENCHECK
		if (window_prompt) {
			fprintf(stderr, "nonexistant file\n");
			sprintf(buff, "Cannot access calendar file %s -", apts_pathname);
			if (notice_prompt(frame, NULL,
				NOTICE_MESSAGE_STRINGS,
					buff,
					"Create new .appointments file?",
					0,
				NOTICE_BUTTON_YES, "Create",
				NOTICE_BUTTON_NO,  "Abort ",
				0) == NOTICE_YES) {
				/* try to create the file */
				fprintf(stderr, "creating file\n");
				if ((fd=open(apts_pathname, O_CREAT|O_RDWR, 0644)) <= 0) {
					perror(apts_pathname);
					errflag = 1;
					fprintf(stderr, "..error\n");
				} else {
					if (write(fd, HEADER, sizeof(HEADER)) != sizeof(HEADER)) {
						perror("writing header");
						errflag = 1;
					}
					close(fd);
					fprintf(stderr, "..wrote header\n");
					one_based = 1;
				}
			} else {
				return(1);
			}
		} else {
#endif
			fprintf(stderr, "Cannot access calendar file %s - create? ", apts_pathname);
			fgets(buff, 80, stdin);
			if (buff[0] == 'y' || buff[0] == 'Y') {
				if ((fd=open(apts_pathname, O_CREAT|O_RDWR, 0644)) <= 0) {
					perror(apts_pathname);
					return(1);
				} else {
					if (write(fd, HEADER, sizeof(HEADER)) != sizeof(HEADER)) {
						perror("writing header");
						close(fd);
						return(1);
					}
					close(fd);
					one_based = 1;
				}
			} else
				return(1);
		}
#ifndef CALENCHECK
	}
	if (!read_only)
		if (access(apts_pathname, W_OK) == -1)
			read_only = 1;
	/*
	 * set permissions on tmp file based on .appointments file
	 * with the expception that we need at least write permission
	 * for the owner.
	 */
	(void)stat(apts_pathname, &statbuf);
	numask = ~statbuf.st_mode & 0077;  /* yes, this is octal 77 */
	(void)umask(numask);

	/* update base frame label, if the tool is running */
	if (frame) {
		strcpy(t_title, apts_pathname);
		if (read_only)
			strcat(t_title, " [Read Only]");
		xv_set(frame, FRAME_LEFT_FOOTER, t_title,
			FRAME_SHOW_FOOTER, TRUE,
			0);
	}
#endif

	/* check first line of appts file to see if it is the new style */
	if ((appts = fopen(apts_pathname, "r")) != NULL) {
		fgets(buff, 80, appts);
		fclose(appts);
		if (!strcmp(buff, OHEADER) || !strncmp(buff, HEADER, 18)) {
			version2 = 1;
			one_based = 1;
		}
#ifdef CALENCHECK
		else
			err_rpt("wrong version appointments file format", FATAL);
#endif
	}

#ifndef CALENCHECK
	/* Convert from old format to new. There may some appts files
	 * that are one-based, but are still old style. These are
	 * also handled. (Note: for old-style one-based appts files
	 * the user MUST start calentool with the -z flag.)
	 */
	 if (!version2)
		ver1to2();
#endif
	
	return(0);
}

