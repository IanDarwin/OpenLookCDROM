/*
 * $Id: expire.c,v 2.3 1994/08/19 19:53:08 billr Exp $
 */
/*
 * expire.c -
 * expire outdated appts, i.e. remove them from the appts file, if they
 * are older than <n> days and store them in .appointmentYY if <save_old>.
 *
 * calentool - day/week/month/year-at-a-glance calendar for XView/Open Look
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
#include <sys/time.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/errno.h>
#include "ct.h"

extern struct tm today,current;
extern char apts_pathname[], tmpapts_pathname[];
extern char inbuf[], apts_dir[];
extern int save_old,read_only;
extern int errno;
extern double julian_day();
extern double nth_mday_of_month();

/*
 * Scan appointments file for outdated appointments. If <save_old> is
 * TRUE then save them to a special file of the form ".appointments.YY",
 * where YY is the year of that appointment.  Outdated appointments are
 * appointments that are older that <edays> old.  If <edays> is zero then
 * Outdated appointments are appointments from previous years. If <save_old>
 * is not set, then Outdated appointments are just deleted.
 * Setting <save_old> TRUE (-o) and <edays> to 0 (-x 0) gives the behavior
 * described by the "-o" switch.
 */
void
expire(edays)
int edays;
{
	FILE *apts, *temp_apts, *fp;
	int read_stat;
	int runl, week = 0;
	char save_file[128];
	struct appt_entry appt;
	struct stat sbuf;
	struct tm savecurrent;
	int successful = 1;    /* assume this worked */

	/*
	 * method: for each regular appt in the file (or limited
	 * duration appt), compare the Julian date of that appt
	 * and the Julian date of today, looking for a difference
	 * greater than <edays>.
	 */
	if (read_only) /* we can't expire from a calendar if read_only */
		return;
	if ((apts = fopen(apts_pathname, "r")) == NULL) {
		successful = 0;
		err_rpt("can't open appointments file, aborting expire", NON_FATAL);
	}
	if ((temp_apts = fopen(tmpapts_pathname, "w")) == NULL) {
		successful = 0;
		err_rpt("can't open temp file for writing, aborting expire", NON_FATAL);
	}
	/*
	 * now go thru the appointments file
	 */
	savecurrent = current;  /* save so can restore after loop */
	while (successful && (read_stat=get_aentry(apts, &appt, TRUE, 0, 0)) != EOF) {
		if (read_stat)
			continue;	/* read error (ignore) */
		if (appt.flags & A_COMMENT) {
			fputs(inbuf, temp_apts);
			continue;
		}
		current.tm_year = appt.year;
		current.tm_mon = appt.month;
		current.tm_mday = appt.day;
		if (appt.flags & ALL_YEARS)
			/* force this to be saved */
			current.tm_year = today.tm_year + 1;
		if (appt.flags & ALL_MONTHS)
			/* maybe saved, pick worse case */
			current.tm_mon = DEC;
		if (appt.flags & ALL_DAYS || appt.flags & EVERY_MON_FRI) {
			if (current.tm_year < today.tm_year ||
			   (current.tm_year == today.tm_year &&
			    current.tm_mon < today.tm_mon)) {
				/* maybe saved, pick worse case */
				current.tm_mday = monthlength(current.tm_mon);
				fix_current_day();
				if (appt.flags & EVERY_MON_FRI)
					while (current.tm_wday == SAT
					    || current.tm_wday == SUN) {
						current.tm_mday--;
						fix_current_day();
					}
			}
		}
		if (appt.flags & EVERY_SOMEDAY) {
			if ((appt.repeat & ALL_WEEKS) == ALL_WEEKS || appt.repeat & LAST_WEEK ||
			    appt.repeat & WEEK5)
				week = 5;
			else if (appt.repeat & WEEK4)
				week = 4;
			else if (appt.repeat & WEEK3)
				week = 3;
			else if (appt.repeat & WEEK2)
				week = 2;
			else if (appt.repeat & WEEK1)
				week = 1;
			current.tm_mday = (int)nth_mday_of_month(week, Pickday(appt.flags), current.tm_mon, current.tm_year+1900);
			if (current.tm_mday > monthlength(current.tm_mon))
				current.tm_mday = (int)nth_mday_of_month(week-1, Pickday(appt.flags), current.tm_mon, current.tm_year+1900);
			if (appt.flags & RUN) {
				current.tm_mday += appt.runlength;
				fix_current_day();
			}
		} else if (appt.flags & REPEAT) {
			if (appt.flags & RUN)
				runl = appt.runlength;
			else
				runl = 1;
			while (ymd_compare(current, today) < 0 && runl) {
				if (appt.flags & RUN)
					--runl;
				if (runl) {
					current.tm_mday += appt.repeat;
					fix_current_day();
				}
			}
		}
		current.tm_mday += edays;  /* offset by expire days */
		fix_current_day();
		if (((edays == 0) && (current.tm_year >= today.tm_year)) ||
		    julian_day((double)current.tm_mday, current.tm_mon, current.tm_year+1900) >=
		    julian_day((double)today.tm_mday, today.tm_mon, today.tm_year+1900)) {
			if (put_aentry(temp_apts, &appt)) {
				/* write error */
				break;
			}
		} else {
			if (save_old) {
				/* prepend directory info */
				sprintf(save_file, "%s.%02d",
					apts_pathname, appt.year);
				if (stat(save_file, &sbuf) && errno == ENOENT) {
					/* new file*/
					if ((fp = fopen(save_file, "w")) == NULL) {
						successful = 0;
						err_rpt("can't open save file, aborting expire", NON_FATAL);
					}
					fputs(HEADER, fp);
					fclose(fp);
				}
				if ((fp = fopen(save_file, "a+")) == NULL) {
					successful = 0;
					err_rpt("can't open save file, aborting expire", NON_FATAL);
				} else {
					if (put_aentry(fp, &appt)) {
						successful = 0;
						err_rpt("write to save appt file failed, aborting expire", NON_FATAL);
					}
					fclose(fp);
				}
			}
		}
        }
	if (ferror(temp_apts)) {
		successful = 0;
		err_rpt("write on temp file failed", NON_FATAL);
	}
	fclose(temp_apts);
        fclose(apts);
	current = savecurrent;   /* restore current from temp */
	/* don't rename zero length files */
	stat(tmpapts_pathname, &sbuf);
	if (sbuf.st_size == (off_t) 0)
		err_rpt("zero length temp file - not renamed", NON_FATAL);
	else if (successful)
		xrename(tmpapts_pathname, apts_pathname);
}

