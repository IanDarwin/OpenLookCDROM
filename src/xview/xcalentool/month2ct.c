/*
 * $Id: month2ct.c,v 2.3 1994/08/19 19:53:08 billr Exp $
 */
/*
 * month2ct - convert month schedule files to calentool style files
 *
 * Copyright 1989, 1991, 1994 by Tektronix, Inc. - All Rights Reserved.
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

#include "month.h"
#include "ct.h"

struct appt_entry appt;
struct event_rec events;
FILE *fp;
char filename[128];
char *dir;

#ifdef __STDC__
void write_ct_file (void);
#else
void write_ct_file ();
#endif

void
main(argc, argv)
int argc;
char *argv[];
{
	if (argc > 1)
		dir = argv[1];
	else {
		strcpy(filename, getenv("HOME"));
		dir = filename;
	}

	if (read_schedule(dir, READ_ONLY)) {
		fprintf(stderr, "no reminders read from %s/.month\n", dir);
		exit(1);
	}
	strcpy(filename, getenv("HOME"));
	strcat(filename, "/.appointments");
	if ((fp = fopen(filename, "w")) == NULL) {
		fprintf(stderr, "can't open .appointments file for writing\n");
		exit(1);
	}
	write_ct_file();
}

/*
 * write out the new .appointments file
 */
void
write_ct_file()
{
	struct event_rec *evt;
	int length, oflags, i;

	evt = &events;
	fputs(HEADER, fp);
	/* first event is empty */
	evt = evt->next_event;
	while (evt) {
		/* DEBUG
		fprintf(stderr,"evt struct:\n  mly=%d, yly=%d, evry=%d\n  nth=%d, last=%d, nthon=%d\n  str=%s\n",
			evt->monthly, evt->yearly, evt->every, evt->nth, evt->last, evt->nth_is_on, evt->event_string);
		*/
		appt.flags = appt.repeat = appt.lookahead = 0;
		appt.warn = 10;  /* default */
		appt.year = evt->start_date.year - 1900;
		appt.month = evt->start_date.month - 1;
		appt.day = evt->start_date.day;
		strcpy(appt.str, evt->event_string);
		if (evt->monthly)
			appt.flags |= ALL_MONTHS;
		if (evt->yearly)
			appt.flags |= ALL_YEARS;
		appt.hour = evt->start_time.hour;
		appt.minute = evt->start_time.minute;
		if (appt.hour > 23 || appt.hour < 0 || appt.minute > 59 || appt.minute < 0)
			appt.flags |= A_NOTE;
		if (appt.minute < 15)
			appt.minute = 0;
		else if (appt.minute < 45)
			appt.minute = 30;
		else {
			appt.minute = 0;
			appt.hour++;
		}
		length = evt->duration.hour * 60 + evt->duration.minute;
		appt.arrows = length / 30 - 1;
		if (appt.arrows < 0)
			appt.arrows = 0;
		if (evt->anti)
			appt.flags |= DELETED;
		if (evt->warning.hour >= 24)
			appt.lookahead = evt->warning.hour / 24;
		else
			appt.warn = evt->warning.hour * 60 + evt->warning.minute;
		if (evt->until) {
			fprintf(stderr, "this appointment runs more than 1 day,\n");
			fprintf(stderr, "modify the calentool entry after conversion:\n");
			fprintf(stderr, "start date (m/d/y): %d/%d/%d, ",
			  evt->start_date.month, evt->start_date.day,
			  evt->start_date.year);
			fprintf(stderr, "start time: %d:%d, ", evt->start_time.hour,
			  evt->start_time.minute);
			fprintf(stderr, "runs until (m/d/y): %d/%d/%d\n",
			  evt->until_date.month, evt->until_date.day,
			  evt->until_date.year);
		}
		if (evt->every) {
			/* event occurs on an every something */
			appt.flags |= REPEAT;
			if (evt->last)
				appt.repeat = LAST_WEEK;
			else if (evt->nth_is_on)
				appt.repeat = 1<<(evt->nth_is_on-1);
			else
				appt.repeat = ALL_WEEKS;
			oflags = appt.flags;
			/* check for mon-fri */
			if (!evt->smtwtfs[0] && !evt->smtwtfs[6] &&
			    evt->smtwtfs[1] && evt->smtwtfs[2] &&
			    evt->smtwtfs[3] && evt->smtwtfs[4] &&
			    evt->smtwtfs[5]) {
				appt.flags |= EVERY_MON_FRI;
				if (put_aentry(fp, &appt)) {
					fprintf(stderr,
					  "error writing .appointments file\n");
					return;
				}
			} else
				for (i=0; i<7; i++) {
					if (evt->smtwtfs[i]) {
						appt.flags = oflags | Setday(i);
						if (put_aentry(fp, &appt)) {
							fprintf(stderr,
							  "error writing .appointments file\n");
							return;
						}
					}
				}
		} else
			if (put_aentry(fp, &appt)) {
				fprintf(stderr, "error writing .appointments file\n");
				return;
			}
		fputs("\n", fp);
		evt = evt->next_event;
	}
}
