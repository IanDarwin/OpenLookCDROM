/*
 * $Id: calencheck.c,v 2.3 1994/08/19 19:53:08 billr Exp $
 *
 * calencheck.c - check for pending appts without the overhead
 *		  of the full blown calentool
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

/*
 * define NO_DEFAULTS if you don't need info looked up in the .defaults
 * file (saves memory by not linking in pixrect lib with SunOs > 4.0).
 * This is probably only useful for SunView.
 */
#include <stdio.h>
#include <sys/time.h>
#include <sys/file.h>
#ifndef NO_DEFAULTS
#include <xview/defaults.h>
#endif
#include "ct.h"

int read_only = 1;		/* no modifications allowed */
int n_tslots, otherfile = 0;
int include_old = 0, save_old = 0;
int new_entry = 0;
int start_hour, end_hour, n_slots;
struct tm current, today, closedate;
struct tm First, Last;
char *progname, *othername;
struct dayslot *slots;
int show_future = 1;
int one_based = 0, version2 = 0;

void
main(argc,argv)
int argc;
char *argv[];
{
	int flag;
	char buff[128];
	extern char *optarg;
	char *rindex();
	
	if ((progname = rindex(*argv, '/')))
		progname++;
	else
		progname = *argv;

#ifdef NO_DEFAULTS
	start_hour = START_HOUR;
	end_hour = END_HOUR;
#else
	sprintf(buff, "%s.startHour", progname);
	start_hour = defaults_get_integer_check(buff, "Calentool.startHour", START_HOUR, 0, 23);
	sprintf(buff, "%s.endHour", progname);
	end_hour = defaults_get_integer_check(buff, "Calentool.endHour", END_HOUR, start_hour+1, 23);
#endif

	get_today();	/* initial day is today */
	current = today;
	
	while ((flag = getopt(argc, argv, "f:S:s:")) != EOF)
		switch (flag) {
		    case 'f':   /* use this file */
			otherfile = 1;
			othername = optarg;
			break;

		    case 's':
			start_hour = atoi(optarg);
			if (start_hour > 23 || start_hour < 0)
				printf("start hour must be in range 0 - 23\n");
			break;

		    case 'S':
			end_hour = atoi(optarg);
			if (end_hour > 24 || end_hour < 1)
				printf("end hour must be in range 1 - 24\n");
			break;

		    case '?':
		    default:
			fprintf(stderr, "usage: %s [-f <appt_file>]", progname);
			fprintf(stderr, " [-s <starthour>]");
			fprintf(stderr, " [-S <endhour>]\n");
			exit(1);
		}

	err2console(TRUE);

	/*
	** setup number of slots and allocate memory for them
	*/
	if ( start_hour >= end_hour )
		err_rpt("Start Hour must be less than Stop Hour", FATAL);
	n_tslots = (end_hour - start_hour) * 2;
	n_slots = n_tslots + N_NOTESLOTS;
	/* make room for n_slot dayslot entries and week entries */
	if ((slots = (struct dayslot *)malloc(n_slots* sizeof(struct dayslot))) == NULL)
		err_rpt("Can't get enough storage for day slots", FATAL);

	do_files(FALSE);
	closedate = today;
	check_calendar();
	for (;;) {
		/* only check appointments every TIME_OUT minutes */
		sleep(TIME_OUT*60);
		check_calendar();
	}
}

/* some stubs required by routines in common.c not used by calencheck */
void next_appt(i, j)
int i, j;
{
}

void draw_day_appts()
{
}

void deactivate_slot(i, j)
int i, j;
{
}

int
put_aentry(f, a)
FILE *f;
struct appt_entry *a;
{
	return 0;
}

void working(i)
int i;
{
}
