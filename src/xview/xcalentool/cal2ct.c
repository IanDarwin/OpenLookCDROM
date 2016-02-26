/*
 * $Id: cal2ct.c,v 2.3 1994/08/29 17:32:54 billr Exp $
 */
/*
 * cal2ct - convert calendar reminder files to calentool style files
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

#include <stdio.h>
#include <ctype.h>
#include <sys/time.h>
#include "ct.h"

struct appt_entry appts, *aptr;
char filename[128], *file, *ofile;
FILE *fp;
struct tm current, start, today, *localtime();
struct timeval tp;
int run_days = -1, use_date = 0;
int day_first = FALSE;
void err_rpt();

extern char *getenv();
extern int optind;
extern char *optarg;
#ifdef __STDC__
int read_cal_file(void);
int write_ct_file(void);
int parse_cal_date(char *str);
void fix_current_day(void);
int monthlength(int month);
int ymd_compare(struct tm day0, struct tm day1);
int parse_date(char *str, int cmdline);
void err_rpt(char *str, int flag);
#ifdef NEED_DYSIZE
int dysize(int y);
#endif
#else
int read_cal_file();
int write_ct_file();
int parse_cal_date();
void fix_current_day();
int monthlength();
int ymd_compare();
int parse_date();
void err_rpt();
#ifdef NEED_DYSIZE
int dysize();
#endif
#endif

void
main(argc, argv)
int argc;
char *argv[];
{
	int flag;

#ifdef SVR4
	gettimeofday(&tp);
#else
	gettimeofday(&tp, NULL);
#endif
	current = today = *localtime(&tp.tv_sec);
	ofile = NULL;
	while ((flag = getopt(argc, argv, "d:f:r:eE")) != EOF)
		switch (flag) {
			case 'd':  /* starting date */
				/* updates "current" */
				(void)parse_date(optarg, TRUE);
				use_date++;
				break;
			case 'e':
			case 'E':  /* European style dates */
				day_first = 1;
				break;
			case 'f':  /* output file */
				ofile = optarg;
				break;
			case 'r':  /* number of days to process */
				run_days = atoi(optarg);
				break;
			default:  /* unknown option */
				fprintf(stderr, "usage: cal2ct [-d date] [-r days] [-e] [-f outfile] [file]\n");
				break;
		}

	start = current;
	if (optind < argc)
		file = argv[optind];
	else {
		strcpy(filename, getenv("HOME"));
		strcat(filename, "/calendar");
		file = filename;
	}

	if ((fp = fopen(file, "r")) == NULL) {
		fprintf(stderr, "can't open calendar file for reading\n");
		exit(1);
	}
	if (!read_cal_file()) {
		fprintf(stderr, "no reminders read from %s\n", file);
		exit(1);
	}
	fclose(fp);
	if (ofile)
		strcpy(filename, ofile);
	else {
		strcpy(filename, getenv("HOME"));
		strcat(filename, "/.appointments");
	}
	if ((fp = fopen(filename, "w")) == NULL) {
		fprintf(stderr, "can't open appointments file for writing\n");
		exit(1);
	}
	write_ct_file();
}

/*
 * read dates from calendar file and stuff into appts struct
 */
int read_cal_file()
{
	char *fgets();
	char buf[512];
	struct appt_entry *optr;

	aptr = &appts;
	optr = aptr;
	while (fgets(buf, 512, fp) != NULL) {
		aptr->repeat = aptr->lookahead = 0;
		aptr->warn = 10;
		aptr->flags = A_NOTE;
		aptr->next = NULL;
		if (parse_cal_date(buf))
			continue;
		aptr->next = (struct appt_entry *)malloc(sizeof(struct appt_entry));
		if (aptr->next == NULL) {
			fprintf(stderr, "out of memory\n");
			return(0);
		}
		optr = aptr;
		aptr = aptr->next;
	}
	if (aptr == &appts)
		return(0);	/* nothing read */
	/* don't need the last one */
	free(aptr);
	optr->next = NULL;
	return(1);
}

/*
 * write out the new .appointments file
 */
int write_ct_file()
{
	aptr = &appts;
	fputs(HEADER, fp);
	while (aptr) {
		if (put_aentry(fp, aptr)) {
			fprintf(stderr, "error writing appointments file\n");
			return(0);
		}
		aptr = aptr->next;
	}
	return(1);
}

char *dayname[7] = {"SU", "MO", "TU", "WE", "TH", "FR", "SA"};
char *monthname[12] = {"JAN", "FEB", "MAR", "APR", "MAY", "JUN",
	"JUL", "AUG", "SEP", "OCT", "NOV", "DEC"};

/*
 * parse the date in the buffer and reset the "current"
 * date to reflect that date. The date may take the form of a
 * month and day where the month may be spelled or numeric, e.g.:
 * Feb 27, feb 27, 2/27. The month may be a `*' to refer to any
 * month, e.g. "* 27" or "* /27".
 * Limitation: the date is expected to start at the begining of
 * the line. (calendar allows it anywhere in the line.)
 */
int parse_cal_date(str)
char *str;
{
	char c[4];
	int i, m = -1, d = -1;
	void fix_current_day();

	current = start;	/* start with this month, day, year */
	while (isspace(*str))
		++str;

	if (isdigit(*str)) {
		/* must be a m/d date */
		/* assume it's a month first */
		m = *str++ - '0';
		if (isdigit(*str))
			m = m*10 + *str++ - '0';
		--m; /* make it zero based */
		if (*str != '/') {
			/* no more chars => bad format */
			fprintf(stderr, "badly formed date: %s\n", str-2);
			return(1);
		} else {
			++str;
			if (isdigit(*str)) {
				d = *str++ - '0';
				while (isdigit(*str))
					d = d*10 + *str++ - '0';
			} else {
				fprintf(stderr, "badly formed date: %s\n", str-2);
				return(1);
			}
		}
	} else if (*str == '*') {
		aptr->flags |= ALL_MONTHS;
		++str;
		while (isspace(*str) || *str == '/')
			++str;
		d = *str++ - '0';
		while (isdigit(*str))
			d = d*10 + *str++ - '0';
	} else {
		/* month name */
		c[0] = islower(*str) ? toupper(*str) : *str;
		++str;
		c[1] = islower(*str) ? toupper(*str) : *str;
		if (*++str) {
			c[2] = islower(*str) ? toupper(*str) : *str;
			c[3] = '\0';
		} else
			c[2] = '\0';
		while (!isspace(*str))
			++str;
		/* check month names */
		for (i=0; i<12; i++) {
			if (!strcmp(c, monthname[i])) {
				m = i;
				break;;
			}
		}
		if (m >= 0) {
			/* match found */
			while (!isspace(*str))
				++str;
			d = *++str - '0';
			++str;
			while (isdigit(*str))
				d = d*10 + *str++ - '0';
		} else {
			fprintf(stderr, "badly formed date: %s\n", str-2);
			return(1);
		}
	}
	current.tm_mon = m;
	current.tm_mday = d;
	if (use_date || run_days >= 0) {
		if (!run_days) {
			if (ymd_compare(current, start) != 0)
				return(1);
		} else if (run_days > 0) {
			if (ymd_compare(current, start) >= 0) {
				struct tm Save;

				Save = current;
				current = start;
				current.tm_mday += run_days;
				fix_current_day();
				if (ymd_compare(Save, current) > 0)
					return(1);
				current = Save;
			} else
				return(1);
		} else if (ymd_compare(current, start) < 0)
			return(1);
	}
	while (isspace(*str))
		++str;
	strcpy(aptr->str, str);
	aptr->year = current.tm_year;
	aptr->month = current.tm_mon;
	aptr->day = current.tm_mday;
	return(0);
}

/*
 *	Reset some values in current tm structure. Year, month and
 *	day-of-month are valid but day and/or month may be < 0 or
 *	greater than the maximum value, in which case they are adjusted
 *	accordingly. Day-of-year and day-of-week are then recalculated.
 */
void
fix_current_day()
{
	int month, totdays = 0;
	struct tm from, to;

	if (current.tm_mon < JAN) {
		current.tm_mon = DEC;
		current.tm_year--;
	} else if (current.tm_mon > DEC) {
		current.tm_mon = JAN;
		current.tm_year++;
	}
	if (current.tm_mday < 1) {
		current.tm_mon--;
		if (current.tm_mon < JAN) {
			current.tm_mon = DEC;
			current.tm_year--;
		}
		current.tm_mday += monthlength(current.tm_mon);
	} else if (current.tm_mday > monthlength(current.tm_mon)) {
		current.tm_mday -= monthlength(current.tm_mon);
		current.tm_mon++;
		if (current.tm_mon > DEC) {
			current.tm_mon = JAN;
			current.tm_year++;
		}
	}
	current.tm_yday = current.tm_mday - 1;
	for (month = 0; month < current.tm_mon; month++) {
		current.tm_yday += monthlength(month);
	}
	if ((current.tm_year < today.tm_year)
		|| ((current.tm_year == today.tm_year)
		&& (current.tm_yday < today.tm_yday))) {
		from = current;
		to = today;
	} else {
		from = today;
		to = current;
	}
	if (from.tm_year != to.tm_year) {
		for (totdays = 0; from.tm_year < to.tm_year; from.tm_year++)
			totdays += dysize(from.tm_year + 1900);
	}
	totdays += to.tm_yday - from.tm_yday;
	if ((current.tm_year < today.tm_year)
		|| ((current.tm_year == today.tm_year)
		&& (current.tm_yday < today.tm_yday)))
		totdays = -totdays;
	current.tm_wday =
		((totdays % 7) + 7 + today.tm_wday) % 7;
}

int
monthlength(month)
int	month;
{
	static int	monthlengths[] = {31,28,31,30,31,30,31,31,30,31,30,31};

	if (month == FEB && (dysize(current.tm_year + 1900) == 366))
		return(29);
	else
		return(monthlengths[month]);
}

/*
 * Compares two sets of year/month/day.  Returns -1 if the first is earlier than
 * the second, +1 if later, 0 if they are the same.
 */
int
ymd_compare(day0, day1)
struct tm day0, day1;
{
        if (day0.tm_year > day1.tm_year) return(1);
        if (day0.tm_year < day1.tm_year) return(-1);
        if (day0.tm_mon > day1.tm_mon) return(1);
        if (day0.tm_mon < day1.tm_mon) return(-1);
        if (day0.tm_mday > day1.tm_mday) return(1);
        if (day0.tm_mday < day1.tm_mday) return(-1);
        return(0);
}

/*
 * parse the date on the given string and reset the "current"
 * date to reflect that date. The date may take the form of a
 * day name (e.g. Tu, Tue, Tuesday) or a date in m/d/y format
 * where the month and/or year may be missing (e.g. 27 = 27th
 * of this month, 8/27 = August 27 of this year, 8/27/89 =
 * August 27 of 1989. If 'cmdline' is true, then the string
 * came from the command line '-d' option.
 * If the first character of the date is + or - scan the number and
 * use it as an offset in days from the current date.  Thus -1 becomes
 * yesterday and +1 becomes tomorrow. pbm.
 */
int
parse_date(str, cmdline)
char *str;
int cmdline;
{
	char c[4];
	int i, dow = -1, m = -1, d = -1, y = -1;

	if (isdigit(*str)) {
		/* must be a m/d/y date */
		/* assume it's a month first */
		m = *str++ - '0';
		if (isdigit(*str))
			m = m*10 + *str++ - '0';
		if (!*str) {
			/* no more chars => day only */
			d = m;
			m = -1;
		} else if (*str++ != '/') {
			if (cmdline)
				err_rpt("badly formed date for -d option (ignored)", NON_FATAL);
			else
				err_rpt("badly formed date - please reenter", NON_FATAL);
			return(1);
		} else {
			d = *str++ - '0';
			if (isdigit(*str))
				d = d*10 + *str++ - '0';
			if (*str++ == '/') {
				/* year also specified */
				y = *str++ - '0';
				if (isdigit(*str)) {
					y = y*10 + *str++ - '0';
					if (*str && isdigit(*str))
						y = y*10 + *str++ - '0';
					if (*str && isdigit(*str))
						y = y*10 + *str++ - '0';
				}
			}
		}
		if (y > 0) {
			if (y > 1900)
				y -= 1900;
			current.tm_year = y;
		}
		if (day_first) {
			if (m > 0) {
				current.tm_mon = d - 1;
				current.tm_mday = m;
			} else if (d > 0)
				current.tm_mday = d;
		} else {
			if (m > 0) {
				current.tm_mon = m - 1;
				current.tm_mday = d;
			} else if (d > 0)
				current.tm_mday = d;
		}
		fix_current_day();
	} else if (*str == '-' || *str == '+') {
		/*
		 * If the argument begins with a + or - assume that it is an
		 * offset in days from the current date. Use current date if the
		 * number doesn't scan after the - or +. pbm
		 */
		if (sscanf(str, "%d", &i) == 1) {
			current.tm_mday += i;
			fix_current_day();
		}
	} else {
		/* day of week */
		/* check for day names */
		c[0] = islower(*str) ? toupper(*str) : *str;
		++str;
		c[1] = islower(*str) ? toupper(*str) : *str;
		if (*++str) {
			c[2] = islower(*str) ? toupper(*str) : *str;
			c[3] = '\0';
		} else
			c[2] = '\0';
		for (i=0; i<7; i++) {
			if (!strncmp(c, dayname[i], 2)) {
				dow = i;
				break;
			}
		}
		if (dow >= 0) {
			/* match found */
			current.tm_mday += dow - current.tm_wday;
			fix_current_day();
		} else if (!strcmp(c, "TOM")) {
			/* tommorrow */
			current.tm_mday++;
			fix_current_day();
		} else if (!strcmp(c, "YES")) {
			/* yesterday */
			current.tm_mday--;
			fix_current_day();
		} else if (strcmp(c, "TOD")) {
			if (cmdline)
				err_rpt("badly formed date for -d option (ignored)", NON_FATAL);
			else
				err_rpt("badly formed date - please reenter", NON_FATAL);
			return(1);
		}
	}
	return(0);
}

void
err_rpt(str, flag)
char *str;
int flag;
{
	fprintf(stderr, "%s\n", str);
	if (flag == FATAL)
		exit(1);
}

#ifdef NEED_DYSIZE
int
dysize(y)
int y;
{
    
	if (y % 4 == 0 &&
	    y % 100 != 0 || y % 400 == 0)
		return(366);		/* ----------> */
	else
		return(365);		/* ----------> */
    
}
#endif /* NEED_DYSIZE */
