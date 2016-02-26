/*
 * $Id: utils.c,v 2.3 1994/08/29 17:35:04 billr Exp $
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
/*
 * Modified parse_date to allow +nnn and -nnn syntax for dates relative to the
 * current date.  Peter Marshall <peter.marshall@uwo.ca>. 1989-09-19.
 */
/********************************************
 *					    *
 *              Utility routines.	    *
 *					    *
 ********************************************/



#include "ct.h"
#include <stdio.h>
#ifndef NOTOOL
#include <xview/xview.h>
#include <xview/notice.h>
#endif  /* NOTOOL */
#include <ctype.h>
#include <sys/types.h>
#include <sys/time.h>
#include <sys/file.h>
#include <sys/stat.h>
#include <sys/errno.h>
#ifdef NEED_CUSERID
#include <pwd.h>
#endif


extern struct tm today, current;
extern struct tm First;
extern int day_is_open, read_only;
extern int n_slots;
extern struct dayslot *slots;
extern char apts_pathname[], tmpapts_pathname[];
extern char *progname;
extern int one_based, version2, new_entry;
extern char apts_dir[], lib_dir[];
extern int include_old, save_old, expire_days;
#ifndef NOTOOL
extern Frame frame;
extern int mainsw_state;
extern Frame prompt_frame;
extern int update_interval;
extern int monday_first, hour24, day_first;
#endif  /* NOTOOL */
extern int week_ofs;
extern int errno;

char inbuf[512], strbuf[256], errbuf[128];
char clockstr[32];
static int include_level = 0;
static int log_to_console;
char *daynames[] = {
#ifdef FRENCH
	"Dimanche", "Lundi", "Mardi", "Mercredi",
	"Jeudi", "Vendredi", "Samedi"};
#else
# ifdef SWEDISH
	"Sondag", "Mandag", "Tisdag", "Onsdag",
	"Torsdag", "Fredag", "Lordag"};
# else
	"Sunday","Monday","Tuesday","Wednesday",
	"Thursday","Friday","Saturday"};
# endif /* SWEDISH */
#endif /* FRENCH */
char *monthnames[] = {
#ifdef FRENCH
	"Janvier", "Fevrier", "Mars", "Avril",
	"Mai", "Juin", "Juillet", "Aout",
	"Septembre", "Octobre", "Novembre", "Decembre"};
#else
# ifdef SWEDISH
	"Januari", "Februari", "Mars", "April", "Maj",
	"Juni", "Juli", "Augusti", "September",
	"Oktober", "November", "December"};
# else
	"January","February","March","April",
	"May","June","July","August",
	"September","October","November","December"};
# endif /* SWEDISH */
#endif /* FRENCH */
char *smonthnames[] = {
#ifdef FRENCH
	"Jan", "Fev", "Mar", "Avr",
	"Mai", "Jun", "Jul", "Aou",
	"Sep", "Oct", "Nov", "Dec"};
#else
# ifdef SWEDISH
	"Jan","Feb","Mar","Apr",
	"Maj","Jun","Jul","Aug",
	"Sep","Okt","Nov","Dec"};
# else
	"Jan","Feb","Mar","Apr",
	"May","Jun","Jul","Aug",
	"Sep","Oct","Nov","Dec"};
# endif /* SWEDISH */
#endif /* FRENCH */
char *dayname[8] = {"SU", "MO", "TU", "WE", "TH", "FR", "SA", "MF"};

#ifdef __STDC__
int monthlength (int month);
void err_rpt (char *errstr, int fatal_flag);
int put_aentry (FILE *apts_file, struct appt_entry *appt);
void xrename (char *from, char *to);
int do_wk_repeat (char **ptr);
int do_repeat (char **ptr);
int do_lookahead (char **ptr);
#else
int monthlength ();
void err_rpt ();
int put_aentry ();
void xrename ();
int do_wk_repeat ();
int do_repeat ();
int do_lookahead ();
#endif

/*
 * sets "today" and current time
 */
void
get_today()
{
	struct tm *tm;
	struct timeval tv;
#ifndef CALENCHECK
	char timstr[16];
#endif

#ifdef SVR4
	gettimeofday(&tv);
#else
	gettimeofday(&tv, 0);
#endif
	tm = localtime(&tv.tv_sec);

	today = *tm;

#ifndef CALENCHECK
	if (day_first)
		sprintf(clockstr, "%3.3s %d %s %d, ", daynames[today.tm_wday],
		    today.tm_mday, smonthnames[today.tm_mon], today.tm_year+1900);
	else
		sprintf(clockstr, "%3.3s %s %d %d, ", daynames[today.tm_wday],
		    smonthnames[today.tm_mon], today.tm_mday, today.tm_year+1900);
	if (update_interval >= 60)
		sprintf(timstr, "%02d:%02d", today.tm_hour, today.tm_min);
	else
		sprintf(timstr, "%02d:%02d:%02d", today.tm_hour, today.tm_min, today.tm_sec);
	if (!hour24) {
		/* display am/pm for 12-hour time */
		if (today.tm_hour > 12) {
			strcat(timstr, "pm");
			timstr[0] = ((today.tm_hour - 12) / 10) + '0';
			timstr[1] = ((today.tm_hour - 12) % 10) + '0';
		} else if (today.tm_hour == 12) {
			strcat(timstr, "pm");
		} else {
			strcat(timstr, "am");
		}
		if (timstr[0] == '0')
			timstr[0] = ' ';
	}
	strcat(clockstr, timstr);
#endif  /* CALENCHECK */
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
	current.tm_yday = day_of_year((double)current.tm_mday, current.tm_mon+1, current.tm_year+1900) - 1;
	current.tm_wday = get_day_of_week((double)current.tm_mday, current.tm_mon+1, current.tm_year+1900);
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
 * Compares two sets of year/month/day.  Returns -1 if the first is earlier than
 * the second, +1 if later, 0 if they are the same.  Similar to
 * ymd_compare() only compares given date to date of an appt entry.
 */
int
ymd2_compare(day0, aday)
struct tm *day0;
struct appt_entry *aday;
{
        if (day0->tm_year > aday->year) return(1);
        if (day0->tm_year < aday->year) return(-1);
        if (day0->tm_mon > aday->month) return(1);
        if (day0->tm_mon < aday->month) return(-1);
        if (day0->tm_mday > aday->day) return(1);
        if (day0->tm_mday < aday->day) return(-1);
        return(0);
}

int
monthlength(month)
int	month;
{
	static int	monthlengths[] = {31,28,31,30,31,30,31,31,30,31,30,31};

	if (month == FEB && (length_of_year(current.tm_year + 1900) == 366))
		return(29);
	else
		return(monthlengths[month]);
}

/*
 *
 * Append data from active timeslots to end of "tmp.appointments"
 * file, then copy "tmp.appointments" to "appointments".  Note that
 * when we opened the current day we filtered "appointments":
 * all items that applied to the current day were displayed and
 * stored in slots; all others were copied to "tmp.appointments".
 * So by now "tmp.appointments" contains no entries for the
 * current day.
 * As an optimization, if nothing changed in the day then the
 * original appointments file is left unchanged.
 *
 */

int
close_day()
{
#ifndef CALENCHECK
        int i;
        FILE *f;
	struct stat sbuf;
	struct appt_entry *aptr, *optr;
#endif

	if (read_only || !new_entry) {
		new_entry = 0;
		day_is_open = FALSE;
		return(0);
	}

#ifndef CALENCHECK
	f = fopen(tmpapts_pathname, "a+");
	if (f == NULL) {
		err_rpt("can't open temp file for appending", NON_FATAL);
		day_is_open = FALSE;
		return(1);
	}
	
	for (i=0; i<n_slots; i++) {
                if (slots[i].first != NULL) {
			aptr = slots[i].first;
			if (put_aentry(f, aptr))
				/* write error */
				break;
			optr = aptr;
			while ((aptr = aptr->next)) {
				free(optr);
				if (put_aentry(f, aptr))
					/* write error */
					break;
				optr = aptr;
			}
			free(optr);
		}
        }
	if (ferror(f))
		err_rpt("write on temp file failed", FATAL);
        fclose(f);
	new_entry = 0;
	day_is_open = FALSE;
	/* don't rename zero length files */
	stat(tmpapts_pathname, &sbuf);
	if (sbuf.st_size == (off_t) 0)
		return(1);
	xrename(tmpapts_pathname, apts_pathname);
#endif  /* CALENCHECK */
	return(0);
}

/*
 * get entry from appointments file
 */
int
get_aentry(apts_file, appt, noInclude, noUmkNotes, target)
FILE *apts_file;
struct appt_entry *appt;
int noInclude;
int noUmkNotes;
int target;
{
	char *ptr, *str;
	char *fgets(), *index();
	char *incl_ptr, incl_buf[128], wday[3];
	int i, lib, parse_options, nodata = 1;
	int n, nflag, lrange, hrange;
	struct stat sbuf;
	static FILE *include[MAX_INCLUDE_NESTING];

	appt->flags = appt->repeat = appt->lookahead = 0;
	appt->sindex = 0;
	appt->runlength = 0;
	appt->warn = 10;
	appt->next = NULL;
	/* If noInclude is set then don't follow include files, i.e.
	 * treat #include directives as comments. This is useful for
	 * copying the appts file.
	 */
	while (nodata) {
		if (include_level) {
			if (fgets(inbuf, 512, include[include_level-1]) == NULL) {
				/* end of include file - get next entry
				 * from previous level of nesting
				 */
			    unwind:
				fclose(include[include_level-1]);
				include_level--;
				appt->flags = 0;
				nodata = 1;
			} else {
				nodata = 0;	/* still data in file */
			}
		} else {
			if (fgets(inbuf, 512, apts_file) == NULL)
				return(EOF);
			else
				nodata = 0;	/* still data in file */
		}
	}
	ptr = inbuf;
	if (include_level)
		/* don't modify or copy stuff from include files */
		appt->flags |= READONLY;
	if (noInclude && *ptr == '#') {
		appt->flags |= A_COMMENT;
		return(0);
	}
	if (*ptr == '#') {
		if (!include_level && (!strcmp(inbuf, OHEADER) ||
		    !strncmp(inbuf, HEADER, 18))) {
			/* first line in base file read */
			if (include_old && (First.tm_year <= today.tm_year)) {
				/* read in old include file (if it exists) */
				/* prepend directory info */
				sprintf(incl_buf, "%s.%02d",
					apts_pathname, First.tm_year);
				if (!stat(incl_buf, &sbuf)) {
					if ((include[include_level] = fopen(incl_buf, "r")) == NULL) {
						strcpy(errbuf, "can't open include file <");
						strcat(errbuf, incl_buf);
						strcat(errbuf, "> (ignored)");
						err_rpt(errbuf, NON_FATAL);
					} else
						include_level++;
				}
			}
			if (!strcmp(inbuf, OHEADER))
				/* substitute new header format */
				strcpy(inbuf, HEADER);
		} else if (!strncmp(inbuf, HEADER, 18)) {
			/* found a ver 2.2 header - get statistics */
			n = sscanf(inbuf, "%*[^-]- nflag=%d range=%d,%d",
				&nflag, &lrange, &hrange);
			if (n == 3 && include_level) {
				/* proper match found */
				if (noUmkNotes && nflag)
					goto unwind;
				if (target && (target < lrange || target > hrange))
					goto unwind;
			}
		} else if (!strncmp(inbuf, "#include", 8)) {
			/* include file */
			if (include_level > MAX_INCLUDE_NESTING) {
				err_rpt("include files nested too deep (ignored)", NON_FATAL);
				appt->flags |= A_COMMENT;
				return(0);
			}
			incl_ptr = strbuf;
			if ((ptr = index(inbuf, '"')) == NULL)

				if ((ptr = index(inbuf, '<')) == NULL) {
					err_rpt("missing '\"' or '<' in include file spec", NON_FATAL);
					appt->flags |= A_COMMENT;
					return(0);
				} else {
					lib = 1;
				}
			else
				lib = 0;
			ptr++;
			while (*ptr && *ptr != '"' && *ptr != '>')
				*incl_ptr++ = *ptr++;
			if (! *ptr) {
				err_rpt("missing '\"' or '>' in include file spec", NON_FATAL);
				appt->flags |= A_COMMENT;
				return(0);
			}
			*incl_ptr = '\0';
			if (strbuf[0] == '/')
				/* full pathname provided */
				strcpy(incl_buf, strbuf);
			else
				/* prepend directory info */
				if (lib)
					sprintf(incl_buf, "%s/%s", lib_dir, strbuf);
				else
					sprintf(incl_buf, "%s/%s", apts_dir, strbuf);
			if ((include[include_level] = fopen(incl_buf, "r")) == NULL) {
				strcpy(errbuf, "can't open include file <");
				strcat(errbuf, incl_buf);
				strcat(errbuf, "> (ignored)");
				err_rpt(errbuf, NON_FATAL);
			} else 
				include_level++;
		}
		appt->flags |= A_COMMENT;
		return(0);
	}
	while (isspace(*ptr))
		++ptr;
	if (!*ptr) {
		/* empty line */
		appt->flags |= A_COMMENT;
		return(0);
	}
	if (*ptr == '*') {
		appt->flags |= ALL_YEARS;
		appt->year = START_YEAR;
		++ptr;	/* point to second '*' */
		++ptr;	/* point to space */
	} else {
		appt->year = 0;
		while (isdigit(*ptr)) {
			appt->year *= 10;
			appt->year += *ptr++ - '0';
		}
		/* sanity check */
		if (appt->year < 0) {
			sprintf(errbuf, "illegal year value [%d] (ignored)", appt->year);
			err_rpt(errbuf, NON_FATAL);
			return(1);
		}
	}
	while (isspace(*ptr))
		++ptr;
	if (*ptr == '*') {
		appt->flags |= ALL_MONTHS;
		appt->month = 0;
		++ptr;
	} else {
		appt->month = (*ptr - '0') * 10;
		appt->month += *++ptr - '0';
		if (one_based) (appt->month)--;
		/* sanity check */
		if (appt->month < JAN || appt->month > DEC) {
			sprintf(errbuf, "illegal month value [%d] (ignored)", appt->month);
			err_rpt(errbuf, NON_FATAL);
			return(1);
		}
	}
	++ptr;
	while (isspace(*ptr))
		++ptr;
	if (*ptr == '*') {
		appt->flags |= ALL_DAYS;
		appt->day = 1;
		appt->repeat = 1;
		++ptr;
	} else if (isdigit(*ptr)) {
		appt->day = (*ptr - '0') * 10;
		appt->day += *++ptr - '0';
		if (!one_based) (appt->day)++;
		/* sanity check */
		if (appt->day < 1 || appt->day > 31) {
			sprintf(errbuf, "illegal day value [%d] (ignored)", appt->day);
			err_rpt(errbuf, NON_FATAL);
			return(1);
		}
	} else {
		/* check for day names */
		wday[0] = islower(*ptr) ? toupper(*ptr) : *ptr;
		++ptr;
		wday[1] = islower(*ptr) ? toupper(*ptr) : *ptr;
		wday[2] = '\0';
		i = 0;
		if (!strcmp(wday, dayname[i++]))
			appt->flags |= EVERY_SUN;
		else if (!strcmp(wday, dayname[i++]))
			appt->flags |= EVERY_MON;
		else if (!strcmp(wday, dayname[i++]))
			appt->flags |= EVERY_TUE;
		else if (!strcmp(wday, dayname[i++]))
			appt->flags |= EVERY_WED;
		else if (!strcmp(wday, dayname[i++]))
			appt->flags |= EVERY_THU;
		else if (!strcmp(wday, dayname[i++]))
			appt->flags |= EVERY_FRI;
		else if (!strcmp(wday, dayname[i++]))
			appt->flags |= EVERY_SAT;
		else if (!strcmp(wday, dayname[i]))
			appt->flags |= EVERY_MON_FRI;
		else {
			/* sanity check */
			sprintf(errbuf, "illegal day name [%s] (ignored)", wday);
			err_rpt(errbuf, NON_FATAL);
			return(1);
		}
		appt->day = 1;
		if (appt->flags & EVERY_MON_FRI) {
			appt->repeat = 1;
		} else {
			appt->flags |= REPEAT;
			appt->repeat = ALL_WEEKS;	/* default to every week */
		}
	}
	++ptr;
	while (isspace(*ptr))
		++ptr;
	appt->hour = (*ptr - '0') * 10;
	appt->hour += *++ptr - '0';
	/* sanity check */
	if (appt->hour < 0 || (appt->hour > 23 && appt->hour != 99)) {
		sprintf(errbuf, "illegal hour value [%d] (ignored)", appt->hour);
		err_rpt(errbuf, NON_FATAL);
		return(1);
	}
	if ((version2 && appt->hour == 99) || (!version2 && appt->hour == 0))
		appt->flags |= A_NOTE;
	++ptr;
	while (isspace(*ptr))
		++ptr;
	appt->minute = (*ptr - '0') * 10;
	appt->minute += *++ptr - '0';
	/* sanity check */
	if (appt->minute < 0 || (appt->minute > 59 && appt->minute != 99)) {
		/* minutes currently can only be 00 or 30
		 * unless it's a note.
		 */
		sprintf(errbuf, "illegal minute value [%d] (ignored)", appt->minute);
		err_rpt(errbuf, NON_FATAL);
		return(1);
	}
	if ((appt->flags & A_NOTE) && version2 && appt->minute == 99)
		appt->flags |= MARKED;  /* don't show in mon/yr display */
	++ptr;
	while (isspace(*ptr))
		++ptr;
	appt->arrows = (*ptr - '0') * 10;
	appt->arrows += *++ptr - '0';
	/* sanity check */
	if (appt->arrows < 0 || appt->arrows > 48) {
		sprintf(errbuf, "illegal arrow value [%d] (ignored)", appt->arrows);
		err_rpt(errbuf, NON_FATAL);
		return(1);
	}
	++ptr;
	while (isspace(*ptr))
		++ptr;
	/* lookahead and repeat entries are free format, i.e. they */
	/* can occur in any order */
	parse_options = TRUE;
	while (parse_options) {
		switch (*ptr) {
			case '\\':
				/* start of string text */
				parse_options = FALSE;
				++ptr;
				break;

			case '%':
				/* advance warning time (minutes) */
				appt->warn = 0;
				while (isdigit(*++ptr))
					appt->warn = appt->warn * 10 + (int)(*ptr - '0');
				if (appt->warn < 0) {
					err_rpt("illegal advance warning (ignored)", NON_FATAL);
					appt->warn = 10;
				}
				break;

			case '[':
				/* repeating appointment */
				appt->flags |= REPEAT;
				if (appt->flags & EVERY_SOMEDAY) {
					if ((appt->repeat = do_wk_repeat(&ptr)) < 0)
						return(1);
				} else {
					if ((appt->repeat = do_repeat(&ptr)) < 0)
						return(1);
				}
				break;
			
			case '<':
				/* remind us ahead of time */
				appt->flags |= LOOKAHEAD;
				if ((appt->lookahead = do_lookahead(&ptr)) < 0)
					return(1);
				break;
			
			case '+':
				/* this appointment lasts for n days */
				appt->flags |= RUN;
				while (isdigit(*++ptr))
					appt->runlength = appt->runlength * 10 + (int)(*ptr - '0');
				if (appt->runlength < 0)
					return(1);
				if (!(appt->flags & REPEAT)) {
					/* default to run of days */
					appt->flags |= REPEAT;
					appt->repeat = 1;
				}
				break;

			case '#':
				/* deleted appointment */
				appt->flags |= DELETED;
				++ptr;
				break;
			
			default:
				parse_options = FALSE;
				break;
		}
		while (isspace(*ptr))
			++ptr;
	}
	str = strbuf;
	while (*ptr && *ptr != '\n')
		*str++ = *ptr++;
	*str = '\0';
	strcpy(appt->str, strbuf);
	if (appt->flags & DELETED)
		/* ignore some flags */
		appt->flags &= ~(RUN | REPEAT);

	return(0);
}

/* parse normal repeated entry field */
int
do_repeat(ptr)
char **ptr;
{
	int repeat = 0;

	while (isdigit(*++*ptr))
		repeat = repeat * 10 + (int)(**ptr - '0');
	if (**ptr != ']') {
		err_rpt("bad entry (ignored)", NON_FATAL);
		return(-1);
	}
	/* sanity check */
	if (repeat < 0) {
		err_rpt("illegal repeat interval (ignored)", NON_FATAL);
		return(-1);
	}
	++*ptr;
	return(repeat);
}

/* parse weekly repeated entry field */
int
do_wk_repeat(ptr)
char **ptr;
{
	int repeat = 0;

	while (*++*ptr != ']') {
		if (**ptr == ',')
			continue;	/* get next week */
		if (isdigit(**ptr)) {
			repeat |= 0x1<<(**ptr - '1');
		} else if (**ptr == 'L' || **ptr == 'l') {
			/* last week in month */
			repeat |= LAST_WEEK;
		} else {
			/* format error */
			err_rpt("illegal repeat specification (ignored)", NON_FATAL);
			return(-1);
		}
	}
	/* sanity check */
	if ((unsigned int)repeat > WEEK_LIMIT) {
		err_rpt("illegal weekly repeat (ignored)", NON_FATAL);
		return(-1);
	}
	++*ptr;
	return(repeat);
}

/* parse lookahead entry field */
int
do_lookahead(ptr)
char **ptr;
{
	int lookahead = 0;

	while (isdigit(*++*ptr))
		lookahead = lookahead * 10 + (int)(**ptr - '0');
	if (**ptr != '>') {
		err_rpt("bad entry (ignored)", NON_FATAL);
		return(-1);
	}
	/* sanity check */
	if (lookahead < 0) {
		err_rpt("illegal lookahead interval (ignored)", NON_FATAL);
		return(-1);
	}
	++*ptr;
	return(lookahead);
}

#ifndef CALENCHECK
/*
 * put entry into appointments file
 */
int
put_aentry(apts_file, appt)
FILE *apts_file;
struct appt_entry *appt;
{
	char *to_str();

	if (read_only)
		return(0);

	if (appt->flags & READONLY)
		/* don't copy include file entries */
		/* (the include directive is copied as a comment) */
		return(0);
	if (appt->flags & A_COMMENT) {
		fputs(inbuf, apts_file);
		return(ferror(apts_file));
	}
	if (appt->flags & ALL_YEARS)
		fputs("** ", apts_file);
	else if (appt->year > 99)
		fprintf(apts_file, "%03d ", appt->year);
	else
		fprintf(apts_file, "%02d ", appt->year);
	if (appt->flags & ALL_MONTHS)
		fputs("** ", apts_file);
	else
		fprintf(apts_file, "%02d ", one_based ? appt->month+1 : appt->month);
	if (appt->flags & ALL_DAYS)
		fputs("** ", apts_file);
	else if (appt->flags & EVERY_SOMEDAY) {
		switch (appt->flags & EVERY_SOMEDAY) {
			case EVERY_SUN:
				fputs("Su ", apts_file);
				break;
			case EVERY_MON:
				fputs("Mo ", apts_file);
				break;
			case EVERY_TUE:
				fputs("Tu ", apts_file);
				break;
			case EVERY_WED:
				fputs("We ", apts_file);
				break;
			case EVERY_THU:
				fputs("Th ", apts_file);
				break;
			case EVERY_FRI:
				fputs("Fr ", apts_file);
				break;
			case EVERY_SAT:
				fputs("Sa ", apts_file);
				break;
		}
	} else if (appt->flags & EVERY_MON_FRI) {
		fputs("MF ", apts_file);
	} else
		fprintf(apts_file, "%02d ", one_based ? appt->day : appt->day-1);
	if (appt->flags & A_NOTE) {
		appt->hour = 99;
		appt->minute = 0;	/* assume unmarked note */
	}
	if ((appt->flags & MARKED_NOTE) == MARKED_NOTE)
		appt->minute = 99;
	if (!(appt->flags & (ALL_DAYS|DELETED|EVERY_MON_FRI)) && appt->flags & REPEAT) {
		if (appt->flags & EVERY_SOMEDAY)
			fprintf(apts_file, "%02d %02d %02d %s ", appt->hour, appt->minute, appt->arrows, to_str(appt->repeat));
		else
			fprintf(apts_file, "%02d %02d %02d [%d] ", appt->hour, appt->minute, appt->arrows, appt->repeat);
	} else
		fprintf(apts_file, "%02d %02d %02d ", appt->hour, appt->minute, appt->arrows);

	if (appt->flags & LOOKAHEAD)
		fprintf(apts_file, "<%d> ", appt->lookahead);
	if (appt->flags & RUN)
		fprintf(apts_file, "+%d ", appt->runlength);
	if (appt->warn != 10)
		fprintf(apts_file, "%%%d ", appt->warn);
	if (appt->flags & DELETED)
		fputs("# ", apts_file);
	if (isalnum(*(appt->str)))
		fprintf(apts_file, "%s\n", appt->str);
	else
		fprintf(apts_file, "\\%s\n", appt->str);
	
	/* check for failure (e.g. file system full) */
	return(ferror(apts_file));
}

char rptstr[10];

/* convert repeat bit map to printable string */
char *
to_str(repeat)
int repeat;
{
	int i, j = 0;

	if (repeat == ALL_WEEKS)
		/* if it's every week, then don't write [] spec */
		rptstr[0] = '\0';
	else {
		rptstr[j++] = '[';
		for (i=0; i<5; i++) {
			if (repeat & (0x1<<i)) {
				rptstr[j++] = i+1 + '0';
				rptstr[j++] = ',';
			}
		}
		if (repeat & LAST_WEEK) {
			rptstr[j++] = 'L';
			rptstr[j++] = ',';
		}
		rptstr[j] = '\0';
		rptstr[--j] = ']';
	}
	return (rptstr);
}

/*
 * Convert from version 1 appts files to version 2 file format.
 */
void
ver1to2()
{
	FILE *oappts, *nappts, *fp;
	struct appt_entry appt;
	int err_flag, save_base;
	char save_file[128];
	struct stat stbuf;

	/*
	 * The main difference is that the ver 2 files are one-based, i.e
	 * days and months start with 1, rather than 0. Another difference
	 * is that a hour entry of 99 is used to flag a memo entry, rather
	 * than 00. This allows for 24-hour appointments.
	 * Version 2 appts files are marked with a special header line
	 * defined by the HEADER string (in ct.h).  Version 2 files
	 * also support a "lookahead" reminder service to remind one
	 * in advance of a future appointment.
	 * If "save_old" is set, then any appointments for years prior
	 * to this one are saved in a special file of the form
	 * ".appointments.YY", where YY is the year.
	 */

	if (read_only != 0) {
		err_rpt("appts file is read-only, no conversion done", NON_FATAL);
		return;
	}

	 /* open files, etc */
	if ((oappts = fopen(apts_pathname, "r")) == NULL) {
		err_rpt("can't open appts file for reading", FATAL);
		/* NOT REACHED */
	 }
	if ((nappts = fopen(tmpapts_pathname, "w")) == NULL) {
		err_rpt("can't open temp file for writing", NON_FATAL);
		return;
	 }
	 /* write new header line */
	 fputs(HEADER, nappts);

	 /* copy existing entries to the new file */
	 save_base = one_based;
	 while ((err_flag = get_aentry(oappts, &appt, TRUE, 0, 0)) != EOF) {
		if (err_flag)
			continue;	/* ignore badly formatted input */
		if (appt.hour == 0)
			appt.flags |= A_NOTE;
		one_based = 1;		/* force new format output */
		if (save_old && !(appt.flags & ALL_YEARS) && (appt.year < today.tm_year)
		   && !((appt.flags & REPEAT) && !(appt.flags & EVERY_SOMEDAY))
		   && !(appt.flags & A_COMMENT)) {
			/* prepend directory info */
			sprintf(save_file, "%s.%02d",
				apts_pathname, appt.year);
			if (stat(save_file, &stbuf) && errno == ENOENT) {
				/* new file*/
				if ((fp = fopen(save_file, "w")) == NULL)
					err_rpt("can't open save file, bailing out", FATAL);
				fputs(HEADER, fp);
				fclose(fp);
			}
			if ((fp = fopen(save_file, "a+")) == NULL)
				err_rpt("can't open save file, bailing out", FATAL);
			else {
				if (put_aentry(fp, &appt))
					err_rpt("write to save appt file failed, bailing out", FATAL);
				fclose(fp);
			}
		} else {
			if (appt.flags & A_COMMENT)
				fputs(inbuf, nappts);
			else
				if (put_aentry(nappts, &appt))
					err_rpt("write to new appt file failed, bailing out", FATAL);
		}
		one_based = save_base;	/* (maybe) force old format input */
	}
	fclose(oappts);
	fclose(nappts);
	xrename(tmpapts_pathname, apts_pathname);
	one_based = 1;
	version2 = 1;
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
	char tmp[9];

	if ((strlen(str) > 7) && (str[2] == '-') && (str[5] == '-')) {
		/* Assume a yy-mm-dd format (Scandinavian/ISO) style */
		/* Turn it into m/d/y format */
		sprintf(tmp, "%c%c/%c%c/%c%c", str[6], str[7],
			str[3], str[4], str[0], str[1]);
		    strcpy(str, tmp);
	}
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
		c[2] = '\0';
		for (i=0; i<7; i++) {
			if (!strcmp(c, dayname[i])) {
				dow = i;
				break;
			}
		}
		if (dow >= 0) {
			/* match found */
			current.tm_mday += dow - current.tm_wday;
			fix_current_day();
		} else if (!strncmp(c, "TOM", 3)) {
			/* tommorrow */
			current.tm_mday++;
			fix_current_day();
		} else if (!strncmp(c, "YES", 3)) {
			/* yesterday */
			current.tm_mday--;
			fix_current_day();
		} else {
			if (cmdline)
				err_rpt("badly formed date for -d option (ignored)", NON_FATAL);
			else
				err_rpt("badly formed date - please reenter", NON_FATAL);
			return(1);
		}
	}
	return(0);
}
#endif  /* CALENCHECK */

/* set error logging flag */
void
err2console(state)
int state;
{
	/*
	 * if TRUE, forces error messages to the console, even
	 * if the base frame is running
	 */
	log_to_console = state;
}

/*
 * Error reporting. Try first to put message in a popup frame, then
 * the console, then stderr as a last resort.
 */
void
err_rpt(errstr, fatal_flag)
char *errstr;
int fatal_flag;
{
	FILE	*f;
#ifndef NOTOOL
	int	closed;
#endif

#ifndef NOTOOL
	closed = (int) xv_get(frame, FRAME_CLOSED);
	if (frame && !log_to_console && !closed) {
		/* base frame exists */
		(void)notice_prompt(frame, NULL,
			NOTICE_MESSAGE_STRINGS, errstr, 0,
			NOTICE_BUTTON_YES, "Ok",
			0);
	} else if ((f=fopen("/dev/console", "w")) != NULL) {
#else
	if (getenv("WINDOW_PARENT") != NULL && (f=fopen("/dev/console", "w")) != NULL) {
#endif
		fprintf(f, "%s: %s\n", progname, errstr);
		fclose(f);
	} else
		fprintf(stderr, "%s: %s\n", progname, errstr);
	if (fatal_flag)
		exit(1);
}

#ifndef CALENCHECK
/* Clean-up */
void
cleanup()
{
	if (day_is_open)
		close_day();

	/* create outdated include files (if necessary) */
	if (save_old || expire_days)
		expire(expire_days);
			
	/* delete tmp file */
	if (access(tmpapts_pathname, R_OK) == 0 && unlink(tmpapts_pathname) < 0)
		perror(tmpapts_pathname);
}

char sysbuf[512];

/* Rename files, copying if necessary */
void
xrename(from, to)
char *from, *to;
{
	if (rename(from, to) == -1) {
		/* rename sys call fialed, try doing a copy */
		sprintf(sysbuf, "cp %s %s", from, to);
		if (system(sysbuf) != 0)
			err_rpt("couldn't rename/copy tmp file", NON_FATAL);
	}
}

/*
 *  Conventional and ISO standard week numbers (week starts with monday)
 *  Assume if moday_first is set TRUE then ISO format is desired.
 */
int
week_number()
{
	int week_n, week_d, W, r, day, year, leap;

	/*  First day of this week */
	if (monday_first)
		week_d = (current.tm_wday?(current.tm_wday-1):6);
	else
		week_d = current.tm_wday;
	current.tm_mday -= week_d;
	fix_current_day();
	day = current.tm_yday + 1;
	week_n = day / 7 + 1;       /*  "Raw" week number  */ 
	r = day % 7 - 1;           /*  No of days in the first week this year  */

	if (!monday_first) {
		if (week_n == 53)
			week_n = 1;
		else if (r > 0 && day > r)
			if ((++week_n == 53) && ((day + 6) > length_of_year(current.tm_year+1900)))
				week_n = 1;
	} else {
		if (r >= 4) week_n++;       /*  First week has more than three days  */
		if (week_n == 53) {         /*  Week number of last week of year  */
			year = current.tm_year + 1900;
			leap = ((year % 4 == 0) && ((year % 100 != 0) || (year % 400 == 0)));
			day += 7 - ( leap ? 366 : 365 );
			W = day / 7 + 1;
			r = day % 7 - 1;
			if (r >= 4) W++;
			if (W == 2) week_n = 1;
		    }
	}
	current.tm_mday += week_d;
	fix_current_day();
	return week_n;
}
#endif  /* CALENCHECK */

#ifdef NEED_CUSERID
char *
cuserid(s)
char *s;
{
	static char userid[16];
	char *ptr;
	int uid;
	struct passwd *pw;
	struct passwd *getpwuid ();
	char *getlogin ();

	if ((ptr = getenv ("USER")) != NULL)
		(void) strcpy (userid, ptr);
	else if ((ptr = getlogin()) != NULL)
		(void) strcpy (userid, ptr);
	else {
		uid = getuid ();
		pw = getpwuid (uid);
		(void) strcpy (userid, pw->pw_name);
	}
	if (s != NULL)
		(void)strcpy(s, userid);
}
#endif /* NEED_CUSERID */

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
