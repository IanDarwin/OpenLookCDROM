/*
 * $Id: put_aentry.c,v 2.3 1994/08/19 19:53:08 billr Exp $
 */
/*
 * put_aentry - write calentool style files
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

#include "ct.h"
#include <ctype.h>

/*
 * put entry into appointments file
 */
int
put_aentry(apts_file, appt)
FILE *apts_file;
struct appt_entry *appt;
{
	char *to_str();
	char inbuf[512];
	int one_based = 1;

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
	if (!(appt->flags & (ALL_DAYS|DELETED)) && appt->flags & REPEAT) {
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
		fprintf(apts_file, "%s", appt->str);
	else
		fprintf(apts_file, "\\%s", appt->str);
	
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
