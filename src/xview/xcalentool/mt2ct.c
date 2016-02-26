/*
 * $Id: mt2ct.c,v 2.3 1994/08/19 19:53:08 billr Exp $
 */
/*
 * mt2ct - convert monthtool reminder files to calentool style files
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

#include "ct.h"
#include <stdio.h>
#include <ctype.h>

struct appt_entry appts;
struct appt_entry *aptr;
char filename[128], *file;
FILE *fp;

#ifdef __STDC__
int read_mt_file (void);
void write_ct_file (void);
#else
int read_mt_file ();
void write_ct_file ();
#endif

void
main(argc, argv)
int argc;
char *argv[];
{
	if (argc > 1)
		file = argv[1];
	else {
		strcpy(filename, getenv("HOME"));
		strcat(filename, "/.monthtool");
		file = filename;
	}

	if ((fp = fopen(file, "r")) == NULL) {
		fprintf(stderr, "can't open monthtool file <%s> for reading\n", file);
		exit(1);
	}
	if (!read_mt_file()) {
		fprintf(stderr, "no reminders read from %s\n", file);
		exit(1);
	}
	fclose(fp);
	strcpy(filename, getenv("HOME"));
	strcat(filename, "/.appointments");
	if ((fp = fopen(filename, "w")) == NULL) {
		fprintf(stderr, "can't open .appointments file for writing\n");
		exit(1);
	}
	write_ct_file();
}

/*
 * read in the monthtool file, filling the appts records
 */
int
read_mt_file()
{
	char *ptr, *fgets();
	char buf[512];
	struct appt_entry *optr;

	aptr = &appts;
	optr = aptr;
	while (fgets(buf, 512, fp) != NULL) {
		aptr->flags = aptr->repeat = aptr->lookahead = 0;
		aptr->warn = 10;  /* default */
		aptr->sindex = 0;
		aptr->next = NULL;
		ptr = buf;
		while (isspace(*ptr))
			++ptr;
		aptr->month = *ptr++ - '0';
		if (isdigit(*ptr))
			aptr->month = aptr->month * 10 + (*ptr++ - '0');
		++ptr;	/* skip ',' */
		aptr->day = *ptr++ - '0';
		if (isdigit(*ptr))
			aptr->day = aptr->day * 10 + (*ptr++ - '0');
		++ptr;	/* skip ',' */
		aptr->year = 0;
		while (isdigit(*ptr))
			aptr->year = aptr->year * 10 + (*ptr++ - '0');
		if (aptr->year > 1900)
			aptr->year -= 1900;
		++ptr;	/* skip ',' */
		if (!strncmp(ptr, "    ", 4)) {
			aptr->flags |= A_NOTE;
			ptr += 5;
		} else {
			aptr->hour = (*ptr++ - '0') * 10;
			aptr->hour += *ptr++ - '0';
			aptr->minute = (*ptr++ - '0') * 10;
			aptr->minute += *ptr++ - '0';
			if (aptr->minute < 15)
				aptr->minute = 0;
			else if (aptr->minute < 45)
				aptr->minute = 30;
			else {
				aptr->minute = 0;
				aptr->hour++;
			}
			++ptr;	/* skip ',' */
		}
		strcpy(aptr->str, ptr);
		if (aptr->year == 0)
			aptr->flags |= ALL_YEARS;
		if (aptr->month == 0)
			aptr->flags |= ALL_MONTHS;
		else if (aptr->month == 99) {
			/* weekly reminder */
			aptr->flags |= ALL_MONTHS;
			aptr->flags |= Setday(aptr->day-1);
		} else
			--aptr->month;
		if (aptr->day == 0)
			aptr->flags |= ALL_DAYS;
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
void
write_ct_file()
{
	aptr = &appts;
	fputs(HEADER, fp);
	while (aptr) {
		if (put_aentry(fp, aptr)) {
			fprintf(stderr, "error writing .appointments file\n");
			return;
		}
		aptr = aptr->next;
	}
}
