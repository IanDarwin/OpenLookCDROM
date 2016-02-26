/*
 * $Id: pcaldw.c,v 2.3 1994/08/29 17:34:39 billr Exp $
 */
/*
 * pcaldw - print pretty PostScript image of a day or week calendar
 *
 * Pieces extracted from the pcal program by Ken Keirnan and modified
 * heavily by Bill Randle, Tektronix, Inc. <billr@saab.CNA.TEK.COM>.
 * Support for non-English characters by Hakan Kallberg <hk@simulina.se>.
 * 
 * Copyright 1991, 1994 by Tektronix, Inc. - All Rights Reserved.
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
#include "ct.h"		/* for NO_PRINTER define */

#ifndef NO_PRINTER
#ifndef RASTER_ONLY
#include <stdio.h>
#include <time.h>

extern struct tm current, First;
extern struct dayslot *slots;
extern struct weekrect week_boxes[];
extern char *daynames[], *monthnames[];
extern int day_first, hour24, n_tslots, nr_weekdays, n_slots;
extern int start_hour;
extern int print_trailer;
extern char trailer[];
extern int day_of_year(), days_remaining_in_year();
int weekday;

#define DAY_FORMAT	1
#define WEEK_FORMAT	2
#define MAX_SLOTS 64

/*
 * natural language support for non-English characters
 */
/*
 *  p_lang_header - provides national language support
 */
char *p_lang_header[] = {
#ifdef LANGUAGE
/* define LANGUAGE only for small modifications of default font encoding */
/* If you use ISO-8859 character set, do not define LANGUAGE.		 */
/** Note: Only SWEDISH is currently supported **/
/** send your local encodings to billr@saab.CNA.TEK.COM **/
/*#define SWEDISH */
/* #define GERMAN */
    "/ReEncodeSmall",
    "  { 0 begin",
    "    /newcodesandnames exch def",
    "    /basefontname exch def",
    "    /basefontdict basefontname findfont def",
    "    /newfont basefontdict maxlength dict def",
    "    basefontdict",
    "      { exch dup /FID ne",
    "        { dup /Encoding eq",
    "          { exch dup length array copy",
    "            newfont 3 1 roll put }",
    "          { exch newfont 3 1 roll put }",
    "          ifelse }",
    "        { pop pop }",
    "        ifelse",
    "      } forall",
    "    newcodesandnames aload pop",
    "    newcodesandnames length 2 idiv",
    "      { newfont /Encoding get 3 1 roll put}",
    "      repeat",
    "    basefontname newfont definefont pop",
    "    end",
    "  } def",
    "/ReEncodeSmall load 0 12 dict put",
    "/codevec [",
#ifdef  SWEDISH
      "  8#140 /eacute		% `",
      "  8#176 /udieresis	% ~",
      "  8#175 /aring		% }",
      "  8#173 /adieresis	% {",
      "  8#174 /odieresis	% |",
      "  8#100 /Eacute		% @",
      "  8#136 /Udieresis	% ^",
      "  8#135 /Aring		% ]",
      "  8#133 /Adieresis	% [",
      "  8#134 /Odieresis	% \\",
#endif  SWEDISH
#ifdef  GERMAN
#endif  GERMAN
      "]  def",
    "daytextfont dup codevec ReEncodeSmall",
#else
	"/reencodeISO { %def",
	"     /newdict /ISOLatin1Encoding where ",
	"     { pop ISOLatin1Encoding }{ StandardEncoding } ifelse def",
	"     findfont  dup length dict begin",
	"     { 1 index /FID ne { def }{ pop pop } ifelse } forall",
	"     /Encoding newdict def",
	"     currentdict end definefont pop",
	"} bind def",
	"daytextfont dup reencodeISO",
	"dayfont dup reencodeISO",
	"titlefont dup reencodeISO",
#endif LANGUAGE
(char *)0
};


/*
 * pheader - provides the PostScript routines
 */
static char *prolog[] = {
    "%!",
    "%%Creator: Bill Randle, Tektronix, Inc.",
    "%%Title: calentool's day-or-week-at-a-glance",
    "%%For: calentool",
    "%%DocumentFonts: Times-Bold Helvetica Times-Roman",
    "%%Bounding Box: 0 0 500 700",
    "%%Pages: 1",
    "%%EndComments",
    "/titlefont /Times-Bold def",
    "/dayfont /Helvetica def",
    "/daytextfont /Times-Roman def",
    (char *)0
};

static char *pheader[] = {
    "/prtnum { dup 9 le {(1) stringwidth rmoveto} if 3 string cvs show} def",

    /*
     * Draw the day name and the grid for the current day
     */
    "/drawgrid {",
    "  1.0 setlinewidth",
    "  0 1 nslots 1 sub {",
    "    gsave",
    "      400 0 rlineto",
    "      0 slotHeight neg rlineto",
    "      -400 0 rlineto",
    "      closepath stroke",
    "    grestore",
    "    0 slotHeight neg rmoveto",
    "  } for",
    "} def",

    "/drawtimes {",
    "  /timex exch def",
    "  dayfont findfont 12 scalefont setfont",
    "  0 1 nslots 1 sub {",
    "    /slot exch def",
    "    slot slotHeight mul slotHeight 0.75 mul add neg timex exch moveto",
    "    slot ntslots eq {",
    "      (Notes) show",
    "    } { slot ntslots lt {",
    "        hour24 0 eq {",
    "          shour slot 2 idiv add 12 mod dup 0 eq { pop (12) show",
    "            } { prtnum } ifelse (:) show slot 2 mod 0 eq {",
    "              (00) } { (30) } ifelse show",
    "          shour slot 2 idiv add 12 lt { (am) } { (pm) } ifelse show",
    "        } {",
    "          shour slot 2 idiv add prtnum (:) show",
    "          slot 2 mod 0 eq { (00) } { (30) } ifelse show",
    "        } ifelse",
    "      } if",
    "    } ifelse",
    "  } for",
    "} def",

    /*
     * define clipping region for the text
     */
    "/pageclip {",
    "  newpath",
    "    100 0 moveto",
    "    400 0 rlineto",
    "    0 slotHeight nslots mul neg rlineto",
    "    -400 0 rlineto",
    "  closepath",
    "} def",

    /*
     * string width center
     * centers <string> in the width given by <width>
     */
    "/center {",
    "  /width exch def",
    "  /str exch def width str ",
    "  stringwidth pop sub 2 div 0 rmoveto str show",
    "} def",

    /*
     * print the string for a given slot s
     *   str x s slotstr
     */
    "/slotstr {",
    "  slotHeight mul slotHeight 0.75 mul add neg moveto show",
    "} def",

    /*
     * draw an arrow shaft from point x1, slot s1 to slot s2.
     *   x1 s1 s2 arrowshaft
     */	
    "/arrowshaft {",
    "  3 1 roll 3 copy 1.0 setlinewidth",
    "  dup slotHeight mul neg exch 3 1 roll moveto",
    "  sub 1 add slotHeight mul neg 0 exch rlineto",
    "  dup slotHeight mul neg exch 3 1 roll exch 7 add exch moveto",
    "  sub 1 add slotHeight mul neg 0 exch rlineto",
    "} def",

    /*
     * draw an arrowhead at point x, slot s.
     *   x s arrowhead
     */
    "/arrowhead {",
    "  2 copy 2 setlinejoin 1.0 setlinewidth",
    "  slotHeight mul neg moveto 0 slotHeight neg rlineto",
    "  -5 slotHeight 0.4 mul rlineto stroke",
    "  slotHeight mul neg exch 7 add exch moveto 0 slotHeight neg rlineto",
    "  5 slotHeight 0.4 mul rlineto stroke",
    "} def",

    /*
     * draw a vertical bar to seperate multiple appts in the same slot
     *   x s apptbar
     */
    "/apptbar {",
    "  2.0 setlinewidth\n",
    "  slotHeight mul neg moveto 0 slotHeight neg rlineto",
    "} def",

    /*
     * do one day page
     */
    "/daypage",
    "{",
    "  100 0 moveto",
    "  drawgrid",
    "  gsave",
    "    pageclip clip",
    "    daytext",
    "  grestore",
    "} def",

    /*
     * printweek - Draw an entire week calendar by printing
     * scaled versions of individual day pages.
     */
    "/printweek",
    "{",
    "  90 rotate",
    "  40 -50 translate",
    "  /offset -380 def",
    "  /xscale 8.75 nweekd div 5.625 div def",
    "  dayfont findfont 10 scalefont setfont",
    "  gsave",
    "    0 1 nweekd 1 sub {",
    "      /daynum exch def",
    "      /offset offset 405 add def",
    "      gsave",
    "        xscale .73 scale",
    "        offset 0 translate",
    "        daytextfont findfont [30 0 0 14 0 0] makefont setfont",
    "        daytextindex daynum get cvn cvx /daytext exch def",
    "        daypage",
    "      grestore",
    "      gsave",
    "        offset xscale mul 0 translate",
    "        100 xscale mul 20 .73 mul moveto",
    "        weekday daynum get",
    "        400 xscale mul center",
    "        100 xscale mul -620 .73 mul moveto",
    "        weekdate daynum get",
    "        400 xscale mul center",
    "      grestore",
    "    } for",
    "    gsave",
    "      1 .73 scale",
    "      -20 drawtimes",
    "      405 nweekd mul xscale mul 40 add drawtimes",
    "    grestore",
    "  grestore",
    "  120 -480 moveto",
    "  yearweek show",
    "} def",

    /*
     * printday - print a single day page
     */
    "/printday {",
    "  40 700 translate",
    "  titlefont findfont 24 scalefont setfont",
    "  100 40 moveto",
    "  title 400 center",
    "  dayfont findfont 10 scalefont setfont",
    "  daytextfont findfont 10 scalefont setfont",
    "  daypage",
    "  drawtimes",
    "  dayfont findfont 10 scalefont setfont",
    "  100 -640 moveto",
    "  yearday 400 center",
    "} def",

    "/daytextindex [ (day0) (day1) (day2) (day3) (day4) (day5) (day6) ] def",

    /*
     * print a page trailer with appts file, userid and date
     */
    "/print_wtrailer {",
    "  40 -520 moveto",
    "  trailer 650 center",
    "} def",
    "/print_dtrailer {",
    "  100 -660 moveto",
    "  trailer 400 center",
    "} def",

  (char *)0,
};

#ifdef __STDC__
void do_ps_header (FILE *fp);
void do_ps_day (FILE *fp, int noteflag, int format);
int deleted (struct appt_entry *ap, struct dayslot *slptr);
#else
void do_ps_header ();
void do_ps_day ();
int deleted ();
#endif

void
print_psday(fp, noteflag)
FILE *fp;
int noteflag;
{
    char daystring[128];

    /*
     * write out headers and prologs
     */
    do_ps_header(fp);
    /*
     * Do the calendar
     */
    First = current;
    if (day_first)
	    sprintf(daystring, "%s %d %s %d",
		    daynames[current.tm_wday], current.tm_mday,
		    monthnames[current.tm_mon], 1900 + current.tm_year);
    else
	    sprintf(daystring, "%s %s %d, %d",
		    daynames[current.tm_wday], monthnames[current.tm_mon],
		    current.tm_mday, 1900 + current.tm_year);
    fprintf(fp, "/title (%s) def\n", daystring);
    sprintf(daystring,
	    "Day of year: %d  --  %d days remaining.      Week number: %d",
	    day_of_year((double)current.tm_mday, current.tm_mon+1, current.tm_year+1900),
	    days_remaining_in_year((double)current.tm_mday, current.tm_mon+1, current.tm_year+1900),
	    week_number());
    fprintf(fp, "/yearday (%s) def\n", daystring);

    /*
     * printout the text for this day
     */
    fprintf(fp, "/daytext {\n");
    do_ps_day(fp, noteflag, DAY_FORMAT);
    fprintf(fp, "} def\n");
    fprintf(fp, "printday\n");
    if (print_trailer) {
	fprintf(fp, "/trailer (%s) def\n", trailer);
	fprintf(fp, "print_dtrailer\n");
    }

    /*
     * Write out PostScript postlog
     */
    fprintf(fp, "showpage\n");
}

void
print_psweek(fp, noteflag)
FILE *fp;
int noteflag;
{
    /*
     * write out headers and prologs
     */
    do_ps_header(fp);
    /*
     * Do the calendar
     */
    First = current;
    fprintf(fp, "/weekday [ ");
    for (weekday=0; weekday<nr_weekdays; weekday++)
    	fprintf(fp, "(%3.3s) ", daynames[(First.tm_wday+weekday) % 7]);
    fprintf(fp, "] def\n");
    fprintf(fp, "/weekdate [ ");
    for (weekday=0; weekday<nr_weekdays; weekday++) {
	if (day_first)
	    fprintf(fp, "(%d %3.3s) ", current.tm_mday,
		    monthnames[current.tm_mon]);
	else
	    fprintf(fp, "(%3.3s %d) ", monthnames[current.tm_mon],
		    current.tm_mday);
	current.tm_mday++;
	fix_current_day();
    }
    fprintf(fp, "] def\n");
    current = First;
    fprintf(fp, "/yearweek (%d         Week number: %d) def\n",
	    current.tm_year+1900, week_number());

    /*
     * get list of appts for this week
     */
    get_week_appts();

    /*
     * printout the text for each weekday
     */
    for (weekday=0; weekday<nr_weekdays; weekday++) {
	fprintf(fp, "/day%d\n{\n", weekday);
	do_ps_day(fp, noteflag, WEEK_FORMAT);
	fprintf(fp, "} def\n");
    }
    fprintf(fp, "printweek\n");
    if (print_trailer) {
	fprintf(fp, "/trailer (%s) def\n", trailer);
	fprintf(fp, "print_wtrailer\n");
    }

    /*
     * Write out PostScript postlog
     */
    fprintf(fp, "showpage\n");

    /*
     * free up memory
     */
    free_week_appts();
}

void
do_ps_header(fp)
FILE *fp;
{
    char **ap;

    /*
     * Write out PostScript header
     */
    for (ap = prolog; *ap; ap++)
	fprintf(fp, "%s\n", *ap);

    /*
     * Write out PS prolog for font reencoding
     */
    for (ap = p_lang_header; *ap; ap++)
	fprintf(fp, "%s\n", *ap);

    fprintf(fp, "%%EndProlog\n");

    /*
     * Write out common PostScript
     */
    for (ap = pheader; *ap; ap++)
	fprintf(fp, "%s\n", *ap);

    fprintf(fp, "/shour %d def\n", start_hour);
    fprintf(fp, "/nslots %d def\n", n_slots);
    fprintf(fp, "/ntslots %d def\n", n_tslots);
    fprintf(fp, "/hour24 %d def\n", hour24);
    fprintf(fp, "/nweekd %d def\n", nr_weekdays);
    fprintf(fp, "/slotHeight %.2f def\n", (double)600/n_slots);
}

/*
 * printout text for appts and the continuation arrows
 */
void
do_ps_day(fp, noteflag, format)
FILE *fp;
int noteflag;
int format;
{
    char      *p, *q;
    int		i, offset = 0, narrows;
    int	 slotno, xpos=105;
    struct appt_entry	*aptr;
    char strbuf[MAX_STRLEN+MAX_STRLEN/4];
    int arrow_loc[MAX_SLOTS];
    struct dayslot *slptr;

    for (slotno=0; slotno<n_slots; slotno++)
	arrow_loc[slotno] = 0;

    for (slotno=0; slotno<n_slots; slotno++) {
	/* any appts in this timeslot? */
	slptr = (format==WEEK_FORMAT ? &week_boxes[weekday].weekslots[slotno] :
		&slots[slotno]);
	if (slptr->active) {
	    fprintf(fp, "  /xpos %d def\n", xpos);
	    if (slptr->count > slptr->active) {
		/*
		 * there are arrows here from above. offset our start
		 * of text so they won't be on top of each other.
		 */
		offset = 0;
		while (arrow_loc[slotno] & 1<<offset)
			offset++;
		fprintf(fp, "  xpos 30 %d mul add /xpos exch def\n",
			offset+1);
	    }
	    i = 0;
	    for (aptr=slptr->first; aptr; aptr=aptr->next) {
		/* get printable string from each appt */
		if (!deleted(aptr, slptr) &&
		    (!noteflag || ((aptr->flags & MARKED_NOTE) != MARKED_NOTE))) {
		    if (i) {
			/* not the first time thru the loop */
			/* move over, print a bar then next appt */
			fprintf(fp, "  xpos %d apptbar\n", slotno);
			fprintf(fp, "  xpos 5 add /xpos exch def\n");
		    }
		    /* cleanup the strings */
		    p = aptr->str;
		    q = strbuf;
		    while (*p) {
			switch (*p) {
			    /* ignore these */
			    case '\b':
			    case '\f':
			    case '\n':
			    case '\r':
			    case '\t':
				    break;
			    /* escape these for PostScript */
			    case '\\':
			    case '(':
			    case ')':
				    *q++ = '\\';
				    break;
			}
			*q++ = *p++;
		    }
		    *q = '\0';
		    /* print the string and any arrows */
		    fprintf(fp, "  (%s) xpos %d slotstr\n", strbuf, slotno);
		    if ((narrows = aptr->arrows) > 0) {
			while (narrows)
				arrow_loc[slotno+narrows--] |= 1<<offset;
			if (aptr->arrows > 1)
			    fprintf(fp, "    xpos 20 add %d %d arrowshaft\n",
				slotno+1, slotno+aptr->arrows-1);
			fprintf(fp, "    xpos 20 add %d arrowhead\n",
				slotno+aptr->arrows);
		    }
		    fprintf(fp, "    (%s) stringwidth pop xpos add 5 add /xpos exch def\n", strbuf);
		    i++;
		}
	    }
	}
    }
}

/* check to see if appt pointed to by ap has been deleted */
int
deleted(ap, slptr)
struct appt_entry *ap;
struct dayslot *slptr;
{
    struct appt_entry *a;

    if (ap->flags & DELETED)
	return(1);

    if (Repeating(ap->flags))
	/* run through the list to see if there are any deleted */
	for (a=slptr->first; a; a=a->next)
	    if (a->flags & DELETED) {
		/* now see if the current one matches */
		if (!strcmp(a->str, ap->str))
		    return(1);
		}

    return(0);
}
#endif /* RASTER_ONLY */
#endif /* NO_PRINTER */
