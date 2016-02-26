/*
 * $Id: pcal.c,v 2.3 1994/08/29 17:34:39 billr Exp $
 */
#include "ct.h"		/* for NO_PRINTER define */

#ifndef NO_PRINTER
#ifndef RASTER_ONLY
/*
 * pcal - print pretty PostScript image of a month calendar
 *
 * Pieces extracted from the pcal program by Ken Keirnan and modified
 * slightly by Bill Randle, Tektronix, Inc. <billr@saab.CNA.TEK.COM>.
 * 
 * "Pcal" is a program to print PostScript calendars for any month and year.
 * Pcal is the combined effort of several people, most notably Patrick Wood
 * of Pipeline Associates, Inc. for the original PostScript code and Bill
 * Vogel of AT&T for the calendar file mechanism.  My part was simple
 * translation to a "C" program, the addition of a couple options and a more
 * generalized date searching routine (oh yes, and a manual page :-).
 * 
 * The original calendar PostScript was Copyright (c) 1987 by Patrick Wood
 * and Pipeline Associates, Inc. with permission to modify and redistribute.
 * 
 * Ken Keirnan
 * Pacific Bell
 * San Ramon, CA.
 *
 * Changes and additions Copyright (C) 1989, 1991 Tektronix, Inc.
 *	All Rights Reserved
 * Permission is hereby granted to use and modify the modifications in source
 * or binary form as long as they are not sold for profit and this copyright
 * notice remains intact.
 *
 * Modified by PM Lashley, KLA Instruments, Inc.
 * Fixes and extentions:				1..2 June, 1989
 *	1. Notes were extending beyond the right and bottom edges of the day.
 *	   They are now line-wrapped with indentation.
 *	3. Bottom justify notes for each day.  Truncate if there are too many
 *	   to fit.
 *	3. If month starts on a Wednesday or later, put the previous/next
 *	   month inserts in the first two day boxes instead of the last two.
 *	4. Center the month inserts vertically in the day box.
 *	5. When possible, draw the calendar in a 5x7 grid instead of a 6x7
 *	   grid.  Each day will be taller, and accomodate more note text.
 *	6. Added various comments to the Postscript code array.
 *	7. If a periodic event was removed for given occurance, that event
 *	   would be printed twice when it should not be printed at all.
 *	8. Replaced the constant "Helvetica-Narrow" with PS_NOTE_FONT for
 *	   sites which do not have the Helvetica-Narrow font but do have
 *	   some preference other than the default.
 *
 * Left to do:
 *	1. If a 31 day month starts on Tuesday, the previous month should
 *	   be in the upper left, and the next month in the lower right.
 *	   This may be more trouble than it is worth.
 *
 * Notes:
 *	1. The Postscript code layout is a compromise between readability
 *	   and compaction.  All comments and vertical spacing are in the
 *	   c source only to keep the output file size small.
 *
 * Further changes and additions Copyright (C) 1989 PM Lashley
 *	All Rights Reserved
 * Permission is hereby granted to use and modify the modifications in source
 * or binary form as long as they are not sold for profit and this copyright
 * notice remains intact.
 *
 * Several more additions by Hakan Kallberg (hk@simulina.se) to support
 * printing Monday first in the week and natural language support for
 * Swedish.
 *
 */

#include <stdio.h>
#include <time.h>

extern struct tm current, First;
extern struct dayslot *slots;
extern int get_day_appts();
extern int monday_first;
extern int n_slots;
extern int deleted();
extern char *p_lang_header[];
extern int print_trailer;
extern char trailer[];

/*
 * pmonfirst - replace strings for entries in pheader for monday first
 * calendars.
 */
static char *pmonfirst[] = {
#ifdef FRENCH
	"  [ (Lundi)  (Mardi) (Mercredi) ",
	"    (Jeudi)  (Vendredi) (Samedi) (Dimanche) ]",
#else
# ifdef SWEDISH
	"  [ (Mandag) (Tisdag) (Onsdag) ",
	"    (Torsdag) (Fredag) (Lordag) (Sondag) ]",
# else
	"  [ (Monday) (Tuesday) (Wednesday) ",
	"    (Thursday) (Friday) (Saturday) (Sunday) ]",
# endif /* SWEDISH */
#endif /* FRENCH */
	"    day start add 1 add 7 mod 0 eq {",
	"    day start add 1 add 7 mod 1 eq {",
	"  1 sub dup -1 eq { pop 6 } if"
};

/*
 * pheader - provides the PostScript routines
 */
static char *prolog[] = {
    "%!",
	"%%Creator: Pipeline Associates",
	"%%Title: calentool's month-at-a-glance",
	"%%Modifed: Ken Keirnan, Bill Randle, PM Lashley, Richard Wolff and Hakan Kallberg",
	"%%DocumentFonts: Times-Bold Helvetica-Bold Helvetica-Narrow",
	"%%Bounding Box: 0 0 500 700",
	"%%Pages: 1",
	"%%EndComments",
    "/titlefont /Times-Bold def",
    "/dayfont /Helvetica-Bold def",
    (char *)0
};

static char *pheader[] = {
#ifdef FRENCH
    "/month_names [ (Janvier) (Fevrier) (Mars) (Avril) (Mai) (Juin) (Juillet)",
    "  (Aout) (Septembre) (Octobre) (Novembre) (Decembre) ] def",
#else
# ifdef SWEDISH
    "/month_names [ (Januari) (Februari) (Mars) (April) (Mai) (Juni) (Juli)",
    "  (Augusti) (September) (Oktober) (November) (December) ] def",
# else
    "/month_names [ (January) (February) (March) (April) (May) (June) (July)",
    "  (August) (September) (October) (November) (December) ] def",
# endif /* SWEDISH */
#endif /* FRENCH */
    "/prtnum { 3 string cvs show} def",
    /*
     * -  -weeks-  int
     *
     * Pushes the number of week lines (rows) necessary for the current month.
     */
    "/weeks {",
    "  startday ndays add 35 gt { 6 } { 5 } ifelse",
    "} def",

    /*
     * -  -monthHeight-  int
     *
     * Pushes the height of an individual day box for the current month.
     */
    "/monthHeight { weeks 5 eq { 96 } { 80 } ifelse } def",

    /*
     * Draw the day names and the grid for the current month
     */
    "/drawgrid {",
    "  dayfont findfont 10 scalefont setfont",
    "  0 1 6 {",
    "    dup dup 100 mul 40 moveto",
#ifdef FRENCH
	"  [ (Dimanche) (Lundi)  (Mardi) (Mercredi) ",
	"    (Jeudi)  (Vendredi) (Samedi) ]",
#else
# ifdef SWEDISH
    "    [  (Sondag) (Mandag) (Tisdag) (Onsdag) ",
    "      (Torsdag) (Fredag) (Lordag) ]",
# else
    "    [  (Sunday) (Monday) (Tuesday) (Wednesday)",
    "      (Thursday) (Friday) (Saturday) ]",
# endif /* SWEDISH */
#endif /* FRENCH */
    "    exch get",
    "    100 center",
    "    100 mul 35 moveto",
    "    1.0 setlinewidth",
    "    0 1 weeks 1 sub {",
    "      gsave",
    "        100 0 rlineto",
    "        0 monthHeight neg rlineto",
    "        -100 0 rlineto",
    "        closepath stroke",
    "      grestore",
    "      0 monthHeight neg rmoveto",
    "    } for",
    "  } for",
    "} def",

    /*
     * Draw in the day numbers for each day of the current month.
     *
     * Sunday (and possibly Saturday) will be gray.
     */
    "/drawnums {",
    "  dayfont findfont 30 scalefont setfont",
    "  /start startday def",
    "  /days ndays def",
    "  start 100 mul 5 add 10 rmoveto",
    "  1 1 days {",
    "    /day exch def",
    "    gsave",
#ifndef SATBLK
    "      day start add 7 mod 0 eq {",
    "        submonth 0 eq { .8 setgray } if",
    "      } if",
#endif
    "      day start add 7 mod 1 eq {",
    "        submonth 0 eq { .8 setgray } if",
    "      } if",
    "      day prtnum",
    "    grestore",
    "    day start add 7 mod 0 eq",
    "    { currentpoint exch pop monthHeight sub 5 exch moveto }",
    "    { 100 0 rmoveto }",
    "    ifelse",
    "  } for",
    "} def",

    /*
     * Gray out the day boxes before the first day of the month and after
     * the last day of the month.  If this is not a submonth, leave two
     * blank for the previous and next month miniature calendars.
     */
    "/drawfill {",
    "  /start startday def",
    "  1.0 setlinewidth",
    "  submonth 1 eq {",
    "   0 35 rmoveto",
    "   /grayWidth start 100 mul def",
    "   /lastday 7 def",
    "  } {",
    "   start 3 ge {",
    "     200 35 rmoveto",
    "     /grayWidth start 2 sub 100 mul def",
    "     /lastday 7 def",
    "   } {",
    "     0 35 rmoveto",
    "     /grayWidth start 100 mul def",
    "     /lastday 5 def",
    "   } ifelse",
    "  } ifelse",
    "  grayWidth 0 gt {",
    "   gsave",
    "     .9 setgray",
    "     grayWidth 0 rlineto",
    "     0 monthHeight neg rlineto",
    "     grayWidth neg 0 rlineto",
    "     closepath fill",
    "   grestore",
    "  } if",
    "  /endday startday ndays add 7 mod def",
    "  endday 0 ne {",
    "   ndays startday add 7 mod 100 mul",
    "   weeks 1 sub neg monthHeight mul 35 add moveto",
    "   /grayWidth lastday 100 mul currentpoint pop sub def",
    "   grayWidth 0 gt {",
    "    gsave",
    "      .9 setgray",
    "      grayWidth 0 rlineto",
    "      0 monthHeight neg rlineto",
    "      grayWidth neg 0 rlineto",
    "      closepath fill",
    "    grestore",
    "   } if",
    "  } if",
    "} def",

    "/isleap {",
    "  year 4 mod 0 eq",
    "  year 100 mod 0 ne",
    "  year 400 mod 0 eq or and",
    "} def",

    "/days_month [ 31 28 31 30 31 30 31 31 30 31 30 31 ] def",

    /*
     * -  -ndays-  int
     *
     * Push number of days in current month.  Account for leap February.
     */
    "/ndays {",
    "  days_month month 1 sub get",
    "  month 2 eq",
    "  isleap and { 1 add } if",
    "} def",

    /*
     * -  -startday-  int
     *
     * Push the day of the week on which the first of the current month falls.
     */
    "/startday {",
    "  /off year 2000 sub def",
    "  off",
    "  off 4 idiv add",
    "  off 100 idiv sub",
    "  off 400 idiv add",
    "  6 add 7 mod 7 add",
    "  /off exch def",
    "  1 1 month 1 sub {",
    "    /idx exch def",
    "    days_month idx 1 sub get",
    "    idx 2 eq",
    "    isleap and",
    "    { 1 add } if",
    "    /off exch off add def",
    "  } for",
    "  off 7 mod",
    "% Place holder",
    "} def",

    "/center {",
    "  /width exch def",
    "  /str exch def width str ",
    "  stringwidth pop sub 2 div 0 rmoveto str show",
    "} def",

    /*
     * Draw an entire month calendar.
     * (Without any previous/next subcalendars.)
     */
    "/calendar",
    "{",
    "  titlefont findfont 48 scalefont setfont",
    "  0 60 moveto",
    "  /month_name month_names month 1 sub get def",
    "  month_name show",
    "  /yearstring year 10 string cvs def",
    "  700 yearstring stringwidth pop sub 60 moveto",
    "  yearstring show",
    "  0 0 moveto",
    "  drawnums",
    "  0 0 moveto",
    "  drawfill",
    "  0 0 moveto",
    "  drawgrid",
    "} def",

    /*
     * array-of-notes  -daytext-  -
     */
    "/daytext {",
    "  /mytext exch def /myday exch def",
    "  /bottom monthHeight 30 sub def",
    "  startday myday 1 sub add dup",
    "  7 mod 100 mul 5 add /LM exch def",
    "  7 idiv monthHeight neg mul /ylimit exch def",
    "  ylimit bottom sub /ypos exch def",
    "  /RM LM 95 add def /ystart ypos def",
    "  mytext {",
    "    95 90 { pop pop /ystart ystart 8 add def } breakIntoLines",
    "    ystart ylimit le { /ypos ystart def } if",
    "  } forall",
    "  /ylimit ylimit bottom sub def",
    "  mytext { 95 90 { prstr } breakIntoLines } forall",
    "} def",

    /*
     * string maxwidth  -prstr-  -
     */
    "/prstr {",
    "  ypos ylimit gt {",
    "    RM exch sub ypos moveto show",
    "    /ypos ypos 8 sub def",
    "  } {",
    "   pop pop",
    "  } ifelse",
    "} def",

    /*
     * Word break string for breakIntoLines.
     */
    "/space ( ) def",

    /*
     * string  first-width  next-width  proc  -breakIntoLines-  -
     *
     * Break the string into lines.  The first line will fit within
     * first-width.  Later lines will fit within next-width.  For each
     * line, push the string and current width then execute proc.
     *
     * This is a modification of the function listed in the Blue book
     * (Postscript Language Tutorial and Cookbook, Adobe Systems, Inc.).
     * The modifications are:
     *	1.	The addition of the next-width parameter to handle indentation.
     *  2.	The original would not break the line if only the last word
     *		extended beyond the limit.
     */
    "/breakIntoLines {",
    "  /proc exch def",
    "  /nextlinewidth exch def",
    "  /linewidth exch def",
    "  /textstring exch def",
    "  /breakwidth space stringwidth pop def",
    "  /curwidth 0 def",
    "  /lastwordbreak 0 def",
    "  /startchar 0 def",
    "  /restoftext textstring def",
    "  {",
    "   restoftext space search {",
    "     /nextword exch def pop",
    "     /restoftext exch def",
    "      /wordwidth nextword stringwidth pop def",
    "      curwidth wordwidth add linewidth gt {",
    "           textstring startchar",
    "        lastwordbreak startchar sub",
    "        getinterval linewidth proc",
    "        /startchar lastwordbreak def",
    "        /curwidth wordwidth breakwidth add def",
    "           /linewidth nextlinewidth def",
    "      } {",
    "           /curwidth curwidth wordwidth add breakwidth add def",
    "     } ifelse",
    "      /lastwordbreak lastwordbreak nextword length add 1 add def",
    "    } {",
    "     stringwidth pop curwidth add linewidth gt {",
    "           textstring startchar",
    "           lastwordbreak startchar sub",
    "           getinterval linewidth proc",
    "        /startchar lastwordbreak def",
    "           /linewidth nextlinewidth def",
    "     } if",
    "     exit",
    "   }",
    "    ifelse",
    "  } loop",
    "  /lastchar textstring length def",
    "  textstring startchar lastchar startchar sub getinterval linewidth proc",
    "} def",

    "/printmonth {",
    "  90 rotate",
    "  50 -120 translate",
    "  /submonth 0 def",
    "  calendar",
    "  month 1 sub 0 eq {",
    "    /lmonth 12 def",
    "    /lyear year 1 sub def",
    "  } {",
    "    /lmonth month 1 sub def",
    "    /lyear year def",
    "  } ifelse",
    "  month 1 add 13 eq {",
    "    /nmonth 1 def",
    "    /nyear year 1 add def",
    "  } {",
    "    /nmonth month 1 add def",
    "    /nyear year def",
    "  } ifelse",
    "  /savemonth month def",
    "  /saveyear year def",
    "  /submonth 1 def",
    "  gsave",
    "    /offset monthHeight 80 sub 2 div neg 35 add def",
    "    startday 3 lt",
    "    { 500 weeks 1 sub neg monthHeight mul offset add translate }",
    "    { 0  offset translate }",
    "    ifelse",
    "   /year lyear def",
    "   /month lmonth def",
    "    gsave",
    "      .138 .138 scale",
    "      10 -120 translate",
    "      calendar",
    "    grestore",
    "    /submonth 1 def",
    "    /year nyear def",
    "    /month nmonth def",
    "    100 0 translate",
    "    gsave",
    "      .138 .138 scale",
    "      10 -120 translate",
    "      calendar",
    "    grestore",
    "    /month savemonth def",
    "    /year saveyear def",
    "    /submonth 0 def",
    "  grestore",
    "} def",
    /*
     * print a page trailer with appts file, userid and date
     */
    "/print_mtrailer {",
    "  40 -470 moveto",
    "  trailer 600 center",
    "} def",
  (char *)0,
};

#ifdef __STDC__
void print_mday (FILE *fp, int noteflag);
#else
void print_mday ();
#endif

void
print_psmonth(fp, noteflag)
FILE *fp;
int noteflag;
{
    char      **ap;
    int		i, startdow, nweeks;
    char	weeknums[70], wn[5];

    
    /*
     * muck with prolog for Monday first (ISO) weeks.
     * N.B. if the prolog changes, these indices may also change
     */
    if (monday_first) {
	pheader[11] = pmonfirst[0];
	pheader[12] = pmonfirst[1];
#ifndef SATBLK
	pheader[36] = pmonfirst[2];
	pheader[39] = pmonfirst[3];
	pheader[121] = pmonfirst[4];
#else
	pheader[36] = pmonfirst[3];
	pheader[118] = pmonfirst[4];
#endif
    }

    /*
     * Write out PostScript header
     */
    for (ap = prolog; *ap; ap++)
	fprintf(fp, "%s\n", *ap);
    fprintf (fp, "/daytextfont /%s def\n", PS_NOTE_FONT);

    /*
     * Write out PS prolog for font reencoding
     */
    for (ap = p_lang_header; *ap; ap++)
	fprintf(fp, "%s\n", *ap);

    fprintf(fp, "%%EndProlog\n");

    /*
     * Write out common core PostScript
     */
    for (ap = pheader; *ap; ap++)
	fprintf(fp, "%s\n", *ap);

    /*
     * Do the calendar
     */
    fprintf (fp, "/year %d def\n", current.tm_year+1900);
    fprintf (fp, "/month %d def\n", current.tm_mon+1);
    fprintf (fp, "printmonth\n");
    if (print_trailer) {
	fprintf(fp, "/trailer (%s) def\n", trailer);
	fprintf(fp, "print_mtrailer\n");
    }

    First = current;

    /*
     *  Calculate and print the week numbers
     */
    current.tm_mday = 1;
    fix_current_day();
    if (monday_first)
	startdow = (current.tm_wday == 0 ? 6 : current.tm_wday-1);
    else
	startdow = current.tm_wday;
    nweeks = (startdow + monthlength(current.tm_mon) > 35 ? 6 : 5);
    strcpy(weeknums, "/weeknums [");
    for(i=0; i<nweeks; i++) {
	sprintf(wn, " (%2d)", week_number());
	strcat(weeknums, wn);
	current.tm_mday += 7;
	fix_current_day();
    }
    strcat( weeknums, " ] def");
    fprintf(fp, "/weekn {\n");
    fprintf(fp, "  %s\n", weeknums);
    fprintf(fp, "  /Helvetica-Bold findfont 16 scalefont setfont\n");
    fprintf(fp, "  /wy -10 def\n");
    if (monday_first)
	fprintf(fp, "  0 1 weeks 1 sub { 710 wy moveto\n");
    else
	fprintf(fp, "  0 1 weeks 1 sub { -35 wy moveto\n");
    fprintf(fp, "    weeknums exch get show\n");
    fprintf(fp, "    /wy wy monthHeight sub def\n");
    fprintf(fp, "  } for\n");
    if (monday_first)
	fprintf(fp, "  705 40 moveto\n");
    else
	fprintf(fp, "  -40 40 moveto\n");
    fprintf(fp, "  /Helvetica-Bold findfont 10 scalefont setfont\n");
    fprintf(fp, "  (Week) show\n");
    fprintf(fp, "} def\n");
    fprintf(fp, "weekn\n");
    current = First;

    /*
     * Set the font here to reduce the number of complainings if
     * it is not found.
     */
    fprintf (fp, "daytextfont findfont 6 scalefont setfont\n");

    current.tm_mday = 1;
    for (i=0; i<monthlength(current.tm_mon); i++) {
	fix_current_day();
	(void)get_day_appts();
	print_mday(fp, noteflag);
	current.tm_mday++;
    }
    current = First;

    /*
     * Write out PostScript postlog
     */
    fprintf(fp, "showpage\n");
}

void
print_mday(fp, noteflag)
FILE *fp;
int noteflag;
{
    int	 slotno;
    struct appt_entry	*aptr, *optr;
    struct dayslot *slptr;
    
    fprintf(fp, "%d [\n", current.tm_mday);

    for (slotno=0; slotno<n_slots; slotno++) {
	/* any appts in this timeslot? */
	slptr = &slots[slotno];
	if (slptr->active) {
	    /* get printable string from each appt */
	    for (aptr=slptr->first; aptr;) {
		if (!deleted(aptr, slptr) && 
		    (!noteflag || ((aptr->flags & MARKED_NOTE) != MARKED_NOTE)))
			fprintf(fp, "    (%s)\n", format_appt_nd(aptr, TRUE));
		/* free up memory used */
		optr = aptr;
		aptr = aptr->next;
		free(optr);
	    }
	}
    }
    fprintf(fp, "] daytext\n");
}
#endif /* RASTER_ONLY */
#endif /* NO_PRINTER */
