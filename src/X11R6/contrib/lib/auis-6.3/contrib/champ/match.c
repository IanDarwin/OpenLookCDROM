/* ********************************************************************** *\
 *         Copyright IBM Corporation 1988,1991 - All Rights Reserved      *
 *        For full copyright information see:'andrew/config/COPYRITE'     *
\* ********************************************************************** */

/*
	$Disclaimer: 
 * Permission to use, copy, modify, and distribute this software and its 
 * documentation for any purpose is hereby granted without fee, 
 * provided that the above copyright notice appear in all copies and that 
 * both that copyright notice, this permission notice, and the following 
 * disclaimer appear in supporting documentation, and that the names of 
 * IBM, Carnegie Mellon University, and other copyright holders, not be 
 * used in advertising or publicity pertaining to distribution of the software 
 * without specific, written prior permission.
 * 
 * IBM, CARNEGIE MELLON UNIVERSITY, AND THE OTHER COPYRIGHT HOLDERS 
 * DISCLAIM ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING 
 * ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS.  IN NO EVENT 
 * SHALL IBM, CARNEGIE MELLON UNIVERSITY, OR ANY OTHER COPYRIGHT HOLDER 
 * BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY 
 * DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, 
 * WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS 
 * ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE 
 * OF THIS SOFTWARE.
 *  $
*/

#ifndef NORCSID
#define NORCSID
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/contrib/champ/RCS/match.c,v 1.3 1992/12/15 21:49:28 rr2b R6tape $";
#endif

#include <andrewos.h>  /* time.h */
#include <champ.h>

int MonthLengths[] = {31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31};

#ifndef TESTINGHEBREW
void ClearAllFlaggedEvents() {
    struct eventnode *en;

    for(en = RootEventNode; en; en=en->next) {
	en->flagged = 0;
    }
}

void IterateFlaggedEvents(proc, rock)
int (*proc)();
long rock;
{
    struct eventnode *en;
    for(en = RootEventNode; en; en=en->next) {
	if (en->flagged) {
	    (*proc)(en, rock);
	}
    }
}


int FlagEventsMatchingDate(date)
struct tm *date;
{
    struct eventnode *en;
    int total = 0;

    for(en = RootEventNode; en; en=en->next) {
	if (matchdate(date, &en->ds) == CHAMPERR_NOERR) {
	    en->flagged = 1;
	    ++total;
	}
    }
    return(total);
}

int matchdate(date, spec)
struct tm *date;
struct datespec *spec;
{
    switch(spec->calsys) {
	case CALSYS_GREGORIAN:
	    return(matchgregoriandate(date, &spec->sys.gd));
	case CALSYS_HEBREW:
	    return(matchhebrewdate(date, &spec->sys.hd));
	case CALSYS_ECCLESIASTICAL:
	    return(matchecclesiasticaldate(date, &spec->sys.ed));
	default:
	    return(CHAMPERR_BADCALSYS);
    }
}

static int matchgregoriandate(date, ds)
struct tm *date;
struct gregoriandatespec *ds;
{
    int rightweek=0, mlen;

    if (ds->year >= 0 && ds->year != date->tm_year) return(CHAMPERR_NOMATCH);
    if (ds->month >= 0 && ds->month != date->tm_mon) return(CHAMPERR_NOMATCH);
    if (ds->day >= 0 && ds->day != date->tm_mday) return(CHAMPERR_NOMATCH);
#ifdef IGNORINGNOW
    if (ds->hour >= 0 && ds->hour != date->tm_hour) return(CHAMPERR_NOMATCH);
    if (ds->min >= 0 && ds->min != date->tm_min) return(CHAMPERR_NOMATCH);
#endif /* IGNORINGNOW */
    if (ds->wkday >= 0 && ds->wkday != date->tm_wday) return(CHAMPERR_NOMATCH);
    if (ds->wkdayselector > 5) {
	mlen = MonthLengths[date->tm_mon];
	if (date->tm_mon == 1
	    && (date->tm_year%4 != 0 || (date->tm_year%100 == 0 && (date->tm_year+1900)%400 != 0))) {
	    --mlen;
	}
	if ((mlen - date->tm_mday) < 7) {
	    rightweek = 1;
	}
    }
    if (ds->wkday >= 0
	 && ds->wkdayselector >= 0
	 && !rightweek
	 && (ds->wkdayselector != (date->tm_mday-1)/7)) {
	return(CHAMPERR_NOMATCH);
    }
    return(CHAMPERR_NOERR);
}

IncrementDate(d)
struct tm *d;
{
    ++d->tm_yday;
    if (++d->tm_wday > 6) d->tm_wday = 0;
    ++d->tm_mday;
    if ((d->tm_mday > MonthLengths[d->tm_mon])
	 || (d->tm_mon == 1 && ((d->tm_year%4 != 0 || (d->tm_year%100 == 0 && (d->tm_year+1900)%400 != 0))
	     && (d->tm_mday == 29)))) {
	++d->tm_mon;
	d->tm_mday = 1;
	if (d->tm_mon >= 12) {
	    d->tm_mon = 0;
	    ++d->tm_year;
	    d->tm_yday = 0; /* IS THIS RIGHT OR SHOULD IT BE 1??? */
	}
    }
}

/* This struct gives, for some Year, the number of days in the year, plus the day-in-year for several landmark dates in the ecclesiastical calendar for that year.  The only anomaly is that ChristmasSunday, the Sunday past Christmas, may occur on January 1 of the following year, but is treated as if it occurred in the current year; that is, its value may be 365 or 366. */
struct EcclData {int Year, DaysInYear, Advent1, Christmas, ChristmasSunday, EpiphanySunday, Easter;};
#define	EPIPHANY_DAY	5   /* January 6 */
#define	SEPTUAGESIMA_OFFSET -63	/* 63 days before Easter */
#define	PENTECOST_OFFSET	49  /* 49 days after Easter */
#define	TRINITY_OFFSET	56  /* 56 days after Easter */
#define	CHRISTMAS_DAY_BASE  358	/* in non-leap years */

static int isLeap(Year)
int Year;
{/* Return TRUE iff Year is a leap year. */
    if ((Year % 4) != 0) return 0;
    else if ((Year % 100) != 0) return 1;
    else return ((Year % 400) == 0);
}

static struct EcclData *GetEccl(year)
int year;
{/* Returns the EcclData for the given year, possibly cacheing the result. */
#define NUMECCLS 8
    static struct EcclData Eccls[NUMECCLS] = {0};
    static int numE = 0;
    int G, C, X, Z, D, E, N;	/* Algorithm straight from Knuth vol 1. */
    int ix, YesLeap;

    if (year < 1900) year += 1900;  /* this arithmetic in true years */
    if (year != 0) for (ix = 0; ix < NUMECCLS; ++ix)
	if (year == Eccls[ix].Year) return &Eccls[ix];

    ix = numE;
    ++numE; if (numE >= NUMECCLS) numE = 0;	/* Wrap the cache index for next time. */

    G = (year % 19) + 1;	/* ``Golden Number'' */
    C = (year/100)	+ 1;	/* ``century'', sort of */
    X = ((3*C) / 4) - 12;	/* Count of dropped leap years */
    Z = ((8*C+5)/25) - 5;	/* Moon-orbit static correction */
    D = ((5*year)/4) - X - 10;
    E = (11*G + 20 + Z - X);
    while (E < 0) E += 30;
    E = E % 30;
    if ((E == 24) || (E == 25 && G > 11)) ++E;	/* ``Epact'' */
    N = 44 - E;
    if (N < 21) N += 30;	/* Paschal full moon */
    N = N + 7 - ((D+N) % 7);	/* Advance to next Sunday. */

    YesLeap = (isLeap(year) ? 1 : 0);

    Eccls[ix].Year = year;
    Eccls[ix].DaysInYear = 365 + YesLeap;
    Eccls[ix].Advent1 = 336 - ((D+5) % 7) + YesLeap;	/* 11/27 thru 12/3 */
    Eccls[ix].Christmas = CHRISTMAS_DAY_BASE + YesLeap;
    Eccls[ix].ChristmasSunday = 365 - ((D+6) % 7) + YesLeap;	/* 12/26 thru 1/1 (12/32) */
    Eccls[ix].EpiphanySunday = 12 - ((D-YesLeap+3) % 7);	/* 1/7 thru 1/12 */
    Eccls[ix].Easter = N+58+YesLeap;

    return &Eccls[ix];
}

static int matchecclesiasticaldate(date, ds)
struct tm *date;
struct ecclesiasticaldatespec *ds;
{
    struct EcclData *e;

    if (ds->year >= 0 && ds->year != date->tm_year) return(CHAMPERR_NOMATCH);
#ifdef IGNORINGNOW
    if (ds->hour >= 0 && ds->hour != date->tm_hour) return(CHAMPERR_NOMATCH);
    if (ds->min >= 0 && ds->min != date->tm_min) return(CHAMPERR_NOMATCH);
#endif /* IGNORINGNOW */
    e = GetEccl(date->tm_year);
    switch (ds->landmark) {
	case 1:	/* First Sunday in Advent. */
	    if (date->tm_yday != (e->Advent1 + ds->offset)) return(CHAMPERR_NOMATCH);
/* Ensure that it's between Pentecost and Christmas. */
	    if (date->tm_yday <= (e->Easter + PENTECOST_OFFSET)
		|| date->tm_yday >= (e->Christmas)) return(CHAMPERR_NOMATCH);
	    break;
	case 2:	/* First Sunday after Christmas */
/* Ensure that it's between Christmas and Epiphany. */
	    if (date->tm_yday >= EPIPHANY_DAY && date->tm_yday < e->Christmas) return (CHAMPERR_NOMATCH);
	    if (date->tm_yday >= EPIPHANY_DAY) {	/* now know the correct year */
		if (date->tm_yday != (e->ChristmasSunday + ds->offset)) return (CHAMPERR_NOMATCH);
	    } else {
		e = GetEccl(date->tm_year - 1);	/* Get last year's info. */
		if ((date->tm_yday + e->DaysInYear) != (e->ChristmasSunday + ds->offset)) return (CHAMPERR_NOMATCH);
	    }
	    break;
	case 3:	/* First Sunday after Epiphany */
	    if (date->tm_yday != (e->EpiphanySunday + ds->offset)) return (CHAMPERR_NOMATCH);
/* Ensure that it's between Epiphany and Septuagesima. */
	    if (date->tm_yday < EPIPHANY_DAY || date->tm_yday >= (e->Easter + SEPTUAGESIMA_OFFSET)) return (CHAMPERR_NOMATCH);
	    break;
	case 4:	/* Easter Sunday */
	    if (date->tm_yday != (e->Easter + ds->offset)) return (CHAMPERR_NOMATCH);
/* Ensure that it's between Septuagesima and Advent. */
	    if (date->tm_yday < (e->Easter + SEPTUAGESIMA_OFFSET) || date->tm_yday >= e->Advent1) return (CHAMPERR_NOMATCH);
	    break;
	case 5:	/* Pentecost Sunday */
	    if (date->tm_yday != (e->Easter + PENTECOST_OFFSET + ds->offset)) return (CHAMPERR_NOMATCH);
/* Ensure that it's between Septuagesima and Advent. */
	    if (date->tm_yday < (e->Easter + SEPTUAGESIMA_OFFSET) || date->tm_yday >= e->Advent1) return (CHAMPERR_NOMATCH);
	    break;
	case 6:	/* Trinity Sunday */
	    if (date->tm_yday != (e->Easter + TRINITY_OFFSET + ds->offset)) return (CHAMPERR_NOMATCH);
/* Ensure that it's between Septuagesima and Advent. */
	    if (date->tm_yday < (e->Easter + SEPTUAGESIMA_OFFSET) || date->tm_yday >= e->Advent1) return (CHAMPERR_NOMATCH);
	    break;
	default:
	    return(CHAMPERR_NOMATCH);
    }
/* printf("year %d: yday %d, month %d, mday %d matches (%d)+%d\n",
 date->tm_year, date->tm_yday, date->tm_mon, date->tm_mday, ds->landmark,
 ds->offset); */
/* printf("  (%d: EpSun %d, Easter %d, AdvSun %d, ChristmasSun %d)\n", e->Year,
 e->EpiphanySunday, e->Easter, e->Advent1, e->ChristmasSunday); */
    return(CHAMPERR_NOERR);
}
#endif /* TESTINGHEBREW */

/* There are 6 kinds of years in the Hebrew calendar.  The second and fifth lines
  in the array are the "normal" non-leap and leap years.  In either kind of year,
  a day may also be added to Heshvan or subtracted from Kislev, yielding 6 year 
  types in all. */

static int HebrewMonthLengths[][13] = {
    {30, 29, 29, 29, 30, 29,  0, 30, 29, 30, 29, 30, 29},
    {30, 29, 30, 29, 30, 29,  0, 30, 29, 30, 29, 30, 29},
    {30, 30, 30, 29, 30, 29,  0, 30, 29, 30, 29, 30, 29},
    {30, 29, 29, 29, 30, 30, 29, 30, 29, 30, 29, 30, 29},
    {30, 29, 30, 29, 30, 30, 29, 30, 29, 30, 29, 30, 29},
    {30, 30, 30, 29, 30, 30, 29, 30, 29, 30, 29, 30, 29}
};

static int YearLengths[] = {353, 354, 355, 383, 384, 385};

/* The dates are calculated as offsets from January 1, 1970, of course.  Thus we
  need a few constants to define the Hebrew date for that date (Tevet 23, 5730) */

#define STARTYEAR 5730
#define STARTMONTH 3 /* Tevet */
#define STARTDAY 23
#define DAYSLEFTINSTARTYEAR 272

/* A table defining the type of year for every year from 5730 (1969-70) to 5780 (2019-2020)
    as indices into the HebrewMonthLenghts arrays (recall there are 6 types of years */

static char YearTypes[] = {
/* 5730 */    3, 1, 2, 3, 2, 1, 5, 0, 4, 2,
/* 5740 */    2, 3, 1, 2, 5, 1, 3, 2, 1, 3,
/* 5750 */    2, 1, 5, 0, 2, 4, 2, 3, 1, 2,
/* 5760 */    5, 0, 1, 5, 2, 3, 1, 2, 3, 1,
/* 5770 */    2, 5, 1, 0, 5, 1, 5, 0, 1, 5,
/* 5780 */    2};


/* In general, you get the Hebrew year from the Gregorian year by adding 3760.  However, if
    Rosh Hashanah has already occurred (i.e. it is fall or early winter), you add 3761. */

static int matchhebrewdate(date, hds)
struct tm *date;
struct hebrewdatespec *hds;
{
    static struct hebrewdatespec myhd;
    static struct tm mytm;
    int MonthOK = 0, yearspec, monthspec, dayspec, thisyeartype, thismonthlen;

    if (bcmp(&mytm, date, sizeof(struct tm))) {
	TranslateTmToHebrew(date, &myhd);
    }
    if (hds->year < 0) {
	yearspec = myhd.year;
    } else {
	yearspec = hds->year;
    }
    if (hds->month < 0) {
	monthspec = myhd.month;
    } else {
	monthspec = hds->month;
    }
    /* Let the date spec wrap a bit into the next month */
    dayspec = hds->day;
    thisyeartype = YearTypes[yearspec - STARTYEAR];
    thismonthlen = HebrewMonthLengths[thisyeartype][monthspec];
    if ((thismonthlen > 0) && (dayspec > thismonthlen)) { 
	dayspec -= thismonthlen;
	monthspec++;
	if (HebrewMonthLengths[thisyeartype][monthspec] == 0) ++monthspec;
	if (monthspec > 12) {
	    monthspec = 0;
	    if (hds->year >= 0) ++yearspec;
	}
    }
    if (yearspec != myhd.year) return(CHAMPERR_NOMATCH);
    /* Special check for Adar II dates in non-leap years */

    if (monthspec == 6) {
	if (myhd.month == 5) {
	    if (HebrewMonthLengths[YearTypes[myhd.year - STARTYEAR]][6] == 0) {
		MonthOK = 1;
	    }
	 }
    }
	    
    if (!MonthOK && monthspec != myhd.month) return(CHAMPERR_NOMATCH);
    if (dayspec >= 0 && dayspec != myhd.day) return(CHAMPERR_NOMATCH);
    return(CHAMPERR_NOERR);
}

TranslateTmToHebrew(date, hebdate)
struct tm *date;
struct hebrewdatespec *hebdate;
{
    int day = date->tm_mday - 1;
    int year, month, i;

    for (i=0; i<date->tm_mon; ++i) {
	day += MonthLengths[i];
	if (date->tm_mon == 1
	    && (date->tm_year%4 != 0 || (date->tm_year%100 == 0 && date->tm_year%400 != 0))) {
	    --day;
	}
    }
    day += (date->tm_year-70) * 365 + (((date->tm_year - 1) / 4) - 17);
    if (day > DAYSLEFTINSTARTYEAR) {
	day -= DAYSLEFTINSTARTYEAR;
	year = 1;
	while (day > YearLengths[YearTypes[year]]) {
	    day -= YearLengths[YearTypes[year]];
	    year++;
	}
	month = 0;
    } else {
	year = 0;
	month = STARTMONTH;
	day += STARTDAY;
    }
    while (day > HebrewMonthLengths[YearTypes[year]][month]) {
	day -= HebrewMonthLengths[YearTypes[year]][month];
	month++;
    }
    hebdate->year = year + STARTYEAR;
    hebdate->month = month;
    hebdate->day = day;
}

#ifdef TESTINGHEBREW

/* The names of the 12 or 13 months of the Hebrew calendar */

static char *MonthNames[] = {"Tishri", "Heshvan", "Kislev", "Tevet", "Shvat", "First Adar", "Second Adar", "Nisan", "Iyyar", "Sivan", "Tammuz", "Av", "Elul", 0};

main(argc, argv) 
int argc;
char **argv;
{
    struct tm date;
    struct hebrewdatespec hd;

    if (argc < 4) {
	printf("Usage:  hebdate mon day year\n");
	exit(1);
    }
    bzero(&date, sizeof(struct tm));
    date.tm_mon = atoi(argv[1]) - 1;
    date.tm_mday = atoi(argv[2]);
    date.tm_year = atoi(argv[3]);
    if (date.tm_year > 1900) date.tm_year -= 1900;
    TranslateTmToHebrew(&date, &hd);
    printf("%d/%d/%d translates to %s %d, %d\n", date.tm_mon+1, date.tm_mday, date.tm_year, MonthNames[hd.month], hd.day, hd.year);
}
#endif /* TESTINGHEBREW */
