/*
 * $Id: datelib.c,v 2.3 1994/08/29 17:32:54 billr Exp $
 *
 * datelib.c - Calendar (date) computation library
 *
 * R. P. C. Rodgers, UCSF, November 1988
 * (with thanks to Greg Couch, Conrad Huang, and Dave Yee for their helpful
 *  suggestions)
 *
 *        Copyright 1988, 1989 R. P. C. Rodgers,  All Rights Reserved
 * Permission is granted by the author for use of this code under the following
 * conditions: 1) improvements and corrections to this code are shared with the
 * author; 2) the code is made freely available to anyone requesting it; 3) the
 * code is not used for any profit-making venture without the express
 * permission of the author; and, 3) all copies of the code will preserve the
 * above authorship and copyright information and this notice.
 *
 * (requires math library)
 * 
 * Source of formulae:
 * 
 * 1) Schocken, Wolfgang Alexander
 *    The calculated confusion of calendars; puzzles in Christian, Jewish and
 *       Moslem calendars.
 *    1st Ed.
 *    Vantage Press
 *    New York
 *    1976
 * 
 * 2) Meeus, Jean
 *    Astronomical Formulae for Calculators
 *    Monografieen over Astronomie en Astrofysica
 *    Volkssterrenwacht Urania V.Z.W.
 *    Mattheessensstraat 62, B 2540 Hove, Belgium
 *    Vereniging voor Sterrenkunde V.Z.W.
 *    Ringlaan 3, B 1180 Brussel, Belgium
 *    Vol. 4
 *    Derde Druk
 *    October 1980
 *    (3rd edition of 1985 available from Willmann-Bell, address below)
 *    (formulae for Easter, Julian and Gregorian date formation, and
 *     solstices/equinoxes)
 * 
 * 3) Assorted contributions to the Hewlett-Packard HP-41C Software Library
 * 
 * In his 1987 Sun program "moontool," John Walker (Autodesk, Sausalito, CA)
 * mentions several other potentially useful references:
 * 
 * 4) Pierre Bretagnon and Jean-Louis Simon
 *    Planetary Programs and Tables from -4000 to +2800
 *    Willmann-Bell
 *    1986
 *    (for utmost accuracy in planetary computations)
 * 
 * 5) Eric Burgess
 *    Celestial BASIC
 *    Revised Edition
 *    Sybex
 *    1985
 *    (cookbook oriented, many algorithms hard to dig out of turgid BASIC)
 * 
 * Many of these references can be obtained from Willmann-Bell, P.O. Box
 * 35025, Richmond, VA 23235, USA.  Phone: (804) 320-7016.  In addition
 * to their own publications, they stock most of the standard references
 * for mathematical and positional astronomy.
 * 
 * NOTES:
 * check ranges on days (0-365)
 * islamic new year and jewish dates not thoroughly tested
 * 
 */

#include "ct.h"		/* for the NO_HOLIDAYS #define */

#include	<stdio.h>	/* for NULL */
#include	<math.h>

double	julian_day();

#ifndef NO_HOLIDAYS
extern	char	*malloc();
double  easter_offset(), passover_offset();

static char	*monthname[] = { 
#ifdef FRENCH
		"Janvier", "Fevrier", "Mars", "Avril", "Mai",
		"Juin", "Juillet", "Aout", "Septembre",
		"Octobre", "Novembre", "Decembre"	};
#else
# ifdef SWEDISH
		"Januari", "Februari", "Mars", "April", "Mai",
		"Juni", "Juli", "Augusti", "September",
		"Oktober", "November", "December"	};
# else
		"January", "February", "March", "April", "May",
		"June", "July", "August", "September",
		"October", "November", "December"	};
# endif /* SWEDISH */
#endif /* FRENCH */
static char	*dayname[] = {
#ifdef FRENCH
		"Dimanche", "Lundi", "Mardi", "Mercredi",
		"Jeudi", "Vendredi", "Samedi"		};
#else
# ifdef SWEDISH
		"Sondag", "Mandag", "Tisdag", "Onsdag",
		"Torsdag", "Fredag", "Lordag"		};
# else
		"Sunday", "Monday", "Tuesday", "Wednesday",
		"Thursday", "Friday", "Saturday"	};
# endif /* SWEDISH */
#endif /* FRENCH */
static char	timebuf[16];
static double	passoverJD, easterJD;
static int	passoverJY;

#ifdef __STDC__
void gregorian_date (double *p_day, int *p_month, int *p_year, double jday);
int wday (int day_of_week, double day, int month, int year);
int wday2 (double day, int month, int year);
int dday (double day, int month, int year);
#else
void gregorian_date ();
int wday ();
int wday2 ();
int dday ();
#endif

/*
 * date_string:
 * Return date string of form: "Monday 12 December 1988"
 * given day of week (0-6), day (1-31), month (1-12), year
 */
char *
date_string(day_of_week, day, month, year)
	double	day;
	int	day_of_week, month, year;
{
	char *date;

	date = (char *) malloc(81 * sizeof(char));
	if (date == NULL)
		err_rpt("out of memory", FATAL);
	(void) sprintf(date, "%s %d %s %d",
		dayname[day_of_week], (int) day, monthname[--month], year);
	return date;
}

/*
 * date_string_2:
 * Return date string of form: "Monday 12 December 1988 (NYEAR)"
 * given day of week (0-6), day (1-31), month (1-12), year, and alternate
 * (that is, non-Gregorian) year, NYEAR
 */
char *
date_string_2(day_of_week, day, month, year, nyear)
	double	day;
	int	day_of_week, month, nyear, year;
{
	char *date;

	date = (char *) malloc(81 * sizeof(char));
	if (date == NULL)
		err_rpt("out of memory", FATAL);
	(void) sprintf(date, "%s %d %s %d (%d)",
		dayname[day_of_week], (int) day, monthname[--month],
			year, nyear);
	return date;
}

/*
 * date_time_string:
 * Return date/time string of form: "10:42 Monday 12 December 1988"
 * given hour (0-24), minute (0-59), day of week (0-6), day (1-31),
 * month (1-12), year
 */
char *
date_time_string(hour, min, day_of_week, day, month, year)
	double	day;
	int	day_of_week, hour, min, month, year;
{
	char *date;

	date = (char *) malloc(81 * sizeof(char));
	if (date == NULL)
		err_rpt("out of memory", FATAL);
	(void) sprintf(date, "%02d:%02d %s %d %s %d",
		hour, min, dayname[day_of_week], (int) day,
		monthname[--month], year);
	return date;
}

/*
 * election_day:
 * Compute date of Election Day given year (1584-9999)
 * Election day is defined as the first Tuesday following the first
 * Monday in November.
 */
double
election_day(year)
	int	year;
{
	double day, nth_mday_of_month();

	/* find first Tuesday in Nov. */
	day = nth_mday_of_month(1, 2, 11, year);
	/* if it's the first day of the month then Election day is next week */
	if (day == 1.)
		day = 8.;
	return julian_day(day, 11, year);
}

/*
 * dday2:
 * Compute dday2 for various holiday routines given x value, year (>1583)
 */
int
dday2(x, year)
	int	x, year;
{
	int	atmp, btmp;

	atmp = 1.25 * year;
	btmp = year / 100;
	btmp = 0.75 * (1 + btmp);
	return (x + atmp - btmp) % 7;
}
#endif	/* NO_HOLIDAYS */

/*
 * days_remaining_in_year:
 * Compute remaining days of year (0-365)
 * given day (1-31), month (1-12), year (1901-2009)
 */
int
days_remaining_in_year(day, month, year)
	double	day;
	int	month, year;
{
	int	length_of_year(), day_of_year();

	return length_of_year(year) - day_of_year(day, month, year);
}

/*
 * length_of_year:
 * Compute days in year (365 or 366)
 * given year (1901-2009)
 */
int
length_of_year(year)
	int	year;
{
	int	ylength;

	if ((year % 400) == 0)
		ylength = 366;
	else if ((year % 100) == 0)
		ylength = 365;
	else if ((year % 4) == 0)
		ylength = 366;
	else
		ylength = 365;
	return ylength;
}

/*
 * get_day_of_week:
 * Compute day of week (0-6 for Sunday-Saturday)
 * given day (1-31), month (1-12), year (1901-2009)
 */
int
get_day_of_week(day, month, year)
	double	day;
	int	month, year;
{
	int	atmp;

	atmp = julian_day(day, month, year) + 1.5;
	return atmp % 7;
}

/*
 * day_of_year:
 * Compute day of year (1-366)
 * given day (1-31), month (1-12), year (1901-2009)
 */
int
day_of_year(day, month, year)
	double	day;
	int	month, year;
{
	return (int) julian_day(day, month, year)
		- (int) julian_day(0.0, 1, year);
}

/*
 * nth_mday_of_month:
 * Compute nth m-day of the month (1-31)
 * given n (1-5), day of week (0-6, 0 for Sunday), month (1-12),
 * year (1583-9999)
 */
double
nth_mday_of_month(n, day_of_week, month, year)
	int	day_of_week, month, n, year;
{
	int	atmp, btmp, ctmp, dtmp, etmp, tmonth, tyear;

	if (month > 2) {
		tmonth = month + 1;
		tyear = year;
	}
	else {
		tmonth = month + 13;
		tyear = year - 1;
	}
	atmp = 2.6 * tmonth;
	btmp = 1.25 * tyear;
	ctmp = (tyear / 100) - 7;
	dtmp = 0.75 * ctmp;
	etmp = (day_of_week - atmp - btmp + dtmp) % 7;
	return (double) (etmp + (n * 7));
}

/*
 * julian_day:
 * Compute Julian day (>=1)
 * given day (1-31), month (1-12), year (1901-2009)
 *
 * Notes: The Gregorian reform is taken into account (that is, the day
 * following 4 October 1582 is 15 October 1582; dates on this latter day or
 * later are said to be in the Gregorian calendar).  B.C. years are counted
 * astronomically (the year prior to year 1 is year 0).  The Julian day
 * begins at GMT noon (the day is expressed in floating point).
 * Example: to obtain Julian day for 4 Jul 1979: julian_day(4.0, 7, 1979)
 */
double
julian_day(day, month, year)
	double	day;
	int	month, year;
{
	int	atmp, monthp, yearp;
	double	ctmp = 1720994.5 + day;

	if (month > 2) {
		monthp = month + 1;
		yearp = year;
	}
	else {
		monthp = month + 13;
		yearp = year - 1;
	}
	if ((year > 1582) || (year == 1582 && month >= 10)
		|| (year == 1582 && month == 10 && day >= 15)) {
		atmp = year / 100;
		ctmp += 2 - atmp + (int)(atmp / 4);
	}
	ctmp += (int)(365.25 * yearp) + (int)(30.6001 * monthp);
	return ctmp;
}

#ifndef NO_HOLIDAYS
/*
 * datelib_int:
 * Calculate often used quantities (e.g. Easter, Passover) as an
 * optimization.
 */
void
datelib_init(year)
	int	year;
{
	void passover_init(), easter_init();

	easter_init(year);
	passover_init(year);
}

/*
 * corrected_julian_day:
 * Correct Julian day (>=1) for conversion from JULIAN CALENDAR
 * to GREGORIAN CALENDAR.
 */
double
corrected_julian_day(jday)
	double	jday;
{
	double	day;
	int	atmp, btmp, month, year;

	gregorian_date(&day, &month, &year, jday);
	atmp = year / 100;
	btmp = ((3 * atmp) - 5) / 4;
	return julian_day(day, month, year) + btmp;
}

/*
 * gregorian_date:
 * Return pointers to day (1-31), month (1-12), year (1901-2009),
 * given Julian day (>=0)
 *
 * Notes: The Gregorian reform is taken into account (that is, the day
 * following 4 October 1582 is 15 October 1582; dates on this latter day or
 * later are said to be in the Gregorian calendar).  B.C. years are counted
 * astronomically (the year prior to year 1 is year 0).  The Julian day
 * begins at GMT noon.  The Julian day can be expressed in
 * floating point below to get a day result with decimal places.  This method
 * is valid only for positive Julian day numbers.
 */
void
gregorian_date(p_day, p_month, p_year, jday)
	int	*p_month, *p_year;
	double	*p_day, jday;
{
	int	atmp, btmp, ctmp, dtmp, etmp, gtmp, ztmp;
	double	ftmp;

	ztmp = jday + 0.5;
	ftmp = (jday + 0.5) - ztmp;
	if (ztmp >= 2299161) {
		gtmp = (ztmp - 1867216.25) / 36524.25;
		ctmp = gtmp / 4;
		atmp = ztmp + 1 + gtmp - ctmp;
	}
	else
		atmp = ztmp;
	btmp = atmp + 1524;
	ctmp = (btmp - 122.1) / 365.25;
	dtmp = 365.25 * ctmp;
	etmp = ((btmp - dtmp) / 30.6001);
	ztmp = 30.6001 * etmp;
	*p_day = btmp - dtmp - ztmp + ftmp;
	if (etmp > 13.5)
		*p_month = etmp - 13;
	else
		*p_month = etmp - 1;
	if (*p_month > 2.5)
		*p_year = ctmp - 4716;
	else
		*p_year = ctmp - 4715;
}

/*
 * mdays_between_dates:
 * Compute number of mdays between two dates (exclusive: don't count the
 * days themselves) given day of week (0-6, O for Sunday),
 * two sets of day (1-31), month (1-12), year (1583-9999)
 */
int
mdays_between_dates(day_of_week, day1, month1, year1, day2, month2, year2)
	int	day_of_week, month1, year1, month2, year2;
	double	day1, day2;
{
	return wday(day_of_week, day2, month2, year2)
		- wday(day_of_week, day1, month1, year1);
}

/*
 * years_date_is_mday:
 * Compute year(s) for which a given date is an m-day
 * given starting year, ending year,
 * day of week (0-6, 0 for Sunday), day, and month
 * algorithm said to be valid for 1 Mar 1900 to 28 Feb 2100.
 */
int
years_date_is_mday(day_of_week, day, month, start_year, end_year, yearlist,
	number_of_years)
	int	day_of_week, end_year, month, *number_of_years, yearlist[],
		start_year;
	double	day;
{
	int		diff, index = 0, year, tdow;
	static int	augment[] = {6, 11, 6, 5}; 

	*number_of_years = 0;
	if (start_year > end_year) return -1;
	for (year = start_year; year <= end_year; year++ ) {
		tdow = get_day_of_week(day, month, year);
		if (tdow == day_of_week) {
			yearlist[(*number_of_years)++] = year;
		}
		if (*number_of_years == 2 || *number_of_years == 3) {
			diff = yearlist[*number_of_years]
				- yearlist[*number_of_years - 1];
			if (diff == 5) {
				year++;
				index = 0;
				break;
			}
			else if (diff == 11) {
				year++;
				index = 2;
				break;
			}
		}
	}
	for ( ; year <= end_year; year++ ) {
		yearlist[(*number_of_years + 1)] =
			yearlist[*number_of_years] + augment[index++ % 4];
		(*number_of_years)++;
	}
	return 0;
}

/*
 * weekdays_between_dates:
 * Compute weekdays between any two dates
 * given two sets of day (1-31), month (1-12), year (1901-2009)
 */
int
weekdays_between_dates(day1, month1, year1, day2, month2, year2)
	int	month1, month2, year1, year2;
	double	day1, day2;
{
	return wday2(day2, month2, year2) - wday2(day1, month1, year1);
}

/*
 * wday:
 * Compute wday for mdays_between_dates routine
 * given day of week, day, month, year
 */
int
wday(day_of_week, day, month, year)
	int	day_of_week, month, year;
	double	day;
{
	int	atmp, btmp, ctmp, dtmp;

	atmp = dday(day, month, year) - day_of_week;
	btmp = atmp / 7;
	ctmp = atmp % 7;
	dtmp = 0.11 * ctmp + 0.9;
	return btmp + (0.5 * dtmp);
}

/*
 * wday2:
 * Compute wday2 for weekdays_between_dates routine
 * given day, month, year
 */
int
wday2(day, month, year)
	int	month, year;
	double	day;
{
	int	atmp, btmp, ctmp, dtmp;

	atmp = dday(day, month, year);
	btmp = atmp / 7;
	ctmp = atmp % 7;
	dtmp = 1.801 * ctmp;
	return (5 * btmp) + (0.5 * dtmp);
}

/*
 * dday:
 * Compute dday for other routines
 * given day (1-31), month (1-12), year (1901-2009)
 */
int
dday(day, month, year)
	int	month, year;
	double	day;
{
	int	atmp, btmp, ctmp, dtmp, tmonth, tyear;

	if (month > 2) {
		tmonth = month + 1;
		tyear = year;
	}
	else {
		tmonth = month + 13;
		tyear = year - 1;
	}
	atmp = tyear / 100;
	btmp = 0.75 * (atmp - 7);
	ctmp = 365.25 * tyear;
	dtmp = 30.6001 * tmonth;
	return (int) day - btmp + ctmp + dtmp;
}

/*
 * vernal_equinox:
 * Compute Julian day for Vernal (March) Equinox given year (1901-2009)
 */
double
vernal_equinox(year)
	int	year;
{
	double	solstice_equinox_exact();

	return solstice_equinox_exact(0, year);
}

/*
 * summer_solstice:
 * Compute Julian day for Summer Solstice (June) given year (1901-2009)
 */
double
summer_solstice(year)
	int	year;
{
	double	solstice_equinox_exact();

	return solstice_equinox_exact(1, year);
}

/*
 * autumn_equinox:
 * Compute Julian day for Autumnal (September) Equinox given year (1901-2009)
 */
double
autumn_equinox(year)
	int	year;
{
	double	solstice_equinox_exact();

	return solstice_equinox_exact(2, year);
}

/*
 * winter_solstice:
 * Compute Julian day for Winter (December) Solstice given year (1901-2009)
 */
double
winter_solstice(year)
	int	year;
{
	double	day, jday;
	int	month, nyear;
	double	solstice_equinox_exact();

	jday = solstice_equinox_exact(3, year);
	gregorian_date(&day, &month, &nyear, jday);
	if (nyear == year)
		return jday;
	else
		return solstice_equinox_exact(3, (year + 1));
}

/*
 * solstice_equinox__exact:
 * Compute more precise date for Solstice/Equinox
 * given code (0-3, 0 for Vernal Equinox), year (1901-2009)
 */
double
solstice_equinox_exact(code, year)
	int	code, year;
{
	int	count;

	double	corr, jday_app;
	double	solstice_equinox_approx(), solstice_equinox_correction();

	/* compute first approximation to Julian day */
	jday_app = solstice_equinox_approx(code-1, year);
	/* iteratively recompute corrected Julian day */
	for(count = 0; count < 15; count++) {
		corr = solstice_equinox_correction(jday_app, code);
		jday_app += corr;
		if (fabs(corr) < 0.00001)
			break;
	}
	return jday_app;
}

/*
 * solstice_equinox_correction:
 * Compute correction for Solstice/Equinox Julian day
 * approximate Julian day, code (0-3, 0 for Vernal Equinox)
 */
double
solstice_equinox_correction(jday, code)
	int	code;
	double	jday;
{
	double	apparent_longitude(), sin_degrees();

	return(58.0 * sin_degrees((code * 90.0) - apparent_longitude(jday)));
}

/*
 * apparent_longitude:
 * Compute apparent longitude (true equinox) of Sun for
 * Solstice/Equinox given approximate Julian day
 */
double
apparent_longitude(jday)
	double	jday;
{
	double	btmp, ctmp, dtmp, etmp, ftmp;
	double	sin_degrees();

	btmp = (jday - 2415020.0) / 36525.0;	/* time in Julian centuries: */
						/* epoch 0.5 January 1900 */
	ctmp = 279.69668 + (36000.76892 * btmp) /* geometric mean longitude */
		+ (0.0003025 * btmp * btmp);	/* (mean equinox of the date) */
	dtmp = 358.47583 + (35999.04975 * btmp)	/* mean anomaly of Sun */
		+ (0.00015 * btmp * btmp)
		+ (0.0000033 * btmp * btmp * btmp);
	etmp = (1.919460 - (0.004789 * btmp)	/* Sun's eqn of the center */
		- (0.000014 * btmp)) * sin_degrees(dtmp)
		+ (0.020094 - (0.0001 * btmp)) * sin_degrees(2 * dtmp)
		+ 0.000293 * sin_degrees(3 * dtmp);
	ftmp = ctmp + etmp;			/* Sun's true longitude */
	return ftmp - 0.00569			/* app. long. of Sun */
		- 0.00479 * sin_degrees(259.18	/* (true equinox of the date) */
		- (1934.142 * btmp));
}

/*
 * sin_degrees:
 * Compute sin for argument in degrees
 */
double
sin_degrees(degrees)
#define	PI_CORR	0.01745329252				/* pi / 180 */
	double	degrees;
{
	return sin(degrees * PI_CORR);
}

/*
 * solstice_equinox_approx:
 * Compute approximate date for Solstice/Equinox
 * given code (0-3, 0 for Vernal Equinox), year (1901-2009)
 */
double
solstice_equinox_approx(code, year)
	int	code, year;
{
	return (365.2422 * (year + (code / 4))) + 1721141.3;
}

/*
 * easter:
 * Return Julian date for Easter, given year (1901-2009)
 *
 * Offered in the book of Meeus, which cites:
 *
 * 1) Spencer Jones
 *    General Astronomy
 *    1922
 *    pg. 73-74
 *
 * 2) Journal of the British Astronomical Association
 *    Vol. 8
 *    Pg. 91
 *    Dec. 1977
 *    (which attributes method to Butcher's Ecclesiastical Calendar of 1876)
 *
 * Method valid for all dates in the Gregorian calendar
 * (from 15 October 1583 on)
 */
void
easter_init(year)
	int	year;
{
	double	day;
	int	atmp, btmp, ctmp, dtmp, etmp, ftmp,
		gtmp, htmp, itmp, ktmp, ltmp, mtmp;
	int	month;

	atmp = year % 19;
	btmp = year / 100;
	ctmp = year % 100;
	dtmp = btmp / 4;
	etmp = btmp % 4;
	ftmp = (btmp + 8) / 25;
	gtmp = (btmp - ftmp + 1) / 3;
	htmp = ((19 * atmp) + btmp - dtmp - gtmp + 15) % 30;
	itmp = ctmp / 4;
	ktmp = ctmp % 4;
	ltmp = (32 + (2 * etmp) + (2 * itmp) - htmp - ktmp) % 7;
	mtmp = (atmp + (11 * htmp) + (22 * ltmp)) / 451;
	month = (htmp + ltmp - (7 * mtmp) + 114) / 31;
	day = ((htmp + ltmp - (7 * mtmp) + 114) % 31) + 1;
	easterJD = julian_day(day, month, year);
}

double
easter(year)
	int	year;
{
	return easterJD;
}

/*
 * first_sunday_advent:
 * Christian holidays: compute Julian day for First Sunday in Advent
 * (closest Sunday to St. Andrew's day, the last day in November)
 * given year (>1583)
 */
double
first_sunday_advent(year)
	int	year;
{
	int	atmp;

	atmp = get_day_of_week(30.0, 11, year);
	if (atmp <= 3) {
		return julian_day((30.0 - (double) atmp), 11, year);
	}
	else {
		return julian_day(30.0, 11, year) + (7 - atmp);
	}
}

/*
 * easter_offset:
 * Christian holidays: compute Julian day as offset from Easter
 * given year (>1583)
 */
double
easter_offset(offset, year)
	double	offset;
	int	year;
{
	return easterJD + offset;
}

/*
 * septuagesima:
 * Christian holidays: compute Julian day for Septuagesima Sunday
 * (Third Sunday before Lent)
 * given year (>1583)
 */
double
septuagesima(year)
	int	year;
{
	return easter_offset(-63.0, year);
}

/*
 * sexagesima:
 * Christian holidays: compute Julian day for Sexagesima Sunday
 * (Second Sunday before Lent)
 * given year (>1583)
 */
double
sexagesima(year)
	int	year;
{
	return easter_offset(-56.0, year);
}

/*
 * quinquagesima:
 * Christian holidays: compute Julian day for Quinquagesima (Shrove) Sunday
 * (Sunday before Lent begins on Ash Wednesday)
 * given year (>1583)
 */
double
quinquagesima(year)
	int	year;
{
	return easter_offset(-49.0, year);
}

/*
 * shrove_monday:
 * Christian holidays: compute Julian day for Shrove Monday
 * (Two days before Lent begins on Ash Wednesday)
 * given year (>1583)
 */
double
shrove_monday(year)
	int	year;
{
	return easter_offset(-48.0, year);
}

/*
 * shrove_tuesday:
 * Christian holidays: compute Julian day for Shrove Tuesday
 * (Day before Lent begins on Ash Wednesday; Mardi Gras)
 * given year (>1583)
 */
double
shrove_tuesday(year)
	int	year;
{
	return easter_offset(-47.0, year);
}

/*
 * ash_wednesday:
 * Christian holidays: compute Julian day for Ash Wednesday
 * given year (>1583)
 */
double
ash_wednesday(year)
	int	year;
{
	return easter_offset(-46.0, year);
}

/*
 * first_sunday_lent:
 * Christian holidays: compute Julian day for First Sunday in Lent
 * given year (>1583)
 */
double
first_sunday_lent(year)
	int	year;
{
	return easter_offset(-42.0, year);
}

/*
 * second_sunday_lent:
 * Christian holidays: compute Julian day for Second Sunday in Lent
 * given year (>1583)
 */
double
second_sunday_lent(year)
	int	year;
{
	return easter_offset(-35.0, year);
}

/*
 * third_sunday_lent:
 * Christian holidays: compute Julian day for Third Sunday in Lent
 * given year (>1583)
 */
double
third_sunday_lent(year)
	int	year;
{
	return easter_offset(-28.0, year);
}

/*
 * fourth_sunday_lent:
 * Christian holidays: compute Julian day for Fourth Sunday in Lent
 * given year (>1583)
 */
double
fourth_sunday_lent(year)
	int	year;
{
	return easter_offset(-21.0, year);
}

/*
 * passion_sunday:
 * Christian holidays: compute Julian day for Passion Sunday
 * (Second Sunday before Easter)
 * given year (>1583)
 */
double
passion_sunday(year)
	int	year;
{
	return easter_offset(-14.0, year);
}

/*
 * palm_sunday:
 * Christian holidays: compute Julian day for Palm Sunday
 * (Sunday before Easter)
 * given year (>1583)
 */
double
palm_sunday(year)
	int	year;
{
	return easter_offset(-7.0, year);
}

/*
 * maundy_thursday:
 * Christian holidays: compute Julian day for Maundy Thursday
 * (Three days prior to Easter)
 * given year (>1583)
 */
double
maundy_thursday(year)
	int	year;
{
	return easter_offset(-3.0, year);
}

/*
 * good_friday:
 * Christian holidays: compute Julian day for Good Friday
 * (Two days prior to Easter)
 * given year (>1583)
 */
double
good_friday(year)
	int	year;
{
	return easter_offset(-2.0, year);
}

/*
 * easter_monday:
 * Christian holidays: compute Julian day for Easter Monday (Canada)
 * given year (>1583)
 */
double
easter_monday(year)
	int	year;
{
	return easter_offset(1.0, year);
}

/*
 * rogation_sunday:
 * Christian holidays: compute Julian day for Rogation Sunday
 * (Rogation is the period of three days prior to Ascension Day; strictly
 * speaking, this is the period Mon-Wed)
 * given year (>1583)
 */
double
rogation_sunday(year)
	int	year;
{
	return easter_offset(35.0, year);
}

/*
 * ascension_day:
 * Christian holidays: compute Julian day for Ascension Day
 * (Holy Thursday; fortieth day after Easter, inclusive)
 * given year (>1583)
 */
double
ascension_day(year)
	int	year;
{
	return easter_offset(39.0, year);
}

/*
 * whitsunday:
 * Christian holidays: compute Julian day for Whitsunday (Pentecost)
 * (Seventh Sunday after Easter)
 * given year (>1583)
 */
double
whitsunday(year)
	int	year;
{
	return easter_offset(49.0, year);
}

/*
 * trinity_sunday:
 * Christian holidays: compute Julian day for Trinity Sunday
 * (Eighth Sunday after Easter)
 * given year (>1583)
 */
double
trinity_sunday(year)
	int	year;
{
	return easter_offset(56.0, year);
}

/*
 * corpus_christi:
 * Christian holidays: compute Julian day for Corpus Christi
 * (First Thursday after Trinity Sunday)
 * given year (>1583)
 */
double
corpus_christi(year)
	int	year;
{
	return easter_offset(60.0, year);
}

/*
 * passover:
 * Jewish holidays: compute Julian day of Pesach (first day of Passover)
 * and establish the Gregorian day of month and month, and Jewish year,
 * given Julian year (>1583)
 * This formula, due to Karl Friedrich Gauss (1777-1855) is described in
 * excruciating detail in the book by Schocken P. 51-61).  Note that it
 * computes the Julian date, which has to be corrected to a Gregorian date,
 * and that it exhibits the eccentricity of always computing the day
 * relative to March, so that April 2 appears as March 33, which is also
 * corrected below.
 *
 * Floating point implementation by R.P.C. Rodgers; integer implementation
 * (for faster calculation) by Amos Shapir (amos@nsc.com).
 */
void
passover_init(year)
	int	year;
{
	int	etmp, p_day;
	int	atmp, btmp, ctmp, day_of_week, dtmp, ftmp, gtmp;
	int	p_month;

	atmp = year + 3760;
	passoverJY = atmp;
	btmp = (12 * atmp + 17) % 19;
	ctmp = atmp % 4;
	etmp = (765433 * btmp) - (1565 * atmp)
		+ (ctmp * 123120) + 15781075;
	dtmp = etmp / 492480;
	etmp %= 492480;
		/* day_of_week is not to be confused with the
		 value returned by the day_of_week routine; here, Sunday = 1 */
	day_of_week = ((3 * atmp) + (5 * ctmp) + dtmp + 5) % 7;
	if (day_of_week == 0 && btmp > 11 && etmp >= 442111)
		p_day = dtmp + 1;
	else if (day_of_week == 1 && btmp > 6 && etmp >= 311676)
		p_day = dtmp + 2;
	else if (day_of_week == 2 || day_of_week == 4 || day_of_week ==6)
		p_day = dtmp + 1;
	else {
		p_day = dtmp;
	}
	ftmp = year / 100;		/* Correct to Gregorian date */
	gtmp = ((3 * ftmp) - 5) / 4;
	p_day += gtmp;
	if (p_day > 31) {		/* Correct for March days > 31 */
		p_day -= 31;
		p_month = 4;
		}
	else
		p_month = 3;
	passoverJD = julian_day((double)p_day, p_month, year);
}

double
passover(year, jyear)
	int year, *jyear;
{
	*jyear = passoverJY;
	return passoverJD;
}

/*
 * passover_offset:
 * Jewish holidays: compute Julian day as offset from Passover
 * given year (>1583)
 */
double
passover_offset(offset, year, jyear)
	double	offset;
	int	*jyear, year;
{
	*jyear = passoverJY;
	return passoverJD + offset;
}

/*
 * purim:
 * Jewish holidays: compute Julian day and Jewish year for Purim (Feast of Lots)
 * given year (>1583)
 */
double
purim(year, jyear)
	int	*jyear, year;
{
	return passover_offset(-30.0, year, jyear);
}

/*
 * shavuot:
 * Jewish holidays: compute Julian day and Jewish year for First day of Shavuot
 * given year (>1583)
 */
double
shavuot(year, jyear)
	int	*jyear, year;
{
	return passover_offset(50.0, year, jyear);
}

/*
 * rosh_hashanah:
 * Jewish holidays: compute Julian day and Jewish year for first day of
 * Rosh Hashanah (New Year) given year (>1583)
 */
double
rosh_hashanah(year, jyear)
	int	*jyear, year;
{
	double	atmp;
	atmp = passover_offset(163.0, year, jyear);
	(*jyear)++;
	return atmp;
}

/*
 * yom_kippur:
 * Jewish holidays: compute Julian day and Jewish year for Yom Kippur
 * given year (>1583)
 */
double
yom_kippur(year, jyear)
	int	*jyear, year;
{
	double	atmp;
	atmp = passover_offset(172.0, year, jyear);
	(*jyear)++;
	return atmp;
}

/*
 * sukkot:
 * Jewish holidays: compute Julian day and Jewish year for
 * First day of Sukkot (9 days) given year (>1583)
 */
double
sukkot(year, jyear)
	int	*jyear, year;
{
	double	atmp;
	atmp = passover_offset(177.0, year, jyear);
	(*jyear)++;
	return atmp;
}

/*
 * simchat_torah:
 * Jewish holidays: compute Julian day and Jewish year for Simchat Torah
 * (in Diapsora) given year (>1583)
 */
double
simchat_torah(year, jyear)
	int	*jyear, year;
{
	double	atmp;
	atmp = passover_offset(185.0, year, jyear);
	(*jyear)++;
	return atmp;
}

/*
 * chanukah:
 * Jewish holidays: compute Julian day and Jewish year for
 * first day of Chanukah (8 days) given year (>1583)
 */
double
chanukah(year, jyear)
	int	*jyear, year;
{
	double	atmp, ptmp;
	int	btmp, ytmp;

	atmp = passover(year, jyear);
	/* we need top compute passover for next year, so
	 * save current info and restore when done
	 */
	ptmp = passoverJD;
	ytmp = passoverJY;
	passover_init(year + 1);
	btmp = passoverJD - atmp;
	passoverJD = ptmp;
	passoverJY = ytmp;
	(*jyear)++;
	if (btmp == 355 || btmp == 385)
		return atmp + 247.0;
	else
		return atmp + 246.0;
}

/*
 * islamic_date:
 * Islamic holidays: compute Gregorian date(s) and Islamic year for any
 * Islamic day, given ISLAMIC day (1-30), ISLAMIC month(1-12),
 * and GREGORIAN year (>1583)
 * This is complicated by the fact that a given Islamic date can appear as
 * often as twice within the same Gregorian year.
 */
int
islamic_date(
	mday, mmonth, number_of_days, day1, month1, day2, month2, year, myear1,
	myear2)
	double	*day1, *day2, mday;
	int	mmonth, *month1, *month2, *myear1, *myear2,
		*number_of_days, year;
{
	double	day;
	int	count, month, nyear;
	double	islamic_to_julian();

	*myear1 = year - 621;		/* approx. >= Muslim year */
	nyear = year - 1;
	for (count = 0; nyear != year && count <= 100; count++) {
		gregorian_date(&day, &month, &nyear,
			islamic_to_julian(mday, mmonth, *myear1));
		*myear1 = *myear1 + (year - nyear);
	}
	if (nyear == year) {
		*day1 = day;
		*month1 = month;
		/*
		 * See if there is a second occurrence in same Gregorian year
		 */
		gregorian_date(&day, &month, &nyear,
			islamic_to_julian(mday, mmonth, (*myear1 + 1)));
		if (nyear == year) {
			*day2 = day;
			*month2 = month;
			*number_of_days = 2;
			*myear2 = *myear1 + 1;
		}
		else {
			*number_of_days = 1;
			gregorian_date(&day, &month, &nyear,
				islamic_to_julian(mday, mmonth, (*myear1 - 1)));
			if (nyear == year) {
				*day2 = day;
				*month2 = month;
				*number_of_days = 2;
				*myear2 = *myear1 + 1;
			}
		}
	}
/*	else return -1; */
	return 0;
}

/*
 * islamic_to_julian:
 * Islamic holidays: compute Julian day for any Islamic day,
 * given ISLAMIC day (1-30), ISLAMIC month(1-12), and ISLAMIC year (>962)
 * Formula from Schocken (p. 66)
 */
double
islamic_to_julian(mday, mmonth, myear)
	double	mday;
	int	mmonth, myear;
{
	double	etmp, ftmp, jday;
	int	atmp, btmp, ctmp, dtmp, nyear;
	double	corrected_julian_day();

	nyear = myear + 621;		/* approx. Julian year */
	atmp = ((19 * myear) - 4) % 30;
	btmp = nyear % 4;
	ctmp = (mmonth - 1) / 2;
	dtmp = (mmonth - 1) % 2;
	etmp = mday + (59.0 * ctmp) + (30.0 * dtmp) + (atmp / 30.0)
	 	+ (btmp / 4.0) - (10.8833333 * myear) + 146.8833333;
	if (etmp < 0.0) {
		ftmp = (4.0 * fabs(etmp)) / 1461.0;
		atmp = ftmp;
		dtmp = (4.0 * fabs(etmp));
		dtmp = dtmp % 1461;
		nyear -= (atmp + 1);
		ctmp = nyear % 4;
		jday = julian_day(1.0, 3, nyear)
			+ (((1461.0 - dtmp) + ctmp - btmp) / 4.0)
			- 1.0;
		return corrected_julian_day(jday);
	}
	jday = julian_day(1.0, 3, nyear) + etmp - 1.0;
	return corrected_julian_day(jday);
}

/*
 * islamic_new_year:
 * Islamic holidays: compute Julian date(s) and Islamic year(s)
 * for Islamic New Year given JULIAN year (>1583)
 * (note: due to the length of the Islamic year (355 d), there can be portions
 * of as many as two Islamic New Years within a given Gregorian year; for
 * example, according to the algorithm below, the Islamic New Year occured
 * twice in 1975: Wednesday 1 January, Sunday 21 December)
 *
 * Algorithm: Schocken, page 66, which agrees with dates I have obtained for
 * the (Gregorian) years 1962-2000.  Schocken outlines a Muslim calendar
 * consisting of 12 months which are alternately 30 and 29 days in length
 * (the first month, Muharram, is 30 days in length).  In a 30-year cycle,
 * there are 11 leap years in which a 30th day is added to the final month
 * of the year.  See full details below.
 *
 * According to Dr. Omar Afzal of Cornell University ((607)255-5118,
 * 277-6707; Chairman of the Committee for Crescent Observation;
 * irfan@mvax.ee.cornell.edu), this is an antiquated system which has been
 * in use for some 400 years, but today the Muslim calendar follows a more
 * strictly lunar basis.  I wish to acknowledge the Pakistani Consular
 * Office of San Francisco (415)788-0677, who referred me to Dr. Muzammil
 * Siddiqui of the Orange County Islamic Center, (714)531-1722, who in turn
 * referred me to to Dr. Afzal.  I thank Dr. Afzal for his patient and lucid
 * explanations of the Islamic calendar, and for providing printed matter and
 * tables of dates to assist me.  Among the sources he provided:
 *
 * %A U. V. Tsybulsky
 * %B Calendar of Middle Eastern Countries
 * %I Nauka Publishing House
 * %C Moscow
 * %L English
 * %D 1979
 *
 * which provides a scholarly discussion of calendrical conventions in various
 * Muslim countries, as well as simple formulas for conversion between
 * Gregorian and Islamic dates.  It also includes translations of tables which
 * originally appeared in:
 *
 * %A F. R. Unat
 * %B Hicri Tarihleri
 * %I Turktarih Kurumu Basimevi
 * %C Ankara
 * %L Turkish
 * %D 1959
 *
 * Additional tabular material is available in:
 *
 * %A G. S. P. Freeman-Grenville
 * %B The Muslim and Christian Calendars
 * %I Oxford University Press
 * %C London
 * %D 1963
 *
 * The Islamic calendar is also known as the Hijri calendar, and dates often
 * have "A.H." or "a.h." appended to them to indicate "Anno Hijri."  A
 * listing of the months and their lengths (in the old scheme):
 *
 *    Month Name     Days         Days to add to 1 Muharram to get 1st of month
 * 1  Muharram        30          0
 * 2  Safar           29         30
 * 3  Rabi' al-Awwal  30         59
 * 4  Rabi' ath-Thani 29         89
 * 5  Jumada al-Ula   30        118
 * 6  Jumada al-Akhir 29        148
 * 7  Rajab           30        177
 * 8  Sha'ban         29        207
 * 9  Ramadan         30        236
 * 10 Shawwal         29        266
 * 11 Dhul-Qa'da      30        295
 * 12 Dhul-Hijja      29 (30)   325
 *
 * In the old scheme, used here for simplicity, adherence to this set of rules
 * led to a gradual lag of the month behind the actual crescent moon.  Thus,
 * a leap day was appended to the final month in each of 11 years out of every
 * 30 year cycle: (2, 5, 7, 10, 13, 16, 18, 21, 24, 26, 29).  If the Hijra year
 * is divided by 30, and the remainder is equal to one of these numbers, it is
 * a leap year.
 *
 * The Islamic day begins after sunset, and the beginning of the month is tied
 * to the first VISIBLE appearance of the crescent moon, which often lags behind
 * the astronomical new moon.  Also, the crescent was sometimes computed
 * according to Mecca time.  There are numerous methods for defining the date
 * of the crescent moon (and hence calendar dates) among the various Muslim
 * regions of the world.  Given these uncertainties, the dates computed here are
 * likely to be accurate to only +/- 2 days.
 *
 * Any errors in this code are my own fault, and
 * not intended to offend any members of the Islamic faith.
 */
int
islamic_new_year(year, number_of_dates, date1, date2, myear1, myear2)
	double	*date1, *date2;
	int	*number_of_dates, *myear1, *myear2, year;
{
	double	day1, day2;
	int	month1, month2;

	if (islamic_date(1.0, 1, number_of_dates, &day1, &month1, &day2,
		&month2, year, myear1, myear2) < 0) return -1;
	*date1 = julian_day(day1, month1, year);
	if (*number_of_dates == 2) *date2 = julian_day(day2, month2, year);
	return 0;
}

/*
 * islamic_offset:
 * Islamic holidays: compute Julian day(s) for an Islamic date as an offset
 * from the Islamic New Year, given (Gregorian) year
 */
void
islamic_offset(offset, year, number_of_dates, date1, date2, myear1, myear2)
	double	*date1, *date2, offset;
	int	*number_of_dates, *myear1, *myear2, year;
{
	double	day, tdate1, tdate2;
	int	month, tyear1, tyear2;

	(void) islamic_new_year(year, number_of_dates, date1, date2,
		myear1, myear2);
	if (*number_of_dates == 2) {
		tdate2 = *date2 + offset;
		gregorian_date(&day, &month, &tyear2, tdate2);
		if (tyear2 > year) {
			tdate2 = *date1 + offset;
			(void) islamic_new_year((year - 1), number_of_dates,
				date1, date2, myear1, myear2);
			tdate1 = *date1 + offset;
			gregorian_date(&day, &month, &tyear1, tdate1);
			if (tyear1 == year) {
				*date1 = tdate1;
				*date2 = tdate2;
				*number_of_dates = 2;
				*myear2 = *myear1 + 1;
			}
			else {
				*date1 = tdate2;
				*number_of_dates = 1;
				*myear1 = *myear1 + 1;
			}
		}
		else {
			*date1 += offset;
			*date2 = tdate2;
		}
	}
	else {
		tdate2 = *date1 + offset;
		gregorian_date(&day, &month, &tyear2, tdate2);
		if (tyear2 > year) {
			(void) islamic_new_year((year - 1), number_of_dates,
				date1, date2, myear1, myear2);
			if (*number_of_dates == 2) {
				*date1 = *date2 + offset;
				*number_of_dates = 1;
				*myear1 = *myear2;
			}
			else {
				*date1 = *date1 + offset;
			}
		}
		else {
			(void) islamic_new_year((year - 1), number_of_dates,
				date1, date2, myear1, myear2);
			if (*number_of_dates == 2) {
				tdate1 = *date2 + offset;
				*myear1 = *myear2;
			}
			else {
				tdate1 = *date1 + offset;
			}
			gregorian_date(&day, &month, &tyear1, tdate1);
			if (tyear1 == year) {
				*date1 = tdate1;
				*date2 = tdate2;
				*number_of_dates = 2;
				*myear2 = *myear1 + 1;
			}
			else {
				*date1 = tdate2;
				*myear1 = *myear1 + 1;
				*number_of_dates = 1;
			}
		}
	}
}

/*
 * muharram_9:
 * Islamic holidays: compute Julian day(s) and Islamic year(s) for
 * Muharram 9 (Day of fasting) given year (>1583)
 */
void
muharram_9(year, number_of_dates, date1, date2, myear1, myear2)
	double	*date1, *date2;
	int	*number_of_dates, *myear1, *myear2, year;
{
	islamic_offset(
		8.0, year, number_of_dates, date1, date2, myear1, myear2);
}

/*
 * muharram_10:
 * Islamic holidays: compute Julian day(s) and Islamic year(s) for
 * Muharram 10 (Day of deliverance of Moses from the Pharoah; for Shia Islam,
 * martyrdom of Husain) given year (>1583)
 */
void
muharram_10(year, number_of_dates, date1, date2, myear1, myear2)
	double	*date1, *date2;
	int	*number_of_dates, *myear1, *myear2, year;
{
	islamic_offset(
		9.0, year, number_of_dates, date1, date2, myear1, myear2);
}

/*
 * muharram_16:
 * Islamic holidays: compute Julian day(s) and Islamic year(s) for
 * Muharram 16 (Imamat Day; Ismaili Khoja) given year (>1583)
 */
void
muharram_16(year, number_of_dates, date1, date2, myear1, myear2)
	double	*date1, *date2;
	int	*number_of_dates, *myear1, *myear2, year;
{
	islamic_offset(
		15.0, year, number_of_dates, date1, date2, myear1, myear2);
}

/*
 * eid_i_milad_un_nabi:
 * Islamic holidays: compute Julian day(s) and Islamic year(s) for
 * Rabi I 12 (Eid-i-Milad-un-Nabi: The Prophet's Birthday) given year (>1583)
 */
void
eid_i_milad_un_nabi(year, number_of_dates, date1, date2, myear1, myear2)
	double	*date1, *date2;
	int	*number_of_dates, *myear1, *myear2, year;
{
	islamic_offset(
		70.0, year, number_of_dates, date1, date2, myear1, myear2);
}

/*
 * jumada_al_akhir_23:
 * Islamic holidays: compute Julian day(s) and Islamic year(s) for
 * Jumada al-Akhir 23 (Birth of Agha Khan IV, Ismaili) given year (>1583)
 */
void
jumada_al_akhir_23(year, number_of_dates, date1, date2, myear1, myear2)
	double	*date1, *date2;
	int	*number_of_dates, *myear1, *myear2, year;
{
	islamic_offset(
		170.0, year, number_of_dates, date1, date2, myear1, myear2);
}

/*
 * shab_e_miraj:
 * Islamic holidays: compute Julian day(s) and Islamic year(s) for
 * Rajab 27 (Shab-e-Mi'raj: The Prophet's Ascension) given year (>1583)
 */
void
shab_e_miraj(year, number_of_dates, date1, date2, myear1, myear2)
	double	*date1, *date2;
	int	*number_of_dates, *myear1, *myear2, year;
{
	islamic_offset(
		203.0, year, number_of_dates, date1, date2, myear1, myear2);
}

/*
 * shab_e_barat:
 * Islamic holidays: compute Julian day(s) and Islamic year(s) for
 * Shaban 15 (Shab-e-Bara't: Night, followed by day of fasting) given year (>1583)
 */
void
shab_e_barat(year, number_of_dates, date1, date2, myear1, myear2)
	double	*date1, *date2;
	int	*number_of_dates, *myear1, *myear2, year;
{
	islamic_offset(
		221.0, year, number_of_dates, date1, date2, myear1, myear2);
}

/*
 * ramadan:
 * Islamic holidays: compute Julian day(s) and Islamic year(s) for
 * Ramadan 1 (Fasting month begins) given year (>1583)
 */
void
ramadan(year, number_of_dates, date1, date2, myear1, myear2)
	double	*date1, *date2;
	int	*number_of_dates, *myear1, *myear2, year;
{
	islamic_offset(
		236.0, year, number_of_dates, date1, date2, myear1, myear2);
}

/*
 * shab_e_qadr:
 * Islamic holidays: compute Julian day(s) and Islamic year(s) for
 * Ramadan 27 (Shab-e Qadr: Night vigil) given year (>1583)
 */
void
shab_e_qadr(year, number_of_dates, date1, date2, myear1, myear2)
	double	*date1, *date2;
	int	*number_of_dates, *myear1, *myear2, year;
{
	islamic_offset(
		262.0, year, number_of_dates, date1, date2, myear1, myear2);
}

/*
 * eid_al_fitr:
 * Islamic holidays: compute Julian day(s) and Islamic year(s) for
 * Shawwal 1 (Eid-al-Fitr: Day of Feast) given year (>1583)
 */
void
eid_al_fitr(year, number_of_dates, date1, date2, myear1, myear2)
	double	*date1, *date2;
	int	*number_of_dates, *myear1, *myear2, year;
{
	islamic_offset(
		266.0, year, number_of_dates, date1, date2, myear1, myear2);
}

/*
 * dhul_hijja_9:
 * Islamic holidays: compute Julian day(s) and Islamic year(s) for
 * Dhul-Hijj 9 (Day of Pilgrimage at Arafat, Mecca) given year (>1583)
 */
void
dhul_hijja_9(year, number_of_dates, date1, date2, myear1, myear2)
	double	*date1, *date2;
	int	*number_of_dates, *myear1, *myear2, year;
{
	islamic_offset(
		333.0, year, number_of_dates, date1, date2, myear1, myear2);
}

/*
 * eid_al_adha:
 * Islamic holidays: compute Julian day(s) and Islamic year(s) for
 * Dhul-Hijj 10 (Eid-al-Adha: Day of Abraham's Sacrifice) given year (>1583)
 */
void
eid_al_adha(year, number_of_dates, date1, date2, myear1, myear2)
	double	*date1, *date2;
	int	*number_of_dates, *myear1, *myear2, year;
{
	islamic_offset(
		334.0, year, number_of_dates, date1, date2, myear1, myear2);
}

/*
 * ghadir:
 * Islamic holidays: compute Julian day(s) and Islamic year(s) for
 * Dhul-Hijj 18 (Ghadir: Ali's Nomination) given year (>1583)
 */
void
ghadir(year, number_of_dates, date1, date2, myear1, myear2)
	double	*date1, *date2;
	int	*number_of_dates, *myear1, *myear2, year;
{
	islamic_offset(
		342.0, year, number_of_dates, date1, date2, myear1, myear2);
}

/*
 * print_islamic_string:
 * Print formatted date string(s) of form:
 *    Monday 1 August 1988 (Islamic year 1409)
 * given string labelling date, (Gregorian) year of event, and function which
 * takes (Gregorian) year as an argument and produces: the (integer) number of
 * Gregorian dates (1 or 2), the first and second (Julian) days, and the first
 * and second Islamic years.
 */
void
print_islamic_string(string, year, func)
	char	*string;
	int	year;
	int	(*func) ();
{

	char	*date;
	double	date1, date2, day, holiday;
	int	month, myear1, myear2, number_of_dates;
	char	*date_string();

	holiday = (*func) (
		year, &number_of_dates, &date1, &date2, &myear1, &myear2);
	if (holiday < 0)
		(void) printf("Can not determine requested date\n");
	else {
		(void) printf("%s (%d) Julian day: %f\n", string, year, date1);
		gregorian_date(&day, &month, &year, date1);
		date = date_string(get_day_of_week(day, month, year),
			day, month, year);
		(void) printf("%s (%d): %s", string, year, date);
		(void) printf(" (Islamic year %d)\n", myear1);
		if (number_of_dates == 2) {
			(void) printf(
				"%s (%d) Julian day: %f\n", string, year,
				date2);
			gregorian_date(&day, &month, &year, date2);
			date = date_string(get_day_of_week(day, month, year),
				day, month, year);
			(void) printf("%s (%d): %s", string, year, date);
			(void) printf(" (Islamic year %d)\n",
				myear2);
		}
	}
}

/*
 * print_date_and_time_string:
 * Print formatted date/time string of form: 10:42 Thursday 8 December 1988
 * given string labelling date, year of event, and function which takes year
 * as an argument and produces Julian day, 
 */
void
print_date_and_time_string(string, year, func)
	char	*string;
	int	year;
	double	(*func)();
{
	char	*date;
	double	btmp, ctmp, day, holiday;
	int	atmp, hour, min, month;
	char	*date_time_string();

	holiday = (*func) (year);
	(void) printf("%s (%d) Julian day: %f\n", string, year, holiday);
	gregorian_date(&day, &month, &year, holiday);
	atmp = day;
	btmp = day - atmp;
	hour = btmp * 24.0;
	ctmp = (btmp - (hour / 24.0)) * 1440.0;
	min = ctmp;
	date = date_time_string(hour, min, get_day_of_week(day, month, year),
		day, month, year);
	(void) printf("%s (%d): %s\n", string, year, date);
}

/*
 * print_date_string:
 * Print formatted date string of form: Thursday 8 December 1988
 * given string labelling date, year of event, and function which takes year
 * as an argument and produces Julian day. 
 */
void
print_date_string(string, year, func)
	char	*string;
	int	year;
	double	(*func) ();
{
	char	*date;
	double	day, holiday;
	int	month;
	char	*date_string();

	holiday = (*func) (year);
	(void) printf("%s (%d) Julian day: %f\n", string, year, holiday);
	gregorian_date(&day, &month, &year, holiday);
	date = date_string(get_day_of_week(day, month, year), day, month, year);
	(void) printf("%s (%d): %s\n", string, year, date);
}

/*
 * print_jewish_string:
 * Print formatted date string of form: Thursday 8 December 1988 (NYEAR)
 * given string labelling date, year of event, and function which takes year
 * as an argument, sets the value of a non-Gregorian year (NYEAR), and produces
 * the Julian day.
 */
void
print_jewish_string(string, year, func)
	char	*string;
	int	year;
	double	(*func) ();
{
	char	*date;
	double	day, holiday;
	int	month, nyear;
	char	*date_string();

	holiday = (*func) (year, &nyear);
	(void) printf("%s (%d) Julian day: %f\n", string, year, holiday);
	gregorian_date(&day, &month, &year, holiday);
	date = date_string_2(get_day_of_week(day, month, year), day, month,
		year, nyear);
	(void) printf("%s (%d): %s\n", string, year, date);
}

/*
 * extract time of day from Julian day
 */
char *
julian_time(jday)
double jday;
{
	double	h1, floor();
	int	hours, minutes;

	jday += 0.5;  /* Julian day starts at noon GMT */
	h1 = (jday - floor(jday)) * 24.; /* number of hours & frac of hours */
	hours = (int) floor(h1);
	minutes = (int) ((h1 - (double) hours) * 60.);
	sprintf(timebuf, " at %d:%02d", hours, minutes);
	return(timebuf);
}

/*
 * End of code
 */
#endif	/* NO_HOLIDAYS */
