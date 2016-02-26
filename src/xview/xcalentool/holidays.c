/*
 * $Id: holidays.c,v 2.3 1994/08/19 19:53:08 billr Exp $
 */
/*
 * holidays.c - (based on a test driver by R.P.C. Rodgers, UCSF)
 *
 * calentool - day/week/month/year-at-a-glance calendar for XView/Open Look
 * 
 * Copyright 1989, 1994 by Tektronix, Inc. - All Rights Reserved.
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
 * functions from datelib (by R.P.C. Rodgers)
 */
#include "ct.h"		/* for the NO_HOLIDAYS #define */
#ifndef NO_HOLIDAYS

#include <sys/time.h>

#ifdef __STDC__
double	election_day(int year);			/* Secular US holidays */

double	autumn_equinox(int year),		/* Astronomical events */
	summer_solstice(int year),
	vernal_equinox(int year),
	winter_solstice(int year);

double	ascension_day(int year),		/* Christian holidays */
	ash_wednesday(int year),
	corpus_christi(int year),
	easter(int year),
	easter_monday(int year),
	easter_offset(double offset, int year),
	first_sunday_advent(int year),
	first_sunday_lent(int year),
	fourth_sunday_lent(int year),
	good_friday(int year),
	maundy_thursday(int year),
	palm_sunday(int year),
	passion_sunday(int year),
	rogation_sunday(int year),
	septuagesima(int year),
	sexagesima(int year),
	quinquagesima(int year),
	second_sunday_lent(int year),
	shrove_monday(int year),
	shrove_tuesday(int year),
	third_sunday_lent(int year),
	trinity_sunday(int year),
	whitsunday(int year);

double	islamic_new_year(int year, int *number_of_dates, double *date1, double *date2, int *myear1, int *myear2),		/* Islamic holidays */
	muharram_9(int year, int *number_of_dates, double *date1, double *date2, int *myear1, int *myear2),
	muharram_10(int year, int *number_of_dates, double *date1, double *date2, int *myear1, int *myear2),
	muharram_16(int year, int *number_of_dates, double *date1, double *date2, int *myear1, int *myear2),
	eid_i_milad_un_nabi(int year, int *number_of_dates, double *date1, double *date2, int *myear1, int *myear2),
	jumada_al_akhir_23(int year, int *number_of_dates, double *date1, double *date2, int *myear1, int *myear2),
	shab_e_miraj(int year, int *number_of_dates, double *date1, double *date2, int *myear1, int *myear2),
	shab_e_barat(int year, int *number_of_dates, double *date1, double *date2, int *myear1, int *myear2),
	shab_e_qadr(int year, int *number_of_dates, double *date1, double *date2, int *myear1, int *myear2),
	ramadan(int year, int *number_of_dates, double *date1, double *date2, int *myear1, int *myear2),
	eid_al_fitr(int year, int *number_of_dates, double *date1, double *date2, int *myear1, int *myear2),
	dhul_hijja_9(int year, int *number_of_dates, double *date1, double *date2, int *myear1, int *myear2),
	eid_al_adha(int year, int *number_of_dates, double *date1, double *date2, int *myear1, int *myear2),
	ghadir(int year, int *number_of_dates, double *date1, double *date2, int *myear1, int *myear2);

double	chanukah(int year, int *jyear),			/* Jewish holidays */
	passover(int year, int *jyear),
	passover_offset(double offset, int year, int *jyear),
	purim(int year, int *jyear),
	rosh_hashanah(int year, int *jyear),
	shavuot(int year, int *jyear),
	simchat_torah(int year, int *jyear),
	sukkot(int year, int *jyear),
	yom_kippur(int year, int *jyear);

void fill_holiday (struct appt_entry *aptr, double hday);
void get_date (struct appt_entry *aptr, char *str, double (*func) (/* ??? */), int timeflag);
void get_jdate (struct appt_entry *aptr, char *str, double (*func) (/* ??? */));
#else
double	election_day();

double	autumn_equinox(),
	summer_solstice(),
	vernal_equinox(),
	winter_solstice();

double	ascension_day(),
	ash_wednesday(),
	corpus_christi(),
	easter(),
	easter_monday(),
	easter_offset(),
	first_sunday_advent(),
	first_sunday_lent(),
	fourth_sunday_lent(),
	good_friday(),
	maundy_thursday(),
	palm_sunday(),
	passion_sunday(),
	rogation_sunday(),
	septuagesima(),
	sexagesima(),
	quinquagesima(),
	second_sunday_lent(),
	shrove_monday(),
	shrove_tuesday(),
	third_sunday_lent(),
	trinity_sunday(),
	whitsunday();

double	islamic_new_year(),
	muharram_9(),
	muharram_10(),
	muharram_16(),
	eid_i_milad_un_nabi(),
	jumada_al_akhir_23(),
	shab_e_miraj(),
	shab_e_barat(),
	shab_e_qadr(),
	ramadan(),
	eid_al_fitr(),
	dhul_hijja_9(),
	eid_al_adha(),
	ghadir();

double	chanukah(),
	passover(),
	passover_offset(),
	purim(),
	rosh_hashanah(),
	shavuot(),
	simchat_torah(),
	sukkot(),
	yom_kippur();

void fill_holiday ();
void get_date ();
void get_jdate ();
#endif

extern struct tm current;	/* current day displayed from calentool */
#define NUM_ADATES	4
#define NUM_CDATES	22
#define NUM_IDATES	28
#define NUM_JDATES	8
#define NUM_SDATES	1

int	marked_note;
int	gsave_year = -1;  /* indicates datelib initialized */
struct appt_entry a_appts[NUM_ADATES], c_appts[NUM_CDATES];
struct appt_entry i_appts[NUM_IDATES], j_appts[NUM_JDATES];
struct appt_entry s_appts[NUM_SDATES];

int
a_dates(flag)
int flag;
{
	static int save_year = -1;

	/*
	 * Astromonical Events
	 */
	if (!flag || current.tm_year == save_year)
		return(NUM_ADATES);
	marked_note = flag - 1;
	if (current.tm_year != gsave_year) {
		datelib_init(current.tm_year+1900);
		gsave_year = current.tm_year;
	}
	save_year = current.tm_year;
	get_date(&a_appts[0], "Vernal Equinox", vernal_equinox, TRUE);
	get_date(&a_appts[1], "Summer Solstice", summer_solstice, TRUE);
	get_date(&a_appts[2], "Autumn Equinox", autumn_equinox, TRUE);
	get_date(&a_appts[3], "Winter Solstice", winter_solstice, TRUE);
	return(NUM_ADATES);
}

int
c_dates(flag)
int flag;
{
	static int save_year = -1;

	/*
	 * Christian holidays
	 */
	if (!flag || current.tm_year == save_year)
		return(NUM_CDATES);
	marked_note = flag - 1;
	if (current.tm_year != gsave_year) {
		datelib_init(current.tm_year+1900);
		gsave_year = current.tm_year;
	}
	save_year = current.tm_year;
	get_date(&c_appts[0], "Septuagesima Sunday", septuagesima, FALSE);
	get_date(&c_appts[1], "Sexagesima Sunday", sexagesima, FALSE);
	get_date(&c_appts[2], "Quinquagesima Sunday", quinquagesima, FALSE);
	get_date(&c_appts[3], "Shrove Monday", shrove_monday, FALSE);
	get_date(&c_appts[4], "Shrove Tuesday (Mardi Gras)", shrove_tuesday, FALSE);
	get_date(&c_appts[5], "Ash Wednesday", ash_wednesday, FALSE);
	get_date(&c_appts[6], "First Sunday in Lent", first_sunday_lent, FALSE);
	get_date(&c_appts[7], "Second Sunday in Lent", second_sunday_lent, FALSE);
	get_date(&c_appts[8], "Third Sunday in Lent", third_sunday_lent, FALSE);
	get_date(&c_appts[9], "Fourth Sunday in Lent", fourth_sunday_lent, FALSE);
	get_date(&c_appts[10], "Passion Sunday", passion_sunday, FALSE);
	get_date(&c_appts[11], "Palm Sunday", palm_sunday, FALSE);
	get_date(&c_appts[12], "Maundy Thursday", maundy_thursday, FALSE);
	get_date(&c_appts[13], "Good Friday", good_friday, FALSE);
	get_date(&c_appts[14], "Easter", easter, FALSE);
	get_date(&c_appts[15], "Easter Monday (Canada)", easter_monday, FALSE);
	get_date(&c_appts[16], "Rogation Sunday", rogation_sunday, FALSE);
	get_date(&c_appts[17], "Ascension Day", ascension_day, FALSE);
	get_date(&c_appts[18], "Whitsunday", whitsunday, FALSE);
	get_date(&c_appts[19], "Trinity Sunday", trinity_sunday, FALSE);
	get_date(&c_appts[20], "Corpus Christi", corpus_christi, FALSE);
	get_date(&c_appts[21], "First Sunday in Advent", first_sunday_advent, FALSE);
	return(NUM_CDATES);
}

int
i_dates(flag)
int flag;
{
	static int save_year = -1, i = 0;
	int get_idate();

	/*
	 * Islamic holidays
	 */
	if (!flag || current.tm_year == save_year)
		return(i);
	marked_note = flag - 1;
	if (current.tm_year != gsave_year) {
		datelib_init(current.tm_year+1900);
		gsave_year = current.tm_year;
	}
	save_year = current.tm_year;
	i = 0;
	if (get_idate(&i_appts[i++],
	    "Muharram 1, %d A.H.: Islamic New Year", islamic_new_year, 1) == 2)
		get_idate(&i_appts[i++],
		    "Muharram 1, %d A.H.: Islamic New Year", islamic_new_year, 2);
	if (get_idate(&i_appts[i++],
	    "Muharram 9, %d A.H.: Day of fasting", muharram_9, 1) == 2)
		get_idate(&i_appts[i++],
		    "Muharram 9, %d A.H.: Day of fasting", muharram_9, 2);
	if (get_idate(&i_appts[i++],
	    "Muharram 10, %d A.H.: Deliverance of Moses from the Pharoah (for Shia Islam, martyrdom of Husain)",
	    muharram_10, 1) == 2)
		get_idate(&i_appts[i++],
		    "Muharram 10, %d A.H.: Deliverance of Moses from the Pharoah (for Shia Islam, martyrdom of Husain)",
		    muharram_10, 2);
	if (get_idate(&i_appts[i++],
	    "Muharram 16, %d A.H. (Imamat Day; Ismaili Khoja)", muharram_16, 1) == 2)
		get_idate(&i_appts[i++],
		    "Muharram 16, %d A.H. (Imamat Day; Ismaili Khoja)", muharram_16, 2);
	if (get_idate(&i_appts[i++],
	    "Rabi I 12, %d A.H. (Eid-i-Milad-un-Nabi: The Prophet's Birthday)",
	    eid_i_milad_un_nabi, 1) == 2)
		get_idate(&i_appts[i++],
		    "Rabi I 12, %d A.H. (Eid-i-Milad-un-Nabi: The Prophet's Birthday)",
		    eid_i_milad_un_nabi, 2);
	if (get_idate(&i_appts[i++],
	    "Jumada al-Akhir 23, %d A.H. (Birth of Agha Khan IV, Ismaili)",
	    jumada_al_akhir_23, 1) == 2)
		get_idate(&i_appts[i++],
		    "Jumada al-Akhir 23, %d A.H. (Birth of Agha Khan IV, Ismaili)",
		    jumada_al_akhir_23, 2);
	if (get_idate(&i_appts[i++],
	    "Rajab 27, %d A.H. (Shab-e-Mi'raj: The Prophet's Ascension)",
	    shab_e_miraj, 1) == 2)
		get_idate(&i_appts[i++],
		    "Rajab 27, %d A.H. (Shab-e-Mi'raj: The Prophet's Ascension)",
		    shab_e_miraj, 2);
	if (get_idate(&i_appts[i++],
	    "Shaban 15, %d A.H. (Shab-e-Bara't: Night, followed by day of fasting)",
	    shab_e_barat, 1) == 2)
		get_idate(&i_appts[i++],
		    "Shaban 15, %d A.H. (Shab-e-Bara't: Night, followed by day of fasting)",
		    shab_e_barat, 2);
	if (get_idate(&i_appts[i++],
	    "Ramadan 1, %d A.H. (Fasting month begins)", ramadan, 1) == 2)
		get_idate(&i_appts[i++],
		    "Ramadan 1, %d A.H. (Fasting month begins)", ramadan, 2);
	if (get_idate(&i_appts[i++],
	    "Ramadan 27, %d A.H. (Shab-e-Qadr: Night vigil)", shab_e_qadr, 1) == 2)
		get_idate(&i_appts[i++],
		    "Ramadan 27, %d A.H. (Shab-e-Qadr: Night vigil)", shab_e_qadr, 2);
	if (get_idate(&i_appts[i++],
	    "Shawwal 1, %d A.H. (Eid-al-Fitr: Day of Feast)", eid_al_fitr, 1) == 2)
		get_idate(&i_appts[i++],
		    "Shawwal 1, %d A.H. (Eid-al-Fitr: Day of Feast)", eid_al_fitr, 2);
	if (get_idate(&i_appts[i++],
	    "Dhul-Hijj 9, %d A.H. (Day of Pilgrimage at Arafat, Mecca)",
	    dhul_hijja_9, 1) == 2)
		get_idate(&i_appts[i++],
		    "Dhul-Hijj 9, %d A.H. (Day of Pilgrimage at Arafat, Mecca)",
		    dhul_hijja_9, 2);
	if (get_idate(&i_appts[i++],
	    "Dhul-Hijj 10, %d A.H. (Eid-al-Azha: Day of Abraham's Sacrifice)",
	    eid_al_adha, 1) == 2)
		get_idate(&i_appts[i++],
		    "Dhul-Hijj 10, %d A.H. (Eid-al-Azha: Day of Abraham's Sacrifice)",
		    eid_al_adha, 2);
	if (get_idate(&i_appts[i++],
	    "Dhul-Hijj 18, %d A.H. (Ghadir: Ali's Nomination)", ghadir, 1) == 2)
		get_idate(&i_appts[i++],
		    "Dhul-Hijj 18, %d A.H. (Ghadir: Ali's Nomination)", ghadir, 2);
	return(i);
}

int
j_dates(flag)
int flag;
{
	static int save_year = -1;

	/*
	 * Jewish holidays
	 */
	if (!flag || current.tm_year == save_year)
		return(NUM_JDATES);
	marked_note = flag - 1;
	if (current.tm_year != gsave_year) {
		datelib_init(current.tm_year+1900);
		gsave_year = current.tm_year;
	}
	save_year = current.tm_year;
	get_jdate(&j_appts[0], "Purim", purim);
	get_jdate(&j_appts[1], "First day of Passover (8 days)", passover);
	get_jdate(&j_appts[2], "Shavuot (2 days)", shavuot);
	get_jdate(&j_appts[3], "Rosh Hashanah (Jewish New Year) (2 days)", rosh_hashanah);
	get_jdate(&j_appts[4], "Yom Kippur", yom_kippur);
	get_jdate(&j_appts[5], "First day of Sukkot (9 days)", sukkot);
	get_jdate(&j_appts[6], "Simchat Torah", simchat_torah);
	get_jdate(&j_appts[7], "First day of Chanukah (8 days)", chanukah);
	return(NUM_JDATES);
}

int
s_dates(flag)
int flag;
{
	static int save_year = -1;

	/*
	 * SECULAR US HOLIDAYS
	 * [Holidays specified as a given date (e.g. July 4) or as
	 *  a known nth mday of a month (e.g. 2nd Monday) are not
	 *  calculated here.  They are available from one of the
	 *  auxillary date files.]
	 */
	if (!flag || current.tm_year == save_year)
		return(NUM_SDATES);
	marked_note = flag - 1;
	if (current.tm_year != gsave_year) {
		datelib_init(current.tm_year+1900);
		gsave_year = current.tm_year;
	}
	save_year = current.tm_year;
	get_date(&s_appts[0], "Election Day", election_day, FALSE);
	return(NUM_SDATES);
}

/*
 * check for gregorian (i.e. US secular & Christian) holidays
 */
void
get_date(aptr, str, func, timeflag)
struct appt_entry *aptr;
char *str;
double (*func)();
int timeflag;
{
	double	hday;
	char 	*julian_time();

	hday = (*func)(current.tm_year+1900);
	fill_holiday(aptr, hday);
	strcpy(aptr->str, str);
	if (timeflag) {
		strcat(aptr->str, julian_time(hday));
		strcat(aptr->str, " GMT");
	}
}

/*
 * check for Islamic holidays
 */
int
get_idate(aptr, str, func, which)
struct appt_entry *aptr;
char *str;
int (*func)();
int which;
{
	double	date1, date2;
	int	nr_dates, year1, year2;

	(void)(*func)(current.tm_year+1900, &nr_dates, &date1, &date2, &year1, &year2);
	if (which == 1) {
		fill_holiday(aptr, date1);
		sprintf(aptr->str, str, year1);
	} else if (which == 2 && nr_dates == 2) {
		fill_holiday(aptr, date2);
		sprintf(aptr->str, str, year2);
	}
	return (nr_dates);
}

/*
 * check for Jewish holidays
 */
void
get_jdate(aptr, str, func)
struct appt_entry *aptr;
char *str;
double (*func)();
{
	int	jyear;
	double	hday;
	char	buf[32];

	hday = (*func)(current.tm_year+1900, &jyear);
	fill_holiday(aptr, hday);
	strcpy(aptr->str, str);
	sprintf(buf, " [Jewish year %d]", jyear);
	strcat(aptr->str, buf);
}

void
fill_holiday(aptr, hday)
struct appt_entry *aptr;
double hday;
{
	int	month, year;
	double	day;

	gregorian_date(&day, &month, &year, hday);
	aptr->year = current.tm_year;
	aptr->month = month - 1;
	aptr->day = (int)day;
	aptr->arrows = aptr->repeat = aptr->lookahead = 0;
	aptr->runlength = 0;
	aptr->warn = 10;
	aptr->flags = (A_NOTE | READONLY);
	if (marked_note)
		aptr->flags |= MARKED;
}
#endif	/* NO_HOLIDAYS */
