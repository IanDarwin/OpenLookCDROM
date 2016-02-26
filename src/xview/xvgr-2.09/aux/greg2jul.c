/*
 * convert Gregorian days to Julian date
 *
 * Compile with 'cc greg2jul.c -o greg2jul'
 *
 * Modify as needed for your application.
 *
 * The Julian day starts at noon of the Gregorian day and extends
 * to noon the next Gregorian day.
 *
 */

#include <stdio.h>
#include <math.h>

double julday();

char *dayofweekstr[] = {"Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"};

main(argc, argv)
    int argc;
    char **argv;
{
    int cnt, month, day, year, hours = 12, minutes = 0;
    double seconds = 0.0, data;
    char s[256];

    while (gets(s) != NULL) {
/* note that seconds is a double */
	sscanf(s, "%d %d %d %d %d %lf %lf", &month, &day, &year, &hours, &minutes, &seconds, &data);
        printf("%lf %lf\n", julday(month, day, year, hours, minutes, seconds), data);
    }
}

/*
** Takes a date, and returns a Julian day. A Julian day is the number of
** days since some base date  (in the very distant past).
** Handy for getting date of x number of days after a given Julian date
** (use jdate to get that from the Gregorian date).
** Author: Robert G. Tantzen, translator: Nat Howard
** Translated from the algol original in Collected Algorithms of CACM
** (This and jdate are algorithm 199).
*/
double julday(mon, day, year, h, mi, se)
    int mon, day, year, h, mi;
    double se;
{
    long m = mon, d = day, y = year;
    long c, ya, j;
    double seconds = h * 3600.0 + mi * 60 + se;

    if (m > 2)
	m -= 3;
    else {
	m += 9;
	--y;
    }
    c = y / 100L;
    ya = y - (100L * c);
    j = (146097L * c) / 4L + (1461L * ya) / 4L + (153L * m + 2L) / 5L + d + 1721119L;
    if (seconds < 12 * 3600.0) {
	j--;
	seconds += 12.0 * 3600.0;
    }
    else {
	seconds = seconds - 12.0 * 3600.0;
    }
    return (j + (seconds / 3600.0) / 24.0);
}

/* Julian date converter. Takes a julian date (the number of days since
** some distant epoch or other), and returns an int pointer to static space.
** ip[0] = month;
** ip[1] = day of month;
** ip[2] = year (actual year, like 1977, not 77 unless it was  77 a.d.);
** ip[3] = day of week (0->Sunday to 6->Saturday)
** These are Gregorian.
** Copied from Algorithm 199 in Collected algorithms of the CACM
** Author: Robert G. Tantzen, Translator: Nat Howard
*/
calcdate(jd, m, d, y, h, mi, sec)
    double jd;
    int *m, *d, *y, *h, *mi;
    double *sec;
{
    static int ret[4];

    long j = jd;
    double tmp, frac = jd - j;
    if (frac >= 0.5) {
	frac = frac - 0.5;
        j++;
    }
    else {
	frac = frac + 0.5;
    }

    ret[3] = (j + 1L) % 7L;
    j -= 1721119L;
    *y = (4L * j - 1L) / 146097L;
    j = 4L * j - 1L - 146097L * *y;
    *d = j / 4L;
    j = (4L * *d + 3L) / 1461L;
    *d = 4L * *d + 3L - 1461L * j;
    *d = (*d + 4L) / 4L;
    *m = (5L * *d - 3L) / 153L;
    *d = 5L * *d - 3 - 153L * *m;
    *d = (*d + 5L) / 5L;
    *y = 100L * *y + j;
    if (*m < 10)
	*m += 3;
    else {
	*m -= 9;
	*y++;
    }
    if (*m < 3) *y++;
    tmp = 3600.0 * (frac * 24.0);
    *h = (int) (tmp / 3600.0);
    tmp = tmp - *h * 3600.0;
    *mi = (int) (tmp / 60.0);
    *sec = tmp - *mi * 60.0;
}

int dayofweek(j)
    double j;
{
    j += 0.5;
    return (int) (j + 1) % 7;
}
