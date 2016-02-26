/* $Id: utils.c,v 1.9 91/12/08 15:34:09 pturner Exp Locker: pturner $
 *
 * misc utilities
 *
 * Contents:
 *
 * void cxfree() - cfree and check for NULL pointer
 * void fswap()  - swap doubles
 * void iswap()  - swap ints
 * void lowtoupper() - convert a string to upper case
 * void convertchar() - remove commas and Fortran D format
 * int ilog2() - integer log base 2, for the fft routine
 * double comp_area() - compute the area of a polygon
 * double comp_perimeter() - compute the perimeter
 * double fmin(), fmax()
 * Julian date routines
 *
 */

#include <stdio.h>
#include <ctype.h>
#include <math.h>

/*
 * cfree and check for NULL pointer
 */
void cxfree(ptr)
    void *ptr;
{
    if (ptr != NULL) {
	cfree(ptr);
    }
}

/*
 * swap doubles and ints
 */
void fswap(x, y)
    double *x, *y;
{
    double tmp;

    tmp = *x;
    *x = *y;
    *y = tmp;
}

void iswap(x, y)
    int *x, *y;
{
    int tmp;

    tmp = *x;
    *x = *y;
    *y = tmp;
}

int isoneof(c, s)
    int c;
    char *s;
{
    while (*s) {
	if (c == *s) {
	    return 1;
	} else {
	    s++;
	}
    }
    return 0;
}

int argmatch(s1, s2, atleast)
    char *s1;
    char *s2;
    int atleast;
{
    int l1 = strlen(s1);
    int l2 = strlen(s2);

    if (l1 < atleast) {
	return 0;
    }
    if (l1 > l2) {
	return 0;
    }
    return (strncmp(s1, s2, l1) == 0);
}

/*
 * convert a string from lower to upper case
 * leaving quoted strings alone
 */
void lowtoupper(s)
    char *s;
{
    int i, quoteon = 0;

    for (i = 0; i < strlen(s); i++) {
	if (s[i] == '"') {
	    if (!quoteon) {
		quoteon = 1;
	    } else {
		quoteon = 0;
	    }
	}
	if (s[i] >= 'a' && s[i] <= 'z' && !quoteon) {
	    s[i] -= ' ';
	}
    }
}

/*
 * remove all that fortran nastiness
 */
void convertchar(s)
    char *s;
{
    while (*s++) {
	if (*s == ',')
	    *s = ' ';
	if (*s == 'D' || *s == 'd')
	    *s = 'e';
    }
}

/*
 * log base 2
 */
int ilog2(n)
    int n;
{
    int i = 0;
    int n1 = n;

    while (n1 >>= 1)
	i++;
    if (1 << i != n)
	return -1;
    else
	return i;
}

/*
 * compute the area bounded by the polygon (xi,yi)
 */
double comp_area(n, x, y)
    int n;
    double x[], y[];

{
    int i;
    double sum = 0.0;

    for (i = 0; i < n; i++) {
	sum = sum + x[i] * y[(i + 1) % n] - y[i] * x[(i + 1) % n];
    }
    return sum * 0.5;
}

/*
 * compute the perimeter bounded by the polygon (xi,yi)
 */
double comp_perimeter(n, x, y)
    int n;
    double x[], y[];

{
    int i;
    double sum = 0.0;

    for (i = 0; i < n - 1; i++) {
	sum = sum + hypot(x[i] - x[(i + 1) % n], y[i] - y[(i + 1) % n]);
    }
    return sum;
}

/* should be macros */
double fmin(x, y)
    double x, y;
{
    return (x < y ? x : y);
}

double fmax(x, y)
    double x, y;
{
    return (x > y ? x : y);
}

/*
 * Time and date routines
 */

char *dayofweekstrs[] = {"Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"};
char *dayofweekstrl[] = {"Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"};
char *months[] = {"Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"};
char *monthl[] = {"January", "February", "March", "April", "May", "June",
"July", "August", "September", "October", "November", "December"};

static int days1[] = {0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334, 365};
static int days2[] = {0, 31, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335, 366};

/*
 * return the Julian day + hms as a real number
 */

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
    } else {
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
void calcdate(jd, m, d, y, h, mi, sec)
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
    } else {
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
	*y += 1;
    }
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

int leapyear(year)
    int year;
{
    if (year % 4 == 0) {
	return (1);
    } else {
	return (0);
    }
}

/*
   get the month and day given the number of days
   from the beginning of the year 'yr'
*/
void getmoday(days, yr, mo, da)
    int days, yr, *mo, *da;
{
    int i;

    if (leapyear(yr)) {
	for (i = 0; i < 13; i++) {
	    if (days <= days2[i]) {
		*mo = i;
		*da = (days - days2[i - 1]);
		goto out1;
	    }
	}
    } else {
	for (i = 0; i < 13; i++) {
	    if (days <= days1[i]) {
		*mo = i;
		*da = (days - days1[i - 1]);
		goto out1;
	    }
	}
    }
out1:;
}

/*
   return the number of days from the beginning of the year 'yr'
*/
int getndays(j)
    double j;
{
    int m, d, y, hh, mm;
    double ss;

    calcdate(j, &m, &d, &y, &hh, &mm, &ss);
    if (leapyear(y)) {
	return days2[m - 1] + d;
    } else {
	return days1[m - 1] + d;
    }
}

/*
   return hms
*/
int gethms(j, h, m, s)
    double j;
    int *h, *m, *s;
{
    double rem = j - (long) j;

    *h = (int) (rem * 24);
    *m = (int) ((rem * 24 - *h) * 60);
    *s = (int) (((rem * 24 - *h) * 60 - *m) * 60);
    *h = (*h + 12) % 24;
}

/*
 * strip special chars from a string
 */
void stripspecial(s, cs)
    char *s, *cs;
{
    int i, slen = strlen(s), curcnt = 0;

    for (i = 0; i < slen; i++) {
	if (s[i] == '\\' && isdigit(s[i + 1])) {
	    i++;
	} else if (s[i] == '\\' && isoneof(s[i + 1], "cCbxsSNuU+-")) {
	    i++;
	} else if (s[i] == '\\' && s[i + 1] == '\\') {
	    i++;
	} else {
	    cs[curcnt++] = s[i];
	}
    }
    cs[curcnt] = 0;
}
