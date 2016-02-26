#if defined(timemaker)
/*
From: "Oliver Laumann" <talcott!seismo!unido!tub!net>
Subject: tm_to_time(3) -- convert broken-down time into time_t.
Newsgroups: mod.sources
Approved: jpn@panda.UUCP

Mod.sources:  Volume 4, Issue 38
Submitted by: "Oliver Laumann" <seismo!unido!tub!net>


A number of requests have been posted to the net in the past asking
for a function that can be used to convert a broken-down time (such
as returned by localtime()) into a time_t (such as returned by
time()).
The following shell archive contains a function (plus manual page)
that performs this conversion.

Regards,
    Oliver Laumann, Technical University of Berlin, Germany.
    ...ihnp4!seismo!unido!tub!net   or   net@DB0TUI6.BITNET
          ...!mcvax!unido!tub!net
*/


#include <sys/types.h>
#include <time.h>

/*
 * Return 1 if `y' is a leap year, 0 otherwise.
 */
static int 
leap (y) 
int y; 
{
    y += 1900;
    if (y % 400 == 0)
	return (1);
    if (y % 100 == 0)
	return (0);
    return (y % 4 == 0);
}

/* Return the number of days between Jan 1, 1970 and the given
 * broken-down time.
 */

static int 
ndays (p) 
struct tm *p; 
{
    int n = p->tm_mday;
    int m, y;
    char *md = "\37\34\37\36\37\36\37\37\36\37\36\37";

    for (y = 70; y < p->tm_year; ++y) {
	n += 365;
	if (leap (y)) ++n;
    }
    for (m = 0; m < p->tm_mon; ++m)
	n += md[m] + (m == 1 && leap (y));
    return (n);
}

/* Convert a broken-down time (such as returned by localtime())
 * back into a `time_t'.
 */

time_t 
tm_to_time (tp) 
struct tm *tp; 
{
    int m1, m2;
    time_t t;
    struct tm otm;

    t = (ndays (tp) - 1) * 86400L + tp->tm_hour * 3600L
	+ tp->tm_min * 60 + tp->tm_sec;
    /*
     * Now the hard part -- correct for the time zone:
     */
    otm = *tp;
    tp = localtime (&t);
    m1 = tp->tm_hour * 60 + tp->tm_min;
    m2 = otm.tm_hour * 60 + otm.tm_min;
    t -= ((m1 - m2 + 720 + 1440) % 1440 - 720) * 60L;
    return (t);
}
#endif
