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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/overhead/util/lib/RCS/gtime.c,v 1.5 1992/12/15 21:10:10 rr2b R6tape $";
#endif

/* gtime.c - inverse of localtime */
/*   value is unpredictable for illegal input values */


 

#include <andrewos.h> /* sys/time.h sys/types.h */

static int dmsize[12] = {31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31};

#define FEBRUARY_P(m) ((m)==1)
/* Gregorian definition: */
#define LEAPYEAR_P(y) (  ((y)%4==0) && ((((y)%100) != 0) || (((y)%400) == 0)))
/* Used only in a relative sense */
#define LEAPDAYS_SINCE(y) ( (y)/4 - (y)/100 + (y)/400 )

/* return inverse of local time */

#if defined(_POSIX_SOURCE) || defined(_IBMR2)
time_t gtime(ct)
register struct tm *ct;
{
    return (mktime(ct));
}
#else /* _POSIX_SOURCE */
time_t gtime(ct)
register struct tm *ct;
{
    time_t copyt;
    register int day;
    register int month;
    register int year;
    static int init = 0;

    /* this part is the inverse of gmtime */

    year = ct->tm_year + 1900;
    month = ct->tm_mon;
    day = ct->tm_mday - 1;
    if (month >= 12)
	month = 11;
    while (month-- > 0)
      day += ((FEBRUARY_P(month) && LEAPYEAR_P(year)) ? 29 : dmsize[month]);
    /* The following calculation is broken for the year 2100: */
    day += (year - 1970) * 365 + (((year - 1901) / 4) - 17);
    /* here's the fix:
    day += ( ((year)*356 + LEAPDAYS_SINCE(year))
           - (1970*356 + LEAPDAYS_SINCE(1970)) ); */
    copyt = ct->tm_sec + 60 * (ct->tm_min + 60 * (ct->tm_hour + 24 * (long)day));

    /* this inverts the dst adjustment in localtime */

    if (init == 0) {
	osi_SetZone();
	init = 1;
    }
    if (osi_IsEverDaylight && ct->tm_isdst)
	copyt -= 60*60;
    return (copyt + (time_t)osi_SecondsWest);
}
#endif /* _POSIX_SOURCE */

#ifdef TESTINGONLYTESTING
#include <stdio.h>
main()
{
  struct tm *ct;
  unsigned long int the_time, parsed_time;

  the_time = osi_GetSecs();
  ct = localtime(&the_time);
  parsed_time = (long int) gtime(ct);
  printf("The time is %ld, parsed time is %ld.\n", the_time, parsed_time);
  if (the_time != parsed_time) printf("And they are DIFFERENT!  (by a factor of %ld)\n", the_time - parsed_time);
}
#endif /* TESTINGONLYTESTING */
