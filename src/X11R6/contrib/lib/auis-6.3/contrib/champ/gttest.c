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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/contrib/champ/RCS/gttest.c,v 1.3 1992/12/15 21:49:28 rr2b R6tape $";
#endif

#include <andrewos.h>	/* time.h */
#include <stdio.h>
#include <champ.h>
#include <ctype.h>

extern char *index();
extern int errno;

static int MonthLengths[] = {31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31};

/* fake */ long gtime(tm)
struct tm *tm;
{
    int result;
    result = tm->tm_year - 70;
    result = (result * 365) +(result/4) + tm->tm_yday;
    result = (result * 24) + tm->tm_hour;
    result = (result * 60) + tm->tm_min;
    result = (result * 60) + tm->tm_sec;
    return(result);
}

main(argc, argv) 
int argc;
char **argv;
{
    long clock, gt;
    struct tm *thisdate;
    int lefttocheck;

    if (argc < 2) {
	lefttocheck = 7;
    } else {
	lefttocheck = atoi(argv[1]);
	if (lefttocheck <= 0) {
	    printf("Usage: gttest [days-to-check]\n");
	    exit(1);
	}
    }
    clock = time(0);
    thisdate = gmtime(&clock);
    do {
	IncrementDate(thisdate);
	clock += 24*60*60;
	gt = gtime(thisdate);
	printf("%s %d %d %d\n", (gt == clock) ? "GOOD" : "BAD", gt, clock, gt-clock);
	--lefttocheck;
    } while (lefttocheck > 0);
}

IncrementDate(d)
struct tm *d;
{
    ++d->tm_yday;
    if (++d->tm_wday > 6) d->tm_wday = 0;
    ++d->tm_mday;
    if ((d->tm_mday > MonthLengths[d->tm_mon])
	 || ((d->tm_year%4 != 0 || (d->tm_year%100 == 0 && (d->tm_year+1900)%400 != 0))
	     && (d->tm_mday == 29))) {
	++d->tm_mon;
	d->tm_mday = 1;
	if (d->tm_mon >= 12) {
	    d->tm_mon = 0;
	    ++d->tm_year;
	    d->tm_yday = 0; /* IS THIS RIGHT OR SHOULD IT BE 1??? */
	}
    }
}
