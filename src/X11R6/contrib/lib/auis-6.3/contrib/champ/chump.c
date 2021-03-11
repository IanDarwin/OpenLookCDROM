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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/contrib/champ/RCS/chump.c,v 1.3 1992/12/15 21:49:28 rr2b R6tape $";
#endif

#include <andrewos.h>	/* time.h */
#include <stdio.h>
#include <champ.h>
#include <ctype.h>

extern int errno;

static char *Weekdays[] = {"Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"};

static void PrintNode(en, thisdate)
struct eventnode *en;
struct tm *thisdate;
{
    printf("%s, %d/%d/%d: %s\n", Weekdays[thisdate->tm_wday], thisdate->tm_mon+1, thisdate->tm_mday, thisdate->tm_year+1900, en->event);
}

main(argc, argv) 
int argc;
char **argv;
{
    long clock;
    struct tm *thisdate;
    int lefttocheck;

    if (argc < 2) {
	lefttocheck = 7;
    } else {
	lefttocheck = atoi(argv[1]);
	if (lefttocheck <= 0) {
	    printf("Usage: chump [days-to-check]\n");
	    exit(1);
	}
    }
    clock = time(0);
    thisdate = localtime(&clock);
    ReadDatesFromChampPath(NULL);
    while (lefttocheck > 0) {
	ClearAllFlaggedEvents();
	FlagEventsMatchingDate(thisdate);
	IterateFlaggedEvents(PrintNode, thisdate);
	IncrementDate(thisdate);
	--lefttocheck;
    }
    exit(0);
}

