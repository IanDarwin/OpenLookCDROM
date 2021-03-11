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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/contrib/champ/RCS/champ.c,v 1.3 1992/12/15 21:48:53 rr2b R6tape $";
#endif

#include "class.h"
#include "champ.eh"

extern struct eventnode *readdateintoeventnode();

int champ__ReadDatesFromChampPath(c, path)
struct classheader *c;
char *path;
{
    return(ReadDatesFromChampPath(path));
}

void champ__ClearAllFlaggedEvents(c)
struct classheader *c;
{
    ClearAllFlaggedEvents();
}

int champ__FlagEventsMatchingDate(c, thisdate)
struct classheader *c;
struct tm *thisdate;
{
    return(FlagEventsMatchingDate(thisdate));
}


void champ__IterateFlaggedEvents(c, proc, rock)
struct classheader *c;
int (*proc)();
long rock;
{
    IterateFlaggedEvents(proc, rock);
}

void champ__IncrementDate(c, d)
struct classheader *c;
struct tm *d;
{
    IncrementDate(d);
}

struct eventnode *champ__ReadDateIntoEventNode(c, str)
struct classheader *c;
char *str;
{
    return(readdateintoeventnode(str));
}

