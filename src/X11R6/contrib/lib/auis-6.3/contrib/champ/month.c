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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/contrib/champ/RCS/month.c,v 1.3 1992/12/15 21:49:28 rr2b R6tape $";
#endif

#include <andrewos.h>	/* time.h */
#include <stdio.h>
#include <class.h>
#include <month.eh>

boolean month__InitializeObject(c, self)
struct classheader *c;
struct month *self;
{
    long clock = time(0);
    struct tm *thisdate = localtime(&clock);

    self->mon = thisdate->tm_mon;
    self->year = thisdate->tm_year;
    return(TRUE);
}

long month__Write(self, fp, id, level)
struct month *self;
FILE *fp;
long id;
int level;
{
    long clock;
    struct tm *thisdate;

    if (id != month_GetWriteID(self)) {
	/* New write operation */
	month_SetWriteID(self, id);
	clock = time(0);
	thisdate = localtime(&clock);
	fprintf(fp, "\\begindata{%s,%d}\n%d\n%d\n\\enddata{%s,%d}\n",
		class_GetTypeName(self), month_UniqueID(self),
		self->mon - thisdate->tm_mon, self->year - thisdate->tm_year,
		class_GetTypeName(self), month_UniqueID(self));
    }
    return(month_UniqueID(self));
}

long month__Read(self, fp, id)
struct month *self;
FILE *fp;
long id;
{
    char LineBuf[250];
    long clock = time(0);
    struct tm *thisdate = localtime(&clock);

    month_SetID(self, month_UniqueID(self));
    if (fgets(LineBuf,sizeof(LineBuf), fp) == NULL) {
	return(dataobject_PREMATUREEOF);
    }
    self->mon = thisdate->tm_mon + atoi(LineBuf);
    if (fgets(LineBuf,sizeof(LineBuf), fp) == NULL) {
	return(dataobject_PREMATUREEOF);
    }
    self->year = thisdate->tm_year + atoi(LineBuf);
    /* Now read in the \enddata line */
    if (fgets(LineBuf,sizeof(LineBuf), fp) == NULL) {
	return(dataobject_PREMATUREEOF);
    }
    if (strncmp(LineBuf, "\\enddata", 8)) {
	return(dataobject_MISSINGENDDATAMARKER);
    }
    return(dataobject_NOREADERROR);
}

void month__SetMonthAndYear(self, mon, year)
struct month *self;
int mon, year;
{
    self->mon = mon;
    self->year = year;
    month_NotifyObservers(self, 0);
}
