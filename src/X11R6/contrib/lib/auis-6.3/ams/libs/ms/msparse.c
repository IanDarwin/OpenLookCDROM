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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/ams/libs/ms/RCS/msparse.c,v 2.8 1992/12/15 21:20:00 rr2b R6tape $";
#endif

#include <ms.h>
#include <andrewos.h> /* sys/time.h */
#include <stdio.h>
#include <mail.h>

MS_ParseDate(indate, year, month, day, hour, min, sec, wday, gtm)
char *indate;
int *year, *month, *day, *hour, *min, *sec, *wday, *gtm;
{
    struct tm TmBuf;

    if (parsedateheader(indate, &TmBuf, 1, 1, 1, 0)) {
	AMS_RETURN_ERRCODE(EINVAL, EIN_PARSEDATE, EVIA_PARSEDATE);
    }
    if (TmBuf.tm_hour == -1)
	TmBuf.tm_hour = 0;
    if (TmBuf.tm_min == -1)
	TmBuf.tm_min = 0;
    if (TmBuf.tm_sec == -1)
	TmBuf.tm_sec = 0;
    *year = TmBuf.tm_year;
    *month = TmBuf.tm_mon;
    *day = TmBuf.tm_mday;
    *hour = TmBuf.tm_hour;
    *min = TmBuf.tm_min;
    *sec = TmBuf.tm_sec;
    *wday = TmBuf.tm_wday;
    *gtm = gtime(&TmBuf);
    return(0);
}

/* A standin for parsedate() that gets rid of RFC822 comments */

parsedateheader(str, tmp, settm, select, err, gmt)
char *str;
struct tm *tmp;
int settm, select, err;
long *gmt;
{
    char *strstart, *strend, TokenBuf[500], FinalBuf[1500];
    int code822 = is822Atom, prevcode = is822Special;

    FinalBuf[0] = '\0';
    strstart = str;
    strend = str + strlen(str);
    while ((code822 != is822End) && code822) {
	code822 = Next822Word(&strstart, strend, TokenBuf, sizeof(TokenBuf));
	if (((code822 == is822Atom) || (code822 == is822QuotedString)) && ((prevcode == is822Atom) || (prevcode == is822QuotedString))) {
	    strcat(FinalBuf, " ");
	}
	strcat(FinalBuf, TokenBuf);
	prevcode = code822;
    }
    return parsedate(FinalBuf, tmp, settm, select, err, gmt);
}
