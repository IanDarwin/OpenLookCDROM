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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/ams/libs/ms/RCS/blddate.c,v 2.11 1992/12/15 21:17:22 rr2b R6tape $";
#endif

#include <ms.h>
#include <hdrparse.h>
#include <andrewos.h> /* sys/time.h */

BuildDateField(Msg, datetype)
struct MS_Message *Msg;
int datetype;
{
    unsigned long when;
    char DateBuf[250];
    struct tm TmBuf;

    debug(1, ("Build date field\n"));
    switch(datetype) {
	case DATETYPE_FROMHEADER:
	    if (Msg->ParsedStuff->HeadBody[HP_DATE]) {
		when = Msg->ParsedStuff->HeadBodyLen[HP_DATE];
		if (when > (sizeof(DateBuf)-1)) when = (sizeof(DateBuf)-1);
		strncpy(DateBuf, Msg->ParsedStuff->HeadBody[HP_DATE], when);
		DateBuf[when] = '\0';
		if (!parsedateheader(DateBuf, &TmBuf, 1, 1, 1, 0 /* &when */)) {
		    when = (unsigned long) gtime(&TmBuf);
		    if (when <= ((unsigned long) time(0) + 7*24*60*60) && when <= (unsigned long) 0xc0000000) break;
		}
	    } /* drop through */
	case DATETYPE_FROMFILE:
	    when = (unsigned long) Msg->RawFileDate;
	    if (when <= ((unsigned long) time(0) + 7*24*60*60) && when <= (unsigned long) 0xc0000000) break;
	case DATETYPE_CURRENT:
	default:
	    when = (unsigned long) time(0);
	    if (when > ((unsigned long) time(0) + 7*24*60*60) || when > (unsigned long) 0xc0000000) {
		AMS_RETURN_ERRCODE(EMSCLOCKBOGUS, EIN_PARAMCHECK, EVIA_BUILDDATE);
	    }
	    break;
    }
    strcpy(AMS_DATE(Msg->Snapshot), convlongto64(when, 0));
    return(0);
}
