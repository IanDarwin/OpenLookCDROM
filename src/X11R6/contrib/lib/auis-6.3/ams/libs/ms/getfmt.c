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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/ams/libs/ms/RCS/getfmt.c,v 2.8 1992/12/15 21:19:11 rr2b R6tape $";
#endif


 

#include <ms.h>
#include <hdrparse.h>

GetFormatFromMessage(Msg, ThisFormat, bufsize, IsBE2)
struct MS_Message *Msg;
char *ThisFormat;
int bufsize, *IsBE2;
{
    char HeadBuf[2000], fmttype[25], fmtvers[25], fmtresources[200];
    int len;

    ThisFormat[0] = '\0';
    len = Msg->ParsedStuff->HeadBodyLen[HP_CONTENTTYPE];
    if (len > 0) {
	if (len > (sizeof(HeadBuf) - 1)) len = sizeof(HeadBuf) - 1;
	strncpy(HeadBuf, Msg->ParsedStuff->HeadBody[HP_CONTENTTYPE], len);
	HeadBuf[len] = '\0';
	BreakDownContentTypeField(HeadBuf, fmttype, sizeof(fmttype), fmtvers, sizeof(fmtvers), fmtresources, sizeof(fmtresources));
	if (!lc2strncmp("x-be2", fmttype, strlen(fmttype))) {
	    strncpy(ThisFormat, fmtvers, bufsize);
	}
    } else {
	len = Msg->ParsedStuff->HeadBodyLen[HP_SCRIBEFORMAT];
	if (len > 0) {
	    if (len > (sizeof(HeadBuf) - 1)) len = sizeof(HeadBuf) - 1;
	    strncpy(HeadBuf, Msg->ParsedStuff->HeadBody[HP_SCRIBEFORMAT], len);
	    HeadBuf[len] = '\0';
	    strncpy(ThisFormat, StripWhiteEnds(HeadBuf), bufsize);
	}
    }
    *IsBE2 = (ThisFormat[0]) ? 1 : 0;
    return(0);
}

