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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/ams/libs/ms/RCS/bldwide.c,v 2.13 1993/09/21 21:52:15 gk5g Exp $";
#endif

#include <andrewos.h>
#include <ms.h>
#include <hdrparse.h>

BuildWideReply(Msg, IncludeFrom)
struct MS_Message *Msg;
Boolean IncludeFrom;
{
    struct ParsedMsg *PStuff;
    char *NewWide;
    int WLen;

    debug(1, ("BuildWideReply\n"));
    PStuff = Msg->ParsedStuff;
    Msg->WideReply = malloc(WLen = 2000);
    if (Msg->WideReply == NULL) {
	AMS_RETURN_ERRCODE( ENOMEM, EIN_MALLOC, EVIA_BUILDWIDEREPLY);
    }
    if (IncludeFrom && PStuff->HeadBody[HP_ALLREPLY]) {
	if ((Msg->ParsedStuff->HeadBodyLen[HP_ALLREPLY]+5) > WLen) {
	    WLen = Msg->ParsedStuff->HeadBodyLen[HP_ALLREPLY]+5;
	    Msg->WideReply = realloc(Msg->WideReply, WLen);
	    if (Msg->WideReply == NULL) {
		AMS_RETURN_ERRCODE( ENOMEM, EIN_MALLOC, EVIA_BUILDWIDEREPLY);
	    }
	}
	/* Use strncat when we could use strncpy, to guarantee null-termination */
	*Msg->WideReply = 0;
	strncat(Msg->WideReply, PStuff->HeadBody[HP_ALLREPLY], PStuff->HeadBodyLen[HP_ALLREPLY]);
	return(0);
    }
    if (PStuff->HeadBody[HP_WIDEREPLY]) {
	if ((Msg->ParsedStuff->HeadBodyLen[HP_WIDEREPLY]+5) > WLen) {
	    WLen = Msg->ParsedStuff->HeadBodyLen[HP_WIDEREPLY]+5+80;
	    Msg->WideReply = realloc(Msg->WideReply, WLen);
	    if (Msg->WideReply == NULL) {
		AMS_RETURN_ERRCODE( ENOMEM, EIN_MALLOC, EVIA_BUILDWIDEREPLY);
	    }
	}
	/* Use strncat when we could use strncpy, to guarantee null-termination */
	*Msg->WideReply = 0;
	strncat(Msg->WideReply, PStuff->HeadBody[HP_WIDEREPLY], PStuff->HeadBodyLen[HP_WIDEREPLY]);
	debug(16, ("Wide Reply field is <%s>\n", Msg->WideReply));
	if (IncludeFrom && Msg->ReplyTo && *Msg->ReplyTo) {
	    if ((strlen(Msg->ReplyTo)+strlen(Msg->WideReply)+5) > WLen) {
		WLen = strlen(Msg->ReplyTo)+strlen(Msg->WideReply)+5;
		Msg->WideReply = realloc(Msg->WideReply, WLen);
		if (Msg->WideReply == NULL) {
		    AMS_RETURN_ERRCODE( ENOMEM, EIN_MALLOC, EVIA_BUILDWIDEREPLY);
		}
	    }
	    strcat(Msg->WideReply, ", ");
	    strcat(Msg->WideReply, Msg->ReplyTo);
	}
	return(0);
    }
    *Msg->WideReply = 0;
    if (PStuff->HeadBody[HP_TO]) {
	if ((Msg->ParsedStuff->HeadBodyLen[HP_TO]+strlen(Msg->WideReply)+5) > WLen) {
	    WLen = Msg->ParsedStuff->HeadBodyLen[HP_TO]+strlen(Msg->WideReply)+5+80;
	    Msg->WideReply = realloc(Msg->WideReply, WLen);
	    if (Msg->WideReply == NULL) {
		AMS_RETURN_ERRCODE(ENOMEM, EIN_MALLOC, EVIA_BUILDWIDEREPLY);
	    }
	}
	/* Note that only the first one omits the comma addition */
	/* Use strncat when we could use strncpy, to guarantee null-termination */
	*Msg->WideReply = 0;
	strncat(Msg->WideReply, PStuff->HeadBody[HP_TO], PStuff->HeadBodyLen[HP_TO]);
    }
#ifdef NOTDEF
    if (PStuff->HeadBody[HP_CC]) {
	if ((PStuff->HeadBodyLen[HP_CC]+strlen(Msg->WideReply)+5) > WLen) {
	    WLen = PStuff->HeadBodyLen[HP_CC]+strlen(Msg->WideReply)+5+80;
	    Msg->WideReply = realloc(Msg->WideReply, WLen);
	    if (Msg->WideReply == NULL) {
		AMS_RETURN_ERRCODE(ENOMEM, EIN_MALLOC, EVIA_BUILDWIDEREPLY);
	    }
	}
	/* The aforementioned comma addition */
	if (*Msg->WideReply) strcat(Msg->WideReply, ", ");
	strncat(Msg->WideReply, PStuff->HeadBody[HP_CC], PStuff->HeadBodyLen[HP_CC]);
    }
#endif /* NOTDEF */
    if (PStuff->HeadBody[HP_RESENTTO]) {
	if ((PStuff->HeadBodyLen[HP_RESENTTO]+strlen(Msg->WideReply)+5) > WLen) {
	    WLen = PStuff->HeadBodyLen[HP_RESENTTO]+strlen(Msg->WideReply)+5+80;
	    Msg->WideReply = realloc(Msg->WideReply, WLen);
	    if (Msg->WideReply == NULL) {
		AMS_RETURN_ERRCODE( ENOMEM, EIN_MALLOC, EVIA_BUILDWIDEREPLY);
	    }
	}
	/* The aforementioned comma addition */
	if (*Msg->WideReply) strcat(Msg->WideReply, ", ");
	strncat(Msg->WideReply, PStuff->HeadBody[HP_RESENTTO], PStuff->HeadBodyLen[HP_RESENTTO]);
    }
    if (IncludeFrom && Msg->ReplyTo) {
	if ((strlen(Msg->ReplyTo)+strlen(Msg->WideReply)+5) > WLen) {
	    WLen = strlen(Msg->ReplyTo)+strlen(Msg->WideReply)+5+80;
	    Msg->WideReply = realloc(Msg->WideReply, WLen);
	    if (Msg->WideReply == NULL) {
		AMS_RETURN_ERRCODE( ENOMEM, EIN_MALLOC, EVIA_BUILDWIDEREPLY);
	    }
	}
	/* The aforementioned comma addition */
	if (*Msg->WideReply) strcat(Msg->WideReply, ", ");
	strcat(Msg->WideReply, Msg->ReplyTo);
    }
    if (PStuff->HeadBody[HP_BCC]) {
	if ((PStuff->HeadBodyLen[HP_BCC]+strlen(Msg->WideReply)+5) > WLen) {
	    WLen = PStuff->HeadBodyLen[HP_BCC]+strlen(Msg->WideReply)+5;
	    Msg->WideReply = realloc(Msg->WideReply, WLen);
	    if (Msg->WideReply == NULL) {
		AMS_RETURN_ERRCODE( ENOMEM, EIN_MALLOC, EVIA_BUILDWIDEREPLY);
	    }
	}
	if (*Msg->WideReply) strcat(Msg->WideReply, ", ");
	strncat(Msg->WideReply, PStuff->HeadBody[HP_BCC], PStuff->HeadBodyLen[HP_BCC]);
    }
    if (!StripMyselfFromAddressList(Msg->WideReply, &NewWide)) {
	free(Msg->WideReply);
	Msg->WideReply = NewWide;
    }
    debug(16, ("Wide Reply field is <%s>\n", Msg->WideReply));
    return(0);
}
