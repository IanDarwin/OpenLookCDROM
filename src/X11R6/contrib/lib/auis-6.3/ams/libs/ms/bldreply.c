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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/ams/libs/ms/RCS/bldreply.c,v 2.11 1993/09/21 21:51:22 gk5g Exp $";
#endif

#include <andrewos.h>
#include <ms.h>
#include <hdrparse.h>

BuildReplyField(Msg)
struct MS_Message *Msg;
{
    struct ParsedMsg *PStuff;

    debug(1, ("Building Reply field\n"));
    PStuff = Msg->ParsedStuff;
    if (PStuff->HeadBody[HP_REPLY_TO]) {
	if (Msg->ReplyTo)
	    free(Msg->ReplyTo);
	Msg->ReplyTo = malloc(1+PStuff->HeadBodyLen[HP_REPLY_TO]);
	if (Msg->ReplyTo == NULL) {
	    AMS_RETURN_ERRCODE( ENOMEM, EIN_MALLOC, EVIA_BUILDREPLY);
	}
	strncpy(Msg->ReplyTo, PStuff->HeadBody[HP_REPLY_TO], PStuff->HeadBodyLen[HP_REPLY_TO]);
	Msg->ReplyTo[PStuff->HeadBodyLen[HP_REPLY_TO]] = '\0';
    } else if (PStuff->HeadBody[HP_FROM]) {
	if (Msg->ReplyTo)
	    free(Msg->ReplyTo);
	Msg->ReplyTo = malloc(1+PStuff->HeadBodyLen[HP_FROM]);
	if (Msg->ReplyTo == NULL) {
	    AMS_RETURN_ERRCODE( ENOMEM, EIN_MALLOC, EVIA_BUILDREPLY);
	}
	strncpy(Msg->ReplyTo, PStuff->HeadBody[HP_FROM], PStuff->HeadBodyLen[HP_FROM]);
	Msg->ReplyTo[PStuff->HeadBodyLen[HP_FROM]] = '\0';
    } 
/* The following are commented out on cfe's advice: */
#ifdef UNDEFINEDGARBAGE
    else if (PStuff->HeadBody[HP_RESENTFROM]) {
	if (Msg->ReplyTo)
	    free(Msg->ReplyTo);
	Msg->ReplyTo = malloc(1+PStuff->HeadBodyLen[HP_RESENTFROM]);
	if (Msg->ReplyTo == NULL) {
	    AMS_RETURN_ERRCODE( ENOMEM, EIN_MALLOC, EVIA_BUILDREPLY);
	}
	strncpy(Msg->ReplyTo, PStuff->HeadBody[HP_RESENTFROM], PStuff->HeadBodyLen[HP_RESENTFROM]);
	Msg->ReplyTo[PStuff->HeadBodyLen[HP_RESENTFROM]] = '\0';
    } else if (PStuff->HeadBody[HP_SENDER]) {
	if (Msg->ReplyTo)
	    free(Msg->ReplyTo);
	Msg->ReplyTo = malloc(1+PStuff->HeadBodyLen[HP_SENDER]);
	if (Msg->ReplyTo == NULL) {
	    AMS_RETURN_ERRCODE( ENOMEM, EIN_MALLOC, EVIA_BUILDREPLY);
	}
	strncpy(Msg->ReplyTo, PStuff->HeadBody[HP_SENDER], PStuff->HeadBodyLen[HP_SENDER]);
	Msg->ReplyTo[PStuff->HeadBodyLen[HP_SENDER]] = '\0';
    } else if (PStuff->HeadBody[HP_RETURNPATH]) {
	if (Msg->ReplyTo)
	    free(Msg->ReplyTo);
	Msg->ReplyTo = malloc(1+PStuff->HeadBodyLen[HP_RETURNPATH]);
	if (Msg->ReplyTo == NULL) {
	    AMS_RETURN_ERRCODE( ENOMEM, EIN_MALLOC, EVIA_BUILDREPLY);
	}
	strncpy(Msg->ReplyTo, PStuff->HeadBody[HP_RETURNPATH], PStuff->HeadBodyLen[HP_RETURNPATH]);
	Msg->ReplyTo[PStuff->HeadBodyLen[HP_RETURNPATH]] = '\0';
    }
#endif /* UNDEFINEDGARBAGE */
    debug(16, ("Reply to field is <%s>\n", Msg->ReplyTo));
    return(0);
}
