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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/ams/libs/ms/RCS/addhdr.c,v 2.15 1993/05/04 00:58:50 susan Exp $";
#endif

#include <andrewos.h>
#include <ms.h>
#include <hdrparse.h>

#ifndef _IBMR2
extern char *malloc();
#endif /* _IBMR2 */

#define ALTER_ADD 0
#define ALTER_DEL 1

AddHeader(Msg, Head)
struct MS_Message *Msg;
char *Head;
{
    return(AlterMessage(Msg, ALTER_ADD, 0, Head));
}

DeleteHeader(Msg, num)
struct MS_Message *Msg;
int num;
{
    return(AlterMessage(Msg, ALTER_DEL, num, NULL));
}

DelHeaderByName(Msg, Head)
struct MS_Message *Msg;
char *Head;
{
    int i, len;

    len = strlen(Head);
    for (i=0; Msg->ParsedStuff->HeadName[i]; ++i) {
	if (Msg->ParsedStuff->HeadName[i][len] == '\0' && !lc2strncmp(Msg->ParsedStuff->HeadName[i], Head, len)) {
	    break;
	}
    }
    if (!Msg->ParsedStuff->HeadName[i]) {
	AMS_RETURN_ERRCODE(EINVAL, EIN_PARAMCHECK, EVIA_CHOOSEDIRECTORIES);
    }
    return(DeleteHeader(Msg, i));
}

AlterMessage(Msg, code, delhead, Head)
struct MS_Message *Msg;
int code, delhead;
char *Head;
{
    char *newRawBits;

    if (code == ALTER_ADD) {
	int newHeaderLen;
	debug(1,("Addheader\n"));
	newHeaderLen = strlen(Head) + 1; /* add 1 for newline */
	newRawBits = malloc(newHeaderLen + Msg->HeadSize + 1); /* add 1 for terminator */
	if (!newRawBits) {
	    AMS_RETURN_ERRCODE(ENOMEM, EIN_MALLOC, EVIA_CHOOSEDIRECTORIES);
	}
	strcpy(newRawBits, Head); 
	strcat(newRawBits, "\n");
	strcat(newRawBits, Msg->RawBits);
	Msg->FullSize += newHeaderLen;
	Msg->HeadSize += newHeaderLen;
    } else {
	char *cutHere;
	int firstPartLen, headerLen, cutLen;

	debug(1, ("Delete header\n"));
	if ((delhead < 0) || (delhead >= Msg->ParsedStuff->HeadsAllocated)) {
	    AMS_RETURN_ERRCODE(EINVAL, EIN_PARAMCHECK, EVIA_CHOOSEDIRECTORIES);
	}
	headerLen = Msg->ParsedStuff->HeadBodyLen[delhead];
	cutHere = Msg->ParsedStuff->HeadBody[delhead];
	if ((headerLen < 0) || (cutHere == NULL)) {
	    return(0); /* Already not there */
	}
	if (cutHere) { /* try to backup to previous newline */
	    --cutHere; /* to work right when the header is empty */
	    while ((cutHere > Msg->RawBits) && *cutHere && *cutHere != '\n') --cutHere;
	}
	cutLen = (Msg->ParsedStuff->HeadBody[delhead] - cutHere) + headerLen;
	if (cutHere <= Msg->RawBits) 
	    ++cutLen; /* to avoid a leading newline */
	newRawBits = malloc(Msg->HeadSize - cutLen + 1); /* add 1 for terminator */
	if (!newRawBits) 
	    AMS_RETURN_ERRCODE(ENOMEM, EIN_MALLOC, EVIA_CHOOSEDIRECTORIES);
	if (cutHere > Msg->RawBits) {
	    firstPartLen = cutHere - Msg->RawBits;
	    bcopy(Msg->RawBits, newRawBits, firstPartLen); /* copy firstPart */
	} else {
	    firstPartLen = 0;
	}
	bcopy(cutHere + cutLen, newRawBits + firstPartLen, Msg->HeadSize - (cutLen + firstPartLen) + 1); /* copy lastPart; add 1 to copy terminator */
	Msg->FullSize -= cutLen;
	Msg->HeadSize -= cutLen;
    }
    /* Parsed stuff has pointers into raw message which must be fixed now */

    free(Msg->RawBits);
    Msg->RawBits = newRawBits;
    mserrcode = OnlyParseMessageFromRawBody(Msg);
    Msg->WeFiddled = TRUE;
    return(mserrcode);
}

