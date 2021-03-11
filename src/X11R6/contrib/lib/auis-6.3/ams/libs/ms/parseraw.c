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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/ams/libs/ms/RCS/parseraw.c,v 2.15 1992/12/15 21:20:00 rr2b R6tape $";
#endif

#include <stdio.h>
#include <ms.h>
#include <hdrparse.h>
#include <andrewos.h>

extern char *StandardHeaderNames[];

#define ALLOCCHUNK (10+HP_END_PREDEFINED)

/* The following routine takes a raw file as delivered by the mail system
	and build an MS_Message structure for it.  They will malloc space
	for Snapshot, ParsedStuff, ReplyTo, and WideReply which the caller
	must later free.  (The other stuff will be freed when you free the
	RawMessage, which is malloced by the first of these routines.) */

ParseMessageFromRawBody(NewMessage)
struct MS_Message *NewMessage;
{
    if (NewMessage->Snapshot) free(NewMessage->Snapshot);
    NewMessage->Snapshot = malloc(AMS_SNAPSHOTSIZE);
    if (NewMessage->Snapshot == NULL) {
	AMS_RETURN_ERRCODE(ENOMEM, EIN_MALLOC, EVIA_PARSEMSGFROMRAWBODY);
    }
    bzero(NewMessage->Snapshot, AMS_SNAPSHOTSIZE);
    if (NewMessage->ReplyTo) free(NewMessage->ReplyTo);
    NewMessage->ReplyTo = NULL;
    if (NewMessage->WideReply) free(NewMessage->WideReply);
    NewMessage->WideReply = NULL;
    mserrcode = OnlyParseMessageFromRawBody(NewMessage);
    if (mserrcode) {
	if (NewMessage->ReplyTo) free(NewMessage->ReplyTo);
	NewMessage->ReplyTo = NULL;
	if (NewMessage->WideReply) free(NewMessage->WideReply);
	NewMessage->WideReply = NULL;
	if (NewMessage->Snapshot) free(NewMessage->Snapshot);
	NewMessage->Snapshot = NULL;
    }
    return(mserrcode);
}

/* The following does the real work of the above routine, without all the aggressive malloc'ing and freeing; it is separated primarily for use by AlterMessage */

OnlyParseMessageFromRawBody(NewMessage)
struct MS_Message *NewMessage;
{
    char   *s,
           *t,
           *u;
    int     i,
            tlen,
            ulen = 0;
    struct ParsedMsg   *PStuff;
    char   *RawBody;

    debug(1,("Entering OnlyParseMessageFromRawBody with body: \n%s\n", NewMessage->RawBits));
    RawBody = NewMessage->RawBits;
    if (NewMessage->ParsedStuff) FreeParsedStuff(NewMessage);
    PStuff = NewMessage->ParsedStuff = (struct ParsedMsg   *) malloc (sizeof (struct ParsedMsg));
    if (PStuff == NULL) {
	AMS_RETURN_ERRCODE(ENOMEM, EIN_MALLOC, EVIA_PARSEMSGFROMRAWBODY);
    }
    PStuff->HeadsAllocated = ALLOCCHUNK;
    PStuff->IsDuplicated = NULL;
    PStuff->HeadBodyLen = NULL;
    PStuff->HeadBody = NULL;
    PStuff->HeadName = (char **) malloc(PStuff->HeadsAllocated * sizeof (char *));
    PStuff->HeadBody = (char **) malloc(PStuff->HeadsAllocated * sizeof (char *));
    PStuff->HeadBodyLen = (int *) malloc(PStuff->HeadsAllocated * sizeof (int));
    PStuff->IsDuplicated = (Boolean *) malloc(PStuff->HeadsAllocated * sizeof (Boolean));
    if (!PStuff->HeadName || !PStuff->HeadBody || !PStuff->HeadBodyLen || !PStuff->IsDuplicated) {
	if (PStuff->IsDuplicated) free(PStuff->IsDuplicated);
	if (PStuff->HeadBodyLen) free(PStuff->HeadBodyLen);
	if (PStuff->HeadBody) free(PStuff->HeadBody);
	if (PStuff->HeadName) free(PStuff->HeadName);
	free(PStuff);
	AMS_RETURN_ERRCODE(ENOMEM, EIN_MALLOC, EVIA_PARSEMSGFROMRAWBODY);
    }
    bzero(PStuff->HeadName, PStuff->HeadsAllocated * sizeof (char *));
    bzero(PStuff->HeadBody, PStuff->HeadsAllocated * sizeof (char *));
    bzero(PStuff->HeadBodyLen, PStuff->HeadsAllocated * sizeof (int));
    bzero(PStuff->IsDuplicated, PStuff->HeadsAllocated * sizeof (Boolean));
    bcopy(StandardHeaderNames, PStuff->HeadName, HP_END_PREDEFINED * sizeof(char *));

    /* If there are NULL characters in a message, we turn them into newlines for server sanity */
    for (s=RawBody; s < (RawBody + NewMessage->HeadSize); ++s) {
	if (!*s) *s = '\n';
    }

    s = RawBody;
    while (*s) {
    /* Beginning of new header */
	t = s;
	while (*++s && *s != ':' && *s != '\n') {
	    ;
	}
	if (!*s) {
	    break;
	}
	if (*s == '\n') {
	    if (*++s == '\n') {
		debug(16, ("Last header has no colon\n"));
		break;
	    }
	    debug(16, ("Ignoring a header with no colon\n"));
	    continue;
	}
	tlen = s - t;
	u = ++s;
	while (*s) {
	    if (*s == '\n' && *(s + 1) != ' ' && *(s + 1) != '\t') {
		ulen = s - u;
		s++;
		break;
	    }
	    ++s;
	}
	if (!*s) {
	    break;
	}
	while (*u == ' ' || *u == '\t') {
	    ++u;
	    --ulen;
	}
	if (ulen >= 0) { /* Will this actually work???  Used to be > 0 */
	    if (MSDebugging & 16) {
		printf(", Header body is ");
		fwrite(u, sizeof(*u), ulen, stdout);
	    }
	    /* Header body is now pointed to by u */
	    for (i = 0; TRUE; ++i) {
		if ((i+1) >= PStuff->HeadsAllocated) {
		    int oldalloced = PStuff->HeadsAllocated;

		    debug(16, ("REALLOCATING THE HEADERS - %d so far!!\n", i));
		    PStuff->HeadsAllocated += ALLOCCHUNK;
		    PStuff->HeadName = (char **) realloc(PStuff->HeadName, PStuff->HeadsAllocated * sizeof (char *));
		    PStuff->HeadBody = (char **) realloc(PStuff->HeadBody, PStuff->HeadsAllocated * sizeof (char *));
		    PStuff->HeadBodyLen = (int *) realloc(PStuff->HeadBodyLen, PStuff->HeadsAllocated * sizeof (int));
		    PStuff->IsDuplicated = (Boolean *) realloc(PStuff->IsDuplicated, PStuff->HeadsAllocated * sizeof (Boolean));
		    if (!PStuff->HeadName || !PStuff->HeadBody || !PStuff->HeadBodyLen || !PStuff->IsDuplicated) {
			if (PStuff->IsDuplicated) free(PStuff->IsDuplicated);
			if (PStuff->HeadBodyLen) free(PStuff->HeadBodyLen);
			if (PStuff->HeadBody) free(PStuff->HeadBody);
			if (PStuff->HeadName) free(PStuff->HeadName);
			free(PStuff);
			AMS_RETURN_ERRCODE(ENOMEM, EIN_MALLOC, EVIA_PARSEMSGFROMRAWBODY);
		    }
		    bzero(&PStuff->HeadName[oldalloced], (PStuff->HeadsAllocated-oldalloced) * sizeof (char *));
		    bzero(&PStuff->HeadBody[oldalloced], (PStuff->HeadsAllocated-oldalloced) * sizeof (char *));
		    bzero(&PStuff->HeadBodyLen[oldalloced], (PStuff->HeadsAllocated-oldalloced) * sizeof (int));
		    bzero(&PStuff->IsDuplicated[oldalloced], (PStuff->HeadsAllocated-oldalloced) * sizeof (Boolean));
		}
		if (!PStuff->HeadName[i]) {
		    PStuff->HeadName[i] = malloc(tlen + 1);
				/* Will be freed by FreeMessage */
		    strncpy(PStuff->HeadName[i], t, tlen);
		    PStuff->HeadName[i][tlen] = 0;
		    LowerStringInPlace(PStuff->HeadName[i], tlen);
		    PStuff->HeadBody[i] = u;
		    PStuff->HeadBodyLen[i] = ulen;
		    debug(16,(" (Parsed: new, type %s)\n", PStuff->HeadName[i]));
		    break;
		}
		if ((tlen == strlen(PStuff->HeadName[i])) && !lc2strncmp(PStuff->HeadName[i], t, tlen)) {
		    if (PStuff->HeadBody[i]) {
			PStuff->IsDuplicated[i] = TRUE;
			debug(16,(" (Duplicate %s header)\n", PStuff->HeadName[i]));
			continue;
		    }
		    PStuff->HeadBody[i] = u;
		    PStuff->HeadBodyLen[i] = ulen;
		    debug(16,(" (Parsed: standard %s header)\n", PStuff->HeadName[i]));
		    break;
		}
	    }
	}
	if (*s == '\n') {	/* done with all headers */
	    break;
	}
    /* We are now pointing to the next header and can loop again. */
    }
    return(0);
}
