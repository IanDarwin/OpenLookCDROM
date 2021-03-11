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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/ams/libs/ms/RCS/authuid.c,v 2.18 1992/12/15 21:17:22 rr2b R6tape $";
#endif

#include <andrewos.h>
#include <ms.h>
#include <util.h>
#include <mailconf.h>
#include <hdrparse.h>
#ifdef AFS_ENV
#include <afs/param.h>
#ifdef CMUCS
#include <afs/ptint.h>
#include <afs/ptserver.h>
#else
#include <afs/print.h>
#include <afs/prserver.h>
#endif
#endif /* AFS_ENV */

extern char *StripWhiteEnds(), *GenAuthField();
extern int postmanvuid, myvuid, homeUsesAMSDelivery;
extern char MyMailDomain[];

/* There are really three cases here.  either we want to trust the name in
  the X-Andrew-Authenticated-As header, or we want to use the vuid/cell information
  to build up an authenticated name, or we want to consider it to be unauthenticated
  mail and to simply use the From: header.

  Unfortunately, there are 6 possible values for both the authenticated writer
      of the mail file AND the vuid in the authentication header, and the convoluted
      code below is a reflection of the painful resolution of those 36 cases.  The 6 cases are
      ANONYMOUSID,  ANYUSERID, postmanvuid, myvuid, 0 (root-net mail),
	and anything else.

	      Just trust us, it's right.
*/


CheckAuthUid(NewMessage)
struct MS_Message *NewMessage;
{
    char *cellname, *s;
    char CopyOfAuthHeader[1000];
    int len, idfromauthhdr;
    Boolean IsMyCell;

    if (homeUsesAMSDelivery < 0 || !AMS_ViceIsRunning || NewMessage->AuthCell == NULL) {
	/* All mail is considered unauthenticated.  */
	NewMessage->AuthUid = 0;
	return(0);
    }
    if (!NewMessage->ParsedStuff->HeadBody[HP_AUTHENTICATED_AS]) {
	/* The AuthUid/AuthCell fields are good enough */
	return(0);
    }
    len = NewMessage->ParsedStuff->HeadBodyLen[HP_AUTHENTICATED_AS];
    strncpy(CopyOfAuthHeader, NewMessage->ParsedStuff->HeadBody[HP_AUTHENTICATED_AS], len);
    CopyOfAuthHeader[len] = '\0';

    if (strncmp(CopyOfAuthHeader, "UID", 3)) {
	idfromauthhdr  = atoi(CopyOfAuthHeader);
    } else {
	idfromauthhdr = atoi(CopyOfAuthHeader + 3);
    }
    IsMyCell = ULstrcmp(NewMessage->AuthCell, MyMailDomain) == 0;
#ifndef PHASINGOUTEVENTUALLY	/* pending release... */
    if (idfromauthhdr == postmanvuid) idfromauthhdr = 0; /* formerly only in else clause */
#endif /* PHASINGOUTEVENTUALLY	 */
    if (((NewMessage->AuthUid == 0)
#ifdef AFS_ENV
	  || (NewMessage->AuthUid == ANONYMOUSID) || (NewMessage->AuthUid == ANYUSERID)
#endif /* AFS_ENV */
	  ) || ((((NewMessage->AuthUid == postmanvuid)
		 || (NewMessage->AuthUid == myvuid)) && IsMyCell)
		&& (
#ifdef AFS_ENV
		    (idfromauthhdr == ANONYMOUSID) || (idfromauthhdr == ANYUSERID) ||
#endif /* AFS_ENV */
		    (idfromauthhdr == 0)
#ifndef PHASINGOUTEVENTUALLY /* Pending release of delivery system changes */
		    || ((idfromauthhdr == postmanvuid) && IsMyCell)
#endif /* PHASINGOUTEVENTUALLY  */
		    ))) {
	/* This is the case where the mail is considered unauthenticated.
	    We erase any in-memory traces and the caption-building routines
	    will use the From: header and insist on @foo for some value of foo.
		*/
	NewMessage->AuthUid = 0;
	if (NewMessage->AuthCell) free(NewMessage->AuthCell);
	if (NewMessage->AuthName) free(NewMessage->AuthName);
	NewMessage->AuthName = NULL;
	NewMessage->AuthCell = NULL;
    } else if (((NewMessage->AuthUid == postmanvuid) || (NewMessage->AuthUid == myvuid)) && IsMyCell) {
	/* This is the case where we believe the authentication information.
	    We extract the AuthName in order to avoid wp lookups, but we
	    also hang onto the id and cell as well. */
	NewMessage->AuthUid = idfromauthhdr;
	cellname = strchr(CopyOfAuthHeader, ';');
	if (cellname) {
	    s = strchr(++cellname, ';');
	    if (s) *s++ = '\0';
	    cellname = StripWhiteEnds(cellname);
	    if (NewMessage->AuthCell != NULL) {
		if (ULstrcmp(NewMessage->AuthCell, cellname) != 0) {
		    free(NewMessage->AuthCell);
		    NewMessage->AuthCell = NULL;
		}
	    }
	    if (NewMessage->AuthCell == NULL) {
		NewMessage->AuthCell = NewString(cellname);
		/* bogus -- ignore malloc errors here */
	    }
	    if (s) {
		int WasQuoted;

		s = StripWhiteEnds(s);
		WasQuoted = 0;
		if (*s == '\"') {
		    char *t;

		    WasQuoted = 1;
		    ++s;
		    t = strrchr(s, '\"');
		    if (t) *t = '\0';
		}
		if (NewMessage->AuthName) free(NewMessage->AuthName);
		NewMessage->AuthName = malloc(1+strlen(s));
		if (NewMessage->AuthName) {
		    /* bogus -- ignore malloc errors here */
		    char *Src, *Dst;

		    if (WasQuoted) {
			strcpy(NewMessage->AuthName, s);
		    } else {
			Dst = NewMessage->AuthName;
			for (Src = s; *Src != '\0' && *Src != ';'; ++Src) {
			    if (*Src == '\\' && Src[1] != '\0') ++Src;
			    *Dst++ = *Src;
			}
			*Dst++ = '\0';
			/* If there are more params, *Src will be ';' here. */
		    }
		}
	    }
	}
	debug(16, ("Reset authorized uid to %d, cell %s, auth name %s\n", NewMessage->AuthUid, NewMessage->AuthCell ? NewMessage->AuthCell : "<cell name>", NewMessage->AuthName ? NewMessage->AuthName : "<NULL>"));
    } else {
	/* We are not interested in the authentication field contents here, in that we believe we know the person who wrote the file and we don't want to trust what he wrote in the authentication header.  Therefore, we need to make sure it isn't a lie. */
	char FullName[1000], *ahead;

	GetCellularUserName(NewMessage->AuthUid, NewMessage->AuthCell ? NewMessage->AuthCell : WorkstationName, FullName, sizeof(FullName));
	NewMessage->AuthName = malloc(1+strlen(FullName));
	if (NewMessage->AuthName) {
	    /* Once again we just bogusly ignore malloc error */
	    strcpy(NewMessage->AuthName, FullName);
	}
	ahead = GenAuthField(NewMessage);
	if (!ahead) {
	    AMS_RETURN_ERRCODE(ENOMEM, EIN_MALLOC, EVIA_CHECKAUTH);
	}
	ReduceWhiteSpace(ahead); /* canonicalize the spacing */
	ReduceWhiteSpace(CopyOfAuthHeader); /* ditto */
	if (strcmp(StripWhiteEnds(ahead), StripWhiteEnds(CopyOfAuthHeader))) {
	    char *AuthHead;
	    long rval;
	    AuthHead = malloc(35+strlen(ahead));
	    if (!AuthHead) {
		free(ahead);
		AMS_RETURN_ERRCODE(ENOMEM, EIN_MALLOC, EVIA_CHECKAUTH);
	    }
	    strcpy(AuthHead, "X-Andrew-Authenticated-As: ");
	    strcat(AuthHead, ahead);
	    free(ahead);
	    rval = AddHeader(NewMessage, AuthHead);
	    free(AuthHead);
	    return(rval);
	}
	free(ahead);
    }
    return(0);
}

