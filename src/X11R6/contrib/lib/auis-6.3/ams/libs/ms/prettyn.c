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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/ams/libs/ms/RCS/prettyn.c,v 2.21 1993/05/05 19:49:43 susan Exp $";
#endif

#include <andyenv.h>
#include <andrewos.h>
#include <ms.h>
#include <hdrparse.h>
#include <util.h>
#include <svcconf.h>
#include <pwd.h>
#ifdef WHITEPAGES_ENV
#include <wp.h>
#endif /* WHITEPAGES_ENV */
#include <parseadd.h>

#define BIGFROM 24  /* c.f. bldcapt.c */
#define MINHOST 3

extern char MyMailDomain[];

extern PARSED_ADDRESS *SingleAddress();

#define strncpy2maxes(s1, s2, l1, l2) strncpy((s1), (s2), ((l1) > (l2)) ? (l2) : (l1))

FindPrettiestName(msg, FromBuf, lim)
struct MS_Message *msg;
char *FromBuf;
int lim;
{
    char MyFromBuf[1500];
    struct passwd *p;

    if (msg->ParsedStuff->HeadBody[HP_FROM]) {
	strncpy2maxes(MyFromBuf, msg->ParsedStuff->HeadBody[HP_FROM], msg->ParsedStuff->HeadBodyLen[HP_FROM], sizeof(MyFromBuf) - 1);
	MyFromBuf[msg->ParsedStuff->HeadBodyLen[HP_FROM]] = '\0';
    } else if (msg->ParsedStuff->HeadBody[HP_RESENTFROM]) {
	strncpy2maxes(MyFromBuf, msg->ParsedStuff->HeadBody[HP_RESENTFROM], msg->ParsedStuff->HeadBodyLen[HP_RESENTFROM], sizeof(MyFromBuf) - 1);
	MyFromBuf[msg->ParsedStuff->HeadBodyLen[HP_RESENTFROM]] = '\0';
    } else if (msg->ParsedStuff->HeadBody[HP_SENDER]) {
	strncpy2maxes(MyFromBuf, msg->ParsedStuff->HeadBody[HP_SENDER], msg->ParsedStuff->HeadBodyLen[HP_SENDER], sizeof(MyFromBuf) - 1);
	MyFromBuf[msg->ParsedStuff->HeadBodyLen[HP_SENDER]] = '\0';
    } else if (msg->ParsedStuff->HeadBody[HP_RETURNPATH]) {
	strncpy2maxes(MyFromBuf, msg->ParsedStuff->HeadBody[HP_RETURNPATH], msg->ParsedStuff->HeadBodyLen[HP_RETURNPATH], sizeof(MyFromBuf) - 1);
	MyFromBuf[msg->ParsedStuff->HeadBodyLen[HP_RETURNPATH]] = '\0';
    } else if (msg->ReplyTo) {
	strncpy(MyFromBuf, msg->ReplyTo, sizeof(MyFromBuf) - 1);
    } else if (msg->AuthUid > 0) {
	errno = cpw_error = 0;
#ifdef WHITEPAGES_ENV
	p = getcpwuid(msg->AuthUid, (msg->AuthCell ? msg->AuthCell : WorkstationName));
	errno = ConvertWpErrToMSErr(cpw_error, E2BIG, FALSE);
#else /* WHITEPAGES_ENV */
	p = getpwuid(msg->AuthUid);
#endif /* WHITEPAGES_ENV */
	if (p == NULL) {
	    if (tfail(errno)) {
		strcpy(FromBuf, "<unknown (temp failure)>");
#ifdef WHITEPAGES_ENV
	    } else if (cpw_error == wperr_NoKeysFound) {
		sprintf(FromBuf, "<unknown (no user %d in cell %s)>", msg->AuthUid, (msg->AuthCell ? msg->AuthCell : WorkstationName));
#endif /* WHITEPAGES_ENV */
	    } else {
		sprintf(FromBuf, "<unknown (error %d)>", cpw_error);
	    }
	    return(0);
	} else {
	    char *uname;
	    GetNameFromGecos(p->pw_gecos, p->pw_name, (msg->AuthCell ? msg->AuthCell : WorkstationName), &uname);
	    if (uname) {
		ShrinkName(uname, FromBuf, BIGFROM);
		free(uname);
	    } else {
		strcpy(FromBuf, "<unknown (out of memory)>");
	    }
	    return(0);
	}
    } else {
	strcpy(FromBuf, "<unknown sender>");
	return(0);
    }
    return(FindPrettiestFromString(MyFromBuf, FromBuf, lim));
}

FindPrettiestFromString(MyFromBuf, FromBuf, lim)
char *MyFromBuf, *FromBuf;
int lim;
{
    char *RealFrom, *s, *oparen, *cparen;
    int dummy = 0, len;
    PARSED_ADDRESS *Addr, *ListHead;

    debug(1, ("Finding prettiest in %s\n", MyFromBuf));
    if (ParseAddressList(MyFromBuf, &ListHead) != PA_OK) {
	/* illegal format, do what we can */
	RealFrom = MyFromBuf;
	oparen = strchr(RealFrom, '(');
	if (oparen) RealFrom = ++oparen;
	len = strlen(RealFrom);
	if (len > BIGFROM) {
	    RealFrom[BIGFROM] = '\0';
	}
	if (!strchr(RealFrom, '@')) {
	    RealFrom[BIGFROM-2] = '@';
	    RealFrom[BIGFROM-1] = '?';
	}
	strncpy(FromBuf, RealFrom, lim);
	debug(1, ("Illegal address, returning %s\n", RealFrom));
	return(0);
    }
    Addr = SingleAddress(ListHead, &dummy);
    if (!Addr || (dummy < 1)) { /* empty address! */
	*FromBuf = '\0';
	FreeAddressList(ListHead);
	return(0);
    }
    if (Addr->RoutePhrase) {
	char *s;

	s = strchr(Addr->RoutePhrase, '(');
	if (s && (s > (Addr->RoutePhrase + 8))) {
	    /* This routephrase has something that looks like a comment in it.
	     The only time we've seen this it was something like 
	    "John Doe (jdoe%foo@bar.baz)" <jdoe%foo@bar.baz>
	     So we're currently tuned to that case... */
	    *s = '\0';
	}
	RealFrom = Addr->RoutePhrase;
    } else {
	ADDRESS_COMMENT *This;

	RealFrom = NULL;
	for (This = Addr->Comments; This != NULL; This = This->Next) {
	    if (This->Text && This->Text[0]) {
		char *s;

		RealFrom = This->Text;
		if (*RealFrom = '(') {
		    ++RealFrom;
		    s = strrchr(RealFrom, ')');
		    if (s) *s = '\0';
		}
		break;
	    }
	}
	if (!RealFrom) {
	    RealFrom = Addr->LocalPart;
	}
    }
    debug(1, ("RealFrom is %s\n", RealFrom));
    s= RealFrom;
    while (TRUE) {
	oparen = strchr(s, '(');
	if (oparen) {
	    *oparen++ = '\0';
	    cparen = strrchr(oparen, ')');
	    if (cparen) {
		s = oparen;
		*cparen = '\0';
		continue;
	    }
	}
	break;
    }
    debug(1, ("Choosing between %s and %s\n", s, RealFrom));
    if (strlen(RealFrom) > strlen(s)) s = RealFrom;
    debug(1, ("OK, I'm working with %s\n", s));
    ShrinkName(s, FromBuf, BIGFROM-1-MINHOST);
    strcat(FromBuf, "@");
    if (Addr->Hosts->Prev && Addr->Hosts->Prev->Name) {
	len = BIGFROM - strlen(FromBuf);
	strncat(FromBuf, Addr->Hosts->Prev->Name, len);
	FromBuf[BIGFROM] = '\0';
    } else {
	for (len = MINHOST; len; --len) {
	    strcat(FromBuf, "?");
	}
    }
    FreeAddressList(ListHead);
    return(0);
}
