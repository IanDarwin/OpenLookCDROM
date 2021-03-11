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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/ams/libs/ms/RCS/bldcapt.c,v 2.26 1992/12/15 21:17:22 rr2b R6tape $";
#endif

#include <andrewos.h> /* sys/time.h */
#include <ms.h>
#include <hdrparse.h>
#include <svcconf.h>
#include <util.h>
#include <pwd.h>
#ifdef WHITEPAGES_ENV
#include <wp.h>
#endif /* WHITEPAGES_ENV */
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
#include <sys/stat.h>

#define BIGSUBJECT 30
#define BIGFROM 24  /* c.f. prettyn.c */
#define BIGCAPTPASTDATE 65

extern int postmanvuid, myvuid, homeUsesAMSDelivery;
extern char *months[], MyMailDomain[], *MyPrettyAddress;

BuildCaption(Msg, Template, IsMyMail)
struct MS_Message *Msg;
struct MS_CaptionTemplate *Template;
Boolean IsMyMail;
{
    struct passwd *p;
    struct MS_CaptionTemplate DefaultTemplate;
    char Buf[1200], FromBuf[1000], *s, CountStr[20], *DateHead, NewFromBuf[1000];
    long now;
    struct tm *tmbuf;
    int limit, bodylen, subjlim;
    struct stat stbuf;
    Boolean UseArrows = FALSE;
    Boolean InMyCell;

    /* Eventually, we should build this based on real templates, instead of
      the minor variations the template now specifies. */
    /* However, there should always be a default when Template is null */

    debug(1, ("BuildCaption\n"));


    if (!Template) {
	Template = &DefaultTemplate;
	bzero(Template, sizeof(struct MS_CaptionTemplate));
	DefaultTemplate.basictype = BASICTEMPLATE_NORMAL;
	DefaultTemplate.datetype = DATETYPE_CURRENT;
    }

    switch (Template->datetype) {
	case DATETYPE_FROMHEADER: 
	    DateHead = malloc(3+Msg->ParsedStuff->HeadBodyLen[HP_DATE]);
	    if (!DateHead) AMS_RETURN_ERRCODE(ENOMEM, EIN_MALLOC, EVIA_BUILDCAPTIONFIELD);
	    strncpy(DateHead, Msg->ParsedStuff->HeadBody[HP_DATE], Msg->ParsedStuff->HeadBodyLen[HP_DATE]);
	    DateHead[Msg->ParsedStuff->HeadBodyLen[HP_DATE]] = 0;
	    if (!ParseAndShrinkDate(DateHead, Buf)) {
		debug(16, ("Buf is %s\n", Buf));
		strcat(Buf, "\t");
		free(DateHead);
		break;
	    }
	    free(DateHead); /* And drop through */
	case DATETYPE_FROMFILE: 
	    now = Msg->RawFileDate;
	    tmbuf = localtime (&now);
	    sprintf(Buf, "%2d-%s-%02d\t", tmbuf->tm_mday, months[tmbuf->tm_mon], tmbuf->tm_year);
	    break;
	case DATETYPE_CURRENT:
	default:
	    now = time(0);
	    tmbuf = localtime (&now);
	    sprintf(Buf, "%2d-%s-%02d\t", tmbuf->tm_mday, months[tmbuf->tm_mon], tmbuf->tm_year);
	    break;
    }
    subjlim = (Template->basictype == BASICTEMPLATE_NOFROM) ? BIGCAPTPASTDATE : BIGSUBJECT;
    if (Msg->ParsedStuff->HeadBody[HP_SUBJECT]) {
	limit = Msg->ParsedStuff->HeadBodyLen[HP_SUBJECT];
	if (limit > subjlim) {
	    limit = subjlim - 3;
	    strncat(Buf, Msg->ParsedStuff->HeadBody[HP_SUBJECT], limit);
	    strcat(Buf, "..");
	} else {
	    strncat(Buf, Msg->ParsedStuff->HeadBody[HP_SUBJECT], limit);
	}
    }
    debug(16, ("Buf is %s\n", Buf));
    /* Strip out all embedded newlines & tabs; really should shrink all white space */
    while (s=strchr(Buf, '\n')) *s = ' '; 
    while (s=strchr(Buf+11, '\t')) *s = ' '; 

    if (Template->basictype != BASICTEMPLATE_NOFROM) {

	if (strlen(Buf) < BIGSUBJECT + 11) strcat(Buf, "\t");

	InMyCell = (Msg->AuthCell && ULstrcmp(MyMailDomain, Msg->AuthCell) == 0);
	if (AuthenticReSentHeader(Msg)) {
	    FindPrettiestName(Msg, FromBuf, sizeof(FromBuf));
	} else if (IsMyMail && ((homeUsesAMSDelivery<0 && !AMS_ViceIsRunning && SeemsToBeFromMe(Msg)) || ReallyIsFromMe(Msg, InMyCell))) {
	    int lim;

	    UseArrows = TRUE;
	    lim = Msg->ParsedStuff->HeadBodyLen[HP_TO];
	    if (lim > 0) {
		char *s, *t;

		if (lim >= (sizeof(FromBuf) - 4)) lim = sizeof(FromBuf) -4;
		s = Msg->ParsedStuff->HeadBody[HP_TO];
		t = strchr(s, '(');
		if (t && ((t - s) <= lim)) {
		    s = ++t;
		    t = strchr(s, ')');
		    if (t) {
			lim = t - s;
		    } else {
			lim -= s - Msg->ParsedStuff->HeadBody[HP_TO];
		    }
		}
		strncpy(FromBuf, s, lim);
		FromBuf[lim] = '\0';
		if (lim > BIGFROM - 3) lim = BIGFROM - 3;
		FindPrettiestFromString(FromBuf, NewFromBuf);
		strcpy(FromBuf, NewFromBuf);
	    } else {
		strcpy(FromBuf, "<No 'To:' header>");
	    }
	} else if (Msg->AuthUid <= 0	/* the unauthenticated case */
		   || (Msg->AuthCell == NULL)
#ifdef AFS_ENV
		   || (Msg->AuthUid == ANONYMOUSID) || (Msg->AuthUid == ANYUSERID)
#endif /* AFS_ENV */
		   || (Msg->AuthUid == postmanvuid && InMyCell)) {
	    FindPrettiestName(Msg, FromBuf, sizeof(FromBuf));
/* if couldn't read WP once, don't leave name as ``User 5333 in cell block.foo.bar'' forever. */
	} else if (Msg->AuthName && strncmp(Msg->AuthName, "User ", 5) != 0) {
	    strcpy(FromBuf, Msg->AuthName);
	    if (!InMyCell) {
		strcat(FromBuf, " (*)"); /* Flags remote authenticated user */
	    }
	} else {
	    p = getcpwuid(Msg->AuthUid, Msg->AuthCell);
	    if (p == NULL) {
		auto char TempBuf[400];

#ifdef WHITEPAGES_ENV
		errno = ConvertWpErrToMSErr(cpw_error, E2BIG, FALSE);
#endif /* WHITEPAGES_ENV */
		if (tfail(errno)) {
		    sprintf(FromBuf, "Uncertain user %d", Msg->AuthUid);
		} else {
		    sprintf(FromBuf, "Unknown user %d", Msg->AuthUid);
		}
		if (!InMyCell) {
		    strcat(FromBuf, "@");
		    strcat(FromBuf, Msg->AuthCell);
		}
		strcat(FromBuf, ": ");
		FindPrettiestName(Msg, TempBuf, sizeof(TempBuf));
		strcat(FromBuf, TempBuf);
	    } else {
		char *uname;
		GetNameFromGecos(p->pw_gecos, p->pw_name, MyMailDomain, &uname);
		if (uname) {
		    strcpy(FromBuf, uname);
		    free(uname);
		    if (!InMyCell) strcat(FromBuf, " (*)");
		} else {
		    sprintf(FromBuf, "Unknown user (%s/%s@%s: out of memory)", p->pw_gecos, p->pw_name, Msg->AuthCell);
		}
	    }
	}
	ShrinkName(FromBuf, NewFromBuf, UseArrows ? BIGFROM - 3 : BIGFROM);
	if (UseArrows) strcat(Buf, "=> ");
	strcat(Buf, NewFromBuf);
    }
    debug(16, ("Buf is %s\n", Buf));

    if (fstat(Msg->OpenFD, &stbuf)) {
	bodylen = Msg->FullSize;
	if (Msg->FullSize > Msg->HeadSize) bodylen -= Msg->HeadSize;
    } else {
	if (stbuf.st_size > 0) {
	    bodylen = stbuf.st_size - Msg->BodyOffsetInFD;
	} else {
	    bodylen = Msg->FullSize - Msg->BodyOffsetInFD;
	}
    }
    strcat(Buf, " (");
    if (Msg->ParsedStuff->HeadBody[HP_SCRIBEFORMAT] || Msg->ParsedStuff->HeadBody[HP_CONTENTTYPE]) {
	if (Msg->ParsedStuff->HeadBody[HP_MESSAGESIZE]) {
	    int len;

	    len = Msg->ParsedStuff->HeadBodyLen[HP_MESSAGESIZE];
	    if (len > (sizeof(CountStr) - 1)) len = sizeof(CountStr) - 1;
	    strncpy(CountStr, Msg->ParsedStuff->HeadBody[HP_MESSAGESIZE], len);
	    CountStr[len] = '\0';
	} else {
	    sprintf(CountStr, "%d*", bodylen); 
	}
    } else {
	sprintf(CountStr, "%d", bodylen); 
    }
    strcat(Buf, CountStr);
    strcat(Buf, ")");
    debug(16, ("Buf is %s\n", Buf));

    if (strlen(Buf) > AMS_CAPTIONSIZE) {
	strncpy(AMS_CAPTION(Msg->Snapshot), Buf, AMS_CAPTIONSIZE -1);
	AMS_CAPTION(Msg->Snapshot)[AMS_CAPTIONSIZE-1] = 0;
    } else {
	strcpy(AMS_CAPTION(Msg->Snapshot), Buf);
    }
    debug(16, ("Caption field is %s\n", AMS_CAPTION(Msg->Snapshot)));
    return(0);
}

SeemsToBeFromMe(Msg)
struct MS_Message *Msg;
{
    if (Msg->ParsedStuff->HeadBody[HP_FROM]
	 && !strncmp(Msg->ParsedStuff->HeadBody[HP_FROM], MyPrettyAddress, Msg->ParsedStuff->HeadBodyLen[HP_FROM])) {
	return(1);
    }
    if (!Msg->ParsedStuff->HeadBody[HP_FROM]
	 && !Msg->ParsedStuff->HeadBody[HP_SENDER]
	 && !Msg->ParsedStuff->HeadBody[HP_RESENTFROM]
	 && !Msg->ParsedStuff->HeadBody[HP_RETURNPATH]
	 && !Msg->ReplyTo
	 && (Msg->AuthUid <= 0)) {
	return(1);
    }
    return(0);
}
	
ReallyIsFromMe(Msg, InMyCell)
struct MS_Message *Msg;
{
    if ((!(Msg->AuthUid <= 0	/* the BCC case */
	    || (Msg->AuthCell == NULL)
#ifdef AFS_ENV
	    || (Msg->AuthUid == ANONYMOUSID) || (Msg->AuthUid == ANYUSERID)
#endif /* AFS_ENV */
	    || (Msg->AuthUid == postmanvuid))) && Msg->AuthUid == myvuid && InMyCell) {
	return(1);
    }
    return(0);
}
