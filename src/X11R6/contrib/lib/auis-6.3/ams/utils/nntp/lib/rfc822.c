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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/ams/utils/nntp/lib/RCS/rfc822.c,v 1.20 1993/07/09 19:26:06 gk5g Exp $";
#endif

/*
** Get header info from mail-format file.
** Return non-zero on success.
*/

#include "defs.h"
#include <ctype.h>
#include <andrewos.h> /* sys/types.h */
struct timeb
{
	time_t	time;
	unsigned short millitm;
	short	timezone;
	short	dstflag;
};
#include <stdio.h>
#include <system.h>
#include <errprntf.h>
#ifdef HAS_SYSEXITS
#include <sysexits.h>
#endif /* HAS_SYSEXITS */
#include <header.h>
#include <llist.h>

#ifndef isblank
#define isblank(c)	((c) == ' ' || (c) == '\t')
#endif /* isblank */

char	*hfgets();
char	*arpadate();
char	*errmsg();
char	*strpbrk();
time_t	cgtdate();
extern  char *progname;
extern	time_t	time();
extern	char retmsg[];

#define FROM 		1
#define NEWSGROUP 	2
#define TITLE 		3
#define SUBMIT		4
#define RECEIVE		5
#define EXPIRE		6
#define ARTICLEID	7
#define MESSAGEID	8
#define REPLYTO		9
#define FOLLOWID	10
#define CONTROL		11
#define SENDER		12
#define FOLLOWTO	13
#define PATH		14
#define POSTVERSION	15
#define RELAYVERSION	16
#define DISTRIBUTION	17
#define ORGANIZATION	18
#define NUMLINES	19
#define KEYWORDS	20
#define APPROVED	21
#define NFID		22
#define NFFROM		23
#define XREF		24
#define SUMMARY		25
#define FULLNAME	26
#define OTHER		99

/*
** This is the list of headers we recognize.
** All others get stripped before they get to inews.
*/
struct htype	{
	char	*hname;
	int	hid;
} htype[] = {
	{"Approved:",		APPROVED},
	{"Article-I.D.:",	ARTICLEID},
	{"Control:",		CONTROL},
	{"Date-Received:",	RECEIVE},
	{"Date:",		SUBMIT},
	{"Distribution:",	DISTRIBUTION},
	{"Expires:",		EXPIRE},
	{"Followup-To:",	FOLLOWTO},
	{"From:",		FROM},
/*	{"Full-Name:",		FULLNAME},	*/
	{"In-Reply-To:",	FOLLOWID},
	{"Keywords:",		KEYWORDS},
	{"Lines:",		NUMLINES},
	{"Message-ID:",		MESSAGEID},
	{"Newsgroups:",		NEWSGROUP},
	{"Nf-From:",		NFFROM},
	{"Nf-ID:",		NFID},
	{"Organization:",	ORGANIZATION},
	{"Path:",		PATH},
	{"Posted:",		SUBMIT},
	{"Posting-Version:",	POSTVERSION},
/*	{"Received:",		RECEIVE},	a bad name w.r.t. RFC822 */
	{"References:",		FOLLOWID},
	{"Relay-Version:",	RELAYVERSION},
	{"Reply-To:",		REPLYTO},
	{"Sender:",		SENDER},
	{"Subject:",		TITLE},
	{"Summary:",		SUMMARY},
	{"Title:",		TITLE},
	{"Xref:",		XREF},
	{(char *)NULL,		NULL}
};

#if !POSIX_ENV
char *malloc();
#endif

rfc822read(hp, fp, bfr)
register struct hbuf *hp;
register FILE *fp;
char	*bfr;
{
	register int	i = type(bfr);
	long	curpos;
	int	status = EX_OK;


	if (fp == NULL) 
	    errprintf(progname,ERR_CRITICAL,0,0,"Null file in rfc822read");
	do {
		if (status != EX_OK) return(status);
		curpos = ftell(fp);
		switch (i) {
		case PATH:
			status = getfield(bfr, hp->path, sizeof(hp->path));
			break;
		case FROM:
			status = getfield(bfr, hp->from, sizeof(hp->from));
			break;
		case NEWSGROUP:
			status = getfield(bfr, hp->nbuf, sizeof(hp->nbuf));
			break;
		case TITLE:
			status = getfield(bfr, hp->title, sizeof(hp->title));
			break;
		case SUBMIT:
			status = getfield(bfr, hp->subdate, sizeof(hp->subdate));
			break;
		case EXPIRE:
			status = getfield(bfr, hp->expdate, sizeof(hp->expdate));
			break;
		case MESSAGEID:
			status = getfield(bfr, hp->ident, sizeof(hp->ident));
			break;
		case REPLYTO:
			status = getfield(bfr, hp->replyto, sizeof(hp->replyto));
			break;
		case FOLLOWID:
			status = getfield(bfr, hp->followid, sizeof(hp->followid));
			break;
		case SENDER:
			status = getfield(bfr, hp->sender, sizeof(hp->sender));
			break;
		case FOLLOWTO:
			status = getfield(bfr, hp->followto, sizeof(hp->followto));
			break;
		case CONTROL:
			status = getfield(bfr, hp->ctlmsg, sizeof(hp->ctlmsg));
			break;
		case DISTRIBUTION:
			status = getfield(bfr, hp->distribution, sizeof(hp->distribution));
			break;
		case ORGANIZATION:
			status = getfield(bfr, hp->organization, sizeof(hp->organization));
			break;
		case KEYWORDS:
			status = getfield(bfr, hp->keywords, sizeof(hp->keywords));
			break;
		case APPROVED:
			status = getfield(bfr, hp->approved, sizeof(hp->approved));
			break;
		case SUMMARY:
			status = getfield(bfr, hp->summary, sizeof(hp->summary));
			break;
		}

	} while ((i = type(hfgets(bfr, LBUFLEN, fp))) > 0);

	if (*bfr != '\n')
		fseek(fp, curpos, 0);
	return(status);
}

#define its(type) (prefix(ptr, type))

type(ptr)
register char	*ptr;
{
	register struct htype	*hp;
	register char	*colon, *space;
	static int	lasthdr = FALSE;	/* for continuation headers */

	/*
	** some consistency checks (i.e. is this really a header line?)
	*/
	if ((ptr == NULL) || !isascii(*ptr) || (*ptr == '\n'))
		return(lasthdr = FALSE);

	if (isblank(*ptr))		/* continuation line? */
		return(lasthdr);

	colon = index(ptr, ':');
	space = index(ptr, ' ');
	if (!colon || space && space < colon)
		return(lasthdr = FALSE);

	for(hp = htype; hp->hname; hp++) {
		if (its(hp->hname))
			return(lasthdr = hp->hid);
	}
	return(lasthdr = OTHER);
}

/*
** Get the contents of the field of the header line, appending it,
** with a space delimeter if it's a continuation line.
** If there is already something in the header storage, skip this
** header line and the continuations.
*/
getfield(src, dest, size)
char	*src, *dest;
int	size;			/* of dest (total bytes) */
{
	static int	skip = FALSE;	/* skip the continuation lines */
#ifdef NOTDEF
	int status;
#endif /* NOTDEF */

	if (src == (char *)NULL || dest == (char *)NULL) {
		sprintf(retmsg,"Error in parsing article header: %s",src);

		return(EX_DATAERR);
	}

	
#ifdef NOTDEF
	if ((status = checkspace(src)) != EX_OK)
	    return(status);
#endif /* NOTDEF */
	    

	if (isblank(*src)) {				/* continuation line? */
		if (skip)
			return(EX_OK);
		if ((size -= strlen(dest)) <= 0){	/* any space left? */
			sprintf(retmsg,"Error in parsing article header: %s",src);
			return(EX_DATAERR);
		}
		while(*src && isblank(*src))		/* eat whitespace */
			src++;
		*--src = ' ';				/* backup & add one */
		(void) strncat(dest, src, size - 1);	/* append to hdr */
	} else {
		skip = FALSE;
		if (*dest)				/* already got one? */
			return(skip = EX_OK);		/* skip continuation */
		if ((src = index(src, ':')) == (char *)NULL){	/* impossible */
			sprintf(retmsg,"Error in parsing article header: %s",src);
			return(EX_DATAERR);
		}
		src++;					/* skip colon */
		while(*src && isblank(*src))		/* eat whitespace */
			src++;
		(void) strncpy(dest, src, size - 1);
	}
	(void) nstrip(dest);
	return(EX_OK);
}

/*
** Write out an RFC822 header, paying no attention to line limits.
** Ideally, we should do continuations in here...
*/
rfc822write(hp, fp)
register struct hbuf *hp;
register FILE *fp;
{
	time_t t;

	if (hp->path[0])
		fprintf(fp, "Path: %s\n", hp->path);
	if (hp->from[0])
		fprintf(fp, "From: %s\n", hp->from);
	if (hp->nbuf[0])
		fprintf(fp, "Newsgroups: %s\n", hp->nbuf);
	if (hp->title[0])
		fprintf(fp, "Subject: %s\n", hp->title);
	if (hp->ident[0])
		fprintf(fp, "Message-ID: %s\n", hp->ident);
	if (hp->subdate[0])
		t = cgtdate(hp->subdate);
	else
		time(&t);
	fprintf(fp, "Date: %s\n", arpadate(&t));
	if (*hp->expdate)
		fprintf(fp, "Expires: %s\n", hp->expdate);
	if (*hp->followid)
		fprintf(fp, "References: %s\n", hp->followid);
	if (*hp->ctlmsg)
		fprintf(fp, "Control: %s\n", hp->ctlmsg);
	if (*hp->sender)
		fprintf(fp, "Sender: %s\n", hp->sender);
	if (*hp->replyto)
		fprintf(fp, "Reply-To: %s\n", hp->replyto);
	if (*hp->followto)
		fprintf(fp, "Followup-To: %s\n", hp->followto);
	if (*hp->distribution)
		fprintf(fp, "Distribution: %s\n", hp->distribution);
	if (*hp->organization)
		fprintf(fp, "organize: %s\n", hp->organization);
	if (*hp->keywords)
		fprintf(fp, "Keywords: %s\n", hp->keywords);
	if (*hp->summary)
		fprintf(fp, "Summary: %s\n", hp->summary);
	if (*hp->approved)
		fprintf(fp, "Approved: %s\n", hp->approved);
	putc('\n', fp);
	return(!ferror(fp));
}

/*
** strip leading and trailing spaces
*/
char *
sp_strip(s)
register char	*s;
{
	register char	*cp;

	if (s == NULL)
		return(NULL);

	if (*s == '\0')
		return(s);
	
	cp = &s[strlen(s) - 1];
	while(cp > s && isspace(*cp))
		cp--;

	*++cp = '\0';	/* zap trailing spaces */

	for(cp = s; *cp && isspace(*cp); cp++)
		continue;

	return(cp);	/* return pointer to first non-space */
}

/*
** crack an RFC822 from header field into address and fullname.
*/
crackfrom(addr,name,field)
char	*addr, *name, *field;
{
	char	commbuf[LBUFLEN];
	char	addrbuf[LBUFLEN];
	register char	*p;
	register char	*ap = addrbuf;
	register char	*np = NULL;
	short	commfound = 0, comment = 0;
	short	addrfound = 0, address = 0;
	struct llist	comm, *cp = &comm;

	*name = '\0';	/* just make sure */
	*addr = '\0';

	if ((p = field) == NULL)
		return(FALSE);

	while(*p && isspace(*p))	/* eat leading white space */
		p++;

	while(*p) {
		switch(*p) {
		case '(':
			if (comment == 0) {
				np = commbuf;
				*np = '\0';
			}
			comment++;
			break;
		case ')':
			if (comment > 0 && --comment == 0) {
				*np++ = *p++;	/* note incr; skip `)' */
				*np++ = '\0';
				if ((cp = l_alloc(cp, commbuf, strlen(commbuf) + 1)) == NULL) {
					if (commfound)
						l_free(comm);
					return(FALSE);
				}
				commfound++;
				np = NULL;
				continue;
			}
			break;
		case '<':
			if (address) {
				if (commfound)
					l_free(comm);
				return(FALSE);	/* AWK! Abort! */
			}
			if (!comment) {
				address++;
				*ap = '\0';
				ap = addr;
			}
			break;
		case '>':
			if (!comment && address) {
				address--;
				addrfound++;
				*ap = '\0';
				ap = &addrbuf[strlen(addrbuf)];
				p++;	/* skip the `>' */
			}
			break;
		}

		if (comment) {
			*np++ = *p;
		} else if (address) {
			if (*p != '<')
				*ap++ = *p;
		} else {
			*ap++ = *p;
		}
		if (*p)
			p++;
		else
			break;
	}

	*ap++ = '\0';

	if (addrfound) {
		(void) strcpy(name, sp_strip(addrbuf));
		(void) strcpy(addr, strcpy(commbuf, sp_strip(addr)));
	} else {
		(void) strcpy(addr, sp_strip(addrbuf));
		*name = '\0';
	}
	/*
	** Just to be sure that we got the full name,
	** we'll take all of the comments!
	*/
	if (commfound) {
		register int	flag = (*name != '\0' ? TRUE : FALSE);

		for(cp = &comm; cp->l_item; cp = cp->l_next) {
			if (flag)
				(void)strcat(name, ", ");
			flag = TRUE;
			if (cp->l_item[cp->l_len - 2] == ')')
				cp->l_item[cp->l_len - 2] = '\0';
			(void)strcat(name, sp_strip(&cp->l_item[1]));
		}
		l_free(comm);
	}
	return(TRUE);
}

/*
** Check on the validity of an RFC822 message-id.
** Check for enclosing `<>', an `@', and a `.' after
** the `@'. Also make sure that everything is ASCII,
** and non-control characters.
*/
msgid_ok(id)
register char	*id;
{
	register atdot = FALSE;

	if (id == NULL)
		return(FALSE);		/* don't waste my time! */

	if (*id != '<')
		return(FALSE);

	/* skip the first `<', cause we check for more */
	for(++id; *id != '\0'; ++id) {
		switch(*id) {
		case '<':
			return(FALSE);	/* we've already got one */
		case '>':
			return(atdot);
		case '.':
		case '@':		/* should be a domain spec */
			atdot = TRUE;
			break;
		default:
			if (!isascii(*id) || iscntrl(*id) || isspace(*id))
				return(FALSE);	/* quit immediately */
			break;
		}
	}
	return(FALSE);
}

/*
** hfgets is like fgets, but deals with continuation lines.
** It also ensures that even if a line that is too long is
** received, the remainder of the line is thrown away
** instead of treated like a second line.
*/
char *
hfgets(buf, len, fp)
char *buf;
int len;
FILE *fp;
{
	register int c = 0;
	register int n = 0;
	register char *cp;

	cp = buf;
	while (n < len && (c = getc(fp)) != EOF) {
		if (c == '\n')
			break;
		if (!iscntrl(c) || c == '\b' || c == '\t') {
			*cp++ = c;
			n++;
		}
	}
	if (c == EOF && cp == buf)
		return NULL;
	*cp = '\0';

	if (c != '\n') {
		/* Line too long - part read didn't fit into a newline */
		while ((c = getc(fp)) != '\n' && c != EOF)
			;
	} else if (cp == buf) {
		/* Don't look for continuation of blank lines */
		*cp++ = '\n';
		*cp = '\0';
		return buf;
	}

	while ((c = getc(fp)) == ' ' || c == '\t') {	/* for each cont line */
		/* Continuation line. */
		if ((n += 2) < len) {
			*cp++ = '\n';
			*cp++ = c;
		}
		while ((c = getc(fp)) != '\n' && c != EOF)
			if ((!iscntrl(c) || c == '\b' || c == '\t') && n++ < len)
				*cp++ = c;
	}
	if (n >= len - 1)
		cp = buf + len - 2;
	*cp++ = '\n';
	*cp = '\0';
	if (c != EOF)
		(void) ungetc(c, fp); /* push back first char of next header */
	return buf;
}

/*
 * arpadate is like ctime(3) except that the time is returned in
 * an acceptable ARPANET time format instead of ctime format.
 */
char *
arpadate(longtime)
time_t *longtime;
{
	register char *p, *q, *ud;
	register int i;
	static char b[40];

	/*  Get current time. This will be used resolve the timezone. */
	ud = asctime(gmtime(longtime));

	/*  Crack the UNIX date line in a singularly unoriginal way. */
	q = b;

#ifdef notdef
/* until every site installs the fix to getdate.y, the day
   of the week can cause time warps */
	p = &ud[0];		/* Mon */
	*q++ = *p++;
	*q++ = *p++;
	*q++ = *p++;
	*q++ = ','; *q++ = ' ';
#endif /* notdef */

	p = &ud[8];		/* 16 */
	if (*p == ' ')
		p++;
	else
		*q++ = *p++;
	*q++ = *p++; *q++ = ' ';

	p = &ud[4];		/* Sep */
	*q++ = *p++; *q++ = *p++; *q++ = *p++; *q++ = ' ';

	p = &ud[22];		/* 1979 */
	*q++ = *p++; *q++ = *p++; *q++ = ' ';

	p = &ud[11];		/* 01:03:52 */
	for (i = 8; i > 0; i--)
		*q++ = *p++;

	*q++ = ' ';
	*q++ = 'G';		/* GMT */
	*q++ = 'M';
	*q++ = 'T';
	*q = '\0';

	return b;
}

time_t
cgtdate(datestr)
char *datestr;
{
	char	junk[40], month[40], day[30], tod[60], year[50], buf[BUFLEN];
	static time_t	lasttime;
	static char	lastdatestr[BUFLEN] = "";
	extern time_t	agetdate();

	if (lastdatestr[0] && strcmp(datestr, lastdatestr) == 0)
		return(lasttime);
	lasttime = agetdate(datestr, (struct timeb *)NULL);
	if (lasttime < 0 &&
	  sscanf(datestr, "%s %s %s %s %s", junk, month, day, tod, year) == 5) {
		(void) sprintf(buf, "%s %s, %s %s", month, day, year, tod);
		lasttime = agetdate(buf, (struct timeb *)NULL);
	}
	strncpy(lastdatestr, datestr, BUFLEN);
	return(lasttime);
}

char *
errmsg(code)
int code;
{
	extern int sys_nerr;
	extern char *sys_errlist[];
	static char ebuf[6+5+1];

	if (code > sys_nerr) {
		(void) sprintf(ebuf, "Error %d", code);
		return ebuf;
	} else
		return sys_errlist[code];
}

/*
 * Strip trailing newlines, blanks, and tabs from 's'.
 * Return TRUE if newline was found, else FALSE.
 */
nstrip(s)
register char *s;
{
	register char *p;
	register int rc;

	rc = FALSE;
	p = s;
	while (*p)
		if (*p++ == '\n')
			rc = TRUE;
	while (--p >= s && (*p == '\n' || *p == ' ' || *p == '\t'));
	*++p = '\0';
	return rc;
}

prefix(full, pref)
register char *full, *pref;
{
	register char fc, pc;

	while ((pc = *pref++) != '\0') {
		fc = *full++;
		if (isupper(fc))
			fc = tolower(fc);
		if (isupper(pc))
			pc = tolower(pc);
		if (fc != pc)
			return FALSE;
	}
	return TRUE;
}

#if !defined(USG) && !defined(NeXT)
char *
strpbrk(str, chars)
register char *str, *chars;
{
	register char *cp;

	do {
		cp = chars - 1;
		while (*++cp) {
			if (*str == *cp)
				return str;
		}
	} while (*str++);
	return NULL;
}
#endif /* USG */

#ifdef NOTDEF
/* Checks to make sure there is  a space after : in header.
*/

checkspace(header)
char *header;
{
	char *cp;
        int i;

	cp = header;

        for (i=0;*cp != '\0' &&  *cp != ':';i++)
	    cp++;
        cp++;
	if (*cp != ' ') {  
#ifdef DEBUG
	    fprintf(stderr,"No space after :! %s",header);
#endif /* DEBUG */
	    sprintf(retmsg,"No space after : in %s",header);
	    return(EX_DATAERR);

	}
        else return(EX_OK);




}
#endif /* NOTDEF */
