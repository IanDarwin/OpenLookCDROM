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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/ams/utils/nntp/lib/RCS/remote.c,v 1.16 1993/10/12 22:11:18 gk5g Exp $";
#endif

#include <stdio.h>
#include <system.h>
#include <andrewos.h>
#include <ctype.h>
#include <setjmp.h>
#include <signal.h>
#ifdef HAS_SYSEXITS
#include <sysexits.h>
#endif /* HAS_SYSEXITS */
#include <tcpconn.h>
#include <respcode.h>
#include <nntpxmit.h>

#define	TRUE	1
#define	FALSE	0

static	jmp_buf	SFGstack;
FILE	*rmt_rd;
FILE	*rmt_wr;
char	*sfgets();
char	*rfgets();

extern char	*progname;
extern	int	errno;
extern	char	*pname;
extern	char	*errmsg();
extern  char	retmsg[];
extern int test;
/*
** send cmd to remote, terminated with a CRLF.
*/
sendcmd(cmd)
char	*cmd;
{
#ifdef DEBUG
	fprintf(stderr, "<<< %s\n", cmd);
#endif /* DEBUG */
	(void) fprintf(rmt_wr, "%s\r\n", cmd);
	(void) fflush(rmt_wr);
	return(ferror(rmt_wr));
}

/*
** read a reply line from the remote server and return the code number
** as an integer, and the message in a buffer supplied by the caller.
** Returns FAIL if something went wrong.
*/
readreply(buf, size)
register char	*buf;
int	size;
{
	register char	*cp;
	register int	len;

/* make sure its invalid, unless we say otherwise*/
	buf[0] = '\0';

	/*
	** read one line from the remote
	*/
	if (sfgets(buf, size, rmt_rd) == NULL)
		return(FAIL);	/* error reading from remote */

	/*
	** Make sure that what the remote sent us had a CRLF at the end
	** of the line, and then null it out.
	*/
	if ((len = strlen(buf)) > 2 && *(cp = &buf[len - 2]) == '\r' &&
		*(cp + 1) == '\n')
	{
		*cp = '\0';
	} else
		return(FAIL);	/* error reading from remote */

#ifdef DEBUG
	printf(stderr, ">>> %s\n", buf);
#endif /* DEBUG */
	/*
	** Skip any non-digits leading the response code 
	** and then convert the code from ascii to integer for
	** return from this routine.
	*/
	cp = buf;
	while(*cp != '\0' && isascii(*cp) && !isdigit(*cp))
		cp++;	/* skip anything leading */

	if (*cp == '\0' || !isascii(*cp))
		return(FAIL);	/* error reading from remote */

	return(atoi(cp));
}
	
/*
** send a command to the remote, and wait for a response
** returns the response code, and the message in the buffer
*/
converse(buf, size)
char	*buf;
int	size;
{
	register int	resp;
	int code;

	if (code = sendcmd(buf)) 
		return(code);	/* Ooops! Something went wrong in xmit */
	/*
	** Skip the silly 100 series messages, since they're not the
	** final response we can expect
	*/
	while((resp = readreply(buf, size)) >= 100 && resp < 200)
		continue;
	return(resp);
}
	
/*
** Contact the remote server and set up the two global FILE pointers
** to that socket.
*/
hello(host)
char	*host;
{
	int	socket0, socket1;	/* to me (bad pun) */
	static char	*service = "nntp";
	int	code;
	char	buf[BUFSIZ];

	switch(socket0 = get_tcp_conn(host, service)) {
	case NOHOST:
		sprintf(retmsg,"No such host <%s>", host);
		return(EX_NOHOST);
	case NOSERVICE: 
		sprintf(retmsg,"No such service <%s>", service);
		return(EX_UNAVAILABLE);
	case FAIL:
		sprintf(retmsg,"%s: %s", host, errmsg(errno));
		return(EX_TEMPFAIL);
	}

	if ((socket1 = dup(socket0)) < 0) {
		close(socket0);
		sprintf(retmsg,"dup(2): %s", errmsg(errno));
		return(EX_TEMPFAIL);
	}

	if ((rmt_rd = fdopen(socket0, "r")) == (FILE *)NULL) {
		close(socket0);
		close(socket1);
		sprintf(retmsg,"fdopen(3): %s", errmsg(errno));
		return(EX_TEMPFAIL);
	}

	if ((rmt_wr = fdopen(socket1, "w")) == (FILE *)NULL) {
		fclose(rmt_rd);
		rmt_rd = (FILE *)NULL;
		close(socket1);
		sprintf(retmsg," fdopen(3): %s", errmsg(errno));
		return(EX_TEMPFAIL);
	}

	switch(code = readreply(buf, sizeof(buf))) {
	case OK_CANPOST:
	case OK_NOPOST:
		if (ferror(rmt_rd)) {
			goodbye(DONT_WAIT);
		        sprintf(retmsg,"Posting not allowed");
			return(EX_TEMPFAIL);
		}
		break;
	default:
		if (buf[0] != '\0') {
		    sprintf(retmsg, "%s:%d",buf,code);
		}
		goodbye(DONT_WAIT);
		return(EX_TEMPFAIL);
	}
	return(EX_OK);
}
	
/* Say goodbye to the nice remote server.*/
	
goodbye(wait)
int	wait;
{
	if (sendcmd("QUIT"))
		wait = FALSE;	/* override, something's wrong. */
/*	I dont care what they say to me; this is just being polite.*/
	if (wait) {
		char	buf[BUFSIZ];

		(void) readreply(buf, sizeof(buf));
	}
	(void) fclose(rmt_rd);
	rmt_rd = (FILE *)NULL;
	(void) fclose(rmt_wr);
	rmt_wr = (FILE *)NULL;
}
	
static
to_sfgets()
{
	longjmp(SFGstack, 1);
}

/*
** "Safe" fgets, ala sendmail. This fgets will timeout after some
** period of time, on the assumption that if the remote did not
** return, they're gone.
** WARNING: contains a possibly unportable reference to stdio
** error macros.
*/
char *
sfgets(buf, size, fp)
char	*buf;
int	size;
FILE	*fp;
{
	register char	*ret;

	if (setjmp(SFGstack)) {
		alarm(0);			/* reset alarm clock */
		signal(SIGALRM, SIG_DFL);	/* reset SIGALRM */
#ifdef _STDIO_USES_IOSTREAM
		fp->_flags |= STDIO_S_ERR_SEEN;
#else
		fp->_flag |= _IOERR;		/* set stdio error */
#endif
		return(NULL);			/* bad read, remote time out */
	}
	signal(SIGALRM, to_sfgets);
	alarm(TIMEOUT);
	ret = fgets(buf, size, fp);
	alarm(0);			/* reset alarm clock */
	signal(SIGALRM, SIG_DFL);	/* reset SIGALRM */
	return(ret);
}
		
/*
** Remote fgets - converts CRLF to \n, and returns NULL on . EOF from
** the remote. Otherwise it returns its first argument, like fgets(3).
*/
char *
rfgets(buf, size, fp)
char	*buf;
int	size;
FILE	*fp;
{
	register char	*cp = buf;
	register int	len;

	*cp = '\0';
	if (sfgets(buf, size, fp) == NULL)
		return(NULL);

	/* <CRLF> => '\n' */
	if ((len = strlen(buf)) > 2 && *(cp = &buf[len - 2]) == '\r') {
		*cp++ = '\n';
		*cp = '\0';
	}

	/* ".\n" => EOF */
	cp = buf;
	if (*cp++ == '.' && *cp == '\n') {
		return(NULL);	/* EOF */
	}

	/* Dot escaping */
	if (buf[0] == '.') strcpy(&buf[0], &buf[1]);
	return(buf);
}

/*
** send the contents of an open file descriptor to the remote,
** with appropriate RFC822 filtering (e.g. CRLF line termination,
** and dot escaping). Return non-zero if something went wrong.
*/
sendfile(fp)
	register FILE *fp;
{
	register int	c;
	register int	nl = TRUE;	/* assume we start on a new line */

	if (test) {
		fprintf(stderr,"Sendfile: (not) sending file\n");
		return(EX_UNAVAILABLE);
	}


	if (fp == (FILE *)NULL) {
	        sprintf(retmsg,"Article file not open for reading");
		return(EX_NOINPUT);
	}

	/* 1st send headers with appropriate stripping */
	sendheaders(fp);

	while((c = fgetc(fp)) != EOF) {
		switch(c) {
		case '\n':
			(void) fputc('\r', rmt_wr);	/* \n -> \r\n */
			(void) fputc(c, rmt_wr);
			nl = TRUE;		/* for dot escaping */
			break;
		case '.':
			if (nl) {
				(void) fputc(c, rmt_wr);	/* add a dot */
				nl = FALSE;
			}
			(void) fputc(c, rmt_wr);
			break;
		default:
			(void) fputc(c, rmt_wr);
			nl = FALSE;
			break;
		}
	}
	if (!nl) {
		(void) fputs("\r\n", rmt_wr);
	}
	(void) fputs(".\r\n", rmt_wr);	/* <CRLF>.<CRLF> termination */
	(void) fflush(rmt_wr);
	return(EX_OK);
}

static char *headers[] = {

	"approved",	"article-i.d.",	"content-description",
	"content-id",	"content-transfer-encoding",	"content-type",	
	"control",	"date-received",
	"date",		"distribution",	"expires",	"followup-to",
	"from",		"in-reply-to",	"keywords",	"lines",
	"message-id",	"mime-version", "newsgroups",
	"nf-from",	"nf-id", 
	"organization",	"path",		"posted",	"posting-version",
	"references",	"relay-version","reply-to",
	"sender",	"subject",	"summary",	"supersedes",
	"title",	"xref"
};

#define NHEADERS    (sizeof(headers)/sizeof(headers[0]))

#define HEADER_LEN  100

static void lc(h)
    register char *h;
{
    for (; *h!='\0'; h++)
	if (*h >= 'A' && *h <= 'Z')
	    *h += ('a'-'A');
}

static int goodheader(h)
    register char *h;
{
    char buffer[HEADER_LEN];
    register int i;

    /* Lower case it & compare */
    strcpy(buffer, h);
    lc(buffer);
    for (i=0; i<NHEADERS; i++)
	if (strcmp(buffer, headers[i]) == 0)
	    return TRUE;

    return FALSE;
}

/* Scan this header & echo it if put is TRUE. */

static void readheader(fp, put)
    register FILE *fp;
    register int put;
{
    register int c;

    /* Header name & : have been read.  Skip leading spaces */
    for (c=fgetc(fp); c==' ' || c=='\t'; c=fgetc(fp)) ;

    /* 1st non-blank character */
    for (;;)
	switch (c) {
	    case EOF:	return;
	    case '\n':	if (put) fputs("\r\n", rmt_wr);
			c = fgetc(fp);
			if (c != ' ' && c != '\t') {
			    ungetc(c, fp);
			    return;
			}
	    default:	if (put) fputc(c, rmt_wr);
			c = fgetc(fp);
			break;
	}
}

static void putheader(fp)
    FILE *fp;
{
    readheader(fp, TRUE);
}

static void skipheader(fp)
    FILE *fp;
{
    readheader(fp, FALSE);
}

/* Send only recognized headers */

static sendheaders(fp)
    register FILE *fp;
{

    for (;;) {
	register int i, c, nl, done;

	/* Read next line into buffer */
	nl = TRUE;
	done = FALSE;
	for (i=0; !done; i++) {
	    char buffer[HEADER_LEN];	/* Long enough for any recognized hdr */

	    c = fgetc(fp);
	    switch (c) {
		case EOF:
		    return;
		case ':':
		    buffer[i] ='\0';
		    if (goodheader(buffer)){
			fputs(buffer, rmt_wr);
			fputs(": ", rmt_wr);
			putheader(fp);
		    } else
			skipheader(fp);
		    nl = TRUE;
		    done = TRUE;
		    break;
		case '\n':
		    if (nl) {
			/* No more headers */
			fputs("\r\n", rmt_wr);
			return;
		    }
		    /* Seems to be no header here */
		    skipheader(fp);
		    nl = TRUE;
		    done = TRUE;
		    break;
		default:
		    if (i >= sizeof buffer) {
			skipheader(fp);
			nl = TRUE;
			done = TRUE;
			break;
		    } else {
			nl = FALSE;
			buffer[i] = c;
		    }
	    }
	}
    }
}
