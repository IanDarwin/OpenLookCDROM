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

static char *nntpxmit_rcsid = "$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/ams/utils/nntp/cmd/RCS/nntpxmit.c,v 1.26 1993/09/29 20:54:29 gk5g Exp $";

/*
** nntpxmit - transmit netnews articles across the internet with nntp
**
** Modified by John Myers to POST instead of IHAVE
**
** This program is for transmitting netnews between sites that offer the
** NNTP service, internet style.  Ideally, there are two forms of
** transmission that can be used in this environment, since the
** communication is interactive (and relatively immediate, when compared
** with UUCP).  They are:  passive poll (what have you gotten lately?) and
** active send (I have `x', do you want it?).  The USENET as a whole
** uniformly uses active send, and where the communication is batched
** (e.g. UUCP, or electronic mail) the software sends without even asking,
** unless it can determine that the article has already been to some site.
**
** It turns out that when you implement passive poll, you have to be
** *very* careful about what you (the server) tell the client, because
** something that you might wish to restrict distribution of (either
** internal newsgroups, or material posted in the international groups in
** local distributions) will otherwise leak out.  It is the case that if
** the server doesn't tell the client that article `x' is there, the
** client won't ask for it.  If the server tells about an article which
** would otherwise stay within some restricted distribution, the onus is
** then on the client to figure out that the article is not appropriate to
** post on its local system.  Of course, at that point, we have already
** wasted the network bandwidth in transferring the article...
**
** This is a roundabout way of saying that this program only implements
** active send.  There will have to be once-over done on the NNTP spec
** before passive poll goes in, because of the problems that I have cited.
**
** Erik E. Fair	<ucbvax!fair>, Oct 14, 1985
*/
#include <stdio.h>
#include <andrewos.h> /* sys/types.h sys/file.h strings.h syslog.h */
#include <ctype.h>
#ifdef HAS_SYSEXITS
#include <sysexits.h>
#endif /* HAS_SYSEXITS */
#include <errprntf.h>
#include <sys/param.h>
#include <util.h>
#include <mailconf.h>
#include <defs.h>
#include <header.h>
#include <respcode.h>
#include <nntpxmit.h>

extern	int	errno;
extern	char	*errmsg();
char    *progname = "nntpxmit";
char	*USAGE = "USAGE: nntpxmit file [user [orgname]]";
char	retmsg[LBUFLEN];
FILE	*getfp();
#define ERR_BADART  510
int test = 0;   /* Turn on to keep articles from actually being posted*/
int	GotOrg = 0;

main(ac, av)
int	ac;
char	*av[];
{
	register int	i;
	int status;
	char *file, *user, *orgname;

	if (ac < 2) {
		strcpy(retmsg, USAGE);
		fprintf(stdout, "%s\n", USAGE);
		exit(EX_USAGE);
	}
	errno = 0;
	if (CheckAMSConfiguration() != 0) {
	    sprintf(retmsg, "Configuration failed: %s", UnixError(errno));
	    printf("%s\n", retmsg);
	    exit(EX_TEMPFAIL);
	}

	file = av[1];
	user = orgname = NULL;
	if (ac > 2) user = av[2];
	if (ac > 3) orgname = av[3];

	for(i = 4; i < ac; i++) {
		if (av[i][0] == '-') {
			switch(av[i][1]) {
			case 't':
				fprintf(stderr, "%s: testing only.\n", progname);
				test = 1;
				break;
			default:
				sprintf(retmsg, "No such option: -%c (%d)", av[i][1], EX_USAGE);
				fprintf(stdout, "%s\n", retmsg);
				exit(EX_USAGE);
			}
			continue;
		}
	}

	status = sendnews(NNTPhost, file, user, orgname);
	fprintf(stdout, "%s\n", retmsg);
	exit(status);

}

/*
** Given a hostname to connect to, and a filename (which contains a
** netnews article), send those article to the named host using NNTP.
** ORGANIZATION: 
headers are added also.
*/
static sendnews(host, file, user, orgname)
char	*host, *file, *user, *orgname;
{
	register int	code;
	register FILE	*fp;
	char    tmpmsg[BUFSIZ], buf[BUFSIZ], file2[MAXPATHLEN];
	int status = EX_OK;
	/*
	** if no news to send, return
	*/

	if ((status = hello(host)) != EX_OK)
		return(status);
	if (!(fp = fopen(file, "r"))) {
	    if (vdown(errno)) {
		sprintf(retmsg, "AFS is apparently down for %s (%d)",
		    file, errno);
		return(EX_TEMPFAIL);
	    }
	    sprintf(retmsg, "Cannot open article file %s for reading", file);
	    return(EX_NOINPUT);
	}

	code = post(fp);

	fix_headers(fp, user, orgname, file, file2);

#ifdef DEBUG
	{
	char b[100];
	printf("Message:\n");
	sprintf(b, "cat %s", file2);
	printf("End of Message\n");
	system(b);
	}
#endif /* DEBUG */
	switch(code) {
		case CONT_POST:
			if ((status = sendfile(fp)) != EX_OK) {
				sprintf(tmpmsg,
					"Transmission of article failed (%d)",
					status);
				strcat(retmsg, tmpmsg);
				fclose(fp);
				goodbye(DONT_WAIT);
				if (!test) unlink(file2);
				return(status);
			}
			fclose(fp);

			code = readreply(buf, sizeof(buf));
			if (code != OK_POSTED) { 
			    if (!strcmp(buf, "441 435 Duplicate")) {
				/* INN's duplicate-post error message */
				code = OK_POSTED;
				status = EX_OK;
				sprintf(retmsg, "Article was a duplicate");
			    }
			    else if (code == ERR_POSTFAIL)  {
				sprintf(retmsg,
				   "Unable to post article to netnews: %s",
					buf);
				status = EX_UNAVAILABLE;
			    }
			    else {
				sprintf(retmsg,
					"Article not transferred (%d): %s",
					code, buf);
				if (code >= 100 && code < 400)
				    status = EX_OK;
				if ((code>=400 && code<500) || code == FAIL) 
				    status = EX_TEMPFAIL;
				else 
				    status = EX_UNAVAILABLE;
			    }
	    
			}
			else sprintf(retmsg,
				     "Article transferred successfully");
			break;
		case ERR_NOPOST: 
		case ERR_POSTFAIL:
			fclose(fp);
			status = EX_TEMPFAIL;
			break;
		case ERR_BADART:
			fclose(fp);
			status = EX_DATAERR;
			break;
		default:
			fclose(fp);
			if (code >= 100 && code < 400)
			    status = EX_OK;
			if (code >= 400 && code < 500)
			    status = EX_TEMPFAIL;
			else 
			    status = EX_UNAVAILABLE;
			break;


	}
	if (status == EX_OK)
	    sprintf(retmsg, "Article posted sucessfully");
	unlink(file2);
	goodbye(WAIT);
	return(status);
}

/*
** Post an article to netnews.  If an error occurs
** an explanatory messages will be put in Retmsg.  
*/
static post(fp)
FILE	*fp;
{
	char	scr[LBUFLEN];
	register int	code;
	char	buf[BUFSIZ];

	strcpy(buf, "POST");
#ifdef DEBUG
	fprintf(stderr, ">>>%s\n",buf);
#endif /* DEBUG */
	code = converse(buf, sizeof(buf));

	switch(code) {
	case CONT_POST:
		rewind(fp);
		return(code);
	case ERR_NOPOST:
		sprintf(retmsg,"The NNTP receiver on %s is not accepting articles: %s.",NNTPhost, buf);
		return(code);
	case ERR_COMMAND:
	case ERR_CMDSYN:
		sprintf(retmsg,"The NNTP receiver on %s found a syntax error in our POST command and rejected this article with: %s.", NNTPhost, buf);
		return(code);
	default:
		sprintf(retmsg,"The NNTP receiver on %s rejected this article with: %s.", NNTPhost, buf);
		return(code);

	}
}
/* if not already present, add PATH and ORGANIZATION headers
 * Also, fix Date: header and remove blank in-reply-to headers.
 */
static fix_headers(fp, user, orgname, file, file2)
FILE *fp;
char *user, *orgname, *file, *file2;
{
	FILE	*wfp;
	char	abuf[BUFSIZ];
	int	inheaders;

	sprintf(file2,"/tmp/temp.%d",getpid());
	if (!(wfp = fopen(file2,"w"))) {
	    if (vdown(errno)) {
		sprintf(retmsg,"AFS is apparently down for temp file %s (%d)",
		    file2,errno);
		return(EX_TEMPFAIL);
	    }
	    sprintf(retmsg,"Cannot open temp file %s for writing",file2);
	    return(EX_CANTCREAT);
	}

	fprintf(wfp, "Path: %s!%s\n", ThisDomain, (user == NULL || user[0] == '\0' ? NNTPuser : user));
	inheaders = 1;
	while (fgets(abuf,sizeof(abuf),fp)) {
	    if (*abuf == '\n') {
		if (inheaders && !GotOrg) {
		    fputs("Organization: ", wfp);
		    if (orgname != NULL && orgname[0] != '\0' && isupper(orgname[0])) {
			fputs(orgname, wfp);
		    } else {
			fputs(Organization, wfp);
		    }
		    putc('\n', wfp);
		}
		inheaders = 0;
	    }
	    else if (inheaders && prefix(abuf, "Date:")) {
		char	*from, *to;
		int	c, depth;

		/* remove RFC-822 comments */
		from = to = abuf;
		depth = 0;
		while (c = *from++) {
		    if (c == '(') depth++;
		    else if (c == ')') depth--;
		    else if (!depth) *to++ = c;
		}
		*to++ = 0;
	    }
	    else if (inheaders && prefix(abuf, "In-reply-to:")) {
		char *p = abuf + sizeof("In-reply-to:") - 1;

		/* If In-reply-to: header is blank, B-news will rewrite
		 * it into a header C-news will drop.  In that case, just
		 * delete the header
		 */
		while (*p && isspace(*p)) p++;
		if (!*p) continue;
	    }
	    else if (inheaders && prefix(abuf, "Path:")) {
		/* Strip Path: headers */
		continue;
	    }
	    else if (inheaders && prefix(abuf, "Organization:")) {
		GotOrg = 1;
	    }

	    fputs(abuf,wfp);
	    while (abuf[strlen(abuf)-1] != '\n' && fgets(abuf,sizeof(abuf),fp))
	      fputs(abuf,wfp);
	}
	fclose(wfp);
	fclose(fp);
	if (!(fp = fopen(file2,"r"))) {
	    if (vdown(errno)) {
		    sprintf(retmsg,"AFS is apparently down for %s (%d)",
			file2,errno);
		    unlink(file2);
		    return(EX_TEMPFAIL);
	    }
	    unlink(file2);
	    sprintf(retmsg,"Cannot open %s for reading",file2);
	    return(EX_NOINPUT);
	}
}
