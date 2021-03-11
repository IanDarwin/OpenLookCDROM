/* muclient.c -- master update client routines
 *
 *	(C) Copyright 1991-1992 by Carnegie Mellon University
 *
 *                      All Rights Reserved
 *
 * Permission to use, copy, modify, and distribute this software and its 
 * documentation for any purpose and without fee is hereby granted, 
 * provided that the above copyright notice appear in all copies and that
 * both that copyright notice and this permission notice appear in 
 * supporting documentation, and that the name of CMU not be
 * used in advertising or publicity pertaining to distribution of the
 * software without specific, written prior permission.  
 * 
 * CMU DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING
 * ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL
 * CMU BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR
 * ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,
 * WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION,
 * ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS
 * SOFTWARE.
 *
 * Author: Chris Newman
 * Start Date: 9/19/91
 */
#ifdef _IBMR2
#ifndef _BSD
#define _BSD 43
#endif
#endif

#include <andrewos.h>
#include <stdio.h>
#include <ctype.h>
#include <mailconf.h>
#include <ms.h>
#include <fcntl.h>
#include <signal.h>
#include <sys/types.h>
#include <sys/time.h>
#include <sys/socket.h>
#include <sys/file.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <netdb.h>

#ifndef NORCSID
#define NORCSID
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/ams/libs/ms/RCS/muclient.c,v 1.10 1993/10/12 18:56:00 gk5g Exp $";
#endif

#ifdef _IBMR2
#include <sys/select.h>
#endif

#ifdef POSIX_ENV
    typedef void SignalReturnType;
#else
    typedef int SignalReturnType;
#endif
    

/* port number for master update server
 */
#define SERVER_PORT 6666

/* seconds to wait for server on a connect
 */
#define SERV_TIMEOUT 5

/* number of path elements on the muserver
 */
#define PATHELT 3

/* connect to host at port, waiting secs for connection
 */
int cli_connect(host, port, secs)
    char *host;
    int port, secs;
{
    int sock;
    struct sockaddr_in server;
    struct timeval tv;
    struct hostent *hp, *gethostbyname();
    fd_set fset;

    /* get host, create socket */
    if (!isdigit(host[0]) || (server.sin_addr.s_addr = inet_addr(host)) == -1) {
	if ((hp = gethostbyname(host)) == (struct hostent *) NULL) {
	    return (-1);
	}
	bcopy(hp->h_addr, &server.sin_addr, hp->h_length);
    }
    server.sin_port = htons(port);
    server.sin_family = AF_INET;

    /* open socket, and put it in non-blocking mode */
    if ((sock = socket(AF_INET, SOCK_STREAM, 0)) < 0) {
	return (-1);
    }
    if (fcntl(sock, F_SETFL, FNDELAY) < 0) {
	(void) close(sock);
	return (-1);
    }

    /* connect, non-blocking */
    if (connect(sock, (struct sockaddr *) &server, sizeof (server)) < 0) {
	if (errno == EWOULDBLOCK || errno == EINPROGRESS) {
	    FD_ZERO(&fset);
	    FD_SET(sock, &fset);
	    tv.tv_usec = 0;
	    tv.tv_sec = secs;
	    if (select(sock + 1, 0, &fset, 0, &tv) > 0) {
		return (sock);
	    }
	}
	(void) close(sock);
	return (-1);
    }

    return (sock);
}

/* do a quick check of the master update server
 */
void mu_quickcheck(subs, num)
    struct SubscriptionProfile **subs;
    int num;
{
    int subw, subr, j, i, servnum, subdiff;
    int size, count, mufd, waitwrite, len;
#if defined(__STDC__) && !defined(_NO_PROTO)    
    SignalReturnType (*tmpfunc)(int sig);
#else
    SignalReturnType (*tmpfunc)();
#endif    
    struct timeval to;
    char *base, *pos, *opos, *cpy, *label;
    fd_set fset, fwset;
    char inbuf[4096];
    char outbuf[4096];
    static int goodelt[PATHELT];

    /* initialize HasChanged field */
    for (i = 0; i < num; ++i) {
	subs[i]->HasChanged = 0;
    }

    /* read our personal andrew setup options */
    CheckAMSConfiguration();
    if (!numAMS_MasterUpdateHosts) return;

    /* pick a server to try */
    servnum = (time(0) ^ getpid()) % numAMS_MasterUpdateHosts;
    if ((mufd = cli_connect(AMS_MasterUpdateHosts[servnum], SERVER_PORT, SERV_TIMEOUT)) < 0) {
	return;
    }
    tmpfunc = signal(SIGPIPE, SIG_IGN);

    /* initialize goodelt array */
    for (i = 0; i < PATHELT; ++i) {
	goodelt[i] = -2;
    }

    subw = 0;
    subr = 0;
    size = 0;
    waitwrite = 0;
    pos = inbuf;
    opos = outbuf;
    while (subr < num) {
	/* write a master update request, if one is left to be written */
	while (subw < num) {
	    /* make sure we want to look it up */
	    if (subs[subw]->status == AMS_UNSUBSCRIBED) {
		++subw;
		continue;
	    }
	
	    /* make sure this is one we can lookup */
	    for (j = 0; j < PATHELT; ++j) {
		if (subs[subw]->pathelt == goodelt[j]) break;
	    }
	    if (j == PATHELT) {
		for (j = 0; j < PATHELT && goodelt[j] != -2; ++j);
		if (j == PATHELT || subs[subw]->pathelt < 0
		    || subs[subw]->pathelt >= MS_NumDirsInSearchPath) {
		    ++subw;
		    continue;
		}
		label = SearchPathElements[subs[subw]->pathelt].label;
		if (label && (!strcmp("external", label)
			      || !strcmp("official", label)
			      || !strcmp("local", label))) {
		    goodelt[j] = subs[subw]->pathelt;
		} else {
		    ++subw;
		    continue;
		}
	    }

	    /* look it up & send it */
	    len = strlen(subs[subw]->sname) + 1;
	    if (opos - outbuf + len > sizeof (outbuf) - 3) {
		waitwrite = 1;
		break;
	    }
	    for (base = subs[subw]->sname; *opos = *base++; ++opos);
	    *opos++ = '\n';

	    ++subw;
	}

	if (subw == num) {
	    subw++;
 /* send quit command to minimize connect time */
	    *opos++ = '$';
	    *opos++ = 'q';
	    *opos++ = '\n';
	    waitwrite = 1;
	}
	
	/* flush any pending output */
	if (waitwrite && opos > outbuf) {
	    count = write(mufd, outbuf, opos - outbuf);
	    if (count < opos - outbuf) {
		if (count >= 0) {
		    base = outbuf;
		    cpy = outbuf + count;
		    while (cpy < opos) *base++ = *cpy++;
		    opos = base;
		} else if (errno != EWOULDBLOCK && errno != EINPROGRESS) {
		    goto ABORT;
		}
		waitwrite = 1;
	    } else {
		opos = outbuf;
		waitwrite = 0;
	    }
	}

	/* check for incoming data to read */
	to.tv_sec = !waitwrite && subw < num ? 0 : 5;
	to.tv_usec = 0;
	FD_ZERO(&fset);
	FD_ZERO(&fwset);
	FD_SET(mufd, &fset);
	if (waitwrite) FD_SET(mufd, &fwset);
	if ((j = select(mufd + 1, &fset, &fwset, 0, &to)) <= 0) {
	    if (j == 0 && !waitwrite && subw < num) continue;
	    break;
	}
	if (!FD_ISSET(mufd, &fset)) continue;

	/* copy data into buffer */
	count = read(mufd, pos, sizeof (inbuf) - size);
	if (count <= 0) {
	    break;
	}
	size += count;

	/* break buffer at newlines */
	while (count > 0 && *pos != '\n') --count, ++pos;
	base = inbuf;
	while (count > 0) {
	    *pos++ = '\0', --count;
	    
	    /* skip uninteresting subscriptions */
	    while (subr < num &&
		   (subs[subr]->status == AMS_UNSUBSCRIBED ||
		    (subs[subr]->pathelt != goodelt[0] &&
		     subs[subr]->pathelt != goodelt[1] &&
		     subs[subr]->pathelt != goodelt[2]))) {
		++subr;
	    }
	    if (subr == num) break;

	    /* make sure it's what we want */
	    if (!strncmp(base, subs[subr]->sname, strlen(subs[subr]->sname))) {
		/* ok, it's a valid one */
		++subs[subr]->HasChanged;
		subdiff = strncmp(pos - AMS_DATESIZE, "000000", AMS_DATESIZE) ?
		    strncmp(pos - AMS_DATESIZE, subs[subr]->time64, AMS_DATESIZE) : 0;
		if (subdiff < 0) FixSubsDate(subs[subr], pos - AMS_DATESIZE);
		if (subs[subr]->status == AMS_SHOWALLSUBSCRIBED || subdiff > 0) {
		    ++subs[subr]->HasChanged;
		}
	    }
	    ++subr;

	    /* look for another newline */
	    base = pos;
	    while (count > 0 && *pos != '\n') --count, ++pos;
	    if (count == 0) {
		/* if no newline found, copy contents of buffer to beginning */
		size = count = pos - base;
		for (pos = inbuf; count > 0; --count) *pos++ = *base++;
		break;
	    }
	}
    }

 ABORT:
    (void) signal(SIGPIPE, tmpfunc);
    (void) close(mufd);
}
