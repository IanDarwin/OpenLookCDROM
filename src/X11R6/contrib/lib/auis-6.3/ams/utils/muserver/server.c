/* server.c -- master update server
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
 * Start Date: 9/4/91
 */

#ifndef NORCSID
#define NORCSID
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/ams/utils/muserver/RCS/server.c,v 1.16 1993/10/26 20:01:05 gk5g Exp $";
#endif


#ifdef _IBMR2
#ifndef _BSD
#define _BSD 43
#endif
#endif

#include <andrewos.h>
#include <fcntl.h>
#include <stdio.h>
#include <signal.h>
#include <errno.h>
#include <sys/wait.h>

#include "mufbuf.h"
#include "servlib.h"
#include "config.h"

#define BUFSIZE 4096
#define FDBASE 3		/* base file descriptor */

#ifdef CMU_ENV
static char logfile[] = "/usr/log/muserver.log";
static char oldfile[] = "/usr/log/muserver.old";
#else
static char logfile[] = "/tmp/muserver.log";
static char oldfile[] = "/tmp/muserver.old";
#endif

typedef struct fdinfo {
    int active;
    int pos;
    int requests;
    struct timeval tv;
    char buf[BUFSIZE];
} fdinfo;

/* from OS: */
extern int errno;
extern int sys_nerr;
extern char *sys_errlist[];

/* global storage buffers */
static mufbuf current, new;
static char outbuf[BUFSIZE];

/* global status info */
static int lastupdate = 0;
static int numupdates = 0;
static int conntotal = 0, connnum = 0;
static int lasttm = 0;
static int reqtotal = 0;
static int nactive = 0;
static int maxactive = 0;
static int isserver = 1;

/* output statistics to a file descriptor
 */
static void statistics(fd)
    int fd;
{
    int cnum = (connnum ? connnum : 1);
    int avgtm = conntotal / cnum;
    
    sprintf(outbuf, "lastupdate: %snum updates: %d\nconns active: %d\nconns done: %d\nlast time: %d.%06d\navg time: %d.%06d\navg requests: %d\nmax active: %d\n",
	    ctime(&lastupdate), numupdates, nactive, connnum,
	    lasttm / 1000000, lasttm % 1000000,
	    avgtm / 1000000, avgtm % 1000000,
	    reqtotal / cnum, maxactive);
    (void) write(fd, outbuf, strlen(outbuf));
}

/* print an error message with time to the logfile
 */
static void server_msg(msg, num)
    char *msg;
    int num;
{
    struct timeval tv;

    gettimeofday(&tv, 0);
    fprintf(stderr, "%s (%d) %s", msg, num, ctime(&tv.tv_sec));
}
static void server_fatal(msg, num)
    char *msg;
    int num;
{
    server_msg(msg, num);
    fflush(stderr);
    statistics(2);
    exit(1);
}
static void unix_fatal(msg)
    char *msg;
{
    struct timeval tv;

    gettimeofday(&tv, 0);
    fprintf(stderr, "%s: %s %s", msg,
	    (errno < sys_nerr ? sys_errlist[errno] : "unknown error"),
	    ctime(&tv.tv_sec));
    exit(1);
}

/* process input and send update info
 * returns: -1 = should be closed, 0 = normal, 1 = blocked
 */
int sendupdate(fd, buf)
    int fd;
    fdinfo *buf;
{
    int rtval = 0;
    int count;
    int len;
    char *scan, *wbase, *base = buf->buf;
    char *end = base + buf->pos;
    char *optr, *entry;

    optr = outbuf;
    wbase = base;
    do {
	for (scan = base; scan < end && *scan != '\n'; ++scan);
	if (scan < end) {
	    *scan = '\0';
	    if (base[0] == '$') {
		if (base[1] == 'q') {
		    rtval = -1;
		    break;
		}
		if (base[1] == 'i') {
		    buf->active = 2;
		    statistics(fd);
		}
	    }
	    entry = mufbuf_lookup(&current, base);
	    if (entry == (char *) NULL) entry = base;
	    len = strlen(entry);
	    if (optr + len >= outbuf + BUFSIZE) {
		count = write(fd, outbuf, optr - outbuf);
		if (count < optr - outbuf) {
		    if (count > 0 || (count < 0 && errno != EWOULDBLOCK)) {
			return (-1);
		    }
		    *scan = '\n';
		    rtval = 1;
		    break;
		}
		optr = outbuf;
		wbase = base;
	    }
	    (void) strcpy(optr, entry);
	    optr += len;
	    *optr++ = '\n';
	    *scan++ = '\n';
	    base = scan;
	    ++buf->requests;
	}
    } while (scan < end);
    if (rtval < 1 && optr > outbuf) {
	count = write(fd, outbuf, optr - outbuf);
	if (count < optr - outbuf) {
	    if (count > 0 || (count < 0 && errno != EWOULDBLOCK)) {
		return (-1);
	    }
	    rtval = 1;
	} else {
	    wbase = base;
	}
    }
    if (rtval != -1) {
	for (scan = buf->buf; wbase < end; *scan++ = *wbase++);
	buf->pos = scan - buf->buf;
    }

    return (rtval);
}

/* unzombie processes
 */
int unzombie()
{
#ifdef POSIX_ENV
    int status;
    while (waitpid(&status, WNOHANG, 0) > 0);
#else
    while (wait3((union wait *) 0, WNOHANG, (struct rusage *) 0) > 0);
#endif 
}

/* reset statistics
 */
int statreset()
{
    static char sreset[] = "statistics reset";
		    
    reqtotal = conntotal = connnum = 0;
    server_msg(sreset, 0);
    fflush(stderr);
}

/* close client connection
 */
void closeconn(fd, buf)
    int fd;
    fdinfo *buf;
{
    struct timeval tv;

    (void) close(fd);
    if (buf->active == 1) {
	gettimeofday(&tv, 0);
	lasttm = (tv.tv_sec - buf->tv.tv_sec) * 1000000
	    + tv.tv_usec - buf->tv.tv_usec;
	++connnum;
	conntotal += lasttm;
	reqtotal += buf->requests;
    }
    buf->active = 0;
    if (!--nactive && !isserver) {
	gettimeofday(&tv, 0);
	printf("Subserver (%d) exiting %s", getpid(), ctime(&tv.tv_sec));
	exit(0);
    }
    
}

/* muserver server process
 */
void server()
{
    int sock, port = SERVER_PORT;
    fd_set frset, fwset, freadset, fwriteset;
    int nfds, fd, count;
    int instate = 0, size, remaining;
    fdinfo *fdbufs, *buf;
    char *inptr;
    unsigned char inbuf[4];

    /* init logfile */
    (void) rename(logfile, oldfile);
    if ((fd = open(logfile, O_CREAT|O_WRONLY|O_TRUNC, 0644)) < 0) {
	unix_fatal(logfile);
    }
    (void) dup2(fd, 1);
    (void) dup2(fd, 2);
    (void) close(fd);
    
    /* init socket */
    if ((sock = serv_init(&port)) < 0) {
	unix_fatal("serv_init");
    }

    /* init master update buffers */
    if (mufbuf_init(&current) < 0 || mufbuf_init(&new) < 0) {
	server_fatal("mufbuf_init", 0);
    }

    /* init fd buffers */
    if ((nfds = getdtablesize()) <= 0) {
	unix_fatal("getdtablesize");
    }
    fdbufs = (fdinfo *) malloc(sizeof (fdinfo) * (nfds - FDBASE));
    if (fdbufs == (fdinfo *) NULL) {
	server_fatal("out of memory", 0);
    }
    for (fd = FDBASE, buf = fdbufs; fd < nfds; ++fd, ++buf) {
	buf->active = 0;
    }
    
    /* set stdin to no delay, and add it to freadset list */
    if (fcntl(0, F_SETFL, FNDELAY) < 0) {
	unix_fatal("fcntl");
    }
    FD_ZERO(&freadset);
    FD_ZERO(&fwriteset);
    FD_SET(0, &freadset);

    /* handle statistic reset requests */
    (void) signal(SIGHUP, statreset);
    
    /* handle children */
    (void) signal(SIGCHLD, unzombie);

    /* ignore SIGPIPE errors */
    (void) signal(SIGPIPE, SIG_IGN);

    /* loop */
    for (;;) {
	frset = freadset;
	fwset = fwriteset;
	if (select(nfds, &frset, &fwset, 0, 0) <= 0) {
	    continue;
	}

	/* read from update process */
	if (FD_ISSET(0, &frset)) {
	    /* get size parameter */
	    if (instate < sizeof (inbuf)) {
		count = read(0, inbuf + instate, sizeof (inbuf) - instate);
		if (count <= 0) {
		    server_fatal("lost connection to update process", count);
		}
		if ((instate += count) == sizeof (inbuf)) {
		    size = ((int) inbuf[0] << 24) + ((int) inbuf[1] << 16)
			+ ((int) inbuf[2] << 8) + inbuf[3];
		    remaining = size;
		    inptr = mufbuf_datasize(&new, size);
		    if (inptr == (char *) NULL) {
			server_fatal("mufbuf_datasize", size);
		    }
		}
		continue;
	    }

	    /* fill input buffer */
	    count = read(0, inptr, remaining);
	    if (count <= 0) {
		server_fatal("Lost connection to update process", count);
	    }
	    inptr += count;
	    if (!(remaining -= count)) {
		/* use new table & activate sock accepts */
		instate = 0;
		mufbuf_table(&new, size);
		if (mufbuf_copy(&current, &new, 0) < 0) {
		    server_fatal("mufbuf_copy failed", size);
		}
		if (!FD_ISSET(sock, &freadset)) {
		    FD_SET(sock, &freadset);
		    server_msg("started", 0);
		}
		lastupdate = time(0);
		++numupdates;
	    }
	    continue;
	}

	/* accept connections */
	if (FD_ISSET(sock, &frset)) {
	    while ((fd = serv_accept()) >= 0) {
		if (fcntl(fd, F_SETFL, FNDELAY) < 0) {
		    server_msg("connection failed FNDELAY", fd);
		    (void) close(fd);
		    continue;
		}
		buf = fdbufs + fd - FDBASE;
		gettimeofday(&buf->tv, 0);
		buf->active = 1;
		buf->requests = 0;
		buf->pos = 0;
		FD_SET(fd, &freadset);
		if (++nactive > maxactive) maxactive = nactive;
	    }

	    /* if we ran out of file descriptors, fork() */
	    if (errno == EMFILE && (isserver = fork()) >= 0) {
		if (isserver) {
		    buf = fdbufs;
		    for (fd = FDBASE; fd < nfds; ++buf, ++fd) {
			if (buf->active) {
			    buf->active = 0;
			    FD_CLR(fd, &freadset);
			    FD_CLR(fd, &fwriteset);
			    (void) close(fd);
			    --nactive;
			}
		    }
		} else {
		    FD_CLR(sock, &freadset);
		    FD_CLR(0, &freadset);
		    (void) close(0);
		    (void) close(sock);
		}
	    }
	    continue;
	}

	/* read/write data */
	for (buf = fdbufs, fd = FDBASE; fd < nfds; ++fd, ++buf) {
	    /* read data from client */
	    if (buf->active && FD_ISSET(fd, &frset)) {
		count = read(fd, buf->buf + buf->pos, BUFSIZE - buf->pos);
		if (count < 0) {
		    if (errno != EWOULDBLOCK) {
			FD_CLR(fd, &freadset);
			FD_CLR(fd, &fwriteset);
			closeconn(fd, buf);
		    }
		} else {
		    buf->pos += count;
		    FD_SET(fd, &fwset);
		}
	    }

	    /* write data to client */
	    if (buf->active && FD_ISSET(fd, &fwset)) {
		switch (sendupdate(fd, buf)) {
		    case -1:
			FD_CLR(fd, &fwriteset);
			FD_CLR(fd, &freadset);
			closeconn(fd, buf);
			break;
		    case 0:
			FD_CLR(fd, &fwriteset);
			break;
		    case 1:
			FD_SET(fd, &fwriteset);
			break;
		}
	    }
	}
    }
}
