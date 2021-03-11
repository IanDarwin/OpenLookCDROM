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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/contrib/snap2/nametime/RCS/nametime.c,v 2.13 1993/09/22 19:49:50 gk5g Exp $";
#endif

/* ************************************************************ *\

	nametime.c	Name and Time Server

	Author: David King
	Information Technology Center
	Carnegie-Mellon University

	(c) Copyright IBM Corporation, 1985
	Written: June 1, 1984


\* ************************************************************ */

#ifdef _IBMR2
#ifndef _BSD
#define _BSD 43
#endif
#endif

#include <andrewos.h> /* sys/types.h sys/time.h */
#include <stdio.h>
#include <sys/socket.h>
#include <sys/ioctl.h>
#include <netinet/in.h>
#include <netdb.h>

#define	IPNAME_TIMESERVER   "time"
#define	IPNAME_NAMESERVER   "name"

#ifndef IPPORT_TIMESERVER
#define	IPPORT_TIMESERVER	37
#endif /* IPPORT_TIMESERVER */

#ifndef IPPORT_NAMESERVER
#define	IPPORT_NAMESERVER	42
#endif /* IPPORT_NAMESERVER */

int nsock;
int tsock;
int selmask;
char buff[100];
struct sockaddr_in client;

main ()
{
    int readfds;
    struct servent *servent;
#ifndef DEBUG
    {
	register int i;
	if (fork ())
	    exit (0);
	for (i = 0; i < 10; i++)
	    close (i);
	open ("/", 0);
	dup2 (0, 1);
	dup2 (0, 2);
#ifdef TIOCNOTTY
	i = open ("/dev/tty", 2);
	if (i >= 0) {
	    ioctl (i, TIOCNOTTY, 0);
	    close (i);
	}
#endif
    }
#endif /* DEBUG */
    nsock = socket (AF_INET, SOCK_DGRAM, 0);
    if (nsock < 0) {
	panics ("socket");
	exit (-1);
    }
    tsock = socket (AF_INET, SOCK_DGRAM, 0);
    if (tsock < 0) {
	panics ("socket");
	exit (-1);
    }
    client.sin_addr.s_addr = INADDR_ANY;
    client.sin_family = AF_INET;
    if(servent = getservbyname(IPNAME_NAMESERVER,(char*)0)) {
	client.sin_port = servent->s_port;
    }
    else {
	fprintf(stderr,"nametime: Could not find '%s' service in /etc/services.  Using default port '%d'.\n", IPNAME_NAMESERVER, IPPORT_NAMESERVER);
	client.sin_port = IPPORT_NAMESERVER;
    }
    if (bind (nsock, &client, sizeof client) < 0) {
	panics ("bind name");
	exit (-1);
    }
    if(servent = getservbyname(IPNAME_TIMESERVER,(char*)0)) {
	client.sin_port = servent->s_port;
    }
    else {
	fprintf(stderr,"nametime: Could not find '%s' service in /etc/services.  Using default port '%d'.\n", IPNAME_TIMESERVER, IPPORT_TIMESERVER);
	client.sin_port = IPPORT_TIMESERVER;
    }
    if (bind (tsock, &client, sizeof client) < 0) {
	panics ("bind time");
	exit (-1);
    }
    selmask = 1 << nsock | 1 << tsock;
    while (1) {
	register int cc;
	int fromlen;

	readfds = selmask;
	cc = select (20, &readfds, 0, 0, 0);
	if (cc < 0) {
	    panics ("select");
	    continue;
	}
	if (readfds & (1 << nsock)) {
	    fromlen = sizeof client;
	    cc = recvfrom (nsock, buff, 100, 0, &client, &fromlen);
	    if (cc < 0) {
		panics ("recvfrom name");
		continue;
	    }
#ifdef DEBUG
	    printf ("received name request\n");
#endif /* DEBUG */
	    nameit (buff, cc);
	}
	else {
	    fromlen = sizeof client;
	    cc = recvfrom (tsock, buff, 100, 0, &client, &fromlen);
	    if (cc < 0) {
		panics ("recvfrom time");
		continue;
	    }
	    timeit ();
	}
    }
}

nameit (buff, size)
char *buff;
{
    register char *mp;
    register int nl;
    struct sockaddr_in target;
    mp = buff;
    if (size < 3) {
#ifdef DEBUG
	printf ("bad len\n");
#endif /* DEBUG */
	return;
    }
    if (*mp++ != 1 || (nl = *mp++) != size - 2) {
#ifdef DEBUG
	printf ("bad fmt\n");
#endif /* DEBUG */
	return;
    }
    if (find_addr (mp, nl, &target)) {
        unsigned long myaddr=target.sin_addr.s_addr;
	mp += nl;
	*mp++ = 2;
	*mp++ = 4;
	*mp++ = ((myaddr>>24)&0xff);
	*mp++ = ((myaddr>>16)&0xff);
	*mp++ = ((myaddr>> 8)&0xff);
	*mp++ = ((myaddr    )&0xff);
	nl += 8;
    }
    else {
	mp += nl;
	*mp++ = 3;
	*mp++ = 2;
	*mp++ = 1;
	*mp++ = '?';
	nl += 6;
    }
    if (sendto (nsock, buff, nl, 0, &client, sizeof client) < 0)
	panics ("sendto name");
#ifdef DEBUG
    else
	printf("answer sent\n");
#endif /* DEBUG */
}

int find_addr (np, nl, tg)
register char *np;
struct sockaddr_in *tg;
{
    register struct hostent *hptr;
    np[nl] = '\0';
    hptr = gethostbyname (np);
    if (hptr == 0) {
#ifdef DEBUG
	printf ("gethostbynamereturned zero\n");
#endif /* DEBUG */
	return (0);
    }
    if (hptr->h_addrtype != AF_INET || hptr->h_length != 4) {
#ifdef DEBUG
	printf ("gethostbyname returned non-IP name\n");
#endif /* DEBUG */
	return (0);
    }
    bcopy (hptr->h_addr, &tg->sin_addr.s_addr, 4);
#ifdef DEBUG
    printf ("resolving name %s to address %x\n", np, tg->sin_addr.s_addr);
#endif /* DEBUG */
    return (1);
}

timeit ()
{
    int tbuild;
    struct timeval tv;

    gettimeofday (&tv, 0);
    tbuild = htonl (tv.tv_sec + 2208988800);
    if (sendto (tsock, &tbuild, sizeof tbuild, 0, &client, sizeof client) < 0)
	panics ("sendto time");
}

panic (s)
char *s;
{
    register int fd;
    char msg[128];
    sprintf (msg, "nametime: %s\n", s);
    fd = open ("/dev/console", 1, 0744);
    write (fd, msg, strlen (msg));
    close (fd);
}

panics (s)
char *s;
{
    char msg[100];
    extern int errno;
    extern int sys_nerr;
    extern char *sys_errlist[];

    strcpy (msg, s);
    if (errno <= sys_nerr) {
	strcat (msg, ": ");
	strcat (msg, sys_errlist[errno]);
    }
    else
	strcat (msg, ": invalid error number");
    panic (msg);
}
