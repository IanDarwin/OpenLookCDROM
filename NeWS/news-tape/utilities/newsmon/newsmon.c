/*
 * This file is a product of Sun Microsystems, Inc. and is provided for
 * unrestricted use provided that this legend is included on all tape
 * media and as a part of the software program in whole or part.  Users
 * may copy or modify this file without charge, but are not authorized to
 * license or distribute it to anyone else except as part of a product
 * or program developed by the user.
 *
 * THIS FILE IS PROVIDED AS IS WITH NO WARRANTIES OF ANY KIND INCLUDING THE
 * WARRANTIES OF DESIGN, MERCHANTIBILITY AND FITNESS FOR A PARTICULAR
 * PURPOSE, OR ARISING FROM A COURSE OF DEALING, USAGE OR TRADE PRACTICE.
 *
 * This file is provided with no support and without any obligation on the
 * part of Sun Microsystems, Inc. to assist in its use, correction,
 * modification or enhancement.
 *
 * SUN MICROSYSTEMS, INC. SHALL HAVE NO LIABILITY WITH RESPECT TO THE
 * INFRINGEMENT OF COPYRIGHTS, TRADE SECRETS OR ANY PATENTS BY THIS FILE
 * OR ANY PART THEREOF.
 *
 * In no event will Sun Microsystems, Inc. be liable for any lost revenue
 * or profits or other special, indirect and consequential damages, even
 * if Sun has been advised of the possibility of such damages.
 *
 * Sun Microsystems, Inc.
 * 2550 Garcia Avenue
 * Mountain View, California  94043
 *
 * Mark Opperman
 * opcode@sun.com
 * 3 September, 1988
 */

#include <stdio.h>
#include <curses.h>
#include <signal.h>
#include <sys/types.h>
#ifdef notdef
#include <sys/fcntlcom.h>
#else
#include <sys/fcntl.h>
#endif
#include <sys/socket.h>
#include <sys/time.h>
#include <netinet/in.h>
#include <netdb.h>
#include "newsmon.h"

/*
 * 4.3 and V8 style file descriptor mask macros
 * Should be in <sys/types.h> but aren't in 4.2.
 */
#ifndef FD_SET
#ifndef NOFILE
#define NOFILE 32               /* wrong, but sufficient */
#endif
#ifdef notdef                   /* this already exists in 4.2 */
typedef struct fd_set {
        int     fds_bits;
      } fd_set;
#endif
#define FD_SET(n, p)    ((p)->fds_bits[0] |= (1 << (n)))
#define FD_CLR(n, p)    ((p)->fds_bits[0] &= ~(1 << (n)))
#define FD_ISSET(n, p)  ((p)->fds_bits[0] & (1 << (n)))
#endif

/*
 * 4.3 uses FD_SETSIZE rather than NOFILE for the mask macros.
 */
#ifndef NOFILE
#define MAXFD FD_SETSIZE
#else
#define MAXFD NOFILE
#endif

#define	MAXCONNECTIONS	2
#define	ADDFD(fd)	if (fd > maxfd) maxfd = fd; FD_SET(fd, &activefds)
#define	REMOVEFD(fd)	FD_CLR(fd, &activefds)

int maxfd = 0;
int serverport = 2000;
int clientport = 2001;
int clientlistener = -1;
int terseflag = 0;
int tokensflag = 0;
int nconnections = 0;

fd_set activefds;
struct connection connections[MAXCONNECTIONS];
struct hostent myhostent;

static char *argv0;

extern char *sprintf();

main(argc, argv)
    int argc;
    char **argv;
{
    fd_set fds;
    register int n;
    register int i;
    register int index;

    init(argc, argv);

    for (;;) {
	fds = activefds;
	n = select(maxfd+1, &fds, (fd_set *) 0, (fd_set *) 0,
							(struct timeval *) 0);
	if (n <= 0)
	    continue;
	if (FD_ISSET(clientlistener, &fds)) {
	    setupnewclient();
	    n--;
	}
	for (i=0; n && i<=maxfd; i++) {
	    if (FD_ISSET(i, &fds)) {
		if ((index = fromclient(i)) >= 0)
		    handledatafromclient(index);
		else if ((index = fromserver(i)) >= 0)
		    handledatafromserver(index);
		else
		    msgexit("select: %d?\n", i);
		n--;
	    }
	}
    }
}

/*
 * Returns index in connections if fd is a connection to a client.
 * Else returns -1;
 */
fromclient(fd)
    register int fd;
{
    register int i;

    for (i=0; i<MAXCONNECTIONS; i++)
	if (fd == connections[i].client_fd)
	    return i;
    
    return -1;
}

/*
 * Returns index in connections if fd is a connection to the server.
 * Else returns -1;
 */
fromserver(fd)
    register int fd;
{
    register int i;

    for (i=0; i<MAXCONNECTIONS; i++)
	if (fd == connections[i].server_fd)
	    return i;
    
    return -1;
}

handledatafromclient(index)
{
    char buf[1024];
    register int nread;
    register int nwritten;

    nread = read(connections[index].client_fd, buf, sizeof buf);

    switch (nread) {
    case -1:
	msgexit("handledatafromclient read");
	break;
    case 0:
	takedownconnection(index);
	break;
    default:
	nwritten = write(connections[index].server_fd, buf, nread);
	if (nwritten != nread)
	    msgexit("handledatafromclient write");
	else {
	    connections[index].client_nbytes += nread;
	    displaybytesfromclient(index);
	    displayclientdata(index, buf, nread);
	    refresh();
	}
	break;
    }
}

handledatafromserver(index)
{
    char buf[1024];
    register int nread;
    register int nwritten;

    nread = read(connections[index].server_fd, buf, sizeof buf);

    switch (nread) {
    case -1:
	msgexit("handledatafromserver read:");
	break;
    case 0:
	takedownconnection(index);
	break;
    default:
	nwritten = write(connections[index].client_fd, buf, nread);
	if (nwritten != nread)
	    msgexit("handledatafromserver write");
	else {
	    connections[index].server_nbytes += nread;
	    displaybytesfromserver(index);
	    displayserverdata(index, buf, nread);
	    refresh();
	}
	break;
    }
}

displayserverdata(index, buf, nbytes)
    int index;
    char *buf;
    int nbytes;
{
    displaygenericdata(index, buf, nbytes, 1);
}

displayclientdata(index, buf, nbytes)
    int index;
    char *buf;
    int nbytes;
{
    displaygenericdata(index, buf, nbytes, 0);
}

displaygenericdata(index, buf, nbytes, server)
    int index;
    char *buf;
    int nbytes;
    int server;		/* 0 = client, 1 = server */
{
    register struct connection *conn = &connections[index];
    register int x, y;
    register int i;

    y = conn->firstrow + 2 + server +
		((conn->currentpos / (COLS - 1)) % conn->ngroups);
    x = conn->currentpos % (COLS - 1);
    move(y, x);

    for (i=0; i<nbytes; i++) {
	y = conn->firstrow + 2 + server +
			((conn->currentpos / (COLS - 1)) % conn->ngroups * 3);
	x = conn->currentpos % (COLS - 1);
	if (x == 0) {
	    move(y, 0);
	    clrtoeol();
	    move(server ? y-1 : y+1, 0);
	    clrtoeol();
	}
	if ((buf[i]) >= ' ' && buf[i] <= '~') {
	    mvaddch(y, x, (u_char *) buf[i]);
	    conn->currentpos += 1;
	}
	else {
	    if (tokensflag) {
#ifdef	notdef
		char *dbuf;
		decodestr(conn, server, buf, nbytes, &i);
#endif	/*notdef*/
	    }
	    else if (!terseflag) {
		char obuf[5];
		(void) sprintf(obuf, "\\%03o", buf[i] & 0xff);
		displaygenericstr(conn, server, obuf);
	    }
	    else {
		mvaddch(y, x, '.');
		conn->currentpos += 1;
	    }
	}
    }

    refresh();
}

displaygenericstr(conn, server, buf)
    register struct connection *conn;
    register int server;
    register char *buf;
{
    register int x, y;

    while (*buf) {
	y = conn->firstrow + 2 + server +
		((conn->currentpos / (COLS - 1)) % conn->ngroups * 3);
	x = conn->currentpos % (COLS - 1);
	if (x == 0) {
	    move(y, 0);
	    clrtoeol();
	    move(server ? y-1 : y+1, 0);
	    clrtoeol();
	}
	mvaddch(y, x, (u_char) *buf);
	buf++;
	conn->currentpos += 1;
    }
}

takedownallconnections()
{
    register int i;

    for (i=0; i<nconnections; i++)
	takedownconnection(i);
    nconnections = 0;
}

takedownconnection(index)
    register int index;
{
    register int i;

    (void) close(connections[index].client_fd);
    REMOVEFD(connections[index].client_fd);
    (void) close(connections[index].server_fd);
    REMOVEFD(connections[index].server_fd);
    nconnections--;

    for (i=index; i<MAXCONNECTIONS-1; i++)
	connections[i] = connections[i+1];

    /*
    recalcgroups();
    redisplaygroups();
    */
}

setupnewclient()
{
    struct sockaddr_in client;
    struct sockaddr_in server;
    int fd;
    int i;
    int namelen = sizeof client;

    bzero((char *) &client, sizeof client);
    if ((fd = accept(clientlistener, (struct sockaddr *)&client, &namelen)) < 0)
	perrorexit("accept");

    if (nconnections >= MAXCONNECTIONS) {
#ifdef  notdef
	standout();
	(void) fprintf(stderr, "connection refused");
	standend();
	refresh();
#endif
	(void) close(fd);
	return;
    }

    i = nconnections;
    nconnections += 1;

    connections[i].client_fd = fd;

    if ((fd = socket(AF_INET, SOCK_STREAM, IPPROTO_TCP)) < 0)
	perrorexit("socket");
    bcopy(myhostent.h_addr, &(server.sin_addr.s_addr), myhostent.h_length);
    server.sin_port = htons(serverport);
    server.sin_family = AF_INET;
    if (connect(fd, &server, sizeof server) < 0)
	perrorexit("server connect");

    connections[i].server_fd = fd;

    connections[i].currentpos = 0;
    connections[i].client_nbytes = 0;
    connections[i].server_nbytes = 0;

    ADDFD(connections[i].server_fd);
    ADDFD(connections[i].client_fd);

    recalcgroups();
    redisplaygroups();
    /*
    getyx(stdscr, y, x);
    mvcur(y, x, connections[i].firstrow + 2, 0);
    move(connections[i].firstrow + 2, 0);
    */
    refresh();
}

displaybytesfromserver(index)
    register int index;
{
    move(connections[index].firstrow, 56);
    printw("%6d", connections[index].server_nbytes);
}

displaybytesfromclient(index)
    register int index;
{
    move(connections[index].firstrow, 24);
    printw("%6d", connections[index].client_nbytes);
}

dashrow(row)
    register int row;
{
    register int i;
    register int cols = COLS - 1;

    move(row, 0);
    for (i=0; i<cols; i++)
	addch('-');
}

displayheader()
{
    move(0, COLS/2-18);
    printw("Client Port ");
    standout();
    printw("%5d", clientport);
    standend();
    printw("  Server Port ");
    standout();
    printw("%5d", serverport);
    standend();
    dashrow(1);
}

redisplaygroups()
{
    register int i;
    register int j;

    move(2, 0);
    clrtobot();

    for (i=0; i<nconnections; i++) {
	move(connections[i].firstrow, 0);
	printw("Total Bytes From CLIENT ");
	displaybytesfromclient(i);
	move(connections[i].firstrow, 32);
	printw("Total Bytes From SERVER ");
	displaybytesfromserver(i);
	dashrow(connections[i].firstrow+1);
	for (j=0; j<connections[i].ngroups; j++)
	    dashrow((j * 3) + connections[i].firstrow + 4);
    }

    refresh();
}

recalcgroups()
{
    register int ngroups;
    register int i;
    register int nrem;
    register int firstrow;

    if (nconnections) {
	ngroups = ((LINES - 2 * (nconnections + 1)) / nconnections) / 3;
	nrem = ((LINES - 2 * (nconnections + 1)) / nconnections) % 3;
    }

    for (i=0, firstrow=2; i<nconnections; i++) {
	connections[i].firstrow = firstrow;
	connections[i].ngroups = ngroups;
	firstrow += 2 + (ngroups * 3);
	if (i < nrem)
	    firstrow++;
	connections[i].currentpos = 0;
    }
}

/*
 * Sets up curses stuff and then opens the client listener socket.
 * Adds the client listener fd to activefds.
 */
init(argc, argv)
    int argc;
    char **argv;
{
    register int fd;
    char myhostname[32];
    struct hostent *hp;
    void *controlc();

    argv0 = *argv;

    for (--argc, ++argv; argc; --argc, ++argv) {
	if (!strncmp(*argv, "-c", 2)) {
	    if (argc == 1)
		usage();
	    --argc, ++argv;
	    clientport = atoi(*argv);
	}
	else if (!strncmp(*argv, "-s", 2)) {
	    if (argc == 1)
		usage();
	    --argc, ++argv;
	    serverport = atoi(*argv);
	}
	else if (!strncmp(*argv, "-tty", 4) && strlen(*argv) <= 6) {
	    char file[12];
	    (void) sprintf(file, "/dev/tty%s", *argv + 4);
	    if ((fd = open(file, O_RDWR, 0)) < 0)
		usage();
	    (void) dup2(fd, 0);
	    (void) dup2(fd, 1);
	    (void) close(fd);
	}
#ifdef	notdef
	else if (!strncmp(*argv, "-tokens", 8)) {
	    tokensflag = 1;
	}
#endif	/*notdef*/
	else if (!strncmp(*argv, "-terse", 6)) {
	    terseflag = 1;
	}
	else {
	    usage();
	}
    }

    initscr();
    LINES = LINES;
    COLS = COLS;
    (void) signal(SIGINT, controlc);
    noecho();
    nonl();
    scrollok(stdscr, FALSE);
    leaveok(stdscr, TRUE);
    clear();
    displayheader();
    dashrow(1);
    refresh();

    (void) gethostname(myhostname,sizeof myhostname);
    hp = gethostbyname(myhostname);
    myhostent = *hp;

    if ((fd = socket(AF_INET, SOCK_STREAM, IPPROTO_TCP)) > 0) {
	struct sockaddr_in addr;

	bzero((char *)&addr, sizeof(addr));
	addr.sin_family = AF_INET;
	addr.sin_port = htons(clientport);
	if (setsockopt(fd, SOL_SOCKET, SO_REUSEADDR, (char *)0, 0) < 0)
	    perrorexit("setsockopt");
	addr.sin_addr.s_addr = INADDR_ANY;
	if (bind(fd, (struct sockaddr *) &addr, sizeof addr) < 0)
	    perrorexit("bind");
	if (listen(fd, 3) < 0)
	    perrorexit("listen");
	ADDFD(fd);
	clientlistener = fd;
    }
}

fixupwindow()
{
    mvcur(0, COLS-1, LINES-1, 0);
    endwin();
}

void *
controlc()
{
    fixupwindow();
    takedownallconnections();
    (void) close(clientlistener);
    exit(0);
}

msgexit(fmt, a, b, c)
    char *fmt;
{
    fixupwindow();
    takedownallconnections();
    (void) close(clientlistener);
    (void) fprintf(stderr, fmt, a, b, c);
    exit(1);
}

perrorexit(s)
    char *s;
{
    fixupwindow();
    takedownallconnections();
    (void) close(clientlistener);
    perror(s);
    exit(1);
}

usage()
{
    (void) fprintf(stderr,
#ifdef	notdef
    "usage: %s [-c clientport#] [-s serverport#] [-ttyNN] [-tokens] [-terse]\n",
#endif	/*notdef*/
    "usage: %s [-c clientport#] [-s serverport#] [-ttyNN] [-terse]\n",
		    argv0);
    exit(1);
}
