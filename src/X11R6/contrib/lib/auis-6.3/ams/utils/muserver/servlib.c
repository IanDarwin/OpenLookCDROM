/* servlib.c -- basic TCP/IP stream socket server routines
 *
 *	(C) Copyright 1991 by Carnegie Mellon University
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

#include <andrewos.h>
#include <fcntl.h>
#include <stdio.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>

/* static address information */
static int servfd = 0;
static struct sockaddr_in server;

/* initialize the server with a given port (0 for dynamic port)
 * returns -1 on error, fd on success
 */
#ifdef __STDC__
int serv_init(int *port)
#else
int serv_init(port)
    int *port;
#endif
{
    int sock, len;

    /* create the socket */
    if ((sock = socket(AF_INET, SOCK_STREAM, 0)) < 0) {
	return (-1);
    }

    /* bind the address */
    server.sin_family = AF_INET;
    server.sin_addr.s_addr = htonl(INADDR_ANY);
    server.sin_port = htons(*port);
    if (bind(sock, (struct sockaddr *) &server, sizeof (server)) < 0) {
	(void) close(sock);
	return (-1);
    }

    /* get the port number if we don't know it */
    if (*port == 0) {
	len = sizeof (server);
	if (getsockname(sock, &server, &len) < 0) {
	    (void) close(sock);
	    return (-1);
	}
	*port = ntohs(server.sin_port);
    }

    /* put in non-blocking mode */
    if (fcntl(sock, F_SETFL, FNDELAY) < 0) {
	(void) close(sock);
	return (-1);
    }

    /* start listening */
    (void) listen(sock, 5);

    return (servfd = sock);
}

/* accept a connection
 * returns -1 on error, newfd on success
 */
int serv_accept()
{
    int len = sizeof (server);
    
    return (accept(servfd, (struct sockaddr *) &server, &len));
}

/* close a connection
 */
#ifdef __STDC__
void serv_close(int fd)
#else
void serv_close(fd)
    int fd;
#endif
{
    (void) close(fd);
}

/* shutdown the server
 */
void serv_shutdown()
{
    (void) close(servfd);
}

#ifdef TESTSERVER
#define MAXCLIENTS 1

static char noconn[] = "ERROR: Server is currently in use.  Please try Later.\n";

main()
{
    char buf[1024];
    int port = 0;
    int sfd;
    int numclient = 0;
    int cfd[MAXCLIENTS], efd;
    int maxfd, i, j, count;
    fd_set fds;

    /* start server */
    if ((maxfd = sfd = serv_init(&port)) < 0) {
	perror("serv_init");
	exit(1);
    }

    /* output port number */
    printf("port number: %d\n", port);

    /* wait for clients */
    do {
	/* check for file activity */
	FD_ZERO(&fds);
	if (sfd >= 0) {
	    FD_SET(sfd, &fds);
	}
	for (i = 0; i < numclient; i++) {
	    FD_SET(cfd[i], &fds);
	}
	if (select(maxfd + 1, &fds, (fd_set *) 0, (fd_set *) 0,
		   (struct timeval *) 0) <= 0) {
	    continue;
	}

	/* look for connection */
	if (sfd >= 0 && FD_ISSET(sfd, &fds) && (efd = serv_accept()) >= 0) {
	    if (numclient == MAXCLIENTS) {
		(void) write(efd, noconn, sizeof (noconn) - 1);
		serv_close(efd);
		continue;
	    }
	    cfd[numclient++] = efd;
	    if (efd > maxfd) maxfd = efd;
	    continue;
	}

	/* handle clients */
	for (i = 0; i < numclient; ++i) {
	    if (FD_ISSET(cfd[i], &fds)) {
		count = read(cfd[i], buf, sizeof (buf));
		if (count <= 0) {
		    serv_close(cfd[i]);
		    for (j = i; j < numclient; ++j) {
			cfd[j] = cfd[j + 1];
		    }
		    numclient--;
		} else {
		    if (*buf == 4) {
			printf("<shutdown>\n");
			serv_shutdown();
			sfd = -1;
			continue;
		    }
		    (void) write(1, buf, count);
		}
	    }
	}
    } while (sfd >= 0 || numclient > 0);

    exit(0);
}
#endif /* TESTSERVER */
