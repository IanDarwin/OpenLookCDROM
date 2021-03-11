/* mtst.c -- simple test routine for master update server
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
 */

#include <andrewos.h>
#include <stdio.h>
#include <ctype.h>
#include <errno.h>
#include <fcntl.h>
#include <signal.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>
#include "config.h"

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
    if ((hp = gethostbyname(host)) == (struct hostent *) NULL) {
	return (-1);
    }
    if ((sock = socket(AF_INET, SOCK_STREAM, 0)) < 0) {
	return (-1);
    }
    server.sin_family = AF_INET;
    bcopy(hp->h_addr, &server.sin_addr, hp->h_length);
    server.sin_port = htons(port);

    /* put in non-blocking mode */
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
	close(sock);
	return (-1);
    }

    return (sock);
}

void usage()
{
    puts("usage: mtst [-p] [-f#] <hostname>");
    exit(1);
}

main(argc, argv)
    int argc;
    char **argv;
{
    static char buf[1024];
    struct timeval t1, t2, t3;
    char *scan;
    int (*tmpfunc)();
    int fd, diff, diff2, i;
    int printopt = 0;
    FILE *in, *out;

    while (*++argv != (char *) NULL && **argv == '-') {
	switch ((*argv)[1]) {
	    case 'p':
		printopt = 1;
		break;
	    case 'f':
		for (i = atoi((*argv) + 2); fork(); --i);
		break;
	    default:
		usage();
	}
    }
    if (argv[1] != (char *) NULL) usage();
    
    (void) strcpy(buf, getenv("HOME"));
    (void) strcat(buf, "/.AMS.prof");
    if ((in = fopen(buf, "r")) == (FILE *) NULL) {
	perror(buf);
	exit(1);
    }
    gettimeofday(&t1, 0);
    fd = cli_connect(*argv, SERVER_PORT, SERV_TIMEOUT);
    if (fd < 0) {
	perror("cli_connect");
	exit(1);
    }
    if ((out = fdopen(fd, "w")) == (FILE *) NULL) {
	perror("fdopen");
	exit(1);
    }
    tmpfunc = signal(SIGPIPE, SIG_IGN);
    while (fgets(buf, sizeof (buf), in) != (char *) NULL) {
	for (scan = buf; *scan && !isspace(*scan); ++scan);
	*scan++ = '\n';
	*scan = '\0';
	fputs(buf, out);
    }
    (void) fclose(in);
    fputs("$q\n", out);
    if (fflush(out) == EOF) {
	perror("fflush");
	(void) close(out);
	exit(1);
    }
    signal(SIGPIPE, tmpfunc);
    if ((in = fdopen(fd, "r")) == (FILE *) NULL) {
	perror("fdopen");
	exit(1);
    }

    /* put in blocking mode */
    if (fcntl(fd, F_SETFL, 0) < 0) {
	perror("fcntl");
	(void) close(fd);
	exit(1);
    }

    if (printopt) fputs("Results:\n", stdout);
    gettimeofday(&t2, 0);
    while (fgets(buf, sizeof (buf), in) != (char *) NULL) {
	if (printopt) fputs(buf, stdout);
    }
    (void) close(fd);
    gettimeofday(&t3, 0);
    if (printopt) fputs("--------\n", stdout);
    diff = (t3.tv_sec - t1.tv_sec) * 1000000 + (t3.tv_usec - t1.tv_usec);
    diff2 = (t3.tv_sec - t2.tv_sec) * 1000000 + (t3.tv_usec - t2.tv_usec);
    printf("connect time: %d.%06d, response time: %d.%06d\n",
	   diff / 1000000, diff % 1000000, diff2 / 1000000, diff2 % 1000000);
}
