static char *servers_rcsid = "$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/ams/utils/nntp/lib/RCS/servers.c,v 1.1 1993/06/22 19:28:38 Zarf Exp $";

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

/*
 * NNTP server routines.
 *
 * @(#)clients.c	1.2	(Berkeley) 4/13/86
 */

#include <stdio.h>
#include <ctype.h>
#include <andrewos.h> /* sys/types.h */
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>
#include <errno.h>
#include <errprntf.h>
#include "respcode.h"

extern FILE *ser_rd_fp;
extern FILE *ser_wr_fp;
int server_socket;
extern char *progname;
extern int errno;

/*
 * server_create  Set ourselves up to be an NNTP server
 *
 *	Returns:	-1 on error, socket otherwise.
 *
 *	Side effects:	Binds a socket to the server listening port
 */

server_create()
{
	int s;
	struct	sockaddr_in sin;
	struct	servent *getservbyname(), *sp;

	if ((sp = getservbyname("nntp", "tcp")) ==  NULL) {
		n_errprintf(progname,ERR_CRITICAL,0,0, 
		    "nntp/tcp: Unknown service.");
		return (-1);
	}

	bzero((char *) &sin, sizeof(sin));
	sin.sin_family = AF_INET;
	sin.sin_port = sp->s_port;

	if ((s = socket(AF_INET, SOCK_STREAM, 0)) < 0) { /* Get the socket */
		perror("socket");
		return (-1);
	}

	if (bind(s, &sin, sizeof(sin)) < 0) { /* Bind to port */
		n_errprintf(progname,ERR_CRITICAL,0,0, 
		    "unable to bind to nntp service");
		return (-1);
	}
	if (listen(s, 1) < 0) {
		perror("listen");
		return (-1);
	}

	server_socket = s;

	return 0;
}

/*
 * server_accept -- accept a connection from the news server.
 *
 *	Parameters:	"machine" is the machine the server is running on.
 *
 *	Returns:	0 if all is ok, else -1 on error.
 *
 *	Side effects:	Server connects to us.
 *
 *	Errors:		Printed via perror.
 */
server_accept(machine)
char	*machine;
{
	int	s;
	struct	sockaddr_in sin;
	int sin_size;
	struct	hostent *gethostbyname(), *hp;

	if ((hp = gethostbyname(machine)) == NULL) {
		n_errprintf(progname,ERR_CRITICAL,0,0, 
	        "%s: Unknown host.", machine);
		return (-1);
	}

	for (;;) {
	    sin_size = sizeof(sin);
	    s = accept(server_socket, &sin, &sin_size);
	    if (s < 0) {
		if (errno == EINTR) continue;
		perror("accept");
	        return (-1);
	    }
	    
	    if ((ser_wr_fp = fdopen(s, "w")) == NULL) {
		(void) close(s); 
		perror("server_accept: fdopen #1");
		return (-1);
	    }

	    if (bcmp(hp->h_addr, (char *) &sin.sin_addr, hp->h_length)) {
		put_server("502 Go away");
		close(s);
		continue;
	    }

	    s = dup(s);
	    if ((ser_rd_fp = fdopen(s, "r")) == NULL) {
		(void) close(s);
		(void) fclose(ser_wr_fp);
		perror("server_accept: fdopen #2");
		return (-1);
	    }

	    put_server("200 Hi there");
	    return 0;
	}
}

/*
 * server_disconnect -- close the connection to the client
 *
 *	Parameters:	None.
 *
 *	Returns:	Nothing.
 *
 *	Side effects:	Closes the connection with the server.
 *			You can't use "put_server" or "get_server"
 *			after this routine is called.
 */

void
server_disconnect()
{
	if (vfclose(ser_wr_fp) != 0) {
	    n_errprintf(progname, ERR_CRITICAL, 0, 0,
		"Error in closing write-side socket descriptor.");
	    fclose(ser_wr_fp);
	}
	if (vfclose(ser_rd_fp) != 0)  {
	    n_errprintf(progname, ERR_CRITICAL, 0, 0, "Error in closing read-side socket");
	    fclose(ser_rd_fp);
	}
}
