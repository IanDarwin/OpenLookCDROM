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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/ams/utils/nntp/lib/RCS/tcpconn.c,v 1.11 1992/12/15 21:24:39 rr2b R6tape $";
#endif

#include <andrewos.h> /* sys/types.h */
#include <sys/socket.h>
#include <netinet/in.h>
#include <ctype.h>
#include <netdb.h>
#include <andyenv.h>
#include "tcpconn.h"

extern int	errno;

#ifdef RESOLVER_ENV
extern int h_errno;
#endif /* RESOLVER_ENV */

/*
** Take the name of an internet host in ASCII (this may either be its
** official host name or internet number (with or without enclosing
** backets [])), and return the internet address number in 32 bit quantity.
**
** returns FAIL for failure to find the host name in the local database,
** or for a bad internet address spec.
*/
u_long
name_to_address(host)
register char	*host;
{
	if (host == (char *)NULL)
		return(FAIL);

	/*
	** Is this an ASCII internet address? (either of [10.0.0.78] or
	** 10.0.0.78).
	*/
#ifdef RESOLVER_ENV
	h_errno = TRY_AGAIN;
#endif /* RESOLVER_ENV */
	if (*host == '[' || isdigit(*host)) {
		u_long	host_address;
		char	namebuf[128];
		register char	*cp = namebuf;

		/*
		** strip brackets [] or anything else we don't want.
		*/
		while(*host && cp < &namebuf[sizeof(namebuf)]) {
			if (isdigit(*host) || *host == '.')
				*cp++ = *host++;
			else
				host++;
		}

		if ((host_address = inet_addr(namebuf)) == FAIL)
			return(FAIL);	/* malformed internet address spec */
		return(host_address);
	} else {
		struct hostent	*hstp = gethostbyname(host);

		if (hstp == NULL)
			return(FAIL);	/* no such host */
		return(*(u_long *)hstp->h_addr);	/* we assume... */
	}
}

/*
** given a host name (either name or internet address) and service name
** (or port number) (both in ASCII), give us a TCP connection to the
** requested service at the requested host (or give us FAIL).
*/
get_tcp_conn(host,serv)
char	*host;
char	*serv;
{
	u_short	port;
	struct in_addr	host_address;

	if ((host_address.s_addr = name_to_address(host)) == FAIL) {
#ifdef RESOLVER_ENV
		switch (h_errno) {
		    case HOST_NOT_FOUND:
		    case NO_ADDRESS:
			return(NOHOST);
		    case TRY_AGAIN:
		    default:
			return(FAIL);
		}
#else /* RESOLVER_ENV */
		return(NOHOST);
#endif /* RESOLVER_ENV */
	}

	if (isdigit(*serv)) {
		port = htons((u_short)(atoi(serv)));
	} else {
		struct servent	*srvp = getservbyname(serv, "tcp");
		if (srvp == NULL) {

			return(NOSERVICE);
		}
		port = (u_short) srvp->s_port;
	}

	return(mkconn(&host_address, port, IPPROTO_TCP, SOCK_STREAM));
}

/*
** create a socket and connect it to a remote host on the specified
** port by the specified protocol. Return FAIL if something goes
** wrong somewhere. Since these are exclusively system calls,
** errno will have the correct error in it.
*/
mkconn(host_address, port, protocol, proto_type)
struct in_addr	*host_address;
u_short	port;
int	protocol, proto_type;
{
	register int	skt;
	struct sockaddr_in	sadr;

	sadr.sin_family = (u_short)AF_INET;	/* Only internet for now */
	sadr.sin_addr.s_addr = host_address->s_addr;
	sadr.sin_port = port;


	if ((skt = socket(AF_INET, proto_type, protocol)) < 0) {
#ifdef DEBUG
		printf("mkconn: socketfailed: %d %d %d \n",port,
		    proto_type,protocol);
#endif /* DEBUG */
		return(FAIL);
	}

	if (connect(skt, &sadr, sizeof(sadr)) < 0) {
		int	save = errno;
#ifdef DEBUG
		printf("mkconn: connect: %d %d %d (%d)\n",port,proto_type,
		    protocol,save);
#endif /* DEBUG */
		close(skt);
		errno = save;
		return(FAIL);
	}
	return(skt);
}
