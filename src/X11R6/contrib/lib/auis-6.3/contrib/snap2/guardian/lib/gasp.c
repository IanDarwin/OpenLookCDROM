
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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/contrib/snap2/guardian/lib/RCS/gasp.c,v 2.10 1993/01/15 19:07:55 gk5g Exp $";
#endif

/*
		gasp.c -- Message server guardian interface routines.


	Written: 06 November 1985

*/

#ifdef _IBMR2
#ifndef _BSD
#define _BSD 43
#endif
#endif

#include <stdio.h>
#include <netdb.h>
#include <andrewos.h> /* sys/types.h */
#include <sys/socket.h>
#include <netinet/in.h>

#define VICE
#include <gasp.h>
#include <snap.h>

typedef char KEY[SNAP_KEYLEN];

#define NIL	0

static struct sockaddr_in gaddr;
static int clientfd;

/*
   Start up Guardian-Server interface:

	(i)	Read key from guardian
	(ii)	Initialize SNAP for server
	(iii)	Set up for talking to guardian later
	(iv)	Set *client to point to client name
	(v)	Set *fd to be client fd
	(vi)	Set *authenticated: 0 - unauthenticated
				    1 - authenticated
*/

int GASP_ServerInit(argc, argv, parms, client, fd, authenticated)
    int argc;
    char *argv[];
    SNAP_CPARMS *parms;
    char **client;
    int *fd, *authenticated;
{
    char key[SNAP_KEYLEN], host[512];
    int keyfd, code;
    register struct servent *sv;
    register struct hostent *ho;

    /*
       Check on # arguments & process:

		<server> <client> <key fd> <client fd> [-a]
    */
    if (argc != 4 && argc != 5) return GASP_BAD_ARGS;
    *client = argv[1];
    keyfd = atoi(argv[2]);
    clientfd = atoi(argv[3]);
    *fd = clientfd;
    *authenticated = (argc == 5);

    /* Read key */
    if (read(keyfd, key, SNAP_KEYLEN) != SNAP_KEYLEN) {
	close(keyfd);
	return GASP_NO_KEY;
    }
    close(keyfd);

    /* Got key, now initialize SNAP */
    code = SNAP_ServerInit(clientfd, parms, key);
    if (code != SNAP_SUCCESS) return code;

    /* Now try to find port for sending count requests to Guardian */
    sv = getservbyname("snap.guardian01", "udp");
    if (sv == NIL) return GASP_NO_GUARDIAN;

    if (gethostname(host, sizeof host) < 0)
	return GASP_NO_HOSTNAME;
    ho = gethostbyname(host);
    if (ho == NIL) return GASP_NO_HOST_ENTRY;

    /* Set up address of guardian */
    bzero(&gaddr, sizeof gaddr);
    gaddr.sin_family = AF_INET;
    gaddr.sin_port = sv -> s_port;
    bcopy(ho->h_addr, &gaddr.sin_addr, ho->h_length);

    return SNAP_SUCCESS;
}

int GASP_ServerTerm()
{
    close(clientfd);
    return SNAP_ServerTerm();
}

int GASP_Count(count)
    int count;
{
    SERVER_PACKET packet;

    packet.type = COUNT_RQ;
    packet.count = count;
    return sendto(clientfd,
		  &packet,
		  SERVER_PACKET_SIZE,
		  0,
		  &gaddr, sizeof gaddr);
}

#if 0

/* Return null-terminated string describing error */

char *GASP_Error(errno)
    register int errno;
{
    static char *errors[] = {
	/*1*/	"(Unused)",
	/*2*/	"Unknown service requested",
	/*3*/	"Can't create server",
	/*4*/	"Too many clients for server",
	/*5*/	"Authentication failed",
	/*6*/	"Max server limit reached",
	/*7*/	"(Unused)",
	/*8*/	"System call failed (pipe)",
	/*9*/	"System call failed (fork)",
	/*10*/	"System call failed (bind)",
	/*11*/	"Client name is too long",
	/*12*/	"Guardian not accepting remote requests",
	/*13*/	"Guardian not accepting unauthenticated requests",
	/*14*/	"System call failed (setuid)",
	/*15*/	"Temporary failure due to a file server",
	/*16*/	"Format of authorization string incorrect",
	/*17*/	"RPC initialization failed",
	/*18*/	"RPC returned an impossible value",
	/*19*/	"User barred from this machine",
	/*20*/	"User not in /etc/passwd",
	/*21*/	"Authentication mismatch in this request",
	/*22*/	"No guest account on this cell",
	/*23*/	"Code to service authentication request not compiled"
    };
#define NERRORS	(sizeof(errors) / sizeof(errors[0]))

    if (errno<1 || errno>=NERRORS)
	return "Unknown error";
    else
	return errors[errno-1];
}
#endif /* 0 */

