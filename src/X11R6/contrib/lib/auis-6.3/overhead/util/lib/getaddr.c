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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/overhead/util/lib/RCS/getaddr.c,v 2.12 1993/08/25 20:35:21 susan Exp $";
#endif


 

/* getaddr -- get our internet address */

#include <andrewos.h>		/* types.h */
#include <sys/socket.h>
#include <net/if.h>
#include <netinet/in.h>
#include <sys/ioctl.h>
#ifdef SOLARIS /* (was SY_U54... If we're SVR4 we need this. */
#include <sys/sockio.h>
#endif

#if defined(_IBMR2) && !defined(AF_LINK)
/* For binary compatibility with AIX 3.2... */
#define	AF_LINK		18		/* Link layer interface */
#endif

#define NIFS		6

/* Return our internet address as a long in network byte order.  Returns zero if it can't find one. */
unsigned long getaddr ()
{
    int     s;
    int     i, len;
    struct ifconf   ifc;
    struct ifreq    ifs[NIFS];
    struct sockaddr_in *a;

    s = socket(AF_INET, SOCK_DGRAM, 0);
    if (s < 0)
	return 0;
    ifc.ifc_len = sizeof(ifs);
    ifc.ifc_buf = (caddr_t) ifs;
    i = ioctl(s, SIOCGIFCONF, &ifc);
    close(s);
    if (i < 0)
	return 0;
    len = ifc.ifc_len / sizeof(struct ifreq);
    if (len > NIFS)
	len = NIFS;
    for (i = 0; i < len; ++i) {
	a = (struct sockaddr_in *) &ifs[i].ifr_addr;
#ifdef _IBMR2
	if (a->sin_family == AF_LINK)
	    continue;	/* Skip <link> addresses */
#endif
	if (a->sin_addr.s_addr != 0 && strcmp(ifs[i].ifr_name, "lo0") != 0)
	    return a->sin_addr.s_addr;
    }
    return 0;
}

#ifdef TESTINGONLYTESTING
#include <stdio.h>
#include <arpa/inet.h>
main(argc,argv)
int argc;
char *argv[];
{
  struct in_addr a;

  a.s_addr = getaddr();
  printf("%ul == %s\n", a.s_addr, inet_ntoa(a));
}
#endif /* TESTINGONLYTESTING */
