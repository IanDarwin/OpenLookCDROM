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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/overhead/mail/cmd/RCS/decodeid.c,v 1.12 1992/12/15 21:04:17 rr2b R6tape $";
#endif

#include <stdio.h>
#include <mailconf.h>
#include <ctype.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#ifdef hpux
extern char *inet_ntoa();
#else /* hpux */
#include <arpa/inet.h>
#endif /* hpux */
#include <netdb.h>
#include <andrewos.h> /* sys/time.h */

main(argc, argv)
int argc;
unsigned char **argv;
{
    int i, j;
    unsigned long foo, bar, baz;
    struct in_addr A;
    unsigned char *cp;
    unsigned char Addr[5];
    unsigned char ArgCopy[20];
    struct hostent *Host;
    
    if (argc > 1) {
	for (i=1; i<argc; ++i) {
	    bzero(ArgCopy, sizeof(ArgCopy));
	    strncpy((char *) ArgCopy, (char *) argv[i], sizeof(ArgCopy));
	    ArgCopy[sizeof(ArgCopy)-1] = '\0';
	    cp=ArgCopy;
	    if (*cp == '+') ++cp;
	    foo = conv64tolong(cp);
	    bar = conv64tolong(cp+6);
	    bar = htonl(bar);
	    baz = conv64tolong(cp+12);
	    printf("``%s'': generated %s from ", cp, NiceTime(foo));
	    strncpy((char *) Addr, (char *) &bar, 4);
	    Addr[4] = '\0';
	    Host = NULL;
	    if (Addr[0] != '\0') Host = gethostbyaddr(Addr, 4, AF_INET);
	    if (Host != NULL) {
		fputs(Host->h_name, stdout);
		fputs(" (", stdout);
	    };
	    fputc('[', stdout);
	    A.s_addr = bar;
	    fputs(inet_ntoa(A), stdout);
	    fputc(']', stdout);
	    if (Host != NULL) fputc(')', stdout);
	    j=strlen((char *) cp);
	    if (j <= 13) {
		printf(", ctr (mod 64) of %d.\n", baz);
	    } else {
		printf(", pid %d, ctr (mod 256) of %d.\n",
		       (baz >> 16) & 0xFFFF, (baz >> 8) & 0xFF);
	    }
	}
	exit(0);
    }
    else {
	printf("usage: %s id [id] [id]...\n", argv[0]);
	exit(2);
    }
}
