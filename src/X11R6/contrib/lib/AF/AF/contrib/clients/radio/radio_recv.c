/* 
 * mcast_recv.c
 *
 * Receive multicast'ed audio data
 *
 */
/*              Copyright 1990, 1991, 1992, 1993 Digital Equipment Corporation
 *			 Maynard, Massachusetts.
 * 
 * Permission to use, copy, modify, distribute, and sell this software and its 
 * documentation for any purpose is hereby granted without fee, provided that 
 * the above copyright notice appear in all copies and that both that 
 * copyright notice and this permission notice appear in supporting 
 * documentation, and that the name of Digital not be used in advertising or 
 * publicity pertaining to distribution of the software without specific, 
 * written prior permission.  Digital makes no representations about the 
 * suitability of this software for any purpose.  It is provided "as is" 
 * without express or implied warranty.
 * 
 * DIGITAL DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING 
 * ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL 
 * DIGITAL BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY 
 * DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN 
 * AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF 
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
*/

/* options:
   -m address : specify multicast address to use
   -i ifname : specify interface name (default ln0)
 */

/* includes */
#include <stdio.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <fcntl.h>
#include <sys/uio.h>
#include <sys/time.h>
#include <net/if.h>
#include <net/pfilt.h>
#include <netinet/in.h>
#include <netinet/if_ether.h>

/* defines */
#define PACKETSIZE	1486
#define MADDRLEN	18

/* global variables */
char buf[PACKETSIZE];
char interface[IFNAMSIZ];
char dest_addr[MADDRLEN];
static char *optstring = "i:m:d";
int debug = 0;

/* functions */
int pf_setup(char *pname);

main(int argc, char **argv)
{
    extern char *optarg;
    extern int optind, opterr;
    int curopt,fd,length;
    char *cp;
    int i = 0;
    int mflag = 0;

    /* set default interface to ln0 */
    strcpy(interface,"ln0");
    while ((curopt = getopt(argc,argv,optstring)) != EOF) {
	switch (curopt) {
	    case 'i':
		if (strlen(optarg) > 0) {
		    strcpy(interface,optarg);
		} else {
		    fprintf(stderr,"%s: Invalid interface\n",argv[0]);
		    exit(1);
		}
		break;
	    case 'm':
		/* copy address into dest_addr */
		if (strlen(optarg) != 17) {
		    fprintf(stderr,"%s: Invalid multicast address\n",argv[0]);
		    exit(1);
		}
		cp = optarg;
		while ( *cp ) {
		    if ( *cp == '-' ) {
			cp++;
			continue;
		    }
		    else {
			sscanf(cp, "%2x", &dest_addr[i++]);
			cp += 2;
		    }
		}
		mflag = 1;
 		break;
	    case 'd':
		debug = 1;
		break;
	    default:
		fprintf(stderr,"%s: Invalid argument\n",argv[0]);
	}
    }

    /* if no multicast address in args, use default */
    if (mflag == 0) {
	dest_addr[0] = 0xab;
	dest_addr[1] = 0x00;
	dest_addr[2] = 0x00;
	dest_addr[3] = 0x12;
	dest_addr[4] = 0x34;
	dest_addr[5] = 0x00;
    }

    fd = pf_setup(argv[0]);
    while (1) {
	/* read input */
	length = pf_read(fd);
	if (debug)
	    fprintf(stderr,"successfully read %d bytes \n",length);
	/* send it to stdout */
	(void) fwrite(buf,1,length,stdout);
    }
}

/* add multicast address and setup the packetfilter */
int pf_setup(char *pname)
{
    int fd;
    int sock;
    struct ifreq *ifr;
    struct ifreq the_ifr;
    unsigned int queuelen;
    struct enfilter pf;
    u_short *fp = pf.enf_Filter;
    u_short *m_addr = (u_short *)dest_addr;

    ifr = &the_ifr;
    /* setup the packetfilter */
    if ((fd = pfopen(interface,O_RDWR)) < 0) {
	fprintf(stderr,"%s: Couldn't set up packetfilter.\n",pname);
	exit(1);
    }

    queuelen = 8;
    if (ioctl(fd, EIOCSETW, &queuelen) < 0) {
	perror("ioctl: set recv queue length");
	exit(1);
    }

    pf.enf_Priority = 128;	/* priority can be anywhere from 0 (lowest)
				   to 255 (highest) */
    *fp++ = ENF_PUSHWORD + 0;
    *fp++ = ENF_PUSHLIT;
    *fp++ = *m_addr++;
    *fp++ = ENF_CAND;
    *fp++ = ENF_PUSHWORD + 1;
    *fp++ = ENF_PUSHLIT;
    *fp++ = *m_addr++;
    *fp++ = ENF_CAND;
    *fp++ = ENF_PUSHWORD + 2;
    *fp++ = ENF_PUSHLIT;
    *fp++ = *m_addr++;
    *fp++ = ENF_EQ;
    pf.enf_FilterLen = 12;

    /* install the filter */
    if (ioctl(fd, EIOCSETF, &pf) < 0) {
	perror("ioctl: protocol filter");
	return(-1);
    }

    /* set up ifr */
    strcpy(ifr->ifr_name,interface);
    ioctl(fd,EIOCSETIF,ifr);

    ifr->ifr_addr.sa_family = AF_UNSPEC;
    bcopy(dest_addr, ifr->ifr_addr.sa_data, 6);

    /* open a socket, temporarily, to use for SIOC* ioctls */
    if ((sock = socket(AF_INET, SOCK_DGRAM, 0)) < 0) {
	perror("socket");
	exit(1);
    }
    if (ioctl(sock, SIOCADDMULTI, (struct ifreq *)ifr) < 0) {
	perror("SIOCADDMULTI");
	close(sock);
	exit(1);
    }

    return(fd);
}

int pf_read(int fd)
{
    struct iovec iov[2];
    struct ether_header ea;
    int cc;

    iov[0].iov_base = (caddr_t)&ea;
    iov[0].iov_len = sizeof(ea);
    iov[1].iov_base = (caddr_t)buf;
    iov[1].iov_len = PACKETSIZE;
    if ((cc = readv(fd, iov, 2)) < 0) {
	perror("pf_read");
	exit(1);
    }

    return(cc - sizeof(ea));
}
