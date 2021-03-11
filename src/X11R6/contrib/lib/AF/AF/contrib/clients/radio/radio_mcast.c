/* 
 * mcast.c
 *
 * Multicast audio data
 *
 * Author: Alec Wolman
 *         DEC Cambridge Research Lab
 */
/*              Copyright 1990, 1991, 1992, 1993 Digital Equipment Corporation
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
   -r samplerate : attempt to keep up with specified samplerate (default 8000)
   -s samplesize : bits per sample (default 8)
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
#include <netinet/in.h>
#include <netinet/if_ether.h>

/* defines */
#define PACKETSIZE 1486
#define IFNAMELEN	10
#define MADDRLEN	18

/* global variables */
char buf[PACKETSIZE];
char interface[IFNAMELEN];
static char dest_addr[MADDRLEN];  /* statics are initialized to 0 */
struct timeval init_time;
static char *optstring="m:r:s:i:d";
int debug = 0;

/* functions */
int pf_write(int fd,int buf_len,char *buf,char *dest);

main(int argc, char **argv)
{
    extern char *optarg;
    extern int optind, opterr;
    int curopt,fd,length;
    int sample_rate = 8000;
    int sample_size = 8;
    int bps,rc;
    char *cp;
    int i = 0;
    int mflag = 0;

    while ((curopt = getopt(argc,argv,optstring)) != EOF) {
	switch (curopt) {
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
	    case 'r':
		sample_rate = atoi(optarg);
		break;
	    case 's':
		sample_size = atoi(optarg);
		break;
	    case 'i':
		if (strlen(optarg) > 0) {
		    strcpy(interface,optarg);
		} else {
		    fprintf(stderr,"%s: Invalid interface\n",argv[0]);
		    exit(1);
		}
		break;
	    case 'd':
		debug = 1;
		break;
	    default:
		fprintf(stderr,"%s: Invalid argument\n",argv[0]);
		exit(1);
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

    bps = (sample_rate * sample_size) / 8;

    /* setup the packetfilter */
    if ((fd = pfopen(interface,O_RDWR)) < 0) {
	fprintf(stderr,"%s: Couldn't set up packetfilter.\n",argv[0]);
	exit(1);
    }

    gettimeofday(&init_time, 0);
   
    while (1) {
	/* read input */
	length = fread(buf,1,PACKETSIZE,stdin);
	if (length <= 0)
		break;
	/* check timing */
	wait_if_necessary(bps,length);
	rc = pf_write(fd,length,buf,dest_addr);
	if (rc != length) {
	    fprintf(stderr,"radio_mcast: write failed, rc is %d\n",rc);
	}
	if (debug) {
	    fprintf(stderr,"wrote packet of size %d\n",rc);
	}
    }
    if (length == 0) {
	printf("radio_mcast: received EOF, exiting.\n");
    } else {
	perror("radio_mcast: fread failed");
    }
}

int pf_write(int fd,int buf_len,char *buf,char *dest)
{
    struct iovec iov[2];
    struct ether_header hdr;
    char *ptr;
    int rc;

    iov[0].iov_base = (caddr_t)&hdr;
    iov[0].iov_len = 14;
    iov[1].iov_base = (caddr_t)buf;
    iov[1].iov_len = buf_len;

    bcopy(dest,hdr.ether_dhost,6);
    hdr.ether_type = htons(buf_len);

    if ((rc = writev(fd,iov,2)) < 0) {
	perror("radio_mcast: writev failed");
	return(rc);
    }
    /* for successful writes, subtract ethernet header length */
    return(rc - 14);
}

/*
 * routine to wait between packets
 */
wait_if_necessary(int bytes_per_sec,int length)
{
    static unsigned long count=0; /* num bytes already sent */
    static unsigned long microsecs;

    struct timeval now;
    long sleep_time;		/* must be a signed number */

    count += length;
    gettimeofday(&now, 0);

    microsecs = ((now.tv_sec - init_time.tv_sec) * 1000000 + 
		 (now.tv_usec - init_time.tv_usec));
    /* do division in floating point, then convert to microseconds */
    sleep_time = (long)(((double) count / (double) bytes_per_sec) * 1000000)
	- microsecs;
    if (debug) {
	fprintf(stderr,"sleep time is %d\n",sleep_time);
    }
    if (sleep_time > 0) {
	/* reuse now */
	now.tv_sec = sleep_time / 1000000;
	now.tv_usec = sleep_time % 1000000;
	(void) select(0, NULL, NULL, NULL, &now);
    }
    /* every five minutes, update count and init_time, to avoid overflow
       problems */
    if (count > (300 * bytes_per_sec)) {
	count -= (300 * bytes_per_sec);
	init_time.tv_sec += 300;
    }
}
