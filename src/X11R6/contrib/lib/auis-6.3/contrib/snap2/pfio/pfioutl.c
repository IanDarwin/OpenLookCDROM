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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/contrib/snap2/pfio/RCS/pfioutl.c,v 1.14 1993/10/10 17:54:35 gk5g Exp $";
#endif

#include <andrewos.h>
#include <pfiou.h>

void pfio_with_timeout(dowrite,fd,buf,len,timeout)
int dowrite;
int fd;
char *buf;
int len;
int timeout;
{
  register int rc;
  do {
    if(dowrite)
    	rc=write(fd,buf,len);	/*write what we can*/
    else
      rc=read(fd,buf,len);	/*read what we can*/
    if(rc != -1) {
      len-=rc;
      buf+=rc;
      if(len==0)return;
    }
    else if(errno!=EWOULDBLOCK)
	er_ucall(rc,("io"));	/*make sure its not an error code*/
 }while(timeout>time(0L));
 rl_error("io timed out, dowrite=%d",dowrite);
}

void rl_parerr();

/*
 * send an error message or an console message
 */
void send_guar_news(serv_port,kind,isfinal,news)
int serv_port;
int kind;
int isfinal;
char *news;
{
  PFM outpar;
  int newslen=strlen(news);
  char opack[5000];
  outpar.initial=TO_pfstr(opack+IHEAD_LEN);
  outpar.buf=outpar.initial;
  outpar.limit=TO_pfstr(opack+sizeof(opack)-1);
  outpar.parerr=rl_parerr;
  PFpack(
	 &outpar,
	  LS_aloc(newslen+30),	/*size of out buffer*/
	  /*opcode to send*/
	  LS_msend_num(LS_int8,kind), /*this is an error, (not news)*/
	  LS_msend_num(LS_int8,isfinal), /*end of connection*/
	  LS_msend_rstr(newslen,news),	/*string to print*/
	  LS_dropoff);
  outpar.initial=TO_pfstr(opack);
  PF_dropoff(&outpar);
  pfio_with_timeout(
		  TRUE,
		  serv_port,
		  opack,
		  BYTES_IN_BUF(outpar),
		  time(0L)+10);
}


void get_foreign_addr(fd,sin)
int fd;
register struct sockaddr_in *sin;
{
    int sin_len=sizeof(*sin);
    er_ucall(getpeername(fd,sin,&sin_len),("getpeername"));
}

void set_no_block(fd)
int fd;
{
 int noblock=FNDELAY;
#ifdef SOMEDAY
 er_ucall(fcntl(fd,F_SETFL,&noblock,sizeof(noblock)),("fcntl"));
#endif
 er_ucall(fcntl(fd,F_SETFL,noblock),("fcntl"));
}

int new_socket(port)
int port;
{
    struct sockaddr_in sin;
    int yes_reuse=1;
    register int result;
    er_ucall((result=socket(AF_INET,SOCK_STREAM,0)),("socket"));
    er_ucall(setsockopt(result,SOL_SOCKET,SO_REUSEADDR,&yes_reuse,sizeof(yes_reuse)),("setsockopt"));

    bzero(&sin,sizeof(sin));
    sin.sin_family=AF_INET;
    sin.sin_port=htons(port);
    sin.sin_addr.s_addr=INADDR_ANY;
    er_ucall(bind(result,&sin,sizeof(sin)),("bind"));
    return result;
}

/*
 * Get internet address from kernel, zero on error.
 * code from ddp+
 */
long get_netaddr(sock)
    long sock;
{
    struct ifconf ifc;
#ifndef NETWORKS
#define NETWORKS (30)
#endif
    struct ifreq conf[NETWORKS];
    struct sockaddr_in *sa_struct;
    int i;
    int interfaces;             /* number of interfaces returned by ioctl */

    ifc.ifc_len = sizeof(conf);
    ifc.ifc_buf = (caddr_t) conf;
    if (ioctl(sock, SIOCGIFCONF, (char *)&ifc) < 0)
        return (0);

    interfaces = ifc.ifc_len / sizeof(struct ifreq);
    for (i = 0; i < interfaces; i++) {
        sa_struct = (struct sockaddr_in *) &conf[i].ifr_addr;
        if (sa_struct->sin_addr.s_addr != 0 &&
                    strcmp(conf[i].ifr_name, "lo0") != 0)
        return (sa_struct->sin_addr.s_addr);
    }
    return (0);
}

void get_local_addr(fd,sin)
register int fd;
register struct sockaddr_in *sin;
{
    int sock_len=sizeof(*sin);
    er_ucall(getsockname(fd,sin,&sock_len),("getsockname"));
}

