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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/ams/libs/ms/RCS/logit.c,v 1.6 1993/01/19 23:40:00 gk5g Exp $";
#endif

#ifdef _IBMR2
#ifndef _BSD
#define _BSD 43
#endif
#endif

#include <stdio.h>
#include <andrewos.h>                  /* sys/file.h sys/types.h */
#include <sys/socket.h>
#include <sys/param.h>
#include <netinet/in.h>
#include <netdb.h>

#ifdef CMU_ENV

#define LOGIT_VERS "1.1"	/*version of logit info*/
#define AMS_LOG_PORT (903)
#ifndef LOGIT_DELAY
#define LOGIT_DELAY (14*60)	/*log every this many seconds*/
#endif

#ifndef AMS_LOG_LOCATION
void logging_hook(s)
char *s;
{
}
#else

static struct {
    int nsock;
    int send_errors;
    struct sockaddr_in logto;
    char *out_buf;
    char *out_limit;
} ams_log_gl;

static void ams_log_add(s)
char *s;
{
    char ch;
    if ((ams_log_gl.out_limit-ams_log_gl.out_buf) < 0)
      return;
    if (s == 0) {
	*ams_log_gl.out_buf++ = '?';
	*ams_log_gl.out_buf++ = ',';
	return;
    }
    while ((ams_log_gl.out_limit-ams_log_gl.out_buf) > 0) {
	ch = *s++;
	ch &= 0x7f;
	if (ch == 0)
	  break;
	if ((ch<' ') || (ch>=0x7f) || (ch==','))
	  ch='?';
	*ams_log_gl.out_buf++ = ch;
    }
    *ams_log_gl.out_buf++ = ',';
}

static void ams_log_add_num(anum)
long anum;
{
    char buf[30];
    sprintf(buf, "%ld", anum);
    ams_log_add(buf);
}

/*
 * sin_addr.s_addr is in network byte order so this
 * macro will work on a machine with any byte ordering
 */
#define AMS_LOG_IP(xx1,xx2,xx3,xx4) \
  (((xx1)<<24)| \
   ((xx2)<<16)| \
   ((xx3)<< 8)| \
   ((xx4)    ))

static void ams_log_init()
{
    struct sockaddr_in client;
    /*
     * some systems have extra fields in sockaddr_in
     * that must be zero
     */
    memset(&client, 0, sizeof(client));
    ams_log_gl.logto.sin_addr.s_addr = htonl(AMS_LOG_LOCATION);
    ams_log_gl.logto.sin_family = AF_INET;
    ams_log_gl.logto.sin_port = htons(AMS_LOG_PORT);
    
    ams_log_gl.nsock = socket (AF_INET,SOCK_DGRAM,0);
    if (ams_log_gl.nsock < 0)
      return;
    client.sin_addr.s_addr = INADDR_ANY;
    client.sin_family = AF_INET;
    client.sin_port = 0; /*pick any socket for local port*/
    if (bind(ams_log_gl.nsock,&client,sizeof client) < 0)
      ams_log_gl.nsock = (-1);
}

void logging_hook()
{
    char buf[1024];
    extern char *CUI_ClientVersion;
    static long next_send_time=0;
    static long first_time=0;
    static long N=0;
    long now;
    if (CUI_ClientVersion == 0)
      return;
    if (ams_log_gl.nsock == 0)
      ams_log_init();
    if (ams_log_gl.nsock < 0)
      return;
    now = time(0L);
    if (next_send_time > now)
      return;
    if (first_time == 0)
      first_time = now;
    N++;
    next_send_time = now+LOGIT_DELAY;
    ams_log_gl.out_buf = buf;
    ams_log_gl.out_limit = buf+sizeof(buf)-10;
    ams_log_add(LOGIT_VERS);
    ams_log_add(OPSYSNAME);
    ams_log_add(CUI_ClientVersion);
    ams_log_add_num(getpid());
    ams_log_add_num(getuid());
    ams_log_add_num(N);
    ams_log_add_num(first_time);
    ams_log_add(getenv("USER"));
    *(ams_log_gl.out_buf-1) = 0;
    if (sendto(ams_log_gl.nsock,buf,strlen(buf),0,
	      &ams_log_gl.logto,sizeof(ams_log_gl.logto)) < 0)
      ams_log_gl.send_errors++;
}
#endif /* AMS_LOG_LOCATION */
#endif /* CMU_ENV */
