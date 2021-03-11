/*
 * Copyright 1993 Network Computing Devices, Inc.
 *
 * Permission to use, copy, modify, distribute, and sell this software and
 * its documentation for any purpose is hereby granted without fee, provided
 * that the above copyright notice appear in all copies and that both that
 * copyright notice and this permission notice appear in supporting
 * documentation, and that the name Network Computing Devices, Inc. not be
 * used in advertising or publicity pertaining to distribution of this 
 * software without specific, written prior permission.
 * 
 * THIS SOFTWARE IS PROVIDED `AS-IS'.  NETWORK COMPUTING DEVICES, INC.,
 * DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING WITHOUT
 * LIMITATION ALL IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS FOR A
 * PARTICULAR PURPOSE, OR NONINFRINGEMENT.  IN NO EVENT SHALL NETWORK
 * COMPUTING DEVICES, INC., BE LIABLE FOR ANY DAMAGES WHATSOEVER, INCLUDING
 * SPECIAL, INCIDENTAL OR CONSEQUENTIAL DAMAGES, INCLUDING LOSS OF USE, DATA,
 * OR PROFITS, EVEN IF ADVISED OF THE POSSIBILITY THEREOF, AND REGARDLESS OF
 * WHETHER IN AN ACTION IN CONTRACT, TORT OR NEGLIGENCE, ARISING OUT OF OR IN
 * CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 * 
 * $NCDId: @(#)connection.c,v 1.7 1994/06/01 19:30:43 greg Exp $
 */
/***********************************************************
Some portions derived from:

Copyright 1987, 1989 by Digital Equipment Corporation, Maynard, Massachusetts,
and the Massachusetts Institute of Technology, Cambridge, Massachusetts.

                        All Rights Reserved

Permission to use, copy, modify, and distribute this software and its 
documentation for any purpose and without fee is hereby granted, 
provided that the above copyright notice appear in all copies and that
both that copyright notice and this permission notice appear in 
supporting documentation, and that the names of Digital or MIT not be
used in advertising or publicity pertaining to distribution of the
software without specific, written prior permission.  

DIGITAL DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING
ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL
DIGITAL BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR
ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,
WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION,
ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS
SOFTWARE.

******************************************************************/
/*****************************************************************
 *  Stuff to create connections --- OS dependent
 *
 *      EstablishNewConnections, CreateWellKnownSockets, ResetWellKnownSockets,
 *      CloseDownConnection, CheckConnections, AddEnabledDevice,
 *	RemoveEnabledDevice, OnlyListToOneClient,
 *      ListenToAllClients,
 *
 *      (WaitForSomething is in its own file)
 *
 *      In this implementation, a client socket table is not kept.
 *      Instead, what would be the index into the table is just the
 *      file descriptor of the socket.  This won't work for if the
 *      socket ids aren't small nums (0 - 2^8)
 *
 *****************************************************************/

#include <audio/audio.h>
#include <audio/Aproto.h>
#ifndef _MINIX
#include <sys/param.h>
#endif
#include <errno.h>
#include <audio/Aos.h>
#if !defined(AMOEBA) && !defined(_MINIX)
#ifdef ESIX
#include <lan/socket.h>
#else
#include <sys/socket.h>
#endif
#endif

#include <signal.h>
#include <setjmp.h>

#ifdef hpux
#include <sys/utsname.h>
#include <sys/ioctl.h>
#endif

#ifdef SVR4
#include <sys/resource.h>
#endif

#ifdef AIXV3
#include <sys/ioctl.h>
#endif

#ifdef TCPCONN
#ifndef _MINIX
# include <netinet/in.h>
# ifndef hpux
#  ifdef apollo
#   ifndef NO_TCP_H
#    include <netinet/tcp.h>
#   endif
#  else
#   include <netinet/tcp.h>
#  endif
# endif
#else  /* _MINIX */
#include <sys/ioctl.h>
#include <stdlib.h>
#include <net/netlib.h>
#include <net/gen/in.h>
#include <net/gen/tcp.h>
#include <net/gen/tcp_io.h>
#endif /* _MINIX */
#endif

#if defined(SO_DONTLINGER) && defined(SO_LINGER)
#undef SO_DONTLINGER
#endif

#ifdef UNIXCONN
/*
 * sites should be careful to have separate /tmp directories for diskless nodes
 */
#include <sys/un.h>
#include <sys/stat.h>
static int unixDomainConnection = -1;
#endif

#include <stdio.h>
#if !defined(AMOEBA) && !defined(_MINIX)
#include <sys/uio.h>
#endif
#include "osstruct.h"
#include "osdep.h"
#include "opaque.h"
#include "dixstruct.h"

#if defined(SYSV) || defined(SVR4)
#define signal sigset
#endif

#ifdef DNETCONN
#include <netdnet/dn.h>
#endif /* DNETCONN */

#ifndef SCO
#define	_OSWriteV	writev
#endif /* SCO */

#ifdef SIGNALRETURNSINT
#define SIGVAL int
#else
#define SIGVAL void
#endif

typedef long CCID;      /* mask of indices into client socket table */

#ifndef X_UNIX_PATH
#ifdef hpux
#define X_UNIX_DIR	"/usr/spool/sockets/audio"
#define X_UNIX_PATH	"/usr/spool/sockets/audio/"
#define OLD_UNIX_DIR	"/tmp/.sockets"
#else
#define X_UNIX_DIR	"/tmp/.sockets"
#define X_UNIX_PATH	"/tmp/.sockets/audio"
#endif
#endif

extern Bool	allow_any_host;	/* simplistic access control */

#ifdef SERVER_LOCALCONN
#include <sys/stream.h>
#include <sys/stropts.h>
#include <sys/utsname.h>
#ifndef UNIXCONN
#include <sys/stat.h>
#endif
#ifdef SVR4
static int NstrFd = -1;
#endif
static int ptsFd = -1;
static int spxFd = -1;
static int xsFd = -1;
static long AllStreams[mskcnt]; /* keep up, whos on a STREAMS pipe */
/*
 * Why not use the same path as for UNIXCONN ?? 
 * Diskless workstations may have a common /tmp directory. This may cause much
 * trouble. Since every workstation MUST have it's own /dev, so lets use this
 * directory.
 */
#define AUDIO_STREAMS_DIR "/dev/Au"
#define AUDIO_STREAMS_PATH "/dev/Au/server."
#ifdef SVR4
# define AUDIO_NSTREAMS_PATH "/dev/Au/Nserver."
#endif
#define AUDIO_XSIGHT_PATH "/dev/Au"
#if defined(SVR4_ACP) && defined(UNIXCONN)
# define AUDIO_ISC_DIR "/tmp/.ISC-unix"
# define AUDIO_ISC_PATH "/tmp/.ISC-unix/Au"
#endif
#endif /* SERVER_LOCALCONN */

extern char *display;		/* The display number */
#ifndef AMOEBA
int lastfdesc;			/* maximum file descriptor */

#ifndef _MINIX
long WellKnownConnections;	/* Listener mask */
long EnabledDevices[mskcnt];	/* mask for input devices that are on */
long AllSockets[mskcnt];	/* select on this */
long AllClients[mskcnt];	/* available clients */
long LastSelectMask[mskcnt];	/* mask returned from last select call */
long ClientsWithInput[mskcnt];	/* clients with FULL requests in buffer */
long ClientsWriteBlocked[mskcnt];/* clients who cannot receive output */
long OutputPending[mskcnt];	/* clients with reply/event data ready to go */
long NConnBitArrays = mskcnt;
#endif
long MaxClients = MAXSOCKS ;
Bool NewOutputPending;		/* not yet attempted to write some new output */
Bool AnyClientsWriteBlocked;	/* true if some client blocked on write */

Bool RunFromSmartParent;	/* send SIGUSR1 to parent process */
Bool PartialNetwork;		/* continue even if unable to bind all addrs */
static int ParentProcess;

int     AudioListenPort = AU_DEFAULT_TCP_PORT;

static Bool debug_conns = FALSE;

static long IgnoredClientsWithInput[mskcnt];
static long GrabImperviousClients[mskcnt];
#ifndef _MINIX
static long SavedAllClients[mskcnt];
static long SavedAllSockets[mskcnt];
static long SavedClientsWithInput[mskcnt];
#endif /* _MINIX */
int GrabInProgress = 0;

int ConnectionTranslation[MAXSOCKS];
#endif /* !AMOEBA */

#ifdef _MINIX
asio_fd_set_t InprogressFdSet;	/* fds that have an operation in progress */
asio_fd_set_t ListenFdSet;	/* fds that accept new connections */
asio_fd_set_t CompletedFdSet;	/* fds that completed some I/O but have not
				 * been able to process this information
				 * synchronously (or completely) */
asio_fd_set_t ClientFdSet;	/* fds that belong to clients */
asio_fd_set_t IgnoreFdSet;	/* Ignore these clients if they have completed
				 * I/O */
asio_fd_set_t GrabFdSet;	/* This is the client who has the grab, 
				 * if any */

static int TcpListenFd= -1;	/* initialy there is no tcp fd. */
Bool AnyClientsWithInput= FALSE;

struct NewConnection
{
	int nc_result;		/* What was the result */
	int nc_errno;		/* and the error */
} NewTcpConnection;
#endif

extern int auditTrailLevel;
extern ClientPtr NextAvailableClient();

extern SIGVAL AutoResetServer();
extern SIGVAL GiveUp();
extern AuID CheckAuthorization();
#ifndef AMOEBA
static void CloseDownFileDescriptor(), ErrorConnMax();
#endif
extern void FreeOsBuffers(), ResetOsBuffers();

#ifdef TCPCONN
#ifndef _MINIX
static int
open_tcp_socket ()
{
    struct sockaddr_in insock;
    int request;
    int retry;
#ifdef SVR4
#undef SO_DONTLINGER
#endif
#ifndef SO_DONTLINGER
#ifdef SO_LINGER
    static int linger[2] = { 0, 0 };
#endif /* SO_LINGER */
#endif /* SO_DONTLINGER */

#ifdef AIXV3
#ifndef FORCE_DISPLAY_NUM
    extern int AIXTCPSocket;
    if (AIXTCPSocket>=0) {
        request= AIXTCPSocket;
    } else
#endif /* FORCE_DISPLAY_NUM */
#endif /* AIX && etc. */
    if ((request = socket (AF_INET, SOCK_STREAM, 0)) < 0) 
    {
	Error ("Creating TCP socket");
	return -1;
    } 
#ifdef SO_REUSEADDR
    /* Necesary to restart the server without a reboot */
    {
	int one = 1;
	setsockopt(request, SOL_SOCKET, SO_REUSEADDR, (char *)&one, sizeof(int));
    }
#endif /* SO_REUSEADDR */
#ifdef AIXV3
#ifndef FORCE_DISPLAY_NUMBER
    if (AIXTCPSocket<0)
#endif
#endif
    {
    bzero ((char *)&insock, sizeof (insock));
#ifdef BSD44SOCKETS
    insock.sin_len = sizeof(insock);
#endif
    insock.sin_family = AF_INET;
    insock.sin_port = htons ((unsigned short)(AudioListenPort + atoi (display)));
    insock.sin_addr.s_addr = htonl(INADDR_ANY);
    retry = 20;
    while (bind(request, (struct sockaddr *) &insock, sizeof (insock)))
    {
	if (--retry == 0) {
	    Error ("Binding TCP socket");
	    close (request);
	    return -1;
	}
#ifdef SO_REUSEADDR
	sleep (1);
#else
	sleep (10);
#endif /* SO_REUSEDADDR */
    }
    }
#ifdef SO_DONTLINGER
    if(setsockopt (request, SOL_SOCKET, SO_DONTLINGER, (char *)NULL, 0))
	Error ("Setting TCP SO_DONTLINGER");
#else
#ifdef SO_LINGER
    if(setsockopt (request, SOL_SOCKET, SO_LINGER,
		   (char *)linger, sizeof(linger)))
	Error ("Setting TCP SO_LINGER");
#endif /* SO_LINGER */
#endif /* SO_DONTLINGER */
    if (listen (request, 5)) {
	Error ("TCP Listening");
	close (request);
	return -1;
    }
    return request;
}
#else  /* _MINIX */

Bool EstablishNewConnections();

static int
MNX_open_tcp_socket (extra_fd)
int *extra_fd;
{
	int fd, r, flags, retry;
	char *tcp_dev, *check;
	int display_no;
	nwio_tcpconf_t tcpconf;
	nwio_tcpcl_t tcpcl;

	/* Allow the audio server to run on a different IP device with the 
	 * TCP_DEVICE environment variable, otherwise we take the default.
	 */
	tcp_dev= getenv("TCP_DEVICE");
	if (tcp_dev == NULL)
		tcp_dev= TCP_DEVICE;

	fd= open(tcp_dev, O_RDWR);
	if (fd == -1)
	{
		Error ("Creating TCP socket");
		return -1;
	} 
	if (extra_fd)
	{
		*extra_fd= fd;

		fd= open(tcp_dev, O_RDWR);
		if (fd == -1)
		{
			Error ("Creating TCP socket");
			close(*extra_fd);
			return -1;
		} 
	}

	/* Bind the socket */
	display_no= strtol(display, &check, 0);
	if (check[0] != '\0')
	{
		Error("Unable to parse display number");
		return -1;
	}
	tcpconf.nwtc_flags= NWTC_SHARED | NWTC_LP_SET | NWTC_UNSET_RA |
		NWTC_UNSET_RP;
	tcpconf.nwtc_locport= htons(AUDIO_TCP_PORT + display_no);
	r= ioctl(fd, NWIOSTCPCONF, &tcpconf);
	if (r == -1)
	{
		Error ("Binding TCP socket");
		close (fd);
		return -1;
	}

	/* Mark the filedescriptor as asynchronous */
	flags= fcntl(fd, F_GETFL);
	if (flags == -1)
	{
		Error("Unable to get the flags of a tcp fd");
		close(fd);
		return -1;
	}
	r= fcntl(fd, F_SETFD, flags | FD_ASYNCHIO);
	if (r == -1)
	{
		Error("Unable to enable asynchronous I/O on a tcp fd");
		close(fd);
		return -1;
	}

	/* Now try to listen, possible return values are:
	 * EINPROGRESS:	the default, we can return the fd
	 * EAGAIN:	all entry in the connection table are inuse, 
	 *		we wait a few seconds.
	 * 0:		some client arrived, we enqueue
	 *		EstablishNewConnections
	 */		
	for (retry= 0; retry<10; retry++)
	{
		tcpcl.nwtcl_flags= 0;
		r= ioctl(fd, NWIOTCPLISTEN, &tcpcl);
		if (r == -1 && errno == EINPROGRESS)
			return fd;	/* Normal case */
		else if (r == -1 && errno == EAGAIN)
		{
			sleep(1);
			continue;
		}
		else
		{
			NewTcpConnection.nc_result= r;
			NewTcpConnection.nc_errno= errno;

			/* Let EstablishNewConnections deal with this
			 * situation
			 */
			QueueWorkProc(EstablishNewConnections, NULL, 
				(pointer)&NewTcpConnection);
			return fd;
		}
	}
	Error ("Binding TCP socket");
	close (fd);
	return -1;
}
#endif /* _MINIX */
#endif /* TCPCONN */

#ifdef UNIXCONN

static struct sockaddr_un unsock;

static int
open_unix_socket ()
{
    int oldUmask;
    int request;

    bzero ((char *) &unsock, sizeof (unsock));
    unsock.sun_family = AF_UNIX;
    oldUmask = umask (0);
#ifdef X_UNIX_DIR
    if (!mkdir (X_UNIX_DIR, 0777))
	chmod (X_UNIX_DIR, 0777);
#endif
    strcpy (unsock.sun_path, X_UNIX_PATH);
    strcat (unsock.sun_path, display);
#ifdef BSD44SOCKETS
    unsock.sun_len = strlen(unsock.sun_path);
#endif
#ifdef hpux
    {  
        /*    The following is for backwards compatibility
         *    with old HP clients. This old scheme predates the use
 	 *    of the /usr/spool/sockets directory, and uses hostname:display
 	 *    in the /tmp/.sockets directory
         */
        struct utsname systemName;
	static char oldLinkName[256];

        uname(&systemName);
        strcpy(oldLinkName, OLD_UNIX_DIR);
        if (!mkdir(oldLinkName, 0777))
	    chown(oldLinkName, 2, 3);
        strcat(oldLinkName, "/");
        strcat(oldLinkName, systemName.nodename);
        strcat(oldLinkName, display);
        unlink(oldLinkName);
        symlink(unsock.sun_path, oldLinkName);
    }
#endif	/* hpux */
    unlink (unsock.sun_path);
    if ((request = socket (AF_UNIX, SOCK_STREAM, 0)) < 0) 
    {
	Error ("Creating Unix socket");
	return -1;
    } 
#ifdef BSD44SOCKETS
    if (bind(request, (struct sockaddr *)&unsock, SUN_LEN(&unsock)))
#else
    if (bind(request, (struct sockaddr *)&unsock, strlen(unsock.sun_path)+2))
#endif
    {
	Error ("Binding Unix socket");
	close (request);
	return -1;
    }
    if (listen (request, 5))
    {
	Error ("Unix Listening");
	close (request);
	return -1;
    }
    (void)umask(oldUmask);
    return request;
}
#endif /*UNIXCONN */

#ifdef SERVER_LOCALCONN

#if !defined(SVR4) || defined(SVR4_ACP)
static int
connect_spipe(fd1, fd2)
     int fd1, fd2;
{
  long temp;
  struct strfdinsert sbuf;

  sbuf.databuf.maxlen = -1;
  sbuf.databuf.len = -1;
  sbuf.databuf.buf = NULL;
  sbuf.ctlbuf.maxlen = sizeof(long);
  sbuf.ctlbuf.len = sizeof(long);
  sbuf.ctlbuf.buf = (caddr_t)&temp;
  sbuf.offset = 0;
  sbuf.fildes = fd2;
  sbuf.flags = 0;
  if (ioctl(fd1, I_FDINSERT, &sbuf) == -1) return (-1);
  
  return (0);
}

static int
named_spipe(fd, path)
     int fd;
     char *path;
{
  int oldUmask, ret;
  struct stat sbuf;

  oldUmask = umask(0);

  (void) fstat(fd, &sbuf);
  ret = mknod(path, 0020666, sbuf.st_rdev);

  umask(oldUmask);

  return (ret < 0 ? -1 : fd);
}

static int
open_isc_local ()
{
  int fd = -1,fds = -1;
  long temp;
  struct strfdinsert buf;
  char path[64];

#if defined(SVR4_ACP) && defined(UNIXCONN)
  /*
   * ISC local connections go the same place as Unix-domain sockets (brain
   * death of the highest magnitude.  To allow this to function, we put
   * the ISC streams pipe elsewhere.  This will require that a binary edit
   * be done on ISC binaries under SVR4, but life is tough some times.
   */
  mkdir (AUDIO_ISC_DIR, 0777);
  chmod (AUDIO_ISC_DIR, 0777);

  strcpy (path, AUDIO_ISC_PATH);
#else /* SVR4_ACP && UNIXCONN */
  mkdir (X_UNIX_DIR, 0777);
  chmod (X_UNIX_DIR, 0777);

  strcpy (path, X_UNIX_PATH);
#endif /* SVR4_ACP && UNIXCONN */

  strcat (path, display);
  
  if(unlink(path) < 0 && errno != ENOENT) {
    ErrorF ("audio server: ISC listener pipe  in use (%s)\n", path);
    return(-1);
  }

  if ((fds = open("/dev/spx", O_RDWR)) >= 0 &&
      (fd  = open("/dev/spx", O_RDWR)) >= 0 )

    if (connect_spipe(fds, fd) != -1 &&
	named_spipe(fds, path) != -1)

      return(fd);
    else
      Error("audio server: Can't set up ISC listener pipes");

#ifndef SVR4
  /*
   * At this point, most SVR4 versions will fail on this, so leave out the
   * warning
   */
  else
    Error("audio server: Cannot open \"/dev/spx\" for ISC listener");
#endif

  (void) close(fds);
  (void) close(fd);
  return(-1);
}


static int
accept_isc_local()
{
  struct strrecvfd buf;

  while (ioctl(spxFd, I_RECVFD, &buf) < 0)
    if (errno != EAGAIN) {
      Error("audio server: Can't read fildes from ISC client");
      return(-1);
    }

  BITSET(AllStreams, buf.fd);
  return(buf.fd);
}

static int
open_xsight_local()
{
  int fds = -1,fdr = -1;
  char pathS[64], pathR[64];

  sprintf(pathS, "%s%sS",AUDIO_XSIGHT_PATH, display);
  sprintf(pathR, "%s%sR",AUDIO_XSIGHT_PATH, display);
  
  if((unlink(pathS) < 0 && errno != ENOENT) ||
     (unlink(pathR) < 0 && errno != ENOENT))
    {
      ErrorF ("audio server: SCO listener pipe in use (%s)\n",pathR);
      return(-1);
    }
  
  if ((fds = open("/dev/spx", O_RDWR)) >= 0 &&
      (fdr = open("/dev/spx", O_RDWR)) >= 0 )

    if (connect_spipe(fds, fdr) != -1 &&
	named_spipe(fds, pathS) != -1 &&
	named_spipe(fdr, pathR) != -1)
      
      return(fds);
    else
      Error("audio server: Can't set up SCO listener pipes");
  
#ifndef SVR4
  /*
   * At this point, most SVR4 versions will fail on this, so leave out the
   * warning
   */
  else
    Error("audio server: Cannot open \"/dev/spx\" for SCO listener");
#endif

  (void) close(fds);
  (void) close(fdr);
  return(-1);
}


static int
accept_xsight_local()
{
  char c;
  int fd;
  long temp;
  struct strfdinsert buf;

  if (read(xsFd, &c, 1) < 0) {
    Error("audio server: Can't read from SCO client");
    return(-1);
  }

  if ((fd = open("/dev/spx", O_RDWR)) < 0) {
    Error("audio server: Can't open \"/dev/spx\" for SCO client connection");
    return(-1);
  }

  if (connect_spipe(xsFd, fd) < 0) {
    Error("audio server: Can't connect pipes for SCO client connection");
    (void) close(fd);
    return(-1);
  }

  BITSET(AllStreams, fd);
  return(fd);
}
#endif /* SVR4 */

static int
open_att_local ()
{
  char *slave;
  int fd;
  char path[64];

  mkdir (AUDIO_STREAMS_DIR, 0777);
  chmod (AUDIO_STREAMS_DIR, 0777);
  
  strcpy (path, AUDIO_STREAMS_PATH);
  strcat (path, display);
  
  if((unlink(path) < 0 && errno != ENOENT)) {
    ErrorF ("audio server: USL listener pipe in use (%s)\n", path);
    return(-1);
  }
  
  if( (fd = open("/dev/ptmx", O_RDWR)) < 0 ) {
    Error ("audio server: Cannot open \"/dev/ptmx\" for USL listener");
    return(-1);
  }
  
  grantpt(fd);
  unlockpt(fd);
  slave = (char *) ptsname(fd);
  if (link(slave, path) < 0 || chmod(path, 0666) < 0) {
    Error("audio server: Can't set up local USL listener");
    return(-1);
  }

  if (open(path, O_RDWR) < 0) {
    ErrorF("audio server: Can't open %s for USL listener\n", path);
    close(fd);
    return(-1);
  }

  return(fd);
}

#ifdef SVR4
static int
open_att_svr4_local ()
{
  int fd[2], tfd;
  char path[64];

  mkdir (AUDIO_STREAMS_DIR, 0777);
  chmod (AUDIO_STREAMS_DIR, 0777);
  
  strcpy (path, AUDIO_NSTREAMS_PATH);
  strcat (path, display);
  
  if ((unlink(path) < 0 && errno != ENOENT)) {
    ErrorF ("audio server: SVR4 named listener pipe in use (%s)\n", path);
    return(-1);
  }

  if ((tfd = creat(path, (mode_t)0666)) < 0) {
    ErrorF("audio server: Can't create named-streams path (%s)\n", path);
    return(-1);
  }
  close(tfd);
  if (chmod(path, (mode_t)0666) < 0) {
    ErrorF("audio server: Can't change mode on %s\n", path);
    return(-1);
  }

  if (pipe(fd) != 0) {
    Error("audio server: SVR4 named listener pipe creation failed\n");
    return(-1);
  }

  if (ioctl(fd[0], I_PUSH, "connld") != 0) {
    Error("audio server: ioctl(I_PUSH) failed for SVR4 named listener pipe\n");
    return(-1);
  }

  if (fattach(fd[0], path) != 0) {
    ErrorF("SVR4: fattach on %s failed for SVR4 named listener pipe\n", path);
    return(-1);
  }

  return(fd[1]);
}
#endif /* SVR4 */

static int
accept_att_local()
{
  int newconn;
  int read_in;
  char length;
  char path[64];

  /*
   * first get device-name
   */
  if( (read_in = read(ptsFd, &length, 1)) <= 0 ) {
    Error("audio server: Can't read slave name length from USL client connection");
    return(-1);
  }

  if( (read_in = read(ptsFd, path, length)) <= 0 ) {
    Error("audio server: Can't read slave name from USL client connection");
    return(-1);
  }

  path[ length ] = '\0';
      
  if( (newconn = open(path,O_RDWR)) < 0 ) {
    Error("audio server: Can't open slave for USL client connection");
    return(-1);
  }

  (void) write(newconn, "1", 1); /* send an acknowledge to the client */

  BITSET(AllStreams, newconn);
  return(newconn);
}

#ifdef SVR4
static int
accept_att_svr4_local()
{
  struct strrecvfd str;

  if (ioctl(NstrFd, I_RECVFD, &str) < 0) {
    ErrorF("audio server: I_RECVFD failed on SVR4 named client connection\n");
    return(-1);
  }
  BITSET(AllStreams, str.fd);
  return(str.fd);
}
#endif /* SVR4 */
#endif /* SERVER_LOCALCONN */

#ifdef SYSV386
int
sysv386_getpeername(fd, from, fromlen)
     int fd;
     struct sockaddr *from;
     int *fromlen;
{
#ifdef SERVER_LOCALCONN
  /*
   * check up whether our fd is really a streams pipe ( /dev/pts??? )
   */
  if (GETBIT(AllStreams, fd))
    {
      from->sa_family = AF_UNSPEC;
      *fromlen = 0;
      return 0;
    }
#endif /* SERVER_LOCALCONN */
#if defined(TCPCONN) || defined(DNETCONN) || defined(UNIXCONN)
  return getpeername(fd, from, fromlen);
#endif
}

int
sysv386_accept (fd, from, fromlen)
int fd;
struct sockaddr *from;
int *fromlen;
{
#ifdef SERVER_LOCALCONN
  if (fd == ptsFd) return accept_att_local();
#ifdef SVR4
  if (fd == NstrFd) return accept_att_svr4_local();
#endif /* SVR4 */
#if !defined(SVR4) || defined(SVR4_ACP)
  if (fd == spxFd) return accept_isc_local();
  if (fd == xsFd)  return accept_xsight_local(); 
#endif /* !SVR4 || SVR4_ACP) */
#endif /* SERVER_LOCALCONN */
/*
 * else we are handling the normal accept case
 */
#if defined(TCPCONN) || defined(DNETCONN) || defined(UNIXCONN)
  return accept (fd, from, fromlen);
#endif
}

#define	getpeername	sysv386_getpeername
#define	accept		sysv386_accept

#endif /* SYSV386 */

#ifdef hpux
/*
 * hpux returns EOPNOTSUPP when using getpeername on a unix-domain
 * socket.  In this case, smash the socket address with the address
 * used to bind the connection socket and return success.
 */
hpux_getpeername(fd, from, fromlen)
    int	fd;
    struct sockaddr *from;
    int		    *fromlen;
{
    int	    ret;
    int	    len;

    ret = getpeername(fd, from, fromlen);
    if (ret == -1 && errno == EOPNOTSUPP)
    {
	ret = 0;
	len = strlen(unsock.sun_path)+2;
	if (len > *fromlen)
	    len = *fromlen;
	bcopy ((char *) &unsock, (char *) from, len);
	*fromlen = len;
    }
    return ret;
}

#define getpeername(fd, from, fromlen)	hpux_getpeername(fd, from, fromlen)

#endif

#ifdef DNETCONN
static int
open_dnet_socket ()
{
    int request;
    struct sockaddr_dn dnsock;

    if ((request = socket (AF_DECnet, SOCK_STREAM, 0)) < 0) 
    {
	Error ("Creating DECnet socket");
	return -1;
    } 
    bzero ((char *)&dnsock, sizeof (dnsock));
    dnsock.sdn_family = AF_DECnet;
    sprintf(dnsock.sdn_objname, "AUDIO$%d", atoi (display));
    dnsock.sdn_objnamel = strlen(dnsock.sdn_objname);
    if (bind (request, (struct sockaddr *) &dnsock, sizeof (dnsock)))
    {
	Error ("Binding DECnet socket");
	close (request);
	return -1;
    }
    if (listen (request, 5))
    {
	Error ("DECnet Listening");
	close (request);
	return -1;
    }
    return request;
}
#endif /* DNETCONN */

#define NOROOM "Maximum number of clients reached"

#ifndef AMOEBA

/*****************
 * CreateWellKnownSockets
 *    At initialization, create the sockets to listen on for new clients.
 *****************/

void
CreateWellKnownSockets()
{
    int		request, i;
#ifdef SVR4
    struct rlimit Rlimit;
#endif
#if _MINIX
    int		extra_fd;
#endif

#if defined(SERVER_LOCK)
    Lock_Server();
#endif /* SERVER_LOCK */

#ifndef _MINIX
    CLEARBITS(AllSockets);
    CLEARBITS(AllClients);
    CLEARBITS(LastSelectMask);
    CLEARBITS(ClientsWithInput);

    for (i=0; i<MAXSOCKS; i++) ConnectionTranslation[i] = 0;
#ifndef X_NOT_POSIX
    lastfdesc = sysconf(_SC_OPEN_MAX) - 1;
#else
#ifdef hpux
    lastfdesc = _NFILE - 1;
#else
#ifdef SVR4
    if (getrlimit(RLIMIT_NOFILE, &Rlimit) != 0)
    {
	lastfdesc = _NFILE - 1;
    }
    else
    {
	/*
	 * If the limit is at infinity, the server could be QUITE busy,
	 * so set a reasonable limit.
	 */
	if (Rlimit.rlim_cur == RLIM_INFINITY)
	    lastfdesc = 1024;
	else
	    lastfdesc = Rlimit.rlim_cur;
    }
#else
    lastfdesc = getdtablesize() - 1;
#endif /* SVR4 */
#endif /* hpux */
#endif /* X_NOT_POSIX */
    if (lastfdesc > MAXSOCKS)
    {
	lastfdesc = MAXSOCKS;
	if (debug_conns)
	    ErrorF( "GOT TO END OF SOCKETS %d\n", MAXSOCKS);
    }

    WellKnownConnections = 0;
#ifdef SERVER_LOCALCONN
    CLEARBITS(AllStreams);
    if ((ptsFd = open_att_local ()) != -1) {
	WellKnownConnections |= (1L << ptsFd);
    }
#ifdef SVR4
    if ((NstrFd = open_att_svr4_local ()) != -1) {
	WellKnownConnections |= (1L << NstrFd);
    }
#endif /* SVR4 */
#if !defined(SVR4) || defined(SVR4_ACP)
    if ((spxFd = open_isc_local ()) != -1) {
	WellKnownConnections |= (1L << spxFd);
    }
    if ((xsFd = open_xsight_local ()) != -1) {
	WellKnownConnections |= (1L << xsFd);
    } 
#endif /* !SVR4 || SVR4_ACP */
#endif /* SERVER_LOCALCONN */
#ifdef TCPCONN
    if ((request = open_tcp_socket ()) != -1) {
	WellKnownConnections |= (1L << request);
	DefineSelf (request);
    }
    else if (!PartialNetwork) 
    {
	FatalError ("Cannot establish tcp listening socket\n");
    }
    else
    {
	ErrorF ("Cannot establish tcp listening socket\n");
    }
#endif /* TCPCONN */
#ifdef DNETCONN
    if ((request = open_dnet_socket ()) != -1) {
	WellKnownConnections |= (1L << request);
	DefineSelf (request);
    }
    else if (!PartialNetwork) 
    {
	FatalError ("Cannot establish dnet listening socket\n");
    }
    else 
    {
	ErrorF ("Cannot establish dnet listening socket\n");
    }
#endif /* DNETCONN */
#ifdef UNIXCONN
    if ((request = open_unix_socket ()) != -1) {
	WellKnownConnections |= (1L << request);
	unixDomainConnection = request;
    }
    else if (!PartialNetwork) 
    {
	FatalError ("Cannot establish unix listening socket\n");
    }
    else 
    {
	ErrorF ("Cannot establish unix listening socket\n");
    }
#endif /* UNIXCONN */
    if (WellKnownConnections == 0)
        FatalError ("Cannot establish any listening sockets\n");
#else  /* _MINIX */
    {
	int no_listeners= 0;

	for (i=0; i<MAXSOCKS; i++) 
		ConnectionTranslation[i] = 0;
    
	lastfdesc = ASIO_FD_SETSIZE-1;	/* Can only fwait on these. */ 

	if (lastfdesc > MAXSOCKS)
	{
		lastfdesc = MAXSOCKS;
		if (debug_conns)
			ErrorF( "GOT TO END OF SOCKETS %d\n", MAXSOCKS);
	}

	ASIO_FD_ZERO(&ListenFdSet);
	ASIO_FD_ZERO(&InprogressFdSet);
	ASIO_FD_ZERO(&CompletedFdSet);
	ASIO_FD_ZERO(&ClientFdSet);
	ASIO_FD_ZERO(&IgnoreFdSet);
	ASIO_FD_ZERO(&GrabFdSet);
#ifdef TCPCONN
	TcpListenFd = MNX_open_tcp_socket (&extra_fd);
	if (TcpListenFd != -1)
	{
		if (TcpListenFd < 0 || TcpListenFd > lastfdesc)
			FatalError("invaling tcp fd: %d\n", TcpListenFd);
		ASIO_FD_SET(TcpListenFd, ASIO_IOCTL, &ListenFdSet);
		ASIO_FD_SET(TcpListenFd, ASIO_IOCTL, &InprogressFdSet);
		DefineSelf (extra_fd);
		close(extra_fd);
		no_listeners++;
	}
	else if (!PartialNetwork) 
	{
		FatalError ("Cannot establish tcp listening socket");
	}
#endif /* TCPCONN */
	if (no_listeners == 0)
		FatalError ("Cannot establish any listening sockets");
    }
#endif /* _MINIX */
    signal (SIGPIPE, SIG_IGN);
    signal (SIGHUP, AutoResetServer);
    signal (SIGINT, GiveUp);
    signal (SIGTERM, GiveUp);
#ifndef _MINIX
    AllSockets[0] = WellKnownConnections;
#endif
    ResetHosts(display);
    /*
     * Magic:  If SIGUSR1 was set to SIG_IGN when
     * the server started, assume that either
     *
     *  a- The parent process is ignoring SIGUSR1
     *
     * or
     *
     *  b- The parent process is expecting a SIGUSR1
     *     when the server is ready to accept connections
     *
     * In the first case, the signal will be harmless,
     * in the second case, the signal will be quite
     * useful
     */
    if (signal (SIGUSR1, SIG_IGN) == SIG_IGN)
	RunFromSmartParent = TRUE;
    ParentProcess = getppid ();
    if (RunFromSmartParent) {
	if (ParentProcess > 0) {
	    kill (ParentProcess, SIGUSR1);
	}
    }
}

void
ResetWellKnownSockets ()
{
    ResetOsBuffers();
#if defined(UNIXCONN) && !defined(SVR4)
    if (unixDomainConnection != -1)
    {
	/*
	 * see if the unix domain socket has disappeared
	 */
	struct stat	statb;

	if (stat (unsock.sun_path, &statb) == -1 ||
	    (statb.st_mode & S_IFMT) != S_IFSOCK)
	{
	    ErrorF ("Unix domain socket %s trashed, recreating\n",
	    	unsock.sun_path);
	    (void) unlink (unsock.sun_path);
	    (void) close (unixDomainConnection);
	    WellKnownConnections &= ~(1L << unixDomainConnection);
	    unixDomainConnection = open_unix_socket ();
	    if (unixDomainConnection != -1)
		WellKnownConnections |= (1L << unixDomainConnection);
	}
    }
#endif /* UNIXCONN */
#ifdef SERVER_LOCALCONN
    CLEARBITS(AllStreams);
#endif /* SERVER_LOCALCONN */
    ResetAuthorization ();
    ResetHosts(display);
    /*
     * See above in CreateWellKnownSockets about SIGUSR1
     */
    if (RunFromSmartParent) {
	if (ParentProcess > 0) {
	    kill (ParentProcess, SIGUSR1);
	}
    }
    /*
     * restart XDMCP
     */
}

static void
AuthAudit (client, letin, saddr, len, proto_n, auth_proto)
    int client;
    Bool letin;
    struct sockaddr *saddr;
    int len;
    unsigned short proto_n;
    char *auth_proto;
{
    char addr[128];

    if (!len)
        strcpy(addr, "local host");
    else
	switch (saddr->sa_family)
	{
	case AF_UNSPEC:
#ifdef UNIXCONN
	case AF_UNIX:
#endif
	    strcpy(addr, "local host");
	    break;
#ifdef TCPCONN
	case AF_INET:
	    sprintf(addr, "IP %s port %d",
		    inet_ntoa(((struct sockaddr_in *) saddr)->sin_addr),
		    ((struct sockaddr_in *) saddr)->sin_port);
	    break;
#endif
#ifdef DNETCONN
	case AF_DECnet:
	    sprintf(addr, "DN %s",
		    dnet_ntoa(&((struct sockaddr_dn *) saddr)->sdn_add));
	    break;
#endif
	default:
	    strcpy(addr, "unknown address");
	}
    if (letin)
	AuditF("client %d connected from %s\n", client, addr);
    else
	AuditF("client %d rejected from %s\n", client, addr);
    if (proto_n)
	AuditF("  Auth name: %.*s\n", proto_n, auth_proto);
}

/*****************************************************************
 * ClientAuthorized
 *
 *    Sent by the client at connection setup:
 *                typedef struct _auConnClientPrefix {
 *                   CARD8	byteOrder;
 *                   BYTE	pad;
 *                   CARD16	majorVersion, minorVersion;
 *                   CARD16	nbytesAuthProto;    
 *                   CARD16	nbytesAuthString;   
 *                 } auConnClientPrefix;
 *
 *     	It is hoped that eventually one protocol will be agreed upon.  In the
 *        mean time, a server that implements a different protocol than the
 *        client expects, or a server that only implements the host-based
 *        mechanism, will simply ignore this information.
 *
 *****************************************************************/

#ifndef _MINIX
char * 
ClientAuthorized(client, proto_n, auth_proto, string_n, auth_string)
    ClientPtr client;
    char *auth_proto, *auth_string;
    unsigned short proto_n, string_n;
{
    register OsCommPtr priv;
    union {
	struct sockaddr sa;
#ifdef UNIXCONN
	struct sockaddr_un un;
#endif /* UNIXCONN */
#ifdef TCPCONN
	struct sockaddr_in in;
#endif /* TCPCONN */
#ifdef DNETCONN
	struct sockaddr_dn dn;
#endif /* DNETCONN */
    } from;
    int	fromlen = sizeof (from);
    AuID	 auth_id;

    auth_id = CheckAuthorization (proto_n, auth_proto,
				  string_n, auth_string);

    priv = (OsCommPtr)client->osPrivate;
    if (auth_id == (AuID) ~0L && !allow_any_host)
    {
	if (getpeername (priv->fd, &from.sa, &fromlen) != -1)
	{
	    if (InvalidHost (&from.sa, fromlen))
		AuthAudit(client->index, FALSE, &from.sa, fromlen,
			  proto_n, auth_proto);
	    else
	    {
		auth_id = (AuID) 0;
		if (auditTrailLevel > 1)
		    AuthAudit(client->index, TRUE, &from.sa, fromlen,
			      proto_n, auth_proto);
	    }
	}
	if (auth_id == (AuID) ~0L)
	    return "Client is not authorized to connect to Server";
    }
    else if (auditTrailLevel > 1)
    {
	if (getpeername (priv->fd, &from.sa, &fromlen) != -1)
	    AuthAudit(client->index, TRUE, &from.sa, fromlen,
		      proto_n, auth_proto);
    }

    priv->auth_id = auth_id;
    priv->conn_time = 0;

    /* At this point, if the client is authorized to change the access control
     * list, we should getpeername() information, and add the client to
     * the selfhosts list.  It's not really the host machine, but the
     * true purpose of the selfhosts list is to see who may change the
     * access control list.
     */
    return((char *)NULL);
}

/*****************
 * EstablishNewConnections
 *    If anyone is waiting on listened sockets, accept them.
 *    Returns a mask with indices of new clients.  Updates AllClients
 *    and AllSockets.
 *****************/

/*ARGSUSED*/
Bool
EstablishNewConnections(clientUnused, closure)
    ClientPtr clientUnused;
    pointer closure;
{
    long readyconnections;     /* mask of listeners that are ready */
    int curconn;                  /* fd of listener that's ready */
    register int newconn;         /* fd of new client */
    long connect_time;
    register int i;
    register ClientPtr client;
    register OsCommPtr oc;

#ifdef TCP_NODELAY
    union {
	struct sockaddr sa;
#ifdef UNIXCONN
	struct sockaddr_un un;
#endif /* UNIXCONN */
#ifdef TCPCONN
	struct sockaddr_in in;
#endif /* TCPCONN */
#ifdef DNETCONN
	struct sockaddr_dn dn;
#endif /* DNETCONN */
    } from;
    int	fromlen;
#endif /* TCP_NODELAY */

    readyconnections = (((long)closure) & WellKnownConnections);
    if (!readyconnections)
	return TRUE;
    connect_time = GetTimeInMillis();
    /* kill off stragglers */
    for (i=1; i<currentMaxClients; i++)
    {
	if (client = clients[i])
	{
	    oc = (OsCommPtr)(client->osPrivate);
	    if (oc && (oc->conn_time != 0) &&
		(connect_time - oc->conn_time) >= TimeOutValue)
		CloseDownClient(client);     
	}
    }
    while (readyconnections) 
    {
	curconn = ffs (readyconnections) - 1;
	readyconnections &= ~(1 << curconn);
	if ((newconn = accept (curconn,
			      (struct sockaddr *) NULL, 
			      (int *)NULL)) < 0) 
	    continue;
	if (newconn > lastfdesc)
	{
	    ErrorConnMax(newconn);
	    close(newconn);
	    continue;
	}
#ifdef TCP_NODELAY
	fromlen = sizeof (from);
	if (!getpeername (newconn, &from.sa, &fromlen))
	{
	    if (fromlen && (from.sa.sa_family == AF_INET)) 
	    {
		int mi = 1;
		setsockopt (newconn, IPPROTO_TCP, TCP_NODELAY,
			   (char *)&mi, sizeof (int));
	    }
	}
#endif /* TCP_NODELAY */
    /* ultrix reads hang on Unix sockets, hpux reads fail, AIX fails too */
#if defined(O_NONBLOCK) && (!defined(SCO) && !defined(ultrix) && !defined(hpux) && !defined(AIXV3) && !defined(uniosu))
	(void) fcntl (newconn, F_SETFL, O_NONBLOCK);
#else
#ifdef FIOSNBIO
	{
	    int	arg;
	    arg = 1;
	    ioctl(newconn, FIOSNBIO, &arg);
	}
#else
#if (defined(AIXV3) || defined(uniosu)) && defined(FIONBIO)
	{
	    int arg;
	    arg = 1;
	    ioctl(newconn, FIONBIO, &arg);
	}
#else
	fcntl (newconn, F_SETFL, FNDELAY);
#endif
#endif
#endif
	oc = (OsCommPtr)xalloc(sizeof(OsCommRec));
	if (!oc)
	{
	    ErrorConnMax(newconn);
	    close(newconn);
	    continue;
	}
	if (GrabInProgress)
	{
	    BITSET(SavedAllClients, newconn);
	    BITSET(SavedAllSockets, newconn);
	}
	else
	{
	    BITSET(AllClients, newconn);
	    BITSET(AllSockets, newconn);
	}
	oc->fd = newconn;
	oc->input = (ConnectionInputPtr)NULL;
	oc->output = (ConnectionOutputPtr)NULL;
	oc->conn_time = connect_time;
	if (client = NextAvailableClient((pointer)oc))
	{
	    ConnectionTranslation[newconn] = client->index;
	}
	else
	{
	    ErrorConnMax(newconn);
	    CloseDownFileDescriptor(oc);
	}
    }
    return TRUE;
}
#endif /* _MINIX */

/************
 *   ErrorConnMax
 *     Fail a connection due to lack of client or file descriptor space
 ************/

static void
ErrorConnMax(fd)
    register int fd;
{
    auConnSetupPrefix csp;
    char pad[3];
    struct iovec iov[3];
    char byteOrder = 0;
    int whichbyte = 1;
    struct timeval waittime;
#ifndef _MINIX
    long mask[mskcnt];
#endif /* !_MINIX */

#ifndef _MINIX
    /* if these seems like a lot of trouble to go to, it probably is */
    waittime.tv_sec = BOTIMEOUT / MILLI_PER_SECOND;
    waittime.tv_usec = (BOTIMEOUT % MILLI_PER_SECOND) *
		       (1000000 / MILLI_PER_SECOND);
    CLEARBITS(mask);
    BITSET(mask, fd);
    (void)select(fd + 1, (fd_set *) mask, (fd_set *) NULL, (fd_set *) NULL, 
	&waittime);
    /* try to read the byte-order of the connection */
    (void)read(fd, &byteOrder, 1);
#else
     /* Try to read the byte-order of the connection.
      * We sleep to avoid a call fwait.
      */
     sleep(1);
     if (read(fd, &byteOrder, 1) == -1)
     {
       /* Out of luck */
       return;
     }
#endif
    if ((byteOrder == 'l') || (byteOrder == 'B'))
    {
	csp.success = auFalse;
	csp.lengthReason = sizeof(NOROOM) - 1;
	csp.length = (sizeof(NOROOM) + 2) >> 2;
	csp.majorVersion = AuProtocolMajorVersion;
	csp.minorVersion = AuProtocolMinorVersion;
	if (((*(char *) &whichbyte) && (byteOrder == 'B')) ||
	    (!(*(char *) &whichbyte) && (byteOrder == 'l')))
	{
	    swaps(&csp.majorVersion, whichbyte);
	    swaps(&csp.minorVersion, whichbyte);
	    swaps(&csp.length, whichbyte);
	}
#ifndef _MINIX
	iov[0].iov_len = sz_auConnSetupPrefix;
	iov[0].iov_base = (char *) &csp;
	iov[1].iov_len = csp.lengthReason;
	iov[1].iov_base = NOROOM;
	iov[2].iov_len = (4 - (csp.lengthReason & 3)) & 3;
	iov[2].iov_base = pad;
	(void)_OSWriteV(fd, iov, 3);
#else
	/* assume the underlying devices buffer a bit */
	write(fd, (char *)&csp, sz_xConnSetupPrefix);
	write(fd, NOROOM, csp.lengthReason);
	write(fd, pad, (4 - (csp.lengthReason & 3)) & 3);
#endif
    }
}

/************
 *   CloseDownFileDescriptor:
 *     Remove this file descriptor and it's I/O buffers, etc.
 ************/

#ifndef _MINIX

static void
CloseDownFileDescriptor(oc)
    register OsCommPtr oc;
{
    int connection = oc->fd;

    close(connection);
    FreeOsBuffers(oc);
    BITCLEAR(AllSockets, connection);
    BITCLEAR(AllClients, connection);
#ifdef SERVER_LOCALCONN
    BITCLEAR(AllStreams, connection);
#endif
    BITCLEAR(ClientsWithInput, connection);
    BITCLEAR(GrabImperviousClients, connection);
    if (GrabInProgress)
    {
	BITCLEAR(SavedAllSockets, connection);
	BITCLEAR(SavedAllClients, connection);
	BITCLEAR(SavedClientsWithInput, connection);
    }
    BITCLEAR(ClientsWriteBlocked, connection);
    if (!ANYSET(ClientsWriteBlocked))
    	AnyClientsWriteBlocked = FALSE;
    BITCLEAR(OutputPending, connection);
    xfree(oc);
}

/*****************
 * CheckConections
 *    Some connection has died, go find which one and shut it down 
 *    The file descriptor has been closed, but is still in AllClients.
 *    If would truly be wonderful if select() would put the bogus
 *    file descriptors in the exception mask, but nooooo.  So we have
 *    to check each and every socket individually.
 *****************/

void
CheckConnections()
{
    long		mask;
    long		tmask[mskcnt]; 
    register int	curclient, curoff;
    int			i;
    struct timeval	notime;
    int r;

    notime.tv_sec = 0;
    notime.tv_usec = 0;

    for (i=0; i<mskcnt; i++)
    {
	mask = AllClients[i];
        while (mask)
    	{
	    curoff = ffs (mask) - 1;
 	    curclient = curoff + (i << 5);
            CLEARBITS(tmask);
            BITSET(tmask, curclient);
            r = select (curclient + 1, (fd_set *)tmask, (fd_set *)NULL, 
		(fd_set *)NULL, &notime);
            if (r < 0 && errno != EINTR)
		CloseDownClient(clients[ConnectionTranslation[curclient]]);
	    mask &= ~(1 << curoff);
	}
    }	
}

/*****************
 * CloseDownConnection
 *    Delete client from AllClients and free resources 
 *****************/

void
CloseDownConnection(client)
    ClientPtr client;
{
    OsCommPtr oc = (OsCommPtr)client->osPrivate;

    if (oc->output && oc->output->count)
	FlushClient(client, oc, (char *)NULL, 0);
    ConnectionTranslation[oc->fd] = 0;
    CloseDownFileDescriptor(oc);
    client->osPrivate = (pointer)NULL;
    if (auditTrailLevel > 1)
	AuditF("client %d disconnected\n", client->index);
}


AddEnabledDevice(fd)
    int fd;
{
    BITSET(EnabledDevices, fd);
    BITSET(AllSockets, fd);
}


RemoveEnabledDevice(fd)
    int fd;
{
    BITCLEAR(EnabledDevices, fd);
    BITCLEAR(AllSockets, fd);
}

/*****************
 * OnlyListenToOneClient:
 *    Only accept requests from  one client.  Continue to handle new
 *    connections, but don't take any protocol requests from the new
 *    ones.  Note that if GrabInProgress is set, EstablishNewConnections
 *    needs to put new clients into SavedAllSockets and SavedAllClients.
 *    Note also that there is no timeout for this in the protocol.
 *    This routine is "undone" by ListenToAllClients()
 *****************/

OnlyListenToOneClient(client)
    ClientPtr client;
{
    OsCommPtr oc = (OsCommPtr)client->osPrivate;
    int connection = oc->fd;

    if (! GrabInProgress)
    {
	COPYBITS(ClientsWithInput, SavedClientsWithInput);
	MASKANDSETBITS(ClientsWithInput,
		       ClientsWithInput, GrabImperviousClients);
	if (GETBIT(SavedClientsWithInput, connection))
	{
	    BITCLEAR(SavedClientsWithInput, connection);
	    BITSET(ClientsWithInput, connection);
	}
	UNSETBITS(SavedClientsWithInput, GrabImperviousClients);
	COPYBITS(AllSockets, SavedAllSockets);
	COPYBITS(AllClients, SavedAllClients);
	UNSETBITS(AllSockets, AllClients);
	MASKANDSETBITS(AllClients, AllClients, GrabImperviousClients);
	BITSET(AllClients, connection);
	ORBITS(AllSockets, AllSockets, AllClients);
	GrabInProgress = client->index;
    }
}

/****************
 * ListenToAllClients:
 *    Undoes OnlyListentToOneClient()
 ****************/

ListenToAllClients()
{
    if (GrabInProgress)
    {
	ORBITS(AllSockets, AllSockets, SavedAllSockets);
	ORBITS(AllClients, AllClients, SavedAllClients);
	ORBITS(ClientsWithInput, ClientsWithInput, SavedClientsWithInput);
	GrabInProgress = 0;
    }	
}

/****************
 * IgnoreClient
 *    Removes one client from input masks.
 *    Must have cooresponding call to AttendClient.
 ****************/

IgnoreClient (client)
    ClientPtr	client;
{
    OsCommPtr oc = (OsCommPtr)client->osPrivate;
    int connection = oc->fd;

    if (!GrabInProgress || GETBIT(AllClients, connection))
    {
    	if (GETBIT (ClientsWithInput, connection))
	    BITSET(IgnoredClientsWithInput, connection);
    	else
	    BITCLEAR(IgnoredClientsWithInput, connection);
    	BITCLEAR(ClientsWithInput, connection);
    	BITCLEAR(AllSockets, connection);
    	BITCLEAR(AllClients, connection);
	BITCLEAR(LastSelectMask, connection);
    }
    else
    {
    	if (GETBIT (SavedClientsWithInput, connection))
	    BITSET(IgnoredClientsWithInput, connection);
    	else
	    BITCLEAR(IgnoredClientsWithInput, connection);
	BITCLEAR(SavedClientsWithInput, connection);
	BITCLEAR(SavedAllSockets, connection);
	BITCLEAR(SavedAllClients, connection);
    }
    isItTimeToYield = TRUE;
}

/****************
 * AttendClient
 *    Adds one client back into the input masks.
 ****************/

AttendClient (client)
    ClientPtr	client;
{
    OsCommPtr oc = (OsCommPtr)client->osPrivate;
    int connection = oc->fd;

    if (!GrabInProgress || GrabInProgress == client->index ||
	GETBIT(GrabImperviousClients, connection))
    {
    	BITSET(AllClients, connection);
    	BITSET(AllSockets, connection);
	BITSET(LastSelectMask, connection);
    	if (GETBIT (IgnoredClientsWithInput, connection))
	    BITSET(ClientsWithInput, connection);
    }
    else
    {
	BITSET(SavedAllClients, connection);
	BITSET(SavedAllSockets, connection);
	if (GETBIT(IgnoredClientsWithInput, connection))
	    BITSET(SavedClientsWithInput, connection);
    }
}

/* make client impervious to grabs; assume only executing client calls this */

MakeClientGrabImpervious(client)
    ClientPtr client;
{
    OsCommPtr oc = (OsCommPtr)client->osPrivate;
    int connection = oc->fd;

    BITSET(GrabImperviousClients, connection);
}

/* make client pervious to grabs; assume only executing client calls this */

MakeClientGrabPervious(client)
    ClientPtr client;
{
    OsCommPtr oc = (OsCommPtr)client->osPrivate;
    int connection = oc->fd;

    BITCLEAR(GrabImperviousClients, connection);
    if (GrabInProgress && (GrabInProgress != client->index))
    {
	if (GETBIT(ClientsWithInput, connection))
	{
	    BITSET(SavedClientsWithInput, connection);
	    BITCLEAR(ClientsWithInput, connection);
	}
	BITCLEAR(AllSockets, connection);
	BITCLEAR(AllClients, connection);
	isItTimeToYield = TRUE;
    }
}

#else  /* _MINIX */

char * 
ClientAuthorized(client, proto_n, auth_proto, string_n, auth_string)
    ClientPtr client;
    char *auth_proto, *auth_string;
    unsigned short proto_n, string_n;
{
    register OsCommPtr priv;
    AuID	 auth_id;
    int 		len;
    int 		r;
    struct sockaddr 	addr;
    nwio_tcpconf_t	tcpconf;

    auth_id = CheckAuthorization (proto_n, auth_proto,
				  string_n, auth_string);

    priv = (OsCommPtr)client->osPrivate;

    /* Assume we only have tcp connections. */
    r= ioctl(priv->fd, NWIOGTCPCONF, &tcpconf);
    if (r == -1)
    {
	Error("Unable to get remote address from tcp fd");
	return ;
    }
    addr.sa_u.sa_in.sin_family= AF_INET;
    addr.sa_u.sa_in.sin_addr= tcpconf.nwtc_remaddr;
    addr.sa_u.sa_in.sin_port= tcpconf.nwtc_remport;
    len= sizeof(addr);

    if (auth_id == (AuID) ~0L && 
        !InvalidHost (&addr, len))
    {
    	ErrorF("(warning) Authorization succeeded\n");
	auth_id = (AuID) 0;
    }

    if (auth_id == (AuID) ~0L)
	return "Client is not authorized to connect to Server";

    priv->auth_id = auth_id;
    priv->conn_time = 0;

#ifdef XDMCP
    /* indicate to Xdmcp protocol that we've opened new client */
    XdmcpOpenDisplay(priv->fd);
#endif /* XDMCP */

    return((char *)NULL);
}

void CheckListeners();

Bool
EstablishNewConnections(clientUnused, closure)
    ClientPtr clientUnused;
    pointer closure;
{
    struct NewConnection *newConnP;
    register int newconn;         /* fd of new client */
    long connect_time;
    register ClientPtr client;
    register OsCommPtr oc;
    int i;

    newConnP= (struct NewConnection *)closure;

    newconn= -1;

    /* Let's take some transport protocol specific actions */
    if (newConnP == &NewTcpConnection)
    {
    	if (TcpListenFd < 0 || TcpListenFd > lastfdesc)
    		FatalError("strange value in TcpListenFd\n");
    	if (!ASIO_FD_ISSET(TcpListenFd, ASIO_IOCTL, &ListenFdSet) ||
    		!ASIO_FD_ISSET(TcpListenFd, ASIO_IOCTL, &InprogressFdSet))
    		FatalError("TcpListenFd not in progress\n");
    	ASIO_FD_CLR(TcpListenFd, ASIO_IOCTL, &ListenFdSet);
    	ASIO_FD_CLR(TcpListenFd, ASIO_IOCTL, &InprogressFdSet);
    	newconn= TcpListenFd;
    	TcpListenFd= -1;
    }

    if (newconn == -1)
    	FatalError("Unable to locate transport protocol for NewConnection\n");

    connect_time = GetTimeInMillis();
    /* kill off stragglers */
    for (i=1; i<currentMaxClients; i++)
    {
	if (client = clients[i])
	{
	    oc = (OsCommPtr)(client->osPrivate);
	    if (oc && (oc->conn_time != 0) &&
		(connect_time - oc->conn_time) >= TimeOutValue)
		CloseDownClient(client);     
	}
    }

    /* Let's check if we can start some listeners that stopped */
    CheckListeners();

    oc = (OsCommPtr)xalloc(sizeof(OsCommRec));
    if (!oc)
    {
	ErrorConnMax(newconn);
	close(newconn);
	return TRUE;
    }
    /* Make sure that the client get called the first time */
    ASIO_FD_SET(newconn, ASIO_READ, &CompletedFdSet);
    AnyClientsWithInput= TRUE;

    oc->fd = newconn;
    oc->input = (ConnectionInputPtr)NULL;
    oc->inputFake = (ConnectionInputPtr)NULL;
    oc->output = (ConnectionOutputPtr)NULL;
    oc->outputNext = (ConnectionOutputPtr)NULL;
    oc->conn_time = connect_time;
    if ((newconn < lastfdesc) &&
	(client = NextAvailableClient((pointer)oc)))
    {
	ConnectionTranslation[newconn] = client->index;
    }
    else
    {
	ErrorConnMax(newconn);
	CloseDownFileDescriptor(oc);
    }
    return TRUE;
}

static void
CloseDownFileDescriptor(oc)
    register OsCommPtr oc;
{
    int connection = oc->fd;
    int i;

    close(connection);
    FreeOsBuffers(oc);

    for (i= 0; i< ASIO_NR; i++)
    {
	ASIO_FD_CLR(connection, i, &InprogressFdSet);
	ASIO_FD_CLR(connection, i, &CompletedFdSet);
	ASIO_FD_CLR(connection, i, &ClientFdSet);
	ASIO_FD_CLR(connection, i, &IgnoreFdSet);
	ASIO_FD_CLR(connection, i, &GrabFdSet);
    }
    xfree(oc);
}

CloseDownConnection(client)
    ClientPtr client;
{
    OsCommPtr oc = (OsCommPtr)client->osPrivate;

    if (oc->output && oc->output->count)
	FlushClient(client, oc, (char *)NULL, 0);
    ConnectionTranslation[oc->fd] = 0;
#ifdef XDMCP
    XdmcpCloseDisplay(oc->fd);
#endif
    CloseDownFileDescriptor(oc);
    client->osPrivate = (pointer)NULL;
}

AddEnabledDevice(fd)
    int fd;
{
    ASIO_FD_SET(fd, ASIO_READ, &InprogressFdSet);
}

RemoveEnabledDevice(fd)
    int fd;
{
    ASIO_FD_CLR(fd, ASIO_READ, &InprogressFdSet);
}

OnlyListenToOneClient(client)
    ClientPtr client;
{
    OsCommPtr oc = (OsCommPtr)client->osPrivate;
    int connection = oc->fd;

    if (! GrabInProgress)
    {
        ASIO_FD_ZERO(&GrabFdSet);
        ASIO_FD_SET(connection, ASIO_READ, &GrabFdSet);
        GrabInProgress = TRUE;
    }
}

ListenToAllClients()
{
    if (GrabInProgress)
    {
	GrabInProgress = 0;
	AnyClientsWithInput= TRUE;
    }
}

IgnoreClient (client)
    ClientPtr	client;
{
    OsCommPtr   oc = (OsCommPtr) client->osPrivate;
    int         connection = oc->fd;

    ASIO_FD_SET(connection, ASIO_READ, &IgnoreFdSet);
}

AttendClient (client)
    ClientPtr	client;
{
    OsCommPtr   oc = (OsCommPtr) client->osPrivate;
    int         connection = oc->fd;

    ASIO_FD_CLR(connection, ASIO_READ, &IgnoreFdSet);
    AnyClientsWithInput= TRUE;
}

void
EnqueueNewConnection(fd, operation, result, error)
int fd;
int operation;
int result;
int error;
{
	/* Let's see which transport protocol got a new connection */
	if (!ASIO_FD_ISSET(fd, operation, &ListenFdSet))
		FatalError("result not in ListenFdSet\n");

	if (fd == TcpListenFd)
	{
		/* New tcp connection */
		NewTcpConnection.nc_result= result;
		NewTcpConnection.nc_errno= error;

		/* Let EstablishNewConnections deal with this
		 * situation
		 */
		QueueWorkProc(EstablishNewConnections, NULL, 
			(pointer)&NewTcpConnection);
		return;
	}

	FatalError("Unable to find transport protocol for new connection\n");
}

void 
CheckListeners()
{
	/* Check if all devices have listeners hanging around */
#ifdef TCPCONN
	if (TcpListenFd == -1)
	{
	    TcpListenFd = MNX_open_tcp_socket (NULL);
	    if (TcpListenFd != -1)
	    {
		    if (TcpListenFd < 0 || TcpListenFd > lastfdesc)
			    FatalError("invalid tcp fd: %d\n", TcpListenFd);
		    ASIO_FD_SET(TcpListenFd, ASIO_IOCTL, &ListenFdSet);
		    ASIO_FD_SET(TcpListenFd, ASIO_IOCTL, &InprogressFdSet);
	    }
	}
#endif /* TCPCONN */
}
#endif /* _MINIX */

#else /* AMOEBA */

#include <amoeba.h>
#include <cmdreg.h>
#include <stdcom.h>
#include <stderr.h>
#include <ampolicy.h>
#include <server/ip/hton.h>
#include <server/ip/types.h>
#include <server/ip/tcpip.h>
#include <server/ip/tcp_io.h>
#include <server/ip/gen/in.h>
#include <server/ip/gen/tcp.h>
#include <server/ip/gen/tcp_io.h>

/*
 * Size of reply buffer 
 */
#define REPLY_BUFSIZE   30000

#ifdef XDEBUG
Bool		amDebug;		/* amoeba debug toggle */
#define dbprintf(list)	if (amDebug) { ErrorF list; }
#else
#define dbprintf(list)	/* nothing */
#endif /* XDEBUG */

capability	Au;			/* AudioServer capability */
char		*AuServerHostName;	/* audio server host name */
char		*AuTcpServerName;	/* TCP/IP server name */
long		MaxClients = MAXTASKS;
ClientPtr	newClient = NULL;	/* new connections */
ClientPtr	Clients[MAXTASKS];	/* clients with input */
int		maxClient;		/* Highest numbered client */
int		totalClients;		/* all connected applications */
ClientPtr       grabClient = NULL;	/* for grabs */

mutex		NewConnsLock;		/* prevent concurrent updates */
int		nNewConns;		/* # of new clients */
OsCommPtr	NewConns[MAXTASKS];	/* new client connections */

static void AmoebaConnectorThread();
static void AmoebaTCPConnectorThread();

void
CreateWellKnownSockets()
{
    char		host[100];
    char		*getenv();
    void		DeviceReader();
    errstat		err;
    capability		pubAu;
    static int		threadsStarted = FALSE;

    /*
     * Each time the server is reset this routine is called to
     * setup the new well known sockets. For Amoeba we'll just
     * keep using the old threads that are already running.
     */
    if (!threadsStarted) {
	threadsStarted = TRUE;
	/*
	 * Create a new capability for this audio server
	 */
	if (AuServerHostName == NULL)
	    AuServerHostName = getenv("AUDIOHOST");
	if (AuServerHostName == NULL)
	    FatalError("AUDIOHOST not set, or server host name not given\n");
	sprintf(host, "%s/%s:%s", DEF_AUSVRDIR, AuServerHostName, 0 /* port */);

	uniqport(&Au.cap_port);
	priv2pub(&Au.cap_port, &pubAu.cap_port);
	(void) name_delete(host);
	if ((err = name_append(host, &pubAu)) != 0) {
	    (void) ErrorF("Cannot create capability %s: %s\n",
		host, err_why(err));
	    exit(1);
	}

	/*
	 * Initialize new connections lock
	 */
	mu_init(&NewConnsLock);

	/*
	 * This critical region prevents the subthread from proceeding until
	 * main has finished initializing. The matching sema_down() is in
	 * WaitFor.c.
	 */
	sema_init(&init_sema, 0);

	/*
	 * Also, initialize main thread locking
	 */
	InitMainThread();

	/*
	 * Initialize and start IOP reader thread
	 */
	InitializeIOPServerReader();

	/*
	 * Start native Amoeba service threads
	 */
	if (thread_newthread(AmoebaConnectorThread, CONNECTOR_STACK, 0, 0) <= 0)
	    FatalError("Cannot start Amoeba connector thread\n");
	if (thread_newthread(AmoebaConnectorThread, CONNECTOR_STACK, 0, 0) <= 0)
	    FatalError("Cannot start Amoeba connector thread\n");

	/*
	 * Start TCP/IP service threads
	 */
	if (AuTcpServerName) {
	    if (thread_newthread(AmoebaTCPConnectorThread,
	      CONNECTOR_STACK, 0, 0) <= 0)
		FatalError("Cannot start TCP connector thread\n");
	    if (thread_newthread(AmoebaTCPConnectorThread,
	      CONNECTOR_STACK, 0, 0) <= 0)
		FatalError("Cannot start TCP connector thread\n");
	}
    }

    ResetHosts(display);

#ifdef XDMCP
    XdmcpInit ();
#endif
}

void
ResetWellKnownSockets ()
{
    ResetAuthorization ();
    ResetHosts(display);

    /*
     * restart XDMCP
     */
#ifdef XDMCP
    XdmcpReset ();
#endif
}

char * 
ClientAuthorized(client, proto_n, auth_proto, string_n, auth_string)
    ClientPtr client;
    char *auth_proto, *auth_string;
    unsigned short proto_n, string_n;
{
    register OsCommPtr priv;
    AuID	 auth_id;

    auth_id = CheckAuthorization (proto_n, auth_proto,
				  string_n, auth_string);

    priv = (OsCommPtr)client->osPrivate;
    /*
     * Access control only works for audio connections over a TCP/IP stream.
     * The Amoeba philosophy is, when you have the capability you are
     * allowed to talk with the server.
     */
    if (auth_id == (AuID) ~0L) {
	nwio_tcpconf_t tcpconf;

	if (priv->family == FamilyInternet && 
	  tcp_ioc_getconf(&priv->conn.tcp.cap, &tcpconf) == STD_OK &&
	  !InvalidHost(&tcpconf.nwtc_remaddr, sizeof(ipaddr_t)))
	    auth_id = (AuID) 0;
	if (priv->family == FamilyAmoeba)
	    auth_id = (AuID) 0;
    }

    if (auth_id == (AuID) ~0L)
	return "Client is not authorized to connect to Server";

    priv->auth_id = auth_id;
    priv->conn_time = 0;

#ifdef XDMCP
    /* indicate to Xdmcp protocol that we've opened new client */
    XdmcpOpenDisplay(priv->fd);
#endif /* XDMCP */
    /* At this point, if the client is authorized to change the access control
     * list, we should getpeername() information, and add the client to
     * the selfhosts list.  It's not really the host machine, but the
     * true purpose of the selfhosts list is to see who may change the
     * access control list.
     */
    return((char *)NULL);
}

/*ARGSUSED*/
Bool
EstablishNewConnections(clientUnused, closure)
    ClientPtr clientUnused;
    pointer closure;
{
    register ClientPtr	newClient;
    register OsCommPtr	oc;
    register int	i;
    struct vc		*vc;
    int			index;

    mu_lock(&NewConnsLock);
    for(index = 0; index < nNewConns; index++) {
	oc = NewConns[index];

	/*
	 * Find a new slot
	 */
	if (totalClients >= MAXTASKS) {
	    ErrorF("Too many audio-clients are being served already\n");
	    am_close(oc, VC_BOTH|VC_ASYNC);
	    xfree((char *)oc);
	    continue;
	}
	totalClients++;
	for (i = 0; i < maxClient+1; i++)
	    if (Clients[i] == 0) break;
	if (i == maxClient) maxClient++;

	/*
	 * Fill in client's connection number
	 */
	oc->number = i;

	/*
	 * Now stuff the new client in the array where
	 * WaitForSomething will find it and hand it up.
	 */
	newClient = NextAvailableClient((pointer)oc);
	Clients[i] = newClient;
    }
    nNewConns = 0;
    mu_unlock(&NewConnsLock);
    return TRUE;
}

#define NOROOM "Maximum number of clients reached"

OnlyListenToOneClient(client)
    ClientPtr client;
{
    if (grabClient != NULL && grabClient != client) {
	ErrorF("Uncancelled OnlyListenToOneClient()?\n");
	grabClient = NULL;
    } else
	grabClient = client;
}

CloseDownConnection(client)
    ClientPtr client;
{
    register OsCommPtr	oc;

    dbprintf(("Connection closed\n"));
    oc = (OsCommPtr)client->osPrivate;
    Clients[oc->number] = NULL;
    if (oc->number == maxClient) maxClient--;
    am_close(oc, VC_BOTH|VC_ASYNC);
    if (oc->buffer) xfree(oc->buffer);
    xfree(oc);
    client->osPrivate = (pointer) NULL;
}

ListenToAllClients()
{
    grabClient = NULL;
}

IgnoreClient (client)
    ClientPtr	client;
{
    OsCommPtr oc = (OsCommPtr)client->osPrivate;
    oc->status |= IGNORE;
}

AttendClient (client)
    ClientPtr	client;
{
    OsCommPtr oc = (OsCommPtr)client->osPrivate;
    oc->status &= ~IGNORE;
    if (am_avail(oc, VC_IN)) {
	WakeUpMainThread();
    }
}

/* These two are dummies -- and are never called at run-time */
AddEnabledDevice(fd)
    int fd;
{
    return;
}

RemoveEnabledDevice(fd)
    int fd;
{
    return;
}

/*
 * Wakeup main thread if necessary
 */
static void
UnblockMain(oc)
    register OsCommPtr oc;
{
    if ((oc->status & IGNORE) == 0) {
	WakeUpMainThread();
    }
}

static char *
OsCommFamily(family)
    int family;
{
    switch (family) {
    case FamilyAmoeba:
	return "AMOEBA";
    case FamilyInternet:
	return "TCP/IP";
    }
    return "UNKNOWN";
}

static char *
OsCommStatus(status)
    int status;
{
    static char buf[100];

    buf[0] = '\0';
    if (status == 0)
	sprintf(buf, "NONE");
    if (status & CONN_KILLED)
	sprintf(buf, "%s KILLED", buf);
    if (status & REQ_PUSHBACK)
	sprintf(buf, "%s PUSHBACK", buf);
    if (status & IGNORE)
	sprintf(buf, "%s IGNORE", buf);
    return buf;
}

/*
 * Return status information about the open connections
 */
errstat
ConnectionStatus(hdr, buf, size)
    header *hdr;
    char *buf;
    int size;
{
    OsCommPtr	oc;
    int		i;
    char	*begin, *end;
    char	*bprintf();

    begin = buf;
    end = buf + size;

    /* all active clients */
    if (maxClient > 0) {
	begin = bprintf(begin, end, "Active clients:\n");
	for (i = 0; i < maxClient; i++) {
	    if (Clients[i] && (oc = (OsCommPtr)Clients[i]->osPrivate)) {
		begin = bprintf(begin, end, "%d: Family %s, Status %s\n",
		    i, OsCommFamily(oc->family), OsCommStatus(oc->status));
	    }
	}
    }

    /* all new (awaiting) clients */
    mu_lock(&NewConnsLock);
    if (nNewConns > 0) {
	begin = bprintf(begin, end, "New clients:\n");
	for (i = 0; i < nNewConns; i++) {
	    oc = NewConns[i];
	    begin = bprintf(begin, end, "%d: Family %s, Status %s\n",
		i, OsCommFamily(oc->family), OsCommStatus(oc->status));
	}
    }
    mu_unlock(&NewConnsLock);

    if (begin == NULL) {
	hdr->h_size = 0;
	return STD_SYSERR;
    } else {
	hdr->h_size = begin - buf;
	return STD_OK;
    }
	
}
    
/*
 * Establishing a new connection is done in two phases. This thread does the
 * first part. It filters out bad connect requests. A new rendevous port is
 * sent to the client and the main loop is informed if there is a legal
 * request. The sleep synchronizes with the main loop so that the paperwork
 * is finished for the current connect request before the thread is ready to
 * accept another connect.
 */
static void
AmoebaConnectorThread()
{
    header	req, rep;
    port	client_ports[2];
    port	server_ports[2];
    short	s;
    OsCommPtr	oc;
    char	*repb;
    extern	CreateNewClient();

    WaitForInitialization();
    dbprintf(("AmoebaConnectorThread() running ...\n"));
    if ((repb = (char *)malloc(REPLY_BUFSIZE)) == NULL)
	FatalError("Amoeba connector thread: malloc failed");
    for (;;) {
	do {
	    req.h_port = Au.cap_port;
	    s = getreq(&req, NILBUF, 0);
	} while (ERR_CONVERT(s) == RPC_ABORTED);
	if (ERR_STATUS(s))
	    FatalError("Amoeba connector thread: getreq failed");
	/* TODO: check privilege fields here */

	dbprintf(("AmoebaConnectorThread() accepting a request\n"));

	switch (req.h_command) {

	case STD_INFO:
	    rep.h_status = STD_OK;
	    sprintf(repb, "audio server on %s", AuServerHostName);
	    rep.h_size = strlen(repb);
	    putrep(&rep, repb, rep.h_size);
	    break;

	case STD_STATUS:
	    rep.h_status = ConnectionStatus(&rep, repb, REPLY_BUFSIZE);
	    putrep(&rep, repb, rep.h_size);
	    break;

	case AX_SHUTDOWN:
	    GiveUp();
	    rep.h_status = STD_OK;
	    putrep(&rep, NILBUF, 0);
	    break;

	case AX_REINIT:
	    AutoResetServer();
	    rep.h_status = STD_OK;
	    putrep(&rep, NILBUF, 0);
	    break;

	case AX_CONNECT:
	    /*
	     * All is well. Open a virtual circuit and read the prefix
	     */
	    if (totalClients >= MAXTASKS) {
		ErrorF("Connection refused: %s\n", NOROOM);
		goto NoSpace;
	    }

	    /*
	     * Fill operating system's communication structure
	     */
	    oc = (OsCommPtr)xalloc(sizeof(OsCommRec));
	    if (oc == (OsCommPtr)NULL) {
		ErrorF("Connection refused: No memory for connection data\n");
		goto NoSpace;
	    }
	    oc->number = -1;
	    oc->buffer = NULL;
	    oc->size = 0;
	    oc->status = 0;
	    oc->family = FamilyAmoeba;
	    oc->conn_time = 0L;

	    /*
	     * Now some priv2pub magic vc_create must be called with a put
	     * port as first parameter and a get port as 2nd parameter.
	     * Since we are a server we send the opposite to the client; ie.
	     * a get port and a put port respectively.
	     */
	    uniqport(&client_ports[0]);
	    uniqport(&server_ports[1]);
	    priv2pub(&client_ports[0], &server_ports[0]);
	    priv2pub(&server_ports[1], &client_ports[1]);
	    oc->conn.vc = vc_create(&server_ports[0], &server_ports[1],
				    MAXBUFSIZE, MAXBUFSIZE);
	    if (oc->conn.vc == (struct vc *)NULL) {
NoSpace:
	        rep.h_status = AX_FULLHOUSE;
	        putrep(&rep, NILBUF, 0);
	    } else {
	        rep.h_status = AX_OK;
	        putrep(&rep, (bufptr)client_ports, 2*sizeof(port));
	        dbprintf(("Amoeba connection accepted\n"));

		/*
		 * Store for the main loop to finish creation
		 */
		vc_warn(oc->conn.vc, VC_IN, UnblockMain, (int) oc);
		mu_lock(&NewConnsLock);
		NewConns[nNewConns++] = oc;
		mu_unlock(&NewConnsLock);
		WakeUpMainThread();
	    }
	    break;

	default:
	    rep.h_status = STD_COMBAD;
	    putrep(&rep, NILBUF, 0);
	    break;
	}
    }
}

static void
TcpIpReaderSignalCatcher(sig, us, extra)
    signum sig;
    thread_ustate *us;
    char *extra;
{
    register OsCommPtr oc = (OsCommPtr) extra;

    dbprintf(("TcpIpReaderSignalCatcher(%d), number %d\n", sig, oc->number));
    if (oc->conn.tcp.signal != sig) {
	ErrorF("TCP/IP Reader: Connection %s got unexpected signal %d\n",
	    oc->number, sig);
    }
    oc->conn.tcp.signal = -1;
    thread_exit();
}

/*
 * TCP/IP reader thread
 */
static void
TcpIpReaderThread(argptr, argsize)
    void *argptr;
    int argsize;
{
    register OsCommPtr oc;

    if (argsize != sizeof(OsCommPtr))
	FatalError("Internal error: TcpIpReaderThread incorrectly called\n");
    oc = *((OsCommPtr *) argptr);
    (void)sig_catch(oc->conn.tcp.signal, TcpIpReaderSignalCatcher, (char*)oc);
    while (TRUE) {
	char buffer[MAXBUFSIZE];
	bufsize size;

	size = tcpip_read(&oc->conn.tcp.cap, buffer, sizeof(buffer));
	dbprintf(("TcpIpReaderThread() read %d bytes\n", size));
	if (ERR_STATUS(size)) {
	    ErrorF("TCP/IP read failed (%s)\n", tcpip_why(ERR_CONVERT(size)));
	    oc->status |= CONN_KILLED;
	    oc->conn.tcp.signal = -1;
	    thread_exit();
	}
	if (size == 0 || cb_puts(oc->conn.tcp.cb, buffer, size)) {
	    if (size != 0)
		ErrorF("TCP/IP short write to circular buffer\n");
	    oc->status |= CONN_KILLED;
	    oc->conn.tcp.signal = -1;
	    thread_exit();
	}
	UnblockMain(oc);
    }
}

/*
 * To prevent the audio-server from generating lots of error messages,
 * in case the server is gone or when its full.
 */
#define	LOOP_OPEN	1
#define	LOOP_SETCONF	2
#define	LOOP_LISTEN	4

/*
 * The TCP/IP connector thread listens to a well known port (6000 +
 * display number) for connection request. When such a request arrives
 * it allocates a communication structure and a reader thread. This
 * thread prevents the main loop from blocking when there's no data.
 */
static void
AmoebaTCPConnectorThread()
{
    capability		svrcap, chancap;
    nwio_tcpconf_t	tcpconf;
    nwio_tcpcl_t	tcpconnopt;
    char 		name[BUFSIZ];
    OsCommPtr		oc, *param;
    errstat 		err;
    int			result;
    int			looping = 0;

    strncpy(name, AuTcpServerName, BUFSIZ);
    if ((err = name_lookup(name, &svrcap)) != STD_OK) {
	sprintf(name, "%s/%s", TCP_SVR_NAME, AuTcpServerName);
	if ((err = name_lookup(name, &svrcap)) != STD_OK)
	    FatalError("Lookup %s failed: %s\n", AuTcpServerName, err_why(err));
    }

    WaitForInitialization();
    dbprintf(("AmoebaTCPConnectorThread() running ...\n"));

    for (;;) {
	/*
	 * Listen to TCP/IP port AUDIO_TCP_PORT + offset for connections.
	 * Some interesting actions have to be taken to keep this connection
	 * alive and kicking :-)
	 */
	if ((err = tcpip_open(&svrcap, &chancap)) != STD_OK) {
	    /* the server probably disappeared, just wait for it to return */
	    if (looping & LOOP_OPEN) {
		ErrorF("TCP/IP open failed: %s\n", tcpip_why(err));
		looping |= LOOP_OPEN;
	    }
	    sleep(60);
	    (void) name_lookup(name, &svrcap);
	    continue;
	}
	looping &= ~LOOP_OPEN;

	tcpconf.nwtc_locport = htons(AUDIO_TCP_PORT + atoi(offset));
	tcpconf.nwtc_flags = NWTC_EXCL | NWTC_LP_SET | NWTC_UNSET_RA | 
								NWTC_UNSET_RP;
	if ((err = tcp_ioc_setconf(&chancap, &tcpconf)) != STD_OK) {
	    /* couldn't configure, probably server space problem */
	    if (looping & LOOP_SETCONF) {
		ErrorF("TCP/IP setconf failed: %s\n", tcpip_why(err));
		looping |= LOOP_SETCONF;
	    }
	    std_destroy(&chancap);
	    sleep(60);
	    continue;
	}
	looping &= ~LOOP_SETCONF;

	tcpconnopt.nwtcl_flags = 0;
	if ((err = tcp_ioc_listen(&chancap, &tcpconnopt)) != STD_OK) {
	    /* couldn't listen, definitely a server memory problem */
	    if (looping & LOOP_LISTEN) {
		ErrorF("TCP/IP listen failed: %s\n", tcpip_why(err));
		looping |= LOOP_LISTEN;
	    }
	    std_destroy(&chancap);
	    sleep(60);
	    continue;
	}
	looping &= ~LOOP_LISTEN;

	if ((err = tcpip_keepalive_cap(&chancap)) != STD_OK) {
	    ErrorF("TCP/IP keep alive failed: %s\n", tcpip_why(err));
	    std_destroy(&chancap);
	    continue;
	}

	/*
	 * Fill operating system's communication structure
	 */
	oc = (OsCommPtr)xalloc(sizeof(OsCommRec));
	if (oc == (OsCommPtr)NULL) {
	    ErrorF("Connection refused: No memory for connection data\n");
	    goto NoSpace;
	}
	oc->number = -1;
	oc->buffer = NULL;
	oc->size = 0;
	oc->status = 0;
	oc->family = FamilyInternet;
	oc->conn_time = 0L;
	oc->conn.tcp.cap = chancap;
	if ((oc->conn.tcp.cb = cb_alloc(MAXBUFSIZE)) == NULL) {
	    ErrorF("Connection refused: No memory for circular buffer\n");
	    xfree((char *)oc);
	    goto NoSpace;
	}

	/*
	 * Start TCP/IP reader thread
	 */
	oc->conn.tcp.signal = sig_uniq();
	param = (OsCommPtr *) malloc(sizeof(OsCommPtr));
	*param = oc; /* stupid convention */
	result = thread_newthread(TcpIpReaderThread,
	    MAXBUFSIZE + CONNECTOR_STACK, (char *)param, sizeof(OsCommPtr));
	if (result == 0) {
	    ErrorF("Cannot start reader thread\n");
	    cb_close(oc->conn.tcp.cb);
	    cb_free(oc->conn.tcp.cb);
	    xfree((char *)oc);
NoSpace:
	    std_destroy(&chancap);
	    continue;
	}

	/*
	 * Store for the main loop to finish creation
	 */
	mu_lock(&NewConnsLock);
	NewConns[nNewConns++] = oc;
	mu_unlock(&NewConnsLock);
	WakeUpMainThread();
    }
}
#endif /* AMOEBA */

#ifdef AIXV3

static long pendingActiveClients[mskcnt];
static BOOL reallyGrabbed;

/****************
* DontListenToAnybody:
*   Don't listen to requests from any clients. Continue to handle new
*   connections, but don't take any protocol requests from anybody.
*   We have to take care if there is already a grab in progress, though.
*   Undone by PayAttentionToClientsAgain. We also have to be careful
*   not to accept any more input from the currently dispatched client.
*   we do this be telling dispatch it is time to yield.

*   We call this when the server loses access to the glass
*   (user hot-keys away).  This looks like a grab by the 
*   server itself, but gets a little tricky if there is already
*   a grab in progress.
******************/

void
DontListenToAnybody()
{
    if (!GrabInProgress)
    {
	COPYBITS(ClientsWithInput, SavedClientsWithInput);
	COPYBITS(AllSockets, SavedAllSockets);
	COPYBITS(AllClients, SavedAllClients);
	GrabInProgress = TRUE;
	reallyGrabbed = FALSE;
    }
    else
    {
	COPYBITS(AllClients, pendingActiveClients);
	reallyGrabbed = TRUE;
    }
    CLEARBITS(ClientsWithInput);
    UNSETBITS(AllSockets, AllClients);
    CLEARBITS(AllClients);
    isItTimeToYield = TRUE;
}

void
PayAttentionToClientsAgain()
{
    if (reallyGrabbed)
    {
	ORBITS(AllSockets, AllSockets, pendingActiveClients);
	ORBITS(AllClients, AllClients, pendingActiveClients);
    }
    else
    {
	ListenToAllClients();
    }
    reallyGrabbed = FALSE;
}

#endif


#if defined(SYSV) && defined(SYSV386) && !defined(STREAMSCONN)
#ifdef SCO

/*
 * SCO does not have writev so we emulate
 */

#include <sys/uio.h>

int _OSWriteV(fd, iov, iovcnt)
	int				 fd;
	struct iovec	*iov;
	int				 iovcnt;
{
    int i, len, total;
    char *base;

    errno = 0;
    for (i = 0, total = 0; i < iovcnt; i++, iov++) {
		len = iov->iov_len;
		base = iov->iov_base;
		while (len > 0) {
			register int nbytes;

			nbytes = write(fd, base, len);
			if (nbytes < 0 && total == 0)
				return(-1);
			if (nbytes <= 0)
				return(total);
			errno = 0;
			len   -= nbytes;
			total += nbytes;
			base  += nbytes;
		}
    }
    return(total);
}

#endif /* SCO */
#endif /* SYSV && SYSV386 && !STREAMSCONN */
