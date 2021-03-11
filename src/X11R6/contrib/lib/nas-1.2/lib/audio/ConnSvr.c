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
 * THIS SOFTWARE IS PROVIDED 'AS-IS'.  NETWORK COMPUTING DEVICES, INC.,
 * DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING WITHOUT
 * LIMITATION ALL IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS FOR A
 * PARTICULAR PURPOSE, OR NONINFRINGEMENT.  IN NO EVENT SHALL NETWORK
 * COMPUTING DEVICES, INC., BE LIABLE FOR ANY DAMAGES WHATSOEVER, INCLUDING
 * SPECIAL, INCIDENTAL OR CONSEQUENTIAL DAMAGES, INCLUDING LOSS OF USE, DATA,
 * OR PROFITS, EVEN IF ADVISED OF THE POSSIBILITY THEREOF, AND REGARDLESS OF
 * WHETHER IN AN ACTION IN CONTRACT, TORT OR NEGLIGENCE, ARISING OUT OF OR IN
 * CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 * 
 * $NCDId: @(#)ConnSvr.c,v 1.18 1994/06/01 23:27:46 greg Exp $
 */

/* Portions derived from */
/*
 * $XConsortium: XConnDis.c,v 11.94 92/12/31 16:25:05 rws Exp $
 *
 * Copyright 1989 Massachusetts Institute of Technology
 *
 * Permission to use, copy, modify, and distribute this software and its
 * documentation for any purpose and without fee is hereby granted, provided
 * that the above copyright notice appear in all copies and that both that
 * copyright notice and this permission notice appear in supporting
 * documentation, and that the name of M.I.T. not be used in advertising
 * or publicity pertaining to distribution of the software without specific,
 * written prior permission.  M.I.T. makes no representations about the
 * suitability of this software for any purpose.  It is provided "as is"
 * without express or implied warranty.
 *
 * M.I.T. DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING ALL
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL M.I.T.
 * BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION
 * OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN 
 * CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *
 * 
 * This file contains operating system dependencies.
 */

#ifdef __FreeBSD__
#include <sys/param.h>
#endif /* __FreeBSD__ */
#include <ctype.h>
#include <audio/Alibint.h>
#include <audio/Aos.h>
#include "Alibnet.h"
#include <X11/Xauth.h>			/* to get at the .Xauthority */
#include <stdio.h>
#ifdef DNETCONN
#include <netdnet/dn.h>
#include <netdnet/dnetdb.h>
#endif
#ifdef SVR4
#include <sys/stropts.h>
#endif

#ifdef STREAMSCONN
#define select _AuSelect
#endif

#ifndef AU_CONNECTION_RETRIES		/* number retries on ECONNREFUSED */
#define AU_CONNECTION_RETRIES 5
#endif

#ifdef DNETCONN
static int MakeDECnetConnection();
#endif
#ifdef UNIXCONN
static int MakeUNIXSocketConnection();
#endif
#ifdef TCPCONN
static int MakeTCPConnection();
#endif
#ifdef STREAMSCONN
extern int _AuMakeStreamsConnection();
#endif

#ifndef FNDELAY
#define FNDELAY O_NDELAY
#endif

#ifdef AMOEBA
static int MakeAmConnection();
#endif
#ifdef CLIENTS_LOCALCONN
static int MakeLOCALConnection();
static int ParseLocalName();
static int LocalConnType;

#define TYPE_INVALID	-1	/* Invalid local type */
#define TYPE_UNIX	0	/* Unix-domain socket */
#define TYPE_USL	1	/* USL streams pipe */
#define TYPE_NAMED	2	/* USL named pipe */
#define TYPE_ISC	3	/* ISC streams pipe */
#define TYPE_SCO	4	/* SCO streams pipe */
#define TYPE_ANY	5	/* Any - try XLOCAL or fallback list */

static struct conntype {
	char *pseudo_host;
	char *env_name;
	int conn_type;
} LocalConnModes[] = {
#ifdef UNIXCONN
	{"unix",  "UNIX",  TYPE_UNIX},
#else
	{"unix",  "UNIX",  TYPE_ANY},
#endif
	{"usl",   "USL",   TYPE_USL},
	{"named", "NAMED", TYPE_NAMED},
	{"isc",  "ISC",    TYPE_ISC},
	{"sco",  "SCO",    TYPE_SCO},
	{"",     "",       TYPE_INVALID}
};

#endif /* CLIENTS_LOCALCONN */

static void GetAuthorization();

static char *copystring (src, len)
    register _AuConst char *src;
    int len;
{
    char *dst = Aumalloc (len + 1);

    if (dst) {
	strncpy (dst, src, len);
	dst[len] = '\0';
    }

    return dst;
}


/* 
 * Attempts to connect to server, given server name. Returns file descriptor
 * (network socket) or -1 if connection fails.  Server names may be of the
 * following format:
 *
 *     tcp/hostname:port
 *     decnet/nodename::tasknum
 * 
 * Or, an X-style name that is converted automatically into audio server 
 * format internally:
 * 
 *     [hostname] : [:] servernumber
 *
 * The second colon indicates a DECnet style name.  No hostname is interpretted
 * as the most efficient local connection to a server on the same machine.  
 * This is usually:
 *
 *     o  shared memory
 *     o  local stream
 *     o  UNIX domain socket
 *     o  TCP to local host
 */
int _AuConnectServer (server_name, fullnamep, svrnump, 
		      auth_namep, auth_namelenp, auth_datap, auth_datalenp)
    _AuConst char *server_name;
    char **fullnamep;			/* RETURN */
    int *svrnump;			/* RETURN */
    char **auth_namep;			/* RETURN */
    int *auth_namelenp;			/* RETURN */
    char **auth_datap;			/* RETURN */
    int *auth_datalenp;			/* RETURN */
{
    int family;
    int saddrlen;
    char *saddr;
    _AuConst char *lastp, *p;		/* char pointers */
    char *phostname = NULL;		/* start of host of server */
    char *psvrnum = NULL;		/* start of svrnum of server */
    char *realtrans;			/* real audio transport prefix */
    char newsvrnum[10];			/* in case need to convert from X */
    int saviserver;			/* a temp variable */
    AuBool xname = AuTrue;		/* most applications it will be */
    AuBool dnet = AuFalse;		/* if true, then DECnet format */
    int iserver;			/* required server number */
    int (*connfunc)();			/* method to create connection */
    int fd = -1;			/* file descriptor to return */
    int len;				/* length tmp variable */


    saddrlen = 0;			/* set so that we can clear later */
    saddr = NULL;

    /*
     * Step 0, see if it is an audio server name or an X display name.
     */
    for (p = server_name; *p; p++) {
	if (*p == '/') {
	    int len = (p - server_name);
	    char tmptransport[41];
	    register _AuConst char *src;
	    register char *dst;

	    if (len >= sizeof tmptransport)    /* too AuInt32 */
		goto bad;
	    src = server_name;
	    dst = tmptransport;
	    while (src < p) {
		*dst++ = ((isupper(*src)) ? tolower(*src) : *src);
		src++;
	    }
	    *dst = '\0';
	    if (strcmp (tmptransport, "tcp") == 0 ||
		strcmp (tmptransport, "decnet") == 0) {
		xname = AuFalse;	/* got a real name! */
		server_name = (p + 1);	  /* skip prefix */
		break;
	    }
	}
    }

    /*
     * Step 1, find the hostname.  This is delimited by the required 
     * first colon.
     */
    /* SUPPRESS 530 */
    for (lastp = p = server_name; *p && *p != ':'; p++) ;
    if (!*p) return -1;		/* must have a colon */

    if (p != lastp) {		/* no hostname given */
	phostname = copystring (lastp, p - lastp);
	if (!phostname) goto bad;	/* no memory */
    }


    /*
     * Step 2, see if this is a DECnet address by looking for the optional
     * second colon.
     */
    if (p[1] == ':') {			/* then DECnet format */
	dnet = AuTrue;
	p++;
    }

    /*
     * see if we're allowed to have a DECnet address
     */
#ifndef DNETCONN
    if (dnet) goto bad;
#endif

    
    /*
     * Step 3, find the server number.  This field is required and is 
     * delimited either by a nul
     */

    /* SUPPRESS 530 */
    for (lastp = ++p; *p && isascii(*p) && isdigit(*p); p++) ;
    if ((p == lastp) ||			 /* required field */
	((*p != '.') && (*p != '\0')) || /* invalid non-digit terminator */
	!(psvrnum = copystring (lastp, p - lastp)))  /* no memory */
      goto bad;
    saviserver = iserver = atoi (psvrnum);


    /*
     * At this point, we know the following information:
     *
     *     phostname                hostname string or NULL
     *     iserver                  server number
     *     dnet                     DECnet boolean
     * 
     * We can now decide which transport to use based on the ConnectionFlags
     * build parameter the hostname string.  If phostname is NULL or equals
     * the string "local", then choose the best transport.  If phostname
     * is "unix", then choose BSD UNIX domain sockets (if configured).
     *
     * First, choose default transports:  DECnet else (TCP or STREAMS)
     */


#ifdef DNETCONN
    if (dnet)
      connfunc = MakeDECnetConnection;
    else
#endif
#ifdef TCPCONN
      connfunc = MakeTCPConnection;
#else
#ifdef STREAMSCONN
      connfunc = _AuMakeStreamsConnection;
#else
#ifdef AMOEBA
      connfunc = MakeAmConnection;
#else
      connfunc = NULL;
#endif
#endif
#endif

#if defined(UNIXCONN) && !defined(CLIENTS_LOCALCONN)
    /*
     * Now that the defaults have been established, see if we have any 
     * special names that we have to override:
     *
     *     :N         =>     if UNIXCONN then unix-domain-socket
     *     ::N        =>     if UNIXCONN then unix-domain-socket
     *     unix:N     =>     if UNIXCONN then unix-domain-socket
     *
     * Note that if UNIXCONN isn't defined, then we can use the default
     * transport connection function set above.
     */
    if (!phostname) {
#ifdef apollo
	;   /* Unix domain sockets are *really* bad on apollos */
#else
	connfunc = MakeUNIXSocketConnection;
#endif
    }
    else if (strcmp (phostname, "unix") == 0) {
	connfunc = MakeUNIXSocketConnection;
    }
#endif /* UNIXCONN && !CLIENTS_LOCALCONN */

#ifdef CLIENTS_LOCALCONN
    /*
     *     :N         =>     if CLIENTS_LOCALCONN then streams 
     *     unix:N     =>     if UNIXCONN then unix-domain-socket
     *                                   else streams
     *     <pseudo>:N =>     if CLIENTS_LOCALCONN then specified mode
     */
    if (!phostname) {
	connfunc = MakeLOCALConnection;
	LocalConnType = TYPE_ANY;
    }
#if ABI_ALLOWS_PSEUDO_NAMES
    /*
     * ABI committee has decided to only use 'unix:N' until other pseudo
     * hostnames get blessed by the X Consortium.
     */
    else if ((LocalConnType = ParseLocalName(phostname)) != TYPE_INVALID) {
	connfunc = MakeLOCALConnection;
    }
#else /* ABI_ALLOWS_PSEUDO_NAMES */
    else if (strcmp (phostname, "unix") == 0) {
      connfunc = MakeLOCALConnection;
#ifdef UNIXCONN
      LocalConnType = TYPE_UNIX;
#else
      LocalConnType = TYPE_ANY;
#endif
    }
#endif /* ABI_ALLOWS_PSEUDO_NAMES */
#endif /* CLIENTS_LOCALCONN */

    if (!connfunc)
	goto bad;


#if defined(UNIXCONN) && !defined(CLIENTS_LOCALCONN)
#define LOCALCONNECTION (!phostname || connfunc == MakeUNIXSocketConnection)
#endif

#ifdef CLIENTS_LOCALCONN
#define LOCALCONNECTION (!phostname || connfunc == MakeLOCALConnection)
#endif

#ifndef LOCALCONNECTION
#define LOCALCONNECTION (!phostname)
#endif

    if (LOCALCONNECTION) {
	/*
	 * Get the auth info for local hosts so that it doesn't have to be
	 * repeated everywhere; the particular values in these fields are
	 * not part of the protocol.
	 */
	char hostnamebuf[256];
	int len = _AuGetHostname (hostnamebuf, sizeof hostnamebuf);

	family = AuNetworkLocal;
	if (len > 0) {
	    saddr = Aumalloc (len + 1);
	    if (saddr) {
		strcpy (saddr, hostnamebuf);
		saddrlen = len;
	    } else {
		saddrlen = 0;
	    }
	}
    }
#undef LOCALCONNECTION


    /*
     * Make the connection, also need to get the auth address info for
     * non-local connections.  Do retries in case server host has hit its
     * backlog (which, unfortunately, isn't distinguishable from there not
     * being a server listening at all, which is why we have to not retry
     * too many times).
     */
    if ((fd = (*connfunc) (phostname, &iserver, xname, AU_CONNECTION_RETRIES,
			   &family, &saddrlen, &saddr)) < 0)
      goto bad;
    if (fd >= OPEN_MAX)
	goto bad;

    /*
     * Set close-on-exec so that programs that fork() doesn't get confused.
     */

#if !defined(AMOEBA) && !defined(_MINIX)
#ifdef FIOSNBIO
    {
	int arg = 1;
	ioctl (fd, FIOSNBIO, &arg);
    }
#else
# ifdef FNDELAY
    (void) fcntl (fd, F_SETFL, FNDELAY);
# else
    (void) fcntl (fd, F_SETFL, O_NDELAY);
# endif /* FNDELAY */
#endif /* FIOSNBIO */
#endif /* !AMOEBA && !_MINIX */

#ifdef FD_CLOEXEC
    (void) fcntl (fd, F_SETFD, FD_CLOEXEC);
#else
    (void) fcntl (fd, F_SETFD, 1);
#endif

    /*
     * Build the expanded server name:
     *
     *     [host] : [:] svr \0
     */
    if (xname) {
	newsvrnum[0] = '\0';
	sprintf (newsvrnum, "%d", iserver);
	if (!newsvrnum[0]) 
	    goto bad;
	if (psvrnum) Aufree (psvrnum);
	psvrnum = newsvrnum;
    }

    switch (family) {
      case AuNetworkInternet:
	realtrans = "tcp/";
	break;
      case AuNetworkDECnet:
	realtrans = "decnet/";
	break;
      case AuNetworkLocal:
	realtrans = "local/";
	break;
      default:
	realtrans = "?";
	break;
    }

    /* prefix hostname ':' [':'] servernum */

    len = (strlen(realtrans) + (phostname ? strlen(phostname) : 0) +
	   1 + (dnet ? 1 : 0) + strlen(psvrnum) + 1 );
    *fullnamep = (char *) Aumalloc (len);
    if (!*fullnamep) goto bad;

    sprintf (*fullnamep, "%s%s%s%d",
	     realtrans, (phostname ? phostname : ""),
	     (dnet ? "::" : ":"), iserver);

    *svrnump = iserver;
    if (phostname) Aufree (phostname);
    if (psvrnum && psvrnum != newsvrnum) Aufree (psvrnum);

    GetAuthorization(fd, family, saddr, saddrlen, saviserver,
		     auth_namep, auth_namelenp, auth_datap, auth_datalenp);
    return fd;


    /*
     * error return; make sure everything is cleaned up.
     */
  bad:
    if (fd >= 0) (void) close (fd);
    if (saddr) Aufree (saddr);
    if (phostname) Aufree (phostname);
    if (psvrnum && psvrnum != newsvrnum) Aufree (psvrnum);
    return -1;

}


/*****************************************************************************
 *                                                                           *
 *			   Make Connection Routines                          *
 *                                                                           *
 *****************************************************************************/

#ifndef _MINIX
#ifdef DNETCONN				/* stupid makedepend */
#define NEED_BSDISH
#endif
#ifdef UNIXCONN
#define NEED_BSDISH
#endif
#ifdef TCPCONN
#define NEED_BSDISH
#endif
#endif /* !_MINIX */

#ifdef NEED_BSDISH			/* makedepend can't handle #if */
/*
 * 4.2bsd-based systems
 */
#include <sys/socket.h>

#ifdef hpux
#define NO_TCP_H
#endif
#ifdef MOTOROLA
#ifdef SYSV
#define NO_TCP_H
#endif
#endif
#ifndef NO_TCP_H
#ifdef __OSF1__
#include <sys/param.h>
#endif
#ifdef m88k
#include <sys/stream.h>
#include <netinet/types.h>
#include <netinet/ether.h>
#include <netinet/ip.h>
#include <net/route.h>
#include <net/if.h>
#include <sys/tihdr.h>
#include <sys/tpimux.h>
#endif
#include <netinet/tcp.h>
#endif
#endif /* NEED_BSDISH */


#ifdef DNETCONN
/* ARGSUSED */
static int MakeDECnetConnection (phostname, iserverp, xname, retries,
				 familyp, saddrlenp, saddrp)
    char *phostname;
    int *iserverp;			/* can be RETURN */
    AuBool xname;
    int retries;
    int *familyp;			/* RETURN */
    int *saddrlenp;			/* RETURN */
    char **saddrp;			/* RETURN */
{
    int fd;
    char objname[20];
    extern int dnet_conn();
    struct dn_naddr *dnaddrp, dnaddr;
    struct nodeent *np;

    if (!phostname) phostname = "0";

    /*
     * build the target object name.
     */
    sprintf (objname, "%s%d", AU_DEFAULT_DECNET_TASK, *iserverp);

    /*
     * Attempt to open the DECnet connection, return -1 if fails; ought to
     * do some retries here....
     */
    if ((fd = dnet_conn (phostname, objname, SOCK_STREAM, 0, 0, 0, 0)) < 0) {
	return -1;
    }

    *familyp = AuNetworkDECnet;
    if (dnaddrp = dnet_addr (phostname)) {  /* stolen from xhost */
	dnaddr = *dnaddrp;
    } else {
	if ((np = getnodebyname (phostname)) == NULL) {
	    (void) close (fd);
	    return -1;
	}
	dnaddr.a_len = np->n_length;
	bcopy (np->n_addr, dnaddr.a_addr, np->n_length);
    }

    *saddrlenp = sizeof (struct dn_naddr);
    *saddrp = Aumalloc (*saddrlenp);
    if (!*saddrp) {
	(void) close (fd);
	return -1;
    }
    bcopy ((char *)&dnaddr, *saddrp, *saddrlenp);
    return fd;
}
#endif /* DNETCONN */


#ifdef UNIXCONN
#include <sys/un.h>

/*ARGSUSED*/
static int MakeUNIXSocketConnection (phostname, iserverp, xname, retries,
				     familyp, saddrlenp, saddrp)
    char *phostname;
    int *iserverp;			/* can be RETURN */
    AuBool xname;
    int retries;
    int *familyp;			/* RETURN */
    int *saddrlenp;			/* RETURN */
    char **saddrp;			/* RETURN */
{
    struct sockaddr_un unaddr;		/* UNIX socket data block */
    struct sockaddr *addr;		/* generic socket pointer */
    int addrlen;			/* length of addr */
    int fd;				/* socket file descriptor */
#ifdef hpux /* this is disgusting */
    struct sockaddr_un ounaddr;		/* UNIX socket data block */
    struct sockaddr *oaddr;		/* generic socket pointer */
    int oaddrlen;			/* length of addr */
#endif

    unaddr.sun_family = AF_UNIX;
    sprintf (unaddr.sun_path, "%s%d", AU_DEFAULT_UNIX_PATH, *iserverp);
#ifdef BSD44SOCKETS
    unaddr.sun_len = strlen(unaddr.sun_path);
#endif

    addr = (struct sockaddr *) &unaddr;
#ifdef BSD44SOCKETS
    addrlen = SUN_LEN(&unaddr);
#else
    addrlen = strlen(unaddr.sun_path) + sizeof(unaddr.sun_family);
#endif
  
    /*
     * Open the network connection.
     */
    do {
	if ((fd = socket ((int) addr->sa_family, SOCK_STREAM, 0)) < 0) {
	    return -1;
	}

	if (connect (fd, addr, addrlen) < 0) {
	    int olderrno = errno;
	    (void) close (fd);
#ifdef hpux /* this is disgusting */
	    if (olderrno == ENOENT) {
		fd = socket ((int) oaddr->sa_family, SOCK_STREAM, 0);
		if (fd >= 0) {
		    if (connect (fd, oaddr, oaddrlen) >= 0)
			break;
		    olderrno = errno;
		    (void) close (fd);
		}
	    }
#endif
	    if (olderrno != ENOENT || retries <= 0) {
		errno = olderrno;
		return -1;
	    }
	    sleep (1);
	} else {
	    break;
	}
    } while (retries-- > 0);

    /*
     * Don't need to get auth info since we're local
     */
    return fd;
}
#endif /* UNIXCONN */


#ifdef TCPCONN
static int MakeTCPConnection (phostname, iserverp, xname, retries,
			      familyp, saddrlenp, saddrp)
    char *phostname;
    int *iserverp;			/* can be RETURN */
    AuBool xname;
    int retries;
    int *familyp;			/* RETURN */
    int *saddrlenp;			/* RETURN */
    char **saddrp;			/* RETURN */
{
    char hostnamebuf[256];		/* tmp space */
#ifndef _MINIX
    AuUint32 hostinetaddr;		/* result of inet_addr of arpa addr */
    struct sockaddr_in inaddr;		/* IP socket */
    struct sockaddr *addr;		/* generic socket pointer */
#else /* _MINIX */
    ipaddr_t hostinetaddr;		/* result of inet_addr or arpa addr */
    tcpport_t au_port;			/* the tcp port of the audio server */
    struct rwio_tcpconf tcpconf;	/* for bind operation */
    struct rwio_tcpcl tcpcl;		/* options for connect */
#endif /* !_MINIX */
    int addrlen;			/* length of addr */
    struct hostent *hp;			/* entry in hosts table */
    char *cp;				/* character pointer iterator */
    int fd;				/* file descriptor to return */
    int len;				/* length tmp variable */

#define INVALID_INETADDR ((AuUint32) -1)

    if (!phostname) {
	hostnamebuf[0] = '\0';
	(void) _AuGetHostname (hostnamebuf, sizeof hostnamebuf);
	phostname = hostnamebuf;
    }

    /*
     * if numeric host name then try to parse it as such; do the number
     * first because some systems return garbage instead of INVALID_INETADDR
     */
    if (isascii(phostname[0]) && isdigit(phostname[0])) {
	hostinetaddr = inet_addr (phostname);
    } else {
	hostinetaddr = INVALID_INETADDR;
    }

    /*
     * try numeric
     */
    if (hostinetaddr == INVALID_INETADDR) {
	if ((hp = gethostbyname(phostname)) == NULL) {
	    /* No such host! */
	    return -1;
	}
	if (hp->h_addrtype != AF_INET) {  /* is IP host? */
	    /* Not an Internet host! */
	    return -1;
	}
 
#ifndef _MINIX
	/* Set up the socket data. */
	inaddr.sin_family = hp->h_addrtype;
#if defined(CRAY) && defined(OLDTCP)
	/* Only Cray UNICOS3 and UNICOS4 will define this */
	{
	    AuInt32 t;
	    bcopy ((char *)hp->h_addr, (char *)&t, sizeof(t));
	    inaddr.sin_addr = t;
	}
#else
	bcopy ((char *)hp->h_addr, (char *)&inaddr.sin_addr, 
	       sizeof(inaddr.sin_addr));
#endif /* CRAY and OLDTCP */
#else /* _MINIX */
	bcopy ((char *)hp->hp_addr, &hostinetaddr, sizeof(hostinetaddr));
#endif /* !_MINIX */
    } else {
#ifndef _MINIX
#if defined(CRAY) && defined(OLDTCP)
	/* Only Cray UNICOS3 and UNICOS4 will define this */
	inaddr.sin_addr = hostinetaddr;
#else
	inaddr.sin_addr.s_addr = hostinetaddr;
#endif /* CRAY and OLDTCP */
	inaddr.sin_family = AF_INET;
#endif /* _MINX */
    }

#ifndef _MINIX
    addr = (struct sockaddr *) &inaddr;
    addrlen = sizeof (struct sockaddr_in);
#ifdef BSD44SOCKETS
    inaddr.sin_len = addrlen;
#endif
    if (xname)				/* name came from X */
	*iserverp += AU_DEFAULT_TCP_PORT;
    inaddr.sin_port = *iserverp;
    inaddr.sin_port = htons (inaddr.sin_port);	/* may be funky macro */

    /*
     * Open the network connection.
     */
    do {
	if ((fd = socket ((int) addr->sa_family, SOCK_STREAM, 0)) < 0) {
	    return -1;
	}

	/*
	 * turn off TCP coalescence
	 */
#ifdef TCP_NODELAY
	{
	    int tmp = 1;
	    setsockopt (fd, IPPROTO_TCP, TCP_NODELAY, (char *)&tmp, 
		sizeof (int));
	}
#endif

	/*
	 * connect to the socket; if there is no X server or if the backlog has
	 * been reached, then ECONNREFUSED will be returned.
	 */
	if (connect (fd, addr, addrlen) < 0) {
	    int olderrno = errno;
	    (void) close (fd);
	    if (olderrno != ECONNREFUSED || retries <= 0) {
		errno = olderrno;
		return -1;
	    }
	    sleep (1);
	} else {
	    break;
	}
    } while (retries-- > 0);
#else /* _MINIX */
    au_port = htons(*iserverp);

    /*
     * Open the network connection.
     */
    do {
	fd= open("/dev/tcp", O_RDWR);
	if (fd == -1)
	    return -1;


	tcpconf.nwtc_flags= NWTC_EXCL | NWTC_LP_SEL | NWTC_SET_RA | NWTC_SET_RP;
	tcpconf.nwtc_remaddr= hostinetaddr;
	tcpconf.nwtc_remport= au_port;

	if (ioctl(fd, NWIOSTCPCONF, &tcpconf) == -1)
	{
	    int olderrno;

	    olderrno= errno;
	    close(fd);
	    errno= olderrno;
	    return -1;
	}
 
	/*
	 * connect to the socket; if there is no audio server or if the backlog has
	 * been reached, then ECONNREFUSED will be returned.
	 */
	tcpcl.nwtcl_flags= 0;
	if (ioctl(fd, NWIOTCPCONN, &tcpcl) == -1)
	{
	    int olderrno = errno;
	    (void) close (fd);
	    if (olderrno != ECONNREFUSED || retries <= 0) {
		errno = olderrno;
		return -1;
	    }
	    sleep (1);
	} else {
	    break;
	}
    } while (retries-- > 0);
#endif /* _MINIX */

    /*
     * Success!  So, save the auth information
     */
#ifdef CRAY
#ifdef OLDTCP
    len = sizeof(inaddr.sin_addr);
#else
    len = SIZEOF_in_addr;
#endif /* OLDTCP */
    cp = (char *) &inaddr.sin_addr;
#else /* else not CRAY */
#ifndef _MINIX
    len = sizeof(inaddr.sin_addr.s_addr);
    cp = (char *) &inaddr.sin_addr.s_addr;
#else
    len = sizeof(hostinetaddr);
    cp = (char *) &hostinetaddr;
#endif /* !_MINIX */
#endif /* CRAY */

    /*
     * We are special casing the BSD hack localhost address
     * 127.0.0.1, since this address shouldn't be copied to
     * other machines.  So, we simply omit generating the auth info
     * since we set it to the local machine before calling this routine!
     */
    if (!((len == 4) && (cp[0] == 127) && (cp[1] == 0) &&
	  (cp[2] == 0) && (cp[3] == 1))) {
	*saddrp = Aumalloc (len);
	if (*saddrp) {
	    *saddrlenp = len;
	    bcopy (cp, *saddrp, len);
	    *familyp = AuNetworkInternet;
	} else {
	    *saddrlenp = 0;
	}
    }

    return fd;
}
#undef INVALID_INETADDR
#endif /* TCPCONN */

#ifdef CLIENTS_LOCALCONN

#include <signal.h>
#ifdef UNIXCONN
# define AUDIO_ISC_PATH "/tmp/.ISC-unix/Au"
#else
# define AUDIO_ISC_PATH "/tmp/.X11-unix/Au"
#endif
#define AUDIO_XSIGHT_PATH "/dev/Au"
#define AUDIO_ATT_PATH "/dev/Au/server."
#define AUDIO_NSTREAMS_PATH "/dev/Au/Nserver."

#ifdef SVR4
static char Def_Local_Search[] = "UNIX:NAMED:USL:SCO:ISC";
#endif /* SVR4 */
#ifdef ISC
static char Def_Local_Search[] = "ISC:USL:UNIX";
#endif /* ISC */
#ifdef SCO
static char Def_Local_Search[] = "SCO:USL:UNIX";
#endif /* SCO */
#ifdef ATT
static char Def_Local_Search[] = "USL:UNIX";
#endif /* ATT */

static int ParseLocalName(name)
    char *name;
{
    struct conntype *p;

    p = LocalConnModes;
    while (*(p->pseudo_host) != '\0') {
	if ((strcmp(name, p->pseudo_host) == 0) ||
	    (strcmp(name, p->env_name) == 0))
	    return(p->conn_type);
	p++;
    }
    return(TYPE_INVALID);
}

static void _dummy(temp)
    int temp;
{
    return;
}

static int MakeLOCALConnection (phostname, iserverp, retries,
				familyp, saddrlenp, saddrp)
    char *phostname;
    int *iserverp;			/* can be RETURN */
    int retries;
    int *familyp;			/* RETURN */
    int *saddrlenp;			/* RETURN */
    char **saddrp;			/* RETURN */
{
    int fds, fd, server, fl, ret;
    unsigned alarm_time;
    void (*savef)();
    char rbuf[64], *slave;
    char server_path[64];
    long temp;
    char *to_try;
    extern char *getenv();
    int conn_mode;
#if !defined (SVR4) || defined(SVR4_ACP)
    struct strfdinsert buf;
    struct strbuf ctlbuf;
#endif

    /*
     * Get list of local connection modes to try.
     */
    if (LocalConnType == TYPE_ANY) {
	/*
	 * First check XLOCAL environment variable, then use default path.
	 */
	to_try = getenv("XLOCAL");
	if (!to_try) {
	    to_try = (char *)Aumalloc(strlen(Def_Local_Search)+1);
	    strcpy(to_try,Def_Local_Search);
	}
    }
    else {
	to_try = (char *)Aumalloc(strlen(LocalConnModes[LocalConnType].env_name)+1);
	strcpy(to_try,LocalConnModes[LocalConnType].env_name);
    }

    /* 
     * Now try each of the modes, in the search path.  If the user specifies
     * a mode that is not supported, it will be silently ignored, and the
     * connection will fail later on.
     */
    to_try = strtok(to_try, ": ");
    while (to_try) {
	switch (ParseLocalName(to_try)) {
	case TYPE_UNIX:
#ifdef UNIXCONN
    	    fd = MakeUNIXSocketConnection(phostname, iserverp, retries,
				          familyp, saddrlenp, saddrp);
	    if (fd >= 0)
		return(fd);
#endif /* UNIXCONN */
	    break;
	case TYPE_USL:
  	    /*
   	     * USL's old-style local connection (SVR3&SVR4)
   	     */
  	    sprintf(server_path,"%s%d", AUDIO_ATT_PATH, *iserverp);
            if ((server = open(server_path, O_RDWR)) >= 0) {
      		if ((fd = open("/dev/ptmx", O_RDWR)) >= 0) {
		    grantpt(fd);
        	    unlockpt(fd);
        	    slave = (char *)ptsname(fd);  /* get name */

        	    /*
         	     * write slave name to server
         	     */
		    temp = strlen(slave);
		    rbuf[0] = temp;
		    sprintf(&rbuf[1], slave);
        	    write(server, rbuf, temp+1);

        	    /*
         	     * wait for server to respond
         	     */
        	    savef = signal(SIGALRM, _dummy);
        	    alarm_time = alarm (30);
        	    ret = read(fd, rbuf, 1);
		    alarm(alarm_time);
		    signal(SIGALRM, savef);
        	    if (ret == 1) {
			/*
			 * Success!
			 */
	  		(void) close(server);
	  		return(fd);
		    }
      		}
      		(void) close(server);
      		(void) close(fd);
    	    }
	    break;
	case TYPE_NAMED:
#ifdef SVR4
  	    /*
   	     * USL's new-style local connection (SVR4)
   	     */
  	    sprintf(server_path,"%s%d", AUDIO_NSTREAMS_PATH, *iserverp);
  	    if ((fd = open(server_path, O_RDWR)) >= 0) {
		if (isastream(fd) == 1) {
		    /*
		     * Success!
		     */
		    return(fd);
		}
	    }
#endif /* SVR4 */
	    break;
	case TYPE_ISC:
#if !defined(SVR4) || defined(SVR4_ACP)
  	    /*
   	     * ISC's local connection (ISC or SVR4 w/ACP)
   	     */
  	    sprintf(server_path,"%s%d", AUDIO_ISC_PATH, *iserverp);
  	    if ((server = open(server_path, O_RDWR)) >= 0) {
                if ((fds = open("/dev/spx", O_RDWR)) >= 0 &&
	            (fd  = open("/dev/spx", O_RDWR)) >= 0) {
		    /*
	 	     * make a STREAMS-pipe
	 	     */
		    buf.databuf.maxlen = -1;
		    buf.databuf.len = -1;
		    buf.databuf.buf = NULL;
		    buf.ctlbuf.maxlen = sizeof(long);
		    buf.ctlbuf.len = sizeof(long);
		    buf.ctlbuf.buf = (caddr_t)&temp;
		    buf.offset = 0;
		    buf.fildes = fd;
		    buf.flags = 0;
	
		    if (ioctl(fds, I_FDINSERT, &buf) != -1 &&
	    	        ioctl(server, I_SENDFD, fds) != -1) {
			/*
			 * Success!
			 */
	    	        (void) close(fds); 
	    	        (void) close(server);
	    	        return (fd);
	  	    }
      	        }
      	        (void) close(server);
      	        (void) close(fds);
      	        (void) close(fd);
    	    }
#endif /* !SVR4 || SVR4_ACP */
	    break;
	case TYPE_SCO:
#if !defined(SVR4) || defined(SVR4_ACP)
  	    /*
   	     * SCO Xsight's local connection (SCO or SVR4 w/ACP)
   	     */
  	    sprintf(server_path,"%s%1dR", AUDIO_XSIGHT_PATH, *iserverp);
  	    if ((server = open(server_path, O_RDWR)) >= 0) {
      		if ((fd = open("/dev/spx", O_RDWR)) >= 0) {
		    (void) write(server, (char *) &server, 1);
		    ctlbuf.len = 0;
		    ctlbuf.maxlen = sizeof(long);
		    ctlbuf.buf = (caddr_t)&temp;
		    fl = 0;
		    /*
		     * Wait for server to respond
		     */
		    savef = signal(SIGALRM, _dummy);
		    alarm_time = alarm(10);
		    ret =  getmsg(server, &ctlbuf, 0, &fl);
		    alarm(alarm_time);
		    signal(SIGALRM, savef);
	    
		    if (ret >= 0) {
	  	        /* 
			 * Success!
			 *
			 * The msg we got via getmsg is the result of an 
			 * I_FDINSERT, so if we do a putmsg with whatever 
			 * we recieved, we're doing another I_FDINSERT ...
	   		 */
	  		putmsg(fd, &ctlbuf, 0, 0);
	  		fcntl(fd, F_SETFL, fcntl(fd, F_GETFL, 0) | O_NDELAY);
	  	 	(void) close(server);
	  	 	return(fd);
		    }
		    (void) close (server);
		    (void) close (fd);
      	    	}
    	    }
#endif /* !SVR4 || SVR4_ACP */
	    break;
	default:
	    break;
	}

	/*
	 * Get the next token...
	 */
	to_try = strtok(NULL, ": ");
    }

    return (-1);
}
#endif /* CLIENTS_LOCALCONN */


#ifdef AMOEBA
/*
 * Make an Amoeba TCP/IP or virtual circuit connection
 */
#include <assert.h>
#include "XAmoeba.h"
#include <server/ip/hton.h>
#include <server/ip/types.h>
#include <server/ip/gen/in.h>
#include <server/ip/gen/tcp.h>
#include <server/ip/tcpip.h>
#include <server/ip/tcp_io.h>
#include <server/ip/gen/tcp_io.h>
#include <server/ip/gen/netdb.h>

#define	THREAD_STACK_SIZE (4*1024)

char *getenv();
void XAmReaderThread();

static int
MakeAmConnection(phostname, iserverp, retries, familyp, saddrlenp, saddrp)
    char *phostname;
    int *iserverp;			/* can be RETURN */
    int retries;
    int *familyp;			/* RETURN */
    int *saddrlenp;			/* RETURN */
    char **saddrp;			/* RETURN */
{
    capability xservercap;
    char xserverpath[256];
    XAmChanDesc *chandesc;
    errstat err;

    /* Amoeba requires a server hostname */
    if (phostname == NULL || *phostname == '\0') {
	fprintf(stderr, "audiolib: audio server host name expected\n");
	return -1;
    }

    /* allocate channel descriptor */
    chandesc = XAmAllocChanDesc();
    if (chandesc == (XAmChanDesc *)NULL) {
	fprintf(stderr, "audiolib: Out of channel capabilities\n");
	return -1;
    }

    /*
     * There are two possible way to make a connection on Amoeba. Either
     * through an Amoeba RPC or a TCP/IP connection. Depending on whether
     * the audio server resides on Amoeba, Amoeba RPC's are used. Otherwise
     * it uses a TCP/IP connection.
     */
    (void)sprintf(xserverpath, "%s/%s:%d", DEF_AUSVRDIR, phostname, *iserverp);
    if ((err = name_lookup(xserverpath, &xservercap)) == STD_OK) {
	port vccaps[2];
	bufsize size;
	errstat err;
	header hdr;

	/* Amoeba virtual circuit connection */
	chandesc->type = ACDT_VIRTCIRC;

	/* get the two connection ports from the audio-server */
	hdr.h_command = AX_CONNECT;
	hdr.h_port = xservercap.cap_port;
	hdr.h_priv = xservercap.cap_priv;
	size = trans(&hdr, NILBUF, 0, &hdr, (char *)vccaps, sizeof(vccaps));
	if (ERR_STATUS(size)) {
	    err = ERR_CONVERT(size);
	} else {
	    err = ERR_CONVERT(hdr.h_status);
	}
	if (err != STD_OK || size != sizeof(vccaps)) {
	    fprintf(stderr, "audiolib: connect to Amoeba audio-server failed (%s)\n",
		    err_why(err));
	    XAmFreeChanDesc(chandesc);
	    return -1;
	}

	/* setup an Amoeba virtual circuit */
	chandesc->virtcirc =
	    vc_create(&vccaps[1], &vccaps[0], MAXBUFSIZE, MAXBUFSIZE);
	if (chandesc->virtcirc == (struct vc *)NULL) {
	    fprintf(stderr, "audiolib: Amoeba virtual circuit create failed\n");
	    XAmFreeChanDesc(chandesc);
	    return -1;
	}

	/*
	 * Special Amoeba family type. For Amoeba no access control
	 * mechanism exists, when you have the capability you have
	 * the access.
	 */
	*familyp = FamilyAmoeba;
	*saddrlenp = strlen(phostname);
	*saddrp = copystring(phostname, *saddrlenp);
    } else {
	char tcpname[100];
	capability tcpcap;
	ipaddr_t ipaddr;
	char *tcpsvr;
	nwio_tcpcl_t tcpcl;
	nwio_tcpconf_t tcpconf;
	XAmChanDesc **param;
	int result;

	/* Amoeba TCP/IP connection */
	chandesc->type = ACDT_TCPIP;

	/* lookup up TCP/IP server */
	if ((tcpsvr = getenv("TCP_SERVER")) == NULL)
	    tcpsvr = TCP_SVR_NAME;
	if ((err = name_lookup(tcpsvr, &tcpcap)) != STD_OK) {
	    fprintf(stderr, "audiolib: Cannot lookup %s (%s)\n",
		tcpsvr, err_why(err));
	    std_destroy(&chandesc->chancap);
	    XAmFreeChanDesc(chandesc);
	    return -1;
	}

	/* establish TCP/IP connection */
	if ((err = tcpip_open(&tcpcap, &chandesc->chancap)) != STD_OK) {
	    fprintf(stderr, "audiolib: Cannot open TCP/IP server on %s (%s)\n",
		tcpsvr, tcpip_why(err));
	    std_destroy(&chandesc->chancap);
	    XAmFreeChanDesc(chandesc);
	    return -1;
	}

	/* lookup TCP/IP hostname */
	if (isdigit(phostname[0])) {
	    ipaddr = inet_addr(phostname);
	} else {
	    struct hostent *hp = gethostbyname(phostname);
	    if (hp == NULL) {
		fprintf(stderr, "audiolib: %s unknown host\n", phostname);
		return -1;
	    }
	    bcopy(hp->h_addr, &ipaddr, hp->h_length);
	}

	/* set remote address/port on the TCP/IP connection */
	tcpconf.nwtc_flags = NWTC_SET_RA|NWTC_SET_RP|NWTC_LP_SEL;
	tcpconf.nwtc_remaddr = ipaddr;
	tcpconf.nwtc_remport = htons(6000+*iserverp);
	if ((err = tcp_ioc_setconf(&chandesc->chancap, &tcpconf)) != STD_OK) {
	    fprintf(stderr, "audiolib: Cannot configure TCP/IP server (%s)\n",
		tcpip_why(err));
	    std_destroy(&chandesc->chancap);
	    XAmFreeChanDesc(chandesc);
	    return -1;
	}

	/* make the actual TCP/IP connection */
	tcpcl.nwtcl_flags = 0;
	if ((err = tcp_ioc_connect(&chandesc->chancap, &tcpcl)) != STD_OK) {
	    fprintf(stderr, "audiolib: Cannot make TCP/IP connection (%s)\n",
		tcpip_why(err));
	    std_destroy(&chandesc->chancap);
	    XAmFreeChanDesc(chandesc);
	    return -1;
	}

	/* start TCP/IP reader thread */
	chandesc->signal = sig_uniq();
	chandesc->circbuf = cb_alloc(CIRCBUFSIZE);
	param = Aumalloc(sizeof(XAmChanDesc *));
	*param = chandesc; /* stupid convention */
	result = thread_newthread(XAmReaderThread,
	    THREAD_STACK_SIZE, (char *)param, sizeof(XAmChanDesc *));
	if (result == 0) {
	    fprintf(stderr, "audiolib: Cannot start reader thread\n");
	    std_destroy(&chandesc->chancap);
	    XAmFreeChanDesc(chandesc);
	    return -1;
	}
	threadswitch(); /* give reader a try */

	/*
	 * Family type is set to Internet so that the .Xauthority
	 * files from Unix will work under Amoeba (for Unix displays).
	 */
	*familyp = FamilyInternet;
	*saddrlenp = sizeof(ipaddr_t);
	*saddrp = Aumalloc(sizeof(ipaddr_t));
	bcopy((char *)&ipaddr, *saddrp, sizeof(ipaddr_t));
    }

    return XAmChanDescToFd(chandesc);
}
#endif /* AMOEBA */

/*****************************************************************************
 *                                                                           *
 *			  Connection Utility Routines                        *
 *                                                                           *
 *****************************************************************************/

/* 
 * Disconnect from server.
 */

int _AuDisconnectServer (server)

    int server;

{
#ifdef AMOEBA
    register XAmChanDesc *chandesc;

    chandesc = XAmFdToChanDesc(server);
    if (chandesc->type == ACDT_TCPIP) {
      sig_raise(chandesc->signal);
      std_destroy(&chandesc->chancap);
    }
    if (chandesc->type == ACDT_VIRTCIRC)
      vc_close(chandesc->virtcirc, VC_BOTH);
    XAmFreeChanDesc(chandesc);
#else
    (void) close(server);
#endif
    return 0;
}



static int padlength[4] = {0, 3, 2, 1};	 /* make sure auth is multiple of 4 */

AuBool
_AuSendClientPrefix (aud, client, auth_proto, auth_string)
     AuServer *aud;
     auConnClientPrefix *client;	/* contains count for auth_* */
     char *auth_proto, *auth_string;	/* NOT null-terminated */
{
    int auth_length = client->nbytesAuthProto;
    int auth_strlen = client->nbytesAuthString;
    char padbuf[3];			/* for padding to 4x bytes */
    int pad;
    struct iovec iovarray[5], *iov = iovarray;
    int niov = 0;
    int len = 0;

#define add_to_iov(b,l) \
  { iov->iov_base = (b); iov->iov_len = (l); iov++, niov++; len += (l); }

    add_to_iov ((caddr_t) client, SIZEOF(auConnClientPrefix));

    /*
     * write authorization protocol name and data
     */
    if (auth_length > 0) {
	add_to_iov (auth_proto, auth_length);
	pad = padlength [auth_length & 3];
	if (pad) add_to_iov (padbuf, pad);
    }
    if (auth_strlen > 0) {
	add_to_iov (auth_string, auth_strlen);
	pad = padlength [auth_strlen & 3];
	if (pad) add_to_iov (padbuf, pad);
    }

#undef add_to_iov

#ifndef _MINIX
    len -= WritevToServer (aud->fd, iovarray, niov);
#else
    len -= MNX_WritevToServer (aud, iovarray, niov);
#endif

#if !defined(AMOEBA) && !defined(_MINIX)
    /*
     * Set the connection non-blocking since we use select() to block.
     */
    /* ultrix reads hang on Unix sockets, hpux reads fail */
#if defined(O_NONBLOCK) && (!defined(SCO) && !defined(ultrix) && !defined(hpux) && !defined(AIXV3) && !defined(uniosu))
    (void) fcntl (aud->fd, F_SETFL, O_NONBLOCK);
#else
#ifdef FIOSNBIO
    {
	int arg = 1;
	ioctl (aud->fd, FIOSNBIO, &arg);
    }
#else
#if (defined(AIXV3) || defined(uniosu)) && defined(FIONBIO)
    {
	int arg;
	arg = 1;
	ioctl(aud->fd, FIONBIO, &arg);
    }
#else
    (void) fcntl (aud->fd, F_SETFL, FNDELAY);
#endif
#endif
#endif
#else /* !AMOEBA && !_MINIX */
#ifdef _MINIX
    {
	int flags, r;

	flags= fcntl(dpy->fd, F_GETFD);
	if (flags == -1)
	    return 0;
	flags |= FD_ASYNCHIO;
	r= fcntl(dpy->fd, F_SETFD, flags);
	if (r == -1)
	    return 0;
    }
#endif /* _MINIX */
#endif /* !AMOEBA && !_MINIX */
    return len == 0;
}


#ifdef STREAMSCONN
#ifdef SVR4
#include <tiuser.h>
#else
#undef HASXDMAUTH
#endif
#endif

#ifdef SECURE_RPC
#include <rpc/rpc.h>
#ifdef ultrix
#include <time.h>
#include <rpc/auth_des.h>
#endif
#endif

/*
 * This is a list of the authorization names which Xlib currently supports.
 * Xau will choose the file entry which matches the earliest entry in this
 * array, allowing us to prioritize these in terms of the most secure first
 */

static char *default_xauth_names[] = {
#ifdef SECURE_RPC
    "SUN-DES-1",
#endif
#ifdef HASXDMAUTH
    "XDM-AUTHORIZATION-1",
#endif
    "MIT-MAGIC-COOKIE-1"
};

static int default_xauth_lengths[] = {
#ifdef SECURE_RPC
    9,	    /* strlen ("SUN-DES-1") */
#endif
#ifdef HASXDMAUTH
    19,	    /* strlen ("XDM-AUTHORIZATION-1") */
#endif
    18	    /* strlen ("MIT-MAGIC-COOKIE-1") */
};

#define NUM_DEFAULT_AUTH    (sizeof (default_xauth_names) / sizeof (default_xauth_names[0]))
    

#ifdef SECURE_RPC
/*
 * Create a credential that we can send to the X server.
 */
static int
auth_ezencode(servername, window, cred_out, len)
        char           *servername;
        int             window;
	char	       *cred_out;
        int            *len;
{
        AUTH           *a;
        XDR             xdr;

        a = authdes_create(servername, window, NULL, NULL);
        if (a == (AUTH *)NULL) {
                perror("auth_create");
                return 0;
        }
        xdrmem_create(&xdr, cred_out, *len, XDR_ENCODE);
        if (AUTH_MARSHALL(a, &xdr) == FALSE) {
                perror("auth_marshall");
                AUTH_DESTROY(a);
                return 0;
        }
        *len = xdr_getpos(&xdr);
        AUTH_DESTROY(a);
	return 1;
}
#endif

#if XlibSpecificationRelease < 5
/* pre-X11R5 compatability */

Xauth *XauGetBestAuthByAddr(
#if NeedFunctionPrototypes
#ifndef _Xconst
#if __STDC__ || defined(__cplusplus) || defined(c_plusplus) || (FUNCPROTO&4)
#define _Xconst const
#else
#define _Xconst
#endif
#endif /* _Xconst */

#if NeedWidePrototypes
unsigned int	/* family */,
unsigned int	/* address_length */,
#else
unsigned short	/* family */,
unsigned short	/* address_length */,
#endif
_Xconst char*	/* address */,
#if NeedWidePrototypes
unsigned int	/* number_length */,
#else
unsigned short	/* number_length */,
#endif
_Xconst char*	/* number */,
int		/* types_length */,
char**		/* type_names */,
_Xconst int*	/* type_lengths */
#endif
);

#endif /* XlibSpecificationRelease < 5 */

/* ARGSUSED */
static void
GetAuthorization(fd, family, saddr, saddrlen, iserver,
		 auth_namep, auth_namelenp, auth_datap, auth_datalenp)
    int fd;
    int family;
    int saddrlen;
    int iserver;
    char *saddr;
    char **auth_namep;			/* RETURN */
    int *auth_namelenp;			/* RETURN */
    char **auth_datap;			/* RETURN */
    int *auth_datalenp;			/* RETURN */
{
#ifdef SECURE_RPC
    char rpc_cred[MAX_AUTH_BYTES];
#endif
#ifdef HASXDMAUTH
    char xdmcp_data[192/8];
#endif
    char *auth_name = *auth_namep;
    int auth_namelen = *auth_namelenp;
    char *auth_data = *auth_datap;
    int auth_datalen = *auth_datalenp;
    Xauth *authptr = NULL;

/*
 * Look up the authorization protocol name and data if necessary.
 */
    if (!(auth_name && auth_data)) {
	char svrnumbuf[40];		/* big enough to hold 2^64 and more */
	(void) sprintf (svrnumbuf, "%d", iserver);

	authptr = XauGetBestAuthByAddr ((unsigned short) family,
				    (unsigned short) saddrlen,
				    saddr,
				    (unsigned short) strlen (svrnumbuf),
				    svrnumbuf,
				    NUM_DEFAULT_AUTH,
				    default_xauth_names,
				    default_xauth_lengths);
	if (authptr) {
	    auth_namelen = authptr->name_length;
	    auth_name = (char *)authptr->name;
	    auth_datalen = authptr->data_length;
	    auth_data = (char *)authptr->data;
	} else {
	    auth_namelen = 0;
	    auth_name = NULL;
	    auth_datalen = 0;
	    auth_data = NULL;
	}
    }
#ifdef HASXDMAUTH
    /*
     * build XDM-AUTHORIZATION-1 data
     */
    if (auth_namelen == 19 && !strncmp (auth_name, "XDM-AUTHORIZATION-1", 19))
    {
	int     j;
	AuInt32    now;
	for (j = 0; j < 8; j++)
	    xdmcp_data[j] = auth_data[j];
#ifdef STREAMSCONN /* && SVR4 */
	{
	    int			i;
	    struct netbuf	netb;
	    char		addrret[1024];

	    netb.maxlen = sizeof addrret;
	    netb.buf = addrret;
	    if (t_getname (fd, &netb, LOCALNAME) == -1)
		t_error ("t_getname");
	    /*
	     * XXX - assumes that the return data
	     * are in a struct sockaddr_in, and that
	     * the data structure is layed out in
	     * the normal fashion.  This WILL NOT WORK
	     * on a non 32-bit machine (same in Xstreams.c)
	     */
	    for (i = 4; i < 8; i++)
		xdmcp_data[j++] = netb.buf[i];
	    for (i = 2; i < 4; i++)
		xdmcp_data[j++] = netb.buf[i];
	}
#else
	{
	    AuUint32	addr;
	    unsigned short	port;
#ifdef TCPCONN
	    int	    addrlen;
	    struct sockaddr_in	in_addr;

#if defined(SYSV) && defined(SYSV386) && defined(CLIENTS_LOCALCONN)
	    if(family == FamilyLocal) goto UseLocalID;
#endif
	    addrlen = sizeof (in_addr);
	    if (getsockname (fd,
			     (struct sockaddr *) &in_addr,
			     &addrlen) != -1 &&
		addrlen >= sizeof in_addr &&
		in_addr.sin_family == AF_INET)
	    {
		addr = ntohl (in_addr.sin_addr.s_addr);
		port = ntohs (in_addr.sin_port);
	    }
	    else
UseLocalID:
#endif
	    {
		static AuUint32	unix_addr = 0xFFFFFFFF;
		addr = unix_addr--;
		port = getpid ();
	    }
	    xdmcp_data[j++] = (addr >> 24) & 0xFF;
	    xdmcp_data[j++] = (addr >> 16) & 0xFF;
	    xdmcp_data[j++] = (addr >>  8) & 0xFF;
	    xdmcp_data[j++] = (addr >>  0) & 0xFF;
	    xdmcp_data[j++] = (port >>  8) & 0xFF;
	    xdmcp_data[j++] = (port >>  0) & 0xFF;
	}
#endif
	time (&now);
	xdmcp_data[j++] = (now >> 24) & 0xFF;
	xdmcp_data[j++] = (now >> 16) & 0xFF;
	xdmcp_data[j++] = (now >>  8) & 0xFF;
	xdmcp_data[j++] = (now >>  0) & 0xFF;
	while (j < 192 / 8)
	    xdmcp_data[j++] = 0;
	XdmcpWrap (xdmcp_data, auth_data + 8,
		      xdmcp_data, j);
	auth_data = xdmcp_data;
	auth_datalen = j;
    }
#endif /* HASXDMAUTH */
#ifdef SECURE_RPC
    /*
     * The SUN-DES-1 authorization protocol uses the
     * "secure RPC" mechanism in SunOS 4.0+.
     */
    if (auth_namelen == 9 && !strncmp(auth_name, "SUN-DES-1", 9)) {
	char servernetname[MAXNETNAMELEN + 1];

	/*
	 * Copy over the server's netname from the authorization
	 * data field filled in by XauGetAuthByAddr().
	 */
	if (auth_datalen > MAXNETNAMELEN) {
	    auth_datalen = 0;
	    auth_data = NULL;
	} else {
	    bcopy(auth_data, servernetname, auth_datalen);
	    servernetname[auth_datalen] = '\0';

	    auth_datalen = sizeof (rpc_cred);
	    if (auth_ezencode(servernetname, 100, rpc_cred,
			      &auth_datalen))
		auth_data = rpc_cred;
	    else
		auth_data = NULL;
	}
    }
#endif
    if (saddr) Aufree (saddr);
    if ((*auth_namelenp = auth_namelen) != 0)
    {
	if ((*auth_namep = Aumalloc(auth_namelen)) != NULL)
	    bcopy(auth_name, *auth_namep, auth_namelen);
	else
	    *auth_namelenp = 0;
    }
    else
	*auth_namep = NULL;
    if ((*auth_datalenp = auth_datalen) != 0)
    {
	if ((*auth_datap = Aumalloc(auth_datalen)) != NULL)
	    bcopy(auth_data, *auth_datap, auth_datalen);
	else
	    *auth_datalenp = 0;
    }
    else
	*auth_datap = NULL;
    if (authptr) XauDisposeAuth (authptr);
}
