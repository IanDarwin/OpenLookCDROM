/* $XConsortium: socket.c,v 1.7 94/04/17 20:40:30 eswu Exp $ */
/*

Copyright (c) 1988  X Consortium

Permission is hereby granted, free of charge, to any person obtaining
a copy of this software and associated documentation files (the
"Software"), to deal in the Software without restriction, including
without limitation the rights to use, copy, modify, merge, publish,
distribute, sublicense, and/or sell copies of the Software, and to
permit persons to whom the Software is furnished to do so, subject to
the following conditions:

The above copyright notice and this permission notice shall be included
in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
IN NO EVENT SHALL THE X CONSORTIUM BE LIABLE FOR ANY CLAIM, DAMAGES OR
OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,
ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
OTHER DEALINGS IN THE SOFTWARE.

Except as contained in this notice, the name of the X Consortium shall
not be used in advertising or otherwise to promote the sale, use or
other dealings in this Software without prior written authorization
from the X Consortium.

*/

/*
 * THIS IS AN OS DEPENDENT FILE! It should work on 4.2BSD derived
 * systems.  VMS and System V should plan to have their own version.
 *
 * This code was cribbed from lib/X/XConnDis.c.
 * Compile using   
 *                    % cc -c socket.c -DUNIXCONN
 */

#include <stdio.h>
#include <X11/Xos.h>
#include <X11/Xproto.h>
#include <errno.h>
#include <netinet/in.h>
#include <sys/ioctl.h>
#include <netdb.h> 
#include <sys/socket.h>
#ifndef hpux
#include <netinet/tcp.h>
#endif

extern int errno;		/* Certain (broken) OS's don't have this */
				/* decl in errno.h */

#ifdef UNIXCONN
#include <sys/un.h>
#ifndef X_UNIX_PATH
#ifdef hpux
#define X_UNIX_PATH "/usr/spool/sockets/X11/"
#define OLD_UNIX_PATH "/tmp/.X11-unix/X"
#else /* hpux */
#define X_UNIX_PATH "/tmp/.X11-unix/X"
#endif /* hpux */
#endif /* X_UNIX_PATH */
#endif /* UNIXCONN */

#ifndef hpux
void bcopy();
#endif /* hpux */

/* 
 * Attempts to connect to server, given host and display. Returns file 
 * descriptor (network socket) or 0 if connection fails.
 */

int connect_to_server (host, display)
     char *host;
     int display;
{
  struct sockaddr_in inaddr;	/* INET socket address. */
  struct sockaddr *addr;		/* address to connect to */
  struct hostent *host_ptr;
  int addrlen;			/* length of address */
#ifdef UNIXCONN
  struct sockaddr_un unaddr;	/* UNIX socket address. */
#endif
  extern char *getenv();
  extern struct hostent *gethostbyname();
  int fd;				/* Network socket */
  {
#ifdef UNIXCONN
    if ((host[0] == '\0') || (strcmp("unix", host) == 0)) {
	/* Connect locally using Unix domain. */
	unaddr.sun_family = AF_UNIX;
	(void) strcpy(unaddr.sun_path, X_UNIX_PATH);
	(void) sprintf(&unaddr.sun_path[strlen(unaddr.sun_path)], "%d", display);
	addr = (struct sockaddr *) &unaddr;
#ifdef BSD44SOCKETS
	unaddr.sun_len = strlen(unaddr.sun_path);
	addrlen = SUN_LEN(&unaddr);
#else
	addrlen = strlen(unaddr.sun_path) + 2;
#endif
	/*
	 * Open the network connection.
	 */
	if ((fd = socket((int) addr->sa_family, SOCK_STREAM, 0)) < 0) {
#ifdef hpux /* this is disgusting */  /* cribbed from X11R4 xlib source */
  	    if (errno == ENOENT) {  /* No such file or directory */
	      (void) sprintf(unaddr.sun_path, "%s%d", OLD_UNIX_PATH, display);
              addrlen = strlen(unaddr.sun_path) + 2;
              if ((fd = socket ((int) addr->sa_family, SOCK_STREAM, 0)) < 0)
                return(-1);     /* errno set by most recent system call. */
	    } else 
#endif /* hpux */
	    return(-1);	    /* errno set by system call. */
        }
    } else 
#endif /* UNIXCONN */
    {
      /* Get the statistics on the specified host. */
      if ((inaddr.sin_addr.s_addr = inet_addr(host)) == -1) 
	{
	  if ((host_ptr = gethostbyname(host)) == NULL) 
	    {
	      /* No such host! */
	      errno = EINVAL;
	      return(-1);
	    }
	  /* Check the address type for an internet host. */
	  if (host_ptr->h_addrtype != AF_INET) 
	    {
	      /* Not an Internet host! */
	      errno = EPROTOTYPE;
	      return(-1);
	    }
	  /* Set up the socket data. */
	  inaddr.sin_family = host_ptr->h_addrtype;
#ifdef hpux
	  (void) memcpy((char *)&inaddr.sin_addr, 
			(char *)host_ptr->h_addr, 
			sizeof(inaddr.sin_addr));
#else /* hpux */
	  (void) bcopy((char *)host_ptr->h_addr, 
		       (char *)&inaddr.sin_addr, 
		       sizeof(inaddr.sin_addr));
#endif /* hpux */
	} 
      else 
	{
	  inaddr.sin_family = AF_INET;
	}
#ifdef BSD44SOCKETS
      inaddr.sin_len = sizeof(inaddr);
#endif
      addr = (struct sockaddr *) &inaddr;
      addrlen = sizeof (struct sockaddr_in);
      inaddr.sin_port = display + X_TCP_PORT;
      inaddr.sin_port = htons(inaddr.sin_port);
      /*
       * Open the network connection.
       */
      if ((fd = socket((int) addr->sa_family, SOCK_STREAM, 0)) < 0){
	return(-1);	    /* errno set by system call. */}
      /* make sure to turn off TCP coalescence */
#ifdef TCP_NODELAY
      {
	int mi = 1;
	setsockopt (fd, IPPROTO_TCP, TCP_NODELAY, &mi, sizeof (int));
      }
#endif
    }

    /*
     * Changed 9/89 to retry connection if system call was interrupted.  This
     * is necessary for multiprocessing implementations that use timers,
     * since the timer results in a SIGALRM.	-- jdi
     */
    while (connect(fd, addr, addrlen) == -1) {
	if (errno != EINTR) {
  	    (void) close (fd);
  	    return(-1); 	    /* errno set by system call. */
	}
      }
  }
  /*
   * Return the id if the connection succeeded.
   */
  return(fd);
}
