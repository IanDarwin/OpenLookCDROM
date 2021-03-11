/* -*-C-*-
********************************************************************************
*
* File:         wl.c
* RCS:          $Header: /users/npm/src/winterp/src-client/RCS/wl.c,v 2.5 1994/06/06 15:14:40 npm Exp $
* Description:  Unix Domain CLIENT to send lisp expressions to WINTERP lisp-server.
*		This code derived from winterp/src-client/wl-tcpip.c
*		as well as code contributed by Victor Kan.
* Author:       Niels Mayer, Victor Kan <kan@DG-RTP.DG.COM>
* Created:      Sat Jun 10 02:15:35 1989
* Modified:     Sun Jun  5 16:12:42 1994 (Niels Mayer) npm@indeed
* Language:     C
* Package:      N/A
* Status:       X11r6 contrib release
*
* Copyright (C) 1994, Enterprise Integration Technologies Corp. and Niels Mayer.
* WINTERP 1.15-1.99, Copyright (c) 1993, Niels P. Mayer.
* WINTERP 1.0-1.14, Copyright (c) 1989-1992 Hewlett-Packard Co. and Niels Mayer.
* 
* Permission to use, copy, modify, distribute, and sell this software and its
* documentation for any purpose is hereby granted without fee, provided that
* the above copyright notice appear in all copies and that both that
* copyright notice and this permission notice appear in supporting
* documentation, and that the name of Enterprise Integration Technologies,
* Hewlett-Packard Company, or Niels Mayer not be used in advertising or
* publicity pertaining to distribution of the software without specific,
* written prior permission. Enterprise Integration Technologies, Hewlett-Packard
* Company, and Niels Mayer makes no representations about the suitability of
* this software for any purpose.  It is provided "as is" without express or
* implied warranty.
* 
* ENTERPRISE INTEGRATION TECHNOLOGIES, HEWLETT-PACKARD COMPANY AND NIELS MAYER
* DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING ALL IMPLIED
* WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL ENTERPRISE
* INTEGRATION TECHNOLOGIES, HEWLETT-PACKARD COMPANY OR NIELS MAYER BE LIABLE
* FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER
* RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF
* CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN
* CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
*
********************************************************************************
*/
static char rcs_identity[] = "@(#)$Header: /users/npm/src/winterp/src-client/RCS/wl.c,v 2.5 1994/06/06 15:14:40 npm Exp $";

/*
*------------------------------------------------------------------------------
* See ./winterp/COPYRIGHT for information on contacting the authors.
* Please e-mail comments, modifications, questions, improvements and
* bugfixes to the WINTERP mailing list winterp@netcom.com. Please send 
* mailing list subscribe/unsubscribe notices to winterp-request@netcom.com .
* Post XLISP-specific questions/information to the USENET newsgroup
* comp.lang.lisp.x.
*------------------------------------------------------------------------------
*/

#include <stdio.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/un.h>
#include "../src-server/config.h"

char *progname = NULL;


/*******************************************************************************
 * Send <message> to the server via socket <s>
 *******************************************************************************/
void Server_Send(s, message)
     int s;
     char *message;
{
  if (send(s, message, strlen(message), 0) < 0) {
    perror(progname);
    fprintf(stderr, "%s: unable to send() on Unix Domain Socket.\n", progname);
    exit(1);
  }
}


/*******************************************************************************
 * Establish a connection with server process, returning socket descriptor
 * if successful.
 *
 * This is an AF_UNIX version of wl.c:Server_Connect()
 *******************************************************************************/
int Server_Connect(socket_path)
     char *socket_path;
{
  int s;
  struct sockaddr_un peeraddr_un;

  peeraddr_un.sun_family = AF_UNIX;

  /*
   * Set the file system entry used for the AF_UNIX Domain Socket.
   */
#ifndef SOCKADDR_UN_MAXLEN
#define SOCKADDR_UN_MAXLEN 108	/* can't find SOCKADDR_UN_MAXLEN on hpux 7.0, however "char sun_path[108];" */ 
#endif
  if (strlen(socket_path) > (SOCKADDR_UN_MAXLEN - 1)) {
    fprintf(stderr, "%s: Unix Domain Socket filepath %s must be shorter than %d bytes.\n",
	    progname,
	    socket_path,
	    SOCKADDR_UN_MAXLEN - 1);
    exit(1);
  }
  else
    strcpy(peeraddr_un.sun_path, socket_path);

  /*
   * Create the socket.
   */
  if ((s = socket(peeraddr_un.sun_family, SOCK_STREAM, 0)) == -1) {
    perror(progname);
    fprintf(stderr, "%s: socket() failed to create Unix Domain Socket %s .\n",
	    progname, socket_path);
    exit(1);
  }
  
  /*
   * Try to connect to the local server through the socket
   * which was just built into peeraddr.
   */
  if (connect(s, (struct sockaddr *) &peeraddr_un, sizeof(peeraddr_un.sun_family) + strlen(peeraddr_un.sun_path)) == -1) {
    perror(progname);
    fprintf(stderr, "%s: unable to connect() to remote via Unix Domain Socket %s .\n",
	    progname, socket_path);
    exit(1);
  }
  
  return (s);
}


/******************************************************************************
 * Server_Disconnect -- shutdown the connection for further sends. This causes
 * the server to receive an EOF condition after it has received all data from
 * Server_Send().
******************************************************************************/
void Server_Disconnect(s)
     int s;
{
  static char rdbuf[BUFSIZ];
  int len;
  
  Server_Send(s, WINTERP_CLIENT_SPECIAL_EOF_STRING); /* !!! hack !!! -- see ../src-server/winterp.c:Read_Eval_Print() and ../src-server/config.h */
  
  if (shutdown(s, 1) == -1) {
    perror(progname);
    fprintf(stderr, "%s: unable to shutdown() Unix Domain Socket.\n", progname);
    exit(1);
  }

  if ((len = recv(s, rdbuf, BUFSIZ, 0)) > 0) {
    rdbuf[len] = '\0';
    fprintf(stdout, "%s", rdbuf);
  }
  else if (len == 0) {
    fprintf(stderr, "%s: an evaluation error occured in WINTERP server.\n", progname);
    exit(128);			/* this sets exit status to "128" */
  }
  else if (len < 0) {
    perror(progname);
    fprintf(stderr, "%s: unable to recv() server reply from Unix Domain Socket.\n", progname);
    exit(1);			/* this sets exit status to "1" */
  }

  while ((len = recv(s, rdbuf, BUFSIZ, 0)) > 0) {
    rdbuf[len] = '\0';
    fprintf(stdout, "%s", rdbuf);
  }

  fprintf(stdout, "\n");

  if (len < 0) {
    perror(progname);
    fprintf(stderr, "%s: unable to recv() server reply from Unix Domain Socket.\n", progname);
    exit(1);			/* this sets exit status to "1" */
  }
  else {
    exit(0);			/* this sets exit status to "0" */
  }
}


/******************************************************************************
 * AF_UNIX version of wl-tcpip.c:main()
 ******************************************************************************/
main(argc, argv)
     int argc;
     char *argv[];
{
  extern int getopt();		/* proc to get option letter from argv */
  extern char* optarg;		/* arg string being pointed to by getopt() */
  extern int optind;		/* index into argv set by getopt() */
  int opt;			/* option argument */
  char* socket_path = NULL;	/* for pointing to environment vars */
  int s;			/* connected socket desriptor */

  progname = argv[0];

  while ((opt = getopt(argc, argv, "f:")) != EOF)
    switch (opt) {
    case 'f':
      socket_path = (char*) malloc((unsigned) strlen(optarg) + 1);
      strcpy(socket_path, optarg);
      break;
    case '?':
      fprintf(stderr, "usage: %s [-f <unix-socket-filepath>] [sexpr]\n", progname);
      fprintf(stderr, "\t<unix-socket-filepath> defaults to %s\n", DEFAULT_UNIX_SOCKET_FILEPATH); /* DEFAULT_UNIX_SOCKET_FILEPATH in "../src-server/config.h" */
      exit(1);
      break;
    }

  if (socket_path == NULL)
    if ((socket_path = (char *) getenv(DEFAULT_UNIX_SOCKET_FILEPATH_ENVVAR)) == NULL) /* DEFAULT_UNIX_SOCKET_FILEPATH_ENVVAR in "../src-server/config.h" */
      socket_path = DEFAULT_UNIX_SOCKET_FILEPATH;

  s = Server_Connect(socket_path);
    
  /*
   * note that due to a bug/feature of winterp, we must send it only
   * one sexp. Here, in the case that multiple args get passed, we also
   * want to ensure that they're whitespace separated for tokenization
   */
  for (; optind < argc; optind++) {
    Server_Send(s, argv[optind]);
    Server_Send(s, " ");
  }

  Server_Disconnect(s);		/* this routine calls exit() */
}
