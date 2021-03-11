/* -*-C-*-
********************************************************************************
*
* File:         wl-tcpip.c
* RCS:          $Header: /users/npm/src/winterp/src-client/RCS/wl-tcpip.c,v 2.5 1994/06/06 15:14:40 npm Exp $
* Description:  TCP INET CLIENT to send lisp expressions to WINTERP lisp-server.
*               (this code derived from examples in HP manual 50952-9000
*               "ARPA Services/300 User's Guide" (pp 4-32 to 4-36)).
* Author:       Niels Mayer
* Created:      Sat Jun 10 02:15:35 1989
* Modified:     Sun Jun  5 16:11:39 1994 (Niels Mayer) npm@indeed
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
static char rcs_identity[] = "@(#)$Header: /users/npm/src/winterp/src-client/RCS/wl-tcpip.c,v 2.5 1994/06/06 15:14:40 npm Exp $";

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
#include <netinet/in.h>
#include <netdb.h>
#include "../src-server/config.h" /* defines DEFAULT_INET_SERVICE_PORT, etc */

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
    fprintf(stderr, "%s: unable to send() on INET Domain Socket.\n", progname);
    exit(1);
  }
}


/*******************************************************************************
 * Establish a connection with server process, returning socket descriptor
 * if successful.
*******************************************************************************/
int Server_Connect(serverhost, port)
     char *serverhost;
     u_short port;
{
  int s;			/* connected socket descriptor */
  struct hostent *hp;		/* pointer to host info for remote host */
  struct servent *sp;		/* service information pointer */
  struct sockaddr_in peeraddr_in; /* for peer socket address */

  /* clear out address structures */
  memset((char *)&peeraddr_in, 0, sizeof(struct sockaddr_in));
  
  /* Set up the peer address to which we will connect. */
  peeraddr_in.sin_family = AF_INET;

  /*
   * Get the host information for the hostname that the
   * user passed in. If serverhost==NULL, then make a quicker connection
   * by bypassing search in /etc/hosts and using loopback adrs directly.
   */
  if (serverhost) {
    if ((hp = gethostbyname(serverhost)) == NULL) {
      fprintf(stderr, "%s: unable to find INET server host %s in /etc/hosts or from YP.\n",
	      progname, serverhost);
      exit(1);
    }
    peeraddr_in.sin_addr.s_addr = ((struct in_addr *)(hp->h_addr))->s_addr;
  }
  else {
    peeraddr_in.sin_addr.s_addr = inet_addr(DEFAULT_INET_SERVICE_HOSTADDR); /* loopback */
  }

  if (port == 0) {
    /* get port number for service "widget_interp" */
    if ((sp = getservbyname(DEFAULT_INET_SERVICE_NAME, "tcp")) == NULL) {	
      fprintf(stderr, "%s: unable to find \"%s\" in /etc/services.\n",
	      progname,
	      DEFAULT_INET_SERVICE_NAME);
      exit(1);
    }
    peeraddr_in.sin_port = sp->s_port;
  }
  else
    peeraddr_in.sin_port = htons(port);
  
  /* Create the socket. */
  if ((s = socket(AF_INET, SOCK_STREAM, 0))== -1) {
    perror(progname);
    fprintf(stderr, "%s: unable to create INET Domain Socket().\n", progname);
    exit(1);
  }

  /*
   * Try to connect to the remote server at the address
   * which was just built into peeraddr.
   */
  if (connect(s, (struct sockaddr *) &peeraddr_in, sizeof(struct sockaddr_in)) == -1) {
    perror(progname);
    fprintf(stderr, "%s: unable to connect() to remote via INET.\n", progname);
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
    fprintf(stderr, "%s: unable to shutdown() INET Domain Socket.\n", progname);
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
    fprintf(stderr, "%s: unable to recv() server reply from INET Domain Socket.\n", progname);
    exit(1);			/* this sets exit status to "1" */
  }

  while ((len = recv(s, rdbuf, BUFSIZ, 0)) > 0) {
    rdbuf[len] = '\0';
    fprintf(stdout, "%s", rdbuf);
  }

  fprintf(stdout, "\n");

  if (len < 0) {
    perror(progname);
    fprintf(stderr, "%s: unable to recv() server reply from INET Domain Socket.\n", progname);
    exit(1);			/* this sets exit status to "1" */
  }
  else {
    exit(0);			/* this sets exit status to "0" */
  }
}


/******************************************************************************
 *
 ******************************************************************************/
main(argc, argv)
     int argc;
     char *argv[];
{
  extern int getopt();		/* proc to get option letter from argv */
  extern char* optarg;		/* arg string being pointed to by getopt() */
  extern int optind;		/* index into argv set by getopt() */
  int opt;			/* option argument */
  char hostname[200];		/* for holding server host name */
  int hostname_given = 0;	/* true if hostname was spec'd by user */
  int s;			/* connected socket desriptor */
  u_short port = 0;		/* port number */
  char* portstr;
  char* hoststr;

  progname = argv[0];

  while ((opt = getopt(argc, argv, "h:p:")) != EOF)
    switch (opt) {
    case 'h':
      strcpy(hostname, optarg);
      hostname_given = 1;
      break;
    case 'p':
      port = atoi(optarg);
      break;
    case '?':
      fprintf(stderr, "usage: %s [-h hostname] [-p port] [sexpr]\n", progname);
      exit (1);
      break;
    }

  if (port == 0) {
    if ((portstr = (char *) getenv(DEFAULT_INET_PORT_ENVVAR)) != NULL) /* DEFAULT_INET_PORT_ENVVAR in "../src-server/config.h" */
      port = (u_short) atoi(portstr);
    else
      port = DEFAULT_INET_SERVICE_PORT; /* default port # for winterp server */
  }

  if (hostname_given)
    s = Server_Connect(hostname, port); /* a remote connection w/ host spec'd on command line */
  else {
    if ((hoststr = (char *) getenv(DEFAULT_INET_HOSTADDR_ENVVAR)) != NULL) /* DEFAULT_INET_HOSTADDR_ENVVAR in "../src-server/config.h" */
      s = Server_Connect(hoststr, port); /* use hostname given in environment var */
    else
      s = Server_Connect(NULL, port); /* a local connection -- use loopback */
  }

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
