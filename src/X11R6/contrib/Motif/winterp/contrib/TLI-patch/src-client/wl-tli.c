/* -*-C-*-
********************************************************************************
*
* File:         wl-tcpip.c
* RCS:          $Header: /seq5/u/wolfe/Src/winterp-1.11/src-client/RCS/wl-tli.c,v 1.1 91/04/06 16:15:02 wolfe Exp Locker: wolfe $
* Description:  TCP INET CLIENT to send lisp expressions to WINTERP lisp-server.
*               (this code derived from examples in Sequent DYNIX/ptx ???)
* Author:       David Wolfe, Sequent Computers (after Niels Mayer, HPLabs)
* Created:      Sat Jun 10 02:15:35 1989
* Modified:     Sat Oct  5 23:11:15 1991 (Niels Mayer) mayer@hplnpm
* Language:     C
* Package:      N/A
* Status:       X11r5 contrib tape release
*
* WINTERP Copyright 1989-1991 Hewlett-Packard Company (by Niels Mayer).
* XLISP version 2.1, Copyright (c) 1989, by David Betz.
*
* Permission to use, copy, modify, distribute, and sell this software and its
* documentation for any purpose is hereby granted without fee, provided that
* the above copyright notice appear in all copies and that both that
* copyright notice and this permission notice appear in supporting
* documentation, and that the name of Hewlett-Packard and David Betz not be
* used in advertising or publicity pertaining to distribution of the software
* without specific, written prior permission.  Hewlett-Packard and David Betz
* make no representations about the suitability of this software for any
* purpose. It is provided "as is" without express or implied warranty.
*
* HEWLETT-PACKARD AND DAVID BETZ DISCLAIM ALL WARRANTIES WITH REGARD TO THIS
* SOFTWARE, INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS,
* IN NO EVENT SHALL HEWLETT-PACKARD NOR DAVID BETZ BE LIABLE FOR ANY SPECIAL,
* INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM
* LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE
* OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
* PERFORMANCE OF THIS SOFTWARE.
*
* See ./winterp/COPYRIGHT for information on contacting the authors.
* 
* Please send modifications, improvements and bugfixes to mayer@hplabs.hp.com
* Post XLISP-specific questions/information to the newsgroup comp.lang.lisp.x
*
********************************************************************************
*/
static char rcs_identity[] = "@(#)$Header: /seq5/u/wolfe/Src/winterp-1.11/src-client/RCS/wl-tli.c,v 1.1 91/04/06 16:15:02 wolfe Exp Locker: wolfe $";

#include <stdio.h>
#include <sys/types.h>
#include <fcntl.h>     
#include <tiuser.h>
#include <netinet/in.h>
#include <netinet/netinet.h>
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
  if (t_snd(s, message, strlen(message), 0) < 0) {
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
     ushort port;
{

  int fd;			/* connected socket descriptor */
  struct t_call *sndcall;
  struct sockaddr_in *server;
  struct servent *sp;
  struct hostent *hp, *gethostbyname();
	
  /* establish connection */
  if ((fd = t_open(TLI_TCP, O_RDWR, NULL)) < 0) {
    t_error("t_open failed");
    exit(1);
  }

  if (t_bind(fd, NULL, NULL) < 0) {
    t_error("t_bind failed");
    exit(2);
  }

  if ((sndcall = (struct t_call *)t_alloc(fd, T_CALL, T_ALL)) == NULL) {
    t_error("t_alloc failed");
    exit(3);
  }

  hp = gethostbyname(serverhost);
  if (hp == NULL) {
    hp = gethostbyname("localhost");
    if ( hp == NULL ) {
      fprintf(stderr, "gethostbyname failed for %s\n", serverhost);
      exit(3);
    }
  }

  server = (struct sockaddr_in *)sndcall->addr.buf;
  server->sin_family = AF_INET;
  server->sin_addr.s_addr = *(unsigned long *)hp->h_addr;

  /* select port */
  if (port != 0)
    server->sin_port = htons(port);
  else {
    if ((sp = getservbyname(DEFAULT_INET_SERVICE_NAME, "tcp")) == NULL) {	
      fprintf(stderr, "%s: unable to find \"%s\" in /etc/services.\n",
	      progname,
	      DEFAULT_INET_SERVICE_NAME);
      exit(1);
    }
    server->sin_port = sp->s_port;
  }

  sndcall->addr.len = sizeof(struct sockaddr_in);
	
  if (t_connect(fd, sndcall, NULL) < 0) {
    t_error("t_connect");
    exit(-1);
  }

  return(fd);

}


/******************************************************************************
 * Server_Disconnect -- shutdown the connection for further sends. This causes
 * the server to receive an EOF condition after it has received all data from
 * Server_Send().
******************************************************************************/
void Server_Disconnect(s)
     int s;
{
  /* send disconnect */
  if (t_sndrel(s) < 0) {
    t_error("t_sndrel failed");
    exit(1);
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
  ushort port = 0;		/* port number */
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
      port = (ushort) atoi(portstr);
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

  Server_Disconnect(s);
  exit(0);
}
