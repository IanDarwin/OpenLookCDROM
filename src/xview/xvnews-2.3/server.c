/*
 *
 * Copyright (c) 1988, 1989, 1990, 1991, Ellen M. Sentovich and Rick L. Spickelmier.
 *
 * Permission to use, copy, modify, and distribute this software and its
 * documentation for any purpose and without fee is hereby granted, provided
 * that the above copyright notice appear in all copies and that both that
 * copyright notice and this permission notice appear in supporting
 * documentation.
 *
 */

#include <unistd.h>
#include <ctype.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>
#include <xview/xview.h>

#ifdef __STDC__
#  include <stdarg.h>
#else
#  include <varargs.h>
#endif

#include "codes.h"
#include "utils.h"
#include "xvnews.h"

/* All of this code was modified from xrn. I made it handle reconnects to 
 * the news server.
 */

static FILE	*ser_rd_fp = NULL;
static FILE	*ser_wr_fp = NULL;

extern struct globals *Global;

STATIC_FUNCTION( int nntp_connect, (char *));
STATIC_FUNCTION( int socket_connect, (char *));

STATIC int nntp_connect(machine)
char	*machine;
{
	int	sockt_rd, sockt_wr;
	int	status, alive = 1;
	char	*c, version[16];
	char	line[256];

	sockt_rd = socket_connect(machine);

	if (sockt_rd < 0)
		return (-1);

	/*
	 * Now we'll make file pointers (i.e., buffered I/O) out of
	 * the socket file descriptor.  Note that we can't just
	 * open a fp for reading and writing -- we have to open
	 * up two separate fp's, one for reading, one for writing.
	 */

	if ((ser_rd_fp = fdopen(sockt_rd, "r")) == NULL) {
		perror("xvnews->server_init: fdopen #1");
		return (-1);
	}

	sockt_wr = dup(sockt_rd);
	if ((ser_wr_fp = fdopen(sockt_wr, "w")) == NULL) {
		perror("xvnews->server_init: fdopen #2");
		ser_rd_fp = NULL;		/* from above */
		return (-1);
	}

	/* Set the socket status to avoid stale socket connections. */ 
        if (setsockopt(sockt_wr, SOL_SOCKET, SO_KEEPALIVE, (char *)&alive, sizeof(alive)) < 0)
                printf("Failed to set KEEPALIVE for socket write!\n");
        alive = 1;
        if (setsockopt(sockt_rd, SOL_SOCKET, SO_KEEPALIVE, (char *)&alive, sizeof(alive)) < 0)
                printf("Failed to set KEEPALIVE for socket read!\n");

	/* Now get the server's signon message and check the revision on
	   of the server. Old servers don't have the distribution features
           that xvnews uses, so that feature off. 
	*/

	memset(line, '\0', 256);
	status = get_server(line, sizeof(line));
	if (status != 200 && status != 201) {
		if (strlen(line))
			printf("Failed message from server is:\n %s\n", line);
		return(-1);
	}

	if ((strstr(line, "INN") != NULL) && (strstr(line, "NNRP") == NULL)) 
	{
		put_server("mode reader");
		status = get_server(line, sizeof(line));
	}

	if (status == 201)
		printf("Server %s does not allow posting!\n", machine);

	if (strstr(line, "NNRP") != NULL) {
		put_server("XPAT");
		get_server(line, sizeof(line));
		if (strstr(line, "What") == NULL)
			Global->nnrp = 1;
		else
			Global->nnrp = 0;
		return (atoi(line));
	}
	Global->nnrp = 0;
	c = (char *)strstr(line, "version");
	if (c == NULL)
		return (atoi(line));
	sscanf(c, "%*s%s", version);

	if (atoi(&version[2]) > 5 || atoi(&version[0]) > 1) {
		Global->list = 1;
		return (atoi(line));
	}
	if ((int)strlen(version) > 4 && atoi(&version[4]) > 3)
		Global->list = 1;
	else {
		Global->list = 0;
		printf("NNTP version is %s, you need > 1.5.3 for extended LIST commands!\n", version);
		printf("Group descriptions feature and distribution menu turned off!\n");
	}

	return (atoi(line));
}

STATIC int socket_connect(machine)
char	*machine;
{
	int	s;
	struct	sockaddr_in sin;
	struct servent *sp;
	struct hostent *hp;

	if ((sp = getservbyname("nntp", "tcp")) ==  NULL) {
		(void) fprintf(stderr, "nntp/tcp: Unknown service.\n");
		return (-1);
	}

	if ((hp = gethostbyname(machine)) == NULL) {
		(void) fprintf(stderr, "%s: Unknown host.\n", machine);
		return (-1);
	}

	(void) memset((char *) &sin, 0, sizeof(sin));
	sin.sin_family = hp->h_addrtype;
	sin.sin_port = sp->s_port;

	/*
	 * The following is kinda gross.  The name server under 4.3
	 * returns a list of addresses, each of which should be tried
	 * in turn if the previous one fails.  However, 4.2 hostent
	 * structure doesn't have this list of addresses.
	 * Under 4.3, h_addr is a #define to h_addr_list[0].
	 * We use this to figure out whether to include the NS specific
	 * code...
	 */

	if ((s = socket(AF_INET, SOCK_STREAM, 0)) < 0) { /* Get the socket */
		(void) perror("xvnews->socket");
		return (-1);
	}

	/* And then connect */

	(void) memcpy((char *) &sin.sin_addr, hp->h_addr, hp->h_length);
	if (connect(s, (struct sockaddr *) &sin, sizeof(sin)) < 0) {
		(void) perror("xvnews->connect");
		(void) close(s);
		return (-1);
	}

	return (s);
}

#ifdef __STDC__
extern void put_server(char *formatstr, ...)
#else
extern void put_server(formatstr, va_alist)
  char *formatstr;
  va_dcl
#endif
{
  va_list args;

#ifdef __STDC__
  va_start(args, formatstr);
#else
  va_start(args);
#endif
  vfprintf(ser_wr_fp, formatstr, args);
  fprintf(ser_wr_fp, "\r\n");
  fflush(ser_wr_fp);

  va_end(args);
}

extern int get_server(string, size)
char	*string;
int	size;
{
	register char *cp;
	int result = -2;

	if (fgets(string, size, ser_rd_fp) == NULL) {
	  strcpy(string, "NNTP connection was unexpectedly lost!");
	  return (-1);
	}

	if ((cp = (char *)strchr(string, '\r')) != NULL)
		*cp = '\0';
	else if ((cp = (char *)strchr(string, '\n')) != NULL)
		*cp = '\0';

	sscanf(string, "%d", &result);
	if (result < 0) {
	  result = 0;
	}

	return result;
}

extern void
close_server()
{
	char	ser_line[256];

	if (ser_wr_fp == NULL || ser_rd_fp == NULL)
		return;

	put_server("QUIT");
	get_server(ser_line, sizeof(ser_line));

	(void) fclose(ser_wr_fp);
	(void) fclose(ser_rd_fp);
	Global->connected = 0;
}

extern void
connect_server(machine)
char *machine;
{
  int response;
  int count = 10, max = 10;
  
  response = nntp_connect(machine);
  if (response < 0) {
    printf("Failed to connect to news server %s, retrying...\n", machine);
    while (count) {
      response = nntp_connect(machine);
      if (response < 0)  
	sleep(((max - count--) + 1) * 30);
      else
	break;
    }
    if (!count) {
      printf("Failed to connect to NNTP server %s, aborting!\n", machine);
      if (Global->head != NULL)
	save_newsrc();
      exit(-1);
    }
  }

#ifdef USE_NNRP_PASSWD
  response = NNTPsendpassword(machine, ser_rd_fp, ser_wr_fp);
  if (response) {
    printf("Authorization failed for NNTP server %s, aborting!\n", machine);
    if (Global->head) {
      save_newsrc();
    }
    exit(-1);
  }
#endif
  
  Global->connected = 1;
}

extern char *
get_nntp_server()
{
static char server[] = NNTPSERVER;
	char *a;

	if ((a = getenv("NNTPSERVER")) != NULL)
		return a;

	if (strlen(server))
		return server;

	printf("Please set the environment variable NNTPSERVER to a valid machine name!\n");
	exit(-1);
}

extern void reconnect_server()
{
	char message[MAX_MESSAGE_LEN];

	connect_server(get_nntp_server());
	if (Global->mode == ARTICLE_MODE) {
		put_server("GROUP %s", Global->group->newsgroup); 
		get_server(message, sizeof(message));
	}
}

extern Notify_value
signal_handler()
{
	return NOTIFY_DONE;
}

extern char *
nntp_gtitle(group)
  char *group;
{
  static char line[MAX_MESSAGE_LEN], *start;
  int status;

  if (Global->nnrp) {
    put_server("XGTITLE %s", group);
    status = get_server(line, sizeof(line));
    if (status == OK_LIST) {
      get_server(line, sizeof(line));
      if (line[0] != '.') {
	start = line;
	while (*start && !isspace(*start))
	  start++;
	while (*start && isspace(*start))
	  start++;
	while (line[0] != '.')
	  get_server(line, sizeof(line));
	return strdup(start);
      }
    }
  }
  return "No description available";
}
