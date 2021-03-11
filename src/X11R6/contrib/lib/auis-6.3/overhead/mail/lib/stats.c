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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/overhead/mail/lib/RCS/stats.c,v 2.22 1993/05/06 18:57:56 gk5g Exp $";
#endif

/*
		stats.c -- Make statistics entries for mail system.
*/

#ifdef _IBMR2
#ifndef _BSD
#define _BSD 43
#endif
#endif

#include <andrewos.h> /* sys/types.h sys/time.h */
#include <stdio.h>
#include <errno.h>
#include <netdb.h>
#include <sys/socket.h>
#include <netinet/in.h>

#include <fdplumb.h>
extern FILE *fopen();
#include "errprntf.h"

#if !POSIX_ENV
extern int errno;
#endif

typedef int bool;
#ifndef TRUE
#define FALSE	0
#define TRUE	1
#endif

#define NIL	0

#define LOGSTRLEN	25

#ifdef CMU_ENV
static char HostsFile[] = "/afs/andrew.cmu.edu/usr0/postman/stats/LogHosts";
#else /* CMU_ENV */
static char HostsFile[] = "/nonexistent";
#endif /* CMU_ENV */
static char LogHost[100];
static char LoggingService[] = "mail.logger";

static char MyName[LOGSTRLEN+1];
static char ProgName[LOGSTRLEN+1];
static int TransactionNumber;

static int Sequence;

static int ShowErrors = 0;  /* !=0 if caller wants to see errors */
static int StatFile = -1;

static struct sockaddr_in LogAddress;

/*VARARGS1*/
static void warning(format, a0, a1, a2, a3, a4)
    char *format, *a0, *a1, *a2, *a3, *a4;
{
    errprintf(ProgName, ERR_WARNING, NIL, NIL,
	      format, a0, a1, a2, a3, a4);
}

static bool GetMyName()
{
    static bool virgin = TRUE;
    char buf[200];

    if (!virgin) return TRUE;

    if (GetHostDomainName(buf, sizeof buf) < 0) {
	if (ShowErrors)
	    warning("Can't find my name: %d", errno);
	return FALSE;
    }

    virgin = FALSE;
    buf[sizeof(buf)-1] = '\0';
    Format(MyName, buf, LOGSTRLEN);
    return TRUE;
}

static Format(to, from, len)
    register char *to, *from;
    register int len;	/* Doesn't include '\0' */
{
    register int flen;

    flen = strlen(from);
    if (flen >= len) {
	strncpy(to, from, len);
	to[len] = '\0';
	return;
    }

    for (; len>flen; len--) *to++ = ' ';
    strcpy(to, from);
}

/*
    Choose a host to log to.

	(1) Read HostsFile to get list of possible hosts (1 to a
	    line)
	(2) Choose 1 at radnom

    Returns:

	FALSE	    Error
	TRUE	    Okay

    Format of HostsFile is:

	# hosts
	host1-address	<white space> comment
	host2-address	<white space> comment
	...
	hostn-address	<white space> comment

*/

static bool ChooseLogHost()
{
    register FILE *f;
    char buffer[50];
    register char *c;
    register int nhosts, host;

    /* Open hosts file */
    f = fopen(HostsFile, "r");
    if (f == NULL) {
	if (ShowErrors)
	    warning("Can't open hosts file (%d): \"%s\"", errno, HostsFile);
	return FALSE;
    }

    /* Read 1st line: # hosts to follow */
    if (fgets(buffer, sizeof(buffer), f) == NULL) {
	if (ShowErrors)
	    warning("Can't get 1st line of file: \"%s\"", HostsFile);
	return FALSE;
    }
    nhosts = atoi(buffer);
/*
    printf("# hosts = %d\n", nhosts);
*/

    /* Choose host # */
    host = (nhosts == 1 ? 0 : ((int) time(0)) % nhosts);
#ifdef NOTDEF
    printf("I choose host %d...", host);
    fflush(stdout);
#endif /* NOTDEF */

    /* Read to that host */
    for (; host>=0; host--)
	if (fgets(LogHost, sizeof(LogHost), f) == NULL) {
	    if (ShowErrors)
		warning("Not enough lines (%d) in \"%s\"", host, HostsFile);
	    return FALSE;
	}

    /* replace 1st space or '\n' with '\0' */
    for (c=LogHost; *c!='\n' && *c!='\t' && *c!=' '; c++) ;
    *c = '\0';
#ifdef NOTDEF
    printf("\"%s\"\n", LogHost);
#endif /* NOTDEF */

    fclose(f);
    return TRUE;
}

/*
    Create a datagram socket for talking to host LogHost & FILE
    for fprintf'ing to it.

    Returns:

	NULL	    Failed
	!=NULL	    Succeeded
*/

static int OpenSocket()
{
    register int s;
#ifdef NOTDEF
    register struct servent *serv;
#endif /* NOTDEF */
#ifndef hp9000s800 /* socket.h defines as extern u_long */
    extern long inet_addr();
#endif /* hp9000s800 */

    /* Look up mail statistics service */
#ifdef NOTDEF
    serv = getservbyname(LoggingService, "udp");
    if (serv == NIL) {
	if (ShowErrors)
	    warning("Can't find logging service: \"%s\"", LoggingService);
	return NULL;
    }
#endif /* NOTDEF */

    /* Create socket */
    s = socket(AF_INET, SOCK_DGRAM, 0);
    if (s < 0) {
	warning("Socket failed: %d", errno);
	return -1;
    }

    /* Create server/machine address */
    bzero(&LogAddress, sizeof LogAddress);
    LogAddress.sin_family = AF_INET;
#ifdef NOTDEF
    LogAddress.sin_port = serv -> s_port;
#else /* NOTDEF */
    LogAddress.sin_port = htons(514);
#endif /* NOTDEF */
    LogAddress.sin_addr.s_addr = inet_addr(LogHost);

    return s;
}

/*
    Initialize the stats logging package:

	prog	    This program name
	errflag	    !=0 if errors to be reported to /dev/console

    Returns:

	0	    Okay
	!=0	    Error
*/

int InitStats(prog, errflag)
    char *prog;
    int errflag;
{
    if (StatFile != -1) {
	if (errflag)
	    warning("Can't initialize statistics logging twice");
	return -1;
    }
    ShowErrors = errflag;
    Format(ProgName, prog, LOGSTRLEN);

    /* Choose host to log to */
    if (!ChooseLogHost()) return -1;

    /* Bind to host & make file */
    StatFile = OpenSocket();
    if (StatFile < 0) return -1;

    /* Get my host name & save it */
    if (!GetMyName()) return -1;

    Sequence = 1;

    return 0;
}

/*

    Terminate logging support.

    Returns:

	0	    Okay
	!=0	    Error

*/

int TermStats()
{
    if (StatFile < 0) {
	if (ShowErrors) warning("Stats support not initialized");
	return -1;
    }
    close(StatFile);
    StatFile = -1;
    return 0;
}   

/*
    Log a message:

	format	    Printf format-style string
	a0	    Args
	a1	    "
	...	    ...
	a9	    ""

    Returns:

	0	    Okay
	!=0	    Failure

    Format of output line is:

    YYYY-MM-DD HH:MM:SS.DD prog trans# pid pgrp module call# seq# machine msg

    Output is in fixed columns:

    Field   Start   Stop    Length
    -----   -----   ----    ------
    date    1	    10	    10
    time    12	    22	    11
    prog    24	    48	    25
    trans#  50	    59	    10
    pid	    60	    65	    6
    pgrp    67	    72	    6
    module  74	    98	    25
    call #  100	    104	    5
    seq #   106	    116	    11
    machine 118	    132	    25
    msg	    134	    --	    --

*/

int Logstat(module, call, format, a0, a1, a2, a3, a4, a5, a6, a7, a8, a9)
    char *module;
    int call;
    char *format;
{
    int save_errno;
    char ModuleName[LOGSTRLEN+1];
    struct osi_Times tp;
    register struct tm *now;
    char buffer[1500];	/* Larger than datagram size */
    register char *c;

    if (StatFile < 0) return -1;
    save_errno = errno;

    /* Get the time & format it */
    osi_GetTimes(&tp);
    now = localtime(tp.Secs);

    /* Format system info */
    Format(ModuleName, module, LOGSTRLEN);
#if POSIX_ENV || defined(SGI_4D_ENV)
    sprintf(buffer,
	    "%04d-%02d-%02d %02d:%02d:%02d.%02d %s %10d %6d %6d %s %5d %11d %s; ",
	    now->tm_year+1900, now->tm_mon+1, now->tm_mday,
	    now->tm_hour, now->tm_min, now->tm_sec, tp.USecs/10000,
	    ProgName, TransactionNumber, getpid(), getpgrp(),
	    ModuleName, call, Sequence++, MyName);
#else
    sprintf(buffer,
	    "%04d-%02d-%02d %02d:%02d:%02d.%02d %s %10d %6d %6d %s %5d %11d %s; ",
	    now->tm_year+1900, now->tm_mon+1, now->tm_mday,
	    now->tm_hour, now->tm_min, now->tm_sec, tp.USecs/10000,
	    ProgName, TransactionNumber, getpid(), getpgrp(0),
	    ModuleName, call, Sequence++, MyName);
#endif

    /* Get to end of string */
    for (c=buffer; *c!='\0'; c++) /* NOOP */ ;

    sprintf(c, format, a0, a1, a2, a3, a4, a5, a6, a7, a8, a9);
    if (sendto(StatFile, buffer, strlen(buffer)+1, 0,
	       &LogAddress, sizeof LogAddress) < 0) {
	warning("Sendto failed: %d", errno);
	errno = save_errno;
	return 1;
    }
    errno = save_errno;
    return 0;
}

/*
    Specify a module name for logging:

	name	    Module name

    Returns:

	nothing
*/

SetTransaction(number)
    int number;
{
    TransactionNumber = number;
}
