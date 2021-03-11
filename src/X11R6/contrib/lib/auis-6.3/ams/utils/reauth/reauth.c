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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/ams/utils/reauth/RCS/reauth.c,v 1.18 1993/10/12 22:16:45 gk5g Exp $";
#endif

/*
	Reauthenticate to Vice repeatedly.

	Run in background with nohup.

*/

#include <andrewos.h>
#include <stdio.h>

#if defined(AIX) && defined(i386)
#include <sys/types.h>
#endif

#include <sys/wait.h>
#include <mailconf.h>
#include <errprntf.h>
#include <signal.h>
#include <errno.h>
#include <ctype.h>

#define NIL	0

static char myname[] = "reauth";

extern int errno;

/*
	reauth time who password [dirWithAuths]
*/

/* basically a no-op - used mostly just to register the fact that the signal was indeed caught */
dolog()
{
    errprintf(myname, ERR_CRITICAL, NIL, NIL, "Signal caught...");
}

static int logme(name, password, cellname)
char *name, *password, *cellname;
{
    register int pid;
    int outStatus, valWait;

    errprintf(myname, ERR_CRITICAL, NIL, NIL, "Attempting re-authentication as %s@%s", name, (cellname == NULL ? WorkstationCell : cellname));
    /* Fork */
    pid = fork();
    if (pid < 0) {
	errprintf(myname, ERR_CRITICAL, NIL, NIL, "Can't fork: %d", errno);
	return -2;
    }
    if (pid > 0) {
	do valWait = wait(&outStatus); while (valWait != pid && valWait != -1);
	if (valWait != pid) {
	    errprintf(myname, ERR_CRITICAL, NIL, NIL, "Wait returned %d, not %d",
		      valWait, pid);
	    return -1;
	}
	return outStatus;
    }

    /* Here's the child */
    if (cellname == NULL) {
	execl(logpgm, "log", name, password, NIL);
    } else {
	execl(logpgm, "log", "-x", name, password, "-c", cellname, NULL);
    }
    errprintf(myname, ERR_CRITICAL, NIL, NIL, "Execl failed: %d", errno);
    _exit(1);
}

#ifdef AFS_ENV
#include <afs/param.h>

static int LogViaDir(dirn)
char *dirn;
{/* Scan the directory ``dirn'' for files corresponding to other-cell authentications. */
    register DIR *dirp = NIL;
    register DIRENT_TYPE *dp;
    int err, AnyFailed, fd, len;
    char FName[1024];
    char FContent[1024];
    char *user, *pw; register char *x;

    AnyFailed = 0;
    errno = 0;
    dirp = opendir(dirn);
    if (dirp == NIL) {
	err = errno;
	errprintf(myname, ERR_CRITICAL, NIL, NIL, "reauth: can't open dir %s: %s", dirn, UnixError(err));
	return (tfail(err) ? -1 : 0);
    }
    for (;;) {
	errno = 0;
	dp = readdir(dirp);
	if (dp == NULL) break;
	if (strcmp(dp->d_name, ".") == 0 || strcmp(dp->d_name, "..") == 0) continue;
	if ((strlen(dirn) + strlen(dp->d_name) + 5) > sizeof(FName)) {
	    errprintf(myname, ERR_CRITICAL, NIL, NIL, "reauth: pw filename too big (%s/%s)", dirn, dp->d_name);
	    AnyFailed = 1;
	    continue;
	}
	sprintf(FName, "%s/%s", dirn, dp->d_name);
	fd = open(FName, O_RDONLY, 0);
	if (fd < 0) {
	    err = errno;
	    errprintf(myname, ERR_CRITICAL, NIL, NIL, "reauth: can't open pw filename (%s): %s", FName, UnixError(err));
	    AnyFailed = 1;
	    continue;
	}
	errno = 0;
	len = read(fd, FContent, sizeof(FContent));
	err = errno;
	close(fd);
	if (len <= 0 || len >= sizeof(FContent)) {
	    err = errno;
	    errprintf(myname, ERR_CRITICAL, NIL, NIL, "reauth: pw file %s read problem: result %d/%d (%s)", FName, len, sizeof(FContent), UnixError(err));
	    AnyFailed = 1;
	    continue;
	}
	FContent[len] = '\0';
	/* Now scan the file */
	x = FContent;	/* eliminate leading spaces */
	while (*x != '\0' && isspace(*x)) ++x;
	user = x;
	while (*x != '\0' && !isspace(*x)) ++x;
	*x++ = '\0';
	pw = x;		/* allow spaces in passwords: assume a single-character delimiter */
	while (*x != '\0' && (*x == ' ' || !iscntrl(*x))) ++x;
	*x = '\0';		/* flush a trailing newline */
	if (logme(user, pw, dp->d_name) != 0) AnyFailed = 1;
    }
    closedir(dirp);
    return (AnyFailed);
}
#endif /* AFS_ENV */

main(argc, argv)
int argc;
char *argv[];
{
    register int len, nap, pid, LogStatus, SleepTime;
    struct timeval tv;
    char password[512];	/* Should be big enough */
#ifdef AFS_ENV
    char DirToScan[1024];
#endif /* AFS_ENV */

#ifdef AFS_ENV
    if (argc != 4 && argc != 5) {
	errprintf(myname, ERR_CRITICAL, NIL, NIL, "Wrong number of args: %d", argc);
	fprintf(stderr, "Wrong number of args: %d\nusage: %s time who password [dirWithOtherLog]", argc, (argc >= 1 ? argv[0] : "reauth"));
	exit(1);
    }
#else /* AFS_ENV */
    if (argc != 4) {
	errprintf(myname, ERR_CRITICAL, NIL, NIL, "Wrong number of args: %d", argc);
	fprintf(stderr, "Wrong number of args: %d\nusage: %s time who password", argc, (argc >= 1 ? argv[0] : "reauth"));
	exit(1);
    }
#endif /* AFS_ENV */

    /* Copy password & clobber */
    len = strlen(argv[3]);
    if (len >= sizeof(password)) {
	errprintf(myname, ERR_CRITICAL, NIL, NIL, "Password too long: %d", len);
	exit(1);
    }
    strcpy(password, argv[3]);
    bzero(argv[3], len);

#ifdef AFS_ENV
    DirToScan[0] = '\0';
    if (argc == 5) {
	strncpy(DirToScan, argv[4], sizeof(DirToScan));
	DirToScan[sizeof(DirToScan)-1] = '\0';
	bzero(argv[4], strlen(argv[4]));
    }
#endif /* AFS_ENV */

    len = CheckAMSConfiguration();
    if (len != 0) {
	errprintf(myname, ERR_CRITICAL, NIL, NIL, "CheckAMSConfiguration failed: %d", len);
	exit(1);
    }

    /* Fork & become daemon */
    pid = fork();
    if (pid < 0) {
	errprintf(myname, ERR_CRITICAL, NIL, NIL, "Fork failed: %d", errno);
	exit(1);
    }
    if (pid > 0) exit(0);
    NEWPGRP();

    /* Get sleep time */
    nap = atoi(argv[1]);
    signal(SIGHUP, dolog);
#ifdef SIGXFSZ
    signal(SIGXFSZ, dolog);
#else /* SIGXFSZ */
#ifdef SIGUSR2
    signal(SIGUSR2, dolog);
#endif /* SIGUSR2 */
#endif /* SIGXFSZ */

    for (;;) {
	/* Use `nap' if forked OK, execed OK, no signals, exitstat == 0, or 5 min otherwise. */
	SleepTime = nap;
#ifdef AFS_ENV
	if (DirToScan[0] != '\0') {
	    LogStatus = LogViaDir(DirToScan);
	    /* 15 minutes on cross-cell failure */
	    if (LogStatus != 0) SleepTime = 15*60;
	}
#endif /* AFS_ENV */
	LogStatus = logme(argv[2], password, NULL);
	if (LogStatus != 0) SleepTime = 5*60;
	tv.tv_sec = SleepTime;
	tv.tv_usec = 0;
	select(0, NULL, NULL, NULL, &tv);
    }
}
