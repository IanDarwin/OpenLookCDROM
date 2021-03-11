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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/overhead/util/lib/RCS/topen.c,v 2.26 1993/11/11 19:45:43 gk5g Exp $";
#endif

#include <andrewos.h>
#include <stdio.h>
#include <signal.h>
#include <errno.h>
#include <setjmp.h>
extern int errno;
#define	tst(a,b)	(*mode == 'r'? (b) : (a))
#define	RDR	0
#define	WTR	1
static	int	*popen_pid = NULL;
static	int	dtablesize = -1;

#ifdef VMUNIX
#define	mask(s)	(1<<((s)-1))
#endif
#ifndef	SIGRETRO
#define	sigchild()
#endif /* SIGRETRO */

#if POSIX_ENV
typedef void SignalReturnType;
#else
typedef int SignalReturnType;
#endif

#ifdef __STDC__
static SignalReturnType (*oldfunc)(int);
#else
static SignalReturnType (*oldfunc)();
#endif


FILE *topen(name, argv, mode, pgrp)
char *name, *argv[], *mode;
int *pgrp;
{
    int p[2];
    register myside, hisside;

    if (popen_pid == NULL) {
	dtablesize = getdtablesize();
	if (dtablesize > 0) {
	    popen_pid = (int *) malloc(dtablesize * sizeof(int));
	}
    }
    if (popen_pid == NULL) {errno = ENOMEM; return NULL;}

    /* Set up to ignore broken pipe signals */
    oldfunc = signal(SIGPIPE, SIG_IGN);

    if(pipe(p) < 0) return NULL;
    myside = tst(p[WTR], p[RDR]);
    hisside = tst(p[RDR], p[WTR]);
    if((*pgrp = osi_vfork()) == 0) {
	int pid, fd;
	/* myside and hisside reverse roles in child */
	sigchild();
	pid = getpid();
	NEWPGRP();
	close(myside);
	dup2(hisside, tst(0, 1));
	close(hisside);
	for (fd = getdtablesize(); fd > 2; --fd) close(fd);
	execv(name, argv);
	_exit(1);
    }
    if (*pgrp == -1) return NULL;
    /* I am parent */
    popen_pid[myside] = *pgrp;
    close(hisside);
    return fdopen(myside, mode);
}

static jmp_buf env;

static
#if defined(__STDC__)
void lclalarm(int i)
#else
lclalarm()
#endif
{
    longjmp(env, 1);
}

int tclose(ptr, seconds, timedout)
FILE *ptr;
int seconds, *timedout;
{
    register f, r;
    int status;
    SIGSET_TYPE omask, nmask;
    extern int errno;
    struct itimerval val;
#ifdef __STDC__
    SignalReturnType (*old)(int);
#else
    SignalReturnType (*old)();
#endif

    f = fileno(ptr);
    fclose(ptr);
#ifdef POSIX_ENV
     (void) sigemptyset(&nmask);
     (void) sigaddset(&nmask, SIGINT);
     (void) sigaddset(&nmask, SIGQUIT);
     (void) sigaddset(&nmask, SIGHUP);
     (void) sigaddset(&nmask, SIGCHLD);
     if (sigprocmask(SIG_BLOCK, &nmask, &omask) < 0)
	 perror("sigprocmask");
#else
    omask = sigblock(mask(SIGINT)|mask(SIGQUIT)|mask(SIGHUP)|mask(SIGCHLD));
#endif

    if (timedout != 0) {
	/* Enable signal handler */
	old = signal(SIGALRM, lclalarm);

	/* Set timer */
	val.it_interval.tv_sec = 0;
	val.it_interval.tv_usec = 0;
	val.it_value.tv_sec = seconds;
	val.it_value.tv_usec = 0;
	setitimer(ITIMER_REAL, &val, 0);

	if (setjmp(env) == 0) {
	    while((r = wait(&status)) != popen_pid[f] && r != -1) ;

	    /* Turn off timer */
	    val.it_value.tv_sec = 0;
	    val.it_value.tv_usec = 0;
	    setitimer(ITIMER_REAL, &val, 0);
	    *timedout = 0;
	} else {
	    /* Timer expired */
	    status = -1;
	    *timedout = 1;
	}
	(void) signal(SIGALRM, old);	/* Restore old value */
    } else {
	while ((r=wait(&status)) != popen_pid[f] && r != -1) ;
    }

#ifdef POSIX_ENV
    (void) sigprocmask(SIG_SETMASK, &omask, (sigset_t *)NULL);
#else
    sigsetmask(omask);
#endif

    /* Reenable old SIGPIPE action */
    (void) signal(SIGPIPE, oldfunc);

    return status;
}

FILE *qopen(name, argv, mode)
char *name, *argv[], *mode;
{
    int dummy;

    return topen(name, argv, mode, &dummy);
}

int qclose(ptr)
FILE *ptr;
{
    return tclose(ptr, 0, 0);
}

int getpidfromfp(ptr)
FILE *ptr;
{
    return(popen_pid[fileno(ptr)]);
}
