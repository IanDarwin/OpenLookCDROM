/*
 * Copyright (c) 1988 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software written by Ken Arnold and
 * published in UNIX Review, Vol. 6, No. 8.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. All advertising materials mentioning features or use of this software
 *    must display the following acknowledgement:
 *	This product includes software developed by the University of
 *	California, Berkeley and its contributors.
 * 4. Neither the name of the University nor the names of its contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 */

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)popen.c	5.15 (Berkeley) 2/23/91";
#endif /* LIBC_SCCS and not lint */

#include "machine.h"
#include <sys/param.h>
#include <sys/signal.h>
#include <sys/wait.h>
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

#if !defined(STDOUT_FILENO)
#define STDOUT_FILENO   1
#endif

#if !defined(STDIN_FILENO)
#define STDIN_FILENO    0
#endif

#if !defined(STDERROR_FILENO)
#define STDERROR_FILENO  2
#endif


static struct _files {
	int pid;
#if defined(_AIX)&&defined(_BSD)
	union wait status;
#else
	int status;
#endif
} *files = NULL;
static int   num_fds = 0;

FILE *
lpopen(program, type)
	char *program;
	char *type;
{
	FILE *iop;
	int pdes[2], fds, pid;

	if (*type != 'r' && *type != 'w' || type[1])
		return (NULL);

	if (files ==  NULL) {
#if defined(_SC_OPEN_MAX)
		if ((fds = sysconf(_SC_OPEN_MAX)) <= 0)
#else
	        if ((fds = getdtablesize()) <= 0)
#endif
			return (NULL);
		if ((files = (struct _files*)
				XtMalloc(fds*sizeof(struct _files))) == NULL)
			return (NULL);
		ZERO((char *)files, fds * sizeof(struct _files));
		num_fds = fds;
	}
	if (pipe(pdes) < 0)
		return (NULL);
	switch (pid = fork()) {
	case -1:			/* error */
		(void) close(pdes[0]);
		(void) close(pdes[1]);
		return (NULL);
		/* NOTREACHED */
	case 0:				/* child */
		if (*type == 'r') {
			if (pdes[1] != STDOUT_FILENO) {
				(void) dup2(pdes[1], STDOUT_FILENO);
				(void) close(pdes[1]);
			}
		        (void) dup2(STDOUT_FILENO, STDERROR_FILENO);
			(void) close(pdes[0]);
		} else {
			if (pdes[0] != STDIN_FILENO) {
				(void) dup2(pdes[0], STDIN_FILENO);
				(void) close(pdes[0]);
			}
			(void) close(pdes[1]);
		}
#if defined(SVR4)||defined(HAVE_UNISTD_H)||!defined(BSD)
		setpgrp();
#else
		setpgrp(0, getpid());
#endif
		execl("/bin/sh", "sh", "-c", program, NULL);
		_exit(127);
		/* NOTREACHED */
	}
	/* parent; assume fdopen can't fail...  */
	if (*type == 'r') {
		iop = fdopen(pdes[0], type);
		(void) close(pdes[1]);
	} else {
		iop = fdopen(pdes[1], type);
		(void) close(pdes[0]);
	}
#if defined(_AIX)&&defined(_BSD)
	files[fileno(iop)].status.w_status = 0;
#else
	files[fileno(iop)].status = 0;
#endif
	files[fileno(iop)].pid = pid;
	return (iop);
}

int
lpclose(iop)
	FILE *iop;
{
	int fdes;
#if defined(_AIX)&&defined(_BSD)
	union wait pstat;
#else
	int  pstat;
#endif
	pid_t pid;
#if defined(SIG_SETMASK)
	sigset_t newmask;
	sigset_t oldmask;
#else
	int omask;
#endif

	/*
	 * pclose returns -1 if stream is not associated with a
	 * `popened' command, if already `pclosed', or waitpid
	 * returns an error.
	 */
	if (files == NULL || files[fdes = fileno(iop)].pid == 0)
		return (-1);
	(void) fclose(iop);
	if (files[fdes].pid > 0)  {
#if defined(SIG_SETMASK)
	    sigfillset(&newmask);
	    sigdelset(&newmask, SIGINT);
	    sigdelset(&newmask, SIGQUIT);
	    sigdelset(&newmask, SIGHUP);
	    sigprocmask(SIG_SETMASK,&newmask,&oldmask);
#else
 	    omask = sigblock(sigmask(SIGINT)|sigmask(SIGQUIT)|sigmask(SIGHUP));
#endif
	    do {
		pid = waitpid(files[fdes].pid, &pstat, 0);
	    } while (pid == -1 && errno == EINTR);
#if defined(SIG_SETMASK)
	    sigprocmask(SIG_SETMASK,&oldmask,NULL);
#else
            (void) sigsetmask(omask);
#endif
	} else {
#if defined(_AIX)&&defined(_BSD)
	    pstat.w_status = files[fdes].status.w_status;
#else
	    pstat = files[fdes].status;
#endif
	    pid = -files[fdes].pid;
	}
	files[fdes].pid = 0;
#if defined(_AIX)&&defined(_BSD)
	files[fdes].status.w_status = 0;
#else
	files[fdes].status = 0;
#endif
	return (pid == -1 ? -1 : WEXITSTATUS(pstat));
}

int
lpabort(iop)
	FILE *iop;
{
	int fdes;

	if (files == NULL || files[fdes = fileno(iop)].pid == 0)
		return (-1);
	if (files[fdes].pid > 0) {
	    kill(-files[fdes].pid, SIGKILL);
	}
	return pclose(iop);
}

void
reapem()
{
    int pid, i;
#if defined(_AIX)&&defined(_BSD)
	union wait pstat;
#else
	int  pstat;
#endif

    do {
       	pid = waitpid((pid_t)-1, &pstat, WNOHANG); 
	if (pid > 0) {
	    for (i=0;i<num_fds; i++) {
	   	if (files[i].pid == pid) {
		    files[i].pid = -pid;
#if defined(_AIX)&&defined(_BSD)
		    files[i].status.w_status = pstat.w_status;
#else
		    files[i].status = pstat;
#endif
	     	}
	    }
	}
    } while ((pid == -1 && errno == EINTR) || pid > 0);
#if defined(SIGCHLD)&&defined(SVR4)
    signal(SIGCHLD, reapem);
#endif
}
