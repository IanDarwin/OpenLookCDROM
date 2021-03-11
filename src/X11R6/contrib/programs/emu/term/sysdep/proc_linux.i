/* -*-Mode: C; -*- */

#define DEBUG

#ifndef _PROCESS_LINUX_INCLUDE
#define _PROCESS_LINUX_INCLUDE

/* proc_linux.i,v 1.4 1994/06/02 14:07:13 me Exp */

/*
 * This file is part of the Emu system.
 *
 * Copyright 1990 by PCS Computer Systeme, GmbH. Munich, West Germany.
 * 
 * Copyright 1994 by Jordan K. Hubbard and Michael W. Elbel
 *
 * Permission is hereby granted, free of charge, to any person obtaining
 * a copy of this software and associated documentation files (the
 * "Software"), to deal in the Software without restriction, including
 * without limitation the rights to use, copy, modify, merge, publish,
 * distribute, sublicense, and/or sell copies of the Software, and to
 * permit persons to whom the Software is furnished to do so, subject to
 * the following conditions:
 * 
 * The above copyright notice and this permission notice shall be included
 * in all copies or substantial portions of the Software.
 * 
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
 * OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
 * MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
 * IN NO EVENT SHALL PCS, THE AUTHORS, OR THEIR HOUSEPETS BE LIABLE FOR
 * ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
 * TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
 * SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE. SO DON'T SUE US.
 * THANK YOU.
 */

/*
 * Various process manipulation routines specific to linux systems
 *
 * Author: Michael Elbel
 * Date: Jan. 4th 1994
 * 
 * proc_linux.i,v
 * Revision 1.4  1994/06/02  14:07:13  me
 * Updated for R6 contrib
 *
 * Revision 1.3  1994/05/29  10:37:37  me
 * Took out removal of TERMCAP environment variable, since this is now set
 */

/* Mandatory */
#ifndef _PROCESS_INCLUDED
#define _PROCESS_INCLUDED
#endif

#include "sysdep/proc_unix.i"
#include <grp.h>

/* Fire one process torpedo the SYSV way */
Local void
fork_process(w)
TermWidget w;
{
     int i;
     int omask;

     /* Make sure we don't get the signal just yet - do it the BSD way,
	linux doesn't support sighold */
     omask = sigblock(sigmask(SIGCHLD));

     if (!(w->term.pid = fork())) {
	  /* Now in child process */
	  char *args[2], **argp;
	  int fd;
      	  struct group *gr;

	  /* We're not particularly interested */
	  signal(SIGCHLD, SIG_DFL);

      	  if (setsid() < 0)
               pmessage("failed to set process group");

	  /* Close everything in sight */
	  for (i = 0; i < NFILES; i++)
	       close(i);

	  /*
	   * Open the slave side for stdin and dup twice for stdout and stderr.
	   * We need to open all fd's read/write since some weird applications
	   * (like "more") READ from stdout/stderr or WRITE to stdin!
	   * Ack, bletch, barf.
	   *
	   * This should also take care of the controlling TTY. I hope.
	   */
          fd = open(w->term.tname, O_RDWR, 0);		/* 0 */
          dup(fd);					/* 1 */
          dup(fd);					/* 2 */

	  /* It's MINE, I tell you! Mine! Mine! Mine! */
	  fchown(fd, w->term.uid, w->term.gid);
	  fchmod(fd, 0622);

	  /* set various signals to non annoying (for SYSV) values */
	  if (getpgrp() == getpid()) {
	       signal(SIGINT, exit);
	       signal(SIGQUIT, exit);
	       signal(SIGTERM, exit);
	  }
	  else {
	       signal(SIGINT, SIG_IGN);
	       signal(SIGQUIT, SIG_IGN);
	       signal(SIGTERM, SIG_IGN);
	  }
	  signal(SIGPIPE, exit);

	  /* since we may be setuid root (for utmp), be safe */
	  setuid(w->term.uid);
	  setgid(w->term.gid);

#ifdef DEBUG
          debug("child: opened %s for stdin stdout stderr", w->term.tname);
          debug("child: about to exec %s.", w->term.command);
#endif
	  args[1] = NULL;
	  argp = w->term.command_args ? w->term.command_args : args;
	  if (w->term.login_shell)
	       argp[0] = "-";
	  else	
	       argp[0] = w->term.command;
	  execvp(w->term.command, argp);
	  /*
	   * This will show up in the client window (hopefully), so
	   * sleep for some time before exiting to let the user read
	   * the message
	   */
	  pmessage("Couldn't exec %s", args[0]);
	  sleep(5);
	  exit(PROC_EXEC_FAILED);
     }
     /* Now is the time to add the utmp entry */
     tty_add_utmp(w);

     /*
      * Now that we have stored the widget ID, we can enable SIGCHLD
      * and deal with a possible early failure. 
      */
     sigsetmask(omask);
}

Export void
process_init(w)
TermWidget w;
{
     char envterm[2048], *tmp;
     String res;

     /* record uid and gid values for later use */
     w->term.uid = getuid();
     w->term.gid = getgid();

     /* TERM */
     sprintf(envterm, "TERM=%s", w->term.term_type);
     tmp = XtNewString(envterm);
     putenv(tmp);

     /* TERMCAP */
     if (res = get_sub_resource_string((Widget)w, w->term.term_type,
				       "termcap", "Termcap")) {
	  strcpy(envterm, "TERMCAP=");
	  strcat(envterm, res);
	  putenv(envterm);
     }

     fork_process(w);
     XpNrememberWidget(w);
}

Export void
process_cleanup(w)
TermWidget w;
{
     /* Just remove the utmp entry */
     tty_remove_utmp(w);
}

#define WAIT_STATUS_TYPE int
Local int
process_wait(fc)
int *fc;
{
     int pid;
     
     if ((pid = wait3(fc, WNOHANG, (struct rusage *)NULL)) == -1)
	  perror("wait");
     return pid;
}

#endif	/* _PROCESS_LINUX_INCLUDE */
