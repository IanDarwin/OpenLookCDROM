#if !defined(lint) && !defined(__clipper__)
static char *rcsid = "process.c,v 1.2 1994/05/27 06:20:58 me Exp";
#endif

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
 * Various process manipulation routines.
 *
 * Author: Jordan K. Hubbard
 * Date: March 20th, 1990.
 * Description: Here are all the public routines for frobbing the state of
 *		of the main process.
 *
 * Revision History:
 *
 * process.c,v
 * Revision 1.2  1994/05/27  06:20:58  me
 * New copyright message
 *
 * Revision 1.1.1.1  1994/05/22  11:22:44  me
 * Initial import into CVS
 *
 * Revision 1.17  92/05/16  06:11:44  jkh
 * Reorganization for new process driver structure.
 * 
 * Revision 1.16  92/02/26  11:39:56  me
 * Steve Crooks' clix port and general cleanup
 * 
 * Revision 1.10  90/10/12  13:44:37  jkh
 * Renaming and reorganization of system dependent stuff.
 */

#include "TermP.h"
#include "TermCanvas.h"
#include <sys/wait.h>

/*
 * Since different systems vary widely in their handling of handling 
 * processes, each system type has its own file. If you can get by with
 * making only subtle tweaks to an existing file, then by all means
 * #ifdef them in, but DO NOT make major changes in this way; create
 * another file instead. I am trying to avoid the crazed proliferation
 * of #ifdefs that makes so much of the original xterm code unreadable.
 * Thanks. -jkh
 *
 * I use "#if defined(xxx)" instead of "#ifdef xxx" so that you can easily
 * append new conditionals for systems that share existing .i files. Sticking
 * to this convention is encouraged.
 *
 * In order to avoid #ifdefs that walk across the screen (as was before the
 * case), I've changed things so that the pre-processor symbol is assumed
 * to be unique (if there is a clash, && together some combination of
 * symbols that are unique for your system) and only one file will be
 * included.  sysdep/<file>_process.i is now expected to define the
 * preprocessor symbol _PROCESS_INCLUDED.
 * 
 * Note: Do not simply say something like "#if defined(BSD) ..." since
 * that will almost certainly break other systems that are merely derived
 * from BSD and need their own specific initialization files to be included
 * instead.  Always include a unique file for your system and then inherit
 * those routines from bsd/sysv/posix (or whatever) when and where you can.
 * Some of the existing configurations do this, so look to them for working
 * examples. - Jordan
 *
 */

#if defined(PCS)
#     include "sysdep/proc_pcs.i"
#endif

#if defined(mips) && !defined(PCS)
#     include "sysdep/proc_mips.i"
#endif

#if defined(sun)
#     include "sysdep/proc_sun.i"
#endif

#if defined(__clipper__)
#     include "sysdep/proc_clix.i"
#endif

#if defined(MACH)
#     include "sysdep/proc_mach.i"
#endif

#if defined(hpux)
#     include "sysdep/proc_hpux.i"
#endif

#if defined(SVR4)
#     include "sysdep/proc_gen_svr4.i"
#endif

#if defined(__BSD_NET2__)
#     include "sysdep/proc_gen_bsd.i"
#endif

#if defined(linux)
#     include "sysdep/proc_linux.i"
#endif

#ifndef _PROCESS_INCLUDED
#include "sysdep/unknown.i"
#endif


/* System independent (hopefully) work procs */

/*
 * Child death handler. Should be largely (UNIX) system independent, but
 * those with really specialized needs are free to either #ifdef this or
 * move it into an appropriate proc_<sys>.i file.  The local process
 * file is expected to define WAIT_STATUS_TYPE as well as the W<blah>
 * macros, if necessary.
 */
/*ARGSUSED*/
Local void
process_reaper(int sig)
{
     int pid;
     TermWidget w;
     WAIT_STATUS_TYPE failcode;

     /* Ok, who croaked? */
     pid = process_wait(&failcode);

     /* Try and find a widget associated with this pid */
     if ((w = XpNfindWidgetByPid(pid)) != NULL) {
	  XpNremoveRememberedWidget(w);

	  /* Pre-process fail code for special cases */
	  switch (WEXITSTATUS(failcode)) {
	  case PROC_EXEC_FAILED:
	       warn("Couldn't exec %s", w->term.command);
	       break;

	  case PROC_TTYOPEN_FAILED:
	       warn("Sub-process couldn't open /dev/tty");
	       break;

	  default:
	       if (WIFSTOPPED(failcode))
		    warn("Process %s caught signal %d",
			 w->term.command, WSTOPSIG(failcode));
	  }

	  /* Avoid spurious errors */
	  tty_close(w);

	  /* Call process cleanup code */
	  process_cleanup(w);

	  /* let the destroy handler do the rest */
	  process_destroy(w, failcode);
     }
     /*
      * If we couldn't find the process, just fall through. Note that this
      * is eminently possible since we use popen() in a few places which
      * will generate spurious calls to this routine.
      */
#if defined(SYSV) && !defined(__clipper__)
     /* Reset for SYSV braindeath */
     signal(SIGCHLD, process_reaper);
#endif
}

Import TermClassRec termClassRec;

/* Create the process */
Export void
process_create(TermWidget w)
{
     /* Make sure the class signal handler gets invoked */
#ifdef __clipper__
     sigset(SIGCHLD, process_reaper);
#else
     signal(SIGCHLD, process_reaper);
#endif
     ++termClassRec.term_class.sig_cnt;

     /* Call the system dependent routine to actually get the work done */
     process_init(w);

     /*
      * It's really kludge to init the canvas from here, but it solves
      * number of problems with making the client do it. This is only
      * for the very first initialization, subsequent calls are done
      * automatically by parser_install().
      */
     XpTermDoRop(w, ROP_INIT_CANVAS);
}

/*
 * Nuke the process. "Fail" will be set to the process's return code
 * if this routine is being called from the class's SIGCHLD signal handler.
 */

Export void
process_destroy(TermWidget w, int fail)
{
     /* Call any callbacks we have for child deletion */
     XtCallCallbackList((Widget)w, w->term.proc_death, (XtPointer)fail);

     /*
      * Let the widget Destroy handler do most of the memory cleanup work
      * so we don't have to dup the code.
      */
     XtDestroyWidget((Widget)w);

     /* If we don't need it anymore, remove it */
     if (!--termClassRec.term_class.sig_cnt)
#ifdef __clipper__
	  sigset(SIGCHLD, SIG_DFL);
#else
	  signal(SIGCHLD, SIG_DFL);
#endif
}
     
Export void
process_signal(TermWidget w, int sig)
{
     if (w->term.pid)
	  kill(w->term.pid, sig);
}
