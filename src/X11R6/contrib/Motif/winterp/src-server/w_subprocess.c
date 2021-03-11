/* -*-C-*-
********************************************************************************
*
* File:         w_subprocess.c
* RCS:          $Header: /users/npm/src/winterp/src-server/RCS/w_subprocess.c,v 2.7 1994/06/06 15:40:53 npm Exp $
* Description:  Facilities for creating and interacting with subprocesses;
*		Interfaces to Don Libes' (libes@cme.nist.gov) 'expect'
*		library in ../src-server/expect/.
* Author:       Niels Mayer
* Created:      Sat Jul  6 00:17:23 1991
* Modified:     Sun Jun  5 14:51:51 1994 (Niels Mayer) npm@indeed
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
static char rcs_identity[] = "@(#)$Header: /users/npm/src/winterp/src-server/RCS/w_subprocess.c,v 2.7 1994/06/06 15:40:53 npm Exp $";

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
#include <sys/types.h>		/* for EXP_WAIT, EXP_KILL */
#include <sys/wait.h>		/* for EXP_WAIT */
#include <signal.h>		/* for EXP_KILL */
#include <errno.h>
#include <Xm/Xm.h>		/* Xm/Xm.h only needed for "winterp.h"*/
#include "winterp.h"
#include "expect/expect.h"

extern char *sys_errlist[];	/* part of <errno.h>, but not defined by it. */
extern int sys_nerr;		/* part of <errno.h>, but not defined by it. */
extern int errno;		/* some but not all systems require this */


/******************************************************************************
 * (EXP_SPAWN <executable-filepath> <arg0> [<arg1> [[<arg2>] ...] )
 *	--> returns a STREAM (with no buffering, however)
 *		returns NIL if an error occurs...
 * where
 * <executable-filepath> is a string, the full path and name of the executable.
 * <arg0> is a string corresponding to argv[0] in the spawned executable,
 *        this should typically be the same as <executable-filepath>.
 * <arg-i> are optional string arguments corresponding to argv[i] passed to
 *        the spawned executable.
 *
 * Call EXP_GET_PID after calling EXP_SPAWN to find out process ID.
 * To shut down the subprocess, call CLOSE on the returned STREAM, followed
 * by a call to EXP_WAIT.
 *
 * Prior to calling EXP_SPAWN, if EXP_STTY_INIT is called, the string arg to
 * that function is interpreted in the style of stty(1) arguments as further
 * configuration for any pty used by future spawn commands. For example,
 * exp_stty_init = "sane" repeats the default initialization.
 * 
 * If EXP_STTY_INIT was never called prior to calling EXP_SPAWN, then 
 * the pty is initialized the same way as the user's tty.  When this is not
 * possible (i.e., expect was not started with a controlling terminal), spawn
 * uses the tty settings that correspond to "stty sane".
 *
 * Note: it is ok to write to the EXP_SPAWN-created subprocess with FORMAT,
 * PRINT, PRIN1, PRINC PPRINT, TERPRI, WRITE-CHAR, and WRITE-BYTE.
 * However, one must be very careful in reading from the STREAM created by
 * EXP_SPAWN -- because you can't know how many characters are
 * "ready for reading", you shouldn't use READ, READ-LINE, because they might
 * cause WINTERP to lock-up waiting to read a character that may never arrive.
 * For reading, you should use READ-NONBLOCKING or EXP_EXPECT with a suitable
 * timeout.
 *
 *------------------------------------------------------------------------------
 * int exp_spawnv(file,argv);
 * char *file, *argv[ ];
 *
 *       exp_spawnv forks a new process so that its stdin,
 *       stdout, and stderr can be written and read by the current process.
 *       file is the name of a file to be executed.  The arg pointers are
 *       null-terminated strings.  Following the style of execve(), arg0 (or
 *       argv[0]) is customarily a duplicate of the name of the file.
 * 
 *       If the process is successfully created, a file descriptor is returned
 *       which corresponds to the process's stdin, stdout and stderr.  A stream
 *       may be associated with the file descriptor by using fdopen().  (This
 *       should almost certainly be followed by setbuf() to unbuffer the I/O.)
 * 
 *       Closing the file descriptor will typically be detected by the process
 *       as an EOF.  Once such a process exits, it should be waited upon (via
 *       wait) in order to free up the kernel process slot.  (Some systems
 *       allow you to avoid this if you ignore the SIGCHLD signal).
 * 
 *       After a process is started, the variable exp_pid is set to the
 *       process-id of the new process.
 * 
 *       The spawn functions uses a pty to communicate with the process.  By
 *       default, the pty is initialized the same way as the user's tty.  When
 *       this is not possible (i.e., expect was not started with a controlling
 *       terminal), spawn uses the tty settings that correspond to "stty sane".
 *       If the variable exp_stty_init is defined, it is interpreted in the
 *       style of stty arguments as further configuration for any pty used by
 *       future spawn commands.  For example, exp_stty_init = "sane"; repeats
 *       the default initialization.
 ******************************************************************************/
LVAL Prim_EXP_SPAWN()
{
  String* spawn_argv;
  String  exec_file;
  int     i;
  int	  fd;

  exec_file = getstring(xlgastring()); /* get req'd <executable-filepath> */
  if (!moreargs())		/* make sure that there's at least one more arg */
    xltoofew();

  /* allocate space for argv including NULL termination */
  spawn_argv = (String*) XtMalloc((unsigned) ((xlargc + 1) * sizeof(String)));

  for (i = 0 ; moreargs() ; i++) {
    LVAL lval_str = nextarg();
    if (!stringp(lval_str)) {
      XtFree((char*) spawn_argv);
      xlbadtype(lval_str);
    }
    spawn_argv[i] = getstring(lval_str); /* get <arg-i> */
  }
  spawn_argv[i] = NULL;

  fd = exp_spawnv(exec_file, spawn_argv);

  XtFree((char*) spawn_argv);

  if (fd < 0) {
    if (errno == ENODEV) {
      (void) sprintf(temptext, "ran out of ptys while spawning %s -- too many programs spawned?",
		     exec_file);
      xlfail(temptext);
    }
    else {
      if (errno < sys_nerr)
	(void) sprintf(temptext, "in spawning, %s", sys_errlist[errno]);
      else
	(void) strcpy(temptext, "in spawning, unknown error.");
      xlfail(temptext);
    }
  }
  else {
    FILE *fp;
    if (!(fp = fdopen(fd, "r+")))
      return (NIL);
    setbuf(fp, NULL);

#ifdef BETTERIO
    return (cvfile(fp, S_FORREADING|S_FORWRITING)); /* NOTE: STREAM in xlisp has buffer char (???) */
#else /* !defined(BETTERIO) */
    return (cvfile(fp));	/* NOTE: STREAM in xlisp has buffer char (???) */
#endif /* BETTERIO */
  }
}


/******************************************************************************
 * (EXP_POPEN <command>)
 *	--> returns a STREAM (FILE*) or NIL
 *
 * Unlike POPEN, EXP_POPEN allows for reading/writing to the <command> subprocess
 * and executing <command> doesn't block.
 *
 * Call EXP_GET_PID after calling EXP_POPEN to find out process ID.
 * To shut down the subprocess, call CLOSE on the returned STREAM, followed
 * by a call to EXP_WAIT.
 *
 * Prior to calling EXP_POPEN, if EXP_STTY_INIT is called, the string arg to
 * that function is interpreted in the style of stty(1) arguments as further
 * configuration for any pty used by future spawn commands. For example,
 * exp_stty_init = "sane" repeats the default initialization.
 * 
 * If EXP_STTY_INIT was never called prior to calling EXP_POPEN, then 
 * the pty is initialized the same way as the user's tty.  When this is not
 * possible (i.e., expect was not started with a controlling terminal), spawn
 * uses the tty settings that correspond to "stty sane".
 *
 * Note: it is ok to write to the EXP_SPAWN-created subprocess with FORMAT,
 * PRINT, PRIN1, PRINC, PPRINT, TERPRI, WRITE-CHAR, and WRITE-BYTE.
 * However, one must be very careful in reading from the STREAM created by
 * EXP_SPAWN -- because you can't know how many characters are
 * "ready for reading", you shouldn't use READ, READ-LINE, because they might
 * cause WINTERP to lock-up waiting to read a character that may never arrive.
 * For reading, you should use READ-NONBLOCKING or EXP_EXPECT with a suitable
 * timeout.
 *------------------------------------------------------------------------------
 *
 * FILE* exp_popen(command);
 * char *command;
 * 
 *    Similar to exp_spawnv above, exp_popen is yet another interface,
 *    styled after popen().  It takes a Bourne shell command line, and returns
 *    a stream that corresponds to the process's stdin, stdout and stderr. 
 ******************************************************************************/
LVAL Prim_EXP_POPEN()
{
  char *command;
  FILE *fp;

  command = getstring(xlgastring());
  xllastarg();

  fp = exp_popen(command);

  if (!fp) {
    if (errno == ENODEV) {
      (void) sprintf(temptext, "ran out of ptys while spawning popen(%s) -- too many programs spawned?",
		     command);
      xlfail(temptext);
    }
    else {
      if (errno < sys_nerr)
	(void) sprintf(temptext, "in spawning, %s", sys_errlist[errno]);
      else
	(void) strcpy(temptext, "in spawning, unknown error.");
      xlfail(temptext);
    }
  }

#ifdef BETTERIO
  return (fp ? cvfile(fp, S_FORREADING|S_FORWRITING) : NIL);
#else /* !defined(BETTERIO) */
  return (fp ? cvfile(fp) : NIL);
#endif /* BETTERIO */
}


/******************************************************************************
 * (EXP_GET_PID)
 * 	--> FIXNUM representing the process ID of the last EXP_POPEN or EXP_SPAWN.
 *
 *------------------------------------------------------------------------------
 * extern int exp_pid;
 *
 *    ... After a process is started, the variable exp_pid is set to the
 *    process-id of the new process...
 ******************************************************************************/
LVAL Prim_EXP_GET_PID()
{
  xllastarg();
  return (cvfixnum((FIXTYPE) exp_pid));
}


/******************************************************************************
 * (EXP_STTY_INIT <stty-string>)
 *	--> NIL
 *
 * If EXP_STTY_INIT is called prior to calling EXP_POPEN or EXP_SPAWN,
 * <stty-string> is interpreted in the style of stty(1) arguments as further
 * configuration for any pty used by future EXP_POPEN or EXP_SPAWN commands.
 *
 * For example,
 * (EXP_STTY_INIT "sane")
 * 		-- repeats the default initialization.
 * (EXP_STTY_INIT "-echo -echoe -echok")
 *		-- turns off echoing of text written to the subprocess.
 * 
 * If EXP_STTY_INIT was never called prior to calling EXP_POPEN or EXP_SPAWN,
 * then the pty is initialized the same way as the user's tty.  When this is not
 * possible (i.e., WINTERP was not started with a controlling terminal), spawn
 * uses the tty settings that correspond to "stty sane".
 *------------------------------------------------------------------------------
 * extern char *exp_stty_init;
 ******************************************************************************/
LVAL Prim_EXP_STTY_INIT()
{
  extern char* exp_stty_init;	/* from expect/lib_exp.c */
  char* stty_string;
  int	len;

  stty_string = getstring(xlgastring());
  xllastarg();

  /* If previously set string exp_stty_init, free previous string ... */
  if ((exp_stty_init != 0) && (*exp_stty_init != 0)) /* if not set to default/init value */
    XtFree((char*) exp_stty_init); /* free it */

  /*
   * Make a copy of stty_string and set that to exp_stty_init.
   * The copy is needed because the stty_string comes from the lisp argument
   * to this function, which could get garbage collected (aka free()'d)
   * before EXP_SPAWN or EXP_POPEN get called.
   */
  len = strlen(stty_string);    
  exp_stty_init = XtMalloc((unsigned) len + 1);
  strncpy(exp_stty_init, stty_string, len);
  exp_stty_init[len] = '\0';

  return (NIL);
}


/******************************************************************************
 * (EXP_WAIT)
 *	--> returns dotted pair (pid . exit-status) on success;
 *          returns dotted pair (-1  . sys_errlist[errno]) on failure.
 *
 * call this after calling CLOSE on stream returned by EXP_SPAWN or EXP_POPEN.
 ******************************************************************************/
LVAL Prim_EXP_WAIT()
{
#ifdef X_NOT_POSIX		/* an X11r5/Imake define, e.g. used for NeXT/CoeXist compile */
  union wait statusp;		/* see wait(2) def for your machine */
  int pid;			/* if your C compiler doesn't grok pid_t, use "int" */
#else /* POSIX */
  int statusp;			/* see wait(2) def for your machine */
  pid_t pid;
#endif /* X_NOT_POSIX */
  LVAL lval_result, lval_pid, lval_exit_status;

  /* protect some pointers */
  xlstkcheck(3);
  xlsave(lval_result);
  xlsave(lval_pid);
  xlsave(lval_exit_status);

  xllastarg();

  pid = wait(&statusp);

  if (pid < 0) {
    if (errno < sys_nerr)
      (void) sprintf(temptext, "wait(2) error on spawned subprocess -- %s", sys_errlist[errno]);
    else
      (void) strcpy(temptext, "wait(2) error on spawned subprocess -- unknown error.");
    lval_exit_status = cvstring(temptext);
  }
  else
#ifdef X_NOT_POSIX		/* an X11r5/Imake define, e.g. used for NeXT/CoeXist compile */
    lval_exit_status = cvfixnum((FIXTYPE) statusp.w_status);
#else /* POSIX */
    lval_exit_status = cvfixnum((FIXTYPE) statusp);
#endif /* X_NOT_POSIX */

  lval_pid = cvfixnum((FIXTYPE) pid);

  lval_result = cons(lval_pid, lval_exit_status);

  /* restore the stack */
  xlpopn(3);

  return (lval_result);
}

/******************************************************************************
 * (EXP_KILL <signal> <proccess-id-fixnum>)
 *	--> returns NIL
 *
 * Calls kill(2) returning NIL if successful. Signals an error otherwise.
 * See kill(2) for details/semantics.
 *
 *	<signal> is either a FIXNUM or a STRING. See "/usr/include/sys/signal.h"
 *	on your system for appropriate values for <signal>. Alternately, you can
 *	specify a STRING name for the signal -- the string name is the same
 *	as the SIG* names in "/usr/include/sys/signal.h", but without the "SIG"
 *	prefix. For example (EXP_KILL <pid> "INT") sends a SIGINT to <pid>.
 *
 *	<process-id-fixnum> is a FIXNUM representing a process ID. Process id
 *	FIXNUMs are returned, for example, by EXP_GET_PID.
 *
 ******************************************************************************/
LVAL Prim_EXP_KILL()
{
  int sig;
  LVAL lval_signal = xlgetarg();
#ifdef X_NOT_POSIX		/* an X11r5/Imake define, e.g. used for NeXT/CoeXist compile -- if your C compiler doesn't grok pid_t, use "int" */
  int pid = (int) getfixnum(xlgafixnum());
#else  /* POSIX */
  pid_t pid = (pid_t) getfixnum(xlgafixnum());
#endif /* X_NOT_POSIX */
  xllastarg();

  if (stringp(lval_signal)) {
    char* str_signal = getstring(lval_signal);
    if (0 == 1) {
      /* bogus... */
    }
#ifdef SIGHUP
    /* SIGHUP      1       Exit      Hangup [see termio(7)] */
    else if (0 == strcmp(str_signal, "HUP"))
      sig = SIGHUP;
#endif
#ifdef SIGINT
    /* SIGINT      2       Exit      Interrupt [see termio(7)] */
    else if (0 == strcmp(str_signal, "INT"))
      sig = SIGINT;
#endif
#ifdef SIGQUIT
    /* SIGQUIT     3       Core      Quit [see termio(7)] */
    else if (0 == strcmp(str_signal, "QUIT"))
      sig = SIGQUIT;
#endif
#ifdef SIGILL
    /* SIGILL      4       Core      Illegal Instruction */
    else if (0 == strcmp(str_signal, "ILL"))
      sig = SIGILL;
#endif
#ifdef SIGTRAP
    /* SIGTRAP     5       Core      Trace/Breakpoint Trap */
    else if (0 == strcmp(str_signal, "TRAP"))
      sig = SIGTRAP;
#endif
#ifdef SIGABRT
    /* SIGABRT     6       Core      Abort */
    else if (0 == strcmp(str_signal, "ABRT"))
      sig = SIGABRT;
#endif
#ifdef SIGEMT
    /* SIGEMT      7       Core      Emulation Trap */
    else if (0 == strcmp(str_signal, "EMT"))
      sig = SIGEMT;
#endif
#ifdef SIGFPE
    /* SIGFPE      8       Core      Arithmetic Exception */
    else if (0 == strcmp(str_signal, "FPE"))
      sig = SIGFPE;
#endif
#ifdef SIGKILL
    /* SIGKILL     9       Exit      Killed */
    else if (0 == strcmp(str_signal, "KILL"))
      sig = SIGKILL;
#endif
#ifdef SIGBUS
    /* SIGBUS      10      Core      Bus Error */
    else if (0 == strcmp(str_signal, "BUS"))
      sig = SIGBUS;
#endif
#ifdef SIGSEGV
    /* SIGSEGV     11      Core      Segmentation Fault */
    else if (0 == strcmp(str_signal, "SEGV"))
      sig = SIGSEGV;
#endif
#ifdef SIGSYS
    /* SIGSYS      12      Core      Bad System Call */
    else if (0 == strcmp(str_signal, "SYS"))
      sig = SIGSYS;
#endif
#ifdef SIGPIPE
    /* SIGPIPE     13      Exit      Broken Pipe */
    else if (0 == strcmp(str_signal, "PIPE"))
      sig = SIGPIPE;
#endif
#ifdef SIGALRM
    /* SIGALRM     14      Exit      Alarm Clock */
    else if (0 == strcmp(str_signal, "ALRM"))
      sig = SIGALRM;
#endif
#ifdef SIGTERM
    /* SIGTERM     15      Exit      Terminated */
    else if (0 == strcmp(str_signal, "TERM"))
      sig = SIGTERM;
#endif
#ifdef SIGUSR1
    /* SIGUSR1     16      Exit      User Signal 1 */
    else if (0 == strcmp(str_signal, "USR1"))
      sig = SIGUSR1;
#endif
#ifdef SIGUSR2
    /* SIGUSR2     17      Exit      User Signal 2 */
    else if (0 == strcmp(str_signal, "USR2"))
      sig = SIGUSR2;
#endif
#ifdef SIGCHLD
    /* SIGCHLD     18      Ignore    Child Status Changed */
    else if (0 == strcmp(str_signal, "CHLD"))
      sig = SIGCHLD;
#endif
#ifdef SIGPWR
    /* SIGPWR      19      Ignore    Power Fail/Restart */
    else if (0 == strcmp(str_signal, "PWR"))
      sig = SIGPWR;
#endif
#ifdef SIGWINCH
    /* SIGWINCH    20      Ignore    Window Size Change */
    else if (0 == strcmp(str_signal, "WINCH"))
      sig = SIGWINCH;
#endif
#ifdef SIGURG
    /* SIGURG      21      Ignore    Urgent Socket Condition */
    else if (0 == strcmp(str_signal, "URG"))
      sig = SIGURG;
#endif
#ifdef SIGPOLL
    /* SIGPOLL     22      Ignore    Pollable Event [see streamio(7)] */
    else if (0 == strcmp(str_signal, "POLL"))
      sig = SIGPOLL;
#endif
#ifdef SIGSTOP
    /* SIGSTOP     23      Stop      Stopped (signal) */
    else if (0 == strcmp(str_signal, "STOP"))
      sig = SIGSTOP;
#endif
#ifdef SIGTSTP
    /* SIGTSTP     24      Stop      Stopped (user) [see termio(7)] */
    else if (0 == strcmp(str_signal, "TSTP"))
      sig = SIGTSTP;
#endif
#ifdef SIGCONT
    /* SIGCONT     25      Ignore    Continued */
    else if (0 == strcmp(str_signal, "CONT"))
      sig = SIGCONT;
#endif
#ifdef SIGTTIN
    /* SIGTTIN     26      Stop      Stopped (tty input) [see termio(7)] */
    else if (0 == strcmp(str_signal, "TTIN"))
      sig = SIGTTIN;
#endif
#ifdef SIGTTOU
    /* SIGTTOU     27      Stop      Stopped (tty output) [see termio(7)] */
    else if (0 == strcmp(str_signal, "TTOU"))
      sig = SIGTTOU;
#endif
#ifdef SIGVTALRM
    /* SIGVTALRM   28      Exit      Virtual Timer Expired */
    else if (0 == strcmp(str_signal, "VTALRM"))
      sig = SIGVTALRM;
#endif
#ifdef SIGPROF
    /* SIGPROF     29      Exit      Profiling Timer Expired */
    else if (0 == strcmp(str_signal, "PROF"))
      sig = SIGPROF;
#endif
#ifdef SIGXCPU
    /* SIGXCPU     30      Core      CPU time limit exceeded [see getrlimit(2)] */
    else if (0 == strcmp(str_signal, "XCPU"))
      sig = SIGXCPU;
#endif
#ifdef SIGXFSZ
    /* SIGXFSZ     31      Core      File size limit exceeded [see getrlimit(2)] */
    else if (0 == strcmp(str_signal, "XFSZ"))
      sig = SIGXFSZ;
#endif
#ifdef SIGRTMIN
    /* SIGRTMIN    49      Exit      Posix.4 SIGRTMIN */
    else if (0 == strcmp(str_signal, "RTMIN"))
      sig = SIGRTMIN;
#endif
#ifdef SIGRTMAX
    /* SIGRTMAX    64      Exit      Posix.4 SIGRTMAX */
    else if (0 == strcmp(str_signal, "RTMAX"))
      sig = SIGRTMAX;
#endif
    else {
      xlbadtype(lval_signal);
    }
  }
  else if (fixp(lval_signal)) {
    sig = (int) getfixnum(lval_signal);
  }
  else {
    xlbadtype(lval_signal);
  }

  if (0 == kill(pid, sig)) {
    return (NIL);
  }
  else {
    if (errno < sys_nerr)
      (void) sprintf(temptext, "in kill(2), %s", sys_errlist[errno]);
    else
      (void) strcpy(temptext, "in kill(2), unknown error.");
    xlfail(temptext);
  }
}


#if 0
/******************************************************************************
 * (EXP_EXPECT <stream> :<keywd-0> <pat-str-0>
 * 			:<keywd-1> <pat-str-1>
 *			...
 * 			:<keywd-n> <pat-str-n>)
 * 	--> returns :<keywd-i> or :EOF or :TIMEOUT
 *
 * 		usage:
 * 		(case (exp_expect p :FOO "foo" :BAR "bar*" :BAZ "baz")
 * 			(:FOO
 * 				(EXP_GET_MATCH p)
 * 			)
 * 			(:BAR
 * 				(EXP_GET_MATCH p)
 * 			)
 * 			(:BAZ
 * 				(EXP_GET_MATCH p)
 * 			)
 * 			(:EOF
 * 				"eof"
 * 			)
 * 			(:TIMEOUT
 * 				"timeout"
 * 			)
 * 			(T
 * 				(error)
 * 			)
 * 		)
 *-----------------------------------------------------------------------------
 * Alternately, we could have something like this:
 *
 * (EXP_EXPECT <stream> <re-string-0> '( <code0> )
 * 			<re-string-1> '( <code1> )
 *			...
 * 			<re-string-n> '( <code-n> )
 *			:EOF	      '( <code-eof> )
 *                      :TIMEOUT      '( <code-timeout> )
 * 	--> returns the result of evaluating one of the <code>s above.
 *
 * Within the lexical scope of <code>, 
 *	EXPECT_MATCH is bound to the string matching <re-string-i>
 *	EXPECT_MATCH_LENGTH is bound to the max length of EXPECT_MATCH
 *	EXPECT_TIMOUT is bound to the # of seconds of timeout.
 *
 * (but why duplicate all the work that already exists in "case"???)
  ******************************************************************************/
LVAL Prim_EXP_EXPECT()
{

}


/******************************************************************************
 * (EXP_GET_MATCH)
 * 	--> returns a STRING, the last match attempted by EXP_EXPECT.
 ******************************************************************************/
LVAL Prim_EXP_GET_MATCH()
{

}


/******************************************************************************
 * (EXP_GET_MATCH_MAX_SIZE)
 * 	--> returns a FIXNUM, the max number of bytes in a match-string returned
 *          by EXP_EXPECT.
 ******************************************************************************/
LVAL Prim_EXP_GET_MATCH_MAX_SIZE()
{

}


/******************************************************************************
 * (EXP_SET_MATCH_MAX_SIZE <size>)
 * 	--> returns <size>.
 * <size> is a FIXNUM, the max length of a string that can be matched by
 * EXP_EXPECT.
 ******************************************************************************/
LVAL Prim_EXP_SET_MATCH_MAX_SIZE()
{

}


/******************************************************************************
 * (EXP_GET_TIMEOUT)
 * 	--> returns a FIXNUM, the timeout value in seconds for EXP_EXPECT.
 ******************************************************************************/
LVAL Prim_EXP_GET_TIMEOUT()
{

}


/******************************************************************************
 * (EXP_SET_TIMEOUT <timeout>)
 * 	--> returns <timeout>.
 * <timeout> is a FIXNUM, the timeout value in seconds for EXP_EXPECT.
 ******************************************************************************/
LVAL Prim_EXP_SET_TIMEOUT()
{

}


/******************************************************************************
 * (READ-NONBLOCKING <stream>)
 * 	--> returns a STRING of as many characters as could be read from
 *          <stream>.
 *
 * Note that READ-NONBLOCKING is suitable for use inside XT_ADD_INPUT, and
 * is suitable for reading the output of subprocesses started via
 * EXP_POPEN and EXP_SPAWN.
 ******************************************************************************/
LVAL Prim_EXP_READ_NONBLOCKING()
{

}
#endif /* if 0 */
