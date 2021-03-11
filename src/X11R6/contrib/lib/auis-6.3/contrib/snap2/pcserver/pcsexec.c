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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/contrib/snap2/pcserver/RCS/pcsexec.c,v 2.13 1993/09/22 19:52:42 gk5g Exp $";
#endif

/*
 *	PC Server - Command Execution Component
 *	Access to the UNIX File System for IBM PC/XT/ATs
 *
 *	(C)Copyright IBM Corporation, 1984, 1985, 1986, 1987
 *	Program Property of IBM
 *
 *	Author - Larry K. Raper
 *	Version 1.3
 *	Developed for the Information Technology Center at
 *	Carnegie-Mellon University
 *
 *	Release history
 *	1.0 08/86 Initial release
 *	1.1 11/86 Disassociates /dev/tty so passwd can be exec'd
 *	1.2 03/87 Added CleanUpOrphans
 *	1.3 10/87 Remove unreferenced variables
 *
 */

#include <stdio.h>
#include <andrewos.h> /* sys/file.h */
#include <sys/wait.h>
#include <pwd.h>
#include <sys/ioctl.h>
#include <ctype.h>
#include <pcserver.h>

#define TRUE 1
#define FALSE 0

extern int PCS_debuglevel;

char **environ;
char *MyEnvironment[10];
#define USUALPATH "PATH=%s/bin:/usr/andrew/bin:/usr/andy/bin:/usr/local/bin:\
/usr/ucb:/usr/bin:/bin:/usr/etc:/etc"

#ifndef _IBMR2
char *malloc ();
#endif

extern int errno;

SetEnvironment ()
{
    struct passwd *p;
    static int init_done = FALSE;
    int fd;

    if (init_done)
	return;

    p = getpwuid(getuid());
    if (p == NULL) {
	Terminate (-2);
    }

    MyEnvironment[0] = malloc (8 + strlen (p->pw_dir));
    sprintf (MyEnvironment[0], "HOME=%s", p->pw_dir);
    MyEnvironment[1] = malloc (8 + strlen (p->pw_shell));
    sprintf (MyEnvironment[1], "SHELL=%s", p->pw_shell);
    MyEnvironment[2] = malloc (8 + strlen (p->pw_name));
    sprintf (MyEnvironment[2], "USER=%s", p->pw_name);
    MyEnvironment[3] = malloc (sizeof (USUALPATH) + strlen (p->pw_dir) + 2);
    sprintf (MyEnvironment[3], USUALPATH, p->pw_dir);

    MyEnvironment[4] = NULL;
    environ = MyEnvironment;

#ifdef TIOCNOTTY
    /* Grubby fix to disassociate this process from /dev/tty, */
    /* courtesy of Bruce Lucas and Drew Perkins */
    fd=open("/dev/tty", 2);
    if (fd>=0) {
	ioctl(fd, TIOCNOTTY, 0);
	close(fd);
    }
#endif

    init_done = TRUE;
    return;
}

PrintEnv ()
{
    int i = 0;
    printf ("Current environment is:\n\n");
    if (environ == NULL)
	printf ("[empty]\n");
    else
	while (environ[i])
	    printf ("%s\n", environ[i++]);
    printf ("\n");
}

ShiftLeft (str, n)
char *str;
int n;
{
    register int i, len;

    if (n == 0)
	return;
    len = strlen (str);
    if (len < n) {
	str[0] = 0;
	return;
    }
    for (i=0; str[i] = str[i+n]; i++);
}

RemoveDoubleQuote (tok)
char *tok;
{
    register int i;
    for (i=0; tok[i]; i++)
	if (tok[i] == '\"')
	    ShiftLeft (tok+i, 1);
}

int NoSeparator1 (chr)
char chr;
{
    if ((chr != 0) && (chr != ' ') && (chr != ',') && (chr != '\t'))
	return (TRUE);
    else
	return (FALSE);
}

int NoSeparator2 (chr)
char chr;
{
    if ((chr != 0) && (chr != '\"'))
	return (TRUE);
    else
	return (FALSE);
}

Tokenize (cmd, cmdtokens, maxtokens)
char *cmd, *cmdtokens[];
int maxtokens;
{
    register int len, i, j;
    int (*NoSeparator)();

    len = strlen (cmd);
    maxtokens--;

    for (j=i=0; (i<len) && (j <maxtokens); ) {
	NoSeparator = NoSeparator1;
	while ((cmd[i] == ' ') || (cmd[i] == ',') || (cmd[i] == '\t'))
	    i++;		    /* Find next non-separator */
	cmdtokens[j++] = &cmd[i];
	while ((*NoSeparator)(cmd[i])) {
	    if (cmd[i] == '\"')
		NoSeparator = NoSeparator2;
	    i++;		    /* Find next separator */
	}
	cmd[i++] = 0;
    }
    cmdtokens[j] = NULL;

    for (i=0; cmdtokens[i]; i++)
	RemoveDoubleQuote (cmdtokens[i]);
}

#define MAXTOKENS 300

int PCS_Execute (cmd, output, waitopt)
char *cmd;
char *output;
int waitopt;
{
    int pid, code, status;
    char *cmdtokens[MAXTOKENS];
    int stdout_fildes[2];
    int stderr_fildes[2];
    int remaining_output, rc;
    char *orig_output;

    output[0] = 0;
    code = 0;
    orig_output = output;
    SetEnvironment ();

    Tokenize (cmd, cmdtokens, MAXTOKENS);

    if (waitopt) {
	if (pipe (stdout_fildes) < 0)
	    return (PCS_EXECFAIL);
	if (pipe (stderr_fildes) < 0)
	    return (PCS_EXECFAIL);
    }

    if ((pid = fork()) == 0) {	    /* child */
	int fd;

	if (waitopt) {		    /* Pipe stdout & stderr to parent */
	    close (stdout_fildes[0]);
	    dup2 (stdout_fildes[1], 1);
	    close (stdout_fildes[1]);
	    close (stderr_fildes[0]);
	    dup2 (stderr_fildes[1], 2);
	    close (stderr_fildes[1]);
	}
	else {			    /* Throw away stdout */
	    fd = open ("/dev/null", O_WRONLY, 0644);
	    if (fd >= 0) dup2 (fd, 1);
	    close (fd);
	}

	execvp (cmdtokens[0], &cmdtokens[0]);

	/* should never get here unless execvp fails */
	if (waitopt)
	    printf ("pcserver: execvp failed - command: \"%s\" errno %d",
		    cmd, errno);
	else
	    fprintf (stderr,
		     "pcserver: execvp failed - command: \"%s\" errno %d",
		     cmd, errno);
	Terminate (-3);
    }

    /* Parent */
    if (!waitopt)
	return (PCS_SUCCESS);

    remaining_output = OUTPUT_LIMIT;
    close (stdout_fildes[1]);
    close (stderr_fildes[1]);

    /*
      code = 0;
      do {
	  rc = read (stdout_fildes[0], output, remaining_output);
	  DBGMSG (2, ("RC from pipe read is %d", rc));
	  if (rc == -1)
	      break;
	  remaining_output -= rc;
	  output += rc;
	  code = wait3 (&status, WNOHANG, NULL);
	  DBGMSG (2,("Wait code = %d", code));
      } while ((code == 0) && (rc != 0) && (remaining_output > 0));
      */
    rc = read (stdout_fildes[0], output, remaining_output);
    if (rc > 0) {
	output += rc;
	remaining_output -= rc;
    }
    *output = 0;
    rc = read (stderr_fildes[0], output, remaining_output);
    if (rc > 0)
	output += rc;
    *output = 0;

    DBGMSG (2,("Finished read from pipes"));
    DBGMSG (2,("Data: %s", orig_output));
    close (stdout_fildes[0]);
    close (stderr_fildes[0]);

    errno = 0;
    while ((code != pid) && (code != -1))
	code = wait (&status);

    DBGMSG (2,("Original PID = 0x%X", pid));
    DBGMSG (2,("Wait ProcessId = 0x%X, Status = 0x%X", code, status));

    if (code == pid && ((status>>8) & 0xFF) == 0)
	return (PCS_SUCCESS);
    if (TextOutput (orig_output))
	return (PCS_SUCCESS);
    return (PCS_EXECFAIL);
}

CleanUpOrphans ()
{
#if defined(POSIX_ENV)
    int status;
    waitpid (&status, WNOHANG | WUNTRACED, 0);
#else
    union wait status;
    wait3 (&status, WNOHANG | WUNTRACED, 0);
#endif
}

TextOutput (str)
char *str;
{
    register int i;

    for (i=0; str[i]; i++) {
	if ((str[i] != ' ') && (isascii(str[i])) && (isprint (str[i])))
	    return (TRUE);
    }
    return (FALSE);
}


