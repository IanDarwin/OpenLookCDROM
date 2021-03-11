#ifndef lint
static char rcsid[] = "spawn.c,v 2.0 1994/05/19 02:01:23 dan Exp";
#endif

/*
 * Copyright (c) 1994    Daniel Williams
 * 
 * The X Consortium, and any party obtaining a copy of these files from
 * the X Consortium, directly or indirectly, is granted, free of charge,
 * a full and unrestricted irrevocable, world-wide, paid up,
 * royalty-free, nonexclusive right and license to deal in this software
 * and documentation files (the "Software"), including without limitation
 * the rights to use, copy, modify, merge, publish, distribute,
 * sublicense, and/or sell copies of the Software, and to permit persons
 * who receive copies from any such party to do so.  This license
 * includes without limitation a license to do the foregoing actions
 * under any patents of the party supplying this software to the X
 * Consortium.  The following conditions apply:
 * 
 * The above copyright notice and this permission notice shall be
 * included in all copies or substantial portions of the Software.
 * 
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
 * EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
 * MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
 * IN NO EVENT SHALL DANIEL WILLIAMS OR SYSTEMS & SCIENTIFIC SOFTWARE BE
 * LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
 * OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
 * WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

#include <X11/Xos.h>

#include <stdio.h>
#include <stdlib.h>
#include <errno.h>

#define BLOCKSIZE 10

/* 
 * run a program with command line arguments and two pathname 
 * arguments via fork/exec and return a pipe file descriptor into 
 * which standard output and standard error have been redirected
 */
FILE *spawn_diff (char *prog, char *args, char *path1, char *path2)
{
    int pipe_fds[2];
    char buffer[1024];
    extern char *progname;
    char **argv;
    char *ptr;
    int argc, count;

    if (pipe (pipe_fds) == -1) {
	(void) fprintf (stderr, "%s: system call ", progname);
	perror ("pipe");
	exit (2);
    }

    switch (fork ()) {
    case 0:			/* the child */
	/* 
	 * redirect standard output and standard error into the pipe
	 */
	(void) close (fileno (stdout));
	if (dup (pipe_fds[1]) == -1) {
	    (void) fprintf (stderr, "%s: system call ", progname);
	    perror ("dup");
	    exit (2);
	}
	(void) close (fileno (stderr));
	if (dup (pipe_fds[1]) == -1) {
	    (void) fprintf (stderr, "%s: system call ", progname);
	    perror ("dup");
	    exit (2);
	}

	(void) close (pipe_fds[0]);

	/* 
	 * split up args passed in into an argument vector for the execvp
	 * system call.  this works for an unlimited number of arguments,
	 * but fails to do any quote processing.  arguments with embedded
	 * spaces will break this.
	 */

	argc = 0;
	count = BLOCKSIZE;
	argv = (char **) malloc (sizeof (char *) * BLOCKSIZE);
	argv[argc++] = prog;

	for (ptr = strtok (args, " \t"); ptr; ptr = strtok (NULL, " \t")) {
	    if (argc >= count) {
		argv = (char **) realloc (argv, sizeof (char *) * BLOCKSIZE);
		count += BLOCKSIZE;
	    }
	    argv[argc++] = strdup (ptr);
	}

	if ((argc + 3) >= count)
	    argv = (char **) realloc (argv, sizeof (char *) * 3);

	argv[argc++] = path1;
	argv[argc++] = path2;
	argv[argc++] = NULL;

	if (execvp (prog, argv) == -1) {
	    (void) sprintf (buffer, "%s: %s: %s", progname, "exec", prog);
	    perror (buffer);
	    exit (2);
	}

	break;
    case -1:			/* fork error */
	(void) fprintf (stderr, "%s: system call ", progname);
	perror ("fork");
	exit (2);
	break;
    default:			/* the parent */
	/* 
	 * we must close this in the parent or else the close of the 
	 * writer end of the pipe in the child will not cause an EOF 
	 * condition for the reader
	 */
	(void) close (pipe_fds[1]);
	/* 
	 * return the reader side of the pipe as a stdio stream
	 */
	return (fdopen (pipe_fds[0], "r"));
	/* NOTREACHED */
	break;
    }
    /* NOTREACHED */
}
