/*
 * Copyright 1993 Network Computing Devices, Inc.
 *
 * Permission to use, copy, modify, distribute, and sell this software and
 * its documentation for any purpose is hereby granted without fee, provided
 * that the above copyright notice appear in all copies and that both that
 * copyright notice and this permission notice appear in supporting
 * documentation, and that the name Network Computing Devices, Inc. not be
 * used in advertising or publicity pertaining to distribution of this 
 * software without specific, written prior permission.
 * 
 * THIS SOFTWARE IS PROVIDED `AS-IS'.  NETWORK COMPUTING DEVICES, INC.,
 * DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING WITHOUT
 * LIMITATION ALL IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS FOR A
 * PARTICULAR PURPOSE, OR NONINFRINGEMENT.  IN NO EVENT SHALL NETWORK
 * COMPUTING DEVICES, INC., BE LIABLE FOR ANY DAMAGES WHATSOEVER, INCLUDING
 * SPECIAL, INCIDENTAL OR CONSEQUENTIAL DAMAGES, INCLUDING LOSS OF USE, DATA,
 * OR PROFITS, EVEN IF ADVISED OF THE POSSIBILITY THEREOF, AND REGARDLESS OF
 * WHETHER IN AN ACTION IN CONTRACT, TORT OR NEGLIGENCE, ARISING OUT OF OR IN
 * CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 * 
 * $NCDId: @(#)auctl.c,v 1.4 1994/04/07 18:10:01 greg Exp $
 */

#include <malloc.h>
#include "auctl.h"

char *ProgramName;

static int do_command_loop PROTO((AuServer *, AuBool));

int
main (argc, argv)
    int argc;
    char **argv;
{
    int i;
    char *audioname = NULL;
    AuServer *aud;
    char *msg = NULL;
    AuBool doprompt = (isatty(fileno(stdin)) && isatty(fileno(stdout)));
    char **cmd = NULL;
    int nwords = 0;
    int errors = 0;
    static char *help_cmd[] = { "help", (char *) 0 };
    static int n_help_cmd = 1;

    ProgramName = argv[0];

    for (i = 1; i < argc; i++) {
	char *arg = argv[i];

	if (arg[0] == '-') {
	    switch (arg[1]) {
	      case 'a':
		if (++i >= argc) goto usage;
		audioname = argv[i];
		continue;
	      case 'q':
		doprompt = AuFalse;
		continue;
	    }
	  usage:
	    fprintf (stderr, "usage:  %s [-audio server] [-q] [command]\n\n",
		     ProgramName);
	    cmd = help_cmd;
	    nwords = n_help_cmd;
	    break;
	}
	cmd = argv + i;
	nwords = argc - i;
	break;
    }

    aud = AuOpenServer (audioname, 0, NULL, 0, NULL, &msg);
    if (!aud) {
	fprintf (stderr, "%s: unable to open audio server: %s\n",
		 ProgramName, (msg ? msg : ""));
	exit (1);
    }

    if (cmd)
	errors += execute_command (aud, nwords, cmd, (AuBool *)NULL);
    else
	errors += do_command_loop (aud, doprompt);

    AuCloseServer (aud);
    exit (errors);
}


static char *skip_space (s)
    register char *s;
{
    if (!s) return NULL;

    for ( ; *s && isascii(*s) && isspace(*s); s++)
        ;
    return s;
}


static char *skip_nonspace (s)
    register char *s;
{
    if (!s) return NULL;

    /* put quoting into loop if need be */
    for ( ; *s && isascii(*s) && !isspace(*s); s++)
        ;
    return s;
}


static char **split_into_words (src, argcp)  /* argvify string */
    char *src;
    int *argcp;
{
    char *jword;
    char savec;
    char **argv;
    int cur, total;

    *argcp = 0;
#define WORDSTOALLOC 4			/* most lines are short */
    argv = (char **) malloc (WORDSTOALLOC * sizeof (char *));
    if (!argv) return NULL;
    cur = 0;
    total = WORDSTOALLOC;

    /*
     * split the line up into separate, nul-terminated tokens; the last
     * "token" will point to the empty string so that it can be bashed into
     * a null pointer.
     */

    do {
	jword = skip_space (src);
	src = skip_nonspace (jword);
	savec = *src;
	*src = '\0';
	if (cur == total) {
	    total += WORDSTOALLOC;
	    argv = (char **) realloc (argv, total * sizeof (char *));
	    if (!argv) return NULL;
	}
	argv[cur++] = jword;
	if (savec) src++;		/* if not last on line advance */
    } while (jword != src);

    argv[--cur] = NULL;			/* smash empty token to end list */
    *argcp = cur;
    return argv;
}


static int do_command_loop (aud, doprompt)
    AuServer *aud;
    AuBool doprompt;
{
    char buf[256];
    int len;
    char *line;
    int argc = 0;
    char **argv = NULL;
    int errors = 0;
    AuBool done = AuFalse;

    while (!done) {
	if (doprompt) {
	    fprintf (stdout, "auctl> ");
	    fflush (stdout);
	}
	buf[0] = '\0';
	if (fgets (buf, sizeof buf, stdin) == NULL)
	    break;
	buf[(sizeof buf) - 1] = '\0';
	/* EMPTY */
	for (line = buf; *line && isspace(*line); line++) /* EMPTY */;

	len = strlen (line);
	if (len == 0 || *line == '#')
	    continue;
	
	if (buf[len-1] != '\n') {
	    fprintf (stderr, "%s: line too long\n", ProgramName);
	    continue;
	}

	argv = split_into_words (buf, &argc);
	if (argv) {
	    errors += execute_command (aud, argc, argv, &done);
	    free ((char *) argv);
	} else {
	    fprintf (stderr, "%s: unable to split line into words\n",
		     ProgramName);
	    errors++;
	}
    }
    return errors;
}

