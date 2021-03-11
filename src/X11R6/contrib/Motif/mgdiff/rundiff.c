#ifndef lint
static char rcsid[] = "rundiff.c,v 2.0 1994/05/19 02:01:22 dan Exp";
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
#include <ctype.h>
#include <sys/wait.h>
#include <limits.h>
#include <assert.h>

#include <X11/Intrinsic.h>
#include <Xm/Xm.h>

#include "mgdiff.h"
#include "externs.h"

/* 
 * this is the maximum number of lines shown to the user if diff
 * returns an error
 */
#define MAX_ERROR_LINES 25

/* 
 * pick this parameter to strike a balance between keeping the display 
 * refreshed and processing the diff command rapidly.  The program will
 * process at least this many lines from the input files before checking
 * to see if any X Events need to be handled.
 */
#define UPDATE_GRANULARITY 500

typedef enum { ADD = 1, CHANGE, DELETE, IGNORE, ERROR} DiffType;

static char *duplicate (char *s, int *flag);
static DiffType parse_diff_line (char *buf, int *f1n1, int *f1n2, int *f2n1, int *f2n2);
static int eatline (FILE *f);
static void getline (FILE *f, char **cooked, char **raw);
static void reset_blist (void);
static Block *get_blist (void);
static void add_blist (Block *b);

/* 
 * release all storage used by the DiffInfo structure and its 
 * constituent data structures
 */
void free_diff_info (DiffInfo *di)
{
    Block *b, *bold;
    int i;

    /* 
     * free the error text, if any
     */
    if (di->etext != NULL) {
	char **p;

	for (p = di->etext; *p != NULL; p++)
	    free ((void *) *p);
	free ((void *) di->etext);
    }

    /* 
     * free all text information
     */
    for (b = di->first; b != NULL; /* increment done in the loop */) {
	/* 
	 * if the blocks are identical the text is stored only on the 
	 * left hand data structures
	 */
	if ((b->arr[LEFT].type == SAME) && (b->arr[RIGHT].type == SAME)) {
	    for (i = 0; i < b->arr[LEFT].fsize; i++) {
		free ((void *) b->arr[LEFT].text[i]);
		if ((b->arr[LEFT].wtext != NULL) && (b->arr[LEFT].wtext[i] != NULL))
		    free ((void *) b->arr[LEFT].wtext[i]);
	    }
	    free ((void *) b->arr[LEFT].text);
	    if (b->arr[LEFT].wtext != NULL)
		free ((void *) b->arr[LEFT].wtext);
	    free ((void *) b->arr[LEFT].tlen);
	}
	else if ((b->arr[LEFT].type == DIFF) && (b->arr[RIGHT].type == DIFF)) {
	    for (i = 0; i < b->arr[LEFT].fsize; i++) {
		free ((void *) b->arr[LEFT].text[i]);
		if ((b->arr[LEFT].wtext != NULL) && (b->arr[LEFT].wtext[i] != NULL))
		    free ((void *) b->arr[LEFT].wtext[i]);
	    }
	    for (i = 0; i < b->arr[RIGHT].fsize; i++) {
		free ((void *) b->arr[RIGHT].text[i]);
		if ((b->arr[RIGHT].wtext != NULL) && (b->arr[RIGHT].wtext[i] != NULL))
		    free ((void *) b->arr[RIGHT].wtext[i]);
	    }
	    free ((void *) b->arr[LEFT].text);
	    if (b->arr[LEFT].wtext != NULL)
		free ((void *) b->arr[LEFT].wtext);
	    free ((void *) b->arr[LEFT].tlen);
	    free ((void *) b->arr[RIGHT].text);
	    if (b->arr[RIGHT].wtext != NULL)
		free ((void *) b->arr[RIGHT].wtext);
	    free ((void *) b->arr[RIGHT].tlen);
	}
	else if ((b->arr[LEFT].type == INSERT) && (b->arr[RIGHT].type == BLANK)) {
	    for (i = 0; i < b->arr[LEFT].fsize; i++) {
		free ((void *) b->arr[LEFT].text[i]);
		if ((b->arr[LEFT].wtext != NULL) && (b->arr[LEFT].wtext[i] != NULL))
		    free ((void *) b->arr[LEFT].wtext[i]);
	    }
	    free ((void *) b->arr[LEFT].text);
	    if (b->arr[LEFT].wtext != NULL)
		free ((void *) b->arr[LEFT].wtext);
	    free ((void *) b->arr[LEFT].tlen);
	}
	else if ((b->arr[LEFT].type == BLANK) && (b->arr[RIGHT].type == INSERT)) {
	    for (i = 0; i < b->arr[RIGHT].fsize; i++) {
		free ((void *) b->arr[RIGHT].text[i]);
		if ((b->arr[RIGHT].wtext != NULL) && (b->arr[RIGHT].wtext[i] != NULL))
		    free ((void *) b->arr[RIGHT].wtext[i]);
	    }
	    free ((void *) b->arr[RIGHT].text);
	    if (b->arr[RIGHT].wtext != NULL)
		free ((void *) b->arr[RIGHT].wtext);
	    free ((void *) b->arr[RIGHT].tlen);
	}
	else			/* CONSTCOND */
	    assert (False);

	bold = b;
	b = b->next;
	free ((void *) bold);
    }

    free ((void *) di);
}

/* 
 * build just enough of a DiffInfo structure so that routines that use 
 * the data won't be shocked.  this is used to show a blank display 
 * (no files loaded).
 */
DiffInfo *blank_diff_info (void)
{
    Block *b;
    DiffInfo *di;

    di = (DiffInfo *) calloc (1, sizeof (DiffInfo));
    di->longline = "  ";
    di->maxcols = strlen (di->longline);
    di->status = 2;

    b = (Block *) calloc (1, sizeof (Block));
    b->selected = NEITHER;
    b->arr[LEFT].type = b->arr[RIGHT].type = SAME;
    b->sline = 0;
    b->ssize = 1;
    b->arr[LEFT].fline = b->arr[RIGHT].fline = 0;
    b->arr[LEFT].fsize = b->arr[RIGHT].fsize = 1;
    b->arr[LEFT].text = (char **) calloc (1, sizeof (char *));
    b->arr[LEFT].wtext = (char **) calloc (1, sizeof (char *));
    b->arr[LEFT].tlen = (short *) calloc (1, sizeof (short));
    b->arr[LEFT].text[0] = strdup ("  ");
    b->arr[LEFT].tlen[0] = strlen (b->arr[LEFT].text[0]);

    b->arr[RIGHT].text = NULL;
    b->arr[RIGHT].wtext = NULL;
    b->arr[RIGHT].tlen = NULL;
    di->first = di->last = b;
    di->lines = b->sline + b->ssize;
    di->flines[LEFT] = b->arr[LEFT].fline + b->arr[LEFT].fsize;
    di->flines[RIGHT] = b->arr[RIGHT].fline + b->arr[RIGHT].fsize;

    return (di);
}

/* 
 * run the diff command on two files with optional arguments and
 * return a data structure that captures the differences between 
 * them in a way that can be displayed.
 */
DiffInfo *build_diff_info (char *prog, char *args, char *path1, char *path2)
{
    FILE *diff, *file1, *file2;
    char buffer[BUFSIZ+1];
    int sline, fline1, fline2;
    Block *b;
    int i, lines, counter;
    int stat_loc;
    DiffInfo *di;
    extern Widget toplevel;
    extern char *progname;

    file1 = fopen (path1, "r");
    file2 = fopen (path2, "r");
    diff = spawn_diff (prog, args, path1, path2);

    di = (DiffInfo *) calloc (1, sizeof (DiffInfo));
    di->longline = "";

    XmUpdateDisplay (toplevel);
    counter = 0;

    reset_blist ();
    sline = 0;
    fline1 = fline2 = 1;
    while (fgets (buffer, BUFSIZ, diff) != NULL) {
	int f1n1, f1n2, f2n1, f2n2;

	if ((sline - counter) > UPDATE_GRANULARITY) {
	    counter = sline;
	    XmUpdateDisplay (toplevel);
	}

	switch (parse_diff_line (buffer, &f1n1, &f1n2, &f2n1, &f2n2)) {
	case IGNORE:
	    break;
	case ERROR:
	    if (di->errors == 0) {
		char cmdline[4096];

		di->etext = (char **) calloc (MAX_ERROR_LINES + 1, sizeof (char *));
		(void) sprintf (cmdline, "    \"%s %s %s %s\"", prog, args, path1, path2);
		di->etext[di->errors++] = strdup ("diff command line:");
		di->etext[di->errors++] = strdup ("");
		di->etext[di->errors++] = strdup (cmdline);
		di->etext[di->errors++] = strdup ("");
		di->etext[di->errors++] = strdup ("produced this output:");
		di->etext[di->errors++] = strdup ("");
	    }
	    if (di->errors < MAX_ERROR_LINES)
		di->etext[di->errors++] = strdup (buffer);
	    break;
	case ADD:
	    if (f2n1 != fline2) {
		b = (Block *) calloc (1, sizeof (Block));
		b->selected = NEITHER;
		b->arr[LEFT].type = b->arr[RIGHT].type = SAME;
		b->sline = sline;
		b->arr[LEFT].fline = fline1 - 1;
		b->arr[RIGHT].fline = fline2 - 1;
		b->arr[LEFT].fsize = b->arr[RIGHT].fsize = f2n1 - fline2;
		b->ssize = f2n1 - fline2;
		
		b->arr[LEFT].text = (char **) calloc (b->arr[LEFT].fsize, sizeof (char *));
		b->arr[LEFT].wtext = (char **) calloc (b->arr[LEFT].fsize, sizeof (char *));
		b->arr[LEFT].tlen = (short *) calloc (b->arr[LEFT].fsize, sizeof (short));
		for (i = 0; i < b->arr[LEFT].fsize; i++) {
		    getline (file1, &b->arr[LEFT].text[i], &b->arr[LEFT].wtext[i]);
		    b->arr[LEFT].tlen[i] = strlen (b->arr[LEFT].text[i]);
		    if (di->maxcols < b->arr[LEFT].tlen[i]) {
			di->maxcols = b->arr[LEFT].tlen[i];
			di->longline = b->arr[LEFT].text[i];
		    }
		    (void) eatline (file2);
		}
		fline1 += b->arr[LEFT].fsize;
		fline2 += b->arr[RIGHT].fsize;
		sline += b->ssize;
		add_blist (b);
	    }

	    b = (Block *) calloc (1, sizeof (Block));
	    b->selected = NEITHER;
	    b->arr[LEFT].type = BLANK;
	    b->arr[RIGHT].type = INSERT;
	    b->sline = sline;
	    b->arr[LEFT].fline = fline1 - 1;
	    b->arr[RIGHT].fline = fline2 - 1;
	    b->arr[LEFT].fsize = 0;
	    b->arr[RIGHT].fsize = f2n2 - f2n1 + 1;
	    b->ssize = max (b->arr[LEFT].fsize, b->arr[RIGHT].fsize);

	    b->arr[RIGHT].text = (char **) calloc (b->arr[RIGHT].fsize, sizeof (char *));
	    b->arr[RIGHT].wtext = (char **) calloc (b->arr[RIGHT].fsize, sizeof (char *));
	    b->arr[RIGHT].tlen = (short *) calloc (b->arr[RIGHT].fsize, sizeof (short));
	    for (i = 0; i < b->arr[RIGHT].fsize; i++) {
		getline (file2, &b->arr[RIGHT].text[i], &b->arr[RIGHT].wtext[i]);
		b->arr[RIGHT].tlen[i] = strlen (b->arr[RIGHT].text[i]);
		if (di->maxcols < b->arr[RIGHT].tlen[i]) {
		    di->maxcols = b->arr[RIGHT].tlen[i];
		    di->longline = b->arr[RIGHT].text[i];
		}
	    }
	    fline2 += b->arr[RIGHT].fsize;

	    b->arr[LEFT].text = NULL;
	    b->arr[LEFT].tlen = NULL;

	    sline += b->ssize;

	    add_blist (b);
	    break;
	case CHANGE:
	    if (f1n1 != fline1) {
		b = (Block *) calloc (1, sizeof (Block));
		b->selected = NEITHER;
		b->arr[LEFT].type = b->arr[RIGHT].type = SAME;
		b->sline = sline;
		b->arr[LEFT].fline = fline1 - 1;
		b->arr[RIGHT].fline = fline2 - 1;
		b->arr[LEFT].fsize = b->arr[RIGHT].fsize = f1n1 - fline1;
		b->ssize = f1n1 - fline1;
		
		b->arr[LEFT].text = (char **) calloc (b->arr[LEFT].fsize, sizeof (char *));
		b->arr[LEFT].wtext = (char **) calloc (b->arr[LEFT].fsize, sizeof (char *));
		b->arr[LEFT].tlen = (short *) calloc (b->arr[LEFT].fsize, sizeof (short));
		for (i = 0; i < b->arr[LEFT].fsize; i++) {
		    getline (file1, &b->arr[LEFT].text[i], &b->arr[LEFT].wtext[i]);
		    b->arr[LEFT].tlen[i] = strlen (b->arr[LEFT].text[i]);
		    if (di->maxcols < b->arr[LEFT].tlen[i]) {
			di->maxcols = b->arr[LEFT].tlen[i];
			di->longline = b->arr[LEFT].text[i];
		    }
		    (void) eatline (file2);
		}
		fline1 += b->arr[LEFT].fsize;
		fline2 += b->arr[RIGHT].fsize;
		sline += b->ssize;
		add_blist (b);
	    }
	    b = (Block *) calloc (1, sizeof (Block));
	    b->selected = NEITHER;
	    b->arr[LEFT].type = b->arr[RIGHT].type = DIFF;
	    b->sline = sline;
	    b->arr[LEFT].fline = fline1 - 1;
	    b->arr[RIGHT].fline = fline2 - 1;
	    b->arr[LEFT].fsize = f1n2 - f1n1 + 1;
	    b->arr[RIGHT].fsize = f2n2 - f2n1 + 1;
	    b->ssize = max (b->arr[LEFT].fsize, b->arr[RIGHT].fsize);

	    b->arr[LEFT].text = (char **) calloc (b->arr[LEFT].fsize, sizeof (char *));
	    b->arr[LEFT].wtext = (char **) calloc (b->arr[LEFT].fsize, sizeof (char *));
	    b->arr[LEFT].tlen = (short *) calloc (b->arr[LEFT].fsize, sizeof (short));
	    for (i = 0; i < b->arr[LEFT].fsize; i++) {
		getline (file1, &b->arr[LEFT].text[i], &b->arr[LEFT].wtext[i]);
		b->arr[LEFT].tlen[i] = strlen (b->arr[LEFT].text[i]);
		if (di->maxcols < b->arr[LEFT].tlen[i]) {
		    di->maxcols = b->arr[LEFT].tlen[i];
		    di->longline = b->arr[LEFT].text[i];
		}
	    }
	    fline1 += b->arr[LEFT].fsize;

	    b->arr[RIGHT].text = (char **) calloc (b->arr[RIGHT].fsize, sizeof (char *));
	    b->arr[RIGHT].wtext = (char **) calloc (b->arr[RIGHT].fsize, sizeof (char *));
	    b->arr[RIGHT].tlen = (short *) calloc (b->arr[RIGHT].fsize, sizeof (short));
	    for (i = 0; i < b->arr[RIGHT].fsize; i++) {
		getline (file2, &b->arr[RIGHT].text[i], &b->arr[RIGHT].wtext[i]);
		b->arr[RIGHT].tlen[i] = strlen (b->arr[RIGHT].text[i]);
		if (di->maxcols < b->arr[RIGHT].tlen[i]) {
		    di->maxcols = b->arr[RIGHT].tlen[i];
		    di->longline = b->arr[RIGHT].text[i];
		}
	    }
	    fline2 += b->arr[RIGHT].fsize;

	    sline += b->ssize;

	    add_blist (b);
	    break;
	case DELETE:
	    if (f1n1 != fline1) {
		b = (Block *) calloc (1, sizeof (Block));
		b->selected = NEITHER;
		b->arr[LEFT].type = b->arr[RIGHT].type = SAME;
		b->sline = sline;
		b->arr[LEFT].fline = fline1 - 1;
		b->arr[RIGHT].fline = fline2 - 1;
		b->arr[LEFT].fsize = b->arr[RIGHT].fsize = f1n1 - fline1;
		b->ssize = f1n1 - fline1;
		
		b->arr[LEFT].text = (char **) calloc (b->arr[LEFT].fsize, sizeof (char *));
		b->arr[LEFT].wtext = (char **) calloc (b->arr[LEFT].fsize, sizeof (char *));
		b->arr[LEFT].tlen = (short *) calloc (b->arr[LEFT].fsize, sizeof (short));
		for (i = 0; i < b->arr[LEFT].fsize; i++) {
		    getline (file1, &b->arr[LEFT].text[i], &b->arr[LEFT].wtext[i]);
		    b->arr[LEFT].tlen[i] = strlen (b->arr[LEFT].text[i]);
		    if (di->maxcols < b->arr[LEFT].tlen[i]) {
			di->maxcols = b->arr[LEFT].tlen[i];
			di->longline = b->arr[LEFT].text[i];
		    }
		    (void) eatline (file2);
		}
		fline1 += b->arr[LEFT].fsize;
		fline2 += b->arr[RIGHT].fsize;
		sline += b->ssize;
		add_blist (b);
	    }

	    b = (Block *) calloc (1, sizeof (Block));
	    b->selected = NEITHER;
	    b->arr[LEFT].type = INSERT;
	    b->arr[RIGHT].type = BLANK;
	    b->sline = sline;
	    b->arr[LEFT].fline = fline1 - 1;
	    b->arr[RIGHT].fline = fline2 - 1;
	    b->arr[LEFT].fsize = f1n2 - f1n1 + 1;
	    b->arr[RIGHT].fsize = 0;
	    b->ssize = max (b->arr[LEFT].fsize, b->arr[RIGHT].fsize);

	    b->arr[LEFT].text = (char **) calloc (b->arr[LEFT].fsize, sizeof (char *));
	    b->arr[LEFT].wtext = (char **) calloc (b->arr[LEFT].fsize, sizeof (char *));
	    b->arr[LEFT].tlen = (short *) calloc (b->arr[LEFT].fsize, sizeof (short));
	    for (i = 0; i < b->arr[LEFT].fsize; i++) {
		getline (file1, &b->arr[LEFT].text[i], &b->arr[LEFT].wtext[i]);
		b->arr[LEFT].tlen[i] = strlen (b->arr[LEFT].text[i]);
		if (di->maxcols < b->arr[LEFT].tlen[i]) {
		    di->maxcols = b->arr[LEFT].tlen[i];
		    di->longline = b->arr[LEFT].text[i];
		}
	    }
	    fline1 += b->arr[LEFT].fsize;

	    b->arr[RIGHT].text = NULL;
	    b->arr[RIGHT].tlen = NULL;

	    sline += b->ssize;

	    add_blist (b);
	    break;
	default:		/* CONSTCOND */
	    assert (False);
	    break;
	}
    }

    /* 
     * if we've read no lines and there are diff errors then blow off
     */
    if ((fline1 == 1) && (fline2 == 1) && (di->errors > 0)) {
	DiffInfo *newdi = blank_diff_info ();

	if (wait (&stat_loc) == -1) {
	    (void) fprintf (stderr, "%s: system call ", progname);
	    perror ("wait");
	    exit (2);
	}
	
	newdi->errors = di->errors;
	newdi->etext = di->etext;
	newdi->status = (WIFEXITED (stat_loc)) ? (WEXITSTATUS (stat_loc)) : 2;
	return (newdi);
    }

    /* 
     * we've parsed all the diff commands but the possibility exists 
     * that the rest of the two files are identical
     */

    /* get a line count from the first file */
    for (lines = 0; eatline (file1); lines++)
	;

    if (lines > 0) {
	b = (Block *) calloc (1, sizeof (Block));
	b->selected = NEITHER;
	b->arr[LEFT].type = b->arr[RIGHT].type = SAME;
	b->sline = sline;
	b->arr[LEFT].fline = fline1 - 1;
	b->arr[RIGHT].fline = fline2 - 1;
	b->arr[LEFT].fsize = b->arr[RIGHT].fsize = lines;
	b->ssize = lines;
		
	b->arr[LEFT].text = (char **) calloc (b->arr[LEFT].fsize, sizeof (char *));
	b->arr[LEFT].wtext = (char **) calloc (b->arr[LEFT].fsize, sizeof (char *));
	b->arr[LEFT].tlen = (short *) calloc (b->arr[LEFT].fsize, sizeof (short));
 	for (i = 0; i < b->arr[LEFT].fsize; i++) {
	    getline (file2, &b->arr[LEFT].text[i], &b->arr[LEFT].wtext[i]);
	    b->arr[LEFT].tlen[i] = strlen (b->arr[LEFT].text[i]);
	    if (di->maxcols < b->arr[LEFT].tlen[i]) {
		di->maxcols = b->arr[LEFT].tlen[i];
		di->longline = b->arr[LEFT].text[i];
	    }
	}
	add_blist (b);
    }

    di->first = get_blist ();
    di->last = b;
    di->lines = b->sline + b->ssize;
    di->flines[LEFT] = b->arr[LEFT].fline + b->arr[LEFT].fsize;
    di->flines[RIGHT] = b->arr[RIGHT].fline + b->arr[RIGHT].fsize;
    if (wait (&stat_loc) == -1) {
	(void) fprintf (stderr, "%s: system call ", progname);
	perror ("wait");
	exit (2);
    }

    di->status = (WIFEXITED (stat_loc)) ? (WEXITSTATUS (stat_loc)) : 2;
    return (di);
}

/* 
 * make a copy of a string, converting tabs to spaces and control characters
 * to printable form if needed.
 */
static char *duplicate (char *s, int *flag)
{
    int len, i, tabs, ctrls;

    /* 
     * compute length of new string, taking tabs and control 
     * characters into account
     */
    for (i = 0, len = 0, ctrls = tabs = 0; s[i] != '\0'; i++) {
	if (isascii (s[i])) {
	    if (s[i] == '\t') {
		tabs++;
		len += 8;
		len /= 8;
		len *= 8;
	    }
	    else if (iscntrl (s[i])) {
		ctrls++;
		len += 2;
	    }
	    else
		len++;
	}
	else {
	    ctrls++;
	    len += 4;
	}
    }

    if (tabs || ctrls) {
	char *ret = (char *) calloc (1, len + 1);
	int j;

	for (i = 0, j = 0; s[i] != '\0'; i++) {
	    if (isascii (s[i])) {
		if (s[i] == '\t') {
		    ret[j++] = ' ';
		    while ((j % 8) != 0)
			ret[j++] = ' ';
		}
		else if (iscntrl (s[i])) {
		    ret[j++] = '^';
		    ret[j++] = (s[i] + '@') & 0x7f;
		}
		else
		    ret[j++] = s[i];
	    }
	    else {
		unsigned char c = s[i];

		/* 
		 * create octal escape
		 */
		ret[j++] = '\\';
		ret[j+2] = (c % 8) + '0'; c /= 8;
		ret[j+1] = (c % 8) + '0'; c /= 8;
		ret[j+0] = (c % 8) + '0'; 
		j += 3;
	    }
	}
	*flag = True;
	return (ret);
    }
    else {
	*flag = False;
	return (strdup (s));
    }
}

/* 
 * this code taken from "ediff.c" by David MacKenzie, a published,
 * uncopyrighted program to translate diff output into plain English 
 */
static DiffType parse_diff_line (char *buf, int *f1n1, int *f1n2, int *f2n1, int *f2n2)
{
    if ((buf[0] == '<') || (buf[0] == '>') || (buf[0] == '-')) {
	return (IGNORE);
    }
    else if (sscanf (buf, "%d,%dc%d,%d\n", f1n1, f1n2, f2n1, f2n2) == 4) {
	return (CHANGE);
    }
    else if (sscanf (buf, "%d,%dc%d\n", f1n1, f1n2, f2n1) == 3) {
	*f2n2 = *f2n1;
	return (CHANGE);
    }
    else if (sscanf (buf, "%dc%d,%d\n", f1n1, f2n1, f2n2) == 3) {
	*f1n2 = *f1n1;
        return (CHANGE);
    }
    else if (sscanf (buf, "%dc%d\n", f1n1, f2n1) == 2) {
        *f2n2 = *f2n1;
        *f1n2 = *f1n1;
        return (CHANGE);
    }
    else if (sscanf (buf, "%d,%dd%d\n", f1n1, f1n2, f2n1) == 3) {
	*f2n2 = *f2n1;
	return (DELETE);
    }
    else if (sscanf (buf, "%dd%d\n", f1n1, f2n1) == 2) {
	*f2n2 = *f2n1;
	*f1n2 = *f1n1;
	return (DELETE);
    }
    else if (sscanf (buf, "%da%d,%d\n", f1n1, f2n1, f2n2) == 3) {
	*f1n2 = *f1n1;
	return (ADD);
    }
    else if (sscanf (buf, "%da%d\n", f1n1, f2n1) == 2) {
	*f1n2 = *f1n1;
	*f2n2 = *f2n1;
	return (ADD);
    }
    else
	return (ERROR);
}

/* 
 * read a line and throw it away
 */
static int eatline (FILE *f)
{
    for (;;) {
	int c;

	c = getc (f);
	if (c == '\n')
	    return (1);
	/* 
	 * EOF (aka 0xff, 0377) is valid byte that might appear in our
	 * input; for this reason, we cannot compare against EOF.
	 */
	if (feof (f) || ferror (f))
	    return (0);
    }
    /* NOTREACHED */
}

/* 
 * read a single line from a stream and return both raw (as read)
 * and cooked (tabs converted to spaces and control characters made
 * printable) if necessary.  Silently truncate input lines at BUFSIZ
 * characters.
 */
static void getline (FILE *f, char **cooked, char **raw)
{
    char buffer[BUFSIZ+1];
    char *s;
    int j, flag;

    for (j = 0; j < BUFSIZ; j++) {
	buffer[j] = getc (f);
	/* see comment above about EOF */
	if ((buffer[j] == '\n') || feof (f) || ferror (f))
	    break;
    }
    /* see comment above about EOF */
    if ((j == 0) && (feof (f) || ferror (f)))
	*cooked = *raw = NULL;
    buffer[j] = '\0';
    s = duplicate (buffer, &flag);
    /* the line was too long; toss the rest */
    if (j >= BUFSIZ)
	while (getc (f) != '\n')
	    ;
    *cooked = s;
    *raw = (flag) ? strdup (buffer) : NULL;
}

/* 
 * minimal doubly-linked list functions
 */
static Block *blist, *last;

static void reset_blist (void)
{
    blist = last = NULL;
}

static Block *get_blist (void)
{
    return (blist);
}

static void add_blist (Block *b)
{
    if (blist == NULL)
	blist = last = b;
    else {
	last->next = b;
	b->prev = last;
	last = last->next;
    }
}
