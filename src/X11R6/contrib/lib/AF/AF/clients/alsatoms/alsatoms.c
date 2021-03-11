/*
 * $XConsortium: xlsatoms.c,v 1.3 90/12/17 18:47:10 gildea Exp $
 *
 * Copyright 1989 Massachusetts Institute of Technology
 *
 * Permission to use, copy, modify, distribute, and sell this software and its
 * documentation for any purpose is hereby granted without fee, provided that
 * the above copyright notice appear in all copies and that both that
 * copyright notice and this permission notice appear in supporting
 * documentation, and that the name of M.I.T. not be used in advertising or
 * publicity pertaining to distribution of the software without specific,
 * written prior permission.  M.I.T. makes no representations about the
 * suitability of this software for any purpose.  It is provided "as is"
 * without express or implied warranty.
 *
 * M.I.T. DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING ALL
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL M.I.T.
 * BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION
 * OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN 
 * CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *
 */
/*
 * Copyright 1993 by Digital Equipment Corporation, Maynard, Massachusetts.
 * 
 * Permission to use, copy, modify, distribute, and sell this software and its 
 * documentation for any purpose is hereby granted without fee, provided that 
 * the above copyright notice appear in all copies and that both that 
 * copyright notice and this permission notice appear in supporting 
 * documentation, and that the name of Digital not be used in advertising or 
 * publicity pertaining to distribution of the software without specific, 
 * written prior permission.  Digital makes no representations about the 
 * suitability of this software for any purpose.  It is provided "as is" 
 * without express or implied warranty.
 * 
 * DIGITAL DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING 
 * ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL 
 * DIGITAL BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY 
 * DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN 
 * AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF 
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 */
#include <stdio.h>
#include <stdlib.h>			/* exit, etc. proto		*/
#include <strings.h>
#include <AF/AFlib.h>
#include <AF/audioproto.h>

char *ProgramName;
void do_range (
    AFAudioConn *aud,
    char *format,
    char *range
	);
void do_name (
    AFAudioConn *aud,
    char *format,
    char *name
    );

void list_atoms (
    AFAudioConn *aud,
    char *format,
    int mask,
    long low,
    long high	
	);

static void usage ()
{
    fprintf (stderr, "usage:  %s [-options...]\n\n", ProgramName);
    fprintf (stderr, "where options include:\n");
    fprintf (stderr,
	     "    -server aud             Audio server to which to connect\n");
    fprintf (stderr,
	     "    -format string          printf-style format to use\n");
    fprintf (stderr,
	     "    -range [num]-[num]      atom values to list\n");
    fprintf (stderr,
	     "    -name string            name of single atom to print\n");
    putc ('\n', stderr);
    exit (1);
}


main (int argc, char **argv)
{
    char *servername = NULL;
    char *format = "%lu\t%s";
    int i, doit;
    int didit = 0;
    AFAudioConn	*aud;

    ProgramName = argv[0];

    for (doit = 0; doit < 2; doit++) {	/* pre-parse to get display */
	for (i = 1; i < argc; i++) {
	    char *arg = argv[i];

	    if (arg[0] == '-') {
		switch (arg[1]) {
		  case 's':			/* -display dpy */
		    if (++i >= argc) usage ();
		    if (!doit) servername = argv[i];
		    continue;
		  case 'f':			/* -format string */
		    if (++i >= argc) usage ();
		    if (doit) format = argv[i];
		    continue;
		  case 'r':			/* -range num-[num] */
		    if (++i >= argc) usage ();
		    if (doit) {
			do_range (aud, format, argv[i]);
			didit = 1;
		    }
		    continue;
		  case 'n':			/* -name string */
		    if (++i >= argc) usage ();
		    if (doit) {
			do_name (aud, format, argv[i]);
			didit = 1;
		    }
		    continue;
		}
	    }
	    usage ();
	}
	if (!doit) {
	    aud = AFOpenAudioConn (servername);
	    if (!aud) {
		fprintf (stderr, "%s:  unable to open display \"%s\"\n",
			 ProgramName, servername);
		exit (1);
	    }
	} else
	    if (!didit)		/* no options, default is list all */
		list_atoms(aud, format, 0, 0, 0);
    }

    AFCloseAudioConn (aud);
    exit (0);
    /*NOTREACHED*/
}

void do_name (
    AFAudioConn *aud,
    char *format,
    char *name
    )
{
    AAtom a = AFInternAtom (aud, name, ATrue);

    if (a != ANone) {
	printf (format, (unsigned long) a, name);
	putchar ('\n');
    } else {
	fprintf (stderr, "%s:  no atom named \"%s\" on server \"%s\"\n",
		 ProgramName, name, AudioConnString(aud));
    }
}


#define RangeLow (1 << 0)
#define RangeHigh (1 << 1)

static int parse_range (
    char *range,
    long *lowp,
    long *highp
	)
{
    char *dash;
    int mask = 0;

    if (!range) {			/* NULL means default */
	*lowp = 1;
	return RangeLow;
    }

    dash = index (range, '-');
    if (!dash) dash = index (range, ':');
    if (dash) {
	if (dash == range) {		/* -high */
	    *lowp = 1;
	} else {			/* low-[high] */
	    *dash = '\0';
	    *lowp = atoi (range);
	    *dash = '-';
	}
	mask |= RangeLow;
	dash++;
	if (*dash) {			/* [low]-high */
	    *highp = atoi (dash);
	    mask |= RangeHigh;
	}
    } else {				/* number (low == high) */
	*lowp = *highp = atoi (range);
	mask |= (RangeLow | RangeHigh);
    }

    return mask;
}

void do_range (
    AFAudioConn *aud,
    char *format,
    char *range
	)
{
    int mask;
    long low, high;

    mask = parse_range (range, &low, &high);
    list_atoms (aud, format, mask, low, high);
}

static int catcher (AFAudioConn *aud, AFErrorEvent *err)
{
    char buf[100];

    if (err->request_code != A_GetAtomName) {
	AFGetErrorText(aud, err->request_code, buf, 100);
	fprintf(stderr, buf);
    }
    return 0;
}

void list_atoms (
    AFAudioConn *aud,
    char *format,
    int mask,
    long low,
    long high	
	)
{
    int (*oldhandler)(AFAudioConn *, AFErrorEvent *) = AFSetErrorHandler (catcher);

    switch (mask) {
      case RangeHigh:
	low = 1;
	/* fall through */
      case (RangeLow | RangeHigh):
	for (; low <= high; low++) {
	    char *s = AFGetAtomName (aud, (AAtom)low);
	    if (s) {
		printf (format, low, s);
		putchar ('\n');
		AFree (s);
	    }
	}
	break;

      default:
	low = 1;
	/* fall through */
      case RangeLow:
	for (; ; low++) {
	    char *s = AFGetAtomName (aud, (AAtom)low);
	    if (s) {
		printf (format, low, s);
		putchar ('\n');
		AFree (s);
	    } else {
		break;
	    }
	}
	break;
    }

    AFSetErrorHandler (oldhandler);
    return;
}
