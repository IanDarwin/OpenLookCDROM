/**********************************************************************
 * File Exchange turnin client
 *
 * $Author: rr2b $
 * $Source: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/contrib/mit/fxlib/clients/RCS/turnin.c,v $
 * $Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/contrib/mit/fxlib/clients/RCS/turnin.c,v 1.3 1992/12/15 21:52:15 rr2b R6tape $
 *
 * Copyright 1990 by the Massachusetts Institute of Technology.
 *
 * For copying and distribution information, please see the file
 * <mit-copyright.h>.
 **********************************************************************/

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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/contrib/mit/fxlib/clients/RCS/turnin.c,v 1.3 1992/12/15 21:52:15 rr2b R6tape $";
#endif

#include <mit-copyright.h>
#include <stdio.h>
#include "fxmain.h"

/*
 * turnin_arg checks to see if the current argument is -g author,
 * which is an option unique to the turnin program.
 */

/*ARGSUSED*/
int
turnin_arg(argc, argv, ip, p, flagp)
     int argc;
     char *argv[];
     int *ip;
     Paper *p;
     int *flagp;
{
  if (argv[*ip][0] == '-' && argv[*ip][1] == 'g') {
    p->type = GRADED;
    p->author = argv[++(*ip)];
    return(1);
  }
  return(0);
}

long
do_turnin(fxp, p, flags, filename)
     FX *fxp;
     Paper *p;
     int flags;
     char *filename;
{
  long code;

  /* If no filename is specified, use stdin if it's not a terminal. */
  if (!filename) {
    if (isatty(0)) return(ERR_USAGE);
    if (!p->filename) p->filename = "stdin";
    if (code = fx_send(fxp, p, stdin))
      strcpy(fxmain_error_context, "while sending from stdin");
    return(code);
  }

  /* If a filename is specified, send that file. */
  if (code = fx_send_file(fxp, p, filename))
    sprintf(fxmain_error_context, "while sending %s", filename);
  else if (flags & VERBOSE) {
    if (p->type == GRADED) printf("Returned %s to %s in %s.", filename,
				  full_name(p->author), fxp->name);
    else printf("Turned in %s to %s on %s.\n",
		filename, fxp->name, fxp->host);
  }
  return(code);
}

main(argc, argv)
     int argc;
     char *argv[];
{
  Paper turnin_paper;

  paper_clear(&turnin_paper);
  turnin_paper.type = TURNEDIN;
  if (fxmain(argc, argv,
	     "USAGE: %s [options] assignment filename\n",
	     &turnin_paper, turnin_arg, do_turnin)) exit(1);
  exit(0);
}
