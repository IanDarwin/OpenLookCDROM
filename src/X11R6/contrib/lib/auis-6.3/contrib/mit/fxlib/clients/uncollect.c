/**********************************************************************
 * File Exchange uncollect client
 *
 * $Author: rr2b $
 * $Source: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/contrib/mit/fxlib/clients/RCS/uncollect.c,v $
 * $Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/contrib/mit/fxlib/clients/RCS/uncollect.c,v 1.3 1992/12/15 21:52:15 rr2b R6tape $
 *
 * Copyright 1989, 1990 by the Massachusetts Institute of Technology.
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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/contrib/mit/fxlib/clients/RCS/uncollect.c,v 1.3 1992/12/15 21:52:15 rr2b R6tape $";
#endif

#include <mit-copyright.h>

 

#include <stdio.h>
#include <sys/time.h>
#include <memory.h>
#include <ctype.h>
#include <strings.h>
#include <sys/errno.h>
#include <sys/types.h>
#include <sys/stat.h>
#include "fxmain.h"

/*
 * do_uncollect -- dumps papers into files
 */

long
do_uncollect(fxp, criterion, flags, user)
     FX *fxp;
     Paper *criterion;
     int flags;
     char *user;
{
  long code;
  Paperlist_res *plist;
  Paperlist l;
  PaperType newtype;
  Paper newp;

  newtype = criterion->type;
  criterion->type = TAKEN;
  criterion->author = user;
  /******** get list of papers from server ********/
  code = fx_list(fxp, criterion, &plist);
  criterion->type = newtype;
  if (code) {
    strcpy(fxmain_error_context, "while retrieving list");
    return(code);
  }

  /******** main loop through list ********/
  for (l = plist->Paperlist_res_u.list; l != NULL; l = l->next) {

    /******* Skip papers not in time range ********/
    if (l->p.modified.tv_sec < criterion->created.tv_sec ||
        l->p.modified.tv_sec > criterion->modified.tv_sec) continue;

    paper_copy(&(l->p), &newp);
    newp.type = newtype;
    if (flags & VERBOSE) {
      /******** print information about file ********/
      printf("%5d %-9s %9d  %-16.16s  %s\n", newp.assignment,
             newp.owner, newp.size, ctime(&(newp.created.tv_sec)),
	     newp.filename);
    }
    if (!(flags & LISTONLY)) {
      code = fx_move(fxp, &(l->p), &newp);
      if (code) {
	sprintf(fxmain_error_context, "while restoring %s by %s",
		l->p.filename, l->p.author);
	goto DO_UNCOLLECT_ABORT;
      }
    }
  }

DO_UNCOLLECT_ABORT:
  fx_list_destroy(&plist);
  return(code);
}

/*
 * main uncollect procedure
 */

main(argc, argv)
  int argc;
  char *argv[];
{
  Paper p;

  paper_clear(&p);
  p.type = TURNEDIN;

  if (fxmain(argc, argv,
             "Usage: %s [options] [assignment] [username ...]\n",
             &p, NULL, do_uncollect)) exit(1);
  exit(0);
}
