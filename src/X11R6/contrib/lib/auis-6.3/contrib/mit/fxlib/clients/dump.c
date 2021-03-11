/**********************************************************************
 * File Exchange collect client
 *
 * $Author: rr2b $
 * $Source: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/contrib/mit/fxlib/clients/RCS/dump.c,v $
 * $Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/contrib/mit/fxlib/clients/RCS/dump.c,v 1.3 1992/12/15 21:51:52 rr2b R6tape $
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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/contrib/mit/fxlib/clients/RCS/dump.c,v 1.3 1992/12/15 21:51:52 rr2b R6tape $";
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
#include <sys/file.h>
#include "fxmain.h"

/*** Global variables ***/
Paper **paperv;
char *full_name();
int compar();
long prep_paper();
void adjust_criterion();
int verbose;

/*
 * do_dump -- dumps papers into files
 */

long
do_dump(fxp, criterion, flags, string)
     FX *fxp;
     Paper *criterion;
     int flags;
     char *string;
{
  extern int errno;
  long code;
  Paperlist_res *plist;
  int count, i;
  char *s;
  char filename[256], newfilename[256];
  int tilde;			/* number for .~n~ backup extension */
  struct timeval tvp[2];	/* for changing mod time */

  verbose = flags & VERBOSE;
  adjust_criterion(criterion, string);
  if ((flags & ONE_AUTHOR) && !(criterion->author))
    criterion->author = fxp->owner;

  /******** get list of papers from server ********/
  code = fx_list(fxp, criterion, &plist);
  if (code) {
    strcpy(fxmain_error_context, "while retrieving list");
    return(code);
  }

  count = get_array(plist->Paperlist_res_u.list, &paperv);

  /******** deal with empty list ********/
  if (count == 0) {
    if (verbose)
      empty_list(criterion);
    goto DUMP_CLEANUP;
  }

  /******** main loop through list ********/
  for (i=0; i<count; i++) {

    /******* Skip papers not in time range ********/
    if (paperv[i]->modified.tv_sec < criterion->created.tv_sec ||
        paperv[i]->modified.tv_sec > criterion->modified.tv_sec) continue;

    /*** do things particular to pickup or collect ***/
    if (code=prep_paper(paperv[i], filename, flags)) goto DUMP_CLEANUP;

    /*** change spaces to underscores ***/
    for (s=filename; *s != '\0'; s++)
      if (isspace(*s)) *s = '_';

    /******** rename local file of same name ********/
    if (access(filename, F_OK) == 0) {
      tilde = 0;
      do {
	sprintf(newfilename, "%s.~%d~", filename, ++tilde);
      } while (access(newfilename, F_OK) == 0);
      if (!(flags & LISTONLY)) {
	if (rename(filename, newfilename)) {
	  sprintf(fxmain_error_context, "renaming %s to %s",
		  filename, newfilename);
	  code = (long) errno;
	  goto DUMP_CLEANUP;
	}
      }
    }

    if (verbose) {
      /******** print information about file ********/
      printf("%5d %-9s %9d  %-16.16s  %s\n", paperv[i]->assignment,
	     paperv[i]->owner, paperv[i]->size,
	     ctime(&(paperv[i]->created.tv_sec)), filename);
    }

    if (!(flags & LISTONLY)) {
      /******** retrieve file from server ********/
      code = fx_retrieve_file(fxp, paperv[i], filename);
      if (code) {
	sprintf(fxmain_error_context, "while retrieving \"%s\"", filename);
	goto DUMP_CLEANUP;
      }

      if (!(flags & PRESERVE)) mark_retrieved(fxp, paperv[i]);

      /******** change accessed, updated times of local file ********/
      tvp[0].tv_sec = paperv[i]->modified.tv_sec;
      tvp[0].tv_usec = paperv[i]->modified.tv_usec;
      tvp[1].tv_sec = paperv[i]->created.tv_sec;
      tvp[1].tv_usec = paperv[i]->created.tv_usec;
      utimes(filename, tvp);	/* Do we care if this fails? */
    }
  }


 DUMP_CLEANUP:
  fx_list_destroy(&plist);
  free((char *) paperv);
  return(code);
}
