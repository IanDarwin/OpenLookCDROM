/**********************************************************************
 * File Exchange return client
 *
 * $Author: rr2b $
 * $Source: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/contrib/mit/fxlib/clients/RCS/return.c,v $
 * $Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/contrib/mit/fxlib/clients/RCS/return.c,v 1.3 1992/12/15 21:52:15 rr2b R6tape $
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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/contrib/mit/fxlib/clients/RCS/return.c,v 1.3 1992/12/15 21:52:15 rr2b R6tape $";
#endif

#include <mit-copyright.h>

 

#include <stdio.h>
#include <ctype.h>
#include <strings.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/errno.h>
#include "fxmain.h"

/*
 * compar -- compares two papers by modify time, used by qsort
 */

compar(p1, p2)
     Paper **p1, **p2;
{
  int ret;

  ret = strcmp((*p1)->author, (*p2)->author);
  if (ret) return(ret);
  ret = strcmp((*p1)->filename, (*p2)->filename);
  if (ret) return(ret);
  ret = (int) (*p1)->modified.tv_sec - (*p2)->modified.tv_sec;
  if (ret) return(ret);
  return((int) ((*p1)->modified.tv_usec - (*p2)->modified.tv_usec));
}

/*
 * do_return -- returns papers from files
 */

/*ARGSUSED*/
long
do_return(fxp, criterion, flags, arg)
     FX *fxp;
     Paper *criterion;
     int flags;
     char *arg;
{
  extern int errno;
  long code;
  Paperlist_res *plist;
  int count, i;
  char *s;
  char filename[256];
  Paper **paperv;
  struct stat buf;
  Paper gpaper;
  PaperType newtype;

  newtype = criterion->type;
  criterion->type = TAKEN;
  /******** get list of papers from server ********/
  code = fx_list(fxp, criterion, &plist);
  criterion->type = newtype;
  if (code) {
    strcpy(fxmain_error_context, "while retrieving list");
    return(code);
  }

  count = get_array(plist->Paperlist_res_u.list, &paperv);

  /******** deal with empty list ********/
  if (count == 0) {
    if (flags & VERBOSE)
      printf("No papers to return.\n");
    return(0L);
  }

  /******** main loop through list ********/
  for (i=0; i<count; i++) {

    /*** Skip duplicates ***/
    if (i < count-1 &&
	!strcmp(paperv[i]->author, paperv[i+1]->author) &&
	!strcmp(paperv[i]->filename, paperv[i+1]->filename)) {
      if (!(flags & LISTONLY)) fx_delete(fxp, paperv[i]);
      continue;
    }

    /******* Skip papers not in time range ********/
    if (paperv[i]->modified.tv_sec < criterion->created.tv_sec ||
        paperv[i]->modified.tv_sec > criterion->modified.tv_sec) continue;

    /*** Form filename ***/
    (void) sprintf(filename, "%s/%s", paperv[i]->author,
		   paperv[i]->filename);
    /* change spaces to underscores */
    for (s=filename; *s != '\0'; s++)
      if (isspace(*s)) *s = '_';

    if (flags & VERBOSE) {
      /******** print information about file ********/
      printf("%5d %-9s %9d  %-16.16s  %s\n", paperv[i]->assignment,
	     paperv[i]->owner, paperv[i]->size,
	     ctime(&(paperv[i]->created.tv_sec)), filename);
    }

    if (stat(filename, &buf)) {
      printf("    Couldn't return %s (%s)\n", filename,
	     error_message((long) errno));
      continue;
    }

    if (buf.st_mtime == paperv[i]->created.tv_sec) {
      printf("    Won't return %s (not modified)\n", filename);
      continue;
    }
    if (!(flags & LISTONLY)) {
      /******** return file to server ********/
      paper_copy(paperv[i], &gpaper);
      gpaper.type = newtype;
      code = fx_send_file(fxp, &gpaper, filename);
      if (code) {
	sprintf(fxmain_error_context, "while returning \"%s\"", filename);
	return(code);
      }
      printf("    Returned %s to %s.\n", filename, full_name(gpaper.author));
      fx_delete(fxp, paperv[i]);
    }
  }


  /******** clean up ********/
  fx_list_destroy(&plist);
  free((char *) paperv);
  return(0L);
}

main(argc, argv)
  int argc;
  char *argv[];
{
  Paper p;

  paper_clear(&p);
  p.type = GRADED;
  if (fxmain(argc, argv,
             "Usage: %s [-c course] [options] [assignment] [student ...]\n",
             &p, NULL, do_return)) exit(1);
  exit(0);
}
