/**********************************************************************
 * File Exchange client routines
 *
 * $Author: rr2b $
 * $Source: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/contrib/mit/fxlib/clients/RCS/arg.c,v $
 * $Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/contrib/mit/fxlib/clients/RCS/arg.c,v 1.3 1992/12/15 21:51:18 rr2b R6tape $
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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/contrib/mit/fxlib/clients/RCS/arg.c,v 1.3 1992/12/15 21:51:18 rr2b R6tape $";
#endif

#include <mit-copyright.h>

 

#include <stdio.h>
#include <fxcl.h>
#include <strings.h>
#include <ctype.h>

/*** Global variables ***/

extern char Course[];
extern Paperlist_res *Plist;
extern Paper Criterion;

count_papers(plist)
     Paperlist plist;
{
  int i = 0;
  Paperlist node;

  for (node = plist; node; node = node->next) i++;
  return(i);
}

paper_named(s)
     char *s;
{
  int i=0;
  Paperlist node;

  for (node = Plist->Paperlist_res_u.list; node; node = node->next) {
    i++;
    if (strcmp(s, node->p.filename) == 0) return(i);
  }
  return(0);
}

low_bound(arg)
     char *arg;
{
  if (isalpha(arg[0])) return(paper_named(arg));
  if (arg[0] == ',' || arg[0] == ':' || strcmp(arg, "all") == 0
      || strcmp(arg, "*") == 0) return(1);
  return(atoi(arg));
}

high_bound(arg)
     char *arg;
{
  char *s;

  if (isalpha(arg[0])) return(paper_named(arg));
  if (arg[strlen(arg)-1] == ',' || arg[strlen(arg)-1] == ':'
      || strcmp(arg, "*") == 0 || strcmp(arg, "all") == 0)
    return(count_papers(Plist->Paperlist_res_u.list));
  
  if (index(arg, ',')) s = index(arg, ',');
  else s = index(arg, ':');

  if (s) return(atoi(s+1));
  else return(atoi(arg));
}
