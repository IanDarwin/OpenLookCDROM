/**********************************************************************
 * File Exchange client library
 *
 * $Author: rr2b $
 * $Source: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/contrib/mit/fxlib/lib/RCS/fx_list.c,v $
 * $Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/contrib/mit/fxlib/lib/RCS/fx_list.c,v 1.3 1992/12/15 21:52:40 rr2b R6tape $
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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/contrib/mit/fxlib/lib/RCS/fx_list.c,v 1.3 1992/12/15 21:52:40 rr2b R6tape $";
#endif

#include <mit-copyright.h>

 

#include "fxcl.h"

/*
 * fx_list -- fill in a Paperlist_res for given criterion
 */

long
fx_list(fxp, p, ret)
     FX *fxp;
     Paper *p;
     Paperlist_res **ret;
{
  register Paperlist node;
  Paper criterion;
  char new_owner[FX_UNAMSZ], new_author[FX_UNAMSZ];

  /* take care of null pointers */
  if (p) paper_copy(p, &criterion);
  else paper_clear(&criterion);

  if (!criterion.location.host)
    criterion.location.host = ID_WILDCARD;
  if (!criterion.author) criterion.author = AUTHOR_WILDCARD;
  if (!criterion.owner) criterion.owner = OWNER_WILDCARD;
  if (!criterion.filename) criterion.filename = FILENAME_WILDCARD;
  if (!criterion.desc) criterion.desc = FX_DEF_DESC;

#ifdef KERBEROS
  /* lengthen usernames to kerberos principals */
  if (strcmp(criterion.owner, OWNER_WILDCARD))
    criterion.owner = _fx_lengthen(fxp, criterion.owner, new_owner);
  if (strcmp(criterion.author, AUTHOR_WILDCARD))
    criterion.author = _fx_lengthen(fxp, criterion.author, new_author);
#endif

  /* try to retrieve list */
  if ((*ret = list_1(&criterion, fxp->cl)) == NULL)
    return(_fx_rpc_errno(fxp->cl));

#ifdef KERBEROS
  /* shorten kerberos principals to usernames */
  for (node = (*ret)->Paperlist_res_u.list; node; node = node->next) {
    _fx_shorten(fxp, node->p.owner);
    _fx_shorten(fxp, node->p.author);
  }
#endif

  return((*ret)->errno);
}
