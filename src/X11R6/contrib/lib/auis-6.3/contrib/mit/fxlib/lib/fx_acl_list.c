/**********************************************************************
 * File Exchange client library
 *
 * $Author: rr2b $
 * $Source: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/contrib/mit/fxlib/lib/RCS/fx_acl_list.c,v $
 * $Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/contrib/mit/fxlib/lib/RCS/fx_acl_list.c,v 1.3 1992/12/15 21:52:40 rr2b R6tape $
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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/contrib/mit/fxlib/lib/RCS/fx_acl_list.c,v 1.3 1992/12/15 21:52:40 rr2b R6tape $";
#endif

#include <mit-copyright.h>

 

#include "fxcl.h"

/*
 * fx_acl_list -- fills in list of users in access control list
 */

long
fx_acl_list(fxp, aclname, ret)
     FX *fxp;
     char *aclname;
     stringlist_res **ret;
{
  register stringlist node;

  *ret = list_acl_1(&aclname, fxp->cl);
  if (!*ret) return(_fx_rpc_errno(fxp->cl));

#ifdef KERBEROS
  /* shorten kerberos principals to usernames */
  for (node = (*ret)->stringlist_res_u.list; node; node = node->next)
    _fx_shorten(fxp, node->s);
#endif

  return((*ret)->errno);
}
