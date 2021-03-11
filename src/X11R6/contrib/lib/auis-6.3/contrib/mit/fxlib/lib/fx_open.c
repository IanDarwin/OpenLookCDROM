/**********************************************************************
 * File Exchange client library
 *
 * $Author: rr2b $
 * $Source: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/contrib/mit/fxlib/lib/RCS/fx_open.c,v $
 * $Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/contrib/mit/fxlib/lib/RCS/fx_open.c,v 1.3 1992/12/15 21:53:01 rr2b R6tape $
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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/contrib/mit/fxlib/lib/RCS/fx_open.c,v 1.3 1992/12/15 21:53:01 rr2b R6tape $";
#endif

#include <mit-copyright.h>

 

#include <stdio.h>
#include <strings.h>
#include "fxcl.h"
#include "memory.h"

/*** Global variables ***/

#ifdef __STDC__
void (*fx_open_error_hook)(FX *, long) = fx_open_perror;
#else
void (*fx_open_error_hook)() = fx_open_perror;
#endif

char *fx_sync_host = NULL;  /* host known as sync site */
extern int errno;

/*
 * fx_open(s, codep) -- connect to file exchange named s.
 *
 * Sets *codep to error number if codep != NULL;
 *   (Some access may still be allowed despite error)
 * Returns NULL upon total failure
 */

FX *
fx_open(s, codep)
     char *s;
     long *codep;
{
  init_res *res = NULL;
  FX *ret;
  stringlist hosts, node;
  long code = 0L;

  /* Initialization needed for com_err routines */
  initialize_fxcl_error_table();
  initialize_rpc_error_table();
  initialize_fxsv_error_table();
  initialize_krb_error_table();

  /* set up new FX */
  if ((ret = New(FX)) == NULL) {
    code = (long) errno;
    goto FX_OPEN_CLEANUP;
  }
  (void) strcpy(ret->name, s);
  ret->cl = NULL;

  /* get list of hosts to try */
  code = ERR_FXCL_HOSTS;
  hosts = fx_host_list(FX_DEF_SERVICE);

  /* try to initialize course at each host */
  node = hosts;
  while (code && node) {
    (void) strcpy(ret->host, node->s);
    if (res) xdr_free(xdr_init_res, (char *) res);
    code = fx_init(ret, &res);
    if (node->next) fx_open_error_hook(ret, code);
    node = node->next;
  }
  fx_host_list_destroy(hosts);
  if (code) goto FX_OPEN_CLEANUP;

  /* if this host is not the sync host, need to start over */
  if (res->errno == ERR_NOT_SYNC) {
    clnt_destroy(ret->cl);
    (void) strcpy(ret->host, res->init_res_u.sync);
    code = fx_init(ret, &res);
  }
  if (res->errno) code = res->errno;

 FX_OPEN_CLEANUP:
  if (res) xdr_free(xdr_init_res, (char *) res);
  if (codep) *codep = code;
  if (ret)
    if (!ret->cl) {
      free((char *) ret);
      ret = NULL;
    }
  return(ret);
}

void
fx_open_perror(fxp, code)
     FX *fxp;
     long code;
{
  if (code)
    com_err(fxp->host, code, "(%s)", fxp->name);
}

void
fx_close(fxp)
     FX *fxp;
{
  if (fxp) {
    if (fxp->cl) clnt_destroy(fxp->cl);
    free((char *)fxp);
  }
}
