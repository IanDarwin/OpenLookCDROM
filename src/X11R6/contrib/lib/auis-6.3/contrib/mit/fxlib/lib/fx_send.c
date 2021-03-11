/**********************************************************************
 * File Exchange client library
 *
 * $Author: rr2b $
 * $Source: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/contrib/mit/fxlib/lib/RCS/fx_send.c,v $
 * $Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/contrib/mit/fxlib/lib/RCS/fx_send.c,v 1.3 1992/12/15 21:53:01 rr2b R6tape $
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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/contrib/mit/fxlib/lib/RCS/fx_send.c,v 1.3 1992/12/15 21:53:01 rr2b R6tape $";
#endif

#include <mit-copyright.h>

 

#include "fxcl.h"

/*
 * fx_send -- send a stream to the exchange
 */

long
fx_send(fxp, p, fp)
     FX *fxp;
     Paper *p;
     FILE *fp;
{
  long *ret, code = 0L;
  int dummy;
  burst_data data;
  Paper to_send;
#ifdef KERBEROS
  char new_owner[FX_UNAMSZ], new_author[FX_UNAMSZ];
#else
  char new_owner[9], new_author[9];
#endif

  /* take care of null pointers */
  if (p) paper_copy(p, &to_send);
  else paper_clear(&to_send);

  if (!to_send.location.host)
    to_send.location.host = fxp->host;
  if (!to_send.author) to_send.author = fxp->owner;
  if (!to_send.owner) to_send.owner = fxp->owner;
  if (!to_send.filename) to_send.filename = FX_DEF_FILENAME;
  if (!to_send.desc) to_send.desc = FX_DEF_DESC;
  if (!to_send.assignment) to_send.assignment = FX_DEF_ASSIGNMENT;
  if (!to_send.type) to_send.type = FX_DEF_TYPE;

#ifdef KERBEROS
  /* lengthen usernames to kerberos principals */
  to_send.owner = _fx_lengthen(fxp, to_send.owner, new_owner);
  to_send.author = _fx_lengthen(fxp, to_send.author, new_author);
#endif

  if ((ret = send_file_1(&to_send, fxp->cl)) == NULL)
    goto FX_SEND_CLEANUP;

  if (*ret) goto FX_SEND_CLEANUP;

  /* send the bursts */
  do {
    if (ret) xdr_free(xdr_long, (char *) ret);
    data.size = fread(data.data, 1, MAX_BURST_SIZE, fp);
    if ((ret = send_burst_1(&data, fxp->cl)) == NULL)
      goto FX_SEND_CLEANUP;
    if (*ret) goto FX_SEND_CLEANUP;
  } while (data.size == MAX_BURST_SIZE);

  ret = end_send_1(&dummy, fxp->cl);

 FX_SEND_CLEANUP:
  if (!ret) return(_fx_rpc_errno(fxp->cl));
  code = *ret;
  xdr_free(xdr_long, (char *) ret);
  return(code);
}
