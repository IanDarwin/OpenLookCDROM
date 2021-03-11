/**********************************************************************
 * File Exchange client library
 *
 * $Author: susan $
 * $Source: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/contrib/mit/fxlib/lib/RCS/fx_init.c,v $
 * $Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/contrib/mit/fxlib/lib/RCS/fx_init.c,v 1.4 1993/05/17 17:06:39 susan Exp $
 *
 * Copyright 1989, 1990 by the Massachusetts Institute of Technology.
 *
 * For copying and distribution information, please see the file
 * <mit-copyright.h>.
 **********************************************************************/

#include <mit-copyright.h>

#ifndef lint
static char rcsid_fx_init_c[] = "$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/contrib/mit/fxlib/lib/RCS/fx_init.c,v 1.4 1993/05/17 17:06:39 susan Exp $";
#endif /* lint */

#include <netdb.h>
#include <strings.h>
#include <krb.h>
#include <des.h>
#include "fxcl.h"

/* Hack!  Ensure we are using the RIGHT version of the RPC
 * Library. */

extern int link_with_libfxrpc;

/*
 * fx_init -- establish client connection for FX *
 *             fxp->name and fxp->host should already be set;
 *             fxp->host will be canonicalized if possible;
 *             if return value is nonzero, be sure to
 *               xdr_free(xdr_init_res, (char *) *resp);
 */

long
fx_init(fxp, resp)
     FX *fxp;
     init_res **resp;
{
  init_data params;
  long code;
  struct hostent *h;
#ifndef KERBEROS
  struct passwd *pw;
#endif /* KERBEROS */

  /* The following line forces us to confirm at link time
    that we have the right version of the rpc lib linked. */
  link_with_libfxrpc = 1;

  /* establish RPC client connection */
  fxp->cl = clnt_create(fxp->host, FXSERVER, FXVERS, "tcp");
  if (!fxp->cl) return(_fx_rpc_errno(fxp->cl));

#ifdef KERBEROS

  /* get kerberos authentication */
  if (code = _fx_get_auth(fxp, &params.auth)) return(code);

#else /* KERBEROS */

  /* stolen from ZGetSender.c,v 1.7 88/05/13 15:05:07 rfrench */

  /* XXX a uid_t is a u_short (now),  but getpwuid
   * wants an int. AARGH! */
  pw = getpwuid((int) getuid());
  if (!pw)
    strcpy(fxp->owner, "unknown");
  else (void) strcpy(fxp->owner, pw->pw_name);
  fxp->extension = &fxp->owner[strlen(fxp->owner)];

#endif /* KERBEROS */

  /* initialize connection for course */
  params.course = fxp->name;
  if ((*resp = init_1(&params, fxp->cl)) == NULL) {
    code = _fx_rpc_errno(fxp->cl);
    clnt_destroy(fxp->cl);
    fxp->cl = NULL;
    return(code);
  }

  /* change hostname to official name, if possible */
  if (h = gethostbyname(fxp->host))
    (void) strcpy(fxp->host, h->h_name);
  return(0L);
}

#ifdef KERBEROS

/*
 * _fx_get_auth -- fill in authenticator, owner of fxp
 */

long
_fx_get_auth(fxp, authent)
     FX *fxp;
     KTEXT_ST *authent;
{
  krb_info_res *res;
  int dummy;
  int opened = 0;
  char pname[ANAME_SZ], pinst[INST_SZ], prealm[REALM_SZ];

  res = krb_info_1(&dummy, fxp->cl);
  if (!res) return(_fx_rpc_errno(fxp->cl));

  if (res->errno) {
    xdr_free(xdr_krb_info_res, (char *) res);
    return(res->errno);
  }

  /*
   * We must find the realm of the ticket file here before calling
   * tf_init because since the realm of the ticket file is not
   * really stored in the principal section of the file, the
   * routine we use must itself call tf_init and tf_close.
   */
  if ((dummy = krb_get_tf_realm((char *)TKT_FILE, prealm)) != KSUCCESS)
    goto _FX_GET_AUTH_CLEANUP;

  dummy = krb_mk_req(authent,
		     res->krb_info_res_u.info.service,
		     res->krb_info_res_u.info.instance,
		     krb_realmofhost(fxp->host), 0);
  if (dummy) goto _FX_GET_AUTH_CLEANUP;

  /* fill in owner */
  dummy = tf_init((char *)TKT_FILE, R_TKT_FIL);
  if (dummy) goto _FX_GET_AUTH_CLEANUP;
  opened = 1;
  dummy = tf_get_pname(pname);
  if (dummy) goto _FX_GET_AUTH_CLEANUP;
  dummy = tf_get_pinst(pinst);
  if (dummy) goto _FX_GET_AUTH_CLEANUP;
  (void) sprintf(fxp->owner, "%s%s%s@%s", pname, (pinst[0]?".":""),
		 pinst, prealm);
  fxp->extension = index(fxp->owner, '@');

 _FX_GET_AUTH_CLEANUP:
  if (opened) tf_close();
  xdr_free(xdr_krb_info_res, (char *) res);
  if (dummy) return(ERROR_TABLE_BASE_krb + (long) dummy);
  return(0L);
}

#endif /* KERBEROS */
