/*
 * The FX (File Exchange) Library
 *
 * $Author: susan $
 * $Source: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/contrib/mit/fxlib/include/RCS/fx-internal.h,v $
 * $Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/contrib/mit/fxlib/include/RCS/fx-internal.h,v 1.3 1993/05/17 17:08:10 susan Exp $
 *
 * Copyright 1989, 1990 by the Massachusetts Institute of Technology.
 *
 * For copying and distribution information, please see the file
 * <mit-copyright.h>.
 */

/*
 * Definitions common to both library and server.
 */

#ifndef _fx_internal_h_

/*
 * ACL names.
 */

#define ACL_TURNIN "turnin"
#define ACL_GRADER "grader"
#define ACL_MAINT "maint"

/*
 * Kerberos authentication service name.
 */

#ifdef KERBEROS

#define KRB_SERVICE "rcmd"

#endif /* KERBEROS */

/*
 * Hesiod name service.
 */

#ifdef HESIOD

#define HES_NAME "turnin"
#define HES_TYPE "sloc"

#else

#if defined(_IBMR2)
#define SERVER_LIST_FILE "/usr/lpp/exchange/server.list"
#else /* defined(_IBMR2) */
#if defined(ultrix)
#define SERVER_LIST_FILE "/var/exchange/server.list"
#else
#define SERVER_LIST_FILE "/site/exchange/server.list"
#endif /* defined(ultrix) */
#endif /* defined(_IBMR2) */

#endif /* HESIOD */

/*
 * Format of a paper displayed to the user - usually for debugging.
 */

#define PRINTPAPER(p) "%s:%s %d:\"%s\",%d (%d,%d,%d)\n    [%s:%d.%d] [%d.%d] [%d.%d]\n", \
    (p)->author, (p)->owner, (p)->type, (p)->filename, \
    (p)->assignment, (p)->size, (p)->words, (p)->lines, (p)->location.host, \
    (p)->location.time.tv_sec, (p)->location.time.tv_usec, \
    (p)->created.tv_sec, (p)->created.tv_usec, (p)->modified.tv_sec, \
    (p)->modified.tv_usec

#endif /* _fx_internal_h_ */
