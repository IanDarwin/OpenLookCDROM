/* ********************************************************************** *\
 *         Copyright IBM Corporation 1988,1991 - All Rights Reserved      *
 *        For full copyright information see:'andrew/config/COPYRITE'     *
\* ********************************************************************** */
/* $Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/overhead/util/hdrs/RCS/profile.h,v 2.4 1991/09/12 20:34:54 bobg R6tape $ */
/* $ACIS: $ */
/* $Source: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/overhead/util/hdrs/RCS/profile.h,v $ */

#if !defined(lint) && !defined(LOCORE) && defined(RCS_HDRS)
static char *rcsid_h = "$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/overhead/util/hdrs/RCS/profile.h,v 2.4 1991/09/12 20:34:54 bobg R6tape $ ";
#endif /* !defined(lint) && !defined(LOCORE) && defined(RCS_HDRS) */

struct proFile {
    long lastmod;
    struct proFile *next;
    char name[1];
};

struct proFile *proFileList();
