/* ********************************************************************** *\
 *         Copyright IBM Corporation 1988,1991 - All Rights Reserved      *
 *        For full copyright information see:'andrew/config/COPYRITE'     *
\* ********************************************************************** */
 
/* $Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/text/RCS/bp.ch,v 1.3 1991/09/12 19:51:22 bobg R6tape $ */

#if !defined(lint) && !defined(LOCORE) && defined(RCS_HDRS)
static char *rcsid_bp_ch = "$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/text/RCS/bp.ch,v 1.3 1991/09/12 19:51:22 bobg R6tape $ ";
#endif

class bp : dataobject [dataobj] {
overrides:
      ViewName() returns char *;
};

