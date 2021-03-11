/* ********************************************************************** *\
 *         Copyright IBM Corporation 1988,1991 - All Rights Reserved      *
 *        For full copyright information see:'andrew/config/COPYRITE'     *
\* ********************************************************************** */
/* $Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/text/RCS/textref.ch,v 1.2 1991/09/12 19:52:44 bobg R6tape $ */
/* $ACIS: $ */
/* $Source: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/text/RCS/textref.ch,v $ */

#if !defined(lint) && !defined(LOCORE) && defined(RCS_HDRS)
static char *rcsid_celview_H = "$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/text/RCS/textref.ch,v 1.2 1991/09/12 19:52:44 bobg R6tape $ ";
#endif

class textref :fnote {
overrides:
	ViewName() returns char *;
methods:
	GetRef(long size, char * buf) returns char *;
};

