/* ********************************************************************** *\
 *         Copyright IBM Corporation 1988,1991 - All Rights Reserved      *
 *        For full copyright information see:'andrew/config/COPYRITE'     *
\* ********************************************************************** */
/* $Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/extensions/RCS/dsearch.ch,v 2.7 1991/09/12 19:38:45 bobg R6tape $ */
/* $ACIS:dsearch.ch 1.2$ */
/* $Source: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/extensions/RCS/dsearch.ch,v $ */

#if !defined(lint) && !defined(LOCORE) && defined(RCS_HDRS)
static char *rcsiddynsearch_H = "$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/extensions/RCS/dsearch.ch,v 2.7 1991/09/12 19:38:45 bobg R6tape $";
#endif /* !defined(lint) && !defined(LOCORE) && defined(RCS_HDRS) */

package dynsearch[dsearch] {
    classprocedures:
	InitializeClass() returns boolean;
};
