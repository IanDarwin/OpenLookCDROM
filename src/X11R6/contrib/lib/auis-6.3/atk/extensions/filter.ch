/* ********************************************************************** *\
 *         Copyright IBM Corporation 1988,1991 - All Rights Reserved      *
 *        For full copyright information see:'andrew/config/COPYRITE'     *
\* ********************************************************************** */
/* $Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/extensions/RCS/filter.ch,v 2.5 1991/09/12 19:38:51 bobg R6tape $ */
/* $ACIS:filter.ch 1.2$ */
/* $Source: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/extensions/RCS/filter.ch,v $ */

#if !defined(lint) && !defined(LOCORE) && defined(RCS_HDRS)
static char *rcsidfilter_H = "$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/extensions/RCS/filter.ch,v 2.5 1991/09/12 19:38:51 bobg R6tape $";
#endif /* !defined(lint) && !defined(LOCORE) && defined(RCS_HDRS) */

/* filter the textview selection region through a command
 */

package filter {
    classprocedures:
        InitializeClass() returns boolean;
};
