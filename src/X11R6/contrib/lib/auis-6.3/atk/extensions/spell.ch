/* ********************************************************************** *\
 *         Copyright IBM Corporation 1988,1991 - All Rights Reserved      *
 *        For full copyright information see:'andrew/config/COPYRITE'     *
\* ********************************************************************** */
/* $Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/extensions/RCS/spell.ch,v 2.5 1991/09/12 19:39:06 bobg R6tape $ */
/* $ACIS:spell.ch 1.2$ */
/* $Source: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/extensions/RCS/spell.ch,v $ */

#if !defined(lint) && !defined(LOCORE) && defined(RCS_HDRS)
static char *rcsidspell_H = "$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/extensions/RCS/spell.ch,v 2.5 1991/09/12 19:39:06 bobg R6tape $";
#endif /* !defined(lint) && !defined(LOCORE) && defined(RCS_HDRS) */

package spell {
    classprocedures:
        CheckDocument(struct textview *self, long rock);
        InitializeClass() returns boolean;
};
