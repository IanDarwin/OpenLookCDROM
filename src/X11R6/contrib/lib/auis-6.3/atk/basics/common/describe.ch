/* ********************************************************************** *\
 *         Copyright IBM Corporation 1988,1991 - All Rights Reserved      *
 *        For full copyright information see:'andrew/config/COPYRITE'     *
\* ********************************************************************** */
/* $Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/basics/common/RCS/describe.ch,v 2.7 1991/09/12 19:21:37 bobg R6tape $ */
/* $Source: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/basics/common/RCS/describe.ch,v $ */

#if !defined(lint) && !defined(LOCORE) && defined(RCS_HDRS)
static char *rcsiddescriber_H = "$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/basics/common/RCS/describe.ch,v 2.7 1991/09/12 19:21:37 bobg R6tape $";
#endif /* !defined(lint) && !defined(LOCORE) && defined(RCS_HDRS) */

#include <view.ih>

class describer[describe] {
methods:
    Describe(struct view * viewToDescribe, char * format, FILE * file, long rock) returns enum view_DescriberErrs;
};
