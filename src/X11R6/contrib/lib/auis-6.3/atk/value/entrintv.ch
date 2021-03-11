/* ********************************************************************** *\
 *         Copyright IBM Corporation 1988,1989 - All Rights Reserved      *
 *        For full copyright information see:'andrew/config/COPYRITE'     *
\* ********************************************************************** */
/* $Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/value/RCS/entrintv.ch,v 1.1 1993/08/20 20:05:27 susan Exp $ */
/* $ACIS: $ */
/* $Source: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/value/RCS/entrintv.ch,v $ */

#if !defined(lint) && !defined(LOCORE) && defined(RCS_HDRS)
static char *rcsid_enterintV_H = "$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/value/RCS/entrintv.ch,v 1.1 1993/08/20 20:05:27 susan Exp $ ";
#endif /* !defined(lint) && !defined(LOCORE) && defined(RCS_HDRS) */
class enterintV[entrintv] : buttonV[buttonv] {
overrides:
   DoHit(enum view_MouseAction type, long x,y,hits) returns struct buttonV *;
   ObservedChanged( struct observable * observed, long status );
   LinkTree(struct view *parent);
  DrawButtonText(char *text,long len,struct rectangle *rect,struct rectangle*rect2,pushd);
Hit(enum view_MouseAction type, long x, long y, long numberOfClicks)
    returns struct view *;

classprocedures:
   InitializeObject(struct enterintV *self) returns boolean;
   FinalizeObject(struct enterintV *self) returns boolean;
data:
    struct enterint *eint;
    struct eintview *eintview;
};

