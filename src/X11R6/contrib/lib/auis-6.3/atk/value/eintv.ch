/* ********************************************************************** *\
 *         Copyright IBM Corporation 1988,1989 - All Rights Reserved      *
 *        For full copyright information see:'andrew/config/COPYRITE'     *
\* ********************************************************************** */
/* $Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/value/RCS/eintv.ch,v 1.1 1993/08/20 20:05:59 susan Exp $ */
/* $ACIS: $ */
/* $Source: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/value/RCS/eintv.ch,v $ */

#if !defined(lint) && !defined(LOCORE) && defined(RCS_HDRS)
static char *rcsid_eintview_H = "$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/value/RCS/eintv.ch,v 1.1 1993/08/20 20:05:59 susan Exp $ ";
#endif /* !defined(lint) && !defined(LOCORE) && defined(RCS_HDRS) */
class eintview[eintv]:textview[textv] {
overrides :
    ReceiveInputFocus();
    LoseInputFocus();
macromethods:
    SetClearOnRIF(val) self->ClearOnRIF = val
    SetResetOnLIF(val) self->ResetOnLIF = val
    SetValueview(val) self->valueview = val
classprocedures:
    InitializeObject(struct eintview *self) returns boolean;
    InitializeClass(struct eintview *self) returns boolean;
data:
    struct keystate *keystate;
    boolean ClearOnRIF,ResetOnLIF;
    struct valueview *valueview;
};
