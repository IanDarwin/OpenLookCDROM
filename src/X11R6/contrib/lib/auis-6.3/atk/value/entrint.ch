/* ********************************************************************** *\
 *         Copyright IBM Corporation 1988,1989 - All Rights Reserved      *
 *        For full copyright information see:'andrew/config/COPYRITE'     *
\* ********************************************************************** */
/* $Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/value/RCS/entrint.ch,v 1.1 1993/08/20 20:05:27 susan Exp $ */
/* $ACIS: $ */
/* $Source: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/value/RCS/entrint.ch,v $ */

#if !defined(lint) && !defined(LOCORE) && defined(RCS_HDRS)
static char *rcsid_enterint_H = "$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/value/RCS/entrint.ch,v 1.1 1993/08/20 20:05:27 susan Exp $ ";
#endif /* !defined(lint) && !defined(LOCORE) && defined(RCS_HDRS) */
#define enterint_BUFCHANGEDFLAG 10101
class enterint[entrint] : text {
overrides:
    InsertCharacters(long pos, char *str, long len) returns boolean;
    DeleteCharacters(long pos, long len) returns boolean;
    ViewName() returns char *;
methods:
    updatebuf();
    Changed()returns boolean;
    SetChars(char *str,int len);
macromethods:
    buflen() (self->buflen)
    GetString() (self->buf)
    GetInt() (atoi(self->buf))
classprocedures:
    InitializeObject(struct enterint *self)returns boolean;
    FinalizeObject(struct enterint *self)returns boolean;
data:
    char *buf;
    long buflen,mod,realbuflen;
    struct style *Style;
    boolean needswrap;
};
