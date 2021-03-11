/* ********************************************************************** *\
 *         Copyright IBM Corporation 1988,1991 - All Rights Reserved      *
 *        For full copyright information see:'andrew/config/COPYRITE'     *
\* ********************************************************************** */
/* $Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/preview/RCS/previewa.ch,v 2.7 1991/09/12 19:45:53 bobg R6tape $ */
/* $ACIS:previewa.ch 1.2$ */
/* $Source: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/preview/RCS/previewa.ch,v $ */

#if !defined(lint) && !defined(LOCORE) && defined(RCS_HDRS)
static char *rcsidpreviewapp_H = "$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/preview/RCS/previewa.ch,v 2.7 1991/09/12 19:45:53 bobg R6tape $";
#endif /* !defined(lint) && !defined(LOCORE) && defined(RCS_HDRS) */

class previewapp[previewa] : application[app] {
    classprocedures:
      InitializeObject(struct ezprintapp *self) returns boolean;
    overrides:
	ParseArgs(int argc,char **argv) returns boolean;
        Start() returns boolean;
	Run() returns int;
};
