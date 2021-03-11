/* ********************************************************************** *\
 *         Copyright IBM Corporation 1988,1991 - All Rights Reserved      *
 *        For full copyright information see:'andrew/config/COPYRITE'     *
\* ********************************************************************** */
/* $Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/ezprint/RCS/ezprinta.ch,v 2.7 1991/09/12 19:39:24 bobg R6tape $ */
/* $ACIS:ezprinta.ch 1.2$ */
/* $Source: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/ezprint/RCS/ezprinta.ch,v $ */

#if !defined(lint) && !defined(LOCORE) && defined(RCS_HDRS)
static char *rcsidezprintapp_H = "$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/ezprint/RCS/ezprinta.ch,v 2.7 1991/09/12 19:39:24 bobg R6tape $";
#endif /* !defined(lint) && !defined(LOCORE) && defined(RCS_HDRS) */

class ezprintapp[ezprinta] : application[app] {
    classprocedures:
      InitializeObject(struct ezprintapp *self) returns boolean;
    overrides:
	ParseArgs(int argc,char **argv) returns boolean;
	Run() returns int;
        ReadInitFile();
};
