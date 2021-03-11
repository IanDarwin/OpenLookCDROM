/* ********************************************************************** *\
 *         Copyright IBM Corporation 1988,1989 - All Rights Reserved      *
 *        For full copyright information see:'andrew/config/COPYRITE'     *
\* ********************************************************************** */
/* $Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/value/RCS/value.h,v 2.5 1991/09/30 18:26:45 susan R6tape $ */
/* $ACIS: $ */
/* $Source: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/value/RCS/value.h,v $ */

#if !defined(lint) && !defined(LOCORE) && defined(RCS_HDRS)
static char *rcsid_h = "$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/value/RCS/value.h,v 2.5 1991/09/30 18:26:45 susan R6tape $ ";
#endif /* !defined(lint) && !defined(LOCORE) && defined(RCS_HDRS) */

/* Values for Notify Observers */

#define value_NOCHANGE		0
#define value_BADVALUE		1
#define value_NEWVALUE		2
#define	value_NOTACTIVE		3
#define value_ACTIVE		4

