/* ********************************************************************** *\
 *         Copyright IBM Corporation 1988,1991 - All Rights Reserved      *
 *        For full copyright information see:'andrew/config/COPYRITE'     *
\* ********************************************************************** */
/* $Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/text/RCS/bp.c,v 1.4 1993/10/26 22:43:58 gk5g Exp $ */
/* $ACIS:$ */
/* $Source: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/text/RCS/bp.c,v $ */

#ifndef lint
static char *rcsid = "$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/text/RCS/bp.c,v 1.4 1993/10/26 22:43:58 gk5g Exp $";
#endif /* lint */

#include <andrewos.h>
#include <class.h>
#include <bp.eh>

char *bp__ViewName(self)
struct bp *self;
{
    return "bpv";
}
