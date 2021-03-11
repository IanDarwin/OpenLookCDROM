/* ********************************************************************** *\
 *         Copyright IBM Corporation 1988,1991 - All Rights Reserved      *
 *        For full copyright information see:'andrew/config/COPYRITE'     *
\* ********************************************************************** */
static char *getascft_rcsid = "$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/ams/libs/ms/RCS/getascft.c,v 2.4 1991/09/12 15:43:38 bobg R6tape $";

/*
$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/ams/libs/ms/RCS/getascft.c,v 2.4 1991/09/12 15:43:38 bobg R6tape $
$Source: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/ams/libs/ms/RCS/getascft.c,v $
*/
#include <ms.h>

MS_GetAssociatedFileTime(FullName, fdate)
char *FullName;
long *fdate;
{
    debug(1, ("MS_GetAssociatedFileTime %s\n", FullName));
    return(GetAssocFileTime(FullName, fdate));
}
