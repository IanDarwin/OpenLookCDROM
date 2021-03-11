/* ********************************************************************** *\
 *         Copyright IBM Corporation 1988,1991 - All Rights Reserved      *
 *        For full copyright information see:'andrew/config/COPYRITE'     *
\* ********************************************************************** */
/* $Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/basics/common/RCS/sizes.h,v 2.4 1991/09/12 19:23:37 bobg R6tape $ */
/* $ACIS:sizes.h 1.2$ */
/* $Source: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/basics/common/RCS/sizes.h,v $ */

#if !defined(lint) && !defined(LOCORE) && defined(RCS_HDRS)
static char *rcsidsizes = "$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/basics/common/RCS/sizes.h,v 2.4 1991/09/12 19:23:37 bobg R6tape $";
#endif /* !defined(lint) && !defined(LOCORE) && defined(RCS_HDRS) */

#ifndef TransportableSizesDefined
#define TransportableSizesDefined
typedef	long int16;
typedef	long int32;
typedef	short int8;
#endif /* TransportableSizesDefined */
