/* ********************************************************************** *\
 *         Copyright IBM Corporation 1988,1991 - All Rights Reserved      *
 *        For full copyright information see:'andrew/config/COPYRITE'     *
\* ********************************************************************** */

/* 	ckndbm.h
	Includes <ndbm.h> and defines tst_NDBM if it believes that ndbm.h is available.
*/

#include <system.h>
/* The test was pushed onto the system.h file. */
#ifdef NDBM_ENV
#define	tst_NDBM	1
#include	<ndbm.h>
#endif /* #ifdef NDBM_ENV */
