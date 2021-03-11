/* ********************************************************************** *\
 *         Copyright IBM Corporation 1988,1991 - All Rights Reserved      *
 *        For full copyright information see:'andrew/config/COPYRITE'     *
\* ********************************************************************** */

/* 
 * Control inclusion of debugging code in object code
 */

/* $Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/ams/msclients/vui/RCS/vuidebug.h,v 2.4 1991/09/12 19:17:34 bobg R6tape $ */

#ifndef DEBUG
#define debug(xxx_foo)
#endif /* DEBUG */
#ifdef DEBUG
#define debug(xxx_foo) debugrtn xxx_foo
#endif /* DEBUG */
