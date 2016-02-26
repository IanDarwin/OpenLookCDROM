/*  -------------------------------------------------------------------

This program is the property of:

                             Cornell University 
                        Center for Applied Mathematics 
                              Ithaca, NY 14853

and may be used, modified and distributed freely, subject to the 
following restrictions:

       Any product which incorporates source code from the dstool
       program or utilities, in whole or in part, is distributed
       with a copy of that source code, including this notice. You
       must give the recipients all the rights that you have with
       respect to the use of this software. Modifications of the
       software must carry prominent notices stating who changed
       the files and the date of any change.

DsTool is distributed in the hope that it will be useful, but 
WITHOUT ANY WARRANTY; without even the implied warranty of FITNESS 
FOR A PARTICULAR PURPOSE.  The software is provided as is without 
any obligation on the part of Cornell faculty, staff or students to 
assist in its use, correction, modification or enhancement.

  -----------------------------------------------------------------  */

#ifndef PORTABILITY_HEADER
#define PORTABILITY_HEADER

#ifdef SUNOS5

#define SYSV_TYPE

#endif


#ifdef IRIX5

#define SYSV_TYPE 
#define HAS_NO_TIMEB_H

#endif


#ifdef SUNOS4

#define BSD_TYPE
#define HAS_STRINGS_H


#endif

#ifdef LINUX
#define HAS_STRCASECMP
#define HAS_STRING_H
#define HAS_STRRCHR
#define HAS_STRSTR
#define HAS_STRCHR
#endif

#ifdef SYSV_TYPE

#define HAS_STRING_H
#define HAS_DIRENT_H
#define HAS_STRRCHR
#define HAS_STRSTR
#define HAS_STRCHR


#endif

#ifdef BSD_TYPE

#define HAS_STRING_H
#define HAS_INDEX
#define HAS_RINDEX

#endif

#if defined(USING_GCC) && !defined(SUNOS4)
#define HAS_STRCASECMP
#endif


#endif
