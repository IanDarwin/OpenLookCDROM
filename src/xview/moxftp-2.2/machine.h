/*
 * Copyright (c) 1993 The Regents of the University of Texas System.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted provided
 * that the above copyright notice and this paragraph are duplicated in all
 * such forms and that any documentation, advertising materials,  and other
 * materials  related to such  distribution  and use  acknowledge  that the
 * software  was  developed  by the  University of Texas.  The  name of the
 * University may not be  used to endorse or promote  products derived from
 * this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR IMPLIED
 * WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED WARRANTIES OF
 * MERCHANTIBILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 *
 * $Id: machine.h,v 1.1 1994/03/14 18:57:41 jones Exp $
 * $Source: /mintaka/u0/xx/ss/jones/jones.old/mftp.src/mftpnew/RCS/machine.h,v $
 *
 * $Log: machine.h,v $
 * Revision 1.1  1994/03/14  18:57:41  jones
 * Initial revision
 *
 */

#include <stdio.h>
#include <string.h>
#include <X11/Xos.h>

#define INDEX strchr
#define RINDEX strrchr
extern char *bsdstrstr();

/*
 * Default to using POSIX readdir routines.
 */
#define USE_DIRENT

/*
 * Sun
 */
#if defined(SUN)||defined(sun)
#define KEEPALIVE
#define NONBLOCK(s)   fcntl(s,  F_SETFL, O_NDELAY)
#define USE_SETENV
#if defined(SUN)
#define GMTOFF
#endif
#ifndef RETSIGTYPE
#if defined(sun)&&defined(SVR4)&&!defined(__STDC__)
#define SIG_TYPE int
#else
#define SIG_TYPE void
#endif 
#endif
#if defined(sun)&&!defined(__STDC__)&&defined(SVR4)
#undef USE_DIRENT
#endif
#endif /* SUN */

/*
 * Convex.
 */
#if defined(__convex__)
#if (defined(__convex__)&&defined(convex))
#define USE_SYS_FILE
#endif
#endif

/*
 * Sgi
 */
#if defined(sgi)
#include <sys/sysmacros.h>
#define USE_SETENV
#endif

/*
 * AIX
 */
#if defined(_AIX)
#include <sys/ioctl.h>
#define NONBLOCK(s)  { int one = 1; ioctl(s, FIONBIO, &one); }
#define USE_SETENV
#endif

/*
 * HP
 */
#if defined(hpux)
#define USE_SETENV
#endif

/*
 * Get O_XXX flags.
 */
#if defined(USE_SYS_FILE)
#include  <sys/file.h>
#else
#include <fcntl.h>
#endif

/*
 * Define macros to define NONBLOCK IO
 */
#if !defined(NONBLOCK)
#if defined(O_NDEALY)
#define NONBLOCK(s)   fcntl(s,  F_SETFL, O_NDELAY)
#else
#if defined(O_NONBLOCK)
#define NONBLOCK(s)   fcntl(s,  F_SETFL, O_NONBLOCK)
#else
#define NONBLOCK(s)   fcntl(s,  F_SETFL, O_NONBLOCKING)
#endif
#endif
#endif


/*
 * Define byte copy routines.
 */
#if defined(bsd)
#define COPY(s1, s2, n) bcopy((void *)(s1), (void *)(s2), n)
#define ZERO(s1, n) bzero(s1, n)
#else
#define COPY(s1, s2, n) memcpy((void *)(s2), (void *)(s1), n)
#define ZERO(s1, n) memset(s1, 0, n)
#endif


/*
 * Define sig type.
 */
#ifndef SIG_TYPE
#ifndef RETSIGTYPE
#define SIG_TYPE void
#else
#define SIG_TYPE RETSIGTYPE
#endif
#endif
