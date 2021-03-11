/*
Copyright (c) 1991 Bell Communications Research, Inc. (Bellcore)

Permission to use, copy, modify, and distribute this material 
for any purpose and without fee is hereby granted, provided 
that the above copyright notice and this permission notice 
appear in all copies, and that the name of Bellcore not be 
used in advertising or publicity pertaining to this 
material without the specific, prior written permission 
of an authorized representative of Bellcore.  BELLCORE 
MAKES NO REPRESENTATIONS ABOUT THE ACCURACY OR SUITABILITY 
OF THIS MATERIAL FOR ANY PURPOSE.  IT IS PROVIDED "AS IS", 
WITHOUT ANY EXPRESS OR IMPLIED WARRANTIES.
*/

/* This is the top-level configuration file for the metamail distribution. */
/* If your compiler does not automatically do so, you may wish to 
      add a #define here that defines your system type, e.g.
      #define LINUX
      #define SYSV 
      #define MSDOS
      #define AMIGA
*/

#ifdef __svr4__
#ifndef SYSV
/* Stupid Solaris 2.0 machines define __svr4__ but not SYSV */
#define SYSV
#endif
#endif

#ifdef LINUX
#define SYSV /* Linux is SysV */
#endif

#ifdef SVR3
#ifndef SYSV
/* Stupid SGI machines define SVR3 but not SYSV */
#define SYSV
#endif
#endif

#ifdef __MSDOS__	/* Defined if using Borland C compiler? */
#ifndef BORLAND
#define BORLAND		/* Make sure BORLAND gets defined */
#endif
#ifndef MSDOS
#define MSDOS   /* A common symbol we can use for all DOS compilers */
#endif
#endif

#ifdef _MSC_VER		/* Using Microsoft C compiler */
#ifndef MICROSOFT
#define MICROSOFT	/* Make sure MICROSOFT gets defined */
#endif
#ifndef MSDOS
#define MSDOS   /* A common symbol we can use for all DOS compilers */
#endif
#endif

/* NOTE:  The RESET_PROGRAM resets the terminal to a "normal" state 
   If you comment out the definition, all will be well except that metamail's
   -R switch won't work, and metamail-called programs might be more likely
   to screw up your terminal state */

#ifdef SYSV
#define RESET_PROGRAM "tput clear"
#else
#ifdef __BSD_4_4__
#define RESET_PROGRAM "/usr/bin/reset"
#else
#define RESET_PROGRAM "/usr/ucb/reset"
#endif
#endif

#ifdef __hpux
/* Basically SYSV */
#define SYSV
#define NO_RLIMITS 1
/* I've gotten conflicting reports about the best way to reset the terminal 
  state under hpux.  I'm now using "/usr/bin/reset" as the default, but there 
  have been two other suggestions as well.  Your mileage may vary! -- NSB */
#undef RESET_PROGRAM
#define RESET_PROGRAM "/usr/bin/reset"
/* #define RESET_PROGRAM "tput clear" */
/* #define RESET_PROGRAM "/bin/reset" */
#endif

#ifdef SYSV
#ifndef sgi
#define killpg(a, b) kill(-(a), (b))
#endif
#define bcopy(a, b, c) memcpy(b, a, c)
#define bzero(a, b) memset(a, 0, b)
#define bcmp memcmp
#define index strchr
#define rindex strrchr
#define initstate srand
#define random rand
#define NO_RLIMITS 1
#define sigtype void
#endif

/* This constant should define the ASCII code for newlines on systems where 
   the newline convention is other than CRLF.  On UNIX, it is ^J, ASCII 10. 
    Here we define it as '\n' which should be right on MOST systems... */
#define NEWLINE_CHAR '\n'

#ifdef MSDOS
#undef NEWLINE_CHAR /* DOS uses CRLF */
#undef RESET_PROGRAM
#include <string.h>
#define index  strchr
#define rindex strrchr
#define popen  fopen
#define pclose fclose
#define NO_RLIMITS 1
#endif

#ifdef AMIGA
#undef RESET_PROGRAM
#define index  strchr
#define rindex strrchr
#define NO_RLIMITS 1
#define DEFAULT_SPLIT_SIZE 95000
#endif

/* The following defines the default size at which long
    messages will be split into multiple messages of type
    "message/partial" by the mailto and splitmail commands,
    at least. */
#ifndef DEFAULT_SPLIT_SIZE
#define DEFAULT_SPLIT_SIZE 250000
#endif

#ifndef sigtype
#define sigtype int
#endif

#ifdef MSDOS
#define PATH_SEPARATOR ';'
#ifndef STDPATH
#define STDPATH ".\\mailcap;\\mailcap"
#endif
#else
#ifdef AMIGA
#define PATH_SEPARATOR ' '
#ifndef STDPATH
#define STDPATH "uulib:mailcap"
#endif
#else
#define PATH_SEPARATOR ':'
#ifndef STDPATH
#define STDPATH "/usr/local/etc/mailcap:/usr/etc/mailcap:/etc/mailcap:/etc/mail/mailcap:/usr/public/lib/mailcap"
#endif
#endif
#endif

/* The following can be set to a directory or colon-separated list of 
  directories that will be prepended to the user's search path before 
  executing any mailcap-derived commands. 

  It should be set to NULL if there are no directories to prepend.  
*/

#define AUXPATH NULL
