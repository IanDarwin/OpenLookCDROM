/* Copyright TELMAT Informatique 1992,1993 - All Rights Reserved */
/* For full copyright information see:'andrew/config/COPYRITE' */

#define In_Imake 1
/* The next two lines need to be kept in sync */
#include <telmat_svr4/system.h>
#undef In_Imake

SYSTEM_H_FILE=telmat_svr4/system.h

/* These next three lines help configure the embedded machine-dependent
    directories overhead/class/machdep, atk/console/stats, and
    atk/console/stats/common. */
SYS_IDENT=telmat_svr4   /*directory for atk/console/stats/${SYS_IDENT} */
SYS_OS_ARCH=telmat_svr4 /*directory for overhead/class/machdep/${SYS_OS_ARCH} */

/* Get parent inclusions */
#include <allsys.mcr>


CRT0PATH = /usr/ccs/lib/crt0.o
CLIB = /usr/ccs/lib/libc.a
SHELL = /usr/bin/ksh
STD_DEFINES = -Dsys_telmat -DGROFF_ENV -DSYSV -DUSG
NETLIBS = -lsocket -lnsl -lelf
PICFLAG = -KPIC
SHLIBLDFLAGS = -G
DYN_LINK_LIB = -ldl

/* Get site-specific inclusions */
#include <site.mcr>



SYS_CONFDIR = telmat_svr4
