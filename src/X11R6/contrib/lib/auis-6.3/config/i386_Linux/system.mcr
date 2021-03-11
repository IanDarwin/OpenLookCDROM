/* Copyright IBM Corporation 1988,1991 - All Rights Reserved */
/* For full copyright information see:'andrew/config/COPYRITE' */

#define In_Imake 1
/* The next two lines need to be kept in sync */
#include <i386_Linux/system.h>
        SYSTEM_H_FILE = i386_Linux/system.h
#undef In_Imake

/* These next three lines help configure the embedded machine-dependent
    directories overhead/class/machdep, atk/console/stats, and
    atk/console/stats/common. */
        SYS_IDENT = i386_Linux
        SYS_OS_ARCH = i386_Linux

/* Get parent inclusions */
#include <allsys.mcr>

STD_DEFINES = -DSYSV
MFLAGS = -Sr
COMPILERFLAGS=-m486

/* Now for the system-dependent information. */
CC = gcc
GNULIB = /usr/lib/libgcc.a
XUTILDIR = /usr/bin/X11
XLIBDIR = /usr/lib
XLIB = -L$(XLIBDIR) -lX11
LEX = flex

/* Get site-specific inclusions */
#include <site.mcr>
SYS_CONFDIR = i386_Linux
