/* Copyright IBM Corporation 1988,1991 - All Rights Reserved */

/*
	$Disclaimer: 
Permission to use, copy, modify, and distribute this software and its 
documentation for any purpose is hereby granted without fee, 
provided that the above copyright notice appear in all copies and that 
both that copyright notice, this permission notice, and the following 
disclaimer appear in supporting documentation, and that the names of 
IBM, Carnegie Mellon University, and other copyright holders, not be 
used in advertising or publicity pertaining to distribution of the software 
without specific, written prior permission.

IBM, CARNEGIE MELLON UNIVERSITY, AND THE OTHER COPYRIGHT HOLDERS 
DISCLAIM ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING 
ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS.  IN NO EVENT 
SHALL IBM, CARNEGIE MELLON UNIVERSITY, OR ANY OTHER COPYRIGHT HOLDER 
BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY 
DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, 
WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS 
ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE 
OF THIS SOFTWARE.
 $
*/

/* For full copyright information see:'andrew/config/COPYRITE' */

XCOMM
XCOMM The following is from the allsys.mcr file, and may be over-ridden
XCOMM by the platform-specific system.mcr file or the
XCOMM site-specific site.mcr file.
XCOMM

/* System-independent macros: included at the head of system.mcr. */

/* GCCLIB is used at the end of invocations of makedo to pick up calls gcc generates to it's own library. GNULIB is used on some platforms to extract functions from libgcc.a and place them in libcx.a.  Normally GNULIB should be set in the site.mcr file, on some platforms it may be necessary to explicitly set GCCLIB to be empty. */
GCCLIB = $(GNULIB)

/* XBASEDIR should point to the top of an X11 installation */
/* if you haven't installed X11, you can carefully set everything below */
/* that uses XBASEDIR to point to the right places */
        XBASEDIR = 
/* XSRCDIR should be an absolute path to the top of an X11 source tree */
/* it is only used for getting imake and makedepend (and fc under X11R2) */
/* if you have imake and makedepend installed somwhere, you can leave */
/* this blank and take care of imake and makedepend (and fc) below */
        XSRCDIR = 

#ifdef AFS_ENV
/* AFSBASEDIR should point to the top of an AFS installation. */
/* Thus, $(AFSBASEDIR)/lib/afs $(AFSBASEDIR)/include/afs should both exist. */
/* if you need to change this setting, you can do so in site.h . */
        AFSBASEDIR = /usr/local
#endif /* AFS_ENV */

/* If your AFS protection server relies on the MIT Athena Kerberos */
/* library, set KRBLIB to name where the Kerberos library lives. */
/* You will also need to define this variable if you make the MIT neos */
/* application */
        KRBLIB = 

/* set DESTDIR to where you want Andrew installed */
/* it is necessary that it be installed incrementally, while it is built */
/* DEFAULT_ANDREWDIR_ENV is set in the allsys.h file to the location where */
/* DESTDIR will be visible by users after installation.  If this is different */
/* from the location where it will be installed incrementally at build time, */
/* set BASEDIR to that latter location by redefining it in your site.h file. */
        BASEDIR = DEFAULT_ANDREWDIR_ENV
        DESTDIR = ${BASEDIR}

/* XBINDIR is for programs installed in the X bin directory*/

        XBINDIR = /usr/bin/X11

/* XUTILDIR is for programs that have been installed in some place other */
/* than the standard location - the ITC uses it for 'makedepend' and 'imake' */
        XUTILDIR = /usr/bin/X11

/* CDEBUGFLAGS are passed to C compilations.  */
/* To generate debugger symbol tables, use -g instead of -O.  */
        CDEBUGFLAGS = -O

/* MAKEDODEBUG determines whether .dog files are generated from makedo. */
/* Set it to -g to generate them.  You must set CDEBUGFLAGS to -g as well. */
        MAKEDODEBUG = 

        SHELL = /bin/sh
        CSHELL = /bin/csh
        CC = cc
/* Malloc will generate addresses divisible by MALLOCALIGNMENT,  */
/* which must be a multiple of 4 */
/* MALLOCALIGNMENT is only used if the Andrew malloc package is */
/* used, i.e. - ANDREW_MALLOC_ENV is defined. */
        MALLOCALIGNMENT = 4

/* Specify the default standard C library. */
#ifdef CMUCS
        CRT0PATH = /usr/cs/lib/crt0.o
        CLIB = /usr/cs/lib/libc.a
#else /* CMUCS */
        CRT0PATH = /lib/crt0.o
        CLIB = /lib/libc.a
#endif /* CMUCS */

/* Libraries that are used for curses apps, like vui */
        CURSESLIBS = -lcurses  -ltermcap

/* BEGINNING of macros included in LDFLAGS */

/* the resolver lib is usually found in the C lib, but it can also
*  be broken out into it's own lib.  On the sun_sparc_41, we
* fly our own shared resolver library and override this value.
*/
        RESOLVLIB =

/* the following variable is for any libraries
 * needed for networking support
 */
        NETLIBS =

/* this is to catch machine-specific libraries that contain
 * fixes for various functions
 */
        SUPLIBS =

/* this is the ATK class library;
*  this may need to specified as a relative reference if your
*  system supports shared libraries.
*/
        CLASSLIB = $(BASEDIR)/lib/libclass.a

/* this is the BSD library.  It's specified in individual Imakefiles that actually 
* need Berkeley sockets or other BSD networking code.
*/
        BSDLIB = 

/* this is the X library libX11.a.  On some platforms, to get the shared
 * version you would specify XLIB relatively:
 *	-L$(XLIBDIR) -lX11
 */
        XLIB = $(XLIBDIR)/libX11.a

/* If you have AT&T SYSV shared library support library 
    set this to something like -ldl */
        DYN_LINK_LIB =

/* The Lex library macro.  System that define FLEX_ENV now: i386_Linux, i386_BSD */
#ifdef FLEX_ENV
LEXLIB = -lfl
#else
LEXLIB = -ll
#endif

/* this macro is for specifying libraries that are generally needed to link
*  executables on you system.  For instance, alternate C libraries or 
*  compatibility libs.
*/
        SYS_LIBRARIES =

/* END of macros included in LDFLAGS */


/* uncomment this if your make program has MAKEFLAGS but not MFLAGS */
/* #define ConstructMFLAGS */

        ADDALIASES = $(BASEDIR)/etc/addalias
        CLASS = ${BASEDIR}/bin/class
        REGISTER = $(BASEDIR)/bin/cregister
        MAKEDO = ${BASEDIR}/bin/makedo
        MAKEDOFLAGS = ${MAKEDODEBUG} -d ${BASEDIR}/lib -b ${BASEDIR}/bin
        DOINDEX = $(BASEDIR)/bin/doindex
        XINCDIR = $(XBASEDIR)/usr/include
#ifdef AFS_ENV
        INCLUDES =  -I${BASEDIR}/include/atk -I${BASEDIR}/include $(BSDINCLUDES) -I$(AFSBASEDIR)/include -I${XINCDIR}
#else /* AFS_ENV */
        INCLUDES =  -I${BASEDIR}/include/atk -I${BASEDIR}/include $(BSDINCLUDES) -I${XINCDIR}
#endif /* AFS_ENV */
        CLASSINCLUDES = $(LOCALINCLUDES) $(INCLUDES)
        DEPENDSCRIPT = $(TOP)/config/depend.csh
        XMAKEDEPEND = $(XUTILDIR)/makedepend
        IMAKE = $(XUTILDIR)/imake
        XLIBDIR = $(XBASEDIR)/usr/lib
        OLDXLIB = $(XLIBDIR)/liboldX.a
        IRULESRC = $(TOP)/config
        FDBWM = $(BASEDIR)/bin/fdbwm
        FDBBDF = $(BASEDIR)/bin/fdbbdf
#ifndef FONTS_TO_PCF_ENV
        XFC = $(XBASEDIR)/$(XBINDIR)/bdftosnf
#else /* FONTS_TO_PCF_ENV */
        XFC = $(XBASEDIR)/$(XBINDIR)/bdftopcf
#endif /* FONTS_TO_PCF_ENV */
        XMKFONTDIR = $(XBASEDIR)/$(XBINDIR)/mkfontdir
        MACH = and

        TOP = TOPDIR
        AS = as
        CPP = /lib/cpp
        LD = ld
        LINT = lint
#ifdef BUILDANDREWINSTALL_ENV
        INSTALL = $(BASEDIR)/etc/install
#else /* BUILDANDREWINSTALL_ENV */
        INSTALL = install
#endif /* BUILDANDREWINSTALL_ENV */
        TAGS = ctags
        RM = rm -f
        MV = mv
        CP = cp
        LN = ln -s
        RANLIB = ranlib
        AR = ar clq
        ARDEL = ar d
        CHMODW = chmod +w
        LS = ls
        AWK = awk
        SORT = sort
        TR = tr
        NM = nm
        MAKE = make
        SED = sed
        LEX = lex
        YACC = yacc
        SCRIBE = scribe
        LINTOPTS = -axz
        LINTLIBFLAG = -C
        STD_DEFINES =

XCOMM This MATHLIB macro is a workaround for a bug in HPUX8.0 ld.
XCOMM That loader has problems linking normal archive libraries
XCOMM into a shared library.  [console/cmd, ness/objects]
XCOMM This macro is made empty in the appropriate hp system.mcr
XCOMM files.
        MATHLIB = -lm


/* If there is already a JPEG distribution at your site, #undefine MK_JPEG and set these two macros to point to the JPEG library and include files */

        JPEGLIB = ${BASEDIR}/lib/libjpeg.a
XCOMM        JPEGINCLUDES = ${BASEDIR}/include

/* If there is already a TIFF distribution at your site, #undefine MK_TIFF and set these two macros to point to the TIFF library and include files */

        TIFFLIB = ${BASEDIR}/lib/libtiff.a
XCOMM        TIFFINCLUDES = -I${BASEDIR}/include

/* The directories in $ANDREWDIR where different kinds of fonts are kept. */
FONTDESTX = X11fonts
FONTDESTOPENWIN = Xnewsfonts
FONTDESTWM = fonts

#ifdef mips
ASMPP_CC = $(CC) -E
AS_FLAGS = -nocpp
#else		/* mips */
#ifdef SCOunix
ASMPP_CC = $(CPP)
AS_FLAGS = 
#else		/* SCOunix */
#ifdef GNU_ENV
ASMPP_CC = $(CPP)
AS_FLAGS =
#else		/* GNU_ENV */ /* DEFAULT */
ASMPP_CC = $(CC) -E
AS_FLAGS =
#endif		/* GNU_ENV */
#endif		/* SCOunix */
#endif		/* mips */

XCOMM
XCOMM End of what comes from the allsys.mcr file.
XCOMM
