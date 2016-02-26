# Release: Grass-2
#
# This file is a product of Sun Microsystems, Inc. and is provided for
# unrestricted use provided that this legend is included on all tape
# media and as a part of the software program in whole or part.
# Users may copy, modify or distribute this file at will.
# 
# THIS FILE IS PROVIDED AS IS WITH NO WARRANTIES OF ANY KIND INCLUDING THE
# WARRANTIES OF DESIGN, MERCHANTIBILITY AND FITNESS FOR A PARTICULAR
# PURPOSE, OR ARISING FROM A COURSE OF DEALING, USAGE OR TRADE PRACTICE.
# 
# This file is provided with no support and without any obligation on the
# part of Sun Microsystems, Inc. to assist in its use, correction,
# modification or enhancement.
# 
# SUN MICROSYSTEMS, INC. SHALL HAVE NO LIABILITY WITH RESPECT TO THE
# INFRINGEMENT OF COPYRIGHTS, TRADE SECRETS OR ANY PATENTS BY THIS FILE
# OR ANY PART THEREOF.
# 
# In no event will Sun Microsystems, Inc. be liable for any lost revenue
# or profits or other special, indirect and consequential damages, even
# if Sun has been advised of the possibility of such damages.
# 
# Sun Microsystems, Inc.
# 2550 Garcia Avenue
# Mountain View, California  94043
#
#
# ancestor: @(#)Makefile.psterm 9.9 88/01/19
# @(#)$Header: Makefile,v 2.33 91/05/14 00:04:31 hugh Exp $
#
# Copyright (c) 1987 by Sun Microsystems, Inc.
#
# Modifications to the original Sun Microsystems, Inc. source code
# made by the Grasshopper Group are in the Public Domain.
#
# Extensions to this file by Eric Messick & ||ugh Daniel
#  of the Grasshopper Group.
#
# Grasshopper Group
# 210 Clayton St
# San Francisco, CA 94117
#
# Please note that this will be the last V7 make based makefile we will
# release, all future makefiles will be based on gnumake.  Be prepared.
#

SHELL=/bin/sh

# Global macro definitions

# Set these variables for your site
# OpenWindows
NEWSHOME=/usr/openwin
PSHDIR=${INSDIR}/etc

# NeWS 1.1 users will need to uncomment these lines
#NEWSHOME=/usr/NeWS
#PSHDIR=${INSDIR}/lib

# Other variables you might want to change
INSDIR=${NEWSHOME}
INSTALLDIR=${INSDIR}/clientsrc/client/gterm
BINDIR=${INSDIR}/bin
LIBDIR=${INSDIR}/lib
INCLUDE=${INSDIR}/include
MANEXT=1
MANDIR=/usr/local/man/man${MANEXT}

BINNAME=gterm

# --- Global macro definitions ---
OPT=-O
CC=cc
OSI=REF
SYSV_NETWORK=
BYTES=
TERMFLAG=
SYSV_NETLIB=
TERMLIB=-ltermcap
PREPROC=-I. -I${INCLUDE} -I${INCLUDE}/NeWS -D${OSI} -DUTMP ${TERMFLAG} ${SYSV_NETWORK}

COPTS= ${BYTES}
CPS= ${BINDIR}/cps
LIBCPS= ${LIBDIR}/libcps.a
# For debugging its helps to use the debugging version of the library
LIBCPS_G= ${LIBDIR}/libcps_g.a

CFLAGS=${OPT} ${COPTS} ${PREPROC}

# Too many lints -- pick your own lint flags.
#LINT=/usr/5bin/lint
LOPTS=-abch
#LOPTS=
LINTOPTS=${LOPTS} ${PREPROC}

LIBS=${TERMLIB} ${LIBCPS} ${SYSV_NETLIB}

# When adding files, also modify 'make shar' below.
SRCS = 	display.c \
	selection.c \
	io.c \
	main.c \
	parsers.c \
	slave.c \
	tcap_ops.c \
	tcap_parse.c \
	bsd.c \
	sys5.c

OBJS = 	display.o \
	selection.o \
	io.o \
	main.o \
	parsers.o \
	slave.o \
	tcap_ops.o \
	tcap_parse.o \
	bsd.o \
	sys5.o

HDRS = 	screen.h \
	tcap.h \
	termcap.h

# Note: gterm?XXX.psh the ? is a Protocall level and NOT a patchlevel.
PSCODE = gtermstart.psh gterm1lte1.psh gterm1lte2.psh

# What is the size limet on a shar file?
SHAR1 =	README \
	BUGS \
	patchlevel.h \
	Makefile \
	gterm.1 \
	UsingGTerm \
	termcap.NeWS \
	terminfo.NeWS 
SHAR2 =	slave.c \
	gterm1lte2.psh
SHAR3 =	ref/config_aux \
	ref/config_sunos \
	selection.c \
	gtermstart.psh \
	gterm1lte1.psh \
	fkeys.psh \
	sys5.c
SHAR4 = tcap.cps \
	tcap_ops.c \
	tcap_parse.c \
	termcap.h 
SHAR5 = parsers.c \
	display.c \
	bsd.c \
	main.c \
	screen.h
SHAR6 = sampl.user.ps  \
	sampl.gtermrc \
	io.c


all:	gterm

psterm: gterm

aux:
	cd ref; rm -f config.h; ln -s config_aux config.h
	${MAKE} NEWSHOME=/usr/NeWS PSHDIR=/usr/NeWS/lib

sunos:
	cd ref; rm -f config.h; ln -s config_sunos config.h
	${MAKE}

debug:
	${MAKE} OPT=-g LIBCPS=${LIBCPS_G} BINNAME=gterm.g

lint:	tcap.h
	${LINT} ${LINTOPTS} ${SRCS} | grep -v _CPS

gterm:	ref/config.h ${OBJS} 
	${CC} ${CFLAGS} -o ${BINNAME} ${OBJS} ${LIBS}

gprof:
	${MAKE} BINNAME=gterm.pg OPT=-pg gterm
#	${MAKE} BINNAME=gterm.pg OPT=-pg LIBCPS=${LIBCPS_G} gterm

tcap.h: tcap.cps

ref/config.h:
	@echo "You need to create the file ref/config.h.  See the installation"
	@echo "instructions in the file README."
	@false

install: gterm ${PSCODE}
	-cd ${BINDIR} ; mv gterm RemoveMe.$$ ; rm -f RemoveMe.$$
	@echo You may have to remove ${BINDIR}/RemoveMe.\* manually on
	@echo  systems which cannot delete busy text files
	cp gterm ${BINDIR}
	-chmod 755 ${BINDIR}/gterm
	-chgrp bin ${BINDIR}/gterm
	-chown bin ${BINDIR}/gterm
	-cd ${PSHDIR} ; rm -f ${PSCODE}
	cp ${PSCODE} ${PSHDIR}
	cp termcap.NeWS ${PSHDIR}
	-cd ${PSHDIR} ; chmod 644 ${PSCODE} termcap.NeWS
	-cd ${PSHDIR} ; chgrp bin ${PSCODE} termcap.NeWS
	-cd ${PSHDIR} ; chown bin ${PSCODE} termcap.NeWS
	cp gterm.1 ${MANDIR}/gterm.${MANEXT}
	-chmod 644 ${MANDIR}/gterm.${MANEXT}

#	Used in creation of the NeWS client source tree
srcinstall:
	-mkdir ${INSTALLDIR}
	@echo '[Errors from mkdir are OK]'
	(cd ${INSTALLDIR}; rm -rf *)
	mkdir ${INSTALLDIR}/ref
	cp ${SHAR1} ${SHAR2} ${SHAR3} ${SHAR4} ${SHAR5} ${SHAR6}  ${INSTALLDIR}
	(cd ${INSTALLDIR}/ref; mv ../config_* .; ln -s config_aux config.h)

clean:
	rm -f *.o gterm gterm.pg psterm tcap tcap.h core *.BAK *~

shar:
	echo ": gterm part 1 of 6" > gterm.shar.1
	shar ${SHAR1}    >> gterm.shar.1
	echo ": gterm part 2 of 6" > gterm.shar.2
	shar ${SHAR2}    >> gterm.shar.2
	echo ": gterm part 3 of 6" > gterm.shar.3
	echo "echo ref"  >> gterm.shar.3
	echo "mkdir ref" >> gterm.shar.3
	shar ${SHAR3}    >> gterm.shar.3
	echo ": gterm part 4 of 6" > gterm.shar.4
	shar ${SHAR4}    >> gterm.shar.4
	echo ": gterm part 5 of 6" > gterm.shar.5
	shar ${SHAR5}    >> gterm.shar.5
	echo ": gterm part 6 of 6" > gterm.shar.6
	shar ${SHAR6}    >> gterm.shar.6

etags:
	etags ${SRCS}

ctags:
	ctags ${SRCS}

tags:	ctags etags

rcslabel:
	for afile in ${SHAR1} ${SHAR2} ${SHAR3} ${SHAR4} ${SHAR5} ${SHAR6}; do\
		if test -w $${afile};\
		then\
		    echo File  $${afile}  is writable and likely checked out;\
		    echo All files MUST be checked in before a labeling;\
		    exit 121;\
		fi;\
	done
	for afile in ${SHAR1} ${SHAR2} ${SHAR3} ${SHAR4} ${SHAR5} ${SHAR6}; do\
	   co -l $${afile};\
	   ci -f -u -q -n${LABEL} -s$${STATE:-${LABEL}} $${afile} < /dev/null;\
	done

depend: ${HDRS} ${SRCS}
	${INSDIR}/clientsrc/makedepend ". -I${INCLUDE}"

.SUFFIXES: .h .cps
.cps.h:
	${CPS} $*.cps

# START OF AUTOMATICALLY GENERATED DEPENDENCIES 
bsd.o: bsd.c
display.o: display.c screen.h tcap.h
io.o: io.c
main.o: main.c
parsers.o: termcap.h
selection.o: screen.h selection.c tcap.h
slave.o: slave.c
sys5.o: sys5.c
tcap_ops.o: screen.h tcap.h tcap_ops.c termcap.h
tcap_parse.o: tcap_parse.c termcap.h
