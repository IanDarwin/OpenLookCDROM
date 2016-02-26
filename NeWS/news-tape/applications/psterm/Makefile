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
# @(#)Makefile.psterm 9.9 88/01/19
# @(#)$Header: /u/eric/client/psterm/RCS/Makefile,v 2.7 88/11/15 21:18:04 eric Update $
#
# Copyright (c) 1987 by Sun Microsystems, Inc.
#
# Modifications to the original Sun Microsystems, Inc. source code
# made by the Grasshopper Group are in the Public Domain.
#
# Extensions to this file by Eric Messick of the Grasshopper Group.
#
# Grasshopper Group
# 212 Clayton St
# San Francisco, CA 94117
#

SHELL=/bin/sh

# Global macro definitions
NEWSHOME=/usr/NeWS
INSDIR=${NEWSHOME}
BINDIR=${INSDIR}/bin
INCLUDE=${INSDIR}/include
LIBDIR=${INSDIR}/lib
MANDIR=/usr/man/manl
MANEXT=l

# --- Global macro definitions ---
OPT=-O
TARGET_ARCH=
OSI=REF
SYSV_NETWORK=
BYTES=
TERMFLAG=
SYSV_NETLIB=
TERMLIB=-ltermcap
PREPROC=-I. -I${INCLUDE} -D${OSI} -DUTMP ${TERMFLAG} ${SYSV_NETWORK}

COPTS= ${BYTES}
CPS= ${BINDIR}/cps
LIBCPS= ${LIBDIR}/libcps.a

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
	slave.c \
	tcap_ops.c \
	tcap_parse.c \
	bsd.c \
	sys5.c

OBJS = 	display.o \
	selection.o \
	io.o \
	main.o \
	slave.o \
	tcap_ops.o \
	tcap_parse.o \
	bsd.o \
	sys5.o

HDRS = 	screen.h \
	tcap.h \
	termcap.h

SHAR1 =	README \
	BUGS \
	patchlevel.h \
	Makefile \
	psterm.1 \
	UsingPSTerm \
	bsd.c \
	screen.h
SHAR2 =	sampl.pstermrc \
	sampl.user.ps \
	psterm.tcap \
	psterm.tinfo \
	slave.c \
	io.c \
	main.c \
	display.c
SHAR3 =	ref/config_aux \
	ref/config_sunos \
	psterm_1.ps \
	selection.c \
	sys5.c
SHAR4 = tcap.cps \
	tcap_ops.c \
	tcap_parse.c \
	termcap.h

PSCODE = psterm_1.ps

all:	psterm

lint:	tcap.h
	${LINT} ${LINTOPTS} ${SRCS} | grep -v _CPS

psterm:	${OBJS}
	${CC} ${CFLAGS} -o psterm ${OBJS} ${LIBS}

tcap.h: tcap.cps

install: psterm ${PSCODE}
	cd ${LIBDIR} ; rm -f ${PSCODE}
	cd ${BINDIR} ; mv psterm RemoveMe.$$$$ ; rm -f RemoveMe.$$$$
	@echo You may have to remove ${BINDIR}/RemoveMe.\* manually on
	@echo  systems which cannot delete busy text files
	cp psterm ${BINDIR}
	-chown bin ${BINDIR}/psterm
	-chgrp bin ${BINDIR}/psterm
	chmod 755 ${BINDIR}/psterm
	cp ${PSCODE} ${LIBDIR}
	-cd ${LIBDIR} ; chown bin ${PSCODE}
	-cd ${LIBDIR} ; chgrp bin ${PSCODE}
	cd ${LIBDIR} ; chmod 644 ${PSCODE}
	cp psterm.1 ${MANDIR}/psterm.${MANEXT}
	chmod 644 ${MANDIR}/psterm.${MANEXT}

clean:
	rm -f *.o psterm tcap tcap.h *.BAK *~ core

shar:
	echo ": psterm part 1 of 4" > psterm.shar.1
	shar ${SHAR1}    >> psterm.shar.1
	echo ": psterm part 2 of 4" > psterm.shar.2
	shar ${SHAR2}    >> psterm.shar.2
	echo ": psterm part 3 of 4" > psterm.shar.3
	echo "echo ref"  >> psterm.shar.3
	echo "mkdir ref" >> psterm.shar.3
	shar ${SHAR3}    >> psterm.shar.3
	echo ": psterm part 4 of 4" > psterm.shar.4
	shar ${SHAR4}    >> psterm.shar.4

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
selection.o: screen.h selection.c tcap.h
slave.o: slave.c
sys5.o: sys5.c
tcap_ops.o: screen.h tcap.h tcap_ops.c termcap.h
tcap_parse.o: tcap_parse.c termcap.h
