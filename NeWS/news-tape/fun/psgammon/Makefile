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
# @(#)Makefile 9.10 88/10/02 SMI
#
# Copyright (c) 1988 by Sun Microsystems, Inc.
#

# directories

DESTDIR=
TARGETDIR=$(DESTDIR)/usr/local/bin
GFONTDIR = gammonfont
MAKEDEST = DESTDIR=$(DESTDIR)

# programs and libraries

CPS=/usr/NeWS/bin/cps
LIBCPS=/usr/NeWS/lib/libcps.a

# options

INCLUDE=-I/usr/NeWS/include
LIBS=${LIBCPS}
CFLAGS=${INCLUDE} ${COPTS}
COPTS=-g

# lists of source, header, and object files

SRCS = buttons.c computer.c human.c main.c score.c
HDRS = bkgcodes.h defs.h score.h
OBJ1 = buttons.o computer.o human.o main.o score.o
CPS_SRCS = board.cps init.cps interface.cps message.cps panel.cps
CPS_CS   =                    interface.c
CPS_OS	 =		      interface.o
CPS_HDRS = board.h   init.h   interface.h   message.h   panel.h
OBJS = $(OBJ1) $(CPS_OS)
ALLHDRS = ${HDRS} ${CPS_HDRS}

# dependencies

all: psgammon GFont

$(OBJS): $(ALLHDRS)

debug:	all

psgammon: ${OBJS}
	${CC} -o psgammon ${CFLAGS} ${OBJS} ${LDFLAGS} ${LIBS}

GFont: 
	cd ${GFONTDIR}; ${MAKE} ${MAKEDEST}

clean:
	rm -f ${OBJS} ${CPS_HDRS} ${CPS_CS} psgammon core *.BAK *~
	cd ${GFONTDIR}; ${MAKE} ${MAKEDEST} clean

install: all
	rm -f ${TARGETDIR}/gammon
	cp gammon ${TARGETDIR}
	cd ${GFONTDIR}; ${MAKE} ${MAKEDEST} install

init.h: init.cps ${GFONTDIR}/GammonCursors.ps

interface.c interface.h: interface.cps
	$(CPS) -i interface.cps

${GFONTDIR}/GammonCursors.ps:
	cd ${GFONTDIR}; ${MAKE} GammonCursors.ps

# implicit rules

.SUFFIXES: .h .cps .cps~
.cps~.cps:
	${GET} -G$*.cps ${GFLAGS} $<
.cps~.h:
	${GET} -G$*.cps ${GFLAGS} $<
	${CPS} $*.cps
.cps.h:
	${CPS} $*.cps
