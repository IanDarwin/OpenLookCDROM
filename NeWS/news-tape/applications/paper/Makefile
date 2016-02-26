# 
# THIS FILE IS PROVIDED AS IS WITH NO WARRANTIES OF ANY KIND INCLUDING THE
# WARRANTIES OF DESIGN, MERCHANTIBILITY AND FITNESS FOR A PARTICULAR
# PURPOSE, OR ARISING FROM A COURSE OF DEALING, USAGE OR TRADE PRACTICE.
# 
# This file by Eric Messick of the Grasshopper Group.
#
# This file is in the Public Domain
#
# Grasshopper Group
# 212 Clayton St
# San Francisco, CA 94117
#
# @(#)$Header: Makefile,v 1.4 88/09/14 15:16:31 eric Exp $
#

SHELL=/bin/sh

# Global macro definitions
INSDIR=/usr/NeWS
BINDIR=${INSDIR}/bin
LIBDIR=${INSDIR}/lib
INCLUDE=${INSDIR}/include

OPT=-O
OSI=REF
VER=SUNREF
SYSV_NETWORK=
BYTES=
COPTS=${BYTES}

CFLAGS=${OPT} ${COPTS} -I${INCLUDE} -I. -D${OSI} -D${VER} ${SYSV_NETWORK}
LDFLAGS=${LIBDIR}/libcps.a -lm

all: paper

clean:
	rm -f paper.o paper.h paper core *.BAK *~ 

install: paper
	cp paper ${BINDIR}
	cp paper.ps ${LIBDIR}

paper.h: paper.cps

paper: paper.o
	cc -o $@ $@.o ${LDFLAGS}

.SUFFIXES: .h .cps

.cps.h:
	${BINDIR}/cps $*.cps

paper.o: paper.c paper.h
