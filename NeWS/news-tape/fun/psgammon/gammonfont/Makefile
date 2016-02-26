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
# %Z%%M% %I% %E%
#
# Copyright (c) 1988 by Sun Microsystems, Inc.
#

BINDIR = /usr/NeWS/bin

FONTDIR = $(DESTDIR)/usr/NeWS/fonts

SRCS = 	hand.curs hand_m.curs 		\
	fist.curs fist_m.curs		\
	cash.curs cash_m.curs		\
	hourglass.curs hourglass_m.curs

all: GFont12.afb GFont12.fb GFont.ff GammonCursors.ps

debug:	all

GFont12.afb: ${SRCS} gammonfont.lst ${BINDIR}/mkiconfont
	${BINDIR}/mkiconfont gammonfont.lst GFont > GFont12.afb

GFont12.fb: GFont12.afb ${BINDIR}/dumpfont
	${BINDIR}/dumpfont -d "." GFont12.afb

GFont.ff: GFont12.fb ${BINDIR}/bldfamily
	${BINDIR}/bldfamily -d "." GFont

GammonCursors.ps: GFont12.afb makegammonps
	sh makegammonps

install: all
	rm -f ${FONTDIR}/GFont*.f?
	cp GFont12.fb GFont.ff ${FONTDIR}

clean:
	rm -f GFont12.afb GFont12.fb GFont.ff GammonCursors.ps core *.BAK *~ 
