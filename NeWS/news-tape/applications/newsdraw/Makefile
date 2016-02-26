# $Header: Makefile,v 1.9 88/12/02 10:50:43 bvs Exp $
# Copyright (C) 1988 by Sun Microsystems. All rights reserved.
HS   = draw.h go.h gopvt.h comm.h
CPSS = psint.cps
PSS  = draw.ps menubar.ps

GCS  = circ.c line.c oval.c rect.c text.c poly.c
OCS  = draw.c item.c eh.c font.c go.c file.c prop.c stretch.c
CS   = $(GCS) $(OCS)
SRCS = $(HS) $(CPSS) $(PSS) $(CS)

CLEAN  = *.o psint.h psint.c
OFILES = draw.o psint.o eh.o go.o item.o file.o poly.o \
		line.o rect.o circ.o oval.o text.o font.o stretch.o prop.o

CFLAGS = -g -I/usr/NeWS/include

all:	$(PSS) draw

wc:
	wc -l $(SRCS)

print:
	enscript -2r $(SRCS)

draw:	$(OFILES)
	cc $(CFLAGS) -o draw -L/usr/NeWS/lib $(OFILES) -lcps -lm

psint.o:	psint.c
circ.o:		go.h draw.h psint.h gopvt.h
draw.o:		go.h draw.h psint.h
eh.o:		go.h draw.h psint.h
go.o:		go.h draw.h psint.h
item.o:		go.h draw.h psint.h gopvt.h
line.o:		go.h draw.h psint.h gopvt.h
oval.o:		go.h draw.h psint.h gopvt.h
poly.o:		go.h draw.h psint.h gopvt.h
rect.o:		go.h draw.h psint.h gopvt.h
stretch.o:	go.h draw.h psint.h gopvt.h comm.h
text.o:		go.h draw.h psint.h gopvt.h
prop.o:		psint.h comm.h

$(SRCS):
	co $@

tar:
	tar cvf /dev/rst0 $(SRCS)

SHARS = shar1 shar2 shar3 shar4 shar5
shar:
	rm -f $(SHARS)
	make $(SHARS)

shar1:
	shar README Makefile $(HS) > $@
shar2:
	shar SUG.ps > $@
shar3:
	shar $(CPSS) $(PSS) > $@
shar4:
	shar $(GCS) > $@
shar5:
	shar $(OCS) > $@

clean:
	rm -f $(CLEAN) *.BAK draw draw.sun3 draw.sun4 core *~ 
#	-rcsclean
#	-co Makefile README

.SUFFIXES:	.cps

.cps.h:
	cps -i $<
.cps.c:
	cps -i $<
