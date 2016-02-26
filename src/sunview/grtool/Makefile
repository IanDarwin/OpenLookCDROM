#
# makefile for grtool
# $Header: Makefile,v 1.17 89/09/07 22:15:40 pturner Locked $
#
OBJS = gr.o grtool.o compose.o files.o utils.o plotone.o fit.o fourier.o\
	setwin.o strwin.o ptswin.o io.o setutils.o objutils.o params.o\
	compute.o alerts.o sunview.o draw.o hp.o ps.o generic.o raster.o chersh.o

SRCS = gr.c grtool.c compose.c files.c utils.c plotone.c fit.c fourier.c\
	setwin.c strwin.c ptswin.c io.c setutils.c objutils.c params.c\
	compute.c alerts.c sunview.c draw.c hp.c ps.c generic.c raster.c chersh.c

INCS = defines.h externs.h globals.h objdefs.h icon.h

DEMOS = test.dat test.par test2.dat test2.par test3.dat test3.par test4.dat test4.par\
	testc.par errbar.dat errbar.par

HERSHINCS = hersh.h symdef.h special.h

LIBS = -lsuntool -lsunwindow -lpixrect -lm 

DESTDIR = /usr/local/bin

CFLAGS = -O

# All mine (well, mostly)
#
PARSOBJS = pars.o
PARSSRCS = pars.y

grtool: $(OBJS) $(PARSOBJS)
	cc $(OBJS) $(PARSOBJS) -o grtool $(LIBS) 

pars.o: pars.y
grtool.o: defines.h globals.h objdefs.h icon.h
compose.o: defines.h globals.h
gr.o: defines.h
calc.o: globals.h
fit.o: defines.h
io.o: defines.h
params.o: globals.h objdefs.h
plotone.o: defines.h globals.h objdefs.h
setutils.o: defines.h globals.h
objutils.o: defines.h globals.h objdefs.h
setwin.o: defines.h globals.h
strwin.o: defines.h globals.h
ptswin.o: defines.h globals.h
chersh.o: hersh.h special.h symdef.h
generic.o: externs.h
hp.o: externs.h
ps.o: externs.h
sunview.o: externs.h

install:
	mv grtool $(DESTDIR)

lint:
	lint -v $(SRCS)

clean:
	rm grtool *.o

# dist is a directory
distrib:
	cp Makefile README INSTALL grtool.1 grtool.doc $(SRCS) $(INCS) $(PARSSRCS) $(HERSHINCS) $(DEMOS) dist

rcs:
	ci -l Makefile $(SRCS) $(INCS) $(PARSSRCS) hersh.h special.h symdef.h

wc:
	wc Makefile $(SRCS) $(INCS) $(PARSSRCS) hersh.h special.h symdef.h

edit:
	vi $(SRCS)

print:
	plst $(SRCS) $(INCS) $(PARSSRCS) > pr.prt 

ls:
	ls $(SRCS) $(PARSSRCS) $(INCS) >src.ls

version:
	grep Header Makefile  $(SRCS) $(PARSSRCS) $(INCS) hersh.h special.h symdef.h >>version.doc
