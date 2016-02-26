#
# Makefile for NeWS distributed MicroEmacs.
#

TARGETDIR = .
TARGET  = ${TARGETDIR}/nemacs
NEWSDIR = $(NEWSHOME)
NEWSLIB = ${NEWSDIR}/lib/libcps.a
NEWSINC = ${NEWSDIR}/include
CFLAGS  = -I${NEWSINC} -DNeWS -DCOLOR -DBSD
LIBS    = ${NEWSLIB}
HDRS 	=	ebind.h \
			evar.h \
			estruct.h \
			esearch.h \
	 		epath.h \
	 		efunc.h \
			edef.h 
OBJS	= 	./O/news.o \
			./O/termio.o \
			./O/display.o \
			./O/word.o \
			./O/window.o \
			./O/spawn.o \
			./O/region.o \
			./O/search.o \
			./O/main.o \
			./O/random.o \
			./O/isearch.o \
			./O/lock.o \
			./O/line.o \
			./O/input.o \
			./O/fileio.o \
			./O/exec.o \
			./O/file.o \
			./O/eval.o \
			./O/dolock.o \
			./O/buffer.o \
			./O/crypt.o \
			./O/bind.o \
			./O/basic.o \
			./O/ansi.o

$(TARGET) : $(OBJS) 
	cc $(CFLAGS) -o $(TARGET) $(OBJS) $(LIBS)

clean :
	rm -f *.o news.h nemacs O/*.o core *.BAK *~ 

news.h: news.cps
	cps news.cps

./O/news.o: news.c /usr/include/stdio.h estruct.h edef.h news.h
	rm -f $@
	cc ${CFLAGS} -o $@ -c news.c
./O/word.o: word.c /usr/include/stdio.h estruct.h edef.h
	rm -f $@
	cc ${CFLAGS} -o $@ -c word.c
./O/vmsvt.o: vmsvt.c /usr/include/stdio.h estruct.h edef.h
	rm -f $@
	cc ${CFLAGS} -o $@ -c vmsvt.c
./O/window.o: window.c /usr/include/stdio.h estruct.h edef.h
	rm -f $@
	cc ${CFLAGS} -o $@ -c window.c
./O/vt52.o: vt52.c /usr/include/stdio.h estruct.h edef.h
	rm -f $@
	cc ${CFLAGS} -o $@ -c vt52.c
./O/tipc.o: tipc.c /usr/include/stdio.h estruct.h edef.h 
	rm -f $@
	cc ${CFLAGS} -o $@ -c tipc.c
./O/tcap.o: tcap.c /usr/include/stdio.h estruct.h edef.h
	rm -f $@
	cc ${CFLAGS} -o $@ -c tcap.c
./O/termio.o: termio.c /usr/include/stdio.h \
				   /usr/include/sgtty.h \
				   /usr/include/sys/ioctl.h \
				   /usr/include/sys/ttychars.h \
				   /usr/include/sys/ttydev.h \
				   /usr/include/signal.h \
				   /usr/include/sys/ioctl.h \
				   estruct.h edef.h
	rm -f $@
	cc ${CFLAGS} -o $@ -c termio.c
./O/spawn.o: spawn.c /usr/include/stdio.h \
			     /usr/include/signal.h \
				 estruct.h edef.h
	rm -f $@
	cc ${CFLAGS} -o $@ -c spawn.c
./O/st520.o: st520.c /usr/include/stdio.h estruct.h edef.h
	rm -f $@
	cc ${CFLAGS} -o $@ -c st520.c
./O/region.o: region.c /usr/include/stdio.h estruct.h edef.h
	rm -f $@
	cc ${CFLAGS} -o $@ -c region.c
./O/search.o: search.c /usr/include/stdio.h estruct.h edef.h
	rm -f $@
	cc ${CFLAGS} -o $@ -c search.c
./O/main.o: main.c /usr/include/stdio.h estruct.h efunc.h edef.h ebind.h
	rm -f $@
	cc ${CFLAGS} -o $@ -c main.c
./O/random.o: random.c /usr/include/stdio.h estruct.h edef.h
	rm -f $@
	cc ${CFLAGS} -o $@ -c random.c
./O/isearch.o: isearch.c /usr/include/stdio.h estruct.h edef.h
	rm -f $@
	cc ${CFLAGS} -o $@ -c isearch.c
./O/lock.o: lock.c /usr/include/stdio.h estruct.h edef.h /usr/include/sys/errno.h
	rm -f $@
	cc ${CFLAGS} -o $@ -c lock.c
./O/line.o: line.c /usr/include/stdio.h estruct.h edef.h
	rm -f $@
	cc ${CFLAGS} -o $@ -c line.c
./O/ibmpc.o: ibmpc.c /usr/include/stdio.h estruct.h edef.h
	rm -f $@
	cc ${CFLAGS} -o $@ -c ibmpc.c
./O/input.o: input.c /usr/include/stdio.h estruct.h edef.h
	rm -f $@
	cc ${CFLAGS} -o $@ -c input.c
./O/hp110.o: hp110.c /usr/include/stdio.h estruct.h edef.h
	rm -f $@
	cc ${CFLAGS} -o $@ -c hp110.c
./O/hp150.o: hp150.c /usr/include/stdio.h estruct.h edef.h
	rm -f $@
	cc ${CFLAGS} -o $@ -c hp150.c
./O/fileio.o: fileio.c /usr/include/stdio.h estruct.h edef.h
	rm -f $@
	cc ${CFLAGS} -o $@ -c fileio.c
./O/exec.o: exec.c /usr/include/stdio.h estruct.h edef.h
	rm -f $@
	cc ${CFLAGS} -o $@ -c exec.c
./O/file.o: file.c /usr/include/stdio.h estruct.h edef.h
	rm -f $@
	cc ${CFLAGS} -o $@ -c file.c
./O/eval.o: eval.c /usr/include/stdio.h estruct.h edef.h evar.h
	rm -f $@
	cc ${CFLAGS} -o $@ -c eval.c
./O/display.o: display.c /usr/include/stdio.h ./estruct.h edef.h
	rm -f $@
	cc ${CFLAGS} -o $@ -c display.c
./O/dolock.o: dolock.c
	rm -f $@
	cc ${CFLAGS} -o $@ -c dolock.c
./O/buffer.o: buffer.c /usr/include/stdio.h ./estruct.h ./edef.h
	rm -f $@
	cc ${CFLAGS} -o $@ -c buffer.c
./O/crypt.o: crypt.c /usr/include/stdio.h estruct.h edef.h
	rm -f $@
	cc ${CFLAGS} -o $@ -c crypt.c
./O/dg10.o: dg10.c /usr/include/stdio.h estruct.h edef.h
	rm -f $@
	cc ${CFLAGS} -o $@ -c dg10.c
./O/bind.o: bind.c /usr/include/stdio.h estruct.h edef.h epath.h
	rm -f $@
	cc ${CFLAGS} -o $@ -c bind.c
./O/basic.o: basic.c /usr/include/stdio.h estruct.h edef.h
	rm -f $@
	cc ${CFLAGS} -o $@ -c basic.c
./O/ansi.o:	ansi.c /usr/include/stdio.h estruct.h edef.h
	rm -f $@
	cc ${CFLAGS} -o $@ -c ansi.c
