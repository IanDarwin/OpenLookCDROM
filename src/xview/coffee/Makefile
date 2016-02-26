CC =		acc # must be ansi CC
CFLAGS +=	-g
LDFLAGS +=	-g
OBJS =		coffee.o xview.o util.o
XVINCLUDE =	-I/usr/openwin/include
#XLIBS = 	-L/usr/openwin/lib -lxview -lolgx -lX11 /stage/lib.linux/uflow.o
XLIBS = 	-L/usr/openwin/lib -lxview -lolgx -lX11

INSBIN =	/usr/local/bin
INSMAN =	/usr/local/man/man1

coffee:		$(OBJS)
		rm -f coffee
		cc $(LDFLAGS) $(OBJS) $(XLIBS) -o coffee

install:	coffee
		cp coffee $(INSBIN)/coffee
		cp coffee.1 $(INSBIN)/coffee.1

coffee.o util.o xview.o:	coffee.h
xview.o:	empty.icon full.icon half.icon yuch.icon unkn.icon

xview.o:	xview.c
		$(CC) $(CFLAGS) -c $(XVINCLUDE) xview.c

shar:
		bundle Imakefile Makefile *.1 *.h *.c *.icon > $@

clean:
		rm -f core coffee *.o
