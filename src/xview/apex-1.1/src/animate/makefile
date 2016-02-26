
HPX11INCLUDES=/usr/include/X11R4
XVIEWINCLUDES=/usr/local/lib/xview3/include
HPX11LIBS=/usr/lib/X11R4
LOCALLIBS=/usr/local/lib

EXTRAOBJS = /tmp/signal.o /tmp/sigvec.o /tmp/getwd.o

CC=cc
CFLAGS= -I$(HPX11INCLUDES) -I$(XVIEWINCLUDES)
LIBS= -L$(LOCALLIBS) -lxview -lolgx -L$(HPX11LIBS) -lX11

animate: animate.o $(EXTRAOBJS)
	$(CC) $(CFLAGS) -o animate animate.o $(EXTRAOBJS) $(LIBS)
animate.o:	animate.c

/tmp/sigvec.o:
	(cd /tmp ; ar -x /usr/lib/libBSD.a sigvec.o )

/tmp/signal.o:
	(cd /tmp ; ar -x /usr/lib/libBSD.a signal.o )

/tmp/getwd.o:
	(cd /tmp ; ar -x /usr/lib/libBSD.a getwd.o )


