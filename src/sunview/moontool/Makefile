#   Make instructions for moon tool

CC	= cc 
INCLUDE = -I/usr/openwin/include
CFLAGS	= -g -O $(INCLUDE)
STDLIBS	= -lm
LIBDIR	= -L/usr/openwin/lib -R/usr/ucblib -L/usr/ucblib
X_LIBS	= -lxview -lolgx -lX11 -lucb

moontool: moontool.o
	$(CC) $(CFLAGS) moontool.o -o moontool $(STDLIBS) $(LIBDIR) $(X_LIBS)
moontool.o: moontool.c
	$(CC) -c $(CFLAGS) moontool.c
