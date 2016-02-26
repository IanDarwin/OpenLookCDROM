# Makefile for XVirtual

# For Linux
CC       = g++   
CPPFLAGS = -g -O2 -m486 -DNO_ASM 

# For SunOS4 - it won't compile with Sun C++; need to try gcc.
#CC	= CC
#CPPFLAGS = -g -O

obj	= animate.o clipping.o figuren.o flaeche.o koerper.o load.o main.o nvekt.o shape.o szene.o view.o x.o zbuffer.o shadow.o 
 
all:	$(obj)
	$(CC) -o xvirtual $(obj) -lm -L/usr/X386/lib -L/usr/openwin/lib -lxview -L/usr/openwin/lib -lolgx -lXext -lX11 -lc

x.o:	x.cc
	$(CC) -I/usr/openwin/include $(CPPFLAGS) -c x.cc

clean:
	rm -f *~
	rm -f *.o
# DO NOT DELETE THIS LINE -- make depend depends on it.
