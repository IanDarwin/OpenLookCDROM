#CC = gcc -ansi
CC = cc
F77 = f77

INCLUDE = ${OPENWINHOME}/include
#CFLAGS = -O
CFLAGS = -g

.SUFFIXES: .o .for .f .c

.f.o:
	$(F77) -c $(CFLAGS) $*.f -I$(INCLUDE)

.for.o:
	$(F77) -c $(CFLAGS) $*.for -I$(INCLUDE)
.c.o:
	$(CC) -c $(CFLAGS) $*.c -I$(INCLUDE)

OBJECTS =   libark.a 

all:	arkcplot.o ARKfort.o libark.a

arkcplot.o:	arkcplot.c kana.h kana2.h
ARKfort.o:	ARKfort.f axecom comfil

libark.a:	ARKfort.o arkcplot.o
		ar rv $@ $?
		ranlib $@
libark.a(%.o):	%.o
		@true

clean:
	rm -f core *.o *%
