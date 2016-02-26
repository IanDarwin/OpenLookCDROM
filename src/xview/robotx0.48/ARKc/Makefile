#CC = gcc
# define the real home of ARK
ARKHOME = ../ARK/
CC = gcc
F77 = f77
F2C = f2c

# home of XView include files
OWINCDIR = $(OPENWINHOME)/include
# home of f2c.h on astro.psu.edu
F2CINCDIR = /usr/local/include
#CFLAGS = -g
CFLAGS = -O


OBJECTS =   libark.a 

all:	arkcplot.o ARKfort.o libark.a Makefile


libark.a:	ARKfort.o arkcplot.o Makefile
		ar rv $@ $?
		ranlib $@
libark.a(%.o):	%.o
		@true
arkcplot.o:	$(ARKHOME)arkcplot.c $(ARKHOME)kana.h $(ARKHOME)kana2.h
	$(CC) -c $(CFLAGS) $(ARKHOME)arkcplot.c -I$(OWINCDIR)
ARKfort.o:	ARKfort.c comfil axecom
	$(CC) -c $(CFLAGS) ARKfort.c -I$(F2CINCDIR)
ARKfort.c:	$(ARKHOME)ARKfort.f
	$(F2C) $(ARKHOME)ARKfort.f
comfil: $(ARKHOME)comfil
	cp $(ARKHOME)comfil .
axecom: $(ARKHOME)axecom
	cp $(ARKHOME)axecom .

clean:
	rm -f core *.o *%
