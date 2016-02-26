# Make C versions of the Bill Pence FITSIO routines



F2C = f2c
FITSDIR = ../Fitsio/

# home of f2c.h on astro.psu.edu
INCLUDE = /usr/local/include

OBJECTS =   libfits.a
# machine specific code
# MFILE = fitsdec
MFILE = fitssun

# compile time flags
# CLAGS = -g
# CFALGS = -O
CFLAGS =

all:	fitsio.o $(MFILE).o idate.o libfits.a


idate.o: idate.c
	cc -c idate.c $(CFLAGS)
fitsio.o:	fitsio.c
	cc -c fitsio.c -I$(INCLUDE) $(CFLAGS)
fitsio.c:	$(FITSDIR)fitsio.f
	$(F2C) -Nx300 $(FITSDIR)fitsio.f
$(MFILE).o:	$(MFILE).c
	cc -c $(MFILE).c -I$(INCLUDE) $(CFLAGS)
$(MFILE).c:	$(FITSDIR)$(MFILE).f
	$(F2C) -Nx300 $(FITSDIR)$(MFILE).f

libfits.a:	fitsio.o $(MFILE).o idate.o
		ar rv $@ $?
		ranlib $@
libfits.a(%.o):	%.o
		@true

clean:
	rm -f core *% *.o fitsio.c $(MFILE).c 
