#if building on a DECstation replace fitssun.f with fitsdec.f
# MFILE = fitsdec
MFILE = fitssun


.SUFFIXES: .o .for .f

.f.o:
	f77 -g -c -Nx400 $*.f

.for.o:
	f77 -g -c -Nx400 $*.for

OBJECTS =   libfits.a 

all:	fitsio.o $(MFILE).o libfits.a

fitsio.o:	fitsio.f
$(MFILE).o:	$(MFILE).f  

libfits.a:	fitsio.o $(MFILE).o
		ar rv $@ $?
		ranlib $@
libfits.a(%.o):	%.o
		@true

clean:
	rm -f core *% *.o
