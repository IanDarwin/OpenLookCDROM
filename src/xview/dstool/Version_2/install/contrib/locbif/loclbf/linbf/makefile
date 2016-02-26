# Created at Fri Dec 16 15:54:40 EST 1994
FFLAGS=-O -cg92
OBJECTS=crsbf.o curntp.o dervar.o extrbf.o funadd.o funcvl.o funs.o initbf.o khib.o orbitc.o orbitd.o ordbf.o outfun.o outlbf.o prntbf.o secant.o wline.o 

LIBRARY=linbf.a


$(LIBRARY): $(OBJECTS)
	$(AR) r $(LIBRARY) $(OBJECTS)
#	$(AR) ts $(LIBRARY)

clean:
	rm -f *.o

clobber:
	rm -f *.o $(LIBRARY)
