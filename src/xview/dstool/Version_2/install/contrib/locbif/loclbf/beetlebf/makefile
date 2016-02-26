# Created at Fri Dec 16 15:54:36 EST 1994
FFLAGS=-O -cg92
OBJECTS=beetle.o cnewtn.o corr.o decomp.o dern.o dfdx.o extr.o jacob.o leqv.o newtn.o poisk.o servce.o slfcrs.o solve.o tangnt.o wrtcrs.o wrtdt.o wrtend.o wrtext.o wrtout.o wrtzer.o zero.o 

LIBRARY=beetlebf.a


$(LIBRARY): $(OBJECTS)
	$(AR) r $(LIBRARY) $(OBJECTS)
#	$(AR) ts $(LIBRARY)

clean:
	rm -f *.o

clobber:
	rm -f *.o $(LIBRARY)
