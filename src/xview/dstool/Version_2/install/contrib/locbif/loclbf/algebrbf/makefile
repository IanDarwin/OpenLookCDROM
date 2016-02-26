# Created at Fri Dec 16 15:54:34 EST 1994
FFLAGS=-O -cg92
OBJECTS=dbalan.o decomp.o delmhe.o detbf.o dhqr.o dmfgr.o dminv.o dsimq.o dvgaus.o koeev1.o ortg2.o plo.o rzltnt.o solve.o 

LIBRARY=algebrbf.a


$(LIBRARY): $(OBJECTS)
	$(AR) r $(LIBRARY) $(OBJECTS)
#	$(AR) ts $(LIBRARY)

clean:
	rm -f *.o

clobber:
	rm -f *.o $(LIBRARY)
