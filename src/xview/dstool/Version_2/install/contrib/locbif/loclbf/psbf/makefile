# Created at Fri Dec 16 15:54:57 EST 1994
FFLAGS=-O -cg92
OBJECTS=psbdata.o psfunbf.o psfunm.o pslinlbf.o psmodjac.o 

LIBRARY=psbf.a


$(LIBRARY): $(OBJECTS)
	$(AR) r $(LIBRARY) $(OBJECTS)
#	$(AR) ts $(LIBRARY)

clean:
	rm -f *.o

clobber:
	rm -f *.o $(LIBRARY)
