# Created at Fri Dec 16 15:54:58 EST 1994
FFLAGS=-O -cg92
OBJECTS=psinitnm.o pslinebf.o psmessng.o psoutpt.o 

LIBRARY=psinf.a


$(LIBRARY): $(OBJECTS)
	$(AR) r $(LIBRARY) $(OBJECTS)
#	$(AR) ts $(LIBRARY)

clean:
	rm -f *.o

clobber:
	rm -f *.o $(LIBRARY)
