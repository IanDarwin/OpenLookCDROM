# Created at Fri Dec 16 15:54:41 EST 1994
FFLAGS=-O -cg92
OBJECTS=cldecsol.o cldop5.o clrad5.o drkts1.o integr.o 

LIBRARY=integrbf.a


$(LIBRARY): $(OBJECTS)
	$(AR) r $(LIBRARY) $(OBJECTS)
#	$(AR) ts $(LIBRARY)

clean:
	rm -f *.o

clobber:
	rm -f *.o $(LIBRARY)
