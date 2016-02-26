# Created at Fri Dec 16 15:54:52 EST 1994
FFLAGS=-O -cg92
OBJECTS=lcinitnm.o lclinebf.o lcmessng.o lcoutpt.o 

LIBRARY=lcinf.a


$(LIBRARY): $(OBJECTS)
	$(AR) r $(LIBRARY) $(OBJECTS)
#	$(AR) ts $(LIBRARY)

clean:
	rm -f *.o

clobber:
	rm -f *.o $(LIBRARY)
