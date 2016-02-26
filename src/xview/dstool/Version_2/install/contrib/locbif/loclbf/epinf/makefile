# Created at Fri Dec 16 15:54:46 EST 1994
FFLAGS=-O -cg92
OBJECTS=epinitnm.o eplinebf.o epmessng.o epoutpt.o 

LIBRARY=epinf.a


$(LIBRARY): $(OBJECTS)
	$(AR) r $(LIBRARY) $(OBJECTS)
#	$(AR) ts $(LIBRARY)

clean:
	rm -f *.o

clobber:
	rm -f *.o $(LIBRARY)
