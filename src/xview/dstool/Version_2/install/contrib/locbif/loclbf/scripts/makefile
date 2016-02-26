# Created at Fri Dec 16 15:53:34 EST 1994
FFLAGS=-O -cpp
OBJECTS=

LIBRARY=scripts.a


$(LIBRARY): $(OBJECTS)
	$(AR) r $(LIBRARY) $(OBJECTS)
#	$(AR) ts $(LIBRARY)

clean:
	rm -f *.o

clobber:
	rm -f *.o $(LIBRARY)
