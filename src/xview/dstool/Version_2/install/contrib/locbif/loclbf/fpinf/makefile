# Created at Fri Dec 16 15:54:49 EST 1994
FFLAGS=-O -cg92
OBJECTS=fpinitnm.o fplinebf.o fpmessng.o fpoutpt.o 

LIBRARY=fpinf.a


$(LIBRARY): $(OBJECTS)
	$(AR) r $(LIBRARY) $(OBJECTS)
#	$(AR) ts $(LIBRARY)

clean:
	rm -f *.o

clobber:
	rm -f *.o $(LIBRARY)
