# Created at Fri Dec 16 15:54:47 EST 1994
FFLAGS=-O -cg92
OBJECTS=fpbdata.o fpfunbf.o fpfunm.o fplinlbf.o fpmodjac.o 

LIBRARY=fpbf.a


$(LIBRARY): $(OBJECTS)
	$(AR) r $(LIBRARY) $(OBJECTS)
#	$(AR) ts $(LIBRARY)

clean:
	rm -f *.o

clobber:
	rm -f *.o $(LIBRARY)
