# Created at Fri Dec 16 15:54:51 EST 1994
FFLAGS=-O -cg92
OBJECTS=fsecpl.o lcbdata.o lcfunbf.o lcfunm.o lclinlbf.o lcmodjac.o 

LIBRARY=lcbf.a


$(LIBRARY): $(OBJECTS)
	$(AR) r $(LIBRARY) $(OBJECTS)
#	$(AR) ts $(LIBRARY)

clean:
	rm -f *.o

clobber:
	rm -f *.o $(LIBRARY)
