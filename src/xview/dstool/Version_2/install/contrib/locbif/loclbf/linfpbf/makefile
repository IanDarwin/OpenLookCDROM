# Created at Fri Dec 16 15:54:55 EST 1994
FFLAGS=-O -cg92
OBJECTS=detfld.o detflp.o dethpf.o dp1fld.o dp1flp.o dp1hpf.o fpcodebf.o fpder1bf.o fpder1l.o fpder2q.o fpdetev.o fpdfunbf.o fpeiglbf.o fpfunall.o fpquadr.o fpregpbf.o fpzerobf.o trfld.o trflp.o trhpf.o 

LIBRARY=linfpbf.a


$(LIBRARY): $(OBJECTS)
	$(AR) r $(LIBRARY) $(OBJECTS)
#	$(AR) ts $(LIBRARY)

clean:
	rm -f *.o

clobber:
	rm -f *.o $(LIBRARY)
