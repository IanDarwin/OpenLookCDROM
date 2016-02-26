# Created at Fri Dec 16 15:54:38 EST 1994
FFLAGS=-O -cg92
OBJECTS=comp.o diagno.o endmes.o errmsg.o funt.o funtds.o lbdump.o lbf.o lbfmain.o message.o output.o protoc.o rdinit.o recomp.o setch.o showit.o store.o storei.o termnt.o wrtcom.o 

LIBRARY=complbf.a


$(LIBRARY): $(OBJECTS)
	$(AR) r $(LIBRARY) $(OBJECTS)
#	$(AR) ts $(LIBRARY)

clean:
	rm -f *.o

clobber:
	rm -f *.o $(LIBRARY)
