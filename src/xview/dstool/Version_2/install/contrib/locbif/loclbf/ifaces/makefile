# Created at Fri Dec 16 15:54:42 EST 1994
FFLAGS=-O -cg92
OBJECTS=bdata.o codebf.o der1bf.o der1l.o der2q.o detev.o dfunbf.o eiglbf.o funall.o funbf.o funm.o initname.o linebf.o linlbf.o messing.o modjac.o outpt.o quadr.o regpbf.o zerobf.o 

LIBRARY=ifaces.a


$(LIBRARY): $(OBJECTS)
	$(AR) r $(LIBRARY) $(OBJECTS)
#	$(AR) ts $(LIBRARY)

clean:
	rm -f *.o

clobber:
	rm -f *.o $(LIBRARY)
