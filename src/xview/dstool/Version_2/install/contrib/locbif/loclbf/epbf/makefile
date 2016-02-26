# Created at Fri Dec 16 15:54:44 EST 1994
FFLAGS=-O -cg92
OBJECTS=block2.o der23l.o dervar.o detjac.o detkrv.o detntv.o detrg.o detvpr.o dxp1kr.o dxp1nt.o dxp1p2.o epbdata.o epcodebf.o epder1bf.o epder1l.o epder2q.o epdetev.o epdfunbf.o epeiglbf.o epfunall.o epfunbf.o eplinlbf.o epquadr.o epregpbf.o epzerobf.o funts.o lyapun.o vliap.o vlp.o 

LIBRARY=epbf.a


$(LIBRARY): $(OBJECTS)
	$(AR) r $(LIBRARY) $(OBJECTS)
#	$(AR) ts $(LIBRARY)

clean:
	rm -f *.o

clobber:
	rm -f *.o $(LIBRARY)
