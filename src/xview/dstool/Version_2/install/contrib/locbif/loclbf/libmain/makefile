MAIN_LIB=libmain.a

COMMON=algebrbf beetlebf complbf linbf integrbf ifaces

VER1=epbf epinf 
VER2=fpbf fpinf linfpbf
VER3=lcbf lcinf linfpbf
VER4=linfpbf psbf psinf


version1: common
	$(AR) x ../epbf/epbf.a
	$(AR) x ../epinf/epinf.a 
	$(AR) x ../fpbf/fpbf.a
	$(AR) x ../fpinf/fpinf.a
	$(AR) x ../linfpbf/linfpbf.a
	$(AR) x ../lcbf/lcbf.a
	$(AR) x ../lcinf/lcinf.a
	$(AR) x ../linfpbf/linfpbf.a
	$(AR) x ../linfpbf/linfpbf.a
	$(AR) x ../psbf/psbf.a
	$(AR) x ../psinf/psinf.a
	$(AR) rv libmain.a *.o
	$(AR) t libmain.a
	rm -f *.o

common:
	echo "Common part..."
	rm -f *.o libmain.a
	$(AR) x ../algebrbf/algebrbf.a;
	$(AR) x ../beetlebf/beetlebf.a;
	$(AR) x ../complbf/complbf.a;
	$(AR) x ../linbf/linbf.a;
	$(AR) x ../integrbf/integrbf.a;
	$(AR) x ../ifaces/ifaces.a;

#	ranlib  libmain.a
