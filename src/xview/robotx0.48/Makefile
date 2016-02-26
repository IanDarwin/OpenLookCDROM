# The default is to use a FORTRAN compiler
all:
	(cd Robot; make)
# This is if you are going to use f2c instead of a FORTRAN compiler
f2c:
	(cd Robot; make -f makefile.f2c)
clean:
	(cd Robot; make clean)
	(cd Fitsio; make clean)
	(cd ARK; make clean)
	(cd ARKc; make clean)
	(cd Fitsioc; make clean)
