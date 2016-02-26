MAIN_EXE=lbfepc

MAIN_OBJ=lbfmain.o funtds.o 

MAIN_LIB=../../libmain/libmain.a

# Common files in directories: algebrbf beetlebf complbf linbf integrbf ifaces
# Version 1 specific files in directories: epbf epinf 

$(MAIN_EXE): $(MAIN_OBJ) 
	cd ../../libmain;make version1
	$(FC) -o $(MAIN_EXE) $(MAIN_OBJ) $(MAIN_LIB)
	rm -f ../../libmain/libmain.a
