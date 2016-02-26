
#	@(#) Makefile 1.10 92/05/28 

INSTALL_DIR = ../..

INCLUDEFILES =	box.h array.h canshell.h drawobj.h grip.h \
		disp_list.h list.h rectobj.h \
		tree.h patchlevel.h

all:	$(INCLUDEFILES)

clean:
	-sccs clean

install: FORCE
	cp $(INCLUDEFILES) $(INSTALL_DIR)/include/sspkg

FORCE:

