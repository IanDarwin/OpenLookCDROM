# (C) Universitaet Passau, Michael Himsolt 1986-1991
#########################################################################
#									#
#			Sgraph Makefile					#
#									#
#########################################################################

all : Sgraph

# not really neccessary, but quite helpful :
# including GraphEd's makefile.template
GRAPHEDDIR = ..


CSOURCES = sgraph.c snode.c sedge.c std.c slist.c graphed.c \
	   sgragra.c sprod.c sembed.c salpha.c
HSOURCES = algorithms.h	graphed_structures.h sgraph_interface.h \
	   application_interface.h sgragra.h slist.h \
	   dispatch_commands.h sgragra_interface.h std.h \
	   graphed.h sgraph.h
MYFILES = $(HSOURCES) $(CSOURCES) sscanner.l sparser.y

#OBJECTFILES = $(CSOURCES:.c=.o)
MODULE=Sgraph

include ../contrib/makefile.template
include ../makefile.template

# The following lines may be interesting for building a standalone
# version of Sgraph (libsgraph.a)
# GraphEd will include Sgraph in its own graphed.o

#LIBSGRAPH = libsgraph.a

#CC  = cc
#DBX = -g
#CFLAGS  = $(DBX)
#LDFLAGS = $(DBX)

#
# sgraph - Library
#
#sgraph :	$(LIBRARY)

#$(LIBRARY) :	$(OBJECTFILES)
#		ar rc $(LIBRARY) $(OBJECTFILES)
#ranlib :
#		ranlib $(LIBRARY)


# How to install Sgraph

install ::	FORCE
		if [ ! -d $(SGRAPH_INSTALL_INCLUDEDIR) ]; \
		then \
			mkdir $(SGRAPH_INSTALL_INCLUDEDIR); \
		fi
		cp $(HSOURCES) $(SGRAPH_INSTALL_INCLUDEDIR)

#clean ::
#		/bin/rm -f $(LIBRARY)

# DO NOT DELETE THIS LINE -- make depend depends on it.

sgraph.o: std.h /usr/include/assert.h /usr/include/stdio.h
sgraph.o: /usr/include/malloc.h sgraph.h
snode.o: std.h /usr/include/assert.h /usr/include/stdio.h
snode.o: /usr/include/malloc.h sgraph.h
sedge.o: std.h /usr/include/assert.h /usr/include/stdio.h
sedge.o: /usr/include/malloc.h sgraph.h
std.o: std.h /usr/include/assert.h /usr/include/stdio.h /usr/include/malloc.h
std.o: sgraph.h /usr/include/varargs.h
slist.o: std.h /usr/include/assert.h /usr/include/stdio.h
slist.o: /usr/include/malloc.h slist.h sgraph.h
graphed.o: std.h /usr/include/assert.h /usr/include/stdio.h
graphed.o: /usr/include/malloc.h sgraph.h slist.h graphed.h
graphed.o: graphed_structures.h /usr/include/sunwindow/rect.h
graphed.o: dispatch_commands.h sgraph_interface.h sgragra_interface.h
sgragra.o: /usr/include/string.h /usr/include/sys/stdtypes.h std.h
sgragra.o: /usr/include/assert.h /usr/include/stdio.h /usr/include/malloc.h
sgragra.o: slist.h sgraph.h sgragra.h
sprod.o: /usr/include/string.h /usr/include/sys/stdtypes.h std.h
sprod.o: /usr/include/assert.h /usr/include/stdio.h /usr/include/malloc.h
sprod.o: slist.h sgraph.h sgragra.h
sembed.o: std.h /usr/include/assert.h /usr/include/stdio.h
sembed.o: /usr/include/malloc.h slist.h sgraph.h sgragra.h
salpha.o: std.h /usr/include/assert.h /usr/include/stdio.h
salpha.o: /usr/include/malloc.h slist.h sgraph.h sgragra.h
