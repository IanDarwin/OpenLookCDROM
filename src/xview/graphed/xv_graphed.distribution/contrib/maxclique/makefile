# (C) Universitaet Passau, Michael Himsolt 1986-1991
#########################################################################
#									#
#			    GRAPHED MAKEFILE				#
#									#
#########################################################################


#
# Directories
#
GRAPHEDDIR = ../..

#
# Sourcefiles
#
CSOURCES = max_clique.c clique.c kclique.c liste.c gfunc.c  
HSOURCES = maxclique.h clique.h kclique.h liste.h gfunc.h 
MYFILES = $(HSOURCES) $(CSOURCES)

MODULE=maxclique
include ../makefile.template
include $(GRAPHEDDIR)/makefile.template

# DO NOT DELETE THIS LINE -- make depend depends on it.

max_clique.o: ../../sgraph/std.h /usr/include/assert.h /usr/include/stdio.h
max_clique.o: /usr/include/malloc.h ../../sgraph/sgraph.h
max_clique.o: ../../sgraph/slist.h ../../sgraph/graphed.h
max_clique.o: ../../sgraph/graphed_structures.h
max_clique.o: /usr/openwin/include/xview/rect.h
max_clique.o: /usr/openwin/include/xview/xv_c_types.h
max_clique.o: ../../sgraph/dispatch_commands.h
max_clique.o: ../../sgraph/sgraph_interface.h
max_clique.o: ../../sgraph/sgragra_interface.h gfunc.h liste.h clique.h
clique.o: ../../sgraph/std.h /usr/include/assert.h /usr/include/stdio.h
clique.o: /usr/include/malloc.h ../../sgraph/sgraph.h gfunc.h liste.h
clique.o: kclique.h
kclique.o: ../../sgraph/std.h /usr/include/assert.h /usr/include/stdio.h
kclique.o: /usr/include/malloc.h ../../sgraph/sgraph.h gfunc.h liste.h
kclique.o: clique.h
liste.o: /usr/include/string.h /usr/include/sys/stdtypes.h ../../sgraph/std.h
liste.o: /usr/include/assert.h /usr/include/stdio.h /usr/include/malloc.h
liste.o: ../../sgraph/sgraph.h liste.h gfunc.h
gfunc.o: ../../sgraph/std.h /usr/include/assert.h /usr/include/stdio.h
gfunc.o: /usr/include/malloc.h ../../sgraph/sgraph.h
