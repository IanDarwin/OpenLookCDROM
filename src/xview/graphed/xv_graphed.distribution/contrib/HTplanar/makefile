# (C) Universitaet Passau, Michael Himsolt 1986-1991
#########################################################################
#									#
#			    GRAPHED MAKEFILE				#
#									#
#########################################################################


#
# Directories
#

#
# Sourcefiles
#
CSOURCES = HTembed.c HTplanarity.c HTmain.c dualopt.c
HSOURCES = planar.h embed.h
MYFILES = $(HSOURCES) $(CSOURCES)

GRAPHEDDIR = ../..
MODULE=HTplanar

include ../makefile.template
include $(GRAPHEDDIR)/makefile.template

# DO NOT DELETE THIS LINE -- make depend depends on it.

HTembed.o: ../../sgraph/std.h /usr/include/assert.h /usr/include/stdio.h
HTembed.o: /usr/include/malloc.h ../../sgraph/sgraph.h ../../sgraph/slist.h
HTembed.o: embed.h
HTplanarity.o: ../../sgraph/std.h /usr/include/assert.h /usr/include/stdio.h
HTplanarity.o: /usr/include/malloc.h ../../sgraph/sgraph.h
HTplanarity.o: ../../sgraph/slist.h planar.h
HTmain.o: ../../sgraph/std.h /usr/include/assert.h /usr/include/stdio.h
HTmain.o: /usr/include/malloc.h ../../sgraph/sgraph.h ../../sgraph/slist.h
HTmain.o: ../../sgraph/graphed.h ../../sgraph/graphed_structures.h
HTmain.o: /usr/openwin/include/xview/rect.h
HTmain.o: /usr/openwin/include/xview/xv_c_types.h
HTmain.o: ../../sgraph/dispatch_commands.h ../../sgraph/sgraph_interface.h
HTmain.o: ../../sgraph/sgragra_interface.h planar.h embed.h
dualopt.o: ../../sgraph/std.h /usr/include/assert.h /usr/include/stdio.h
dualopt.o: /usr/include/malloc.h ../../sgraph/sgraph.h ../../sgraph/slist.h
dualopt.o: ../../sgraph/graphed.h ../../sgraph/graphed_structures.h
dualopt.o: /usr/openwin/include/xview/rect.h
dualopt.o: /usr/openwin/include/xview/xv_c_types.h
dualopt.o: ../../sgraph/dispatch_commands.h ../../sgraph/sgraph_interface.h
dualopt.o: ../../sgraph/sgragra_interface.h
