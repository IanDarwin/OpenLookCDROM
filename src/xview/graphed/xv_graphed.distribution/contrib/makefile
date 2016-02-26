# (C) Universitaet Passau, Michael Himsolt 1986-1991
#########################################################################
#									#
#			    GRAPHED MAKEFILE				#
#									#
#########################################################################

SUBDIRS = PQplanar HTplanar rtlayout sugiyama maxclique termgraph \
          ChrobakPayne conn springembedder ConvexDraw bends tree \
          fileselector connect woods spring2

all :	$(SUBDIRS)
#
# Directories
#
GRAPHEDDIR = ..
include makefile.template
include $(GRAPHEDDIR)/makefile.template

$(SUBDIRS) :	FORCE
		cd $@; make $(TARGET)

		
clean ::
		make $(SUBDIRS) TARGET=clean

FORCE :		
# DO NOT DELETE THIS LINE -- make depend depends on it.
