# (C) Universitaet Passau, Michael Himsolt 1986-1991

SUBDIRS = sgraph contrib graphed lib
# SUBDIRS = sgraph graphed lib
#DISTRIBDIR    = /home/br/graphed/xv-graphed/
DISTRIBDIR    = /home/br/graphed/xv_graphed.distribution/

$(SUBDIRS) :	FORCE
		cd $@; make $(TARGET)

distribution ::	FORCE
		rm -rf $(DISTRIBDIR)

install ::	FORCE
		if [ ! -d $(INSTALLBASE) ] ; \
		then \
			mkdir $(INSTALLBASE) ; \
		fi
		if [ ! -d $(INSTALLBASE)/bin ] ; \
		then \
			mkdir $(INSTALLBASE)/bin ; \
		fi
		if [ ! -d $(INSTALLBASE)/lib ] ; \
		then \
			mkdir $(INSTALLBASE)/lib ; \
		fi
		if [ ! -d $(INSTALLBASE)/include ] ; \
		then \
			mkdir $(INSTALLBASE)/include ; \
		fi

include makefile.template

all	:	
		make $(SUBDIRS) TARGET=all

$(SOURCES) :	FORCE
		cd $@; make $(TARGET)

distribution.local::	FORCE
		cp -r images $(DISTRIBDIR)
