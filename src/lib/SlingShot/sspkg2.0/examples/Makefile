 
#       @(#) Makefile 1.17 92/10/23 

SSPKGHOME	= ..

LIBDIR		= $(OPENWINHOME)/lib
LIBS		= -lxview -lolgx -lX11
SSPKGLIB	= $(SSPKGHOME)/lib/libsspkg.a


DBXFLAGS	=  -g 
CFLAGS		= $(DBXFLAGS) -I$(SSPKGHOME)/include -I$(OPENWINHOME)/include 
LDFLAGS		= -L$(LIBDIR) $(LIBS) $(SSPKGLIB) -lm

all: icons array_tile clockobj dnd drawarea drawline grip \
	icons misc selection tree \
	drawtext drawimage tacho dnd2

array_tile: FORCE
	cd $@; make

clockobj: FORCE
	cd $@; make

dnd: FORCE
	cd $@; make

dnd2: FORCE
	cd $@; make

drawarea: FORCE
	cd $@; make

drawline: FORCE
	cd $@; make

grip: FORCE
	cd $@; make

icons: FORCE
	cd $@; make

misc: FORCE
	cd $@; make

selection: FORCE
	cd $@; make

tree: FORCE
	cd $@; make

drawtext: FORCE
	cd $@; make

drawimage: FORCE
	cd $@; make

tacho: FORCE
	cd $@; make

clean:
	-(cd array_tile; make clean)
	-(cd clockobj; make clean)
	-(cd dnd; make clean)
	-(cd dnd2; make clean)
	-(cd drawarea; make clean)
	-(cd drawline; make clean)
	-(cd grip; make clean)
	-(cd icons; make clean)
	-(cd misc; make clean)
	-(cd selection; make clean)
	-(cd tree; make clean)
	-(cd drawtext; make clean)
	-(cd drawimage; make clean)
	-(cd tacho; make clean)
	-sccs clean

FORCE:

