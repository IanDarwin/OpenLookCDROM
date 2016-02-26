
#CFLAGS = -g -DNEWS -f68881 -I/usr/NeWS/include
CFLAGS = -g -DNEWS -I/usr/NeWS/include
.SUFFIXES:	.cps
.cps.h:
	cps $*.cps

CPS = draw.cps

PICS = maryelln maryelln.ini tetra tetra.ini cube cube.ini \
	parabola parabola.ini robot2 robot2.ini fan.in fan fan.ini \
	x29 x29.ini space space.ini glass glass.ini vw vw.ini\
	mushroom mushroom.ini epcot3 epcot3.ini fractal1 fractal1.ini \
	fractal2 fractal2.ini spring spring.ini sphere3

OTHER = readme files.idx surfmod1.doc \
	reflect.c mprep.c fem2surf.c func3d.c func2sur.c

HDR = preproc.h surfmodl.h

PRESRC = preproc.c extrude.c patch.c plane.c preinit.c \
     preread.c revolve.c scshrot.c writefil.c
COBJ = preproc.o extrude.o patch.o plane.o preinit.o \
     preread.o revolve.o scshrot.o writefil.o

SRC = surfmodl.c  inreal.c readini.c writeini.c \
   readfile.c viewchan.c initial.c  perspect.c \
   normaliz.c colormod.c drawplot.c border.c openwin.c \
   getkey.c litemenu.c paramenu.c menu.c menumsg.c setgmode.c \
    wirefram.c onscreen.c storline.c swaps.c shellpts.c \
    fillsurf.c minmax.c shelsurf.c shading.c hidnline.c \
   inlimits.c cheksurf.c shadows.c surface.c storshad.c \
   intrfill.c gouraud.c drawaxes.c  
OBJ = surfmodl.o  inreal.o readini.o writeini.o \
   readfile.o viewchan.o initial.o  perspect.o \
   normaliz.o colormod.o drawplot.o border.o openwin.o \
   getkey.o litemenu.o paramenu.o menu.o menumsg.o setgmode.o \
    wirefram.o onscreen.o storline.o swaps.o shellpts.o \
    fillsurf.o minmax.o shelsurf.o shading.o hidnline.o \
   inlimits.o cheksurf.o shadows.o surface.o storshad.o \
   intrfill.o gouraud.o drawaxes.o  

LIBS = -lm $(NEWSHOME)/lib/libcps.a

surf: $(OBJ)
	cc $(CFLAGS) $(OBJ) -o surf $(LIBS)

preproc: $(COBJ)
	cc $(CFLAGS) $(COBJ) -o preproc -lm
clean:
	rm -f *.o surf preproc a.out core draw.h *~ *.BAK
shar:
	nshar -c -f surf Makefile $(HDR) $(CPS) $(SRC) $(PRESRC) \
	$(OTHER) $(PICS)
archive:
	tar cf surfmodl.tar Makefile *.h $(CPS) $(SRC) $(PRESRC) \
	$(OTHER) $(PICS)
depend:	$(SRC)
	maketd $(SRC) $(PRESRC)

draw.h: $(CPS)
$(PRESRC): preproc.h

# DO NOT DELETE THIS LINE - make depend DEPENDS ON IT

surfmodl.o: surfmodl.c surfmodl.h

inreal.o: inreal.c surfmodl.h

readini.o: readini.c surfmodl.h

writeini.o: surfmodl.h writeini.c

readfile.o: readfile.c surfmodl.h

viewchan.o: surfmodl.h viewchan.c

initial.o: initial.c surfmodl.h

perspect.o: perspect.c surfmodl.h

normaliz.o: normaliz.c surfmodl.h

colormod.o: colormod.c surfmodl.h

drawplot.o: draw.h drawplot.c surfmodl.h

border.o: border.c surfmodl.h

openwin.o: openwin.c surfmodl.h

getkey.o: getkey.c surfmodl.h

litemenu.o: litemenu.c surfmodl.h

paramenu.o: paramenu.c surfmodl.h

menu.o: menu.c surfmodl.h

menumsg.o: menumsg.c

setgmode.o: draw.h setgmode.c surfmodl.h

wirefram.o: surfmodl.h wirefram.c

onscreen.o: onscreen.c surfmodl.h

storline.o: storline.c surfmodl.h

swaps.o: swaps.c

shellpts.o: shellpts.c surfmodl.h

fillsurf.o: fillsurf.c surfmodl.h

minmax.o: minmax.c surfmodl.h

shelsurf.o: shelsurf.c surfmodl.h

shading.o: shading.c surfmodl.h

hidnline.o: hidnline.c surfmodl.h

inlimits.o: inlimits.c surfmodl.h

cheksurf.o: cheksurf.c surfmodl.h

shadows.o: shadows.c surfmodl.h

surface.o: surface.c surfmodl.h

storshad.o: storshad.c surfmodl.h

intrfill.o: intrfill.c surfmodl.h

gouraud.o: gouraud.c surfmodl.h

drawaxes.o: drawaxes.c surfmodl.h

preproc.o: preproc.c preproc.h

extrude.o: extrude.c preproc.h

patch.o: patch.c preproc.h

plane.o: plane.c preproc.h

preinit.o: preinit.c preproc.h

preread.o: preproc.h preread.c

revolve.o: preproc.h revolve.c

scshrot.o: preproc.h scshrot.c

writefil.o: preproc.h writefil.c

# *** Do not add anything here - It will go away. ***
