# No imake file exists only this Makefile
# define compilers
CC = cc
#CC = gcc -ansi
F77 = f77
# define compile options here if required, 
# e.g. debug 
CFLAGS = -g
# or optimise (if you have enough swap space)
# CFLAGS = -O
# or nothing at all
# CFLAGS = 

# specify here the directory where the openwindow include
# files are kept
# INCLUDE = /usr/local/openwin/include
INCLUDE = ${OPENWINHOME}/include

# location of the FITS library
FITSDIR = ../Fitsio/
FITSLIB = $(FITSDIR)libfits.a

# if USA is defined we make some attempt to use USA spellings
LANGUAGE = -DUSA
#LANGUAGE = -DUK


# This specifies the location of the graphics routines in
# the ARK directory
ARKDIR = ../ARK/
ARKLIB = $(ARKDIR)libark.a
# installation directory
INSTDIR = /bulk1/pkg/xray/bin
OBJS = robot.o subroutines.o subroutines2.o subroutines3.o \
user.o main.o rfitsf.o rfitsc.o fits3d.o york.o classify.o \
nr.o colors.o fitter.o browser.o about.o history.o axes.o zoom.o dnd.o \
specials.o split.o math.o text_editor.o canvas_menu.o

robot:  $(OBJS) Makefile FITSLIB ARKLIB
	$(F77) $(OBJS)\
  $(ARKLIB)\
  $(OPENWINHOME)/lib/libxview.a\
  $(OPENWINHOME)/lib/libolgx.a\
  -lX11\
  -lF77\
  $(FITSLIB)\
  $(CFLAGS) -o robot
FITSLIB: 
	(cd $(FITSDIR); make libfits.a)
ARKLIB:
	(cd $(ARKDIR); make libark.a)
robot.o: robot.f robcom msizcom fitcom
	$(F77)  -c -Nx300 $(CFLAGS) robot.f
subroutines.o: subroutines.f robcom fitcom
	$(F77)  -c $(CFLAGS) subroutines.f
subroutines2.o: subroutines2.f robcom msizcom fitcom
	$(F77)  -c $(CFLAGS) subroutines2.f
subroutines3.o: subroutines3.f robcom msizcom
	$(F77)  -c $(CFLAGS) subroutines3.f
nr.o: nr.f robcom
	$(F77)  -c $(CFLAGS) nr.f
main.o: main.c robot.h patchlevel.h
	$(CC) -c  $(CFLAGS) main.c -I$(INCLUDE) $(LANGUAGE) 
colors.o: colors.c
	$(CC) -c  $(CFLAGS) colors.c -I$(INCLUDE) $(LANGUAGE) 
fitter.o: fitter.c
	$(CC) -c  $(CFLAGS) fitter.c -I$(INCLUDE) $(LANGUAGE) 
browser.o: browser.c
	$(CC) -c  $(CFLAGS) browser.c -I$(INCLUDE)
about.o: about.c
	$(CC) -c  $(CFLAGS) about.c -I$(INCLUDE) $(LANGUAGE) 
history.o: history.c
	$(CC) -c  $(CFLAGS) history.c -I$(INCLUDE) $(LANGUAGE) 
canvas_menu.o: canvas_menu.c
	$(CC) -c  $(CFLAGS) canvas_menu.c -I$(INCLUDE)
text_editor.o: text_editor.c
	$(CC) -c  $(CFLAGS) text_editor.c -I$(INCLUDE)
axes.o: axes.c
	$(CC) -c  $(CFLAGS) axes.c -I$(INCLUDE) $(LANGUAGE) 
zoom.o: zoom.c
	$(CC) -c  $(CFLAGS) zoom.c -I$(INCLUDE) $(LANGUAGE) 
specials.o: specials.c
	$(CC) -c  $(CFLAGS) specials.c -I$(INCLUDE) $(LANGUAGE) 
split.o: split.c
	$(CC) -c  $(CFLAGS) split.c -I$(INCLUDE) $(LANGUAGE) 
dnd.o: dnd.c
	$(CC) -c  $(CFLAGS) dnd.c -I$(INCLUDE)
math.o: math.c
	$(CC) -c  $(CFLAGS) math.c
user.o: user.f
	$(F77) -c  $(CFLAGS) user.f
york.o: york.f robcom
	$(F77) -c  $(CFLAGS) york.f
rfitsf.o: rfitsf.f robcom
	$(F77) -c $(CFLAGS) rfitsf.f
rfitsc.o: rfitsc.c
	$(CC) -c $(CFLAGS) rfitsc.c
fits3d.o: fits3d.f robcom
	$(F77) -c $(CFLAGS) fits3d.f
classify.o: classify.c
	$(CC) -c $(CFLAGS) classify.c
.mfile:
	@echo "WARNING - SENDING MAIL TO corbet@astro.psu.edu"
	echo "Makefile" > .mfile
	cat patchlevel.h >> .mfile
	/bin/mail corbet@astro.psu.edu < .mfile
backup:
	cp $(INSTDIR)/robot $(INSTDIR)/robot.old
install:
	(strip robot; mv robot $(INSTDIR))
bitclean:
	rm -f robot.o subroutines.o subroutines2.o subroutines3.o
	rm -f nr.o user.o rfitsf.o fits3d.o york.o
clean:
	rm -f *.o *% core robot.trace
veryclean:
	rm -f *.o *% core robot.trace robot
