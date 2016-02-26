PROGRAM= sweeper
SOURCES= sweep.c window.c
INCLUDES= sweep.h window.h
OTHER= MANIFEST TODO sweeper.6 sweeper.icon Makefile CHANGE.LOG
OBJECTS= sweep.o window.o
SCOREFILE= /usr/local/lib/games/sweeper.scores
SHARFILE= sweeper.shar

## FLAGS FOR HP-UX
#CFLAGS = -D_BSD -I/usr/local/xview/usr/include -I/usr/include/X11R4 -DHIGHSCOREFILE=\"$(SCOREFILE)\" 
#LDFLAGS= -L/usr/local/xview/usr/lib -L/usr/lib/X11R4 -lxview -lolgx -lX11

## FLAGS for Sun 0S
CFLAGS=	-I/usr/openwin/include -O -DHIGHSCOREFILE=\"$(SCOREFILE)\"
LDFLAGS= -L/usr/openwin/lib -lxview -lolgx -lX11

CCFLAGS=-I/usr/openwin/include -g 

all: $(OBJECTS)
	cc $(CFLAGS) -o $(PROGRAM) $(OBJECTS) $(LDFLAGS)

ccenter:
	# load $(SOURCES) $(CCFLAGS) $(LDFLAGS)
	#

shar:
	shar $(SOURCES) $(INCLUDES) $(OTHER)> $(SHARFILE)

clean:
	/bin/rm -f *.o *.~?~

lint:
	lint -I/usr/openwin/include $(SOURCES)

sweep.o: sweep.c sweep.h
window.o: window.c window.h sweep.h



