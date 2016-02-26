.SUFFIXES:

###################
# Solaris 2 flags #
###################

CC           = cc
CFLAGS       = -DSolaris2 -Xa -I/usr/dt/include -I/usr/openwin/include -I/usr/openwin/include/X11
LDFLAGS      = -L/usr/dt/lib -L/usr/openwin/include

X_LIB        = -lX11 -lsocket -lnsl
MOTIF_LIB    = -lXm -lXt
DPS_LIB      = -ldps -lXext fatal.o
DPS          = -DDPS

#########
# Rules #
#########

all:  psview xpsview.exe xpsview

##########	
# psview #
##########

psview : psview.o dps.o gs.o
	$(CC) $(CFLAGS) -o psview psview.o dps.o gs.o \
	      $(LDFLAGS) $(DPS_LIB) $(X_LIB) -lm

psview.o : psview.c psview.h version.h cursor.h
	$(CC) $(CFLAGS) $(DPS) -c psview.c -DHELPFILE='"'$(LIB)/psview.help'"'

dps.o : dps.c psview.h
	$(CC) $(CFLAGS) $(DPS) -c dps.c

gs.o : gs.c psview.h
	$(CC) $(CFLAGS) $(DPS) -c gs.c -DGS='"'$(GS)'"'

###########
# xpsview #
###########

xpsview.exe : xpsview.c version.h
	$(CC) $(CFLAGS) -o xpsview.exe xpsview.c \
	      $(LDFLAGS) $(MOTIF_LIB) $(X_LIB)

xpsview : ../src/xpsview
	sed -e "s:LIB/:$(LIB)/:g;s:ARCH/:$(ARCH)/:g" < ../src/xpsview > xpsview

#################
# Miscellaneous #
#################

clean: 
	rm -f *~ *.o *.a *.d *.dvi *.aux *.log *.toc psview xpsview.exe xpsview
