.SUFFIXES:

###############
# OSF/1 flags #
###############

CC           = cc
CFLAGS       = -std1 -DPASS_THROUGH
LDFLAGS      = -s -lbsd

X_LIB        = -lX11 -ldnet_stub
MOTIF_LIB    = -lXm -lXt
DPS_LIB      = -ldps -lXext
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
