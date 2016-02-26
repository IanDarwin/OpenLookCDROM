# Generated automatically from Makefile.in by configure.
     srcdir = ../../.
      VPATH = .
      SHELL = /bin/sh
       LIBS =  -LWcl-2.4/Wc -lWc -LWcl-2.4/Xp -lXp -L/usr/openwin/lib -lXaw -lXmu -lXt -lXext -lX11  -lnsl -lm
        OPT =  -O 
     RANLIB = ranlib
OLD_XT_SRCS = 

         AR = ar cq
    DEFINES =  -I.. 
     CFLAGS = $(OPT) $(DEFINES)   -DRETSIGTYPE=void -DXAW -I/usr/openwin/include -DHAVE_UNISTD_H=1 $(STD_INCLUDES)

   SRCS = Table.c TableLoc.c TableVec.c Xp.c
   OBJS = Table.o TableLoc.o TableVec.o Xp.o

all:	libXp.a 

libXp.a: $(OBJS)
	-rm -f $#
	$(AR) $@ $(OBJS)
	$(RANLIB) $@

clean:
	rm -f libXp.a *.o core *~

install uninstall:

