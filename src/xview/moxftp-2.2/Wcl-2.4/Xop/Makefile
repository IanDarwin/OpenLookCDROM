# Generated automatically from Makefile.in by configure.
     srcdir = ../../.
      VPATH = .
      SHELL = /bin/sh
       LIBS =  -LWcl-2.4/Wc -lWc -LWcl-2.4/Xp -lXp -L/usr/openwin/lib -lXaw -lXmu -lXt -lXext -lX11  -lnsl -lm
     RANLIB = ranlib
        OPT =  -O 
OLD_XT_SRCS = 

         AR = ar cq
    DEFINES =  -I.. 
     CFLAGS = $(OPT) $(DEFINES)   -DRETSIGTYPE=void -DXAW -I/usr/openwin/include -DHAVE_UNISTD_H=1 $(STD_INCLUDES)

   SRCS = Table.c TableLoc.c TableVec.c Xop.c
   OBJS = Table.o TableLoc.o TableVec.o Xop.o

all:	libXop.a 

libXop.a: $(OBJS)
	-rm -f $@
	$(AR) $@ $(OBJS)
	$(RANLIB) $@

clean:
	rm -f libXop.a *.o core *~

install uninstall:

