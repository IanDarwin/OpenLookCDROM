# Generated automatically from Makefile.in by configure.
     srcdir = ../../.
      VPATH = .
      SHELL = /bin/sh
       LIBS =  -LWcl-2.4/Wc -lWc -LWcl-2.4/Xp -lXp -L/usr/openwin/lib -lXaw -lXmu -lXt -lXext -lX11  -lnsl -lm
     RANLIB = ranlib
       OPT  =  -O 
OLD_XT_SRCS = 
OLD_XT_OBJS = 

         AR = ar cq
CREATE_DEFS = -DWCL_ERRORDB=\"/usr/openwin/lib/app-defaults/WclErrorDB\"
    DEFINES =  -I.. 
     CFLAGS = $(OPT) $(DEFINES) $(CREATE_DEFS)  -DRETSIGTYPE=void -DXAW -I/usr/openwin/include -DHAVE_UNISTD_H=1 $(STD_INCLUDES)

HEADERS = \
	MapAg.h \
	WcCreate.h \
	WcCreateP.h \
	COPY COPY_X

SRCS = \
	MapAg.c \
	WcActCB.c \
	WcConvert.c \
	WcCreate.c \
	WcInvoke.c \
	WcLateBind.c \
	WcLoadRes.c \
	WcName.c \
	WcReg.c \
	WcRegXt.c \
	WcSetValue.c \
	WcTemplate.c \
	WcWarn.c $(OLD_XT_SRCS)

OBJS = \
	MapAg.o \
	WcActCB.o \
	WcConvert.o \
	WcCreate.o \
	WcInvoke.o \
	WcLateBind.o \
	WcLoadRes.o \
	WcName.o \
	WcReg.o \
	WcRegXt.o \
	WcSetValue.o \
	WcTemplate.o \
	WcWarn.o $(OLD_XT_OBJS)

all:	libWc.a 


libWc.a: $(OBJS)
	-rm -f $@
	$(AR) $@ $(OBJS)
	$(RANLIB) $@

clean:
	rm -f libWc.a *.o core *~

install uninstall:

