# @(#)26	1.1  R2/DPS/examples/texteroids/Makefile, gos, gos320 6/27/91 19:51:11
#
#   COMPONENT_NAME: XDPS
#
#   FUNCTIONS: none
#
#   ORIGINS: 1
#
#

#******************************************************************************
#*                                                                            *
#*  Config file for the IBM architecture                                      *
#*                                                                            *
#******************************************************************************
#-------------------------------------------------------#
# Definitions of some basic places to find things       #
#-------------------------------------------------------#
X11ROOT = /usr/openwin
XTROOT  = /usr/openwin
XMROOT  = /usr/dt
DPSROOT = /usr/openwin
#-------------------------------------------------------#
# Definitions of places to look for include files       #
#-------------------------------------------------------#
X11INC  = -I$(X11ROOT)/include
XMINC   = -I$(XMROOT)/include
DPSINC  = -I$(DPSROOT)/include/X11
XTRAINC =
#-------------------------------------------------------#
# Definitions of library files                          #
#-------------------------------------------------------#
X11LIBS  = -lX11
X11EXT   = -lXext
XTLIBS   = -lXt
XMLIBS   = -lXol -lXmu
DPSLIBS  = -ldps
XTRALIBS = -lm
#-------------------------------------------------------#
# REQUIRED defs for use in the rest of the makefile     #
#-------------------------------------------------------#
CC        = cc
LD        = ld
LINK	  = ln -s
RM        = rm -f
PSWRAP    = $(X11ROOT)/bin/pswrap
LIBS      = $(DPSLIBS) $(XMLIBS) $(XTLIBS) $(X11EXT) $(X11LIBS) $(XTRALIBS)
INCS      = -I. -I$(SOURCES) $(DPSINC) $(XMINC) $(X11INC) $(XTRAINC)
XTRAFLAGS =
#-------------------------------------------------------#
# Standard definitions for all machines                 #
#-------------------------------------------------------#
LDFLAGS     = -L$(X11ROOT)/lib -L$(XTROOT)/lib -L$(XMROOT)/lib -L$(DPSROOT)/lib
CDEBUGFLAGS =
COPTFLAGS   =
CCFLAGS     = $(CDEBUGFLAGS) $(COPTFLAGS) $(INCS) $(XTRAFLAGS)
SOURCES     = ../src/
#-------------------------------------------------------#
# Standard make rules			                #
#-------------------------------------------------------#
.SUFFIXES: .psw

.psw.o :
	$(RM) $@ $*.h
	$(PSWRAP) -o $*.c -h $*.h $<
	$(CC) -c $(CCFLAGS) $*.c
	$(RM) $*.c

.c.o :
	$(RM) $@
	$(CC) -c $(CCFLAGS) $*.c

#-------------------------------------------------------#
# That's all folks                                      #
#-------------------------------------------------------#
PROGRAM = texteroids
OBJS    = texteroids.o texterwraps.o isinf.o
DERIVED_FILES = texterwraps.h

all : $(PROGRAM)

$(PROGRAM) : $(OBJS)
	$(CC) $(LDFLAGS) -o $(PROGRAM) $(OBJS) $(LIBS)

objs : $(OBJS)

clean :
	$(RM) core $(OBJS) *.BAK *.CKP *~ *% make.log made $(DERIVED_FILES)

clean_all : clean
	$(RM) $(PROGRAM)

scratch : clean_all all
