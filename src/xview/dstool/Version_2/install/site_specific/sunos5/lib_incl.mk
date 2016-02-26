# Compiler flags.
CC = cc
ARCH = SUN5

#USING_GCC_FLAG = "-DUSING_GCC"

FC = f77
FFLAGS = -O -cg92

# Code uses the C preprocessor to check whether or not the macro USING_FORTRAN 
# is defined.
# Comment the following out if FORTRAN is not being used:
USING_FORTRAN_FLAG = "-DUSING_FORTRAN"

# Comment the following out if TCL is not being used:
USING_TCL_FLAG = "-DUSING_TCL"

# Comment the following out if PVM is not being used:
#USING_PVM_FLAG = "-DUSING_PVM"

# SUNOS4
#FORTHOME = /misc/lang/fortran_1.4/SC1.0
#F_LIBS = $(FORTHOME)/libF77.a $(FORTHOME)/libm.a

# SUNOS5
#FORTHOME = /opt/SUNWspro/SC2.0.1
F_LIBS = $(FORTHOME)/libF77.a $(FORTHOME)/libM77.a $(FORTHOME)/libsunmath.a $(FORTHOME)/libFxview.a $(FORTHOME)/libV77.a $(FORTHOME)/libpfc.a 

CFLAGS = -O -D$(ARCH) -DSUNOS5

TCLINCHOME = $(TCLHOME)
TCLFLAGS = -I$(TCLINCHOME)
PVMFLAGS = 
DSFLAGS = -I$(DSTOOL)/src/include $(TCLFLAGS) $(USING_GCC_FLAG) $(USING_FORTRAN_FLAG) $(USING_TCL_FLAG)  $(USING_PVM_FLAG)

TCLLIBHOME = $(TCLHOME)
PVMLIBHOME = /usr/local/lib
LDFLAGS = 
TCLLIBS = -L$(TCLLIBHOME) -ltcl
#PVMLIBS = -L$(PVMLIBHOME) -lpvm
LDLIBS =  $(PVMLIBS) $(TCLLIBS) $(F_LIBS) -lm

WINCPPFLAGS = -I$(OPENWINHOME)/include -I$(DSTOOL)/src/windows -I$(DSTOOL)/src/twoD
WINLDFLAGS = -L$(OPENWINHOME)/lib -lxview -lolgx -lX11

LINK.c = $(CC) $(CFLAGS) $(CPPFLAGS) $(LDFLAGS)
RM = rm -f

