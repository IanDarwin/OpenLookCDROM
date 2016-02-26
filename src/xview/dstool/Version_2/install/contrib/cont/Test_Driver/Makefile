
DSTOOL = /marcy/newkaos/dstool
DSTOOL_LIBS = ${DSTOOL}/libraries

# Parameters.

include pgm.name

SOURCES.c = cont_driver.c \
	pitchfork.c \
	Routines/check_file.c \
	Routines/ivector.c \
	Routines/dvector.c \
	Routines/dmatrix.c \
	Routines/free_dmatrix.c \
	Routines/free_dvector.c \
	Routines/free_ivector.c 

# The following are the libs that you want to check.
TESTLIBS = ../contlib.a  \

# These are all the libs; include those that are in TESTLIBS too !
LIBS = \
  	${DSTOOL_LIBS}/Memorylib.a  \
 	
#
# Derived parameters.

SOURCES = \
	$(SOURCES.h) \
	$(SOURCES.c) 

OBJECTS = \
	$(SOURCES.c:%.c=%.o) 

OBJS =  ${DSTOOL}/storage/postmaster/pm.o \
	../cont_proc.o \
	../pitcon.o \
	../coqual.o \
	../corect.o \
	../dslv.o \
	../root.o \
	../tangnt.o \
	../vector.o \
	${DSTOOL}/storage/postmaster/TRAJ_DS/traj_ds_rw.o \
	${DSTOOL}/storage/postmaster/FIXED_DS/fixed_ds_rw.o \
	${DSTOOL}/storage/postmaster/COLORMAP/colormap_rw.o \
	${DSTOOL}/storage/postmaster/FLOW_CNTL/flow_cntl_rw.o \
	${DSTOOL}/storage/postmaster/CONT_CNTL/cont_cntl_rw.o \
	${DSTOOL}/storage/postmaster/STRUCT_CNTL/struct_cntl_rw.o \
	${DSTOOL}/storage/postmaster/MEM_CNTL/mem_cntl_rw.o \
	${DSTOOL}/storage/postmaster/MODEL_CNTL/model_cntl_rw.o \
	${DSTOOL}/storage/postmaster/MANIFOLD/manifold_rw.o \
	${DSTOOL}/storage/postmaster/DEFAULTS/defaults_rw.o \
	${DSTOOL}/storage/postmaster/MULT_CNTL/mult_cntl_rw.o



# Compiler flags.

CFLAGS += -g -DDEBUG
CPPFLAGS += -DMAIN
DSFLAGS = -I$(DSTOOL)/include 
LDLIBS += -lm


# Standard targets.

$(PROGRAM): ${MAINLIB} ${TESTLIBS} ${OBJECTS} FORCE 
	${LINK.c} ${OBJECTS} ${OBJS} ${MAINLIB} ${LIBS} ${LDFLAGS} $(LDLIBS) -o ${PROGRAM}

$(MAINLIB): $(OBJECTS)
	ar rv $@ $?


${OBJECTS}:
	cc $(CFLAGS) $(CPPFLAGS) $(DSFLAGS) -c $*.c -o $@

${TESTLIBS} : FORCE
	cd ${@D} ; ${MAKE} ${@F}

FORCE:

clean:
	rm *.o mainlib.a core

