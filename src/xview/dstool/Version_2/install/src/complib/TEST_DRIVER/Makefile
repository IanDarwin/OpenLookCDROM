PROGRAM1 = complib_test

CFLAGS = -g -DDEBUG
DSFLAGS = -I${DSTOOL}/src/models -I${DSTOOL}/src/windows -I${DSTOOL}/src/include
LDFLAGS = -g 

# CPPFLAGS += -I${OPENWINHOME}/include
# LDLIBS +=  -lxview -lolgx -L$(OPENWINHOME)/lib -lX -lm
LDLIBS =  -lm

TEST_SOURCES = driver.c 
TEST_OBJECTS = driver.c 

SOURCES = \
	problem1.c \
	problem2.c \
	problem3.c \
	problem4.c \
	problem5.c \
	problem6.c \
	problem7.c \
	problem8.c \
	def_problem.c \
	routines.c \
	../jacobian.c \
	../INTEGRATION/integrate.c \
	../INTEGRATION/ode_nsteps.c \
	../INTEGRATION/ode_stop.c \
	../INTEGRATION/ode_poincare.c \
	../INTEGRATION/ode_time.c \
	../INTEGRATION/last_step.c \
	../ITERATION/blas.c \
	../ITERATION/iter_back.c \
	../ITERATION/iter_forw.c \
	../ITERATION/iterate.c \
	../ITERATION/manifold.c \
	../ITERATION/random.c \
	../ALGORITHMS/rk4.c \
	../ALGORITHMS/euler.c \
	../ALGORITHMS/rkqc.c \
	../ALGORITHMS/bs.c \
	${DSTOOL}/src/utilities/system_mess_proc.c \
	${DSTOOL}/src/utilities/vector.c \
	${DSTOOL}/src/computation/eigenlib/ludcmp.c \
	${DSTOOL}/src/computation/eigenlib/lubksb.c


OBJECTS = $(SOURCES:.c=.o)

$(PROGRAM1): ${OBJECTS} ${TEST_OBJECTS}  
	cc -g ${TEST_SOURCES} ${OBJECTS} $(LIBS) ${DSFLAGS} ${LDFLAGS} $(LDLIBS) -o ${PROGRAM1}

${OBJECTS}:
	cc $(CFLAGS) $(CPPFLAGS) $(DSFLAGS) -c $*.c -o $@

${TEST_OBJECTS}:
	cc $(CFLAGS) $(CPPFLAGS) $(DSFLAGS) -c $*.c -o $@

