############################################
# dstool - complib makefile                #
############################################

include ${DSTOOL}/site_specific/lib_incl.mk
############################################
# Modify the following segment only.       #
############################################
LIBRARY = integlib.a

SOURCES_A = \
	integrate.c \
	ode_nsteps.c \
	ode_stop.c  \
	ode_time.c  \
	ode_poincare.c \
	last_step.c



DEPS_A = $(DSTOOL)/src/include/complib.h \
	$(DSTOOL)/src/include/manifold.h \
	$(DSTOOL)/src/include/history.h \
	$(DSTOOL)/src/include/pm.h

SOURCES_B =

DEPS_B = 

TEST_PROGRAM = 

TEST_SOURCES = 

TEST_DEPS =

##############################################
# The following line should not be modified  #
##############################################

include ${DSTOOL}/site_specific/targetsA.mk

