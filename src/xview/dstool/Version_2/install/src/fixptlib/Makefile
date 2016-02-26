############################################
# dstool - fixptlib makefile               #
############################################

include ${DSTOOL}/site_specific/lib_incl.mk
############################################
# Modify the following segment only.       #
############################################
LIBRARY = fixptlib.a

SOURCES_A = \
	fixed_install.c \
	fp_init.c \
	fp_compute.c \
	fp_eval.c \
	fp_get_type.c \
	mnewt.c \
	msecant.c \
	fp_jac.c \
	fp_get_atts.c \
	fp_1dman.c \
	trkman_map.c \
	trkman_ode.c

DEPS_A =  \
	${DSTOOL}/src/include/fixptlib.h \
        ${DSTOOL}/src/include/complib.h \
	${DSTOOL}/src/include/constants.h \
	${DSTOOL}/src/include/defaults.h \
	${DSTOOL}/src/include/pm.h

SOURCES_B =

DEPS_B = 

TEST_PROGRAM = 

TEST_SOURCES = 

TEST_DEPS =

##############################################
# The following line should not be modified  #
##############################################

include ${DSTOOL}/site_specific/targetsA.mk

