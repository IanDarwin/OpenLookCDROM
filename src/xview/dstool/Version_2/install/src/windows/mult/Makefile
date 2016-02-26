############################################
# dstool - GENERIC LIBRARY makefile        #
############################################

include ${DSTOOL}/site_specific/lib_incl.mk
############################################
# Modify the following segment only.       #
############################################
LIBRARY = multwinlib.a

SOURCES_A = \
	mult.c mult_cui.c mult_hnd.c

DEPS_A = mult_cui.h \
	${DSTOOL}/src/windows/ui_init.h \
	${DSTOOL}/src/windows/c_varieties.h \
	${DSTOOL}/src/include/pm.h \
	${DSTOOL}/src/include/constants.h

SOURCES_B = 

DEPS_B = 

TEST_PROGRAM =

TEST_SOURCES =

TEST_DEPS =

##############################################
# The following line should not be modified  #
##############################################

include ${DSTOOL}/site_specific/win_targetsA.mk

