############################################
# dstool - GENERIC LIBRARY makefile        #
############################################

include ${DSTOOL}/site_specific/lib_incl.mk
############################################
# Modify the following segment only.       #
############################################
LIBRARY = windowlib.a

SOURCES_A = \
	ui_init.c \
	windows.c \
	start_xv_loop.c \
	sys_panels.c \
	stubs.c

#	user_panels.c

DEPS_A = \
	ui_init.h sys_panels.h user_panels.h \
	dstool.icon \
	${DSTOOL}/src/include/constants.h \
	${DSTOOL}/src/include/pm.h

SOURCES_B = 

DEPS_B =

TEST_PROGRAM =

TEST_SOURCES =

TEST_DEPS =

##############################################
# The following line should not be modified  #
##############################################

include ${DSTOOL}/site_specific/win_targetsA.mk

