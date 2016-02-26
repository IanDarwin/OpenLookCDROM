############################################
# dstool - GENERIC LIBRARY makefile        #
############################################

include ${DSTOOL}/site_specific/lib_incl.mk
############################################
# Modify the following segment only.       #
############################################
LIBRARY = mainlib.a

SOURCES_A = \
	main.c \
	control.c \
	sys_install.c \
	defaults.c \
	selected.c \
	model.c \
	manifold.c \
	memory.c \
	color.c \
	load.c \
	config.c \
	print.c \
	user_install.c

#	pvmtcl_stubs.c

DEPS_A = \
	$(DSTOOL)/src/include/defaults.h \
	$(DSTOOL)/src/include/version.h \
	$(DSTOOL)/src/include/constants.h \
	$(DSTOOL)/src/include/pm.h \
	Makefile_nowin \
	$(DSTOOL)/site_specific/lib_incl.mk \
	stubs.c

SOURCES_B =

DEPS_B = 

TEST_PROGRAM =

TEST_SOURCES =

TEST_DEPS =

##############################################
# The following line should not be modified  #
##############################################

include ${DSTOOL}/site_specific/targetsA.mk

