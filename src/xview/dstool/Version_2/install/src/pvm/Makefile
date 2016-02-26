############################################
# dstool - GENERIC LIBRARY makefile        #
############################################

include ${DSTOOL}/site_specific/lib_incl.mk
############################################
# Modify the following segment only.       #
############################################
LIBRARY = pvmlib.a

SOURCES_A = \
	pvm.c \
	pvm_install.c \
	pvm_host.c \
	pvm_pm.c \
	pvm_mult.c \
	pvm_send_traj.c

DEPS_A = \
	pvmuser.h \
	pvm_messages.h \
	$(DSTOOL)/src/include/constants.h \
	$(DSTOOL)/src/include/defaults.h \
	$(DSTOOL)/src/include/complib.h \
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

