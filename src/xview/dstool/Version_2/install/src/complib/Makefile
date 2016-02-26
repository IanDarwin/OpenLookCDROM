############################################
# dstool - complib makefile                #
############################################

include ${DSTOOL}/site_specific/lib_incl.mk
############################################
# Modify the following segment only.       #
############################################
LIBRARY = complib.a

SOURCES_A = \
	flow_install.c \
	flow.c \
	load_int.c \
	prop_def.c \
	symbols_def.c \
	symbol_code.c \
	traj_mem_orb.c \
	compute_orb.c


DEPS_A = $(DSTOOL)/src/include/complib.h \
	$(DSTOOL)/src/include/manifold.h \
	$(DSTOOL)/src/include/symbols.h \
	$(DSTOOL)/src/include/modellib.h \
	$(DSTOOL)/src/include/history.h \
	$(DSTOOL)/site_specific/lib_incl.mk \
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

