############################################
# dstool - savelib makefile                #
############################################

include ${DSTOOL}/site_specific/lib_incl.mk
############################################
# Modify the following segment only.       #
############################################
LIBRARY = saveloadlib.a

SOURCES_A = \
	save.c \
	write_mem.c \
	load_procs.c \
	save_procs.c \
	unform.c

DEPS_A = \
	saveload_def.h \
	$(DSTOOL)/src/include/saveload.h \
	${DSTOOL}/src/include/pm.h \
	$(DSTOOL)/src/include/pm_hash.h

SOURCES_B =

DEPS_B = 

TEST_PROGRAM = 

TEST_SOURCES = 

TEST_DEPS =

##############################################
# The following line should not be modified  #
##############################################

include ${DSTOOL}/site_specific/targetsA.mk

