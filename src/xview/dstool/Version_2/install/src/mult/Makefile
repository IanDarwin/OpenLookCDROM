############################################
# dstool                                   #
############################################
                                            
include ${DSTOOL}/site_specific/lib_incl.mk

############################################
# Modify the following segment only.       #
############################################

LIBRARY = multlib.a

SOURCES_A = \
	mult_install.c \
	mult_go.c

DEPS_A = $(DSTOOL)/src/include/mult.h \
	$(DSTOOL)/src/include/mult_proj.h \
	$(DSTOOL)/src/include/complib.h \
	$(DSTOOL)/src/include/prop.h \
	$(DSTOOL)/src/include/iterate.h \
	$(DSTOOL)/src/include/memory.h \
	$(DSTOOL)/src/include/constants.h \
	$(DSTOOL)/src/include/defaults.h \
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

include $(DSTOOL)/site_specific/targetsA.mk
