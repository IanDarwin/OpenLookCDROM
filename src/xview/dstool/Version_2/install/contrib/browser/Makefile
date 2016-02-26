############################################
# dstool - GENERIC LIBRARY makefile        #
############################################

include ${DSTOOL}/site_specific/lib_incl.mk
############################################
# Modify the following segment only.       #
############################################
LIBRARY = browserlib.a

SOURCES_A = \
	browser_install.c \
	browser_cui.c \
	browser_hnd.c \
	browser.c \
	browser_go.c

DEPS_A = browser_cui.h \
         browser_def.h \
	 browser.h \
	$(DSTOOL)/src/include/pm.h

SOURCES_B =

DEPS_B =

TEST_PROGRAM =

TEST_SOURCES =

TEST_DEPS =

##############################################
# The following line should not be modified  #
##############################################

include ${DSTOOL}/site_specific/win_targetsA.mk

