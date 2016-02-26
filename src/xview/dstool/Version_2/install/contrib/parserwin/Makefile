############################################
# dstool - GENERIC LIBRARY makefile        #
############################################

include ${DSTOOL}/site_specific/lib_incl.mk
############################################
# Modify the following segment only.       #
############################################
LIBRARY = parserwinlib.a

SOURCES_A = \
	parserwin_ui.c \
	parserwin_hnd.c \
	parserwin.c \
	parserwin_go.c

DEPS_A = parserwin_ui.h \
	$(DSTOOL)/src/include/constants.h \
	$(DSTOOL)/src/include/defaults.h \
	$(DSTOOL)/src/include/version.h \
	$(DSTOOL)/src/include/manifold.h \
	$(DSTOOL)/src/include/pm.h 

SOURCES_B = \
	parser_main.c \
	parser_ops.c \
	parser_prims.c \
	sign.c

DEPS_B = parser_defs.h \
	parserwin_ui.h \
	parser.h \
	$(DSTOOL)/src/include/pm.h

TEST_PROGRAM = 

TEST_SOURCES = 

TEST_DEPS =

##############################################
# The following line should not be modified  #
##############################################

include ${DSTOOL}/site_specific/win_targetsAB.mk

