############################################
# dstool                                   #
#                                          #
# Standard Makefile   3/31/91              #
#                                          #
include ${DSTOOL}/site_specific/lib_incl.mk
# Modify the following segment only.       #
############################################

LIBRARY = contlib.a

SOURCES_A = \
	cont_install.c \
	continue.c \
	continue_hnd.c \
	continue_cui.c \
	cont_utilities.c \
	cont_proc.c \
	coqual.c \
	corect.c \
	dslv.c \
	pitcon.c \
	root.c \
	tangnt.c \
	vector.c \
	contstate/contstate.c \
	contstate/contstate_cui.c \
	contstate/contstate_hnd.c \
	Bif_Modes/det_func.c \
	Bif_Modes/static_func.c \
	Bif_Modes/sn_func.c \
	Bif_Modes/kubicek_hopf1_func.c \
	Bif_Modes/bp_func.c  \
	Bif_Modes/sp_func.c  \
	Bif_Modes/jgr_func.c  \
	Bif_Modes/hopf_bp_func.c 



DEPS_A = \
	continue_cui.h \
	continue.h \
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
# The following lines should not be modified #
# except for working with test code          #
##############################################

include ${DSTOOL}/site_specific/win_targetsA.mk


