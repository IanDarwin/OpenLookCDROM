############################################
# dstool - GENERIC LIBRARY makefile        #
############################################

include ${DSTOOL}/site_specific/lib_incl.mk
############################################
# Modify the following segment only.       #
############################################
LIBRARY = lbfpanels.a

SOURCES_A = \
        lbmain.c \
        lbmain_hnd.c \
        lbmain_cui.c \
        lbinstall.c \
        lbsolver/lbsolver.c \
        lbsolver/lbsolver_cui.c \
        lbsolver/lbsolver_hnd.c \
        lbcont/lbcont.c \
        lbcont/lbcont_cui.c \
        lbcont/lbcont_hnd.c \
        lbstate/lbstate.c \
        lbstate/lbstate_cui.c \
        lbstate/lbstate_hnd.c \
        lbdisplay/lbdisplay.c \
        lbdisplay/lbdisplay_cui.c \
        lbdisplay/lbdisplay_hnd.c \
	locbif_mem_go.c

DEPS_A = lbmain_cui.h lbsolver/lbsolver_cui.h lbdisplay/lbdisplay_cui.h lbcont/lbcont_cui.h lbstate/lbstate_cui.h complbf.h

SOURCES_B = 

DEPS_B = 

TEST_PROGRAM = 

TEST_SOURCES = 

TEST_DEPS =

##############################################
# The following line should not be modified  #
##############################################

include ${DSTOOL}/site_specific/win_targetsA.mk

