############################################
# dstool - twoDlib  makefile               #
############################################

include ${DSTOOL}/site_specific/lib_incl.mk
############################################
# Modify the following segment only.       #
############################################
LIBRARY = twoDlib.a 

SOURCES_A = \
	twoD.c twoD_cui.c twoD_hnd.c \
	mouse.c plot_orbit.c twoD_pm.c \
	twoD_opt.c twoD_opt_hnd.c twoD_opt_cui.c \
	print.c print_cui.c print_hnd.c print_procs.c

DEPS_A = \
	twoD_cui.h \
	twoD_opt_cui.h \
	twoD.h \
	twoD_def.h \
	twoD_opt.h \
	twoD_opt_def.h \
	twoD_ip.h \
	twoD.icon \
	owner.icon \
	print_ui.h \
	$(DSTOOL)/src/include/constants.h \
	$(DSTOOL)/src/include/filters.h \
	$(DSTOOL)/src/include/complib.h \
	$(DSTOOL)/src/include/pm.h \
	$(DSTOOL)/src/windows/ui_init.h \
	$(DSTOOL)/src/windows/plot.h \
	$(DSTOOL)/src/include/print.h

SOURCES_B = \
	gcm.c \
	error_util.c \
	color_filter.c \
	traj_plot.c \
	complete.c complete_name.c expand.c env_expand.c


DEPS_B = \
	$(DSTOOL)/src/windows/gcc.h \
	$(DSTOOL)/src/windows/gcm.h \
	$(DSTOOL)/src/windows/gdd.h \
	$(DSTOOL)/src/include/pm.h

TEST_PROGRAM = 

TEST_SOURCES = 

TEST_DEPS =

##############################################
# The following line should not be modified  #
##############################################

include ${DSTOOL}/site_specific/win_targetsAB.mk

