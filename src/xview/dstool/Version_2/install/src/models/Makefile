############################################
# dstool - modellib  makefile              #
############################################

include ${DSTOOL}/site_specific/lib_incl.mk
############################################
# Modify the following segment only.       #
############################################
LIBRARY = modellib.a 

SOURCES_A = \
	modellib_def.c

DEPS_A = \
	$(DSTOOL)/src/include/mult_proj.h \
	$(DSTOOL)/src/include/pm.h

SOURCES_B = \
	models.c \
	lorenz_def.c \
	standard_def.c \
	cubic_def.c \
	hh_def.c \
	vdpol_def.c \
	pendulum1d_def.c \
	d4_def.c \
	kotorusmap_def.c \
	henon_def.c \
	d3_symm_def.c \
	double_toroid_def.c \
	duffing_def.c \
	field_worfolk_def.c \
	logistic_def.c \
	mathieu_def.c \
	perturbed_dtoroid_def.c \
	rla_def.c \
	simple_torus_def.c \
	symm_red_def.c \
	four_param_def.c \
	lp4_def.c

DEPS_B = \
	$(DSTOOL)/src/include/model_headers.h \
	$(DSTOOL)/src/include/ds_define.c \
	$(DSTOOL)/src/include/pm.h

TEST_PROGRAM = 

TEST_SOURCES = 

TEST_DEPS =

##############################################
# The following line should not be modified  #
##############################################

include ${DSTOOL}/site_specific/targetsAB.mk

