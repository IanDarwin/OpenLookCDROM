#
#
# Master makefile for dstool
#
#
SHELL = /bin/sh
PROGRAM = dstool
PROGRAM_NOWIN = dstool_nowin
PVM_SLAVE = dstool_pvm
DSLIBS = dslibs
CLEAN = clean

MAINLIB = $(DSTOOL)/src/main/mainlib.a
MAINLIB_NOWIN = $(DSTOOL)/src/main/mainlib_nowin.a

PRE_LIBS = \
	$(DSTOOL)/src/saveload/saveloadlib.a \
	$(DSTOOL)/src/mult/multlib.a \
	$(DSTOOL)/src/complib/complib.a \
	$(DSTOOL)/src/postmaster/pmlib.a \
	$(DSTOOL)/src/pvm/pvmlib.a \
 	$(DSTOOL)/src/memory/memorylib.a

POST_LIBS = \
	$(DSTOOL)/src/complib/ALGORITHMS/alglib.a \
	$(DSTOOL)/src/complib/INTEGRATION/integlib.a \
	$(DSTOOL)/src/complib/ITERATION/iterlib.a \
	$(DSTOOL)/src/fixptlib/fixptlib.a \
	$(DSTOOL)/src/eigenlib/eigenlib.a \
 	$(DSTOOL)/src/math_utilities/math_utillib.a \
	$(DSTOOL)/src/tcl/tcllib.a \
 	$(DSTOOL)/src/utilities/utillib.a


WIN_LIBS = \
	$(DSTOOL)/src/windows/windowlib.a \
	$(DSTOOL)/src/windows/cmd/cmdlib.a \
	$(DSTOOL)/src/twoD/twoDlib.a \
	$(DSTOOL)/src/windows/defaults/defaultslib.a \
	$(DSTOOL)/src/windows/periodic/periodiclib.a \
	$(DSTOOL)/src/windows/save/savelib.a \
	$(DSTOOL)/src/windows/load/loadlib.a \
	$(DSTOOL)/src/windows/function/functionlib.a \
	$(DSTOOL)/src/windows/mult/multwinlib.a \
	$(DSTOOL)/src/windows/orbit/orbitlib.a \
	$(DSTOOL)/src/windows/prop/proplib.a \
	$(DSTOOL)/src/windows/selected/selectlib.a

LIBS = \
	$(DSTOOL_LIBS)/mainlib.a \
	$(DSTOOL_LIBS)/batchlib.a \
	$(DSTOOL_LIBS)/uilib.a \
	$(DSTOOL_LIBS)/cmdlib.a \
	$(DSTOOL_LIBS)/twoDlib.a \
	$(DSTOOL_LIBS)/defaultslib.a \
	$(DSTOOL_LIBS)/periodiclib.a \
	$(DSTOOL_LIBS)/savelib.a \
	$(DSTOOL_LIBS)/loadlib.a \
	$(DSTOOL_LIBS)/printlib.a \
	$(DSTOOL_LIBS)/functionlib.a \
	$(DSTOOL_LIBS)/multlib.a \
	$(DSTOOL_LIBS)/orbitlib.a \
	$(DSTOOL_LIBS)/proplib.a \
	$(DSTOOL_LIBS)/complib.a \
	$(DSTOOL_LIBS)/alglib.a \
	$(DSTOOL_LIBS)/integlib.a \
	$(DSTOOL_LIBS)/iterlib.a \
	$(DSTOOL_LIBS)/selectlib.a \
	$(DSTOOL_LIBS)/fixptlib.a \
	$(DSTOOL_LIBS)/eigenlib.a \
 	$(DSTOOL_LIBS)/memorylib.a \
	$(DSTOOL_LIBS)/storagelib.a \
 	$(DSTOOL_LIBS)/math_utillib.a \
 	$(DSTOOL_LIBS)/utillib.a \
	$(DSTOOL_LIBS)/guidelib.a


MODELLIB = $(DSTOOL)/src/models/modellib.a
USERLIB = $(DSTOOL)/src/userlib/userlib.a

DEPENDENT_LIBS = $(MODELLIB) $(USERLIB)

USER_WIN_LIBS = \
	$(DSTOOL)/contrib/browser/browserlib.a \
	$(DSTOOL)/contrib/parserwin/parserwinlib.a \
	$(DSTOOL)/contrib/tclwin/tclwinlib.a \
	$(DSTOOL)/contrib/oned/onedlib.a \
	$(DSTOOL)/contrib/geomview/geomviewlib.a \
	$(DSTOOL)/contrib/cont/contlib.a \
	$(DSTOOL)/contrib/porbit/porbitlib.a \
	$(DSTOOL)/contrib/locbif/locpanels/lbfpanels.a \
	$(DSTOOL)/contrib/locbif/loclbf/lb_main.a \
	$(DSTOOL)/contrib/autoif/autoiflib.a

#	$(DSTOOL)/src/trial/triallib.a

#F_LIBS = \
#	/misc/lang/fortran_1.4/SC1.0/libF77.a \
#	/misc/lang/fortran_1.4/SC1.0/libm.a

#	/opt/SUNWspro/SC2.0.1/libF77.a \
#	/opt/SUNWspro/SC2.0.1/libM77.a \
#	/opt/SUNWspro/SC2.0.1/libsunmath.a 

#	/opt/SUNWspro/SC2.0.1/libFxview.a \
#	/opt/SUNWspro/SC2.0.1/libV77.a \
#	/opt/SUNWspro/SC2.0.1/libpfc.a


### Derived parameters.

OBJECTS = $(SOURCES.c:.c=.o)
ALL_LIBS = $(MAINLIB) $(MAINLIB_NOWIN) \
	$(PRE_LIBS) $(WIN_LIBS) $(DEPENDENT_LIBS) $(USER_WIN_LIBS) $(POST_LIBS)

CLEAN_LIST = \
	$(MAINLIB:.a=.clean) \
	$(MAINLIB_NOWIN:.a=.clean) \
	$(PRE_LIBS:.a=.clean) \
	$(POST_LIBS:.a=.clean) \
	$(WIN_LIBS:.a=.clean) \
	$(DEPENDENT_LIBS:.a=.clean) \
	$(USER_WIN_LIBS:.a=.clean)
FILES_LIST = \
	$(MAINLIB:.a=.dir) \
	$(MAINLIB_NOWIN:.a=.dir) \
	$(PRE_LIBS:.a=.dir) \
	$(POST_LIBS:.a=.dir) \
	$(WIN_LIBS:.a=.dir) \
	$(DEPENDENT_LIBS:.a=.dir) \
	$(USER_WIN_LIBS:.a=.dir)

include $(DSTOOL)/site_specific/lib_incl.mk


### Standard targets.

$(PROGRAM): $(MAINLIB) $(PRE_LIBS) $(DEPENDENT_LIBS) \
	$(WIN_LIBS) $(USER_WIN_LIBS) $(POST_LIBS)
	$(LINK.c) $(MAINLIB) $(PRE_LIBS) $(MODELLIB) \
		$(USER_LIBS) $(WIN_LIBS) $(USERLIB) $(USER_WIN_LIBS) $(POST_LIBS) \
		$(WINLDFLAGS) $(LDFLAGS) $(LDLIBS)\
		-o $(DSTOOL)/bin/$(PROGRAM)

$(PROGRAM_NOWIN): $(MAINLIB_NOWIN) $(PRE_LIBS) $(MODELLIB) $(POST_LIBS)
	$(LINK.c) $(MAINLIB_NOWIN) $(PRE_LIBS) $(MODELLIB) $(DSTOOL)/src/userlib/mult_proj.o $(POST_LIBS) \
		$(LDFLAGS) $(LDLIBS) -o $(DSTOOL)/bin/$(PROGRAM_NOWIN)

$(PVM_SLAVE): $(PROGRAM_NOWIN)
	$(RM) $(HOME)/pvm/$(ARCH)/$(PVM_SLAVE)
	ln -s $(DSTOOL)/bin/$(PROGRAM_NOWIN) $(HOME)/pvm/$(ARCH)/$(PVM_SLAVE)

$(DSLIBS): $(ALL_LIBS)
	for i in $(ALL_LIBS); do \
	(cp $$i $(DSTOOL)/libraries); \
	done
	ranlib -t $(DSTOOL)/libraries/*.a

INSTALL_LIBS = $(ALL_LIBS:.a=.install)

install: $(INSTALL_LIBS)
	
$(INSTALL_LIBS):
	cd $(@D); $(MAKE) install

all: $(PROGRAM) $(PROGRAM_NOWIN) $(DSLIBS)

$(MAINLIB) $(PRE_LIBS) $(WIN_LIBS) $(USER_WIN_LIBS) $(POST_LIBS): FORCE
	cd $(@D) ; $(MAKE) $(@F)

$(MAINLIB_NOWIN): FORCE
	cd $(@D) ; $(MAKE) -f Makefile_nowin $(@F)

$(DEPENDENT_LIBS): FORCE
	cd $(@D) ; $(MAKE) $(@F) 

$(CLEAN): $(CLEAN_LIST)
	$(RM) *~ include/*~ *% include/*%

$(CLEAN_LIST): FORCE
	cd $(@D); $(MAKE) $(CLEAN)

listallfiles: 
	@echo $(PWD)/Makefile
	@make $(FILES_LIST)

$(FILES_LIST):
	@cd $(@D); make listallfiles "CWD = $(@D)"

filelist: FORCE
	@make listallfiles | sort -u > filelist

FORCE:

