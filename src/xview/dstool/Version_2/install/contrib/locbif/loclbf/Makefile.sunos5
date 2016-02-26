include ${DSTOOL}/site_specific/lib_incl.mk


COMMON=algebrbf beetlebf complbf linbf integrbf ifaces
VER1=epbf epinf 
VER2=fpbf fpinf linfpbf
VER3=lcbf lcinf linfpbf
VER4=linfpbf psbf psinf

ROOT_DIR= $(DSTOOL)/contrib/locbif/loclbf
MKMKF=$(ROOT_DIR)/scripts/mkmkf.sh

LIBRARY = lb_main.a 

FSOURCES = \
	lbfmain.F \
	setch.F \
	funtds.F \
	algebrbf/dbalan.F \
	algebrbf/decomp.F \
	algebrbf/delmhe.F \
	algebrbf/detbf.F \
	algebrbf/dhqr.F \
	algebrbf/dmfgr.F \
	algebrbf/dminv.F \
	algebrbf/dsimq.F \
	algebrbf/dvgaus.F \
	algebrbf/koeev1.F \
	algebrbf/ortg2.F \
	algebrbf/plo.F \
	algebrbf/rzltnt.F \
	algebrbf/solve.F \
	beetlebf/beetle.F \
	beetlebf/cnewtn.F \
	beetlebf/corr.F \
	beetlebf/decomp.F \
	beetlebf/dern.F \
	beetlebf/dfdx.F \
	beetlebf/extr.F \
	beetlebf/jacob.F \
	beetlebf/leqv.F \
	beetlebf/newtn.F \
	beetlebf/poisk.F \
	beetlebf/servce.F \
	beetlebf/slfcrs.F \
	beetlebf/solve.F \
	beetlebf/tangnt.F \
	beetlebf/wrtcrs.F \
	beetlebf/wrtdt.F \
	beetlebf/wrtend.F \
	beetlebf/wrtext.F \
	beetlebf/wrtout.F \
	beetlebf/wrtzer.F \
	beetlebf/zero.F \
	complbf/comp.F \
	complbf/diagno.F \
	complbf/endmes.F \
	complbf/errmsg.F \
	complbf/funt.F \
	complbf/funtds.F \
	complbf/lbdump.F \
	complbf/lbf.F \
	complbf/lbfmain.F \
	complbf/message.F \
	complbf/output.F \
	complbf/protoc.F \
	complbf/rdinit.F \
	complbf/recomp.F \
	complbf/setch.F \
	complbf/showit.F \
	complbf/store.F \
	complbf/storei.F \
	complbf/termnt.F \
	complbf/wrtcom.F \
	epbf/block2.F \
	epbf/der23l.F \
	epbf/dervar.F \
	epbf/detjac.F \
	epbf/detkrv.F \
	epbf/detntv.F \
	epbf/detrg.F \
	epbf/detvpr.F \
	epbf/dxp1kr.F \
	epbf/dxp1nt.F \
	epbf/dxp1p2.F \
	epbf/epbdata.F \
	epbf/epcodebf.F \
	epbf/epder1bf.F \
	epbf/epder1l.F \
	epbf/epder2q.F \
	epbf/epdetev.F \
	epbf/epdfunbf.F \
	epbf/epeiglbf.F \
	epbf/epfunall.F \
	epbf/epfunbf.F \
	epbf/eplinlbf.F \
	epbf/epquadr.F \
	epbf/epregpbf.F \
	epbf/epzerobf.F \
	epbf/funts.F \
	epbf/lyapun.F \
	epbf/vliap.F \
	epbf/vlp.F \
	epinf/epinitnm.F \
	epinf/eplinebf.F \
	epinf/epmessng.F \
	epinf/epoutpt.F \
	fpbf/fpbdata.F \
	fpbf/fpfunbf.F \
	fpbf/fpfunm.F \
	fpbf/fplinlbf.F \
	fpbf/fpmodjac.F \
	fpinf/fpinitnm.F \
	fpinf/fplinebf.F \
	fpinf/fpmessng.F \
	fpinf/fpoutpt.F \
	ifaces/bdata.F \
	ifaces/codebf.F \
	ifaces/der1bf.F \
	ifaces/der1l.F \
	ifaces/der2q.F \
	ifaces/detev.F \
	ifaces/dfunbf.F \
	ifaces/eiglbf.F \
	ifaces/funall.F \
	ifaces/funbf.F \
	ifaces/funm.F \
	ifaces/initname.F \
	ifaces/linebf.F \
	ifaces/linlbf.F \
	ifaces/messing.F \
	ifaces/modjac.F \
	ifaces/outpt.F \
	ifaces/quadr.F \
	ifaces/regpbf.F \
	ifaces/zerobf.F \
	integrbf/cldecsol.F \
	integrbf/cldop5.F \
	integrbf/clrad5.F \
	integrbf/drkts1.F \
	integrbf/integr.F \
	lcbf/fsecpl.F \
	lcbf/lcbdata.F \
	lcbf/lcfunbf.F \
	lcbf/lcfunm.F \
	lcbf/lclinlbf.F \
	lcbf/lcmodjac.F \
	lcinf/lcinitnm.F \
	lcinf/lclinebf.F \
	lcinf/lcmessng.F \
	lcinf/lcoutpt.F \
	linbf/crsbf.F \
	linbf/curntp.F \
	linbf/dervar.F \
	linbf/extrbf.F \
	linbf/funadd.F \
	linbf/funcvl.F \
	linbf/funs.F \
	linbf/initbf.F \
	linbf/khib.F \
	linbf/orbitc.F \
	linbf/orbitd.F \
	linbf/ordbf.F \
	linbf/outfun.F \
	linbf/outlbf.F \
	linbf/prntbf.F \
	linbf/secant.F \
	linbf/wline.F \
	linfpbf/detfld.F \
	linfpbf/detflp.F \
	linfpbf/dethpf.F \
	linfpbf/dp1fld.F \
	linfpbf/dp1flp.F \
	linfpbf/dp1hpf.F \
	linfpbf/fpcodebf.F \
	linfpbf/fpder1bf.F \
	linfpbf/fpder1l.F \
	linfpbf/fpder2q.F \
	linfpbf/fpdetev.F \
	linfpbf/fpdfunbf.F \
	linfpbf/fpeiglbf.F \
	linfpbf/fpfunall.F \
	linfpbf/fpquadr.F \
	linfpbf/fpregpbf.F \
	linfpbf/fpzerobf.F \
	linfpbf/trfld.F \
	linfpbf/trflp.F \
	linfpbf/trhpf.F \
	psbf/psbdata.F \
	psbf/psfunbf.F \
	psbf/psfunm.F \
	psbf/pslinlbf.F \
	psbf/psmodjac.F \
	psinf/psinitnm.F \
	psinf/pslinebf.F \
	psinf/psmessng.F \
	psinf/psoutpt.F 

##############################################
# The following lines should not be modified #
# except for working with test code          #
##############################################


#CFLAGS = -g -DDEBUG
DSFLAGS = -I${DSTOOL}/src/models -I${DSTOOL}/src/windows -I${DSTOOL}/src/include
#LDFLAGS = -g 

DSTOOL_LIBS = ${DSTOOL}/libraries
LDLIBS =  ${DSTOOL_LIBS}/complib.a ${DSTOOL_LIBS}/utillib.a ${DSTOOL_LIBS}/eigenlib.a -lm

FOBJECTSA = $(FSOURCES:%.F=%.o) 
COBJECTS = $(CSOURCES:%.c=%.o)

${LIBRARY}: ${COBJECTS} ${FOBJECTSA}
	ar rv $@ $?
#	ranlib $@

${FOBJECTSA}: $(FSOURCES)
	f77 $(CFLAGS) $(CPPFLAGS) $(DSFLAGS) -c $*.F -o $@

clean:
	\rm *.o *.a */*.o 


