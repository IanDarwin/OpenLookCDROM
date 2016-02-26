#
#	This Makefile will not work with the
#	pre-SunPro version of make (i.e., any
#	pre-SunOS 3.5 default installation),
#	and will probably not work with any
#	non-Sun version of make, as it relies
#	heavily on SunOS 3.5 enhancements
#	to the original make utility.
#
#	ARCH must be set in the environment
#	to the architecture type of the machine,
#	e.g., sun3, sun4, etc.
#
#	FLOAT may optionally be set in the
#	environment to an appropriate floating
#	point compile flag, e.g., -f68881, -ffpa, etc.
#
#	BACKUPDEV should be set to an appropriate
#	backup device.
#
.KEEP_STATE:

.INIT:
	@if [ ! -d $(ARCH) ]; then mkdir $(ARCH); fi

CFLAGS= $(FLOAT) -g -D$(ARCH) -I$(OPENWINHOME)/include
#CFLAGS= $(FLOAT) -D$(ARCH) -I$(OPENWINHOME)/include

CCFLAGS= $(CFLAGS)

FTPDIR=	/lamont/lamont/ftp/pub

ICECPSSRCS= \
	icecps-$(ARCH).cps

ICECCSRCS= \
	Grobj.cc\
	Psdoc.cc\
	Raster.cc\
	Intobj.cc\
	Text.cc\
	Vector.cc\
	Curve.cc\
	Marker.cc\
	Rectangle.cc\
	Polygon.cc\
	Axis.cc\
	Composite.cc\
	Pathdir.cc\
	Path.cc\
	Pathdup.cc\
	Fontobj.cc\
	globals.cc\
	ice.cc\
	init.cc\
	xdefaults.cc\
	docpath.cc\
	psfonts.cc\
	error.cc\
	event.cc\
	cmap.cc\
	menuproc.cc\
	page.cc\
	gdfattr.cc\
	insattr.cc\
	select.cc\
	locate.cc\
	dump.cc\
	insice.cc\
	crosshairs.cc\
	cnvarray.cc\
	copy.cc\
	PsdocIns.cc\
	PsdocAttr.cc\
	PsdocTr.cc\
	PsdocCp.cc\
	RasterIns.cc\
	RasterAttr.cc\
	RasterTr.cc\
	RasterCp.cc\
	RasterMisc.cc\
	TextIns.cc\
	TextAttr.cc\
	TextTr.cc\
	TextCp.cc\
	VectorIns.cc\
	VectorAttr.cc\
	VectorTr.cc\
	VectorCp.cc\
	VectorMv.cc\
	CurveIns.cc\
	CurveAttr.cc\
	CurveTr.cc\
	CurveCp.cc\
	CurveMv.cc\
	MarkerIns.cc\
	MarkerAttr.cc\
	MarkerTr.cc\
	MarkerCp.cc\
	RectangleIns.cc\
	RectangleAttr.cc\
	RectangleTr.cc\
	RectangleCp.cc\
	PolygonIns.cc\
	PolygonAttr.cc\
	PolygonTr.cc\
	PolygonCp.cc\
	AxisIns.cc\
	AxisAttr.cc\
	AxisTr.cc\
	AxisCp.cc\
	AxisMv.cc\
	CompositeBd.cc\
	CompositeRd.cc\
	CompositeAttr.cc\
	CompositeUbd.cc\
	CompositeAdd.cc\
	CompositeRm.cc\
	CompositeOp.cc\
	CompositeTr.cc\
	CompositeCp.cc\
	PathIns.cc\
	PathAttr.cc\
	PathTr.cc

ICEOBJS= $(ICECPSSRCS:%.cps=$(ARCH)/%.o) $(ICECCSRCS:%.cc=$(ARCH)/%.o)

LIBFLAGS= -L$(OPENWINHOME)/lib -lcps -llxt -lX11 -lpixrect -lldgoc++ -lm

LDGOLIBS=	/usr/lib/liblxt.a /usr/lib/libldgoc++.a

prog:	$(ARCH)/ice

static:	$(ARCH)/ice-static

strip:	prog
	cp $(ARCH)/ice $(ARCH)/ice-debug
	strip $(ARCH)/ice

all:	prog strip

install:	installexec installman

icecps-$(ARCH).c:	icecps-$(ARCH).cps

.cps.c:
	$(OPENWINHOME)/bin/cps -i $*.cps

icecps.h:	icecps-$(ARCH).cps

.cps.h:
	$(OPENWINHOME)/bin/cps -i $*.cps

$(ARCH)/ice: $(ICEOBJS) $(LDGOLIBS)
	CC -o $@ $(CCFLAGS) $(ICEOBJS) $(LIBFLAGS)

$(ARCH)/ice-static: $(ICEOBJS) $(LDGOLIBS)
	CC -o $@ $(CCFLAGS) -Bstatic $(ICEOBJS) $(LIBFLAGS)

$(ARCH)/%.o: %.cc
	$(COMPILE.cc) $(@F:.o=.cc) -o $@

$(ARCH)/%.o: %.c
	cc $(CFLAGS) -c $(@F:.o=.c) -o $@

installexec:	installexec-$(ARCH)

installexec-sun3:	sun3/ice
	cp $? /duke/duke/sun3
	cp $? /geo/geo/sun3
	cp $? /ocean/ocean/sun3

installexec-sun4:	sun4/ice
	cp $? /trane/src/sun4
	cp $? /chaos/chaos/sun4
	cp $? /geo/geo/sun4
	cp $? /ocean/ocean/sun4

installman:	ice.1
	cp $? /chaos/chaos/man/man1
	cp $? /geo/geo/man/man1
	cp $? /ocean/ocean/man/man1

backup:	FRC
	tar cvf $(BACKUPDEV) *.cc *.cps *.c *.h README Makefile BLURB BUGS ice.1 doc *.bitmap

dist:	FRC
	tar cvf - *.cc *.cps *.h README NOTES Makefile ice.1 *.bitmap | compress >ice.tar.Z
	-cp ice.tar.Z $(FTPDIR)

clean:	FRC
	-@rm $(ARCH)/*.o $(ARCH)/ice

FRC:

