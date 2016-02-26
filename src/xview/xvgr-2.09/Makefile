# $Id: Makefile,v 1.46 92/08/15 15:54:53 pturner Exp Locker: pturner $
#
# Makefile for xvgr Release 2.0
#

#
# HP variables you should check
##################################
HPX11INCLUDES=/usr/include/X11R4
XVIEWINCLUDES=/usr/local/lib/xview3/include
HPX11LIBS=/usr/lib/X11R4
LOCALLIBS=/usr/local/lib

EXTRAOBJS = /tmp/signal.o /tmp/sigvec.o /tmp/getwd.o

# you may wish to change these
##################################
MANDIR = /usr/local/X11/man/man1
MANSECT = 1
DESTDIR = /usr/local/X11/bin

# Location of libraries
LIBS = -L$(LOCALLIBS) -lxview -lolgx -L$(HPX11LIBS) -lX11 -lm

#
# xvgr now has context sensitive help, see the User's manual
# on using this feature. OpenWindows has a spot for this.
HELPDIR = /usr/local/help

#
# standard location for systems using OpenWindows rather than MIT X
#INCLUDES = -I/usr/openwin/include
# this is where we have them
INCLUDES = -I$(HPX11INCLUDES) -I$(XVIEWINCLUDES)

CC = cc -Wp,-H500000

#MY_OSVERSION = -DSVR4
MY_OSVERSION =

#################################

# Install a printer
#

# The default is PostScript landscape using "lpr -h" to spool
# if this is OK then skip this Steps 1 and 2

# Step 1.
#
# To install the default hardcopy device uncomment ONE of the
# following (HARDCOPYDEV lines), the hardcopy device can also 
# be set using environment variables, and on the command line:
#
# PostScript landscape [device number 1]
# HARDCOPYDEV = -DHDEV=GR_PS_L 
#
# PostScript portrait [device number 2]
#HARDCOPYDEV = -DHDEV=GR_PS_P 
#
# FrameMaker MIF landscape [device number 3]
#HARDCOPYDEV = -DHDEV=GR_MIF_L 
#
# FrameMaker MIF portrait [device number 4]
#HARDCOPYDEV = -DHDEV=GR_MIF_P 
#
#  HP 8.5x11 landscape [device number 5]
HARDCOPYDEV = -DHDEV=GR_HPGL_L
#
#  HP 8.5x11 Portrait [device number 6]
#HARDCOPYDEV = -DHDEV=GR_HPGL_P
#
# InterLeaf landscape [device number 7]
#HARDCOPYDEV = -DHDEV=GR_LEAF_L 
#
# InterLeaf portrait [device number 8]
#HARDCOPYDEV = -DHDEV=GR_LEAF_P 
#

# Step 2.
#
# To install the printer for the hardcopy devices above
# set the following to the appropriate values for your site.
# These can also be set by environment variables. The current
# definitions of these variables are the same as the definitions
# in the drivers, ps.c (PostScript), and mif.c (Maker 
# Interchange format).
# 
# For Postscript
# 
#PS_PRSTR = -DPS_PRSTR='"lpr -h "'
# 
# For FrameMaker MIF format (really goes to a file)
# 
#MIF_PRSTR = -DMIF_PRSTR='"cat >xvgr.mif < "'
#
# For HPGL
# The driver will work for most (probably all) HPGL devices
#
HP_PRSTR1 = -DHP_PRSTR1='"/usr/remote/bin/lpr -Phes_jet "'
# 
# For InterLeaf format (really goes to a file)
# 
#LEAF_PRSTR = -DLEAF_PRSTR='"cat >xvgr.leaf < "'


# Setup scratch arrays
#
# Xvgr uses 4 scratch arrays, designated in the function that
# interprets expressions as a, b, c, and d. MAXARR is the amount
# of memory that will be allocated for these arrays and at present
# there is no provision for dynamically extending the length of these 
# arrays. This sets a limit on the length of a set inasmuch
# as no operation between sets and the scratch arrays should exceed 
# this amount. MAXARR does not set a limit on the size of a set, on our 
# Sun4 (32Mb) we run MAXARR = 60000, I've plotted data sets in excess 
# of 700k points - the limit is available memory.
#
MAXARR = -DMAXARR=20480

###############################
# End installation parameters #
###############################

# experimenting with drag 'n drop
#DND = -DDND

CFLAGS = -O -DLOCAL -DXVIEW $(MY_OSVERSION) $(DND) $(INCLUDES) $(MAXARR) $(HARDCOPYDEV) $(PS_PRSTR) $(MIF_PRSTR) $(HP_PRSTR1) $(LEAF_PRSTR)

# pars.c is now built outside this Makefile
# YFLAGS = -yt

SRCS =  main.c\
	xvgr.c\
	events.c\
	alerts.c\
	fileswin.c\
	blockwin.c\
	eblockwin.c\
	printwin.c\
	drawwin.c\
	propswin.c\
	pagewin.c\
	miscwin.c\
	parmsiowin.c\
	comwin.c\
	compwin.c\
	nonlwin.c\
	setwin.c\
	ptswin.c\
	editpwin.c\
	regionwin.c\
	statuswin.c\
	symwin.c\
	tickwin.c\
	graphwin.c\
	worldwin.c\
	strwin.c\
	labelwin.c\
	locatewin.c\
	framewin.c\
	monwin.c\
	stubs.c\
	plotone.c\
	files.c\
	utils.c\
	drawticks.c\
	fit.c\
	fourier.c\
	io.c\
	graphutils.c\
	graphutils2.c\
	setutils.c\
	setprops.c\
	regionutils.c\
	objutils.c\
	computils.c\
	lmdif1.c\
	defaults.c\
	checkon.c\
	params.c\
	getparms.c\
	compute.c\
	xvlib.c\
	draw.c\
	ps.c\
	mif.c\
	leaf.c\
	hp.c\
	chersh.c\
	pars.c

OBJS =  main.o\
	xvgr.o\
	events.o\
	alerts.o\
	fileswin.o\
	blockwin.o\
	eblockwin.o\
	printwin.o\
	drawwin.o\
	propswin.o\
	pagewin.o\
	miscwin.o\
	parmsiowin.o\
	comwin.o\
	compwin.o\
	nonlwin.o\
	setwin.o\
	ptswin.o\
	editpwin.o\
	regionwin.o\
	statuswin.o\
	symwin.o\
	tickwin.o\
	graphwin.o\
	worldwin.o\
	strwin.o\
	labelwin.o\
	locatewin.o\
	framewin.o\
	monwin.o\
	plotone.o\
	stubs.o\
	files.o\
	utils.o\
	drawticks.o\
	fit.o\
	fourier.o\
	io.o\
	graphutils.o\
	graphutils2.o\
	setutils.o\
	setprops.o\
	regionutils.o\
	objutils.o\
	computils.o\
	lmdif1.o\
	defaults.o\
	checkon.o\
	params.o\
	getparms.o\
	compute.o\
	xvlib.o\
	draw.o\
	ps.o\
	mif.o\
	leaf.o\
	hp.o\
	chersh.o\
	pars.o

D3DINCS = defs.h

# for printing and making a distribution
#
INCS = defines.h externs.h globals.h pars.h symdefs.h f2c.h patterns.h icon.h icon_mask.h patchlevel.h
HERSHINCS = hersh.h special.h
DISTDIR = xvgr-2.09

xvgr: $(OBJS) $(EXTRAOBJS)
	$(CC) $(OBJS) $(EXTRAOBJS) -o xvgr $(LIBS) 

#
# most folks don't have bison
# so pars.c is built outside this Makefile
#
#pars.o: pars.yacc
#	bison $(YFLAGS) pars.yacc
#	$(CC) $(CFLAGS) -c y.tab.c
#	mv y.tab.o pars.o
#	/bin/rm y.tab.c

main.o: Makefile defines.h globals.h
xvgr.o: defines.h globals.h
pars.o: pars.c defines.h globals.h
calc.o: defines.h globals.h
compute.o: defines.h globals.h
fit.o: defines.h
files.o: defines.h globals.h
io.o: defines.h

params.o: defines.h globals.h 
getparams.o: defines.h globals.h 

plotone.o: defines.h globals.h symdefs.h
drawticks.o: defines.h globals.h 

utils.o: defines.h globals.h
setutils.o: defines.h globals.h
setprops.o: defines.h globals.h
regionutils.o: defines.h globals.h
graphutils.o: defines.h globals.h
graphutils2.o: defines.h globals.h
objutils.o: defines.h globals.h 
stubs.o: defines.h globals.h

checkon.o: defines.h globals.h 

#
# user interface
#
graphwin.o: defines.h globals.h
compwin.o: defines.h globals.h
nonlwin.o: defines.h globals.h
setwin.o: defines.h globals.h
framewin.o: defines.h globals.h
strwin.o: defines.h globals.h
worldwin.o: defines.h globals.h
symwin.o: defines.h globals.h
ptswin.o: defines.h globals.h
editpwin.o: defines.h globals.h
drawwin.o: defines.h globals.h
statuswin.o: defines.h globals.h patchlevel.h
printwin.o: defines.h globals.h
labelwin.o: defines.h globals.h
locatewin.o: defines.h globals.h
fileswin.o: defines.h globals.h
blockwin.o: defines.h globals.h
eblockwin.o: defines.h globals.h
parmsiowin.o: defines.h globals.h
comwin.o: defines.h globals.h

defaults.o: defines.h globals.h

#
# graphics drivers
#
chersh.o: hersh.h special.h
draw.o: symdefs.h
mif.o: externs.h
hp.o: externs.h
ps.o: externs.h
xvlib.o: externs.h
leaf.o: externs.h

#globals.h: pars.h

#pars.h:
#	bison -dyt pars.yacc
#	/bin/mv y.tab.h pars.h

install:
	install -s -m 751 xvgr $(DESTDIR)
	install -c -m 644 xvgr.man $(MANDIR)/xvgr.$(MANSECT)
	install -c -m 644 xvgr.info $(HELPDIR)/xvgr.info
	rm $(DESTDIR)/grbatch
	ln -s $(DESTDIR)/xvgr $(DESTDIR)/grbatch

lint:
	lint  $(INCLUDES) $(SRCS)

clean: 
	/bin/rm xvgr $(OBJS)

distrib:
	/bin/cp -p Imakefile Makefile INSTALL README README.EPS COPYRIGHT xvgr.man xvgr.info $(SRCS)\
		$(INCS) $(HERSHINCS) pars.yacc $(DISTDIR)
	/bin/cp -p Makefile $(DISTDIR)/Makefile.orig
	/bin/cp -pr examples $(DISTDIR)
	mkdir $(DISTDIR)/doc
	/bin/cp -p doc/xvgr.ps $(DISTDIR)/doc
	mkdir $(DISTDIR)/aux
	/bin/cp -p aux/jul2greg.c $(DISTDIR)/aux/jul2greg.c
	/bin/cp -p aux/greg2jul.c $(DISTDIR)/aux/greg2jul.c
	/bin/cp -p aux/cnvparm.c $(DISTDIR)/aux/cnvparm.c
	/bin/cp -p aux/README $(DISTDIR)/aux/README

tarfile:
	tar cvf $(DISTDIR).tar $(DISTDIR)
	compress $(DISTDIR).tar

vi:
	vi Makefile README $(SRCS) $(INCS) hersh.h special.h

wc:
	wc Makefile README $(SRCS) $(INCS) hersh.h special.h

rcs: Makefile README $(SRCS) $(INCS) pars.yacc hersh.h special.h
	ci -l $?
	touch rcs

/tmp/sigvec.o:
	(cd /tmp ; ar -x /usr/lib/libBSD.a sigvec.o )

/tmp/signal.o:
	(cd /tmp ; ar -x /usr/lib/libBSD.a signal.o )

/tmp/getwd.o:
	(cd /tmp ; ar -x /usr/lib/libBSD.a getwd.o )

