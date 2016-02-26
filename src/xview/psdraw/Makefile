#
# @(#)Makefile	2.5 91/10/15 Copyright 1991 Sun Microsystems
#
# This file is a product of Sun Microsystems, Inc. and is provided for
# unrestricted use provided that this legend is included on all tape
# media and as a part of the software program in whole or part.  Users
# may copy or modify this file without charge, but are not authorized to
# license or distribute it to anyone else except as part of a product
# or program developed by the user.
# 
# THIS FILE IS PROVIDED AS IS WITH NO WARRANTIES OF ANY KIND INCLUDING THE
# WARRANTIES OF DESIGN, MERCHANTIBILITY AND FITNESS FOR A PARTICULAR
# PURPOSE, OR ARISING FROM A COURSE OF DEALING, USAGE OR TRADE PRACTICE.
# 
# This file is provided with no support and without any obligation on the
# part of Sun Microsystems, Inc. to assist in its use, correction,
# modification or enhancement.
# 
# SUN MICROSYSTEMS, INC. SHALL HAVE NO LIABILITY WITH RESPECT TO THE
# INFRINGEMENT OF COPYRIGHTS, TRADE SECRETS OR ANY PATENTS BY THIS FILE
# OR ANY PART THEREOF.
# 
# In no event will Sun Microsystems, Inc. be liable for any lost revenue
# or profits or other special, indirect and consequential damages, even
# if Sun has been advised of the possibility of such damages.
# 
# Sun Microsystems, Inc.
# 2550 Garcia Avenue
# Mountain View, California  94043
#

# Parameters.

PROGRAM = psdraw
SOURCES.c = psdraw.c
SOURCES.h = point.pr line.pr rectangle.pr circle.pr ellipse.pr text.pr  \
	    hold.pr join.pr
SOURCES.cps = psdraw.cps
SOURCES.G = psdraw.G
STUBS.G =

# Derived parameters.

SOURCES = \
	$(SOURCES.G) \
	$(SOURCES.h) \
	$(SOURCES.l) \
	$(SOURCES.y) \
	$(SOURCES.cps) \
	$(SOURCES.c)

TARGETS.c = \
	$(SOURCES.G:%.G=%_ui.c) \
	$(STUBS.G:%.G=%_stubs.c)

TARGETS.h = \
	$(SOURCES.G:%.G=%_ui.h) \
	$(SOURCES.l:%.l=%.h) \
	$(SOURCES.y:%.y=%.h) \
	$(SOURCES.cps:%.cps=%.h)

TARGETS.other = \
	$(SOURCES.G:%.G=%.info)

TARGETS = \
	$(TARGETS.other) \
	$(TARGETS.h) \
	$(TARGETS.c)

OBJECTS = \
	$(SOURCES.c:%.c=%.o) \
	$(TARGETS.c:%.c=%.o)

# Compiler flags.

CFLAGS += -g
CPPFLAGS += -I$(GUIDEHOME)/include -I$(OPENWINHOME)/include -DMAIN
LDFLAGS += -L$(GUIDEHOME)/lib -L$(OPENWINHOME)/lib
LDLIBS += -lguidexv -lxvps -lxview -lolgx -lX11 -lcps -lm

# Standard targets.

all:	 $(TARGETS.other) $(PROGRAM)
objects: $(SOURCES.c) $(TARGETS.c) $(TARGETS.h) $(OBJECTS)
sources: $(SOURCES)
targets: $(SOURCES) $(TARGETS)

$(PROGRAM): $(SOURCES.c) $(TARGETS.c) $(TARGETS.h) $(OBJECTS)
	$(LINK.c) -o $@ $(OBJECTS) $(LDLIBS)

# Targets to be used by Saber-C.

saber_src:
	#load $(CFLAGS) $(CPPFLAGS) $(LDFLAGS) $(TARGET_ARCH) \
		$(SOURCES.c) $(TARGETS.c) $(LDLIBS)

saber_obj:
	#load $(CFLAGS) $(CPPFLAGS) $(LDFLAGS) $(TARGET_ARCH) $(OBJECTS) $(LDLIBS)

clean:
	$(RM) $(SOURCES.G:%.G=%_ui.c) $(TARGETS.other) $(TARGETS.h) $(OBJECTS) *.BAK *.delta core

%_ui.c: %.G
	$(GUIDEHOME)/bin/gxv  $*

%_ui.h: %_ui.c
	@touch $@

%_stubs.c: %_ui.c
	@touch $@

%.info: %_ui.c
	@touch $@
