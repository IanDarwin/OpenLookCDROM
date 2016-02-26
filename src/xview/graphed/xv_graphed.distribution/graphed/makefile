# (C) Universitaet Passau, Michael Himsolt 1986-1991
########################################################################
#									#
#			    GRAPHED MAKEFILE				#
#									#
#########################################################################

it :	graphed

#
# Sourcefiles
#

GRAPHEDDIR = ..

EXPORTABLE_CSOURCES = adjust.c canvas.c draw.c eedge.c error.c \
           find.c font.c ggraph.c graphed_mpr.c load.c main.c \
           menu.c misc.c nnode.c paint.c panel.c repaint.c \
           store.c textsw.c type.c user.c dispatch.c \
           create_mode.c select_mode.c group.c gragra.c print.c \
           config.c \
           la_paint.c postscri.c \
	   graphed_sgraph_interface.c graphed_sgragra_interface.c\
           buffer.c state.c \
           template.c about.c user_event_functions.c \
           gridder.c util.c drag.c p7+_colourprint.c extensions_config.c \
           graphed_svi.c \
           edge_sf.c file_sf.c font_sf.c node_sf.c type_sf.c \
           node_defaults_sf.c edge_defaults_sf.c group_sf.c print_sf.c \
           gragra_sf.c graph_sf.c

CSOURCES = $(EXPORTABLE_CSOURCES) $(SF_SOURCES) user_commands.c

HSOURCES = adjust.h draw.h error.h find.h font.h graph.h graphed_mpr.h \
           install.h load.h config.h menu.h misc.h paint.h repaint.h \
           store.h type.h user.h user_header.h group.h gragra.h \
           print.h la_paint.h postscri.h modula.h graphed_subwindows.h \
           graphed_sgraph_interface.h graphed_sgragra_interface.h \
	   user_commands.h dispatch_commands.h\
	   state.h user_event_functions.h gridder.h util.h \
           extensions_config.h graphed_svi.h

YSOURCES = parser.y
LSOURCES = scanner.l

MYFILES = $(HSOURCES) $(CSOURCES) $(YSOURCES) $(LSOURCES)

#
# Objectfiles
#

include makefile.extensions

EXPORTABLE_OBJECTFILES = $(EXPORTABLE_CSOURCES:%.c=%.o) scanner.o parser.o

OBJECTFILES = $(EXPORTABLE_OBJECTFILES) user_commands.o


#LINKLIBS  = -lsuntool -lsunwindow -lpixrect -lm -ll
#LINKLIBS  = -lsuntool_p -lsunwindow_p -lpixrect_p -lm_p -ll
LINKLIBS = -lpixrect -L/usr/openwin/lib -lxview -lolgx -lX11 -lm -ll

include $(GRAPHEDDIR)/makefile.template
CFLAGS += -g
LDFLAGS += -g
#
# How to make the programm
#
#CFLAGS  += -pg
#LDFLAGS += -pg -Bstatic

graphed :	$(OBJECTFILES)
		$(CC) $(LDFLAGS) $(OBJECTFILES) $(EXTENSIONS) $(LINKLIBS) -o graphed
#		$(CC) $(LDFLAGS) $(OBJECTFILES) $(LINKLIBS) -o graphed /usr/lib/debug/malloc.o

lib :		graphed.o
graphed.o :	$(EXPORTABLE_OBJECTFILES)
		ld -r -X $(EXPORTABLE_OBJECTFILES) $(EXTENSIONS) -o graphed.o
#		ld -r -X $(EXPORTABLE_OBJECTFILES) /usr/lib/debug/malloc.o -o graphed.o


all :		graphed lib

#
# Include here a file with the dependencies
#
include	makedepend

# some additional dependencies
scanner.c :	scanner.l	# cc -M cannot recognize lex and yacc ...
parser.c  :	parser.y
y.tab.h   :	parser.c
scanner.o :	parser.o


#
# install
#
install.local::	graphed lib install.local.makedirs install.local.graphed install.local.lib

install.local.makedirs:	FORCE
		if [ ! -d $(INSTALL_BINDIR) ]; \
		then \
			mkdir $(INSTALL_BINDIR); \
		fi
		if [ ! -d $(INSTALL_LIBDIR) ]; \
		then \
			mkdir $(INSTALL_LIBDIR); \
		fi

install.local.graphed:	FORCE
		cp ./graphed $(INSTALL_BINDIR)/$(INSTALLEDNAME)

install.local.lib:	FORCE
		if [ -f $(NEWLIBGRAPHED) ];                        \
		then                                               \
	                cp $(NEWLIBGRAPHED) $(INSTALL_LIBDIR);     \
		fi

#
# Goodies
#
extensions :		makefile.extensions
makefile.extensions :	makefile.extensions.cpp extensions_config.h
			/lib/cpp < makefile.extensions.cpp > makefile.extensions
