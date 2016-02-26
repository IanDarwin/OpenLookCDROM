#
#   Makefile for building PhoneTool
#
#   This tool was designed for use in Sun's OpenWindows environment.  If
#   you are not installing this on a Sun running OpenWindows, there are
#   a few environment variables and directories you'll need to have.
#   The following are examples of how OpenWindows was set up on our
#   system:
#
#	setenv OPENWINHOME /usr/openwin
#	setenv HELPPATH $OPENWINHOME/lib/help
#
#   Set up the definitions below to match those on your system, and be
#   sure to have your HELPPATH environment variable set.
#
#   Then type 'make install' and all should be taken care of for you...
#
#   (Three icon files are included... you can rotate them in by changing
#   their names to phone.icon.  To prevent losing the current phone.icon,
#   move it to a temporary file first.)
#
#	___________________________________________________________
#	indi (Cathy Young)            Supercomputer Systems, Inc
#	uunet!ssi!young           2021 Las Positas Court, Suite 101
#	(415)373-8044                   Livermore, CA  94550


#
# Include files, libraries, and other macros.  Be sure to change 
# these to match your own system.
#

HOME = /home/ugiashak/young
OPENWINHOME = /usr/openwin

INFOFILE = phonetool.info
MANPAGE = phonetool.1
STARTUPFILE = .phonerc

INSTALLDIR = /usr/local/bin
INFODIR = $(OPENWINHOME)/lib/help
MANDIR = /usr/man/man1
HOMEDIR = $(HOME)

#XLIBS = -L$(OPENWINHOME)/lib -lxview -lolgx -lX11 /stage/lib.linux/uflow.o
XLIBS = -L$(OPENWINHOME)/lib -lxview -lolgx -lX11
XINCLUDE = -I$(OPENWINHOME)/include -I$(OPENWINHOME)/include/xview

#
# Set CPP flags here to the things you really want.
#

CPPFLAGS = -g $(XINCLUDES)

EXECUTABLE = phonetool

#
# Source file definitions
#

SOURCES = 				\
	phonetool_data.c		\
	phonetool_build.c		\
	phonetool.c

OBJECTS =       $(SOURCES:.c=.o)

all: $(EXECUTABLE)

install: $(EXECUTABLE) $(INFOFILE) $(MANPAGE)
	install -m 7555 $(EXECUTABLE) $(INSTALLDIR)
	install -m 0644 $(INFOFILE) $(INFODIR)
	install -m 0644 $(MANPAGE) $(MANDIR)
	install -m 0644 $(STARTUPFILE) $(HOMEDIR)

clean:
	rm -f $(OBJECTS) $(EXECUTABLE)

$(EXECUTABLE): $(OBJECTS)
	$(LINK.c) -o $@ $(OBJECTS) $(XLIBS)
