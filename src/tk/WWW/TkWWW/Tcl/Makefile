# Generated automatically from Makefile.in by configure.
.SUFFIXES: .tcl .sed

#### Start of system configuration section. ####

srcdir = .
VPATH = .

CC = cc
INSTALL = cp
INSTALL_PROGRAM = $(INSTALL)

AR = ar
RANLIB = :
RM = rm -f

DEFS =  -DUSE_DIRENT -DSTDC_HEADERS=1 -DNO_GETWD=1
LIBS =  -lelf -lsocket -lnsl
LIBPROGS = @LIBPROGS@

CFLAGS = -O $(DEFS)
LDFLAGS =

prefix = /usr/local
exec_prefix = ${prefix}

# Where to install the executables.
bindir = $(exec_prefix)/bin

# Where to put mvdir, if your system lacks the rename system call.
libdir = $(exec_prefix)/lib
tkwww_libdir = $(libdir)/tkWWW

# Where to put the manual pages.
mandir = $(prefix)/man/man1
# Extension (not including `.') for the installed manual page filenames.
manext = 1

# Where the program finds things
tk_www_src_dir = $(tkwww_libdir)
tk_www_extra_path = :/usr/local/bin
tk_www_home_page = file://localhost/cdrom/index.html
tk_www_start_page = file://localhost/cdrom/index.html
tk_www_mail = mail
tk_www_tk_cmd = $(bindir)/wwwish

#### End of system configuration section. ####

SHELL = /bin/sh

TCL2C = $(srcdir)/tcl2c

CFLAGS = -O
LDFLAGS = -O
DEFINES =  -DUSE_DIRENT -DSTDC_HEADERS=1 -DNO_GETWD=1

XINCLUDES =  -I/usr/openwin/include
XLIBSW = -L/usr/openwin/lib -lX11
TCL_INCDIR =  -I/usr/local/include
TK_INCDIR = 
TCL_LIBDIR = -L/usr/local/lib
TK_LIBDIR = 

INCLUDES = -I$(srcdir)/../Server $(TCL_INCDIR) $(TK_INCDIR) \
	$(XINCLUDES) -I$(srcdir)/../../Library/Implementation

ALL_CFLAGS= $(DEFINES) $(INCLUDES) $(CFLAGS) 

WWW_LIB= $(srcdir)/../Library/libwww.a
TKWWW_LIB= $(srcdir)/../Server/libtkwww.a

LIBS= $(TK_LIBDIR) $(TCL_LIBDIR) -ltk -ltcl $(XLIBSW)  -lelf -lsocket -lnsl -lm

objs = binary.o annotate.o bookmarks.o dialog.o edit.anchor.o \
   edit.generate.o edit.modified.o edit.selection.o edit.o \
   editwin.o file.o font.o grpan.o help.o history.o \
   hypertext.o image.o init.o main.o menu.o msgdialog.o \
   navigate.o output.o pan.o parse_args.o server.o telnet.o \
   version.o

srcs =	binary.sed annotate.tcl bookmarks.tcl dialog.tcl edit.anchor.tcl \
   edit.generate.tcl edit.modified.tcl edit.selection.tcl edit.tcl \
   editwin.tcl file.tcl font.tcl grpan.tcl help.tcl history.tcl \
   hypertext.tcl image.tcl init.tcl main.tcl menu.tcl msgdialog.tcl \
   navigate.tcl output.tcl pan.tcl parse_args.tcl server.tcl telnet.tcl \
   version.tcl

tkobjs = tkMain.o TclAppInit.o

.sed.o:
	sed -e 's;tk_www_src_dir;$(tk_www_src_dir);' \
	    -e 's;tk_www_extra_path;$(tk_www_extra_path);' \
	    -e 's;tk_www_home_page;$(tk_www_home_page);' \
	    -e 's;tk_www_start_page;$(tk_www_start_page);' \
	    -e 's;tk_www_mail;$(tk_www_mail);' \
	    -e 's;tk_www_tk_cmd;$(tk_www_tk_cmd);' $<  > $*.sed_tcl
	$(TCL2C) $*.sed_tcl
	$(CC) -c $(ALL_CFLAGS) $*.sed_tcl.c -o $@
	$(RM) $*.sed_tcl.c $*.sed_tcl
	
.tcl.o:
	$(TCL2C) $< 
	$(CC) -c $(ALL_CFLAGS) $*.tcl.c -o $@
	$(RM) $*.tcl.c

.c.o:
	$(CC) -c $(ALL_CFLAGS) $<

all: $(tkobjs) $(objs)
	$(CC) $(tkobjs) $(objs) $(LDFLAGS) \
	    $(TKWWW_LIB) $(WWW_LIB) $(LIBS) -o tkwww

interp: tkwww.sed
	sed -e 's;tk_www_src_dir;$(tk_www_src_dir);' \
	    -e 's;tk_www_extra_path;$(tk_www_extra_path);' \
	    -e 's;tk_www_home_page;$(tk_www_home_page);' \
	    -e 's;tk_www_start_page;$(tk_www_start_page);' \
	    -e 's;tk_www_mail;$(tk_www_mail);' \
	    -e 's;tk_www_tk_cmd;$(tk_www_tk_cmd);' \
	    $(srcdir)/tkwww.sed > tkwww
	chmod a+x tkwww

clean: 
	$(RM) *.o tkwww *~ core *.tcl.c *.sed_tcl *.sed_tcl.c

interpinstall:
	$(INSTALL_PROGRAM) $(srcdir)/tkwww $(bindir)/tkwww
	cp $(srcdir)/*.tcl $(tkwww_libdir)
	cp $(srcdir)/tclIndex $(tkwww_libdir)

install:
	$(INSTALL_PROGRAM) $(srcdir)/tkwww $(bindir)/tkwww
