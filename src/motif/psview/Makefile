# Package: PsView
# Author:  Francois Bourdoncle <bourdoncle@prl.dec.com>
#
# This package *requires* an ANSI C compiler (gcc, c89, etc)
# and *must* be compiled as a *strict* ANSI C program.
#
# To configure:
#
#   1) This makefile
#
#     ARCH      : architecture (mips-ultrix/alpha-osf1)
#     MAN       : man pages (nroff)
#     CAT       : man pages (text)
#     LIB       : libraries directory
#     BIN       : binaries directory
#
#   2) $ARCH/Makefile
#
#     DPS       : set to "-DDPS" if your system has the
#                 Display Postscript extension
#
#     CC        : C compiler
#     CFLAGS    : compiler flags
#     LDFLAGS   : linker flags
#
#     X_LIB     : X libray
#     MOTIF_LIB : OSF/MOTIF librairies
#     DPS_LIB   : Display PostScript library
#     
# To build:
#
#    make all
#
# To install:
#
#    make install
#

ARCH = sol2
MAN  = /usr/man/man1
CAT  = /usr/man/cat1
LIB  = /usr/local/lib/psview
BIN  = /usr/local/bin
GS   = /usr/local/bin/gs

all:
	cd $(ARCH); $(MAKE) all LIB=$(LIB) GS=$(GS) ARCH=$(ARCH)

clean:
	cd $(ARCH); $(MAKE) clean
	rm -f *~

install:
	install -c $(ARCH)/psview $(BIN)/psview
	install -c $(ARCH)/xpsview $(BIN)/xpsview
	install -c $(ARCH)/xpsview.exe $(LIB)/$(ARCH)/xpsview
	install -m 644 -c man/psview.1 $(MAN)/psview.n
	install -m 644 -c man/xpsview.1 $(MAN)/xpsview.n
	install -m 644 -c man/psview.1 $(CAT)/psview.1
	install -m 644 -c man/xpsview.1 $(CAT)/xpsview.1
	install -m 644 -c lib/XPsView $(LIB)/app-defaults/XPsView
	install -m 644 -c lib/psview.help $(LIB)/psview.help
