
#	@(#) Makefile 1.19 92/11/16 

SSPKGHOME = 	.
INSTALL_DIR =	/home2/install
TARFILE =	/home2/tmp/sspkgs-tar

SRCDIR =	$(SSPKGHOME)/src
LIBDIR =	$(SSPKGHOME)/lib
INCLUDEDIR = 	$(SSPKGHOME)/include/sspkg

.PRECIOUS: $(SRCDIR) $(INCLUDEDIR) examples

all: subdirs

subdirs: $(INCLUDEDIR) $(SRCDIR) examples

$(INCLUDEDIR): FORCE
	cd $@; make

$(SRCDIR): FORCE Make.config
	cd $@; make


examples: FORCE
	cd $@; make

#
# Make.config has things that are needed to build on various platforms.
# Here /vmunix existance is tested to determine if we're running svr4 
# or sunos4.x.  This is a hack, but is works.
#
Make.config: config.svr4 config.sunos4x
	rm -f Make.config
	@ if [ -s /vmunix ]; then  \
		echo cp config.sunos4x Make.config ; \
		cp config.sunos4x Make.config ; \
	else  \
		echo cp config.svr4 Make.config ; \
		cp config.svr4 Make.config ; \
	fi


FORCE:

clean:
	-sccs clean
	(cd $(INCLUDEDIR); make clean)
	(cd $(SRCDIR); make clean)
	(cd examples; make clean)
	rm -f Make.config

install: $(INSTALL_DIR) FORCE
	(cd $(INCLUDEDIR); make install INSTALL_DIR=$(INSTALL_DIR))
	(cp $(LIBDIR)/* $(INSTALL_DIR)/lib)
	(cd $(INSTALL_DIR)/lib; rm -f libsspkg.so; ln -s libsspkg.so.* libsspkg.so)

$(INSTALL_DIR): FORCE
	-mkdir -p $(INSTALL_DIR)
	-mkdir -p $(INSTALL_DIR)/include/sspkg
	-mkdir $(INSTALL_DIR)/lib

#tarfile: FORCE examples/README examples/things

tarfile: FORCE LEGAL_NOTICE README examples/README examples/dnd2/README config.svr4 config.sunos4x
	$(RM) $(TARFILE)
	(cd ..; find ./sspkg2.0 -name SCCS -print > /tmp/excludelist)
	(cd ..; find ./sspkg2.0 -name lex.yy.c -print >> /tmp/excludelist)
	(cd ..; find ./sspkg2.0 -name libsspkg.a -print >> /tmp/excludelist)
	(cd ..; find ./sspkg2.0 -name .make.state -print >> /tmp/excludelist)
	(cd ..; find ./sspkg2.0 -name .nse_depinfo -print >> /tmp/excludelist)
	(cd ..; find ./sspkg2.0 -type f -perm -700 -print >> /tmp/excludelist)
	(echo ./sspkg2.0/Make.config >> /tmp/excludelist)
	(echo ./sspkg2.0/test >> /tmp/excludelist)
	(echo ./sspkg2.0/book >> /tmp/excludelist)
	(echo ./sspkg2.0/lib >> /tmp/excludelist)
	(cd ..; tar cvfXFF $(TARFILE) /tmp/excludelist ./sspkg2.0)
	$(RM) /tmp/excludelist

#	(cd `dirname $(TARFILE)`; tar xf $(TARFILE); makekit `find sspkg2.0 -print`)
# note: copy doc files by hand.

