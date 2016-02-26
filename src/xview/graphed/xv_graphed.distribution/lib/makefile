# (C) Universitaet Passau, Michael Himsolt 1986-1991
include ../makefile.template

all :	

MYFILES = dot.graphed dot.graphed_fileselectors \
          graphed.header.ps graphed.tail.ps \
          uni-passau.big.logo uni-passau.logo \
          fonts graphs help types

install.local::	FORCE
		if [ ! -d $(INSTALL_COMMON_LIBDIR) ]; \
		then \
			mkdir $(INSTALL_COMMON_LIBDIR) ; \
		fi
		cp -r $(MYFILES) $(INSTALL_COMMON_LIBDIR)
		rm -f $(INSTALL_COMMON_LIBDIR)/.graphed
		ln -s $(INSTALL_COMMON_LIBDIR)/dot.graphed \
		      $(INSTALL_COMMON_LIBDIR)/.graphed
