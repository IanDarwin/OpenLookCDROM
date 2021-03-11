#
#
#
include	../../Makefile.rules

#
# Sub directories for recursive make.
#
# MAKEABLE=
MAKEABLE_MACHDEP= src

none:

all clean install install.man update update.`/bin/machine` ::
#	@for i in $(MAKEABLE); \
#	do \
#	  echo ------ $$i; \
#	  (cd $$i; $(MAKE) $@ ); \
#	done
	@for i in $(MAKEABLE_MACHDEP); \
	do \
	  echo --- `/bin/machine`.$$i; \
	  (cd `/bin/machine`.$$i; $(MAKE) $@ ); \
	done

#
#
#
HSOURCES=bba.h bba_reg.h
update::	update.osf update.mips
update.osf::
	@echo "Sorry, this driver is for Ultrix/RISC only."

update.mips::
	rm -f mips.kit/src/usr/include/io/tc/*
	@for i in $(HSOURCES); \
	do \
		$(INSTALL) -c -m 444  $$i mips.kit/src/usr/include/io/tc ;\
	done

clean::
	rm -f *.CKP *.ln *.BAK *.bak *.o core errs ,* *~ .emacs_* tags TAGS make.log MakeOut

