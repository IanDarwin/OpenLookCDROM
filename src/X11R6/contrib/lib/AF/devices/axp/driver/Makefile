#
#
#
include	../../Makefile.rules

#
# Sub directories for recursive make.
#
MAKEABLE=
MAKEABLE_MACHDEP= 

none:

all clean install install.man update update.`/bin/machine` ::
#	@for i in $(MAKEABLE); \
#	do \
#	  echo ------ $$i; \
#	  (cd $$i; $(MAKE) $@ ); \
#	done
#	@for i in $(MAKEABLE_MACHDEP); \
#	do \
#	  echo --- `/bin/machine`.$$i; \
#	  (cd `/bin/machine`.$$i; $(MAKE) $@ ); \
#	done

#
#
#
HSOURCES=
update::	update.osf update.mips
update.mips::

update.osf::

clean::
	rm -f *.CKP *.ln *.BAK *.bak *.o core errs ,* *~ .emacs_* tags TAGS make.log MakeOut

