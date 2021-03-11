#
#
#
include	../Makefile.rules

#
# Sub directories for recursive make.
#
MAKEABLE= driver
MAKEABLE_MACHDEP =

none:

all install clean install.man::
	@for i in $(MAKEABLE); \
	do \
	  echo ----- $$i; \
	  (cd $$i; $(MAKE) $@ ); \
	done
#	@for i in $(MAKEABLE_MACHDEP); \
#	do \
#	  echo --- $$i.`/bin/machine`; \
#	  (cd $$i.`/bin/machine`; $(MAKE) $@ ); \
#	done

all::

install::

install.man::

clean::
	rm -f *.CKP *.ln *.BAK *.bak *.o core errs ,* *~ .emacs_* tags TAGS make.log MakeOut

