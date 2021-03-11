#
#
#
include	./Makefile.rules

#
# Sub directories for recursive make.
#
MAKEABLE= lofi maxine axp

none:

clean all install install.man::
	@for i in $(MAKEABLE); \
	do \
	  echo ---- $$i; \
	  (cd $$i; $(MAKE) $@ ); \
	done

all::

install::

install.man::

clean::
	rm -f *.CKP *.ln *.BAK *.bak *.o core errs ,* *~ .emacs_* tags TAGS make.log MakeOut

