MANDIR=/usr/man/man1

all:

clean:
	rm -f *~ *.PS

install:
	for i in *.1; do \
		install -m 444 -c $$i $(DESTDIR)$(MANDIR)/$$i; done

.SUFFIXES: .PS .1

.1.PS:
	rm -f $@
	psroff -t -man $*.1 > $@
