
tooltool : tooltool.c tooltool.h
	cc -v -I$$NEWSHOME/include -g -o tooltool \
		tooltool.c $$NEWSHOME/lib/libcps.a

tooltool.h : tooltool.cps
	cps tooltool.cps

listings:
	enscript -2r tooltool.c tooltool.cps

lint:
	lint -I$$NEWSHOME/include tooltool.c

clean:
	rm -f tooltool tooltool.o tooltool.h core *~ *.BAK
