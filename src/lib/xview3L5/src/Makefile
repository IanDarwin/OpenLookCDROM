
all:	extract xview sspkg uic meminfo

xview: FORCE
	-(cd xview && xmkmf -a && make)

sspkg: FORCE
	-(cd sspkg && make)

uic: FORCE
	-(cd uic && make)

meminfo: FORCE
	-(cd meminfo && make)

install_xview: xview
	-(cd xview && make install)

install_sspkg: sspkg
	-(cd sspkg && make install)

install_uic: uic
	-(cd uic && make install)

install_meminfo: meminfo
	-(cd meminfo && make install)

install: xview sspkg uic meminfo
	-(cd xview && make install)
	-(cd sspkg && make install)
	-(cd uic && make install)
	-(cd meminfo && make install)

extract_xview: FORCE
	tar xf xviewex.tar

extract_sspkg: FORCE
	tar xf sspkgex.tar

extract_uic: FORCE
	tar xf uicex.tar

extract: FORCE
	tar xf xviewex.tar
	tar xf sspkgex.tar
	tar xf uicex.tar

clean:
	rm -rf xview sspkg uic
	(cd meminfo && make clean)

FORCE:
