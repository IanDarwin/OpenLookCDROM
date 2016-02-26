
OPENWINHOME = /usr/openwin
X11HOME = /usr/X386

all: meminfo

meminfo: meminfo.cc
	g++ -O -I$(OPENWINHOME)/include -L$(OPENWINHOME)/lib -L$(X11HOME)/lib -o meminfo meminfo.cc -lUIC -lxview -lolgx -lX11 -lm

install:
	install	-m 755 -s meminfo $(OPENWINHOME)/bin
	install -m 444 meminfo.info $(OPENWINHOME)/lib/help

clean:
	rm -f meminfo core
