
LINKS = -lsuntool -lsunwindow -lpixrect -lcurses -ltermcap

all:hwtool hwtool.sales hwtool.d

hwtool.d: hwtool.c
	cc -g hwtool.c $(LINKS) -o hwtool.d

hwtool: hwtool.c
	cc -g hwtool.c $(LINKS) -o hwtool

hwtool.sales: hwtool.c
	cc -g hwtool.c $(LINKS) -DREAD_ONLY -o hwtool.sales

shar:
	shar README Makefile backup.c conflict.icon day.backup highlight.cursor hwtool.c hwtool.icon machines policies schedule > hwtool.shar
	compress hwtool.shar
	cat hwtool.shar.Z | uuencode hwtool.shar.Z > hwtool.shar.Z.uu
	rm hwtool.shar.Z

