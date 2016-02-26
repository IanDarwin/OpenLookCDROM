func:	func.o
	cc -O -o func func.o /usr/NeWS/lib/libcps.a -lm

func.o:	func.c func.h
	cc -O -c func.c -I/usr/NeWS/include

func.h: func.cps
	/usr/NeWS/bin/cps func.cps

clean:
	rm -f func func.h func.o core *~ *.BAK
