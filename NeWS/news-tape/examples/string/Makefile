string:	string.o
	cc -o string string.o /usr/NeWS/lib/libcps.a

string.o: string.c string.h
	cc -c -I"/usr/NeWS/include" string.c

string.h: string.cps
	cps string.cps

clean:
	rm -f string.h string.o string core *~ *.BAK
