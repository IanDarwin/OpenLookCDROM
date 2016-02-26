CFLAGS=-g
INCLUDE=-I/usr/NeWS/include
LIBS=/usr/NeWS/lib/libcps.a

newsbiff: newsbiff.c newsbiff.h
	$(CC) $(CFLAGS) $(INCLUDE) newsbiff.c -o newsbiff $(LIBS)
	cp newsbiff /usr/NeWS/bin

newsbiff.h: newsbiff.cps
	cps newsbiff.cps

clean:
	rm -f newsbiff.h newsbiff *.o core *~ *.BAK
