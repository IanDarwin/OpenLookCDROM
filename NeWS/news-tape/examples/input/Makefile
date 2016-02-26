CFILES =	cn.c
CPSFILES =	cn.cps
LIBS =		-I/usr/NeWS/include -L/usr/NeWS/lib -lcps
CFLAGS =	-O

cn:		$(CFILES) $(CPSFILES)
		cps $(CPSFILES)
		$(CC) -o cn $(CFILES) $(CFLAGS) $(LIBS)

clean:
	rm -f cn cn.o cn.h core *~ *.BAK
