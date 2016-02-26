INCLUDES =	-I/usr/openwin/include -I/usr/dt/include
LIBS =		-L/usr/dt/lib -R/usr/dt/lib
olmotif:	olmotif.o
		$(CC) $(CFLAGS) olmotif.o $(LIBS) -lXm -lXt -lX11 -o $@
olmotif.o:	olmotif.c
		$(CC) $(CFLAGS) -c $(INCLUDES) olmotif.c 

clean:
		rm -f core *.o olmotif
