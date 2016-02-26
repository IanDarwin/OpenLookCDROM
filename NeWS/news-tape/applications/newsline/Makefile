LFLAGS = -L/usr/NeWS/lib
CFLAGS = -I/usr/NeWS/include

newsline:	newsline.o 
	cc $(CFLAGS) $(LFLAGS) -o newsline newsline.o -lcps

newsline.o:	newsline.h

newsline.h:	newsline.cps
	cps newsline.cps
	rm -f *.BAK

clean:
	rm -rf *.BAK *.o newsline.h newsline core *~ 
