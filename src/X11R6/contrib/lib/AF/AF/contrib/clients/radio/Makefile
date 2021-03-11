CC=cc
IFLAGS = 
CFLAGS = -g $(IFLAGS)
LDFLAGS = 
LIBS = 

default: radio_mcast radio_recv

radio_mcast:	radio_mcast.o
	$(CC) $(LDFLAGS) -o radio_mcast radio_mcast.o $(LIBS)

radio_recv:	radio_recv.o
	$(CC) $(LDFLAGS) -o radio_recv radio_recv.o $(LIBS)

clean:
	-rm -f radio_mcast radio_recv *.o *~ *.BAK TAGS
