OBJS = rstat_svc.o rstat_xdr.o rstat_prog.o
CFLAGS = -O

# Uncomment this unless RPC functions are in libc
#RPCLIB = -lrpclib

all:rstatd librpcsvc.a

install:all
	cp librpcsvc.a /usr/lib
	cp rstat.h /usr/include
	cp rstatd /usr/etc/rpc.rstatd

rstatd:$(OBJS)
	$(CC) -o $@ $(OBJS) $(RPCLIB)

librpcsvc.a:rstat.o rstat_xdr.o
	ar crv librpcsvc.a rstat.o rstat_xdr.o

rstat.h rstat_clnt.c rstat_svc.c rstat_xdr.c:rstat.x
	rpcgen rstat.x

clean:
	-rm -f rstatd librpcsvc.a *.o
	-rm -f rstat.h rstat_clnt.c rstat_svc.c rstat_xdr.c
