#
# Makefile for the FX (file exchange) clients
#
# $Author: wdc $
# $Source: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/contrib/mit/fxlib/clients/RCS/Makefile,v $
# $Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/contrib/mit/fxlib/clients/RCS/Makefile,v 1.1 1992/02/13 18:16:13 wdc R6tape $
#
# Copyright 1989, 1990 by the Massachusetts Institute of Technology.
#
# For copying and distribution information, please see the file
# <mit-copyright.h>.
#

CLIENTS = turnin pickup fxcreate fxdestroy collect uncollect \
	 return fxblanche fxls fxping fxpurge

INCLUDES = -I../lib -I../rpc3.9/usr/include -I../include
LIBS = -L../lib -L../rpc3.9/usr/lib -lss -lfxcl -lhesiod -lkrb -ldes -lcom_err -lrpclib
FXOBJS = main.o fx_ct.o fx.o arg.o
FXSRCS = main.c fx_ct.o fx.c arg.c

CFLAGS = ${INCLUDES}

.SUFFIXES: .o .h .lint .c .ct

.ct.c:
	rm -f $*.c
	mk_cmds $*.ct

.c.lint:
	lint -u $(INCLUDES) $*.c $(LIBS) | tee $@

all: $(CLIENTS)

install: all
	for i in $(CLIENTS); do \
	if [ -f $(DESTDIR)/$$i ]; then mv $(DESTDIR)/$$i $(DESTDIR)/$$i~; fi; \
	install $$i $(DESTDIR); done

turnin: turnin.o fxmain.o full_name.o
	$(CC) $(CFLAGS) -o turnin turnin.o fxmain.o full_name.o $(LIBS)

pickup: pickup.o dump.o array.o fxmain.o
	$(CC) $(CFLAGS) -o $@ dump.o pickup.o array.o fxmain.o $(LIBS)

collect: collect.o full_name.o dump.o array.o fxmain.o
	$(CC) $(CFLAGS) -o $@ full_name.o dump.o collect.o array.o fxmain.o $(LIBS)

return: return.o array.o full_name.o fxmain.o
	$(CC) $(CFLAGS) -o $@ return.o array.o full_name.o fxmain.o $(LIBS)

fxcreate: fxcreate.o full_name.o
	$(CC) $(CFLAGS) -o fxcreate fxcreate.o full_name.o $(LIBS)

fxdestroy: fxdestroy.o
	$(CC) $(CFLAGS) -o fxdestroy fxdestroy.o $(LIBS)

fx: $(FXOBJS)
	$(CC) $(CFLAGS) -o fx $(FXOBJS) $(LIBS)

fxpurge: fxpurge.o full_name.o purge.o array.o fxmain.o
	$(CC) $(CFLAGS) -o $@ full_name.o purge.o fxpurge.o array.o fxmain.o $(LIBS)

uncollect: uncollect.o fxmain.o
	$(CC) $(CFLAGS) -o $@ uncollect.o fxmain.o $(LIBS)

fxblanche: fxblanche.o full_name.o
	$(CC) $(CFLAGS) -o $@ fxblanche.o full_name.o $(LIBS)

example: example.o
	$(CC) $(CFLAGS) -o $@ example.o $(LIBS)

fxbash: fxbash.o
	$(CC) $(CFLAGS) -o $@ fxbash.o $(LIBS)

fxls: fxls.o
	$(CC) $(CFLAGS) -o $@ fxls.o $(LIBS)

fxping: fxping.o
	$(CC) $(CFLAGS) -o $@ fxping.o $(LIBS)

clean:
	rm -f *.lint *.o core $(CLIENTS)

fx_ct.o: fx_ct.c

# The following lines are to be used by saber.

fx_src:
	#suppress 223
	#suppress 287
	#load $(INCLUDES) $(FXSRCS) $(LIBS)

turnin_src:
	#suppress 223
	#suppress 287
	#load $(INCLUDES) turnin.c fxmain.c full_name.c $(LIBS)

pickup_src:
	#suppress 223
	#suppress 287
	#load $(INCLUDES) pickup.c $(LIBS)

fxcreate_src:
	#suppress 223
	#suppress 287
	#load $(INCLUDES) fxcreate.c $(LIBS)
fxping_src:
	#suppress 223
	#suppress 287
	#load $(INCLUDES) fxping.c $(LIBS)

fxdestroy_src:
	#suppress 223
	#suppress 287
	#load $(INCLUDES) fxdestroy.c $(LIBS)

collect_src:
	#suppress 223
	#suppress 287
	#load $(INCLUDES) full_name.c collect.c dump.c $(LIBS)

return_src:
	#suppress 223
	#suppress 287
	#load $(INCLUDES) full_name.c newreturn.c array.c $(LIBS)
