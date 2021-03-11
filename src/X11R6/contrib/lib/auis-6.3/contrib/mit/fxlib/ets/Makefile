#
# Makefile for the FX (file exchange) error tables
#
# $Author: wdc $
# $Source: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/contrib/mit/fxlib/ets/RCS/Makefile,v $
# $Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/contrib/mit/fxlib/ets/RCS/Makefile,v 1.1 1992/02/13 20:22:06 wdc R6tape $
#
# Copyright 1989, 1990 by the Massachusetts Institute of Technology.
#
# For copying and distribution information, please see the file
# <mit-copyright.h>.
#

.SUFFIXES: .et .o

CFLAGS = -g

.et.c:
	rm -f $*.o $*.c
	compile_et $*.et
	rm -f ../include/$*.h
	mv $*.h ../include

all: rpc_err.o fxserver_err.o fxcl_err.o krb_err.o

rpc_err.c: rpc_err.et
rpc_err.o: rpc_err.c
fxserver_err.c: fxserver_err.et
fxserver_err.o: fxserver_err.c
fxcl_err.c: fxcl_err.et
fxcl_err.o: fxcl_err.c
krb_err.c: krb_err.et
krb_err.o: krb_err.c

krb_err.et: mk_krb_err
	./mk_krb_err > $@

mk_krb_err: mk_krb_err.c
	$(CC) ${CFLAGS} -o $@ mk_krb_err.c -lkrb

clean:
	rm -f core *.o *~ rpc_err.c fxserver_err.c fxcl_err.c \
	krb_err.c krb_err.et mk_krb_err
