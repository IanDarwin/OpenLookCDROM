#
# Makefile for the FX (file exchange) server
#
# $Author: wdc $
# $Source: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/contrib/mit/fxlib/server/RCS/Makefile,v $
# $Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/contrib/mit/fxlib/server/RCS/Makefile,v 1.1 1992/02/13 20:37:46 wdc R6tape $
#
# Copyright 1989, 1990 by the Massachusetts Institute of Technology.
#
# For copying and distribution information, please see the file
# <mit-copyright.h>.
#

# Client-server protocol, error tables kept in separate directories
PROTDIR=../protocol
ETSDIR=../ets

INCLUDES = -I../rpc3.9/usr/include -I../include

KRBLIBS = -lkrb -ldes
#HESLIBS = -lhesiod

LIBS = -L../rpc3.9/usr/lib -L../lib -lacl ${KRBLIBS} -lcom_err -lrpclib ${HESLIBS}

# Define KERBEROS for Kerberos support
DEFKRB = -DKERBEROS

# Define HESIOD for Hesiod support
#DEFHES = -DHESIOD

# Define MULTI for multiple server support
DEFMULTI = -DMULTI

# Define DEBUG for command debugging messages
# Define DEBUGDB for db debugging messages
# Define DEBUGMULTI for debugging multiple server support
# Define MALLOC_LEAK for malloc scribble checking and function tracing
#DEFDEBUG = -DDEBUG -DDEBUGDB

CFLAGS = -g ${INCLUDES} ${DEFKRB} ${DEFHES} ${DEFMULTI} ${DEFDEBUG}

all: fxserver

FXSRVOBJS = \
	${ETSDIR}/fxserver_err.o \
	${PROTDIR}/local_prot_clnt.o \
	${PROTDIR}/fx_prot_svc.o \
	${PROTDIR}/fx_prot_xdr.o \
	db.o \
	commands.o \
	main.o \
	malloc.o \
	multi.o \
	util.o

FXSRVSRCS = \
	${ETSDIR}/fxserver_err.c \
	${PROTDIR}/local_prot_clnt.c \
	${PROTDIR}/fx_prot_svc.c \
	${PROTDIR}/fx_prot_xdr.c \
	db.c \
	commands.c \
	main.c \
	malloc.c \
	multi.c \
	util.c

fxserver: ${FXSRVOBJS}
	$(CC) -o fxserver ${FXSRVOBJS} ${LIBS}

clean:
	rm -f *.o core fxserver

saber:
	saber2 ${INCLUDES} ${LIBS} /lib/libc.a -DKERBEROS -DDEBUG ${FXSRVSRCS} fxserver_err.o fx_prot_xdr.o ../rpc3.9/rpc/rpc_reference.o

saber_src:
	#suppress 223
	#suppress 287
	#load ${INCLUDES} ${DEFKRB} ${DEFHES} ${DEFMULTI} -DDEBUG -DDEBUGMULTI -DDEBUGDB ${FXSRVSRCS} ${LIBS}
lint:
	lint -u ${CFLAGS} ${FXSRVSRCS}
