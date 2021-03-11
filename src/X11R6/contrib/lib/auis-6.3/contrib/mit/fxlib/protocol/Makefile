#
# Makefile for the FX (file exchange) protocol
#
# $Author: wdc $
# $Source: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/contrib/mit/fxlib/protocol/RCS/Makefile,v $
# $Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/contrib/mit/fxlib/protocol/RCS/Makefile,v 1.1 1992/02/13 20:32:38 wdc R6tape $
#
# Copyright 1989, 1990 by the Massachusetts Institute of Technology.
#
# For copying and distribution information, please see the file
# <mit-copyright.h>.
#

INCLUDES = -I../rpc3.9/usr/include -I../include

# Define KERBEROS for Kerberos support
DEFKRB = -DKERBEROS

# Define HESIOD for Hesiod support (not meaningful in protocol)
#DEFHES = -DHESIOD

# Define MULTI for multiple server support
DEFMULTI = -DMULTI

# Define DEBUG for command debugging messages
# Define DEBUGDB for db debugging messages
# Define DEBUGMULTI for debugging multiple server support
# Define MALLOC_LEAK for malloc scribble checking and function tracing
DEFDEBUG = -DMALLOC_LEAK

CFLAGS = -g ${INCLUDES} ${DEFKRB} ${DEFHES} ${DEFMULTI} ${DEFDEBUG}

all: local_prot_clnt.o fx_prot_svc.o fx_prot_clnt.o fx_prot_xdr.o

local_prot_clnt.o: local_prot_clnt.c
fx_prot_svc.o: fx_prot_svc.c
fx_prot_clnt.o: fx_prot_clnt.c
fx_prot_xdr.o: fx_prot_xdr.c

local_prot_clnt.c fx_prot_svc.c fx_prot_clnt.c fx_prot_xdr.c: fx_prot.x
	../rpc3.9/usr/bin/rpcgen fx_prot.x
	cat xdr_extras.c >> fx_prot_xdr.c
	../rpc3.9/usr/bin/rpcgen -m fx_prot.x -o fx_prot_svc.c
	rm -f local_prot_clnt.c fxtemp.c ../include/fx_prot.h
	mv fx_prot.h ../include
	grep -v "include <sys/time" < fx_prot_clnt.c > fxtemp.c
	rm -f fx_prot_clnt.c
	mv fxtemp.c fx_prot_clnt.c
	sed	-e 's/init_1/_init_1/' \
		-e 's/list_acl_1/_list_acl_1/' \
		-e 's/add_acl_1/_add_acl_1/' \
		-e 's/delete_acl_1/_delete_acl_1/' \
		-e 's/create_course_1/_create_course_1/' \
		-e 's/delete_course_1/_delete_course_1/' \
		-e 's/list_courses_1/_list_courses_1/' \
		-e 's/list_1/_list_1/' \
		-e 's/send_file_1/_send_file_1/' \
		-e 's/send_burst_1/_send_burst_1/' \
		-e 's/end_send_1/_end_send_1/' \
		-e 's/retrieve_file_1/_retrieve_file_1/' \
		-e 's/retrieve_burst_1/_retrieve_burst_1/' \
		-e 's/copy_1/_copy_1/' \
		-e 's/move_1/_move_1/' \
		-e 's/^delete_1/_delete_1/' \
		-e 's/portion_1/_portion_1/' \
		-e 's/krb_info_1/_krb_info_1/' \
		-e 's/kill_server_1/_kill_server_1/' \
		-e 's/server_stats_1/_server_stats_1/' \
		-e 's/server_quorum_1/_server_quorum_1/' \
		-e 's/server_store_1/_server_store_1/' \
		-e 's/server_delete_1/_server_delete_1/' \
		-e 's/server_commit_1/_server_commit_1/' \
		-e 's/server_end_course_1/_server_end_course_1/' \
		-e 's/server_requpdate_1/_server_requpdate_1/' \
		-e 's/server_start_upd_1/_server_start_upd_1/' \
		-e 's/server_end_upd_1/_server_end_upd_1/' \
		< fx_prot_clnt.c > local_prot_clnt.c

clean:
	rm -f *.o local_prot_clnt.c fx_prot_svc.c fx_prot_clnt.c fx_prot_xdr.c fx_prot.h
