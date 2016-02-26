#
# This file is a product of Sun Microsystems, Inc. and is provided for
# unrestricted use provided that this legend is included on all tape
# media and as a part of the software program in whole or part.  Users
# may copy or modify this file without charge, but are not authorized to
# license or distribute it to anyone else except as part of a product
# or program developed by the user.
# 
# THIS FILE IS PROVIDED AS IS WITH NO WARRANTIES OF ANY KIND INCLUDING THE
# WARRANTIES OF DESIGN, MERCHANTIBILITY AND FITNESS FOR A PARTICULAR
# PURPOSE, OR ARISING FROM A COURSE OF DEALING, USAGE OR TRADE PRACTICE.
# 
# This file is provided with no support and without any obligation on the
# part of Sun Microsystems, Inc. to assist in its use, correction,
# modification or enhancement.
# 
# SUN MICROSYSTEMS, INC. SHALL HAVE NO LIABILITY WITH RESPECT TO THE
# INFRINGEMENT OF COPYRIGHTS, TRADE SECRETS OR ANY PATENTS BY THIS FILE
# OR ANY PART THEREOF.
# 
# In no event will Sun Microsystems, Inc. be liable for any lost revenue
# or profits or other special, indirect and consequential damages, even
# if Sun has been advised of the possibility of such damages.
# 
# Sun Microsystems, Inc.
# 2550 Garcia Avenue
# Mountain View, California  94043


SRCS=bytestring.c display.c document.c view.c ched.c psinter.c formatter.c \
	styles.c scrolling.c ctops.c
SHAR=README Makefile document.h encoding.h pstoctable.h ${SRCS} TEST psinter.cps ched.ps
OBJS=${SRCS:.c=.o}
DEBUG=

INCL=-I/usr/NeWS/include
CFLAGS=-O ${INCL}

all: ched
debug: ched
debug:=CFLAGS = -g ${DEBUG} ${INCL}
debug:=DEBUG=-DDEBUG

.KEEP_STATE:

psinter.c: psinter.h
.SUFFIXES: .h .cps
psinter.h: psinter.cps
	/usr/NeWS/bin/cps -i ${DEBUG} psinter.cps
	chmod 444 psinter.h
ched: ${OBJS}
	cc ${CFLAGS} -o ched ${OBJS} /usr/NeWS/lib/libcps.a
${OBJS}: document.h psinter.h
ctops.c: encoding.h pstoctable.h
ched.shar: ${SHAR}
	rm -f ched.shar
	echo "#! /bin/sh">ched.shar
	for i in $(SHAR); do \
	  (echo "cat>$$i <<'EOT'"; cat $$i; echo EOT )\
	done >>ched.shar
	chmod 555 ched.shar

clean:
	rm -f *.o ched core *~ *.BAK
