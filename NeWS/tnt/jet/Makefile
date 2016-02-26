#
# Makefile for the OpenWindows jet demo
#
# @(#) Makefile.customer
#

#
# Copyright (c) 1991 by Sun Microsystems, Inc.
#

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
#

COMMAND			= jet
SOURCES			= parse.c \
                          main.c \
                          pty.c \
                          tnt.c \
                          select.c \
                          gen_parse.c  \
			  windowargs.c

OBJECTS                 = ${SOURCES:.c=.o}

HEADERS 		= patchlevel.h \
                          actions.h \
                          tnt_cps.h \
                          term.h \
                          windowargs.h

CPSFLAGS		= 
INCLUDE                 = -I$$OPENWINHOME/include
CFLAGS			= -g ${INCLUDE}
LIBRARIES               = -lwire -lcps

ansi: ${SOURCES} ${HEADERS} ${OBJECTS}
	${CC} ${CFLAGS} -o jet ${OBJECTS} ${LIBRARIES}

tnt_cps.h: tnt_cps.cps package.ps

.KEEP_STATE:
