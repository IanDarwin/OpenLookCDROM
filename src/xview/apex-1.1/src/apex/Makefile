#
# $Header: /n/homeserver/i/gounares/pu/apex/src/apex/RCS/Makefile,v 1.1 93/01/06 03:27:56 gounares Exp Locker: gounares $
#

HPX11INCLUDES=/usr/include/X11R4
XVIEWINCLUDES=/usr/local/lib/xview3/include
HPX11LIBS=/usr/lib/X11R4
LOCALLIBS=/usr/local/lib

EXTRAOBJS = /tmp/signal.o /tmp/sigvec.o /tmp/getwd.o

CC = cc 
CFLAGS = -g -I$(XVIEWINCLUDES) -I$(HPX11INCLUDES) -I../../include -Wp,-H500000 
LIBS = -L$(LOCALLIBS) -lxview -lolgx -lsspkg -L$(HPX11LIBS) -lX11 -lm
OBJS = apex.o editor.o alloc.o file_io.o misc.o object.o state_server.o \
       file_browser.o apex_shell.o object_list.o navigator.o s_pad.o \
       symbol.o apex_help.o apex_options.o
PSRC = apex.cpp editor.cpp alloc.cpp file_io.cpp misc.cpp object.cpp  \
       state_server.cpp file_browser.cpp apex_shell.cpp object_list.cpp \
       navigator.cpp s_pad.cpp symbol.cpp apex_help.cpp apex_options.cpp

apex: $(OBJS) $(EXTRAOBJS)
	$(CC) $(LDFLAGS) $(CFLAGS) $(OBJS) $(EXTRAOBJS) $(LIBS) -o apex 

all: apex parse

apex.o:		apex.c apex.h
	$(CC) -E $(CFLAGS) apex.c > apex.cpp
	$(CC) -c $(CFLAGS) apex.c
editor.o:	editor.c editor.h
	$(CC) -E $(CFLAGS) editor.c > editor.cpp
	$(CC) -c $(CFLAGS) editor.c
alloc.o:	alloc.c alloc.h
	$(CC) -E $(CFLAGS) alloc.c > alloc.cpp
	$(CC) -c $(CFLAGS) alloc.c
file_io.o:	file_io.c file_io.h
	$(CC) -E $(CFLAGS) file_io.c > file_io.cpp
	$(CC) -c $(CFLAGS) file_io.c
misc.o:		misc.c misc.h
	$(CC) -E $(CFLAGS) misc.c > misc.cpp
	$(CC) -c $(CFLAGS) misc.c
object.o:	object.c object.h editor.h
	$(CC) -E $(CFLAGS) object.c > object.cpp
	$(CC) -c $(CFLAGS) object.c
state_server.o: state_server.c state_server.h editor.h
	$(CC) -E $(CFLAGS) state_server.c > state_server.cpp
	$(CC) -c $(CFLAGS) state_server.c
file_browser.o:	file_browser.c file_browser.h editor.h
	$(CC) -E $(CFLAGS) file_browser.c > file_browser.cpp
	$(CC) -c $(CFLAGS) file_browser.c
apex_shell.o:	apex_shell.c apex_shell.h editor.h
	$(CC) -E $(CFLAGS) apex_shell.c > apex_shell.cpp
	$(CC) -c $(CFLAGS) apex_shell.c
object_list.o:	object_list.c object_list.h editor.h
	$(CC) -E $(CFLAGS) object_list.c > object_list.cpp
	$(CC) -c $(CFLAGS) object_list.c
navigator.o:	navigator.c navigator.h editor.h symbol.h
	$(CC) -E $(CFLAGS) navigator.c > navigator.cpp
	$(CC) -c $(CFLAGS) navigator.c
s_pad.o:	s_pad.c s_pad.h editor.h
	$(CC) -E $(CFLAGS) s_pad.c > s_pad.cpp
	$(CC) -c $(CFLAGS) s_pad.c -o s_pad.o
symbol.o:	symbol.c symbol.h
	$(CC) -E $(CFLAGS) symbol.c > symbol.cpp
	$(CC) -c $(CFLAGS) symbol.c
apex_help.o:	apex_help.c apex_help.h
	$(CC) -E $(CFLAGS) apex_help.c > apex_help.cpp
	$(CC) -c $(CFLAGS) apex_help.c
apex_options.o:	apex_options.c apex_options.h
	$(CC) -E $(CFLAGS) apex_options.c > apex_options.cpp
	$(CC) -c $(CFLAGS) apex_options.c
parse:	$(SRC)
	cat $(PSRC) | parse > apex.parse

/tmp/sigvec.o:
	(cd /tmp ; ar -x /usr/lib/libBSD.a sigvec.o )

/tmp/signal.o:
	(cd /tmp ; ar -x /usr/lib/libBSD.a signal.o )

/tmp/getwd.o:
	(cd /tmp ; ar -x /usr/lib/libBSD.a getwd.o )
