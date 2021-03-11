#	@(#)Makefile	5.12 (Berkeley) 5/11/90

# BSD 4.3 use
LDADD=	-ltermlib
DEFINES=

# BSD 4.2 use
# LDADD= -ltermcap
# DEFINES=

# System V use
# LDADD= -ltermlib
# DEFINES= -DSYSV

PROG=	ctalk
SRCS=	talk.c get_names.c display.c io.c ctl.c init_disp.c \
	msgs.c get_addrs.c ctl_transact.c invite.c look_up.c \
	curses.c
OBJS=	talk.o get_names.o display.o io.o ctl.o init_disp.o \
	msgs.o get_addrs.o ctl_transact.o invite.o look_up.o \
	curses.o
CFLAGS=	-O -I. $(DEFINES)

all: ${PROG}

${PROG}: ${OBJS}
	$(CC) $(CFLAGS) -o ${PROG} ${OBJS} ${LDADD}


# DO NOT DELETE THIS LINE - maketd DEPENDS ON IT
# Dependencies generated at: Sat Nov 30 01:58:43 EST 1991

ctl.o: curses.h
ctl.o: protocols/talkd.h
ctl.o: talk.h
ctl.o: talk_ctl.h
ctl.o: ctl.c
ctl_transact.o: curses.h
ctl_transact.o: protocols/talkd.h
ctl_transact.o: talk.h
ctl_transact.o: talk_ctl.h
ctl_transact.o: ctl_transact.c
curses.o: curses.h
curses.o: curses.c
display.o: curses.h
display.o: talk.h
display.o: display.c
get_addrs.o: curses.h
get_addrs.o: protocols/talkd.h
get_addrs.o: talk.h
get_addrs.o: talk_ctl.h
get_addrs.o: get_addrs.c
get_names.o: curses.h
get_names.o: protocols/talkd.h
get_names.o: talk.h
get_names.o: get_names.c
init_disp.o: curses.h
init_disp.o: talk.h
init_disp.o: init_disp.c
invite.o: curses.h
invite.o: protocols/talkd.h
invite.o: talk.h
invite.o: talk_ctl.h
invite.o: invite.c
io.o: curses.h
io.o: talk.h
io.o: io.c
look_up.o: curses.h
look_up.o: protocols/talkd.h
look_up.o: talk.h
look_up.o: talk_ctl.h
look_up.o: look_up.c
msgs.o: curses.h
msgs.o: talk.h
msgs.o: msgs.c
talk.o: curses.h
talk.o: talk.h
talk.o: talk.c

# DO NOT ADD ANYTHING HERE - WILL GO AWAY
