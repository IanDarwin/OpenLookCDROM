#	@(#)Makefile	5.10 (Berkeley) 5/11/90

PROG=	ntalkd
SRCS=	talkd.c announce.c process.c table.c print.c
OBJS=	talkd.o announce.o process.o table.o print.o
MAN8=	talkd.0

CFLAGS= -O -I. $(DEFINES)

all: ${PROG}

${PROG}: ${OBJS}
	$(CC) $(CFLAGS) -o ${PROG} ${OBJS} ${LDADD}

