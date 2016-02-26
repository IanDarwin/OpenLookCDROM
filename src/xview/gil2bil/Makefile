#
# Makefile for GIL to BIL translator.
#
SHELL		= /bin/sh
CC		= gcc
CFLAGS		= -g
TESTF		= test.G

try:		gil2bil $(TESTF)
		gil2bil < $(TESTF) | more

OBJS = 		lex.yy.o y.tab.o writeBil.c misc.o
gil2bil:	$(OBJS)
		$(CC) $(CFLAGS) $(OBJS) -ly -ll -o gil2bil

lex.yy.c:	gil.l
		lex gil.l
lex.yy.o:	lex.yy.c x.tab.h
		$(CC) $(CFLAGS) -DLEXDEBUG -c lex.yy.c 

y.tab.c y.tab.h:	gil.y
		yacc -d gil.y
y.tab.o:	y.tab.c
		$(CC) -DYYDEBUG=1 -c y.tab.c

x.tab.h:	y.tab.h
		cmp -s y.tab.h x.tab.h || cp y.tab.h x.tab.h

clean:
		rm -f lex.* y.tab.*
		rm -f *.o gil2bil core
