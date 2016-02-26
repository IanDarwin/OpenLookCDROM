BINDIR = /usr/local/bin
LIBDIR = /usr/local/lib/lps
YACC = byacc
YACC = yacc
CFLAGS = -g
YFLAGS = -d
SRCS = gram.y lps-main.c scan.l
OBJS = gram.o lps-main.o scan.o

lps-main:	$(OBJS)
	cc $(CFLAGS) $(OBJS) -ll -o lps-main

install:
	cp clps scmps ${BINDIR}
	install -d ${LIBDIR}
	cp lps-main scm-suffix l-suffix c-suffix ${LIBDIR}
	   
gram.o:
	${YACC} $(YFLAGS) `pwd`/gram.y
	cc -c $(CFLAGS) y.tab.c
	mv y.tab.o gram.o

prog.o:	y.tab.h

scan.o: y.tab.h
	lex scan.l
	cc $(CFLAGS) -c lex.yy.c
	mv lex.yy.o scan.o

lint:
	lint -ax $(SRCS) y.tab.c lex.yy.c

tags:
	etags $(SRCS)

clean:
	rm -f $(OBJS) lps-main y.tab.c lex.yy.c core *.BAK *~ 
