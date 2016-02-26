LDFLAGS=-lsuntool -lsunwindow -lpixrect -lm
fractool:	fractool.o init.o notify.o math.o utils.o ruledef.o allocate.o io.o automatic.o
	cc -o fractool fractool.o init.o notify.o math.o utils.o ruledef.o allocate.o io.o automatic.o ${LDFLAGS}
fractool.o:	fractool.c
	cc -c fractool.c
init.o:		init.c
	cc -c init.c
notify.o:	notify.c
	cc -c notify.c
math.o:		math.c
	cc -c math.c
utils.o:	utils.c
	cc -c utils.c
ruledef.o:	ruledef.c
	cc -c ruledef.c
allocate.o:	allocate.c
	cc -c allocate.c
io.o:	io.c
	cc -c io.c
automatic.o:	automatic.c
	cc -c automatic.c

clean:	; rm -f fractool *.o *.BAK
