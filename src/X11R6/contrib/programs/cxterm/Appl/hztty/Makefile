DEFINES=
CDEBUGFLAGS=-O
CFLAGS=${CDEBUGFLAGS} ${DEFINES}
OBJS = main.o tty.o io.o hz2gb.o b2g.o b2g_tables.o log.o
PROG = hztty
LIBS=

CC=cc
MAKE=make
RM=rm -f

all:	
	@echo "Choose one of the make command according to your system type:"
	@echo "	Sun (SunOS 4.x):		make sun"
	@echo "	Sun (SunOS 5.x/Solaris 2):	make sun-os5"
	@echo "	SGI (IRIX 3.x):			make sgi-irix3"
	@echo "	SGI (IRIX 4.x):			make sgi-irix4"
	@echo "	DEC (Ultrix):			make ultrix"
	@echo "	IBM RS6000 (AIX):		make ibm-rs"
	@echo "	HP (UX):			make hpux"
	@echo "	HP/Apollo (domain):		make apollo"
	@echo "	MIPS (???):			make mips"
	@echo "	Sequent (BSD):			make sequent-bsd"
	@echo "	Sequent (System V R4):		make sequent-sysv4"
	@echo "	Symmetry:			make symmetry"
	@echo "	Macintosh II (A/UX 2.0):	make mac2"
	@echo "	Motorola (R32Vx/R3Vx):		make m88k-r3"
	@echo "	Motorola (R40Vx):		make m88k-r4"
	@echo ""
	@echo "If none of above is for you, try one of these:"
	@echo "	BSD Unix:			make bsd"
	@echo "	Systerm V Unix:			make sys"


sun:
	${MAKE} ${MFLAGS} ${PROG}

sun-os5: svr4

sgi-irix3:
	${MAKE} ${MFLAGS} \
		DEFINES='-DSYSV -DIRIX3 -D__STDC__ -DUNISTD' \
		${PROG}

sgi-irix4:
	${MAKE} ${MFLAGS} \
		DEFINES='-DSYSV -DIRIX4 -D__STDC__ -DUNISTD' \
		${PROG}

ultrix:
	${MAKE} ${MFLAGS} \
		DEFINES='-YBSD' \
		${PROG}

apollo:	bsd

hpux:	sys

ibm-rs:	sys

mips:
	${MAKE} ${MFLAGS} \
		DEFINES='-DSYSV -DSIG_RETURNS_INT' \
		${PROG}

sequent-bsd:
	${MAKE} ${MFLAGS} DEFINES='-DBSD' ${PROG} LIBS='-lseq'

sequent-sysv4:
	${MAKE} ${MFLAGS} DEFINES='-DSVR4' ${PROG} LIBS='-lseq'

symmetry:
	${MAKE} ${MFLAGS} \
		DEFINES='-DSIG_RETURNS_INT' \
		${PROG}

mac2:
	${MAKE} ${MFLAGS} \
		DEFINES='-DSYSV -DmacII' LIBS='-lposix' \
		${PROG}

m88k-r3: sys

m88k-r4: svr4

bsd:
	${MAKE} ${MFLAGS} DEFINES='-DBSD' ${PROG}

sys:
	${MAKE} ${MFLAGS} DEFINES='-DSYSV' ${PROG}

svr4:
	${MAKE} ${MFLAGS} DEFINES='-DSVR4' ${PROG}


${PROG}: ${OBJS}
	$(CC) $(CFLAGS) -o ${PROG} ${OBJS} ${LIBS}

clean:
	${RM} *.o ${PROG} core *~

# DO NOT DELETE THIS LINE -- make depend depends on it.

main.o: config.h io.h
tty.o: config.h
io.o: config.h io.h
hz2gb.o: io.h
b2g.o: io.h
log.o: config.h io.h
