SUBDIR=	dali glass sunclock technichron

PS=	aniclock digital-clock nclock

SCRIPT=	goodclock

FILE=	${PS} ${SCRIPT}

all:
	-for i in ${SUBDIR} ; do (cd $$i; make -k); done

clean:
	rm -f core *~ *.BAK
	-for i in ${SUBDIR} ; do (cd $$i; make clean); done
