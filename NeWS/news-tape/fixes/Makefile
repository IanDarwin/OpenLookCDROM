SUBDIR=	framemaker getanimated goodnews illustrator88 xnews

all:
	-for i in ${SUBDIR} ; do (echo Making $$i; cd $$i; make -k); done

clean:
	rm -f core *~ *.BAK
	-for i in ${SUBDIR} ; do (echo Cleaning $$i; cd $$i; make clean); done

