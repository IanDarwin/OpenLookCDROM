SUBDIR=	calctool ched dviview faces gnu-emacs gnuplot micro-emacs \
	news-illustrator newsdraw newsline paint paintdemo paper \
	psterm surfmodl tree

all:
	-for i in ${SUBDIR} ; do (echo Making $$i; cd $$i; make -k); done

clean:
	rm -f core *~ *.BAK
	-for i in ${SUBDIR} ; do (echo Cleaning $$i; cd $$i; make clean); done

