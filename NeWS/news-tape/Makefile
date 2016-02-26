SUBDIR=	applications documents examples fixes fonts fun pictures utilities
TAPEDIR= /tumtum/don/news-tape
TEMPDIR= /ballast/don
FTPDIR= /tumtum/ftp/NeWS

all:
	for i in ${SUBDIR} ; do (cd $$i; make -k); done

clean:
	rm -f core *~ *.BAK
	-for i in ${SUBDIR} ; do (echo Cleaning $$i; cd $$i; make clean); done

ftp:
	( cd ${TEMPDIR} ; \
	      rm -f news-tape.[tar,tar.Z,ls-RalF] )
	( cd .. ; \
	      ls -RalF news-tape > ${TEMPDIR}/news-tape.ls-RalF ; \
	      tar cf ${TEMPDIR}/news-tape.tar news-tape )
	( cd ${TEMPDIR} ; \
	      ls -l news-tape.tar > news-tape.tar.ls-l ; \
	      compress news-tape.tar ; \
	      mv news-tape.ls-RalF news-tape.tar.ls-l news-tape.tar.Z \
		 ${FTPDIR} )
	( cd ${FTPDIR} ; \
	      make sums )
