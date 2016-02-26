SUBDIR=	codebook debug fontscale func hello-world input paint2 \
	startup string token

all:
	-for i in ${SUBDIR} ; do (echo Making $$i; cd $$i; make -k); done

clean:
	rm -f core *~ *.BAK
	-for i in ${SUBDIR} ; do (echo Cleaning $$i; cd $$i; make clean); done
