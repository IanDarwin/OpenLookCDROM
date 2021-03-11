# Makefile of hz2ps 3.1
CC	= cc -Dunix -DNO_STRDUP
# CC	= cl -Dmsdos -DNO_STRDUP
PROGS	= hz2ps
CFLAGS = -O
# users do not need to care about setting HBFLIB
HBFLIB = /user/lee/hanzi/hbf/lib

all:	$(PROGS)

hz2ps:	hz2ps.o hbf.o
	$(CC) $(CFLAGS) -o hz2ps hz2ps.o hbf.o

hz2ps.o: hz2ps.h hbf.h

hbf.o:	hbf.h

install:  $(PROGS)
	strip $(PROGS)
#	setenv HBFPATH "~/hanzi/hbf:/usr/local/hanzi/fonts/hbf"

doc:	hz2ps.1
	nroff -man hz2ps.1

hbflib:
	cp $(HBFLIB)/hbf.h $(HBFLIB)/hbf.c .

test0:
	echo "Have you installed the necessary font files ?"; \
	echo "Have you set the environment variable HBFPATH ?"; \
	echo "e.g. setenv HBFPATH /user/fonts:/usr/local/hanzi/fonts/hbf"; \
	echo "Try typing "hz2ps -help"

test1:	
	hz2ps poem.gb > out1.ps

test2:
	hz2ps -v poem.gb > out2.ps

test3:
	hz2ps -big -hf kck24.hbf 10 1 poem.big > out3.ps

clean:
	rm -f *.o core LOG a.out $(PROGS)
