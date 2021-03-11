# Makefile for hc (Hanzi Converter) 3.0

hc:	hc.o hclib.o
	cc -o hc hc.o hclib.o

b2g:
	echo "#! /bin/csh" > b2g
	echo $(PWD)/hc "-m b2g -t" $(PWD)/hc.tab "$$*" >> b2g
	chmod a+x b2g

g2b:
	echo "#! /bin/csh" > g2b
	echo $(PWD)/hc -m g2b -t $(PWD)/hc.tab "$$*" >> g2b
	chmod a+x g2b

clean:
	rm *.o hc

tar:
	tar cf - Makefile README hc.1 hclib.h hclib.c hc.c hc.tab > hc-3.0.tar


testbig:
	hc -t hc.tab -m b2g < poem.big | chirk

testbig1:
	hc -t hc.tab -m b2g -map one < poem.big | chirk

testbig2:
	hc -t hc.tab -m b2g -map all < poem.big | chirk

testbig3:
	hc -t hc.tab -m b2g -map allbutsymbols < poem.big | chirk

testgb:
	hc -t hc.tab -m g2b < poem.gb | chirk -5

testgb1:
	hc -t hc.tab -m g2b -map one < poem.gb | chirk -5

testgb2:
	hc -t hc.tab -m g2b -map all < poem.gb | chirk -5

testgb3:
	hc -t hc.tab -m g2b -map allbutsymbols < poem.gb | chirk -5
