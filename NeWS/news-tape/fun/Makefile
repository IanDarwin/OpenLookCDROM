SUBDIR=	15puzzle 3demo backgrounds bez blackjack boing breakout \
	catchme curves dipper display-face dizzy eye flower-editor \
	garden ifs interpolate jump kanji lander mandel melt mistletoe \
	psgammon pspyro ptape puzzle pyramid quine ribbon rose runit \
	spiro-slider spirograph square-limit strange testcurve track \
	visualizer wallpaper wrappingpaper wtf zapper zoom

all:
	-for i in ${SUBDIR} ; do (echo Making $$i; cd $$i; make -k); done

clean:
	rm -f core *~ *.BAK
	-for i in ${SUBDIR} ; do (echo Cleaning $$i; cd $$i; make clean); done

