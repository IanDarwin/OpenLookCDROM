if (-e /usr/itc/tools/lib/global.paths) then
	source /usr/itc/tools/lib/global.paths
	setenv PATH ${PATH}:/afs/andrew/usr0/cl0x/bin:/afs/andrew/usr0/games/bin
else
	setenv ANDREWDIR /usr/itc/released/
	setenv CLASSPATH ${ANDREWDIR}/dlib/atk:/usr/itc/projects/dlib/atk
	setenv PATH ~/bin:/usr/itc/tools/bin:${ANDREWDIR}/bin:/usr/itc/projects/bin:/usr/local/bin:/usr/ucb:/bin:/usr/bin:/usr/contributed/bin:/afs/andrew/usr0/cl0x/bin:/afs/andrew/usr0/games/bin
endif

setenv EDITOR /usr/itc/released/bin/ez
setenv EPATH /usr/local/lib/emacs/maclib
setenv PRINTER plum

setenv TN_PASS_DISPLAY ""

# make LaTeX use PostScript!!
setenv TEXINPUTS .:/usr/local/lib/tex/localstyles:/usr/local/lib/tex/macros:/usr/local/lib/tex/styles
setenv TEXFONTS .:/usr/local/lib/tex/ps/fonts/tfm:/usr/local/lib/tex/fonts/tfm
setenv TEXPKS /usr/local/lib/tex/fonts/pk
setenv PXL_AREA /usr/local/lib/tex/lib/pxl300/ps:/usr/local/lib/tex/lib

