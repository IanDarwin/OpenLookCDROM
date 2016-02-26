#! /bin/sh
#
# %M% %I% %E%
# Test:
# echo '.5 setgray clippath fill' | ps2bits -size 200 200> temp
# OR
# ps2bits -size 200 200> temp <<FOO
# .5 setgray clippath fill
# 0 setgray
# 0 0 moveto width height lineto stroke
# FOO

USAGE="Usage: `basename ${0}` [-size width height]"
WIDTH=100
HEIGHT=100
DEPTH=1

while [ ${#} -gt 0 ]; do
	case "${1}" in
	-size)	shift; WIDTH=${1} shift; HEIGHT=${1} ;;
	*)	echo ${USAGE};exit 0;;
	esac
	shift
done

news_server "
    /width ${WIDTH} def
    /height ${HEIGHT} def
    /depth ${DEPTH} def
    (ps2bits.ps) (r) file cvx exec
"
