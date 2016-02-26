#! /bin/sh
if [ "X$OPENWINHOME" = "X" ]; then
     OPENWINHOME=/usr/openwin
     export OPENWINHOME
fi
if [ "X$XFILESEARCHPATH" = "X" ]; then
     XFILESEARCHPATH=$OPENWINHOME/lib/%T/%N%S
     export  XFILESEARCHPATH
fi
LD_LIBRARY_PATH=$OPENWINHOME/lib
IMAKECPP=/usr/ccs/lib/cpp
export LD_LIBRARY_PATH IMAKECPP
TOP=`pwd`
PATH=$TOP/imake:$PATH:/usr/openwin/bin:
export PATH
echo "#define ConfigDir $TOP/imake"      >imake/ConfigDir
echo "#define ProjectRoot  $OPENWINHOME" >>imake/ConfigDir
echo "#define LibDir $OPENWINHOME/lib"   >>imake/ConfigDir
echo "#define IncRoot $OPENWINHOME/include" >>imake/ConfigDir
echo "#define OwSearchPath  $XFILESEARCHPATH" >>imake/ConfigDir 
echo "#define SystemV4  1" >>imake/ConfigDir 
cd imake
make -f Makefile.ini clean
make -f Makefile.ini 
cd ..
CONFIG=$TOP/imake
export CONFIG
imake/xmkmf
make Makefiles
make World
