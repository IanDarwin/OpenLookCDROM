#! /bin/sh
TOP=`pwd`
PATH=$TOP/imake:$PATH
export PATH
echo "#define ConfigDir               $TOP/imake" >config/ConfigDir
cd imake
make -f Makefile.ini clean
make -f Makefile.ini 
cd ..
CONFIG=$TOP/imake
export CONFIG
imake/xmkmf
make World
