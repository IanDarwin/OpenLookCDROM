/**/#!/bin/sh

/**/#
/**/# generate a Makefile from an Imakefile from inside or outside the sources!
/**/# 

usage="usage:  $0 [top_of_sources_pathname [current_directory]]"

topdir=
curdir=.

case $# in 
    0) ;;
    1) topdir=$1 ;;
    2) topdir=$1  curdir=$2 ;;
    *) echo "$usage" 1>&2; exit 1 ;;
esac

case "$topdir" in
    -*) echo "$usage" 1>&2; exit 1 ;;
esac

if [ -f Makefile ]; then 
    echo mv Makefile Makefile.bak
    mv Makefile Makefile.bak
fi

if [ "$topdir" = "" ]; then
    args="-DUseInstalled "CONFIGDIRSPEC
else
    args="-I$topdir/config -DTOPDIR=$topdir -DCURDIR=$curdir"
fi

echo imake $args
imake $args
