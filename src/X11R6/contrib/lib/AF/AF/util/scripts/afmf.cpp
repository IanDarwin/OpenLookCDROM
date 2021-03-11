/**/#!/bin/sh

/**/# 
/**/# generate a Makefile within the build tree
/**/# 
/**/# usage:  afmf [treedir]
/**/# 

if [ x$1 != x ]; then
	tree=$1
else
	tree=/AF
fi

dir=`pwd`
top=`(cd $tree; /bin/pwd)`
intree=no

case $dir in
	$top*)	intree=yes;;
esac

if [ $intree != yes ]; then
	echo "$0:  Must be underneath $tree"
	exit 1
fi

(cd ..; make SUBDIRS=`basename $dir` Makefiles)
