#!/bin/sh

#
# This accepts bsd-style install arguments and makes the appropriate calls
# to the System V install.
#

flags=""
dst=""
src=""
dostrip=""

while [ x$1 != x ]; do
    case $1 in 
	-c) shift
	    continue;;

	-[mog]) flags="$flags $1 $2 "
	    shift
	    shift
	    continue;;

	-s) dostrip="strip"
	    shift
	    continue;;

	*)  if [ x$src = x ] 
	    then
		src=$1
	    else
		dst=$1
	    fi
	    shift
	    continue;;
    esac
done

if [ x$src = x ] 
then
	echo "bsdinstall:  no input file specified"
	exit 1
fi

if [ x$dst = x ] 
then
	echo "bsdinstall:  no destination specified"
	exit 1
fi


# set up some variable to be used later

rmcmd=""
srcdir="."

# if the destination isn't a directory we'll need to copy it first

if [ ! -d $dst ]
then
	dstbase=`basename $dst`
	cp $src /tmp/$dstbase
	rmcmd="rm -f /tmp/$dstbase"
	src=$dstbase
	srcdir=/tmp
	dst="`echo $dst | sed 's,^\(.*\)/.*$,\1,'`"
	if [ x$dst = x ]
	then
		dst="."
	fi
fi


# If the src file has a directory, copy it to /tmp to make install happy

srcbase=`basename $src`

if [ "$src" != "$srcbase" -a "$src" != "./$srcbase" ] 
then
	cp $src /tmp/$srcbase
	src=$srcbase
	srcdir=/tmp
	rmcmd="rm -f /tmp/$srcbase"
fi

# do the actual install (remove destination since SysV install doesn't)

rm -f $dst/$srcbase
(cd $srcdir ; install -f $dst $flags $src)


# and clean up

$rmcmd

