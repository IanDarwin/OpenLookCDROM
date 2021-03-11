/**/#!/bin/sh
/**/#
/**/# $XConsortium: mergelib.cpp,v 1.2 89/10/17 12:09:30 jim Exp $
/**/# 
/**/# Copyright 1989 Massachusetts Institute of Technology
/**/# 
/**/# Permission to use, copy, modify, distribute, and sell this software and its
/**/# documentation for any purpose is hereby granted without fee, provided that
/**/# the above copyright notice appear in all copies and that both that
/**/# copyright notice and this permission notice appear in supporting
/**/# documentation, and that the name of M.I.T. not be used in advertising or
/**/# publicity pertaining to distribution of the software without specific,
/**/# written prior permission.  M.I.T. makes no representations about the
/**/# suitability of this software for any purpose.  It is provided "as is"
/**/# without express or implied warranty.
/**/# 
/**/# M.I.T. DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING ALL
/**/# IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL M.I.T.
/**/# BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
/**/# WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION
/**/# OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN 
/**/# CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
/**/# 
/**/# Author:  Jim Fulton, MIT X Consortium
/**/# 
/**/# mergelib - merge one library into another; this is commonly used by X
/**/#     to add the extension library into the base Xlib.
/**/#

usage="usage:  $0  to-library from-library [object-filename-prefix]"
objprefix=_

case $# in
    2) ;;
    3) objprefix=$3 ;;
    *) echo "$usage" 1>&2; exit 1 ;;
esac

tolib=$1
fromlib=$2

if [ ! -f $fromlib ]; then
    echo "$0:  no such from-library $fromlib" 1>&2
    exit 1
fi

if [ ! -f $tolib ]; then
    echo "$0:  no such to-library $tolib" 1>&2
    exit 1
fi


/**/#
/**/# Create a temp directory, and figure out how to reference the 
/**/# object files from it (i.e. relative vs. absolute path names).
/**/#

tmpdir=tmp.$$
origdir=..

mkdir $tmpdir

if [ ! -d $tmpdir ]; then
    echo "$0:  unable to create temporary directory $tmpdir" 1>&2
    exit 1
fi

case "$fromlib" in
    /?*) upfrom= ;;
    *)  upfrom=../ ;;
esac

case "$tolib" in
    /?*) upto= ;;
    *)  upto=../ ;;
esac


/**/#
/**/# In the temp directory, extract all of the object files and prefix
/**/# them with some symbol to avoid name clashes with the base library.
/**/#
cd $tmpdir
ar x ${upfrom}$fromlib
for i in *.o; do
    mv $i ${objprefix}$i
done


/**/#
/**/# Merge in the object modules, ranlib (if appropriate) and cleanup
/**/#
ARCMD ${upto}$tolib *.o
RANLIB ${upto}$tolib
cd $origdir
rm -rf $tmpdir



