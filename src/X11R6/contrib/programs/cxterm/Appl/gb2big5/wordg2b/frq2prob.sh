#!/bin/sh
# a script to build "DICT.BMM" and "DICT.FMM" from char & word frequency files.

CHARFRQFILE=$1
WORDFRQFILE=$2
AWK=gawk
BASENAME=DICT
TMPF=/tmp/wordg2b$$

trap "rm -f $TMPF" 1 2 3 15

TOTAL=`$AWK '{ n += $2; }
END {print n}' $CHARFRQFILE`
$AWK '{ printf "%s %e\n", $1, $2/'"$TOTAL"'/2 }' $CHARFRQFILE > $TMPF

TOTAL=`$AWK '{ n += $2; }
END {print n}' $WORDFRQFILE`
$AWK '{ printf "%s %e\n", $1, $2/'"$TOTAL"'/2 }' $WORDFRQFILE >> $TMPF

BuildDICT -b -i $TMPF -o $BASENAME.BMM
BuildDICT -f -i $TMPF -o $BASENAME.FMM

rm -f $TMPF
exit 0
