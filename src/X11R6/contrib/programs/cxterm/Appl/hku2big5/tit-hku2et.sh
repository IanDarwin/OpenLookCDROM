#!/bin/sh
if [ "$1" = "" ]; then
	echo "Which directory did you put those *.tit?"
	echo "usage: $0 directory_name"
	exit 1;
fi
for i in $1/*.tit; do
	if grep -s '^COMMENT[ 	]\$HKU BIG5\$' $i; then
		echo "$i:	save in  $i.hku"
		mv $i $i.hku
		sed -e 's/^COMMENT[   ]\$HKU BIG5\$/COMMENT $ET BIG5$/' $i.hku \
		| et2hku -e > $i
		echo "$i:	converted to ET Big5 encoding."  1>&2
	else
		echo "$i:	not in HKU Big5 encoding, unchanged."
	fi
done
