#!/bin/sh
#
#
touch version.c

MACHINE=$1
if [ "$MACHINE" = "" ]; then
    MACHINE=vax
    if [ -f /bin/machine ]; then
	MACHINE=`/bin/machine`
    fi
fi
DATE=`date | awk '{print $2, $3, $6}'`
awk '	{	version = substr($6,2,10) + 1; }\
END	{	printf "char *version = \"xplay v1.0 #%d", version > "version.c";\
		printf " (CPU) of DATE-HERE\";\n" >> "version.c";\
		}' < version.c
ed - version.c <<%
1,\$s/DATE-HERE/$DATE/
1,\$s/CPU/$MACHINE/
w
q
%
cat version.c
