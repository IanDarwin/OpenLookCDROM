#!/bin/csh -f
## ###################################################################### ##
##         Copyright IBM Corporation 1988,1991 - All Rights Reserved      ##
##        For full copyright information see:'andrew/config/COPYRITE'     ##
## ###################################################################### ##
# $Disclaimer: 
# Permission to use, copy, modify, and distribute this software and its 
# documentation for any purpose is hereby granted without fee, 
# provided that the above copyright notice appear in all copies and that 
# both that copyright notice, this permission notice, and the following 
# disclaimer appear in supporting documentation, and that the names of 
# IBM, Carnegie Mellon University, and other copyright holders, not be 
# used in advertising or publicity pertaining to distribution of the software 
# without specific, written prior permission.
# 
# IBM, CARNEGIE MELLON UNIVERSITY, AND THE OTHER COPYRIGHT HOLDERS 
# DISCLAIM ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING 
# ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS.  IN NO EVENT 
# SHALL IBM, CARNEGIE MELLON UNIVERSITY, OR ANY OTHER COPYRIGHT HOLDER 
# BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY 
# DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, 
# WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS 
# ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE 
# OF THIS SOFTWARE.
#  $


################################################
set nonomatch
################################################
################################################
if ( $#argv < 3 || $#argv > 4) then
	echo 'usage: depends ${DEPEND} ${CLASS} ${INCLUDES} [${DEFINES}]'
	exit(1)
endif
################################################
set DEPEND=$1
set CLASS=$2
set INCLUDES="$3"
if ($#argv == 4) then
set DEFINES="$4"
else
set DEFINES=" "
endif

################################################
set CHFILES=`/bin/ls *.ch` 
set CFILES=`/bin/ls *.c`
if ( "$#CFILES" == 0 && "$#CHFILES" == 0 ) then
	echo "No Dependencies to Check...continuing...."
	exit(0)
endif
################################################
set TMP_MKFILE=/tmp/Dep1_$$
set CH_DEPENDS=/tmp/Dep2_$$
set C_DEPENDS=/tmp/Dep3_$$
set DEPENDS=.depends
################################################
mv Makefile Makefile.BAK
sed -n -e '1,/##### DEPENDENCY LINE - DO NOT DELETE #####/p' Makefile.BAK > Makefile
################################################
echo >! ${CH_DEPENDS}
if ( "$#CHFILES" != 0) then
	foreach i (${CHFILES})
		${CLASS} -M ${INCLUDES} ${i} | egrep : | egrep -v ":.*:" >> $CH_DEPENDS
		wait
	end
	cat Makefile ${CH_DEPENDS} >> ${TMP_MKFILE}
	set IHFILES=`echo ${CHFILES} | sed -e 's;\.ch;.ih;g'`
	make -f ${TMP_MKFILE} ${IHFILES} INCLUDES="${INCLUDES}"
endif
################################################
echo >! ${C_DEPENDS}
if ( "$#CFILES" != 0) then
	${DEPEND} -s "##### DEPENDENCY LINE - DO NOT DELETE #####" -- ${DEFINES} -- ${INCLUDES} -f ${C_DEPENDS} *.c
endif

sed -e 's/##### DEPENDENCY LINE - DO NOT DELETE #####//g' ${CH_DEPENDS} ${C_DEPENDS} >> Makefile

foreach i ( ${C_DEPENDS} ${CH_DEPENDS} ${TMP_MKFILE} )
    rm -f $i $i.bak
end
rm -f ${DEPENDS}.bak
