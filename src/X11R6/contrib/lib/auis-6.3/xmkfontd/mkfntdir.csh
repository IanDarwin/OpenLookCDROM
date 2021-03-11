#!/bin/csh -f

# Copyright 1988, 1992 Carnegie Mellon University and IBM.  All rights reserved.
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

#
# This script is used to map the short file names used to hold 
# font descriptions to reasonable names.  A mechanism in X
# is used.  The various aliases are placed in "fonts.alias" in 
# the directory that holds the font descriptions.
#

set FONTDIR=$1
set EXTENSION=$2
set TMP=/tmp/tmp.a.$$
set OUTFILE=fonts.alias



echo "indexing fonts in $FONTDIR using extension $EXTENSION"

cd ${FONTDIR}
echo >! ${TMP}

#
# First do the adobe fonts from overhead/fonts/fonts/adobe.
# We alias the old, long names too just in case.
#
foreach REAL (cou hel symb tim)
	switch (${REAL})
		case "cou":
			set LINK1="andytype"
			set LINK2="courier"
			breaksw
		case "hel":
			set LINK1="andysans"
			set LINK2="helvetica"
			breaksw
		case "symb":
			set LINK1="andysymbol"
			set LINK2="symbol"
			breaksw
		case "tim":
			set LINK1="andy"
			set LINK2="times"
			breaksw
		default:
			echo "ERROR LINKING FONTS (${REAL})"
			exit(1)
	endsw
	set NAMELIST=`echo ${REAL}*${EXTENSION}`
	foreach NAME ($NAMELIST)
		set FNAME=`basename $NAME $EXTENSION`
		set NAME=`echo ${FNAME} | sed -e s+$REAL+$LINK2+g`
		set LNAME=`echo ${FNAME} | sed -e s+$REAL+$LINK1+g`
		echo "${LNAME}	${NAME}" | tee -a ${TMP}
	end
end

#
# Now do all the ATK program fonts from atk/...
#
foreach REAL (msgs con)
	switch (${REAL})
		case "msgs":
			set LINK="messages"
			breaksw
		case "con":
			set LINK="console"
			breaksw
		default:
			echo "ERROR LINKING FONTS (${REAL})"
			exit(1)
	endsw
	set NAMELIST=`echo ${REAL}*${EXTENSION}`
	foreach NAME ($NAMELIST)
		set FNAME=`basename $NAME $EXTENSION`
		set LNAME=`echo ${FNAME} | sed -e s+$REAL+$LINK+g`
		echo "${LNAME}	${FNAME}" | tee -a ${TMP}
	end
end

cat ${TMP} >> ${OUTFILE}
rm -f ${TMP}
