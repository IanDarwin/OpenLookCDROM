XCOMM!/bin/sh
XCOMM Copyright 1992 NEC Corporation, Tokyo, Japan.
XCOMM
XCOMM Permission to use, copy, modify, distribute and sell this software
XCOMM and its documentation for any purpose is hereby granted without
XCOMM fee, provided that the above copyright notice appear in all copies
XCOMM and that both that copyright notice and this permission notice
XCOMM appear in supporting documentation, and that the name of NEC
XCOMM Corporation not be used in advertising or publicity pertaining to
XCOMM distribution of the software without specific, written prior
XCOMM permission.  NEC Corporation makes no representations about the
XCOMM suitability of this software for any purpose.  It is provided "as
XCOMM is" without express or implied warranty.
XCOMM
XCOMM NEC CORPORATION DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE,
XCOMM INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN 
XCOMM NO EVENT SHALL NEC CORPORATION BE LIABLE FOR ANY SPECIAL, INDIRECT OR
XCOMM CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF 
XCOMM USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR 
XCOMM OTHER TORTUOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR 
XCOMM PERFORMANCE OF THIS SOFTWARE. 
XCOMM
XCOMM $Id: mkromdic.cpp,v 4.3 1994/04/20 03:47:46 kon Exp $

#include "cannaconf.h"

PATH=CANNABINDIR:$PATH:/bin:/usr/bin:/etc:/usr/etc:/usr/nec/bin:/usr/ucb
export PATH

if [ $# -eq 0 ]; then
  echo "usage: mkromdic [...options] <file name>"
  exit 1
fi

while [ -n "$2" ]
do
	case $1 in
	  "-m")  flag_m="-m" ;;
	  "-n")  flag_n="-n" ;;
          *)     args="$args $1" 
        esac 
	shift
done

if [ ! -r $1 ]; then
  echo "mkromdic: cannot open $1"
  exit 1
fi			

if [ "OPT$flag_m" = "OPT-m" -a "OPT$flag_n" = "OPT-n" ]; then
  echo "mkromdic: option error -m -n"
  exit 1 
fi 

INFILE=$1

OUTFILE=`basename $1`
OUTFILE=`echo $OUTFILE |  awk -F. '{printf "%s",$1; for(i=2; i<NF; i++) printf ".%s",$i}'`

if [ "OPT$flag_n" = "OPT-n" ]; then
	KPDIC=crrdic
	OUTFILE=$OUTFILE.rdic
else
	KPDIC="kpdic $flag_m"
	OUTFILE=$OUTFILE.kp
fi
echo "forcpp -7 < $INFILE |" CPP "$args |forcpp -8 | $KPDIC > $OUTFILE"
forcpp -7 < $INFILE | CPP $args |forcpp -8 | $KPDIC > $OUTFILE
