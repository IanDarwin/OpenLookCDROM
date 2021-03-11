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
XCOMM $Id: mkbindic.cpp,v 5.3 1994/04/20 03:47:32 kon Exp $
#include "cannaconf.h"
#if defined(SYSV) || defined(SVR4)
# ifdef nec_ews
PATH=CANNABINDIR:/bin:/usr/bin:/etc:/usr/etc:/usr/ucb:/usr/nec/bin:$PATH;
# else
PATH=CANNABINDIR:/bin:/usr/bin:/etc:/usr/etc:/usr/ucb:$PATH;
# endif /* nec_ews */
#else
PATH=CANNABINDIR:/bin:/usr/bin:/etc:/usr/etc:/usr/5bin:$PATH;
#endif /* SYSV || SVR4 */

export PATH;
text_file=;
dic_name=;
cpp_text=;
spl_text=;
bck_text=;
flag=;

/* main */
{
    while [ $# -gt 0 ]; do {
        case $1 in
	-m)
	    if [ "$flag" = "" ]; then {
		flag="-m";
	    } else {
		echo "usage: mkbindic [-m|-s] [-name dicname] textfile [cpp-args ...]";
		exit 1;
	    } fi;
	    ;;
	-s)
	    if [ "$flag" = "" ]; then {
		flag="-s";
	    } else {
		echo "usage: mkbindic [-m|-s] [-name dicname] textfile [cpp-args ...]";
		exit 1;
	    } fi;
	    ;;
	-name)
    	    shift;
	    if [ "$dic_name" = "" ]; then {
		dic_name=$1;
	    } else {
		echo "usage: mkbindic [-m|-s] [-name dicname] textfile [cpp-args ...]";
		exit 1;
	    } fi;
	    ;;
	*)
	    if [ "$text_file" = "" ]; then {
		text_file=$1;
	    } else {
		args="$args $1";
	    } fi;
	    ;;
	esac;
	shift;
    }; done
/* input file */
    if [ "$text_file" = "" ]; then
	echo "usage: mkbindic [-m|-s] [-name dicname] textfile [cpp-args ...]";
	exit 1;
    fi;
    if [ ! -r $text_file ]; then 
	echo "mkbindic: cannot open $text_file";
	exit 1;
    fi;
    if [ -d $text_file ]; then 
	echo "mkbindic: cannot open $text_file";
	exit 1;
    fi;
    if [ "$dic_name" != "" ]; then
	dic_ck="`echo $dic_name | \
		    awk -F. '{
			printf("%s", $NF)
		    }'`";
        if [ "$dic_ck" != "d" ]; then
	    echo "Invalid name : $dic_name";
            exit 1;
        fi;
        dic_ckn="`echo $dic_name | \
		awk -F/ '{print $NF}' | \
		awk -F. '{print NF}'`";
        if [ $dic_ckn -ne 2 ]; then
	    echo "Invalid name : $dic_name";
            exit 1;
        fi;
        dic_ck="`echo $dic_name | \
		awk -F/ '{print $NF}' | \
		awk -F. '{
			printf("%s", $(NF-1))
                }'`";
        if [ "$dic_ck" =  "" ]; then
	    echo "Invalid name : $dic_name";
            exit 1;
        fi
    fi;
/* mwd or swd */
    if [ "$flag" = "" ]; then
	flag="-m";
    fi;
/* temp file of cpp */
    cpp_text="`echo $text_file | \
		awk -F/ '{print $NF}' | \
		awk -F. '{
		    for(i = 1; i < NF; i++)
			printf("%s.", $i)
		}'`";
    if [ "$cpp_text" = "" ]; then
	cpp_text="`echo $text_file | \
		awk -F/ '{print $NF}'`".;
    fi;
    cpp_text=/tmp/"$cpp_text"cpp;
/* temp file of splitword */
    spl_text="`echo $text_file | \
		awk -F/ '{print $NF}' | \
		awk -F. '{
		    for(i = 1; i < NF; i++)
			printf("%s.", $i)
		}'`";
    if [ "$spl_text" = "" ]; then
	spl_text="`echo $text_file | \
		awk -F/ '{print $NF}'`".;
    fi;
    spl_text=/tmp/"$spl_text"spl;
/* temp file of backup */
    bck_text="`echo $text_file | \
		awk -F/ '{print $NF}' | \
		awk -F. '{
		    for(i = 1; i < NF; i++)
			printf("%s.", $i)
		}'`";
    if [ "$bck_text" = "" ]; then
	bck_text="$text_file".;
    fi;
    bck_text="$bck_text"bk;
/* output file */
    out="`echo $text_file | \
		awk -F/ '{print $NF}' | \
		awk -F. '{
		    for(i = 1; i < NF; i++)
			printf("%s.", $i)
		}'`";
    if [ "$out" = "" ]; then
	out="`echo $text_file | \
		awk -F/ '{print $NF}'`".;
    fi
    out="$out"d;
    if [ "$dic_name" = "" ]; then
	dic_name=$out;
    fi
/* child name */
    child="`echo $text_file | \
		awk -F/ '{print $NF}' | \
		awk -F. '{
		    for(i = 1; i < NF; i++)
			printf("%s.", $i)
		}'`";
    if [ "$child" = "" ]; then
	child="`echo $text_file | \
		awk -F/ '{print $NF}'`".;
    fi
    if [ "$child" = "." ]; then
	  echo "Invalid name : $text_file";
	  exit 1;
    fi

    chinum="`echo $child | \
	  awk -F. '{print NF}'`";

    if [ $chinum -ne 2 ]; then
	  echo "Invalid name : $text_file";
	  exit 1;
    fi
    if [ "$flag" = "-m" ]; then
	child="$child"mwd;
    else
	child="$child"swd;
    fi
/* main routin */
    trap "rm -f $cpp_text $spl_text; exit 1;" 2;
    echo "forcpp -7 < $text_file | /lib/cpp $args | forcpp -8 > $cpp_text";
    forcpp -7 < $text_file | /lib/cpp $args | forcpp -8 > $cpp_text;
    if [ $? != 0 ]; then
	echo "mkbindic: fatal error. exit";
	rm -f $cpp_text $spl_text;
	exit 1;
    fi
    echo "splitword $cpp_text > $spl_text";
    splitword $cpp_text > $spl_text;
    if [ $? != 0 ]; then
	echo "mkbindic: fatal error. exit";
	rm -f $cpp_text $spl_text;
	exit 1;
    fi;
    echo "mv $text_file $bck_text";
    mv $text_file $bck_text;
    echo "forsort -7 < $spl_text | sort -d | forsort -8 | mergeword -X > $text_file";
    forsort -7 < $spl_text | sort -d | forsort -8 | mergeword -X > $text_file;
    if [ $? != 0 ]; then
        mv $bck_text $text_file;
	echo "mkbindic: fatal error. exit";
	rm -f $cpp_text $spl_text;
	exit 1;
    fi;
#ifdef nec_ews
/* \c for crxdic echo back unexpected \n */
    echo "crxdic $flag -o $dic_name $text_file\c";
#else
    echo "crxdic $flag -o $dic_name $text_file";
#endif
    crxdic $flag -o $dic_name $text_file;
    if [ $? != 0 ]; then
        mv $bck_text $text_file;
	echo "mkbindic: fatal error. exit";
	rm -f $cpp_text $spl_text;
	exit 1;
    fi;
    echo "crfreq $dic_name $child";
    crfreq $dic_name $child;
    if [ $? != 0 ]; then
        mv $bck_text $text_file;
	echo "mkbindic: fatal error. exit";
	rm -f $cpp_text $spl_text;
	exit 1;
    fi;
    mv $bck_text $text_file;
    echo "rm $cpp_text $spl_text";
    rm -f $cpp_text $spl_text;
    exit $?;
}
