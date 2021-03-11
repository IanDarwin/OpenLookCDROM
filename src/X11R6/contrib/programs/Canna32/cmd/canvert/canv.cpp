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

XCOMM $Id: canv.cpp,v 3.3 1994/04/20 03:47:06 kon Exp $
#include "cannaconf.h"
PATH=CANNABINDIR:$PATH:/bin:/usr/bin:/etc:/usr/etc:/usr/nec/bin:/usr/necbin:/usr/ucb;
export PATH;

/* main */
{
    if [ $# -lt 5 ]; then {
	echo "usage: canvert [-d dicname|-c] -o old -n new";
	exit 1;
    } fi;
    case $# in
    6)
	if [ $1 = "-d" -a $3 = "-o" -a $5 = "-n" ]; then
            dicold="`echo $4 | \
                        awk -F. '{
                        printf("%s", $NF)
                        }'`";
            if [ "$dicold" != "d" ]; then
               echo "Invalid name : $4";
               exit 1;
            fi
	    dic_ck="`echo $4 | \
			awk -F/ '{print $NF}' | \
		        awk -F. '{print NF}'`";
	    if [ $dic_ck -ne 2 ]; then
		  echo "Invalid name : $4";
		  exit 1;
	    fi
	    dic_ck="`echo $4 | \
			awk -F/ '{print $NF}' | \
		        awk -F. '{print $(NF-1)}'`";
	    if [ "$dic_ck" = "" ]; then
		  echo "Invalid name : $4";
		  exit 1;
	    fi
            dicnew="`echo $6 | \
                        awk -F. '{
                                printf("%s", $NF)
                        }'`";
            if [ "$dicnew" != "d" ]; then
                echo "Invalid name : $6";
                exit 1;
            fi
	    dic_ck="`echo $6 | \
			awk -F/ '{print $NF}' | \
		        awk -F. '{print NF}'`";
	    if [ $dic_ck -ne 2 ]; then
		  echo "Invalid name : $6";
		  exit 1;
	    fi
	    dic_ck="`echo $6 | \
			awk -F/ '{print $NF}' | \
		        awk -F. '{print $(NF-1)}'`";
	    if [ "$dic_ck" = "" ]; then
		  echo "Invalid name : $6";
		  exit 1;
	    fi

	    tmpfile="`echo $6 | \
			awk -F/ '{print $NF}' | \
			awk -F. '{
			    for(i = 1; i < NF; i++)
				printf("%s.", $i)
			}'`";
	    if [ "$tmpfile" = "" ]; then
		tmpfile="`echo $6 | \
			awk -F/ '{print $NF}'`".;
	    fi
	    if [ "$tmpfile" = "." ]; then
		  echo "Invalid name : $6";
	  	  exit 1;
	    fi
	    tmpfile=/tmp/"$tmpfile"t;
	/* ファイル名, 辞書名 , 暫定ファイル */
	    echo "dpwdic $4 $2 > $tmpfile";
	    dpwdic $4 $2 > $tmpfile;
	    if [ $? != 0 ]; then
		echo "cannot dump $4";
                if [ -r $tmpfile ]; then
                   rm -f $tmpfile
                fi
		exit 1;
	    fi;
	/* 辞書名, ファイル名 */
	    echo "mkbindic -m -name $6 $tmpfile";
	    mkbindic -m -name $6 $tmpfile;
            if [ -r $tmpfile ]; then
               rm -f $tmpfile
            fi
	    exit $?;
	fi;
	;;
    5)

	if [ $1 = "-c" -a $2 = "-o" -a $4 = "-n" ]; then
	    if [ ! -f "$3" ]; then
		echo "no such file $3";
		exit $?;
	    fi;
	    echo "itoc $3 > $5";
	    itoc $3 > $5;
	    exit $?;
	fi;
	;;
    *)
	echo "usage: canvert [-d dicname|-c] -o old -n new";
	exit 1;
	;;
    esac;
    echo "usage: canvert [-d dicname|-c] -o old -n new";
    exit 1;
}

main $*
