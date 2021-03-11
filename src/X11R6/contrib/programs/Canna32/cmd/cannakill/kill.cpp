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

XCOMM $Id: kill.cpp,v 5.3 1994/04/20 03:46:51 kon Exp $
#include "cannaconf.h"
#ifndef LOCKFILEPATH
#define LOCKFILEPATH /usr/spool/canna/lock/.CANNALOCK
#endif
CANNALOCKFILE=LOCKFILEPATH;
MAXLOOP=20;

XCOMM main()
{
    if [ $# -gt 1 ]; then
	echo 'Usage: cannakill [ServerNumber|-all|-help]';
	exit 2;
    fi;
/* default */
    if [ "$1" = "" -o "$1" = "0" ]; then {
	if [ ! -r $CANNALOCKFILE ]; then
	    echo "Error: cannaserver is not running.";
	    exit 1;
	fi;
	PID=`cat $CANNALOCKFILE 2> /dev/null`;
	if [ "$PID" = "" ]; then {
	    echo "Error: cannaserver is not running.";
	    rm -f $CANNALOCKFILE;
	    exit 1;
	} fi;
	kill $PID;
	if [ $? != 0 ]; then {
	    echo "Error: cannaserver is not running.";
	    exit 1;
	} fi
	LOOPN=0;
	while : ; do {
	    if [ ! -r "$CANNALOCKFILE" ]; then
		break;
	    fi;
	    sleep 1;
	    LOOPN=`expr $LOOPN + 1`;
	    if [ $LOOPN -gt $MAXLOOP ]; then {
		echo "give up to terminate cannaserver !!";
		exit 1;
	    } fi;
#ifdef nec_ews
	    if [ `expr $LOOPN % 10` = 0 ]; then
		echo '.\c';
	    fi;
#endif
	} done;
	exit 0;
    } fi;
    case $1 in
	-h*)
		echo 'Usage: cannakill [ServerNumber|-all|-help]';
		exit 0;
		;;
	-a*)
		for LOCKF in $CANNALOCKFILE*; do {
		    if [ ! -r "$LOCKF" ]; then
			continue;
		    fi
		    PID=`cat -s $LOCKF`;
		    if [ "$PID" = "" ]; then {
			rm -f $LOCKF;
			continue;
		    } fi;
		    kill $PID;
		    if [ $? = 0 ]; then {
			LOOPN=0;
			while : ; do {
			    if [ ! -r "$LOCKF" ]; then
				break;
			    fi;
			    sleep 1;
			    LOOPN=`expr $LOOPN + 1`;
			    if [ $LOOPN -gt $MAXLOOP ]; then {
				echo "give up to terminate cannaserver !!";
				break;
			    } fi;
#ifdef nec_ews
			    if [ `expr $LOOPN % 10` = 0 ]; then
				echo '.\c';
			    fi;
#endif
			} done;
			continue;
		    } fi;
		    echo "Error: cannaserver($2) is not running.";
		    continue;
		} done;
		exit 0;
		;;
	*)
		if [ ! -r "$CANNALOCKFILE:$1" ]; then
		    echo "Error: cannaserver($1) is not running.";
		    exit 1;
	        fi;
		PID=`cat $CANNALOCKFILE:$1 2> /dev/null`;
		if [ "$PID" = "" ]; then {
		    echo "Error: cannaserver($1) is not running.";
		    rm -f $CANNALOCKFILE:$1;
		    exit 1;
		} fi;
		kill $PID;
		if [ $? = 0 ]; then {
		    LOOPN=0;
		    while : ; do {
			if [ ! -f "$CANNALOCKFILE:$1" ]; then
			    break;
			fi;
			sleep 1;
			LOOPN=`expr $LOOPN + 1`;
			if [ $LOOPN -gt $MAXLOOP ]; then {
			    echo "give up to terminate cannaserver !!";
			    break;
			} fi;
#ifdef nec_ews
			if [ `expr $LOOPN % 10` = 0 ]; then {
			    echo '.\c';
			} fi;
#endif
		    } done;
		    exit 0;
		} fi;
		echo "Error: cannaserver($2) is not running.";
		exit 1;
		;;
    esac;
}
