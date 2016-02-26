#!/bin/sh

# tk -- command-line version of timekeeper.
# $Id: tk.sh,v 1.18 92/11/06 18:15:54 ian Exp $

set -u				# paranoia is the better part of valor

TLOG=$HOME/.timelog
TASKS=${TASKFILE-$HOME/.tasks}
SYSTASKS=/usr/local/lib/timekeeper/tasks
TMP=/tmp/tk.$$

if [ ! -f $TASKS ]; then
	cp $SYSTASKS $TASKS
fi

tkhelp() {

echo "Tk -- timekeeper program.
tk task - starts recording \"task\" (must be in your tasks file).
tk -f hh:mm task - starts recording task at time.
tk end|out - ends
tk -a - appends to your tasks file
tk -p - puts you into edit on your tasks list.
tk -e - puts you into edit on your time log.
tk -l - lists your tasks list.
tk -t - tails your timelog file.
tk -s [file] - summarizes your timelog file or a named file.
" >&2
}

# append -- blindly append one line ($*) to LOG
append() {
	echo "$@" | tee -a $TLOG
}

# ck_append - append "date	time	topic" with checking.
ck_append() {
	DATE=$1; shift
	HHMM=$1; shift
	NMATCHES=`grep -i -c "$*" $TASKS`
	case "$NMATCHES" in
	1)	append "$DATE	$HHMM	Start	`grep -i "$*" $TASKS`"
		continue;;
	0)	echo "$0: Task $* not in $TASKS" >&2;
		exit 1;;
	*)	echo "$0: Task $* ambiguous, matching $NMATCHES lines:">&2
		grep -i "$*" $TASKS >&2
		exit 1;;
	esac
}

# kludge, until sq gets a post-paleolithic OS that can run tkrefmt.
tkrefmt() {
	case "`arch`" in
	sun4)	/usr/local/bin/tkrefmt;;
	*)	rsh sqarc /usr/local/bin/tkrefmt;
	esac
}

tkwrtad() {
date +19%y/%m/%d%t%H:%M
}

tkwrdate() {
date +19%y/%m/%d
}

case $# in
	0)	tkhelp; exit 0;;
esac

case $1 in
	""|-h*)	tkhelp; exit 0;;
	out|end)	append "`tkwrtad`	End";;
	-a)	shift; echo $* >> $TASKS;;
	-e)
		case $# in
		2)	EDITOR=$2;;
		esac
		while :
		do
			${EDITOR-vi} $TLOG
			if sort -c $TLOG
			then
				break
			else
				echo -n "Reenter editor? [yn] "
				read yn
				case "$yn" in
				[Nn]*)  break;;
				esac
			fi
		done
		;;
	-f)	# force time to $1
		HHMM=$2; shift; shift
		case "$HHMM" in
		[0-9][0-9]:[0-9][0-9])	;;	# HH:MM, cool.
		[0-9][0-9][0-9][0-9])		# HHMM, fixable...
			HHMM=`echo $HHMM | sed -e 's/../&:/'`
			;;
		*)	echo "$0: Time $HHMM invalid, must be HH:MM format">&2
			exit 1;
		esac
		case "$*" in
		out|end)	append "`tkwrdate`	$HHMM	End";;
		*)		ck_append `tkwrdate` $HHMM $*
		esac
		;;
	-l)	cat $TASKS;;
	-p)	${EDITOR-vi} $TASKS;;
	-t)	tail $TLOG;;
	-s)	shift; cat ${*-$HOME/.timelog} |
			tkrefmt |
			awk -F'	'  -f /usr/local/lib/tksumm.awk;;
	*)	ck_append `tkwrtad` $*
		;;
esac

