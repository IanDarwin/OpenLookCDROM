head     1.18;
branch   ;
access   ;
symbols  ;
locks    ;
comment  @# @;


1.18
date     92.11.06.18.15.54;  author ian;  state Exp;
branches ;
next     1.17;

1.17
date     92.09.10.15.56.28;  author ian;  state Exp;
branches ;
next     1.16;

1.16
date     92.09.09.17.27.36;  author ian;  state Exp;
branches ;
next     1.15;

1.15
date     92.09.09.14.16.57;  author ian;  state Exp;
branches ;
next     1.14;

1.14
date     92.09.09.13.16.04;  author ian;  state Exp;
branches ;
next     1.13;

1.13
date     92.09.09.12.54.44;  author ian;  state Exp;
branches ;
next     1.12;

1.12
date     92.09.09.12.47.21;  author ian;  state Exp;
branches ;
next     1.11;

1.11
date     92.08.27.12.02.24;  author ian;  state Exp;
branches ;
next     1.10;

1.10
date     92.06.29.13.37.55;  author ian;  state Exp;
branches ;
next     1.9;

1.9
date     92.06.29.13.36.04;  author ian;  state Exp;
branches ;
next     1.8;

1.8
date     92.06.24.11.27.41;  author ian;  state Exp;
branches ;
next     1.7;

1.7
date     92.06.02.15.36.39;  author ian;  state Exp;
branches ;
next     1.6;

1.6
date     92.06.02.13.19.43;  author ian;  state Exp;
branches ;
next     1.5;

1.5
date     92.06.02.13.18.33;  author ian;  state Exp;
branches ;
next     1.4;

1.4
date     92.05.29.10.12.36;  author ian;  state Exp;
branches ;
next     1.3;

1.3
date     92.05.26.16.00.53;  author ian;  state Exp;
branches ;
next     1.2;

1.2
date     92.05.26.10.10.35;  author ian;  state Exp;
branches ;
next     1.1;

1.1
date     92.05.08.15.20.24;  author ian;  state Exp;
branches ;
next     ;


desc
@Command line version of timekeeper.
@


1.18
log
@Print help if no args.
@
text
@#!/bin/sh

# tk -- command-line version of timekeeper.
# $Id: tk.sh,v 1.17 92/09/10 15:56:28 ian Exp $

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
	echo "$@@" | tee -a $TLOG
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

@


1.17
log
@Allow "-e editor" as well as just plain "-e".
@
text
@d4 1
a4 1
# $Id: tk.sh,v 1.16 92/09/09 17:27:36 ian Exp $
d68 4
@


1.16
log
@Usability: allow -f HHMM as well as -f HH:MM.
@
text
@d4 1
a4 1
# $Id: tk.sh,v 1.15 92/09/09 14:16:57 ian Exp $
d73 5
a77 1
	-e)	while :
@


1.15
log
@Add error checking for -f's argument; reinstake tkwrdate() as it's
needed 2 or more places. Check for out or end with -f.
@
text
@d4 1
a4 1
# $Id: tk.sh,v 1.14 92/09/09 13:16:04 ian Exp $
d91 4
a94 1
		[0-9][0-9]:[0-9][0-9])	;;
@


1.14
log
@Use /tmp/tk.$$ instead of /tmp/id!
@
text
@d4 1
a4 1
# $Id: tk.sh,v 1.13 92/09/09 12:54:44 ian Exp $
d32 1
a32 1
# append -- append one line ($*) to LOG
d41 1
a41 2
	grep -i "$*" $TASKS > $TMP
	NMATCHES=`wc -l < $TMP | tr -d ' '` # tr kludge makes "case" work...
d43 4
a46 2
	1)	continue;;
	0)	echo "$0: Task $* not in $TASKS" >&2; exit 1;;
d48 1
a48 1
		cat $TMP >&2
a50 1
	append "$DATE	$HHMM	Start	`cat $TMP`"
d65 4
d89 10
a98 2
		shift
		ck_append `date +19%y/%m/%d`	$*
@


1.13
log
@Finish up the previous change for ck_append and -f.
@
text
@d4 1
a4 1
# $Id: tk.sh,v 1.12 92/09/09 12:47:21 ian Exp $
d11 1
d41 2
a42 2
	grep -i "$*" $TASKS > /tmp/id
	NMATCHES=`wc -l</tmp/id | tr -d ' '` # tr kludge makes "case" work...
d47 1
a47 1
		cat /tmp/id >&2
d50 1
a50 1
	append "$DATE	$HHMM	Start	`cat /tmp/id`"
@


1.12
log
@Consolidate "append with checking" into new shfunction "ck_append",
which isn't QUITE finished. Use this to add new -f hh:mm logic to 
let you retroactively begin a task.
@
text
@d4 1
a4 1
# $Id: tk.sh,v 1.11 92/08/27 12:02:24 ian Exp $
d40 2
a41 2
	# the tr is a kludge to make the "case" work...
	NMATCHES=`grep -i "$*" $TASKS | wc -l | tr -d ' '`
d45 2
a46 2
	*)	echo "$0: Task $* ambiguous:">&2
		grep -i "$*" $TASKS >&2
d49 1
a49 1
	append "$DATE	$HHMM	Start	$*"
@


1.11
log
@Embed tk -e's edit session in a loop that checks for out-of-order lines
(suggested by msb).
@
text
@d4 1
a4 1
# $Id: tk.sh,v 1.10 92/06/29 13:37:55 ian Exp $
d6 2
d20 2
a21 2
tk end - ends
tk out - writes logout record
d36 16
a67 1
	-p)	${EDITOR-vi} $TASKS;;
d83 4
d88 1
d93 1
a93 14
	*)
		task=`grep -i "$*" $TASKS` || {
					echo "Project $* not found" >&2
					exit 1
					}
		# check if "$task" has a newline, if so, too many matches.
		case $task in
		*'
'*)			echo "Tk: Topic \"$*\" ambiguous:" >&2
			echo "$task" >&2
			exit 1
			;;
		esac
		append "`tkwrtad`	Start	$task"
@


1.10
log
@Use cat, not $PAGER, to list the tasks file. Decided by two votes for
cat, 1 for $PAGER, after mail to all of R&D.
@
text
@d4 1
a4 1
# $Id: tk.sh,v 1.9 92/06/29 13:36:04 ian Exp $
d51 15
a65 1
	-e)	${EDITOR-vi} $TLOG;;
@


1.9
log
@New shfn "append" adds a line to TLOGfile, used several places.
Output is via "echo ... | tee -a $TLOG" which should be both optimal
and portable (tnx msb). Add msb's kludge so tk -s works on sq.
@
text
@d4 1
a4 1
# $Id: tk.sh,v 1.8 92/06/24 11:27:41 ian Exp $
d52 1
a52 1
	-l)	${PAGER-more} $TASKS;;
@


1.8
log
@Add msb's merger of the tksumm command here as '-s' option (it just
calls tkrefmt and awk -f tksumm.awk); use tail -1 to print the just-added
line instead of "|tee /dev/tty", since the latter loses big time under rsh.
@
text
@d4 1
a4 1
# $Id: tk.sh,v 1.7 92/06/02 15:36:39 ian Exp $
d29 13
d48 1
a48 1
	out|end)	echo "`tkwrtad`	End" >> $TLOG;;
d70 1
a70 3
		# avoid the obvious "|tee /dev/tty", as it loses under rsh!
		echo "`tkwrtad`	Start	$task" >> $TLOG
		tail -1 $TLOG
@


1.7
log
@Mixed one change in 1.6
@
text
@d4 1
a4 1
# $Id: tk.sh,v 1.6 92/06/02 13:19:43 ian Exp $
d24 2
a25 1
tk -t - tails your timelog file
d41 3
d57 3
a59 1
		echo "`tkwrtad`	Start	$task" |tee /dev/tty >> $TLOG
@


1.6
log
@Make tk.sh and timekeeper.1 agree on the TASKFILE shell variable.
@
text
@d4 1
a4 1
# $Id: tk.sh,v 1.5 92/06/02 13:18:33 ian Exp $
d11 1
a11 1
	cp $SYSTASKS $TOPICS
@


1.5
log
@End and out are synonymous; change filename from topics to tasks.
@
text
@d4 1
a4 1
# $Id: tk.sh,v 1.4 92/05/29 10:12:36 ian Exp $
d7 1
a7 1
TASKS=${TOPICFILE-$HOME/.tasks}
@


1.4
log
@Add code that provides default topics file.
@
text
@d4 1
a4 1
# $Id: tk.sh,v 1.3 92/05/26 16:00:53 ian Exp $
d7 2
a8 2
TOPICS=${TOPICFILE-$HOME/.topics}
SYSTOPICS=/usr/local/lib/timekeeper/topics
d10 2
a11 2
if [ ! -f $TOPICS ]; then
	cp $SYSTOPICS $TOPICS
d17 1
a17 1
tk proj - starts recording \"proj\" (must be in your projects file).
d20 2
a21 2
tk -a - appends to your project file
tk -p - puts you into edit on your project list.
d23 1
a23 1
tk -l - lists your project list.
d34 3
a36 4
	end)	echo "`tkwrtad`	End" >> $TLOG;;
	out)	echo "`tkwrtad`	End	RECORDING" >> $TLOG;;
	-a)	shift; echo $* >> $TOPICS;;
	-p)	${EDITOR-vi} $TOPICS;;
d38 1
a38 1
	-l)	${PAGER-more} $TOPICS;;
d41 1
a41 1
		topic=`grep -i "$*" $TOPICS` || {
d45 2
a46 2
		# check if "$topic" has a newline, if so, too many matches.
		case $topic in
d49 1
a49 1
			echo "$topic" >&2
d53 1
a53 1
		echo "`tkwrtad`	Start	$topic" |tee /dev/tty >> $TLOG
@


1.3
log
@Print the line as we put it onto the logfile.
@
text
@d4 1
a4 1
# $Id: tk.sh,v 1.2 92/05/26 10:10:35 ian Exp $
d8 5
@


1.2
log
@Now use -e for edit timelog and -p for edit projects.
@
text
@d4 1
a4 1
# $Id: tk.sh,v 1.1 1992/05/08 15:20:24 ian Exp $
d49 2
a50 1
		echo "`tkwrtad`	Start	$topic" >> $TLOG;;
@


1.1
log
@Initial revision
@
text
@d4 1
a4 1
# $Id$
a8 8
# tk proj - starts recording project
# tk end - ends
# tk out - writes logout record
# tk -a - appends to your project file
# tk -e - puts you into edit on your project list.
# tk -l - lists your project list.
# tk -g - puts you into edit on your time log.

d11 11
a21 4
echo "Tk -- timekeeper. -a appends to projfile, -e edits projfile, -l
lists it, -g edits history.  -a adds a project to your project file.
end - ends work on a project, out - writes logoff record. Anything else
is a project, which must be in your projects list." >&2
d23 2
d29 2
d32 2
a33 1
	-e)	${EDITOR-vi} $TOPICS;;
d35 1
a35 3
	-g)	${EDITOR-vi} $TLOG;;
	end)	(tkwrtad;	echo "	End") >> $TLOG;;
	out)	(tkwrtad;	echo "	End	RECORDING") >> $TLOG;;
d37 1
a37 1
		grep "$*" $TOPICS >/dev/null || {
d41 9
a49 1
		(tkwrtad; echo "	Start	$*") >> $TLOG;;
@
