#! /bin/sh

: '@(#)xshev.sh	9.1'

: 1994    Arthur David Olson
:
: The X Consortium, and any party obtaining a copy of these files from
: the X Consortium, directly or indirectly, is granted, free of charge, a
: full and unrestricted irrevocable, world-wide, paid up, royalty-free,
: nonexclusive right and license to deal in this software and
: documentation files {the "Software"}, including without limitation the
: rights to use, copy, modify, merge, publish, distribute, sublicense,
: and/or sell copies of the Software, and to permit persons who receive
: copies from any such party to do so.  This license includes without
: limitation a license to do the foregoing actions under any patents of
: the party supplying this software to the X Consortium.

echo hi | read 2>&- || exec ksh "$0" ${1+"$@"}

O="`basename $0`"

flags=
oops=0
sawid=0
window=

while :
do
	case $1 in
		--)		shift ; break ;;
		-display)	shift
				case $# in
					0)	set x ; break ;;
					*)	flags="$flags -display $1"
						shift ;;
				esac ;;
		-id)		shift
				sawid=1
				case $# in
					0)	oops=1 ; break ;;
					*)	window="$1" ; shift ;;
				esac ;;
		*)		break ;;
	esac
done

case $# in
	0)	;;
	*)	oops=1 ;;
esac

case $oops in
	1)	O=`basename "$0"`
		echo "$O: usage is $O" \
			"[-display displayname]" \
			"[-id windowid]" \
			"# 7.1" >&2
		exit 1 ;;
esac

set -e

INNER_WINDOW_WIDTH=50
INNER_WINDOW_HEIGHT=50
INNER_WINDOW_BORDER=4
INNER_WINDOW_X=10
INNER_WINDOW_Y=10
OUTER_WINDOW_MIN_WIDTH=`expr \
	$INNER_WINDOW_WIDTH + \
	2 \* \( $INNER_WINDOW_BORDER + $INNER_WINDOW_X \)`
OUTER_WINDOW_MIN_HEIGHT=`expr \
	$INNER_WINDOW_HEIGHT + \
	2 \* \( $INNER_WINDOW_BORDER + $INNER_WINDOW_Y \)`
OUTER_WINDOW_DEF_WIDTH=`expr $OUTER_WINDOW_MIN_WIDTH + 100`
OUTER_WINDOW_DEF_HEIGHT=`expr $OUTER_WINDOW_MIN_HEIGHT + 100`
OUTER_WINDOW_DEF_X=100
OUTER_WINDOW_DEF_Y=100
OUTER_WINDOW_BORDER=2

# Since the cooperating process ends up in a NextEvent loop,
# and is started off in "signal(SIGINT, SIG_IGN)" mode by ksh,
# it will fail to notice immediately if we ourselves are interrupted.
# So we ask for the opportunity to end it.

case `trap` in
	*2:*3:*|*3:*2:)	;;
	*2:*)	trap 'kill 0' 3 ;;
	*3:*)	trap 'kill 0' 2 ;;
	*)	trap 'kill 0' 2 3 ;;
esac

dox -verbose $flags |&

case $sawid in
	0)	print -p createsimplewindow root \
			$OUTER_WINDOW_DEF_X \
			$OUTER_WINDOW_DEF_Y \
			$OUTER_WINDOW_DEF_WIDTH \
			$OUTER_WINDOW_DEF_HEIGHT \
			$OUTER_WINDOW_BORDER \
			black white
		read -p window
		print -p createsimplewindow $window \
			$INNER_WINDOW_X \
			$INNER_WINDOW_Y \
			$INNER_WINDOW_WIDTH \
			$INNER_WINDOW_HEIGHT \
			$INNER_WINDOW_BORDER \
			black white
		read -p subwindow
		;;
esac

# resizeredirect deliberately omitted below

print -p selectinput $window \
keypress+keyrelease+buttonpress+buttonrelease+enterwindow+leavewindow+\
pointermotion+button1motion+button2motion+button3motion+button4motion+\
button5motion+buttonmotion+keymapstate+exposure+visibilitychange+\
structurenotify+substructurenotify+substructureredirect+focuschange+\
propertychange+colormapchange+ownergrabbutton

case $sawid in
	0)	print -p storename $window \""$O"\"
		print -p seticonname $window \""$O"\"
		print -p mapwindow $subwindow
		print -p mapwindow $window
		;;
esac

while :
do
	print -p nextevent
	read -p
	print "$REPLY"
done
