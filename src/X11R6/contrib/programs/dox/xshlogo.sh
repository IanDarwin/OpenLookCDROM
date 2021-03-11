#! /bin/sh

: '@(#)xshlogo.sh	9.1'

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

while :
do
	case $1 in
		--)		shift ; break ;;
		-display|-d)	shift
				case $# in
					0)	set x ; break ;;
					*)	flags="$flags -display $1"
						shift ;;
				esac ;;
		*)		break ;;
	esac
done

case $# in
	0)	;;
	*)	echo "$O: usage is $O" \
			"[-d displayname]" \
			"[-display displayname]" \
			"# 9.1" >&2
		exit 1 ;;
esac

width=64
height=64

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

print -p createsimplewindow root 0 0 $width $height 1 black white
read -p window
print -p selectinput $window exposure+structurenotify
print -p storename $window \""$O"\"
print -p seticonname $window \""$O"\"
print -p mapwindow $window

while :
do
	set -e
	print -p nextevent
	read -p type balance
	case "$type" in
		ConfigureNotify)	set $balance
					while :
					do
						case $1 in
							width)	width=$2 ;;
							height)	height=$2 ;;
						esac
						shift
						case $# in
							0)	break ;;
						esac
					done
					continue ;;
	esac
	case "$balance" in
		*" 0")	;;
		*)	continue ;;
	esac
	set +e
	(( size = width ))
	if (( size > height ))
	then
		(( size = height ))
	fi
	if (( size % 2 != 0 ))
	then
		(( size = size - 1 ))
	fi
	if (( size <= 0 ))
	then
		(( size = 2 ))
	fi
	(( x = ( width - size ) / 2 ))
	(( y = ( height - size ) / 2 ))
	(( d11 = size / 11 ))
	if (( d11 < 1 ))
	then
		(( d11 = 1 ))
	fi
	(( d21 = ( d11 + 3 ) / 4 ))
	(( d31 = d11 + d11 + d21 ))

	(( p0x = x + size ))
	(( p0y = y ))
	(( p1x = x + size - d31 ))
	(( p1y = y ))
	(( p2x = x ))
	(( p2y = y + size ))
	(( p3x = x + d31 ))
	(( p3y = y + size ))
	points="$p0x $p0y $p1x $p1y $p2x $p2y $p3x $p3y"
	print -p setforeground defaultgc black
	print -p setbackground defaultgc white
	print -p fillpolygon $window defaultgc $points convex origin

	(( p0x = x + d31 / 2 ))
	(( p0y = y + size ))
	(( p1x = x + size / 2 ))
	(( p1y = y + size / 2 ))
	(( p2x = x + ( size / 2 ) + ( d31 - ( d31 / 2 ) ) ))
	(( p2y = y + size / 2 ))
	(( p3x = x + d31 ))
	(( p3y = y + size ))
	points="$p0x $p0y $p1x $p1y $p2x $p2y $p3x $p3y"
	print -p setforeground defaultgc white
	print -p setbackground defaultgc black
	print -p fillpolygon $window defaultgc $points convex origin

	(( p0x = x + size - d31 / 2 ))
	(( p0y = y ))
	(( p1x = x + size / 2 ))
	(( p1y = y + size / 2 ))
	(( p2x = x + ( size / 2 ) - ( d31 - ( d31 / 2 ) ) ))
	(( p2y = y + size / 2 ))
	(( p3x = x + size - d31 ))
	(( p3y = y ))
	points="$p0x $p0y $p1x $p1y $p2x $p2y $p3x $p3y"
	print -p setforeground defaultgc white
	print -p setbackground defaultgc black
	print -p fillpolygon $window defaultgc $points convex origin

	(( p0x = x ))
	(( p0y = y ))
	(( p1x = x + size / 4 ))
	(( p1y = y ))
	(( p2x = x + size ))
	(( p2y = y + size ))
	(( p3x = x + size - size / 4 ))
	(( p3y = y + size ))
	points="$p0x $p0y $p1x $p1y $p2x $p2y $p3x $p3y"
	print -p setforeground defaultgc black
	print -p setbackground defaultgc white
	print -p fillpolygon $window defaultgc $points convex origin

	(( p0x = x + size - d11 ))
	(( p0y = y ))
	(( p1x = x + size - ( d11 + d21 ) ))
	(( p1y = y ))
	(( p2x = x + d11 ))
	(( p2y = y + size ))
	(( p3x = x + d11 + d21 ))
	(( p3y = y + size ))
	points="$p0x $p0y $p1x $p1y $p2x $p2y $p3x $p3y"
	print -p setforeground defaultgc white
	print -p setbackground defaultgc black
	print -p fillpolygon $window defaultgc $points convex origin
done
