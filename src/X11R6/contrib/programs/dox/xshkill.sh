#! /bin/sh

: '@(#)xshkill.sh	9.1'

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

flags=
id=+

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
		-frame)		flags="$flags -frame" ; shift ;;
		-id)		shift
				case $# in
					0)	set x ; break ;;
					*)	id="$1" ; shift ;;
				esac ;;
		*)		break ;;
	esac
done

case $# in
	0)	;;
	*)	O=`basename "$0"`
		echo "$O: usage is $O" \
			"[-display displayname]" \
			"[-id resource]" \
			"[-frame]" \
			"# 9.1" >&2
		exit 1 ;;
esac

exec dox $flags killclient "$id"
