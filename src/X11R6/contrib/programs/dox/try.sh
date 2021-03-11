#! /bin/sh

: '@(#)try.sh	9.1'

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
		*)		break ;;
	esac
done

case $# in
	0)	;;
	*)	O=`basename "$0"`
		echo "$O: usage is $O" \
			"[-display displayname]" \
			"# 9.1" >&2
		exit 1 ;;
esac

DOX="dox -verbose $flags"

: "atom" is the number of a known atom
: "atom_name" is the name of a known atom
: "depth" is the depth of the root window
: "index" and "keycode" are a known index/keycode pair
: "keysym" is a known keysym
: "name" is a known font
: "shape" is a legitimate loadfontcursor index

atom=`xlsatoms | sed -n '1s/	.*//p'`
atom_name=`xlsatoms | sed -n '1s/[^	]*	//p'`
depth=`xwininfo -root | sed -n '/Depth/s/.*[ 	]//p'`
index=0
keycode=`xmodmap -pk | sed -n '$s/ *//;$s/ .*//p'`
keysym=`xmodmap -pk | sed -n '$s/.*(//;$s/).*//p'`
name=`xlsfonts | sed -n '$p'`
shape=0

: Creation functions for which we do freeing at the end

bitmap=`$DOX createpixmap root 1 1 1`
cursor=`$DOX createfontcursor $shape`
font=`$DOX loadfont fixed`
pixmap=`$DOX createpixmap root 1 1 $depth`
window=`$DOX createsimplewindow root 0 0 1 1 1 black white`

: Plus one for the benefit of killclient...

resource=`$DOX createfontcursor $shape`

: And we get to prime the buffers for rotatebuffers

for i in 0 1 2 3 4 5 6 7
do
	$DOX storebuffer $i 1 $i
done

: Other parameters used below
: The 8 used in several places below is the length of "whatever"

accel_denominator=1
accel_numerator=1
allow_exposures=allow
angle1=0
angle2=0
arc_mode=chord
background=white
background_pixel=white
background_pixmap=$pixmap
bg=white
border=black
border_pixel=black
border_pixmap=$pixmap
border_width=1
buffer=0
button=1
bytes=whatever
cap_style=butt
change_mode=delete
clip_x_origin=0
clip_y_origin=0
close_mode=destroyall
color=white
colormap=default
confine_to=$window
coord_mode=origin
dash_list=whatever
dash_offset=0
data=whatever
dest=$window
dest_w=$window
dest_x=0
dest_y=0
direction=raiselowest
discard=true
do_accel=true
do_threshold=true
drawable=$window
event_mask=noevent
event_mode=syncboth
exposures=true
fd=0
fg=black
filename=whatever
fill_rule=winding
fill_shape=complex
fill_style=solid
flags=red
focus=$window
foreground=black
format=8
function=or
gc=default
grab_window=$window
graphics_exposures=true
height=1
icon_name=whatever
interval=0
join_style=miter
keyboard_mode=sync
length=8
line_style=solid
line_width=1
modifiers=any
n=8
nbytes=8
nelements=8
only_if_exists=true
option=whatever
owner=$window
owner_events=true
parent=root
percent=0
pixel=0
plane=1
plane_mask=1
pointer_mode=sync
prefer_blanking=prefer
program=whatever
prop_window=$window
property=$atom_name
requestor=$window
revert_to=none
rotate=1
screen_number=default
selection=$atom_name
src=$window
src_height=1
src_w=$window
src_width=1
src_x=0
src_y=0
stipple=$bitmap
string=whatever
subwindow_mode=clipbychildren
target=$atom_name
threshold=0
tile=$pixmap
time=current
timeout=0
ts_x_origin=0
ts_y_origin=0
type=$atom_name
valuemask=function
width=1
window_name=whatever
x1=0
x2=0
x=0
x_hot=0
y1=0
y2=0
y=0
y_hot=0

: And away we go

set -x

$DOX activatescreensaver
$DOX addtosaveset $window
$DOX allowevents $event_mode $time
$DOX autorepeatoff
$DOX autorepeaton
$DOX bell $percent
$DOX bitmapbitorder
$DOX bitmappad
$DOX bitmapunit
$DOX blackpixel $screen_number
$DOX changeactivepointergrab $event_mask $cursor $time
$DOX changepointercontrol $do_accel $do_threshold $accel_numerator $accel_denominator $threshold
# "mode" changed to "propmodeappend" from the output of $DOX
$DOX changeproperty $window $property $type $format propmodeappend $data $nelements
$DOX changesaveset $window $change_mode
$DOX circulatesubwindows $window $direction
$DOX circulatesubwindowsdown $window
$DOX circulatesubwindowsup $window
$DOX cleararea $window $x $y $width $height $exposures
$DOX clearwindow $window
$DOX closedisplay
$DOX connectionnumber
$DOX convertselection $selection $target $property $requestor $time
$DOX copyarea $src $dest $gc $src_x $src_y $width $height $dest_x $dest_y
$DOX copycolormapandfree $colormap
# "src" and "dest" were changed to "gc" from the output of $DOX...
$DOX copygc $gc $valuemask $gc
$DOX copyplane $src $dest $gc $src_x $src_y $width $height $dest_x $dest_y $plane
$DOX createbitmapfromdata $drawable $data $width $height
$DOX createfontcursor $shape
$DOX createpixmap $drawable $width $height $depth
$DOX createpixmapfrombitmapdata $drawable $data $width $height $fg $bg $depth
$DOX createsimplewindow $parent $x $y $width $height $border_width $border $background
$DOX defaultcolormap $screen_number
$DOX defaultdepth $screen_number
$DOX defaultgc $screen_number
$DOX defaultrootwindow
$DOX defaultscreen
$DOX definecursor $window $cursor
$DOX deleteproperty $window $property
$DOX destroysubwindows $window
: Expect error below if display is remote
$DOX disableaccesscontrol
$DOX displaycells $screen_number
$DOX displayheight $screen_number
$DOX displayheightmm $screen_number
$DOX displaymotionbuffersize
$DOX displayplanes $screen_number
$DOX displaystring
$DOX displaywidth $screen_number
$DOX displaywidthmm $screen_number
$DOX drawarc $drawable $gc $x $y $width $height $angle1 $angle2
$DOX drawarcs $drawable $gc $x $y $width $height $angle1 $angle2
$DOX drawimagestring $drawable $gc $x $y $string $length
$DOX drawline $drawable $gc $x1 $x2 $y1 $y2
# "mode" changed to "coord_mode" from the output of $DOX
$DOX drawlines $drawable $gc $x $y $coord_mode
$DOX drawpoint $drawable $gc $x $y
# "mode" changed to "coord_mode" from the output of $DOX
$DOX drawpoints $drawable $gc $x $y $coord_mode
$DOX drawrectangle $drawable $gc $x $y $width $height
$DOX drawrectangles $drawable $gc $x $y $width $height
$DOX drawsegments $drawable $gc $x1 $y1 $x2 $y2
$DOX drawstring $drawable $gc $x $y $string $length
: Expect error below if display is remote
$DOX enableaccesscontrol
# "mode" changed to queuedalready from the output of $DOX
$DOX eventsqueued queuedalready
$DOX extendedmaxrequestsize
$DOX fillarc $drawable $gc $x $y $width $height $angle1 $angle2
$DOX fillarcs $drawable $gc $x $y $width $height $angle1 $angle2
# "mode" changed to "coord_mode" from the output of $DOX
# "shape" changed to "fill_shape" from the output of $DOX
$DOX fillpolygon $drawable $gc $x $y $fill_shape $coord_mode
$DOX fillrectangle $drawable $gc $x $y $width $height
$DOX fillrectangles $drawable $gc $x $y $width $height
$DOX flush
$DOX flushgc $gc
# "mode" changed to "screensaveractive" from the output of $DOX
$DOX forcescreensaver screensaveractive
$DOX freecolormap $colormap
$DOX freegc $gc
$DOX getatomname $atom
$DOX getdefault $program $option
$DOX getselectionowner $selection
$DOX grabbutton $button $modifiers $grab_window $owner_events $event_mask $pointer_mode $keyboard_mode $confine_to $cursor
$DOX grabkey $keycode $modifiers $grab_window $owner_events $pointer_mode $keyboard_mode
$DOX grabkeyboard $grab_window $owner_events $pointer_mode $keyboard_mode $time
$DOX grabpointer $grab_window $owner_events $event_mask $pointer_mode $keyboard_mode $confine_to $cursor $time
$DOX grabserver
$DOX iconifywindow $window $screen_number
$DOX imagebyteorder
$DOX installcolormap $colormap
$DOX internatom $atom_name $only_if_exists
$DOX keycodetokeysym $keycode $index
$DOX keysymtokeycode $keysym
$DOX killclient $resource
$DOX lastknownrequestprocessed
$DOX loadfont $name
$DOX lockdisplay
$DOX lowerwindow $window
$DOX mapraised $window
$DOX mapsubwindows $window
$DOX mapwindow $window
# maskevent is tricky...
echo "
	selectinput $window propertychange
	changeproperty $window STRING STRING 8 replace whatever 8
	changeproperty $window STRING STRING 8 replace whenever 8
	maskevent propertychange
" | $DOX
$DOX maxrequestsize
$DOX moveresizewindow $window $x $y $width $height
$DOX movewindow $window $x $y
# nextevent is tricky...
echo "
	selectinput $window propertychange
	changeproperty $window STRING STRING 8 replace whatever 8
	changeproperty $window STRING STRING 8 replace whenever 8
	nextevent
" | $DOX
$DOX nextrequest
$DOX noop
# peekevent is tricky...
echo "
	selectinput $window propertychange
	changeproperty $window STRING STRING 8 replace whatever 8
	changeproperty $window STRING STRING 8 replace whenever 8
	peekevent
" | $DOX
$DOX pending
$DOX processinternalconnection fd
$DOX protocolrevision
$DOX protocolversion
$DOX qlength
$DOX raisewindow $window
$DOX removefromsaveset $window
$DOX reparentwindow $window $parent $x $y
$DOX resetscreensaver
$DOX resizewindow $window $width $height
$DOX resourcemanagerstring
$DOX rootwindow $screen_number
$DOX rotatebuffers $rotate
$DOX screencount
$DOX selectinput $window $event_mask
$DOX servervendor
# "mode" changed to "enableaccess" from the output of $DOX
: Expect error below if display is remote
$DOX setaccesscontrol enableaccess
$DOX setarcmode $gc $arc_mode
$DOX setbackground $gc $background
# "pixmap" changed to "bitmap" from the output of $DOX
$DOX setclipmask $gc $bitmap
$DOX setcliporigin $gc $clip_x_origin $clip_y_origin
$DOX setclosedownmode $close_mode
$DOX setdashes $gc $dash_offset $dash_list $n
$DOX setfillrule $gc $fill_rule
$DOX setfillstyle $gc $fill_style
$DOX setfont $gc $font
$DOX setforeground $gc $foreground
$DOX setfunction $gc $function
$DOX setgraphicsexposures $gc $graphics_exposures
$DOX seticonname $window $icon_name
$DOX setinputfocus $focus $revert_to $time
$DOX setlineattributes $gc $line_width $line_style $cap_style $join_style
$DOX setplanemask $gc $plane_mask
$DOX setscreensaver $timeout $interval $prefer_blanking $allow_exposures
$DOX setselectionowner $selection $owner $time
$DOX setstate $gc $foreground $background $function $plane_mask
$DOX setstipple $gc $stipple
$DOX setsubwindowmode $gc $subwindow_mode
$DOX settile $gc $tile
$DOX settransientforhint $window $prop_window
$DOX settsorigin $gc $ts_x_origin $ts_y_origin
$DOX setwindowbackground $window $background_pixel
$DOX setwindowbackgroundpixmap $window $background_pixmap
$DOX setwindowborder $window $border_pixel
$DOX setwindowborderpixmap $window $border_pixmap
$DOX setwindowborderwidth $window $width
$DOX setwindowcolormap $window $colormap
$DOX storebuffer $bytes $nbytes $buffer
$DOX storename $window $window_name
: Expect error below
$DOX storenamedcolor $colormap $color $pixel $flags
$DOX sync $discard
$DOX undefinecursor $window
$DOX ungrabbutton $button $modifiers $grab_window
$DOX ungrabkey $keycode $modifiers $grab_window
$DOX ungrabkeyboard $time
$DOX ungrabpointer $time
$DOX ungrabserver
$DOX uninstallcolormap $colormap
$DOX unlockdisplay
$DOX unmapsubwindows $window
$DOX unmapwindow $window
$DOX vendorrelease
$DOX warppointer $src_w $dest_w $src_x $src_y $src_width $src_height $dest_x $dest_y
$DOX whitepixel $screen_number
# windowevent is tricky...
echo "
	selectinput $window propertychange
	changeproperty $window STRING STRING 8 replace whatever 8
	changeproperty $window STRING STRING 8 replace whenever 8
	windowevent $window propertychange
" | $DOX
$DOX withdrawwindow $window $screen_number
$DOX writebitmapfile $filename $bitmap $width $height $x_hot $y_hot

# Resource freeing

$DOX destroywindow $window
$DOX freecursor $cursor
$DOX freepixmap $bitmap
$DOX freepixmap $pixmap
$DOX unloadfont $font
rm -f whatever
