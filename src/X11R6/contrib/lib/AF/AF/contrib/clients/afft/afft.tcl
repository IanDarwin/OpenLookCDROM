# afft.tcl - Tcl script for realtime fft program

source [info library]/init.tcl
source $tk_library/tk.tcl

# Global configuration parameters

set mainWidth	626 	;# Width of main window
set mainHeight	451 	;# Height of main window
set rbw 	9    	;# Width of radio buttons, in characters
set mbw		11	;# Width of menu buttons, in characters

set bg          bisque1 ;# Background color
set act         bisque4 ;# Active color

# Generate widget label procedure

set mkSeq 0; proc ml {} {global mkSeq; incr mkSeq; return $mkSeq}

# Window hierarchy

set menubar [frame .menu -borderwidth 5 -relief sunken -background $bg]
set exit [button $menubar.exit -width $mbw -text "Exit" \
    -activebackground $act -command {destroy .}]

set colormenu [menubutton .menu.cmb -width $mbw -text "Colors" \
    -activebackground $act -menu .menu.cmb.cm -borderwidth 2 -relief raised]
set colors [menu .menu.cmb.cm -activebackground $act]

set windowmenu [menubutton .menu.wmb -width $mbw -text "Windows" \
    -activebackground $act -menu .menu.wmb.wm -borderwidth 2 -relief raised]
set windows [menu .menu.wmb.wm -activebackground $act]

set lengthmenu [menubutton .menu.lmb -width $mbw -text "FFT Length" \
    -activebackground $act -menu .menu.lmb.lm -borderwidth 2 -relief raised]
set fftlen [menu .menu.lmb.lm -activebackground $act]

set stridemenu [menubutton .menu.smb -width $mbw -text "Stride" \
    -activebackground $act -menu .menu.smb.sm -borderwidth 2 -relief raised]
set stridelen [menu .menu.smb.sm -activebackground $act]

set querymenu [button $menubar.[ml] -width $mbw -text "Query" \
    -activebackground $act -command {query $fft} ]

set fftframe [frame .[ml] -relief sunken -width 1000 -height 1000 \
    -borderwidth 5]
set fft [frame $fftframe.[ml] -width 1000 -height 1000]
set oscframe [frame .[ml] -relief sunken -width 1000 -height 42\
    -borderwidth 5 -background white]
set osc [frame $oscframe.[ml] -width 1000 -height 32 -background white]

set sbox [frame .[ml] -borderwidth 5 -relief sunken -background $bg]

proc sc {parent label} {
    global bg
    return [scale $parent.[ml] -orient horizontal -length 150\
	    -label $label -background $bg]
}

set min  [sc $sbox "Minimum"]; $min config -command "minmax $min min"
set max  [sc $sbox "Maximum"]; $max config -command "minmax $max max"
set gain [sc $sbox "Gain"];
$gain config -from -100 -to -5 -command {setfft gain}

proc filler {parent} {
    global bg
    return [button $parent.[ml] -height 2 -borderwidth 0 \
	    -activebackground $bg -background $bg]
}

set b0	[frame	.[ml] -borderwidth 5 -relief sunken]
set b1	[frame	$b0.[ml] -background $bg]
set b2	[frame	$b0.[ml] -background $bg]

set fill0 [filler $b1]
set fill1 [filler $b1]
set fill2 [filler $b2]
set fill3 [filler $b2]

proc rb {parent label var value comm} {
    global rbw; global act
    return [radiobutton $parent.[ml] -width $rbw -text $label \
	    -activebackground $act -anchor w -variable $var \
	    -value $value -command $comm]
}

set waterfall [rb $b1 "Waterfall" mode 0    {setfft mode $mode}]
set spec      [rb $b2 "Spec"      mode 1    {setfft mode $mode}]
set live      [rb $b1 "Live"      source 0  {setfft source $source}]
set demo      [rb $b2 "Demo"      source 1  {setfft source $source}]
set nodc      [rb $b1 "NoDC"      dc 0      {setfft dc $dc}]
set withdc    [rb $b2 "DC"        dc 1      {setfft dc $dc}]
set log       [rb $b1 "Log"       logsw 1   {setlog $logsw}]
set linear    [rb $b2 "Linear"    logsw 0   {setlog $logsw}]
# set full      [rb $b1 "Full"      dispsw 1  {setfft height $dispsw}]
# set half      [rb $b2 "Half"      dispsw 0  {setfft height $dispsw}]
set scope     [rb $b1 "Scope On"     scopesw 1 {setfft scope $scopesw}]
set noscope   [rb $b2 "Scope Off"   scopesw 0 {setfft scope $scopesw}]

# Window geometry

pack append . \
    $menubar {top fillx} \
    $sbox {bottom fillx} \
    $b0 {right filly} \
    $oscframe {bottom} \
    $fftframe {top}
    
pack append $fftframe \
    $fft {top fill}

pack append $oscframe \
    $osc {top fill}

pack append $menubar \
    $exit {left padx 10 pady 10} \
    $colormenu {left padx 10 pady 10} \
    $windowmenu {left padx 10 pady 10} \
    $lengthmenu {left padx 10 pady 10} \
    $stridemenu {left padx 10 pady 10}
#    $querymenu {left padx 10 pady 10}

pack append $sbox \
    $min {left pady 30 padx 50} \
    $max {left pady 30 padx 50}
    
pack append $b0 \
    $b1 {left filly} \
    $b2 {left filly}

if {[setfft file]  == 0} {
    pack append $b1 \
    	$fill1 {top} \
    	$live {top padx 10 pady 10}
    pack append $b2 \
    	$fill3 {top} \
    	$demo {top padx 10 pady 10}
}

pack append $b1 \
    $fill0 {top} \
    $waterfall {top padx 10 pady 10} \
    $nodc {top padx 10 pady 10} \
    $log {top padx 10 pady 10} \
    $scope {bottom padx 10 pady 10}
#    $full {top padx 10 pady 10} \

pack append $b2 \
    $fill2 {top} \
    $spec {top padx 10 pady 10} \
    $withdc {top padx 10 pady 10} \
    $linear {top padx 10 pady 10} \
    $noscope {bottom padx 10 pady 10}
#    $half {top padx 10 pady 10} \

# Pulldown Menus

$colors add radiobutton -label "Grays" \
    -variable color -value 0 -command {setfft color $color}
$colors add radiobutton -label "Spectral" \
    -variable color -value 1 -command {setfft color $color}
$colors	add radiobutton -label "HSL Colors" \
    -variable color -value 2 -command {setfft color $color}

$windows add radiobutton -label "None" \
    -variable window -value 0 -command {setfft window $window}
$windows add radiobutton -label "Hamming" \
    -variable window -value 1 -command {setfft window $window}
$windows add radiobutton -label "Hanning" \
    -variable window -value 2 -command {setfft window $window}
$windows add radiobutton -label "Triangular" \
    -variable window -value 3 -command {setfft window $window}

$fftlen add radiobutton -label "512" \
    -variable flen -value 512 -command {setfft fftlength 512}
$fftlen add radiobutton -label "256" \
    -variable flen -value 256 -command {setfft fftlength 256}
$fftlen add radiobutton -label "128" \
    -variable flen -value 128 -command {setfft fftlength 128}
$fftlen add radiobutton -label "64" \
    -variable flen -value 64  -command {setfft fftlength 64}

$stridelen add radiobutton -label "512" \
    -variable slen -value 512 -command {setfft stride 512}
$stridelen add radiobutton -label "256" \
    -variable slen -value 256 -command {setfft stride 256}
$stridelen add radiobutton -label "128" \
    -variable slen -value 128 -command {setfft stride 128}
$stridelen add radiobutton -label "64" \
    -variable slen -value 64  -command {setfft stride 64}

# Deal with the window manager

wm title    . "New Real Time Fast Fourier Transform"
wm geometry . "$mainWidth\x$mainHeight+350+50"

# procedure definitions

proc query {window} {
    puts stdout "Width = [winfo width $window]"
    puts stdout "Height = [winfo height $window]"
}

proc minmax {scale which value} {
    setfft $which $value
    set newvalue [setfft $which]
    if {$newvalue != $value} {
    	$scale set $newvalue
    }
}

proc setlog {switch} {
    global max
    global min
    global sbox
    global gain
    set old [setfft log]
    if {$old != $switch} {
    	set oldmin [setfft min]
	set oldmax [setfft max]
    	setfft log $switch
    	if {$switch} {		;# if non-zero, switching to log mode
    	    $min configure -label "Minimum (DB)" -from -100 -to 0
    	    $min set [expr "$oldmin - 100"]

    	    $max configure -label "Maximum (DB)" -from -100 -to 0
	    $max set [expr "$oldmax - 100"]
	    pack append $sbox $gain {left pady 30 padx 50}
    	} else {		;# if zero, switching to linear mode
    	    $max configure -label "Maximum (% peak)" -from 0 -to 100
	    $max set [expr "$oldmax + 100"]

    	    $min configure -label "Minimum (% peak)" -from 0 -to 100
    	    $min set [expr "$oldmin + 100"]
	    pack unpack $gain	;# remove gain scale in linear mode
    	}
    }
}

# set up default active buttons

set color   [setfft color]
set window  [setfft window]
set flen    [setfft fftlength]
set slen    [setfft stride]
set source  [setfft source]
set mode    [setfft mode]
set dc      [setfft dc]
set logsw   [setfft log]
set dispsw  [setfft height]
set scopesw [setfft scope]

# set up scales for log or linear mode

if {[setfft log]} {
    $min configure -label "Minimum (DB)" -from -100 -to 0
    $max configure -label "Maximum (DB)" -from -100 -to 0
    setfft min -100
    setfft max 0
    pack append $sbox $gain {left pady 30 padx 50}
} else {
    $min configure -label "Minimum (% peak)" -from 0 -to 100
    $max configure -label "Maximum (% peak)" -from 0 -to 100
    setfft max 100
    setfft min 0
}

# set initial values for scales

$max  set [setfft initmax]
$min  set [setfft initmin]
$gain set [setfft gain]

# start the fft running once its window is mapped

bind $fft <Map> {runfft [winfo id %W]}
bind $osc <Map> {runosc [winfo id %W]}

# *** End of afft.tcl ***


