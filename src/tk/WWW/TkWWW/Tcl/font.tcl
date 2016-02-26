## font.tcl Procedures for changing fonts
## ==============
## Copyright (C) 1992-1993
## Globewide Network Academy
## Macvicar Institute for Educational Software Development
##
## See the file COPYRIGHT for conditions

## The procedures in this file are responsible for font display

## Conventions:
##   All procedures in this file begin with tkW3Font


proc tkW3FontSetTextFonts {w list} {
    foreach font $list {
	set style [lindex $font 0]
	if {$style == ""} {
	    $w configure -font [lindex $font 1]
	} else {
	    $w tag configure $style -font [lindex $font 1]
	}
    }
    $w tag configure U -underline true
}

proc tkW3FontInitialize {} {
    global tkW3ConfigFont tkW3ConfigFontList tkW3ConfigFontDefault

    DLG:toplevel . .font_dialog
    pack append .font_dialog \
	[frame .font_dialog.f] top 

    foreach item $tkW3ConfigFontList {
	pack append .font_dialog.f \
            [radiobutton .font_dialog.f.[lindex $item 0] \
             -text [lindex $item 1] -relief flat \
                 -variable font_name \
                 -anchor w ] {top fillx}
    }

    DLG:draw_buttons .font_dialog  {"OK" "Apply" "Cancel"}

    DLG:bind_button .font_dialog 1 \
	"tkW3FontSetTextFonts .f.msg \$tkW3ConfigFont(\$font_name)
DLG:hide .font_dialog"
    DLG:bind_button .font_dialog 2 \
	"tkW3FontSetTextFonts .f.msg \$tkW3ConfigFont(\$font_name)"
    DLG:bind_button .font_dialog 3 "DLG:hide .font_dialog"

    tkW3FontSetTextFonts .f.msg $tkW3ConfigFont($tkW3ConfigFontDefault)
}
	
	
    
