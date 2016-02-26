## image.tcl Procedures for displaying images
## ==============
## Copyright (C) 1993-1994
## Globewide Network Academy
## Macvicar Institute for Educational Software Development
##
## See the file COPYRIGHT for conditions

proc tkW3ImageDisplayFile {file_name} {
    global tkW3ConfigViewer tkW3ConfigImageDirectView
    if {$tkW3ConfigImageDirectView} {
	tkW3OutputClearBody
	place .f.msg.image -in .f.msg -relx 0.0 \
	    -rely 0.0 -relwidth 0.0 -relheight 0.0
	update idletasks
	set output [open  "| $tkW3ConfigViewer(image) \
-windowid [winfo id .f.msg.image] -private $file_name" "r"]

	# Some weirdness is in the following lines
	# I have to trap for error in reading with both the gets and the close
	# I don't know why but I suspect it has something to do with 
	# buffering

        set width 100
        set height 100
	while {1} {
	    if [catch {gets $output string} value] {
		# If there is an error in the output
		tkW3OutputError \
		    "Error executing image browser $tkW3ConfigViewer(image)"
		return
	    }

	    # If we are done reading things
	    if {$value == -1} {
		break
	    }

	    regexp {([0-9]+)x([0-9]+)} $string {} width height
	    .f.msg insert insert "$string\n"
	    update idletasks
	} 

	if [catch {close $output}] {
	    # If there is an error in the output
	    tkW3OutputError \
		"Error executing image browser $tkW3ConfigViewer(image)"
	    return
	}

	place .f.msg.image -in .f.msg -relx 0.0 \
	    -rely 0.0 -width $width -height $height
	tkW3OutputClearBody
	tkW3OutputSetMessage {}
    } {
	tkW3NavigatePreface {Image file} {image} \
	    "exec $tkW3ConfigImageViewer $file_name &"
    }
}

proc tkW3ImageHideImageWindow {} {
    global tkW3ConfigImageDirectView
    if {$tkW3ConfigImageDirectView} {
        tkW3ImageReleaseColorCells
	place forget .f.msg.image
    }
}

# This procedure frees the color cells that have been allocated so that 
# they can be used by other applications

proc tkW3ImageReleaseColorCells {} {
    set window [winfo id .f.msg.image]
    if {"$window" != "0x0"} {
       global tkW3ConfigViewer
       if {[file executable $tkW3ConfigViewer(image)]} {
	   set output \
	       [open  "| $tkW3ConfigViewer(image) -windowid $window stdin > /dev/null" "w"]
	   puts $output "P1 1 1 0"
	   close $output
       }
   }
}

proc tkW3ImageEnableAnchor {anchor ismap} {
    if {$anchor == ""} {
	return
    }
    if {$ismap} {
	bind .f.msg.image <1> "tkW3NavigateRecordAndGoto $anchor?%x,%y
update"
        bind .f.msg.image <2> "tkW3NavigateClone $anchor?%x,%y
update"
	tkW3OutputEntryPrint .titles.title_entry \
	    "Anchor map link - Press on image to show link"
    } {
	bind .f.msg.image <1> "tkW3NavigateRecordAndGoto $anchor
update"
        bind .f.msg.image <2> "tkW3NavigateClone $anchor
update"
	tkW3OutputEntryPrint .titles.title_entry \
	    "Anchor link - Press on image to show link"
    }
}

proc tkW3ImageDisableAnchor {} {
    bind .f.msg.image <1> ""
}
