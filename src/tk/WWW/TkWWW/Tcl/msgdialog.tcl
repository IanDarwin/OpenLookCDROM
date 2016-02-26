# THIS FILE REQUIRES tk3.3 or greater.

# Build a message dialog
# Calling arguments are
# parent - dialog to be centered on
#        - "" means center on screen
# w - name of dialog
# msg - message
# bitmap - bitmap to be displayed
# args - button list

proc DLG:msg {parent w msg bitmap args} {
    set done "DLG[set w]done"
    global $done
    set $done 0

    set oldFocus [focus]
    eval [list DLG:msg_nowait $parent $w $msg $bitmap] $args
    for {set i 1} {$i <= [llength $args]} {incr i 1} {
	DLG:bind_button $w $i "global $done; set $done $i"
    }

    tkwait variable $done
    focus $oldFocus
    DLG:destroy $w
    return "[set $done]"
}

proc DLG:msg_nowait {parent w msg bitmap args} {
    DLG:toplevel $parent $w 
    DLG:draw_icon_msg $w $bitmap $msg
    DLG:draw_buttons $w $args
    DLG:show $parent $w
    focus $w
    grab $w
}

# Create an entry dialog
# Calling arguments are
# parent - dialog to be centered on
# w - name of dialog
# message - a list of entry labels
# command_output - what to put in the command button
# command - command to execute if command button is pressed

proc DLG:entry {parent w title message command_output command} {
    DLG:toplevel $parent $w
    DLG:draw_entries $w $message
    DLG:draw_buttons $w [list "$command_output" "Dismiss" "Help"]

    DLG:bind_entry $w [llength $message] "DLG:invoke_button $w 1"
    DLG:bind_button $w 1 "DLG:hide $w
eval $command \[DLG:get_entry_values $w [llength $message]\]"
    DLG:bind_button $w 2 "DLG:hide $w"
    DLG:bind_button $w 3 "tkW3HelpNoHelp"
    return $w
}

proc DLG:listbox {parent w title list_items command_output command} {
    DLG:toplevel $parent $w
    DLG:draw_listbox $w $list_items
    DLG:draw_buttons $w [list "$command_output" "Dismiss" "Help"]

    DLG:bind_button $w 1 "DLG:hide $w
eval $command \[DLG:get_listbox_highlighted $w\]"
    DLG:bind_button $w 2 "DLG:hide $w"
    DLG:bind_button $w 3 "tkWWWHelpNoHelp"
    return $w
}

# The next procedures manage the dialog boxes

proc DLG:show {parent dialog} {
    # First update to make sure the boxes are the right size
    update idletask

    # Then we set the position and update
    DLG:position $parent $dialog
    update idletask

    # and now make it visible. Viola!  Centered over parent.
    wm deiconify $dialog
}

proc DLG:position {parent w} {
    # Tell the WM that we'll do this ourselves.
    wm sizefrom $w user
    wm positionfrom $w user

    # Where is my parent and what are it's dimensions
    if {$parent != ""} {
	set pargeo [split [wm geometry $parent] "+x"]
	set parwidth [lindex $pargeo 0]
	set parheight [lindex $pargeo 1]
	set parx [lindex $pargeo 2]
	set pary [lindex $pargeo 3]
    } {
	set parwidth [winfo screenwidth $w]
	set parheight [winfo screenheight $w]
	set parx 0
	set pary 0
	set parent [winfo parent $w]
    }

   # What are is the offset of the virtual window
    set vrootx [winfo vrootx $parent]
    set vrooty [winfo vrooty $parent]


    # What are my dimensions ?
    set dialogwidth [winfo reqwidth $w]
    set dialogheight  [winfo reqheight $w]

 
    set dialogx [expr $parx+($parwidth-$dialogwidth)/2+$vrootx]
    set dialogy [expr $pary+($parheight-$dialogheight)/2+$vrooty]

    set maxx [expr "[winfo screenwidth $parent] - $dialogwidth"]
    set maxy [expr "[winfo screenheight $parent] - $dialogheight"]

# Make sure it doesn't go off screen
    if {$dialogx < 0} {
	set dialogx 0
    } {
	if {$dialogx > $maxx} {
	    set dialogx $maxx
	}
    }
    if {$dialogy < 0} {
	set dialogy 0
    } {
	if {$dialogy > $maxy} {
	    set dialogy $maxy
	}
    }

    # Build my new position (and dimensions)
    wm geometry $w [ format "%dx%d+%d+%d" $dialogwidth $dialogheight \
			 $dialogx $dialogy]

}

proc DLG:hide {w} {
    wm withdraw $w
}

proc DLG:destroy {w} {
    destroy $w
}

# These are procedures for building the dialog boxes
# You can use them to create new types of boxes

# Create a toplevel window with dialog box characteristics

proc DLG:toplevel { parent w {title ""}} {
    catch { destroy $w }    
    toplevel $w -class Dialog

    # Lets the dialog window be handled like Motif dialogs by the WM
    if {$parent != ""} {
	wm group $w $parent
	wm transient $w $parent
    }

    wm minsize $w 0 0

    if {$title != ""} {
	wm title $w title
    }
    wm withdraw $w

    return $w
}

# Draw a frame with an icon and a message
proc DLG:draw_icon_msg {w bitmap text} {
    pack [frame $w.top -relief raised -bd 1] -side top -fill both

    message $w.msg -aspect 1500 -text $text 
    pack $w.msg -in $w.top -side right -expand 1 -fill both -padx 5m -pady 5m
    if {$bitmap != ""} {
	pack [label $w.bitmap -bitmap $bitmap] \
	    -in $w.top -side left -padx 5m -pady 5m
    }
}

# Draw some entries in your box
proc DLG:draw_entries {w llist} {
    set index 1
    pack append $w \
	[frame $w.entry_frame -borderwidth 2 -relief raised] {top fill expand}
    foreach label $llist {
	set name ef$index
	pack append $w.entry_frame \
	    [frame $w.$name -borderwidth 5 -relief flat] {top fill expand}
 
	# Add the entry widget
	pack append $w.$name \
	    [label $w.$name.label -text "$label" -anchor w ] {top fillx} \
	    [entry $w.$name.entry -relief sunken] { bottom expand fillx}
	incr index 1
    }
}

# Add bindings to the entries
proc DLG:bind_entry {w index binding} {
    bind $w.ef$index.entry <Return> $binding
}

# Get values from entries
proc DLG:get_entry_values {w number} {
    set return_string ""
    for {set i 1} {$i <= $number} {incr i 1} {
	lappend return_string [$w.ef$i.entry get]
    }
    return $return_string
}

proc DLG:get_entry_value {w i} {
    $w.ef$i.entry get
}

# Set entry values
proc DLG:set_entry_value {w index message} {
    $w.ef$index.entry delete 0 end
    $w.ef$index.entry insert 0 $message
}

# create a listbox

proc DLG:draw_listbox {w elements} {
    pack append $w \
	[frame $w.frame -borderwidth 5 -relief flat] {top fill expand}
    pack append $w.frame \
	[listbox $w.list -relief sunken \
	  -xscrollcommand "$w.hs set" \
          -yscrollcommand "$w.vs set"] {top fill expand}
    pack before $w.list \
	[scrollbar $w.vs \
	 -command "$w.list yview" -orient vertical ] {right filly}
    pack before $w.list \
	[scrollbar $w.hs \
	 -command "$w.list xview" -orient horizontal ] {bottom fillx}
    foreach elem $elements {
	$w.list insert end $elem
    }
}

proc DLG:get_listbox_highlighted {w} {
    $w.list curselection
}

# Draw the buttons at the bottom
proc DLG:draw_buttons  { w blist } {
    global DLG_focus
    pack [frame $w.cmds -bd 1] -side bottom -fill both

    set i 1
    foreach btn $blist {
	if {"$btn" != ""} {
	    button $w.cmds.f$i -text "$btn" -anchor center
	    frame $w.cmds.default$i -relief flat -bd 1
	    raise $w.cmds.f$i $w.cmds.default$i
	    pack $w.cmds.default$i -side left -expand 1 -padx 3m -pady 2m
	    pack $w.cmds.f$i -in $w.cmds.default$i -padx 2m -pady 2m \
		    -ipadx 2m -ipady 1m
	    bind $w.cmds.f$i <Enter> \
		"DLG:button_focus_in $w $i;tk_butEnter %W"
	}
	incr i
    }
    set DLG_focus($w) ""
    if {$blist != ""} {
	DLG:button_focus_in $w 1
    }
}

proc DLG:button_focus_in {w i} {
    global DLG_focus
    if {$DLG_focus($w) != ""} {
	$w.cmds.default$DLG_focus($w) config -relief flat
    }

    $w.cmds.default$i config -relief sunken
    bind $w <Return> "$w.cmds.f$i flash; $w.cmds.f$i invoke "
    set DLG_focus($w) $i
}

proc DLG:bind_button {w index command} {
    $w.cmds.f$index config -command $command
}

proc DLG:invoke_button {w index} {
    $w.cmds.f$index invoke
}

