## binary.sed initialization file for binary tkWWW  user interface
## ==============
## Copyright (C) 1992-1993
## Globewide Network Academy
## Macvicar Institute for Educational Software Development
##
## See the file COPYRIGHT for conditions 

# This file is only executed when tkWWW is running as a single executable
# It sets up some tkWWW variables and then executes the default tk/tcl
# startup scripts

if ![info exists env(TK_WWW_HOME_PAGE)] {
    set env(TK_WWW_HOME_PAGE) tk_www_home_page
}

if ![info exists env(TK_WWW_START_PAGE)] {
    set env(TK_WWW_START_PAGE) tk_www_start_page
}

if ![info exists env(TK_WWW_MAIL)] {
    set env(TK_WWW_MAIL) tk_www_mail_page
}
set tkW3BinaryInBinary 1
set tkW3Version "0.11"

# tk.tcl --
#
# Initialization script normally executed in the interpreter for each
# Tk-based application.  Arranges class bindings for widgets.
#
# /mit/tkwww/CVS/WWW/TkWWW/Tcl/binary.sed,v 1.1 1994/04/01 08:36:00 joe Exp SPRITE (Berkeley)
#
# Copyright (c) 1992-1993 The Regents of the University of California.
# All rights reserved.
#
# Permission is hereby granted, without written agreement and without
# license or royalty fees, to use, copy, modify, and distribute this
# software and its documentation for any purpose, provided that the
# above copyright notice and the following two paragraphs appear in
# all copies of this software.
#
# IN NO EVENT SHALL THE UNIVERSITY OF CALIFORNIA BE LIABLE TO ANY PARTY FOR
# DIRECT, INDIRECT, SPECIAL, INCIDENTAL, OR CONSEQUENTIAL DAMAGES ARISING OUT
# OF THE USE OF THIS SOFTWARE AND ITS DOCUMENTATION, EVEN IF THE UNIVERSITY OF
# CALIFORNIA HAS BEEN ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
#
# THE UNIVERSITY OF CALIFORNIA SPECIFICALLY DISCLAIMS ANY WARRANTIES,
# INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY
# AND FITNESS FOR A PARTICULAR PURPOSE.  THE SOFTWARE PROVIDED HEREUNDER IS
# ON AN "AS IS" BASIS, AND THE UNIVERSITY OF CALIFORNIA HAS NO OBLIGATION TO
# PROVIDE MAINTENANCE, SUPPORT, UPDATES, ENHANCEMENTS, OR MODIFICATIONS.

# Insist on running with compatible versions of Tcl and Tk.


# Add Tk's directory to the end of the auto-load search path:

lappend auto_path $tk_library

# Turn off strict Motif look and feel as a default.

set tk_strictMotif 0

# normally auto-loaded files
# button.tcl --
#
# This file contains Tcl procedures used to manage Tk buttons.
#
# /mit/tkwww/CVS/WWW/TkWWW/Tcl/binary.sed,v 1.1 1994/04/01 08:36:00 joe Exp SPRITE (Berkeley)
#
# Copyright (c) 1992-1993 The Regents of the University of California.
# All rights reserved.
#
# Permission is hereby granted, without written agreement and without
# license or royalty fees, to use, copy, modify, and distribute this
# software and its documentation for any purpose, provided that the
# above copyright notice and the following two paragraphs appear in
# all copies of this software.
#
# IN NO EVENT SHALL THE UNIVERSITY OF CALIFORNIA BE LIABLE TO ANY PARTY FOR
# DIRECT, INDIRECT, SPECIAL, INCIDENTAL, OR CONSEQUENTIAL DAMAGES ARISING OUT
# OF THE USE OF THIS SOFTWARE AND ITS DOCUMENTATION, EVEN IF THE UNIVERSITY OF
# CALIFORNIA HAS BEEN ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
#
# THE UNIVERSITY OF CALIFORNIA SPECIFICALLY DISCLAIMS ANY WARRANTIES,
# INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY
# AND FITNESS FOR A PARTICULAR PURPOSE.  THE SOFTWARE PROVIDED HEREUNDER IS
# ON AN "AS IS" BASIS, AND THE UNIVERSITY OF CALIFORNIA HAS NO OBLIGATION TO
# PROVIDE MAINTENANCE, SUPPORT, UPDATES, ENHANCEMENTS, OR MODIFICATIONS.
#

# The procedure below is invoked when the mouse pointer enters a
# button widget.  It records the button we're in and changes the
# state of the button to active unless the button is disabled.

proc tk_butEnter w {
    global tk_priv tk_strictMotif
    if {[lindex [$w config -state] 4] != "disabled"} {
	if {!$tk_strictMotif} {
	    $w config -state active
	}
	set tk_priv(window) $w
    }
}

# The procedure below is invoked when the mouse pointer leaves a
# button widget.  It changes the state of the button back to
# inactive.

proc tk_butLeave w {
    global tk_priv tk_strictMotif
    if {[lindex [$w config -state] 4] != "disabled"} {
	if {!$tk_strictMotif} {
	    $w config -state normal
	}
    }
    set tk_priv(window) ""
}

# The procedure below is invoked when the mouse button is pressed in
# a button/radiobutton/checkbutton widget.  It records information
# (a) to indicate that the mouse is in the button, and
# (b) to save the button's relief so it can be restored later.

proc tk_butDown w {
    global tk_priv
    set tk_priv(relief) [lindex [$w config -relief] 4]
    set tk_priv(buttonWindow) $w
    if {[lindex [$w config -state] 4] != "disabled"} {
	$w config -relief sunken
    }
}

# The procedure below is invoked when the mouse button is released
# for a button/radiobutton/checkbutton widget.  It restores the
# button's relief and invokes the command as long as the mouse
# hasn't left the button.

proc tk_butUp w {
    global tk_priv
    if {$w == $tk_priv(buttonWindow)} {
	$w config -relief $tk_priv(relief)
	if {($w == $tk_priv(window))
		&& ([lindex [$w config -state] 4] != "disabled")} {
	    uplevel #0 [list $w invoke]
	}
	set tk_priv(buttonWindow) ""
    }
}
# dialog.tcl --
#
# This file defines the procedure tk_dialog, which creates a dialog
# box containing a bitmap, a message, and one or more buttons.
#
# /mit/tkwww/CVS/WWW/TkWWW/Tcl/binary.sed,v 1.1 1994/04/01 08:36:00 joe Exp SPRITE (Berkeley)
#
# Copyright (c) 1992-1993 The Regents of the University of California.
# All rights reserved.
#
# Permission is hereby granted, without written agreement and without
# license or royalty fees, to use, copy, modify, and distribute this
# software and its documentation for any purpose, provided that the
# above copyright notice and the following two paragraphs appear in
# all copies of this software.
#
# IN NO EVENT SHALL THE UNIVERSITY OF CALIFORNIA BE LIABLE TO ANY PARTY FOR
# DIRECT, INDIRECT, SPECIAL, INCIDENTAL, OR CONSEQUENTIAL DAMAGES ARISING OUT
# OF THE USE OF THIS SOFTWARE AND ITS DOCUMENTATION, EVEN IF THE UNIVERSITY OF
# CALIFORNIA HAS BEEN ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
#
# THE UNIVERSITY OF CALIFORNIA SPECIFICALLY DISCLAIMS ANY WARRANTIES,
# INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY
# AND FITNESS FOR A PARTICULAR PURPOSE.  THE SOFTWARE PROVIDED HEREUNDER IS
# ON AN "AS IS" BASIS, AND THE UNIVERSITY OF CALIFORNIA HAS NO OBLIGATION TO
# PROVIDE MAINTENANCE, SUPPORT, UPDATES, ENHANCEMENTS, OR MODIFICATIONS.
#

#
# tk_dialog:
#
# This procedure displays a dialog box, waits for a button in the dialog
# to be invoked, then returns the index of the selected button.
#
# Arguments:
# w -		Window to use for dialog top-level.
# title -	Title to display in dialog's decorative frame.
# text -	Message to display in dialog.
# bitmap -	Bitmap to display in dialog (empty string means none).
# default -	Index of button that is to display the default ring
#		(-1 means none).
# args -	One or more strings to display in buttons across the
#		bottom of the dialog box.

proc tk_dialog {w title text bitmap default args} {
    global tk_priv

    # 1. Create the top-level window and divide it into top
    # and bottom parts.

    catch {destroy $w}
    toplevel $w -class Dialog
    wm title $w $title
    wm iconname $w Dialog
    frame $w.top -relief raised -bd 1
    pack $w.top -side top -fill both
    frame $w.bot -relief raised -bd 1
    pack $w.bot -side bottom -fill both

    # 2. Fill the top part with bitmap and message.

    message $w.msg -width 3i -text $text \
	    -font -Adobe-Times-Medium-R-Normal-*-180-*
    pack $w.msg -in $w.top -side right -expand 1 -fill both -padx 5m -pady 5m
    if {$bitmap != ""} {
	label $w.bitmap -bitmap $bitmap
	pack $w.bitmap -in $w.top -side left -padx 5m -pady 5m
    }

    # 3. Create a row of buttons at the bottom of the dialog.

    set i 0
    foreach but $args {
	button $w.button$i -text $but -command "set tk_priv(button) $i"
	if {$i == $default} {
	    frame $w.default -relief sunken -bd 1
	    raise $w.button$i $w.default
	    pack $w.default -in $w.bot -side left -expand 1 -padx 3m -pady 2m
	    pack $w.button$i -in $w.default -padx 2m -pady 2m \
		    -ipadx 2m -ipady 1m
	    bind $w <Return> "$w.button$i flash; set tk_priv(button) $i"
	} else {
	    pack $w.button$i -in $w.bot -side left -expand 1 \
		    -padx 3m -pady 3m -ipadx 2m -ipady 1m
	}
	incr i
    }

    # 4. Withdraw the window, then update all the geometry information
    # so we know how big it wants to be, then center the window in the
    # display and de-iconify it.

    wm withdraw $w
    update idletasks
    set x [expr [winfo screenwidth $w]/2 - [winfo reqwidth $w]/2 \
	    - [winfo vrootx [winfo parent $w]]]
    set y [expr [winfo screenheight $w]/2 - [winfo reqheight $w]/2 \
	    - [winfo vrooty [winfo parent $w]]]
    wm geom $w +$x+$y
    wm deiconify $w

    # 5. Set a grab and claim the focus too.

    set oldFocus [focus]
    grab $w
    focus $w

    # 6. Wait for the user to respond, then restore the focus and
    # return the index of the selected button.

    tkwait variable tk_priv(button)
    destroy $w
    focus $oldFocus
    return $tk_priv(button)
}
# entry.tcl --
#
# This file contains Tcl procedures used to manage Tk entries.
#
# /mit/tkwww/CVS/WWW/TkWWW/Tcl/binary.sed,v 1.1 1994/04/01 08:36:00 joe Exp SPRITE (Berkeley)
#
# Copyright (c) 1992-1993 The Regents of the University of California.
# All rights reserved.
#
# Permission is hereby granted, without written agreement and without
# license or royalty fees, to use, copy, modify, and distribute this
# software and its documentation for any purpose, provided that the
# above copyright notice and the following two paragraphs appear in
# all copies of this software.
#
# IN NO EVENT SHALL THE UNIVERSITY OF CALIFORNIA BE LIABLE TO ANY PARTY FOR
# DIRECT, INDIRECT, SPECIAL, INCIDENTAL, OR CONSEQUENTIAL DAMAGES ARISING OUT
# OF THE USE OF THIS SOFTWARE AND ITS DOCUMENTATION, EVEN IF THE UNIVERSITY OF
# CALIFORNIA HAS BEEN ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
#
# THE UNIVERSITY OF CALIFORNIA SPECIFICALLY DISCLAIMS ANY WARRANTIES,
# INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY
# AND FITNESS FOR A PARTICULAR PURPOSE.  THE SOFTWARE PROVIDED HEREUNDER IS
# ON AN "AS IS" BASIS, AND THE UNIVERSITY OF CALIFORNIA HAS NO OBLIGATION TO
# PROVIDE MAINTENANCE, SUPPORT, UPDATES, ENHANCEMENTS, OR MODIFICATIONS.
#

# The procedure below is invoked to backspace over one character
# in an entry widget.  The name of the widget is passed as argument.

proc tk_entryBackspace w {
    set x [expr {[$w index insert] - 1}]
    if {$x != -1} {$w delete $x}
}

# The procedure below is invoked to backspace over one word in an
# entry widget.  The name of the widget is passed as argument.

proc tk_entryBackword w {
    set string [$w get]
    set curs [expr [$w index insert]-1]
    if {$curs < 0} return
    for {set x $curs} {$x > 0} {incr x -1} {
	if {([string first [string index $string $x] " \t"] < 0)
		&& ([string first [string index $string [expr $x-1]] " \t"]
		>= 0)} {
	    break
	}
    }
    $w delete $x $curs
}

# The procedure below is invoked after insertions.  If the caret is not
# visible in the window then the procedure adjusts the entry's view to
# bring the caret back into the window again.  Also, try to keep at
# least one character visible to the left of the caret.

proc tk_entrySeeCaret w {
    set c [$w index insert]
    set left [$w index @0]
    if {$left >= $c} {
	if {$c > 0} {
	    $w view [expr $c-1]
	} else {
	    $w view $c
	}
	return
    }
    while {([$w index @[expr [winfo width $w]-5]] < $c)
	    && ($left < $c)} {
	set left [expr $left+1]
	$w view $left
    }
}
# listbox.tcl --
#
# This file contains Tcl procedures used to manage Tk listboxes.
#
# /mit/tkwww/CVS/WWW/TkWWW/Tcl/binary.sed,v 1.1 1994/04/01 08:36:00 joe Exp SPRITE (Berkeley)
#
# Copyright (c) 1992-1993 The Regents of the University of California.
# All rights reserved.
#
# Permission is hereby granted, without written agreement and without
# license or royalty fees, to use, copy, modify, and distribute this
# software and its documentation for any purpose, provided that the
# above copyright notice and the following two paragraphs appear in
# all copies of this software.
#
# IN NO EVENT SHALL THE UNIVERSITY OF CALIFORNIA BE LIABLE TO ANY PARTY FOR
# DIRECT, INDIRECT, SPECIAL, INCIDENTAL, OR CONSEQUENTIAL DAMAGES ARISING OUT
# OF THE USE OF THIS SOFTWARE AND ITS DOCUMENTATION, EVEN IF THE UNIVERSITY OF
# CALIFORNIA HAS BEEN ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
#
# THE UNIVERSITY OF CALIFORNIA SPECIFICALLY DISCLAIMS ANY WARRANTIES,
# INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY
# AND FITNESS FOR A PARTICULAR PURPOSE.  THE SOFTWARE PROVIDED HEREUNDER IS
# ON AN "AS IS" BASIS, AND THE UNIVERSITY OF CALIFORNIA HAS NO OBLIGATION TO
# PROVIDE MAINTENANCE, SUPPORT, UPDATES, ENHANCEMENTS, OR MODIFICATIONS.
#

# The procedure below may be invoked to change the behavior of
# listboxes so that only a single item may be selected at once.
# The arguments give one or more windows whose behavior should
# be changed;  if one of the arguments is "Listbox" then the default
# behavior is changed for all listboxes.

proc tk_listboxSingleSelect args {
    foreach w $args {
	bind $w <B1-Motion> {%W select from [%W nearest %y]} 
	bind $w <Shift-1> {%W select from [%W nearest %y]}
	bind $w <Shift-B1-Motion> {%W select from [%W nearest %y]}
    }
}
# menu.tcl --
#
# This file contains Tcl procedures used to manage Tk menus and
# menubuttons.  Most of the code here is dedicated to support for
# pulling down menus and menu traversal via the keyboard.
#
# /mit/tkwww/CVS/WWW/TkWWW/Tcl/binary.sed,v 1.1 1994/04/01 08:36:00 joe Exp SPRITE (Berkeley)
#
# Copyright (c) 1992-1993 The Regents of the University of California.
# All rights reserved.
#
# Permission is hereby granted, without written agreement and without
# license or royalty fees, to use, copy, modify, and distribute this
# software and its documentation for any purpose, provided that the
# above copyright notice and the following two paragraphs appear in
# all copies of this software.
#
# IN NO EVENT SHALL THE UNIVERSITY OF CALIFORNIA BE LIABLE TO ANY PARTY FOR
# DIRECT, INDIRECT, SPECIAL, INCIDENTAL, OR CONSEQUENTIAL DAMAGES ARISING OUT
# OF THE USE OF THIS SOFTWARE AND ITS DOCUMENTATION, EVEN IF THE UNIVERSITY OF
# CALIFORNIA HAS BEEN ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
#
# THE UNIVERSITY OF CALIFORNIA SPECIFICALLY DISCLAIMS ANY WARRANTIES,
# INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY
# AND FITNESS FOR A PARTICULAR PURPOSE.  THE SOFTWARE PROVIDED HEREUNDER IS
# ON AN "AS IS" BASIS, AND THE UNIVERSITY OF CALIFORNIA HAS NO OBLIGATION TO
# PROVIDE MAINTENANCE, SUPPORT, UPDATES, ENHANCEMENTS, OR MODIFICATIONS.
#

# The procedure below is publically available.  It is used to identify
# a frame that serves as a menu bar and the menu buttons that lie inside
# the menu bar.  This procedure establishes proper "menu bar" behavior
# for all of the menu buttons, including keyboard menu traversal.  Only
# one menu bar may exist for a given top-level window at a time.
# Arguments:
#	
# bar -				The path name of the containing frame.  Must
#				be an ancestor of all of the menu buttons,
#				since it will be be used in grabs.
# additional arguments -	One or more menu buttons that are descendants
#				of bar.  The order of these arguments
#				determines the order of keyboard traversal.
#				If no extra arguments are named then all of
#				the menu bar information for bar is cancelled.

proc tk_menuBar {w args} {
    global tk_priv
    if {$args == ""} {
	if [catch {set menus $tk_priv(menusFor$w)}] {
	    return ""
	}
	return $menus
    }
    if [info exists tk_priv(menusFor$w)] {
	unset tk_priv(menusFor$w)
	unset tk_priv(menuBarFor[winfo toplevel $w])
    }
    if {$args == "{}"} {
	return
    }
    set tk_priv(menusFor$w) $args
    set tk_priv(menuBarFor[winfo toplevel $w]) $w
    bind $w <Any-Alt-KeyPress> {tk_traverseToMenu %W %A}
    bind $w <F10> {tk_firstMenu %W}
    bind $w <Any-ButtonRelease-1> tk_mbUnpost
}

proc tk_menus {w args} {
    error "tk_menus is obsolete in Tk versions 3.0 and later; please change your scripts to use tk_menuBar instead"
}

# The procedure below is publically available.  It takes any number of
# arguments that are names of widgets or classes.  It sets up bindings
# for the widgets or classes so that keyboard menu traversal is possible
# when the input focus is in those widgets or classes.

proc tk_bindForTraversal args {
    foreach w $args {
	bind $w <Any-Alt-KeyPress> {tk_traverseToMenu %W %A}
	bind $w <F10> {tk_firstMenu %W}
    }
}

# The procedure below does all of the work of posting a menu (including
# unposting any other menu that might currently be posted).  The "w"
# argument is the name of the menubutton for the menu to be posted.
# Note:  if $w is disabled then the procedure does nothing.

proc tk_mbPost {w} {
    global tk_priv tk_strictMotif
    if {[lindex [$w config -state] 4] == "disabled"} {
	return
    }
    if {$w == $tk_priv(posted)} {
	grab -global $tk_priv(grab)
	return
    }
    set menu [lindex [$w config -menu] 4]
    if {$menu == ""} {
	return
    }
    if ![string match $w* $menu] {
	error "can't post $menu:  it isn't a descendant of $w (this is a new requirement in Tk versions 3.0 and later)"
    }
    set cur $tk_priv(posted)
    if {$cur != ""} tk_mbUnpost
    set tk_priv(relief) [lindex [$w config -relief] 4]
    $w config -relief raised
    set tk_priv(posted) $w
    if {$tk_priv(focus) == ""} {
	set tk_priv(focus) [focus]
    }
    set tk_priv(activeBg) [lindex [$menu config -activebackground] 4]
    set tk_priv(activeFg) [lindex [$menu config -activeforeground] 4]
    if $tk_strictMotif {
	$menu config -activebackground [lindex [$menu config -background] 4]
	$menu config -activeforeground [lindex [$menu config -foreground] 4]
    }
    $menu activate none
    focus $menu
    $menu post [winfo rootx $w] [expr [winfo rooty $w]+[winfo height $w]]
    if [catch {set grab $tk_priv(menuBarFor[winfo toplevel $w])}] {
	set grab $w
    } else {
	if [lsearch $tk_priv(menusFor$grab) $w]<0 {
	    set grab $w
	}
    }
    set tk_priv(cursor) [lindex [$grab config -cursor] 4]
    $grab config -cursor arrow
    set tk_priv(grab) $grab
    grab -global $grab
}

# The procedure below does all the work of unposting the menubutton that's
# currently posted.  It takes no arguments.  Special notes:
# 1. It's important to unpost the menu before releasing the grab, so
#    that any Enter-Leave events (e.g. from menu back to main
#    application) have mode NotifyGrab.
# 2. Be sure to enclose various groups of commands in "catch" so that
#    the procedure will complete even if the menubutton or the menu
#    or the grab window has been deleted.

proc tk_mbUnpost {} {
    global tk_priv
    set w $tk_priv(posted)
    if {$w != ""} {
	catch {
	    set menu [lindex [$w config -menu] 4]
	    $menu unpost
	    $menu config -activebackground $tk_priv(activeBg)
	    $menu config -activeforeground $tk_priv(activeFg)
	    $w config -relief $tk_priv(relief)
	}
	catch {$tk_priv(grab) config -cursor $tk_priv(cursor)}
	catch {focus $tk_priv(focus)}
	grab release $tk_priv(grab)
	set tk_priv(grab) ""
	set tk_priv(focus) ""
	set tk_priv(posted) {}
    }
}

# The procedure below is invoked to implement keyboard traversal to
# a menu button.  It takes two arguments:  the name of a window where
# a keystroke originated, and the ascii character that was typed.
# This procedure finds a menu bar by looking upward for a top-level
# window, then looking for a window underneath that named "menu".
# Then it searches through all the subwindows of "menu" for a menubutton
# with an underlined character matching char.  If one is found, it
# posts that menu.

proc tk_traverseToMenu {w char} {
    global tk_priv
    if {$char == ""} {
	return
    }
    set char [string tolower $char]

    foreach mb [tk_getMenuButtons $w] {
	if {[winfo class $mb] == "Menubutton"} {
	    set char2 [string index [lindex [$mb config -text] 4] \
		    [lindex [$mb config -underline] 4]]
	    if {[string compare $char [string tolower $char2]] == 0} {
		tk_mbPost $mb
		[lindex [$mb config -menu] 4] activate 0
		return
	    }
	}
    }
}

# The procedure below is used to implement keyboard traversal within
# the posted menu.  It takes two arguments:  the name of the menu to
# be traversed within, and an ASCII character.  It searches for an
# entry in the menu that has that character underlined.  If such an
# entry is found, it is invoked and the menu is unposted.

proc tk_traverseWithinMenu {w char} {
    if {$char == ""} {
	return
    }
    set char [string tolower $char]
    set last [$w index last]
    if {$last == "none"} {
	return
    }
    for {set i 0} {$i <= $last} {incr i} {
	if [catch {set char2 [string index \
		[lindex [$w entryconfig $i -label] 4] \
		[lindex [$w entryconfig $i -underline] 4]]}] {
	    continue
	}
	if {[string compare $char [string tolower $char2]] == 0} {
	    tk_mbUnpost
	    $w invoke $i
	    return
	}
    }
}

# The procedure below takes a single argument, which is the name of
# a window.  It returns a list containing path names for all of the
# menu buttons associated with that window's top-level window, or an
# empty list if there are none.

proc tk_getMenuButtons w {
    global tk_priv
    set top [winfo toplevel $w]
    if [catch {set bar [set tk_priv(menuBarFor$top)]}] {
	return ""
    }
    return $tk_priv(menusFor$bar)
}

# The procedure below is used to traverse to the next or previous
# menu in a menu bar.  It takes one argument, which is a count of
# how many menu buttons forward or backward (if negative) to move.
# If there is no posted menu then this procedure has no effect.

proc tk_nextMenu count {
    global tk_priv
    if {$tk_priv(posted) == ""} {
	return
    }
    set buttons [tk_getMenuButtons $tk_priv(posted)]
    set length [llength $buttons]
    for {set i 0} 1 {incr i} {
	if {$i >= $length} {
	    return
	}
	if {[lindex $buttons $i] == $tk_priv(posted)} {
	    break
	}
    }
    incr i $count
    while 1 {
	while {$i < 0} {
	    incr i $length
	}
	while {$i >= $length} {
	    incr i -$length
	}
	set mb [lindex $buttons $i]
	if {[lindex [$mb configure -state] 4] != "disabled"} {
	    break
	}
	incr i $count
    }
    tk_mbUnpost
    tk_mbPost $mb
    [lindex [$mb config -menu] 4] activate 0
}

# The procedure below is used to traverse to the next or previous entry
# in the posted menu.  It takes one argument, which is 1 to go to the
# next entry or -1 to go to the previous entry.  Disabled entries are
# skipped in this process.

proc tk_nextMenuEntry count {
    global tk_priv
    if {$tk_priv(posted) == ""} {
	return
    }
    set menu [lindex [$tk_priv(posted) config -menu] 4]
    if {[$menu index last] == "none"} {
	return
    }
    set length [expr [$menu index last]+1]
    set i [$menu index active]
    if {$i == "none"} {
	set i 0
    } else {
	incr i $count
    }
    while 1 {
	while {$i < 0} {
	    incr i $length
	}
	while {$i >= $length} {
	    incr i -$length
	}
	if {[catch {$menu entryconfigure $i -state} state] == 0} {
	    if {[lindex $state 4] != "disabled"} {
		break
	    }
	}
	incr i $count
    }
    $menu activate $i
}

# The procedure below invokes the active entry in the posted menu,
# if there is one.  Otherwise it does nothing.

proc tk_invokeMenu {menu} {
    set i [$menu index active]
    if {$i != "none"} {
	tk_mbUnpost
	update idletasks
	$menu invoke $i
    }
}

# The procedure below is invoked to keyboard-traverse to the first
# menu for a given source window.  The source window is passed as
# parameter.

proc tk_firstMenu w {
    set mb [lindex [tk_getMenuButtons $w] 0]
    if {$mb != ""} {
	tk_mbPost $mb
	[lindex [$mb config -menu] 4] activate 0
    }
}

# The procedure below is invoked when a button-1-down event is
# received by a menu button.  If the mouse is in the menu button
# then it posts the button's menu.  If the mouse isn't in the
# button's menu, then it deactivates any active entry in the menu.
# Remember, event-sharing can cause this procedure to be invoked
# for two different menu buttons on the same event.

proc tk_mbButtonDown w {
    global tk_priv
    if {[lindex [$w config -state] 4] == "disabled"} {
	return
    }
    if {$tk_priv(inMenuButton) == $w} {
	tk_mbPost $w
    }
}
# text.tcl --
#
# This file contains Tcl procedures used to manage Tk entries.
#
# /mit/tkwww/CVS/WWW/TkWWW/Tcl/binary.sed,v 1.1 1994/04/01 08:36:00 joe Exp SPRITE (Berkeley)
#
# Copyright (c) 1992-1993 The Regents of the University of California.
# All rights reserved.
#
# Permission is hereby granted, without written agreement and without
# license or royalty fees, to use, copy, modify, and distribute this
# software and its documentation for any purpose, provided that the
# above copyright notice and the following two paragraphs appear in
# all copies of this software.
#
# IN NO EVENT SHALL THE UNIVERSITY OF CALIFORNIA BE LIABLE TO ANY PARTY FOR
# DIRECT, INDIRECT, SPECIAL, INCIDENTAL, OR CONSEQUENTIAL DAMAGES ARISING OUT
# OF THE USE OF THIS SOFTWARE AND ITS DOCUMENTATION, EVEN IF THE UNIVERSITY OF
# CALIFORNIA HAS BEEN ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
#
# THE UNIVERSITY OF CALIFORNIA SPECIFICALLY DISCLAIMS ANY WARRANTIES,
# INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY
# AND FITNESS FOR A PARTICULAR PURPOSE.  THE SOFTWARE PROVIDED HEREUNDER IS
# ON AN "AS IS" BASIS, AND THE UNIVERSITY OF CALIFORNIA HAS NO OBLIGATION TO
# PROVIDE MAINTENANCE, SUPPORT, UPDATES, ENHANCEMENTS, OR MODIFICATIONS.
#

# The procedure below is invoked when dragging one end of the selection.
# The arguments are the text window name and the index of the character
# that is to be the new end of the selection.

proc tk_textSelectTo {w index} {
    global tk_priv

    case $tk_priv(selectMode) {
	char {
	    if [$w compare $index < anchor] {
		set first $index
		set last anchor
	    } else {
		set first anchor
		set last [$w index $index+1c]
	    }
	}
	word {
	    if [$w compare $index < anchor] {
		set first [$w index "$index wordstart"]
		set last [$w index "anchor wordend"]
	    } else {
		set first [$w index "anchor wordstart"]
		set last [$w index "$index wordend"]
	    }
	}
	line {
	    if [$w compare $index < anchor] {
		set first [$w index "$index linestart"]
		set last [$w index "anchor lineend + 1c"]
	    } else {
		set first [$w index "anchor linestart"]
		set last [$w index "$index lineend + 1c"]
	    }
	}
    }
    $w tag remove sel 0.0 $first
    $w tag add sel $first $last
    $w tag remove sel $last end
}

# The procedure below is invoked to backspace over one character in
# a text widget.  The name of the widget is passed as argument.

proc tk_textBackspace w {
    $w delete insert-1c insert
}

# The procedure below compares three indices, a, b, and c.  Index b must
# be less than c.  The procedure returns 1 if a is closer to b than to c,
# and 0 otherwise.  The "w" argument is the name of the text widget in
# which to do the comparison.

proc tk_textIndexCloser {w a b c} {
    set a [$w index $a]
    set b [$w index $b]
    set c [$w index $c]
    if [$w compare $a <= $b] {
	return 1
    }
    if [$w compare $a >= $c] {
	return 0
    }
    scan $a "%d.%d" lineA chA
    scan $b "%d.%d" lineB chB
    scan $c "%d.%d" lineC chC
    if {$chC == 0} {
	incr lineC -1
	set chC [string length [$w get $lineC.0 $lineC.end]]
    }
    if {$lineB != $lineC} {
	return [expr {($lineA-$lineB) < ($lineC-$lineA)}]
    }
    return [expr {($chA-$chB) < ($chC-$chA)}]
}

# The procedure below is called to reset the selection anchor to
# whichever end is FARTHEST from the index argument.

proc tk_textResetAnchor {w index} {
    global tk_priv
    if {[$w tag ranges sel] == ""} {
	set tk_priv(selectMode) char
	$w mark set anchor $index
	return
    }
    if [tk_textIndexCloser $w $index sel.first sel.last] {
	if {$tk_priv(selectMode) == "char"} {
	    $w mark set anchor sel.last
	} else {
	    $w mark set anchor sel.last-1c
	}
    } else {
	$w mark set anchor sel.first
    }
}
# This file contains a default version of the tkError procedure.  It
# posts a dialog box with the error message and gives the user a chance
# to see a more detailed stack trace.

proc tkerror err {
    global errorInfo
    set info $errorInfo
    if {[tk_dialog .tkerrorDialog "Error in Tcl Script" \
	    "Error: $err" error 0 OK "See Stack Trace"] == 0} {
	return
    }

    set w .tkerrorTrace
    catch {destroy $w}
    toplevel $w -class ErrorTrace
    wm minsize $w 1 1
    wm title $w "Stack Trace for Error"
    wm iconname $w "Stack Trace"
    button $w.ok -text OK -command "destroy $w"
    text $w.text -relief raised -bd 2 -yscrollcommand "$w.scroll set" \
	    -setgrid true -width 40 -height 10
    scrollbar $w.scroll -relief flat -command "$w.text yview"
    pack $w.ok -side bottom -padx 3m -pady 3m -ipadx 2m -ipady 1m
    pack $w.scroll -side right -fill y
    pack $w.text -side left -expand yes -fill both
    $w.text insert 0.0 $info
    $w.text mark set insert 0.0

    # Center the window on the screen.

    wm withdraw $w
    update idletasks
    set x [expr [winfo screenwidth $w]/2 - [winfo reqwidth $w]/2 \
	    - [winfo vrootx [winfo parent $w]]]
    set y [expr [winfo screenheight $w]/2 - [winfo reqheight $w]/2 \
	    - [winfo vrooty [winfo parent $w]]]
    wm geom $w +$x+$y
    wm deiconify $w
}
# standard TK bindings
# ----------------------------------------------------------------------
# Class bindings for various flavors of button widgets.  $tk_priv(window)
# keeps track of the button containing the mouse $tk_priv(relief) saves
# the original relief of the button so it can be restored when the mouse
# button is released, and $tk_priv(buttonWindow) keeps track of the
# window in which the mouse button was pressed.
# ----------------------------------------------------------------------

bind Button <Any-Enter> {tk_butEnter %W}
bind Button <Any-Leave> {tk_butLeave %W}
bind Button <1> {tk_butDown %W}
bind Button <ButtonRelease-1> {tk_butUp %W}

bind Checkbutton <Any-Enter> {tk_butEnter %W}
bind Checkbutton <Any-Leave> {tk_butLeave %W}
bind Checkbutton <1> {tk_butDown %W}
bind Checkbutton <ButtonRelease-1> {tk_butUp %W}

bind Radiobutton <Any-Enter> {tk_butEnter %W}
bind Radiobutton <Any-Leave> {tk_butLeave %W}
bind Radiobutton <1> {tk_butDown %W}
bind Radiobutton <ButtonRelease-1> {tk_butUp %W}

# ----------------------------------------------------------------------
# Class bindings for entry widgets.
# ----------------------------------------------------------------------

bind Entry <1> {
    %W icursor @%x
    %W select from @%x
    if {[lindex [%W config -state] 4] == "normal"} {focus %W}
}
bind Entry <B1-Motion> {%W select to @%x}
bind Entry <Shift-1> {%W select adjust @%x}
bind Entry <Shift-B1-Motion> {%W select to @%x}
bind Entry <2> {%W scan mark %x}
bind Entry <B2-Motion> {%W scan dragto %x}
bind Entry <Any-KeyPress> {
    if {"%A" != ""} {
	%W insert insert %A
	tk_entrySeeCaret %W
    }
}
bind Entry <Delete> {tk_entryBackspace %W; tk_entrySeeCaret %W}
bind Entry <BackSpace> {tk_entryBackspace %W; tk_entrySeeCaret %W}
bind Entry <Control-h> {tk_entryBackspace %W; tk_entrySeeCaret %W}
bind Entry <Control-d> {%W delete sel.first sel.last; tk_entrySeeCaret %W}
bind Entry <Control-u> {%W delete 0 end}
bind Entry <Control-v> {%W insert insert [selection get]; tk_entrySeeCaret %W}
bind Entry <Control-w> {tk_entryBackword %W; tk_entrySeeCaret %W}
tk_bindForTraversal Entry

# ----------------------------------------------------------------------
# Class bindings for listbox widgets.
# ----------------------------------------------------------------------

bind Listbox <1> {%W select from [%W nearest %y]}
bind Listbox <B1-Motion> {%W select to [%W nearest %y]}
bind Listbox <Shift-1> {%W select adjust [%W nearest %y]}
bind Listbox <Shift-B1-Motion> {%W select to [%W nearest %y]}
bind Listbox <2> {%W scan mark %x %y}
bind Listbox <B2-Motion> {%W scan dragto %x %y}

# ----------------------------------------------------------------------
# Class bindings for scrollbar widgets.  When strict Motif is requested,
# the bindings use $tk_priv(buttons) and $tk_priv(activeFg) to set the
# -activeforeground color to -foreground when the mouse is in the window
# and restore it when the mouse leaves.
# ----------------------------------------------------------------------

bind Scrollbar <Any-Enter> {
    if $tk_strictMotif {
	set tk_priv(activeFg) [lindex [%W config -activeforeground] 4]
	%W config -activeforeground [lindex [%W config -foreground] 4]
    }
}
bind Scrollbar <Any-Leave> {
    if {$tk_strictMotif && ($tk_priv(buttons) == 0)} {
	%W config -activeforeground $tk_priv(activeFg)
    }
}
bind Scrollbar <Any-ButtonPress> {incr tk_priv(buttons)}
bind Scrollbar <Any-ButtonRelease> {incr tk_priv(buttons) -1}

# ----------------------------------------------------------------------
# Class bindings for scale widgets.  When strict Motif is requested,
# the bindings use $tk_priv(buttons) and $tk_priv(activeFg) to set the
# -activeforeground color to -foreground when the mouse is in the window
# and restore it when the mouse leaves.
# ----------------------------------------------------------------------

bind Scale <Any-Enter> {
    if $tk_strictMotif {
	set tk_priv(activeFg) [lindex [%W config -activeforeground] 4]
	%W config -activeforeground [lindex [%W config -sliderforeground] 4]
    }
}
bind Scale <Any-Leave> {
    if {$tk_strictMotif && ($tk_priv(buttons) == 0)} {
	%W config -activeforeground $tk_priv(activeFg)
    }
}
bind Scale <Any-ButtonPress> {incr tk_priv(buttons)}
bind Scale <Any-ButtonRelease> {incr tk_priv(buttons) -1}

# ----------------------------------------------------------------------
# Class bindings for menubutton widgets.  Variables used:
# $tk_priv(posted) -		keeps track of the menubutton whose menu is
#				currently posted (or empty string, if none).
# $tk_priv(inMenuButton)-	if non-null, identifies menu button
#				containing mouse pointer.
# $tk_priv(relief) -		keeps track of original relief of posted
#				menu button, so it can be restored later.
# $tk_priv(dragging) -		if non-null, identifies menu button whose
#				menu is currently being dragged in a tear-off
#				operation.
# $tk_priv(focus) -		records old focus window so focus can be
#				returned there after keyboard traversal
#				to menu.
# ----------------------------------------------------------------------

bind Menubutton <Any-Enter> {
    set tk_priv(inMenuButton) %W
    if {[lindex [%W config -state] 4] != "disabled"} {
	if {!$tk_strictMotif} {
	    %W config -state active
	}
    }
}
bind Menubutton <Any-Leave> {
    set tk_priv(inMenuButton) {}
    if {[lindex [%W config -state] 4] == "active"} {
	%W config -state normal
    }
}
bind Menubutton <1> {tk_mbButtonDown %W}
bind Menubutton <Any-ButtonRelease-1> {
    if {($tk_priv(posted) == "%W") && ($tk_priv(inMenuButton) == "%W")} {
	[lindex [$tk_priv(posted) config -menu] 4] activate 0
    } else {
	tk_mbUnpost
    }
}

# The binding below is trickier than it looks.  It's important to check
# to see that another menu is posted in the "if" statement below.
# The check is needed because some window managers (e.g. mwm in
# click-to-focus mode) cause a button-press event to be preceded by
# a B1-Enter event;  we don't want to process that B1-Enter event (if
# we do, the grab may get mis-set so that the menu is non-responsive).

bind Menubutton <B1-Enter> {
    set tk_priv(inMenuButton) %W
    if {([lindex [%W config -state] 4] != "disabled")
	    && ($tk_priv(posted) != "")} {
	if {!$tk_strictMotif} {
	    %W config -state active
	}
	tk_mbPost %W
    }
}
bind Menubutton <2> {
    if {($tk_priv(posted) == "")
	    && ([lindex [%W config -state] 4] != "disabled")} {
	set tk_priv(dragging) %W
	[lindex [$tk_priv(dragging) config -menu] 4] post %X %Y
    }
}
bind Menubutton <B2-Motion> {
    if {$tk_priv(dragging) != ""} {
	[lindex [$tk_priv(dragging) config -menu] 4] post %X %Y
    }
}
bind Menubutton <ButtonRelease-2> {set tk_priv(dragging) ""}

# ----------------------------------------------------------------------
# Class bindings for menu widgets.  $tk_priv(x) and $tk_priv(y) are used
# to keep track of the position of the mouse cursor in the menu window
# during dragging of tear-off menus.  $tk_priv(window) keeps track of
# the menu containing the mouse, if any.
# ----------------------------------------------------------------------

bind Menu <Any-Enter> {set tk_priv(window) %W; %W activate @%y}
bind Menu <Any-Leave> {set tk_priv(window) {}; %W activate none}
bind Menu <Any-Motion> {
    if {$tk_priv(window) == "%W"} {
	%W activate @%y
    }
}
bind Menu <1> {
    if {$tk_priv(grab) != ""} {
	grab $tk_priv(grab)
    }
}
bind Menu <ButtonRelease-1> {tk_invokeMenu %W}
bind Menu <2> {set tk_priv(x) %x; set tk_priv(y) %y}
bind Menu <B2-Motion> {
    if {$tk_priv(posted) == ""} {
	%W post [expr %X-$tk_priv(x)] [expr %Y-$tk_priv(y)]
    }
}
bind Menu <B2-Leave> { }
bind Menu <B2-Enter> { }
bind Menu <Escape> {tk_mbUnpost}
bind Menu <Any-KeyPress> {tk_traverseWithinMenu %W %A}
bind Menu <Left> {tk_nextMenu -1}
bind Menu <Right> {tk_nextMenu 1}
bind Menu <Up> {tk_nextMenuEntry -1}
bind Menu <Down> {tk_nextMenuEntry 1}
bind Menu <Return> {tk_invokeMenu %W}

# ----------------------------------------------------------------------
# Class bindings for text widgets. $tk_priv(selectMode) holds one of
# "char", "word", or "line" to indicate which selection mode is active.
# ----------------------------------------------------------------------

bind Text <1> {
    set tk_priv(selectMode) char
    %W mark set insert @%x,%y
    %W mark set anchor insert
    if {[lindex [%W config -state] 4] == "normal"} {focus %W}
}
bind Text <Double-1> {
    set tk_priv(selectMode) word
    %W mark set insert "@%x,%y wordstart"
    tk_textSelectTo %W insert
}
bind Text <Triple-1> {
    set tk_priv(selectMode) line
    %W mark set insert "@%x,%y linestart"
    tk_textSelectTo %W insert
}
bind Text <B1-Motion> {tk_textSelectTo %W @%x,%y}
bind Text <Shift-1> {
    tk_textResetAnchor %W @%x,%y
    tk_textSelectTo %W @%x,%y
}
bind Text <Shift-B1-Motion> {tk_textSelectTo %W @%x,%y}
bind Text <2> {%W scan mark %y}
bind Text <B2-Motion> {%W scan dragto %y}
bind Text <Any-KeyPress> {
    if {"%A" != ""} {
	%W insert insert %A
	%W yview -pickplace insert
    }
}
bind Text <Return> {%W insert insert \n; %W yview -pickplace insert}
bind Text <BackSpace> {tk_textBackspace %W; %W yview -pickplace insert}
bind Text <Delete> {tk_textBackspace %W; %W yview -pickplace insert}
bind Text <Control-h> {tk_textBackspace %W; %W yview -pickplace insert}
bind Text <Control-d> {%W delete sel.first sel.last}
bind Text <Control-v> {
    %W insert insert [selection get]
    %W yview -pickplace insert
}
tk_bindForTraversal Text

# Initialize the elements of tk_priv that require initialization.

set tk_priv(buttons) 0
set tk_priv(buttonWindow) {}
set tk_priv(dragging) {}
set tk_priv(focus) {}
set tk_priv(grab) {}
set tk_priv(inMenuButton) {}
set tk_priv(posted) {}
set tk_priv(selectMode) char
set tk_priv(window) {}

# init.tcl --
#
# Default system startup file for Tcl-based applications.  Defines
# "unknown" procedure and auto-load facilities.
#
# /mit/tkwww/CVS/WWW/TkWWW/Tcl/binary.sed,v 1.1 1994/04/01 08:36:00 joe Exp SPRITE (Berkeley)
#
# Copyright (c) 1991-1993 The Regents of the University of California.
# All rights reserved.
#
# Permission is hereby granted, without written agreement and without
# license or royalty fees, to use, copy, modify, and distribute this
# software and its documentation for any purpose, provided that the
# above copyright notice and the following two paragraphs appear in
# all copies of this software.
#
# IN NO EVENT SHALL THE UNIVERSITY OF CALIFORNIA BE LIABLE TO ANY PARTY FOR
# DIRECT, INDIRECT, SPECIAL, INCIDENTAL, OR CONSEQUENTIAL DAMAGES ARISING OUT
# OF THE USE OF THIS SOFTWARE AND ITS DOCUMENTATION, EVEN IF THE UNIVERSITY OF
# CALIFORNIA HAS BEEN ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
#
# THE UNIVERSITY OF CALIFORNIA SPECIFICALLY DISCLAIMS ANY WARRANTIES,
# INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY
# AND FITNESS FOR A PARTICULAR PURPOSE.  THE SOFTWARE PROVIDED HEREUNDER IS
# ON AN "AS IS" BASIS, AND THE UNIVERSITY OF CALIFORNIA HAS NO OBLIGATION TO
# PROVIDE MAINTENANCE, SUPPORT, UPDATES, ENHANCEMENTS, OR MODIFICATIONS.
#

set auto_path [info library]

# unknown:
# Invoked when a Tcl command is invoked that doesn't exist in the
# interpreter:
#
#	1. See if the autoload facility can locate the command in a
#	   Tcl script file.  If so, load it and execute it.
#	2. See if the command exists as an executable UNIX program.
#	   If so, "exec" the command.
#	3. If the command was invoked at top-level:
#	    (a) see if the command requests csh-like history substitution
#		in one of the common forms !!, !<number>, or ^old^new.  If
#		so, emulate csh's history substitution.
#	    (b) see if the command is a unique abbreviation for another
#		command.  If so, invoke the command.

proc unknown args {
    global auto_noexec auto_noload env unknown_pending tcl_interactive;

    set name [lindex $args 0]
    if ![info exists auto_noload] {
	#
	# Make sure we're not trying to load the same proc twice.
	#
	if [info exists unknown_pending($name)] {
	    unset unknown_pending($name)
	    if {[array size unknown_pending] == 0} {
		unset unknown_pending
	    }
	    return -code error "self-referential recursion in \"unknown\" for command \"$name\"";
	}
	set unknown_pending($name) pending;
	set ret [catch {auto_load $name} msg]
	unset unknown_pending($name);
	if {$ret != 0} {
	    return -code $ret "error while autoloading \"$name\": $msg"
	}
	if ![array size unknown_pending] {
	    unset unknown_pending
	}
	if $msg {
	    return [uplevel $args]
	}
    }
    if {([info level] == 1) && ([info script] == "") && $tcl_interactive} {
	if ![info exists auto_noexec] {
	    if [auto_execok $name] {
		return [uplevel exec >&@stdout <@stdin $args]
	    }
	}
	if {$name == "!!"} {
	    return [uplevel {history redo}]
	}
	if [regexp {^!(.+)$} $name dummy event] {
	    return [uplevel [list history redo $event]]
	}
	if [regexp {^\^([^^]*)\^([^^]*)\^?$} $name dummy old new] {
	    return [uplevel [list history substitute $old $new]]
	}
	set cmds [info commands $name*]
	if {[llength $cmds] == 1} {
	    return [uplevel [lreplace $args 0 0 $cmds]]
	}
	if {[llength $cmds] != 0} {
	    if {$name == ""} {
		return -code error "empty command name \"\""
	    } else {
		return -code error \
			"ambiguous command name \"$name\": [lsort $cmds]"
	    }
	}
    }
    return -code error "invalid command name \"$name\""
}

# auto_load:
# Checks a collection of library directories to see if a procedure
# is defined in one of them.  If so, it sources the appropriate
# library file to create the procedure.  Returns 1 if it successfully
# loaded the procedure, 0 otherwise.

proc auto_load cmd {
    global auto_index auto_oldpath auto_path env errorInfo errorCode

    if [info exists auto_index($cmd)] {
	uplevel #0 $auto_index($cmd)
	return 1
    }
    if [catch {set path $auto_path}] {
	if [catch {set path $env(TCLLIBPATH)}] {
	    if [catch {set path [info library]}] {
		return 0
	    }
	}
    }
    if [info exists auto_oldpath] {
	if {$auto_oldpath == $path} {
	    return 0
	}
    }
    set auto_oldpath $path
    catch {unset auto_index}
    foreach dir $path {
	set f ""
	if [catch {set f [open $dir/tclIndex]}] {
	    continue
	}
	set error [catch {
	    set id [gets $f]
	    if {$id == "# Tcl autoload index file, version 2.0"} {
		eval [read $f]
	    } elseif {$id == "# Tcl autoload index file: each line identifies a Tcl"} {
		while {[gets $f line] >= 0} {
		    if {([string index $line 0] == "#")
			    || ([llength $line] != 2)} {
			continue
		    }
		    set name [lindex $line 0]
		    if {![info exists auto_index($name)]} {
			set auto_index($name) "source $dir/[lindex $line 1]"
		    }
		}
	    } else {
		error "$dir/tclIndex isn't a proper Tcl index file"
	    }
	} msg]
	if {$f != ""} {
	    close $f
	}
	if $error {
	    error $msg $errorInfo $errorCode
	}
    }
    if [info exists auto_index($cmd)] {
	uplevel #0 $auto_index($cmd)
	if {[info commands $cmd] != ""} {
	    return 1
	}
    }
    return 0
}

# auto_execok:
# Returns 1 if there's an executable in the current path for the
# given name, 0 otherwise.  Builds an associative array auto_execs
# that caches information about previous checks, for speed.

proc auto_execok name {
    global auto_execs env

    if [info exists auto_execs($name)] {
	return $auto_execs($name)
    }
    set auto_execs($name) 0
    if {[string first / $name] >= 0} {
	if {[file executable $name] && ![file isdirectory $name]} {
	    puts "special, ok!"
	    set auto_execs($name) 1
	}
	return $auto_execs($name)
    }
    foreach dir [split $env(PATH) :] {
	if {[file executable $dir/$name] && ![file isdirectory $dir/$name]} {
	    set auto_execs($name) 1
	    return 1
	}
    }
    return 0
}

# auto_reset:
# Destroy all cached information for auto-loading and auto-execution,
# so that the information gets recomputed the next time it's needed.
# Also delete any procedures that are listed in the auto-load index
# except those related to auto-loading.

proc auto_reset {} {
    global auto_execs auto_index auto_oldpath
    foreach p [info procs] {
	if {[info exists auto_index($p)] && ($p != "unknown")
		&& ![string match auto_* $p]} {
	    rename $p {}
	}
    }
    catch {unset auto_execs}
    catch {unset auto_index}
    catch {unset auto_oldpath}
}

# auto_mkindex:
# Regenerate a tclIndex file from Tcl source files.  Takes as argument
# the name of the directory in which the tclIndex file is to be placed,
# floowed by any number of glob patterns to use in that directory to
# locate all of the relevant files.

proc auto_mkindex {dir args} {
    global errorCode errorInfo
    set oldDir [pwd]
    cd $dir
    set dir [pwd]
    append index "# Tcl autoload index file, version 2.0\n"
    append index "# This file is generated by the \"auto_mkindex\" command\n"
    append index "# and sourced to set up indexing information for one or\n"
    append index "# more commands.  Typically each line is a command that\n"
    append index "# sets an element in the auto_index array, where the\n"
    append index "# element name is the name of a command and the value is\n"
    append index "# a script that loads the command.\n\n"
    foreach file [eval glob $args] {
	set f ""
	set error [catch {
	    set f [open $file]
	    while {[gets $f line] >= 0} {
		if [regexp {^proc[ 	]+([^ 	]*)} $line match procName] {
		    append index "set [list auto_index($procName)]"
		    append index " \"source \$dir/$file\"\n"
		}
	    }
	    close $f
	} msg]
	if $error {
	    set code $errorCode
	    set info $errorInfo
	    catch {close $f}
	    cd $oldDir
	    error $msg $info $code
	}
    }
    set f [open tclIndex w]
    puts $f $index nonewline
    close $f
    cd $oldDir
}

# parray:
# Print the contents of a global array on stdout.
#
# /mit/tkwww/CVS/WWW/TkWWW/Tcl/binary.sed,v 1.1 1994/04/01 08:36:00 joe Exp SPRITE (Berkeley)
#
# Copyright (c) 1991-1993 The Regents of the University of California.
# All rights reserved.
#
# Permission is hereby granted, without written agreement and without
# license or royalty fees, to use, copy, modify, and distribute this
# software and its documentation for any purpose, provided that the
# above copyright notice and the following two paragraphs appear in
# all copies of this software.
#
# IN NO EVENT SHALL THE UNIVERSITY OF CALIFORNIA BE LIABLE TO ANY PARTY FOR
# DIRECT, INDIRECT, SPECIAL, INCIDENTAL, OR CONSEQUENTIAL DAMAGES ARISING OUT
# OF THE USE OF THIS SOFTWARE AND ITS DOCUMENTATION, EVEN IF THE UNIVERSITY OF
# CALIFORNIA HAS BEEN ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
#
# THE UNIVERSITY OF CALIFORNIA SPECIFICALLY DISCLAIMS ANY WARRANTIES,
# INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY
# AND FITNESS FOR A PARTICULAR PURPOSE.  THE SOFTWARE PROVIDED HEREUNDER IS
# ON AN "AS IS" BASIS, AND THE UNIVERSITY OF CALIFORNIA HAS NO OBLIGATION TO
# PROVIDE MAINTENANCE, SUPPORT, UPDATES, ENHANCEMENTS, OR MODIFICATIONS.
#

proc parray a {
    upvar 1 $a array
    if [catch {array size array}] {
	error "\"$a\" isn't an array"
    }
    set maxl 0
    foreach name [lsort [array names array]] {
	if {[string length $name] > $maxl} {
	    set maxl [string length $name]
	}
    }
    set maxl [expr {$maxl + [string length $a] + 2}]
    foreach name [lsort [array names array]] {
	set nameString [format %s(%s) $a $name]
	puts stdout [format "%-*s = %s" $maxl $nameString $array($name)]
    }
}
