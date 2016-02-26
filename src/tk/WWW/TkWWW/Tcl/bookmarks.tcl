## bookmarks.tcl tkW3 bookmarks facility
## ==============
## Copyright (C) 1992-1993
## Globewide Network Academy
## Macvicar Institute for Educational Software Development
##
## See the file COPYRIGHT for conditions

## This file contains code to implement the bookmarks feature of tkW3

## Conventions:
##   all global visible objects in this file should begin with 
##   tkW3Bookmarks

## Global Variables

# tkW3BookmarksList contains bookmarks in 
# {{title address} {title address}} format
set tkW3Bookmarks(list) {}
set tkW3Bookmarks(file) $env(HOME)/.mosaic-hotlist-default

proc tkW3BookmarksInitialize {} {
    global tkW3Bookmarks
    if {[file exists $tkW3Bookmarks(file)]} {
	tkW3BookmarksLoad
    }
}

proc tkW3BookmarksDialog {} {
    global tkW3Bookmarks
    set parent .
    set w .bookmarks

    tkW3BookmarksLoad
    DLG:toplevel $parent $w
    tkW3OutputMakeButtons $w.button_frame {
	{current "Add current" "tkW3BookmarksAdd"}
	{goto "Go To" "tkW3BookmarksGotoSelected"}
	{clone "Clone" "tkW3BookmarksGotoSelected 1"}
	{remove "Remove" "tkW3BookmarksDeleteSelected"}
	{edit "Edit Title" "tkW3BookmarksEditTitle"}
    }

    pack append $w $w.button_frame top
    DLG:draw_listbox $w {}
    DLG:draw_buttons $w [list "Dismiss" "Help"]

    DLG:bind_button $w 1 "DLG:hide $w"
    DLG:bind_button $w 2 "tkW3HelpNoHelp"
    
    foreach item $tkW3Bookmarks(list) {
	$w.list insert end [lindex $item 0]
    }
    bind $w.list <Double-1> "$w.button_frame.goto flash
%W select from \[%W nearest %y\]
tkW3BookmarksGotoSelected"

    bind $w.list <Double-2> "$w.button_frame.clone flash
%W select from \[%W nearest %y\]
tkW3BookmarksGotoSelected 1"

    DLG:show $parent $w
}

proc tkW3BookmarksGotoSelected {{clone 0}} {
    global tkW3Bookmarks
    set address  [ \
       lindex [lindex $tkW3Bookmarks(list) \
	       [DLG:get_listbox_highlighted .bookmarks]] 1]
    
    if {$clone} {
	tkW3NavigateClone $address
    } {
	tkW3NavigateRecordAndGoto $address
    }
}

proc tkW3BookmarksDeleteSelected {} {
    global tkW3Bookmarks
    set i [DLG:get_listbox_highlighted .bookmarks]
    set tkW3Bookmarks(list) [ lreplace $tkW3Bookmarks(list) $i $i]
    .bookmarks.list delete $i
    tkW3BookmarksSave
}

proc tkW3BookmarksEditTitle {} {
    global tkW3Bookmarks
    set i [DLG:get_listbox_highlighted .bookmarks]
    set title [lindex [lindex $tkW3Bookmarks(list) $i] 0]

    DLG:entry . .edit_dialog "Bookmark title" \
	{"Title:"} "Edit" "tkW3BookmarksDoEditTitle $i"
    DLG:set_entry_value .edit_dialog 1 $title
    DLG:show . .edit_dialog
}

proc tkW3BookmarksDoEditTitle {i title} {
    global tkW3Bookmarks
    set address [lindex [lindex $tkW3Bookmarks(list) $i] 1]
    set tkW3Bookmarks(list) \
	[lreplace $tkW3Bookmarks(list) $i $i [list $title $address]]
    .bookmarks.list delete $i
    .bookmarks.list insert $i $title
    tkW3BookmarksSave
}

## tkW3BookmarksAdd: Add a bookmark
## -----------
## Arguments
##   title - Title of the bookmark
##   address - Address bookmark refers to
##
## If called with no arguments it sets title and address to current values
## No return value
## 
## This procedure adds a bookmark to the list of bookmarks
##

proc tkW3BookmarksAdd {{title ""} {address ""}} {
    global tkW3Bookmarks
    if {$title == "" && $address == ""} {
	set address [tkW3NavigateGetAddress]
	set title [tkW3NavigateGetTitle]
    }
    lappend tkW3Bookmarks(list) [list $title $address]
    tkW3BookmarksSave
    if {[winfo exists .bookmarks.list]} {
	.bookmarks.list insert end $title
    }
}

# Load bookmarks from file

proc tkW3BookmarksLoad {{filename ""}} {
    global tkW3Bookmarks

    if {$filename == ""} {
	set filename $tkW3Bookmarks(file)
    }

# open the file
# trap for errors
    if {[catch {open $filename r} file]} {
	tkW3OutputError $file
	return
    }

# clear the bookmarks
    set tkW3Bookmarks(list) ""

    gets $file format
    gets $file name

# while there is stuff in the file
    while {[gets $file string] != -1} { 
	# the line contains a hypertext anchor load it
	set address [lindex [split $string " \t"] 0]
	gets $file title
	lappend tkW3Bookmarks(list) [list $title $address]
    }
    close $file
}

# save bookmarks in "filename"
proc tkW3BookmarksSave {{filename ""}} {
    global tkW3Bookmarks

    if {$filename == ""} {
	set filename $tkW3Bookmarks(file)
    }

    if {[catch {open $filename w} file]} {
	tkW3OutputError $file
	return
    }

    puts $file "ncsa-xmosaic-hotlist-format-1"
    puts $file "default"

    foreach item $tkW3Bookmarks(list) {
	puts $file "[lindex $item 1] [exec date]"
	puts $file "[lindex $item 0]"
    }
    close $file
}
