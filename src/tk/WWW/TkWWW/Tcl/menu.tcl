## menu.tcl: make menu out of list of node names
## ==============
## Copyright (C) 1992-1993
## Globewide Network Academy
## Macvicar Institue for Educational Software Development
##
## See the file COPYRIGHT for conditions

## Conventions:
## All procedures in this file begin with tkW3Menu

# tkW3MenuMakeMenus 
# Add items to the menu bar to allow direct access of
# some information
# The format for the input list is
# {{menuname1 title1} {{title1 address1} {title2 address2}}} 
#  {menuname2 title2} {{title1 address1}}}}
#
# returns a list of menus created

proc tkW3MenuMakeMenus {menubar list} {
    frame $menubar
    
    set menu_list ""
    foreach menu_item $list {
        set menu [lindex $menu_item 0]
        set menu_name $menubar.[lindex $menu 0]
        set menu_title [lindex $menu 1]
        set menu_underline [lindex $menu 2]
        set menu_command [lindex $menu 3]
        
        if {$menu_command == "right"} {
	    set position "right"
	} {
	    set position "left"
	}

	pack [menubutton $menu_name -text $menu_title \
             -menu $menu_name.m] -side $position
        if {$menu_underline != ""} {
	    $menu_name configure -underline $menu_underline
	}

        tkW3MenuMakeMenuPane $menu_name.m [lindex $menu_item 1]
        lappend menu_list $menu_name
    }
    eval tk_menuBar $menubar $menu_list
    tk_bindForTraversal . 
    return $menubar
}


proc tkW3MenuMakeMenuPane {w menu_list} {
    menu $w
    foreach item $menu_list {
       if {[llength $item] == 0} {
	   $w add separator
       } { 
	   set name [lindex $item 0]
	   switch "[lindex $item 1]" {
	       "cascade_external" {
		   $w add cascade -label $name -menu $w.[lindex $item 2]
		   menu $w.[lindex $item 2]
	       } 
	       "cascade" {
		   $w add cascade -label $name \
		       -menu [tkW3MenuMakeMenuPane $w.m [lindex $item 2]]
	       } 
	       default {
		   $w add command -label $name -command [lindex $item 1]
		   if {[lindex $item 2] != ""} {
		       $w entryconfigure last -underline [lindex $item 2]
		   }
		   if {[lindex $item 3] != ""} {
		       $w entryconfigure last -accelerator [lindex $item 3]
		       bind Text [lindex $item 3] [lindex $item 1] 
		   }
	       }
	   }
       }
    }
    return $w
}
