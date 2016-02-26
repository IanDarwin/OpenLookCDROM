## edit.selection.tcl Handling Selection
## ==============
## Copyright (C) 1992-1993
## Globewide Network Academy
## Macvicar Institute for Educational Software Development
##
## See the file COPYRIGHT for conditions


## The procedures in this file are responsible for editing inside of tkWWW.

## Conventions:
##   All procedures in this file begin with tkWWWEditSelection


## ********************
## Selection Handling Procedures:
## ********************

proc tkW3EditSelectionGet {} {
    # the safe way
    set xfSelection ""
    catch "selection get" xfSelection
    if {"$xfSelection" == "selection doesn't exist or form \"STRING\" not defined"} {
	return ""
    } {
	return $xfSelection
    }
}

proc tkW3EditSelectionClear {w} {
    set range [$w tag ranges sel]
    set length [llength $range]
    for {set i 0} {$i < $length} {incr i 2} {
      set j $i
      incr j 1
      $w tag remove sel [lindex $range $i] [lindex $range $j]
    }
}

proc tkW3EditSelectionExists {w} {
    if {[$w tag ranges sel] == ""} {
      return 0
    }
    return 1
}

proc tkW3EditSelectionChange {w start finish} {
     tkW3EditClearSelection $w
     $w tag add sel $start $finish
}

