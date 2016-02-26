## history.tcl: history for tkWWW  user interface
## ==============
## Copyright (C) 1993
## Globewide Network Academy
## MacVicar Institute for Educational Software Development
##
## See the file COPYRIGHT for conditions

set tkW3HistoryList {}
set tkW3HistoryOldList {}

proc tkW3HistoryInitialize {} {
}

proc tkW3HistoryRecord {title address {scroll_pos ""} {anchor ""} {ismap ""}} {
    global tkW3HistoryList tkW3HistoryOldList

    set tkW3HistoryOldList $tkW3HistoryList

    if {$title == {}} {
	set title "(Address)$address"
    }

    lappend tkW3HistoryList \
	[list $title $address {} $scroll_pos $anchor $ismap]
}

proc tkW3HistoryRestore {} {
    global tkW3HistoryList tkW3HistoryOldList

    set tkW3HistoryList $tkW3HistoryOldList
}

proc tkW3HistoryPop {} {
    global tkW3HistoryList
    tkW3HtListPop tkW3HistoryList
}

proc tkW3HistoryDialog {} {
    global tkW3HistoryList
    set w .history
    DLG:toplevel . $w

    tkW3OutputMakeButtons $w.button_frame {
	{goto "Go To" "tkW3HistoryGotoSelected"}
    }

    pack append $w $w.button_frame top
    DLG:draw_listbox $w {}
    DLG:draw_buttons $w [list "Dismiss" "Help"]

    DLG:bind_button $w 1 "DLG:hide $w"
    DLG:bind_button $w 2 "tkW3HelpNoHelp"
    bind $w.list <Double-1> "$w.button_frame.goto flash
%W select from \[%W nearest %y\]
tkW3HistoryGotoSelected"


    foreach item $tkW3HistoryList {
	$w.list insert end [lindex $item 0]
    }
    DLG:show . $w
}

proc tkW3HistoryGotoSelected {} {
    global tkW3HistoryList
    tkW3NavigateRecordAndGoto [ \
       lindex [lindex $tkW3HistoryList \
	       [DLG:get_listbox_highlighted .history]] 1]
}
