## file.tcl file menu items
## ==============
## Copyright (C) 1992-1993
## Globewide Network Academy
## Macvicar Institute for Educational Software Development
##
## See the file COPYRIGHT for conditions

proc tkW3FileInitialize {} {
    global tkW3ConfigViewer
    DLG:entry . .mail_dialog "Enter the To and Subject fields" \
	{"To:" "Subject:"} "Mail" "tkW3NavigateMailText"
    DLG:entry . .find_dialog "Enter the keywords to find" \
	{"Find Keywords:"} "Find" "tkW3NavigateFind"
    DLG:entry . .goto_dialog "Enter the link you wish to go to" \
	{"Goto Page:"} "Goto" "tkW3NavigateRecordAndGoto"
    DLG:entry . .print_dialog "Enter the command to print with" \
	{"Print Command:"} "Print" "tkW3NavigatePrintUsingCommand"
    DLG:entry . .tcl_command "Run TCL Command" \
	{"Tcl Command:"} {Run TCL Command} "eval"
    DLG:set_entry_value .print_dialog 1 $tkW3ConfigViewer(printer)
}

proc tkW3FileQueryCloseWindow {} {
    if {[DLG:msg . .close_window_dialog \
	 "Are you sure you want to close this window?" question "Yes" "No"] \
	     == 1} {
	     tkW3NavigateCloseThisWindow
	 }
}

proc tkW3FileQueryExit {} {
    if {[DLG:msg "" .exit_dialog \
	 "Are you sure you want to close ALL tkWWW windows?" question \
	     "Yes" "No"] == 1} {
	     tkW3NavigateCloseAllWindows
	 }
}

proc tkW3FileLoadSource {} {
    FSBox {Load Source} "source.html" \
	{tkW3NavigateGoto file:$fsBox(path)/$fsBox(name)}
    DLG:show . .fsBox
}

proc tkW3FileSaveSource {} {
    FSBox {Save Source} "source.html" \
	{tkW3NavigateSaveSource $fsBox(path)/$fsBox(name)}
    DLG:show . .fsBox
}


proc tkW3FileSave {} {
    tkW3EditOpen 0
    tkW3EditCopyText .f.msg .edit.main.t
    tkW3EditCopyTags .f.msg .edit.main.t
    tkW3EditConvertToHTML .edit.main.t
    tkW3EditSave 1
}   

proc tkW3FileSaveAs {} {
    tkW3EditOpen 0
    tkW3EditCopyText .f.msg .edit.main.t
    tkW3EditCopyTags .f.msg .edit.main.t
    tkW3EditConvertToHTML .edit.main.t
    tkW3EditSave
}   
    
proc tkW3FileSaveText {} {
    FSBox {Save Text} "file.txt" \
	{tkW3NavigateSaveText $fsBox(path)/$fsBox(name)}
    DLG:show . .fsBox
}

proc tkW3FileRunTclCommand {{w .}} {
    DLG:show $w .tcl_command
}

proc tkW3FileChangeDirectory {} {
    global fsBox
    set oldPattern $fsBox(pattern)
    set fsBox(pattern) */
    FSBox {Change Directory} "" {cd $fsBox(path)}
    DLG:show . .fsBox
    set fsBox(pattern) $oldPattern
}
