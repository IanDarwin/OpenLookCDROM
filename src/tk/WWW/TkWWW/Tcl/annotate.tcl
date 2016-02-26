## annotation.tcl: Procedures for handling annotations
## ================
## Copyright (C) 1993 
## Globewide Network Academy
## Macvicar Institute for Educational Software Development
##
## See the file COPYRIGHT for conditions
##
## The format of the annotations is that of Xmosaic by Marc Andersen

## Add annotation
proc tkW3AnnotateAdd {} {
    set w .annotate_dialog
    set address [tkW3NavigateGetAddress]
    tkW3AnnotateCreateEditor $w "
tkW3PanNewPan $address \[DLG:get_entry_value $w 2\] \
 \[DLG:get_entry_value $w 1\] \[$w.text get 0.0 end\]
tkW3ConfigDisplay" ""
    set author [exec whoami]
    DLG:set_entry_value .annotate_dialog 1 $author 
    DLG:set_entry_value .annotate_dialog 2 "Annotation by $author"
    DLG:show . .annotate_dialog
}

## Edit annotation
proc tkW3AnnotateEdit {} {
    set w .annotate_dialog
    set address [tkW3NavigateGetAddress]
    if {[tkW3PanIsEditablePan $address]} {
	set list [tkW3PanGrokPanPieces $address]
	tkW3AnnotateCreateEditor $w "
tkW3PanModifyPan [lindex $list 0] \[DLG:get_entry_value $w 2\]\
   \[DLG:get_entry_value $w 1\] \[$w.text get 0.0 end\]
tkW3NavigateUncache $address
tkW3NavigateGoto $address" "tkW3PanDeletePan [lindex $list 0]"

	DLG:set_entry_value $w 1 [lindex $list 2]
	DLG:set_entry_value $w 2 [lindex $list 1]
	.annotate_dialog.text insert 0.0 [lindex $list 3]

	DLG:show . $w
    }
}

## Create the annotation editor
proc tkW3AnnotateCreateEditor {w command delete_command} {
    DLG:toplevel . $w
    DLG:draw_entries $w {"Annotation Author:" "Annotation Title:"}

    pack append $w \
	[label $w.label \
	   -text "Please type your comments below" ] { fillx top} \
	[text $w.text ] {expand fill top}

    DLG:draw_buttons $w [list "Annotate" "Delete" "Dismiss" "Help"]
    DLG:bind_button $w 1 "$command ; DLG:hide $w"
    DLG:bind_button $w 2 "$delete_command ; DLG:hide $w"
    DLG:bind_button $w 3 "DLG:hide $w"
    DLG:bind_button $w 4 "tkW3HelpNoHelp"
}

## Display annotations 
proc tkW3AnnotateDisplay {} {
    tkW3PanDisplayAnnotations [tkW3NavigateGetAddress]
}

