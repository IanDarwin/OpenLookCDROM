## edit.anchor.tcl Functions relating to adding anchors
## ==============
## Copyright (C) 1992-1993
## Globewide Network Academy
## Macvicar Institute for Educational Software Development
##
## See the file COPYRIGHT for conditions

## ******************
## tkW3EditAnchor
## -----------------
## Insert or edit an Anchor
## ******************

## tkW3EditBeginAnchor
## -----------------
## Turn the currently highlighted area into an Anchor

proc tkW3EditBeginAnchor {w {anchorDialog "tkW3EditAnchorDialog"}} {
    global tkW3EditVars

    if {[tkW3EditSelectionExists $w]} {

        # store selection before it can be lost during editing the dialog:
        set tkW3EditVars(anchorStart) [.f.msg index sel.first]
        set tkW3EditVars(anchorFinish) [.f.msg index sel.last]
        $anchorDialog
    }
}


## tkW3EditAnchorDialog
## --------------------
## the dialog for entering an anchor.

proc tkW3EditAnchorDialog {} {
    global tkW3EditVars

        DLG:entry . .anchor_dialog "Anchor Dialog" \
            {"Name:" "Href:"} {Begin} "tkW3EditAddAnchor"
        if {"$tkW3EditVars(name)$tkW3EditVars(href)" != ""} {
            DLG:set_entry_value .anchor_dialog 1 $tkW3EditVars(name)
            DLG:set_entry_value .anchor_dialog 2 $tkW3EditVars(href)
        }
        DLG:show . .anchor_dialog
        set tkW3EditVars(name) ""
        set tkW3EditVars(href) ""
}

## tkW3EditSetupAnchorDialog
## -------------------------
## Read the current values of anchor ($link_number)

proc tkW3EditSetupAnchorDialog {w {link_number ""}} {
    global tkW3EditVars
    global tkW3HtPage

        tkW3EditSelectionClear $w
        set tkW3EditVars(name) $tkW3HtPage(anchor.name.$link_number)
        set tkW3EditVars(href) $tkW3HtPage(anchor.href.$link_number)
        $w tag add sel \
            [$w index h$link_number.first] \
            [$w index h$link_number.last]
        set tkW3EditVars(anchorStart) [$w index h$link_number.first]
        set tkW3EditVars(anchorFinish) [$w index h$link_number.last]
    tkW3EditBeginAnchor $w
}

proc tkW3EditAddAnchor {name href} {
    # these array-fileds are set already in tkW3EditBeginAnchor:

    # anchorStart, anchorFinish
    global tkW3EditVars tkW3HtPage

    tkW3HtBeginAnc $name $href
    set number $tkW3HtPage(anchor.index)
    tkW3HtEndAnc

    set start $tkW3EditVars(anchorStart)
    set finish $tkW3EditVars(anchorFinish)

    if { $start != {} } {
        foreach tag [.f.msg tag names $start] {
            if [regexp {h(0-9)+} $tag] {
                .f.msg tag remove $tag $start $finish
            }
        }
        .f.msg tag add Anchor $start $finish
        .f.msg tag add h$number $start $finish
        .f.msg tag remove sel $start $finish
        set $tkW3EditVars(anchorStart) {}
        tkW3EditModifiedSet
    }
}
