## edit.tcl main file for tkWWW  user interface
## ==============
## Copyright (C) 1992-1993
## Globewide Network Academy
## Macvicar Institute for Educational Software Development
##
## See the file COPYRIGHT for conditions

## The procedures in this file are responsible for editing inside of tkWWW.

## Conventions:
##   All procedures in this file begin with tkWWWEdit

## Bugs and Todo:
##   Some abstraction questions

# This is the style that the editor is current writing in

set tkW3EditStyle(para) ""
set tkW3EditStyle(char) ""

# This is the cut buffer
set tkW3EditSelection {}

## ********************
## Initialization procedures
## ********************

proc tkW3EditInitialize {} {
    global tkW3EditConvert tkW3EditChar tkW3EditTags tkW3EditVars

    set convert_list {
	{"" "default"}
	{H1 heading1}
	{H2 heading2}
	{H3 heading3}
	{H4 heading4}
	{H5 heading5}
	{H6 heading6}
	{ADDRESS address}
	{PRE preformatted}
        {XMP example}
	{TT typewriter}
	{B bold}
	{I italics}
	{U underline}
	{EM emphasis}
	{STRONG "strong emphasis"}
	{SAMP sample}
	{KBD keyboard}
	{VAR variable}
	{DFN definition}
	{CITE citation}
    }

    foreach item $convert_list {
	set tkW3EditConvert([lindex $item 0]) [lindex $item 1]
    }

    # divide tags into classes
    set tkW3EditTags(para) {H1 H2 H3 H4 H5 H6 PRE ADDRESS LIST XMP}
    set tkW3EditTags(char) {TT B I U EM STRONG SAMP KBD VAR DFN CITE}
    set tkW3EditTags(bull) {LI DD DT P}

    tkW3EditBindText .f.msg
    tkW3EditDisplayStatus

    set tkW3EditVars(anchorStart) {}
    set tkW3EditVars(href) {}
    set tkW3EditVars(name) {}

# if a key possibly starts a 2-key sequence, to be translated according to
# tkW3EditCharTranslate, store this pending char here:
    set tkW3EditChar(pending) ""
}

set tkW3ConfigEditTk {
    .edit.menu.file.m delete "*Quit*"
    .edit.menu.file.m entryconfigure "Save" -command "tkW3EditSave"
    .edit.menu.edit.m add command -label "Spell Check" -command \
	{ispell_text .edit.main.t}
    set button_list {
	{generate "Generate Source" "tkW3EditGenerate"}
	{save "Save As..." "tkW3EditSave"}
	{close "Close Window" "tkW3EditClose"}
    }
    pack append .edit \
	[ tkW3OutputMakeButtons .edit.button_box $button_list] {bottom fillx}
}

proc tkW3EditBindText {w} {
    global tkW3EditStyle

    bind $w <KeyPress> {tkW3EditInsert %W %A}
    bind $w <Shift-KeyPress> {tkW3EditInsert %W %A}
    bind $w <Control-y> {tkW3EditInsert [tkW3EditSelectionGet]}

    bind $w <ButtonPress-2> {tkW3HtButtonPress %W @%x,%y %b}
    bind $w <ButtonPress-3> {tkW3HtButtonPress %W @%x,%y %b}
    bind $w <ButtonRelease-1> {tkW3HtButtonRelease %W @%x,%y %b}
    bind $w <ButtonRelease-2> {tkW3HtButtonRelease %W @%x,%y %b}
    bind $w <ButtonRelease-3> {tkW3HtButtonRelease %W @%x,%y %b}

    bind $w <ButtonPress-1> {
	tkW3EditSelectionClear %W
	set tk_priv(selectMode) char
	%W mark set insert @%x,%y
	%W mark set insert [tkW3EditDisplace %W [%W index insert]]
	%W mark set anchor insert
	tkW3EditSelectionClear %W
	if {[lindex [%W config -state] 4] == "normal"} {focus %W}
	tkW3EditUseStyle %W
	tkW3HtButtonPress %W @%x,%y %b
    }

    bind $w <Double-Button-1> {
	tkW3EditSelectionClear %W
	set tk_priv(selectMode) word
	%W mark set insert @%x,%y
	%W mark set insert [tkW3EditDisplace %W [%W index insert]]
	%W mark set anchor insert
	%W tag add sel "insert wordstart" "insert wordend"
	if {[lindex [%W config -state] 4] == "normal"} {focus %W}
	tkW3EditUseStyle %W
    }

    bind $w <Return> {
	case $tkW3EditStyle(para) {
	    {H1 H2 H3 H4 H5 H6} {
		tkW3EditInsertClean %W \n
		set tkW3EditStyle(para) ""
		%W yview -pickplace insert
		tkW3EditDisplayStatus
		return
	    }
	    {PRE XMP} {%W insert insert \n}
	    {default} {
		tkW3EditInsertBullet %W "P"
	    }
	}
	%W yview -pickplace insert
	tkW3EditUseStyle %W
    }


    bind $w <BackSpace> {tkW3EditBackspace %W}
    bind $w <Delete>  {tkW3EditBackspace %W}
    bind $w <Control-h> {tkW3EditBackspace %W}
    bind $w <Control-d> {tkW3EditDelChar %W}
    
    bind $w <Control-f> {tkW3EditMoveInsertTo %W "insert + 1 c" 1}
    bind $w <Right>     {tkW3EditMoveInsertTo %W "insert + 1 c" 1}

    bind $w <Control-b> {tkW3EditMoveInsertTo %W "insert - 1 c" 0}
    bind $w <Left>      {tkW3EditMoveInsertTo %W "insert - 1 c" 0}

    bind $w <Control-a> {tkW3EditMoveInsertTo %W "insert linestart" 1}
    
    bind $w <Control-e> {tkW3EditMoveInsertTo %W "insert lineend" 0}

    bind $w <Control-n> {tkW3EditMoveInsertTo %W "insert + 1 lines"}
    bind $w <Down>      {tkW3EditMoveInsertTo %W "insert + 1 lines"}

    bind $w <Control-p> {tkW3EditMoveInsertTo %W "insert - 1 lines" 0}
    bind $w <Up>        {tkW3EditMoveInsertTo %W "insert - 1 lines" 0}

    bind $w <Meta-f>    {tkW3EditMoveInsertTo %W "insert + 1 c wordend" 1}
    bind $w <Meta-b>    {tkW3EditMoveInsertTo %W "insert - 1 c wordstart" 0}
}

## ********************
## Keybinding actions
## ********************

proc tkW3EditInsert {w a} {
    global tkW3EditStyle tkW3EditChar
    set start [$w index insert]
    set tags [$w tag names "insert - 1 c"]
    set add_tag ""
    
    set anchor_number [lsearch $tags "h*"]
    if {$anchor_number != -1} {
	lappend add_tag Anchor
	lappend add_tag [lindex $tags $anchor_number]
    }
    
    if {$tkW3EditStyle(para) != ""} {
	lappend add_tag $tkW3EditStyle(para)
    }

    if {$tkW3EditStyle(char) != ""} {
	lappend add_tag $tkW3EditStyle(char)
    }
    
    if {$a != ""} {
	# this is the regular case of "Modified" :
	tkW3EditModifiedSet
	if { $tkW3EditChar(pending) != ""} {
	    # try to do the translation:
	    set translator [lindex $tkW3EditChar(translate) \
	     [expr [lsearch $tkW3EditChar(translate) $tkW3EditChar(pending)]+1] ]
	    set substit [lindex $translator [lsearch $translator "$a?"]]
	    set char [string index $substit 1]
	    if { $char == "" } {
		if {$a == "\033"} {
		    # ESC = cancell pending char.
		    set a ""
		} {
		    set a "$tkW3EditChar(pending)$a"
		}
	    } {
		set a $char
	    }
	    set tkW3EditChar(pending) ""
	    tkW3OutputSetMessage ""
	}
    }
    if {$a != "\033"} {
	if {[lsearch $tkW3EditChar(translate) $a] == -1} {
	    $w insert insert $a
	    foreach tag [.f.msg tag names $start] {
		.f.msg tag remove $tag $start insert
           }
	    foreach tag $add_tag {
		.f.msg tag add $tag $start insert
	    }
	} {
	    # this key possible starts a 2-key sequence.
	    tkW3OutputSetMessage "char compose for $a"
	    set tkW3EditChar(pending) $a
	}
    }
}

proc tkW3EditBackspace {w} {
    set i [$w index insert]
    $w delete [tkW3EditDisplace $w [$w index "$i-1c"] 0] $i
    $w yview -pickplace insert
    tkW3EditUseStyle $w
    tkW3EditModifiedSet
}

proc tkW3EditDelChar {w} {
     $w delete insert [tkW3EditDisplace $w [$w index "insert +1c"] 1]
     $w yview -pickplace insert
     tkW3EditUseStyle $w
     tkW3EditModifiedSet
}


proc tkW3EditMoveInsertTo {w loc {dir {}}} {
     # 3. parameter 'dir' sets direction to forward (1) or backward (0);
     #    this applies, if the desired location is not allowed and the
     #    insertion cursor needs to be displaced in either direction.

     $w mark set insert $loc
     $w mark set insert [tkW3EditDisplace $w [$w index insert] $dir]
     $w yview -pickplace insert
     tkW3EditUseStyle $w
}

# displace insertion from "index" to next allowed location, if necessary.
# Insertion is not allowed within the representation of a bullet.
# dir 1 means search forward, 0 backward.

proc tkW3EditDisplace {w index {dir {}}} {
    global tkW3EditTags
    set inbul {}
    set names [$w tag names $index]
    foreach bultag $tkW3EditTags(bull) {
	if {[lsearch $names $bultag] != -1} {
	    set inbul $bultag
	    break
	}
     }
    if {$inbul == {}} {return $index}
    set range [tkW3EditCurRange $w $inbul $index]
    if {$dir != {} } {
	return [lindex $range $dir]
    }
    set start [lindex $range 0]
    set finish [lindex $range 1]
    scan $start  "%d" l1
    scan $finish "%d" l2
    scan $index  "%d" li
    if {($li == $l1) && ($li < $l2)} {
	return $start
    }
    return $finish
}


proc tkW3EditUseStyle {w} {
    global tkW3EditStyle tkW3EditTags
    set tags [$w tag names "insert - 1 c"]
    set tkW3EditStyle(char) ""
    set tkW3EditStyle(para) ""


    foreach tag $tags {
	if {[lsearch $tkW3EditTags(para) $tag] != -1} {
	    set tkW3EditStyle(para) $tag
	}
	if {[lsearch $tkW3EditTags(char) $tag] != -1} {
	    set tkW3EditStyle(char) $tag
	}
    }
    tkW3EditDisplayStatus
}

proc tkW3EditDisplayStatus {} {
    global tkW3EditStyle tkW3EditConvert
 
    tkW3OutputEntryPrint .style.char $tkW3EditConvert($tkW3EditStyle(char))
    tkW3OutputEntryPrint .style.para $tkW3EditConvert($tkW3EditStyle(para))
}

proc tkW3EditSetStyle {type style} {
    global tkW3EditStyle
    
    tkW3EditTagSelected .f.msg  $style
    set tkW3EditStyle($type) $style
    tkW3EditDisplayStatus
}

## ********************
## Procedures to show or hide the edit window
## ********************

proc tkW3EditOpen {{display 1}} {
    global tkW3ConfigEditTk
    if {![winfo exists ".edit"]} {
	edittkmain "" "" toplevel "" $tkW3ConfigEditTk
	
    } 

    if {$display} {
	wm deiconify .edit
    }
}

proc tkW3EditClose {} {
    wm withdraw .edit
}


## ********************
## Save Generated HTML to a file
## ********************

proc tkW3EditSave {{no_prompt 0}} {
    global fsBox
    set list [split [tkW3NavigateGetAddress] ":"]
    set type [lindex $list 0]
    set name [lindex $list 1]
    set file [file tail $name]
    set fsBox(path) ""

    if {"$type" == "file" && ![string match $name "//*"]} {
	set fsBox(path) [file dirname $name]
	if {$no_prompt != 0} {
	    tkW3EditClose
	    tkW3EditOutputHTML $fsBox(path)/$file
	    return
	}
    }

    tkW3EditClose
    FSBox {Save HTML} $file \
	{tkW3EditOutputHTML $fsBox(path)/$fsBox(name)}
    DLG:show . .fsBox
}

proc tkW3EditOutputHTML {filename {saveas 0}} {
    # 2. Parameter controls if we are doing a save or a save as...

    set dir [file dirname $filename]
    set has_rcs [tkW3EditRcsFileExists $filename]
 
    if {![file exists $dir]} {
        if {[DLG:msg . .mkdir "directory $dir does not exist" question \
                "Create" "Cancel"] == 1} {
            exec mkdir $dir
        } else {
c            return
        }
    }

    # If file is not writable and is RCS'ed attempt to check out
    if {![file writable $filename] && $has_rcs} {
	if {[DLG:msg . .rcs "RCS file exists, attempt to checkout?" question \
	     "Check out file" "Cancel"] == 1} {
		 tkW3EditRcsCheckout $filename
	     } {
		 tkW3OutputError "Checkout aborted"
	     }
    }

    if {[catch "open $filename w" file] != 0} {
        tkW3OutputError "couldn't open $filename for writing"
        return
    }
    puts $file [.edit.main.t get 1.0 end]
    close $file

    # file is saved:
    tkW3EditModifiedSet 0

    # Ask user about checking file into RCS
    if {$has_rcs || [tkW3EditRcsDirExists $filename]} {
	if {[DLG:msg . .rcs "Attempt to check file into RCS?" question \
	     "Check in file" "Cancel"] == 1} {
		 tkW3EditRcsCheckin $filename
	     }
    }

    # RELOAD:
    # make sure, file and view are identical (this is important for
    # page caching, which would otherwise keep the old page).
    if {$saveas} {
        tkW3NavigateRecordAndGoto file:$filename
    } {
        tkW3NavigateReload
    }
}

## ********************
## These procedure are used to edit the display window
## ********************

proc tkW3EditQueryChangeTitle {w} {
    global tkW3HtPage
    set w .change_title
    DLG:toplevel . $w

    DLG:draw_entries $w {"Title:"}
    tkW3OutputMakeToggles $w.toggle_frame {
	{base "Include BASE tag"}
	{is_index "Is index"}
    } 0 {expand fillx left}
    pack $w.toggle_frame -side top -fill both 
    DLG:draw_buttons $w [list "OK" "Dismiss" "Help"]
    DLG:set_entry_value .change_title 1 $tkW3HtPage(title)

    tkW3OutputToggleSet $w.toggle_frame.base $tkW3HtPage(base.use)
    tkW3OutputToggleSet $w.toggle_frame.is_index $tkW3HtPage(is_index)

    DLG:bind_button $w 1 "tkW3EditChangeTitle $w"
    DLG:bind_button $w 2 "DLG:hide $w"
    DLG:show . .change_title
}    


proc tkW3EditChangeTitle {w} {
    global tkW3HtPage base is_index
    set tkW3HtPage(title) [lindex [DLG:get_entry_values $w 1] 0]
    set tkW3HtPage(base.use) $base
    set tkW3HtPage(is_index) $is_index

    tkW3OutputSetAddress $tkW3HtPage(address) $tkW3HtPage(title)
    DLG:hide $w
}

proc tkW3EditTagSelected {w tag {begin 0} {end 0}} {
    set list [tkW3EditClearTagsFromSelected $w]
    if {$list != {}} {
	if {$begin != 0} {
	    $w mark set insert [lindex $list 0]
	    
	}
	$w tag add $tag [lindex $list 0] [lindex $list 1]
	$w mark set insert [lindex $list 1]
	if {$end != 0} {
	    tkW3HtBlankLines $begin
	    $w mark set insert [lindex $list 1]
	}
	tkW3EditModifiedSet
    }
}

proc tkW3EditBeginListItem {w} {
    tkW3HtAddBul LI
}

proc tkW3EditBeginGlossaryItem {w} {
    tkW3HtAddBul DT
    set tmp [$w index insert]
    tkW3HtAddBul DD
    $w mark set insert $tmp
}

proc tkW3EditClearTagsFromSelected {w} {
    global tkW3EditCurrentStyle

    if {[$w tag ranges sel] != {}} {
	set start [$w index sel.first]
	set finish [$w index sel.last]
	foreach tag [.f.msg tag names $start] {
	    $w tag remove $tag $start $finish
	}
	return [list $start $finish]
    }
    set tkW3EditCurrentStyle Normal
    return {}
}

proc tkW3EditCut {w} {
    tkW3EditCopy $w
    tkW3EditDelete $w
}

proc tkW3EditCopy {w} {
    global tkW3EditSelection
    set tkW3EditSelection [$w get sel.first sel.last]
}

proc tkW3EditDelete {w} {
    $w delete sel.first sel.last
}

proc tkW3EditPaste {w} {
    global tkW3EditSelection
    if {"" != [$w tag ranges sel]} {
	$w delete sel.first sel.last
	$w set mark insert sel.first
    }
    $w insert insert "$tkW3EditSelection"
}

proc tkW3EditClear {w} {
    $w delete 1.0 end
}

proc tkW3EditInsertBullet {w type} {
    tkW3HtAddBul $type
}

proc tkW3EditCurRange {w tag index} {
    set ranges [$w tag ranges $tag]
    if {$ranges == {}} {
        return {}
    }
    set i 0
    while {[$w compare [lindex $ranges $i] <= $index] == 1} {
        if {[$w compare [lindex $ranges [expr $i+1]] >= $index] == 1} {
            return [lrange $ranges $i [incr i]]
        }
        incr i 2
        if {[llength $ranges] < $i} {
            return {}
        }
    }
    return {}
}

proc tkW3EditSelectAll {w} {
    $w tag add sel 1.0 end
}

proc tkW3EditDeselectAll {w} {
    $w tag remove sel 1.0 end
}

proc tkW3EditCreateNewPage {{address ""}} {
    global tkW3HtPage
    if {[tkW3EditModifiedCheck] == 0} {
	return
    }
    
    tkW3HtBeginDoc
    set tkW3HtPage(address) $address
    set tkW3HtPage(title) ""
    tkW3HtEndDoc
    tkW3EditModifiedSet 0
    tkW3EditQueryChangeTitle .f.msg
}

## ********************
## Procedures dealing with image tags
## ********************

proc tkW3EditConvertImages {w} {
    global tkW3HtPage
    $w mark set tmp 1.0
    while {[set range [$w tag nextrange "Image" tmp]] != ""} {
	$w mark set tmp [lindex $range 0]
	foreach t [$w tag names tmp] {
	    if [regexp {i([0-9]+)} $t {} number] {
		set src $tkW3HtPage(image.$number)
	    }
	}
	$w delete tmp [lindex $range 1]
	tkW3EditInsertClean $w "\n<IMG SRC=\"$src\">" tmp
    }
}

proc tkW3EditAddImage {w} {
    DLG:entry . .image_dialog "Image Dialog" \
	{"Source:"} {Add} "tkW3HtSetImg "
    DLG:show . .image_dialog
}


proc tkW3EditChangeImage {i} {
    global tkW3HtPage
    DLG:entry . .image_dialog "Image Dialog" \
	{"Source:"} {Change} "set tkW3HtPage(image.$i) "
    DLG:set_entry_value .image_dialog 1 $tkW3HtPage(image.$i)
    DLG:show . .image_dialog
}

## *********************
## Procedures dealing with RCS
## *********************

proc tkW3EditRcsFileExists {file} {
    set directory [file dirname $file]
    set tail [file tail $file]
    return [expr "[file readable $directory/$tail,v] || 
                  [file readable $directory/RCS/$tail,v]"]
}

proc tkW3EditRcsDirExists {file} {
    set directory [file dirname $file]
    return [file isdirectory "$directory/RCS"]
}

proc tkW3EditRcsCheckout {file} {
    tkW3ConfigExecTerm "co -l $file"
}

proc tkW3EditRcsCheckin {file} {
    tkW3ConfigExecTerm "ci -u $file"
}
