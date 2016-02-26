## edit.generate.tcl Generate HTML
## ==============
## Copyright (C) 1992-1994
## Globewide Network Academy
## Macvicar Institute for Educational Software Development
##
## See the file COPYRIGHT for conditions

## ********************
## Procedures to generate HTML in the edit window from 
## stuff displayed in the display window
## ********************

proc tkW3EditGenerate {} {
    tkW3EditOpen
    tkW3EditCopyText .f.msg .edit.main.t
    tkW3EditCopyTags .f.msg .edit.main.t
    tkW3EditConvertToHTML .edit.main.t
}


proc tkW3EditCopyText {from to} {
    $to delete 1.0 end
    $to insert 1.0 [$from get 1.0 end]
}

proc tkW3EditCopyTags {from to} {
    foreach item [$from tag names] {
	set range [$from tag ranges $item] 
	set length [llength $range]
	for {set i 0} {$i < $length} {incr i 2} {
	    set j $i
	    incr j 1
	    $to tag add $item [lindex $range $i] [lindex $range $j] 
	}
    }
}

proc tkW3EditConvertToHTML {w} {
    global tkW3HtPage

    $w mark set tmp 1.0
    $w insert tmp "<!DOCTYPE htmlplus PUBLIC \"-//Internet/RFC xxxx//EN\">
<HTMLPLUS>
<HEAD>
<TITLE>$tkW3HtPage(title)</TITLE>"

    if {$tkW3HtPage(base) != "" && $tkW3HtPage(base.use)} {
	$w insert tmp "\n<BASE HREF=\"$tkW3HtPage(base)\">"
    }
    if $tkW3HtPage(is_index) {
	$w insert tmp "\n<ISINDEX>"
    }
    if {$tkW3HtPage(next_id) != ""} {
	$w insert tmp "\n<NEXTID N=\"$tkW3HtPage(next_id)\">"
    }
    $w insert tmp "
</HEAD>
<BODY>"

    tkW3EditDeleteTextWithTag $w blank


    for {set n 1} {$n < 7} {incr n} {
	tkW3EditDelimitTag $w H$n "\n<H$n>" "</H$n>"
    }

    tkW3EditConvertLineItems $w     
    set delimit_tags {
	{ADDRESS "\n<ADDRESS>" "\n</ADDRESS>"}
	{PRE "\n<PRE>" "\n</PRE>"}
	{CODE "<CODE>" "</CODE>"}
	{TT "<TT>" "</TT>"}
	{B "<B>" "</B>"}
	{I "<I>" "</I>"}
	{U "<U>" "</U>"}
	{EM "<EM>" "</EM>"}
	{STRONG "<STRONG>" "</STRONG>"}
	{SAMP "<SAMP>" "</SAMP>"}
	{KBD "<KBD>" "</KBD>"}
	{VAR "<VAR>" "</VAR>"}
	{DFN "<DFN>" "</DFN>"}
	{CITE "<CITE>" "</CITE>"}
    }


    foreach tag $delimit_tags {
	tkW3EditDelimitTag $w [lindex $tag 0] [lindex $tag 1] [lindex $tag 2]
    }
    tkW3EditConvertBullets $w HR "\n<HR>"
    tkW3EditConvertBullets $w P "\n<P>"
    tkW3EditBreakLines $w
    tkW3EditConvertAnchors $w
    tkW3EditConvertImages $w
    $w insert end "\n</BODY>"

}

proc tkW3EditConvertAnchors {w} {
    global tkW3HtPage
    $w mark set tmp 1.0
    while {[set range [$w tag nextrange "Anchor" tmp]] != ""} {
	$w mark set tmp [lindex $range 1]
	$w mark set insert [lindex $range 0]

	foreach t [$w tag names insert] {
	    if [regexp {h([0-9]+)} $t {} n] {
		set string "<A"
		if {$tkW3HtPage(anchor.name.$n) != ""} {
		    append string " NAME=\"$tkW3HtPage(anchor.name.$n)\""
		}
		if {$tkW3HtPage(anchor.href.$n) != ""} {
		    append string " HREF=\"$tkW3HtPage(anchor.href.$n)\"" 
		}
		append string ">"
		
		tkW3EditInsertClean $w $string
		tkW3EditInsertClean $w "</A>" tmp
	    }
	}
    }
}

proc tkW3EditBreakLines {w} {
    for {set i 1} {[$w compare $i.0 < end]} {incr i} {
	set chars [lindex [split [$w index "$i.0 lineend"] "."] 1]
	if {$chars > 80 && [$w compare "$i.80 wordstart" > "$i.0"]} {
	    $w mark set tmp "$i.80 wordstart"
	    if {[$w get tmp] == " "} {
		$w delete tmp
	    } 
	    $w insert tmp "\n"
	}
    }
}

proc tkW3EditConvertBullets {w text_tag sgml_tag} {
    $w mark set insert 1.0
    while {[set range [$w tag nextrange $text_tag insert]] != ""} {
	$w mark set insert [lindex $range 0]
	$w delete insert [lindex $range 1]
	tkW3EditInsertClean $w $sgml_tag
    }
}

# This long hairy procedure converts list items to the proper SGML tags


proc tkW3EditConvertLineItems {w} {
    $w mark set tmp 1.0
    set list_stack ""
    set prev_line 0

    while {[set range [$w tag nextrange LI tmp]] != ""} {
	$w mark set tmp [lindex $range 0]
	set string [$w get tmp [lindex $range 1]]

	set new_list_type "UL"
	foreach t [$w tag names [lindex $range 0]] {
	    if [regexp {type.([A-Za-z]+)} $t {} n] {
		set new_list_type $n
		break
	    }
	}

        $w delete tmp [lindex $range 1]

	# Get the list level by counting the number of tabs before the 
	# bullet

	if [regexp {^	+} $string string] {
	    set new_list_level [string length $string]
	    incr new_list_level
	} {
	    set new_list_level 1
	}
	
	# Get the list type

	set list_level [llength $list_stack]

	$w mark set insert [$w index tmp]
	set current_line [lindex [split [$w index tmp] "."] 0]

	if {$current_line > $prev_line && $prev_line > 0} {
	    tkW3EditInsertClean $w "\n</[tkW3HtListPop list_stack]>" endmark
	    incr list_level -1
	    incr current_line
	}

	
	while {$new_list_level < [llength $list_stack]} {
	    tkW3EditInsertClean $w "\n</[tkW3HtListPop list_stack]>"
	    incr list_level -1
	    incr current_line
	}

	if {$new_list_level > $list_level} {
	    tkW3EditInsertClean $w "\n<$new_list_type>"
	    lappend list_stack $new_list_type
	    incr current_line
	}

	$w mark set endmark {insert lineend} 
	if {$new_list_type == "DL"} {
	    tkW3EditInsertClean $w "\n<DT>"
	} {
	    tkW3EditInsertClean $w "\n<LI>"
	}

	incr current_line
	set prev_line $current_line
    }
    while {[llength $list_stack]} {
	tkW3EditInsertClean $w "\n</[tkW3HtListPop list_stack]>" endmark
	incr list_level -1
    }

    tkW3EditConvertBullets $w DD "\n<DD>"
}

proc tkW3EditDeleteTextWithTag {w tag} {
    $w mark set tmp 1.0
    while {[set range [$w tag nextrange $tag tmp]] != ""} {
	$w mark set tmp [lindex $range 1]
	$w delete [lindex $range 0] tmp
    }

}

proc tkW3EditDelimitTag {w tag begin end} {
    $w mark set tmp 1.0
    while {[set range [$w tag nextrange $tag tmp]] != ""} {
	$w mark set insert [lindex $range 0]
	$w mark set tmp [lindex $range 1]
	
	tkW3EditInsertClean $w $begin
	tkW3EditInsertClean $w $end tmp
    }
}

proc tkW3EditInsertClean {w text {tag insert}} {
    set start [$w index $tag]
    $w insert $tag $text
    foreach t [$w tag names $start] {
	$w tag remove $t $start $tag
    }
}
