## hypertext.tcl Procedures for processing the hypertext
## ==============
## Copyright (C) 1992-1993
## Globewide Network Academy
## Macvicar Institute for Educational Software Development
##
## See the file COPYRIGHT for conditions

## Conventions:
## The procedures in this procedure have names which begin with tkW3Ht

# This file contains TCL procedures which are executed by the application
# wwwish

# tkW3HtPage is an array that contains characteristics of the hypertext page
# It is used throughout the application, and should be the only connection
# between this file and the rest of the application

# tkW3HtText is an array that contains internal variable used to create the
# page.  tkW3HtText should not be called outside of this file.

# Set initial values for the array.  These values are set when this file is
# autoloaded.

set tkW3HtText(anchor.max) 0
set tkW3HtText(image.max) 0
set tkW3HtPage(is_index) 0
set tkW3HtPage(base) ""
set tkW3HtPage(modified) 0

# The next procedure is a big bottleneck in tkWWW
# It should be rewriiten in C

proc tkW3HtInsert {text {list ""}} {
    set start [.f.msg index insert]
    .f.msg insert insert $text
    foreach tag [.f.msg tag names $start] {
        .f.msg tag remove $tag $start insert
    }
    foreach tag $list {
	.f.msg tag add $tag $start insert
    }
}

proc tkW3HtSetName {address title} {
    global tkW3HtPage
    set tkW3HtPage(address) $address
    set tkW3HtPage(title) $title
    tkW3OutputSetAddress $address $title
}
 
proc tkW3HtBeginDoc {} {
    global tkW3HtPage tkW3HtText
 
    unset tkW3HtPage
    set tkW3HtPage(anchor.index) 1
    set tkW3HtPage(image.index) 1
    set tkW3HtPage(is_index) 0
    set tkW3HtPage(modified) 0
    set tkW3HtPage(base) ""
    set tkW3HtPage(base.use) 0
    set tkW3HtPage(next_id) ""
    set tkW3HtPage(index.href) ""
    set tkW3HtPage(link) ""

    set tkW3HtText(in_anchor) 0
    set tkW3HtText(list.stack) {}
    set tkW3HtText(para.stack) {}
    set tkW3HtText(char.stack) {}
    set tkW3HtText(background) [lindex [.f.msg configure -background] 4]

    tkW3OutputClearBody
}

proc tkW3HtBeginAnc {name href} {
    global tkW3HtPage tkW3HtText

    set i $tkW3HtPage(anchor.index)
    set tkW3HtPage(anchor.name.$i) $name
    set tkW3HtPage(anchor.href.$i) $href
    set tkW3HtPage(id.$name) h$i
    set tkW3HtText(in_anchor) 1

    if {$i >= $tkW3HtText(anchor.max)} {
        .f.msg tag configure h$i -relief raised -borderwidth 2 \
	    -background $tkW3HtText(background)
	incr tkW3HtText(anchor.max)
    }
}

proc tkW3HtEndAnc {} {
    global tkW3HtPage tkW3HtText
    set tkW3HtText(in_anchor) 0
    incr tkW3HtPage(anchor.index)
}

proc tkW3HtAdd {string {styles ""}} {
    global tkW3HtText tkW3HtPage

    if $tkW3HtText(in_anchor) {
	lappend styles Anchor
	lappend styles h$tkW3HtPage(anchor.index)
    }

    lappend styles [tkW3HtListLast tkW3HtText(para.stack)]
  
    if [llength $tkW3HtText(char.stack)] {
	lappend styles [tkW3HtListLast tkW3HtText(char.stack)]
    }
    tkW3HtInsert $string $styles
}

proc tkW3HtBegin {stack elem} {
    global tkW3HtText
    
    switch -regexp $elem {
	{^H[1-9]+$} -
	{^ADDRESS$} {
	    tkW3HtBlankLines 2
	}
        {^XMP$} -
	{^PRE$} {
	    tkW3HtBlankLines 1
	}
	{^Q$} {
	    tkW3HtInsert "``" blank
	}
    }
    lappend tkW3HtText($stack.stack) $elem
}

proc tkW3HtEnd {stack elem} {
    global tkW3HtText

    tkW3HtListPop tkW3HtText($stack.stack)

    switch -regexp $elem {
	{^H[1-9]+$} -
	{^ADDRESS$} {
	    tkW3HtBlankLines 1
	}
	{^Q$} {
	    tkW3HtInsert "''" blank
	}
    }

    if {$stack == "list" && $tkW3HtText(list.stack) == {}} {
	tkW3HtBlankLines 2
    }
}

proc tkW3HtEndDoc {} {
    global tkW3HtPage
    if {$tkW3HtPage(base) == ""} {
	set tkW3HtPage(base) $tkW3HtPage(address)
    }
}

proc tkW3HtSetNextId {id} {
    global tkW3HtPage
    set tkW3HtPage(next_id) $id
}

proc tkW3HtAddBul {type} {
    global tkW3HtText

    switch -exact $type {
	BR {
	    tkW3HtInsert "\n" BR
	    return
	}

	HR {
	    tkW3HtInsert "\n_______________________________________________________________________________\n" HR
	    return
	}

	P {
	    tkW3HtInsert "\n\n" P
	    return
	}
	
	DT {
	    tkW3HtBlankLines 1
	}
    }

    tkW3HtInsert "\n" blank
    for {set i 1} {$i < [llength $tkW3HtText(list.stack)]} {incr i} {
	if {$type == "DD"} {
	    tkW3HtInsert "\t" "DD"
	} {
	    tkW3HtInsert "\t" "LI"
	}
    }

    case $type {
	"LI" {
	    tkW3HtInsert "* " "LI type.[tkW3HtListLast tkW3HtText(list.stack)]"
	}
	"DD" {
	    tkW3HtInsert "   * " "DD"
	}
	"DT" {
	    tkW3HtInsert "* " "LI type.DL"
	}
    }
}

proc tkW3HtSetImg {source {ismap 0}} {
    global tkW3HtPage tkW3HtText

    set image_string "<IMAGE"
    set i $tkW3HtPage(image.index)

    set tkW3HtPage(image.ismap.$i) $ismap   
    if $tkW3HtText(in_anchor) {
	set anchor_num $tkW3HtPage(anchor.index)
	set tkW3HtPage(image.anchor.$i) $tkW3HtPage(anchor.href.$anchor_num)
        append image_string "-ANCHOR"
	if {$ismap} {
	    append image_string "-ISMAP"
	}
    } {
	set tkW3HtPage(image.anchor.$i) ""
    }
  
    append image_string ">"

    tkW3HtAdd $image_string "Image i$i"
    if {$i > $tkW3HtText(image.max)} {
	.f.msg tag configure i$i -relief raised -borderwidth 2 \
	    -background $tkW3HtText(background)
	incr tkW3HtText(image.max)
    }
    set tkW3HtPage(image.$i) $source
    incr tkW3HtPage(image.index)
}

proc tkW3HtSetInd {href} {
    global tkW3HtPage
    set tkW3HtPage(is_index) 1
    set tkW3HtPage(index.href) $href
}

proc tkW3HtSetBase {href} {
    global tkW3HtPage
    set tkW3HtPage(base.use) 1
    set tkW3HtPage(base) $href
}


proc tkW3HtBlankLines {n} {
    set text [regexp {^[ ]*$} \
	      [.f.msg get {insert linestart} {insert - 1 char lineend}]]
    if {!$text} {
	for {set i 0} {$i<$n} {incr i} {
	    tkW3HtInsert "\n" blank
	}
    }
}

proc tkW3HtListPop {in_list} {
    upvar $in_list list
    set index [llength $list]
    incr index -1
    set item [lindex $list $index]
    incr index -1
    set list [lrange $list 0 $index]
    return $item
}

proc tkW3HtListLast {in_list} {
    upvar $in_list list
    set index [llength $list]
    incr index -1
    lindex $list $index
}
proc tkW3HtSetLink {relation href} {
    global tkW3HtPage
    lappend tkW3HtPage(link) $relation
    set tkW3HtPage(link.$relation) $href
}

proc tkW3HtButtonPress {w loc b} {
    global tkW3HtPage tkW3HtText
    set tag_list [.f.msg tag names $loc]
    set index [lsearch -regexp $tag_list  {^i[0-9]+$}]
 
    if {$index == -1} {
	set index [lsearch -regexp $tag_list  {^h[0-9]+$}]
    }
    if {$index != -1} {
	$w tag configure [lindex $tag_list $index] -relief sunken
	update idletasks
    }
}

proc tkW3HtButtonRelease {w loc b} {
    global tkW3HtPage tkW3HtText
    set tag_list [.f.msg tag names $loc]

    # Search for active tag
    # image tags override hypertext anchor tags 
    set index [lsearch -regexp $tag_list  {^i[0-9]+$}]
    if {$index == -1} {
	set index [lsearch -regexp $tag_list  {^h[0-9]+$}]
    }

    if {$index != -1} {
	set tag [lindex $tag_list $index]
	$w tag configure $tag -relief raised
	update idletasks
	regexp {([hi])([0-9]+)} $tag {} tag_type i
	switch $tag_type {
	    "h" {
		switch $b {
		    "1" {
			tkW3NavigateRecordAndGoto $tkW3HtPage(anchor.href.$i)
		    }
		    "2" {
			tkW3NavigateClone $tkW3HtPage(anchor.href.$i)
		    }
		    "3" {
			tkW3EditSetupAnchorDialog $w $i
		    }
		}
	    }
	    "i" {
		    switch $b {
			"1" {
			    tkW3NavigateRecordAndGoto \
				$tkW3HtPage(image.$i) {} \
				$tkW3HtPage(image.anchor.$i) \
				$tkW3HtPage(image.ismap.$i)
			}
			"2" {
			    tkW3NavigateClone $tkW3HtPage(image.$i) 
			}
			"3" {
			    tkW3EditChangeImage $i
			}
		    }
		}
	}
    }
}

proc tkW3HtProgress {msg} {
    tkW3OutputSetMessage $msg
    update idletasks
}
