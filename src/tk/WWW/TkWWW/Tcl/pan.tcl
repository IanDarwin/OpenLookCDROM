## pan.tcl: Procedures for handling Xmosaic personal annotations
## ===============
## Copyright (C) 1993
## Globewide Network Academy
## Macvicar Institute for Educational Software Development
##
## See the file COPYRIGHT for conditions
##
## This file is a tcl translation of the procedures
## used in X Mosaic by Marc Anderssen

set tkW3PanLogFormatCookieOne \
 "ncsa-mosaic-personal-annotation-log-format-1" 
set tkW3PanAnnotationFormatOne "<ncsa-annotation-format-1>"

set tkW3PanDefaultDirectory "$env(HOME)/.mosaic-personal-annotations"
set tkW3PanLogFilename "LOG"
set tkW3PanAnnotationPrefix "PAN-"


## The format of the list
global tkW3PanAnnotationList

## Initialize
proc tkW3PanInitialize {} {
    global tkW3PanDefaultDirectory 
    global tkW3PanLogFilename

    tkW3PanEnsureDirectoryExists $tkW3PanDefaultDirectory
    tkW3PanReadLog $tkW3PanDefaultDirectory/$tkW3PanLogFilename
}

## Read a Pan File
proc tkW3PanReadLog {filename} {
    global tkW3PanLogFormatCookieOne 
    global tkW3PanAnnotationList

    if {[catch "open $filename r" file] != 0} {
	return
    }
    
    gets $file line
    if {[string first $tkW3PanLogFormatCookieOne $line] == -1} {
	return
    }

    gets $file line

    catch {unset tkW3PanAnnotationList}
    while {[gets $file line] != -1} {
	set tkW3PanAnnotationList([lindex [split $line] 0]) \
	    [lrange $line 1 end]
    }
    close $file
}

proc tkW3PanEnsureDirectoryExists {directory} {
    if {![file isdirectory $directory]} {
	exec mkdir $directory
    }
}

proc tkW3PanIsEditablePan {url} {
    global tkW3AnnotationFormatOne
    if {[regexp {PAN-[0-9]+\.html} $url] == 1} {
	return 1
    } {
	return 0
    }
}

proc tkW3PanWriteLog {filename} {
    global tkW3PanLogFormatCookieOne
    global tkW3PanAnnotationList
    if [catch {open $filename "w"} file] {
	return
    }

    puts $file $tkW3PanLogFormatCookieOne
    puts $file "Personal"
    foreach item [array names tkW3PanAnnotationList] {
	puts $file "$item " nonewline
	foreach piece $tkW3PanAnnotationList($item) {
	    puts $file $piece nonewline
	    puts $file " " nonewline
	}
	puts $file ""
    }

    close $file
}

proc tkW3PanWritePan {filename title author text} {
    global tkW3PanAnnotationFormatOne
    if [catch "open $filename w" file] {
	return
    }
    puts $file "\
$tkW3PanAnnotationFormatOne\n\
<title>$title</title>\n\
<h1>$title</h1>\n\
<address>$author</address>\n\
<address>[exec date]</address>\n\
_______________________________________\n\
<pre>\n\
$text"
    close $file
}

proc tkW3PanNewPan {url title author text} {
    global tkW3PanAnnotationList
    global tkW3PanDefaultDirectory
    global tkW3PanLogFilename

    if {[catch {set tkW3PanAnnotationList($url)} list]} {
	set list ""
    }

# Scan through the files for an available ID
    for {set id 1} {[file exists [tkW3PanIdToFilename $id]]} {incr id 1} {}
 
    lappend list $id

    set tkW3PanAnnotationList($url) $list

    tkW3PanWriteLog $tkW3PanDefaultDirectory/$tkW3PanLogFilename
    tkW3PanWritePan [tkW3PanIdToFilename $id] $title $author $text
}

proc tkW3PanDeletePan {id} {
    global tkW3PanAnnotationList
    foreach i [array names tkW3PanAnnotationList] {
	set j [lsearch $tkW3PanAnnotationList($i) $id]
	if {$j != -1} {
	    set $tkW3PanAnnotationList($i) \
		[ lreplace $tkW3PanAnnotationList($i) $j ]
	    if {[llength $tkW3PanAnnotationList($i)] == 0} {
		unset $tkW3PanAnnotationList($i)
	    }
	    exec mv [tkW3PanIdToFilename $id] \
		[format "%s/#%s%d" \
		 $tkW3PanDefaultDirectory $tkW3PanLogFilename $id]
	    break
	}
    }
}

proc tkW3PanModifyPan {id title author text} {
    tkW3PanWritePan [tkW3PanIdToFilename $id] $title $author $text
}

proc tkW3PanIdToFilename {id} {
    global tkW3PanDefaultDirectory
    global tkW3PanAnnotationPrefix

    return $tkW3PanDefaultDirectory/$tkW3PanAnnotationPrefix$id.html
}

# The next procedure returns a list of hypertext commands which 
# display the annotations
proc tkW3PanDisplayAnnotations {url} {
    global tkW3PanAnnotationList
    catch {destroy .menu.annotate.m.annotate}
    menu .menu.annotate.m.annotate
    if {[catch  {set tkW3PanAnnotationList($url)} list] == 0} {
	foreach item $list {
	    set file [open [tkW3PanIdToFilename $item] "r"]
	    gets $file line
	    gets $file title
	    gets $file line
	    gets $file line
	    gets $file date

	    regexp {<title>(.*)</title>} $title {} title
	    regexp {<address>(.*)</address>} $date {} date
	    .menu.annotate.m.annotate add command -label "$title $date" \
		-command "tkW3NavigateRecordAndGoto file:[tkW3PanIdToFilename $item]"
	}
	return 1
    }
    return 0
}

# return list: {id title author text}

proc tkW3PanGrokPanPieces {url} {
    global tkW3PanAnnotationPrefix

    regexp {:(//[^/])*(.*)} $url {} {} filename
    regexp {([0-9]+)\.html} $filename {} id

    if {[catch "open $filename r" file]} {
	return
    }
    gets $file line
    gets $file title
    gets $file line
    gets $file author
    gets $file date
    gets $file line
    gets $file line

    regexp {<address>(.*)</address>} $author {} author
    regexp {<title>(.*)</title>} $title {} title
    regexp {<address>(.*)</address>} $date {} date

    set text [read $file]
    close $file
    return [list $id $title $author $text]
}

