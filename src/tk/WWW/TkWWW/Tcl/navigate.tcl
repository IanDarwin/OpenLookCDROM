## navigate.tcl Procedures for navigation
## ==============
## Copyright (C) 1992-1993
## Globewide Network Academy
## Macvicar Institute for Educational Software Development
##
## See the file COPYRIGHT for conditions

## The procedures in this file are responsible for navigation between 
## different hypertext pages on World Wide Web.  They are called by the
## objects in other tkWWW files in response to a user's request to 
## display a WWW page.

## Conventions:
##   All procedures in this file begin with tkW3Navigate

proc tkW3NavigateBacktrack {} {
    if {[set backpage [tkW3HistoryPop]] !={}} {
	tkW3NavigateGoto [lindex $backpage 1] [lindex $backpage 3]
	set anchor [lindex $backpage 4]
	if {$anchor != ""} {
	    tkW3ImageEnableAnchor $anchor [lindex $backpage 5]
	} 
    } {
	tkW3OutputError "Error: No pages previous to this"
    }
}

proc tkW3NavigateClone {{page ""} {anchor ""} {ismap 0}} {
  global tkW3SourcePath tkW3ConfigHomePage
  
  if {$page == ""} {
      set page [tkW3NavigateGetAddress]
  }

  tkW3OutputSetMessage "Opening New Window ..." 5000
  set command [list exec tkwww -wwwstart \
             [HtParseName $page [tkW3NavigateGetAddress]] \
            -wwwhome $tkW3ConfigHomePage -wwwnoinfo -name tkWWW ]
  if {$anchor != ""} {
      lappend command "-wwwimageanc"
      lappend command $anchor
      if {$ismap != 0} {
	  lappend command "-wwwimageismap"
      }
  }
  lappend command "&"
  eval $command
}

# This closes this tkW3 window
proc tkW3NavigateCloseThisWindow {} {
# Necessary to clear color map
  tkW3ImageReleaseColorCells
# Necessary to clear name from interpreter table
  destroy .
  catch "exit"
}

# This closes all tkWWW windows 
proc tkW3NavigateCloseAllWindows {} {
    set self [winfo name .]
    foreach interp [winfo interps] {
	if {[regexp {tkWWW.*} $interp] && "$interp" != "$self"} {
	    catch "send \"$interp\" {after 1 {tkW3NavigateCloseThisWindow}}" 
	}
    }
    tkW3NavigateCloseThisWindow
}

## tkW3NavigateFind: Does keyword search off current page
## ----------
## Arguments:
##   key - a space separated list of keywords
## No return value
##
## This procedure performs a keyword search off the currently displayed
## page.  It works by converting the keywords and current page into a
## World Wide Web address, and then calls the procedure tkW3NavigateGoto
## and goes to that page.
##

proc tkW3NavigateFind {key} {
    global tkW3HtPage
    regsub -all {[ ]+} $key {+} key
    if {$tkW3HtPage(index.href) == ""} {
	set address $tkW3HtPage(address)
    } {
	set address [HtParse $tkW3HtPage(index.href) $tkW3HtPage(base)]
    }

    set address [string trim $address " \n?"]
    set address [lindex [split $address "?"] 0]
    append address "?" $key
    tkW3NavigateRecordAndGoto $address
}

# Some abstraction barriers
# Note that tcl returns the value of the last statement executed

proc tkW3NavigateGetAddress {} {
   .titles.address_entry get
}

proc tkW3NavigateGetTitle {} {
   .titles.title_entry get
}

proc tkW3NavigateGoto {page {scroll_position ""}} {
    global tkW3HtPage

    # this is the central place, where the modified-flag is checked and reset.
    # (other cases of reset are:
       #           - tkW3EditCreateNewPage 
       #   - tkW3EditOutputHTML ).
    if { [tkW3EditModifiedCheck] == 0} {
	return
    }

    # Change the cursor to a busy signal and tell user we are getting file
    tkW3OutputCursorWait
    tkW3OutputSetMessage "Getting page ..."
    tkW3ImageHideImageWindow
    update idletask
    
    if {![string match "#*" $page]} {
	# case on the type of page
	# wish doesn't like comments in case statements
	case [set page_name [HtParseName $page $tkW3HtPage(base)]] {
	    
	    {telnet:* tn3270:* rlogin:*} {
		tkW3TelnetPage $page_name
	    }
	    {exec:*} {
		# Find the command
		if [regexp {^exec:(.*)} $page_name {} command] {
		    regsub -all %20 $command " " command

		    tkW3OutputSetAddress $page_name {}
		    
		    tkW3NavigatePreface "System Call
Execute system command \"$command\"
" {Push here to execute System Call} "Executing System Call \"$command\"..." \
"exec $command &"
		}
	    }
	    default {
		# if the file type if file, http, gopher, or wais we need 
		# to load the page in from the server
		tkW3NavigateLoadPageFromServer $page_name
	    }
	} 
	# Set the output to the correct line
	if {$scroll_position != ""} {
	    tkW3OutputSetScrollPosition $scroll_position
	}
    } {
	set page_name $page
    }

    # Goto to the anchor
    set page_tag ""
    regexp {#(.*)$} $page_name {} page_tag
    if {$page_tag != ""} {
	set tag $tkW3HtPage(id.$page_tag)
	if {$tag != ""} {
	    .f.msg yview $tag.first
	}
    }

    # Display
    tkW3ConfigDisplay 

    # Clear the message and set the cursor back to normal
    tkW3OutputSetMessage {}
    tkW3OutputCursorNormal
}

proc tkW3NavigateLoadPageFromServer {page_name} {
    # Chop off the anchor
    regexp {^[^#]*} $page_name page_value

    # Load the page and get the return code
    # If the return code contains "Error" signal an error
    if [catch [list HtLoad $page_value] return_value] {
	# Ooops there's an error
	# If it is a "Page Not Found" error create new page

	if {[regexp {Unable to acces} $return_value] &&
	    [regexp {^file:} $page_name]} {
	    if {[DLG:msg . .page_not_found "$page_name
Do you wish to create?" question "Yes" "No"]==1} {
		     tkW3EditCreateNewPage $page_name
		 } {
		     tkW3HistoryRestore
		 }
	    return
	}
	# Undo any changes we have made to the history list
	tkW3HistoryRestore
	# Tell the user about the error
	tkW3OutputError $return_value
    }
}

proc tkW3NavigatePreface {body_text type command} {
    tkW3OutputClearBody
    tkW3HtInsert "$body_text
To display press the button below and wait a few seconds
"
    tkW3HtInsert "Press here to show $type" action
    .f.msg tag configure action -relief raised -borderwidth 2 -background \
	[lindex [.f.msg configure -background] 4]

    .f.msg tag bind action <ButtonPress> "%W tag configure action \
-relief sunken"
    .f.msg tag bind action <ButtonRelease-1> \
	"%W tag configure action -relief raised
tkW3OutputSetMessage \{Displaying $type...\} 5000
$command"
}

proc tkW3NavigateSaveText {filename} {
   set file [open $filename a+]
   puts $file [.f.msg get 0.0 end]
   close $file
} 

proc tkW3NavigatePrintUsingCommand {command} {
    tkW3NavigateSaveText "| $command"
}

proc tkW3NavigateMailText {to subject} {
    tkW3ConfigSendMail $to $subject [.f.msg get 0.0 end]
}

# Record current address on the history list and go to the requested page

proc tkW3NavigateRecordAndGoto {page {scroll ""} {anchor ""} {ismap 0}} {
    tkW3HistoryRecord \
	[tkW3NavigateGetTitle] [tkW3NavigateGetAddress] \
	[tkW3OutputGetScrollPosition]  $anchor $ismap
    tkW3NavigateGoto $page
    if {$anchor != ""} {
	tkW3ImageEnableAnchor $anchor $ismap
    }
}
    
proc tkW3NavigateUncache {address} {
    HtUncache $address
}

proc tkW3NavigateReload {} {
    set address [tkW3NavigateGetAddress]
    tkW3NavigateUncache $address
    tkW3NavigateGoto $address
}

proc tkW3NavigateInitialize {} {
    global tkW3ConfigFileTypes
    global tkW3ConfigFileEncodings

    foreach i $tkW3ConfigFileTypes {
	eval HtAddType $i
    }

    foreach i $tkW3ConfigFileEncodings {
	eval HtAddEncoding $i
    }
}
