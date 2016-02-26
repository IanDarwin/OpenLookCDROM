## help.tcl tkW3 help facility
## ==============
## Copyright (C) 1992-1993
## Globewide Network Academy
## Macvicar Institute for Educational Software Development
##
## See the file COPYRIGHT for conditions

## Conventions:
##   all global visible objects in this file should begin with 
##   tkW3Help

proc tkW3HelpInitialize {} {
    tkW3HelpCreateWhineDialog . .send_mail_dialog
}

proc tkW3HelpCreateWhineDialog {parent w} {
    global tkW3Version tkW3ConfigMailList

    DLG:toplevel . $w
    DLG:draw_entries $w {To: Subject:}
    DLG:set_entry_value $w 1 $tkW3ConfigMailList(bugs)
    DLG:set_entry_value $w 2 "Comments on tkWWW Version $tkW3Version"
    
    pack append $w \
	[frame $w.f -relief raised] { top expand fill} 

    pack append $w.f \
	[label $w.label \
	   -text "Please type your comments below" ] { fillx top} \
	[scrollbar $w.vs -command "$w.text yview" -orient vertical ] \
	{ right filly } \
        [text $w.text -yscrollcommand "$w.vs set" ] {expand fill top}

    DLG:draw_buttons $w [list "Send Mail" "Dismiss" "Help"]
    DLG:bind_button $w 1 "wm withdraw $w ; 
    tkW3ConfigSendMail \
        \[DLG:get_entry_value $w 1\] \[DLG:get_entry_value $w 2\] \
        \[$w.text get 0.0 end\]"

    DLG:bind_button $w 2 "DLG:hide $w"
    DLG:bind_button $w 3 "tkW3HelpNoHelp"
}

proc tkW3HelpNotAvailable {} {
    DLG:msg . .not_available \
	"Sorry, This feature is not available yet" info "OK"
}

proc tkW3HelpNoHelp {} {
    DLG:msg . .no_help \
	"Sorry, Help is not available yet" info "OK"
}

proc tkW3HelpAddToMailingList {} {
    global tkW3Version tkW3ConfigMailList
    tkW3OutputSetMessage "Sending request to $tkW3ConfigMailList(request)"
    tkW3ConfigSendMail $tkW3ConfigMailList(request) \
	"Subscribe $tkW3Version" "subscribe tkwww"
    tkW3OutputSetMessage "Request sent to $tkW3ConfigMailList(request)"
}

proc tkW3HelpRemoveFromMailingList {} {
    global tkW3Version tkW3ConfigMailList
    tkW3OutputSetMessage "Sending request to $tkW3ConfigMailList(request)"
    tkW3ConfigSendMail $tkW3ConfigMailList(request) \
	"Remove $tkW3Version" "unsubscribe tkwww"
    tkW3OutputSetMessage "Request sent to $tkW3ConfigMailList(request)"
}

proc tkW3HelpGetTopic {topic} {
    if {$topic != "no_help"} {
	global tkW3ConfigHelpRoot
	tkW3NavigateClone $tkW3ConfigHelpRoot/$topic.html
    } {
	DLG:msg . .no_help \
	    "Sorry: No help is available on this topic yet" info "OK"
    }
}


proc tkW3HelpAbout {} {
    global tkW3Version tkW3ConfigLogo tkW3ConfigMailList

# I insert the \n so that the blank lines don't get removed when this 
# is run through tcl2c

    DLG:msg_nowait . .information "
tkWWW Version $tkW3Version
by Joseph Wang (joe@mit.edu)
\nCopyright \251 1992-1993
Globewide Network Academy (GNA)
Macvicar Institute for Educational Software Development
\nUnder the terms of the GNU Public License Version 2.0
\nThe tkWWW mailing list is $tkW3ConfigMailList(list)
Send subscription requests to $tkW3ConfigMailList(request)
Send bug reports to $tkW3ConfigMailList(bugs)
"  $tkW3ConfigLogo "OK" "Credits"
    DLG:bind_button .information 1 {DLG:destroy .information}
    DLG:bind_button .information 2 {tkW3HelpGetTopic credits}
}

