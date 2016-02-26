## telnet.tcl interface with telnet
## ==============
## Copyright (C) 1992-1993 
## Globewide Network Academy
## Macvicar Institute for Educational Software Development
##
## See the file COPYRIGHT for conditions

proc tkW3Telnet {command host port} {
   global tkW3ConfigViewer
   if {$port=={}} {
     exec $tkW3ConfigViewer(terminal) -e $command $host &
   } {
     exec $tkW3ConfigViewer(terminal) -e $command $host $port &
   }
}

proc tkW3TelnetPage {page} {
    # Parse the page to get command user host and port
    if [regexp {([a-zA-z0-9]+)://([a-zA-Z0-9\-]+@)*([-a-zA-Z0-9\.]+)(:[0-9]+)*}  $page {} command user host port] {
	set user [string trim $user "@"] 
	set port [string trimleft $port ":"]
 
	# Put together a string to let user know what is going on
	set body_text "$command to\nhost: $host\n"
	if {$port!={}} {
	    append body_text "Port: $port\n"
	}
	if {$user!={}} {
	    append body_text "Log in as \"$user\"\n"
	}
	tkW3OutputSetAddress $page {}
	tkW3NavigatePreface $body_text "telnet connection" \
	    [format "tkW3Telnet \{%s\} \{%s\} \{%s\}" $command $host $port]
    }
}


